{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parser.Expression
  ( metaexpr
  ) where
--------------------------------------------------------------------------------
import           AST.Expression            hiding (loc, inner)
import qualified AST.Expression            as E (loc, inner)
import           AST.Object                hiding (loc, name, inner)
import qualified AST.Object                as O (loc, name, inner)
import           Entry                     (Entry' (..), Entry'' (..), info,
                                            varType)
import           Graciela
import           Lexer
import           Limits
import           Location
import           MyParseError              as PE
import           Parser.Token
import           SymbolTable               (closeScope, insertSymbol, lookup,
                                            openScope)
import           Token
import           Treelike
import           Type
--------------------------------------------------------------------------------
import           Control.Lens              (makeLenses, use, (%=), (^.))
import           Control.Monad             (void, when)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, evalState, evalStateT, get,
                                            modify, put)
import           Data.Functor              (($>))
import qualified Data.Map                  as Map (lookup)
import           Data.Monoid               ((<>))
import           Data.Sequence             (Seq, (|>))
import qualified Data.Sequence             as Seq (empty, singleton)
import           Data.Text                 (Text, pack, unpack)
import           Prelude                   hiding (lookup)
import           Text.Megaparsec           (between, getPosition, runParser,
                                            runParserT, sepBy, sepBy1, some,
                                            try, (<|>))
import           Text.Megaparsec.Expr      (Operator (..), makeExprParser)
--------------------------------------------------------------------------------
import Control.Lens ((&~))


import Debug.Trace

expression :: Graciela Expression
expression = evalStateT expr Nothing


data ProtoRange
  = ProtoVar                -- ^ The associated expression is the variable
                            -- of interest
  | ProtoQRange  QRange     -- ^ A valid QRange has been formed
  | ProtoLow     Expression -- ^ A lower bound has been found
  | ProtoHigh    Expression -- ^ An upper bound has been found
  | ProtoNothing            -- ^ No manner of range has been formed

type MetaExpr = (Expression, ProtoRange)

type GracielaRange = StateT (Maybe Text) Graciela


expr :: GracielaRange Expression
expr = fst <$> metaexpr


metaexpr :: GracielaRange MetaExpr
metaexpr = makeExprParser term operator


term :: GracielaRange MetaExpr
term =  parens metaexpr
    <|> try call
    <|> variable
    <|> (get >>= bool)
    <|> basicLit integerLit       IntLit        GInt
    <|> basicLit floatLit         FloatLit      GFloat
    <|> basicLit charLit          CharLit       GChar
    <|> setLit   TokEmptySet      EmptySet      GSet
    <|> setLit   TokEmptyMultiset EmptyMultiset GMultiset
    <|> quantification
    <|> ifExp

  where
    bool :: Maybe Text -> GracielaRange MetaExpr
    bool Nothing = basicLit boolLit BoolLit GBool
    bool (Just _) = do
      from <- getPosition
      lit <- boolLit
      to <- getPosition

      let loc = Location (from, to)

      pure $ if lit
        then (,ProtoNothing) Expression
          { E.loc
          , expType = GBool
          , exp' = BoolLit lit }
        else (,ProtoQRange EmptyRange) Expression
          { E.loc
          , expType = GBool
          , exp' = ESkip }


    basicLit :: Graciela a -> (a -> Expression') -> Type
             -> GracielaRange MetaExpr
    basicLit litp expr t = do
      from <- getPosition
      lit <- lift litp
      to <- getPosition
      pure . (,ProtoNothing) $ Expression
        { E.loc   = Location (from, to)
        , expType = t
        , exp'    = expr lit }

    setLit :: Token -> Expression' -> (Type -> Type)
           -> GracielaRange MetaExpr
    setLit tok e t = do
      from <- getPosition
      match tok
      to <- getPosition
      pure . (,ProtoNothing) $ Expression
        { E.loc   = Location (from, to)
        , expType = t GAny
        , exp'    = e }


-- TODO: Check if function is defined
call :: GracielaRange MetaExpr
call = do
  from <- getPosition
  fname <- identifier
  args <- parens (expr `sepBy` match TokComma)
  to <- getPosition
  pure . (,ProtoNothing) $ Expression
    { E.loc = Location (from, to)
    , expType = GBool
    , exp' = FunctionCall
      { fname
      , args }}


variable :: GracielaRange MetaExpr
variable = do
  from <- getPosition
  name <- identifier
  to <- getPosition

  st <- lift (use symbolTable)

  let loc = Location (from, to)

  case name `lookup` st of

    Left _ -> pure . (,ProtoNothing) $ BadExpression
      { E.loc
      , reasons = Seq.singleton . CustomReason loc $
        "`" <> unpack name <> "` isn't defined in this scope"}

    Right entry -> case entry^.info of

      Var { _varType } -> do
        let expr = Expression
              { E.loc
              , expType = _varType
              , exp' = Obj
                { theObj = Object
                  { O.loc
                  , objType = _varType
                  , obj' = Variable
                    { O.name }}}}

        mrangevar <- get
        pure . (expr,) $ case mrangevar of
          Nothing       -> ProtoNothing
          Just rangevar -> if rangevar == name
            then ProtoVar
            else ProtoNothing

   -- Constant { _constType } ???

      _ -> pure . (,ProtoNothing) $ BadExpression
        { E.loc
        , reasons = Seq.singleton . CustomReason loc $
          unpack name <> "isn't defined in this scope as a variable" }


quantification :: GracielaRange MetaExpr
quantification = do
  let reasons = Seq.empty

  from <- getPosition
  match TokLeftPercent
  lift $ symbolTable %= openScope from

  (q, allowedBType) <- quantifier
  (var, t) <- declaration
  match TokPipe

  put (Just var)
  -- (cond @ Expression { E.loc = rloc }, protorange ) <- metaexpr
  (cond, protorange ) <- metaexpr
  let rloc = E.loc cond
  put Nothing

  case protorange of
    ProtoVar ->
      lift . syntaxError $ (`CustomError` rloc)
        "Bad quantification range. Range must be a boolean expression."
    ProtoNothing       ->
      lift . syntaxError $ (`CustomError` rloc)
        "Bad quantification range. Range must be a boolean expression in Conjunctive Normal Form."
    ProtoLow         _ ->
      lift . syntaxError $ (`CustomError` rloc)
        "Bad quantification range. No upper bound was given."
    ProtoHigh        _ ->
      lift . syntaxError $ (`CustomError` rloc)
        "Bad quantification range. No lower bound was given."
    ProtoQRange qrange ->
      pure ()

  match TokPipe

  body@Expression { expType = bType, E.loc = bloc } <- expr

  when (bType /= GError && not (bType =:= allowedBType)) $
    lift . syntaxError . (`CustomError` bloc) $
      "Bad quantification body. Body must be " <> show allowedBType <> "."

  match TokRightPercent
  to <- getPosition
  lift $ symbolTable %= closeScope to

  let loc = Location (from, to)
  pure . (,ProtoNothing) $ case protorange of
    ProtoQRange qrange -> if bType =:= allowedBType
      then Expression
        { E.loc
        , expType = GBool
        , exp' = Quantification
          { qOp      = q
          , qVar     = var
          , qVarType = t
          , qRange   = EmptyRange
          , qCond    = cond
          , qBody    = body }}
      else BadExpression
        { E.loc, reasons = Seq.empty }

    _ -> BadExpression
      { E.loc, reasons = Seq.empty }

  where
    numeric = GOneOf [GBool, GChar, GInt, GFloat]
    quantifier =  (match TokForall $> (ForAll,    GBool))
              <|> (match TokExist  $> (Exists,    GBool))
              <|> (match TokPi     $> (Product,   numeric))
              <|> (match TokSigma  $> (Summation, numeric))
              <|> (match TokMax    $> (Maximum,   numeric))
              <|> (match TokMin    $> (Minimum,   numeric))
              <|> (match TokCount  $> (Count,     GBool))

    declaration = do
      from <- getPosition

      var <- identifier
      match TokColon
      tname <- identifier

      to <- getPosition

      let loc = Location (from, to)

      typeEntry <- Map.lookup tname <$> lift (use typesTable)

      case typeEntry of
        Nothing    -> do
          lift . syntaxError $
            CustomError ("type `" <> unpack tname <> "` does not exist" ) loc
          pure (var, GError)

        Just (t,_) ->
          if t =:= quantifiableTypes
            then do
              lift $ symbolTable %= insertSymbol var Entry
                { _entryName = var
                , _loc = Location (from, to)
                , _info = Var
                  { _varType  = t
                  , _varValue = Nothing }}
              pure (var, t)
            else do
              lift . syntaxError $
                CustomError
                  ("type `" <> unpack tname <> "` is not quantifiable" ) loc
              pure (var, GError)


ifExp :: GracielaRange MetaExpr
ifExp = do
  from <- getPosition
  match TokIf

  -- The StateT on top of Graciela allows us to manage
  -- a state local to this level of abstraction.
  if' <- evalStateT guards (Just (GAny, Seq.empty))

  match TokFi
  if' from <$> getPosition

  where
    guards = do
      -- We run `line` for each "a -> b" pair in the If metaexpr,
      -- and then we extract the final set of guards
      line `sepBy1` lift (match TokSepGuards)
      st <- get

      -- If there were errors, we return a BadExpression, otherwise,
      -- it's the conditional we were looking for
      pure $ \from to -> (,ProtoNothing) $ case st of
        Nothing ->
          BadExpression
            { E.loc = Location (from, to), reasons = Seq.empty }
        Just (t, gs) ->
          Expression
            { E.loc = Location (from, to)
            , expType = t
            , exp' = EConditional
              { eguards = gs }}

    line = do
      st <- get
      -- We first save the state of the previous guards

      lhs <- lift expr
      -- We take the left hand side of the guard,
      -- and three things could have happened,

      case lhs of
        Expression { expType = GBool } ->
          -- 1. We have a good boolean expression, which is ideal
          pure ()

        Expression { E.loc } -> do
          -- 2. We have a good expression which isn't boolean, so we
          -- report the error and clear the previous guards
          put Nothing
          lift . lift . syntaxError $
            CustomError "bad left side in conditional expression" loc

        BadExpression {} ->
          -- 3. We have a bad expression, which means there was an error
          -- before which we can only propagate
          put Nothing

      lift . lift $ match TokArrow

      rhs <- lift expr
      -- We take the right hand side of the guard,
      -- and four things could have happened,

      case rhs of
        Expression { E.loc = rloc, expType } ->
          case st of
            Just (currT, currGs) ->
              -- There had been no errors before this guard and either
              if expType =:= currT
                then
                  -- 1. We have a good expression whose type matches the
                  -- type of previous guards. (In the case of the first guard,
                  -- the match is done against GAny, i.e., any type will
                  -- match). In this case, we add this guard to the list.
                  put $ Just (expType, currGs |> (lhs, rhs))

                else do
                  -- 2. We have a good expression whose type didn't match, so
                  -- we report the error and clear the previous guards.
                  put Nothing
                  lift . lift . syntaxError $
                    CustomError "bad right side in conditional expression" rloc

            Nothing ->
              -- 3. There had been an error before this guard, in which case
              -- we can't tell what the type is supposed to be, so we just
              -- propagate the error.
              pure ()

        BadExpression {} ->
          -- 4. We have a bad expression, which means there was an error
          -- deeper in the expression which we can only propagate
          put Nothing -- IfState { t = GError, gs = Seq.empty }


operator :: [[ Operator GracielaRange MetaExpr ]]
operator =
  [ {-Level 0-}
    -- [ Postfix (foldr1 (flip (.)) <$> some subindex) ]
    -- , Postfix (foldr1 (flip (.)) <$> some field) ]
  -- , {-Level 1-}
    -- [ Prefix  (foldr1 (.) <$> some pointer) ]
  -- , {-Level 2-}
  --   [ Prefix (match TokNot        $> \x -> Node "not" [x])
  --   , Prefix (match TokMinus      $> \x -> Node "minus" [x]) ]
  -- , {-Level 3-}
  --   [ InfixR (match TokPower      $> \x y -> Node "^" [x,y]) ]
  -- , {-Level 4-}
  --   [ InfixL (match TokTimes      $> \x y -> Node "*" [x,y])
  --   , InfixL (match TokDiv        $> \x y -> Node "/" [x,y])
  --   , InfixL (match TokMod        $> \x y -> Node "mod" [x,y]) ]
  -- , {-Level 5-}
  --   [ InfixL (match TokPlus       $> bchecked [iii,fff] Plus)
  --   , InfixL (match TokMinus      $> bchecked [iii,fff] BMinus) ]
  -- , {-Level 6-}
  --   [ InfixL (match TokMax        $> \x y -> Node "max" [x,y])
  --   , InfixL (match TokMin        $> \x y -> Node "min" [x,y]) ]
  -- , {-Level 7-}
  --   [ InfixN (match TokElem       $> \x y -> Node "elem" [x,y])
  --   , InfixN (match TokNotElem    $> \x y -> Node "notelem" [x,y])
  --   , InfixN (match TokLT         $> \x y -> Node "<" [x,y])
  --   , InfixN (match TokLE         $> \x y -> Node "<=" [x,y])
  --   , InfixN (match TokGT         $> \x y -> Node ">" [x,y])
  --   , InfixN (match TokGE         $> \x y -> Node ">=" [x,y]) ]
  -- , {-Level 8-}
  --   [ InfixN (match TokAEQ        $> \x y -> Node "==" [x,y])
  --   , InfixN (match TokANE        $> \x y -> Node "!=" [x,y]) ]
  -- , {-Level 9-}
  --   [ InfixR (match TokAnd        $> \x y -> Node "/\\" [x,y]) ]
  -- , {-Level 10-}
  --   [ InfixR (match TokOr         $> \x y -> Node "\\/" [x,y]) ]
  -- , {-Level 11-}
  --   [ InfixR (match TokImplies    $> \x y -> Node "==>" [x,y])
  --   , InfixL (match TokConsequent $> \x y -> Node "<==" [x,y]) ]
  -- , {-Level 12-}
  --   [ InfixN (match TokBEQ        $> \x y -> Node "===" [x,y])
  --   , InfixN (match TokBNE        $> \x y -> Node "!==" [x,y]) ]
  ]


-- subindex :: GracielaRange (MetaExpr -> MetaExpr)
-- subindex = do
--   from' <- getPosition
--   e @ Expression { expType } <- brackets expr
--   to <- getPosition
--
--   case expType of
--     GInt -> do
--       pure $ \(expr, _) -> case expr of
--         BadExpression { E.loc = Location (from, _) } ->
--           (,ProtoNothing) BadExpression
--             { E.loc = Location (from, to) }
--
--         Expression
--           { E.loc = Location (from, _)
--           , expType = GArray { arrayType }
--           , exp' = Obj o } ->
--             (,ProtoNothing) Expression
--               { E.loc = Location (from, to)
--               , expType = arrayType
--               , exp' = Obj
--                 { theObj = Object
--                   { O.loc = Location (from, to)
--                   , objType = arrayType
--                   , obj' = Index
--                     { O.inner = o
--                     , index = e }}}}
--
--         e ->
--           let Location (from, _) = E.loc e
--           in (,ProtoNothing) BadExpression { E.loc = Location (from, to) }
--
--     _ -> do
--       let subloc = Location (from', to)
--       lift . syntaxError $
--         CustomError "Bad subindex. Must be integer expression." subloc
--       pure $ \(e,_) ->
--         let Location (from, _) = E.loc e
--         in (,ProtoNothing) BadExpression { E.loc = Location (from, to) }





-- field :: Graciela (Object -> Object)
-- field = do
--   match TokDot
--   fieldName <- identifier
--   to <- getPosition
--   pure $ \o @ Object { O.loc = Location (from, _) } ->
--     Object
--       { O.loc = Location (from, to)
--       , objType = GInt
--       , obj' = Member o fieldName
--       }
--
-- pointer :: Graciela (Object -> Object)
-- pointer = do
--   from <- getPosition
--   match TokTimes $> \o @ Object { O.loc = Location (_, to) } ->
--     Object
--       { O.loc = Location (from, to)
--       , objType = GInt
--       , obj' = Deref o
--       }






ii :: (Type, Type)
ii =  (GInt, GInt)
ff :: (Type, Type)
ff =  (GFloat, GFloat)
bb :: (Type, Type)
bb =  (GBool, GBool)

iii :: (Type, Type, Type)
iii =  (GInt, GInt, GInt)
fff :: (Type, Type, Type)
fff =  (GFloat, GFloat, GFloat)
bbb :: (Type, Type, Type)
bbb =  (GBool, GBool, GBool)
iib :: (Type, Type, Type)
iib =  (GInt, GInt, GBool)
ffb :: (Type, Type, Type)
ffb =  (GFloat, GFloat, GBool)


-- bchecked :: [(Type, Type, Type)]
--          -> BinaryOperator
--          -> Expression -> Expression -> Expression
-- bchecked ts binOp
--   lexpr @ Expression { expType = ltype }
--   rexpr @ Expression { expType = rtype }
--   = foldr aux bad ts
--       where
--         aux (left, right, ret) BadExpression {}
--           | ltype == left && rtype == right =
--             Expression
--               { E.loc = location
--               , expType = ret
--               , exp' = Binary
--                 { binOp
--                 , lexpr
--                 , rexpr
--                 }
--               }
--           | otherwise = bad
--         aux _ goodExpr = goodExpr
--         location = Location (from lexpr, to rexpr)
--         bad = BadExpression { E.loc = location }
-- bchecked _ _ l r =
--   BadExpression { E.loc = Location (from l, to r) }





concatLexPar :: Text -> IO ()
concatLexPar input = do
  let Right ets = runParser lexer "" input
  let init' = initialState &~ do
        symbolTable %= openScope (SourcePos "" (unsafePos 4) (unsafePos 10))
        symbolTable %= insertSymbol (pack "a") Entry
          { _entryName  = pack "a"
          , _loc        = Location (SourcePos "" (unsafePos 2) (unsafePos 2), SourcePos "" (unsafePos 2) (unsafePos 20))
          , _info       = Var
            { _varType  = GArray (Right 10) GInt
            , _varValue = Nothing }}
  let Right r   = evalState (runParserT expression "" ets) init'
  putStrLn . drawTree . toTree $ r
