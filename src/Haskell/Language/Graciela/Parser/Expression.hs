{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeFamilies             #-}

module Language.Graciela.Parser.Expression
  ( expression
  ) where
--------------------------------------------------------------------------------
import {-# SOURCE #-} Language.Graciela.Parser.Type (type')
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Definition
import           Language.Graciela.AST.Expression  hiding (inner, loc)
import qualified Language.Graciela.AST.Expression  as E (inner, loc, BinaryOperator(Plus, BMinus))
import           Language.Graciela.AST.Object      hiding (inner, loc, name)
import qualified Language.Graciela.AST.Object      as O (inner, loc, name)
import           Language.Graciela.AST.Struct      (Struct (..), Struct' (..),
                                                    fillTypes)
import           Language.Graciela.AST.Type        (ArgMode (..), Type (..),
                                                    fillType, hasDT, highLevel,
                                                    removeAbst, (=:=))
import           Language.Graciela.Common
import           Language.Graciela.Entry           (Entry (..), Entry' (..),
                                                    info)
import           Language.Graciela.Error           (Error (..))
import           Language.Graciela.Lexer
import           Language.Graciela.Parser.Config
import           Language.Graciela.Parser.ExprM    (Operator (..),
                                                    makeExprParser)
import           Language.Graciela.Parser.Monad
import qualified Language.Graciela.Parser.Operator as Op
import           Language.Graciela.Parser.State    hiding (State)
import           Language.Graciela.SymbolTable     (closeScope, defocus,
                                                    emptyGlobal, insertSymbol,
                                                    lookup, openScope)
import           Language.Graciela.Token
--------------------------------------------------------------------------------
import           Control.Lens                      (elements, makeLenses, use,
                                                    view, (%%=), (%=), (%~),
                                                    (&), (&~), (.=), (<&>),
                                                    (^.), _1, _3, _Just)
import           Control.Monad                     (foldM, unless, void, when,
                                                    (>=>))
import           Control.Monad.Reader              (asks)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.State         (StateT, evalStateT,
                                                    execStateT, get, gets,
                                                    modify, put)
import qualified Data.Array                        as Array (listArray)
import           Data.Foldable                     (foldl')
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map (empty, insert, keys,
                                                           lookup, size)
import           Data.Maybe                        (catMaybes, fromJust, isJust)
import           Data.Monoid                       (First (..))
import           Data.Semigroup                    (Semigroup (..))
import           Data.Sequence                     (Seq, (|>))
import qualified Data.Sequence                     as Seq (empty, fromList,
                                                           singleton, zip)
import           Data.Set                          as Set (member)
import           Data.Text                         (Text, pack, unpack)
import           Prelude                           hiding (Ordering (..), lex,
                                                    lookup)
import           Text.Megaparsec                   (between, getPosition,
                                                    lookAhead, manyTill,
                                                    optional, parseErrorPretty,
                                                    try, (<|>))
--------------------------------------------------------------------------------

data ProtoRange
  = ProtoVar                  -- ^ The associated expression is the
                              -- variable of interest
  | ProtoQRange  QRange       -- ^ A valid QRange has been formed
  | ProtoLow     Expression   -- ^ A lower bound has been found
  | ProtoHigh    Expression   -- ^ An upper bound has been found
  | ProtoNothing              -- ^ No manner of range has been formed
  deriving (Eq, Show)

newtype Taint = Taint Bool -- A MetaExpr is `tainted` when it
                           -- contains a quantified (dummy)
                           -- variable. This means it cannot
                           -- be used as a range limit.
  deriving (Eq, Ord, Show)

instance Semigroup Taint where
  Taint x <> Taint y = Taint (x || y)

instance Monoid Taint where
  mempty = Taint False
  mappend = (<>)

type MetaExpr = (Expression, ProtoRange, Taint)

type ParserExp = StateT [Text] Parser

expr :: ParserExp (Maybe Expression)
expr = pure . (view _1 <$>) =<< filterAny =<< filterRawName =<< metaexpr
  where
    filterAny :: Maybe MetaExpr -> ParserExp (Maybe MetaExpr)
    filterAny Nothing = pure Nothing
    filterAny je@(Just (e, _, _)) = if expType e == GAny
      then do
        putError (pos . E.loc $ e) . UnknownError $
          "Expression lacks a concrete type."
        pure Nothing
      else pure je

metaexpr :: ParserExp (Maybe MetaExpr)
metaexpr = makeExprParser term operator

expression :: Parser (Maybe Expression)
expression = evalStateT expr []

term :: ParserExp (Maybe MetaExpr)
term =  tuple
    <|> sizeof
    <|> variable
    <|> bool
    <|> nullptr
    <|> basicLit integerLit IntV   GInt
    <|> basicLit floatLit   FloatV GFloat
    <|> basicLit charLit    CharV  GChar
    <|> emptySet
    <|> collection
    <|> string
    <|> quantification
    <|> ifExp
    <|> do 
          p <- getPosition
          match TokApostrophe
          putError p . UnknownError $ 
            "Found unexpected: token " <> show TokApostrophe
          term
    <|> do 
          p <- getPosition
          match TokQuotation
          putError p . UnknownError $ 
            "Found unexpected: token " <> show TokQuotation
          term

  where
    bool :: ParserExp (Maybe MetaExpr)
    bool = do
      from <- getPosition
      lit <- boolLit
      to <- getPosition

      let
        expr = Expression
          { E.loc    = Location (from, to)
          , expType  = GBool
          , expConst = True
          , exp'     = Value . BoolV $ lit }
        range = if lit
          then ProtoNothing
          else ProtoQRange EmptyRange

      pure $ Just (expr, range, Taint False)

    nullptr :: ParserExp (Maybe MetaExpr)
    nullptr = do
      from <- getPosition
      match TokNull
      to <- getPosition
      let
        expr = Expression
          { E.loc    = Location (from, to)
          , expType  = GPointer GAny
          , expConst = False
          , exp'     = NullPtr }

      pure $ Just (expr, ProtoNothing, Taint False)

    sizeof :: ParserExp (Maybe MetaExpr)
    sizeof = do
      from <- getPosition
      fun <- lookAhead identifier

      pragmas' <- lift $ use pragmas
      let
        pragOk = fun == "sizeof" && Set.member MemoryOperations pragmas'
      unless pragOk $ do
        void . lookAhead . match $ TokLeftPar
      identifier
      t  <- parens $ lift type'
      to <- getPosition

      let
        expr = Expression
          { E.loc    = Location (from, to)
          , expType  = GInt
          , expConst = False
          , exp'     = SizeOf { sType = t } }

      pure $ Just (expr, ProtoNothing, Taint False)

    basicLit :: Parser a -> (a -> Value) -> Type
             -> ParserExp (Maybe MetaExpr)
    basicLit litp val t = do
      from <- getPosition
      lit <- lift litp
      to <- getPosition

      let
        expr = Expression
          { E.loc    = Location (from, to)
          , expType  = t
          , expConst = True
          , exp'     = Value . val $ lit }

      pure $ Just (expr, ProtoNothing, Taint False)

    emptySet :: ParserExp (Maybe MetaExpr)
    emptySet = do
      loc <- match TokEmptySet
      let
        expr = Expression
          { E.loc
          , expType  = GSet GAny
          , expConst = True
          , exp'     = Collection Set Nothing Seq.empty }
      pure $ Just (expr, ProtoNothing, Taint False)

    string = do
      from <- getPosition
      text <- stringLit
      to   <- getPosition

    -- strId <- lift $ use stringIds >>= \m -> case text `Map.lookup` m of
    --     Just i  -> pure i
    --     Nothing -> use stringCounter >>= \i -> do
    --       stringCounter .= i+1
    --       stringIds %= Map.insert text i
    --       pure i
      strId <- lift $ stringIds %%= \m -> case text `Map.lookup` m of
        Just i  -> (i, m)
        Nothing -> let i = Map.size m in (i, Map.insert text i m)

      let
        expr = Expression
          { E.loc    = Location (from, to)
          , expType  = GString
          , expConst = True
          , exp'     = StringLit strId }

      pure $ Just (expr, ProtoNothing, Taint False)


tuple :: ParserExp (Maybe MetaExpr)
tuple = do
  Location (from, _) <- match TokLeftPar
  elems <- sequence <$>
    ((metaexpr >>= filterRawName) `followedBy` oneOf ([TokComma, TokRightPar] :: Seq Token)) `sepBy` match TokComma
  Location (_, to) <- match TokRightPar

  case elems of
    Nothing -> pure Nothing
    Just [] -> do
      putError from . UnknownError $
        "Expected expression."
      pure Nothing
    Just [(e0, r0, t0)] -> pure $
      Just (e0 { E.loc = Location (from, to)}, r0, t0)
    Just [(e0, _, t0), (e1, _, t1)]
      | allowed e0 && allowed e1 ->
        let expr = Expression
              { E.loc    = Location (from, to)
              , expType  = GTuple (expType e0) (expType e1)
              , expConst = expConst e0 && expConst e1
              , exp'     = Tuple e0 e1 }
        in pure $ Just (expr , ProtoNothing, t0 <> t1)
      | otherwise -> do
        unless (allowed e0) . putError (pos . E.loc $ e0) . UnknownError $
          if expType e0 =:= GTuple GAny GAny
            then "Cannot build tuple of tuples."
            else "Cannot build tuple of " <> show (expType e0)

        unless (allowed e1) . putError (pos . E.loc $ e1) . UnknownError $
          if expType e1 =:= GTuple GAny GAny
            then "Cannot build tuple of tuples."
            else "Cannot build tuple of " <> show (expType e1)

        pure Nothing

    Just exps -> do
      putError from . UnknownError $
        show (length exps) <> "-Tuples are not supported."
      pure Nothing

    where
      allowed = allowed' . expType
      allowed' = (=:= GOneOf [GBool, GChar, GInt, GFloat, GATypeVar, GPointer GAny])


collection :: ParserExp (Maybe MetaExpr)
collection = do
  (colKind, Location (from,_)) <- (Set,)      <$> match TokLeftBrace
                              <|> (Multiset,) <$> match TokLeftBag
                              <|> (Sequence,) <$> match TokLeftSeq

  mvrc <- optional (varAndRange from)
  melems <- elems $ case colKind of
    Set      -> TokRightBrace
    Multiset -> TokRightBag
    Sequence -> TokRightSeq

  Location (_, to) <- match' $ case colKind of
    Set      -> TokRightBrace
    Multiset -> TokRightBag
    Sequence -> TokRightSeq

  case mvrc of
    Just _ -> lift $ symbolTable %= closeScope to
    _      -> pure ()

  case mvrc of
    -- A variable was given but an error occurred, either syntax or semantic
    Just Nothing -> pure Nothing

    -- A variable was not given, only (maybe) elems
    Nothing -> case melems of
      -- There were errors in the elems
      Nothing -> pure Nothing
      -- All elems are ok
      Just (els, t) -> lift (use isDeclarative) >>= \case
        True ->
          let
            e = Expression
              { E.loc   = Location (from, to)
              , expType = (case colKind of
                Set      -> GSet
                Multiset -> GMultiset
                Sequence -> GSeq
                ) t
              , expConst = False -- FIXME
              , exp' = Collection
                { colKind
                , colVar   = Nothing
                , colElems = els }}
          in pure $ Just (e, ProtoNothing, Taint False)
        False -> do
          putError from . UnknownError $
            "Set comprehension not allowed in imperative code."
          pure Nothing

    -- A variable was given without errors
    Just (Just (var, varTy, range, cond)) -> case melems of
      -- But there were errors in the elems
      Nothing -> pure Nothing
      -- And the elems are okay
      Just (els, t) -> if null els
        -- The element list is empty
        then do
          putError from . UnknownError $
            "Set comprehension without elements."
          pure Nothing

        -- The element list is not empty
        else lift (use isDeclarative) >>= \case
          True ->
            let
              e = Expression
                { E.loc   = Location (from, to)
                , expType = (case colKind of
                  Set      -> GSet
                  Multiset -> GMultiset
                  Sequence -> GSeq
                  ) t
                , expConst = False -- FIXME
                , exp' = Collection
                  { colKind
                  , colVar   = Just (var, varTy, range, cond)
                  , colElems = els }}
            in pure $ Just (e, ProtoNothing, Taint False)
          False -> do
            putError from . UnknownError $
              "Set comprehension not allowed in imperative code."
            pure Nothing

  where
    varAndRange :: SourcePos -> ParserExp (Maybe (Text, Type, QRange, Expression))
    varAndRange from = do
      void . lookAhead . try $ identifier *> match TokColon

      lift $ symbolTable %= openScope from
      (var, varTy) <- declaration

      void $ match' TokPipe

      range <- case varTy of
        GUndef -> do
          anyToken `manyTill` match' TokPipe
          pure Nothing
        _ -> do
          modify (var :)
          range <- metaexpr
          void $ match' TokPipe
          modify tail
          pure range

      case range of
        Nothing -> pure Nothing
        Just (cond, protorange, taint0) -> do
          let
            Location (rfrom, _) = E.loc cond

          case protorange of
            ProtoVar -> do
              putError rfrom . UnknownError $
                "Bad collection range. Range must be a boolean expression \
                \in Conjunctive Normal Form where the variable `" <> unpack var <>
                "` is bounded."
              pure Nothing
            ProtoNothing -> do
              putError rfrom . UnknownError $ case varTy of
                GFloat ->
                  "Bad collection range. Can only build ranges over integral types."
                _ ->
                  "Bad collection range. Range must be a boolean expression \
                  \in Conjunctive Normal Form where the variable `" <>
                  unpack var <> "` is bounded."
              pure Nothing
            ProtoLow l -> do
              putError rfrom . UnknownError $
                "Bad collection range. No upper bound was given." <> drawTree (toTree l)
              pure Nothing
            ProtoHigh _ -> do
              putError rfrom . UnknownError $
                "Bad collection range. No lower bound was given."
              pure Nothing
            ProtoQRange qrange ->
              pure $ Just (var, varTy, qrange, cond)

    quantifiableTypes, allowedCollTypes :: Type
    quantifiableTypes = GOneOf [ GInt, GChar, GBool, GFloat, GATypeVar ]
    allowedCollTypes  = GOneOf
      [ GInt
      , GChar
      , GBool
      , GFloat
      , GATypeVar
      , GPointer GAny
      , GTuple
        (GOneOf [GInt, GChar, GBool, GFloat,GPointer GAny, GATypeVar])
        (GOneOf [GInt, GChar, GBool, GFloat,GPointer GAny, GATypeVar]) ]

    declaration = do
      from <- getPosition

      var <- identifier
      void $ match TokColon
      t <- lift type'

      to <- getPosition
      let loc = Location (from, to)

      if t =:= quantifiableTypes
        then do
          lift $ symbolTable %= insertSymbol var Entry
            { _entryName = var
            , _loc = loc
            , _info = Var
              { _varType  = t
              , _varValue = Nothing
              , _varConst = True }}
          pure (var, t)
        else do
          putError from . UnknownError $
            "type " <> show t <> " is not quantifiable."
          pure (var, GUndef)

    elems :: Token -> ParserExp (Maybe (Seq Expression, Type))
    elems close =  lookAhead (match close) *> pure (Just (Seq.empty, GAny))
               <|> (elem' close `sepBy1'` match TokComma) (Just (Seq.empty, allowedCollTypes))

    elem' :: Token
          -> Maybe (Seq Expression, Type)
          -> ParserExp (Maybe (Seq Expression, Type))
    elem' close acc = do
      pos <- getPosition
      el <- metaexpr `followedBy` oneOf ([close, TokComma] :: [Token])
      case el of
        Nothing ->
          pure Nothing
        Just (e, _, _) -> case acc of
          Nothing -> pure Nothing
          Just (els, t) -> case t <> expType e of
            GUndef -> do
                putError pos . UnknownError $
                  "Unexpected expression of type " <> show (expType e) <> ",\n\t\
                  \expected instead an expression with one of following type:" <>
                  "\n\t\t * " <> show  GInt   <>
                  "\n\t\t * " <> show  GBool  <>
                  "\n\t\t * " <> show  GChar  <>
                  "\n\t\t * " <> show  GFloat <>
                  "\n\t\t * " <> show (GPointer GAny) <>
                  "\n\t\t * " <> show  GATypeVar <>
                  "\n\t\t * " <> show (GTuple GAny GAny)
                pure Nothing
            newType -> pure $ Just (els |> e, newType)
      

variable :: ParserExp (Maybe MetaExpr)
variable = do
  (name, loc) <- identifierAndLoc
             <|> ((pack "func",) <$> match TokFunc)

  st <- lift (use symbolTable)

  maybeStruct  <- lift $ use currentStruct
  abstNamesOk <- lift $ use allowAbstNames
  coupRel     <- lift $ use doingCoupleRel

  (dt,abstractSt) <- case maybeStruct of
    Just (dt@(GDataType _ (Just abstName) _), _, _, _, _) -> do
      adt <- getStruct abstName
      pure $ (dt,) $ case adt of
        Just abst -> structSt abst
        _         -> emptyGlobal
    _ -> pure (GUndef, emptyGlobal)
  let
    entry = case name `lookup` st of
      Left _ | not coupRel -> name `lookup` abstractSt
      x      -> x

  case entry of
    Left _ ->
      let
        expr = Expression
          { E.loc
          , expType = GRawName
          , expConst = False
          , exp' = RawName name }
      in pure $ Just (expr, ProtoNothing, Taint False)

    Right entry -> case entry^.info of
      Alias { _aliasType, _aliasValue } ->
        let
          expr = Expression
            { E.loc
            , expType  = removeAbst dt _aliasType
            , expConst = True
            , exp' = Value _aliasValue }

        in pure $ Just (expr, ProtoNothing, Taint False)
      Enum { _enumType, _enumValue } -> do
        rangevars <- get
        let taint = case rangevars of
                [] -> Taint False
                _  -> if name == head rangevars
                  then Taint True
                  else Taint False
        pure $ Just (_enumValue, ProtoNothing, taint)
      Var { _varType, _varConst } -> lift (use isDeclarative) >>= \declarative ->
        if _varType =:= highLevel && not declarative
          then do
            let Location (pos, _) = loc
            putError pos . UnknownError $
              "Variable `" <> unpack name <> "` of type " <> show _varType <>
              " not allowed in imperative code."
            pure Nothing

        else do
          rangevars <- get

          let expr = Expression
                { E.loc
                , expType = removeAbst dt _varType
                , expConst = _varConst || name `elem` rangevars
                , exp' = Obj
                  { theObj = Object
                    { O.loc
                    , objType = _varType
                    , obj' = Variable
                      { O.name = name
                      , mode = Nothing }}}}

          let protorange = case rangevars of
                [] -> ProtoNothing
                _  -> if name == head rangevars
                  then ProtoVar
                  else ProtoNothing

          let taint = case rangevars of
                [] -> Taint False
                _  -> if name == head rangevars
                  then Taint True
                  else Taint False

          pure $ Just (expr, protorange, taint)

      SelfVar {} | not abstNamesOk -> do
        let Location (pos, _) = loc
        putError pos . UnknownError $
          "Variable `" <> unpack name <> "` not defined in this scope."
        pure Nothing
      SelfVar { _selfType, _selfConst } -> do
        struct <- lift $ use currentStruct
        rangevars <- get

        lift (use isDeclarative) >>= \x -> if _selfType =:= highLevel && not x
          then do
            let Location (pos, _) = loc
            putError pos . UnknownError $
              "Variable `" <> unpack name <> "` of type " <> show _selfType <>
              " not allowed in imperative code."
            pure Nothing
          else
            let
              
              expr = case struct of
                Just (GDataType structName' abstract t, mapTypes, _, _, targs) ->
                  let tt = fillType targs (removeAbst dt _selfType)
                  in case name `Map.lookup` mapTypes of
                      Just (i, _, _, _) -> Expression
                        { loc
                        , expType  = tt
                        , expConst = _selfConst
                        , exp'     = Obj
                          { theObj = Object
                            { loc
                            , objType = tt
                            , obj' = Member
                              { field = i
                              , fieldName = name
                              , inner = Object
                                { loc
                                , objType = GDataType structName' abstract t
                                , obj' = Variable
                                  { O.name = pack "__self"
                                  , mode = Nothing }}}}}}

                      Nothing -> internal $ "Data Type variable `" <>
                                  unpack name <>"` not found"

                Nothing -> internal "Data Type not found"


              protorange = case rangevars of
                  [] -> ProtoNothing
                  _  -> if name == head rangevars
                    then ProtoVar
                    else ProtoNothing

              taint = case rangevars of
                  [] -> Taint False
                  -- _  -> if name `elem` rangevars
                  _  -> if name == head rangevars
                    then Taint True
                    else Taint False

            in pure $ Just (expr, protorange, taint)

      Argument { _argMode, _argType } -> lift (use isDeclarative) >>=
        \x -> if _argType =:= highLevel && not x
          then do
            let Location (pos, _) = loc
            putError pos . UnknownError $
              "Variable `" <> unpack name <> "` of type " <> show _argType <>
              " not allowed in imperative code."
            pure Nothing
          else
            let
              expr = Expression
                { E.loc
                , expType  = removeAbst dt _argType
                , expConst = _argMode == Const
                , exp'     = Obj
                  { theObj = Object
                    { O.loc
                    , objType = _argType
                    , obj'    = Variable
                      { O.name = name
                      , mode   = Just _argMode }}}}
            in pure $ Just (expr, ProtoNothing, Taint False)


filterRawName :: Maybe MetaExpr -> ParserExp (Maybe MetaExpr)
filterRawName Nothing = pure Nothing
filterRawName je@(Just (e, _, _)) = if expType e == GRawName
  then do
    let RawName name = exp' e
    putError (pos . E.loc $ e) . UnknownError $
      "Variable `" <> unpack name <> "` not defined in this scope."
    pure Nothing
  else pure je

quantification :: ParserExp (Maybe MetaExpr)
quantification = do
  from <- getPosition
  void $ match TokLeftPercent
  lift $ symbolTable %= openScope from

  (q, allowedBType) <- quantifier
  (var, vart) <- declaration
  void $ match TokPipe

  modify (var:)
  range <- {-safe-} metaexpr
  void $ match TokPipe
  modify tail

  case range of
    Nothing -> pure ()
    Just (cond, protorange, taint0) -> do
      let
        Location (rfrom, _) = E.loc cond

      case protorange of
        ProtoVar ->
          putError rfrom . UnknownError $
            "Bad quantification range. Range must be a boolean expression \
            \in Conjunctive Normal Form where the variable `" <> unpack var <>
            "` is bounded."
        ProtoNothing       ->
          putError rfrom . UnknownError $
            "Bad quantification range. Range must be a boolean expression \
            \in Conjunctive Normal Form where the variable `" <> unpack var <>
            "` is bounded."
        ProtoLow         _ ->
          putError rfrom . UnknownError $
            "Bad quantification range. No upper bound was given."
        ProtoHigh        _ ->
          putError rfrom . UnknownError $
            "Bad quantification range. No lower bound was given."
        ProtoQRange qrange ->
          pure ()

  body <- {-safe-} metaexpr

  case body of
    Nothing -> pure ()
    Just (Expression { expType = bodyType, E.loc = Location (bfrom, _) }, _, taint1) ->
      unless (bodyType =:= allowedBType) .
        putError bfrom . UnknownError $
          "Bad quantification body. Body must be " <> show allowedBType <> "."

  void $ match' TokRightPercent

  to <- getPosition
  lift $ symbolTable %= closeScope to

  case vart of
    GUndef -> pure Nothing
    t -> case body of
      Nothing -> pure Nothing
      Just (theBody @ Expression { expType = bodyType, expConst = bodyConst }, _, taint1) ->
        case range of
          Nothing -> pure Nothing
          Just (cond@Expression {expConst = condConst}, protorange, taint0) ->
            case protorange of
              ProtoQRange qRange -> case bodyType <> allowedBType of
                GUndef  -> pure Nothing
                newType -> lift (use isDeclarative) >>= \case
                  True ->
                    let
                      loc = Location (from, to)
                      taint = taint0 <> taint1
                      expr = Expression
                        { E.loc
                        , expType = case q of
                          Count -> GInt
                          _     -> newType
                        , expConst = condConst && bodyConst
                        , exp' = Quantification
                          { qOp      = q
                          , qVar     = var
                          , qVarType = t
                          , qRange
                          , qCond    = cond
                          , qBody    = theBody }}
                    in pure $ Just (expr, ProtoNothing, taint)
                  False -> do
                    putError from . UnknownError $
                      "Quantification not allowed in imperative code."
                    pure Nothing
              _ -> pure Nothing

  where
    numeric = GOneOf [ GChar, GInt, GFloat ]

    quantifier =  (match TokForall                    $> (ForAll,    GBool))
              <|> (match TokExist                     $> (Exists,    GBool))
              <|> (match TokProduct                   $> (Product,   numeric))
              <|> (match TokSum                       $> (Summation, numeric))
              <|> (match TokMax                       $> (Maximum,   numeric))
              <|> (match TokMin                       $> (Minimum,   numeric))
              <|> ((match TokCount <|> match TokHash) $> (Count,     GBool))

    quantifiableTypes = GOneOf [ GInt, GChar, GBool, GFloat, GATypeVar ]

    declaration = do
      from <- getPosition

      var <- identifier
      void $ match TokColon
      t <- lift type'

      to <- getPosition
      let loc = Location (from, to)

      if t =:= quantifiableTypes
        then do
          lift $ symbolTable %= insertSymbol var Entry
            { _entryName = var
            , _loc = loc
            , _info = Var
              { _varType  = t
              , _varValue = Nothing
              , _varConst = True }}
          pure (var, t)
        else do
          putError from . UnknownError $
            "type " <> show t <> " is not quantifiable."
          pure (var, GUndef)

data IfBuilder
  = IfGuards     (Seq (Expression, Expression)) (Maybe Expression)
  | IfExp        Expression
  | IfNothing
  deriving (Show)

instance Semigroup IfBuilder where
  IfNothing <> _ = IfNothing
  _ <> IfNothing = IfNothing

  IfExp e <> _ = IfExp e

  IfGuards gs0 Nothing <> IfGuards gs1 t =
    IfGuards (gs0 <> gs1) t
  IfGuards gs Nothing  <> IfExp e =
    IfGuards gs (Just e)

  igt@(IfGuards _ (Just _)) <> _ = igt

instance Monoid IfBuilder where
  mempty = IfGuards Seq.empty Nothing
  mappend = (<>)

data IfState = IfState
  { ifType    :: Type
  , ifBuilder :: IfBuilder
  , ifTaint   :: Taint
  , ifConst   :: Bool }

initialIfState = IfState
  { ifType    = GAny
  , ifBuilder = IfGuards Seq.empty Nothing
  , ifTaint   = Taint False
  , ifConst   = False }


ifExp :: ParserExp (Maybe MetaExpr)
ifExp = do
  from <- getPosition
  void $ match TokIf

  -- The StateT on top of ParserExp allows us to manage
  -- a state local to this level of abstraction.
  IfState { ifType, ifBuilder, ifTaint, ifConst } <- execStateT guards initialIfState

  match' TokFi

  to <- getPosition

  let loc = Location (from, to)

  case ifBuilder of
    IfNothing -> pure Nothing
    IfExp e ->
      pure $ Just (e { E.loc, expType = ifType }, ProtoNothing, ifTaint)
    IfGuards gs t ->
      let
        expr = Expression
          { E.loc
          , expType  = ifType
          , expConst = ifConst
          , exp' = EConditional gs t }
      in pure $ Just (expr, ProtoNothing, ifTaint)

  where
    guards =
      -- We run `line` for each "a -> b" pair in the If metaexpr,
      -- and then we extract the final set of guards
      line `sepBy1` match TokSepGuards

    line = (get >>= lhs) <* match TokArrow >>= rhs

    lhs st = do
      left <- lift metaexpr
      -- We take the left hand side of the guard,
      -- and three things could have happened,

      case left of
        Nothing -> pure (Nothing, st, Taint False)
        Just (l, _, taint0) ->
          case l of
            e @ Expression { expType = GBool, E.loc } ->
            -- 1. We have a good boolean expression, which is ideal
              pure (Just e, st, taint0)

            Expression { E.loc = Location (from, _) } -> do
              -- 2. We have a good expression which isn't boolean, so we
              -- report the error and clear the previous guards
              lift . putError from . UnknownError $
                "bad left side in conditional expression"
              pure (Nothing, st, taint0)

        -- badEXPRESSION { E.loc } ->
        -- -- 3. We have a bad expression, which means there was an error
        -- -- before which we can only propagate
        --   pure (Nothing, st, taint0)


    rhs (ml, st@IfState { ifType, ifBuilder, ifTaint, ifConst }, taint0) = do
      right <- lift metaexpr
      -- We take the right hand side of the guard,
      -- and 7 things could have happened,

      case ifBuilder of
        IfNothing ->
          -- 1. An error occurred in a previous line, there's nothing to do
          -- about this one.
          pure ()

        _ ->
          -- No errors have occured in previous lines, and
          case right of
            Nothing -> put st { ifType = GUndef, ifBuilder = IfNothing }

            Just (r, _, taint1) ->
              case r of
                -- badexpression {} ->
                --   -- 1. The rhs is a badexpression, which means there was
                --   -- an error deep in it which we can only propagate
                --   put st { ifType = GUndef, ifBuilder = IfNothing }

                e@Expression { E.loc = Location (rfrom,_), expType } ->
                  case expType <> ifType of
                    GUndef -> do
                      -- 2. The rhs type doesn't match previous lines
                      lift . putError rfrom . UnknownError $
                        "bad right side in conditional expression \n" <> (drawTree . toTree $ e)
                      put st { ifType = GUndef, ifBuilder = IfNothing }

                    newType ->
                      -- The rhs is perfect in syntax and type
                      case ml of
                        Nothing ->
                          -- 3. But the lhs was bad, so we clear everything
                          put st { ifType = GUndef, ifBuilder = IfNothing }

                        Just l
                          | exp' l == Value (BoolV True) ->
                            -- 4. The lhs has the true value, so we only keep
                            -- the final value of the ifExp is this rhs.

                            put st
                              { ifType = newType
                              , ifBuilder = ifBuilder <> IfExp r
                              , ifTaint = taint1
                              , ifConst = expConst r }

                          | exp' l == Value (BoolV False) ->
                            -- 5. We have a good rhs that must be ignored because
                            -- its lhs was false. Its type does affect the ifExp.
                            put st { ifType = newType }

                          | otherwise ->
                            -- 6. We have a good rhs whose type matches the
                            -- type of previous guards, while the lhs value will be
                            -- known only at runtime, so we leave this guard
                            -- expressed as a tuple. (In the case of the first
                            -- guard, the match is done against GAny, i.e., any
                            -- type will match).
                            put IfState
                              { ifType    = newType
                              , ifBuilder =
                                ifBuilder <> IfGuards (Seq.singleton (l, r)) Nothing
                              , ifTaint   = ifTaint <> taint0 <> taint1
                              , ifConst   = ifConst && expConst l && expConst r }

operator :: [[ Operator ParserExp (Maybe MetaExpr) ]]
operator =
  [ {-Level 0-}
    [ Postfix (foldr1 (>=>) <$> some dotField) ]
  , {-Level 1-}
    [ Postfix (foldr1 (>=>) <$> some call)
    , Postfix (foldr1 (>=>) <$> some subindex) ]
  , {-Level 2-}
    [ Prefix  (foldr1 (>=>) <$> some deref)
    , Prefix  (foldr1 (>=>) <$> some refof)
    , Prefix  (foldr1 (>=>) <$> some unsafeCast) ]
  , {-Level 3-}
    [ Prefix  (foldr1 (>=>) <$> some (TokHash  --> unary Op.card)) ]
  , {-Level 4-}
    [ Prefix  (foldr1 (>=>) <$> some (TokNot   --> unary Op.not   ))
    , Prefix  (foldr1 (>=>) <$> some (TokMinus --> unary Op.uMinus)) ]
  , {-Level 5-}
    [ InfixR (TokPower        ==> binary Op.power) ]
  , {-Level 6-}
    [ InfixL (TokTimes        ==> binary Op.times)
    , InfixL (TokDiv          ==> binary Op.div )
    , InfixL (TokMod          ==> binary Op.mod ) ]
  , {-Level 7-}
    [ InfixL (TokPlus         ==> binary Op.plus      )
    , InfixL (TokMinus        ==> binary Op.bMinus    )
    , InfixL (TokSetUnion     ==> binary Op.union     )
    , InfixL (TokMultisetSum  ==> binary Op.multisum  )
    , InfixL (TokSetIntersect ==> binary Op.intersect )
    , InfixL (TokSetMinus     ==> binary Op.difference)
    , InfixL (TokConcat       ==> binary Op.concat    ) ]
  , {-Level 8-}
    [ InfixL (TokMax          ==> binary Op.max)
    , InfixL (TokMin          ==> binary Op.min) ]
  , {-Level 9-}
    [ InfixN (TokElem         ==> membership             )
    , InfixN (TokNotElem      ==> binary Op.notElem      )
    , InfixN (TokLT           ==> comparison Op.lt       )
    , InfixN (TokLE           ==> comparison Op.le       )
    , InfixN (TokGT           ==> comparison Op.gt       )
    , InfixN (TokGE           ==> comparison Op.ge       )
    , InfixN (TokSubset       ==> binary     Op.subset   )
    , InfixN (TokSSubset      ==> binary     Op.ssubset  )
    , InfixN (TokSuperset     ==> binary     Op.superset )
    , InfixN (TokSSuperset    ==> binary     Op.ssuperset) ]
  , {-Level 10-}
    [ InfixN (TokAEQ          ==> pointRange   )
    , InfixN (TokANE          ==> binary Op.ane)
    , InfixN (TokBadEQ        ==> badEQ        ) ]
  , {-Level 11-}
    [ InfixR (TokAnd          ==> conjunction) ]
  , {-Level 12-}
    [ InfixR (TokOr           ==> binary Op.or) ]
  , {-Level 13-}
    [ InfixR (TokImplies      ==> binary Op.implies   )
    , InfixL (TokConsequent   ==> binary Op.consequent) ]
  , {-Level 14-}
    [ InfixN (TokBEQ          ==> binary Op.beq)
    , InfixN (TokBNE          ==> binary Op.bne) ]
  ]

token --> unaryOp = do
  loc <- match token
  pure $ \i -> do
    i' <- filterRawName i
    unaryOp loc i'

token ==> binaryOp = do
  loc <- match token
  pure $ \l r -> do
    l' <- filterRawName l
    r' <- filterRawName r
    binaryOp loc l' r'


call :: ParserExp (Maybe MetaExpr -> ParserExp (Maybe MetaExpr))
call = do
  args <- between (match TokLeftPar) (match' TokRightPar) $
    (metaexpr >>= filterRawName) `sepBy` match TokComma
  to <- getPosition

  pure $ \case
    Nothing -> pure Nothing
    Just (e@Expression { E.loc = Location (from, _), expType = callableType, exp' }, _, taint1) -> do
      let loc = Location (from, to)
      case callableType of
        GFunc a b ->
          case args of
            [marg :: Maybe MetaExpr] -> case marg of
              Nothing -> pure Nothing
              Just (arg, _, taint2) -> if expType arg =:= a
                then
                  let
                    expr = Expression
                      { E.loc
                      , expType = b
                      , expConst = expConst arg && expConst e
                      , exp' = Binary
                        { binOp = BifuncAt
                        , lexpr = e
                        , rexpr = arg }}
                  in pure . Just $ (expr, ProtoNothing, taint1 <> taint2)
                else do
                  putError from . UnknownError $
                    "Attempted to evaluate an expression of type " <>
                    show callableType <> " with an argument of type " <>
                    show (expType arg) <> "."
                  pure Nothing
            [] -> do
              putError from . UnknownError $
                "No arguments were given in function evaluation."
              pure Nothing
            _ -> do
              putError from . UnknownError $
                "Too many arguments were given in function evaluation."
              pure Nothing

        GRel a b ->
          case args of
            [marg :: Maybe MetaExpr] -> case marg of
              Nothing -> pure Nothing
              Just (arg, _, taint2) -> if expType arg =:= a
                then
                  let
                    expr = Expression
                      { E.loc
                      , expType = GSet b
                      , expConst = expConst arg && expConst e
                      , exp' = Binary
                        { binOp = BifuncAt
                        , lexpr = e
                        , rexpr = arg }}
                  in pure . Just $ (expr, ProtoNothing, taint1 <> taint2)
                else do
                  putError from . UnknownError $
                    "Attempted to evaluate an expression of type " <>
                    show callableType <> " with an argument of type " <>
                    show (expType arg) <> "."
                  pure Nothing
            [] -> do
              putError from . UnknownError $
                "No arguments were given in relation evaluation."
              pure Nothing
            _ -> do
              putError from . UnknownError $
                "Too many arguments were given in relation evaluation."
              pure Nothing

        GRawName -> do
          let RawName fName = exp'
          defs <- lift (use definitions)
          case fName `Map.lookup` defs of
            Just Definition { defLoc, bound, def' = FunctionDef { funcParams, funcRetType, funcRecursive }} -> do
              let
                nArgs   = length args
                nParams = length funcParams
                Location (pos, _) = defLoc
              if nArgs == nParams
                then do
                  args' <- foldM (checkType fName pos)
                    (Just (Seq.empty, Taint False, True))
                    (Seq.zip args funcParams)
                  pure $ case args' of
                    Nothing -> Nothing
                    Just (fArgs, taint, const') ->
                      let
                        expr = Expression
                          { E.loc
                          , expType  = funcRetType
                          , expConst = True
                          , exp' = FunctionCall
                            { fName
                            , fArgs
                            , fRecursiveCall = False
                            , fRecursiveFunc = isJust bound
                            , fStructArgs = Nothing }}
                      in Just (expr, ProtoNothing, taint)

                else do
                  putError from BadFuncNumberOfArgs
                    { fName
                    , fPos = pos
                    , nParams
                    , nArgs }
                  pure Nothing

            Just Definition { def' = GracielaFunc { signatures, casts } } ->
              case sequence args of
                Nothing -> pure Nothing
                Just args' -> do
                  
                  let
                    aux (es, ts, c0, t0) (e@Expression { expConst, expType }, _, t1) =
                      (es |> e, ts |> expType, c0 && expConst, t0 <> t1)
                    (fArgs', types, const', taint) =
                      foldl' aux (Seq.empty, Seq.empty, True, Taint False) args'
                    i64cast e@Expression { E.loc, expType, expConst, exp' } =
                      e { exp' = I64Cast e, expType = I64 }
                    fArgs = fArgs' & elements (`elem` casts) %~ i64cast
                    SourcePos file line col = from
                    pos' = Expression loc GInt True . Value . IntV . fromIntegral . unPos <$>
                      [ line, col ]
                  
                  fileId <- lift (use stringIds) >>= \ids -> 
                    pure $ case Map.lookup (pack file) ids of
                      Nothing -> internal "No file name."
                      Just strId  -> Expression
                        { E.loc    = Location (from, to)
                        , expType  = GString
                        , expConst = True
                        , exp'     = StringLit strId }


                  case signatures types of
                    Right (funcRetType, fName, canAbort) -> do
                      let
                        expr = Expression
                          { E.loc
                          , expType  = funcRetType
                          , expConst = True
                          , exp' = FunctionCall
                            { fName
                            , fArgs = fArgs <> if canAbort
                              then [fileId] <> pos'
                              else []
                            , fRecursiveCall = False
                            , fRecursiveFunc = False
                            , fStructArgs = Nothing }}
                      pure $ Just (expr, ProtoNothing, taint)

                    Left message -> do
                      putError from message
                      pure Nothing

            Just Definition { defLoc, def' = ProcedureDef {} } -> do
              putError from . UnknownError $
                "Cannot call procedure `" <> unpack fName <> "`, defined at " <>
                show defLoc <> " as an expression; a function was expected."
              pure Nothing

            Nothing -> do
              -- If the function is not defined, it's possible that we're
              -- dealing with a recursive call. The information of a function
              -- that is being defined is stored temporarily at the
              -- Parser.State `currentFunc`.
              currentFunction <- lift (use currentFunc)
              case currentFunction of
                Just cr@CurrentRoutine { _crTypeArgs }
                  | cr^.crName == fName  -> do
                    let
                      nArgs = length args
                      nParams = length (cr^.crParams)

                    if nArgs == nParams
                      then do
                        args' <- foldM (checkType fName (cr^.crPos))
                          (Just (Seq.empty, Taint False, True))
                          (Seq.zip args (cr^.crParams))

                        lift $ (currentFunc . _Just . crRecursive) .= True

                        pure $ case args' of
                          Nothing -> Nothing
                          Just (fArgs, taint, const') ->
                            let
                              expr = Expression
                                { E.loc
                                , expType  = cr^.crType
                                , expConst = True
                                , exp' = FunctionCall
                                  { fName
                                  , fArgs
                                  , fRecursiveCall = cr^.crRecAllowed
                                  , fRecursiveFunc = cr^.crRecAllowed
                                  , fStructArgs    = _crTypeArgs}}
                            in Just (expr, ProtoNothing, taint)
                      else do
                        putError from BadFuncNumberOfArgs
                          { fName
                          , fPos  = cr^.crPos
                          , nParams
                          , nArgs }
                        pure Nothing

                  -- | cr^.crName == fName && not (cr^.crRecAllowed) -> do
                  --   putError from . UnknownError $
                  --     "Function `" <> unpack fName <> "` cannot call itself \
                  --     \recursively because no bound was given for it."
                  --   pure Nothing
                  | otherwise -> dataTypeFunction args fName loc from

                Nothing -> dataTypeFunction args fName loc from

        _ -> do
          putError from . UnknownError $
            "Attempted to call an expression of type " <> show callableType <> "."
          pure Nothing

  where
    dataTypeFunction args fName loc from = do
      let nArgs = length args
      case hasDTType args of
        Nothing -> do
          let args' = sequence args

          case args' of
            Nothing -> do
              putError from . UnknownError $ "Calling function `" <>
                unpack fName <>"` with bad arguments"
              pure Nothing
            Just args'' -> do
              putError from . UndefinedFunction fName $ (\(e,_,_) -> e) <$> args''
              pure Nothing

        Just t@(GDataType name abstName typeArgs') -> do
          lift (use currentStruct) >>= \case
            Nothing -> lift (use dataTypes) >>= \dts -> case name `Map.lookup` dts of
              Nothing -> do
                putError from . UnknownError $
                  "Couldn't find data type " <> show t
                pure Nothing

              Just s@Struct{structProcs, struct' = DataType{abstract}} -> do
                f <- getFunc fName structProcs (Just abstract)
                case f of
                  Just (funcParams, retType', fRec) -> do
                    cs <- lift $ use currentStruct
                    let
                      nParams = length funcParams
                      retType = fillType typeArgs' retType'
                      typeArgs = case cs of
                          Nothing -> typeArgs'
                          Just (GDataType _ _ dtArgs, _, _, _, _) ->
                            fmap (fillType dtArgs) typeArgs'

                    when (nArgs /= nParams) . putError from . UnknownError $
                      "Calling function `" <> unpack fName <>
                      "` with a bad number of arguments."

                    args' <- foldM (checkType' typeArgs fName from)
                        (Just (Seq.empty, Taint False, True))
                        (Seq.zip args funcParams )
                    pure $ case args' of
                      Nothing -> Nothing

                      Just (fArgs, taint, const') ->
                        let
                          expr = case fRec of
                            Just funcRec -> Expression
                              { E.loc
                              , expType = retType
                              , expConst = True
                              , exp' = FunctionCall
                                { fName
                                , fArgs
                                , fRecursiveCall = False
                                , fRecursiveFunc = funcRec
                                , fStructArgs    = Just (name, typeArgs)}}

                            Nothing -> Expression
                              { E.loc
                              , expType = retType
                              , expConst = True
                              , exp' = AbstFunctionCall
                                { fName
                                , fArgs
                                , fStructArgs = Just (name, typeArgs)}}

                        in Just (expr, ProtoNothing, taint)

                  _ -> do
                    putError from . UnknownError $
                      "Data Type `" <> unpack name <>
                      "` does not have a function called `" <>
                      unpack fName <> "`1"
                    return Nothing

            Just (dt@GDataType{dtTypeArgs = t'}, _, _, _, _) | not (t =:= dt) -> do
              getStruct name >>= \case
                Nothing -> do
                  putError from . UnknownError $ "Couldn't find data type " <> show dt
                  pure Nothing
                Just s@Struct{structProcs, struct' = DataType{abstract}} -> do
                  f <- getFunc fName structProcs (Just abstract)
                  case f of
                    Just (funcParams, retType', fRec) -> do
                      let
                        nParams = length funcParams
                        retType = fillType t' retType'
                        typeArgs = fmap (fillType t') typeArgs'

                      when (nArgs /= nParams) . putError from . UnknownError $
                        "Calling function `" <> unpack fName <>
                        "` with a bad number of arguments."

                      args' <- foldM (checkType' typeArgs fName from)
                          (Just (Seq.empty, Taint False, True))
                          (Seq.zip args funcParams )
                      pure $ case args' of
                        Nothing -> Nothing
                        Just (fArgs, taint, const') ->
                          let
                            expr = case fRec of
                              Just funcRec -> Expression
                                { E.loc
                                , expType = retType
                                , expConst = True
                                , exp' = FunctionCall
                                  { fName
                                  , fArgs
                                  , fRecursiveCall = False
                                  , fRecursiveFunc = funcRec
                                  , fStructArgs    = Just (name, typeArgs)}}

                              Nothing -> Expression
                                { E.loc
                                , expType = retType
                                , expConst = True
                                , exp' = AbstFunctionCall
                                  { fName
                                  , fArgs
                                  , fStructArgs = Just (name, typeArgs)}}

                          in Just (expr, ProtoNothing, taint)

                    _ -> do
                      putError from . UnknownError $
                        "Data Type `" <> unpack name <>
                        "` does not have a function called `" <>
                        unpack fName <> "`5"
                      return Nothing
            Just (GDataType {dtTypeArgs, abstName = a}, _, structProcs, _, _) -> do
              f <- getFunc fName structProcs a
              case f of
                Just (funcParams, retType, fRec ) -> do
                  let
                    nParams = length funcParams

                  when (nParams /= nArgs) . putError from . UnknownError $
                      "Calling procedure `" <> unpack fName <>
                      "` with a bad number of arguments."

                  args' <- foldM (checkType' dtTypeArgs fName from)
                    (Just (Seq.empty, Taint False, True))
                    (Seq.zip args funcParams )

                  pure $ case args' of
                    Nothing -> Nothing
                    Just (fArgs, taint, const') ->
                      let
                        expr = case fRec of
                          Just funcRec -> Expression
                            { E.loc
                            , expType = retType
                            , expConst = True
                            , exp' = FunctionCall
                              { fName
                              , fArgs
                              , fRecursiveCall = False
                              , fRecursiveFunc = funcRec
                              , fStructArgs    = Just (name, dtTypeArgs)}}

                          Nothing -> Expression
                            { E.loc
                            , expType = retType
                            , expConst = True
                            , exp' = AbstFunctionCall
                              { fName
                              , fArgs
                              , fStructArgs = Just (name, dtTypeArgs)}}

                      in Just (expr, ProtoNothing, taint)

                Nothing -> do
                  putError from . UnknownError $
                    "Data Type `" <> unpack name <>
                    "` does not have a function called `" <>
                    unpack fName <> "`"
                  pure Nothing

    getFunc :: Text -> Map Text Definition -> Maybe Text
            -> ParserExp (Maybe (Seq (Text, Type), Type, Maybe Bool))
    getFunc name funcs abstName = do
      abstNamesOk <- lift $ use allowAbstNames
      case name `Map.lookup` funcs of
        Just Definition{ bound, def' = FunctionDef{ funcParams, funcRetType }} ->
          pure $ Just (funcParams, funcRetType, Just $ if isJust bound then True else False)
        _ -> if abstNamesOk && isJust abstName
          then do
            getStruct (fromJust abstName) >>= \case
              Nothing -> internal $ "Could not find abstract type " <>
                                     show (fromJust abstName)

              Just Struct{structProcs} ->
                case name `Map.lookup` structProcs of
                  Just Definition
                    { def'= AbstractFunctionDef
                      { abstFParams
                      , funcRetType }} -> do
                    cs <- lift $ use currentStruct
                    case cs of
                      Nothing -> internal $ "Could not find current DT "
                      Just (t, _, _, _, _) ->
                        pure $ Just (removeAbst' t <$> abstFParams, funcRetType, Nothing)

                  _ -> pure Nothing

          else pure Nothing
      where
          removeAbst' dt (c,t) = (c,removeAbst dt t)

    hasDTType = getFirst . foldMap aux
    aux (Just (Expression { expType },_,_)) = First $ hasDT expType
    aux Nothing                             = First Nothing
    checkType = checkType' (Array.listArray (0,-1) [])

    checkType' _ _ _ _ (Nothing, _) = pure Nothing
    checkType' typeArgs fName fPos acc
      (Just (e@Expression { E.loc, expType,expConst, exp' }, _, taint), (name, pType)) = do
        let
          Location (from, _) = loc
          pType' = fillType typeArgs pType
        if  pType' =:= expType
          then do
            let
              type' = case expType of
                GPointer GAny -> pType'
                _             -> expType

            pure $ add e{expType = type'} taint expConst <$> acc
          else do
            putError from $
              BadFunctionArgumentType name fName fPos pType' expType
            pure Nothing
    add e taint1 const1 (es, taint0, const0) =
      (es |> e, taint0 <> taint1, const0 && const1)


subindex :: ParserExp (Maybe MetaExpr -> ParserExp (Maybe MetaExpr))
subindex = do
  from' <- getPosition
  subindices' <- between (match TokLeftBracket) (match' TokRightBracket)
    (subAux `sepBy` match TokComma)
  to <- getPosition
  
  pragOk <- Set.member MemoryOperations <$> lift (use pragmas)
  let subindices = sequence subindices'

  pure $ filterRawName >=> case subindices of
    Nothing -> const $ pure Nothing
    Just subs -> if null subs
      then \case
        Just (e@Expression { expType, loc }, _, _) -> case expType of
          GArray _ _ -> do
            putError (pos loc) . UnknownError $
              "Missing dimensions in array access."
            pure Nothing
          GSeq t -> do
            putError (pos loc) . UnknownError $
              "Missing dimension in sequence access."
            pure Nothing
          _ -> do
            putError (pos loc) . UnknownError $
              "Unexpected subindex. Can only access arrays and sequences.\n" <> drawTree (toTree e)
            pure Nothing
        Nothing -> pure Nothing
      else \case
        Nothing -> pure Nothing
        Just (expr, _, taint1) -> case expr of
          Expression
            { E.loc = Location (from, _)
            , expType = GArray { dimensions, innerType }
            , exp' = Obj o } -> do
              let
                lsubs = length subs
                ldims = length dimensions
              when (lsubs /= ldims) $ do
                  putError from . UnknownError $
                    "Attempted to index " <> show ldims <>"-dimensional array \
                    \with a " <> show lsubs <> "-dimensional subindex."
              let
                taint = foldMap (view _3) subs <> taint1
                expr1 = Expression
                  { E.loc = Location (from, to)
                  , expType = innerType
                  , expConst = False
                  , exp' = Obj
                    { theObj = Object
                      { O.loc = Location (from, to)
                      , objType = innerType
                      , obj' = Index
                        { O.inner = o
                        , isPtr   = False
                        , indices = view _1 <$> subs }}}}
              pure $ Just (expr1, ProtoNothing, taint)

          Expression
            { E.loc = Location (from, _)
            , expType = GPointer innerType
            , exp' = Obj o } | pragOk -> do
              when (length subs /= 1) $ do
                putError from . UnknownError $
                  "Attempted to index too many times an \
                  \object of type " <> show (GPointer innerType)
              let
                taint = foldMap (view _3) subs <> taint1
                expr1 = Expression
                  { E.loc = Location (from, to)
                  , expType = innerType
                  , expConst = False
                  , exp' = Obj
                    { theObj = Object
                      { O.loc = Location (from, to)
                      , objType = innerType
                      , obj' = Index
                        { O.inner = o
                        , isPtr   = True
                        , indices = view _1 <$> subs }}}}
              pure $ Just (expr1, ProtoNothing, taint)
          Expression
            { E.loc = Location (from, _)
            , expType = GSeq t
            , expConst = seqConst
            , exp' } -> do
              let
                lsubs = length subs
              if lsubs /= 1
                then do
                  putError from . UnknownError $
                    "Attempted to index sequence with a " <> show lsubs <>
                    "-dimensional subindex. All sequences are 1-dimensional."
                  pure Nothing
              else
                let
                  [(subexp, _, subtaint)] = subs
                  taint = subtaint <> taint1
                  expr1 = Expression
                    { E.loc    = Location (from, to)
                    , expType  = t
                    , expConst = seqConst && expConst subexp
                    , exp' = Binary
                      { binOp = SeqAt
                      , lexpr = expr
                      , rexpr = subexp }}
                in pure $ Just (expr1, ProtoNothing, taint)
          _ -> do
            putError (pos . E.loc $ expr) . UnknownError $
              "Cannot subindex non-array."

            pure Nothing

  where
    subAux :: ParserExp (Maybe MetaExpr)
    subAux = do
      e <- metaexpr >>= filterRawName
      case e of
        je@(Just (Expression { expType = GInt }, _, _)) -> do
          pure je
        Just (Expression { expType, loc }, _, _) -> do
          putError (pos loc) . UnknownError $
            "Cannot use expression of type " <> show expType <>
            " as subindex, integer expression was expected`."
          pure Nothing
        Nothing -> pure Nothing


dotField :: ParserExp (Maybe MetaExpr -> ParserExp (Maybe MetaExpr))
dotField = do
  lookAhead $ match TokDot
  from' <- getPosition
  match TokDot
  fieldName' <- safeIdentifier

  to <- getPosition

  pure $ filterRawName >=> case fieldName' of
    Nothing -> const $ pure Nothing
    Just fieldName -> \case
      Nothing -> pure Nothing
      Just (e@Expression { exp', loc }, _, taint) -> do
        let Location (from,_) = loc
        case exp' of
          Obj obj -> 
            let 
              doDT n typeArgs = do -- If the object has a DT type, then do the following 
                cstruct <- lift $ use currentStruct
                case cstruct of
                  Just (GDataType name _ _, structFields, _, structAFields, _)
                    | name == n ->
                      aux obj (objType obj) loc fieldName structFields structAFields taint
                  _ -> do
                    structs <- lift $ use dataTypes
                    case n `Map.lookup` structs of
                      Just Struct { structFields, structAFields } ->
                        let structFields' = fillTypes typeArgs structFields
                        in aux obj (objType obj) loc fieldName structFields' structAFields  taint
                      _ -> internal "GDataType without struct."

            in case objType obj of
              GDataType n _ typeArgs            -> doDT n typeArgs
              GAlias _ (GDataType n _ typeArgs) -> doDT n typeArgs
              t -> do
                putError from' . UnknownError $
                  "Bad field access. Cannot access an expression \
                  \of type " <> show t <> "."
                pure Nothing
          _ -> do
            putError from' . UnknownError $
              "Bad field access. Cannot access an expression."
            pure Nothing
  where
    aux :: Object
        -> Type
        -> Location
        -> Text
        -> Map Text (Integer, Type, Bool, a)
        -> Map Text (Integer, Type, Bool, a)
        -> Taint
        -> ParserExp (Maybe MetaExpr)
    aux o oType loc fieldName structFields structAFields taint =
      case fieldName `Map.lookup` structFields of
        Just (i, t, c, _) -> lift (use isDeclarative) >>= \x -> if t =:= highLevel && not x
          then do
            let Location (pos, _) = loc
            putError pos . UnknownError $
              "Bad field access. Use of field `" <> unpack fieldName <>
              "` of type " <> show t <> " not allowed in imperative code."
            pure Nothing

          else
            let
              expr = Expression
                { loc
                , expType  = t
                , expConst = c
                , exp'     = Obj
                  { theObj = Object
                    { loc
                    , objType = t
                    , obj' = Member
                      { inner = o
                      , field = i
                      , fieldName }}}}
            in pure $ Just (expr, ProtoNothing, taint)

        Nothing -> case fieldName `Map.lookup` structAFields of
          Just (i, t, c, _) -> do
            logicAW <- Set.member LogicAnywhere <$> lift (use pragmas)
            if logicAW
              then let
                expr = Expression
                  { loc
                  , expType  = t
                  , expConst = c
                  , exp'     = Obj
                    { theObj = Object
                      { loc
                      , objType = t
                      , obj' = Member
                        { inner = o
                        , field = i
                        , fieldName }}}}
                in pure $ Just (expr, ProtoNothing, taint)

            else do
              let Location (pos, _) = loc
              putError pos . UnknownError $
                "Bad field access. Cannot access the abstract field `" <>
                unpack fieldName <>
                "`\n\toutside the abstract definition or couple relation"
              pure Nothing

          Nothing -> do
            let Location (pos, _) = loc
            putError pos . UnknownError $
              "Bad field access. Object of type " <> show oType <>
              "` does not have a field named `" <>
              unpack fieldName <> "`"
            pure Nothing

deref :: ParserExp (Maybe MetaExpr -> ParserExp (Maybe MetaExpr))
deref = do
  from <- getPosition
  void $ match TokTimes

  pure $ filterRawName >=> \case
    Nothing -> pure Nothing

    Just (expr, _, taint) -> case expr of
      Expression
        { E.loc = Location (_, to)
        , expType = GPointer pointedType
        , exp' = Obj o } ->
          let expr = Expression
                { E.loc = Location (from, to)
                , expType = pointedType
                , expConst = False
                , exp' = Obj
                  { theObj = Object
                    { O.loc = Location (from, to)
                    , objType = pointedType
                    , obj' = Deref
                      { O.inner = o }}}}
          in pure $ Just (expr, ProtoNothing, taint)


      e -> do

        putError from . UnknownError $ "Cannot deref non-pointer."

        pure Nothing

refof :: ParserExp (Maybe MetaExpr -> ParserExp (Maybe MetaExpr))
refof = do
  from <- getPosition
  void $ match TokAmpersand
  pragmas' <- lift $ use pragmas
  let pragOk = Set.member MemoryOperations pragmas'
  unless pragOk . putError from . UnknownError $
    "Unknown token: \ESC[0;33;1m&\ESC[m"
  pure $ filterRawName >=> \case
    Nothing -> pure Nothing

    Just (expr, _, taint) -> case expr of
      Expression
        { E.loc = Location (_, to)
        , expType
        , exp' = Obj o } ->

        let expr' = Expression
              { E.loc = Location (from, to)
              , expType = GPointer expType
              , expConst = False
              , exp' = AddressOf { inner = expr }}
        in pure $ Just (expr', ProtoNothing, taint)

      e -> do

        when pragOk . putError from . UnknownError $
          "Cannot get the address of non-object expression."

        pure Nothing

unsafeCast :: ParserExp (Maybe MetaExpr -> ParserExp (Maybe MetaExpr))
unsafeCast = do 
  from     <- getPosition
  t        <- brackets . lift $ type'
  pragmas' <- lift $ use pragmas
  let pragOk = Set.member MemoryOperations pragmas'
 
  unless pragOk . putError from . UnknownError $
    "Unknown token: " <> show t

  unless (t =:= GPointer GAny) . putError from . UnknownError $
    "Can not cast to a non pointer type"

  pure $ filterRawName >=> \case
    Nothing -> pure Nothing

    Just (expr, _, taint) -> case expr of
      expr | expType expr =:= GPointer GAny ->
        let expr' = Expression
              { E.loc = Location (from, to expr)
              , expType = t
              , expConst = False
              , exp' = UnsafeCast { castExpr = expr }}
        in pure $ Just (expr', ProtoNothing, taint)
      e -> do
        when pragOk . putError from . UnknownError $
          "A pointer was expected. Instead, an expression of type " <> show (expType e) <> " was given"

        pure Nothing


unary :: Op.Un -> Location
      -> Maybe MetaExpr -> ParserExp (Maybe MetaExpr)
unary unOp
  opLoc @ (Location (from,_))
  (Just (i @ Expression { expType = itype, exp', expConst }, _, taint))
  = case Op.unType unOp itype of
    Left expected -> do
      let loc = Location (from, to i)
      putError from . UnknownError $
        "Operator `" <> show (Op.unSymbol unOp) <>
        "`` received an expression of type:" <>
        "\n\t\t" <> show itype <>
        "\n\tbut expected an expression of type " <>
        "\n\t\t" <> expected
      pure Nothing

    Right ret -> do
      mexpr <- case unOp of
        Op.Un { unFunc } ->
          let
            exp'' = case exp' of
              Value v -> Value . unFunc $ v
              _ -> Unary
                { unOp = Op.unSymbol unOp
                , E.inner = i }
            expr = Expression
              { E.loc = Location (from, to i)
              , expType = ret
              , expConst
              , exp' = exp'' }
          in pure $ Just expr
        Op.Un'' { unFunc'' } -> lift $ unFunc'' from i

      case mexpr of
        Nothing -> pure Nothing
        Just expr ->
          pure $ Just (expr, ProtoNothing, taint)

unary _ _ _ = pure Nothing


binary :: Op.Bin -> Location
       -> Maybe MetaExpr -> Maybe MetaExpr -> ParserExp (Maybe MetaExpr)
binary _ _ Nothing _ = pure Nothing
binary _ _ _ Nothing = pure Nothing
binary binOp opLoc
  (Just (l @ Expression { expType = ltype, expConst = lc, exp' = lexp }, _, ltaint))
  (Just (r @ Expression { expType = rtype, expConst = rc, exp' = rexp }, _, rtaint))
  = do 
    pragmas' <- lift $ use pragmas
    let pragOk = Set.member MemoryOperations pragmas'

    case Op.binType binOp ltype rtype of

      Left expected | not (pragOk && (Op.binSymbol binOp == Plus || Op.binSymbol binOp == E.BMinus) &&
                      ((ltype =:= GPointer GAny && rtype =:= GInt) ||
                       (rtype =:= GPointer GAny && ltype =:= GInt))) -> do
        putError (from l) . UnknownError $
          "Operator `" <> show (Op.binSymbol binOp) <>
          "` received two expressions of types:" <>
          "\n\t\t" <> show (ltype, rtype) <>
          "\n\tbut expected an expression of type " <>
          "\n\t\t" <> expected
        pure Nothing
      Left _ -> do 
        mexpr <- case binOp of
          Op.Bin   { binFunc   } ->
            let
              exp' = case (lexp, rexp) of
                (Value v, Value w) -> Value $ binFunc v w
                _ -> Binary
                  { binOp = Op.binSymbol binOp
                  , lexpr = l
                  , rexpr = r }
              expr = Expression
                { E.loc = Location (from l, to r)
                , expType = if (ltype =:= GInt) then rtype else ltype
                , expConst = lc && rc
                , exp' }
            in pure . Just $ expr

          Op.Bin'  { binFunc'  } -> pure . Just $ binFunc' l r

          Op.Bin'' { binFunc'' } -> lift $ binFunc'' l r
        
        case mexpr of
          Nothing -> pure Nothing
          Just expr ->
            let
              taint = ltaint <> rtaint

            in pure $ Just (expr, ProtoNothing, taint)

      Right ret -> do
        mexpr <- case binOp of
          Op.Bin   { binFunc   } ->
            let
              exp' = case (lexp, rexp) of
                (Value v, Value w) -> Value $ binFunc v w
                _ -> Binary
                  { binOp = Op.binSymbol binOp
                  , lexpr = l
                  , rexpr = r }
              expr = Expression
                { E.loc = Location (from l, to r)
                , expType = ret
                , expConst = lc && rc
                , exp' }
            in pure . Just $ expr

          Op.Bin'  { binFunc'  } -> pure . Just $ binFunc' l r

          Op.Bin'' { binFunc'' } -> lift $ binFunc'' l r

        case mexpr of
          Nothing -> pure Nothing
          Just expr ->
            let
              taint = ltaint <> rtaint

            in pure $ Just (expr, ProtoNothing, taint)


membership :: Location
           -> Maybe MetaExpr -> Maybe MetaExpr
           -> ParserExp (Maybe MetaExpr)
membership opLoc
  (Just (l @ Expression { expType = ltype, expConst = lc }, ProtoVar, ltaint))
  (Just (r @ Expression { expType = rtype, expConst = rc }, _, Taint False))
  = case Op.binType Op.elem ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      putError (from l) . UnknownError $
        "Operator `" <> show Elem <>
        "` received two expressions of types:" <>
        "\n\t\t" <> show (ltype, rtype) <>
        "\n\tbut expected an expression of type " <>
        "\n\t\t" <> expected
      pure Nothing

    Right GBool ->
      let
        expr = Expression
          { E.loc    = Location (from l, to r)
          , expType  = GBool
          , expConst = lc && rc
          , exp'     = eSkip }
      in pure $ Just (expr, ProtoQRange (SetRange r), Taint False)

    Right _ -> internal "impossible membership type"

membership opLoc l r = binary Op.elem opLoc l r


comparison :: Op.Bin -> Location
           -> Maybe MetaExpr -> Maybe MetaExpr -> ParserExp (Maybe MetaExpr)
comparison binOp opLoc
  (Just (l @ Expression { expType = ltype, expConst = lc }, _, Taint False))
  (Just (r @ Expression { expType = rtype, expConst = rc }, ProtoVar, _))
    | rtype =:= GOneOf [GBool, GChar, GInt]
  = case Op.binType binOp ltype rtype of
      Left expected -> do
        let loc = Location (from l, to r)
        putError (from l) . UnknownError $
          "Operator `" <> show (Op.binSymbol binOp) <>
          "` received two expressions of types:" <>
          "\n\t\t" <> show (ltype, rtype) <>
          "\n\tbut expected an expression of type " <>
          "\n\t\t" <> expected
        pure Nothing

      Right GBool ->
        let
          range = case Op.binSymbol binOp of
            LT -> ProtoLow  (succ' l)
            LE -> ProtoLow  l
            GT -> ProtoHigh (pred' l)
            GE -> ProtoHigh l
            _  -> internal "impossible comparison operator"
          expr = Expression
            { E.loc    = Location (from l, to r)
            , expType  = GBool
            , expConst = lc && rc
            , exp'     = eSkip }
        in pure $ Just (expr, range, Taint True)

      Right _ ->
        internal "impossible type equality"

  where
    succ' = aux Succ
    pred' = aux Pred
    aux op e @ Expression { E.loc, expType, expConst, exp' } =
      Expression { E.loc, expType, expConst, exp' = Unary op e }

comparison binOp opLoc
  (Just (l @ Expression { expType = ltype, expConst = lc }, ProtoVar, _))
  (Just (r @ Expression { expType = rtype, expConst = rc }, _, Taint False))
    | ltype =:= GOneOf [GBool, GChar, GInt]
  = case Op.binType binOp ltype rtype of
      Left expected -> do
        let loc = Location (from l, to r)
        putError (from l) . UnknownError $
          "Operator `" <> show (Op.binSymbol binOp) <>
          "` received two expressions of types:" <>
          "\n\t\t" <> show (ltype, rtype) <>
          "\n\tbut expected an expression of type " <>
          "\n\t\t" <> expected
        pure Nothing

      Right GBool ->
        let
          range = case Op.binSymbol binOp of
            LT -> ProtoHigh (pred' r)
            LE -> ProtoHigh r
            GT -> ProtoLow  (succ' r)
            GE -> ProtoLow  r
            _  -> internal "impossible comparison operator"
          expr = Expression
            { E.loc    = Location (from l, to r)
            , expType  = GBool
            , expConst = lc && rc
            , exp'     = eSkip }
        in pure $ Just (expr, range, Taint True)

      Right _ ->
        internal "impossible type equality"

  where
    succ' = aux Succ
    pred' = aux Pred
    aux op e @ Expression { E.loc, expType, expConst, exp' } =
      Expression { E.loc, expType, expConst, exp' = Unary op e }

comparison binOp opLoc l r = binary binOp opLoc l r

badEQ :: Location
      -> a -> b
      -> ParserExp (Maybe MetaExpr)
badEQ (Location (from,_)) _ _ = do
  putError from . UnknownError $
    "Graciela doesn't have operator `=`. Use `==` instead."
  pure Nothing

pointRange :: Location
           -> Maybe MetaExpr -> Maybe MetaExpr
           -> ParserExp (Maybe MetaExpr)
pointRange opLoc
  (Just (l @ Expression { expType = ltype, expConst = lc }, ProtoVar, ltaint))
  (Just (r @ Expression { expType = rtype, expConst = rc }, _, Taint False))
  = case Op.binType Op.aeq ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      putError (from l) . UnknownError $
        "Operator `" <> show Elem  <>
        "` received two expressions of types:" <>
        "\n\t\t" <> show (ltype, rtype) <>
        "\n\tbut expected an expression of type " <>
        "\n\t\t" <> expected
      pure Nothing

    Right GBool ->
      let
        expr = Expression
          { E.loc    = Location (from l, to r)
          , expType  = GBool
          , expConst = lc && rc
          , exp'     = eSkip }
      in pure $ Just (expr, ProtoQRange (PointRange r), Taint False)

    Right _ -> internal "impossible type equality"

pointRange opLoc
  (Just (l @ Expression { expType = rtype, expConst = lc }, _, Taint False))
  (Just (r @ Expression { expType = ltype, expConst = rc }, ProtoVar, ltaint))
  = case Op.binType Op.aeq ltype rtype of
    Left expected -> do
      let loc = Location (from l, to r)
      putError (from l) . UnknownError $
        "Operator `" <> show Elem  <> 
        "` received two expressions of types:" <>
        "\n\t\t" <> show (ltype, rtype) <>
        "\n\tbut expected an expression of type " <>
        "\n\t\t" <> expected
      pure Nothing

    Right GBool ->
      let
        expr = Expression
          { E.loc    = Location (from l, to r)
          , expType  = GBool
          , expConst = lc && rc
          , exp'     = eSkip }
      in pure $ Just (expr, ProtoQRange (PointRange l), Taint False)

    Right _ -> internal "impossible type equality"

pointRange opLoc l r = binary Op.aeq opLoc l r


conjunction :: Location
            -> Maybe MetaExpr -> Maybe MetaExpr
            -> ParserExp (Maybe MetaExpr)
conjunction _ Nothing _ = pure Nothing
conjunction _ _ Nothing = pure Nothing
conjunction opLoc
  l@(Just (Expression { }, lproto, ltaint))
  r@(Just (Expression { }, rproto, rtaint))
  |  lproto == ProtoNothing || lproto == (ProtoQRange EmptyRange) -- Without it, conjuntion always give true (false /\ false == true)
  && rproto == ProtoNothing || rproto == (ProtoQRange EmptyRange) -- Possible reason: False lit is always being marked as ProtoQRange
    = binary Op.and opLoc l r
conjunction opLoc
  (Just (l @ Expression { expType = ltype, expConst = lc, exp' = lexp' }, lproto, ltaint))
  (Just (r @ Expression { expType = rtype, expConst = rc, exp' = rexp' }, rproto, rtaint))
  = case Op.binType Op.and ltype rtype of
    Right GBool -> do

      varname <- gets head
      let
        taint = ltaint <> rtaint
        (conds :: Seq Expression, range) = case (lproto, rproto) of
          (ProtoVar, _) -> internal "boolean ProtoVar"
          (_, ProtoVar) -> internal "boolean ProtoVar"

          (q @ (ProtoQRange EmptyRange), _) ->
            ([wrap eSkip], q)
          (_, q @ (ProtoQRange EmptyRange)) ->
            ([wrap eSkip], q)

          (ProtoNothing, proto) ->
            ([l, r], proto)
          (proto, ProtoNothing) ->
            ([l, r], proto)

          (ProtoLow low, ProtoHigh high) ->
            ([l, r], ProtoQRange (ExpRange low high))
          (ProtoHigh high, ProtoLow low) ->
            ([l, r], ProtoQRange (ExpRange low high))
          (ProtoLow  llow,  ProtoLow  rlow ) ->
            ([l, r], ProtoLow  (joinProtos Max llow rlow))
          (ProtoHigh lhigh, ProtoHigh rhigh) ->
            ([l, r], ProtoHigh (joinProtos Min lhigh rhigh))

          (ProtoQRange lr @ ExpRange {}, ProtoQRange rr @ ExpRange {}) ->
            ([l, r], joinExpRanges lr rr)

          (ProtoQRange lr @ ExpRange {}, rr @ ProtoLow {}) ->
            ([l, r], joinExpRangeProto lr rr)
          (ProtoQRange lr @ ExpRange {}, rr @ ProtoHigh {}) ->
            ([l, r], joinExpRangeProto lr rr)
          (lr @ ProtoLow {} , ProtoQRange rr @ ExpRange {}) ->
            ([l, r], joinExpRangeProto rr lr)
          (lr @ ProtoHigh {}, ProtoQRange rr @ ExpRange {}) ->
            ([l, r], joinExpRangeProto rr lr)

          (point @ (ProtoQRange PointRange {}), proto) ->
            ([wrap $ rebuild varname proto, l, r], point)
          (proto, point @ (ProtoQRange PointRange {})) ->
            ([wrap $ rebuild varname proto, l, r], point)

          (set @ (ProtoQRange SetRange {}), proto) ->
            ([wrap $ rebuild varname proto, l, r], set)
          (proto, set @ (ProtoQRange SetRange {})) ->
            ([wrap $ rebuild varname proto, l, r], set)

        expr = foldr1 joinCond conds

      pure $ Just (expr, range, taint)

    Left expected -> do
      let loc = Location (from l, to r)
      putError (from l) . UnknownError $
        "Operator `" <> show And <>
        "` received two expression of types:" <>
        "\n\t\t" <> show (ltype, rtype) <>
        "\n\t but expected expressions of types " <>
        "\n\t\t" <> expected
      pure Nothing

    Right _ -> internal "Bad andOp type"

    where
      loc = E.loc l <> E.loc r

      wrap exp' = Expression
        { E.loc
        , expType  = GBool
        , expConst = False
        , exp' }

      joinCond = Op.binFunc' Op.and

      joinProtos binOp l r = Expression
        { E.loc
        , expType  = expType l
        , expConst = False
        , exp'     = Binary binOp l r }

      joinExpRanges l@ExpRange {} ExpRange { low, high }
        = let ProtoQRange l' = joinExpRangeProto l (ProtoLow low)
          in  joinExpRangeProto l' (ProtoHigh high)

      joinExpRanges _ _ = internal "can only join two ExpRanges"

      joinExpRangeProto
        ExpRange { low = elow, high }
        (ProtoLow plow)
        = let
            t = expType elow
            low = Expression
              { E.loc = Location
                (from elow `min` from plow, to elow `max` to plow)
              , expType = t
              , expConst = False
              , exp' = Binary
                { binOp = Max
                , lexpr = elow
                , rexpr = plow }}

          in ProtoQRange ExpRange { low, high }

      joinExpRangeProto
        ExpRange { low, high = ehigh }
        (ProtoHigh phigh)
        = let
            t = expType low
            high = Expression
              { E.loc = Location
                (from ehigh `min` from phigh, to ehigh `max` to phigh)
              , expType = t
              , expConst = False
              , exp' = Binary
                { binOp = Min
                , lexpr = ehigh
                , rexpr = phigh }}

          in ProtoQRange ExpRange { low, high }

      joinExpRangeProto _ _ = error
        "internal error: can only join ExpRanges to Proto{Low|High}"

      rebuild varname (ProtoQRange (SetRange e)) =
        let
          t = case expType e of
            GSet a      -> a
            GMultiset a -> a
            _           -> internal "impossible set type"
          lexpr = obj varname t
        in Binary
          { binOp = Elem
          , lexpr
          , rexpr = e }

      rebuild varname (ProtoQRange (PointRange rexpr)) =
        let
          t = expType rexpr
          lexpr = obj varname t
        in Binary
          { binOp = AEQ
          , lexpr
          , rexpr }

      rebuild varname (ProtoQRange (ExpRange l h)) =
        let
          lexpr' = rebuild varname (ProtoLow l)
          rexpr' = rebuild varname (ProtoHigh h)
        in Binary
          { binOp = And
          , lexpr = Expression
            { E.loc
            , expType = GBool
            , expConst = False
            , exp' = lexpr' }
          , rexpr = Expression
            { E.loc
            , expType = GBool
            , expConst = False
            , exp' = rexpr' }}

      rebuild varname (ProtoLow l) =
        let
          t = expType l
          rexpr = obj varname t
        in Binary
          { binOp = LT
          , lexpr = l
          , rexpr }

      rebuild varname (ProtoHigh h) =
        let
          t = expType l
          lexpr = obj varname t
        in Binary
          { binOp = LT
          , lexpr
          , rexpr = h }

      rebuild _ ProtoVar = error
        "internal error: can't rebuild range variable"
      rebuild _ ProtoNothing = error
        "internal error: can't rebuild a non-range"
      rebuild _ (ProtoQRange EmptyRange) = error
        "internal error: can't rebuild an empty range"

      obj name expType =
        Expression
          { E.loc
          , expType
          , expConst = False
          , exp' = Obj
            { theObj = Object
              { O.loc
              , objType = expType
              , obj' = Variable
                { O.name = name
                , mode = Nothing}}}}
