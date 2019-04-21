{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE OverloadedStrings        #-}


module Language.Graciela.Parser.Enumeration
  ( enum
  ) where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Enumeration
import           Language.Graciela.AST.Expression
import           Language.Graciela.AST.Type
import           Language.Graciela.Common
import           Language.Graciela.Entry
import           Language.Graciela.Error
import           Language.Graciela.Location
import           Language.Graciela.Parser.Assertion   hiding (bound)
import qualified Language.Graciela.Parser.Assertion   as A (bound)
import           Language.Graciela.Parser.Declaration
import           Language.Graciela.Parser.Expression
import           Language.Graciela.Parser.Instruction
import           Language.Graciela.Parser.Monad
import           Language.Graciela.Parser.State
import           Language.Graciela.Parser.Type
import           Language.Graciela.SymbolTable        hiding (empty)
import qualified Language.Graciela.SymbolTable        as ST (empty, lookup)
import           Language.Graciela.Token
--------------------------------------------------------------------------------
import           Control.Lens                         (use, (%=))
import qualified Data.Sequence                        as Seq (fromList)
import qualified Data.Map                             as Map (insert)
import           Data.Text                            (Text, unpack)
import           Text.Megaparsec.Error                (sourcePosStackPretty)
import           Text.Megaparsec                      (getPosition, lookAhead,
                                                       optional)
--------------------------------------------------------------------------------

enum :: Parser (Maybe Enumeration)
enum = do
  lookAhead $ match TokEnum
  Location(from,_) <- match TokEnum
  ident <- optional identifier
  enumType <- case ident of
    Just name -> do
      userDefinedType %= Map.insert name (GEnum name)
      pure $ GEnum name
    _ -> pure $ GEnum ""
  assigns' <- braces $ (singleAssign enumType) `sepBy` match TokComma

  case sequence (toList assigns') of
    Just assigns -> do
      pairs <- assignValues assigns 0 enumType
      pure . Just $ Enumeration { type' = Just GAny, pairs = Seq.fromList pairs }
    _ -> pure Nothing

  where
    assignValues :: [(Text, SourcePos, Maybe Expression)] -> Int32 -> Type -> Parser [(Text, Expression)]
    assignValues ((name, _, Just e@Expression{exp' = Value {theValue = IntV v}}):xs) currentValue t =
      ((:)(name,e)) <$> assignValues xs (v+1) t
    assignValues ((name, pos, Nothing):xs) currentValue t = do
      let expr = intConst currentValue pos t
      enumEntry name pos t expr
      ((:)(name, expr) ) <$> assignValues xs (currentValue+1) t
    assignValues [(name, _, Just e)] _ _ = pure [(name, e)]
    assignValues [] _ _ = pure []
    assignValues _ _ _ = internal "Enum. What?"

    enumEntry :: Text -> SourcePos -> Type -> Expression -> Parser ()
    enumEntry ident pos enumType expr =
      symbolTable %= insertSymbol ident Entry
                  { _entryName = ident
                  , _loc       = Location (pos, pos)
                  , _info      = Enum
                    { _enumType  = enumType
                    , _enumValue = expr {expType = enumType} }}

    intConst :: Int32 -> SourcePos -> Type -> Expression
    intConst value pos t = Expression
        { loc      = Location (pos,pos)
        , expType  = t
        , expConst = True
        , exp'     = Value
          { theValue = IntV value}}


    singleAssign :: Type -> Parser (Maybe (Text, SourcePos, Maybe Expression))
    singleAssign enumType = do
      st <- use symbolTable
      ident' <- safeIdentifier >>= \case
        Just name -> do
          let lkup = ST.lookup name st
          case lkup of
            Right Entry{ _loc } -> do
              from   <- getPosition
              putError from . UnknownError $ "Identifier `" <>
                unpack name <> "` is duplicate.\n\tPrevios definition in " <>
                (showPos $ pos _loc)
              pure Nothing
            _ -> pure $ Just name
        Nothing -> do
          from   <- getPosition
          putError from . UnknownError $ "An identifier was expected in the enumeration"
          pure Nothing

      from <- getPosition
      hasAssign <- optional $ match TokAssign

      case (ident', hasAssign) of
        (Just ident, Just _) -> do
          expression >>= \case
            Just expr -> do
              if ((not $ expType expr =:= enumType) || expConst expr == False)
                then
                  putError from . UnknownError $ "Expression is not an integer constant expression"
                else enumEntry ident from enumType expr
              pure $ Just (ident, from, Just expr)
            Nothing -> do
              putError from . UnknownError $ "An integer constant expression was expected"
              pure Nothing
        (Just ident, Nothing) -> pure $ Just (ident, from, Nothing)
        _ -> pure Nothing
