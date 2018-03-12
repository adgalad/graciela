{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Graciela.Error where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Expression (Expression (expType))
import           Language.Graciela.AST.Type       (Type (..))
import           Language.Graciela.Common
import           Language.Graciela.Token
--------------------------------------------------------------------------------
import           Data.List                        (intercalate)
import           Data.Sequence                    (Seq)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Text                        (unpack)
import           Text.Megaparsec                  hiding (Token)
import           Text.Megaparsec.Error            
--------------------------------------------------------------------------------

data Error
  = BadAssertType
    { aType :: Type }
  | BadBoundType
    { bType :: Type }
  |BadFuncExpressionType
    { fName :: Text
    , fType :: Type
    , eType :: Type }
  | BadFuncNumberOfArgs
    { fName   :: Text
    , fPos    :: SourcePos
    , nParams :: Int
    , nArgs   :: Int }
  | BadProcNumberOfArgs
    { pName   :: Text
    , pPos    :: SourcePos
    , nParams :: Int
    , nArgs   :: Int }
  | BadFunctionArgumentType
    { paramName :: Text
    , fName     :: Text
    , fPos      :: SourcePos
    , pType     :: Type
    , aType     :: Type }
  | BadFunctionArgumentType'
    { paramNum :: Int
    , fName    :: Text
    , fPos     :: SourcePos
    , pTypes   :: Seq Type
    , aType    :: Type }
  | BadProcedureArgumentType
    { paramName :: Text
    , pName     :: Text
    , pPos      :: SourcePos
    , pType     :: Type
    , aType     :: Type }
  | BadReadArgument
    { aExpr :: Expression }
  | BadReadArgumentType
    { aExpr :: Expression
    , aType :: Type }
  | BadNumberOfTypeArgs
    { dtName    :: Text
    , dtTypes   :: [Type]
    , absName   :: Text
    , abstypes  :: [Type]
    , len       :: Int
    , lenNeeded :: Int }
  | EmptyBlock

  | NoDoInvariant

  | NoDoBound

  | NoAbstractInvariant
    { aName :: Text }
  | NoTypeRepInv
    { tName :: Text }
  | NoTypeCoupInv
    { tName :: Text }
  | NoProcBody
    { pName :: Text }
  | NoProcPrecondition
    { pName :: Text }
  | NoProcPostcondition
    { pName :: Text }
  | NotInScope
    { sName :: Text }
  | UndefinedFunction
    { fName :: Text
    , fArgs :: Seq Expression }
  | UndefinedProcedure
    { pName :: Text
    , pArgs :: Seq Expression }
  | UndefinedSymbol
    { sName :: Text }
  | UndefinedType
    { tName :: Text }
  | UnknownError
    { emsg :: String }
  deriving (Show, Eq, Generic, Serialize)


type GracielaError = ParseError Token Error


instance ShowErrorComponent Error where
  showErrorComponent err = case err of
    BadAssertType { aType } ->
      "Assertions must contain an expression of type " <> show GBool <>
      ". Actual type is " <> show aType <> "."

    BadBoundType  { bType } ->
      "Bounds must contain an expression of type " <> show GInt <>
      ". Actual type is " <> show bType <> "."
    BadFuncExpressionType { fName, fType, eType } ->
      "The function `" <> unpack fName <> "` returns " <> show fType <>
      " but has an expression of type " <> show eType
    BadFuncNumberOfArgs { fName, fPos, nParams, nArgs } ->
      "The function `" <> unpack fName <> "` " <> showPos fPos <>
      " was defined with " <> show nParams <>
      (if nParams == 1 then " parameter" else " parameters") <> ", but received " <>
      show nArgs <> (if nArgs == 1 then " argument." else " arguments.")
    BadProcNumberOfArgs { pName, pPos, nParams, nArgs } ->
      "The procedure `" <> unpack pName <> "` " <> showPos pPos <>
      " was defined with " <> show nParams <>
      (if nParams == 1 then " parameter" else " parameters") <> ", but received " <>
      show nArgs <> (if nArgs == 1 then " argument." else " arguments.")

    BadFunctionArgumentType { paramName, fName, fPos, pType, aType } ->
      "The parameter `" <> unpack paramName <>"` of the function `" <> unpack fName <>
      "` " <> showPos fPos <> " has type " <> show pType <>
      ",\n\tbut received an expression with type " <> show aType <> "."

    BadFunctionArgumentType' { paramNum, fName, fPos, pTypes = [pType], aType} ->
      "Parameter number " <> show paramNum <> " of the function `" <> unpack fName <>
      "` " <> showPos fPos <> " admits type " <> show pType <>
      ",\n\tbut received an expression of type " <> show aType <> "."

    BadFunctionArgumentType' { paramNum, fName, fPos, pTypes, aType} ->
      "Parameter number " <> show paramNum <> " of the function `" <> unpack fName <>
      "` " <> showPos fPos <> " admits one of the following types:\n" <>
      (unlines . fmap (("\t * " <>) . show) . toList $ pTypes) <>
      "\tbut received an expression of type " <> show aType <> "."

    BadProcedureArgumentType { paramName, pName, pPos, pType, aType} ->
      "The parameter `" <> unpack paramName <>"` of the procedure `" <> unpack pName <>
      "` " <> showPos pPos <> " has type " <> show pType <>
      ",\n\tbut received an expression with type " <> show aType <> "."

    BadReadArgument { aExpr } ->
      "The expression `" <> show aExpr <> "` is a constant expression."

    BadReadArgumentType { aExpr, aType } ->
      "The variable `" <> show aExpr <> "` has type " <> show aType <>
      "\n\tbut only variables of type "<> show GChar <>", "<> show GFloat <>
      " or " <> show GInt <>" can be read."

    BadNumberOfTypeArgs { dtName, dtTypes , absName, abstypes, len, lenNeeded } ->
      let
        t a b = if a == 0
                  then "no type"
                else show (length b) <> if a > 1
                    then " type (" <> intercalate "," (fmap show b) <> ")"
                  else " types (" <> intercalate "," (fmap show b) <> ")"

      in "Type `" <> unpack dtName <> "` is implementing `" <>
          unpack absName <> "` with " <> t len dtTypes <>
          "\n\tbut expected " <> t lenNeeded abstypes <> "."

    EmptyBlock ->
      "Instruction blocks must contain at least one instruccion."

    NoDoInvariant ->
      "Missing invariant of instruction `do`."

    NoDoBound ->
      "Missing bound of instruction `do`."

    NoAbstractInvariant { aName } ->
      "Missing invariant in abstract type `" <> unpack aName <> "`."

    NoTypeRepInv { tName } ->
      "Missing representation invariant in type `" <> unpack tName <> "`."

    NoTypeCoupInv { tName } ->
      "Missing couple invariant in type `" <> unpack tName <> "`."

    NoProcBody { pName } ->
      "Procedure `" <> unpack pName <> "` has not instruction block.\n\t" <>
      "Possible solution: Declare a instruction block using `|[` and `]|`."

    NoProcPrecondition { pName } ->
      "Missing precondition of procedure `" <> unpack pName <> "`."

    NoProcPostcondition { pName } ->
      "Missing postcondition of procedure `" <> unpack pName <> "`."

    NotInScope { sName } ->
      "Not in the scope: `" <> unpack sName <> "`."

    UndefinedFunction { fName, fArgs } ->
      "Undefined function `" <> unpack fName <> "(" <>
      intercalate "," (fmap (show . expType) (toList fArgs)) <> ")`."

    UndefinedProcedure { pName, pArgs } ->
      "Undefined procedure `" <> unpack pName <> "(" <>
      intercalate "," (fmap (show . expType) (toList pArgs)) <> ")`."

    UndefinedSymbol { sName } ->
      "Undefined symbol named `" <> unpack sName <> "`."

    UndefinedType { tName } ->
      "Undefined type `" <> unpack tName <> "`."

    UnknownError {emsg} -> emsg

instance Ord Error where
  a <= b = True

instance Ord (ParseError Token Error) where
  a <= b = True  

-- Modify the pretty print of errors
prettyError :: ( Ord t
               , ShowToken t
               , ShowErrorComponent e )
  => ParseError t e    -- ^ Parse error to render
  -> String            -- ^ Result of rendering
prettyError e =
  sourcePosStackPretty (errorPos e) <> ":" <> "\ESC[1;31m Error:\ESC[m\n\t" <>
    parseErrorTextPretty e
  

-- messageItemsPretty :: ShowErrorComponent a
--   => String            -- ^ Prefix to prepend
--   -> Set a             -- ^ Collection of messages
--   -> String            -- ^ Result of rendering
-- messageItemsPretty prefix ts
--   | Set.null ts = ""
--   | otherwise =
--     let f = orList . Set.toAscList . Set.map showErrorComponent
--     in prefix <> f ts <> "\n"


-- orList :: [String] -> String
-- orList (x:[])  = x
-- orList (x:[y]) = x <> " or " <> y
-- orList xs      = intercalate ", " (init xs) <> ", or " <> (last xs)
