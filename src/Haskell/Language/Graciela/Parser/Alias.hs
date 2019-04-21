{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE OverloadedStrings        #-}


module Language.Graciela.Parser.Alias
  ( alias
  ) where
--------------------------------------------------------------------------------
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
import qualified Data.Map                             as Map (insert, lookup)
import           Data.Text                            (Text, unpack)
import           Text.Megaparsec.Error                (sourcePosStackPretty)
import           Text.Megaparsec                      (getPosition, lookAhead,
                                                       optional)
--------------------------------------------------------------------------------

alias :: Parser ()
alias = do
  lookAhead $ match TokAlias
  from <- getPosition
  match TokAlias
  tname <- safeIdentifier
  match' TokAssign
  talias <- type'
  match' TokSemicolon
  case tname of
    Just n -> do
      possibleType <- getType n
      case possibleType of
        Nothing -> use userDefinedType >>= \u -> case Map.lookup n u of
          Nothing -> userDefinedType %= Map.insert n (GAlias n talias)
          Just t  -> putError from . UnknownError $ "Redefinition of type " <>  show t
        Just t -> putError from . UnknownError $ "Redefinition of type " <> show t
    Nothing -> pure ()
