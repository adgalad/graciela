{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Graciela.AST.Module where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Definition  (Definition)
import           Language.Graciela.AST.Expression  hiding (loc)
import           Language.Graciela.AST.Instruction (Instruction)
import           Language.Graciela.AST.Struct      (Struct)
import           Language.Graciela.Pragma          (Pragma)
import           Language.Graciela.AST.Type
import           Language.Graciela.Common
import           Language.Graciela.SymbolTable
import           Language.Graciela.Token
--------------------------------------------------------------------------------
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map (elems, foldrWithKey)
import           Data.Sequence                     (Seq)
import           Data.Set                          (Set)
import           Data.Text                         (Text, unpack)
--------------------------------------------------------------------------------

data Module
  = Module
    { name        :: Text
    , loc         :: Location
    , defs        :: Map Text Definition
    , structs     :: Map Text Struct
    , fullStructs :: Map Text (Struct, Map TypeArgs Bool)
    , pragmas     :: Set Pragma
    , strings     :: Map Text Int }
    deriving (Generic, Serialize)


instance Treelike Module where
  toTree Module { name, loc, defs, structs, strings } =
    Node ("Module " <> unpack name <> " " <> show loc)
      [ Node "Structs" (toTree <$> Map.elems structs)
      , Node "Definitions" (toForest defs)
      , Node "Strings" $ stringsNode strings
      ]

    where
      stringsNode = Map.foldrWithKey aux []
      aux k v f = (: f) (Node (unpack k) [leaf $ show v])

