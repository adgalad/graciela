{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Language.Graciela.LLVM.State
  ( State (..)  
  , initialState
  , nameSupply
  , unnameSupply
  , blockName
  , currentBlock
  , blocks
  , moduleDefs
  , symTable
  , structs
  , fullDataTypes
  , functionsTypes
  , freeArgInsts
  , currentStruct
  , stringIds
  , stringOps
  , boundOp
  , substitutionTable
  , mpragmas
  , doGet
  , coupling
  , evalAssertions
  ) where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Struct (Struct (..))
import           Language.Graciela.AST.Type   (TypeArgs)
import           Language.Graciela.Pragma
import           Language.Graciela.Common
--------------------------------------------------------------------------------
import           Control.Lens                 (makeLenses)
import           Data.Array                   (Array)
import qualified Data.Map.Strict              as Map (empty)
import qualified Data.Sequence                as Seq
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import           LLVM.AST             (BasicBlock (..), Definition (..))
import           LLVM.AST.Instruction (Instruction (..), Named (..))
import           LLVM.AST.Name        (Name (..))
import           LLVM.AST.Operand     (Operand)
import qualified LLVM.AST.Type        as LLVM

--------------------------------------------------------------------------------

type Inst  = Named Instruction

data State = State
  { _nameSupply        :: Map String Word
  , _unnameSupply      :: Word
  , _blockName         :: Maybe Name              -- Cantidad de bloques básicos en el programa
  , _currentBlock      :: Seq (Named Instruction) -- Lista de instrucciones en el bloque básico actual
  , _freeArgInsts      :: Seq (Named Instruction)
  , _blocks            :: Seq BasicBlock          -- Lista de bloques básicos en la definición actual
  , _moduleDefs        :: Seq Definition
  , _symTable          :: [Map Text Name]
  , _structs           :: Map Text Struct
  , _fullDataTypes     :: Map Text (Struct, Map TypeArgs Bool)
  , _functionsTypes    :: Map Name LLVM.Type
  , _currentStruct     :: Maybe Struct
  , _stringIds         :: Map Text Int
  , _stringOps         :: Array Int Operand
  , _boundOp           :: Maybe Operand
  , _substitutionTable :: [TypeArgs]
  , _mpragmas          :: Set Pragma
  , _doGet             :: Bool
  , _coupling          :: Bool
  , _evalAssertions    :: Bool }


makeLenses ''State

initialState :: State
initialState = State
  { _nameSupply        = Map.empty
  , _unnameSupply      = 1
  , _blockName         = Nothing
  , _currentBlock      = Seq.empty
  , _freeArgInsts      = Seq.empty
  , _blocks            = Seq.empty
  , _moduleDefs        = Seq.empty
  , _symTable          = []
  , _structs           = Map.empty
  , _fullDataTypes     = Map.empty
  , _functionsTypes    = Map.empty
  , _currentStruct     = Nothing
  , _stringIds         = Map.empty
  , _stringOps         = undefined
  , _boundOp           = Nothing
  , _substitutionTable = []
  , _mpragmas          = Set.empty
  , _doGet             = True
  , _coupling          = False
  , _evalAssertions    = True }
