{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM.Monad where
--------------------------------------------------------------------------------
import           LLVM.State                   hiding (State)
import qualified LLVM.State                   as LLVM (State)
--------------------------------------------------------------------------------
import           Control.Lens                 (use, (%=), (.=), (<<+=))
import           Control.Monad.State          (MonadState, State)
import           Data.Foldable                (toList)
import           Data.Monoid                  ((<>))
import           Data.Sequence                (Seq, (|>))
import qualified Data.Sequence                as Seq
import           LLVM.General.AST             (BasicBlock (..))
import qualified LLVM.General.AST             as LLVM (Definition (..))
import           LLVM.General.AST.Instruction (Named (..), Terminator (..))
import qualified LLVM.General.AST.Instruction as LLVM (Instruction (..))
import           LLVM.General.AST.Name        (Name (..))
--------------------------------------------------------------------------------

type Inst  = Named LLVM.Instruction
type Insts = Seq Inst

newtype LLVM a = LLVM { unLLVM :: State LLVM.State a }
  deriving (Functor, Applicative, Monad, MonadState LLVM.State)


addDefinitions :: Seq LLVM.Definition -> LLVM ()
addDefinitions defs =
  moduleDefs %= (<> defs)

addDefinition :: LLVM.Definition -> LLVM ()
addDefinition defs =
  moduleDefs %= (|> defs)

addInstructions :: Insts -> LLVM ()
addInstructions insts =
  currentBlock %= (<> insts)

addInstruction :: Inst -> LLVM ()
addInstruction inst =
  currentBlock %= (|> inst)

addBlock :: Named Terminator -> LLVM ()
addBlock terminator = do
  terminate terminator
  name' <- newLabel
  blockName .= Just name'


terminate :: Named Terminator -> LLVM ()
terminate terminator = do
  name' <- use blockName
  case name' of
    Nothing -> error "internal error: attempted to terminate an unnamed block"
    Just name -> do
      insts <- use currentBlock
      currentBlock .= Seq.empty
      blocks %= (|> BasicBlock name (toList insts) terminator)
      blockName .= Nothing


terminate' :: Terminator -> LLVM ()
terminate' = terminate . Do


(#) :: Name -> LLVM ()
(#) name = do
  old <- use blockName
  case old of
    Nothing -> blockName .= Just name
    Just _  -> error "internal error: attempted to rename a block"


-- setLabel :: Name -> Named Terminator -> LLVM ()
-- setLabel name terminator = do
--   addBlock terminator
--   blockName .= Just name

newLabel :: LLVM Name
newLabel = UnName <$> (insCount <<+= 1)

writeLnInt     = "_writeLnInt"
writeLnBool    = "_writeLnBool"
writeLnChar    = "_writeLnChar"
writeLnFloat   = "_writeLnDouble"
writeLnString  = "_writeLnString"
writeInt       = "_writeInt"
writeBool      = "_writeBool"
writeChar      = "_writeChar"
writeFloat     = "_writeDouble"
writeString    = "_writeString"
randomInt      = "_random"
sqrtString     = "llvm.sqrt.f64"
fabsString     = "llvm.fabs.f64"
powString      = "llvm.pow.f64"
minnumString   = "_min"
maxnumString   = "_max"
minnumFstring  = "_minF"
maxnumFstring  = "_maxF"
readIntStd     = "_readIntStd"
readCharStd    = "_readCharStd"
readFloatStd   = "_readDoubleStd"
openFileStr    = "_openFile"
readFileInt    = "_readFileInt"
closeFileStr   = "_closeFile"
readFileChar   = "_readFileChar"
readFileFloat  = "_readFileDouble"
intAdd         = "llvm.sadd.with.overflow.i32"
intSub         = "llvm.ssub.with.overflow.i32"
intMul         = "llvm.smul.with.overflow.i32"