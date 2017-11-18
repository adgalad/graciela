{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Language.Graciela.LLVM.Monad where
--------------------------------------------------------------------------------
import           Language.Graciela.Common
import           Language.Graciela.LLVM.State     hiding (State)
import qualified Language.Graciela.LLVM.State     as LLVM (State)
--------------------------------------------------------------------------------

import           Control.Lens                     (at, ix, use, (%=), (+=),
                                                   (.=), (<<+=), (?=), _head)
import           Control.Monad.State.Class        (MonadState)
import           Control.Monad.Trans.State.Strict (State)
import           Data.Foldable                    (toList)
import           Data.Array                       ((!))
import qualified Data.Map.Strict                  as Map (empty, insert, lookup)
import           Data.Maybe                       (fromMaybe)
import           Data.Sequence                    (Seq, (|>))
import qualified Data.Sequence                    as Seq
import           Data.Text                        (Text, unpack)
import           Data.Word                        (Word32)
import           LLVM.General.AST                 (BasicBlock (..))
import qualified LLVM.General.AST                 as LLVM (Definition (..))
import           LLVM.General.AST.Constant        (Constant (GlobalReference))
import           LLVM.General.AST.Instruction     (Named (..), Terminator (..))
import qualified LLVM.General.AST.Instruction     as LLVM (Instruction (..))
import           LLVM.General.AST.Name            (Name (..))
import           LLVM.General.AST.Operand         (Operand (ConstantOperand))
import           LLVM.General.AST.Type            (Type)
--------------------------------------------------------------------------------

type Inst  = Named LLVM.Instruction
type Insts = Seq Inst

newtype LLVM a = LLVM { unLLVM :: State LLVM.State a }
  deriving ( Functor, Applicative, Monad, MonadState LLVM.State)

{- Symbol Table -}
-- When opening a new scope, llvm wont know which variable is being called
-- if more than 1 variable have the same name. To prevent this confusion,
-- lets call every declared variable with an unique name + identifier
-- (e.g. a -> %var.a1 and %var.a2)

getVariableName :: Text -> LLVM Name
getVariableName name =
  getVariableName' <$> use symTable
  where
    getVariableName' [] =  Name "Error" --error
      -- "internal error: undefined variable `" <> unpack name <> "`."

    getVariableName' (vars:xs) =
      fromMaybe (getVariableName' xs) (name `Map.lookup` vars)


openScope :: LLVM ()
openScope = symTable %= (Map.empty :)


closeScope :: LLVM ()
closeScope = symTable %= tail


insertVar :: Text -> LLVM Name
insertVar text = do
  name <- newLabel ("var." <> unpack text)
  symTable . _head %= Map.insert text name
  pure name
--------------------------------------------------------------------------------

addDefinitions :: [LLVM.Definition] -> LLVM ()
addDefinitions defs =
  moduleDefs %= (<> Seq.fromList defs)

addDefinition :: LLVM.Definition -> LLVM ()
addDefinition defs =
  moduleDefs %= (|> defs)

addInstructions :: Insts -> LLVM ()
addInstructions insts =
  currentBlock %= (<> insts)

addInstruction :: Inst -> LLVM ()
addInstruction inst =
  currentBlock %= (|> inst)

addArgInsts :: Inst -> LLVM ()
addArgInsts inst =
  freeArgInsts %= (|> inst)
--------------------------------------------------------------------------------

terminate :: Terminator -> LLVM ()
terminate terminator = do
  name' <- use blockName
  case name' of
    Nothing -> internal $
      "attempted to terminate an unnamed block with\n" <>
      show (Do terminator) <> "\n"
    Just name -> do
      insts <- use currentBlock
      blocks %= (|> BasicBlock name (toList insts) (Do terminator))
      currentBlock .= Seq.empty
      blockName .= Nothing

(#) :: Name -> LLVM ()
(#) name = do
  old <- use blockName
  case old of
    Nothing -> blockName .= Just name
    Just oldName  -> internal $
      "attempted to rename current bloc, " <> show oldName <>

      " as " <> show name <> "."
--------------------------------------------------------------------------------

newLabel :: String -> LLVM Name
newLabel "" = internal "empty label, use newUnLabel"
newLabel label = do
  ns <- use nameSupply
  case label `Map.lookup` ns of
    Nothing -> do
      nameSupply . at label ?= 1
      pure . Name $ "." <> label
    Just i  -> do
      nameSupply . ix label %= succ
      pure . Name $ "." <> label <> "." <> show i

newUnLabel :: LLVM Name
newUnLabel = UnName <$> (unnameSupply <<+= 1)
--------------------------------------------------------------------------------


getFilePathOperand :: String -> LLVM Operand
getFilePathOperand filePath = use stringIds >>= pure . Map.lookup (pack filePath) >>= \case 
  Just x -> use stringOps >>= pure . (flip (!) x)
  Nothing -> internal $ "Not file found: " <> filePath
--------------------------------------------------------------------------------

callable :: Type -> String -> Either a Operand
callable t = Right . ConstantOperand . GlobalReference t . Name

firstSetString, nextSetString :: String
firstSetString = "___firstSet"
nextSetString = "___nextSet"

copyArrayString :: String
copyArrayString = "___copyArray"

firstMultisetString, nextMultisetString :: String
firstMultisetString = "___firstMultiset"
nextMultisetString = "___nextMultiset"

firstSequenceString, nextSequenceString :: String
firstSequenceString = "___firstSequence"
nextSequenceString = "___nextSequence"

initTrashCollectorString, freeTrashCollectorString, openScopeString :: String
initTrashCollectorString = "___initTrashCollector"
freeTrashCollectorString = "___freeTrashCollector"
openScopeString = "___openScope"

newSetString, newSeqString, newMultisetString :: String
newSetString = "___newSet"
newSeqString = "___newSequence"
newMultisetString = "___newMultiset"

newSetPairString,newMultisetPairString, newSeqPairString, newFunction, newRelation :: String
newSetPairString      = "___newSetPair"
newMultisetPairString = "___newMultisetPair"
newSeqPairString      = "___newSequencePair"
newFunction           = "___newFunction"
newRelation           = "___newRelation"

equalSetString, equalSeqString, equalMultisetString :: String
equalSetString      = "___equalSet"
equalSeqString      = "___equalSequence"
equalMultisetString = "___equalMultiset"
equalFuncString     = "___equalFunction"
equalRelString      = "___equalRelation"
equalSetPairString      = "___equalSetPair"
equalSeqPairString      = "___equalSequencePair"
equalMultisetPairString = "___equalMultisetPair"
equalTupleString        = "___equalTuple"

evalFuncString, evalRelString :: String
evalFuncString     = "___pairFunction"
evalRelString      = "___pairRelation"

sizeSetString, sizeSeqString, sizeMultisetString, sizeRelString, sizeFuncString :: String
sizeSetString      = "___sizeSet"
sizeSeqString      = "___sizeSequence"
sizeMultisetString = "___sizeMultiset"
sizeRelString      = "___sizeRelation"
sizeFuncString     = "___sizeFunction"

supersetSetString, supersetMultisetString :: String
supersetSetString      = "___includesSet"
supersetMultisetString = "___includesMultiset"
supersetSetPairString      = "___includesSetPair"
supersetMultisetPairString = "___includesMultisetPair"

ssupersetSetString, ssupersetMultisetString :: String
ssupersetSetString      = "___includesSSet"
ssupersetMultisetString = "___includesSMultiset"
ssupersetSetPairString  = "___includesSSetPair"
ssupersetMultisetPairString = "___includesSMultisetPair"

insertSetString, insertSeqString, insertMultisetString :: String
insertSetString      = "___insertSet"
insertSeqString      = "___insertSequence"
insertMultisetString = "___insertMultiset"

insertSetPairString, insertMultisetPairString, insertSeqPairString:: String
insertSetPairString      = "___insertSetPair"
insertMultisetPairString = "___insertMultisetPair"
insertSeqPairString      = "___insertSequencePair"


isElemSetString, isElemMultisetString, isElemSeqString :: String
isElemSetString      = "___isElemSet"
isElemMultisetString = "___isElemMultiset"
isElemSeqString      = "___isElemSequence"
isElemSetPairString      = "___isElemSetPair"
isElemMultisetPairString = "___isElemMultisetPair"
isElemSeqPairString      = "___isElemSequencePair"


unionSetString, unionMultisetString :: String
unionSetString          = "___unionSet"
unionMultisetString     = "___unionMultiset"
unionSetPairString      = "___unionSetPair"
unionMultisetPairString = "___unionMultisetPair"
unionFunctionString     = "___unionFunction"

intersectSetString, intersectMultisetString :: String
intersectSetString          = "___intersectSet"
intersectMultisetString     = "___intersectMultiset"
intersectSetPairString      = "___intersectSetPair"
intersectMultisetPairString = "___intersectMultisetPair"
intersectFunctionString     = "___intersectFunction"

differenceSetString, differenceMultisetString :: String
differenceSetString          = "___differenceSet"
differenceMultisetString     = "___differenceMultiset"
differenceSetPairString      = "___differenceSetPair"
differenceMultisetPairString = "___differenceMultisetPair"
differenceFunctionString     = "___differenceFunction"

multisetSumString, concatSequenceString :: String
multisetSumString        = "___sumMultiset"
concatSequenceString     = "___concatSequence"
multisetPairSumString    = "___sumMultisetPair"
concatSequencePairString = "___concatSequencePair"
atSequenceString         = "___atSequence"
atSequencePairString     = "___atSequencePair"


freeString :: String
freeString = "___free"

mallocString :: String
mallocString   = "___malloc"
-- mallocTCString = "___mallocTC"

addPointerString, removePointerString, derefPointerString :: String
addPointerString    = "___addPointer"
removePointerString = "___removePointer"
derefPointerString  = "___derefPointer"

lnString      :: String
lnString      = "___ln"
writeIString  :: String
writeIString  = "___writeInt"
writeBString  :: String
writeBString  = "___writeBool"
writeCString  :: String
writeCString  = "___writeChar"
writeFString  :: String
writeFString  = "___writeDouble"
writeSString  :: String
writeSString  = "___writeString"
writePString  :: String
writePString  = "___writePointer"

randomInt     :: String
randomInt     = "___random"

sqrtString    :: String
sqrtString    = "llvm.sqrt.f64"
fabsString    :: String
fabsString    = "llvm.fabs.f64"
powString     :: String
powString     = "llvm.pow.f64"
powIString  :: String
powIString  = "___powInt"

minnumString  :: String
minnumString  = "___min"
maxnumString  :: String
maxnumString  = "___max"
minnumFstring :: String
minnumFstring = "___minF"
maxnumFstring :: String
maxnumFstring = "___maxF"

readIntStd    :: String
readIntStd    = "___readIntStd"
readBoolStd   :: String
readBoolStd   = "___readBoolStd"
readCharStd   :: String
readCharStd   = "___readCharStd"
readFloatStd  :: String
readFloatStd  = "___readDoubleStd"

openFileStr   :: String
openFileStr   = "___openFile"
readFileBool  :: String
readFileBool  = "___readFileBool"
readFileInt   :: String
readFileInt   = "___readFileInt"
closeFileStr  :: String
closeFileStr  = "___closeFile"
readFileChar  :: String
readFileChar  = "___readFileChar"
readFileFloat :: String
readFileFloat = "___readFileDouble"


safeAdd       :: Word32 -> String
safeAdd n     = "llvm.sadd.with.overflow.i" <> show n
safeSub       :: Word32 -> String
safeSub n     = "llvm.ssub.with.overflow.i" <> show n
safeMul       :: Word32 -> String
safeMul n     = "llvm.smul.with.overflow.i" <> show n
