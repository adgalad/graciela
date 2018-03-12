{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Language.Graciela.LLVM.Monad where
--------------------------------------------------------------------------------
import           Language.Graciela.Common
import           Language.Graciela.LLVM.State     hiding (State)
import qualified Language.Graciela.LLVM.State     as LLVM (State)
import {-# SOURCE #-} Language.Graciela.LLVM.Type      (llvmFunT)
--------------------------------------------------------------------------------

import           Control.Lens                     (at, ix, use, (%=), (+=),
                                                   (.=), (<<+=), (?=), _head)
import           Control.Monad.State.Class        (MonadState)
import           Control.Monad.Trans.State.Strict (State)
import           Data.Foldable                    (toList)
import           Data.Array                       ((!))
import qualified Data.Map.Strict                  as Map (empty, insert, lookup, toList)
import           Data.Maybe                       (fromMaybe)
import           Data.Sequence                    (Seq, (|>))
import qualified Data.Sequence                    as Seq
import           Data.Text                        (Text, unpack)
import           Data.Word                        (Word32)
import           LLVM.AST                         (BasicBlock (..))
import qualified LLVM.AST                         as LLVM (Definition (..))
import qualified LLVM.AST.Global                  as LLVM (Global (..), Parameter(..))
import qualified LLVM.AST.CallingConvention       as CC (CallingConvention (C))
import           LLVM.AST.Constant                (Constant (GlobalReference))
import           LLVM.AST.Instruction             (Named (..), Terminator (..))
import qualified LLVM.AST.Instruction             as LLVM (Instruction)
import           LLVM.AST.Instruction             (Instruction (..))
import           LLVM.AST.Name                    (Name(UnName), mkName)
import           LLVM.AST.Operand                 (Operand (ConstantOperand))
import           LLVM.AST.Type                    (Type)
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
    getVariableName' [] =  mkName "Error" --error
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
addDefinitions defs = forM_ defs addDefinition

addDefinition :: LLVM.Definition -> LLVM ()
addDefinition defs = do
  case defs of 
    LLVM.GlobalDefinition (f@LLVM.Function{}) ->
        let 
          retType    = LLVM.returnType f
          fName      = LLVM.name f
          params     = (\(p,_) -> p) $ LLVM.parameters f
          paramTypes = fmap (\(LLVM.Parameter t _ _) -> t) params
          funType    = llvmFunT retType paramTypes
        in functionsTypes %= Map.insert fName funType

    _ -> pure ()
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
      pure . mkName $ "." <> label
    Just i  -> do
      nameSupply . ix label %= succ
      pure . mkName $ "." <> label <> "." <> show i

newUnLabel :: LLVM Name
newUnLabel = UnName <$> (unnameSupply <<+= 1)
--------------------------------------------------------------------------------


getFilePathOperand :: String -> LLVM Operand
getFilePathOperand filePath = use stringIds >>= pure . Map.lookup (pack filePath) >>= \case 
  Just x -> use stringOps >>= pure . (flip (!) x)
  Nothing -> internal $ "Not file found: " <> filePath
--------------------------------------------------------------------------------

callable :: Type -> String -> Either a Operand
callable t = Right . ConstantOperand . GlobalReference t . mkName

callFunction :: String -> [Operand] -> LLVM Instruction
callFunction name args = do 
  let 
    fName'  = mkName name
    fName'' = mkName $ tail name
  f <- use functionsTypes 
  let
    (funT', fName) = if isNothing (Map.lookup fName' f)
      then (Map.lookup fName'' f, fName'')
      else (Map.lookup fName'  f, fName')

  case funT' of
    Nothing -> internal $ "Function " <> name <> " doesn't exists"
    Just funT -> do 
      pure Call
        { tailCallKind       = Nothing
        , callingConvention  = CC.C
        , returnAttributes   = []
        , function           = callable' funT fName
        , arguments          = (,[]) <$> args
        , functionAttributes = []
        , metadata           = [] }
  where 
    callable' t = Right . ConstantOperand . GlobalReference t


firstSetString = "_firstSet"
nextSetString  = "_nextSet"


copyArrayString = "_copyArray"


firstMultisetString = "_firstMultiset"
nextMultisetString  = "_nextMultiset"


firstSequenceString = "_firstSequence"
nextSequenceString  = "_nextSequence"


initTrashCollectorString = "_initTrashCollector"
freeTrashCollectorString = "_freeTrashCollector"
openScopeString = "_openScope"


newSetString = "_newSet"
newSeqString = "_newSequence"
newMultisetString = "_newMultiset"


newSetPairString      = "_newSetPair"
newMultisetPairString = "_newMultisetPair"
newSeqPairString      = "_newSequencePair"
newFunction           = "_newFunction"
newRelation           = "_newRelation"


equalSetString      = "_equalSet"
equalSeqString      = "_equalSequence"
equalMultisetString = "_equalMultiset"
equalFuncString     = "_equalFunction"
equalRelString      = "_equalRelation"
equalSetPairString      = "_equalSetPair"
equalSeqPairString      = "_equalSequencePair"
equalMultisetPairString = "_equalMultisetPair"
equalTupleString        = "_equalTuple"


evalFuncString     = "_pairFunction"
evalRelString      = "_pairRelation"


sizeSetString      = "_sizeSet"
sizeSeqString      = "_sizeSequence"
sizeMultisetString = "_sizeMultiset"
sizeRelString      = "_sizeRelation"
sizeFuncString     = "_sizeFunction"


supersetSetString      = "_includesSet"
supersetMultisetString = "_includesMultiset"
supersetSetPairString      = "_includesSetPair"
supersetMultisetPairString = "_includesMultisetPair"


ssupersetSetString      = "_includesSSet"
ssupersetMultisetString = "_includesSMultiset"
ssupersetSetPairString  = "_includesSSetPair"
ssupersetMultisetPairString = "_includesSMultisetPair"


insertSetString      = "_insertSet"
insertSeqString      = "_insertSequence"
insertMultisetString = "_insertMultiset"


insertSetPairString      = "_insertSetPair"
insertMultisetPairString = "_insertMultisetPair"
insertSeqPairString      = "_insertSequencePair"



isElemSetString      = "_isElemSet"
isElemMultisetString = "_isElemMultiset"
isElemSeqString      = "_isElemSequence"
isElemSetPairString      = "_isElemSetPair"
isElemMultisetPairString = "_isElemMultisetPair"
isElemSeqPairString      = "_isElemSequencePair"



unionSetString          = "_unionSet"
unionMultisetString     = "_unionMultiset"
unionSetPairString      = "_unionSetPair"
unionMultisetPairString = "_unionMultisetPair"
unionFunctionString     = "_unionFunction"


intersectSetString          = "_intersectSet"
intersectMultisetString     = "_intersectMultiset"
intersectSetPairString      = "_intersectSetPair"
intersectMultisetPairString = "_intersectMultisetPair"
intersectFunctionString     = "_intersectFunction"


differenceSetString          = "_differenceSet"
differenceMultisetString     = "_differenceMultiset"
differenceSetPairString      = "_differenceSetPair"
differenceMultisetPairString = "_differenceMultisetPair"
differenceFunctionString     = "_differenceFunction"


multisetSumString        = "_sumMultiset"
concatSequenceString     = "_concatSequence"
multisetPairSumString    = "_sumMultisetPair"
concatSequencePairString = "_concatSequencePair"
atSequenceString         = "_atSequence"
atSequencePairString     = "_atSequencePair"



freeString = "_free"


mallocString   = "_malloc"
-- mallocTCString = "_mallocTC"


addPointerString    = "_addPointer"
removePointerString = "_removePointer"
derefPointerString  = "_derefPointer"


lnString      = "_ln"
writeIString  = "_writeInt"
writeBString  = "_writeBool"
writeCString  = "_writeChar"
writeFString  = "_writeDouble"
writeSString  = "_writeString"
writePString  = "_writePointer"


randomInt     = "_random"


sqrtString    = "llvm.sqrt.f64"
fabsString    = "llvm.fabs.f64"
powString     = "llvm.pow.f64"
powIString  = "_powInt"


minnumString  = "_min"
maxnumString  = "_max"
minnumFstring = "_minF"
maxnumFstring = "_maxF"

readIntStd    = "_readIntStd"
readBoolStd   = "_readBoolStd"
readCharStd   = "_readCharStd"
readFloatStd  = "_readDoubleStd"

openFileStr   = "_openFile"
readFileBool  = "_readFileBool"
readFileInt   = "_readFileInt"
closeFileStr  = "_closeFile"
readFileChar  = "_readFileChar"
readFileFloat = "_readFileDouble"


safeAdd, safeSub, safeMul :: Word32 -> String
safeAdd n = "llvm.sadd.with.overflow.i" <> show n
safeSub n = "llvm.ssub.with.overflow.i" <> show n
safeMul n = "llvm.smul.with.overflow.i" <> show n

