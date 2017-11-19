{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators  #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}

module Language.Graciela.LLVM.Instruction where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Expression   (Expression (..),
                                                     Expression' (..))
import qualified Language.Graciela.AST.Expression   as E (loc)
import           Language.Graciela.AST.Instruction  (Guard, Instruction (..),
                                                     Instruction' (..))
import qualified Language.Graciela.AST.Instruction  as G (Instruction)
import qualified Language.Graciela.AST.Declaration  as G (Declaration(..))
import           Language.Graciela.AST.Object       hiding (indices)
import qualified Language.Graciela.AST.Object       as O (inner, loc)
import           Language.Graciela.AST.Struct       (Struct (..))
import           Language.Graciela.AST.Type         as T
import           Language.Graciela.Common
import           Language.Graciela.LLVM.Abort       (abort)
import qualified Language.Graciela.LLVM.Abort       as Abort (Abort (..))
import           Language.Graciela.LLVM.Boolean
import           Language.Graciela.LLVM.Declaration
import           Language.Graciela.LLVM.Expression
import           Language.Graciela.LLVM.Monad
import           Language.Graciela.LLVM.Object      (objectRef)
import           Language.Graciela.LLVM.State
import           Language.Graciela.LLVM.Type
import           Language.Graciela.LLVM.Warning     (warn)
import qualified Language.Graciela.LLVM.Warning     as Warning (Warning (Manual))
--------------------------------------------------------------------------------
import           Control.Lens                       (use, (%=), (-=), (.=))
import           Data.Maybe                         (fromMaybe)
import           Data.Sequence                      (ViewR ((:>)))
import qualified Data.Sequence                      as Seq (empty, fromList,
                                                            singleton, viewr,
                                                            zip, (|>))
import           Data.Text                          (Text)
import           Data.Word
import           LLVM.General.AST                   (BasicBlock (..))
import           LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.CallingConvention as CC (CallingConvention (C))
import qualified LLVM.General.AST.Constant          as C (Constant (..))
import           LLVM.General.AST.Instruction       (FastMathFlags (..),
                                                     Instruction (..),
                                                     Named (..),
                                                     Terminator (..))
import qualified LLVM.General.AST.Instruction       as LLVM (Instruction)
import           LLVM.General.AST.IntegerPredicate  (IntegerPredicate (..))
import           LLVM.General.AST.Name              (Name (..))
import           LLVM.General.AST.Operand           (MetadataNode(..) ,CallableOperand, 
                                                     Operand (..))
import           LLVM.General.AST.Type              hiding (void)
--------------------------------------------------------------------------------

guard :: Name -> Name -> Guard -> LLVM Name
guard finish checkLabel (expr, decls, insts) = do
  (checkLabel #)

  yes <- newLabel "instGuardYes"
  no  <- newLabel "instGuardNo"

  condition <- boolean yes no expr

  (yes #)
  openScope
  mapM_ declaration decls
  mapM_ instruction insts
  terminate Br
    { dest      = finish
    , metadata' = [] }

  closeScope
  pure no


copyArray :: T.Type -> Operand -> Operand -> LLVM ()
copyArray t@GArray{dimensions, innerType} sourceHeader destHeader = do
  innerSize <- sizeOf innerType
  inner <- toLLVMType innerType
  type' <- toLLVMType t
  let
    mulDims op1 op2 = do
      label <- newUnLabel
      addInstruction $ label := Mul
        { nsw = False
        , nuw = False
        , operand0 = op1
        , operand1 = op2
        , metadata = [] }
      pure $ LocalReference intType label


  exprs <- mapM expression dimensions
  sizeOp <- foldM mulDims (constantOperand GInt . Left $1) exprs
  let sizeT = constantOperand GInt . Left $innerSize


  sourceArrPtr <- newLabel "sourceArrPtr"
  destArrPtr   <- newLabel "destArrPtr"
  addInstruction $ sourceArrPtr := GetElementPtr
    { inBounds = False
    , address  = sourceHeader
    , indices  = constantOperand GInt . Left <$> [0, fromIntegral $ length dimensions]
    , metadata = [] }


  addInstruction $ destArrPtr := GetElementPtr
    { inBounds = False
    , address  = destHeader
    , indices  = constantOperand GInt . Left <$> [0, fromIntegral $ length dimensions]
    , metadata = [] }

  loadSourceArray <- newLabel "loadSourceArray"
  loadDestArray   <- newLabel "loadDestArray"
  sourceArray <- newLabel "sourceArray"
  destArray   <- newLabel "destArray"


  addInstruction $ loadSourceArray := Load
    { volatile  = False
    , address   = LocalReference (ptr type') sourceArrPtr
    , maybeAtomicity = Nothing
    , alignment = 4
    , metadata  = [] }

  addInstruction $ loadDestArray :=  Load
    { volatile  = False
    , address   = LocalReference (ptr type') destArrPtr
    , maybeAtomicity = Nothing
    , alignment = 4
    , metadata  = [] }

  addInstruction $ sourceArray := BitCast
    { operand0 = LocalReference (ptr type') loadSourceArray
    , type'    = pointerType
    , metadata = [] }

  addInstruction $ destArray :=  BitCast
    { operand0 = LocalReference (ptr type') loadDestArray
    , type'    = pointerType
    , metadata = [] }

  addInstruction $ Do Call
    { tailCallKind       = Nothing
    , callingConvention  = CC.C
    , returnAttributes   = []
    , function           = callable voidType copyArrayString
    , arguments          = (,[]) <$> [ sizeOp
                                     , LocalReference (ptr inner) sourceArray
                                     , LocalReference (ptr inner) destArray
                                     , sizeT ]
    , functionAttributes = []
    , metadata           = [] }


instruction :: G.Instruction -> LLVM ()
instruction i@Instruction {instLoc=Location(pos, _), inst' = ido} = case ido of
  Abort -> do
    abort Abort.Manual pos
    newLabel "unreachable" >>= (#)

  Warn -> do
    warn Warning.Manual pos

  Assertion expr -> do
    -- Create both labels
    trueLabel  <- newLabel "assertTrue"
    falseLabel <- newLabel "assertFalse"
    -- Evaluate the condition expression using short-circuit
    boolean trueLabel falseLabel expr
    -- Set the false label to the abort
    -- And the true label to the next instructions
    (falseLabel #)
    abort Abort.Assert pos

    (trueLabel #)


  Assign { assignPairs } -> do
    -- get the values first
    values <- mapM expression' exprs
    -- then store them
    zipWithM_ assign' lvals values
    -- this way, things like `a, b := b, a` just work (tm).

    where
      (lvals, exprs) = unzip . toList $ assignPairs

      assign' lval value = do
        doGet .= False
        ref <- objectRef lval
        addInstruction $ Do Store
          { volatile       = False
          , address        = ref
          , value
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }


  Conditional { cguards } -> do
    entry  <- newLabel "ifExpEntry"
    finish <- newLabel "ifExpFinish"

    terminate Br
      { dest      = entry
      , metadata' = [] }

    abortLabel <- foldM (guard finish) entry cguards

    (abortLabel #)
    abort Abort.If pos

    (finish #)


  Block decls insts -> do
    block <- newLabel "blockEntry"
    exit <- newLabel "blockExit"
    terminate Br
      { dest      = block
      , metadata' = [] }

    (block #)
    openScope
    mapM_ declaration decls
    mapM_ instruction insts
    
    forM_ decls $ \case 
      G.Declaration {G.declType, G.declIds} -> 
        let 
          aux typeName typeArgs = do
            let fName = "destroy" <> llvmName typeName (toList typeArgs)
            forM_ declIds $ \vName -> do
              name <- getVariableName vName

              addInstruction $ Do Call
                { tailCallKind       = Nothing
                , callingConvention  = CC.C
                , returnAttributes   = []
                , function           = callable voidType fName
                , arguments          = [(LocalReference pointerType name, [])]
                , functionAttributes = []
                , metadata = [] }
        in case declType of 
          GDataType{typeName, dtTypeArgs} -> aux typeName dtTypeArgs
          GAlias _ GDataType{typeName, dtTypeArgs} -> aux typeName dtTypeArgs
          _ -> pure ()
      _ -> pure ()
    closeScope
    terminate Br
      { dest      = exit
      , metadata' = [] }

    (exit #)

  ProcedureCall { pName, pArgs, pStructArgs, pRecursiveCall, pRecursiveProc = prp } -> do
    args <- toList <$> mapM createArg pArgs

    pName' <- case pStructArgs of
      Just (structBaseName, typeArgs) -> do
        t' <- mapM fill (toList typeArgs)
        pure . ('$':) $ llvmName (pName <> pack "-" <> structBaseName) t'

      _ -> pure . ('$':) . unpack $ pName
    asserts <- use evalAssertions
    recArgs <- fmap (,[]) <$> if pRecursiveCall && asserts 
        then do
          use boundOp >>= pure . \case 
            Nothing -> []
            Just boundOperand -> [constantOperand GBool . Left $ 1, boundOperand]
        else if prp && asserts
          then pure [constantOperand GBool . Left $ 0, constantOperand GInt . Left $0]
      else pure []

    addInstruction $ Do Call
      { tailCallKind       = Nothing
      , callingConvention  = CC.C
      , returnAttributes   = []
      , function           = callable voidType pName'
      , arguments          = recArgs <> args
      , functionAttributes = []
      , metadata           = [] }

    zipWithM_ copyOutArgs (toList pArgs) args

    argInsts <- use freeArgInsts
    addInstructions argInsts
    freeArgInsts .= Seq.empty

    where
      primaObject :: Object -> Object
      primaObject obj@Object{ obj' } = case obj' of
         v@Variable{ name } -> obj{ obj' = v { name = name <> "_"}}
         o                  -> primaObject (O.inner o)

      objectName :: Object -> Text
      objectName v@Object{ obj' } = case obj' of
         Variable{ name } -> name <> "_"
         o                -> objectName (O.inner o)

      copyOutArgs :: (Expression, ArgMode) -> (Operand,[t]) -> LLVM ()
      copyOutArgs (e@Expression{expType, exp'}, mode) (operand, _) = do
        if mode `elem` [InOut, Out]
          then do
            let
              Obj o = exp'
            destPtr <- objectRef o
            type'   <- toLLVMType expType
            let 
              doDT t = do
                types <- mapM fill (toList . dtTypeArgs  $ t)

                let
                  copyFunc = "copy" <> llvmName (typeName t) types
                addInstruction $ Do Call
                  { tailCallKind       = Nothing
                  , callingConvention  = CC.C
                  , returnAttributes   = []
                  , function           = callable voidType copyFunc
                  , arguments          = (,[]) <$> [ operand
                                                   , destPtr ]
                  , functionAttributes = []
                  , metadata           = [] }

            case expType of
              t@GArray{} -> do
                copyArray t operand destPtr

              t@GDataType{} -> doDT t

              GAlias _ t@GDataType{} -> doDT t

              _ -> do
                value <- newLabel "value"
                addInstruction $value := Load
                  { volatile  = False
                  , address   = operand
                  , maybeAtomicity = Nothing
                  , alignment = 4
                  , metadata  = [] }

                addInstruction $ Do Store
                  { volatile = False
                  , address  = destPtr
                  , value    = LocalReference type' value
                  , maybeAtomicity = Nothing
                  , alignment = 4
                  , metadata  = [] }
          else pure ()



      createArg :: (Expression, ArgMode) -> LLVM (Operand,[t])
      createArg (e@Expression{expType = expt, exp', E.loc}, mode) = do

        expType <- fill expt
        type' <- toLLVMType expType
        let
          isIn = mode `elem` [In, InOut, Const]
          Location(SourcePos f l c, _) = loc
          line = constantOperand GInt . Left . fromIntegral $ unPos l
          col  = constantOperand GInt . Left . fromIntegral $ unPos c
        filePath <- getFilePathOperand f
        case mode of
          Ref -> do
            label <- newLabel "argCastRef"
            ref   <- objectRef (theObj exp')

            addInstruction $ label := BitCast
              { operand0 = ref
              , type'    = ptr type'
              , metadata = [] }
            pure $ (LocalReference (ptr type') label, [])

          _ -> case exp' of
            Obj o | not (mode `elem` [In, Const] && not (expType =:= T.GOneOf [T.GADataType, T.GAArray]))-> do
              prima <- insertVar (objectName o)
              addInstruction $ prima := Alloca
                { allocatedType = ptr type'
                , numElements   = Nothing
                , alignment     = 4
                , metadata      = [] }

              instruction $ Instruction
                { instLoc = gracielaDef
                , inst'   = New
                  { idName = primaObject o
                  , nType  = expType } }

              primaPtr <- newLabel "primaPtr"
              addInstruction $ primaPtr := Load
                { volatile  = False
                , address   = LocalReference (ptr type') prima
                , maybeAtomicity = Nothing
                , alignment = 4
                , metadata  = [] }

              primaRef <- case expType of
                t@GArray{dimensions} -> do

                  when isIn $ do
                    sourceHeader <- objectRef o
                    copyArray t sourceHeader (LocalReference type' primaPtr)

                  internalArrPtr <- newLabel "internalArrayPtr"

                  addArgInsts $ internalArrPtr := GetElementPtr
                    { inBounds = False
                    , address  = (LocalReference type' primaPtr)
                    , indices  = constantOperand GInt . Left <$> [0, fromIntegral $ length dimensions]
                    , metadata = [] }

                  internalArr <- newLabel "internalArray"
                  addArgInsts $ internalArr := Load
                    { volatile  = False
                    , address   = LocalReference (ptr type') internalArrPtr
                    , maybeAtomicity = Nothing
                    , alignment = 4
                    , metadata  = [] }

                  castToFree <- newLabel "castToFree"
                  addArgInsts $ castToFree := BitCast
                    { operand0 = LocalReference type' internalArr
                    , type'    = pointerType
                    , metadata = [] }
                  
                  addArgInsts $ Do Call
                    { tailCallKind       = Nothing
                    , callingConvention  = CC.C
                    , returnAttributes   = []
                    , function           = callable voidType freeString
                    , arguments          = [ (LocalReference pointerType castToFree,[])
                                           , (filePath, []), (line,[]),(col,[])]
                    , functionAttributes = []
                    , metadata           = [] }

                  if isIn
                    then pure (LocalReference type' primaPtr,[])
                    else pure $ (LocalReference (ptr type') primaPtr,[])

                t | t =:= GADataType -> do
                  t <- pure $ t <> GADataType
                  types <- mapM fill (toList . dtTypeArgs $ t)

                  let
                    postfix = llvmName (typeName t) types

                  destStructPtr <- newLabel "destStructPtr"
                  addInstruction $ destStructPtr := Load
                    { volatile  = False
                    , address   = LocalReference (ptr type') prima
                    , maybeAtomicity = Nothing
                    , alignment = 4
                    , metadata  = [] }

                  when (isIn) $ do
                    sourceStructPtr  <- objectRef o


                    addInstruction $ Do Call
                      { tailCallKind       = Nothing
                      , callingConvention  = CC.C
                      , returnAttributes   = []
                      , function           = callable voidType $ "copy" <> postfix
                      , arguments          = (,[]) <$> [ sourceStructPtr
                                                       , LocalReference (ptr type') destStructPtr ]
                      , functionAttributes = []
                      , metadata           = [] }

                  if isIn
                    then pure (LocalReference type' destStructPtr,[])
                    else pure $ (LocalReference (ptr type') primaPtr,[])


                t | t =:= basic || t =:= GPointer GAny || t =:= highLevel -> do
                  if isIn
                    then do
                      expr <- expression' e

                      label <- newLabel "argCastPointer"
                      addInstruction $ label := BitCast
                        { operand0 = LocalReference type' prima
                        , type'    = ptr type'
                        , metadata = [] }

                      addInstruction $ Do Store
                        { volatile = False
                        , address  = LocalReference (ptr type') label
                        , value    = expr
                        , maybeAtomicity = Nothing
                        , alignment = 4
                        , metadata  = [] }

                      pure $ (LocalReference (ptr type') label,[])
                    else pure $ (LocalReference (ptr type') primaPtr,[])


                t -> internal $ "Cannot pass arguments of type " <> show t



              castToFree <- newLabel "castToFree"

              addArgInsts $ castToFree := BitCast
                { operand0 = LocalReference type' primaPtr
                , type'    = pointerType
                , metadata = [] }

              addArgInsts $ Do Call
                { tailCallKind       = Nothing
                , callingConvention  = CC.C
                , returnAttributes   = []
                , function           = callable voidType freeString
                , arguments          = [ (LocalReference pointerType castToFree,[])
                                       , (filePath,[]), (line,[]),(col,[])]
                , functionAttributes = []
                , metadata           = [] }

              pure primaRef

            _ -> do
              (,[]) <$> expression' e


  Free { idName, freeType } -> do
    labelLoad  <- newLabel "freeLoad"
    labelCast  <- newLabel "freeCast"
    labelNull  <- newLabel "freeNull"
    ref        <- objectRef idName

    type'      <- toLLVMType (T.GPointer freeType)

    let
      SourcePos f l c = pos
      line = constantOperand GInt . Left . fromIntegral $ unPos l
      col  = constantOperand GInt . Left . fromIntegral $ unPos c
    filePath <- getFilePathOperand f
    case freeType of
      GArray { dimensions, innerType } -> do
        addInstruction $ labelLoad := Load
          { volatile  = False
          , address   = ref
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        iarrPtr <- newLabel "freeArrInternalPtr"
        addInstruction $ iarrPtr := GetElementPtr
          { inBounds = False
          , address  = LocalReference type' labelLoad
          , indices  = constantOperand GInt . Left <$> [0, fromIntegral (length dimensions)]
          , metadata = [] }

        inner <- toLLVMType innerType

        let iarrT = ArrayType 1 inner
        -- let iarrT = iterate (ArrayType 1) inner !! length dimensions

        iarr <- newLabel "freeArrInternal"
        addInstruction $ iarr := Load
          { volatile  = False
          , address   = LocalReference (ptr iarrT) iarrPtr
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        iarrCast <- newLabel "freeArrInternalCast"
        addInstruction $ iarrCast := BitCast
          { operand0 = LocalReference (ptr iarrT) iarr
          , type'    = pointerType
          , metadata = [] }

        addInstruction $ Do Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable voidType freeString
          , arguments          = [(LocalReference pointerType iarrCast, [])
                                 ,(filePath,[]), (line,[]), (col,[])]
          , functionAttributes = []
          , metadata           = [] }

        addInstruction $ labelCast := BitCast
          { operand0 = LocalReference type' labelLoad
          , type'    = pointerType
          , metadata = [] }

        addInstruction $ Do Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable voidType freeString
          , arguments          = (,[]) <$> [LocalReference pointerType labelCast, filePath, line, col]
          , functionAttributes = []
          , metadata           = [] }



      _ -> do

        addInstruction $ labelLoad := Load
          { volatile  = False
          , address   = ref
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

        let call name = Do Call
              { tailCallKind       = Nothing
              , callingConvention  = CC.C
              , returnAttributes   = []
              , function           = callable voidType $ "destroy" <> name
              , arguments          = [(LocalReference type' labelLoad, [])]
              , functionAttributes = []
              , metadata           = [] }

        case freeType of
          GDataType n t ta -> do
            types <- mapM fill $ toList ta
            addInstruction $ call (llvmName n types)

          _ -> pure ()

        addInstruction $ labelCast := BitCast
          { operand0 = LocalReference type' labelLoad
          , type'    = pointerType
          , metadata = [] }

        addInstruction $ Do Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable voidType freeString
          , arguments          = (,[]) <$> [LocalReference pointerType labelCast, filePath, line, col]
          , functionAttributes = []
          , metadata           = [] }



  New { idName, nType } -> do
    ref   <- objectRef idName -- The variable that is being mallocated
    type' <- ptr <$> toLLVMType nType

    case nType of
      T.GArray { dimensions, innerType } -> do
        structSize <- sizeOf nType

        structCall <- newLabel "newArrStruct"
        addInstruction $ structCall := Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable pointerType mallocString
          , arguments          = [(constantOperand GInt . Left $structSize, [])]
          , functionAttributes = []
          , metadata           = [] }

        structCast <- newLabel "newArrStructCast"
        addInstruction $ structCast := BitCast
          { operand0 = LocalReference pointerType structCall
          , type'
          , metadata = [] }

        dims <- mapM expression dimensions
        innerSize  <- sizeOf innerType
        bytes <- foldM bytesAux (ConstantOperand (C.Int 32 innerSize)) dims

        iarrCall <- newLabel "newArrInternal"
        addInstruction $ iarrCall := Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable pointerType mallocString
          , arguments          = [(bytes, [])]
          , functionAttributes = []
          , metadata           = [] }

        inner <- toLLVMType innerType

        let iarrT = ArrayType 1 inner
        -- let iarrT = iterate (ArrayType 1) inner !! length dimensions

        iarrCast <- newLabel "newArrInternalCast"
        addInstruction $ iarrCast := BitCast
          { operand0 = LocalReference pointerType iarrCall
          , type'    = ptr iarrT
          , metadata = [] }

        arrPtr <- newUnLabel
        addInstruction $ arrPtr := GetElementPtr
          { inBounds = False
          , address  = LocalReference type' structCast
          , indices  = constantOperand GInt . Left <$> [0, fromIntegral (length dimensions)]
          , metadata = [] }

        addInstruction $ Do Store
          { volatile       = False
          , address        = LocalReference (ptr iarrT) arrPtr
          , value          = LocalReference (ptr iarrT) iarrCast
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }

        void $ foldM (sizeAux (LocalReference type' structCast)) 0 dims

        addInstruction $ Do Store
          { volatile = False
          , address  = ref
          , value    = LocalReference type' structCast
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

          where
            bytesAux operand0 operand1 = do
              result <- newUnLabel
              addInstruction $ result := Mul
                { operand0
                , operand1
                , nsw = False
                , nuw = False
                , metadata = [] }
              pure $ LocalReference intType result

            sizeAux structRef n value = do
              dimPtr <- newLabel "dimPtr"
              addInstruction $ dimPtr := GetElementPtr
                { inBounds = False
                , address  = structRef
                , indices  =
                  [ ConstantOperand (C.Int 32 0)
                  , ConstantOperand (C.Int 32 n) ]
                , metadata = [] }

              addInstruction $ Do Store
                { volatile       = False
                , address        = LocalReference intType dimPtr
                , value
                , maybeAtomicity = Nothing
                , alignment      = 4
                , metadata       = [] }

              pure $ n + 1

      _ -> do
        labelCall <- newLabel "newCall"
        labelCast <- newLabel "newCast"

        typeSize <- sizeOf nType

        addInstruction $ labelCall := Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable pointerType mallocString
          , arguments          = [(constantOperand GInt . Left $typeSize, [])]
          , functionAttributes = []
          , metadata           = [] }

        addInstruction $ labelCast := BitCast
          { operand0 = LocalReference pointerType labelCall
          , type'    = type'
          , metadata = [] }

          -- Store the casted pointer in the variable
        addInstruction $ Do Store
          { volatile = False
          , address  = ref
          , value    = LocalReference type' labelCast
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }


        let
          structArg = LocalReference type' labelCast
          dinamicAllocFlag = constantOperand GBool . Left $ 0
          call name =  Do Call
            { tailCallKind       = Nothing
            , callingConvention  = CC.C
            , returnAttributes   = []
            , function           = callable voidType $ "init" <> name
            , arguments          = (,[]) <$> [structArg, dinamicAllocFlag]
            , functionAttributes = []
            , metadata           = [] }

        case nType of
          GAlias _ (GDataType n t ta) -> do
            types <- mapM fill $ toList ta
            addInstruction $ call (llvmName n types)
          GDataType n t ta -> do
            types <- mapM fill $ toList ta
            addInstruction $ call (llvmName n types)

          _ -> pure ()


  Write { ln, wexprs } -> do
    operands <- mapM expression' wexprs
    mapM_ write (Seq.zip operands (expType <$> wexprs))

    when ln . addInstruction $ Do Call
      { tailCallKind       = Nothing
      , callingConvention  = CC.C
      , returnAttributes   = []
      , function           = callable voidType lnString
      , arguments          = []
      , functionAttributes = []
      , metadata           = [] }
    where
      write (operand, t') = do
        -- Build the operand of the expression

        t <- fill t'

        let
        -- Call the correct C write function
          fun = callable voidType $ case t of
            GBool      -> writeBString
            GChar      -> writeCString
            GFloat     -> writeFString
            GInt       -> writeIString
            GEnum _    -> writeIString
            GString    -> writeSString
            GPointer _ -> writePString
            GAny       -> writeIString
            _          -> internal "attempted to write non-basic type."
        operand' <- if  (t =:= GPointer GAny)
          then do
            pointer <- newLabel "pointerToWrite"
            addInstruction $ pointer := BitCast
              { operand0 = operand
              , type'    = pointerType
              , metadata = [] }
            pure $ LocalReference pointerType pointer
          else pure operand

        addInstruction $ Do Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = fun
          , arguments          = [(operand', [])]
          , functionAttributes = []
          , metadata           = []}


  Read { file, vars } -> mapM_ readVar vars
    where
      readVar var = do
        t <- fill $ objType var

        (args, fread) <- case file of
          Nothing -> pure . ([],) $ case t of
            T.GChar  -> readCharStd
            T.GFloat -> readFloatStd
            T.GInt   -> readIntStd
            T.GBool  -> readBoolStd
            _        -> internal "Unsupported read " <> show t

          Just file' -> do
            let
              fileRef = ConstantOperand . C.GlobalReference (pointerType) . Name $
                        "__" <> unpack file'

            filePtr <- newLabel "filePtr"
            addInstruction $ filePtr := Load
              { volatile  = False
              , address   = fileRef
              , maybeAtomicity = Nothing
              , alignment = 4
              , metadata  = [] }

            let filePtrOp = LocalReference (pointerType) filePtr

            pure . ([(filePtrOp,[])], ) $ case t of
                T.GChar  -> readFileChar
                T.GFloat -> readFileFloat
                T.GInt   -> readFileInt
                T.GBool  -> readFileBool
                _        -> internal "Unsupported type in read: " <> show t

        type' <- toLLVMType t

        readResult <- newLabel "readCall"
        -- Call the C read function
        addInstruction $ readResult := Call
          { tailCallKind       = Nothing
          , callingConvention  = CC.C
          , returnAttributes   = []
          , function           = callable type' fread
          , arguments          = args
          , functionAttributes = []
          , metadata           = [] }

        -- Get the reference of the variable's memory
        objRef <- objectRef var
        -- Store the value saved at `readResult` in the variable memory
        addInstruction $ Do Store
          { volatile = False
          , address  = objRef
          , value    = LocalReference type' readResult
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }


  -- Random { var } -> do
  --   t <- fill $ objType var

  --   let frand = case t of
  --         T.GChar  -> randChar
  --         T.GFloat -> randFloat
  --         T.GInt   -> randInt
  --         T.GBool  -> randBool
  --         _        -> internal "Unsupported random " <> show t

  --   type' <- toLLVMType t

  --   readResult <- newLabel "randCall"
  --   -- Call the C read function
  --   addInstruction $ readResult := Call
  --     { tailCallKind       = Nothing
  --     , callingConvention  = CC.C
  --     , returnAttributes   = []
  --     , function           = callable type' frand
  --     , arguments          = []
  --     , functionAttributes = []
  --     , metadata           = [] }

  --   -- Get the reference of the variable's memory
  --   objRef <- objectRef var
  --   -- Store the value saved at `readResult` in the variable memory
  --   addInstruction $ Do Store
  --     { volatile = False
  --     , address  = objRef
  --     , value    = LocalReference type' readResult
  --     , maybeAtomicity = Nothing
  --     , alignment = 4
  --     , metadata  = [] }


  Repeat { rguards, rinv, rbound } -> do
    begin     <- newLabel "doBegin"
    again     <- newLabel "doAgain"
    checkGte0 <- newLabel "doCheckGte0"
    n         <- newLabel "doN"
    asserts    <- use evalAssertions

    terminate Br
      { dest      = begin
      , metadata' = [] }

    (begin #)
    addInstruction $ n := Alloca
      { allocatedType = intType
      , numElements   = Nothing
      , alignment     = 4
      , metadata      = [] }
      
    
  
    if asserts then do
      boundVal0 <- expression rbound
      Just begin' <- use blockName
      terminate Br
        { dest      = checkGte0
        , metadata' = [] }

      (again #)
      boundVal1 <- expression rbound
      oldBound <- newLabel "doOldBound"
      addInstruction $ oldBound := Load
        { volatile = False
        , address = LocalReference intType n
        , maybeAtomicity = Nothing
        , alignment = 4
        , metadata = [] }
      ltOld <- newLabel "doLtOld"
      addInstruction $ ltOld := ICmp
        { iPredicate = SLT
        , operand0   = boundVal1
        , operand1   = LocalReference intType oldBound
        , metadata   = [] }
      noLtOld <- newLabel "doNoLtOld"

      Just again' <- use blockName
      terminate CondBr
        { condition = LocalReference boolType ltOld
        , trueDest  = checkGte0
        , falseDest = noLtOld
        , metadata' = [] }

      (noLtOld #)
      abort Abort.NonDecreasingBound pos

      (checkGte0 #)
      boundVal <- newLabel "doBound"
      addInstruction $ boundVal := Phi
        { type' = intType
        , incomingValues =
          [ (boundVal0, begin')
          , (boundVal1, again') ]
        , metadata = [] }
      gte0 <- newLabel "doGte0"
      addInstruction $ gte0 := ICmp
        { iPredicate = SGE
        , operand0   = LocalReference intType boundVal
        , operand1   = constantOperand GInt . Left $0
        , metadata   = [] }

      yesGte0 <- newLabel "doGte0Yes"
      noGte0  <- newLabel "doGte0No"
      terminate CondBr
        { condition = LocalReference boolType gte0
        , trueDest  = yesGte0
        , falseDest = noGte0
        , metadata' = [] }

      (noGte0 #)
      abort Abort.NegativeBound pos

      (yesGte0 #)
      yesInv <- newLabel "doInvYes"
      noInv  <- newLabel "doInvNo"

      boolean yesInv noInv rinv

      (noInv #)
      abort Abort.Invariant pos

      (yesInv #)
      addInstruction $ Do Store
        { volatile       = False
        , address        = LocalReference intType n
        , value          = LocalReference intType boundVal
        , maybeAtomicity = Nothing
        , alignment      = 4
        , metadata       = [] }

      firstGuard <- newLabel "doGuards"
      terminate Br
        { dest      = firstGuard
        , metadata' = [] }

      exit <- foldM (guard again) firstGuard rguards

      (exit #)
    else do 
      terminate Br
        { dest      = again
        , metadata' = [] }

      exit <- foldM (guard again) again rguards

      (exit #)


  Skip -> pure ()

  -- _ -> internal $
  --   "I don't know how to generate code for:" <> (drawTree . toTree $ i)
