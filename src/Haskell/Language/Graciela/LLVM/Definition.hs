{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PostfixOperators         #-}
{-# LANGUAGE TupleSections            #-}

module Language.Graciela.LLVM.Definition where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Declaration   (Declaration(..))
import           Language.Graciela.AST.Definition
import           Language.Graciela.AST.Expression    (Expression (..))
import qualified Language.Graciela.AST.Instruction   as G (Instruction)
import qualified Language.Graciela.AST.Object        as O
import           Language.Graciela.AST.Struct        (Struct (..), Struct' (..))
import           Language.Graciela.AST.Type          ((=:=))
import qualified Language.Graciela.AST.Type          as T
import           Language.Graciela.Common
import           Language.Graciela.LLVM.Abort        (abort, abortString)
import qualified Language.Graciela.LLVM.Abort        as Abort (Abort (..))
import           Language.Graciela.LLVM.Boolean
import           Language.Graciela.LLVM.Declaration  (declaration)
import           Language.Graciela.LLVM.Expression
import           Language.Graciela.LLVM.Instruction
import           Language.Graciela.LLVM.Object
import           Language.Graciela.LLVM.Monad
import           Language.Graciela.LLVM.State
import           Language.Graciela.LLVM.Type
import           Language.Graciela.LLVM.Warning      (warn, warnString)
import qualified Language.Graciela.LLVM.Warning      as Warning (Warning (Post, Pre))
import           Language.Graciela.Location
import qualified Language.Graciela.Location          as L (pos)
import           Language.Graciela.Parser.Config
import           Language.Graciela.Treelike
--------------------------------------------------------------------------------
import           Control.Lens                (use, (%=), (&), (.=))
import           Data.Array                  ((!))
import           Data.Foldable               (toList)
import qualified Data.Map.Strict             as Map
import qualified Data.Sequence               as Seq (empty, fromList)
import           Data.Text                   (Text)
import           Data.Word                   (Word32)
import           LLVM.AST                    (BasicBlock (..),
                                              Named (..),
                                              Parameter (..),
                                              Terminator (..),
                                              functionDefaults)
import qualified LLVM.AST                    as LLVM (Definition (..))
import           LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention  as CC (CallingConvention (C))
import qualified LLVM.AST.Constant           as C
import           LLVM.AST.Global             (Global (..),
                                              functionDefaults)
import           LLVM.AST.Instruction
import           LLVM.AST.IntegerPredicate   (IntegerPredicate (EQ, SGE, SLT))
import           LLVM.AST.Linkage            (Linkage (Private))
import           LLVM.AST.Name               (Name, mkName)
import           LLVM.AST.Operand            (MetadataNode (..),
                                                      Operand (..))
import           LLVM.AST.ParameterAttribute (ParameterAttribute (..))
import           LLVM.AST.Type               (Type (..), double, ptr)
import qualified LLVM.AST.Type               as LLVM (Type(..))
import           LLVM.AST.Visibility         (Visibility (Default))
import           Prelude                     hiding (Ordering (EQ))
--------------------------------------------------------------------------------

{- Given the instruction block of the main program, construct the main LLVM function-}
mainDefinition :: G.Instruction -> [String] -> LLVM ()
mainDefinition block files = do
  main <- newLabel "main"

  (main #)
  mapM_ openFile files

  callFunction initTrashCollectorString [] >>= addInstruction . Do

  callFunction openScopeString [] >>= addInstruction . Do

  instruction block

  callFunction freeTrashCollectorString [] >>= addInstruction . Do
    

  mapM_ closeFile files

  terminate $ Ret (Just . constantOperand T.GInt . Left $0) []

  blocks' <- use blocks
  blocks .= Seq.empty
  addDefinition $ LLVM.GlobalDefinition functionDefaults
    { name        = mkName "main"
    , parameters  = ([], False)
    , returnType  = intType
    , basicBlocks = toList blocks'
    }

  where
    openFile file = do
      let
        fileRef = ConstantOperand . C.GlobalReference pointerType . mkName $
                  "__" <> file

      fileLabel <- newLabel "file"
      strs <- use stringIds
      let Just i = (pack file) `Map.lookup` strs
      string <- (!i) <$> use stringOps
      callFunction openFileStr [string] >>= addInstruction . (fileLabel :=)

      addInstruction $ Do Store
          { volatile = False
          , address  = fileRef
          , value    = LocalReference pointerType fileLabel
          , maybeAtomicity = Nothing
          , alignment = 4
          , metadata  = [] }

    closeFile file = do
      let
        fileRef = ConstantOperand . C.GlobalReference pointerType . mkName $
                  "__" <> file
      filePtr <- newLabel "filePtr"
      addInstruction $ filePtr := Load
        { volatile  = False
        , address   = fileRef
        , maybeAtomicity = Nothing
        , alignment = 4
        , metadata  = [] }

      callFunction closeFileStr [LocalReference pointerType filePtr] >>= addInstruction . Do



{- Translate a definition from Graciela AST to LLVM AST -}
definition :: Definition -> LLVM ()
definition Definition { defName
                      , def'
                      , pre
                      , post
                      , bound
                      , defLoc = Location (pos, _to)
                      , isDecl
                      , isExtern}
  = if isDecl then do 
      use evalAssertions >>= \asserts -> do 
        cs <- use currentStruct
        pName <- if isExtern then pure (unpack defName) else case cs of
          Nothing -> do
            pure . ('$':) $ unpack defName
          Just Struct{ structBaseName, structTypes, struct' = DataType{abstract} } -> do
            abstractStruct <- (Map.lookup abstract) <$> use structs
            t' <- mapM fill structTypes
            pure . ('$':) $ llvmName (defName <> "-" <> structBaseName) t'

        case def' of
          FunctionDef { funcRetType, funcParams, funcRecursive  } -> do
              
            params  <- mapM (makeParam' isDecl) . toList $ funcParams
            retType <- toLLVMType funcRetType
            
            let 
              hasOldBound = mkName $ "." <> unpack defName <> "HasOldBound"
              oldBound    = mkName $ "." <> unpack defName <> "OldBound"

              params' = if isJust bound && funcRecursive && asserts
                then [Parameter boolType hasOldBound [], Parameter intType oldBound []]
                else []
            addDefinition $ defineFunction pName (params' <> params) retType

          ProcedureDef { procParams, procRecursive } -> do
            params  <- mapM (makeParam isDecl) . toList $ procParams
            
            let
              hasOldBound = mkName $ "." <> unpack defName <> "HasOldBound"
              oldBound    = mkName $ "." <> unpack defName <> "OldBound"

              params' = if isJust bound && (procRecursive && asserts)
                then [Parameter boolType hasOldBound [], Parameter intType oldBound []]
                else []

            addDefinition $ defineFunction pName (params' <> params) voidType

          GracielaFunc{} -> pure () -- graciela native function should be declared here
                                    -- instead of declaring them manually

    else case def' of
      FunctionDef { funcBody, funcRetType, funcParams, funcRecursive, funcDecls } -> do
        func <- newLabel $ "func" <> unpack defName
        (func #)

        openScope

        params' <- mapM (makeParam' isDecl) . toList $ funcParams
        mapM_ arrAux' funcParams

        mapM_ declaration funcDecls

        asserts <- use evalAssertions

        cond <- if asserts 
          then Just <$> precondition pre
          else pure Nothing 

        params'' <- if isJust bound 
          then recursiveParams (funcRecursive && asserts) -- recursion is verified if the assertions are enabled
          else pure []
        let params = params'' <> params
        cs <- use currentStruct
        returnType <- toLLVMType funcRetType
        let
          invariant' fn (name, t) | asserts = do
            name' <- getVariableName name
            exit  <- case cond of 
              Just cond' -> callInvariant fn cond' name' t Nothing
              Nothing -> pure Nothing
            when (isJust exit) $ (fromJust exit #)
          invariant' _ _ = pure ()

          dts  = filter (\(_, t) -> isJust (T.hasDT t) ) . toList $ funcParams
          
          body = do
            forM_ dts (invariant' "inv"    )
            forM_ dts (invariant' "coupInv")
            forM_ dts (invariant' "repInv" )
            expression' funcBody

          retVar returnOperand = do
            returnVar     <- insertVar defName

            addInstruction $ returnVar := Alloca
              { allocatedType = returnType
              , numElements   = Nothing
              , alignment     = 4
              , metadata      = [] }

            addInstruction $ Do Store
              { volatile = False
              , address  = LocalReference returnType returnVar
              , value    = returnOperand
              , maybeAtomicity = Nothing
              , alignment = 4
              , metadata  = [] }



        (postFix, returnOperand) <- case cs of
          Nothing  -> do
            let paramType = map (\(Parameter t _ _) -> t) params
            functionsTypes %= Map.insert (mkName (unpack defName)) (llvmFunT returnType paramType)
            returnOp <- body
            when asserts $ do 
              retVar returnOp
              postcondition (fromJust cond) post
            pure (unpack defName, returnOp)
          

          Just Struct{ structBaseName, structTypes, struct' = DataType{abstract} } -> do
            abstractStruct <- (Map.lookup abstract) <$> use structs
            t' <- mapM fill structTypes
            let postFix = llvmName (defName <> "-" <> structBaseName) t'
                paramType = map (\(Parameter t _ _) -> t) params
            functionsTypes %= Map.insert (mkName postFix) (llvmFunT returnType paramType)
            let
              maybeProc = case abstractStruct of
                Just Struct {structProcs} -> defName `Map.lookup` structProcs
                Nothing -> error "Internal error: Missing Abstract Data Type."

            case maybeProc of
              Just Definition{ pre = pre', def' = AbstractFunctionDef {abstFDecl}} -> do
                mapM_ declaration abstFDecl
                when asserts $ preconditionAbstract (fromJust cond) pre' pos
              _ -> pure ()

            returnOp <- body
            when asserts $ retVar returnOp

            case maybeProc of
              Just Definition{post = post'} | asserts -> 
                postconditionAbstract (fromJust cond) post' pos
              _  -> pure ()

            when asserts $ postcondition (fromJust cond) post
            pure (postFix, returnOp)

        terminate Ret
          { returnOperand = Just returnOperand
          , metadata' = [] }

        let name = mkName . ('$':) $ postFix
        blocks' <- use blocks
        blocks .= Seq.empty

        addDefinition $ LLVM.GlobalDefinition functionDefaults
          { name        = name
          , parameters  = (params, False)
          , returnType
          , basicBlocks = toList blocks'
          }
        closeScope


      ProcedureDef { procDecl, procParams, procBody, procRecursive } -> do
        proc <- newLabel $ "proc" <> unpack defName
        (proc #)
        asserts <- use evalAssertions
        openScope -- Open scope in symbol table

        params' <- mapM (makeParam isDecl) . toList $ procParams -- Create parameters
        mapM_ declarationsOrRead procDecl -- do internal declarations and reads instructions
        mapM_ arrAux procParams -- create new arrays, if needed
        
        cond <- if asserts 
          then Just <$> precondition pre
          else pure Nothing 

        params'' <- if isJust bound 
          then recursiveParams (procRecursive && asserts) -- recursion is verified if the assertions are enabled
          else pure []
        let params = params'' <> params
        cs <- use currentStruct
        when asserts $ forM_ procParams makeTempVar -- Create variables with the initial value
        let
          invariant' fn (name, t, _) | asserts = do
            name' <- getVariableName name
            exit  <- case cond of 
              Just cond' -> callInvariant fn cond' name' t Nothing
              Nothing -> pure Nothing
            when (isJust exit) $ (fromJust exit #)
          invariant' _ _ = pure ()

          dts  = filter (\(_, t, _) -> isJust (T.hasDT t) ) . toList $ procParams
          
          body abstractStruct = do
            let
              maybeProc = case abstractStruct of
                Just Struct {structProcs} -> defName `Map.lookup` structProcs
                Nothing                   -> Nothing
            when asserts $ do
              forM_ dts (invariant' "coupInv")
              forM_ dts (invariant' "repInv" )

            let cond' = fromJust cond

            case maybeProc of
              Just Definition{ pre = pre', def' = AbstractProcedureDef{ abstPDecl }} | asserts  -> do
                mapM_ declaration abstPDecl
                preconditionAbstract cond' pre' pos
                                
              _ -> pure ()

            when asserts $ forM_ dts (invariant' "inv")

            instruction procBody

            when asserts $ do
              forM_ dts (invariant' "coupInv")

              forM_ dts (invariant' "inv"    )
              forM_ dts (invariant' "repInv" )
              case maybeProc of
                Just Definition{post = post'} -> 
                  postconditionAbstract cond' post' pos
                _                             -> pure ()

        pName <- case cs of
          Nothing -> do
            let 
              fname = (unpack defName)
              paramType = map (\(Parameter t _ _) -> t) params
            functionsTypes %= Map.insert (mkName fname) (llvmFunT voidType paramType)
            body Nothing
            pure $ '$':fname

          Just Struct{ structBaseName, structTypes, struct' = DataType{abstract} } -> do
            abstractStruct <- (Map.lookup abstract) <$> use structs
            t' <- mapM fill structTypes
            let 
              fname = llvmName (defName <> "-" <> structBaseName) t'
              paramType = map (\(Parameter t _ _) -> t) params
            functionsTypes %= Map.insert (mkName  fname) (llvmFunT voidType paramType)
            
            body abstractStruct
            pure $ '$':fname

        when asserts $ do
          postcondition (fromJust cond) post

        terminate $ Ret Nothing []

        blocks' <- use blocks

        addDefinition $ LLVM.GlobalDefinition functionDefaults
          { name        = mkName pName
          , parameters  = (params,False)
          , returnType  = voidType
          , basicBlocks = toList blocks'
          }

        blocks .= Seq.empty
        closeScope

      GracielaFunc {} -> pure ()
      GracielaProc {} -> pure ()

  where

    -- Create temporal variables for each argument. 
    -- These variables can be use only at postconditions.
    makeTempVar' (name, t) = makeTempVar (name, t, T.In)
    makeTempVar :: (Text, T.Type, T.ArgMode) -> LLVM ()
    makeTempVar  (name, t, mode) = do
      let tempVarName = name <> "'"
      declaration Declaration
        { declLoc  = gracielaDef
        , declType = t
        , declIds  = Seq.fromList [tempVarName]
        }
      name' <- getVariableName tempVarName
      t'    <- toLLVMType t

      destVar <- objectRef O.Object 
        { O.loc = gracielaDef
        , O.objType = t
        , O.obj' = O.Variable
          { O.name = tempVarName
          , O.mode = Nothing } }

      sourceVar <- objectRef O.Object 
        { O.loc = gracielaDef
        , O.objType = t
        , O.obj' = O.Variable
          { O.name = name
          , O.mode = Just mode } }
      
      case t of 
        T.GArray {} -> copyArray t sourceVar destVar
        t | t =:= T.GADataType -> do
          t <- pure $ t <> T.GADataType
          types <- mapM fill (toList . T.dtTypeArgs $ t)

          let
            copyFunc = llvmName ("copy" <> T.typeName t) types
            args = [ sourceVar, destVar ]
          callFunction copyFunc args >>= addInstruction . Do
          
        _ -> do
          value <- newLabel "value"
          addInstruction $value := Load
            { volatile  = False
            , address   = sourceVar
            , maybeAtomicity = Nothing
            , alignment = 4
            , metadata  = [] }

          addInstruction $ Do Store
            { volatile = False
            , address  = destVar
            , value    = LocalReference t' value
            , maybeAtomicity = Nothing
            , alignment = 4
            , metadata  = [] }


    makeParam' isDecl (name, t) = makeParam isDecl (name, t, T.In)
    makeParam isDecl (name, t, mode)  = do
      name' <- insertVar name
      t'    <- toLLVMType t
      if mode `elem` [T.In, T.Const] && not (t =:= T.GOneOf [T.GADataType, T.GAArray] )
        then do
          pTemp <- newUnLabel
          unless isDecl $ do
            addInstruction $ name' := Alloca
                { allocatedType = t'
                , numElements   = Nothing
                , alignment     = 4
                , metadata      = [] }

            addInstruction $ Do Store
              { volatile = False
              , address  = LocalReference t' name'
              , value    = LocalReference t' pTemp
              , maybeAtomicity = Nothing
              , alignment = 4
              , metadata  = [] }

          pure $ Parameter t' pTemp []
        else 
          pure $ Parameter (ptr t') name' []

    arrAux' (arr, t) = arrAux (arr, t, T.In) -- ArrAux for Functions
    arrAux  (arr, t@(T.GArray dims inner), mode) = do -- ArrAux for Procedures
      t'      <- toLLVMType t
      arrName <- getVariableName arr
      void $ foldM (dimAux t' arrName) 0 dims
      where
        dimAux t' arrName n dim = do
          paramDim <- expression dim

          dimAddr <- newUnLabel
          addInstruction $ dimAddr := GetElementPtr
            { inBounds = False
            , address  = LocalReference t' arrName
            , indices  =
              [ constantOperand T.GInt . Left $ 0
              , constantOperand T.GInt . Left $ n ]
            , metadata = [] }

          argDim <- newLabel "arrCheck"
          addInstruction $ argDim := Load
            { volatile       = False
            , address        = LocalReference intType dimAddr
            , maybeAtomicity = Nothing
            , alignment      = 4
            , metadata       = [] }

          arrCheckCmp <- newLabel "arrCheckCmp"
          addInstruction $ arrCheckCmp := ICmp
            { iPredicate = EQ
            , operand0 = paramDim
            , operand1 = LocalReference intType argDim
            , metadata = [] }

          arrOk <- newLabel "arrOk"
          arrNotOk <- newLabel "arrNotOk"
          terminate CondBr
            { condition = LocalReference boolType arrCheckCmp
            , trueDest  = arrOk
            , falseDest = arrNotOk
            , metadata' = [] }

          (arrNotOk #)
          
          callFunction writeIString [paramDim] >>= addInstruction . Do
          callFunction lnString [] >>= addInstruction . Do
          callFunction writeIString [LocalReference intType argDim] >>= addInstruction . Do

          abort Abort.BadArrayArg (L.pos . loc $ dim)

          (arrOk #)
          pure $ n + 1

    arrAux _ = pure ()

    loadDtPtr name (T.GPointer t) exit callFunc = do
      yes     <- newLabel "yesNull"
      no      <- newLabel "noNull"
      cast    <- newLabel "cast"
      comp    <- newLabel "comp"
      argLoad <- newLabel "argLoad"
      type'   <- toLLVMType (T.GPointer t)
      exit'   <- case exit of
        Nothing -> newLabel "exit"
        Just e  -> pure e

      addInstruction $ argLoad := Load
            { volatile       = False
            , address        = LocalReference type' name
            , maybeAtomicity = Nothing
            , alignment      = 4
            , metadata       = [] }

      addInstruction $ cast := PtrToInt
            { operand0 = LocalReference type' argLoad
            , type'    = lintType
            , metadata = [] }

      addInstruction $ comp := ICmp
              { iPredicate = EQ
              , operand0 = LocalReference intType cast
              , operand1 = constantOperand T.I64 (Left 0)
              , metadata = [] }

      terminate CondBr
          { condition = LocalReference boolType comp
          , trueDest  = yes
          , falseDest = no
          , metadata' = [] }

      (no #)

      e <- callFunc argLoad t (Just exit')

      terminate Br
          { dest = exit'
          , metadata' = [] }

      (yes #)

      terminate Br
          { dest = exit'
          , metadata' = [] }

      pure e

    callCouple funName name t@(T.GPointer _) exit = do
      loadDtPtr name t exit (callCouple funName)


    callCouple funName name t exit | t =:= T.GADataType = do
      type' <- toLLVMType t
      t <- pure $ t <> T.GADataType
      t' <- mapM fill (toList . T.dtTypeArgs $ t)
      let 
        fName = llvmName (funName <> "-" <> T.typeName t) t'
        args = [LocalReference type' name]
      callFunction fName args >>= addInstruction . Do

      pure exit



    callInvariant funName c name t@(T.GPointer _) exit = do
      loadDtPtr name t exit (callInvariant funName c)


    callInvariant f c n (T.GAlias _ (t@T.GDataType{})) e = callInvariant f c n t e

    callInvariant funName cond name t exit = do
      type' <- toLLVMType t
      t' <- mapM fill (toList . T.dtTypeArgs $ t)
      let 
        fName = llvmName (funName <> "-" <> T.typeName t) t'
        args = [LocalReference type' name, cond]
      callFunction fName args >>= addInstruction . Do

      pure exit
    
    recursiveParams False = pure []
    recursiveParams True  = do
      let
        boundExp = fromMaybe
          (internal "boundless recursive function.")
          bound
        hasOldBound = mkName $ "." <> unpack defName <> "HasOldBound"
        oldBound = mkName $ "." <> unpack defName <> "OldBound"

      funcBodyLabel <- newLabel $ "func" <> unpack defName <> "Body"
      boundOperand <- expression boundExp

      gte0 <- newLabel "funcBoundGte0"
      addInstruction $ gte0 := ICmp
        { iPredicate = SGE
        , operand0   = boundOperand
        , operand1   = constantOperand T.GInt . Left $0
        , metadata   = [] }
      yesGte0 <- newLabel "funcGte0Yes"
      noGte0  <- newLabel "funcGte0No"
      terminate CondBr
        { condition = LocalReference boolType gte0
        , trueDest  = yesGte0
        , falseDest = noGte0
        , metadata' = [] }

      (noGte0 #)
      abort Abort.NegativeBound
        (let Location (pos, _) = loc boundExp in pos)

      (yesGte0 #)
      yesOld <- newLabel "funcOldBoundYes"
      noOld  <- newLabel "funcOldBoundNo"
      terminate CondBr
        { condition = LocalReference boolType hasOldBound
        , trueDest  = yesOld
        , falseDest = noOld
        , metadata' = [] }

      (noOld #)
      terminate Br
        { dest = funcBodyLabel
        , metadata' = [] }

      (yesOld #)
      ltOld <- newLabel "funcLtOld"
      addInstruction $ ltOld := ICmp
        { iPredicate = SLT
        , operand0   = boundOperand
        , operand1   = LocalReference intType oldBound
        , metadata   = [] }
      yesLtOld <- newLabel "funcLtOldBoundYes"
      noLtOld  <- newLabel "funcLtOldBoundNo"
      terminate CondBr
        { condition = LocalReference boolType ltOld
        , trueDest  = yesLtOld
        , falseDest = noLtOld
        , metadata' = [] }

      (noLtOld #)
      abort Abort.NonDecreasingBound
        (let Location (pos, _) = loc boundExp in pos)

      (yesLtOld #)
      terminate Br
        { dest = funcBodyLabel
        , metadata' = [] }

      (funcBodyLabel #)

      boundOp .= Just boundOperand
      pure [Parameter boolType hasOldBound [], Parameter intType oldBound []]

    declarationsOrRead :: Either Declaration G.Instruction -> LLVM ()
    declarationsOrRead (Left decl)   = declaration decl
    declarationsOrRead (Right read') = instruction read'

    precondition :: Expression -> LLVM Operand
    precondition expr@ Expression {loc = Location (pos,_) } = do
        -- Create both labels
        trueLabel  <- newLabel "precondTrue"
        falseLabel <- newLabel "precondFalse"
        -- Evaluate the condition expression
        cond <- wrapBoolean expr
        -- Add the conditional branch
        terminate CondBr
          { condition = cond
          , trueDest  = trueLabel
          , falseDest = falseLabel
          , metadata' = [] }
        -- Set the false label to the warning, then continue normally
        (falseLabel #)
        warn Warning.Pre pos
        terminate Br
          { dest      = trueLabel
          , metadata' = [] }

        -- And the true label to the next instructions
        (trueLabel #)

        pure cond

    preconditionAbstract :: Operand -> Expression -> SourcePos -> LLVM ()
    preconditionAbstract precond expr pos = do
      -- Create both labels
      evaluate   <- newLabel "evaluate"
      trueLabel  <- newLabel "precondAbstTrue"
      falseLabel <- newLabel "precondAbstFalse"
      -- Evaluate the condition expression
      cond <- wrapBoolean expr

      terminate CondBr
        { condition = precond
        , trueDest  = evaluate
        , falseDest = trueLabel
        , metadata' = [] }

      -- Add the conditional branch
      (evaluate #)
      terminate CondBr
        { condition = cond
        , trueDest  = trueLabel
        , falseDest = falseLabel
        , metadata' = [] }
      -- Set the false label to the warning, then continue normally
      (falseLabel #)
      abort Abort.BadAbstractCouple pos

      -- And the true label to the next instructions
      (trueLabel #)

    postconditionAbstract :: Operand -> Expression -> SourcePos -> LLVM ()
    postconditionAbstract precond expr pos = do
      -- Create both labels
      evaluate   <- newLabel "evaluate"
      trueLabel  <- newLabel "precondAbstTrue"
      falseLabel <- newLabel "precondAbstFalse"

      terminate CondBr
        { condition = precond
        , trueDest  = evaluate
        , falseDest = trueLabel
        , metadata' = [] }


      (evaluate #)
      -- Evaluate the condition expression
      cond <- wrapBoolean expr
      -- Add the conditional branch
      terminate CondBr
        { condition = cond
        , trueDest  = trueLabel
        , falseDest = falseLabel
        , metadata' = [] }
      -- Set the false label to the warning, then continue normally
      (falseLabel #)
      abort Abort.AbstractPost pos

      -- And the true label to the next instructions
      (trueLabel #)

    postcondition :: Operand -> Expression -> LLVM ()
    postcondition precond expr@ Expression {loc = Location(pos,_)} = do
      -- Create both labels
      evaluate   <- newLabel "evaluate"
      trueLabel  <- newLabel "postcondTrue"
      falseLabel <- newLabel "postcondFalse"

      -- Create the conditional branch
      terminate CondBr
        { condition = precond
        , trueDest  = evaluate
        , falseDest = trueLabel
        , metadata' = [] }

      (evaluate #)
      -- Evaluate the condition expression
      cond <- wrapBoolean expr
      -- Add the conditional branch
      terminate CondBr
        { condition = cond
        , trueDest  = trueLabel
        , falseDest = falseLabel
        , metadata' = [] }
      -- Set the false label to the warning, then continue normally
      (falseLabel #)
      abort Abort.Post pos
      -- And the true label to the next instructions

      (trueLabel #)



preDefinitions :: [String] -> LLVM ()
preDefinitions files = do
  mapM_ addFile files

  addDefinitions 
    [ defineFunction' copyArrayString [ ("size"     , intType)
                                     , ("arrSource", pointerType)
                                     , ("arrDest"  , pointerType)
                                     , ("sizeT"    , intType) ]
                                     voidType
    -- Trace pseudo Functions
    , defineFunction' traceIntString         intParam intType
    , defineFunction' traceFloatString       floatParam floatType
    , defineFunction' traceCharString        charParam charType
    , defineFunction' traceBoolString        boolParam boolType
    , defineFunction' traceStringIntString   [ ("x", stringType)
                                            , ("y", intType) ]
                                            intType
    , defineFunction' traceStringFloatString [ ("x", stringType)
                                            , ("y", floatType) ]
                                            floatType
    , defineFunction' traceStringCharString  [ ("x", stringType)
                                            , ("y", charType) ]
                                            charType
    , defineFunction' traceStringBoolString  [ ("x", stringType)
                                            , ("y", boolType) ]
                                            boolType

    -- Conversion functions
    , defineFunction' float2intString  ([("x", floatType)] <> posParam) intType
    , defineFunction' pointer2intString ([("x", floatType)] <> posParam) intType
    , defineFunction' char2intString   charParam intType
    , defineFunction' float2charString ([("x", floatType)] <> posParam) charType
    , defineFunction' int2charString   ([("x", intType)] <> posParam) charType
    , defineFunction' char2floatString charParam floatType
    , defineFunction' int2floatString  intParam  floatType

    -- Polymorphic functions
    , defineFunction' sqrtIString ([("x", intType)] <> posParam)intType
    , defineFunction' sqrtFString ([("x", floatType)] <> posParam) floatType
    , defineFunction' absIString  ([("x", intType)] <> posParam) intType

    , defineFunction' isNanString  [ ("x", floatType) ] boolType
    , defineFunction' isInfString  [ ("x", floatType) ] boolType

    , defineFunction' absFString               floatParam floatType
    , defineFunction' toSetMultiString         ptrParam   pointerType
    , defineFunction' toSetSeqString           ptrParam   pointerType
    , defineFunction' toSetFuncString          ptrParam   pointerType
    , defineFunction' toSetRelString           ptrParam   pointerType
    , defineFunction' toMultiSetString         ptrParam   pointerType
    , defineFunction' toMultiSeqString         ptrParam   pointerType
    , defineFunction' toSeqSetString           ptrParam   pointerType
    , defineFunction' toSeqMultiString         ptrParam   pointerType

--------------------------------------------------------------------------------


    , defineFunction' firstSetString ptrParam (ptr iterator)
    , defineFunction' nextSetString [("x", ptr iterator)] (ptr iterator)

    , defineFunction' firstMultisetString ptrParam (ptr iterator)
    , defineFunction' nextMultisetString [("x", ptr iterator)] (ptr iterator)

    , defineFunction' firstSequenceString ptrParam (ptr iterator)
    , defineFunction' nextSequenceString [("x", ptr iterator)] (ptr iterator)

    , defineFunction' initTrashCollectorString [] voidType
    , defineFunction' freeTrashCollectorString [] voidType
    , defineFunction' openScopeString          [] voidType

    -- (Bi)Functors
    , defineFunction' newSetString             [] pointerType
    , defineFunction' newSeqString             [] pointerType
    , defineFunction' newMultisetString        [] pointerType

    , defineFunction' newSetPairString         [] pointerType
    , defineFunction' newMultisetPairString    [] pointerType
    , defineFunction' newSeqPairString         [] pointerType

    , defineFunction' newFunction              [] pointerType
    , defineFunction' newRelation              [] pointerType

--------------------------------------------------------------------------------

    , defineFunction' equalSetString            ptrParam2 boolType
    , defineFunction' equalSeqString            ptrParam2 boolType
    , defineFunction' equalMultisetString       ptrParam2 boolType

    , defineFunction' equalSetPairString        ptrParam2 boolType
    , defineFunction' equalSeqPairString        ptrParam2 boolType
    , defineFunction' equalMultisetPairString   ptrParam2 boolType

    , defineFunction' equalFuncString           ptrParam2 boolType
    , defineFunction' equalRelString            ptrParam2 boolType

    , defineFunction' equalTupleString          [ ("x", ptr tupleType)
                                                , ("y", ptr tupleType)]
                                                boolType
--------------------------------------------------------------------------------
    , defineFunction' sizeSetString             ptrParam intType
    , defineFunction' sizeSeqString             ptrParam intType
    , defineFunction' sizeMultisetString        ptrParam intType
    , defineFunction' sizeRelString             ptrParam intType
    , defineFunction' sizeFuncString            ptrParam intType
--------------------------------------------------------------------------------
    , defineFunction' supersetSetString         ptrParam2 boolType
    , defineFunction' supersetMultisetString    ptrParam2 boolType
    , defineFunction' ssupersetSetString        ptrParam2 boolType
    , defineFunction' ssupersetMultisetString   ptrParam2 boolType

    , defineFunction' supersetSetPairString         ptrParam2 boolType
    , defineFunction' supersetMultisetPairString    ptrParam2 boolType
    , defineFunction' ssupersetSetPairString        ptrParam2 boolType
    , defineFunction' ssupersetMultisetPairString   ptrParam2 boolType
--------------------------------------------------------------------------------
    , defineFunction' insertSetString           ptri64Param voidType
    , defineFunction' insertSeqString           ptri64Param voidType
    , defineFunction' insertMultisetString      ptri64Param voidType

    , defineFunction' insertSetPairString       ptrTupleParam voidType
    , defineFunction' insertSeqPairString       ptrTupleParam voidType
    , defineFunction' insertMultisetPairString  ptrTupleParam voidType
--------------------------------------------------------------------------------

    , defineFunction' isElemSetString          ptri64Param boolType
    , defineFunction' isElemMultisetString     ptri64Param boolType
    , defineFunction' isElemSeqString          ptri64Param boolType

    , defineFunction' isElemSetPairString      ptrTupleParam boolType
    , defineFunction' isElemMultisetPairString ptrTupleParam boolType
    , defineFunction' isElemSeqPairString      ptrTupleParam boolType
--------------------------------------------------------------------------------
    , defineFunction' unionSetString           ptrParam2 pointerType
    , defineFunction' intersectSetString       ptrParam2 pointerType
    , defineFunction' differenceSetString      ptrParam2 pointerType

    , defineFunction' unionSetPairString       ptrParam2 pointerType
    , defineFunction' intersectSetPairString   ptrParam2 pointerType
    , defineFunction' differenceSetPairString  ptrParam2 pointerType

    , defineFunction' unionMultisetString      ptrParam2 pointerType
    , defineFunction' intersectMultisetString  ptrParam2 pointerType
    , defineFunction' differenceMultisetString ptrParam2 pointerType

    , defineFunction' unionMultisetPairString      ptrParam2 pointerType
    , defineFunction' intersectMultisetPairString  ptrParam2 pointerType
    , defineFunction' differenceMultisetPairString ptrParam2 pointerType

    , defineFunction' unionFunctionString     ([("x", pointerType)
                                              , ("y", pointerType)] <> posParam)
                                              pointerType
    , defineFunction' intersectFunctionString  ptrParam2 pointerType
    , defineFunction' differenceFunctionString ptrParam2 pointerType

--------------------------------------------------------------------------------
    , defineFunction' multisetSumString            ptrParam2 pointerType
    , defineFunction' concatSequenceString         ptrParam2 pointerType

    , defineFunction' multiplicityMultiString      [ ("x", lintType)
                                                  , ("y", pointerType)]
                                                  intType
    , defineFunction' multiplicitySeqString        [ ("x", lintType)
                                                  , ("y", pointerType)]
                                                  intType

    , defineFunction' multisetPairSumString        ptrParam2 pointerType
    , defineFunction' concatSequencePairString     ptrParam2 pointerType

    , defineFunction' multiplicityMultiPairString  [ ("x", ptr tupleType)
                                                  , ("y", pointerType)]
                                                  intType

    , defineFunction' multiplicitySeqPairString    [ ("x", ptr tupleType)
                                                  , ("y", pointerType)]
                                                  intType

    , defineFunction' atSequenceString  ([ ("x", pointerType)
                                        , ("y", intType)] <> posParam) lintType
    , defineFunction' atSequencePairString       ([ ("x", pointerType)
                                                  , ("y", intType)] <> posParam)
                                                  tupleType

--------------------------------------------------------------------------------
    , defineFunction' relString                ptrParam   pointerType
    , defineFunction' funcString ([("x", pointerType)] <> posParam) pointerType

    , defineFunction' domainFuncString         ptrParam    pointerType
    , defineFunction' domainRelString          ptrParam    pointerType

    , defineFunction' codomainFuncString       ptrParam    pointerType
    , defineFunction' codomainRelString        ptrParam    pointerType

    , defineFunction' evalFuncString ([("x", pointerType), ("y", lintType)] <> posParam) lintType
    , defineFunction' evalRelString            ptri64Param pointerType

    , defineFunction' inverseFuncString        ptrParam    pointerType
    , defineFunction' inverseRelString         ptrParam    pointerType


--------------------------------------------------------------------------------
    -- Abort
    , defineFunction' abortString ([ ("x", intType)] <> posParam) voidType
    , defineFunction' warnString  ([("x", intType)] <> posParam) voidType
    -- Min and max
    , defineFunction' minnumString intParams2 intType
    , defineFunction' maxnumString intParams2 intType

    -- Line feed
    , defineFunction' lnString [] voidType

    -- Bool Write
    , defineFunction' writeBString boolParam voidType

    -- Char Write
    , defineFunction' writeCString charParam voidType

    -- Float Write
    , defineFunction' writeFString floatParam voidType

    -- Int Write
    , defineFunction' writeIString intParam voidType

    -- Pointer Write
    , defineFunction' writePString ptrParam voidType

    -- String Write
    , defineFunction' writeSString stringParam voidType

    -- Square Root and absolute value
    , defineFunction' sqrtString    floatParam floatType
    , defineFunction' fabsString    floatParam floatType

    , defineFunction' minnumFstring  floatParams2 floatType
    , defineFunction' maxnumFstring  floatParams2 floatType
    , defineFunction' powIString  ([("x", intType), ("y", intType)] <> posParam) intType
    , defineFunction' powString      floatParams2 floatType

    , defineFunction' (safeSub 64) [("x",lintType), ("y",lintType)] (overflow' 64)
    , defineFunction' (safeMul 64) [("x",lintType), ("y",lintType)] (overflow' 64)
    , defineFunction' (safeAdd 64) [("x",lintType), ("y",lintType)] (overflow' 64)

    , defineFunction' (safeSub 32) intParams2 (overflow' 32)
    , defineFunction' (safeMul 32) intParams2 (overflow' 32)
    , defineFunction' (safeAdd 32) intParams2 (overflow' 32)

    , defineFunction' (safeSub  8) charParams2 (overflow' 8)
    , defineFunction' (safeMul  8) charParams2 (overflow' 8)
    , defineFunction' (safeAdd  8) charParams2 (overflow' 8)

    -- Read
    , defineFunction' readIntStd    [] intType
    , defineFunction' readBoolStd   [] boolType
    , defineFunction' readCharStd   [] charType
    , defineFunction' readFloatStd  [] floatType
    , defineFunction' readlnString  [("ptr", ptr intType)] pointerType

    -- Rand
    , defineFunction' randIntString   [("ptr", pointerType)] voidType
    , defineFunction' randBoolString  [("ptr", pointerType)] voidType
    , defineFunction' randCharString  [("ptr", pointerType)] voidType
    , defineFunction' randFloatString [("ptr", pointerType)] voidType

    -- , defineFunction' randomize  [] voidType
    -- , defineFunction' seedRandom [intParam] voidType

    -- Malloc
    , defineFunction' mallocString   intParam pointerType
    -- , defineFunction' mallocTCString intParam pointerType

    , defineFunction' freeString ([("ptr", pointerType)] <> posParam) voidType

    , defineFunction' addPointerString    ([("ptr", pointerType)] <> posParam) voidType
    , defineFunction' removePointerString ([("ptr", pointerType)] <> posParam) voidType
    , defineFunction' derefPointerString  ([ ("ptr", pointerType)
                                              , ("pragma", boolType)] <> posParam)
                                          voidType


    , defineFunction' readFileInt   [("file", pointerType)] intType
    , defineFunction' readFileBool  [("file", pointerType)] boolType
    , defineFunction' readFileChar  [("file", pointerType)] charType
    , defineFunction' readFileFloat [("file", pointerType)] floatType
    , defineFunction' closeFileStr  [("file", pointerType)] voidType
    , defineFunction' openFileStr   [("name", pointerType)] pointerType
    ]

  where
    parameter (name, t) = Parameter t (mkName name) []
    defineFunction' name params ret = 
        defineFunction name (fmap parameter params) ret
    intParam      = [("x",     intType)]
    posParam      = [ ("filePathh", stringType)
                    , ("line", intType)
                    , ("column", intType)]
    charParam     = [("x",    charType)]
    boolParam     = [("x",    boolType)]
    floatParam    = [("x",   floatType)]
    ptrParam      = [("x", pointerType)]
    ptrParam2     = [("x", pointerType), ("y", pointerType)]
    ptri64Param   = [("x", pointerType), ("y", lintType)]
    ptrTupleParam = [("x", pointerType), ("y", ptr tupleType)]
    intParams2    = [("x",   intType), ("y",   intType)]
    charParams2   = [("x",  charType), ("y",  charType)]
    floatParams2  = [("x", floatType), ("y", floatType)]
    stringParam   = [("msg", stringType)]
    overflow' n   = StructureType False [IntegerType n, boolType]
    addFile file  = addDefinition $ LLVM.GlobalDefinition GlobalVariable
        { name            = mkName ("__" <> file)
        , linkage         = Private
        , visibility      = Default
        , dllStorageClass = Nothing
        , threadLocalMode = Nothing
        , addrSpace       = AddrSpace 0
        , unnamedAddr     = Nothing
        , isConstant      = False
        , type'           = pointerType
        , initializer     = Just . C.Null $ pointerType
        , section         = Nothing
        , comdat          = Nothing
        , alignment       = 4
      }


defineFunction name params t = LLVM.GlobalDefinition $ functionDefaults
  { name        = mkName name
  , parameters  = (params, False)
  , returnType  = t
  , basicBlocks = [] }
    