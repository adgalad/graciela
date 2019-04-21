{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Graciela.Parser.Module
  ( gModule
  , includes
  ) where
-------------------------------------------------------------------------------
import           Language.Graciela.Location           (initialPos)
import           Language.Graciela.Error
import           Language.Graciela.AST.Type
import           Language.Graciela.AST.Definition     (Definition(..), Definition'(..))
import           Language.Graciela.AST.Module         hiding (name,pragmas)
import qualified Language.Graciela.AST.Module         as M (Module(name,pragmas))
import           Language.Graciela.AST.Struct         (Struct(..), Struct'(..))
import           Language.Graciela.Common
import           Language.Graciela.Lexer              (lex)
import           Language.Graciela.Location           (Location (..))
import           Language.Graciela.Parser.Alias       (alias)
import           Language.Graciela.Parser.Config
import           Language.Graciela.Parser.Definition 
import           Language.Graciela.Parser.Enumeration 
import           Language.Graciela.Parser.Instruction (block)
import           Language.Graciela.Parser.Monad       hiding (sepBy1)
import           Language.Graciela.Parser.State
import           Language.Graciela.Parser.Struct
import           Language.Graciela.SymbolTable        (closeScope, openScope)
import           Language.Graciela.Token
-------------------------------------------------------------------------------
import           Control.Lens                         (use, (%=), (.=))
import           Control.Monad.State.Lazy             as State (MonadState(get))
import           Control.Monad.IO.Class               (liftIO)
import qualified Data.ByteString                      as BS (readFile, writeFile)
import           Data.Either
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (fromMaybe, fromJust, isJust)

import qualified Data.Sequence                        as Seq (empty, fromList)
import           Data.Serialize                       (encode, decode)
import qualified Data.Set                             as Set
import qualified Data.Text                            as T (intercalate)
import           Prelude                              hiding (lex)
import           Text.Megaparsec                      (eitherP, eof,
                                                       getPosition, optional,
                                                       sepBy1, failure,
                                                       (<|>))
import           Text.Megaparsec                      as MP (getInput,setInput, 
                                                       pushPosition, popPosition)
import           System.FilePath
import           System.Directory                     (doesFileExist, canonicalizePath)
import           System.Exit                          (die)
-------------------------------------------------------------------------------



gModule :: Parser (Maybe Module)
gModule = do
  from@SourcePos{sourceName} <- getPosition
  let text = pack sourceName
  stringIds %= \m -> case text `Map.lookup` m of
    Nothing -> let i = Map.size m in Map.insert text i m
    _ -> m

  includes -- parse include statements and move to the included files
  
  match' TokModule
  namePos <- getPosition
  name' <- safeIdentifier
  ext <- optional $ do
    match TokDot
    exts <- identifier `sepBy1` match TokDot
    pure $ "." <> T.intercalate "." exts
  
  case name' of 
    Nothing -> pure Nothing
    Just name -> do
      
      use modules >>= mapM_ (\m -> when (M.name m == name) $ 
        putError namePos . UnknownError $ 
          "Duplicate module `" <> unpack name 
          <> "`.\n\tMaybe you want to change the module's name.")


      match' TokBegin
      many $ eitherP (abstractDataType <|> dataType) $ eitherP (function <|> procedure) $ eitherP enum alias
      match' TokEnd
      to      <- getPosition
      defs    <- use definitions
      structs <- use dataTypes
      pend    <- use pendingDataType
      fdts'   <- use fullDataTypes
      strings <- use stringIds  

      -- Put pending data types as full data types
      forM_ (Map.toList fdts') $ \(name, typeargs) -> do
        case name `Map.lookup` pend of
          Just pending -> forM_ pending $ \name' -> do
            forM_ (Map.toList typeargs) $ \(x,_) ->
              let
                t = Map.fromList [(x, False)]
                fAlter = Just . \case
                  Nothing     -> t
                  Just types0 -> types0 `Map.union` t

              in fullDataTypes %= Map.alter fAlter name'
          Nothing -> pure ()
        
      let
        aux (name, typeArgs) = case name `Map.lookup` structs of
          Just struct -> (name, (struct, typeArgs))
          Nothing     -> internal $ "Couldn't find struct " <> show name
        fullStructs = Map.fromList $ aux <$> (Map.toList fdts')
      p <- use pragmas
      pure $ Just Module
        { M.name = name <> fromMaybe "" ext     
        , loc = Location (from, to)
        , defs
        , structs
        , fullStructs
        , M.pragmas = p
        , strings }

includes :: Parser ()
includes = do

  files <- many $ do 
    match TokInclude
    pos@(SourcePos file _ _) <- getPosition
    name <- stringLit  

    let fileName = takeDirectory file </> unpack name <.> "gcl"
    exists <- liftIO $ doesFileExist fileName  
    if exists 
      then liftIO $ canonicalizePath fileName >>= \f-> pure $ Just (f, pos)
        
      else do 
        putError pos . UnknownError $ 
         "Could not include `" <> fileName <> "`.\n\tThe file does not exist."
        fail ""
        
  defs  <- use definitions
  dts   <- use dataTypes
  fdts  <- use fullDataTypes    
  pend  <- use pendingDataType
  strings <- use stringIds
  currentPragmas <- use pragmas


  currentStream <- MP.getInput 
  stack <- use modulesStack
  modules' <- forM files $ \case 
    Nothing -> pure Nothing
    Just (fileName, pos)
      | fileName `elem` stack -> do
          putError pos . UnknownError $ "Circular dependency. Trying to include recursively `" 
            <> fileName <> "`"
          fail ""
      | otherwise -> do
        modules' <- use modules

        modulesStack %= (:) fileName
          
        case Map.lookup fileName modules' of     
          Nothing -> do
            -- let oFile = replaceExtension fileName "ogcl"
            -- existsOfile <- liftIO $ doesFileExist oFile
            
            -- if existsOfile then do
            --   bs <- liftIO $ BS.readFile oFile
            --   let eitherMod = (decode bs :: Either String Module)
            --   case eitherMod of 
            --     Left str -> internal str
            --     Right m -> do
            --       modulesStack %= tail
            --       modules %= Map.insert fileName m
            --       pure $ Just m
            -- else do
            source <- liftIO $ readFile fileName
            let 
              (tokens, pragmas') = lex fileName (pack source) 
            
            pragmas .= pragmas'
            
            let 
              prag = [EnableTrace, MemoryOperations, NoAssertions]
              [eT, mO, nA] = fmap (\x -> x `elem` pragmas') prag
              Config{nativeFunctions} = defaultConfig eT mO nA

            definitions     .= nativeFunctions
            dataTypes       .= Map.empty
            fullDataTypes   .= Map.empty
            pendingDataType .= Map.empty
            -- stringIds       .= Map.empty

            

            MP.setInput tokens
            MP.pushPosition $ initialPos fileName
            oldNErrors <- length <$> use errors
            m <- gModule
            newNErrors <- length <$> use errors 
            traceM $ "Compilo el archivo: " <> fileName
            when (isJust m) $
              modules %= Map.insert fileName (fromJust m)

            -- when (oldNErrors == newNErrors && isJust m) $
            --   liftIO $ BS.writeFile oFile (encode . fromJust $ m)

            modulesStack %= tail
            pure m
          savedModule -> do 
            traceM $ "Encontro el archivo ya compilado: " <> fileName
            modulesStack %= tail
            pure savedModule

  MP.popPosition
  MP.setInput currentStream
  definitions     .= defs 
  dataTypes       .= dts  
  fullDataTypes   .= fdts 
  pendingDataType .= pend
  pragmas         .= currentPragmas
  stringIds       .= strings

  forM_ (reverse . toList $ modules') $ \case 
    Nothing -> pure ()
    Just Module { defs = defs'
                , structs
                , fullStructs = m_fdts
                , strings = m_strings } -> do

      defs   <- use definitions
      dts    <- use dataTypes
      fdts   <- use fullDataTypes
      defs'' <- forM defs' $ \d -> pure $ d { isDecl  = True }

      let       
        defIntersect  = Map.intersection defs'' defs
        dtsIntersect  = Map.intersection structs  dts

      duplicatedDefs defIntersect
      duplicatedStructs dtsIntersect

      forM_ (Map.toList m_fdts) $ \(name, (_,types)) -> do
        let
          s = Map.fromList [(x,True) | (x,_) <- Map.toList types]
          fAlter = Just . \case
            Nothing     -> s
            Just types0 -> s `Map.union` types0
        fullDataTypes %= Map.alter fAlter name

      definitions %= Map.union defs''
      dataTypes   %= Map.union structs
      stringIds   %= Map.union m_strings

  pure () 

  where 
    duplicatedDefs :: Map Text Definition -> Parser ()
    duplicatedDefs list = do 
      defs <- use definitions
      forM_ list $ \(Definition {defLoc, defName}) -> do
        case Map.lookup defName defs of 
          Nothing -> internal "Something went wrong. Name is supposed to be in the map"
          Just d@Definition {defLoc = l@(Location (prevDef,_)) } 
            | l /= defLoc -> putError (pos defLoc) . UnknownError $ 
              "Ambiguous redefinition of " <> whichD d <>" `" <> unpack defName <> "`.\n\tAlready defined at "
               <> showRedefPos prevDef (pos defLoc) <> "."
          _ -> pure ()

    duplicatedStructs :: Map Text Struct -> Parser ()
    duplicatedStructs list = do 
      structs <- use dataTypes
      forM_ list $ \(Struct {structLoc, structBaseName}) -> do
        case Map.lookup structBaseName structs of 
          Nothing -> internal "Something went wrong. Name is supposed to be in the map"
          Just s@Struct {structLoc = l@(Location (prevDef, _)) } 
            | l /= structLoc -> putError (pos structLoc) . UnknownError $ 
              "Ambiguous redefinition of " <> whichS s <>" `" <> unpack structBaseName <> "`.\n\tAlready defined at "
               <> showRedefPos prevDef (pos structLoc) <> "."
          _ -> pure ()

    duplicatedFullStructs :: Map Text (Struct, a) -> Parser ()
    duplicatedFullStructs list = do 
      structs <- use dataTypes
      forM_ list $ \(Struct {structLoc, structBaseName}, _) -> do
        case Map.lookup structBaseName structs of 
          Nothing -> internal "Something went wrong. Name is supposed to be in the map"
          Just s@Struct {structLoc = l@(Location (prevDef, _)) } 
            | l /= structLoc -> putError (pos structLoc) . UnknownError $ 
              "Ambiguous redefinition of " <> whichS s <>" `" <> unpack structBaseName <> "`.\n\tAlready defined at "
               <> showRedefPos prevDef (pos structLoc) <> "."
          _ -> pure ()

    whichS Struct{ struct' } = case struct' of 
      AbstractDataType {} -> "Abstract Data Type"
      DataType {}         -> "Data Type"

    whichD Definition{ def' } = case def' of 
      FunctionDef {}          -> "function"
      GracielaFunc {}         -> "function"
      ProcedureDef {}         -> "procedure"
      AbstractProcedureDef {} -> "abstract procedure"
      AbstractFunctionDef {}  -> "abstract function"

