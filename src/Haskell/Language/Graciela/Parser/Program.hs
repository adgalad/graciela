{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Graciela.Parser.Program
  ( program
  ) where
-------------------------------------------------------------------------------
import           Language.Graciela.Location           (initialPos)
import           Language.Graciela.AST.Program
import           Language.Graciela.AST.Type
import           Language.Graciela.Common
import           Language.Graciela.Lexer              (lex)
import           Language.Graciela.Location           (Location (..))
import           Language.Graciela.Parser.Definition
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
import           Data.Either
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (fromMaybe)

import qualified Data.Sequence                        as Seq (empty, fromList)
import qualified Data.Set                             as Set (fromList, insert)
import qualified Data.Text                            as T (intercalate)
import           Prelude                              hiding (lex)
import           Text.Megaparsec                      (eitherP, eof,
                                                       getPosition, optional,
                                                       sepBy1, unsafePos, (<|>))
import           Text.Megaparsec                      as MP (getInput,setInput,pushPosition, popPosition)
import           System.Directory                     (doesFileExist)
import           System.Exit                          (die)
-------------------------------------------------------------------------------

-- MainProgram -> 'program' Id 'begin' ListDefProc Block 'end'

gModule :: Parser ()
gModule = do
  many include
  from <- getPosition
  match' TokModule
  match' TokBegin
  many $ eitherP (abstractDataType <|> dataType) (function <|> procedure)
  match' TokEnd
  pure ()




include :: Parser ()
include = do
  match TokInclude
  name' <- safeIdentifier
  case name' of 
    Nothing -> pure ()
    Just name -> do 
      let fileName = unpack (name <> ".gcl")

      liftIO $ doesFileExist fileName >>= \x -> do 
        unless x $ 
          die $ "\ESC[1;31m" <> "ERROR:" <> "\ESC[m" <>
           " Could not include `" <> fileName <> "`.The file does not exist."
        pure ()
      -- pure (doesFileExist (unpack fileName)) >>= \x -> unless x
      --   (error $ "\ESC[1;31m" <> "ERROR:" <> "\ESC[m" <>
      --          " The file `" <> fileName <> "` does not exist.")
      
      source <- liftIO $ readFile fileName
      let 
        (tokens, pragmas') = lex fileName (pack source)

      currentPragmas <- use pragmas
      pragmas .= pragmas'
      currentStream <- MP.getInput 
      MP.setInput tokens
      MP.pushPosition $ initialPos fileName
      gModule
      MP.popPosition
      MP.setInput currentStream

program :: Parser (Maybe Program)
program = do
  from <- getPosition
  
  symbolTable %= openScope from -- Open the program scope
  
  
  
  
  many include
  
  


  match' TokProgram
  name' <- safeIdentifier
  ext <- optional $ do
    match TokDot
    exts <- identifier `sepBy1` match TokDot
    pure $ "." <> T.intercalate "." exts
  match' TokBegin

  

  many $ eitherP (abstractDataType <|> dataType) (function <|> procedure)

  main' <- mainRoutine

  defs  <- Seq.fromList . toList <$> use definitions

  many $ eitherP (abstractDataType <|> dataType) (function <|> procedure)

    -- These aren't compiled since they can't be reached, but they're
    -- still checked so the user knows.

  match' TokEnd
  eof

  to <- getPosition
  symbolTable %= closeScope to

  case (name', main') of
    (Just name, Just main) -> do
      pend    <- use pendingDataType
      dts     <- use dataTypes
      fdts'   <- use fullDataTypes
      strings <- use stringIds


      -- Put pending data types as full data types
      forM_ (Map.toList fdts') $ \(name, typeargs) -> do
        case name `Map.lookup` pend of
          Just pending -> forM_ pending $ \name' -> do
            forM_ typeargs $ \x ->
              let
                fAlter = Just . \case
                  Nothing -> Set.fromList [x]
                  Just y  -> Set.insert x y

              in fullDataTypes %= Map.alter fAlter name'
          Nothing -> pure ()

      -- Take the new full data types
      fdts' <- use fullDataTypes
      -- internal $ show fdts'

      let
        aux (name, typeArgs) = case name `Map.lookup` dts of
          Just struct -> (name, (struct, typeArgs))
          Nothing     -> internal $ "Couldn't find struct " <> show name
        fdts = Map.fromList $ aux <$> (Map.toList fdts')

      pure $ Just Program
        { name        = name <> fromMaybe "" ext
        , loc         = Location (from, to)
        , defs
        , insts       = main
        , structs     = dts
        , fullStructs = fdts
        , strings }

    _ -> pure Nothing

  where
    mainRoutine = match' TokMain *> block
