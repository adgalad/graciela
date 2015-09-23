module Main where
  
import qualified Control.Monad.RWS.Strict as RWSS
import qualified LLVM.General.CodeGenOpt  as CodeGenOpt
import qualified LLVM.General.Relocation  as Reloc
import qualified LLVM.General.CodeModel   as CodeModel
import qualified Control.Applicative      as AP
import qualified Data.Foldable            as DF
import qualified Data.Text.IO             as TIO
import qualified Data.Text                as T
import Control.Monad.State                as ST
import Control.Monad.Identity
import System.Environment
import LLVM.General.Module
import LLVM.General.Context
import Control.Monad.Except
import LLVM.General.Target
import TokenParser
import SymbolTable
import Text.Parsec
import MyTypeError
import Expression
import VerTypes
import Data.Set (empty)
import ASTtype
import Codegen
import Parser
import State
import Lexer
import Token
import Type 
import AST


concatLexPar :: ParsecT T.Text () Identity (Either ParseError (Maybe (AST Type)), ParserState)
concatLexPar = playParser AP.<$> lexer


playLexer :: T.Text -> IO ()
playLexer inp = putStrLn $ show $ runParser lexer () "" inp


playParser :: [TokenPos] -> (Either ParseError (Maybe (AST Type)), ParserState)
playParser inp = runStateParse (program) "" inp initialState


runStateParse p sn inp init = runIdentity $ ST.runStateT (runPT p () sn inp) init


liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return


withTargetMipsMachine :: (TargetMachine -> IO ()) -> IO ()
withTargetMipsMachine f = do
    initializeAllTargets
    t <- liftError $ lookupTarget (Just "mips") ("x86-unknown-linux-gnu")
    withTargetOptions $ \options -> withTargetMachine (fst t) (snd t) 
               "mips32" empty options Reloc.Default CodeModel.Default CodeGenOpt.Default f
  

generateCode :: Module -> ExceptT String IO ()
generateCode m =
  do withDefaultTargetMachine $ \tm ->
      liftError $ writeObjectToFile tm (File "prueba") m


play :: T.Text -> IO ()
play inp = 
  case (runParser (concatLexPar) () "" (inp)) of
    { Left  err -> putStrLn $ "\nABORT: Ocurrio un error lexicografico\n" ++ (show err)
    ; Right par -> 
      case par of
        { (Left  err', _ ) -> putStrLn $ "\nABORT: Ocurrio un error en el proceso de parseo\n " ++ (show err')
        ; (Right (Just ast) , st) -> 
           let lErrType = DF.toList $ sTableErrorList st
               lErrSyn  = DF.toList $ synErrorList st
           in
             if (null lErrType) && (null lErrSyn) then
               do let (t, l) = runTVerifier (symbolTable st) ast 
                      l' = DF.toList l 
                  if not $ null l' then putStrLn $ show $ l'
                  else 
                    do let newast = astToLLVM $ fst $ runTVerifier (symbolTable st) ast
                       withContext $ \context ->
                          liftError $ withModuleFromAST context newast $ \m -> do
                            --liftError $ generateCode m
                            liftError $ writeLLVMAssemblyToFile (File "prueba.bc") m
             else 
               putStrLn $ drawState st
        ; (Right  _         , st) -> putStrLn $ drawState st
        }
    }


main :: IO ()
main = do args <- getArgs 
          s <- TIO.readFile (head args)
          play s
