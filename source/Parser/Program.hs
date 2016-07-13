module Parser.Program
  (program
  )where


-------------------------------------------------------------------------------
import           Parser.Instructions          (block)
import           Parser.Procedures            (listDefProc,panicMode,panicModeId)
import           Parser.TokenParser
import           Parser.ADT
import           MyParseError                  as PE
import           ParserState
import           Location
import           Graciela
import           Type
import           Token
import           AST
-------------------------------------------------------------------------------
import qualified Control.Monad       as M
import qualified Data.Text as T

import           Text.Parsec
-------------------------------------------------------------------------------

-- MainProgram -> 'program' Id 'begin' ListDefProc Block 'end'
mainProgram :: MyParser (Maybe (AST Type))
mainProgram = do
    pos <- getPosition
    newScopeParser
    panicMode parseProgram parseTokId PE.Program
    id <- panicModeId parseBegin
    panicMode parseBegin (parseProc <|> parseFunc <|> parseTokOpenBlock) PE.Begin


    ast  <- listDefProc parseTokOpenBlock parseTokOpenBlock
    lacc <- block parseEnd parseEnd
    try ( do parseEnd
             parseEOF
             return (M.liftM3 (AST.Program id (toLocation pos)) ast lacc (return (GEmpty)))
        )
        <|> do genNewError parseEOF PE.LexEnd
               return Nothing




-- Program -> Abstract Program
-- Program -> MainProgram
{- The program consists in a set of Abstract Data Types, Data Types and a main program -}
program :: MyParser (Maybe (AST Type))
program = do  many (abstractDataType <|> dataType) -- Por ahora debe haber un programa
              mainProgram                          -- principal al final del archivo



