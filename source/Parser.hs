module Parser where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Text.Parsec.Pos as P
import qualified Control.Monad as M
import Data.Monoid
import Data.Either
import Location
import Token
import TokenParser
import State
import Lexer
import AST
import Declarations
import ParserState as PS
import Expression
import MyParseError as PE

data CasesConditional = CExpression | CAction

program :: MyParser (Maybe (AST (Maybe Int)))
program = do pos <- getPosition
             do try ( do parseProgram
                         do try ( do id  <- parseID
                                     do try ( do parseTokOpenBlock
                                                 parseRestInputProgram id
                                            )
                                        <|> do err <- genNewError (parseEnd) (PE.TokenOB)
                                               return $ Nothing
                                )
                            <|> do err <- genNewError (parseTokOpenBlock <|> parseEnd) (PE.Program)
                                   do try ( do parseTokOpenBlock
                                               merr <- parseRestInputProgram EmptyToken
                                               return $ Nothing
                                          )
                                      <|> do return $ Nothing

                    )
                <|> do err <- genNewError (parseID <|> parseEnd) (PE.Program)
                       do try (do parseID
                                  id  <- parseID
                                  parseTokOpenBlock
                                  merr <- parseRestInputProgram id
                                  return $ Nothing
                              )
                          <|> do return $ Nothing

followListDefProc = followAction <|> parseTokLeftA <|> parseTokLeftInv

parseRestInputProgram :: Token -> MyParser (Maybe (AST (Maybe Int)))
parseRestInputProgram id = do ast  <- listDefProc followListDefProc followListDefProc

                              do lookAhead followListDefProc
                                 do try ( do lacc <- actionsList parseTokCloseBlock parseTokCloseBlock
                                             parseTokCloseBlock
                                             parseEnd
                                             return (AP.liftA3 (AST.Program id) ast lacc (Just (Nothing))) 
                                         )
                                      <|> do genNewError parseEnd ProcOrFunc
                                             return $ Nothing

                                 <|> do genNewError followListDefProc ProcOrFunc
                                        do lookAhead parseEnd
                                           return $ Nothing
                                           <|> do lacc <- actionsList parseTokCloseBlock parseTokCloseBlock
                                                  do parseTokCloseBlock
                                                     parseEnd
                                                     return $ Nothing
                                                     <|> do genNewError (parseEnd) (PE.TokenCB)
                                                            return $ Nothing

--listDefProc :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST])
listDefProc follow recSet = do lookAhead follow
                               return $ return []
                               <|> do pf <- procOrFunc  follow recSet
                                      rl <- listDefProc follow recSet
                                      return (AP.liftA2 (:) pf rl)
                               <|> do return $ Nothing

--procOrFunc :: MyParser Token -> MyParser Token -> MyParser (Maybe AST)
procOrFunc follow recSet =  function follow recSet 
                        <|> proc     follow recSet

followTypeFunction = parseTokOpenBlock <|> parseTokLeftBound

--function :: MyParser Token -> MyParser Token -> MyParser (Maybe AST)
function follow recSet = do parseFunc
                            do id <- parseID
                               do parseColon
                                  do try ( do parseLeftParent
                                              lexp <- listArgFunc parseRightParent (recSet <|> parseRightParent)
                                              parseRightParent
                                              parseArrow
                                              t  <- myType (followTypeFunction) (recSet <|> followTypeFunction)
                                              bo <- maybeBound parseTokOpenBlock (recSet <|> parseTokOpenBlock)
                                              b  <- functionBody parseTokCloseBlock parseTokCloseBlock
                                              return(M.liftM4 (DefFun id) b lexp bo Nothing)
                                          )
                                     <|> do err <- genNewError follow ProcOrFunc
                                            return $ Nothing
                                  <|> do err <- genNewError follow Colon
                                         return $ Nothing
                               <|> do err <- genNewError follow IDError
                                      return $ Nothing


--listArgFunc :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST])
listArgFunc follow recSet = do lookAhead follow
                               return $ return []
                               <|> do id <- parseID
                                      parseColon
                                      t  <- myType (parseRightParent <|> parseComma) (recSet <|> parseRightParent <|> parseComma)
                                      rl <- listArgFuncAux follow recSet
                                      return (AP.liftA2 (:) (AP.liftA2 (FunArg id) t Nothing) rl)

--listArgFuncAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST])
listArgFuncAux follow recSet = do lookAhead follow
                                  return $ return []
                                  <|> do parseComma
                                         id <- parseID
                                         parseColon
                                         t <- myType (parseComma <|> follow) (parseComma <|> recSet)
                                         rl <- listArgFuncAux follow recSet
                                         return (AP.liftA2 (:) (AP.liftA2 (FunArg id) t Nothing) rl)
                                         

--proc :: MyParser Token -> MyParser Token -> MyParser (Maybe AST)
proc follow recSet = do parseProc
                        id <- parseID
                        parseColon
                        parseLeftParent
                        larg <- listArgProc parseRightParent parseRightParent
                        parseRightParent
                        parseLeftBracket
                        do        pre <- precondition (parseTokOpenBlock <|> parseTokLeftBound) (recSet <|> parseTokOpenBlock  <|> parseTokLeftBound)
                                  b   <- maybeBound (parseTokOpenBlock) (recSet <|> parseTokOpenBlock)
                                  parseTokOpenBlock                                  
                                  la <- actionsList parseTokCloseBlock (parseTokCloseBlock <|> recSet)
                                  parseTokCloseBlock
                                  post <- postcondition parseRightBracket (recSet <|> parseRightBracket)
                                  parseRightBracket
                                  return ((M.liftM5 (DefProc id) la larg pre post b) AP.<*> (return Nothing))

                           <|> do dcl <- decListWithRead parseTokLeftPre (parseTokLeftPre <|> recSet)
                                  pre <- precondition (parseTokOpenBlock <|> parseTokLeftBound) (recSet <|> parseTokOpenBlock  <|> parseTokLeftBound)
                                  b   <- maybeBound (parseTokOpenBlock) (recSet <|> parseTokOpenBlock)
                                  parseTokOpenBlock
                                  la <- actionsList parseTokCloseBlock (parseTokCloseBlock <|> recSet)
                                  parseTokCloseBlock
                                  post <- postcondition parseRightBracket (recSet <|> parseRightBracket)
                                  parseRightBracket
                                  return ((M.liftM5 (DefProcDec id) la larg dcl pre  post) AP.<*> b AP.<*> (return Nothing))

--precondition :: MyParser Token -> MyParser Token -> MyParser (Maybe AST)
precondition follow recSet =  do parseTokLeftPre
                                 e <- listExp (parseTokRightPre) (recSet <|> parseTokRightPre)
                                 parseTokRightPre
                                 return(AP.liftA2 (States Pre) e Nothing)

--postcondition :: MyParser Token -> MyParser Token -> MyParser (Maybe AST)
postcondition follow recSet =  do parseTokLeftPost
                                  e <- listExp (parseTokRightPost) (recSet <|> parseTokRightPost)
                                  parseTokRightPost
                                  return(AP.liftA2 (States Post) e Nothing)

--bound :: MyParser Token -> MyParser Token -> MyParser (Maybe AST)
bound follow recSet =  do parseTokLeftBound
                          e <- listExp (parseTokRightBound) (recSet <|> parseTokRightBound)
                          parseTokRightBound
                          return(AP.liftA2 (States Bound) e Nothing)

--assertion :: MyParser Token -> MyParser Token -> MyParser (Maybe AST)
assertion follow recSet =  do parseTokLeftA
                              e <- listExp (parseTokRightA) (recSet <|> parseTokRightA)
                              parseTokRightA
                              return(AP.liftA2 (States Assertion) e Nothing)

--invariant :: MyParser Token -> MyParser Token -> MyParser (Maybe AST)
invariant follow recSet =  do parseTokLeftInv
                              e <- listExp (parseTokRightInv) (recSet <|> parseTokRightInv)
                              parseTokRightInv
                              return(AP.liftA2 (States Invariant) e Nothing)

--maybeBound :: MyParser Token -> MyParser Token -> MyParser (Maybe AST)
maybeBound follow recSet = do lookAhead follow
                              return $ return $ EmptyAST
                              <|> bound follow recSet

--listArgProc :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST])
listArgProc follow recSet = do lookAhead follow
                               return $ return []
                               <|> do ar <- arg (follow <|> parseComma) (recSet <|> parseComma)
                                      rl <- listArgProcAux follow recSet
                                      return (AP.liftA2 (:) ar rl)

--listArgProcAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST])
listArgProcAux follow recSet = do lookAhead follow
                                  return $ return []
                                  <|> do parseComma
                                         ar <- arg (follow <|> parseComma) (recSet <|> parseComma)
                                         rl <- listArgProcAux follow recSet
                                         return (AP.liftA2 (:) ar rl)

--argType :: MyParser Token -> MyParser Token -> MyParser (Maybe TypeArg)
argType follow recSet = do parseIn 
                           return (return (In))
                           <|> do parseOut
                                  return (return (Out))
                           <|> do parseInOut
                                  return (return InOut)

--arg :: MyParser Token -> MyParser Token -> MyParser (Maybe AST)
arg follow recSet = do at <- argType parseID (recSet <|> parseID)
                       id <- parseID
                       parseColon
                       t <- myType follow recSet
                       return (AP.liftA3 (Arg id) at t Nothing)

--functionBody :: MyParser Token -> MyParser Token -> MyParser (Maybe AST)
functionBody follow recSet = do pos <- getPosition
                                do parseTokOpenBlock
                                   do cif <- conditional CExpression parseTokCloseBlock parseTokCloseBlock
                                      parseTokCloseBlock
                                      return (AP.liftA2 FunBody (AP.liftA2 (\f -> f (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) cif Nothing) Nothing)
                                      <|> do e <- expr parseTokCloseBlock parseTokCloseBlock
                                             parseTokCloseBlock
                                             return (AP.liftA2 FunBody e Nothing)

--actionsList :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST])
actionsList follow recSet = do lookAhead (follow)
                               genNewEmptyError
                               return $ Nothing
                               <|> do ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
                                      rl <- actionsListAux follow recSet
                                      return (AP.liftA2 (:) ac rl)
                                      
--actionsListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST])
actionsListAux follow recSet = do lookAhead follow
                                  return $ return []
                                  <|> do parseSemicolon
                                         ac <- action (follow <|> parseSemicolon) (recSet <|> parseSemicolon)
                                         rl <- actionsListAux follow recSet
                                         return (AP.liftA2 (:) ac rl)
                                  -- Aqui paso algo raro pero ese no es mi peo
                                  <|> (return $ Nothing)
                                         
--actionAux :: MyParser Token -> MyParser Token -> MyParser (Maybe (Location -> AST))
actionAux follow recSet = skip follow recSet
                      <|> conditional CAction follow recSet
                      <|> abort follow recSet
                      <|> write follow recSet
                      <|> writeln follow recSet
                      <|> functionCallOrAssign follow recSet
                      <|> repetition follow recSet
                      <|> random follow recSet
                      <|> block follow recSet

followAction = (parseDo <|> parseID <|> parseIf <|> parseAbort <|> parseSkip <|> parseTokOpenBlock <|> parseWrite <|> parseWriteln)

--block :: MyParser Token -> MyParser Token -> MyParser (Maybe (Location -> AST))
block follow recSet = do parseTokOpenBlock
                         ld <- decList followAction (recSet <|> followAction)
                         la <- actionsList (parseTokCloseBlock) (parseTokCloseBlock <|> recSet)
                         parseTokCloseBlock
                         return (AP.liftA2 (Block) ld la)
                       
--random :: MyParser Token -> MyParser Token -> MyParser (Maybe (Location -> AST))
random follow recSet = do parseRandom
                          id <- parseID
                          return(return (Ran id))

--guardsList :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe [AST])
guardsList casec follow recSet = do g  <- guard casec (parseSepGuards <|> follow) (parseSepGuards <|> recSet)
                                    gl <- guardsListAux casec follow recSet
                                    return(AP.liftA2 (:) g gl)

--guardsListAux :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe [AST])
guardsListAux casec follow recSet = do      lookAhead follow
                                            return $ return []
                                       <|>  do parseSepGuards
                                               g  <- guard casec (parseSepGuards <|> follow) (recSet <|> parseSepGuards)
                                               rl <- guardsListAux casec follow recSet
                                               return (AP.liftA2 (:) g rl)
                
--guard :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe AST)
guard CAction follow recSet     = do pos <- getPosition
                                     e <- expr (parseArrow) (recSet <|> parseArrow)
                                     parseArrow
                                     a <- action follow recSet
                                     return (AP.liftA2  (\f -> f (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) (AP.liftA2 Guard e a) Nothing)
guard CExpression follow recSet = do pos <- getPosition
                                     e <- expr (parseArrow) (recSet <|> parseArrow)
                                     parseArrow
                                     a <- expr follow recSet
                                     return (AP.liftA2 (\f -> f (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) (AP.liftA2 GuardExp e a) Nothing)

--functionCallOrAssign :: MyParser Token -> MyParser Token -> MyParser (Maybe (Location -> AST))
functionCallOrAssign follow recSet = do id <- parseID
                                        do try (do parseLeftParent
                                                   lexp <- listExp (follow <|> parseRightParent) (recSet <|> parseRightParent)
                                                   do parseRightParent
                                                      return(fmap (FCall id) lexp)
                                                         
                                                )
                                           <|> try ( do bl <- bracketsList (parseComma <|> parseAssign) (parseComma <|> parseAssign <|> recSet)
                                                        rl <- idAssignListAux parseAssign (recSet <|> parseAssign)
                                                        parseAssign
                                                        le <- listExp follow recSet
                                                        return (AP.liftA2 (LAssign) (AP.liftA2 (:) (fmap ((,) id) bl) rl) le)
                                                   )
                                
--idAssignListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe ([(Token, [AST])]))
idAssignListAux follow recSet = do lookAhead follow
                                   return $ return []
                                   <|> do parseComma
                                          ac <- parseID
                                          bl <- bracketsList (parseComma <|> parseAssign) (parseComma <|> parseAssign <|> recSet)
                                          rl <- idAssignListAux (follow) (recSet)
                                          return ((AP.liftA2 (:) (fmap ((,) ac) bl) rl))

--action :: MyParser Token -> MyParser Token -> MyParser (Maybe AST)
action follow recSet = do pos <- getPosition
                          do  as  <- assertion followAction (followAction <|> recSet)
                              res <- actionAux follow recSet
                              return (AP.liftA3 GuardAction as (AP.liftA2 (\f -> f (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) res Nothing) Nothing)
                              <|> do res <- actionAux follow recSet
                                     return (AP.liftA2 (\f -> f (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) res Nothing)
--write :: MyParser Token -> MyParser Token -> MyParser (Maybe (Location -> AST))
write follow recSet = do pos <- getPosition
                         parseWrite
                         parseLeftParent
                         e <- expr parseRightParent (recSet <|> parseRightParent)
                         parseRightParent
                         return $ (fmap (Write False) e)
           
--writeln :: MyParser Token -> MyParser Token -> MyParser (Maybe (Location -> AST))
writeln follow recSet = do pos <- getPosition
                           parseWriteln
                           parseLeftParent
                           e <- expr parseRightParent (recSet <|> parseRightParent)
                           parseRightParent
                           return $ (fmap (Write True) e)

--abort :: MyParser Token -> MyParser Token -> MyParser (Maybe (Location -> AST))
abort folow recSet = do pos <- getPosition
                        parseAbort
                        return $ return $ Abort

--conditional :: CasesConditional -> MyParser Token -> MyParser Token -> MyParser (Maybe (Location -> AST))
conditional casec follow recSet = do pos <- getPosition
                                     parseIf
                                     gl <- guardsList casec parseFi (recSet <|> parseFi)
                                     parseFi
                                     return(fmap (Cond) gl)
                 
--repetition :: MyParser Token -> MyParser Token -> MyParser (Maybe (Location -> AST))
repetition follow recSet = do pos <- getPosition
                              inv <- invariant (parseTokLeftBound) (recSet <|> parseTokLeftBound)
                              bou <- bound (parseDo) (parseDo <|> recSet)
                              parseDo
                              gl <- guardsList CAction parseOd (recSet <|> parseOd)
                              parseOd
                              return(AP.liftA3 Rept gl inv bou)
                               
--skip :: MyParser Token -> MyParser Token -> MyParser (Maybe (Location -> AST))
skip follow recSet = do parseSkip
                        return $ return $ Skip           
