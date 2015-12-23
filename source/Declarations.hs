module Declarations where

import qualified Control.Applicative as AP 
import qualified Control.Monad       as M
import qualified Data.Text           as T
import Contents                      as CO
import ParserState                 
import Text.Parsec
import TokenParser
import Expression
import ParserType
import Location
import Token
import State
import Type
import AST
import MyParseError

decList :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
decList follow recSet = 
    do loc <- parseLocation
       do parseVar
          idl <- idList (parseColon <|> parseAssign) (recSet <|> parseColon <|> parseAssign)
          do parseColon
             t <- myType parseSemicolon recSet
             addManyUniSymParser idl t
             do parseSemicolon
                rl <- decList follow recSet
                return $ AP.liftA2 (++) (fmap (map (\(name,loc') -> AST.ID loc' name t)) idl) rl
                <|> do genNewError follow SColon
                       return Nothing
             <|> do parseAssign
                    lexp <- consListParser parseColon (parseColon <|> recSet)
                    do parseColon
                       t <- myType parseSemicolon recSet
                       addManySymParser CO.Variable idl t lexp
                       do parseSemicolon
                          rl <- decList follow recSet
                          let idlist = fmap (map (\(id, loc) -> ID loc id t)) idl
                          return $ AP.liftA2 (:) (M.liftM4 LAssign idlist lexp (return loc) (return MyEmpty)) rl
                          <|> do genNewError follow SColon
                                 return Nothing
                       <|> do genNewError follow Colon
                              return Nothing
             <|> do genNewError follow AssignOrColon
                    return Nothing
          <|> do parseConst
                 idl <- idList (parseAssign) (recSet <|> parseAssign)
                 parseAssign
                 lexp <- consListParser parseColon (parseColon <|> recSet)
                 do parseColon
                    t <- myType parseSemicolon recSet
                    addManySymParser CO.Constant idl t lexp
                    parseSemicolon
                    rl <- decList follow recSet
                    let idlist = fmap (map (\(id, loc) -> ID loc id t)) idl
                    return $ AP.liftA2 (:) (M.liftM4 LAssign idlist lexp (return loc) (return MyEmpty)) rl
                    <|> do genNewError follow Colon
                           return Nothing

          <|> do return $ return []


consListParser :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
consListParser follow recSet = 
    do c <- expr (parseComma <|> follow) (parseComma <|> recSet)
       do parseComma
          l <- consListParser follow recSet
          return $ AP.liftA2 (:) c l
          <|> do return $ fmap (:[]) c


idList :: MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Location)])
idList follow recSet = 
    do lookAhead follow
       genNewEmptyError
       return $ Nothing 
       <|> do ac <- parseID
              loc <- parseLocation
              rl <- idListAux follow recSet
              return (fmap ((ac, loc) :) rl)


idListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Location)])
idListAux follow recSet =
  do parseComma
     ac <- parseID
     loc <- parseLocation
     rl <- idListAux (follow) (recSet)
     return (fmap ((ac, loc) :) rl)
     <|> do return $ return []

parseLocation :: MyParser (Location)
parseLocation =
    do pos <- getPosition
       return $ Location (sourceLine pos) (sourceColumn pos) (sourceName pos)
                  

reading follow ld = 
  do loc <- parseLocation
     do parseRead
        do parseLeftParent
           lid <- idList parseRightParent parseRightParent
           ts <- verifyReadVars lid
           do parseRightParent
              do parseWith
                 id <- parseString
                 addFileToReadParser id
                 do parseSemicolon
                    return $ AP.liftA2 (++) ld $ fmap (:[]) $ AP.liftA2 (Read loc (Just id) ts) lid (return MyEmpty)
                    <|> do genNewError follow SColon
                           return Nothing
                 <|> do parseSemicolon
                        return $ AP.liftA2 (++) ld $ fmap (:[]) $ AP.liftA2 (Read loc Nothing ts) lid (return MyEmpty)
                        <|> do genNewError follow SColon
                               return Nothing
              <|> do genNewError follow TokenRP
                     return Nothing
           <|> do genNewError follow TokenLP
                  return Nothing

decListWithRead :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
decListWithRead follow recSet = 
  do lookAhead (parseConst <|> parseVar)
     ld <- decList (follow <|> parseRead) (recSet <|> parseRead)
     do lookAhead parseRead 
        reading follow ld 
        <|> do return ld
     <|> do lookAhead parseRead
            reading follow (return [])
     <|> do return $ return []
