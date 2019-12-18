
import Data.ByteString.Builder as B
import qualified Data.ByteString as BS

-- Expressions
data Expr
    = String Builder
    | Float Builder
    | Int Int
    | Bool Bool
    | Null
    | Array [Expr]

-- Statements
data Stmt 
    = Block [Stmt]
    | EmptyStmmt
    | ExprStmt
    | IfStmt Expr Stmt Stmt
    | Return Expr
    | Var Expr Name 

   
    
-- INDENT LEVEL


data Level =
    Level Builder Level
  
  levelZero :: Level
  levelZero =
    Level mempty (makeLevel 1 (BS.replicate 16 0x09 {-\t-}))
  
  
  makeLevel :: Int -> BS.ByteString -> Level
  makeLevel level oldTabs =
    let
      tabs =
        if level <= BS.length oldTabs
        then oldTabs
        else BS.replicate (BS.length oldTabs * 2) 0x09 {-\t-}
    in
    Level (B.byteString (BS.take level tabs)) (makeLevel (level + 1) tabs)


-- ENCODE

stmtToBuilder :: Stmt -> Builder
stmtToBuilder stmts =
  fromStmt levelZero stmts


exprToBuilder :: Expr -> Builder
exprToBuilder expr =
  snd $ fromExpr levelZero Whatever expr


-- STATEMENTS

fromStmtBlock :: Level -> [Stmt] -> Builder
fromStmtBlock level stmts =
  mconcat (map (fromStmt level) stmts)

fromStmt :: Level -> Stmt -> Builder
fromStmt level@(Level indent nextLevel) statement =
  case statement of
    Block stmts ->
      fromStmtBlock level stmts


      fromStmt :: Level -> Stmt -> Builder
      fromStmt level@(Level indent nextLevel) statement =
        case statement of
          Block stmts ->
            fromStmtBlock level stmts
      
          EmptyStmt ->
            mempty
      
          ExprStmt expr ->
            indent <> snd (fromExpr level Whatever expr) <> ";\n"
      
          IfStmt condition thenStmt elseStmt ->
            mconcat
              [ indent, "if (", snd (fromExpr level Whatever condition), ") {\n"
              , fromStmt nextLevel thenStmt
              , indent, "} else {\n"
              , fromStmt nextLevel elseStmt
              , indent, "}\n"
              ]
      

          Labelled label stmt ->
            mconcat
              [ indent, Name.toBuilder label, ":\n"
              , fromStmt level stmt
              ]
      

    
          Var name expr ->
            Var Name.toBuilder name <> " = " <> snd (fromExpr level Whatever expr) <> ";\n"
      

-- VAR DECLS

varToBuilder :: Level -> (Expr, Name) -> Builder
varToBuilder level (expr, name) =
  Name.toBuilder name <> " = " <> snd (fromExpr level Whatever expr)
      