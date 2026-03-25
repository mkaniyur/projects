module RaskAST where 

import Data.Map (Map)
import qualified Data.Map as Map

type Env = Map SymName RaskExpr

type SymName = String
type NumVal = Integer
type BoolVal = Bool

data RaskExpr 
    = Nil                                   -- empty list, i.e. ()
-- basic types
    | Sym       SymName                     -- a symbolic name, e.g. a variable, like x
    | Num       NumVal                        -- a positive integer
    | Quote     RaskExpr                    -- quote the expr, do not evaluate
    | Bool      BoolVal                     -- true and false base values
-- arithmetic
    | Add       RaskExpr RaskExpr           -- binary arithmetic +
    | Mul       RaskExpr RaskExpr           -- binary arithmetic *
    | Sub       RaskExpr RaskExpr           -- binary arithmetic -
    | Div       RaskExpr RaskExpr           -- binary arithmetic /
-- comparisons
    | Gr        RaskExpr RaskExpr           -- binary arithmetic <
    | Le        RaskExpr RaskExpr           -- binary arithmetic >
    | Eq        RaskExpr RaskExpr           -- binary arithmetic =
    | GEq       RaskExpr RaskExpr           -- binary arithmetic <=
    | LEq       RaskExpr RaskExpr           -- binary arithmetic >=
-- logical conjunctives
    | And       RaskExpr RaskExpr           -- binary boolean and
    | Or        RaskExpr RaskExpr           -- binary boolean or
    | Not       RaskExpr                    -- unary boolean not
-- conditionals
    | If        RaskExpr RaskExpr RaskExpr      -- if statement
    | Cond      [RaskExpr] [RaskExpr] RaskExpr  -- cond statement
    | Let       [SymName] [RaskExpr] RaskExpr   -- let statement
-- list functionality
    | List      [RaskExpr]                  -- list of expressions
    | Head      RaskExpr                    -- first element of a list or cons
    | Tail      RaskExpr                    -- rest of a list / last elt of cons
    | Cons      RaskExpr RaskExpr           -- construct a list from the two expressions, e.g. Cons x xs is like x : xs
    | Map       RaskExpr RaskExpr           -- map function onto list
    | Append    RaskExpr RaskExpr           -- append two lists
    | Filter    RaskExpr RaskExpr           -- filter list according to bool fcn
    | Reverse   RaskExpr                    -- reverse a list
    | Fold     RaskExpr RaskExpr RaskExpr   -- foldl list with fucntion and seed
-- functions & environments
    | Lambda    [SymName] RaskExpr          -- anonymous function with multiple arguments: e.g. Lambda ["x", "y"] (Cons y x) 
    | Apply     RaskExpr [RaskExpr]         -- apply the first expression to the second        
    | Closure   Env [SymName] RaskExpr      -- internal snapshot the environment and function definition in a closure (not directly accessible by users) 
    | RaskError String                      -- for recording errors, also not directly accessible by users
    deriving (Show, Eq)

-- eventually, we want to show the user a nicely formatted string for output
-- that is, turn an expression into a string, which is the opposite of parsing!
-- hopefully this function is straighforward to understand
unparse :: RaskExpr -> String
-- basic types
unparse Nil = "()"
unparse (Sym s) = s
unparse (Num n) = show n
unparse (Quote q) = "\'" ++ unparse q
unparse (Bool True) = "#t"
unparse (Bool False) = "#f"
-- logical conjunctives
unparse (And bool1 bool2) = parenthesize ["and", unparse bool1, unparse bool2]
unparse (Or bool1 bool2) = parenthesize ["or", unparse bool1, unparse bool2]
unparse (Not bool) = parenthesize ["not", unparse bool]
-- comparisons
unparse (Gr num1 num2) = parenthesize [">", unparse num1, unparse num2]
unparse (Le num1 num2) = parenthesize ["<", unparse num1, unparse num2]
unparse (Eq num1 num2) = parenthesize ["=", unparse num1, unparse num2]
unparse (GEq num1 num2) = parenthesize ["=>", unparse num1, unparse num2]
unparse (LEq num1 num2) = parenthesize ["<=", unparse num1, unparse num2]
-- arithmetic
unparse (Add num1 num2) = parenthesize ["+", unparse num1, unparse num2]
unparse (Sub num1 num2) = parenthesize ["-", unparse num1, unparse num2]
unparse (Mul num1 num2) = parenthesize ["*", unparse num1, unparse num2]
unparse (Div num1 num2) = parenthesize ["/", unparse num1, unparse num2]
-- conditionals
unparse (If cond ifExpr elseExpr) = parenthesize ["if", unparse cond, unparse ifExpr, unparse elseExpr]
-- lists
unparse (Head  e) = parenthesize ["head", unparse e]
unparse (Tail e) = parenthesize ["tail", unparse e]
unparse (Cons e1 e2) = parenthesize ["cons", unparse e1, unparse e2]
unparse (List elems) = parenthesize $ ["list"] ++ map unparse elems
unparse (Map function list) = parenthesize $ ["map", unparse function, unparse list]
unparse (Reverse list) = parenthesize $ ["reverse", unparse list]
unparse (Append list1 list2) = parenthesize ["append", unparse list1, unparse list2]
unparse (Filter function list) = parenthesize ["filter", unparse function, unparse list]
unparse (Fold function seed list) = parenthesize ["foldl", unparse function, unparse seed, unparse list]
-- functions & environments
unparse (Lambda params body) = parenthesize $ ["lambda"] ++ params ++ [unparse body]
unparse (Apply expr args) = parenthesize $ [unparse expr] ++ map unparse args 

-- internal errors can get reported to users, even though they do not have internal access to them
unparse (RaskError e) = "RASKELL ERROR: " ++ e

-- closures are not visible to users, it is an internal implementation detail
unparse (Closure _ _ _) = error "attempt to print out internal closure" 

-- wrap a list of 
parenthesize :: [String] -> String
parenthesize exprs = "(" ++ unwords exprs ++ ")"
    
