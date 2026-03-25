module RaskEval where 

import RaskAST 
import RaskParser

import Data.Map (Map)
import qualified Data.Map as Map

-- ******************************
-- ENVIRONMENT HANDLING
-- ******************************

-- inserting a symbol and value into the environment is just a Map.insert
envInsert :: SymName -> RaskExpr -> Map SymName RaskExpr -> Map SymName RaskExpr
envInsert k v m = Map.insert k v m 

-- inserting several symbols and values into the environment is done
-- by recursively inserting one pair of symbol and value at a time
-- (note: this could be done with a fold as well)
envInsertMany :: [SymName] -> [RaskExpr] -> Map SymName RaskExpr -> Map SymName RaskExpr
envInsertMany [] [] m = m
envInsertMany (k:ks) (v:vs) m = envInsert k v (envInsertMany ks vs m) 
envInsertMany _ (v:vs) _ = error "arity mismatch: more args than params, should have been caught at interpretation time"
envInsertMany (k:ks) _ _ = error "arity mismatch: more params than args, should have been caught at interpretation time"

-- looking up a symbol is just a Map lookup, but if the symbol is not
-- found, we return an error that there is an unbound identifier
envLookup :: SymName -> Env -> RaskExpr
envLookup k m = Map.findWithDefault (RaskError $ "unbound identifier: " ++ k) k m

-- the initial environment is empty
initEnv :: Env
initEnv = Map.fromList []

-- HELPER FUNCTIONS
realNum :: RaskExpr -> Integer
realNum (Num n) = n

realBool :: RaskExpr -> Bool
realBool (Bool True) = True
realBool (Bool False) = False

-- ******************************
-- RUNNING AN EXPRESSION
-- ******************************

-- to run an expression, evaluate it in the initial environment
run :: RaskExpr -> RaskExpr
run expr = eval initEnv expr

-- ******************************
-- EVALUATION
-- ******************************

-- eval takes in an environment and an expression,
-- evalautated the expression in that environment,
-- and produces the resulting expression

eval :: Env -> RaskExpr -> RaskExpr

-- the expression () is an empty application
eval _ Nil = RaskError "empty application: RaskExprected procedure applied to 0 or more arguments"

-- to evaluate a symbol, look it up in the environment 
eval env (Sym v) = envLookup v env
eval env (Num n) = (Num n)
eval env (Bool b) = (Bool b)

-- a closure evaluates to itself
eval _ c@(Closure env v expr) = c

-- an error evaluates to itself
eval _ re@(RaskError e) = re

-- a quoted expression evaluates to itself
eval env (Quote q) = (Quote q)

-- NUMERICAL EXPRESSIONS

-- math expressions evaluate to their numerical value
eval env (Add num1 num2) = (Num ((realNum (eval env num1)) + (realNum (eval env num2))))
eval env (Sub num1 num2) = (Num ((realNum (eval env num1)) - (realNum (eval env num2))))
eval env (Mul num1 num2) = (Num ((realNum (eval env num1)) * (realNum (eval env num2))))
eval env (Div num1 num2) = (Num (div (realNum (eval env num1)) (realNum (eval env num2))))

-- evaluate to the boolean value of the comparison on the two nums
eval env (Gr num1 num2) = (Bool ((realNum (eval env num1)) > (realNum (eval env num2))))
eval env (Le num1 num2) = (Bool ((realNum (eval env num1)) < (realNum (eval env num2))))
eval env (Eq num1 num2) = (Bool ((realNum (eval env num1)) == (realNum (eval env num2))))
eval env (GEq num1 num2) = (Bool ((realNum (eval env num1)) >= (realNum (eval env num2))))
eval env (LEq num1 num2) = (Bool ((realNum (eval env num1)) <= (realNum (eval env num2))))


-- BOOLEAN EXPRS
-- true iff both expr1 and expr2 eval to true
eval env (And expr1 expr2) = (Bool ((realBool (eval env expr1)) && (realBool (eval env expr2))))
-- true iff either expr1 or expr2 eval to true
eval env (Or expr1 expr2) = (Bool ((realBool (eval env expr1)) || (realBool (eval env expr2))))
-- true iff expr evals to false
eval env (Not expr) = (Bool (not (realBool (eval env expr))))

-- CONDITIONALS

-- if expr
-- evaluates to the second or third expression depending on
-- whether the first expression evaluates true or false
eval env (If (Bool True) ifExpr elseExpr) = eval env ifExpr
eval env (If (Bool False) ifExpr elseExpr) = eval env elseExpr
eval env (If cond ifExpr elseExpr) = eval env (If (eval env cond) ifExpr elseExpr)

-- cond expr
-- evaluates to the first second expression in a set whose
-- first expression evaluates to true. otherwise the else expr.
eval env (Cond [] [] elseExpr) = eval env elseExpr
eval env (Cond [cond] [expr] elseExpr) = eval env (If cond expr elseExpr)
eval env (Cond (cond:conds) (expr:exprs) elseExpr) = eval env (If cond expr (Cond conds exprs elseExpr))

-- LISTS & PAIRS

-- a cons expression evaluates to a cons of the evaluated subexpressions
eval env (Cons e1 e2) = Cons (eval env e1) (eval env e2)

-- the head of a cons is the first expression, evaluated (car)
eval env (Head (Cons e1 e2)) = eval env e1
-- if the first expression is not already a Cons, evaluate it (hopefully returning a Cons!)
eval env (Head (List elems)) = eval env (head elems)
eval env (Head expr) = eval env (Head (eval env expr))

-- tail
-- the tail of a cons is the second expression, evaluated (cdr)
eval env (Tail (Cons e1 e2)) = eval env e2
eval env (Tail (List elems)) = eval env (List (tail elems))
eval env (Tail expr) = eval env (Tail (eval env expr))

-- list
-- evaluates the elements and puts them in a list 
eval env (List elems) = (List (map (eval env) elems))

-- applies the function to all elems and puts them back in a list
eval env (Map function (List elems)) = (List (map (\e -> (eval env (Apply function [e]))) elems))
eval env (Map function list) = eval env (Map (eval env function) (eval env list))

-- returns the list with only the elems that eval to true when passed 
-- to the function
eval env (Filter function (List elems)) = eval env (List (filter (\e -> (realBool (eval env (Apply function [e])))) elems))
eval env (Filter function list) = eval env (Filter (eval env function) (eval env list))

-- folds the list using the binary function starting with the seed.
eval env (Fold function seed (List elems)) = (foldl (\s e -> eval env (Apply function [s, e])) seed elems)
eval env (Fold function seed list) = eval env (Fold function seed (eval env list))

-- evals and appends the two expressions, which must be of type list after eval
eval env (Append (List list1) (List list2)) = eval env (List (list1 ++ list2))
eval env (Append list1 list2) = (Append (eval env list1) (eval env list2))

-- reverses list which must evaluate to a list expression
eval env (Reverse (List list)) = eval env (List (reverse list))
eval env (Reverse list) = eval env (Reverse (eval env list))

-- an anonymous function evaluates to a closure (snapshot the function body and params and the envirnonment)
eval env (Lambda params body) = Closure env params body

-- evaluates the body with the symbols being bound to the vals and in the env
eval env (Let syms vals body) = eval env (Apply (Lambda syms body) vals)

-- to evaluate an application, eval the function (which should eval 
-- to a Closure) then evaluate the arguments (call by value), 
-- then insert them into the environment along with the associated 
-- symbols (parameters) and evaluate the body in the new environment
eval env (Apply fexpr expr) = 
    let (Closure closureEnv syms body) = eval env fexpr 
        argvals = map (eval env) expr 
        env' = (envInsertMany syms argvals closureEnv)
        in
            eval env' body