module RaskParser (module RaskParser, parseFile, parse) where

import RaskAST 
import ParserCombinators
import Data.Char

-- an atom is just a symbol
atom :: Parser RaskExpr 
atom =  (symbol >>=: Sym)
        <|> boolean
        <|> numb

-- a symbol is a non-keyword identifier
symbol :: Parser SymName
symbol = identifier <=> (`notElem` reservedWords)


-- reserved words
reservedWords :: [String]
reservedWords = [ "head", "tail", "cons", "lambda", "quote", "if", "cond", "else", "let", "and", "not", "or", "list", "filter", "map", "append", "reverse", "foldl", "+", "-", "*", "/", "=", "<", ">", "<=", "<=", "#t", "#f"]

-- BASIC TYPES

-- the nil
nil :: Parser RaskExpr 
nil = text "()" >>: Nil

-- numbers
numb :: Parser RaskExpr
numb = number >>=: Num

-- boolean
boolean :: Parser RaskExpr
boolean =       (text "#t" >>: Bool True)
        <|>     (text "#f" >>: Bool False)

-- a quoted expression either uses the keyword `quote` or uses
-- a single quote (apostrophe) before the quoted expression
quoteExpr :: Parser RaskExpr 
quoteExpr =      (parens $ text "quote" <-+> expr    >>=: Quote)
            <|>  (text "\'" <-+> expr                >>=: Quote)

-- NUMERICAL EXPRESSIONS

-- a math expression is parenthesized and begins with an arithmetic operator
-- followed by 2 exprs that evaluate to numbers
mathExpr :: Parser RaskExpr
mathExpr =  (parens $ text "+" <-+> expr <+> expr >>=: uncurry Add)
        <|> (parens $ text "-" <-+> expr <+> expr >>=: uncurry Sub)
        <|> (parens $ text "*" <-+> expr <+> expr >>=: uncurry Mul)
        <|> (parens $ text "/" <-+> expr <+> expr >>=: uncurry Div)

-- a comp expression is parenthesized and begins with an arithmetic comparison
-- operator by two expressions that evaluate to numbers
compExpr :: Parser RaskExpr
compExpr = (parens $ text ">" <-+> expr <+> expr >>=: uncurry Gr)
        <|> (parens $ text "<" <-+> expr <+> expr >>=: uncurry Le)
        <|> (parens $ text "=" <-+> expr <+> expr >>=: uncurry Eq)
        <|> (parens $ text "=>" <-+> expr <+> expr >>=: uncurry GEq)
        <|> (parens $ text "<=" <-+> expr <+> expr >>=: uncurry LEq)

-- LOGICAL CONJUNCTIVES
-- a log expression is parenthesized and begins with either 'and' 'or'
-- or 'not', followed by two expressions (and, or) or one (not) that
-- evaluate to numbers
logExpr :: Parser RaskExpr
logExpr = (parens $ text "and" <-+> expr <+> expr >>=: uncurry And)
        <|> (parens $ text "or" <-+> expr <+> expr >>=: uncurry Or)
        <|> (parens $ text "not" <-+> expr >>=: Not)

-- CONDITIONALS
-- an if expression is parenthesized and begins with keyword 'if'
-- followed by three expressions, the first of which evaluates to
-- a bool and the second two of which evaluate to the same type
ifExpr :: Parser RaskExpr
ifExpr = parens $ text "if" <-+> expr <+> expr <+> expr >>=: \((v1, v2), v3) -> (If v1 v2 v3)

-- an cond expression is parenthesized and begins with keyword 'cond'
-- followed by one or more sets of two expressions in parentheses
-- the first of which evaluates to a bool and then all followed by
-- else and an expression parenthesized
condExpr :: Parser RaskExpr
condExpr = parens $ text "cond" <-+> (many (parens $ expr <+> expr)) <+> (parens $ text "else" <-+> expr) >>=: \(l, e) -> Cond (map fst l) (map snd l) e

-- LISTS & PAIRS
-- a head expression is parenthesized. it starts with the keyword `head`
-- and is followed by the expression we wish to take the head of 
headExpr :: Parser RaskExpr 
headExpr =  (parens $ text "head" <-+> expr >>=: Head)
        <|> (parens $ text "first" <-+> expr >>=: Head)

-- a tail expression is parenthesized. it starts with the keyword `tail`
-- and is followed by the expression we wish to take the tail/rest of 
tailExpr :: Parser RaskExpr 
tailExpr = parens $ text "tail" <-+> expr >>=: Tail

-- a cons expression is parenthesized. it starts with the keyword `cons`
-- and is followed by the two expressions that we want to cons together  
consExpr :: Parser RaskExpr
consExpr = parens $ text "cons" <-+> expr <+> expr >>=: uncurry Cons

-- a lists expression is parenthesized. it starts with the keyword `list`
-- and is followed by the 0 or 1 expressions to be in the list  
listsExpr :: Parser RaskExpr
listsExpr = parens $ text "list" <-+> (many expr) >>=: List

-- a map expression is parenthesized. it starts with the keyword 'map',
-- followed by a function expression and then an expression that is a list
mapExpr :: Parser RaskExpr
mapExpr = parens $ text "map" <-+> expr <+> expr >>=: uncurry Map

-- a filter expression is parenthesized. it starts with the keyword 'filter',
-- followed by a boolean unary function expression then a list expression
filterExpr :: Parser RaskExpr
filterExpr = parens $ text "filter" <-+> expr <+> expr >>=: uncurry Filter

-- an append expression is parenthesized. it starts with the keyword 'append',
-- followed by two expressions that evaluate to lists
appendExpr :: Parser RaskExpr
appendExpr = parens $ text "append" <-+> expr <+> expr >>=: uncurry Append

-- a reverse expression is parenthesized. it starts with the keyword 'reverse',
-- followed by a list
reverseExpr :: Parser RaskExpr
reverseExpr = parens $ text "reverse" <-+> expr >>=: Reverse

-- a fold expression is parenthesized. it starts with the keyword 'foldl',
-- followed by a binary function, a seed, and then a list
foldExpr :: Parser RaskExpr
foldExpr = parens $ text "foldl" <-+> expr <+> expr <+> expr >>=: \((v1, v2), v3) -> (Fold v1 v2 v3)

-- FUNCTIONS VARIABLES ETC
-- a let expression is parenthesized. it starts with the keyword `let`,
-- is followed by parenthesized sets of symbols and expressions,
-- and then a body expression to be evaluated with the bound symbols.
letExpr :: Parser RaskExpr
letExpr = parens $ text "let" <-+> (parens (many (parens $ symbol <+> expr))) <+> expr >>=: \(tuples, v3) -> Let (map fst tuples) (map snd tuples) v3

-- a lambda expression is parenthesized. it starts with the keyword `lambda`,
-- is followed by a parenthesized list of input parameters (symbols),
-- and then an expression (the body of the lambda function)
lambdaExpr :: Parser RaskExpr 
lambdaExpr = parens $ text "lambda" <-+> parens (many symbol) <+> expr >>=: uncurry Lambda

-- a function / procedure application expression is parenthesized. 
-- it starts with an expression (the function / procedure being called)
-- and is followed by zero or more expressions (the arguments to the function call)
applyExpr :: Parser RaskExpr 
applyExpr = parens $ expr <+> many expr >>=: uncurry Apply

-- a list expression is any one of the expressions that are represented
-- by parenthesized lists 
listExpr :: Parser RaskExpr
listExpr =      headExpr 
            <|> tailExpr
            <|> consExpr 
            <|> listsExpr
            <|> quoteExpr
            <|> mathExpr 
            <|> lambdaExpr 
            <|> applyExpr
            <|> ifExpr
            <|> logExpr
            <|> compExpr
            <|> mapExpr
            <|> appendExpr
            <|> reverseExpr
            <|> filterExpr
            <|> foldExpr
            <|> letExpr
            <|> condExpr
            <|> nil

-- an expression is either an atom or a list expression
expr :: Parser RaskExpr 
expr = atom <|> listExpr