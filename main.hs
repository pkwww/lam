

-- don't want to bother with Church numerals
data Nat = Zero | Succ Nat
data LamTerm where 
  Id :: String -> LamTerm
  LamLam :: String -> LamTerm -> LamTerm 
  LamApp :: LamTerm -> LamTerm -> LamTerm
  NatLit :: Nat -> LamTerm

type Context a = [a]
type VarContext = Context String
type Variable = String

natToInt :: Nat -> Integer
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

instance Show Nat where
  show n = show (natToInt n)

instance Show LamTerm where
  show (Id s) = show s
  show (LamLam var term) = "\\" ++ show var ++ "." ++ show term
  show (LamApp term1 term2) = "(" ++ show term1 ++ ")(" ++ show term2 ++ ")"
  show (NatLit n) = show n

alphaConversion :: VarContext -> Variable -> Variable -> LamTerm -> LamTerm
alphaConversion context to from (Id s) = 
  if s == from 
  then if to `inContext` context
        then undefined
        else Id to 
  else Id s
alphaConversion context to from (LamLam var term) = 
  let extendContext = var : context in
    if var == from
    then LamLam to (alphaConversion extendContext to from term)
    else LamLam var (alphaConversion extendContext to from term)
alphaConversion context to from (LamApp term1 term2) = 
  LamApp (alphaConversion context to from term1) (alphaConversion context to from term2)
alphaConversion context _ _ n = n

inContext :: Variable -> VarContext -> Bool
inContext = elem

{- substitude :: LamTerm -> String -> LamTerm -> LamTerm
substitude (Id s) var substitudeTerm = if s == var then substitudeTerm else (Id s)
substitude (LamLam boundedVar term) substitudeVar substitudeTerm = if substitudeVar == boundedVar 
  then (LamLam boundedVar term) 
  else (LamLam boundedVar (substitude term substitudeVar substitudeTerm))
substitude (LamApp term1 term2) var substitudeTerm = 
  LamApp (substitude term1 var substitudeTerm) (substitude term2 var substitudeTerm)
substitude n _ _ = n -}

termTest = LamLam "x" (Id "y")

main = print termTest