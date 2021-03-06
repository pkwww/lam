import Data.List

type Context a = [a]
type VarContext = Context String
type Variable = String

-- don't want to bother with Church numerals
data Nat = Zero | Succ Nat
data LamTerm where 
  Id :: String -> LamTerm
  LamLam :: String -> LamTerm -> LamTerm 
  LamApp :: LamTerm -> LamTerm -> LamTerm

natToInt :: Nat -> Integer
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

instance Show Nat where
  show n = show (natToInt n)

instance Show LamTerm where
  show (Id s) = show s
  show (LamLam var term) = "\\" ++ show var ++ "." ++ show term
  show (LamApp term1 term2) = "(" ++ show term1 ++ ")(" ++ show term2 ++ ")"

alphaConversionHelper :: VarContext -> Variable -> Variable -> LamTerm -> LamTerm
alphaConversionHelper context to from (Id s) = 
  if s == from 
    then if to `inContext` context 
      then undefined 
      else Id to 
  else Id s
alphaConversionHelper context to from (LamLam var term) = 
  let extendContext = var : context in
    if var == from
      then LamLam to (alphaConversionHelper extendContext to from term)
      else LamLam var (alphaConversionHelper extendContext to from term)
alphaConversionHelper context to from (LamApp term1 term2) = 
  LamApp (alphaConversionHelper context to from term1) (alphaConversionHelper context to from term2)

alphaConversion :: Variable -> Variable -> LamTerm -> LamTerm
alphaConversion to from term = alphaConversionHelper [] to from term

inContext :: Variable -> VarContext -> Bool
inContext = elem

contextOf :: LamTerm -> VarContext
contextOf (Id s) = [s]
contextOf (LamLam var term) = var : (contextOf term)
contextOf (LamApp term1 term2) = (contextOf term1) `union` (contextOf term2)

substitudeHelp :: LamTerm -> VarContext -> Variable -> LamTerm -> LamTerm
substitudeHelp (Id s) _ var substitudeTerm = 
  if s == var 
    then substitudeTerm 
    else (Id s)
substitudeHelp (LamLam boundedVar term) context substitudeVar substitudeTerm = 
  if substitudeVar == boundedVar
    then (LamLam boundedVar term) 
    else if boundedVar `inContext` context
      then undefined
      else (LamLam boundedVar (substitudeHelp term context substitudeVar substitudeTerm))
substitudeHelp (LamApp term1 term2) context var substitudeTerm = 
  LamApp (substitudeHelp term1 context var substitudeTerm) (substitudeHelp term2 context var substitudeTerm)

substitude :: LamTerm -> Variable -> LamTerm -> LamTerm
substitude originalTerm var substitudeTerm = substitudeHelp originalTerm (contextOf substitudeTerm) var substitudeTerm

-- call by name
type Gas = Integer
eval :: LamTerm -> LamTerm
eval (Id s) = Id s
eval (LamLam var term) = LamLam var term
eval (LamApp idx@(Id _) term2) = (LamApp idx (eval term2))
eval (LamApp (LamLam var term) term2) = eval (substitude term var term2)
eval (LamApp app@(LamApp _ _) term2) = eval (LamApp (eval app) (eval term2))

termTest = LamLam "x" (Id "y")
subTerm = (Id "w")
afterTerm = substitude termTest "y" subTerm

main = print afterTerm