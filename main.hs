

-- don't want to bother with Church numerals
data Nat = Zero | Succ Nat
data LamTerm where 
  Id :: String -> LamTerm
  LamLam :: String -> LamTerm -> LamTerm 
  LamApp :: LamTerm -> LamTerm -> LamTerm
  NatLit :: Nat -> LamTerm

data Context a where
  Nil :: Context a
  Cons :: a -> Context a -> Context a

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

alphaConversion :: String -> String -> LamTerm -> LamTerm
alphaConversion to from (Id s) = if s == from then Id to else Id s
alphaConversion to from (LamLam var term) = if var == from 
                                            then LamLam to (alphaConversion to from term) 
                                            else LamLam var (alphaConversion to from term)
alphaConversion to from (LamApp term1 term2) = LamApp (alphaConversion to from term1) (alphaConversion to from term2)
alphaConversion _ _ n = n

-- not yet consider context, free var should not captured by bounded var, need a variable context
substitude :: LamTerm -> String -> LamTerm -> LamTerm
substitude (Id s) var substitudeTerm = if s == var then substitudeTerm else (Id s)
substitude (LamLam boundedVar term) substitudeVar substitudeTerm = if substitudeVar == boundedVar 
  then (LamLam boundedVar term) 
  else (LamLam boundedVar (substitude term substitudeVar substitudeTerm))
substitude (LamApp term1 term2) var substitudeTerm = LamApp (substitude term1 var substitudeTerm) (substitude term2 var substitudeTerm)
substitude n _ _ = n

substitudeTerm1 = Id "x"
termTest = LamLam "x" (Id "y")
termAfter = substitude termTest "y" substitudeTerm1

main = print termAfter