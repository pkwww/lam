

-- don't want to bother with Church numerals
data Nat = Zero | Succ Nat
data LamTerm where 
  Id :: String -> LamTerm
  LamLam :: String -> LamTerm -> LamTerm 
  LamApp :: LamTerm -> LamTerm -> LamTerm
  NatLit :: Nat -> LamTerm
 
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
alphaConversion to from n = n

--substitude :: LamTerm -> String -> LamTerm -> LamTerm


main = print ""