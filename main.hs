-- data LamType = INT | BOOL | Fun LamType LamType
data LamVar = MkVar String
data LamTerm = LamV LamVar | LamI Int | LamB Bool | LamLam LamVar LamTerm | LamApp LamTerm LamTerm

instance Show LamVar where
  show (MkVar s) = s

instance Show LamTerm where
  show (LamV var) = show var
  show (LamI x) = show x
  show (LamB b) = show b
  show (LamLam var term) = "\\" ++ show var ++ "." ++ show term
  show (LamApp term1 term2) = "(" ++ show term1 ++ ")(" ++ show term2 ++ ")"

varX = MkVar "x"
termX = LamV varX
one = LamI 1
lamIdentity = LamLam varX termX
app = LamApp lamIdentity one

main = print app