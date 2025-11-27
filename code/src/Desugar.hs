module Desugar where

import Grammar
import Prelude hiding (lookup)

type Delta = [(String, String)]

-- Construcciones que pertenecen al nucleo
data ASA
  = Num Double
  | Boolean Bool
  | Pair ASA ASA
  | Var String
  | Add ASA ASA
  | Mul ASA ASA
  | Div ASA ASA
  | Pow ASA ASA
  | Sqrt ASA
  | Gt ASA ASA
  | Geq ASA ASA
  | Eq ASA ASA
  | Not ASA
  | If ASA ASA ASA
  | Lambda String ASA
  | App ASA ASA
  | Nil
  | Lin
  | Fst ASA
  | Snd ASA
  | And ASA ASA
  | Or ASA ASA
  deriving (Eq,Show)

desugar:: SASA -> ASA
desugar sasa = auxDesugar sasa []
  where
    auxDesugar :: SASA -> Delta -> ASA
    auxDesugar (NumSrc n) _ = Num n
    auxDesugar (BooleanSrc b) _ = Boolean b
    auxDesugar (PairSrc a b) delta = Pair Lin (Pair (auxDesugar a delta) (auxDesugar b delta))
    auxDesugar (VarSrc i) delta = Var (lookup i delta)
    auxDesugar (NillSrc) _ = Nil
    auxDesugar (NegSrc a) delta = Mul (Num (-1)) (auxDesugar a delta)
    auxDesugar (AddSrc [a1, a2]) delta = Add (auxDesugar a1 delta) (auxDesugar a2 delta)
    auxDesugar (AddSrc (a1:an) ) delta = Add (auxDesugar a1 delta) (auxDesugar (AddSrc an) delta)
    auxDesugar (SubSrc  [a_1, a_2]) delta = Add (auxDesugar a_1 delta) (auxDesugar (NegSrc a_2) delta)
    auxDesugar (SubSrc (a_1:a_n)) delta = Add (auxDesugar a_1 delta) (auxDesugar (NegSrc (AddSrc a_n)) delta)
    auxDesugar (MulSrc [a_1, a_2]) delta = Mul (auxDesugar a_1 delta) (auxDesugar a_2 delta)
    auxDesugar (MulSrc (a_1:a_n)) delta = Mul (auxDesugar a_1 delta) (auxDesugar (MulSrc a_n) delta)
    auxDesugar (DivSrc [a_1, a_2]) delta = Div (auxDesugar a_2 delta) (auxDesugar a_1 delta)
    auxDesugar (DivSrc (a_1:a_n)) delta = Div (auxDesugar (DivSrc a_n) delta) (auxDesugar a_1 delta)
    auxDesugar (AndSrc [a_1, a_2]) delta = And (auxDesugar a_1 delta) (auxDesugar a_2 delta)
    auxDesugar (AndSrc (a_1:a_n)) delta = And (auxDesugar a_1 delta) (auxDesugar (AndSrc a_n) delta)
    auxDesugar (OrSrc [a_1, a_2]) delta = Or (auxDesugar a_1 delta) (auxDesugar a_2 delta)
    auxDesugar (OrSrc (a_1:a_n)) delta =  Or (auxDesugar a_1 delta) (auxDesugar (OrSrc a_n) delta)
    auxDesugar (PowSrc a b) delta = Pow (auxDesugar a delta) (auxDesugar b delta)
    auxDesugar (IncrSrc a) delta = Add (auxDesugar a delta) (Num 1)
    auxDesugar (DecrSrc a) delta = auxDesugar (SubSrc [a,NumSrc 1]) delta
    auxDesugar (SqrtSrc a) delta = Sqrt (auxDesugar a delta)
    auxDesugar (LtSrc l) delta = Not ( auxDesugar (GeqSrc l) delta)
    auxDesugar (GtSrc [a_1, a_2]) delta = Gt (auxDesugar a_1 delta) (auxDesugar a_2 delta)
    auxDesugar (GtSrc (a_1:(a_2:a_n))) delta = auxDesugar (AndSrc [GtSrc [a_1, a_2], GtSrc (a_2:a_n)]) delta
    auxDesugar (EqSrc [a_1, a_2]) delta = Eq (auxDesugar a_1 delta) (auxDesugar a_2 delta)
    auxDesugar (EqSrc (a_1:(a_2:a_n))) delta = auxDesugar (AndSrc [EqSrc [a_1, a_2], EqSrc (a_2:a_n)]) delta
    auxDesugar (LeqSrc a) delta = Not (auxDesugar (GtSrc a) delta)
    auxDesugar (GeqSrc [a_1, a_2]) delta = Geq (auxDesugar a_1 delta) (auxDesugar a_2 delta)
    auxDesugar (GeqSrc (a_1:(a_2:a_n))) delta = auxDesugar (AndSrc [GeqSrc [a_1, a_2], GeqSrc (a_2:a_n)]) delta
    auxDesugar (NeqSrc [a_1, a_2]) delta = Not (Eq (auxDesugar a_1 delta) (auxDesugar a_2 delta))
    auxDesugar (NeqSrc (a_1:(a_2:a_n))) delta =  Or (auxDesugar (NeqSrc [a_1,a_2]) delta) (auxDesugar (NeqSrc (a_2:a_n)) delta)
    auxDesugar (NotSrc a) delta = Not (auxDesugar a delta)
    auxDesugar (IfSrc c t e) delta = If (auxDesugar c delta) (auxDesugar t delta) (auxDesugar e delta)
    auxDesugar (CondSrc [(c, t)] e) delta = If (auxDesugar c delta) (auxDesugar t delta) (auxDesugar e delta)
    auxDesugar (CondSrc ((c_1,t_1):xs) e) delta = If (auxDesugar c_1 delta) (auxDesugar t_1 delta) (auxDesugar (CondSrc xs e) delta)
    auxDesugar (FstSrc a) delta = Fst (auxDesugar a delta)
    auxDesugar (SndSrc a) delta = Snd (auxDesugar a delta)
    auxDesugar (HeadSrc a) delta = Fst (Pair Lin (auxDesugar a delta))
    auxDesugar (TailSrc a) delta = Snd (Pair Lin (auxDesugar a delta))
    -- Desazucaramiento de listas como pares
    auxDesugar (ListSrc [a_1]) delta = Pair (auxDesugar a_1 delta) Nil
    auxDesugar (ListSrc (a_1:a_n)) delta = Pair (auxDesugar a_1 delta) (auxDesugar (ListSrc a_n) delta)
    auxDesugar (AppSrc f [e_1]) delta = App (auxDesugar f delta) (auxDesugar e_1 delta)
    auxDesugar (AppSrc f (e_1:e_n)) delta = App (auxDesugar (AppSrc f e_n) delta) (auxDesugar e_1 delta)
    auxDesugar (LambdaSrc [v_1] b) delta = Lambda v_1 (auxDesugar b ((v_1, v_1):delta))
    auxDesugar (LambdaSrc (v_1:v_n) b) delta = Lambda v_1 (auxDesugar (LambdaSrc v_n b) ((v_1, v_1 ):delta))
    auxDesugar (LetSSrc ([v_1],[e_1]) b) delta = App (Lambda v_1 (auxDesugar b ((v_1, v_1):delta))) (auxDesugar e_1 delta)
    auxDesugar (LetSSrc ((v_1:v_n),(e_1:e_n)) b) delta = App (Lambda v_1 (auxDesugar (LetSSrc (v_n, e_n) b) ((v_1, v_1):delta))) (auxDesugar e_1 delta)
    auxDesugar (LetPSrc (v,e) b) delta = auxLetPSrc (LetPSrc (v,e) b) (delta, delta)
              where
                auxLetPSrc :: SASA -> (Delta, Delta) -> ASA
                auxLetPSrc (LetPSrc ([v_1],[e_1]) b) (delta, delta') = let (s, s_i) = (asigntemp v_1 delta')
                           in App (Lambda (s_i) (auxDesugar b ((s, s_i):delta'))) (auxDesugar e_1 delta)
                auxLetPSrc (LetPSrc ((v_1:v_n), (e_1:e_n)) b) (delta, delta') = let (s, s_i) = (asigntemp v_1 delta')
                           in App (Lambda s_i (auxLetPSrc (LetPSrc (v_n,e_n) b) (delta, (s, s_i):delta'))) (auxDesugar e_1 delta)
    auxDesugar (LetRSrc i v c) delta = App (Lambda i (auxDesugar c ((i,i):delta))) (App z (Lambda i (auxDesugar v ((i,i):delta))))
    auxDesugar (AppendSrc a b) delta = Pair (auxDesugar a delta) (auxDesugar b delta)
    auxDesugar (ConcatSrc  a b) delta = auxDesugar (LetRSrc "@_c" (LambdaSrc ["@_a"] (LambdaSrc ["@_q"] (IfSrc (EqSrc [VarSrc "@_a",NillSrc]) (VarSrc "@_q") (AppendSrc (HeadSrc (VarSrc "@_a")) (AppSrc (VarSrc "@_c") [VarSrc "@_q",TailSrc (VarSrc "@_a")]))))) (AppSrc (VarSrc "@_c") [b,a])) delta
asigntemp :: String -> Delta -> (String, String)
asigntemp s [] = (s, (show 0) ++ "_" ++ s)
asigntemp s ((s',s''):xs)  
                      | s'== s'' = (s, (show 0) ++ "_" ++ s)
                      | otherwise = let (i, _) = (span (/= '_') s'')in (s, show ((read i) + 1) ++ "_" ++ s)

z :: ASA --Operador de punto fijo
z = Lambda "@_f" (
  App 
    (Lambda "@_x" (App (Var "@_f") (Lambda "@_u" (App (App (Var "@_x") (Var "@_x")) (Var "@_u"))))) 
    (Lambda "@_x" (App (Var "@_f") (Lambda "@_u" (App (App (Var "@_x") (Var "@_x")) (Var "@_u")))))
  )

lookup :: String -> Delta -> String
lookup s [] = s 
lookup s ((x,x'):ds)
    | s == x = x'
    | otherwise = lookup s ds
