{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.List
import Data.Char
import Text.Parsec
import Text.Parsec.String
import qualified Text.PrettyPrint as PP

data Term = Var String
  | Application Term Term
  | Abstraction String Term
  deriving(Show,Eq)

data Result = Res Term Int [Term] [String] deriving(Show,Eq)

-------------------- PARSER --------------------------------

lambdaTerm :: Parser Term
lambdaTerm = lambdaAbstraction <|> lambdaApplication <|> simple

lambdaAbstraction :: Parser Term
lambdaAbstraction = do
  char '\\'
  var <- letter
  char '.'
  body <- lambdaTerm
  return(Abstraction [var] body)

lambdaApplication :: Parser Term
lambdaApplication = do
  apps <- many1 simple
  return(foldl1 Application apps)

simple :: Parser Term
simple = lambdaVar <|> paren

lambdaVar :: Parser Term
lambdaVar = do
  var <- letter
  return(Var [var])

paren :: Parser Term
paren = do
  char '('
  term <- lambdaTerm
  char ')'
  return term

myparse :: String -> Term
myparse str = case (parse lambdaTerm "" str) of
  Left msg -> error $ show msg
  Right term' -> term'

test = myparse "\\z.(\\f.\\x.fzx)(\\y.y)"
pair = myparse "\\x.\\y.\\z.zxy"

----------------------- PRETTY PRINT ------------------------

ppr :: Term -> PP.Doc
ppr (Var x) = PP.text x
ppr (Abstraction x e) = PP.fcat [(PP.fcat [PP.text "\\",PP.text x,PP.text "."]),(ppr e)]
ppr apply = PP.fcat (map parenApp (args apply))


args (Application x y) = args x ++ [y]
args x = [x]

parenApp (x@(Application _ _)) = PP.parens (ppr x)
parenApp (x@(Abstraction _ _)) = PP.parens (ppr x)
parenApp x = ppr x

prettyprint :: Term -> String
prettyprint term = PP.render (ppr term)

-- Extra code by Giannos
myunwords :: [String] -> String
myunwords [] = []
myunwords (x:xs) = x ++ myunwords xs

trim :: String -> String
trim = myunwords . words

------------------------ TEST CASES ------------------------

inputString = "(\\q.\\x.\\y.y)(\\z.z)"
parseInputString = myparse inputString

myterm = Application (Abstraction "x" ( Abstraction "y"  (Var "x"))) (Abstraction "z" ( Var "z"))

prettyPrinted = prettyprint myterm
------------------------------------------------------------
strue = "\\x.\\y.x"
sfalse = "\\x.\\y.y"
snot = "\\z.z("++sfalse++")("++strue++")"
scond = "\\z.\\x.\\y.zxy"
spair = "\\x.\\y.\\z.zxy"
sfst = "\\z.z("++strue++")"
ssnd = "\\z.z("++sfalse++")"
sY = "\\f.(\\x.f(xx))(\\x.f(xx))"
s0 = "\\f.\\x.x"
s1 = "\\f.\\x.fx"
s2 = "\\f.\\x.f(fx)"
s3 = "\\f.\\x.f(f(fx))"
s4 = "\\f.\\x.f(f(f(fx)))"
s5 = "\\f.\\x.f(f(f(f(fx))))"
s6 = "\\f.\\x.f(f(f(f(f(fx)))))"
s7 = "\\f.\\x.f(f(f(f(f(f(fx))))))"
s8 = "\\f.\\x.f(f(f(f(f(f(f(fx)))))))"
s9 = "\\f.\\x.f(f(f(f(f(f(f(f(fx))))))))"
ssucc = "\\n.\\f.\\x.nf(fx)"
spred = "\\x.\\y.\\z.x(\\p.\\q.q(py))(\\y.z)(\\x.x)"
siszero = "\\x.x(\\x.\\z.z(\\x.\\y.y)x)(\\x.x)(\\x.\\y.x)"
sAplus = "\\n.\\m.\\f.\\x.nf(mfx)"
sAtimes = "\\n.\\m.\\f.n(mf)"
sAexp = "\\n.\\m.mn"

ltrue = myparse strue
lfalse = myparse sfalse
lsnot = myparse snot
lscond = myparse scond
lspair = myparse spair
lsfst = myparse sfst
lssnd = myparse ssnd
lsY = myparse sY
lsssucc = myparse ssucc
lspred = myparse spred
ls0 = myparse s0
ls1 = myparse s1
ls2 = myparse s2
ls3 = myparse s3
ls4 = myparse s4
ls5 = myparse s5
ls6 = myparse s6
ls7 = myparse s7
ls8 = myparse s8
ls9 = myparse s9
lsAplus = myparse sAplus
lsAtimes = myparse sAtimes
lsAexp = myparse sAexp


-- l_____ used for terms
-- p_____ used for strings

--NOT
lnot x = Application lsnot x
pnot x = "("++snot++")"++"("++x++")"
--COND
lcond b n m = Application (Application (Application lscond b) n) m
pcond b n m = "("++scond++")"++"("++b++")"++"("++n++")"++"("++m++")"
--PAIR
lpair n m = Application (Application lspair n) m
ppair n m = "("++spair++")"++"("++n++")"++"("++m++")"
lfst x = Application lsfst x
pfst x = "("++sfst++")"++"("++x++")"
lsnd x = Application lssnd x
psnd x = "("++ssnd++")"++"("++x++")"
--SUCC
lsucc x = Application lsssucc x
psucc x = "("++ssucc++")"++"("++x++")"
--PRED
lpred x = Application lspred x
ppred x = "("++spred++")"++"("++x++")"
--ISZERO
liszero x = Application (myparse siszero) x
piszero x = "("++siszero++")"++"("++x++")"
--Y COMBINATOR
lY x = Application lsY x
pY x = "("++sY++")"++"("++x++")"
--PLUS
lAplus x y = Application (Application lsAplus x) y
pplus x y = "("++sAplus++")"++"("++x++")"++"("++y++")"
--TIMES
lAtimes x y = Application (Application lsAtimes x) y
ptimes x y = "("++sAtimes++")"++"("++x++")"++"("++y++")"
--EXP
lAexp x y = Application (Application lsAexp x) y
pexp x y = "("++sAexp++")"++"("++x++")"++"("++y++")"

--CHURCHNUMERALS
dec2ch1 n a = if (n==1) then a else dec2ch1 (n-1) (getresult (reduce (lsucc a)))
d2ch n = case n of 1 -> myparse(s1)
                   0 -> myparse(s0)
                   otherwise-> dec2ch1 (n-1) (getresult(reduce (lsucc ls1)))

lch n = d2ch n
pch n = trim (prettyprint (d2ch n))

-- Decimal to Church Numeral parser (should be applied before myparse, if in the input string there are numbers that should be converted.)
-- (e.g. alternate calculation of 3! (instead of <trim (prettyprint (getresult (reduce (lfact 3))))>): <trim (prettyprint (getresult (reduce (myparse (church_num_parser (sfact++"3"))))))> )
church_num_parser :: String -> String
-- Takes a string, and replaces all numbers found with their church numeral (string) form (e.g. "2" becomes "(\\f.\\x.f(fx))", "ab2cd" becomes "ab(\\f.\\x.f(fx))cd" etc)
church_num_parser str = church_num_parser1 str 0 False

church_num_parser1 :: String -> Int -> Bool -> String
-- num is the number currently reading (if not reading, set to 0). Flag is the boolean value that shows if we are reading a number, so when it finishes it must be printed
church_num_parser1 [] num flag = if flag == True then "("++(pch num)++")" else ""
church_num_parser1 (x:xs) num flag = if (isDigit x == True) then church_num_parser1 xs (num*10 + (digitToInt x) ) True
                                     else if flag == True then "("++(pch num)++")"++[x]++(church_num_parser1 xs 0 False)
                                          else (x:(church_num_parser1 xs 0 False))

--FACT
sfact = pY ("\\f.\\n."++(pcond (piszero "n") (pch 1) (ptimes "n" ("f("++(ppred "n")++")" ))))
lsfact = myparse sfact
lfact x = Application lsfact (lch x)
pfact x = "("++sfact++")"++"("++(pch x)++")"

------------------------------------------------------------
remove x [] = []
remove x (y:ys)
  |(x==y) = remove x ys
  |(x/=y) = (y:remove x ys)

myunion [] [] = []
myunion x [] = x
myunion [] x = x
myunion (x:xs) (y:ys)
  |(x/=y)=(x:y:(remove y (remove x (myunion xs ys ))))
  |(x==y)=(x:(remove x (myunion xs ys)))

mymember y [] = False
mymember y (x:xs) = if y == x then True else mymember y xs

allvars=["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","x","y","z","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","X","Y","Z"]

removevars y [] = y
removevars y (x:xs) = removevars (remove x y) xs
------------------------------------------------------------
--Free Variables
freev aterm = case aterm of Var x -> [x]
                            Application x y ->myunion (freev x) (freev y)
                            Abstraction x y ->remove x (freev y)

allv aterm = case aterm of Var x -> [x]
                           Application x y ->myunion (allv x) (allv y)
                           Abstraction x y ->(x:remove x (allv y))
--Substitute
substitute1 aterm (Var v) bterm q = case aterm of Var x -> if x == v then bterm else aterm --case1,2
                                                  Application x y -> Application (substitute1 x (Var v) bterm q) (substitute1 y (Var v) bterm q) --case3
                                                  Abstraction x y -> if x==v
                                                                        then aterm --case4
                                                                        else if (not(mymember x (freev bterm))) || (not(mymember v (freev y)))
                                                                                then Abstraction x (substitute1 y (Var v) bterm q) --case 5
                                                                                else Abstraction (head q) (substitute1 (substitute1 y (Var x) (Var (head q)) q) (Var v) bterm q) --case 6

substitute aterm var bterm = substitute1 aterm var bterm (removevars allvars (myunion (allv aterm) (allv bterm)))

-- h-reduction
h_redex aterm = case aterm of
  Var x -> (aterm, False)
  Application x y -> if snd(h_redex x) == True then
                         (Application (fst (h_redex x)) y, True)
                     else (Application x (fst (h_redex y)), snd(h_redex y))
  Abstraction x y -> case y of
                            Var z -> (aterm, False)
                            Abstraction z w -> (Abstraction x (fst (h_redex y)), snd(h_redex y))
                            Application z w -> if (check_rightmost_term x w == True) && ((mymember x (freev z)) == False) then
                                                    (z, True)
                                               else
                                                    if snd(h_redex z) == True then
                                                        (Abstraction x (Application (fst (h_redex z)) w), True)
                                                    else (Abstraction x (Application z (fst (h_redex w))), snd(h_redex w))

--checks if the right term of an application is a VARIABLE equal to v
check_rightmost_term v aterm = case aterm of
  Var x -> if x == v then True else False
  Application x y -> False
  Abstraction x y -> False


b_redex aterm = case aterm of Var x -> (aterm,False)
                              Application x y -> case x of Abstraction l m -> (substitute m (Var l) y,True)
                                                           Application r q ->if (snd(b_redex x)==True)
                                                                                then (Application (fst (b_redex x)) y,True)
                                                                                else (Application x (fst(b_redex y)),snd(b_redex y))
                                                           Var u ->(Application x (fst(b_redex y)),snd(b_redex y))
                              Abstraction x y -> (Abstraction x (fst(b_redex y)),snd(b_redex y))


ireduce aterm a b c
  |((snd(h_redex aterm))==True)=ireduce (fst(h_redex aterm)) (a+1) (b++[aterm]) (c++["h-redex"])
  |((snd(b_redex aterm))==True)=ireduce (fst(b_redex aterm)) (a+1) (b++[aterm]) (c++["b-redex"])
  |((snd(b_redex aterm))==False && (snd(h_redex aterm))==False) =  Res aterm a b c

reduce aterm = ireduce aterm 0 [] []

-- Returns the final term (after all reductions)
getresult (Res aterm a b c) = aterm

