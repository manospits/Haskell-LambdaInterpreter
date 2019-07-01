# Haskell-LambdaInterpreter
Group project with me and Panagiotis Fotopoulos.

Requirements: cabal and parsec

Load file skeleton.hs and run with the appropriate commands

examples:

> \>strue --string of l-term true

>"\\x.\\y.x"

> \>ltrue --haskell type l-term true

> Abstraction "x" (Abstraction "y" (Var "x"))

> \>lnot ltrue 

> Application (Abstraction "z" (Application (Application (Var "z") (Abstraction "x" (Abstraction "y" (Var "y")))) (Abstraction "x" (Abstraction "y" (Var "x"))))) (Abstraction "x" (Abstraction "y" (Var "x")))

> \>reduce (lnot ltrue) --functions returns number of b/h-redex steps, and type of reduction in each step

> Res (Abstraction "x" (Abstraction "y" (Var "y"))) 3 [Application (Abstraction "z" (Application (Application (Var "z") (Abstraction "x" (Abstraction "y" (Var "y")))) (Abstraction "x" (Abstraction "y" (Var "x"))))) (Abstraction "x" (Abstraction "y" (Var "x"))),Application (Application (Abstraction "x" (Abstraction "y" (Var "x"))) (Abstraction "x" (Abstraction "y" (Var "y")))) (Abstraction "x" (Abstraction "y" (Var "x"))),Application (Abstraction "y" (Abstraction "x" (Abstraction "y" (Var "y")))) (Abstraction "x" (Abstraction "y" (Var "x")))] ["b-redex","b-redex","b-redex"]

> \>getresult ( reduce (lnot ltrue)) --returns only the final term from reduce

> Abstraction "x" (Abstraction "y" (Var "y"))

> \>prettyprint (lfact 3) --prints a human readble format of the l-term

> "(\\f.(\\x.f(xx))(\\x.f(xx)))\n(\\f.\n \\n.\n (\\z.\\x.\\y.zxy)((\\x.x(\\x.\\z.z(\\x.\\y.y)x)(\\x.x)(\\x.\\y.x))n)(\\f.\\x.fx)\n ((\\n.\\m.\\f.n(mf))n(f((\\x.\\y.\\z.x(\\p.\\q.q(py))(\\y.z)(\\x.x))n))))\n(\\f.\\x.f(f(fx)))"


