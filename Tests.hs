import Parse
import Eval

import Test.HUnit

main :: IO ()
main = runTests

-- Aja kaikki testit ja näytä tulokset.
runTests :: IO ()
runTests = do
  all <- allTests
  runTestTT all
  return ()

-- Kaikki testit yhdessä
allTests :: IO Test
allTests = fmap TestList $ sequence $ [
  return $ parsingTests,
  evalTests
  ]  

-- Testataan parsiutuvatko testimerkkijonot oikein.
parsingTests :: Test
parsingTests = TestList [
  TestCase $ assertEqual "Symboli" 
    [(Atom (Symbol "x"))] 
    (applyParser lispExpr "x"),
  TestCase $ assertEqual "Tyhjä lista" 
    [List []] 
    (applyParser lispExpr "()"),
  TestCase $ assertEqual "Numero" 
    [ IntExpr 2 ]
    (applyParser lispExpr "2"),
  TestCase $ assertEqual "Merkkijono" 
    [ StringExpr "hello world" ]
    (applyParser lispExpr "\"hello world\""),
  TestCase $ assertEqual "Monimutkainen lauseke" 
    [  List [IntExpr 0, List [], Atom (Symbol "b-9!"), 
       List [Atom (Symbol "c"), StringExpr "abc"], Atom (Symbol "d")] ]
    (applyParser lispExpr "( 0 ( ) b-9! (c \"abc\") d)")    
  ]

{- Sarja evaluaatiotestejä 
   Yhtenä Test-arvona IO:n sisällä -}
evalTests :: IO Test
evalTests = fmap TestList $ sequence [
  "(+ 1 1)" `evalToSame` "2",
  "(* 2 (+ 1 1))" `evalToSame` "4",
  "((lambda (x y) x) 1 2)" `evalToSame` "1",
  "((lambda (f) (f 3)) (lambda (x) (* 2 x)))" `evalToSame` "6",
  "(define x 1)   x" `evalToSame` "1",
  "(define sq (lambda (x) (* x x)))   (sq 2)" `evalToSame` "4",
  "(define x 1)   (set! x (+ 1 x))   x" `evalToSame` "2"
  ]

{- Testitapausten määrittelemisessä käytettävä apufunktio.
   Ottaa kaksi merkkijonoa, ja palauttaa IO:n sisällä testiassertion
   jonka mukaan ne kaksi ohjelmaaa joiksi merkkijonot parsiutuvat
   tuottavat uudessa globaalissa ympäristössä evaluoitaessa
   arvot joilla on sama merkkijonoesitys. 
   Jos arvot eivät edes parsiudu, palauttaa epäonnistuvan assertion
   jonka viesti kertoo tämän. -}
evalToSame :: String -> String -> IO Test
evalToSame p1 p2 = do
  case (applyParser program p1, applyParser program p2) of
    (exps1:_, exps2:_) -> do
      env <- mkGlobalEnv
      vals1 <- mapM (eval env) exps1
      vals2 <- mapM (eval env) exps2
      let res1 = show $ last vals1
      let res2 = show $ last vals2
      let explanation = "an evalToSame test of " ++ p1 ++ " and " ++ p2 ++ "."
      return $ TestCase $ assertEqual explanation res2 res1
    _ -> return $ TestCase $ assertFailure $ "parse failure in evalToSame of " 
      ++ p1 ++ " and " ++ p2 ++ "."

  
