{-# LANGUAGE ScopedTypeVariables #-}

module Eval where

import Parse

import qualified Data.Map as M
import Control.Exception
import Data.IORef
import Data.List
import System.IO

{- Evaluointi ja ympäristöt -}

{- Lausekkeen evaluoinnin tuottama arvo on joko 
     * lauseke,
     * proseduuriarvo joka koostuu sen parametrien nimien listasta,
       sen rungon koodista, ja sen sulkeumasta eli eräästä ympäristöstä,
     * tai primitiivinen proseduuri joka on funktio listasta 
       argumenttiarvoja tulosarvon tuottavaan IO arvoon -}
data Value = ExprValue LispExpr 
  | Procedure [Symbol] [LispExpr] Env
  | PrimitiveProcedure ([Value] -> IO Value)

-- Arvon näyttäminen
instance Show Value where
  show (ExprValue exp) = show exp
  show (Procedure syms _ _) = "<a procedure of " ++
    (show $ length syms) ++ " argument(s)>"
  show (PrimitiveProcedure _) = "<a primitive procedure>" 

{- Ympäristö on 
     * muutettava taulukko (frame) joka assosioi nimeen muutettavan
       arvon; 
     * ja mahdollisesti toinen ympäristö joka on
       sen ympäröivä ympäristö (enclosing environment) -}
data Env = Env
  { getBindingsRef :: IORef (M.Map Symbol (IORef Value)), 
    getEnclosing :: Maybe Env }   

{- Nimeä vastaavan muutettavan arvon hakeminen ympäristöstä.
   Jos nimi on ympäristön taulukossa, palauta sitä vastaava arvo.
   Jos ei ole, etsi rekursiivisesti ympäröivästä ympäristöstä.
   Jos ympäröivää ympäristöä ei ole, nimelle ei ole arvoa. -}
envLookup :: Env -> Symbol -> IO (Maybe (IORef Value))
envLookup env sym = do
  bindings <- readIORef $ getBindingsRef env
  case M.lookup sym bindings of
    Just rval -> return $ Just rval
    Nothing -> case getEnclosing env of
      Just env2 -> envLookup env2 sym
      Nothing -> return Nothing

{- Luo ympäristö jonka ympäröivä ympäristö on annettu ympäristö ja
   jonka taulukko sitoo annetut nimet annettuihin arvoihin.
   (ensimmäisen nimen ensimmäiseen arvoon, toisen toiseen, jne.) -}
extendEnv :: [Symbol] -> [Value] -> Env -> IO Env
extendEnv names values _ | length names /= length values
  = raise $ "attempt to extend environment with differing number of "
    ++ "names and values: " ++ show names ++ ", " ++ show values
extendEnv names values env = do
  valueRefs <- mapM newIORef values
  bindingsRef <- newIORef $ M.fromList (zip names valueRefs)
  return $ Env bindingsRef (Just env)

{- Muuta nimen arvon ympäristössä.
   Etsii envLookupilla nimeä vastaavan muutettavan arvon, ja muuttaa
   sen sisältämän arvon annettuun uuteen arvoon. -}
assignInEnv :: Symbol -> Value -> Env -> IO ()
assignInEnv sym newVal env = do
  mrVal <- envLookup env sym
  case mrVal of 
    Just rVal -> writeIORef rVal newVal
    Nothing -> do
      rVal <- newIORef newVal
      modifyIORef (getBindingsRef env) (M.insert sym rVal)

-- Nosta poikkeus annetulla viestillä.
raise :: String -> IO a
raise s = ioError $ userError s      

{- Muuttaa kaksipaikkaisen kokonaislukuoperaattorin primitiivifunktio-
   -arvoksi joka laskee saman operaation -}
intOpToPrimitive :: (Integer -> Integer -> Integer) => Value
intOpToPrimitive op = PrimitiveProcedure f where
  f ((ExprValue (IntExpr n)):(ExprValue (IntExpr m)):[]) = 
    return $ ExprValue (IntExpr (n `op` m))
  f args = raise $ "invalid arguments for arithmetic operation: " 
    ++ show args

-- Tulostaminen primitiivifunktiona 
printPrimitive :: Value    
printPrimitive = PrimitiveProcedure f where
  f values = do
    let s = concat $ intersperse " " $ map show values
    putStrLn s
    return $ ExprValue $ StringExpr s

{- Kutsuttaessa lukee yhden syöterivin ja palauttaa sen arvonaan.
   Kutsutaan ilman argumentteja. -}
readlinePrimitive :: Value
readlinePrimitive = PrimitiveProcedure f where 
  f values = case values of 
    [] -> do
      hSetBuffering stdout NoBuffering
      str <- getLine
      return $ ExprValue (StringExpr str)
    _ -> raise $ "readline called with non-empty operand list: " 
      ++ show values

-- True -> (quote true), False -> quote (false)
boolToValue :: Bool -> Value
boolToValue p = ExprValue (Atom (Symbol s)) where
  s = case p of
    True -> "true"
    False -> "false"

{- Vertaa kahden lausekkearvon yhtäsuuruutta -}
equalityPrimitive :: Value 
equalityPrimitive = PrimitiveProcedure f where
  f values = case values of
    (ExprValue a):(ExprValue b):[] -> return $ boolToValue $ a == b
    _ -> raise $ "invalid operands for ==: " ++ show values

{- Vertaa kahta numeroa -}
greaterThanPrimitive :: Value
greaterThanPrimitive = PrimitiveProcedure f where
  f values = case values of
    (ExprValue (IntExpr x)):(ExprValue (IntExpr y)):[] -> 
      return $ boolToValue $ x > y
    _ -> raise $ "invalid operands for >: " ++ show values

-- Alussa määritellyt globaalit muuttujat ja niiden arvot
initialGlobals :: [(Symbol, Value)]
initialGlobals = [ (Symbol str, v)| (str, v) <- [
  ("+", intOpToPrimitive (+) ),
  ("*", intOpToPrimitive (*) ),
  ("-", intOpToPrimitive (-) ),
  ("mod", intOpToPrimitive mod ),
  ("print", printPrimitive),
  ("==", equalityPrimitive),
  (">", greaterThanPrimitive),
  ("readline", readlinePrimitive)
  ]]

{- Luo globaalin ympäristön, eli ympäristön jolla ei ole ympäröivää
   ympäristöä ja jonka taulukko sisältää initialGlobalsin antamat
   arvot -}
mkGlobalEnv :: IO Env
mkGlobalEnv  = do
  valueRefs <- mapM newIORef $ [v | (s, v) <- initialGlobals]
  let names = [s | (s, v) <- initialGlobals]
  bindingsRef <- newIORef $ M.fromList $ 
    zip (map fst initialGlobals) valueRefs
  return $ Env bindingsRef Nothing    

-- Evaluoi lauseke arvoksi ympäristössä
eval :: Env -> LispExpr -> IO Value

{- esim: 1, "hello world" 
   Numero- tai merkkijonolauseke evaluoituu itsekseen. -}
eval _ exp | isSelfEvaluating exp = return $ ExprValue exp

{- esim: (lambda (x y) x), (lambda (a) (set! a (* a 2)) a)
   lambda-lauseke evaluoituu proseduuriarvoksi, jonka parametrien 
   nimien lista ja rungon koodi saadaan lausekkeesta ja jonka sulkeuma
   on ympäristö jossa lambda-lauseke evaluoitiin -}
eval env exp | isLambda exp = do
  params <- getLambdaParams exp
  body <- getLambdaBody exp
  return $ Procedure params body env

{- esim. x, i, square  
   Lauseke joka on pelkkä muuttujan nimi evaluoituu arvoksi joka
   löytyy kun etsitään nykyisestä ympäristöstä nimeä vastaava arvo.
   Jos arvoa ei löydy tästä ympäristöstä, tuloksena on virhe. -}
eval env exp | isVariable exp = do
  name <- getVariableName exp
  mrValue <- envLookup env name
  case mrValue of
    Just rValue -> readIORef rValue
    Nothing -> raise $ "unbound name: " ++ show name

{- esim: (quote a), (quote (x y)), (quote (() () ()))
   Lainauslauseke evaluoituu lainauksen sisällä olevaksi lausekkeeksi.
  -}
eval env exp | isQuoted exp = do
  quoted <- getQuotedExp exp
  return $ ExprValue quoted

{- esim: (if (== x y) 1 2), (if (> x 1) x 1)
   If-lausekkeen evaluoimiseksi evaluoidaan sen ehto,
   ja jos tuloksena on arvo joka on _truthy_, 
   evaluoidaan sen ensimmäinen haara ja palautetaan tulos.
   Muuten sen toinen haara.
  -}
eval env exp | isIf exp = do
  condition <- getIfCondition exp
  condValue <- eval env condition
  branchBody <- getIfBranch (isTruthy condValue) exp
  eval env branchBody 

{- esim: (set! i 0), (set! b (* 2 a))
   Ota nimi johon sidottua arvoa muutetaan, etsi sitä vastaava
   muutettava arvo nykyisestä ympäristöstä, ja muuta tämän muutettavan
   arvon sisältämä arvo siksi arvoksi joka saadaan kun evaluoidaan
   uuden arvon antava lauseke. -}
eval env exp | isAssignment exp = do
  name <- getAssignmentName exp
  toExp <- getAssignmentExp exp
  newVal <- eval env toExp
  assignInEnv name newVal env 
  return newVal

{- esim: (define square ((lambda (x) (* x x))), (define str "hello")
   Muuta nykyisen ympäristön taulukkoa niin että se sitoo
   halutun nimen siihen arvoon joka saadaan kun evaluoidaan
   haluttu lauseke nykyisessä ympäristössä. Vanha sitominen
   korvataan jos sellainen oli olemassa. -}
eval env exp | isDefinition exp = do
  name <- getDefinitionName exp
  asExp <- getDefinitionExp exp
  val <- eval env asExp
  rVal <- newIORef val
  modifyIORef (getBindingsRef env) (M.insert name rVal)
  return val  

eval env exp | isBegin exp = do
  exps <- getBeginExps exp
  evalSeq env exps  

{- esim. (f x), (g x y), (h), (square 2), ((lambda (x y) x) 0 1)
   Kun applikaatio evaluoidaan, evaluoidaan kaikki sen alilausekkeet
   nykyisessä ympäristössä ja sovelletaan (apply) ensimmäisen
   alilausekkeen evaluoinnin antama operaattoriarvo loppujen alilausekkeiden
   evaluointien antamaan operandiarvojen listaan. Operandiarvojen lista 
   voi olla tyhjä jos operaattori on nolla-argumenttinen funktio, 
   niin kuin lausekkeessa "(h)". -}
eval env exp | isApplication exp = do
  op <- getApplicationOperator exp
  ors <- getApplicationOperands exp
  opVal <- eval env op
  orVals <- mapM (eval env) ors
  apply opVal orVals

{- Applikoi annettu operaatioarvo annettuihin operandiarvoihin -}
apply :: Value -> [Value] -> IO Value
{- Ei-primitiivisen proseduurin soveltamiseksi arvoihin, 
   muodosta ympäristö jonka taulukko sitoo annetut proseduurin
   parametrien nimet annettuihin arvoihin ja jonka ympäröivä ympäristö
   on proseduurin sulkeuma. Evaluoi (eval) proseduurin rungon lausekkeet 
   järjestyksessä ensimmäisestä viimeiseen tässä ympäristössä,
   ja palauta haluttuna arvona viimeisen lausekkeen evaluoimisen
   antama arvo.
   Jos proseduurin parametrien listan pituus on eri kuin annetun
   operandilistan pituus, nostetaan virhe joka kertoo väärästä 
   agumenttien lukumäärästä. -}
apply (Procedure argNames body env) args 
  | length argNames /= length args = raise $
    "invalid number of arguments in application.\n" 
    ++ "parameters: " ++ show argNames ++ "\n"
    ++ "operands: " ++ show args
apply (Procedure argNames body env) args = do
  env2 <- extendEnv argNames args env
  mapM (eval env2) (init body)
  eval env2 (last body)
{- Primitiivisen proseduurin soveltamiseksi arvoihin, sovella 
   sen sisältämä funktio annettuihin arvoihin -}
apply (PrimitiveProcedure f) args = f args
apply val _ = raise $ "attempting to apply a non-procedure value: "
  ++ show val

----------

{- Predikaatteja lausekkeiden syntaksista ja lausekkeiden osia
   ekstraktoivia funktoita -}

-- Onko lauseke lista joka alkaa halutun merkkijono antamalla symbolilla?
starts :: String -> LispExpr -> Bool
starts s (List exps) = (length exps >= 1)
  && (head exps == Atom (Symbol s))
starts s _ = False  

{- eval-funktiossa käytetyt lausekepredikaatit.

   isFoo-predikaatti pätee lausekkeesta kun pätee että, jos 
   oletetaan että minkään ylemmän rivin  is*-predikaatti ei
   päde, ja oletetaan tarkistamatta että lauseke on jokin laillinen
   lauseke, niin lausekkeen on oltava Foo-lauseke. -}

{- Ylin is*-predikaatti.
   Itse-evaluoiva lauseke, siis numero- tai merkkijonolauseke -}
isSelfEvaluating :: LispExpr -> Bool
isSelfEvaluating exp = case exp of
  (StringExpr _) -> True
  (IntExpr _) -> True
  _ -> False

-- Lainattu lauseke
isQuoted :: LispExpr -> Bool
isQuoted = starts "quote"

-- Ehtolauseke
isIf :: LispExpr -> Bool
isIf = starts "if"

-- Lambdalauseke
isLambda :: LispExpr -> Bool
isLambda = starts "lambda"

-- Muuttujalauseke, siis pelkkä symboli
isVariable :: LispExpr -> Bool
isVariable exp = case exp of
  Atom _ -> True
  _ -> False

-- Asettamislauseke
isAssignment :: LispExpr -> Bool
isAssignment = starts "set!"

-- Määrittelylauseke
isDefinition :: LispExpr -> Bool
isDefinition = starts "define"

isBegin :: LispExpr -> Bool
isBegin = starts "begin"

-- Applikaatiolauseke
isApplication :: LispExpr -> Bool
isApplication exp = case exp of
  List _ -> True
  _ -> False

-- Lausekkeiden osia ekstraktoivia funktioita  

getQuotedExp :: LispExpr -> IO LispExpr
getQuotedExp exp = case exp of
  (List (_:quoted:[])) -> return quoted
  e -> raise $ "invalid application of quote: " ++ show e

getIfCondition :: LispExpr -> IO LispExpr
getIfCondition exp = case exp of
  (List (_:cond:_)) -> return cond
  _ -> raise $ "invalid if expression: " ++ show exp

getIfBranch :: Bool -> LispExpr -> IO LispExpr
getIfBranch which exp = case exp of
  (List (_:_:trueCase:falseCase:[])) -> case which of
    True -> return trueCase
    False -> return falseCase
  _ -> raise $ "invalid if expression: " ++ show exp

{- Lambda-lausekkeen parametrinimien lista, tai virhe 
   jos kyseessä ei ole laillinen lambda-lauseke -}
getLambdaParams :: LispExpr -> IO [Symbol]
getLambdaParams exp = case exp of
  (List (_:(List args):_)) -> sequence [ case arg of
    Atom sym -> return sym
    exp -> raise $ "invalid lambda parameter: " ++ show exp
    | arg <- args ]
  _ -> raise $ "lambda with invalid parameter list: " ++ show exp

{- Lambda-lausekkeen rungon muodostavien lausekkeiden lista,
   tai virhe -}
getLambdaBody :: LispExpr -> IO [LispExpr]
getLambdaBody exp = case exp of
  (List (_:_:body)) -> case body of
    [] -> raise $ "lambda with empty body"
    exps -> return exps
  exp -> raise $ "lambda with invalid body: " ++ show exp

-- Applikaatiolausekkeen operaattorilauseke
getApplicationOperator :: LispExpr -> IO LispExpr
getApplicationOperator exp = case exp of
  List (op:_) -> return op
  _ -> raise $ "invalid operator is application: " ++ show exp

-- Applikaatiolausekkeen operandilausekkeet
getApplicationOperands :: LispExpr -> IO [LispExpr]  
getApplicationOperands exp = case exp of
  List (_:ors) -> return ors
  _ -> raise $ "invalid operands in application: " ++ show exp

-- Asettamislausekkeen asetettu nimi
getAssignmentName :: LispExpr -> IO Symbol
getAssignmentName exp = case exp of
  List (_:(Atom sym):_) -> return sym
  _ -> raise $ "invalid name in assignment: " ++ show exp

-- Asettamislausekkeen asetettava arvon antava lauseke
getAssignmentExp :: LispExpr -> IO LispExpr
getAssignmentExp exp = case exp of
  (List (_:_:new:[])) -> return new
  _ -> raise $ "invalid assigned expression in assignment: " ++ show exp

-- Määrittelylausekkeen määriteltävä nimi
getDefinitionName :: LispExpr -> IO Symbol
getDefinitionName exp = case exp of
  List (_:(Atom sym):_) -> return sym
  _ -> raise $ "invalid name in definition: " ++ show exp

-- Määrittelylausekkeen määriteltävän arvon antava lauseke
getDefinitionExp :: LispExpr -> IO LispExpr
getDefinitionExp exp = case exp of
  (List (_:_:new:[])) -> return new
  _ -> raise $ "invalid bound expression in definition: " ++ show exp

getBeginExps :: LispExpr -> IO [LispExpr]
getBeginExps exp = case exp of
  (List (_:exps)) -> return exps
  _ -> raise $ "invalid begin expression: " ++ show exp

-- Muuttujalausekkeen muuttujan nimi
getVariableName :: LispExpr -> IO Symbol
getVariableName exp = case exp of
  Atom sym -> return sym
  _ -> raise $ "invalid variable expression: " ++ show exp

-- Muita tarvittavia funktioita

{- Kohdellaanko arvoa totena if-lausekkeessa?
   Kaikkea paitsi epätotuusarvoa false kohdellaan totena. -}
isTruthy :: Value -> Bool
isTruthy (ExprValue (Atom (Symbol "false"))) = False
isTruthy _ = True

{- Evaluoi jono lausekkeita järjestyksessä ympäristössä ja palauta
   niistä viimeisen tuottama arvo -}
evalSeq :: Env -> [LispExpr] -> IO Value
evalSeq env [] = raise $ 
  "attempted evaluation of empty sequence of expressions"
evalSeq env exps = do
  values <- mapM (eval env) exps 
  return $ last values
