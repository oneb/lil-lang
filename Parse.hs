module Parse where

import Data.Char 
import Data.Maybe
import Data.List 

-- Symboli
data Symbol = Symbol String
  deriving (Show, Eq, Ord)

-- Lauseke on symboli tai lista lausekkeita
data LispExpr = 
  Atom Symbol 
  | StringExpr String
  | IntExpr Integer
  | List [LispExpr]
  deriving Eq

-- Lausekkeen näyttäminen merkkijonona
instance Show LispExpr where
  show (Atom (Symbol s)) = s
  show (StringExpr s) = "\"" ++ s ++ "\"" 
  show (IntExpr n) = show n
  show (List exps) = "(" ++ r ++ ")" where
    r = concat $ intersperse " " $ map show exps  

-- Standardi parserityyppi ja sen Monad-instanssi
data Parser a = Parser (String -> [(a, String)])

instance Monad Parser where
  return x = Parser $ \s -> [(x, s)]
  (Parser f) >>= g = Parser $ \s -> concat (map 
    (\(a,s2) -> let (Parser h) = g a in h s2) (f s))

{- Palauta kaikki mahdolliset arvot joiksi annettu parseri
   voi parsia annetun merkkijonon -}
applyParser :: Parser a -> String -> [a]
applyParser (Parser h) s = [a | (a, _) <- h s]  

{- Parseri joka kuluttaa ja palauttaa yhden merkin, mutta vain jos
   merkki on annetussa listassa -}
parseChar :: [Char] -> Parser Char
parseChar cs = Parser h where
  h (c:s) | c `elem` cs = [(c, s)]
  h _ = []  

{- Muuta parseri joka lukee a-tyypin arvoja parseriksi joka tuottaa
   (Maybe a)-tyypin arvoja ja tuottaa onnistuneesti Nothing-arvon jos
   a:n lukeminen ei ole mahdollista -}
optional :: Parser a -> Parser (Maybe a)
optional (Parser h) = Parser h2 where
  h2 s = case h s of
    [] -> [(Nothing, s)]
    res -> [(Just x, s2) | (x, s2) <- res]

{- Muuta parseri joka lukee yhden a-tyypin arvon parseriksi joka
   tuottaa (mahdollisesti tyhjän) listan a-tyypin arvoja -}
many :: Parser a -> Parser [a]
many p = do
  ma <- optional p
  case ma of
    Nothing -> return []
    Just a -> do
      as <- many p
      return (a:as)

-- Nyt lista ei voi olla epätyhjä
many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p
  return (a:as)

end :: Parser ()
end = Parser f where
  f "" = [((),"")]  
  f _ = []

-- Kuin parseChar, mutta käyttää predikaattia listan sijaan
parsePred :: (Char -> Bool) -> Parser Char
parsePred p = Parser h where
  h (c:cs) | p c = [(c, cs)]
  h _ = []

-- Kuluta yksi tai useampi välimerkki ja/tai rivinvaihto
spaces :: Parser ()
spaces = do 
  many1 (parsePred isSpace)  
  return () 

{- Nolla tai useampi a-tyypin arvoksi parsiutuvaa merkkijonoa erotettuna
   b-tyypin arvoksi parsiutuvilla merkkijonoilla. Palautetaan lista 
   a-tyypin arvoista. -}
manySepBy :: Parser a -> Parser b -> Parser [a]
manySepBy p1 p2 = do
  ma <- optional p1
  case ma of
    Nothing -> return []
    Just a -> do
      mb <- optional p2
      case mb of 
        Nothing -> return [a]
        Just _ -> do
          as <- manySepBy p1 p2
          return (a:as)

{- Yrittää parsia p1:llä, ja jos se ei toimi yrittää parsia samaa 
   merkkijonoa p2:lla -}
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = do
  ma <- optional p1
  case ma of
    Nothing -> p2
    Just a -> return a

-- Kuluta ja heitä arvo pois
eat :: Parser a -> Parser ()
eat p = do
  p
  return ()

-- Sulkeilla ympäröity p:llä parsiutuva merkkijono 
parenthesized :: Parser a -> Parser a
parenthesized p = do
  parseChar "("
  a <- p
  parseChar ")"
  return a    

optionallySpaced :: Parser a -> Parser a
optionallySpaced p = do
  optional spaces
  a <- p
  optional spaces
  return a

-- Symboleihin ensimmäisen jälkeen kelpaavat merkit
symbolChars :: [Char]
symbolChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++
 "><=-!?_+*/"

-- Parsi ja palauta symboli
symbolExpr :: Parser LispExpr
symbolExpr = do
  c1 <- parsePred $ (\c -> (c `elem` symbolChars) && (not $ isDigit c))
  cs <- many (parseChar symbolChars)
  return $ Atom (Symbol (c1:cs))  

digits :: [Char]
digits = "0123456789"

-- Parsi jono merkkejä 0-9 kokonaisluvuksi
integer :: Parser Integer  
integer = do
  cs <- many1 (parseChar digits)
  return $ fromIntegral $ sum [ 
    (10^power) * (fromJust $ elemIndex digit digits) 
    | (power, digit) <- zip [0..] $ reverse cs ]

-- Parsi ja palauta numerolauseke 
intExpr :: Parser LispExpr
intExpr = do
  n <- integer
  return $ IntExpr n    

{- Parsi ja palauta merkkijonolauseke, joka on
   "-merkkien ympäröimä jono mitä tahansa muita merkkejä
   kuin rivinvaihto -}
stringExpr :: Parser LispExpr
stringExpr = do
  parseChar ['"']
  s <- many $ parsePred (\c -> 
    not $ c `elem` "\n\r\"")
  parseChar ['"']
  return $ StringExpr s

-- Parsi ja palauta lista
listExpr :: Parser LispExpr
listExpr = parenthesized $ do
  optional nonCode
  exps <- manySepBy lispExpr nonCode
  optional nonCode
  return $ List exps
  where
    nonCode = eat comment <|> spaces

-- Parsi ja palauta lauseke
lispExpr :: Parser LispExpr
lispExpr = symbolExpr <|> intExpr <|> stringExpr <|> listExpr

-- Kommentti
comment :: Parser String 
comment = optionallySpaced $ do
  parseChar "#"
  commentText <- many1 $ parsePred (/= '\n') 
  (end <|> eat (parseChar "\n"))
  return commentText 

{- Lue monta a:ta joiden ympärillä ja/tai seassa voi olla 
   rajoittamattomasti b:itä -}
manyIgnoring :: Parser a -> Parser b -> Parser [a]
manyIgnoring p ig = many $ do
  many ig 
  a <- p
  many ig
  return a

{- Parsi ja palauta ohjelma, eli lista lausekkeita erotettuna
   rivinvaihdoilla, välilyönneillä, puolipilkuilla tai ei 
   millään -}
program :: Parser [LispExpr]
program = do 
  exps <- lispExpr `manyIgnoring` 
    (eat comment <|> spaces)
  end
  return exps
