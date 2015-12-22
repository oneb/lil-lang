{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Parse
import Eval

import Control.Exception
import System.Exit (exitSuccess)
import System.IO
import System.Directory
import System.Environment
import System.Console.Readline
import Data.Maybe

main :: IO ()
main = do
  args <- getArgs
  env <- mkGlobalEnv
  -- Suorita prelude
  execFile env "prelude.lil" 
  case args of
    [] -> repl env
    fn:[] -> do 
      res <- execFile env fn
      case res of
        Left errmsg -> do
          putStrLn errmsg
        Right value -> do
          putStrLn $ show value

-- Aloita REPL
repl :: Env -> IO ()
repl env = do
  -- Suorita tulostuskomennot välittömästi, siis ei stdout-puskurointia
  hSetBuffering stdout NoBuffering
  putStrLn "Ready."
  putStrLn "Enter \"exit\" to exit."
  loop env
  where
    loop env = do
      -- Lue lauseke ja yritä parsia se
      input <- readline "> " 
      -- Jos syöte on "exit", lopetetaan
      case input of
        Just "exit" -> exitSuccess
        Nothing -> exitSuccess
        -- Jos syöte on tyhjä, aloitetaan loop alusta
        Just "" -> loop env
        -- Suorita ohjelma tiedostosta
        Just (':':fn) -> do
          er <- execFile env fn
          case er of
            Left err -> do 
              putStrLn err
              loop env
            Right _ -> do
              putStrLn "ok"
              loop env
        -- Muuten jatketaan
        _ -> return ()
      addHistory $ fromJust input
      case readProgram $ fromJust input of
        Nothing -> parseFailure
        Just exps -> do
          {- Evaluoi kaikki lausekelistan jäsenet 
             argumenttina saadussa ympäristössä. -}
          execResult <- exec env exps  
          case execResult of
            -- Evaluoidessa tapahtui virhe
            Left errMsg -> do
              putStrLn $ "evaluation error: " ++ errMsg
            Right val -> do
              putStrLn $ show val
          -- Loopataan alusta mahdollisesti muuttuneella ympäristöllä
          loop env
      where 
        {- Ilmoita parsimisen epäonnistumisesta 
           ja aloita loop alusta. -}
        parseFailure = do
          putStrLn $ "could not parse as expression"
          loop env

{- Evaluoi annetut lausekkeet annetussa ympäristössä ja
   palauta joko arvo joksi niistä viimeinen evaluoitui
   tai virheestä kertova merkkijono -}
exec :: Env -> [LispExpr] -> IO (Either String Value) 
exec env exps = do
  evalResult <- try $ evalSeq env exps 
  case evalResult of
    Left (e :: IOException) -> return $ Left $ show e
    Right val -> return $ Right val

{- Lue ja evaluoi tiedoston sisältämät lausekkeet annetussa 
   ympäristössä, ja palauta arvo tai virheestä kertova merkkijono -}
execFile :: Env -> FilePath -> IO (Either String Value) 
execFile env fp = do
  exists <- doesFileExist fp
  case exists of
    False -> return $ Left $ "could not find file: " ++ fp
    True -> do
      programText <- readFile fp
      case readProgram programText of
        Nothing -> return $ Left $ "could not parse file"
        Just exps -> exec env exps

{- Lue merkkijonon listaksi lausekkeita, tai Nothing-arvoksi
   jos sitä ei voi parsia. -}
readProgram :: String -> Maybe [LispExpr]
readProgram src = case applyParser program src of
  -- Niiden arvojen lista joksi src voi parsiutua on tyhjä.
  [] -> Nothing
  {- expss on kaikkien niiden ohjelmien lista, siis 
     lausekelistojen lista, joiksi syöte voi parsiutua. 
     Käytetään vain sen ensimmäistä alkiota, siis ensimmäistä
     lausekelistaa, minkä pitäisi joka tapauksessa olla sen ainoa
     alkio. -}
  expss -> Just $ head expss
