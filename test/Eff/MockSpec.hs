{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TemplateHaskell  #-}

module Eff.MockSpec where

import           Control.Exception    (evaluate)
import           Data.GADT.Compare.TH (deriveGEq)
import           Data.GADT.Show.TH    (deriveGShow)
import           Data.Open.Union      (Union, inj)
import           Eff                  (Eff, Member, send)
import           Eff.Mock             (pattern (:->), DSumI, runMock, runMockU)
import           Test.Hspec

--------------------------------------------------------------------------------
                          -- Effect Model --
--------------------------------------------------------------------------------

data LogL s where
  Log :: String -> LogL ()

log' :: Member LogL r => String -> Eff r ()
log' = send . Log

deriveGEq ''LogL
deriveGShow ''LogL

data Teletype s where
  PutStrLn    :: String -> Teletype ()
  GetLine     :: Teletype String

deriveGEq ''Teletype
deriveGShow ''Teletype

putStrLn' :: Member Teletype r => String -> Eff r ()
putStrLn' = send . PutStrLn

getLine'  :: Member Teletype r => Eff r String
getLine' = send GetLine


spec :: Spec
spec = describe "Free mock" $ do
  it "works for getLine" $ do
    let prog = getLine'

    shouldBe (runMock [GetLine :-> "a line"] prog) "a line"

  it "works for a mix of actions" $ do
    let prog = getLine' >>= putStrLn'

    shouldBe (runMock [GetLine :-> "a line", PutStrLn "a line" :-> ()] prog) ()

  it "works for putStr" $ do
    let prog = putStrLn' "a line"
    let actions = [PutStrLn "a line" :-> ()]
    let result  = runMock actions prog
    shouldBe result ()

  it "will fail if action's dont match" $ do
    let prog = getLine'
    let actions = [PutStrLn "a line" :-> ()]
    evaluate ( runMock actions prog ) `shouldThrow` anyException

  it "will fail if action params dont match" $ do
    let prog = putStrLn' "not a line"
    let actions = [PutStrLn "a line" :-> ()]
    evaluate ( runMock actions prog ) `shouldThrow` anyException

  it "will fail if actions are left over" $ do
    let
      prog = putStrLn' "a"
      actions = [PutStrLn "a" :-> (), GetLine :-> "asd"]
      result = runMock actions prog
    evaluate result `shouldThrow` anyException

  it "can do log" $ do
    let prog = log' "line"

    let actions = [Log "line" :-> ()]
    let result  = runMock actions prog
    shouldBe result ()

  it "can do a mix of L's" $ do
    let
      prog :: Eff '[Teletype, LogL] ()
      prog = getLine' >>= log'

      actions :: [DSumI (Union '[Teletype, LogL])]
      actions =
        [ inj GetLine :-> "line"
        , inj (Log "line") :-> ()
        ]

      result = runMockU actions prog

    result `shouldBe` ()

  it "a mix of L's with failure" $ do
    let
      prog :: Eff '[Teletype, LogL] ()
      prog = getLine' >>= log'

      actions :: [DSumI (Union '[Teletype, LogL])]
      actions =
        [ inj GetLine :-> "line"
        , inj (Log "bad") :-> ()
        ]

      result = runMockU actions prog

    evaluate ( result ) `shouldThrow` anyException
