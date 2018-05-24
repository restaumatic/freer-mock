{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeFamilies     #-}

module Eff.Mock
  ( module Eff.Mock
  , inj
  )where

import           Control.Monad.State   (State, runState)
import qualified Control.Monad.State   as State
import           Data.Dependent.Sum    (DSum (..))
import           Data.Functor.Identity (Identity (..))
import           Data.GADT.Compare     ((:~:) (Refl), GEq, geq)
import           Data.GADT.Show        (GShow, gshow)
import           Data.Open.Union       (Union, inj)
import           Data.Open.Union.Extra ()
import           Eff                   (Arr, Eff, run)
import           Eff.Extra.Fold        (foldEffM)
import qualified Eff.Internal          as EI
--------------------------------------------------------------------------------
                        -- Machinery --
--------------------------------------------------------------------------------


type DSumI f = DSum f Identity

pattern (:->) :: f a -> a -> DSumI f
pattern (:->) a b = a :=> Identity b

-- | Verify if any actions are left over after execution of a program
-- | Raise an exception if that is the case
verify :: (GShow f) => (w, [DSumI f]) -> w
verify (result, [])      = result
verify (_, actions) =
  error $ "Unused actions: " ++ foldMap (toString) actions
    where toString (action :=> _) = gshow action


{-|
runMock is used to mock out a series of freer actions from a single language.
If an action doesn't match an exception will be thrown.
If any action remains after execution, an error will be thrown.
We declare the mocked action in this form:
@
  GetLine :-> "a line"
@
This represents an action taking no arguments and returning "a line".
The language we are trying to mock needs to implement instances for GEq and GShow.
They can be derived using the `dependent-sum-template` package.
For example:
@
  data Teletype s where
    PutStrLn    :: String -> Teletype ()
    GetLine     :: Teletype String

  deriveGShow ''Teletype
  deriveGEq ''Teletype
@
Example usage:
@
  let prog = getLine' >>= putStrLn'
  shouldBe (runMock [GetLine :-> "a line", PutStrLn "a line" :-> ()] prog) ()
@
-}
runMock :: (GShow f, GEq f) => [DSumI f] -> Eff '[f] w -> w
runMock actions req =
  verify $ run (EI.handleRelayS actions (\s a -> pure (a, s)) go req)
  where
    go :: (GShow f, GEq f) =>
      [DSumI f]
       -> f v -> ([DSumI f] -> Arr '[] v (w, [DSumI f]))
       -> Eff '[] (w, [DSumI f])
    go [] action _ = error $ "Expected end of program, " ++ " given action: " ++ gshow action
    go ((action' :=> Identity r) : actions) action q
      | Just Refl <- geq action action' = q actions r
      | otherwise = error $ " argument mismatch: " ++ "  given: " ++
        gshow action ++ "\n" ++ "  expected: " ++ gshow action' ++ "\n"

{-|
Mock interpreter for a union of languages.
If an action doesn't match an exception will be thrown.
If any action remains after execution, an error will be thrown.
We declare the mocked action in this form:
@
  inj GetLine :-> "a line"
@
This represents an action taking no arguments and returning "a line" that is injected into the
Union of supported languages. Each language we are trying to mock,
needs to provide instances for GEq and GShow.
@
  data Teletype s where
    PutStrLn    :: String -> Teletype ()
    GetLine     :: Teletype String

  deriveGShow ''Teletype
  deriveGEq ''Teletype

  data LogL s where
    Log :: String -> LogL ()

  deriveGShow ''LogL
  deriveGEq ''LogL
@
Example usage:
@
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
@
-}

type MockActions effs = [DSumI (Union effs)]

runMockU :: forall effs a. (GShow (Union effs), GEq (Union effs))
  => [DSumI (Union effs)] -> Eff effs a -> a
runMockU actions req =
  verify $ runState (foldEffM handleAction req) actions
  where
    handleAction :: forall f a. (GEq f, GShow f) => f a -> State [DSumI f] a
    handleAction action = do
      actions <- State.get
      case actions of
        [] -> error $ "Expected end of program, " ++ " given action: " ++ gshow action
        ((action' :=> Identity r) : rest)
          | Just Refl <- geq action action'
              -> do State.put rest; pure r
          | otherwise
              -> error $ " argument mismatch: " ++ "  given: " ++
                   gshow action ++ "\n" ++ "  expected: " ++ gshow action' ++ "\n"
