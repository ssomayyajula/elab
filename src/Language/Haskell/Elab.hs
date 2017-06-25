{-# LANGUAGE TemplateHaskell, LambdaCase, ApplicativeDo #-}

{-| Module      : Language.Haskell.Elab
    Description : The main interface to the proof elaborator
    Copyright   : (c) Siva Somayyajula, 2017
    License     : MIT
    Maintainer  : sks266@cornell.edu
    Stability   : experimental
    Portability : ...

    Proof elaboration... -}

module Language.Haskell.Elab (
  -- * The type of elaboration state
    Elab
  -- * Interacting with the elaborator
  , getProofState
  , getGoal
  , getHypotheses
  , getConclusion
  , putProofState
  , undo
  -- * Moving around the proof tree
  , up
  , top
  , left
  , right
  -- * Interfacing with Template Haskell
  , fromQ
  , fresh
  -- * Error handling
  , invalidDecomposition
  , emptyProofState
  , invalidMove
  ) where

import Prelude hiding (head, tail)

-- Monad transformer stack
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, runStateT, get, gets, put, modify)
import Control.Monad.Trans (lift)

-- Nonempty lists
import Data.List.NonEmpty (NonEmpty, fromList, head, tail, cons)

-- Template Haskell
import Language.Haskell.TH (Q, Name, Type, newName)

import Data.Tree.Zipper (parent, root, prev, next, toTree)

import Data.Maybe (maybe)

import Language.Haskell.Elab.Internal.Proof

data ProofError = InvalidDecomposition
                | InvalidSubgoals
                | InvalidMove
                | EmptyProofState

instance Show ProofError where
  show = \case
    InvalidDecomposition ->
      "Tactic expected conclusion of different type"
    InvalidMove ->
      "Attempted to move to a nonexistent subgoal"
    EmptyProofState ->
      "Action caused empty proof state"

-- | The elaborator state consists of the proof, code generation, and
--   error state.
--   
--   Terminology:
--     * /current subgoal/: the sequent currently focused by the zipper
type Elab = ExceptT ProofError (StateT (NonEmpty ProofState) Q)

-- | Returns the current proof state
getProofState :: Elab ProofState
getProofState = head <$> get

getGoal :: Elab Sequent
getGoal = proof const (\s _ _ -> s) <$> (tree <$> getProofState)

getHypotheses :: Elab [(Name, Type)]
getHypotheses = fst <$> getGoal

getConclusion :: Elab Type
getConclusion = snd <$> getGoal

putProofState :: ProofState -> Elab ()
putProofState = modify . cons

{--- A subgoal then 
addSubgoal :: Sequent -> Validation -> Name -> Elab ()
addSubgoal s' v' h =
  (proof (\s _     -> Prf s v' [toNode s' h])
         (\s v sub -> Prf s v $ append (toNode s' h) sub)
   <$> (toTree <$> getProofState)) >>= lift . modifyTree-}

{-| Undoes the last action. If there is no prior proof state, then an
    error is thrown. -}
undo :: Elab ()
undo = do
  ps <- gets tail
  if null ps then
    emptyProofState
  else
    put $ fromList ps

unsafeMove :: (ProofState -> Maybe ProofState) -> Elab ()
unsafeMove f = f <$> getProofState >>= maybe invalidMove putProofState

safeMove :: (ProofState -> ProofState) -> Elab ()
safeMove f = f <$> getProofState >>= putProofState

-- | Moves to the goal above (parent) the current one.
up :: Elab ()
up  = unsafeMove parent

-- | Moves to the top-most subgoal.
top :: Elab ()
top = safeMove root

-- | Moves to the subgoal to the left (previous) of the current one.
left :: Elab ()
left = unsafeMove prev

-- | Moves to the subgoal to the right (next) of the current one.
right :: Elab ()
right = unsafeMove next

-- | Lifts a computation from Q to the elaborator
fromQ :: Q a -> Elab a
fromQ = lift . lift

-- | Wrapper around TH's 'newName'
fresh :: String -> Elab Name
fresh = fromQ . newName

-- | An error indicating that a tactic got a conclusion of a different
--   type than it was expecting.
invalidDecomposition :: Elab a
invalidDecomposition = throwError InvalidDecomposition

-- | An error indicating that there is no proof state.
emptyProofState :: Elab a
emptyProofState = throwError EmptyProofState

invalidMove :: Elab a
invalidMove = throwError InvalidMove
