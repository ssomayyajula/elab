{-# LANGUAGE LambdaCase #-}

module Language.Haskell.Elab.Internal.Proof where

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad (mzero)

-- Template Haskell
import Language.Haskell.TH.Syntax (Name, Type, Q, Exp)

import Data.Tree (Tree(..))
import Data.Tree.Zipper (TreePos, Full)

{-| A /sequent/ is a list of hypotheses and a conclusion
    Invariant: Hypothesis labels are unique. -}
type Sequent = ([(Name, Type)], Type)

-- | The type for computations involving extract terms
type TermState = MaybeT Q Exp

{-| The /validation/ of an inference rule produces an evidence term
    validating the sequent in question from its subgoals' terms. -}
type Validation = [Exp] -> TermState

data Term = Hole Name | Val Validation

type ProofNode = (Sequent, Term)

{-| A /refinement proof/ is a rose tree of sequents tagged with code
    generators---incomplete subgoals have typed holes, and proper
    subgoals have validations.
    
    Invariant: Incomplete subgoals never have subgoals
-}
type Proof = Tree ProofNode

{-| The proof state maintained by the elaborator is actually a proof
    tree zipper. -}
type ProofState = TreePos Full ProofNode

-- | Allows induction 
proof :: (Sequent -> Name -> a) ->
         (Sequent -> Validation -> [Proof] -> a) ->
         Proof -> a
proof f g = \case
  Node (s, Hole h) _   -> f s h
  Node (s, Val  v) sub -> g s v sub

incomplete s h = pure (s, Hole h)

(|-) :: [(Name, Type)] -> Type -> Sequent
(|-) = (,)

-- | An error indicating that a validation's input subgoals are
--   invalid for making the evidence term for the current subgoal.
invalidSubgoals :: TermState
invalidSubgoals = mzero
