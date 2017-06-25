module Language.Haskell.Elab.Logic where

intro :: String -> Elab Name
intro x = undefined {-getConclusion >>= \case
  ArrowT `AppT` a `AppT` b -> do
    x' <- fresh x
    addFocusedSubgoal
      ([(x', a)] |- b)
      (\case [b] -> lift [| \ $x' -> $b |]
              _  -> invalidSubgoals)
  _ -> invalidDecomposition-}
