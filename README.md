# elab

`elab` intends to offer an Idris-style elaboration monad to write. Internally, the proof state is held as a refinement-style proof tree zipper. As a result, tactics may range from refinement rules to moving around the proof tree. Using Template Haskell, computations in this monad can be spliced into Haskell code. Additionally, typed holes allow incomplete proofs to be spliced as partial programs.

## Progress

Currently, the internal state of the elaboration monad is implemented with trivial accessor functions. The next major step is to implement all of the rules of the second-order propositional refinement calculus and test whether such proofs can be spliced (extracted in logical terms) into Haskell code.

