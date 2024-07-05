module Pipeline.Verification.Strategy where

import Backend.Ast
import Backend.Utilities
import Backend.Simplifier (eSimplify)
import Pipeline.VIRGoTranslation.Encoding
import Pipeline.VIRGoTranslation.Clauses.CapPrecondition (capPreconditions)
import Pipeline.VIRGoTranslation.Clauses.Postcondition (postcondition)
import Pipeline.VIRGoTranslation.Summary.Summary
import Utilities.PrettyPrint

-- | Strategies contextualize how verification is performed. They drive with pre/postcondition construction.
data Strategy = Strategy {
  -- | Strategy name
  sname :: String,
  -- | A short name for the strategy, that can be added to e.g., a file name.
  shortName :: String,
  -- | A description of the strategy.
  description :: String,
  -- | Message to print if verification succeeds.
  successMessage :: Encoding -> String,
  -- | Encoding-to-precondition factory.
  makePrecondition :: Encoding -> Exp,
  -- | Encoding-to-postcondition factory.
  makePostcondition :: Encoding -> Exp,
  -- | Get the actual precondition, regardless of where it is featured in the encoding.
  realPrecondition :: Encoding -> Exp,
  -- | Encoding transformation strategy for strategies with code suggestions
  transformEncoding :: Encoding -> Encoding
}

-- | Wrap postcondition construction over entire encoding.
encodingToPostcondition :: Encoding -> Exp
encodingToPostcondition Encoding { summaries = ℳ { gs } } = postcondition gs

-- | Generate message from constraints.
generateConstraintMessage :: (Encoding -> Exp) -> Encoding -> String
generateConstraintMessage comPrecon encoding@Encoding { capacities = 𝜅 } =
  -- If the message is trivially tautological, do not generate the message
  let messagesFromTerm msg e = if e == (True ?)
        then []
        else [msg, "\t" ++ prettyPrint 0 e]
      -- Get simplified capacity constraints
      capExp = eSimplify (capPreconditions 𝜅 ...⋀)
      -- Get simplified communication constraints
      comExp = eSimplify $ comPrecon encoding
      -- Generate message from capacity constraints, if not trivial
      cap = messagesFromTerm "Constraints from capacities:" capExp
      -- Generate message from communication constraints, if not trivial
      comm = messagesFromTerm "Communication constraints:" comExp
  in unlines $ cap ++ comm

-- | Strategy 1. Verification is carried out under the weakest precondition (true).
-- If verification succeeds, partial deadlock freedom is demonstrated for all inputs.
strat1 :: Strategy
strat1 = Strategy {
  sname = "Strat. 1",
  shortName = "strat-1",
  description = unlines [
    "Strategy 1. The precondition is assumed to be `true`.",
    "If the encoding verifies, the model is partial deadlock-free and capacity safe for all concurrency parameter values."
  ],
  successMessage = \encoding -> unlines [
    generateConstraintMessage (const (True ?)) encoding,
    "The program has been validated as partial deadlock-free for all inputs."
  ],
  -- Precondition is the weakest possible
  makePrecondition = const (True ?),
  -- Postcondition only states that all processes should terminate without crashing.
  makePostcondition = encodingToPostcondition,
  -- In Strategy 1, the real precondition is the weakest possible
  realPrecondition = const (True ?),
  -- No encoding transformations required
  transformEncoding = id
}

-- | Strategy 2. Verification is carried out under the "balanced-flow" precondition in the WP position.
-- For a program P, the balanced-flow precondition is:
-- > ∀ c ∈ channels(P). receives(c) ≤ sends(c) ≤ receives(c) + κ(c)
-- > ∀ w ∈ waitgroups(P). adds(w) = 0
strat2 :: Strategy
strat2 = Strategy {
  sname = "Strat. 2",
  shortName = "strat-2",
  description = unlines [
    "Strategy 2. The precondition states that for every receive operation there should be a corresponding, and that the number of sends must not exceed the number of receives and the channel capacity.",
    "The precondition is placed in the weakest-precondition position.",
    "If the encoding verifies, the model is partial deadlock-free and capacity safe if and only if the concurrency parameters satisfy the precondition."
  ],
  successMessage = generateConstraintMessage balancedFlowPre,
  -- Precondition is incorporated in the postcondition under equivalence i.e.,
  -- the precondition implies partial deadlock freedom, and vice-versa.
  -- If satisfied, the precondition is also proven to be the weakest (modulo scheduling choices).
  makePrecondition = const (True ?),
  makePostcondition = \encoding -> balancedFlowPre encoding :<==> encodingToPostcondition encoding,

  -- In balanced flow, the real precondition includes capacity and balanced flow communication constraints.
  realPrecondition = balancedFlowPre,
  transformEncoding = id
}

-- | Strategy 3. Verification is carried out under the "balanced-flow" precondition.
-- For a program P, the balanced-flow precondition is:
-- > ∀ c ∈ channels(P). receives(c) ≤ sends(c) ≤ receives(c) + κ(c)
-- > ∀ w ∈ waitgroups(P). adds(w) = 0
strat3 :: Strategy
strat3 = Strategy {
  sname = "Strat. 3",
  shortName = "strat-3",
  description = unlines [
    "Strategy 3. The precondition states that for every receive operation there should be a corresponding, and that the number of sends must not exceed the number of receives and the channel capacity.",
    "If the encoding verifies, the model is partial deadlock-free if the concurrency parameters satisfy the precondition.",
    "However, the converse need not be true."
  ],
  successMessage = \encoding ->
    unlines [
      generateConstraintMessage balancedFlowPre encoding,
      "This condition may be more restrictive than necessary."
    ],
  -- Precondition is positioned normally.
  makePrecondition = balancedFlowPre,
  -- Post condition is the usual postcondition.
  makePostcondition = encodingToPostcondition,

  -- In balanced flow, the real precondition includes capacity and balanced flow communication constraints.
  realPrecondition = balancedFlowPre,
  transformEncoding = id
}
