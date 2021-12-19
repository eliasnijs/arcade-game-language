module Evaluator where

import Parser
import Types

newtype Evaluator a =
  Evaluator { runEvaluation :: (a, State)
            }










