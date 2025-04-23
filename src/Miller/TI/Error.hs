module Miller.TI.Error where

import Control.Effect.Throw
import Miller.Expr (Name)
import Miller.TI.Heap (Addr)
import Miller.TI.Node (Node)

data TIFailure
  = EmptyStack
  | NumberAppliedAsFunction
  | UnboundName Name
  | DeadPointer Addr
  | TooFewArguments Int Int
  | BadArgument Node
  | Unimplemented String
  deriving (Eq, Show)

unboundName :: (Has (Throw TIFailure) sig m) => Name -> m a
unboundName = throwError . UnboundName

deadPointer :: (Has (Throw TIFailure) sig m) => Addr -> m a
deadPointer = throwError . DeadPointer

badArgument :: (Has (Throw TIFailure) sig m) => Node -> m a
badArgument = throwError . BadArgument

emptyStack :: (Has (Throw TIFailure) sig m) => m a
emptyStack = throwError EmptyStack

unimplemented :: (Show info, Has (Throw TIFailure) sig m) => info -> m a
unimplemented = throwError . Unimplemented . show

numberAppliedAsFunction :: (Has (Throw TIFailure) sig m) => m a
numberAppliedAsFunction = throwError NumberAppliedAsFunction

tooFewArguments :: (Has (Throw TIFailure) sig m) => Int -> Int -> m a
tooFewArguments n = throwError . TooFewArguments n
