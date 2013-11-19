module Haskoin.Script.Evaluator (evalScript, evalScriptTest) where

import Haskoin.Util
import Haskoin.Crypto
import Haskoin.Protocol
import Haskoin.Script.Parser


import Debug.Trace (trace)

import Control.Monad.State
import Control.Monad.Error


-- see https://github.com/bitcoin/bitcoin/blob/master/src/script.cpp EvalScript
-- see https://en.bitcoin.it/wiki/Script


isConstant :: ScriptOp -> Bool
isConstant op = case op of
    (OP_PUSHDATA _) -> True
    OP_0  -> True
    OP_1  -> True
    OP_2  -> True
    OP_3  -> True
    OP_4  -> True
    OP_5  -> True
    OP_7  -> True
    OP_8  -> True
    OP_9  -> True
    OP_10 -> True
    OP_11 -> True
    OP_12 -> True
    OP_13 -> True
    OP_14 -> True
    OP_1NEGATE -> True
    _ -> False


isDisabled :: ScriptOp -> Bool
isDisabled = return False -- TODO
-- isDisabled x = case x of


isTrue :: ScriptOp -> Bool
isTrue OP_0 = False
isTrue _ = True

type Instructions = [ScriptOp]
type AltStack = [ScriptOp]
type Stack = [ScriptOp]

type Program = (Instructions, Stack, AltStack)


popScript :: State Program (Maybe ScriptOp)
popScript = state $ \(i:is, s, a) -> (Just i, (is, s, a))

pushStack :: ScriptOp -> State Program Bool
pushStack op = state $ \(i, s, a) -> (True, (i, op:s, a))

popStack :: State Program (Maybe ScriptOp)
popStack = state p
    where   p (i, op:s, a) = (Just op, (i, s, a))
            p (i, [], a) = (Nothing, (i, [], a))


pushAltStack :: ScriptOp -> State Program Bool
pushAltStack op = state $ \(i, s, a) -> (True, (i, op:s, a))

popAltStack :: ScriptOp -> State Program Bool
popAltStack op = state $ \(i, s, a) -> (True, (i, op:s, a))


scriptFail :: State Program Bool
scriptFail = state $ \p -> (False, p)


eval :: ScriptOp -> State Program Bool

eval OP_RETURN = scriptFail

eval OP_VERIFY = do
    mv <- popStack
    case mv of
        Nothing -> return False
        Just v -> return $ isTrue v


eval OP_TOALTSTACK = do
    mv <- popScript
    case mv of
        Nothing -> return False
        Just v -> pushAltStack v

eval op | isConstant op = pushStack op
        | otherwise     = error $ "unknown op " ++ show op



evalT :: ScriptOp -> StateT Program Maybe Bool
evalT OP_RETURN = StateT $ \p -> Just (False, p)

popScriptT :: StateT Program Maybe ScriptOp
popScriptT = StateT f
    where f (i:is, s, a) = Just (i, (is, s, a))
          f ([], _, _) = Nothing

pushStackT :: ScriptOp -> StateT Program Maybe ()
pushStackT op = state $ \(i,s,a) -> ((), (i, op:s, a))

popStackT :: StateT Program Maybe ScriptOp
popStackT = state $ \(i,s:ss,a) -> (s, (i, ss, a))



evalTE :: ScriptOp -> StateT Program (ErrorT e m)


runProgram :: (Bool, Program) -> Bool
runProgram (result, ([], _, _)) = result
runProgram (_, (op:ops, stack, altstack)) =
    let (result', program') = runState (eval op) (ops, stack, altstack)
    in runProgram (result', program')

evalScript :: Script -> Bool
evalScript (Script ops) =
    {-
    let (result, st') = runState runProgram (ops, [], [])
        runProgram :: State Program Bool
        runProgram = state $ \(i, s, a) -> (False, ([], [], []))
    in result
    -}

    runProgram (True, (ops, [], []))

evalScriptTest :: Maybe (ScriptOp, Program)
evalScriptTest = runStateT test init
    where init = ([OP_1, OP_2], [], [])
          test = do
            x <- popScriptT
            y <- popScriptT
            pushStackT x
            popStackT
