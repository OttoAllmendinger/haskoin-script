module Haskoin.Script.Evaluator (evalScript, evalScriptTest) where

import Haskoin.Util
import Haskoin.Crypto
import Haskoin.Protocol
import Haskoin.Script.Parser


import Debug.Trace (trace)

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity


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



eval2 :: ScriptOp -> StateT Program Maybe Bool
eval2 OP_RETURN = StateT $ \p -> Just (False, p)

popScript2 :: StateT Program Maybe ScriptOp
popScript2 = StateT f
    where f (i:is, s, a) = Just (i, (is, s, a))
          f ([], _, _) = Nothing

pushStack2 :: ScriptOp -> StateT Program Maybe ()
pushStack2 op = state $ \(i,s,a) -> ((), (i, op:s, a))

popStack2 :: StateT Program Maybe ScriptOp
popStack2 = state $ \(i,s:ss,a) -> (s, (i, ss, a))



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



-- using StateT and (Either String)


popStack3 :: StateT Program (Either String) ScriptOp
popStack3 = StateT f
    where f (i, s:ss, a) = Right (s, (i, ss, a))
          f (_, [], _) = Left "popStack failed"

pushStack3 :: ScriptOp -> StateT Program (Either String) ()
pushStack3 op = state $ \(i, s, a) -> ((), (i, op:s, a))


eval3 :: ScriptOp -> StateT Program (Either String) ()
eval3 OP_DROP = void popStack3


run3 :: IO ()
run3 = do
        let initState = ([], [OP_1], [])
            p1 = do
                x <- popStack3
                popStack3
                pushStack3 x
            p2 = eval3 OP_DROP
            p3 = do
                eval3 OP_DROP
                eval3 OP_DROP

        let r1 = runStateT p1 initState
        let r2 = runStateT p2 initState
        let r3 = runStateT p3 initState

        printError r1
        printError r2
        printError r3

    where   printError :: Either String ((), Program) -> IO ()
            printError (Left e) = print $ "run3 error: " ++ e
            printError (Right ((), p)) = print $ "success: " ++ show p


--------------------------------------------------------------------------------
-- using StateT inside Error Monad
--
-- nearly optimal, possible without "return" in state?

data EvalError = EvalError String

instance Error EvalError where
    noMsg = EvalError "Evaluation Error"
    strMsg s = EvalError $ noMsg ++ " " ++ s

instance Show EvalError where
    show (EvalError m) = m

type ProgMonad4 a = StateT Program (ErrorT EvalError Identity) a

popStack4 :: ProgMonad4 ScriptOp
popStack4 = StateT f
    where   f (i, s:ss, a) = return (s, (i, ss, a))
            f (_, [], _) = throwError $ EvalError "popStack4 failed"

pushStack4 :: ScriptOp -> ProgMonad4 ()
pushStack4 op = StateT $ \(i, s, a) -> return ((), (i, op:s, a))

eval4 :: ScriptOp -> ProgMonad4 ()
eval4 OP_DROP = void popStack4


run4 :: IO ()
run4 = do
        let initState = ([], [OP_1], [])
            p1 = do
                x <- popStack4
                popStack4
                pushStack4 x
            p2 = eval4 OP_DROP
            p3 = do
                eval4 OP_DROP
                eval4 OP_DROP

        let r1 = runStateT p1 initState
        let r2 = runStateT p2 initState
        let r3 = runStateT p3 initState

        printError r1
        printError r2
        printError r3

    where   printError :: ErrorT EvalError Identity ((), Program) -> IO ()
            printError e = do
                let r = runIdentity . runErrorT $ e
                case r of
                    (Right p) -> print $ "run4 success: " ++ show p
                    (Left m) -> print $ "run4 error: " ++ show m





--------------------------------------------------------------------------------
-- using StateT inside ErrorT Monad ???
--

{-
type ProgMonad5 a = ErrorT EvalError (State Program) a

popStack5 :: ProgMonad5 ScriptOp
popStack5 = StateT f
    where   f (i, s:ss, a) = return (s, (i, ss, a))
            f (_, [], _) = throwError $ EvalError "popStack4 failed"

pushStack5 :: ScriptOp -> ProgMonad5 ()
pushStack5 op = StateT $ \(i, s, a) -> return ((), (i, op:s, a))

eval5 :: ScriptOp -> ProgMonad5 ()
eval5 OP_DROP = void popStack4

run5 :: IO ()
run5 = do
        let initState = ([], [OP_1], [])
            p1 = do
                x <- popStack4
                popStack4
                pushStack4 x
            p2 = eval4 OP_DROP
            p3 = do
                eval4 OP_DROP
                eval4 OP_DROP

        let r1 = runStateT p1 initState
        let r2 = runStateT p2 initState
        let r3 = runStateT p3 initState

        printError r1
        printError r2
        printError r3

    where   printError :: ErrorT EvalError Identity ((), Program) -> IO ()
            printError e = do
                let r = runIdentity . runErrorT $ e
                case r of
                    (Right p) -> print $ "run4 success: " ++ show p
                    (Left m) -> print $ "run4 error: " ++ show m



evalScriptTest :: IO ()
evalScriptTest = run4
-}
