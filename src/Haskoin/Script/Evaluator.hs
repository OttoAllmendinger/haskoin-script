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
    OP_15 -> True
    OP_16 -> True
    OP_1NEGATE -> True
    _ -> False


isDisabled :: ScriptOp -> Bool
isDisabled = return False -- TODO
-- isDisabled x = case x of


isTrue :: ScriptOp -> Bool
isTrue OP_0 = False
isTrue _ = True


toNumber :: Int -> ScriptOp
toNumber x = undefined -- TODO



data EvalError = EvalError String

instance Error EvalError where
    noMsg = EvalError "Evaluation Error"
    strMsg s = EvalError $ noMsg ++ " " ++ s

instance Show EvalError where
    show (EvalError m) = m

type Instructions = [ScriptOp]
type AltStack = [ScriptOp]
type Stack = [ScriptOp]

type Program = (Instructions, Stack, AltStack)

type ProgramState = ErrorT EvalError Identity

type ProgramTransition a = StateT Program ProgramState a


-- Program Primitives

-- getProgram :: ProgramTransition Program
-- getProgram = StateT $ \p -> return (p, p)

-- Script Primitives

popScript :: ProgramTransition ScriptOp
popScript = get >>= f
    where f (i:is, s, a) = put (is, s, a) >> return i
          f (_, [], _)   = throwError $ EvalError "popScript: empty stack"

-- Stack Primitives

getStack :: ProgramTransition Stack
getStack = get >>= \(_, s, _) -> return s

withStack :: ProgramTransition Stack
withStack = getStack >>= f
    where f [] = throwError $ EvalError "empty stack"
          f s  = return s

putStack :: Stack -> ProgramTransition ()
putStack stack = get >>= \(i, _, a) -> put (i, stack, a)

pushStack :: ScriptOp -> ProgramTransition ()
pushStack op = withStack >>= \s -> putStack (op:s)

popStack :: ProgramTransition ScriptOp
popStack = withStack >>= \(s:ss) -> putStack ss >> return s

peekStack :: ProgramTransition ScriptOp
peekStack = withStack >>= \(s:ss) -> return s


-- AltStack Primitives

pushAltStack :: ScriptOp -> ProgramTransition ()
pushAltStack op = get >>= \(i, s, a) -> put (i, s, op:a)

popAltStack :: ProgramTransition ScriptOp
popAltStack = get >>= f
    where   f (i, s, a:as) = put (i, s, as) >> return a
            f (_, _, []) = throwError $ EvalError "popAltStack: empty stack"



-- Instruction Evaluation

eval :: ScriptOp -> ProgramTransition ()

-- FlowControl


-- clumsy?

doUntil :: ScriptOp -> Bool -> ProgramTransition ()
doUntil stop evalOps = do
    op <- popScript
    when evalOps $ eval op
    unless (op == stop) $ skipUntil stop

skipUntil :: ScriptOp -> ProgramTransition ()
skipUntil stop = doUntil stop False

evalUntil :: ScriptOp -> ProgramTransition ()
evalUntil stop = doUntil stop True

evalThen :: ProgramTransition ()
evalThen = evalUntil OP_ELSE >> skipUntil OP_ENDIF

evalElse :: ProgramTransition ()
evalElse = skipUntil OP_ELSE >> evalUntil OP_ENDIF

eval OP_IF = do
    cond <- popStack
    if isTrue cond
        then evalThen
        else evalElse

eval OP_NOTIF = do
    cond <- popStack
    if isTrue cond
        then evalElse
        else evalThen

-- Stack

eval OP_TOALTSTACK = popStack >>= pushAltStack

eval OP_FROMALTSTACK = popAltStack >>= pushStack

eval OP_RETURN = throwError $ EvalError "explicit OP_RETURN"

eval OP_VERIFY = do
    mv <- popStack
    unless (isTrue mv) (throwError $ EvalError "OP_VERIFY failed")

eval OP_DEPTH = do
    (_, s, _) <- get
    pushStack $ toNumber (length s)

eval OP_DROP = void popScript

eval OP_DUP = do
    x <- popStack
    pushStack x
    pushStack x

eval OP_NIP = do
    x <- popStack
    popStack
    pushStack x

eval OP_OVER = getStack >>= f
    where f (x1:x2:xs) = putStack (x1:x2:x1:xs)
          f _ = throwError $ EvalError "OP_OVER: not enough stack items"

eval OP_3DUP = getStack >>= f
    where f (x1:x2:x3:xs) = putStack (x1:x2:x3:x1:x2:x3:xs)
          f _ = throwError $ EvalError "OP_3DUP: not enough stack items"

eval op | isConstant op = pushStack op
        | otherwise     = throwError $ EvalError $ "unknown op " ++ show op



--

evalAll :: ProgramTransition ()
evalAll = do
    (i, s, a) <- get
    case i of
        [] -> return ()
        (op:ops) -> do
            eval op
            put (ops, s, a)
            evalAll

-- exported functions


-- TOOD better sig?
runProgram :: [ScriptOp] -> Either EvalError ((), Program)
runProgram i = runIdentity . runErrorT . runStateT evalAll $ (i, [], [])

evalScript :: [ScriptOp] -> Bool
evalScript i = case runProgram i of
    Left _ -> False
    Right _ -> True -- TODO check top of stack?


evalScriptTest :: IO ()
evalScriptTest = do
    let initState = ([], [], [])
    let result = runIdentity . runErrorT . runStateT (eval OP_OVER) $ initState

    case result of
        Left e -> putStrLn $ "error: " ++ show e
        Right s -> putStrLn $ "success -- state: " ++ show s
