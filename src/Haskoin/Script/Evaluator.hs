{-# LANGUAGE LambdaCase #-}
module Haskoin.Script.Evaluator (evalScript, evalScriptTest) where

import Haskoin.Util
import Haskoin.Crypto
import Haskoin.Protocol
import Haskoin.Script.Parser

import Debug.Trace (trace)

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Control.Applicative ((<$>))


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
isDisabled op = case op of
    OP_CAT      -> True
    OP_SUBSTR   -> True
    OP_LEFT     -> True
    OP_RIGHT    -> True

    OP_INVERT   -> True
    OP_AND      -> True
    OP_OR       -> True
    OP_XOR      -> True

    OP_2MUL     -> True
    OP_2DIV     -> True

    OP_MUL      -> True
    OP_DIV      -> True
    OP_MOD      -> True
    OP_LSHIFT   -> True
    OP_RSHIFT   -> True

    _           -> False


isTrue :: ScriptOp -> Bool
isTrue OP_0 = False
isTrue _ = True


toNumber :: Int -> ScriptOp
toNumber x = undefined -- TODO



data EvalError = EvalError String | StackError ScriptOp

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


-- Stack Primitives

getStack :: ProgramTransition Stack
getStack = get >>= \(_, s, _) -> return s

withStack :: ProgramTransition Stack
withStack = getStack >>= \case
    [] -> throwError $ EvalError "empty stack"
    s  -> return s

putStack :: Stack -> ProgramTransition ()
putStack stack = get >>= \(i, _, a) -> put (i, stack, a)

pushStack :: ScriptOp -> ProgramTransition ()
pushStack op = withStack >>= \s -> putStack (op:s)

popStack :: ProgramTransition ScriptOp
popStack = withStack >>= \(s:ss) -> putStack ss >> return s

peekStack :: ProgramTransition ScriptOp
peekStack = withStack >>= \(s:ss) -> return s

-- transformStack :: (Stack -> Stack) -> ProgramTransition ()
-- transformStack f = (getStack >>= putStack . f)

stackError :: ProgramTransition ()
stackError = get >>= \(i:is, _, _) -> throwError $ StackError i

-- AltStack Primitives

pushAltStack :: ScriptOp -> ProgramTransition ()
pushAltStack op = get >>= \(i, s, a) -> put (i, s, op:a)

popAltStack :: ProgramTransition ScriptOp
popAltStack = get >>= \case
    (i, s, a:as) -> put (i, s, as) >> return a
    (_, _, [])   -> throwError $ EvalError "popAltStack: empty stack"



-- Instruction Evaluation

eval :: ScriptOp -> ProgramTransition ()

-- Flow Control

-- TODO check nested conditionals

evalIf :: Bool -> ProgramTransition ()
evalIf cond = case cond of
    True -> evalUntil OP_ELSE >> skipUntil OP_ENDIF
    False -> skipUntil OP_ELSE >> evalUntil OP_ENDIF
    where
        doUntil stop evalOps = do
            (i:is, s, a) <- get

            when (is == []) $ throwError $ EvalError "premature end"

            unless (i == stop) $ do
                when evalOps $ (eval i)
                put (is, s, a)
                doUntil stop evalOps

        skipUntil stop = doUntil stop False
        evalUntil stop = doUntil stop True



eval OP_NOP = return ()

eval OP_IF = popStack >>= evalIf . isTrue

eval OP_NOTIF = popStack >>= evalIf . not . isTrue

eval OP_ELSE = throwError $ EvalError "OP_ELSE outside OP_IF"

eval OP_ENDIF = throwError $ EvalError "OP_ENDIF outside OP_IF"

eval OP_VERIFY = isTrue <$> popStack >>= \case
    True -> throwError $ EvalError "OP_VERIFY failed"
    _    -> return ()

eval OP_RETURN = throwError $ EvalError "explicit OP_RETURN"



-- Stack


eval OP_TOALTSTACK = popStack >>= pushAltStack

eval OP_FROMALTSTACK = popAltStack >>= pushStack

eval OP_IFDUP = do
    v <- peekStack
    when (isTrue v) (pushStack v)

eval OP_DEPTH = do
    s <- getStack
    pushStack $ toNumber (length s)

eval OP_DROP = void popStack

eval OP_DUP = getStack >>= \case
    (x1:xs) -> putStack (x1:x1:xs)
    _ -> stackError

eval OP_NIP = getStack >>= \case
    (x1:x2:xs)  -> putStack (x1:xs)
    _ -> stackError

eval OP_OVER = getStack >>= \case
    (x1:x2:xs) -> putStack (x1:x2:x1:xs)
    _ -> stackError

eval OP_3DUP = getStack >>= \case
    (x1:x2:x3:xs) -> putStack (x1:x2:x3:x1:x2:x3:xs)
    _ -> stackError

eval op | isConstant op = pushStack op
        | isDisabled op = throwError $ EvalError $ "disabled op " ++ show op
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

runProgram :: [ScriptOp] -> Either EvalError ((), Program)
runProgram i = runIdentity . runErrorT . runStateT evalAll $ (i, [], [])

evalScript :: [ScriptOp] -> Bool
evalScript i = case runProgram i of
    Left _ -> False
    Right ((), (_, stack, _)) -> case stack of
        (x:xs)  -> isTrue x
        []      -> False


evalScriptTest :: IO ()
evalScriptTest = do
    let initState = ([], [], [])
    let result = runIdentity . runErrorT . runStateT (eval OP_OVER) $ initState

    case result of
        Left e -> putStrLn $ "error: " ++ show e
        Right s -> putStrLn $ "success -- state: " ++ show s
