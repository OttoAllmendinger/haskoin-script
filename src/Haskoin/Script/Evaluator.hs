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
isDisabled op = case runProgram [op] of
    Left (DisabledOp _) -> True
    _ -> False


isTrue :: ScriptOp -> Bool
isTrue OP_0 = False
isTrue _ = True


toNumber :: Int -> ScriptOp
toNumber x = undefined -- TODO

fromNumber :: ScriptOp -> Int
fromNumber op = undefined -- TODO

opSize :: ScriptOp -> Int
opSize op = undefined -- TODO



data EvalError =
    EvalError String
    | StackError ScriptOp
    | DisabledOp ScriptOp

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


-- Script Primitives

getOp :: ProgramTransition ScriptOp
getOp = get >>= \case
    ([], _, _) -> throwError $ EvalError "getOp: empty script"
    (i:_, _, _) -> return i

popOp :: ProgramTransition ScriptOp
popOp = get >>= \case
    ([], _, _) -> throwError $ EvalError "popOp: empty script"
    (i:is, s, a) -> put (is, s, a) >> return i

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

pickStack :: Bool -> Int -> ProgramTransition ()
pickStack remove n = do
    stack <- getStack

    when (n < 0) $
        throwError $ EvalError "pickStack: n < 0"
    when (n > (length stack)) $
        throwError $ EvalError "pickStack: n > size"

    let v = stack !! n
    when remove $ putStack $ (take (n-1) stack) ++ (drop n stack)
    pushStack v


-- transformStack :: (Stack -> Stack) -> ProgramTransition ()
-- transformStack f = (getStack >>= putStack . f)

stackError :: ProgramTransition ()
stackError = getOp >>= throwError . StackError

disabled :: ProgramTransition ()
disabled = getOp >>= throwError . DisabledOp

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
            op <- getOp
            unless (op == stop) $ do
                when evalOps $ (eval op)
                popOp
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

eval OP_DEPTH = getStack >>= pushStack . toNumber . length

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

eval OP_PICK = fromNumber <$> popStack >>= (pickStack False)

eval OP_ROLL = fromNumber <$> popStack >>= (pickStack True)

eval OP_ROT = getStack >>= \case
    (x1:x2:x3:xs) -> putStack (x3:x2:x1:xs)
    _ -> stackError

eval OP_SWAP = getStack >>= \case
    (x1:x2:xs) -> putStack (x2:x1:xs)
    _ -> stackError

eval OP_TUCK = getStack >>= \case
    (x1:x2:xs) -> putStack (x2:x1:x2:xs)
    _ -> stackError

eval OP_2DROP = void $ popStack >> popStack

eval OP_2DUP = getStack >>= \case
    (x1:x2:xs) -> putStack (x1:x2:x1:x2:xs)
    _ -> stackError

eval OP_3DUP = getStack >>= \case
    (x1:x2:x3:xs) -> putStack (x1:x2:x3:x1:x2:x3:xs)
    _ -> stackError

-- eval OP_3DUP = transStack3 $ \(a, b, c) -> [a,b,c,a,b,c]

eval OP_2OVER = getStack >>= \case
    (x1:x2:x3:x4:xs) -> putStack (x1:x2:x3:x4:x1:x2:xs)
    _ -> stackError

-- eval OP_2OVER = tStack4 $ \a b c d -> [a, b, c, d, a, b]

eval OP_2ROT = getStack >>= \case
    (x1:x2:x3:x4:x5:x6:xs) -> putStack (x3:x4:x5:x6:x1:x2:xs)
    _ -> stackError

{-
eval OP_2SWAP = getStack >>= \case
    (x1:x2:x3:x4:xs) -> putStack (x3:x4:x1:x2:xs)
    _ -> stackError
-}

eval OP_2SWAP = tStack4 $ \a b c d -> [c, d, a, b]


-- Splice

eval OP_CAT = disabled

eval OP_SUBSTR = disabled

eval OP_LEFT = disabled

eval OP_RIGHT = disabled

eval OP_SIZE = (opSize <$> popStack) >>= pushStack . toNumber

-- Bitwise Logic


eval OP_INVERT = disabled

eval OP_AND = disabled

eval OP_OR = disabled

eval OP_XOR = disabled

eval OP_EQUAL = getStack >>= \case
    (x1:x2:xs) -> if (x1 == x2) then pushStack OP_1 else pushStack OP_0
    _ -> stackError

eval OP_EQUALVERIFY = (eval OP_EQUAL) >> (eval OP_VERIFY)

-- Arithmetic

eval OP_1ADD = arithUnary (+1)

eval OP_1SUB = arithUnary (subtract 1)

eval OP_2MUL = disabled

eval OP_2DIV = disabled

eval OP_NEGATE = arithUnary negate

eval OP_ABS = arithUnary abs

eval OP_NOT = stackOpUnary $ \case
    OP_0 -> OP_1
    _ -> OP_0

eval OP_0NOTEQUAL = stackOpUnary $ \case
    OP_0 -> OP_0
    _ -> OP_1

eval OP_ADD = arithBinary (+)

eval OP_SUB = arithBinary (-)

eval OP_MUL = disabled
eval OP_DIV = disabled
eval OP_MOD = disabled
eval OP_LSHIFT = disabled
eval OP_RSHIFT = disabled




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
            popOp
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
