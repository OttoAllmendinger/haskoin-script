{-# LANGUAGE LambdaCase #-}
module Network.Haskoin.Script.Evaluator (evalScript, evalScriptTest) where

import Debug.Trace (trace)

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Control.Applicative ((<$>), (<*>))

import qualified Data.ByteString as BS 



import Network.Haskoin.Crypto
import Network.Haskoin.Protocol


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


rejectSignature :: SigCheck
rejectSignature _ _ = False

isDisabled :: ScriptOp -> Bool
isDisabled op = case runProgram [op] rejectSignature of
    Left (DisabledOp _) -> True
    _ -> False


opToBool :: ScriptOp -> Bool
opToBool OP_0 = False
opToBool _ = True

boolToOp :: Bool -> ScriptOp
boolToOp False = OP_0
boolToOp True = OP_1


opToInt :: ScriptOp -> Int
opToInt op = undefined -- TODO

intToOp :: Int -> ScriptOp
intToOp x = undefined -- TODO



opToBs :: ScriptOp -> BS.ByteString
opToBs op = undefined

bsToOp :: BS.ByteString -> ScriptOp
bsToOp op = undefined



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
    show (StackError op) = (show op) ++ ": Stack Error"
    show (DisabledOp op) = (show op) ++ ": disabled"

type Instructions = [ScriptOp]
type AltStack = [ScriptOp]
type Stack = [ScriptOp]
type HashOps = [ScriptOp] -- the code that is verified by OP_CHECKSIG

-- type Program = (Instructions, Stack, AltStack, HashCode)

data Program = Program {
    instructions :: Instructions,
    stack        :: Stack,
    altStack     :: AltStack,
    hashOps      :: HashOps,
    sigCheck     :: SigCheck
}

instance Show Program where
    show p = show $ instructions p

type ProgramState = ErrorT EvalError Identity

type ProgramTransition a = StateT Program ProgramState a

type SigCheck = PubKey -> [ScriptOp] -> Bool


-- Script Primitives

getOp :: ProgramTransition ScriptOp
getOp = instructions <$> get >>= \case
    [] -> throwError $ EvalError "getOp: empty script"
    (i:_) -> return i

popOp :: ProgramTransition ScriptOp
popOp = get >>= \prog -> case instructions prog of
    [] -> throwError $ EvalError "popOp: empty script"
    -- (i:is) -> put (is, s, a, h) >> return i
    (i:is) -> put prog { instructions = is } >> return i

-- Stack Primitives

getStack :: ProgramTransition Stack
getStack = stack <$> get

withStack :: ProgramTransition Stack
withStack = getStack >>= \case
    [] -> stackError
    s  -> return s

putStack :: Stack -> ProgramTransition ()
putStack stack = get >>= \p -> put p { stack = stack }

prependStack :: Stack -> ProgramTransition ()
prependStack s = getStack >>= \s' -> putStack $ s ++ s'

pushStack :: ScriptOp -> ProgramTransition ()
pushStack op = getStack >>= \s -> putStack (op:s)

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


pushHashOp :: ScriptOp -> ProgramTransition ()
pushHashOp op = get >>= \p -> put p { hashOps = op:(hashOps p) }

getHashOps :: ProgramTransition HashOps
getHashOps = hashOps <$> get

clearHashOps :: ProgramTransition ()
clearHashOps = get >>= \p -> put p { hashOps = [] }

-- transformStack :: (Stack -> Stack) -> ProgramTransition ()
-- transformStack f = (getStack >>= putStack . f)


tStack1 :: (ScriptOp -> Stack) -> ProgramTransition ()
tStack1 f = f <$> popStack >>= prependStack

tStack2 :: (ScriptOp -> ScriptOp -> Stack) -> ProgramTransition ()
tStack2 f = f <$> popStack <*> popStack >>= prependStack

tStack3 :: (ScriptOp -> ScriptOp -> ScriptOp -> Stack) -> ProgramTransition ()
tStack3 f = f <$> popStack <*> popStack <*> popStack >>= prependStack

tStack4 :: (ScriptOp -> ScriptOp -> ScriptOp -> ScriptOp -> Stack)
            -> ProgramTransition ()
tStack4 f = f <$> popStack <*> popStack <*> popStack <*> popStack
            >>= prependStack

tStack6 :: (ScriptOp -> ScriptOp -> ScriptOp ->
            ScriptOp -> ScriptOp -> ScriptOp -> Stack) -> ProgramTransition ()
tStack6 f = f <$> popStack <*> popStack <*> popStack
              <*> popStack <*> popStack <*> popStack >>= prependStack

tStack1L :: (ScriptOp -> a) -> (b -> ScriptOp) ->
            (a -> b) -> ProgramTransition ()
tStack1L p q f = tStack1 $ return . q . f . p


tStack2L :: (ScriptOp -> a) -> (b -> ScriptOp) ->
            (a -> a -> b) -> ProgramTransition ()
tStack2L p q f = tStack2 $ \a b -> return $ q $ f (p a) (p b)


tStack3L :: (ScriptOp -> a) -> (b -> ScriptOp) ->
            (a -> a -> a -> b) -> ProgramTransition ()
tStack3L p q f = tStack3 $ \a b c -> return $ q $ f (p a) (p b) (p c)



arith1 :: (Int -> Int) -> ProgramTransition ()
arith1 = tStack1L opToInt intToOp

arith2 :: (Int -> Int -> Int) -> ProgramTransition ()
arith2 = tStack2L opToInt intToOp

bool2 :: (Bool -> Bool -> Bool) -> ProgramTransition ()
bool2 = tStack2L opToBool boolToOp


stackError :: ProgramTransition a
stackError = getOp >>= throwError . StackError

disabled :: ProgramTransition ()
disabled = getOp >>= throwError . DisabledOp

-- AltStack Primitives

pushAltStack :: ScriptOp -> ProgramTransition ()
pushAltStack op = get >>= \p -> put p { altStack = op:(altStack p) }

popAltStack :: ProgramTransition ScriptOp
popAltStack = get >>= \p -> case altStack p of
    a:as -> put p { altStack = as } >> return a
    []   -> throwError $ EvalError "popAltStack: empty stack"



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
                void popOp
                doUntil stop evalOps

        skipUntil stop = doUntil stop False
        evalUntil stop = doUntil stop True


eval OP_NOP     = return ()
eval OP_IF      = popStack >>= evalIf . opToBool
eval OP_NOTIF   = popStack >>= evalIf . not . opToBool
eval OP_ELSE    = throwError $ EvalError "OP_ELSE outside OP_IF"
eval OP_ENDIF   = throwError $ EvalError "OP_ENDIF outside OP_IF"

eval OP_VERIFY = opToBool <$> popStack >>= \case
    False -> throwError $ EvalError "OP_VERIFY failed"
    True  -> return ()

eval OP_RETURN = throwError $ EvalError "explicit OP_RETURN"



-- Stack

eval OP_TOALTSTACK = popStack >>= pushAltStack
eval OP_FROMALTSTACK = popAltStack >>= pushStack
eval OP_IFDUP   = tStack1 $ \case OP_0 -> [] ; a -> [a, a]
eval OP_DEPTH   = getStack >>= pushStack . intToOp . length
eval OP_DROP    = void popStack
eval OP_DUP     = tStack1 $ \a -> [a, a]
eval OP_NIP     = tStack2 $ \a b -> [a]
eval OP_OVER    = tStack2 $ \a b -> [a, b, a]
eval OP_PICK    = opToInt <$> popStack >>= (pickStack False)
eval OP_ROLL    = opToInt <$> popStack >>= (pickStack True)
eval OP_ROT     = tStack3 $ \a b c -> [c, b, a]
eval OP_SWAP    = tStack2 $ \a b -> [b, a]
eval OP_TUCK    = tStack2 $ \a b -> [b, a, b]
eval OP_2DROP   = tStack2 $ \a b -> []
eval OP_2DUP    = tStack2 $ \a b -> [a, b, a, b]
eval OP_3DUP    = tStack3 $ \a b c -> [a, b, c, a, b, c]
eval OP_2OVER   = tStack4 $ \a b c d -> [a, b, c, d, a, b]
eval OP_2ROT    = tStack6 $ \a b c d e f -> [c, d, e, f, a, b]
eval OP_2SWAP   = tStack4 $ \a b c d -> [c, d, a, b]

-- Splice

eval OP_CAT     = disabled
eval OP_SUBSTR  = disabled
eval OP_LEFT    = disabled
eval OP_RIGHT   = disabled
eval OP_SIZE    = (opSize <$> popStack) >>= pushStack . intToOp

-- Bitwise Logic

eval OP_INVERT  = disabled
eval OP_AND     = disabled
eval OP_OR      = disabled
eval OP_XOR     = disabled
eval OP_EQUAL   = tStack2 $ \a b -> if a == b then [OP_1] else [OP_0]
eval OP_EQUALVERIFY = (eval OP_EQUAL) >> (eval OP_VERIFY)

-- Arithmetic

eval OP_1ADD    = arith1 (+1)
eval OP_1SUB    = arith1 (subtract 1)
eval OP_2MUL    = disabled
eval OP_2DIV    = disabled
eval OP_NEGATE  = arith1 negate
eval OP_ABS     = arith1 abs
eval OP_NOT         = arith1 $ \case 0 -> 1; _ -> 0
eval OP_0NOTEQUAL   = arith1 $ \case 0 -> 0; _ -> 1
eval OP_ADD     = arith2 (+)
eval OP_SUB     = arith2 (-)
eval OP_MUL     = disabled
eval OP_DIV     = disabled
eval OP_MOD     = disabled
eval OP_LSHIFT  = disabled
eval OP_RSHIFT  = disabled
eval OP_BOOLAND     = bool2 (&&)
eval OP_BOOLOR      = bool2 (||)
eval OP_NUMEQUAL    = bool2 (==)
eval OP_NUMEQUALVERIFY = eval OP_NUMEQUAL >> eval OP_VERIFY
eval OP_NUMNOTEQUAL         = tStack2L opToInt boolToOp (/=)
eval OP_LESSTHAN            = tStack2L opToInt boolToOp (<)
eval OP_GREATERTHAN         = tStack2L opToInt boolToOp (>)
eval OP_LESSTHANOREQUAL     = tStack2L opToInt boolToOp (<=)
eval OP_GREATERTHANOREQUAL  = tStack2L opToInt boolToOp (>=)
eval OP_MIN     = tStack2L opToInt intToOp min
eval OP_MAX     = tStack2L opToInt intToOp max
eval OP_WITHIN  = tStack3L opToInt boolToOp $ \a x y -> (x <= a) && (a < y)

eval OP_RIPEMD160 = tStack1 $ return . bsToOp . hash160BS . opToBs
eval OP_SHA1 = undefined  -- TODO: add sha160 to Network.Haskoin.Crypto.Hash
-- eval OP_SHA1 = tStack1 $ return . bsToOp . hashSha160BS . opToBs

eval OP_SHA256 = tStack1 $ return . bsToOp . hash256BS . opToBs
eval OP_HASH160 = tStack1 $ return . bsToOp . hash160BS . hash256BS . opToBs
eval OP_HASH256 = tStack1 $ return . bsToOp . doubleHash256BS  . opToBs
eval OP_CODESEPARATOR = clearHashOps
eval OP_CHECKSIG = undefined
eval OP_CHECKMULTISIG = popStack >>= checkMultiSig . opToInt
    where  checkMultiSig 0 = return ()
           checkMultiSig x = eval OP_CHECKSIG >> checkMultiSig (x - 1)

eval OP_CHECKSIGVERIFY      = eval OP_CHECKSIG      >> eval OP_VERIFY
eval OP_CHECKMULTISIGVERIFY = eval OP_CHECKMULTISIG >> eval OP_VERIFY

eval op | isConstant op = pushStack op
        | otherwise     = throwError $ EvalError $ "unknown op " ++ show op

--

evalAll :: ProgramTransition ()
evalAll = do
    instructions <$> get >>= \case
        [] -> return ()
        (op:ops) -> do
            eval op
            popOp >>= pushHashOp
            evalAll

-- exported functions

runProgram :: [ScriptOp] -> SigCheck -> Either EvalError ((), Program)
runProgram i sigCheck =
    runIdentity . runErrorT . runStateT evalAll $ Program {
        instructions = i,
        stack = [],
        altStack = [],
        hashOps = [],
        sigCheck = sigCheck
    }

evalScript :: Script -> SigCheck -> Bool
evalScript script sigCheck = case runProgram (scriptOps script) sigCheck of
    Left _ -> False
    Right ((), prog) -> case stack prog of
        (x:_)  -> opToBool x
        []     -> False


evalScriptTest :: IO ()
evalScriptTest = do
    let initState = Program {
        instructions = [OP_1, OP_0, OP_BOOLAND],
        stack = [],
        altStack = [],
        hashOps = [],
        sigCheck = rejectSignature
    }

    let result = runIdentity . runErrorT . runStateT evalAll $ initState

    case result of
        Left e -> putStrLn $ "error: " ++ show e
        Right s -> putStrLn $ "success -- state: " ++ show s
