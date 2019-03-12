module MonadTransformersStepByStep where


import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Maybe

import qualified Data.Map               as Map

-- | very good paper to gain intuition about Monad Transformers and usa cases
--
-- paper url https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf
--
-- Author: Martin Grabm¨uller
-- Title: Monad Transformers Step by Step
-- Date: Oct 16 2006 (Draft)*



------------------------------------------------------------
-- the example for using monad transformers will be an interpreter for the little language
------------------------------------------------------------

-- | variable names
type Name = String

-- | expressions
data Exp = Lit Integer
         -- ^ literal integers (constants)
         | Var Name
         -- ^ variables
         | Plus Exp Exp
         -- ^ addition
         | Abs Name Exp
         -- ^ λ expressions (abstractions)
         | App Exp Exp
         -- ^ function application
           deriving (Show)

-- | values
-- The Env component of a FunVal is the environment in which the corresponding
-- λ-abstraction was evaluated.
data Value = IntVal Integer
           -- ^ integers
           | FunVal Env Name Exp
           -- ^ functions (closures).
             deriving (Show)




-- | mapping from names to values
type Env = Map.Map Name Value


-- | interpreter function
--
-- Integer literals simply evaluate to themselves
-- variables evaluate to the values to which they are bound in the environment
-- Addition is implemented by simply evaluating both operands and returning their sum
--
eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2 ) = let IntVal i1 = eval0 env e1
                              IntVal i2 = eval0 env e2
                          in IntVal (i1 + i2 )
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2 ) = let val1 = eval0 env e1
                             val2 = eval0 env e2
                         in case val1 of
                              FunVal env' n body -> eval0 (Map.insert n val2 env') body


exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "y")) (Lit 4 `Plus` Lit 2))
run0 = eval0 Map.empty exampleExp


-----------------------------------------------------------------
-- Monad Transformers


-- | Converting to Monadic Style


type Eval1 α = Identity α
runEval1 :: Eval1 α -> α
runEval1 ev = runIdentity ev


-- eval1 :: Env -> Exp -> Eval1 Value
eval1 :: Monad m =>  Env -> Exp  -> m Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = case Map.lookup n env of
                      Just i  -> return i
                      Nothing -> error "unkown variable"
eval1 env (Plus e1 e2 ) = do IntVal i1 <- eval1 env e1
                             IntVal i2 <- eval1 env e2
                             return $ IntVal (i1 + i2 )
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2 ) =
    do val1 <- eval1 env e1
       val2 <- eval1 env e2
       case val1 of
         FunVal env' n body -> eval1 (Map.insert n val2 env') body


run1 = runEval1 (eval1 Map.empty exampleExp)


-- | Adding Error Handling

type Eval2 α = ExceptT String Identity α

runEval2 :: Eval2 α -> Either String α
runEval2 ev = runIdentity (runExceptT ev)

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i

eval2a env (Var n) = case Map.lookup n env of
                      Just i  -> return i
                      Nothing -> throwError "unkown variable"

eval2a env (Plus e1 e2 ) = do IntVal i1 <- eval2a env e1
                              IntVal i2 <- eval2a env e2
                              return $ IntVal (i1 + i2 )
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2 ) =
    do val1 <- eval2a env e1
       val2 <- eval2a env e2
       case val1 of
         FunVal env' n body -> eval2a (Map.insert n val2 env') body

run2a = runEval2 (eval2a Map.empty exampleExp)
run2aErr = runEval2 (eval2a Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))

-- | error messages:
eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) = case Map.lookup n env of
                      Just i  -> return i
                      Nothing -> throwError "Data.Map.lookup: Key not found"

eval2b env (Plus e1 e2 ) = do e1' <- eval2b env e1
                              e2' <- eval2b env e2
                              case (e1', e2') of
                                (IntVal i1, IntVal i2 ) -> return $ IntVal (i1 + i2 )
                                _ ->  throwError "type error"

eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2 ) =
    do val1 <- eval2b env e1
       val2 <- eval2b env e2
       case val1 of
         FunVal env0 n body -> eval2b (Map.insert n val2 env0 ) body
         _                  -> throwError "type error"


run2b = runEval2 (eval2b Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))
run2b_1 = runEval2 (eval2b Map.empty (Var "x"))




eval2c :: Env -> Exp -> Eval2 Value
eval2c env (Lit i) = return $ IntVal i
eval2c env (Var n) = case Map.lookup n env of
                      Just i  -> return i
                      Nothing -> throwError "Data.Map.lookup: Key not found"

eval2c env (Plus e1 e2 ) = do IntVal i1 <- eval2c env e1
                              IntVal i2 <- eval2c env e2
                              return $ IntVal (i1 + i2 )
eval2c env (Abs n e) = return $ FunVal env n e
eval2c env (App e1 e2 ) =
    do FunVal env' n body <- eval2c env e1
       val2 <- eval2c env e2
       eval2c (Map.insert n val2 env') body




-- | The Final Version

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
                      Nothing  -> throwError ("unbound variable: " ++ n)
                      Just val -> return val
eval2 env (Plus e1 e2 ) = do e1' <- eval2 env e1
                             e2' <- eval2 env e2
                             case (e1', e2') of
                               (IntVal i1 ,IntVal i2 ) -> return $ IntVal (i1 + i2 )
                               _ -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2 ) =
    do val1 <- eval2 env e1
       val2 <- eval2 env e2
       case val1 of
         FunVal env' n body -> eval2 (Map.insert n val2 env' ) body
         _                  ->  throwError "type error in application"


run2 = runEval2 (eval2 Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))
run2_1 = runEval2 (eval2 Map.empty (Var "x"))


-----------------------------------------------------------------
-- Hiding the Environment


type Eval3 α = ReaderT Env (ExceptT String Identity) α

runEval3 :: Env -> Eval3 α -> Either String α
runEval3 env ev = runIdentity (runExceptT (runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do env <- ask
                   case Map.lookup n env of
                     Nothing  -> throwError ("unbound variable: " ++ n)
                     Just val -> return val
eval3 (Plus e1 e2 ) = do e1' <- eval3 e1
                         e2' <- eval3 e2
                         case (e1', e2') of
                           (IntVal i1 ,IntVal i2 ) -> return $ IntVal (i1 + i2 )
                           _ ->  throwError "type error in addition"
eval3 (Abs n e) = do env <- ask
                     return $ FunVal env n e
eval3 (App e1 e2 ) =
    do val1 <- eval3 e1
       val2 <- eval3 e2
       case val1 of
         FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
         _ ->  throwError "type error in application"


run3 = runEval3 Map.empty (eval3 exampleExp)
