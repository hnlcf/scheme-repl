{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal () where

import Control.Monad.Except
import Control.Monad.Reader
  ( MonadReader,
    ReaderT (ReaderT),
  )
import Data.Map as Map
import Data.Text as T

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader EnvCtx,
      MonadIO
    )

newtype IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

instance Eq IFunc where
  (==) _ _ = False

data LispVal
  = Nil
  | Atom T.Text
  | String T.Text
  | Number Int
  | Bool Bool
  | List [LispVal]
  | Func IFunc
  | Lambda IFunc EnvCtx
  deriving (Eq)

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal Nil = "nil"
showVal (Atom a) = T.unpack a
showVal (String s) = T.unpack s
showVal (Number n) = show n
showVal (List l) = Prelude.concat ["(", Prelude.unwords $ showVal <$> l, ")"]
showVal (Func _) = "internal function"
showVal (Lambda _ _) = "lambda function"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
