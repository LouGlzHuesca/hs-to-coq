{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module HsToCoq.ConvertHaskell.Declarations.Value (convertValDecls) where

import Data.Bitraversable
import Data.Foldable
import Data.Maybe
import Data.Either

import Control.Monad.IO.Class

import GHC hiding (Name)
import Panic

import HsToCoq.Coq.Gallina as Coq

import HsToCoq.ConvertHaskell.Monad
import HsToCoq.ConvertHaskell.Variables
import HsToCoq.ConvertHaskell.Definitions
import HsToCoq.ConvertHaskell.Expr
import HsToCoq.ConvertHaskell.Sigs
import HsToCoq.ConvertHaskell.Declarations.Notations
import HsToCoq.ConvertHaskell.Axiomatize

--------------------------------------------------------------------------------

convertValDecls :: ConversionMonad m => [HsDecl RdrName] -> m [Sentence]
convertValDecls args = do
  (defns, sigs) <- bitraverse pure convertSigs . partitionEithers . flip mapMaybe args $ \case
                     ValD def -> Just $ Left def
                     SigD sig -> Just $ Right sig
                     _        -> Nothing
  
  fold <$> convertTypedBindings defns sigs
             (withConvertedBinding
               (pure . withConvertedDefinition (DefinitionDef Global)     (pure . DefinitionSentence)
                                               (buildInfixNotations sigs) (map    NotationSentence))
               (\_ _ -> convUnsupported "top-level pattern bindings"))
             (Just axiomatizeBinding)
  
  where axiomatizeBinding :: GhcMonad m => HsBind RdrName -> GhcException -> m [Sentence]
        axiomatizeBinding FunBind{..} exn = do
          name <- freeVar $ unLoc fun_id
          pure [translationFailedComment name exn, axiom name]
        axiomatizeBinding _ exn =
          liftIO $ throwGhcExceptionIO exn
