{-# LANGUAGE TemplateHaskell #-}

module XmonadConfig.Presets (preset) where

import Control.Monad ((>=>))
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import XMonad (WorkspaceId)

preset :: QuasiQuoter
preset = QuasiQuoter{quoteExp = parsePresets >=> codegenPresetRules}

data PresetsConfig = PresetsConfig
    { presets :: [Preset]
    , screenComparator :: Name
    }
    deriving (Show)
newtype Preset = Preset [WorkspaceId]
    deriving (Show)

parsePresets :: String -> Q PresetsConfig
parsePresets _ = do
    Just sc <- lookupValueName "screenComparator"
    return $ PresetsConfig [] sc

codegenPresetRules :: PresetsConfig -> ExpQ
codegenPresetRules config = do
    traceShowM config
    return $ AppE (VarE 'return) (TupE [])
