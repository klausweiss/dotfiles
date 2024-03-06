{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XmonadConfig.Presets (preset) where

import Control.Monad (when, (>=>))
import Data.Foldable (maximumBy)
import Data.Functor ((<&>))
import Data.List (group, sort)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text hiding (any, filter, group, length, maximum)
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import XMonad (WorkspaceId)
import XMonad.Actions.PhysicalScreens (PhysicalScreen (P), getScreen)
import Prelude hiding (lines, null)

preset :: QuasiQuoter
preset = QuasiQuoter{quoteExp = parsePresets . pack >=> codegenPresetRules}

data PresetsConfig = PresetsConfig
    { presets :: [Preset]
    , screenComparator :: Name
    }
    deriving (Show)
type Preset = [WorkspaceId]

parsePresets :: Text -> Q PresetsConfig
parsePresets presetText = do
    sc <- lookupValueName "screenComparator" <&> fromMaybe (error "Please define `screenComparator` to use the `preset` QuasiQuoter")
    let presets =
            fmap (fmap (unpack . strip) . splitOn "|")
                . filter (not . null)
                . fmap strip
                . lines
                $ presetText
    when ((any ((> 1) . length) . group . sort) $ length <$> presets) (error "Preset contains multiple rules with the same number of screens, making it impossible to distinguish them.")
    return $ PresetsConfig presets sc

codegenPresetRules :: PresetsConfig -> ExpQ
codegenPresetRules config = do
    let maxMonitors = maximum $ length <$> presets config
    screenVarNames <- mapM (\i -> newName $ "screen" <> show i) [0 .. maxMonitors]
    let mkGetScreenCall i = AppE (AppE (VarE 'getScreen) (VarE $ screenComparator config)) (AppE (ConE 'P) (LitE $ IntegerL $ fromIntegral i))
        screenGetters = [0 .. maxMonitors] <&> (\i -> BindS (VarP (screenVarNames !! i)) (mkGetScreenCall i))
        emptyReturn = AppE (VarE 'return) (TupE [])
    return $
        DoE
            Nothing
            ( screenGetters
                <> [ NoBindS emptyReturn
                   ]
            )
