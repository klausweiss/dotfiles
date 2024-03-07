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
import XMonad (WorkspaceId, def)
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
    sc <- lookupValueName "screenComparator" <&> fromMaybe 'def
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
    screenVarNames <- mapM (\i -> newName $ "screen" <> show i) [0 .. maxMonitors - 1]
    let mkGetScreenCall i = AppE (AppE (VarE 'getScreen) (VarE $ screenComparator config)) (AppE (ConE 'P) (LitE $ IntegerL $ fromIntegral i))
        -- screenGetters is a list of assignments like these - as many as the maximum number of monitors across the groups:
        --   screen0 <- getScreen screenComparator 0
        --   screen1 <- getScreen screenComparator 1
        --   ...
        screenGetters = [0 .. maxMonitors - 1] <&> (\i -> BindS (VarP (screenVarNames !! i)) (mkGetScreenCall i))
        -- for the following input
        --   n = 3
        --   workspaces = ["w1", "w2"]
        -- the following will generate a match case like
        --   (Just just_w1, Just just_w2, Nothing) -> return [(just_w1, w1), (just_w2, w2)]
        makeMatch n workspaces = Match pattern body []
          where
            pattern = TupP ((just <$> workspaces) <> (nothing <$ [length workspaces .. n - 1]))
            just ws = ConP 'Just [] [VarP $ justName ws]
            nothing = ConP 'Nothing [] []
            justName ws = mkName $ "just_" <> ws -- TODO: does not support raw strings as workspace ids, only variables
            body =
                NormalB $
                    AppE
                        (VarE 'return)
                        ( ListE $
                            ( \w ->
                                TupE
                                    [ Just . VarE $ justName w
                                    , Just . VarE $ mkName w
                                    ]
                            )
                                <$> workspaces
                        )
        --  _ -> return []
        makeCatchAll = Match (VarP $ mkName "_") (NormalB $ AppE (VarE 'return) (ListE [])) []
        -- case (screen1, screen2, screen3) of ...
        caseOf = CaseE (TupE $ Just . VarE <$> screenVarNames)
        cases = (makeMatch maxMonitors <$> presets config) <> [makeCatchAll]
    return $ DoE Nothing (screenGetters <> [NoBindS $ caseOf cases])
