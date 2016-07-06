module Lib (deriveFromJSONLensLike) where

  import Language.Haskell.TH.Syntax

  import Data.Char

  import Data.Aeson.TH

  import Control.Lens

  deriveFromJSONLensLike :: Name -> Q [Dec]
  deriveFromJSONLensLike n = deriveFromJSON Options
    { fieldLabelModifier      = (& ix 0 %~ toLower) . drop ((+1) . length . nameBase $ n)
    , constructorTagModifier  = id
    , allNullaryToStringTag   = True
    , omitNothingFields       = False
    , sumEncoding             = defaultTaggedObject
    , unwrapUnaryRecords      = False
    } n
