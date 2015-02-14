{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Rifactor.AWS2
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.AWS2 where

import           BasePrelude
import           Control.Lens
import           Data.Text (Text)
import qualified Network.AWS.EC2 as EC2
import           Rifactor.AWS (find1ByType)
import           Rifactor.Types.AWS hiding (AwsModel)
import           Rifactor.Types.AWS2
import           Rifactor.Types.Model2

default (Text)

totalFactor :: AwsModel -> Float
totalFactor = foldr go 0
  where go (Instance _ i) b =
          b +
          find1ByType (i ^. EC2.i1InstanceType) ^.
          insFactor
        go (Reserved _ _) b = b

allSizes :: AwsModel -> [EC2.InstanceType]
allSizes = foldr go []
  where go (Instance _ i) b =
          cons (i ^. EC2.i1InstanceType) b
        go (Reserved _ _) b = b

totalCount :: AwsModel -> Int
totalCount m =
  case foldr go (Just 0) m of
    Just n -> n
    Nothing -> 0
  where go (Reserved _ r) b =
          liftA2 (+) (r ^. EC2.ri1InstanceCount) b
        go _ _ = Just 0

capacityUsed :: AwsModel -> Int
capacityUsed m = undefined

capacityTotal :: AwsModel -> Int
capacityTotal m = undefined

capacityAvail :: AwsModel -> Int
capacityAvail m = undefined
