module Rifactor.Types where

type AwsCfg = { accounts :: [AwsAccountCfg] }

type AwsAccountCfg = { name :: String
                     , access_key_id :: String
                     , secret_access_key :: String }
