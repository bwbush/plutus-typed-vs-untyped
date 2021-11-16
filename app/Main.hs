-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- Copyright   :  (c) 2021 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <code@functionally.io>
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Oracle for general data.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Mantra.Oracle (
-- * Oracle
  OracleScript
, oracleInstance
, oracleValidator
, oracleAddress
, oracleAddressAny
, plutusOracle
, exportOracle
-- * Access
, fetchDatum
) where


import PlutusTx.Prelude hiding ((<>))

import Cardano.Api               (AddressAny, NetworkId, PaymentCredential(..), StakeAddressReference(..), makeShelleyAddress, toAddressAny)
import Cardano.Api.Shelley       (PlutusScript(..), PlutusScriptVersion(..), PlutusScriptV1, Script(..), hashScript, writeFileTextEnvelope)
import Codec.Serialise           (serialise)
import Control.Monad             (void)
import Control.Monad.Extra       (whenJust)
import Ledger                    (Validator, scriptAddress)
import Ledger.Typed.Scripts      (DatumType, RedeemerType, TypedValidator, ValidatorTypes, mkTypedValidator, validatorScript, wrapValidator)
import Ledger.Value              (assetClassValueOf)
import Mantra.Oracle.Types       (Action(..), Oracle(..))
import Prelude                   (FilePath, IO, (<>), show, writeFile)
import Plutus.V1.Ledger.Address  (Address, )
import Plutus.V1.Ledger.Contexts (ScriptContext(..), TxInInfo(..), TxOut(..), findOwnInput, getContinuingOutputs, valueSpent)
import Plutus.V1.Ledger.Scripts  (Datum(..), DatumHash, unValidatorScript)
import PlutusPrelude             (pretty)
import PlutusTx                  (FromData(..), applyCode, compile, liftCode, makeIsDataIndexed, makeLift)
import PlutusTx.Code             (CompiledCodeIn(DeserializedCode))

import qualified Data.ByteString.Short as SBS (ShortByteString, length, toShort)
import qualified Data.ByteString.Lazy  as LBS (toStrict)


main :: IO ()
main =
  do
    print $ SBS.length serialiseTyped


makeValidatorTyped :: BuiltinData
                   -> BuiltinData
                   -> ScriptContext
                   -> Bool
makeValidator Oracle{..} _ redeemer ScriptContext{..} =
  True


data ExampleTyped

instance ValidatorTypes OracleScript  where
    type instance DatumType    OracleScript  = BuiltinData
    type instance RedeemerType OracleScript  = BuiltinData


instanceTyped :: TypedValidator ExampleTyped
instanceTyped oracle =
  mkTypedValidator @ExampleTyped
    $$(compile [|| makeValidatorType ||])


validatorTyped :: Validator
validatorTyped = validatorScript instanceTyped


serialiseTyped :: SBS.ShortByteString
serialiseTyped = SBS.toShort . LBS.toStrict . serialise $ unValidatorScript validatorTyped
