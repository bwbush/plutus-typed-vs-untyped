{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}


module Main (
  main
) where


import PlutusTx.Prelude hiding ((<>))

import Cardano.Api               (AddressAny, NetworkId, PaymentCredential(..), StakeAddressReference(..), makeShelleyAddress, toAddressAny)
import Cardano.Api.Shelley       (PlutusScript(..), PlutusScriptVersion(..), PlutusScriptV1, Script(..), hashScript, writeFileTextEnvelope)
import Codec.Serialise           (serialise)
import Control.Monad             (void)
import Control.Monad.Extra       (whenJust)
import Ledger                    (Validator, mkValidatorScript, scriptAddress)
import Ledger.Typed.Scripts      (DatumType, RedeemerType, TypedValidator, ValidatorTypes, mkTypedValidator, validatorScript, wrapValidator)
import Ledger.Value              (assetClassValueOf)
import Prelude                   (FilePath, IO, (<>), print, putStrLn, show, writeFile)
import Plutus.V1.Ledger.Address  (Address, )
import Plutus.V1.Ledger.Contexts (ScriptContext(..), TxInInfo(..), TxOut(..), findOwnInput, getContinuingOutputs, valueSpent)
import Plutus.V1.Ledger.Scripts  (Datum(..), DatumHash, unValidatorScript)
import PlutusPrelude             (pretty)
import PlutusTx                  (FromData(..), applyCode, compile, liftCode, makeIsDataIndexed, makeLift, unstableMakeIsData)
import PlutusTx.Code             (CompiledCodeIn(DeserializedCode))

import qualified Data.ByteString.Short as SBS (ShortByteString, length, toShort)
import qualified Data.ByteString.Lazy  as LBS (toStrict)


data DatumTyped = DatumTyped Integer

unstableMakeIsData ''DatumTyped


data RedeemerTyped = RedeemerTyped Integer

unstableMakeIsData ''RedeemerTyped



main :: IO ()
main =
  do
    print $ SBS.length serialiseUntyped
    print $ SBS.length serialiseTyped


{-# INLINABLE makeValidatorUntyped #-}
makeValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
makeValidatorUntyped _ _ _ = ()

validatorUntyped :: Validator
validatorUntyped = mkValidatorScript $$(compile [|| makeValidatorUntyped ||])

serialiseUntyped :: SBS.ShortByteString
serialiseUntyped = SBS.toShort . LBS.toStrict . serialise $ unValidatorScript validatorUntyped


{-# INLINABLE makeValidatorTyped #-}

makeValidatorTyped :: DatumTyped
                   -> RedeemerTyped
                   -> ScriptContext
                   -> Bool
makeValidatorTyped _ _ ScriptContext{..} = True


data ExampleTyped

instance ValidatorTypes ExampleTyped  where
    type instance DatumType    ExampleTyped  = DatumTyped
    type instance RedeemerType ExampleTyped  = RedeemerTyped


instanceTyped :: TypedValidator ExampleTyped
instanceTyped =
  mkTypedValidator @ExampleTyped
    $$(compile [|| makeValidatorTyped ||])
      $$(compile [|| wrap ||])
    where
      wrap = wrapValidator @DatumTyped @RedeemerTyped


validatorTyped :: Validator
validatorTyped = validatorScript instanceTyped


serialiseTyped :: SBS.ShortByteString
serialiseTyped = SBS.toShort . LBS.toStrict . serialise $ unValidatorScript validatorTyped
