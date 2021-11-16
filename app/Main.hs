{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}


module Main (
  main
) where


import PlutusTx.Prelude hiding ((<>))

import Codec.Serialise           (serialise)
import Ledger                    (Validator, mkValidatorScript)
import Ledger.Typed.Scripts      (DatumType, RedeemerType, TypedValidator, ValidatorTypes, mkTypedValidator, validatorScript, wrapValidator)
import Prelude                   (IO, putStrLn, show)
import Plutus.V1.Ledger.Contexts (ScriptContext(..))
import Plutus.V1.Ledger.Scripts  (unValidatorScript)
import PlutusTx                  (compile, makeIsDataIndexed, makeLift)

import qualified Data.ByteString.Short as SBS (ShortByteString, length, toShort)
import qualified Data.ByteString.Lazy  as LBS (toStrict)


--
-- This prints the following:
--   Lengths of validators (bytes):
--     Untyped 'always succeeds':                         14
--     Typed 'always succeeds' with ScriptContext:        2450
--     Typed 'always succeeds' with typed datum/redeemer: 2658
--
main :: IO ()
main =
  do
    putStrLn ""
    putStrLn "Lengths of validators (bytes):"
    putStrLn $ "  Untyped 'always succeeds':                         " ++ show (SBS.length $ serialiseValidator validatorUntyped)
    putStrLn $ "  Typed 'always succeeds' with ScriptContext:        " ++ show (SBS.length $ serialiseValidator validatorTyped  )
    putStrLn $ "  Typed 'always succeeds' with typed datum/redeemer: " ++ show (SBS.length $ serialiseValidator validatorTyped1 )


serialiseValidator :: Validator
                   -> SBS.ShortByteString
serialiseValidator = SBS.toShort . LBS.toStrict . serialise . unValidatorScript


-- Untyped validator that always succeeds.


{-# INLINABLE makeValidatorUntyped #-}

makeValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
makeValidatorUntyped _ _ _ = ()

validatorUntyped :: Validator
validatorUntyped = mkValidatorScript $$(compile [|| makeValidatorUntyped ||])


-- Typed validator that always succeeds.


{-# INLINABLE makeValidatorTyped #-}

makeValidatorTyped :: BuiltinData
                   -> BuiltinData
                   -> ScriptContext
                   -> Bool
makeValidatorTyped _ _ _ = True


data ExampleTyped

instance ValidatorTypes ExampleTyped  where
    type instance DatumType    ExampleTyped  = BuiltinData
    type instance RedeemerType ExampleTyped  = BuiltinData


instanceTyped :: TypedValidator ExampleTyped
instanceTyped =
  mkTypedValidator @ExampleTyped
    $$(compile [|| makeValidatorTyped ||])
      $$(compile [|| wrap ||])
    where
      wrap = wrapValidator @BuiltinData @BuiltinData


validatorTyped :: Validator
validatorTyped = validatorScript instanceTyped


-- Typed validator that always succeeds.


data DatumTyped =
    DTA Integer
  | DTB Integer


data RedeemerTyped =
    RTA Integer
  | RTB Integer


{-# INLINABLE makeValidatorTyped1 #-}

makeValidatorTyped1 :: DatumTyped
                    -> RedeemerTyped
                    -> ScriptContext
                    -> Bool
makeValidatorTyped1 _ _ _ = True


data ExampleTyped1

instance ValidatorTypes ExampleTyped1  where
    type instance DatumType    ExampleTyped1  = DatumTyped
    type instance RedeemerType ExampleTyped1  = RedeemerTyped


instanceTyped1 :: TypedValidator ExampleTyped1
instanceTyped1 =
  mkTypedValidator @ExampleTyped1
    $$(compile [|| makeValidatorTyped1 ||])
      $$(compile [|| wrap ||])
    where
      wrap = wrapValidator @DatumTyped @RedeemerTyped


validatorTyped1 :: Validator
validatorTyped1 = validatorScript instanceTyped1


makeLift ''DatumTyped
makeIsDataIndexed ''DatumTyped [('DTA, 0), ('DTB, 1)]


makeLift ''RedeemerTyped
makeIsDataIndexed ''RedeemerTyped [('RTA, 0), ('RTB, 1)]
