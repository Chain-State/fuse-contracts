{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module SwapOnChain 
  ( SwapDatum (..)
  , ContractParam (..)
  , validator
  ) where

-- import           Data.Aeson           (ToJSON, FromJSON)
import GHC.Generics (Generic)

import           Plutus.V2.Ledger.Api      (POSIXTime, PubKeyHash, Address (..), TxInfo (txInfoInputs), TxInInfo(txInInfoResolved),
                                            ScriptContext (scriptContextTxInfo), TxOut(txOutValue, txOutAddress), 
                                            Validator, from, mkValidatorScript, txInfoSignatories, 
                                            UnsafeFromData (unsafeFromBuiltinData), 
                                            Credential (ScriptCredential))
import           Plutus.V2.Ledger.Contexts (txSignedBy, valuePaidTo, findOwnInput, getContinuingOutputs, ownHash)
import           Plutus.V1.Ledger.Value                             as ValueV1
import qualified Prelude                   as Haskell
-- import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (&&), Eq, Integer)

import           PlutusTx                  (compile, applyCode, liftCode, unstableMakeIsData, makeLift, toBuiltinData)
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Utilities                 (Network, posixTimeFromIso8601,
                                            printDataToJSON,
                                            validatorAddressBech32, wrap,
                                            writeValidatorToFile)

data SwapDatum = SwapDatum {
    swapAmnt  :: Integer -- Amount they are swapping
  } deriving (Haskell.Show, Generic)

unstableMakeIsData ''SwapDatum 

data ContractParam = ContractParam {
    swaper    :: PubKeyHash, -- Who is swapping
    tokenCs   :: CurrencySymbol, 
    tokenTn   :: TokenName 
 } deriving (Haskell.Show)

unstableMakeIsData ''ContractParam -- This is to instantiate the IsData class
makeLift ''ContractParam
    
{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: ContractParam -> SwapDatum -> () -> ScriptContext -> Bool
mkSwapValidator cp dat () ctx =  traceIfFalse "signedBySwaper: Not signed by Swaper" signedBySwaper  &&
  traceIfFalse "outputToSwaper: You have to pay the owner!" outputToSwaper &&
  traceIfFalse "consumeOnlyOneOutput: You can only consume one script UTxO per Tx!" consumeOnlyOneOutput
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedBySwaper :: Bool
    signedBySwaper = txSignedBy info $ swaper cp

    outputToSwaper :: Bool
    outputToSwaper = 
          valuePaidTo info (swaper cp) == singleton (tokenCs cp) (tokenTn cp) (swapAmnt dat)

    allInputs :: [TxInInfo]
    allInputs = txInfoInputs info 

    isScriptInput :: TxInInfo -> Bool
    isScriptInput i = case addressCredential . txOutAddress . txInInfoResolved $ i of
      ScriptCredential vh -> vh == ownHash ctx -- Check that the validator required to spend this output is the current one
      _                   -> False

    -- Helper function to get all inputs that are locked by this script
    getScriptInputs :: [TxInInfo]
    getScriptInputs = filter isScriptInput allInputs
      
    -- Check that we're consuming exactly one script output
    consumeOnlyOneOutput :: Bool
    consumeOnlyOneOutput = case getScriptInputs of
      [_] -> True
      _   -> False


{-# INLINABLE  mkWrappedRequestValidator #-}
mkWrappedRequestValidator :: ContractParam -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedRequestValidator = wrap . mkSwapValidator -- (unsafeFromBuiltinData pkh) -- (unsafeFromBuiltinData red)

validator :: ContractParam -> Validator
validator cp = mkValidatorScript ($$(compile [|| mkWrappedRequestValidator ||]) `applyCode` liftCode cp)
    
-- validatorHash :: ContractParam -> ValidatorHash
-- validatorHash = validatorHash . validator

-- address :: ContractParam -> Address
-- address = V1Address.scriptHashAddress . validatorHash
-- {-# INLINABLE valuePaid #-}

-- valuePaid :: ContractParam -> TxInfo -> Integer
-- valuePaid pkh info = assetClassValueOf (valuePaidTo info pkh)

saveVal :: ContractParam -> Haskell.IO ()
saveVal cp = writeValidatorToFile "./assets/swap.plutus" $ validator cp

vestingAddressBech32 :: ContractParam -> Network -> Haskell.String
vestingAddressBech32 pkh network = validatorAddressBech32 network $ validator pkh

-- printSwapDatumJSON :: ContractParam -> String -> IO ()
-- printSwapDatumJSON pkh time = printDataToJSON $ SwapDatum
--     { beneficiary = pkh
--     , deadline    = fromJust $ posixTimeFromIso8601 time
--     }