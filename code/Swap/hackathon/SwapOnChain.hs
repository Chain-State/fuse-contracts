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
  , validatorCode
  ) where

-- import           Data.Aeson           (ToJSON, FromJSON)
import GHC.Generics (Generic)

import           Plutus.V2.Ledger.Api      (POSIXTime, PubKeyHash, Address (..), TxInfo (txInfoInputs), TxInInfo(txInInfoResolved),
                                            ScriptContext (scriptContextTxInfo), TxOut(txOutValue, txOutAddress), singleton,
                                            Validator, ValidatorHash, from, mkValidatorScript, txInfoSignatories, Value,  
                                            UnsafeFromData (unsafeFromBuiltinData), adaSymbol, adaToken, 
                                            Credential (ScriptCredential))
import           Plutus.V2.Ledger.Contexts (txSignedBy, valuePaidTo, findOwnInput, getContinuingOutputs, ownHash)
import           Plutus.V1.Ledger.Value    (CurrencySymbol, TokenName, assetClassValue, assetClass)
import qualified Plutus.V1.Ledger.Value as V1 
import Plutus.V1.Ledger.Address (scriptHashAddress)

import qualified Prelude                   as Haskell
import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (&&), Eq, Integer, (==))

import           PlutusTx                  (CompiledCode, compile, applyCode, liftCode, unstableMakeIsData, makeLift, toBuiltinData)
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Utilities                 (Network, posixTimeFromIso8601,
                                            printDataToJSON,
                                            validatorAddressBech32, wrap,
                                            writeValidatorToFile)
import qualified Data.String               as S

data SwapDatum = SwapDatum {
    lockDat  :: Integer -- Datum to Lock and Unlock tokens
  } deriving (Haskell.Show, Generic)

unstableMakeIsData ''SwapDatum 

data ContractParam = ContractParam {
    swaper    :: PubKeyHash, -- Who is swapping
    tokenCs   :: CurrencySymbol, 
    tokenTn   :: TokenName,
    swapAmnt  :: Integer -- Amount they are swapping
 } deriving (Haskell.Show)

unstableMakeIsData ''ContractParam -- This is to instantiate the IsData class
makeLift ''ContractParam
    
{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: ContractParam -> SwapDatum -> () -> ScriptContext -> Bool
mkSwapValidator cp dat () ctx =  traceIfFalse "signedBySwaper: Not signed by Swaper" signedBySwaper &&
  traceIfFalse "outputToSwaper: You have to pay the swapper!" outputToSwaper &&
  traceIfFalse "consumeOnlyOneOutput: You can only consume one script UTxO per Tx!" consumeOnlyOneOutput
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedBySwaper :: Bool
    signedBySwaper = txSignedBy info $ swaper cp

    val :: Value
    val = valuePaidTo info (swaper cp)

    num :: Integer
    num = 15000000

    asset :: V1.AssetClass
    asset = assetClass adaSymbol adaToken

    val2 :: Value
    val2 = assetClassValue asset num

    outputToSwaper :: Bool
    outputToSwaper = val == val2
          -- valuePaidTo info (swaper cp) == singleton (tokenCs cp) (tokenTn cp) (swapAmnt cp)

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

----------------------------------------------------------------------------------------------------------------
---------------------------------------------------LUCID CODE---------------------------------------------------

{-# INLINABLE  mkWrappedValidatorLucid #-}
--                            swapper          CS           TN           Amount          dat           redeemer         context
mkWrappedValidatorLucid :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidatorLucid pkh cs tn amnt = wrap $ mkSwapValidator cp
    where
        cp = ContractParam
            { swaper = unsafeFromBuiltinData pkh
            , tokenCs = unsafeFromBuiltinData cs
            , tokenTn = unsafeFromBuiltinData tn 
            , swapAmnt = unsafeFromBuiltinData amnt 
            }

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$( compile [|| mkWrappedValidatorLucid ||])

---------------------------------------------------------------------------------------------------
------------------------------------- SAVE VALIDATOR -------------------------------------------

-- saveOracleCode :: IO ()
-- saveOracleCode = writeCodeToFile "assets/oracle.plutus" validatorCode

-- saveOracleScript :: String -> PubKeyHash -> IO ()
-- saveOracleScript symbol pkh = do
--     let
--     writeValidatorToFile fp $ validator op
--     where
--         op = OracleParams
--             { oNFT= parseToken symbol
--             , oOperator   = pkh
--             }
--         fp = printf "assets/oracle-%s-%s.plutus" (take 3 (show pkh)) $ take 3 (show pkh)

-- {-# INLINABLE valuePaid #-}

-- valuePaid :: ContractParam -> TxInfo -> Integer
-- valuePaid pkh info = assetClassValueOf (valuePaidTo info pkh)

-- saveVal :: ContractParam -> Haskell.IO ()
-- saveVal cp = writeValidatorToFile "./assets/swap.plutus" $ validator cp

-- swapAddressBech32 :: ContractParam -> Network -> Haskell.String
-- swapAddressBech32 cp network = validatorAddressBech32 network $ validator cp

-- printSwapDatumJSON :: ContractParam -> String -> IO ()
-- printSwapDatumJSON pkh time = printDataToJSON $ SwapDatum
--     { beneficiary = pkh
--     , deadline    = fromJust $ posixTimeFromIso8601 time
--     }
