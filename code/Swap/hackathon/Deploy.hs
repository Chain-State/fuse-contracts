{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

module Deploy where

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..))
import           Codec.Serialise (serialise)
import qualified Data.Aeson                          as DataAeson
import           Data.Aeson.Text       (encodeToLazyText)

import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Short               as SBS
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
-- import qualified Ledger
import           Cardano.Api.Shelley ( fromPlutusData )
import qualified Data.Text.Lazy.IO   as LT
-- import           Plutus.V1.Ledger.Value                             as ValueV1

import qualified SwapOnChain            as OnChain
import qualified FTokens                as FTokens

-- import qualified Ledger.Ada               as Ada

main :: IO()
main = do
    writeInitDatum
    writeContractDatum

    _ <- writeSwapValidatorScript
    _ <- writeTokensValidatorScript

    -- fileContents <- readJSON $ basePath++"borrow-request-redeemer.json"
    -- print fileContents
    -- print $ "USDK Currency Symbol -------- "++ show $ FTokens.signedCurrencySymbol fTokensParams
    -- putStrLn $ " maxAdaValue--------"++show maxAdaValue++"\n swapAmount--------"++show swapAmount++" Lovelace"++
    --         "\n loanAmount--------"++show $ loanAmount swapAmount++" USDH"++"\n interestAmount--------"++
    --         show $ interestAmount (loanAmount swapAmount) 5++" USDH"

    return ()

basePath :: FilePath
basePath = "./assets/" 
    
dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n)         = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> LedgerApiV2.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unValidatorScript

writeMintingValidator :: FilePath -> LedgerApiV2.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unMintingPolicyScript

writeInitDatum :: IO ()
writeInitDatum = writeJSON (basePath++"datum/unit.json") ()

readJSON :: FilePath -> IO LBS.ByteString
readJSON = LBS.readFile

writeContractDatum :: IO ()
writeContractDatum = 
    let contributor = swapDatum
        d = PlutusTx.toBuiltinData contributor
    in writeJSON (basePath++"datum/swap-datum.json") d

-- Virtual Fixed Ada price based on the 
-- Average high and low Ada USD price of all time.
-- TODO: Should be calculated daily
minAdaValue :: Double
minAdaValue = 0.487209213

currentAdaValue :: Double
currentAdaValue = 0.377838

swapAmount :: Integer -> Integer
swapAmount i = i

swapDatum :: Integer
swapDatum =  1_000

-- fTokensCs :: LedgerApiV2.CurrencySymbol
-- fTokensCs = FTokens.signedCurrencySymbol fTokensParams

-- fTokensParams :: FTokens.SignParam
-- fTokensParams = FTokens.SignParam {
--     FTokens.beneficiary = LedgerApiV2.PubKeyHash "6dde623cf9cccc589d33172139ba09fa8274c962ea3b6521d084cfc9"
-- }./

contractParams :: OnChain.ContractParam
contractParams =  OnChain.ContractParam {   
    OnChain.swaper = LedgerApiV2.PubKeyHash "329fa233c51fc91902e045ddab1c539aac278a078a6218991c8b11ca ",
    OnChain.tokenCs = LedgerApiV2.CurrencySymbol "",
    OnChain.tokenTn = LedgerApiV2.TokenName ""
}

writeSwapValidatorScript :: IO (Either (FileError ()) ())
writeSwapValidatorScript =  writeValidator (basePath++"plutus-scripts/Swap.plutus") $ OnChain.validator contractParams

writeTokensValidatorScript :: IO (Either (FileError ()) ())
writeTokensValidatorScript =  writeMintingValidator (basePath++"plutus-scripts/FTokens-Policy.plutus") $ FTokens.signedPolicy $ LedgerApiV2.PubKeyHash "6dde623cf9cccc589d33172139ba09fa8274c962ea3b6521d084cfc9"