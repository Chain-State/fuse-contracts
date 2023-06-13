#!/usr/bin/env bash
set -e
set -o pipefail

if [ -z $1 ]; then
    echo "balance.sh:  Invalid script arguments. Please provide wallet name or [addr_]"
    exit 1
fi

. "$(dirname $0)"/env # soure env variables

if [[ $1 != \addr_* ]];
then 
    $CARDANO_CLI query utxo --address $(cat $BASE/.priv/wallets/$1/$1.payment.addr) $PREPROD_TESTNET
    # WALLET_ADDRESS=$(cat $BASE/.priv/wallets/$1/$1.payment.addr)
else
    $CARDANO_CLI query utxo --address $1 $PREPROD_TESTNET
    # WALLET_ADDRESS=$1
fi

