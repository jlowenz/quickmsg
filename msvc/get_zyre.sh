#!/usr/bin/env bash

ZYRE_VERSION=tags/v1.1.0
CZMQ_VERSION=tags/v3.0.2

if [[ ! -e libsodium ]]; then
    git clone --depth 1 -b stable https://github.com/jedisct1/libsodium.git
fi

if [[ ! -e libzmq ]]; then    
    git clone https://github.com/zeromq/libzmq.git
fi

if [[ ! -e czmq ]]; then
    git clone https://github.com/zeromq/czmq.git
fi

if [[ ! -e zyre ]]; then
    git clone https://github.com/zeromq/zyre.git
fi

pushd czmq
git checkout $CZMQ_VERSION
popd

pushd zyre
git checkout $ZYRE_VERSION
popd

