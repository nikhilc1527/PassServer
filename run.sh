#!/bin/sh

set -xe

cabal build
./pass_server
