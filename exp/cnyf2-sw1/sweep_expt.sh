#!/bin/bash

# Path guessing
cwd=$(pwd)
cwd=${cwd%/}            # Strip trailing slash

expt_name=${cwd##*/}

tmp=${cwd%/*}
tmp=${tmp%/*}
model_name=${tmp##*/}

set -x
rm -rf /short/v45/${USER}/auscom/OUTPUT/${model_name}/${expt_name}
rm -rf ../../output/cnyf2-sw1
rm -f Running.dir
rm -f cnyf2-sw1.e*
rm -f cnyf2-sw1.o*
rm -f cnyf2-sw1.log
rm -f cnyf2-sw1.date
