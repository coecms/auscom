#!/bin/bash
# TODO: Create a makefile

BASEDIR=$(pwd)
PLATFORM=RAIJIN
cd submodels/oasis3/prism/compile
./comp_oasis325.${PLATFORM}
cd ${BASEDIR}

cd submodels/cice4.1/compile
./comp_auscom_cice.${PLATFORM}.nP 6
cd ${BASEDIR}

cd submodels/matm/compile
./comp_auscom_matm.${PLATFORM}
cd ${BASEDIR}

cd submodels/mom4p1/compile
./comp_auscom_mom4p1_cfc.${PLATFORM}
cd ${BASEDIR}
