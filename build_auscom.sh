#!/bin/bash
# TODO: Create a makefile

BASEDIR=$(pwd)
cd submodels/oasis3/prism/compile
./comp_oasis325.RAIJIN
cd ${BASEDIR}

cd submodels/cice4.1/compile
./comp_auscom_cice.RAIJIN.nP 6
cd ${BASEDIR}

cd submodels/matm/compile
./comp_auscom_matm.VAYU
cd ${BASEDIR}

cd submodels/mom4p1/compile
./comp_auscom_mom4p1_cfc.VAYU
cd ${BASEDIR}
