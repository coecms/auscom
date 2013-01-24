#!/bin/bash
# TODO: Create a makefile

BASEDIR=$(pwd)
cd submodels/oasis3/prism/compile
./comp_oasis325.VAYU
cd ${BASEDIR}

cd submodels/cice4.1/compile
./comp_access_cice.VAYU.nP 6
cd ${BASEDIR}

cd submodels/matm/compile
./comp_auscom_matm.VAYU
cd ${BASEDIR}

cd submodels/mom4p1/compile
./comp_auscom_mom4p1_cfc.VAYU
cd ${BASEDIR}
