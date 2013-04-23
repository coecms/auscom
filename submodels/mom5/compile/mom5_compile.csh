#!/bin/csh -f
#PBS -q express
#PBS -l ncpus=8
#PBS -l vmem=8GB
#PBS -l walltime=10:00
#PBS -N build_mom5
#PBS -joe
#PBS -wd

# Minimal compile script for fully coupled model CM2M 
set echo
set platform = vayu     # A unique identifier for your platfo
                        # This corresponds to the mkmf templates in $root/bin dir.
set type = MOM_solo     # Type of the experiment

set help = 0

# AusCOM flags
set MPI = 1
set datestamp = 0
set static = 0

# AusCOM variables
setenv CHAN MPI1
echo "CHAN: ${CHAN}"

setenv AusCOMHOME $cwd:h:h:h
echo "AusCOMHOME: ${AusCOMHOME}"

set bindir = "${AusCOMHOME}/bin"
if ( $datestamp ) then
    set datestr = `date +%Y%m%d`
    set exe = "mom5_${CHAN}.${datestr}.${platform}"
else
    set exe = "mom5_${CHAN}.${platform}"
endif

# MOM5 flag support (does this even work)
set argv = (`getopt -u -o h -l type: -l platform:  -l help  --  $*`)
while ("$argv[1]" != "--")
    switch ($argv[1])
        case --type:
                set type = $argv[2]; shift argv; breaksw
        case --platform:
                set platform = $argv[2]; shift argv; breaksw
        case --help:
                set help = 1;  breaksw
        case -h:
                set help = 1;  breaksw
    endsw
    shift argv
end
shift argv
if ( $help ) then
    echo "The optional arguments are:"
    echo "--type       followed by the type of the experiment, currently one of the following:"
    echo "             MOM_solo : solo ocean model"
    echo "             MOM_SIS  : ocean-seaice model"
    echo "             CM2M     : ocean-seaice-land-atmosphere coupled climate model"
    echo "             ESM2M    : ocean-seaice-land-atmosphere coupled climate model with biogeochemistry, EarthSystemModel"
    echo "             ICCM     : ocean-seaice-land-atmosphere coupled model"
    echo "             EBM      : ocean-seaice-land-atmosphere coupled model with energy balance atmosphere"
    echo
    echo "--platform   followed by the platform name that has a corresponfing environ file in the ../bin dir, default is ncrc.intel"
    echo
    echo
    exit 0
endif

#
# User does not need to change anything below!
#
set root          = "$cwd:h"                            # The directory you created when you checkout
set code_dir      = "$root/src"                         # source code directory
set build         = "$cwd/build_$CHAN"                  # build/library path
set executable    = "$bindir/$exe"                      # executable created after compilation
set mppnccombine  = "$bindir/mppnccombine.$platform"    # path to executable mppnccombine
set mkmfTemplate  = "$root/bin/mkmf.template.$platform" # path to template for your platform
set mkmf          = "$root/bin/mkmf"                    # path to executable mkmf

# Base netCDF
set cppDefs = ( "-Duse_netCDF -Duse_netCDF3" )

if (${MPI}) then
    set cppDefs = ( $cppDefs:q "-Duse_libMPI" )
endif

# AusCOM preprocessors
set cppDefs = ( $cppDefs:q "-DAusCOM -DOASIS3" )

#On Altrix systems you may include "-Duse_shared_pointers -Duse_SGI_GSM" in cppDefs for perfomance.
#These are included in the GFDL configuration of the model.
#set cppDefs  = ( "-Duse_netCDF -Duse_netCDF3 -Duse_libMPI -DUSE_OCEAN_BGC -DENABLE_ODA -DSPMD -DLAND_BND_TRACERS" )

if($static) then
  set build = "$cwd/build_${CHAN}_static"
  set cppDefs = ( $cppDefs:q "-DMOM_STATIC_ARRAYS -DNI_=360 -DNJ_=200 -DNK_=50 -DNI_LOCAL_=60 -DNJ_LOCAL_=50" )
endif

# Users must ensure the correct environment file exists for their platform.
source "${root}/bin/environs.${platform}"  # environment variables and loadable modules

if ( ! -d $build ) mkdir -p $build

echo "code_dir: ${code_dir}"
echo "mppnccombine: ${mppnccombine}"

# compile mppnccombine.c, needed only if $npes > 1
if ( $MPI && ! -f $mppnccombine ) then
    $CC -O -o $mppnccombine $code_dir/postprocessing/mppnccombine/mppnccombine.c -lnetcdf
endif

set mkmf_lib = "$mkmf -f -m Makefile -a $code_dir -t $mkmfTemplate"
set lib_include_dirs = "$root/include $code_dir/shared/include $code_dir/shared/mpp/include"

source ./FMS_compile.csh

source ./ocean_compile.csh
if ( $status ) exit $status

# None of these are currently supported (or part of AusCOM)
if( $type != MOM_solo) then
    cd $root/exp
    source ./ice_compile.csh
    if ( $status ) exit $status
endif
if( $type == MOM_SIS) then
    cd $root/exp
    source ./land_null_compile.csh
    if ( $status ) exit $status

    cd $root/exp
    source ./atmos_null_compile.csh
    if ( $status ) exit $status
endif
if( $type == EBM) then
    cd $root/exp
    source ./atmos_ebm_compile.csh
    if ( $status ) exit $status
endif
if( $type == CM2M | $type == ESM2M | $type == ICCM ) then
    cd $root/exp
    source ./atmos_phys_compile.csh
    if ( $status ) exit $status
endif
if( $type == CM2M | $type == ESM2M ) then
    cd $root/exp
    source ./atmos_fv_compile.csh
    if ( $status ) exit $status
endif
if( $type == CM2M | $type == ICCM | $type == EBM ) then
    cd $root/exp
    source ./land_lad_compile.csh
    if ( $status ) exit $status
endif
if( $type == ESM2M ) then
    cd $root/exp
    source ./land_lad2_compile.csh
    if ( $status ) exit $status
endif
if( $type == ICCM ) then
    cd $root/exp
    source ./atmos_bg_compile.csh
    if ( $status ) exit $status
endif

# Build the executable
set mkmf_exec = "$mkmf -f -m Makefile -a $code_dir -t $mkmfTemplate -p $exe"
cd $build
if( $type == MOM_solo ) then
    set srcList = ( mom5/drivers )
    set includes = "-I$build/lib_FMS -I$build/lib_ocean"
    set libs = "$build/lib_ocean/lib_ocean.a $build/lib_FMS/lib_FMS.a"
else if( $type == MOM_SIS ) then
    set srcList = ( coupler )
    set includes = "-I$build/lib_FMS -I$build/lib_ocean -I$build/lib_ice -I$build/lib_atmos_null -I$build/lib_land_null"
    set libs = "$build/lib_ocean/lib_ocean.a $build/lib_ice/lib_ice.a $build/lib_atmos_null/lib_atmos_null.a $build/lib_land_null/lib_land_null.a $build/lib_FMS/lib_FMS.a"
else if( $type == EBM ) then
    set srcList = ( coupler )
    set includes = "-I$build/lib_FMS -I$build/lib_ocean -I$build/lib_ice -I$build/lib_atmos_ebm  -I$build/lib_land_lad"
    set libs = "$build/lib_ocean/lib_ocean.a $build/lib_ice/lib_ice.a $build/lib_atmos_ebm/lib_atmos_ebm.a $build/lib_land_lad/lib_land_lad.a $build/lib_FMS/lib_FMS.a"
else if( $type == CM2M ) then
    set srcList = ( coupler )
    set includes = "-I$build/lib_FMS -I$build/lib_ocean -I$build/lib_ice -I$build/lib_atmos_fv -I$build/lib_atmos_phys -I$build/lib_land_lad"
    set libs = "$build/lib_ocean/lib_ocean.a $build/lib_ice/lib_ice.a $build/lib_atmos_fv/lib_atmos_fv.a $build/lib_atmos_phys/lib_atmos_phys.a $build/lib_land_lad/lib_land_lad.a $build/lib_FMS/lib_FMS.a"
else if( $type == ESM2M ) then
    set srcList = ( coupler )
    set includes = "-I$build/lib_FMS -I$build/lib_ocean -I$build/lib_ice -I$build/lib_atmos_fv -I$build/lib_atmos_phys -I$build/lib_land_lad2"
    set libs = "$build/lib_ocean/lib_ocean.a $build/lib_ice/lib_ice.a $build/lib_atmos_fv/lib_atmos_fv.a $build/lib_atmos_phys/lib_atmos_phys.a $build/lib_land_lad2/lib_land_lad2.a $build/lib_FMS/lib_FMS.a"
else if( $type == ICCM ) then
    set srcList = ( coupler )
    set includes = "-I$build/lib_FMS -I$build/lib_ocean -I$build/lib_ice -I$build/lib_atmos_bg -I$build/lib_atmos_phys -I$build/lib_land_lad" 
    set libs = "$build/lib_ocean/lib_ocean.a $build/lib_ice/lib_ice.a $build/lib_atmos_bg/lib_atmos_bg.a $build/lib_atmos_phys/lib_atmos_phys.a $build/lib_land_lad/lib_land_lad.a $build/lib_FMS/lib_FMS.a"
endif
$mkmf_exec -o "$includes" -l "$libs"  $srcList
make
if( $status ) then
    echo "Make failed to create the $type executable"
    exit 1
endif    

# Move to AusCOM bin
cp $exe $executable

exit
