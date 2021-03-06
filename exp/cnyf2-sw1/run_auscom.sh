#!/bin/ksh
#############################################################################
#                            run_auscom.RAIJIN                              #
#############################################################################
# *** Set up the running environment and run the auscom coupled model  ***  #
#
#   'AusCOM' is a coupled ocean and sea ice model consisting of 3 components
#      1. matm    (a data atmospheric model, providing atmospheric forcing)
#      2. cice4.1 (LANL sea ice model) 
#      3. mom4p1  (GFDL ocean model)
#      built under the OASIS3 PRISM_2-5 framework
#
#   This sample run is set up on the NCI RAIJIN platform for 128 processes: 
#   1 for cpl, 1 for matm, 6 for cice, and 120 for mom4 (mono-cpu cpling) 
#   if DEBUG=yes:
#     *This debug run is set up on the NCI RAIJIN platform for 4 processes: 
#      1 for cpl, 1 for matm, 1 for cice, and 1 for mom4 (mono-cpu cpling) 
#     *Change ncpus=4
#     *Do qsub -I -q normal -v DISPLAY -p 1023 -l ncpus=4 -l walltime=100:00 \
#      -lvmem=24gb -l software=totalview/10
#      and run this script
#############################################################################
#
# New setup
# Each component now runs in its own work directory
# This avoids namespace pollution and renders many of the AusCOM changes to the
# submodels obsolete.
#
# Russ Fiedler
#############################################################################
#
# 0. Prologue
#
#############################################################################
#PBS -P v45
#PBS -q normal
#PBS -l walltime=4:00:00
#PBS -l mem=64GB
#PBS -l ncpus=128
#PBS -l wd
#PBS -N cnyf2-sw1

date
set -e
set -xv
ulimit -s unlimited
ulimit -a

#
#-- Export System depending variables
#
export MPIPROGINF=DETAIL;
export F_PROGINF=detail;
export F_FTRACE=YES;
export MPLARGS=" " ;
export F_SETBUF06=50000
export F_SETBUF07=50000
export F_SETBUF08=50000
export ATM_COMPARAL=1
export F_SYSLEN=300
export F_SETBUF00=50000
MPIEXPORT="F_PROGINF F_SYSLEN"
MPIEXPORT="${MPIEXPORT} MPIPROGINF F_FTRACE MPISUSPEND"
MPIEXPORT="${MPIEXPORT} F_SETBUF00 F_SETBUF06 F_SETBUF07 F_SETBUF08"
export MPI_MULTITASKMIX="ON"
export MPIEXPORT="${MPIEXPORT} MPI_MULTITASKMIX"
export MPI_BUFFER_MAX=5000000

#############################################################################
#
# 1. Primary Setups
#
#############################################################################
#
## 1.1 Define experiment ID etc.
#
project=v45	        # /short disk 'owned' by project (e.g., p66)
jobid=$PBS_JOBID    # job-id assigned by PBS (the queue sys)
job=$PBS_JOBNAME	# name of this script
chan=MPI1		# Message Passage (MPI1/MPI2)

expid=cnyf2-sw1		# change expid for each new experiment
atmdata=core2		# choose the atmospheric forcing dataset
atm_forcing="'${atmdata}'"   # (ncep2, era40, core2, or um96 etc.)
datatype=NY		# NY/IA: Normal Year/Interannual Annual
year_data_end=2007	# data NOT available after this year

# NOTE: Doesn't work on nodes!
#platform=`hostname | sed "s/[0-9]\+$//" | tr "a-z" "A-Z"`
platform=RAIJIN

#
## 1.2 Define all associated paths
#

# Location where jobs are submitted (and this script is located):
cd `pwd`/../..
AusCOMHOME=`pwd`
model=${AusCOMHOME##*/}		#the model name, i.e., AusCOM1.0
jobdir=$AusCOMHOME/exp/$expid

# Location of preprocessed input files for the coupled model:
inputdir=$AusCOMHOME/input

# Location where the model exectuables are stored:
bindir=$AusCOMHOME/bin

# Location where outputs are to be stored:
datahome=/short/$project/$USER/auscom/OUTPUT
outputdir=$datahome/$model/$expid
restdir=${outputdir}/restart
histdir=${outputdir}/history

# Location where the sub-models and coupler are actually runing:
workhome=/short/$project/$USER/auscom
rundir=$workhome/RUNNING/$model/$expid         

ocnrundir=$rundir/OCN_RUNDIR
atmrundir=$rundir/ATM_RUNDIR
icerundir=$rundir/ICE_RUNDIR
cplrundir=$rundir/CPL_RUNDIR

#############################################################################
#
# 2. Exp/Run Time Control etc.
#
#############################################################################
# 
## 2.1 Runtime control for the whole exp and this segment run 
#
# if DEBUG=yes run with totalview on a interactive qsub job
DEBUG=no
#DEBUG=yes

# Initial and final date of the experiment
if [[ $DEBUG = "yes" ]]; then
    iniyear=1;	finalyear=1;		typeset -Z4 iniyear  finalyear
    inimonth=1;	finalmonth=1;		typeset -Z2 inimonth finalmonth
    iniday=1;	finalday=4;		typeset -Z2 iniday   finalday
else
    iniyear=1;	finalyear=1;		typeset -Z4 iniyear  finalyear
    inimonth=1;	finalmonth=12;		typeset -Z2 inimonth finalmonth
    iniday=1;	finalday=31;		typeset -Z2 iniday   finalday
fi

# Duration of this run (maybe the most often visited place for test/short runs):
if [[ $DEBUG = "yes" ]]; then
    nyear=0  		# number of years (ALWAYS 0 ! change nmonth etc...)
    nmonth=0		# number of months
    nday=1			# number of days
else
    nyear=0  		# number of years (ALWAYS 0 ! change nmonth etc...)
    nmonth=12		# number of months
    nday=0			# number of days
fi

# Time steps
dt_cpl_ai=21600		#air-ice coupling interval in seconds
cd $jobdir
if [ ! -f ${expid}.date ]; then  #jobnum=1
    dt_cpl_io=1800        #ice-ocn coupling interval in seconds
    dt_oce=1800           #oce model timestep
    dt_atm=1800           #atm model timestep
    dt_ice=1800           #ice model timestep
else
    dt_cpl_io=3600	#ice-ocn coupling interval in seconds
    dt_oce=3600		#oce model timestep
    dt_atm=3600		#atm model timestep
    dt_ice=3600		#ice model timestep
fi

# in case model crashes (usually CICE, somewhat "too sensitive", due to "vert thermo error")  
#dt_cpl_io=1800        #ice-ocn coupling interval in seconds
#dt_oce=1800           #oce model timestep
#dt_atm=1800           #atm model timestep
#dt_ice=1800           #ice model timestep

#
## 2.2 Processor usage for this run
#

# Processor for each executable:
nproc_cpl=1		#always 1
if [[ $DEBUG = "yes" ]]; then
    nproc_atm=1		#       1
    nproc_ice=6		#changable
    nproc_oce=28	#changable 
else
    nproc_atm=1		#       1
    nproc_ice=6		#changable
    nproc_oce=120	#changable 
    #nproc_oce=28	#changable 
fi

# Total number of procs for this job (must <= requested in the #PSB line):
(( ntproc = nproc_atm + nproc_ice + nproc_oce + nproc_cpl ))

# Currently AusCOM is hardwired for mono-cpu coupling:
ncplproc_atm=1
ncplproc_ice=1
ncplproc_oce=1

# Decide ocean domain MPI partitioning pattern:
if [[ $DEBUG = "yes" ]]; then
    oce_nx=4; oce_ny=7	#oce_nx x oce_ny = nproc_oce
else
    #    oce_nx=4; oce_ny=7	#oce_nx x oce_ny = nproc_oce
    oce_nx=8; oce_ny=15	#oce_nx x oce_ny = nproc_oce
fi

#
## 2.3 Names of the 4 executables 
#

oa3_exe=oasis3
atm_exe=matmxx		#These 3 sub-model exe names much be same as
ice_exe=cicexx		#defined in namcouple and model code
ocn_exe=mom4xx		#(character*6)

#
## 2.4 Calendar date control
#
#-- Calendar type: available calendar options:
#     0   : No leap year (365 days per year)
#     1   : Gregorian (365/366 days per year)
#     n   : Equal months of "n" days (30 for 30 day months)

# Default set as below (for era40 and ncep2 forcing)
caltype=1               #0, 1 or n (eg, 30 for 30 day months)
cal_type="'julian'"     #for caltype=1; "'thirty_day'" for caltype=30. 

# For core and core2 forcing we must use below:
if [[ $atm_forcing = "'core'" || $atm_forcing = "'core2'" ]]; then
    caltype=0
    cal_type="'NOLEAP'"
fi

# Dates in format YYYYMMDD:
inidate=${iniyear}${inimonth}${iniday}
finaldate=${finalyear}${finalmonth}${finalday}

cd $jobdir

typeset -Z4 year; typeset -Z2 month day
if [ ! -f ${expid}.date ]; then
    year=${iniyear}		#
    month=${inimonth}		#
    day=${iniday}			#
    jobnum=1			# 1 for initial run, >1 for continue runs
    truntime0=0			# total accumulated runtime by the end of last run
    if [ -f  ${expid}.log ]; then
        rm ${expid}.log
    fi
    echo "`date` :  Beginning of Experiment ${expid}" > ${expid}.log
else
    #  read year month day jobnum truntime0 < ${expid}.date
    tail -1 ${expid}.date | read year month day jobnum truntime0
    echo "year month day jobnum truntime0: ", $year $month $day $jobnum $truntime0
fi

date=${year}${month}${day}
echo " " >> ${expid}.log
echo "`date` :  ${jobnum} ${date} - starting pre-processing" >> ${expid}.log

# use an external tool to work out the run date inforamtion. 
# there must be a easier way for this. but it works ok anyway.

cat > calendar.in << EOF
${inidate} ${date} ${nyear} ${nmonth} ${nday} ${caltype}
EOF

$AusCOMHOME/bin/calendar.RAIJIN < calendar.in > calendar.out 

prevdate=`cat calendar.out | cut -c '2-9'`
enddate=`cat calendar.out | cut -c '11-18'`
nextdate=`cat calendar.out | cut -c '20-27'`
previnidate=`cat calendar.out | cut -c '29-36'`
days_in_run=`cat calendar.out | cut -c '38-45'`
days_since_start=`cat calendar.out | cut -c '47-54'`
date_in_days=`cat calendar.out | cut -c '56-63'`
days_this_year=`cat calendar.out | cut -c '65-67'`
rm calendar.in calendar.out
prevyear=` echo "$prevdate" | cut -c '1-4'`
prevmonth=` echo "$prevdate" | cut -c '5-6'`
prevday=` echo "$prevdate" | cut -c '7-8'`
endyear=` echo "$enddate" | cut -c '1-4'`
endmonth=` echo "$enddate" | cut -c '5-6'`
endday=` echo "$enddate" | cut -c '7-8'`
nextyear=` echo "$nextdate" | cut -c '1-4'`
nextmonth=` echo "$nextdate" | cut -c '5-6'`
nextday=` echo "$nextdate" | cut -c '7-8'`
previniyear=` echo "$previnidate" | cut -c '1-4'`
previnimonth=` echo "$previnidate" | cut -c '5-6'`
previniday=` echo "$previnidate" | cut -c '7-8'`

echo ""
echo "first day of this run:		${date}"
echo "last day of this run:		${enddate}"
echo "initial date of the experiment:	${inidate}"
echo "final date of the experiment:	${finaldate}"
echo "day before the initial date:	${previnidate}"
echo "last day of the previous run:	${prevdate}"
echo "first day of the next run:	${nextdate}"
echo ""
echo "number of days in this run:	${days_in_run}"
echo "number of days since beginning of the experiment: ${days_since_start}"
echo ""
(( runtime = ${days_in_run} * 86400 ))  #duration of this run in seconds
echo "duration of this run in seconds:	${runtime}"

#############################################################################
#
# 3. Getting All Files into the Run Directory
#
#############################################################################

#
## 3.1 Grids, IC, forcing, exectuables and some preprocessed auxilary files
#

# following setup needs two things be decided first:

boundary_layer=gfdl	# <==how to calculte surface fluxes decided here
runoff_data=core	# <==regrided core rundoff instead of that interpolated by oasis
cold_start=1		# 1/0, this experiment starts from 'scratch'/spinup 
if [ $cold_start = 0 ]; then	#ie, warm start
    # use existing AusCOM run restart to initialise this experiment (for jobnum=1)
    # * next 4-5 lines specify the restart location -----------#
    owner=pju565
    expname=sis2-cnyf2-14
    ic_year=15
    access=~$owner	#if run under $HOME
    #access=/short/p66/$owner	#if run under /short/p66 disk
    #----------------------------------------------------------#
    typeset -Z4 ic_year
    (( ic_yearp1 = $ic_year + 1 ))
    typeset -Z4 ic_yearp1
    rest_date_oasis=${ic_year}1231
    rest_date_mom4=${ic_year}1231
    rest_date_cice=${ic_yearp1}0101
    ic_location=$access/$model/output/$expname/restart
    mom4_ic=$ic_location/mom4
    cice_ic=$ic_location/cice
    oasis_ic=$ic_location/oasis3
fi

#subdirs for CICE
INPUT=INPUT
RESTART=RESTART
HISTORY=HISTORY
#subdirs for MOM4
MOM4_input=INPUT
MOM4_restart=RESTART
MOM4_hist=HISTORY


if [ $jobnum = 1 ]; then	#initial run

    rm -rf $restdir 
    rm -rf $histdir
    rm -rf $AusCOMHOME/output/$expid
    mkdir -p $restdir/ice $restdir/ocn $restdir/cpl
    mkdir -p $histdir/ice $histdir/ocn
    ln -s $outputdir $AusCOMHOME/output/.

    # Make work directories
    rm -fr $rundir; 
    mkdir -p $rundir
    ln -s $rundir $jobdir/Running.dir
    cd $rundir

    # Individual RUNDIRS RASF

    mkdir -p $atmrundir/INPUT				#subdirs for MATM
    mkdir -p $icerundir/$INPUT -p $icerundir/$RESTART -p $icerundir/$HISTORY 	#subdirs for CICE
    mkdir -p $ocnrundir/$MOM4_input $ocnrundir/$MOM4_restart $ocnrundir/$MOM4_hist	#subdirs for MOM4
    mkdir -p $cplrundir
    # Old Auscom uses $MOM4_input, $MOM4_restart. Make compatible for the moment RASF
    cd $ocnrundir
    #  if [ ! -f $MOM4_input ]; then
    #     ln -s INPUT $MOM4_input
    #  fi
    #  if [ ! -f $MOM4_restart ]; then
    #     ln -s RESTART $MOM4_restart
    #  fi

    # get the executables:
    cd $rundir
    if [[ $DEBUG = "yes" ]]; then
        cp -f $bindir/oasis3_${chan}.${platform}_debug $oa3_exe
    else   
        cp -f $bindir/oasis3_${chan}.${platform} $oa3_exe
    fi
    if [[ $DEBUG = "yes" ]]; then
        cp -f $bindir/mom4_${chan}.debug.${platform} $ocn_exe
    else 
        cp -f $bindir/mom4_${chan}.${platform} $ocn_exe
    fi
    if [[ $DEBUG = "yes" ]]; then
        cp -f $bindir/cice_${chan}.debug.${platform}_${nproc_ice}p $ice_exe
    else
        cp -f $bindir/cice_${chan}.${platform}_${nproc_ice}p $ice_exe
    fi
    if [[ $DEBUG = "yes" ]]; then
        cp -f $bindir/matm_${chan}.${platform}_nt62_debug $atm_exe
    else
        cp -f $bindir/matm_${chan}.${platform}_nt62 $atm_exe
    fi

    # get input files for oasis3:

    # a. ref and grids data
    cd $cplrundir
    cp -f $inputdir/oasis3/cf_name_table.txt	.
    cp -f $inputdir/oasis3/oasis3_grids_20101208.nc grids.nc
    cp -f $inputdir/oasis3/oasis3_masks_20101208.nc masks.nc
    cp -f $inputdir/oasis3/oasis3_areas_20101208.nc areas.nc

    # b. restart
    if [ $cold_start = 1 ]; then       #cold start
        # the pre-processed coupling restart files:
        cp -f $inputdir/oasis3/AusCOM3.0_a2i_10fields_T0.nc	a2i.nc
        cp -f $inputdir/oasis3/AusCOM3.0_o2i_7fields_T0.nc	o2i.nc
        cp -f $inputdir/oasis3/AusCOM3.0_i2o_13fields_T0.nc	i2o.nc
        cp -f $inputdir/oasis3/AusCOM3.0_i2a_1fields_T0.nc	i2a.nc
    else					#warm start
        # rstart from an existing run (spinup)
        cp -f $oasis_ic/a2i.nc-$rest_date_oasis	a2i.nc
        cp -f $oasis_ic/o2i.nc-$rest_date_oasis	o2i.nc
        cp -f $oasis_ic/i2o.nc-$rest_date_oasis	i2o.nc
        cp -f $oasis_ic/i2a.nc-$rest_date_oasis	i2a.nc
    fi

    #ln -sf /short/p66/dhb599/AusCOM3.0/exp/rmp_save/rmp* .

    # get input files for matm (to be done in section 4)
    cd $atmrundir
    ln -sf $cplrundir/*.nc .

    # input files for cice:
    cd $icerundir
    if [ $runoff_data = core ]; then
        cp -f $inputdir/cice/AusCOM3.0_core_runoff_regrid.nc $INPUT/core_runoff_regrid.nc 
    fi

    # a. grid data and surface 
    cp -f $inputdir/cice/cice_grid_20101208.nc		$INPUT/grid.nc
    cp -f $inputdir/cice/cice_kmt_20101208.nc		$INPUT/kmt.nc

    # b. IC/restart 
    if [ $cold_start = 1 ]; then       #cold start
        runtype="'initial'"; Lrestart=.false.; ice_ic="'default'"
        #    cp -f $inputdir/cice/A2I_time0_10fields.nc	$INPUT/A2I_time0.nc
        cp -f $inputdir/cice/SSTS_12Jans.nc 	$INPUT/monthly_sstsss.nc 
        if [ $boundary_layer = gfdl ]; then
            cp -f $inputdir/cice/uu_star_t0.nc	$INPUT/u_star.nc
        fi
    else	#warm start
        runtype="'continue'"; Lrestart=.true.; ice_ic="'default'"
        ice_restart=${cice_ic}/iced.$rest_date_cice
        # instead of just copying the CICE restart dump
        # also reset time
        $bindir/cicedumpdatemodify.py -v -i $ice_restart -o $RESTART/iced
        # cp -f $ice_restart 				$RESTART/iced
        echo iced  >  				$RESTART/ice.restart_file
        #    cp -f ${cice_ic}/A2I_time1.nc-$rest_date_oasis	$INPUT/A2I_time0.nc
        if [ $boundary_layer = gfdl ]; then
            cp -f ${cice_ic}/u_star.nc-$rest_date_oasis	$INPUT/u_star.nc
        fi
    fi 
    ln -sf $cplrundir/*.nc .

    # get input files for mom4:
    cd $ocnrundir
    ln -sf $cplrundir/*.nc .
    cd $MOM4_input
    cp -f $inputdir/mom4p1/field_table_20110404	field_table
    cp -f $inputdir/mom4p1/data_table		data_table	
    cp -f $inputdir/mom4p1/grid_spec.auscom.20110618.nc grid_spec.nc 
    cp -f $inputdir/mom4p1/geothermal_heating_auscom_20080605.nc geothermal_heating.nc
    cp -f $inputdir/mom4p1/tides_auscom_20080605.nc     tideamp.nc
    cp -f $inputdir/mom4p1/roughness_auscom_20080605_roughness_amp.nc roughness_amp.nc
    #cp -f $inputdir/mom4p1/seawifs_auscom_edit_time.nc		ssw_atten_depth.nc
    cp -f $inputdir/mom4p1/seawifs_auscom_20111118_edit_time.nc	ssw_atten_depth.nc
    cp -f $inputdir/mom4p1/salt_sfc_restore_20110829.nc 	salt_sfc_restore.nc
    cp -f $inputdir/mom4p1/temp_sfc_restore_20110829.nc 	temp_sfc_restore.nc
    if [ $cold_start = 1 ]; then 
        #get ocean initial condition (only T-S)
        cp -f $inputdir/mom4p1/ocean_temp_salt.20110518.nc	ocean_temp_salt.res.nc
    else
        for restfile in `ls ${mom4_ic}/ocean_*-${rest_date_mom4}`; do
            newfile=${restfile##*/}
            cp ${restfile} ${newfile%-*}
        done
        ystart=$ic_yearp1
        if [ $ystart -lt 10 ]; then
            typeset -Z1 ystart
        elif [ $ystart -lt 100 ]; then
            typeset -Z2 ystart
        elif [ $ystart -lt 1000 ]; then
            typeset -Z3 ystart
        elif [ $ystart -lt 10000 ]; then
            typeset -Z4 ystart
        fi
        ed ocean_solo.res <<eof
g/$ystart/s/$ystart/${iniyear}/
w
q
eof
    fi	#cold_start

else	#for continue runs

    cd $rundir
    rm -f *out* ?weights *.prt* 	#clean up 
    cd $atmrundir
    rm -f *out* ?weights *.prt* 	#clean up 
    cd $icerundir
    rm -f *out* ?weights *.prt* 	#clean up 
    cd $cplrundir
    rm -f *out* ?weights *.prt* 	#clean up 

    #prepare restart files:

    # for oasis3:  
    cd $cplrundir
    for resfile in `ls $restdir/cpl/?2?.nc-${prevdate}`; do
        sresfile=${resfile##*/}		#take away the front path name
        cp $resfile ${sresfile%-*}		#take away the appendix '-YYYYMMDD'
    done

    # for cice: 
    cd $icerundir
    runtype="'continue'"; Lrestart=.true.; ice_ic="'default'"
    #  cp $restdir/ice/A2I_time1.nc-${prevdate} $INPUT/A2I_time0.nc
    cp $restdir/ice/ice.restart_file-${prevdate} $RESTART/ice.restart_file
    cp $restdir/ice/`cat $RESTART/ice.restart_file` $RESTART/.
    cp $restdir/ice/u_star.nc-${prevdate} $INPUT/u_star.nc
    if [ -f $restdir/ice/sicemass.nc-${prevdate} ]; then
        cp $restdir/ice/sicemass.nc-${prevdate} $INPUT/sicemass.nc
    fi

    # for mom4: 
    cd $ocnrundir
    for restfile in `ls $restdir/ocn/ocean*-${prevdate}`; do
        ncfile=${restfile##*/}
        cp $restfile $MOM4_input/${ncfile%-*}
    done

fi	#initial or continue run 

# prepare the atm_forcing dataset needed for this run:
cd $atmrundir/INPUT

typeset -Z4 y1 y2
y1=$endyear
y2=$endyear
if [ $datatype = NY ]; then
    y1=1                  #for 'NY' forcing, y1 should be always '0001' !
fi
$inputdir/matm/get_${atmdata}_${datatype}.ksh $y1 $y2 $AusCOMHOME
y2=`expr $endyear + 1`
y1=$y2
if [ $datatype = NY ]; then
    y1=1
fi
if [ $endyear = $year_data_end ]; then
    y1=$endyear
fi
$inputdir/matm/get_${atmdata}_${datatype}.ksh $y1 $y2 $AusCOMHOME

#
## 3.2 Adapting or creating configuration files
#

# 3.2.1 namelist for oasis3:

nlogprt=1 	#cplout writing control: 0-no, 1-medium, 2-full output
npt1=${nproc_ice}; npc1=${ncplproc_ice}; arg1=$ice_exe; nam1=$ice_exe
npt2=${nproc_atm}; npc2=${ncplproc_atm}; arg2=$atm_exe; nam2=$atm_exe
npt3=${nproc_oce}; npc3=${ncplproc_oce}; arg3=$ocn_exe; nam3=$ocn_exe

#-- buffered MPI Send for coupling communication
#      yes: buffered send   (for MPI, or MPI2 without 'mailbox')
#       no: simple send     (for MPI2 with big enough 'mailbox')
#--------------------------------------------------------------------------------------
#bsend=yes      # needs larger buffer size for MPI_Bsend operation: to make it work,
#       we've doubled "il_bufsendsize" in oasis3/src/inicmc.F!
bsend=no	# this one works fine, and is recommended! 

if [ ${bsend} = no ]; then
    nobsend="NOBSEND"
else
    nobsend=""
fi
if [ $chan = 'MPI1' ]; then
    arg1=""; arg2=""; arg3=""
fi

#
# get and adapt file namcouple
#

cd $cplrundir

cp -f $inputdir/oasis3/namcouple_31fields_mom4p1_pubrel_18dec2009_wind_rotate	namcouple
chmod u+w namcouple

ed namcouple <<eof
g/#Channel/s/#Channel/${chan} ${nobsend}/
g/#Mod1procs/s/#Mod1procs/ $npt1 $npc1 $arg1 /
g/#Mod2procs/s/#Mod2procs/ $npt2 $npc2 $arg2 /
g/#Mod3procs/s/#Mod3procs/ $npt3 $npc3 $arg3 /
g/#Mod1_name/s/#Mod1_name/ $nam1 /
g/#Mod2_name/s/#Mod2_name/ $nam2 /
g/#Mod3_name/s/#Mod3_name/ $nam3 /
g/#Runtime_sec/s/#Runtime_sec/${runtime}/
g/#Inidate/s/#Inidate/${date}/
g/#Caltype/s/#Caltype/${caltype}/
g/#NLOGPRT/s/#NLOGPRT/${nlogprt}/
g/#CPL_intv_ai/s/#CPL_intv_ai/${dt_cpl_ai}/
g/#CPL_intv_io/s/#CPL_intv_io/${dt_cpl_io}/
g/#DT_OCE/s/#DT_OCE/${dt_oce}/
g/#DT_ATM/s/#DT_ATM/${dt_atm}/
g/#DT_ICE/s/#DT_ICE/${dt_ice}/
w
q
eof

# 3.2.2 namelist for matm coupling 

cd $atmrundir

cat > input_atm.nml << eof
&coupling
init_date=${iniyear}${inimonth}${iniday}
inidate=$date
truntime0=$truntime0
runtime=$runtime
dt_cpl=$dt_cpl_ai
dt_atm=$dt_atm
dataset=$atm_forcing
runtype='${datatype}'
caltype=$caltype
days_per_year=$days_this_year
chk_a2i_fields=.false.
chk_i2a_fields=.false.
&end
eof

# get and adapt the forcing pointer file:
cp -f $inputdir/matm/${atmdata}_fields_$datatype.table data_4_matm.table
chmod u+w data_4_matm.table

ed data_4_matm.table <<eof
g/#YEAR/s/#YEAR/$endyear/
g/#FORCING/s/#FORCING/INPUT/
w
q
eof

# 3.2.3 namelists for cice 

# a. standalone mode input
#
cd $icerundir

npt_cice=`expr $runtime / $dt_ice`
if [ $nmonth != 0 ]; then         	#ie, nmonth=1, a whole month run
    histfreq="'m','x','x','x','x'"; hist_avg=.true.; dumpfreq="'m'"; dumpfreq_n=$nmonth
else					#ie, nmonth=0, an nday run 
    histfreq="'d','x','x','x','x'"; hist_avg=.true.; dumpfreq="'d'"; dumpfreq_n=$nday
fi			#hist_avg=.false. would output snapshot hist
mixedocean=.false.      #use or not use the mixed ocean layer

# variables related to ice thickness sensitivity
#
# conductivity MU71 or bubbly
conduct="'bubbly'"
# ridging folding scale default 4
mu_rdg=2.0
# turning angle default 0 degrees
cosw=1.00
sinw=0.0000
# shortwave parametrisation default or dEdd
shortwave="'default'"
# dEdd tuning parameters defs 0.
R_ice=0.0
R_pnd=0.0
R_snw=0.0
# albedos for ice default 0.78 and 0.36
albicev=0.86
albicei=0.44
# albedos for snow default 0.78 and 0.36
albsnowv=0.98
albsnowi=0.70
# snowpatchiness default 0.02
snowpatch=0.01
# change in temperature to give dalb_mlt change default 1.0
dT_mlt=1.0
# albedo change per dT_mlt change in temperature default -0.05
dalb_mlt=-0.02
# maximum thickness of ice that rafts (m) default 1.0
maxraft=0.5
# thickness above which ice albedo is constant (m) default 0.5
ahmax=0.1
# albedo weigths
# visible, direct default 0.00318
awtvdr=0.00318
# near IR, direct default 0.00182
awtidr=0.00182
# visible, diffuse default 0.63282
awtvdf=0.63282
# near IR, diffuse default 0.36218
awtidf=0.36218
# linear_S or constant freezing temp, default linear_S
Tfrzpt="'linear_S'"
# minimum ice-ocean friction velocity def. 0.005
ustar_min=0.0005
# Freezing point of ocean water def -1.8
Tocnfrz=-1.8
# ice-ocean drag def. 0.00536
dragio=0.00536
# ice-ocean heat transfer coefficient def. 0.004
chio=0.004
# ice surfare roughness length, def 0.0005 m
iceruf=0.0005

cp ${inputdir}/cice/cice4.1_in.nml.sis2_new     cice_in.nml
chmod u+w cice_in.nml

ed cice_in.nml <<eof
g/#DAYS_per_year/s/#DAYS_per_year/${days_this_year}/
g/#YEAR_init/s/#YEAR_init/${iniyear}/
g/#DT_CICE/s/#DT_CICE/${dt_ice}/
g/#NPT/s/#NPT/${npt_cice}/
g/#RUNTYPE/s/#RUNTYPE/${runtype}/
g/#HISTFREQ/s/#HISTFREQ/${histfreq}/
g/#HIST_AVG/s/#HIST_AVG/${hist_avg}/
g/#NDAY/s/#NDAY/${nday}/
g/#DUMPFREQ/s/#DUMPFREQ/${dumpfreq}/
g/#DUMPFR_N/s/#DUMPFR_N/${dumpfreq_n}/
g/#RESTART/s/#RESTART/${Lrestart}/
g/#ICE_IC/s/#ICE_IC/${ice_ic}/
g/#FYEAR_init/s/#FYEAR_init/${iniyear}/
g/#MIXEDOCN/s/#MIXEDOCN/${mixedocean}/
g/#NPROCS/s/#NPROCS/${nproc_ice}/
g/#MU_RDG/s/#MU_RDG/${mu_rdg}/
g/#CONDUCT/s/#CONDUCT/${conduct}/
g/#SHORTWAVE/s/#SHORTWAVE/${shortwave}/
g/#R_ICE/s/#R_ICE/${R_ice}/
g/#R_PND/s/#R_PND/${R_pnd}/
g/#R_SNW/s/#R_SNW/${R_snw}/
g/#SNOWPATCH/s/#SNOWPATCH/${snowpatch}/
g/#AHMAX/s/#AHMAX/${ahmax}/
g/#DT_MLT/s/#DT_MLT/${dT_mlt}/
g/#DALB_MLT/s/#DALB_MLT/${dalb_mlt}/
g/#MAXRAFT/s/#MAXRAFT/${maxraft}/
g/#ALBICEI/s/#ALBICEI/${albicei}/
g/#ALBICEV/s/#ALBICEV/${albicev}/
g/#ALBSNOWI/s/#ALBSNOWI/${albsnowi}/
g/#ALBSNOWV/s/#ALBSNOWV/${albsnowv}/
g/#AWTVDF/s/#AWTVDF/${awtvdf}/
g/#AWTIDF/s/#AWTIDF/${awtidf}/
g/#AWTVDR/s/#AWTVDR/${awtvdr}/
g/#AWTIDR/s/#AWTIDR/${awtidr}/
g/#TFRZPT/s/#TFRZPT/${Tfrzpt}/
g/#COSW/s/#COSW/${cosw}/
g/#SINW/s/#SINW/${sinw}/
g/#USTAR_MIN/s/#USTAR_MIN/${ustar_min}/
g/#TOCNFRZ/s/#TOCNFRZ/${Tocnfrz}/
g/#DRAGIO/s/#DRAGIO/${dragio}/
g/#CHIO/s/#CHIO/${chio}/
w
eof

# b. namelist for coupling purpose

POP_ICEDIAG='.true.'		#use POP approach for ice formation/melting
GFDL_FLUXES='.false.'
if [ $boundary_layer = gfdl ]; then
    GFDL_FLUXES='.true.'		#use GFDL code for surface flux calculation
    cat > input_ice_gfdl.nml << eof
&surface_flux_nml
no_neg_q              = .false.
use_virtual_temp      = .true.
alt_gustiness         = .false.
old_dtaudv            = .false.
use_mixing_ratio      = .false.
gust_const            =  1.0
gust_min              =  0.0
ncar_ocean_flux       = .true.
ncar_ocean_flux_orig  = .false.
raoult_sat_vap        = .false.
/
&ocean_rough_nml
roughness_mom   = 5.8e-5
roughness_heat  = 5.8e-5
roughness_moist = 5.8e-5
roughness_min   = 1.0e-6
charnock = 0.032
rough_scheme = 'beljaars'
do_highwind = .false.
do_cap40    = .false.
zcoh1 = 0.0
zcoq1 = 0.0
/
eof
    cat > input_ice_monin.nml << eof
&monin_obukhov_nml
neutral=.true.
&end
eof
fi	#if boundary_layer=gfdl

cat > input_ice.nml << eof
&coupling_nml
init_date=${iniyear}${inimonth}${iniday}
caltype=$caltype
jobnum=$jobnum
inidate=$date
runtime0=$truntime0
runtime=$runtime
dt_cpl_ai=$dt_cpl_ai
dt_cpl_io=$dt_cpl_io
dt_cice=$dt_ice
pop_icediag=$POP_ICEDIAG
ice_pressure_on=.true.
ice_fwflux=.true.
use_ocnslope=.false.
use_umask=.false.
rotate_winds=.false.
limit_icemelt=.false.
meltlimit=-200.0
use_core_runoff=.true.
precip_factor=1.0
cst_ocn_albedo=.true.
ocn_albedo=0.1
gfdl_surface_flux=$GFDL_FLUXES
chk_gfdl_roughness=.false.
chk_frzmlt_sst=.false.
chk_i2o_fields=.true.
chk_o2i_fields=.false.
chk_i2a_fields=.false.
chk_a2i_fields=.false.
&end
eof

# 3.2.4 namelists for mom4p1

cd $ocnrundir

# a. standalone mode input namelist file
#cp -f $inputdir/mom4/mom4_in.nml	input.nml
cp -f $inputdir/mom4p1/mom4_in_20110823.nml-BL input.nml
chmod u+w input.nml

alap=1.0e5
truncate_velocity='.true.'  
truncate_verbose='.true.'
if [[ $year -gt $iniyear ]]; then
    truncate_velocity='.false.'
    truncate_verbose='.false.'
fi

#temp_restore_tscale=30.0	#sst restoring time scale of 30 days
temp_restore_tscale=-1.0	#NO SST restoration!
#salt_restore_tscale=60.0	#sss restoring time scale of 60 days
salt_restore_tscale=15.0	#strong SSS relaxation as 'recommended'
#salt_restore_tscale=-1		#NO SSS restoration!
use_waterflux='.true.'
layout=$oce_nx,$oce_ny		#mpi partitioning pattern
Simple_frazil='.false.'		#simple temp frazil. if '.f.' use complicated scheme
#                    and allow multi-layer frazil.
Accurate_frazil='.true.'        #accurate temp frazil. must be .t. if Simple_frazil=.f. 
#		     vice versa.	
TL_frazil='.false.'		#top layer frazil. if '.f.' multi-layer frazil 

convection='.true.'
aredi=600.
agm=100.
smax=0.002
swidth=0.002
ricr=0.3

# AusCOM treatment of ocean background vertical diffusivity
# 3 possible cases	*** ONLY USE one of the three to avoid double counting
#	1. set in kpp scheme
#	2. set in tidal mixing
#	3. use Bryan-Lewis scheme
# 
###	1 kpp 
#bryan_lewis='.false.'		#turn OFF B-L background diffusion profile
#bg_diff=0.0			#turn OFF tidal background diffusivity
#bg_visc=0.0			#turn OFF tidal background viscosity
#diff_cbt_iw=0.1e-4		#KPP bg diff set ON
#visc_cbu_iw=0.1e-4  		#KPP bg visc set ON
###	2 tidal
bryan_lewis='.false.'		#turn OFF B-L background diffusion profile
bg_diff=1.0e-5			#tidal background diff set ON
bg_visc=1.0e-4			#tidal background visc set ON
diff_cbt_iw=0.0   		#KPP bg diff set OFF
visc_cbu_iw=0.0			#KPP bg viso set OFF
###	3 Bryan Lewis
#bryan_lewis='.true.'		#turn ON B-L background diffusion profile
#bg_diff=0.0			#turn OFF tidal background diff
#bg_visc=1.0e-4 			#turn ON tidal background visc  <<<<<<!!!>>>>>>
#diff_cbt_iw=0.0   	 	#KPP bg diff set OFF
#visc_cbu_iw=0.0			#KPP bg viso set OFF

ed input.nml <<eof
g/#NMONTH/s/#NMONTH/${nmonth}/
g/#NDAY/s/#NDAY/${nday}/
g/#SYEAR/s/#SYEAR/${iniyear}/
g/#SMON/s/#SMON/${inimonth}/
g/#SDAY/s/#SDAY/${iniday}/
g/#CAL_TYPE/s/#CAL_TYPE/${cal_type}/
g/#DT_CPL/s/#DT_CPL/${dt_cpl_io}/
g/#DT_OCE/s/#DT_OCE/${dt_oce}/
g/#LAYOUT/s/#LAYOUT/${layout}/
g/#VLIMIT/s/#VLIMIT/${truncate_velocity}/
g/#VWARN/s/#VWARN/${truncate_verbose}/
g/#SST_restoring/s/#SST_restoring/${temp_restore_tscale}/
g/#SSS_restoring/s/#SSS_restoring/${salt_restore_tscale}/
g/#Freezing_simple/s/#Freezing_simple/${Simple_frazil}/
g/#Freezing_accurate/s/#Freezing_accurate/${Accurate_frazil}/
g/#TL_frazil_only/s/#TL_frazil_only/${TL_frazil}/
g/#VISC_CBU_IW/s/#VISC_CBU_IW/${visc_cbu_iw}/
g/#CONVECTION/s/#CONVECTION/${convection}/
g/#AREDI/s/#AREDI/${aredi}/
g/#AGM/s/#AGM/${agm}/
g/#SMAX/s/#SMAX/${smax}/
g/#SWIDTH/s/#SWIDTH/${swidth}/
g/#RICR/s/#RICR/${ricr}/
g/#USE_waterflux/s/#USE_waterflux/${use_waterflux}/
g/#Bryan_Lewis/s/#Bryan_Lewis/${bryan_lewis}/
g/#DIFF_CBT_IW/s/#DIFF_CBT_IW/${diff_cbt_iw}/
g/#VISC_CBU_IW/s/#VISC_CBU_IW/${visc_cbu_iw}/
g/#BG_DIFF/s/#BG_DIFF/${bg_diff}/
g/#BG_VISC/s/#BG_VISC/${bg_visc}/
w
q
eof


# RASF (old style AusCOM)
if [ ! -f mom4_in.nml ]; then
    ln -s  input.nml mom4_in.nml
fi

#'base_time' is read in from diag_table, and must NOT be changed during the exp.
if [ $jobnum = 1 ]; then 
    cp -f $inputdir/mom4p1/diag_table_20101217_sjm599 $MOM4_input/diag_table
    chmod u+w ${MOM4_input}/diag_table
    ed $MOM4_input/diag_table <<eof
g/#SYEAR/s/#SYEAR/${year}/
g/#SMON/s/#SMON/${month}/
g/#SDAY/s/#SDAY/${day}/
w
q
eof
fi

# b. namelist for coupling purpose
#

icemlt_factor=1.0	#if < 1, reduce the potential ice melt
#only usable when POP_icediag=.f. 
frazil_factor=0.5	#mom4 uses two-level frog time stepping but cice
#uses forward time-stepping (see comments in code)
frazil_factor=1.0	#CH: MOM4 and CICE use same (two-timelevel) stepping!

cat >> input.nml << eof
&auscom_ice_nml
dt_cpl=$dt_cpl_io
do_ice_once=.false.
pop_icediag=$POP_ICEDIAG
kmxice=5 
fixmeltT=.false.
Tmelt=-.216
use_ioaice=.true.
aice_cutoff=0.15
icemlt_factor=$icemlt_factor
frazil_factor=$frazil_factor 
iceform_adj_salt=.false.
sign_stflx=1.0
chk_i2o_fields=.false.
chk_o2i_fields=.false.
/

&bg_diff_lat_dependence_nml
lat_low_bgdiff=20.
bg_diff_eq=1.0e-6
/
eof

#RASF
if [ ! -f data_table ]; then
    ln -s $MOM4_input/*table .
fi
#############################################################################
#
# 4. Launch/Execute the AusCOM Coupled Model on VAYU
#
#############################################################################
set -e

echo "`date` :  ${jobnum} ${date} - starting mpirun/mpiexec" >> $jobdir/${expid}.log

echo
echo "*** mpirun/mpiexec started at: " `date` "***"
echo

#David Singleton's solution to the bad performance of model: 
# NOTE: Surely no longer needed?
#export PATH=/opt/anumpirun/2.1.16a/bin:$PATH
module load openmpi

sleep 10
cd $rundir
if [[ $DEBUG = "yes" ]]; then
    module load totalview
    mpirun --debug --mca mpi_paffinity_alone 1 -wd $cplrundir -n $nproc_cpl $rundir/$oa3_exe : \
    -wd $icerundir -n $nproc_ice $rundir/$ice_exe : \
    -wd $atmrundir -n $nproc_atm $rundir/$atm_exe : \
    -wd $ocnrundir -n $nproc_oce $rundir/$ocn_exe
else
    mpirun --mca mpi_preconnect_mpi 1 --mca mpi_paffinity_alone 1 -wd $cplrundir -n $nproc_cpl $rundir/$oa3_exe : \
    -wd $icerundir -n $nproc_ice $rundir/$ice_exe : \
    -wd $atmrundir -n $nproc_atm $rundir/$atm_exe : \
    -wd $ocnrundir -n $nproc_oce $rundir/$ocn_exe
fi
echo
echo "*** job completed  at: " `date` "***" 
echo
echo "`date` :  ${jobnum} ${enddate} - done mpirun/mpiexec!" >> $jobdir/${expid}.log
echo 'Error code at end of simulation :'$?

#############################################################################
#
# 5. Postprocessing: Saving the Output Data
#
#############################################################################

cd $atmrundir
rm INPUT/*.$endyear.nc	#remove the used forcing data 

#
## 5.1 Output files of the coupler (OASIS3)
#

# Restart files 
cd $cplrundir
for resfile in `ls ?2?.nc`; do
    mv $resfile ${restdir}/cpl/${resfile}-${enddate} 
done

#
## 5.2 Output files of the ocean (mom4)
#

# Restart files
cd $ocnrundir/$MOM4_restart/
for restfile in `ls ocean_*`; do
    mv $restfile ${restdir}/ocn/${restfile}-${enddate}
done  

# History files
cd $ocnrundir/$MOM4_hist
sdir=$ocnrundir/$MOM4_hist 
tdir=${histdir}/ocn
tool=${bindir}/do_mppncombine.ksh
#archjob=`qsub $tool -v sdir=$sdir,tdir=$tdir,idate=${enddate}`
archjob=$(qsub ${tool} -v bdir=${bindir},sdir=${sdir},tdir=${tdir},idate=${enddate})

#
## 5.4 Output files of the ice (cice)
#

cd $icerundir

# Restart files
#mv $INPUT/A2I_time1.nc       ${restdir}/ice/A2I_time1.nc-${enddate}
mv $RESTART/ice.restart_file ${restdir}/ice/ice.restart_file-${enddate}
mv $RESTART/iced.*           ${restdir}/ice/. 
if [ -f u_star.nc ]; then
  mv u_star.nc			 ${restdir}/ice/u_star.nc-${enddate}
fi
if [ -f sicemass.nc ]; then
  mv sicemass.nc			 ${restdir}/ice/sicemass.nc-${enddate}
fi

# History files (iceh_*.nc)
mv $HISTORY/*			${histdir}/ice/.
#grep -e "Arctic" -B 1 -A 10 ice_diag.d > ${histdir}/cice/ice_diag.${enddate}
cp -p ice_diag.d ${histdir}/ice/ice_diag.${enddate}
for fio in iceout??; do
    cp -p $fio ${histdir}/ice/$fio.${enddate}
done

#
## 5.5 Store coupling fields output (if any), e.g., fields_i2o_in_ice.nc etc. 
#
#cd $cplrundir
for tdir in $icerundir $atmrundir $ocnrundir; do
cd $tdir
if [[ `ls fields*.nc | wc -w` -gt 0 ]]; then
for tmpfile in `ls fields*.nc`; do
  mv -f ${tmpfile} ${restdir}/cpl/${tmpfile}_${enddate}
done
fi
done

#############################################################################
#
# 6. Submission of the next job
#
#############################################################################

cd  ${jobdir}

#
# Number of the next job
#
(( nextjob = ${jobnum} + 1 ))

#
# update .date and .log file
#
#if [ -f ${expid}.date ]; then
#  mv ${expid}.date  ${expid}.date_${jobnum}
#fi
truntime0=`expr ${truntime0} + ${runtime}` 
echo "${nextyear} ${nextmonth} ${nextday} ${nextjob} ${truntime0}" >> ${expid}.date
echo "`date` :  ${jobnum} ${enddate} - done post-processing!" >> ${expid}.log

#
# Check whether final date is reached. If not, keep going
# 
if [[ $nextdate -gt $finaldate ]]; then
  echo "Experiment over"
  echo "`date` :  Experiment over" >> ${expid}.log
else
  next_jobid=`qsub -W depend=after:${archjob} run_auscom.VAYU`
  echo "`date` :  New run is submitted."
fi

#############################################################################
#
# 7. Epilogue 
#
#############################################################################

date
exit
