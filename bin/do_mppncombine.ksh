#!/bin/ksh
#############################################################################
#									    #	
# usage: qsub do_mppncombine.ksh -v sdir=$sdir,tdir=$tdir,idate=$enddate    #
#									    #
#############################################################################
#PBS -P v45
#PBS -q copyq
#PBS -l walltime=0:15:00
#PBS -l vmem=2GB
#PBS -l ncpus=1
#PBS -N do-mppncombine
#PBS -wd
#############################################################################
date
set -e
set -xv

ulimit -s unlimited
ulimit -a

bindir=$bdir
datadir=$sdir
workdir=$tdir
enddate=$idate

cd $workdir

# clean up (in case do_mppncombine failed last time)
badfile=`ls *.nc *.nc.???? | wc -w`
if (( $badfile == 0 )); then
  echo 'Good, no leftover! '
else
  for badfile in `ls *.nc *.nc.????`; do
    \rm $badfile
  done 
fi

mv $datadir/*.nc.???? .
mv $datadir/ocean_scalar.nc ./ocean_scalar.nc-${enddate}

# combine netcdf files
for histfile in `ls *.nc.0000`; do
  newfile=${histfile%.*}              #drop the appendix '.0000'!
  #SJM#$bindir/mppnccombine.exe -v -r $newfile ${newfile}.????
  #/home/599/sjm599/AusCOM1.0/bin/mppnccombine.exe -v -r $newfile ${newfile}.????
  $bindir/mppnccombine.exe -v -r $newfile ${newfile}.????
done
for histfile in `ls ocean*.nc`; do
  mv $histfile ${histfile}-${enddate}
done

#gzip *-${enddate}

#############################################################################
date
${job_account}
exit

