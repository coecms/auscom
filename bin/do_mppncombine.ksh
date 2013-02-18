#!/bin/ksh
#############################################################################
#									    #	
# usage: qsub do_mppncombine.ksh -v sdir=$sdir,tdir=$tdir,idate=$enddate    #
#									    #
#############################################################################
#PBS -P p66
###PBS -P p73
###PBS -q normal
###PBS -q express
#PBS -q copyq
#PBS -l walltime=0:15:00
###PBS -l vmem=2GB
#PBS -l vmem=400MB
#PBS -l ncpus=1
#PBS -l software=vampir
#PBS -l other=rms
#PBS -M dave.bi@csiro.au
#PBS -N do-mppncombing
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

# combine netcdf files
for histfile in `ls *.nc.0000`; do
  newfile=${histfile%.*}              #drop the appendix '.0000'!
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

