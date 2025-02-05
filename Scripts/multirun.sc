#!/bin/bash
#
#  ---------------------------------------------------------
#  ---  script that allows for multiple runs of ebmmodel ---
#  ---------------------------------------------------------
#

usage() {
  echo "------------------------------------------"
  echo "-- Options                              --"
  echo "-- -aws name = run model for station name nr times     --"
  echo "-- -run nr     = number of runs, default is 1     --"
  echo "------------------------------------------"
}

INPATH=$(pwd)/../Input/
OUTPATH=$(pwd)/../Output/
RUNS=1

if [[ $# == 0 ]]; then
 usage
 exit 1
fi
while [[ $# -gt 0 ]]; do
 key="$1"
 shift
 
 case $key in
  -aws|-AWS)
   STATION="$1"
   shift
  ;;
  -run|-RUN)
   RUNS=$1
   shift
  ;;
 esac
done

#
INPUT=${INPATH}/${STATION}/
if [[ ! -r $INPUT ]]; then
 echo
 echo "input not present" $INPUT
 echo
 exit
fi
#set RANDOM = ${OUTPATH}"/"${STATION}"/random_"${STATION}".txt"
#if !( -r $RANDOM ) then
#  echo "random summary file not present, created: "
#  echo $RANDOM
#  echo "z0msn rhosnpr summelt meltjan meltfeb meltmar meltapr meltmay meltjun meltjul meltaug meltsep meltoct meltnov meltdec meandt0 stddt0" >> ${RANDOM}
#endif

# SET MAX NUMBER OF RUNS HERE
jmax=$RUNS
j=1
while [[ $j -le $jmax ]]; do
 echo
 echo 'run ' $j 'of' $jmax
#  cp $RANDOM $PATH"/random_"${STATION}".txt"
 ./ebmmodel.sc -aws ${STATION}
 status=$?
 if [[ $status -ne 0 ]]; then
  echo "EBM model exit code $status"
#  exit 1
 fi
#  mv $PATH"/random_"${STATION}".txt" $RANDOM  
 ((j++))
done

