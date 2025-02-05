#!/bin/bash
#
#  ---------------------------------------------------------
#  ---  script "ebmmodel.sc" is the ebmmodel manager ---
#  ---------------------------------------------------------
#

usage() {
  echo "------------------------------------------"
  echo "-- Options                              --"
  echo "-- -m         = compile                 --"
  echo "-- -aws name  = run model for station number i/o including compilation     --"
  echo "-- name = combi region and aws number exp: svb_aws01 --"
  echo "-- -e exp     = experiment name o       --"
  echo "-- -clean     = clean source directory but leave inittables.o (long compilation time)"
  echo "-- -cleanall  = clean source directory completely"
  echo "------------------------------------------"
}

echo "$0 $@" > last_exec.txt
#
#  ---------------------------------------------------------
#  set general environment variables
#
CWD=$(pwd)
SCRATCH=${CWD}/..
SOURCE=${SCRATCH}/Source
UTILS=${SCRATCH}/../Utils
DPATH=${SCRATCH}/Input
#
MAKE="off"
RUN="off"
CLEAN="off"
CLEANALL="off"
STATION=""
EXP=""
TYPE=""

if [[ $# == 0 ]]; then
 usage
 exit 1
fi

while [[ $# -gt 0 ]]; do
 key="$1"
 shift
 
 case $key in
  -m)
   MAKE="on"
  ;;
  -r)
   RUN="on"
  ;;
  -aws|-AWS)
   STATION="$1"
   MAKE="on"
   RUN="on"
   shift
  ;;
  -e)
   EXP="$1"
   shift
  ;;
  -clean)
   CLEAN="on"
  ;;
  -cleanall)
   CLEANALL="on"
  ;;
 esac
done

#echo "compilation" $MAKE 
#echo "running model" $RUN 
#echo "i/o name" $STATION
#echo "experiment name" $EXP

INPUT=${DPATH}/${STATION}
#
#  ---------------------------------------------------------
#  define aws numbers
#
if [[ $RUN == "on" ]]; then
#
echo "AWS name and region: " $STATION 
echo $STATION $TYPE > Last_Station_Name.txt
if [[ ! -r $INPUT ]]; then
  echo
  echo "input not present" $INPUT
  echo
  exit 1
fi
#
fi
#
#  ---------------------------------------------------------
#  run clean function of makefile
#
if [[ $CLEAN != "off" ]]; then
 cd $SOURCE
 echo " cleaning source dir, leaving inittables.o"
 make -f ebmmodel.mk clean
 exit
fi
#
#  ---------------------------------------------------------
#  run cleanall function of makefile
#
if [[ $CLEANALL != "off" ]]; then
 cd $SOURCE
 echo " cleaning source dir"
 make -f ebmmodel.mk cleanall
 exit
fi
#
#  ---------------------------------------------------------
#  execute Makefile if desired
#
if [[ $MAKE != "off" ]]; then
  cd $SOURCE
  echo " execute Makefile ... "
  make -f ebmmodel.mk
  if [[ $? != 0 ]]; then
    echo " !! compilation error !!"
    exit 2
  fi
  cd $CWD
fi
#
#
#  --------------------------------------------------------
#  execute model
#
if [[ $RUN != "off" ]]; then
# check presence of input and output directories
  INPUT=${SCRATCH}/Input/${STATION}/
  OUTPUT=${SCRATCH}/Output/${STATION}/${EXP}/
  if [[ ! -r $INPUT ]]; then
    echo $INPUT
    echo "input directory not present"
    exit 1
  fi
  mkdir -p $OUTPUT
  cd $SOURCE
  FILESTAT=awsname.txt
    echo ${STATION} > ${FILESTAT}
    echo "" >> ${FILESTAT}
    if [[ -f ${INPUT}/${STATION}_ebm.txt ]]; then
      cp ${INPUT}/${STATION}_ebm.txt infofile.txt
    else if [[ -f ${INPUT}/${STATION}_ebm_global.txt ]]; then
      cp ${INPUT}/${STATION}_ebm_global.txt infofile.txt
      echo "Defaulting to global infofile"
    else
      echo "No infofile present"
      exit 3
    fi
    fi
    if [[ ! -f ${INPUT}/${STATION}_HOUR-EBM.txt ]]; then
      echo "No inputdata present"
      exit 4
    fi
    cp ${INPUT}/${STATION}_HOUR-EBM.txt inputdata.txt
    cp ${SCRATCH}/Input/spectrum/* .
  echo " execute model ... "
  time ./ebmmodel.x
  finished=$?
  echo "total elapsed time"
  echo "move output to "${OUTPUT}
  mv runinfo.txt ${OUTPUT}/${STATION}_INFO.txt
  if [[ -e output1.txt ]]; then #Only move outputfiles if they exist
    mv output1.txt ${OUTPUT}/${STATION}_AWS.txt
    mv output2.txt ${OUTPUT}/${STATION}_MB.txt
    mv output3.txt ${OUTPUT}/${STATION}_SNOW.txt
    mv output4.txt ${OUTPUT}/${STATION}_LAY1.txt
    mv output5.txt ${OUTPUT}/${STATION}_TEMP.txt
    mv output6.txt ${OUTPUT}/${STATION}_MONTH.txt
    mv output7.txt ${OUTPUT}/${STATION}_CLIM.txt
    mv output8.txt ${OUTPUT}/${STATION}_YEAR.txt
    mv output9.txt ${OUTPUT}/${STATION}_DAY.txt
    mv output10.txt ${OUTPUT}/${STATION}_SEAS.txt
  fi
  extrafile=fort.99
  if [[ -e $extrafile ]]; then
    echo 'move ' $extrafile
    mv $extrafile ${OUTPUT}/extrafile.txt
  fi
  rm ${FILESTAT} 
  rm infofile.txt
  rm inputdata.txt
#  rm IN_*.txt
  cd $CWD
fi
#
#  ---------------------------------------------------------
#  RUN the model
#
if [[ $RUN != "off" ]]; then
 if [[ $finished != "0" ]]; then
  echo " model exit with error!! " $finished
  case $finished in
   11)
    echo " error in INFO subroutine in input.f"
    echo " input value of SSAfresh is not 60, 80 or 100"
   ;;
   12|13)
    echo " error in INPDATA subroutine in input.f"
    echo " data doesn't contain correct times"
   ;;
   14)
    echo " error in CHECKDATA subroutine in input.f"
    echo " first or last input data contains error value"
   ;;
   15)
    echo " error in INPUTRADPEN subroutine in input.f"
    echo " given grain size doesn't correspond to any Mie scattering file"
   ;;
   20)
    echo " error in SNOWCONTENT subroutine in snowmodel.f"
    echo " water(1) is unphysical"
   ;;
   21)
    echo " error in DENSIFICATION subroutine in snowmodel.f"
    echo " density has become either too large (>densice) or too small (<denssnow)"
   ;;
   30)
    echo " error in INITGRID subroutine in snowgrid.f"
    echo " number of required layers exceeds maximum number of model layers"
   ;;
   40)
    echo " error in METAMORPHISM subroutine in newalbedo.f"
    echo " calculated water content is larger than 1"
   ;;
   41|42)
    echo " error in METAMORPHISM subroutine in newalbedo.f"
    echo " some fraction is either larger than 1 or smaller than 0"
   ;;
   43)
    echo " error in METAMORPHISM subroutine in newalbedo.f"
    echo " calculated snow grain size is smaller than set fresh snow grain size"
   ;;
   50)
    echo " error in INITTABLES subroutine in inittables.f"
    echo " SSAfresh is not 60, 80 or 100, so there is no valid look-up table"
   ;;
   60)
    echo " error in solvequadpos function in routines.f"
    echo " second order polynomial has no real root"
   ;;
   61)
    echo " error in solvequadneg function in routines.f"
    echo " second order polynomial has no real root"
   ;;
   70)
    echo " error in falsepos function in skintemperature.f"
    echo " false position method does not converge fast enough"
   ;;
  esac
  exit 1
 else
  echo " model run finished " $finished
 fi
fi
#
echo " script run finished "
#
