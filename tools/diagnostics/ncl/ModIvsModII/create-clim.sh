#!/bin/bash

# Script for creating climatological monthly data for use in the ModIvsModII diagnostics package

FIRSTYEAR=2005
LASTYEAR=2014

AVERAGEDYEARS="yr$FIRSTYEAR-$LASTYEAR"
echo $AVERAGEDYEARS

#echo "$FIRSTYEAR"
#echo "$FIRSTYEAR + 1"
#echo "$((FIRSTYEAR + 1))"

#AVAILABLEMONTHS=(1)
AVAILABLEMONTHS=(1 2 3 4 5 6 7 8 9 10 11 12)

#INPUTDIRECTORY=/projects/NS9560K/noresm/cases/NHIST_spAer_f19_tn14_20190925/atm/hist/
#INPUTDIRECTORY=/projects/NS2345K/noresm/cases/NFHISTnorpddmsbcsdyn_f09_mg17_20191024/
#INPUTDIRECTORY=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/NHISTfrc2_f09_tn14_20191025/nc4/
#INPUTDIRECTORY=/projects/NS9560K/noresm/cases/NFHISTnorpddmsbc_f19_mg17_20190807/atm/hist/
#INPUTDIRECTORY=/projects/NS9560K/noresm/cases/NFHISTnorpddmsbc_f19_mg17_20191025/atm/hist/
#INPUTDIRECTORY=/projects/NS2345K/noresm/cases/piNFHISTnorpddmsbcsdyn_f09_mg17_20191120/atm/hist/
#INPUTDIRECTORY=/projects/NS2345K/noresm/cases/NFHISTnorpddmsbcsdyn_f09_mg17_20191101/atm/hist/
#INPUTDIRECTORY=/projects/NS9560K/noresm/cases/NHIST_f19_tn14_20190710/atm/hist/
#INPUTDIRECTORY=/projects/NS9560K/noresm/cases/NHIST_02_f19_tn14_20190813/atm/hist/
INPUTDIRECTORY=/projects/NS9560K/noresm/cases/NHIST_03_f19_tn14_20190813/atm/hist/

#EXPERIMENTNAME=NHIST_spAer_f19_tn14_20190925
#EXPERIMENTNAME=NFHISTnorpddmsbcsdyn_f09_mg17_20191024
#EXPERIMENTNAME=NHISTfrc2_f09_tn14_20191025
#EXPERIMENTNAME=NFHISTnorpddmsbc_f19_mg17_20190807
#EXPERIMENTNAME=NFHISTnorpddmsbc_f19_mg17_20191025
#EXPERIMENTNAME=piNFHISTnorpddmsbcsdyn_f09_mg17_20191120
#EXPERIMENTNAME=NFHISTnorpddmsbcsdyn_f09_mg17_20191101
#EXPERIMENTNAME=NHIST_f19_tn14_20190710
#EXPERIMENTNAME=NHIST_02_f19_tn14_20190813
EXPERIMENTNAME=NHIST_03_f19_tn14_20190813

#can chose final directory below, or just cp there later after checking the results:
OUTPUTDIRECTORY=/projects/NS2345K/noresm/cases/divClim4aerosoldiag/$EXPERIMENTNAME/$AVERAGEDYEARS


# *************** no need to edit below this line **************************** 

#Create output directory if it does not exist
if [ ! -d "$OUTPUTDIRECTORY" ];then
	echo CREATING OUTPUT DIR $OUTPUTDIRECTORY
	mkdir -p $OUTPUTDIRECTORY
fi

#For each month ==> do an ensemble average of the variables in question
for aMonth in ${AVAILABLEMONTHS[@]};do
	fileList=""

  	aaMonth=$aMonth
	if [ $aMonth -lt 10 ]; then
	   aaMonth=0$aMonth
        fi              
	
	aYear=$((FIRSTYEAR))
        aaYear=$aYear

	while [ $aYear -le $((LASTYEAR)) ]; do
	        echo $aYear $aaMonth

		if [ $aYear -lt 1000 ]; then
		    aaYear=0$aYear
		    #echo aaYear is $aaYear
                fi
		if [ $aYear -lt 100 ]; then
		    aaYear=00$aYear
		    #echo aaYear is $aaYear
                fi
		if [ $aYear -lt 10 ]; then
		    aaYear=000$aYear
		    #echo aaYear is $aaYear
                fi
		     
#		fileList+="$INPUTDIRECTORY/$EXPERIMENTNAME.cam.h0.$aaYear-$aaMonth.nc"
		fileList+="$INPUTDIRECTORY/$EXPERIMENTNAME.cam.h0.$aYear-$aaMonth.nc"
		fileList+=" "
		#echo $fileList

               aYear=$((aYear + 1))
	       echo aYear is $aYear
	done

#	echo Will perform ncea for month $aaMonth $fileList $OUTPUTDIRECTORY/ENSEMBLE_AVG_$AVERAGEDYEARS_${EXPERIMENTNAME}_$aaMonth.nc
#	ncea -O $fileList $OUTPUTDIRECTORY/ENSEMBLE_AVG_${AVERAGEDYEARS}_${EXPERIMENTNAME}_$aaMonth.nc
#	echo Will perform ncea for month $aaMonth $fileList $OUTPUTDIRECTORY/${EXPERIMENTNAME}.cam.h0.$aaYear-$aaMonth.nc
#	ncea -O $fileList $OUTPUTDIRECTORY/${EXPERIMENTNAME}.cam.h0.nc4.$aaYear-$aaMonth.nc
	echo Will perform ncea for month $aaMonth $fileList $OUTPUTDIRECTORY/${EXPERIMENTNAME}.cam.h0.$FIRSTYEAR-$LASTYEAR-$aaMonth.nc
	ncea -O $fileList $OUTPUTDIRECTORY/${EXPERIMENTNAME}.cam.h0.nc4.$FIRSTYEAR-$LASTYEAR-$aaMonth.nc
	#echo $fileList

        echo Decompress the file to increase the speed of further post-processing 	
#        ncks -6 $OUTPUTDIRECTORY/ENSEMBLE_AVG_${AVERAGEDYEARS}_${EXPERIMENTNAME}_$aaMonth.nc $OUTPUTDIRECTORY/ENSEMBLE_AVGnc3_${AVERAGEDYEARS}_${EXPERIMENTNAME}_$aaMonth.nc
#        ncks -6 $OUTPUTDIRECTORY/${EXPERIMENTNAME}.cam.h0.nc4.$aaYear-$aaMonth.nc $OUTPUTDIRECTORY/${EXPERIMENTNAME}.cam.h0.$aaYear-$aaMonth.nc
        ncks -6 $OUTPUTDIRECTORY/${EXPERIMENTNAME}.cam.h0.nc4.$FIRSTYEAR-$LASTYEAR-$aaMonth.nc $OUTPUTDIRECTORY/${EXPERIMENTNAME}.cam.h0.$FIRSTYEAR-$LASTYEAR-$aaMonth.nc

done


#Clean up and make sure that anyone can read the output files
#rm $OUTPUTDIRECTORY/ENSEMBLE_AVG_${AVERAGEDYEARS}_${EXPERIMENTNAME}_??.nc
rm $OUTPUTDIRECTORY/${EXPERIMENTNAME}.cam.h0.nc4.*.nc
chmod -R a+r $OUTPUTDIRECTORY

exit
