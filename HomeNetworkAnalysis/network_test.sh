#!/bin/bash

DATA_FOLDER=./Data/


TIME=$1*3600 #Time in hours
FILE_NAME=$(date '+%Y-%m-%d')_$2
DESTINATION=$3 #1.1.1.1, 8.8.8.8, or another IP

end=$((SECONDS+TIME))
GREEN='\033[0;32m'
OR='\033[0;33m'
DATECOL='\033[0;34m'
NC='\033[0m' 

clear
trap "chmod a+w $DATA_FOLDER$FILE_NAME.txt" INT

if [ -f "$DATA_FOLDER$FILE_NAME.txt" ]; then
    chmod a-w $DATA_FOLDER$FILE_NAME.txt
fi

#Get user inputs for test parameters
printf "Test reference number: "
read REF
printf "ISP: "
read ISP
printf "LAN Hardware: "
read HARDWARE
printf "MODEM TYPE (ISP_GATEWAY or OWNED): "
read MODEM
printf "MTU setting: "
read MTU
printf "Low temperature today: "
read TEMP_LOW
printf "High temperature today: "
read TEMP_HIGH

printf "REF: $REF" >> $DATA_FOLDER/$FILE_NAME.txt
printf "\nISP: $ISP" >> $DATA_FOLDER/$FILE_NAME.txt
printf "\nLAN_HARDWARE: $HARDWARE" >> $DATA_FOLDER/$FILE_NAME.txt
printf "\nDESTINATION: $DESTINATION" >> $DATA_FOLDER/$FILE_NAME.txt
printf "\nMODEM: $MODEM" >> $DATA_FOLDER/$FILE_NAME.txt
printf "\nMTU: $MTU" >> $DATA_FOLDER/$FILE_NAME.txt
printf "\nTEMP_LOW: $TEMP_LOW" >> $DATA_FOLDER/$FILE_NAME.txt
printf "\nTEMP_HIGH: $TEMP_HIGH" >> $DATA_FOLDER/$FILE_NAME.txt


while [ $SECONDS -lt $end ]; do
	for i in $(seq 1 10); do
		printf '\n\nTEST: PING\nTIME: ' >> $DATA_FOLDER/$FILE_NAME.txt
		date +%s >> $DATA_FOLDER/$FILE_NAME.txt
		printf "${DATECOL}$(date '+%Y-%m-%d %H:%M:%S')${NC}   testing ping...."
		ping -q -i .1 -c 400 $DESTINATION >> $DATA_FOLDER/$FILE_NAME.txt && printf "${GREEN}ping complete${NC}\n"
		sleep 20
	done
	echo -e '\n\nTEST: BANDWIDTH' >> $DATA_FOLDER/$FILE_NAME.txt
	printf "${DATECOL}$(date '+%Y-%m-%d %H:%M:%S')${NC}   testing upload and download...."
	speedtest-cli --simple >> $DATA_FOLDER/$FILE_NAME.txt && printf "${GREEN}speedtest complete${NC}\n"
done
echo -e "\n${GREEN}results saved to $DATA_FOLDER$FILE_NAME.txt"
chmod a+w $DATA_FOLDER$FILE_NAME.txt


