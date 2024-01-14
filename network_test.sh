#!/bin/bash

DATA_FOLDER="./HomeNetworkAnalysis/Data/"
FILE_NAME=$(date '+%Y-%m-%d')_$1
end=$((SECONDS+10))
GREEN='\033[0;32m'
OR='\033[0;33m'
DATECOL='\033[0;34m'
NC='\033[0m' 

clear
trap "chmod a+w $DATA_FOLDER$FILE_NAME.txt" INT

if [ -f "$DATA_FOLDER$FILE_NAME.txt" ]; then
    chmod a-w $DATA_FOLDER$FILE_NAME.txt
fi
while [ $SECONDS -lt $end ]; do
	for i in $(seq 1 2); do
		printf '\n\nTEST: PING\nTIME: ' >> $DATA_FOLDER/$FILE_NAME.txt
		date +%s >> $DATA_FOLDER/$FILE_NAME.txt
		printf "${DATECOL}$(date '+%Y-%m-%d %H:%M:%S')${NC}   testing ping...."
		ping -q -f -c 10 1.1.1.1 >> $DATA_FOLDER/$FILE_NAME.txt
		printf "${GREEN}ping complete${NC}\n"
		printf "${DATECOL}$(date '+%Y-%m-%d %H:%M:%S')${NC}   ${OR}paused${NC}\n"
		sleep 5
	done
	echo -e '\n\nTEST: BANDWIDTH' >> $DATA_FOLDER/$FILE_NAME.txt
	printf "${DATECOL}$(date '+%Y-%m-%d %H:%M:%S')${NC}   testing upload and download....\n"
	speedtest-cli --simple >> $DATA_FOLDER/$FILE_NAME.txt
	printf "${DATECOL}$(date '+%Y-%m-%d %H:%M:%S')${NC}   ${GREEN}speedtest complete${NC}"
done
echo -e "\n${GREEN}results saved to $DATA_FOLDER$FILE_NAME.txt"
chmod a+w $DATA_FOLDER$FILE_NAME.txt


