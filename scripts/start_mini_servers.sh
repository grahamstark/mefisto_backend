#!/bin/bash
NUM_SERVERS=15
PORT=20000
#
# start some sessions
#
for i in `seq 1 $NUM_SERVERS`;
do
        PORT=$((PORT+1))
        echo "starting on PORT $PORT"
        bin/mini_server etc/be_settings.txt etc/web_settings.txt $PORT &> log/mini_server_$PORT.log &       
done
