NUM_SERVERS=15
PORT=20000
#
# stop some sessions
#
for i in `seq 1 $NUM_SERVERS`;
do
        let "PORT+=1"
        # stop
        cygrunsrv.exe -E mini_$PORT   
done
