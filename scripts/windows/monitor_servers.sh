#
# Start a bunch of mini-servers on ports PORT+1 .. PORT[num_servers]. 
# Tests for server already running on each port, and if not starts one.
#
# Needs cygwin with (and?) wget
#
#
cd $MEFISTO_BACKEND
NUM_SERVERS=15
PORT=20000
RESTARTS=0
#
# loop round ports (e.g.) 20001 .. 20015 ... 
#
for i in `seq 1 $NUM_SERVERS`;
do
        let "PORT+=1"
        #
        # start wget rquest to the miniserver on the port, 
        # with 5 retries, 2 second connections, server response printed, writing to monitor.txt
        #
        wget -t 5 -T 2 -S -O monitor_file.txt http://localhost:$PORT/alive/
        rc=$?
        echo "rc for port $PORT |$rc|"
        #
        # wget returns 4 on network failure, 0 on success (see: http://www.gnu.org/software/wget/manual/html_node/Exit-Status.html)
        # , so...
        #
        if [ $rc -gt 0 ];
        then
                #
                # We write a PID into scripts/miniserve_2001 (etc). Kill that pid first..
                #
                let "RESTARTS+=1"
                echo "restarting $PORT"
                # stop
                cygrunsrv.exe -E mini_$PORT
                # start
                cygrunsrv.exe -S mini_$PORT
                # query
                cygrunsrv.exe -Q mini_$PORT -V
        fi
done

echo "$RESTARTS servers were restarted"
