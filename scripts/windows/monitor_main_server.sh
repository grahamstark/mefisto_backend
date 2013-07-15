#
# Tests for main server already running and if not starts it
#
# Needs cygwin with (and?) wget
#
#
TARGET=econflemosi1.econ.kuleuven.ac.be
# local version is 192.168.1.74:9093
cd $MEFISTO_BACKEND
#
wget \
        $TARGET/mefisto/light_page/ \
        -P loadtest/ \
        --post-data 'which_page=light_page&random_string=1&which_package=flat_tax&base_rate=v20&exemption=v12000&submit_action='$ACTION

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
        cygrunsrv.exe -E mefisto_server
        # start
        cygrunsrv.exe -S mefisto_server
        # query
        cygrunsrv.exe -Q mefisto_server
fi
