#!/bin/sh
#
# simple stress tester for Mefisto using WGET
#
# TARGET=192.168.1.74:9093
TARGET=econflemosi1.econ.kuleuven.ac.be
# local version is 192.168.1.74:9093
NUM_SESSIONS=15
DELAY=2s # wait this long between each batch of NUM_SESSIONS requests; 's' = seconds and is needed, I think
ACTION=run # save or run

rm -f loadtest/index.html.*
#
# start some sessions
#
for i in `seq 1 $NUM_SESSIONS`;
do
        rm -f loadtest/cookies_file_$i
        wget $TARGET/mefisto/ --save-cookies loadtest/cookies_file_$i --keep-session-cookies 
done

repeats=0
#
# n quickie job submissions every DELAY seconds
#
while [ 1 ]
do
        for i in `seq 1 $NUM_SESSIONS`;
        do 
                echo "session #$i"
                # this either runs or saves the mefisto light page for each of the sessions
                wget \
			$TARGET/mefisto/light_page/ \
			-P loadtest/ \
			--save-cookies loadtest/cookies_file_$i \
			--load-cookies loadtest/cookies_file_$i  \
			--keep-session-cookies \
		       	--post-data 'which_page=light_page&random_string=1&which_package=flat_tax&base_rate=v20&exemption=v12000&submit_action='$ACTION
        done        
        let "repeats+=1"
        echo " ============ $repeats iterations done ======== "
        # rm -f index.html.*
        sleep $DELAY
done
