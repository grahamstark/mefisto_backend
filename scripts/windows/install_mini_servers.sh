NUM_SERVERS=15
PORT=20000
#
# Windows Mini-Server farm install.
#
# Install NUM_SERVERS servers with names mini_[PORT_NUMBER] 
# on ports 20001 .. 20000 + NUM_SERVERS
# and start them.
# uses Cygwin's cygrunsrv utility. Probably needs to be run with Admin privileges (CHECK THIS).
#
# TODO - find some way to get this to pick up directories from environment variables properly. 
#      - privileges check
#      - ?? NUM_SERVERS and PORT as ARGS?? 
for i in `seq 1 $NUM_SERVERS`;
do
        let "PORT+=1"
        #remove anthing there..
        cygrunsrv.exe -R mini_$PORT
        # install
        cygrunsrv.exe \
                --install mini_$PORT \
                --path c:/VirtualWorlds/projects/mefisto/backend/ada/bin/mini_server.exe \
                --chdir c:/VirtualWorlds/projects/mefisto/backend/ada/ \
                --args "etc/be_settings_windows.txt etc/web_settings_windows.txt $PORT"
        # start        
        cygrunsrv.exe -S mini_$PORT
         # query status
        cygrunsrv.exe -Q mini_$PORT -V 
        # stop
        # cygrunsrv.exe -E mini_$PORT   
done
