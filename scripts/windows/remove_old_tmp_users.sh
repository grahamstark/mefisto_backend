#
# remove 2 day old tmp users
#
/bin/find /cygdrive/c/VirtualWorlds/projects/mefisto/backend/ada/working/users/ -name tmp_* -type d -mmin +720 -exec /bin/rm -rf {} \; 
#
# 
