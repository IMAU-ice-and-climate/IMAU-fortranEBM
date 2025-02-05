#!/bin/bash

#Check if something is happening
lockdir=/tmp/montecarlo.lock
while true; do
 if mkdir "$lockdir" 2>/dev/null; then #If dir can be created, it is the 'lockfile', remove when done (trap)
  trap 'rm -rf "$lockdir"' 0
  break
 else
  sleep 1 #Wait for 1 sec and try again (lockdir)
 fi
done

#Read counter and create new entry
i=$(cat ../counter_neumayer.txt)
echo "run$i" > MCrun_meltonly.txt
awk 'BEGIN{sum=0}(NR>1 && $49>=0){sum+=$49}(NR>1){print sum}' MCrun.txt >> MCrun_meltonly.txt

#Combine new entry with existing entry
mv ../MCrun_master_neumayer.txt MCrun_all.txt
paste MCrun_all.txt MCrun_meltonly.txt > MCrun_new.txt
mv MCrun_new.txt MCrun_all.txt
rm MCrun_meltonly.txt MCrun.txt
mv MCrun_all.txt ../MCrun_master_neumayer.txt

#Increase counter in file
echo $((i+1)) > ../counter_neumayer.txt
