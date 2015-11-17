#!/bin/bash
NP=2                
(cd /Users/carvalhol/Desktop/GITs/RANDOM_FIELD/build; make all)
echo ""
echo "---------------------------------"
echo ""
rm  log*
rm  fort.*
rm  -r results
mpirun --allow-run-as-root -np $NP /Users/carvalhol/Desktop/GITs/RANDOM_FIELD/build/randomField.exe
#mpirun --allow-run-as-root -np $NP /Users/carvalhol/Desktop/GITs/RANDOM_FIELD/build/statistics.exe
 
ls
