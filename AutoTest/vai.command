#!/bin/bash

NP=1

(cd /Users/carvalhol/Desktop/GITs/Utilities/AutoTest/build; make all)
rm -r genTests
echo ""
echo "---------------------------------"
echo ""
# (cd /Users/carvalhol/Desktop/GITs/Utilities/AutoTest/genTest; rm -r WEAK)
#sleep 1

mpirun --allow-run-as-root -np $NP /Users/carvalhol/Desktop/GITs/Utilities/AutoTest/build/autoTest.exe
ls

# INSTRUCIONS TO CREATE A BASH FILE
# create a file like this "FILETEST.sh"
# change the mode of the file, so you can execute this: "chmod u+x FILETEST.sh"
# it can be useful to make things you do every day automatically by executing this file
# more about it on http://tldp.org/LDP/Bash-Beginners-Guide/html/
