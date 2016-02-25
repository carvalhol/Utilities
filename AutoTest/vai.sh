#!/bin/bash

(cd /home/carvalhol/Projects/Utilities/AutoTest/build; make all)
#qsub run.pbs
./build/autoTest.exe 
#qsub all_1.pbs
#qstat -u carvalhol
# INSTRUCIONS TO CREATE A BASH FILE
# create a file like this "FILETEST.sh"
# change the mode of the file, so you can execute this: "chmod u+x FILETEST.sh"
# it can be useful to make things you do every day automatically by executing this file
# more about it on http://tldp.org/LDP/Bash-Beginners-Guide/html/
