#!/bin/bash

clear


for i in {1..1}
do
   echo "Running $i"
cd ./2D/SHI-g/001                                                                                                                                                                                       
./run.command
cd ../../../
cd ./2D/SHI-g/002                                                                                                                                                                                       
./run.command
cd ../../../
cd ./2D/FFT-g/001                                                                                                                                                                                       
./run.command
cd ../../../
cd ./2D/FFT-g/002                                                                                                                                                                                       
./run.command
cd ../../../
sleep 1
done

