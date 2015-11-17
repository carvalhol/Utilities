#!/bin/bash

clear


for i in {1..1}
do
   echo "Running $i"
cd ./2D/SHI-l/001                                                                                                                                                                                       
./run.command
cd ../../../
cd ./2D/SHI-l/002                                                                                                                                                                                       
./run.command
cd ../../../
cd ./2D/FFT-l/001                                                                                                                                                                                       
./run.command
cd ../../../
cd ./2D/FFT-l/002                                                                                                                                                                                       
./run.command
cd ../../../
sleep 1
done

