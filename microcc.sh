#!/bin/bash

microcc="./_build/install/default/bin/microcc"
rt="./_build/install/default/bin/rt-support.bc"
clang="clang -w"
err="A microc file is expected as argument"

if [[ $# -ge 1 ]] ; then 
	fname=$1
	if [[ ${fname:(-3):(3)}==".mc" ]] ; then
		name=${fname%%.*}
		echo "Compiling $name..."
		$microcc $fname -o $name.bc				&&	# compile to bitcode
		$clang $rt $name.bc -o $name.o 					# link with rt-s and build the executable
		echo "Compiled: " $name.o
	else
		echo $err
	fi	
else
	echo $err
fi
