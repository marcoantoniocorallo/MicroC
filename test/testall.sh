#!/bin/bash

microcc="../_build/install/default/bin/microcc"
link="llvm-link"
rt="../_build/install/default/bin/rt-support.bc"
llc="llc -filetype=obj"
clang="clang -w"
testpath="../test/samples/"
lenpath=${#testpath}
output="output.log"

rm -r testall
mkdir testall && cd testall

for f in ${testpath}test*.mc;
do
	name=${f:$lenpath}
	name=${name%%.*}
	echo "Running $name..."
	echo $name >> $output
	$microcc $f -o $name.bc					&&	# compile to bitcode
	$clang $rt $name.bc -o $name.o 					# link with rt-s and build the executable
	./$name.o >> $output
	echo "expected" $(cat ${testpath}$name.out) >> $output
done
