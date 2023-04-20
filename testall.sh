#!/bin/bash

microcc="../_build/install/default/bin/microcc"
link="llvm-link"
rt="rt-support"
emitllvm="clang -emit-llvm -c"
llc="llc -filetype=obj"
clang="clang -w"
testpath="../test/samples/"
lenpath=${#testpath}
output="output.log"

rm -r testall
mkdir testall && cd testall
$emitllvm ../bin/$rt.c -o $rt.bc

for f in ${testpath}test*.mc;
do
	name=${f:$lenpath}
	name=${name%%.*}
	echo "Running $name..."
	echo $name >> $output
	$microcc $f -o $name.bc						&&	# compile to bitcode
	$clang $rt.bc $name.bc -o $name.o 					# link with rt-s and build the executable
	./$name.o >> $output
	echo "expected" $(cat ${testpath}$name.out) >> $output
done
