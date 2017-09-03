#!/bin/bash

if [ $# -ne 1 ]; then
	echo "Usage ./run_bmarks.sh <proc name>"
	exit
fi

simdut=${1}_dut

bmarks_tests=(
	median
	multiply
	qsort
	towers
	vvadd
	rsort
	spmv
	)

vmh_dir=../programs/build/benchmarks/vmh
log_dir=logs
wait_time=3

# create bsim log dir
mkdir -p ${log_dir}

# kill previous bsim if any
pkill bluetcl

# run each test
for test_name in ${bmarks_tests[@]}; do
	echo "-- benchmark test: ${test_name} --"
	# copy vmh file
	mem_file=${vmh_dir}/${test_name}.riscv.vmh
	if [ ! -f $mem_file ]; then
		echo "ERROR: $mem_file does not exit, you need to first compile"
		exit
	fi
	cp ${mem_file} mem.vmh
	cp ${mem_file}.D mem.vmh.D

	# run test
	./${simdut} > ${log_dir}/${test_name}.log
	echo ""
done
