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
	sad
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
		echo "ERROR: $mem_file does not exist, you need to first compile"
		exit
	fi
	cp ${mem_file} mem.vmh
	for ((idx = 0; idx < 8; idx++)); do
	    cp ${mem_file}.D${idx} mem.vmh.D${idx}
	done

	# run test
	./${simdut} | ./buffer.py 64K > ${log_dir}/${test_name}.log
	echo ""
done
