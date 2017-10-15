#!/bin/bash

if [ $# -ne 1 ]; then
	echo "Usage ./cp_asm.sh <asm name>"
	exit
fi

simdut=${1}_dut

test_name=${1}

vmh_dir=../programs/build/benchmarks/vmh

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
