#!/bin/bash

if [ $# -ne 1 ]; then
	echo "Usage ./run_asm.sh <proc name>"
	exit
fi

simdut=${1}_dut

asm_tests=(
	simple
	add addi
	and andi
	auipc
	beq bge bgeu blt bltu bne
	j jal jalr
	lb lbu lh lhu lw
	lui
	or ori
	sb sh sw
	sll slli
	slt slti sltiu
	sra srai
	srl srli
	sub
	xor xori
	bpred_bht bpred_j bpred_j_noloop bpred_ras
	cache conflict
	)

vmh_dir=../programs/build/assembly/vmh
log_dir=logs
wait_time=3

# create bsim log dir
mkdir -p ${log_dir}

# kill previous bsim if any
pkill bluetcl

# run each test
for test_name in ${asm_tests[@]}; do
	echo "-- assembly test: ${test_name} --"
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
