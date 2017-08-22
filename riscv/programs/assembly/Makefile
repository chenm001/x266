#=======================================================================
# Makefile for riscv-tests/isa
#-----------------------------------------------------------------------

asmdir := $(CURDIR)

build_dir := $(asmdir)/../build/assembly
bin_dir := $(build_dir)/bin
dump_dir := $(build_dir)/dump
vmh_dir := $(build_dir)/vmh

default: all

#--------------------------------------------------------------------
# Sources
#--------------------------------------------------------------------
rv32ui_tests = \
	simple \
	add addi \
	and andi \
	auipc \
	beq bge bgeu blt bltu bne \
	j jal jalr \
	lw \
	lui \
	or ori \
	sw \
	sll slli \
	slt slti \
	sra srai \
	srl srli \
	sub \
	xor xori \
	bpred_bht bpred_j bpred_ras bpred_j_noloop \
	cache \

#--------------------------------------------------------------------
# Build rules
#--------------------------------------------------------------------

RISCV_TOOL_DIR := /opt/rh/riscv-tools/bin
RISCV_PREFIX := $(RISCV_TOOL_DIR)/riscv64-unknown-elf
# gcc
RISCV_GCC := $(RISCV_PREFIX)-gcc
RISCV_GCC_OPTS := -static -fvisibility=hidden -nostdlib -nostartfiles -Wa,-march=rv32im -march=rv32im -mabi=ilp32
incs := -I$(asmdir)/../env -I$(asmdir)/macros
# link
RISCV_LINK_OPTS := -T$(asmdir)/link.ld
# objdump
RISCV_OBJDUMP := $(RISCV_PREFIX)-objdump --disassemble --disassemble-zeroes --section=.text --section=.text.startup --section=.data
# convert to VMH: 64KB memory
RISCV_ELF2HEX := $(RISCV_TOOL_DIR)/elf2hex
VMH_WIDTH := 4
VMH_DEPTH := 32768

#------------------------------------------------------------
# Build assembly tests

rv32ui_tests_bin  := $(patsubst %,$(bin_dir)/%.riscv, $(rv32ui_tests))
rv32ui_tests_dump := $(patsubst %,$(dump_dir)/%.riscv.dump, $(rv32ui_tests))
rv32ui_tests_vmh  := $(patsubst %,$(vmh_dir)/%.riscv.vmh, $(rv32ui_tests))

$(rv32ui_tests_vmh): $(vmh_dir)/%.riscv.vmh: $(bin_dir)/%.riscv
	@echo "@0" > $@
	$(RISCV_ELF2HEX) $(VMH_WIDTH) $(VMH_DEPTH) $< >> $@

$(rv32ui_tests_dump): $(dump_dir)/%.riscv.dump: $(bin_dir)/%.riscv
	$(RISCV_OBJDUMP) $< > $@

$(rv32ui_tests_bin): $(bin_dir)/%.riscv: $(asmdir)/src/%.S
	$(RISCV_GCC) $(RISCV_GCC_OPTS) $(incs) $(RISCV_LINK_OPTS) $< -o $@

#------------------------------------------------------------
# Default
$(bin_dir):
	mkdir -p $@

$(obj_dir):
	mkdir -p $@

$(dump_dir):
	mkdir -p $@

$(vmh_dir):
	mkdir -p $@


all: $(bin_dir) $(obj_dir) $(dump_dir) $(vmh_dir) $(rv32ui_tests_dump) $(rv32ui_tests_vmh)


#------------------------------------------------------------
# Clean up

clean:
	rm -rf $(build_dir)


.PHONY: all clean


