OUTPUT_ARCH( "riscv" )

SECTIONS
{
  . = 0x200;
  .text : { *(.text) }
  .data ALIGN(0x1000) : { *(.data) }
  .bss : { *(.bss) }
  _end = .;
}

