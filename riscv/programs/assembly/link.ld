/*======================================================================*/
/* Proxy kernel linker script                                           */
/*======================================================================*/
/* This is the linker script used when building the proxy kernel. */

/*----------------------------------------------------------------------*/
/* Setup                                                                */
/*----------------------------------------------------------------------*/

/* The OUTPUT_ARCH command specifies the machine architecture where the
   argument is one of the names used in the BFD library. More
   specifically one of the entires in bfd/cpu-mips.c */
OUTPUT_ARCH( "riscv" )

/*----------------------------------------------------------------------*/
/* Sections                                                             */
/*----------------------------------------------------------------------*/

MEMORY
{
    rom (RX) : ORIGIN = 0x00000, LENGTH = 0x10000
    ram (RW) : ORIGIN = 0x10000, LENGTH = 0x20000
}

SECTIONS
{
  . = 0x200;
  .text 0x200 : ONLY_IF_RO
  {
    *(.text)
  } > rom

  . = 0x10000;
  .data ALIGN(0x1000) : ONLY_IF_RW
  {
    *(.data*)
  } > ram

  .bss : ONLY_IF_RW
  {
    *(.bss)
  } > ram
  _end = .;
}

