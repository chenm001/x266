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

  /* text: test code section */
  . = 0x100;
  .text 0x100 : ONLY_IF_RO
  {
    crt.o(.text)
    *(.text*)
  } > rom

  /* data segment */
  .data : ONLY_IF_RW
  {
    *(.data)
  } > ram

  .sdata : ONLY_IF_RW
  {
    _gp = . + 0x800;
    *(.srodata.cst16) *(.srodata.cst8) *(.srodata.cst4) *(.srodata.cst2) *(.srodata*)
    *(.sdata .sdata.* .gnu.linkonce.s.*)
  } > ram

  /* bss segment */
  .sbss : ONLY_IF_RW
  {
    *(.sbss .sbss.* .gnu.linkonce.sb.*)
    *(.scommon)
  } > ram
  .bss : { *(.bss) }

  /* thread-local data segment */
  .tdata : ONLY_IF_RW
  {
    _tls_data = .;
    crt.o(.tdata.begin)
    *(.tdata)
    crt.o(.tdata.end)
  } > ram

  .tbss : ONLY_IF_RW
  {
    *(.tbss)
    crt.o(.tbss.end)
  } > ram

  /* End of uninitalized data segement */
  _end = .;
}

