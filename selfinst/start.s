.globl _start
_start:
  movl $0x1, %eax
  movl %esp, %eax
  movl (%esp), %ebx
  addl $4, %eax
  pushl %eax
  pushl %ebx
  call main
  movl %eax, %ebx
  movl $1, %eax
  syscall
