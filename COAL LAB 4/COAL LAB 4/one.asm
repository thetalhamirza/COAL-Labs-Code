COMMENT @
Declare a signed byte variable with value -50 and an unsigned byte variable with value 200.
 Use movzx to load the unsigned value into a 32-bit register.
 Use movsx to load the signed value into another 32-bit register.
 Display both results with DumpRegs.
@

INCLUDE Irvine32.inc

.data

	num1 WORD 1234h
	num2 WORD 5678h


.code
main PROC
	
	mov ax, num1
	mov bx, num2

	call WriteHex
	call crlf

	xchg ax, bx

	call WriteHex



exit
main ENDP
END main