INCLUDE Irvine32.inc
.code
main PROC
	
	mov edx, OFFSET msg
	call WriteString
	
exit
main ENDP

.data
msg BYTE "Assalam u Alaikum Habibi!",0

END main