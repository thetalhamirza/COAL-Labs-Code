INCLUDE Irvine32.inc

.data
marks BYTE 8 DUP(5)
words WORD 5 DUP(?)


.code
main PROC

mov eax, 30000h
add eax, 60000h
sub eax, 10000h
call WriteInt

exit
MAIN endp
end MAIN
