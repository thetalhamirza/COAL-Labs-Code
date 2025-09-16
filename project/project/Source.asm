; Benchmarks.asm
; Clean and corrected MASM (Irvine32) CPU benchmark tool
;
; Assemble:
;   ml /c /coff Benchmarks.asm
; Link:
;   link /subsystem:console Benchmarks.obj Irvine32.lib user32.lib kernel32.lib

.386
.model flat, stdcall
.stack 4096

INCLUDE Irvine32.inc
INCLUDELIB Irvine32.lib
INCLUDELIB user32.lib
INCLUDELIB kernel32.lib

; We declare extern for GetAsyncKeyState and ExitProcess
EXTERN GetAsyncKeyState:PROC
EXTERN ExitProcess:PROC

; -----------------------
; Constants (define at assembly-time)
; -----------------------
TOTAL_ITER  = 10000000       ; 10 million
CHUNK_ITER  = 1000000        ; check every 1 million
CHUNKS      = TOTAL_ITER / CHUNK_ITER ; 10 chunks

ARR_SIZE    = 4096           ; buffer size for memory benchmark (power of two)

VK_Q        = 0x51           ; 'Q' virtual-key

.data
; Messages
headerMsg    BYTE "CPU Performance Benchmark Tool",0Dh,0Ah,0
menuMsg      BYTE "Select benchmark to run (1-4), A=All, Q=Quit: ",0
menu1        BYTE "1 - Addition Loop (10,000,000)",0Dh,0Ah,0
menu2        BYTE "2 - Multiplication Loop (10,000,000)",0Dh,0Ah,0
menu3        BYTE "3 - Loop vs DEC/JNZ Comparison",0Dh,0Ah,0
menu4        BYTE "4 - Memory Read/Write Benchmark",0Dh,0Ah,0
menuA        BYTE "A - Run All sequentially",0Dh,0Ah,0
menuQ        BYTE "Q - Quit",0Dh,0Ah,0

runningAdd   BYTE "Running: Addition Loop",0Dh,0Ah,0
runningMul   BYTE "Running: Multiplication Loop",0Dh,0Ah,0
runningLoop  BYTE "Running: Loop vs DEC/JNZ Comparison",0Dh,0Ah,0
runningMem   BYTE "Running: Memory Read/Write Benchmark",0Dh,0Ah,0

elapsedMsg   BYTE "Elapsed time (ms): ",0
cyclesMsg    BYTE "RDTSC delta (high:low): ",0
abortedMsg   BYTE "*** Benchmark aborted by user (Q) ***",0Dh,0Ah,0
doneMsg      BYTE "Benchmark complete.",0Dh,0Ah,0
pressAny     BYTE "Press any key to continue...",0

; Timing vars
startMS      DWORD 0
endMS        DWORD 0
elapsedMS    DWORD 0

; rdtsc vars
startTSClo   DWORD 0
startTSChigh DWORD 0
endTSClo     DWORD 0
endTSChigh   DWORD 0
deltaLow     DWORD 0
deltaHigh    DWORD 0

; Buffer for memory test
buffer       DWORD ARR_SIZE DUP (0)

.code

; -----------------------
; Helper: Check if user pressed Q (non-blocking).
; Returns: ZF=1 if NOT pressed; ZF=0 if pressed.
; Uses GetAsyncKeyState(VK_Q).
; -----------------------
CheckAbort PROC
    push    VK_Q
    call    GetAsyncKeyState
    add     esp, 4
    test    eax, eax
    ret
CheckAbort ENDP

; -----------------------
; Helper: Print elapsed milliseconds (uses elapsedMS)
; -----------------------
PrintElapsed PROC
    mov edx, OFFSET elapsedMsg
    call WriteString
    mov eax, elapsedMS
    call WriteInt
    call Crlf
    ret
PrintElapsed ENDP

; -----------------------
; Compute and print rdtsc delta (64-bit)
; Assumes startTSClo/high and endTSClo/high set.
; Stores delta into deltaHigh:deltaLow and prints them.
; -----------------------
PrintRDTSCDelta PROC
    ; compute low part
    mov eax, endTSClo
    sub eax, startTSClo
    mov deltaLow, eax
    ; compute high part with borrow
    mov eax, endTSChigh
    sbb eax, startTSChigh
    mov deltaHigh, eax

    mov edx, OFFSET cyclesMsg
    call WriteString

    ; print high then low (as unsigned 32-bit values; WriteInt prints signed but OK)
    mov eax, deltaHigh
    call WriteInt
    call WriteString
    mov edx, OFFSET ":"  ; small separator print (Irvine WriteString expects EDX)
    call WriteString
    mov eax, deltaLow
    call WriteInt
    call Crlf
    ret
PrintRDTSCDelta ENDP

; -----------------------
; Addition benchmark
; -----------------------
AdditionBenchmark PROC
    ; header
    mov edx, OFFSET runningAdd
    call WriteString
    call Crlf

    xor eax, eax         ; accumulator

    ; record start ms and rdtsc
    rdtsc
    mov startTSClo, eax
    mov startTSChigh, edx
    call GetMSeconds
    mov startMS, eax

    mov ecx, CHUNKS      ; chunk counter
AddChunkLoop:
    mov ebx, CHUNK_ITER
AddInner:
    add eax, 1
    dec ebx
    jnz AddInner

    ; check abort
    push VK_Q
    call GetAsyncKeyState
    add esp, 4
    test eax, eax
    jnz Add_Abort

    dec ecx
    jnz AddChunkLoop

    ; finished normally
    call GetMSeconds
    mov endMS, eax
    rdtsc
    mov endTSClo, eax
    mov endTSChigh, edx

    mov eax, endMS
    sub eax, startMS
    mov elapsedMS, eax

    call PrintElapsed
    call PrintRDTSCDelta

    ret

Add_Abort:
    mov edx, OFFSET abortedMsg
    call WriteString
    ; record partial times
    call GetMSeconds
    mov endMS, eax
    rdtsc
    mov endTSClo, eax
    mov endTSChigh, edx

    mov eax, endMS
    sub eax, startMS
    mov elapsedMS, eax

    call PrintElapsed
    call PrintRDTSCDelta
    ret
AdditionBenchmark ENDP

; -----------------------
; Multiplication benchmark (SHL)
; -----------------------
MultiplicationBenchmark PROC
    mov edx, OFFSET runningMul
    call WriteString
    call Crlf

    mov eax, 1           ; start non-zero

    ; timings
    rdtsc
    mov startTSClo, eax
    mov startTSChigh, edx
    call GetMSeconds
    mov startMS, eax

    mov ecx, CHUNKS
MulChunkLoop:
    mov ebx, CHUNK_ITER
MulInner:
    shl eax, 1           ; multiply by 2
    dec ebx
    jnz MulInner

    ; check abort
    push VK_Q
    call GetAsyncKeyState
    add esp, 4
    test eax, eax
    jnz Mul_Abort

    dec ecx
    jnz MulChunkLoop

    call GetMSeconds
    mov endMS, eax
    rdtsc
    mov endTSClo, eax
    mov endTSChigh, edx

    mov eax, endMS
    sub eax, startMS
    mov elapsedMS, eax

    call PrintElapsed
    call PrintRDTSCDelta
    ret

Mul_Abort:
    mov edx, OFFSET abortedMsg
    call WriteString
    call GetMSeconds
    mov endMS, eax
    rdtsc
    mov endTSClo, eax
    mov endTSChigh, edx

    mov eax, endMS
    sub eax, startMS
    mov elapsedMS, eax

    call PrintElapsed
    call PrintRDTSCDelta
    ret
MultiplicationBenchmark ENDP

; -----------------------
; Loop vs DEC/JNZ comparison
; Two runs:
;   - LOOP instruction using ECX for inner loop
;   - DEC/JNZ using EBX for inner loop
; -----------------------
LoopCompareBenchmark PROC
    mov edx, OFFSET runningLoop
    call WriteString
    call Crlf

    ; ---------- LOOP version ----------
    mov edx, OFFSET runningLoop
    call WriteString
    mov edx, OFFSET " - LOOP variant",0
    call WriteString
    call Crlf

    xor eax, eax
    rdtsc
    mov startTSClo, eax
    mov startTSChigh, edx
    call GetMSeconds
    mov startMS, eax

    mov esi, CHUNKS
LC_LOOP_chunks:
    mov ecx, CHUNK_ITER
LC_LOOP_inner:
    add eax, 1
    loop LC_LOOP_inner

    ; check abort
    push VK_Q
    call GetAsyncKeyState
    add esp,4
    test eax, eax
    jnz LC_LOOP_ABORT

    dec esi
    jnz LC_LOOP_chunks

    call GetMSeconds
    mov endMS, eax
    rdtsc
    mov endTSClo, eax
    mov endTSChigh, edx
    mov eax, endMS
    sub eax, startMS
    mov elapsedMS, eax

    mov edx, OFFSET "LOOP instruction result: ",0
    call WriteString
    call PrintElapsed
    call PrintRDTSCDelta

    ; ---------- DEC/JNZ version ----------
    mov edx, OFFSET runningLoop
    call WriteString
    mov edx, OFFSET " - DEC/JNZ variant",0
    call WriteString
    call Crlf

    xor eax, eax
    rdtsc
    mov startTSClo, eax
    mov startTSChigh, edx
    call GetMSeconds
    mov startMS, eax

    mov esi, CHUNKS
LC_DEC_chunks:
    mov ebx, CHUNK_ITER
LC_DEC_inner:
    add eax, 1
    dec ebx
    jnz LC_DEC_inner

    ; check abort
    push VK_Q
    call GetAsyncKeyState
    add esp,4
    test eax, eax
    jnz LC_DEC_ABORT

    dec esi
    jnz LC_DEC_chunks

    call GetMSeconds
    mov endMS, eax
    rdtsc
    mov endTSClo, eax
    mov endTSChigh, edx
    mov eax, endMS
    sub eax, startMS
    mov elapsedMS, eax

    mov edx, OFFSET "DEC/JNZ result: ",0
    call WriteString
    call PrintElapsed
    call PrintRDTSCDelta

    ret

LC_LOOP_ABORT:
    mov edx, OFFSET abortedMsg
    call WriteString
    ; compute and show partial results for LOOP run
    call GetMSeconds
    mov endMS, eax
    rdtsc
    mov endTSClo, eax
    mov endTSChigh, edx
    mov eax, endMS
    sub eax, startMS
    mov elapsedMS, eax
    call PrintElapsed
    call PrintRDTSCDelta
    ret

LC_DEC_ABORT:
    mov edx, OFFSET abortedMsg
    call WriteString
    ; compute and show partial results for DEC run
    call GetMSeconds
    mov endMS, eax
    rdtsc
    mov endTSClo, eax
    mov endTSChigh, edx
    mov eax, endMS
    sub eax, startMS
    mov elapsedMS, eax
    call PrintElapsed
    call PrintRDTSCDelta
    ret

LoopCompareBenchmark ENDP

; -----------------------
; Memory read/write benchmark
; -----------------------
MemoryBenchmark PROC
    mov edx, OFFSET runningMem
    call WriteString
    call Crlf

    xor esi, esi          ; global index

    rdtsc
    mov startTSClo, eax
    mov startTSChigh, edx
    call GetMSeconds
    mov startMS, eax

    mov ecx, CHUNKS
MemChunkLoop:
    mov ebx, CHUNK_ITER
MemInner:
    ; index mod ARR_SIZE via mask (ARR_SIZE power of 2)
    mov eax, esi
    and eax, (ARR_SIZE - 1)
    shl eax, 2          ; index * 4
    lea edx, buffer
    add edx, eax
    mov eax, [edx]      ; read
    add eax, 1
    mov [edx], eax      ; write

    inc esi
    dec ebx
    jnz MemInner

    ; check abort
    push VK_Q
    call GetAsyncKeyState
    add esp,4
    test eax, eax
    jnz Mem_Abort

    dec ecx
    jnz MemChunkLoop

    call GetMSeconds
    mov endMS, eax
    rdtsc
    mov endTSClo, eax
    mov endTSChigh, edx
    mov eax, endMS
    sub eax, startMS
    mov elapsedMS, eax

    call PrintElapsed
    call PrintRDTSCDelta
    ret

Mem_Abort:
    mov edx, OFFSET abortedMsg
    call WriteString
    call GetMSeconds
    mov endMS, eax
    rdtsc
    mov endTSClo, eax
    mov endTSChigh, edx
    mov eax, endMS
    sub eax, startMS
    mov elapsedMS, eax
    call PrintElapsed
    call PrintRDTSCDelta
    ret
MemoryBenchmark ENDP

; -----------------------
; Main program
; -----------------------
main PROC
    ; show header and menu
    mov edx, OFFSET headerMsg
    call WriteString
    call Crlf

    mov edx, OFFSET menuMsg
    call WriteString
    mov edx, OFFSET menu1
    call WriteString
    mov edx, OFFSET menu2
    call WriteString
    mov edx, OFFSET menu3
    call WriteString
    mov edx, OFFSET menu4
    call WriteString
    mov edx, OFFSET menuA
    call WriteString
    mov edx, OFFSET menuQ
    call WriteString
    call Crlf

    ; read choice (blocking)
    call ReadChar
    movzx eax, al

    cmp al, '1' ; addition
    je RunAdd
    cmp al, '2' ; multiplication
    je RunMul
    cmp al, '3' ; loop compare
    je RunLoop
    cmp al, '4' ; memory
    je RunMem
    cmp al, 'A'
    je RunAll
    cmp al, 'a'
    je RunAll
    cmp al, 'Q'
    je ExitProg
    cmp al, 'q'
    je ExitProg

    ; invalid, exit
    jmp ExitProg

RunAdd:
    call AdditionBenchmark
    mov edx, OFFSET pressAny
    call WriteString
    call ReadChar
    jmp EndAll

RunMul:
    call MultiplicationBenchmark
    mov edx, OFFSET pressAny
    call WriteString
    call ReadChar
    jmp EndAll

RunLoop:
    call LoopCompareBenchmark
    mov edx, OFFSET pressAny
    call WriteString
    call ReadChar
    jmp EndAll

RunMem:
    call MemoryBenchmark
    mov edx, OFFSET pressAny
    call WriteString
    call ReadChar
    jmp EndAll

RunAll:
    call AdditionBenchmark
    mov edx, OFFSET pressAny
    call WriteString
    call ReadChar

    call MultiplicationBenchmark
    mov edx, OFFSET pressAny
    call WriteString
    call ReadChar

    call LoopCompareBenchmark
    mov edx, OFFSET pressAny
    call WriteString
    call ReadChar

    call MemoryBenchmark
    mov edx, OFFSET pressAny
    call WriteString
    call ReadChar

    jmp EndAll

ExitProg:
    push 0
    call ExitProcess

EndAll:
    ; finish normally
    mov edx, OFFSET doneMsg
    call WriteString
    push 0
    call ExitProcess

main ENDP

END main
