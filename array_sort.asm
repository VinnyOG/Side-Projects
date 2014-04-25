; Hackerati backend coding assignment problem
; Written as a bootloader in 16-bit real mode
; The array starts at sector two, after the magic boot number
; The size of the array (in ints) is stored in the word variable ARRAY_SIZE

[org 0x7c00]

mov bp, 0x8000             ;Give our stack enough room above us
mov sp, bp

mov [BOOT_DEVICE], dl      ;BIOS puts our boot device into dl so we save this info

jmp Start                  ;Jump to code, leave a convenient space for some variables

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ARRAY_SIZE: dw 128         ; This is the amount of ints we have (256 ints ==> 2 sectors)
BOOT_DEVICE: db 0          ; To know where we came from
DISK_ERROR_MESSAGE:
  db "Disk Error!", 0       ; Friendly message in case we fail to get all the ints
DISK_READ_OKAY:
  db "Disk Okay...", 10, 13, 0        ; Friendly message in case the memory is loaded properly
PROGRAM_STARTING:
  db "Program Starting...", 10, 13, 0 ; Self explanatory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Start:                     ;Program starts here so variables can be easy to find

pusha                      ;Save any useful info BIOS gave us, just in case we want it later
mov si, PROGRAM_STARTING
call PrintString
mov ax, [ARRAY_SIZE]
imul ax, 4                  ;ints are 4 bytes
xor dx, dx                  ;zeros this out
mov cx, 512                ;sectors are 512 bytes
div cx
add ax, 1                  ;to account for potential leftovers. now ax holds the mount of sectors to read (tested and working)

mov bl, al
call ReadSectors           ;read our sectors, a return means success and our numbers are at ARRAY_IN_MEMORY,


;keep current streak in al
;use ah for direction (0=nothing,1=increasing,2=decreasing)
;indices of runs will be kept as 16-bit words (we have room for one 128 of them at INDICES; so 200 bytes before ARRAY_IN_MEMORY)
;when we are done print the start indices of the runs as well as the ints they contain

.indices_left: dw ARRAY_SIZE
.loop0:
cmp word [.indices_left], 0
je .done


;do stuff here


dec word [.indices_left]
jmp .loop0

.done:    ;Yay! now we need to print the results nicely


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
; CompareInt compares two consecutive ints in memory,
; if ascending cx is set to 1, if descending cx is set to 2, if they are off by more than on cx is set to zero
; the address of the first int is to be in si

CompareInt:

pusha
call .compare_bytes                ;si should already be set to our LSBs
cmp cx, 1
je .consecutive_up                 ; is consecutive up
cmp cx, -1
je .consecutive_down              ; is not consecutive down
popa                               ;if they were not cought above they can not be consecutive
mov cx, 0                          ;meaning they are not consecutive in anyway
ret

;-------------------------------------------------------------
.consecutive_up:


;-------------------------------------------------------------
.consecutive_down:

;-------------------------------------------------------------

.compare_bytes:           ;compares [si] and [si+2], sets cx to 1 if consecutive up, -1 if consecutive down and zero if
                          ;same value, cx can be 2 to signify non of the above
push ax
push bx
mov ax, [si]
mov bx, [si + 2]
sub ax, bx
cmp ax, 0
jne .not_equal
mov cx, 0                 ;if they subtract to equal 0 then they are equal
ret

.not_equal:
cmp ax, 1                ;if the remainder is one then ax was bigger than bx so consecutive down
jne .not_bigger
mov cx, -1               ;signifies consecutive down
ret

.not_bigger:
cmp ax, -1               ;see if bx was bigger than ax
jne .nothing
mov cx, 1                ;signifies consecutive up
ret

.nothing:
mov cx, 2             ; to signify nothing
ret

                          ;Becasue we are in little endian, our memory and registers should look something like:
                          ;On disk (for example of course) [ 0x1 0x2 0x3 0x4 ] [ 0x5 0x6 0x7 0x8 ]
                          ;Original numbers                  0x4321              0x87654
                          ;Test program confirms that first byte is loaded into al and second into ah
                          ;Registers  ax->0x21 bx->0x43 cx->65 dx->0x87


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
; ReadSectors routine reads in bl sectors of data after the bootloader into location ARRAY_IN_MEMORY

ReadSectors:

pusha                      ; Be nice and save the registers
mov bl, al                 ; Save the amount we want to read
push bx
mov dl, [BOOT_DEVICE]      ; Our boot device
mov ch, 0                  ; First cylinder
mov dh, 0                  ; First head
mov cl, 2                  ; Sector 2 (counting from 1, not 0)
mov ah, 0x02               ; The BIOS read sector routine
                           ; al stores the number of sectors we want, this should be set as a parameter
mov bx, ARRAY_IN_MEMORY    ; The address we want to load our data into
int 0x13                   ; Invoke BIOS read routine

jc disk_error              ; If the carry flag is set there was an error
pop bx
cmp al, bl                 ; We chack to make sure the amount actually read is the amount we want
jne disk_error
mov si, DISK_READ_OKAY
call PrintString
popa
ret                        ; If we get here then we succesfully read our sectors into memory

disk_error:
mov si, DISK_ERROR_MESSAGE
call PrintString
jmp $                      ; Hang the system after an error occured
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
; PrintString routine, pointer to string in si

PrintString:
pusha

.loop:
cmp byte [si], 0           ; Check for null terminating
jne .continue_printing
popa
ret                        ; Return if that is the case for we are done

.continue_printing:
mov ax, [si]               ; put value of si as parameter (character should be in lower bytes of registers)
mov ah, 0x0e               ; BIOS print character routine in ah, value in al should not be affected
int 0x10                   ; Invoke BIOS print routine
inc si                     ; Onto the next character
jmp .loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Padding and Magic number

times 510-($-$$) db 0     ;pad space with 0s until magic number
dw 0xaa55                 ;so BIOS knows we are a bootloader

incbin "array.dat"      ;this file contains our ints, each one in binary (Intel/AMD CPUs should be little endian)
times 4096 db 0x45       ;temporary place holder/padding to prevent disk read errors

INDICES: times 128 dw 0   ;room for 128 16-bit indices
ARRAY_IN_MEMORY: db 0     ;This gives us a usable address to load our array into

