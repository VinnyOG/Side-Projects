; Hackerati backend coding assignment problem 1
; Written as a bootloader in 16-bit real mode
; The array starts at sector two, after the magic boot number
; The size of the array (in ints) is stored in the word variable ARRAY_SIZE

[org 0x7c00]               ;BIOS places us here upon boot

mov bp, 0x8000             ;Give our stack enough room above us
mov sp, bp

mov [BOOT_DEVICE], dl      ;BIOS puts our boot device into dl so we save this info

jmp start                  ;Jump to code, leave a convenient space for some variables

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ARRAY_SIZE: dw 256         ; This is the amount of ints we have (256 ints ==> 2 sectors)
BOOT_DEVICE: db 0          ; To know where we came from
DISK_ERROR_MESSAGE:
  db "Disk Error", 0       ; Friendly message in case we fail to get all the ints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start:                     ;Program starts here so variables can be easy to find

pusha                      ;Save any useful info BIOS gave us, just in case we want it later
mov ax, [ARRAY_SIZE]
mul ax, 4                  ;ints are 4 bytes
div ax, 512                ;sectors are 512 bytes
add ax, 1                  ;to account for potential leftovers. now ax holds the mount of sectors to read

mov bl, al
call ReadSectors           ;read our sectors, a return means success and our numbers are at ARRAY_IN_MEMORY



;
; CompareInt compares two consecutive ints in memory,
; if ascending cx is set to 1, if descending cx is set to 2, if they are off by more than on cx is set to zero
; the address of the first int is to be in si

CompareInt:

pusha
mov ax, [si]              ;First two bytes of first int
mov bx, [si + 2]          ;Second two bytes of first int
mov cx, [si + 4]          ;First two butes of second int
mov dx, [si + 6]          ;Second two bytes of second int

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

cmp byte [si], 0           ; Check for null terminating
jne continue_printing      
ret                        ; Return if that is the case for we are done

continue_printing:
mov ax, [si]               ; put value of si as parameter (character should be in lower bytes of registers)
mov ah, 0x0e               ; BIOS print character routine in ah, value in al should not be affected
int 0x10                   ; Invoke BIOS print routine
inc si                     ; Onto the next character
jmp PrintString
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Padding and Magic number

times 510-($-$$) db 0     ;pad space with 0s until magic number
dw 0xaa55                 ;so BIOS knows we are a bootloader

%include "array.txt"      ;this file contains our ints, each one in binary (Intel/AMD CPUs should be little endian)

ARRAY_IN_MEMORY: db 0     ;This gives us a usable address to load our array into
