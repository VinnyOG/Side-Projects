[org 0x7c00]

;                the array.dat file looks exactly like: 0x12345678, only these eight bytes (two ints)

mov bp, 0x8000
mov sp, bp

pusha
mov ch, 0
mov dh, 0
mov cl, 2
mov al, 2
mov ah, 0x02
mov bx, loaded
int 0x13
popa

mov si, loaded

mov ax, [si]
mov bx,ax
call print                  ;this prints a B

mov al, bh                  ; this prints a C, meaning the first byte on disk went to al and second byte went to ah
call print

mov ax, [si + 2]          ;should print a D
mov bx, ax
call print

mov al, bh
call print                 ;should print an E

mov ax, [si + 4]
mov bx, ax               ;should print an F
call print

mov al, bh
call print       ;should print a G

mov ax, [si + 6]
mov bx, ax               ;should print an H
call print


mov al, bh
call print          ;should print an I

jmp $   ;hang

print:
add ax, 65
mov ah, 0x0e
int 0x10
ret


times 510-($-$$) db 0
dw 0xaa55

incbin "array.dat"

loaded: db 0
