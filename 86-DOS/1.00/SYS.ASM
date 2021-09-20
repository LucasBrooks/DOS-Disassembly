; 86-DOS SYS Program version 1.00. Revised 4-24-81.
;
; Disassembled from SYS.COM written by Tim Paterson.
;
; Use Seattle Computer Products 8086 Assembler 2.40 or later to
; assemble this file.

                org     100h

LAB000:
                jmp     LAB010

DAT000          dw 34h

LAB010:
                mov     al, [5Ch]
                or      al, al
                jz      LAB050
                dec     al
                jz      LAB050
                mov     [5Ch], al
                mov     cx, [DAT000]
                mov     dx, 0
                mov     bx, 19Ah
                mov     al, 0
                int     25h             ; DOS - ABSOLUTE DISK READ (except DOS 4.0/COMPAQ DOS 3.31 >32M partitn)
                                        ; AL = drive number (0=A, 1=B, etc), DS:BX = Disk Transfer Address (buffer)
                                        ; CX = number of sectors to read, DX = first relative sector to read
                                        ; Return: CF set on error
                jb      LAB020
                popf
                mov     cx, [DAT000]
                mov     dx, 0
                mov     bx, 19Ah
                mov     al, [5Ch]
                mov     ah, 1
                int     26h             ; DOS - ABSOLUTE DISK WRITE (except DOS 4.0/COMPAQ DOS 3.31 >32M partn)
                                        ; AL = drive number (0=A, 1=B, etc), DS:BX = Disk Transfer Address (buffer)
                                        ; CX = number of sectors to write, DX = first relative sector to write
                                        ; Return: CF set on error
                jb      LAB040
                mov     dx, STR000
                jp      LAB030

LAB020:
                mov     dx, STR010

LAB030:
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                int     20h             ; DOS - PROGRAM TERMINATION
                                        ; returns to DOS--identical to INT 21/AH=00h

LAB040:
                mov     dx, STR020
                jp      LAB030

LAB050:
                mov     dx, STR030
                jp      LAB030


STR000          db  "System transfered$"
STR010          db  "Disk read error$"
STR020          db  "Disk write error$"
STR030          db  "Bad drive specification$"


                end
