; 86-DOS Disk consistancy checker version 1.00. Revised 4-24-81.
;
; Disassembled from CHKDSK.COM written by Tim Paterson.
;
; Use Seattle Computer Products 8086 Assembler 2.40 or later to
; assemble this file.

                org     100h

LAB000:
                mov     ah, 19h
                int     21h             ; DOS - GET DEFAULT DISK NUMBER
                mov     [566h], al
                mov     dl, [5Ch]
                or      dl, dl
                jz      LAB010
                mov     ah, 0Eh
                dec     dl
                int     21h             ; DOS - SELECT DISK
                                        ; DL = new default drive number (0 = A, 1 = B, etc.)
                                        ; Return: AL = number of logical drives

LAB010:
                mov     ah, 1Bh
                int     21h             ; DOS - GET ALLOCATION TABLE INFORMATION FOR DEFAULT DRIVE
                                        ; Return: DS:BX -> FAT ID byte for default drive
                push    ds
                push    es
                pop     ds
                mov     [556h], dx
                mov     [558h], cx
                mov     ah, 0
                mov     [55Ah], ax
                mov     cx, dx
                mov     di, 567h
                xor     ax, ax
                rep
                stosw
                mov     cx, ax
                mov     [560h], ax
                mov     [55Ch], ax
                mov     [564h], ax
                mov     bp, di
                mov     [55Eh], bp
                mov     dx, di
                mov     ah, 1Ah
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     ah, 11h
                mov     dx, DAT000
                int     21h             ; DOS - SEARCH FIRST USING FCB
                                        ; DS:DX -> FCB
                inc     al
                pop     es
                jnz     LAB020
                jmp     LAB160

LAB020:
                inc     cx
                add     di, 25h
                mov     dx, di
                mov     ah, 1Ah
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     ah, 12h
                mov     dx, DAT000
                int     21h             ; DOS - SEARCH NEXT USING FCB
                                        ; DS:DX -> FCB
                                        ; Return: AL = status
                inc     al
                jnz     LAB020
                mov     [55Ch], cx
                mov     cx, 1
                mov     dx, [556h]
                inc     dx
                mov     [562h], dx
                add     bp, 1Bh

LAB030:
                xor     ax, ax
                mov     si, [bp+0]
                cmp     si, [562h]
                mov     di, STR020
                ja      LAB060
                or      si, si
                jz      LAB050

LAB040:
                mov     di, cx
                shl     si
                xchg    di, [si+563h]
                shr     si
                or      di, di
                jnz     LAB070
                inc     ax
                call    LAB200
                jz      LAB080
                cmp     di, 0FF8h

LAB050:
                jnb     LAB100
                cmp     di, [562h]
                ja      LAB080
                mov     si, di
                jp      LAB040

LAB060:
                push    ax
                mov     ax, cx
                mov     dx, 25h
                dec     ax
                mul     ax, dx
                mov     dx, ax
                add     dx, [55Eh]
                mov     ah, 13h
                int     21h             ; DOS - DELETE FILE via FCB
                                        ; DS:DX -> FCB with filename field filled with
                                        ; template for  deletion ('?' wildcard allowed, but not '*')
                                        ; Return: AL = 00h file found, FFh file not found
                inc     w, [564h]
                mov     dx, STR020
                jp      LAB090

LAB070:
                mov     dx, STR030
                push    ax
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                mov     ax, di
                call    LAB350
                mov     dx, STR000
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                mov     ax, cx
                call    LAB350
                mov     dx, STR110
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                pop     ax
                add     [560h], ax
                jmp     LAB150

LAB080:
                mov     dx, 0FFFh
                call    LAB220
                mov     dx, STR010
                push    ax

LAB090:
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                mov     ax, cx
                call    LAB350
                mov     dx, STR110
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                pop     ax

LAB100:
                add     [560h], ax
                mov     si, ax
                mov     ax, [bp+2]
                mov     dx, [bp+4]
                push    cx
                mov     cx, [55Ah]
                dec     cx
                add     ax, cx
                adc     dx, 0
                inc     cx

LAB110:
                shr     cx
                jb      LAB120
                shr     dx
                rcr     ax
                jp      LAB110

LAB120:
                div     w, ax, [558h]
                or      dx, dx
                jz      LAB130
                inc     ax

LAB130:
                cmp     ax, si
                jz      LAB140
                xor     di, di
                mov     cx, [55Ah]
                call    LAB260
                mov     [bp+6], si
                mov     [bp+8], di
                mov     dx, bp
                sub     dx, 1Bh
                mov     ah, 0Fh
                int     21h             ; DOS - OPEN DISK FILE
                                        ; DS:DX -> FCB
                                        ; Return: AL = 00h file found, FFh file not found
                mov     ax, [558h]
                mov     [bp-0Dh], ax
                mov     cx, 0
                mov     ah, 28h
                int     21h             ; DOS - RANDOM BLOCK WRITE
                                        ; DS:DX -> FCB
                                        ; CX = number of records to be written
                                        ; if zero, truncate file to current random file position
                mov     ah, 10h
                int     21h             ; DOS - CLOSE DISK FILE
                                        ; DS:DX -> FCB
                                        ; Return: AL = 00h directory update successful
                                        ; FFh file not found in directory
                mov     ah, 1Bh
                push    ds
                int     21h             ; DOS - GET ALLOCATION TABLE INFORMATION FOR DEFAULT DRIVE
                                        ; Return: DS:BX -> FAT ID byte for default drive
                pop     ds
                mov     dx, STR040
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                pop     ax
                push    ax
                call    LAB350
                mov     dx, STR110
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"

LAB140:
                pop     cx

LAB150:
                add     bp, 25h
                inc     cx
                cmp     cx, [55Ch]
                ja      LAB160
                jmp     LAB030

LAB160:
                xor     ax, ax
                mov     bp, 567h
                mov     si, 2
                mov     cx, [556h]

LAB170:
                test    w, [bp+0], 0FFFFh
                jnz     LAB180
                call    LAB200
                jz      LAB180
                xor     dx, dx
                call    LAB220
                inc     ax

LAB180:
                inc     bp
                inc     bp
                inc     si
                loop    LAB170
                or      ax, ax
                jz      LAB190
                mov     bx, STR050
                call    LAB270

LAB190:
                mov     si, [55Ch]
                sub     si, [564h]
                xor     di, di
                mov     bx, STR060
                call    LAB280
                mov     ax, [556h]
                mov     bx, STR070
                call    LAB270
                mov     ax, [556h]
                sub     ax, [560h]
                mov     bx, STR080
                call    LAB270
                mov     ax, [2]
                mov     dx, 10h
                mul     ax, dx
                mov     si, ax
                mov     di, dx
                mov     bx, STR090
                call    LAB280
                mov     ax, [2]
                mov     dx, cs
                sub     ax, dx
                mov     dx, 10h
                mul     ax, dx
                mov     si, ax
                mov     di, dx
                mov     bx, STR100
                call    LAB280
                seg     cs
                mov     dl, [566h]
                mov     ah, 0Eh
                int     21h             ; DOS - SELECT DISK
                                        ; DL = new default drive number (0 = A, 1 = B, etc.)
                                        ; Return: AL = number of logical drives
                int     20h             ; DOS - PROGRAM TERMINATION
                                        ; returns to DOS--identical to INT 21/AH=00h

LAB200:
                mov     di, si
                shr     di
                add     di, si
                seg     es
                mov     di, [bx+di]
                test    si, 1
                jz      LAB210
                shr     di
                shr     di
                shr     di
                shr     di

LAB210:
                and     di, 0FFFh
                ret

LAB220:
                push    si
                mov     di, si
                shr     si
                add     si, bx
                add     si, di
                shr     di
                seg     es
                mov     di, [si]
                jnb     LAB230
                shl     dx
                shl     dx
                shl     dx
                shl     dx
                and     di, 0Fh
                jp      LAB240

LAB230:
                and     di, 0F000h

LAB240:
                or      di, dx
                seg     es
                mov     [si], di
                pop     si

LAB250:
                ret

LAB260:
                shr     cx
                jb      LAB250
                shl     si
                rcl     di
                jp      LAB260

LAB270:
                mul     w, ax, [558h]
                mov     cx, [55Ah]
                mov     si, ax
                mov     di, dx
                call    LAB260

LAB280:
                push    bx
                xor     ax, ax
                mov     bx, ax
                mov     bp, ax
                mov     cx, 20h

LAB290:
                shl     si
                rcl     di
                xchg    ax, bp
                call    LAB340
                xchg    ax, bp
                xchg    ax, bx
                call    LAB340
                xchg    ax, bx
                adc     al, 0
                loop    LAB290
                mov     cx, 1B10h
                call    LAB300
                mov     ax, bx
                call    LAB300
                mov     ax, bp
                call    LAB300
                pop     dx
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                ret

LAB300:
                push    ax
                mov     dl, ah
                call    LAB310
                pop     dx

LAB310:
                mov     dh, dl
                shr     dl
                shr     dl
                shr     dl
                shr     dl
                call    LAB320
                mov     dl, dh

LAB320:
                and     dl, 0Fh
                jz      LAB330
                mov     cl, 0

LAB330:
                dec     ch
                and     cl, ch
                or      dl, 30h
                sub     dl, cl
                mov     ah, 2
                int     21h             ; DOS - DISPLAY OUTPUT
                                        ; DL = character to send to standard output
                ret

LAB340:
                adc     al, al
                daa
                xchg    al, ah
                adc     al, al
                daa
                xchg    al, ah
                ret

LAB350:
                dec     al
                mov     ah, 25h
                mul     ax, ah
                mov     si, ax
                add     si, [55Eh]
                inc     si
                mov     cl, 8
                mov     ah, 2
                call    LAB360
                mov     dl, 20h
                int     21h             ; DOS -
                mov     cl, 3

LAB360:
                lodsb
                mov     dl, al
                int     21h             ; DOS -
                dec     cl
                jnz     LAB360
                ret


DAT000          db 0,"???????????"
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
                db    0
STR000          db  "  and  $"
STR010          db  "Error in allocation table for file $"
STR020          db  "Non-recoverable directory error--file deleted: $"
STR030          db  "Files cross-linked: $"
STR040          db  "Directory error--incorrect size of file $"
STR050          db  " bytes unallocated disk space freed",13,10,13,10,"$"
STR060          db  " disk files",13,10,"$"
STR070          db  " bytes total disk space",13,10,"$"
STR080          db  " bytes remain available",13,10,13,10,"$"
STR090          db  " bytes total system RAM",13,10,"$"
STR100          db  " bytes free"
STR110          db  13,10,"$"


                end

