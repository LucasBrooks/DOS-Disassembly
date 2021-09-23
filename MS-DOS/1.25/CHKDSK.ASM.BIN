CODE            segment public 'CODE'
                assume cs:CODE, ds:CODE, es:CODE

                org     100h

LAB000:
                mov     sp, offset STACK
                cmp     byte ptr ds:5Dh, 20h
                jnz     short LAB030
                or      al, al
                jz      short LAB050

LAB010:
                mov     dx, offset STR020

LAB020:
                push    cs
                pop     ds
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                int     20h             ; DOS - PROGRAM TERMINATION
                                        ; returns to DOS--identical to INT 21/AH=00h

LAB030:
                mov     dx, offset STR030
                jmp     short LAB020

LAB040:
                mov     dx, offset STR080
                jmp     short LAB020

LAB050:
                mov     al, ds:5Ch
                mov     DAT010, al
                mov     dl, al
                mov     ah, 1Ch
                int     21h             ; DOS - GET ALLOCATION TABLE INFORMATION FOR SPECIFIC DRIVE
                                        ; DL = drive number to check (0=default, 1=A, etc)
                or      bx, bx
                jz      short LAB010
                cmp     byte ptr [bx], 0F8h
                jb      short LAB040
                push    ds
                push    es
                pop     ds
                mov     DAT030, dx
                mov     DAT040, cx
                mov     ah, 0
                mov     DAT050, ax
                mov     cx, dx
                mov     di, offset STACK
                xor     ax, ax
                rep stosw
                mov     cx, ax
                mov     DAT080, ax
                mov     DAT060, ax
                mov     DAT100, ax
                mov     DAT110, ax
                mov     DAT120, ax
                mov     DAT130, ax
                mov     bp, di
                mov     DAT070, bp
                mov     dx, di
                mov     ah, 1Ah
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     ah, 11h
                mov     dx, offset DAT000
                int     21h             ; DOS - SEARCH FIRST USING FCB
                                        ; DS:DX -> FCB
                inc     al
                pop     es
                jnz     short LAB060
                jmp     LAB270

LAB060:
                inc     cx
                add     di, 2Ch
                mov     dx, di
                mov     ah, 1Ah
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     ah, 12h
                mov     dx, offset DAT000
                int     21h             ; DOS - SEARCH NEXT USING FCB
                                        ; DS:DX -> FCB
                                        ; Return: AL = status
                inc     al
                jnz     short LAB060
                mov     DAT060, cx
                mov     cx, 1
                mov     dx, DAT030
                inc     dx
                mov     DAT090, dx
                add     bp, 22h

LAB070:
                xor     ax, ax
                mov     si, [bp+0]
                cmp     si, DAT090
                mov     di, offset STR050
                ja      short LAB100
                or      si, si
                jz      short LAB090

LAB080:
                mov     di, cx
                shl     si, 1
                xchg    di, [si+7B4h]
                shr     si, 1
                or      di, di
                jnz     short LAB120
                inc     ax
                call    LAB350
                jz      short LAB130
                cmp     di, 0FF8h

LAB090:
                jnb     short LAB150
                cmp     di, DAT090
                ja      short LAB130
                mov     si, di
                jmp     short LAB080

LAB100:
                push    ax
                mov     [bp+2], ax
                mov     [bp+4], ax
                mov     ax, cx
                mov     dx, 2Ch
                dec     ax
                mul     dx
                mov     dx, ax
                add     dx, DAT070
                mov     ah, 13h
                int     21h             ; DOS - DELETE FILE via FCB
                                        ; DS:DX -> FCB with filename field filled with
                                        ; template for  deletion ('?' wildcard allowed, but not '*')
                                        ; Return: AL = 00h file found, FFh file not found
                or      al, al
                jz      short LAB110
                jmp     LAB040

LAB110:
                inc     DAT100
                mov     dx, offset STR050
                jmp     short LAB140

LAB120:
                mov     dx, offset STR060
                push    ax
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                mov     ax, di
                call    LAB540
                mov     dx, offset STR010
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                mov     ax, cx
                call    LAB540
                mov     dx, offset STR180
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                pop     ax
                add     DAT080, ax
                jmp     LAB260

LAB130:
                mov     dx, 0FFFh
                call    LAB370
                mov     dx, offset STR040
                push    ax

LAB140:
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                mov     ax, cx
                call    LAB540
                mov     dx, offset STR180
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                pop     ax

LAB150:
                push    cx
                cmp     byte ptr [bp-0Fh], 0
                jz      short LAB190
                push    ax
                mov     si, bp
                sub     si, 1Ah
                mov     di, offset STR000
                mov     cx, 0Bh

LAB160:
                lodsb
                cmp     al, [di]
                jz      short LAB170
                jmp     short LAB180

LAB170:
                inc     di
                loop    LAB160
                pop     ax
                add     DAT110, ax
                dec     DAT060
                jmp     short LAB190

LAB180:
                pop     ax
                add     DAT120, ax
                inc     DAT130

LAB190:
                add     DAT080, ax
                mov     si, ax
                mov     ax, [bp+2]
                mov     dx, [bp+4]
                mov     cx, DAT050
                dec     cx
                add     ax, cx
                adc     dx, 0
                inc     cx

LAB200:
                shr     cx, 1
                jb      short LAB210
                shr     dx, 1
                rcr     ax, 1
                jmp     short LAB200

LAB210:
                cmp     dx, DAT040
                jge     short LAB230
                div     DAT040
                or      dx, dx
                jz      short LAB220
                inc     ax

LAB220:
                cmp     ax, si
                jz      short LAB250

LAB230:
                xor     di, di
                mov     cx, DAT050
                call    LAB410
                mov     [bp+6], si
                mov     [bp+8], di
                mov     dx, bp
                sub     dx, 22h
                mov     ah, 0Fh
                int     21h             ; DOS - OPEN DISK FILE
                                        ; DS:DX -> FCB
                                        ; Return: AL = 00h file found, FFh file not found
                or      al, al
                jz      short LAB240
                jmp     LAB040

LAB240:
                mov     ax, DAT040
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
                mov     dx, offset STR070
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                pop     ax
                push    ax
                call    LAB540
                mov     dx, offset STR180
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"

LAB250:
                pop     cx

LAB260:
                add     bp, 2Ch
                inc     cx
                cmp     cx, DAT060
                ja      short LAB270
                jmp     LAB070

LAB270:
                xor     ax, ax
                mov     bp, offset STACK
                mov     si, 2
                mov     cx, DAT030

LAB280:
                test    word ptr [bp+0], 0FFFFh
                jnz     short LAB300
                call    LAB350
                jz      short LAB300
                cmp     di, 0FF7h
                jnz     short LAB290
                inc     DAT110
                inc     DAT080
                jmp     short LAB300

LAB290:
                xor     dx, dx
                call    LAB370
                inc     ax

LAB300:
                inc     bp
                inc     bp
                inc     si
                loop    LAB280
                or      ax, ax
                jz      short LAB310
                mov     bx, offset STR090
                call    LAB430

LAB310:
                mov     dx, offset STR180
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                mov     ax, DAT030
                mov     bx, offset STR100
                call    LAB430
                cmp     DAT120, 0
                jz      short LAB320
                mov     ax, DAT120
                mov     bx, offset STR110
                call    LAB430
                mov     si, DAT130
                xor     di, di
                mov     bx, offset STR140
                call    LAB420

LAB320:
                mov     ax, DAT080
                sub     ax, DAT120
                sub     ax, DAT110
                cmp     ax, 0
                jz      short LAB330
                mov     bx, offset STR110
                call    LAB430
                mov     si, DAT060
                sub     si, DAT100
                sub     si, DAT130
                xor     di, di
                mov     bx, offset STR120
                call    LAB420

LAB330:
                cmp     DAT110, 0
                jz      short LAB340
                mov     ax, DAT110
                mov     bx, offset STR130
                call    LAB430

LAB340:
                mov     ax, DAT030
                sub     ax, DAT080
                mov     bx, offset STR150
                call    LAB430
                mov     ax, ds:2
                mov     dx, 10h
                mul     dx
                mov     si, ax
                mov     di, dx
                mov     bx, offset STR160
                call    LAB440
                mov     ax, ds:2
                mov     dx, cs
                sub     ax, dx
                mov     dx, 10h
                mul     dx
                mov     si, ax
                mov     di, dx
                mov     bx, offset STR170
                call    LAB440
                mov     ah, 0Dh
                int     21h             ; DOS - DISK RESET
                int     20h             ; DOS - PROGRAM TERMINATION

LAB350:
                mov     di, si
                shr     di, 1
                add     di, si
                mov     di, es:[bx+di]
                test    si, 1
                jz      short LAB360
                shr     di, 1
                shr     di, 1
                shr     di, 1
                shr     di, 1

LAB360:
                and     di, 0FFFh
                ret

LAB370:
                mov     byte ptr es:[bx-1], 1
                push    si
                mov     di, si
                shr     si, 1
                add     si, bx
                add     si, di
                shr     di, 1
                mov     di, es:[si]
                jnb     short LAB380
                shl     dx, 1
                shl     dx, 1
                shl     dx, 1
                shl     dx, 1
                and     di, 0Fh
                jmp     short LAB390

LAB380:
                and     di, 0F000h

LAB390:
                or      di, dx
                mov     es:[si], di
                pop     si

LAB400:
                ret

LAB410:
                shr     cx, 1
                jb      short LAB400
                shl     si, 1
                rcl     di, 1
                jmp     short LAB410

LAB420:
                mov     byte ptr DAT020, 1
                jmp     short LAB440

LAB430:
                mul     DAT040
                mov     cx, DAT050
                mov     si, ax
                mov     di, dx
                call    LAB410

LAB440:
                push    bx
                xor     ax, ax
                mov     bx, ax
                mov     bp, ax
                mov     cx, 20h

LAB450:
                shl     si, 1
                rcl     di, 1
                xchg    ax, bp
                call    LAB530
                xchg    ax, bp
                xchg    ax, bx
                call    LAB530
                xchg    ax, bx
                adc     al, 0
                loop    LAB450
                mov     cx, 1310h
                cmp     byte ptr DAT020, 0
                jnz     short LAB460
                mov     cx, 1810h
                xchg    ax, dx
                call    LAB500
                xchg    ax, bx
                call    LAB480

LAB460:
                mov     ax, bp
                call    LAB480
                mov     byte ptr DAT020, 0
                pop     dx
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"

LAB470:
                ret

LAB480:
                push    ax
                mov     dl, ah
                call    LAB490
                pop     dx

LAB490:
                mov     dh, dl
                shr     dl, 1
                shr     dl, 1
                shr     dl, 1
                shr     dl, 1
                call    LAB500
                mov     dl, dh

LAB500:
                and     dl, 0Fh
                jz      short LAB510
                mov     cl, 0

LAB510:
                dec     ch
                and     cl, ch
                or      dl, 30h
                sub     dl, cl
                cmp     byte ptr DAT020, 0
                jz      short LAB520
                cmp     dl, 30h
                jl      short LAB470

LAB520:
                mov     ah, 2
                int     21h             ; DOS - DISPLAY OUTPUT
                                        ; DL = character to send to standard output
                ret

LAB530:
                adc     al, al
                daa
                xchg    al, ah
                adc     al, al
                daa
                xchg    al, ah
                ret

LAB540:
                push    cx
                dec     ax
                mov     cx, 2Ch
                mul     cx
                mov     si, ax
                add     si, DAT070
                add     si, 8
                mov     cx, 8
                mov     ah, 2
                call    LAB550
                mov     dl, 20h
                int     21h             ; DOS -
                mov     cl, 3
                call    LAB550
                pop     cx
                ret

LAB550:
                lodsb
                mov     dl, al
                int     21h             ; DOS -
                loop    LAB550
                ret


DAT000          db -1,0,0,0,0,0,6
DAT010          db 0,"???????????"
                db 25 dup(0)
STR000          db "badtrack   "
DAT020          db 0
STR010          db "  and  $"
STR020          db "Invalid drive specification$"
STR030          db "Invalid parameter$"
STR040          db "Allocation error for file $"
STR050          db "Directory error-file :    $"
STR060          db "Files cross-linked: ",13,10,"     $"
STR070          db "File size error for file  $"
STR080          db "Diskette not initialized $"
STR090          db " bytes disk space freed",13,10,"$"
STR100          db " bytes total disk space",13,10,"$"
STR110          db " bytes in $"
STR120          db " user files",13,10,"$"
STR130          db " bytes in bad sectors",13,10,"$"
STR140          db " hidden files",13,10,"$"
STR150          db " bytes available on disk",13,10,13,10,"$"
STR160          db " bytes total memory",13,10,"$"
STR170          db " bytes free"
STR180          db 13,10,"$"
                db 0
DAT030          dw 0
DAT040          dw 0
DAT050          dw 0
DAT060          dw 0
DAT070          dw 0
DAT080          dw 0
DAT090          dw 0
DAT100          dw 0
DAT110          dw 0
DAT120          dw 0
DAT130          dw 0
                db 100h dup(0)
STACK           label byte
CODE            ends


                end LAB000
