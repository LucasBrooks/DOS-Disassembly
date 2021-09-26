;***************************************************************
;
;       86-DOS FORMAT DISK UTILITY
;
;       This routine formats a new disk,clears the FAT and DIRECTORY
;       then optionally copies the SYSTEM and COMMAND.COM to this
;       new disk
;
;       SYNTAX: FORMAT  [drive][/switch1][/switch2]...[/switch16]
;
;       Regardless of the drive designator , the user will be
;       prompted to insert the diskette to be formatted.
;
;***************************************************************

                public  SWITCHMAP
                public  DRIVE
                extrn   FATID:far
                extrn   FREESPACE:far
                extrn   STARTSECTOR:far
                extrn   SWITCHLIST:byte
                extrn   BADSECTOR:near
                extrn   DISKFORMAT:near
                extrn   DONE:near
                extrn   INIT:near


CODE            segment public 'CODE'
                assume cs:CODE, ds:code, es:code

                org     100h

LAB000:
                mov     sp, offset STACK
                cmp     al, -1
                jnz     short LAB010
                mov     dx, offset STR050
                call    LAB480
                jmp     LAB560

LAB010:
                mov     ah, 19h
                int     21h             ; DOS - GET DEFAULT DISK NUMBER
                mov     ds:DAT030, al
                mov     si, 5Ch
                lodsb
                or      al, al
                jnz     short LAB020
                mov     al, ds:DAT030
                inc     al

LAB020:
                dec     al
                mov     ds:5Ch, al
                mov     ds:DRIVE, al
                xor     bx, bx
                mov     si, 129

LAB030:
                call    LAB070
                lodsb
                cmp     al, "/"
                jz      short LAB040
                cmp     al, 0Dh
                jz      short LAB080
                lodsb
                cmp     al, ":"
                jnz     short LAB060
                cmp     ds:DAT000, 0
                jnz     short LAB060
                inc     ds:DAT000
                jmp     short LAB030

LAB040:
                lodsb
                cmp     al, "A"
                jl      short LAB050
                and     al, 0DFh

LAB050:
                mov     cl, ds:SWITCHLIST
                or      cl, cl
                jz      short LAB060
                mov     ch, 0
                mov     di, (offset SWITCHLIST+1)
                repne scasb
                jnz     short LAB060
                mov     ax, 1
                shl     ax, cl
                or      bx, ax
                jmp     short LAB030

LAB060:
                mov     dx, offset STR060
                call    LAB480
                jmp     LAB560

LAB070:
                lodsb
                cmp     al, " "
                jz      short LAB070
                cmp     al, 9
                jz      short LAB070
                dec     si
                ret

LAB080:
                mov     ds:SWITCHMAP, bx
                test    ds:SWITCHMAP, 1
                jz      short LAB120

LAB090:
                call    LAB620
                jnb     short LAB110

LAB100:
                call    LAB570
                jmp     short LAB090

LAB110:
                call    LAB690
                jb      short LAB100

LAB120:
                call    INIT
                jnb     short LAB130
                mov     dx, offset STR090
                call    LAB480
                jmp     LAB560

LAB130:
                mov     dx, ds:SWITCHMAP
                mov     ds:DAT250, dx

LAB140:
                mov     ds:DAT330, 0
                mov     ds:DAT340, 0
                mov     ds:DAT300, 0
                mov     byte ptr ds:DAT000, 0
                mov     byte ptr ds:DAT010, 0
                mov     dx, ds:DAT250
                mov     ds:SWITCHMAP, dx
                mov     al, ds:DRIVE
                add     al, "A"
                mov     ds:[STR000+30], al
                mov     dx, offset STR000
                call    LAB480
                mov     dx, offset STR010
                call    LAB480
                call    LAB510
                call    LAB500
                call    LAB500
                call    DISKFORMAT
                jnb     short LAB160

LAB150:
                mov     dx, offset STR080
                call    LAB480
                jmp     short LAB140

LAB160:
                call    BADSECTOR
                jb      short LAB150
                cmp     ax, 0
                jnz     short LAB170
                jmp     LAB280

LAB170:
                cmp     bx, word ptr ds:STARTSECTOR
                jge     short LAB180
                mov     dx, offset STR110
                call    LAB480
                jmp     short LAB150

LAB180:
                mov     ds:DAT360, ax
                cmp     ds:DAT010, 0
                jnz     short LAB190
                inc     ds:DAT010
                push    bx
                call    LAB600
                pop     bx

LAB190:
                test    ds:SWITCHMAP, 1
                jz      short LAB210
                cmp     ds:DAT000, 0
                jnz     short LAB200
                inc     ds:DAT000
                call    LAB450
                xor     dx, dx
                mov     ax, ds:DAT300
                div     ds:DAT290
                add     ax, word ptr ds:STARTSECTOR
                mov     ds:DAT350, ax

LAB200:
                cmp     bx, ds:DAT350
                jg      short LAB210
                mov     dx, offset STR100
                call    LAB480
                and     ds:SWITCHMAP, -2
                mov     ds:DAT300, 0

LAB210:
                mov     ax, ds:DAT360
                mul     ds:DAT290
                add     ds:DAT330, ax
                jnb     short LAB220
                inc     ds:DAT340

LAB220:
                mov     ax, ds:DAT360
                div     ds:DAT280
                or      dx, dx
                jz      short LAB230
                inc     ax

LAB230:
                mov     cx, ax
                mov     ax, ds:DAT360
                add     bx, ax
                sub     bx, word ptr ds:STARTSECTOR
                xchg    ax, bx
                div     ds:DAT280
                xchg    ax, bx

LAB240:
                mov     dx, 0FF7h
                call    LAB250
                dec     bx
                loop    LAB240
                jmp     LAB160

LAB250:
                push    es
                mov     es, ds:DAT270
                push    bx
                push    dx
                mov     si, bx
                shr     bx, 1
                add     bx, ds:DAT260
                add     bx, si
                shr     si, 1
                mov     si, es:[bx]
                jnb     short LAB260
                shl     dx, 1
                shl     dx, 1
                shl     dx, 1
                shl     dx, 1
                and     si, 0Fh
                jmp     short LAB270

LAB260:
                and     si, 0F000h

LAB270:
                or      si, dx
                mov     es:[bx], si
                pop     dx
                pop     bx
                pop     es
                ret

LAB280:
                cmp     ds:DAT010, 0
                jnz     short LAB290
                call    LAB600
                call    LAB450

LAB290:
                push    es
                mov     ax, ds:DAT270
                mov     es, ax
                mov     di, ds:DAT260
                mov     byte ptr es:[di-1], 1
                pop     es
                test    ds:SWITCHMAP, 1
                jz      short LAB300
                call    LAB670
                call    LAB720
                mov     dx, offset STR020
                call    LAB480

LAB300:
                call    LAB500
                mov     ah, 0Dh
                int     21h             ; DOS - DISK RESET
                call    DONE
                jnb     short LAB310
                jmp     LAB150

LAB310:
                mov     ax, ds:DAT310
                mul     ds:DAT290
                mov     cx, ds:DAT280
                call    LAB440
                mov     ds:DAT310, ax
                mov     ds:DAT320, dx
                mov     si, ax
                mov     di, dx
                mov     bx, offset STR130
                call    LAB350
                cmp     ds:DAT300, 0
                jz      short LAB320
                mov     si, ds:DAT300
                xor     di, di
                mov     bx, offset STR140
                call    LAB350

LAB320:
                cmp     ds:DAT330, 0
                jnz     short LAB330
                cmp     ds:DAT340, 0
                jz      short LAB340

LAB330:
                mov     si, ds:DAT330
                mov     di, ds:DAT340
                mov     bx, offset STR150
                call    LAB350

LAB340:
                mov     cx, ds:DAT310
                mov     bx, ds:DAT320
                sub     cx, ds:DAT330
                sbb     bx, ds:DAT340
                sub     cx, ds:DAT300
                sbb     bx, 0
                mov     si, cx
                mov     di, bx
                mov     bx, offset STR160
                call    LAB350
                call    LAB490
                jmp     LAB140

LAB350:
                push    bx
                xor     ax, ax
                mov     bx, ax
                mov     bp, ax
                mov     cx, 20h

LAB360:
                shl     si, 1
                rcl     di, 1
                xchg    ax, bp
                call    LAB420
                xchg    ax, bp
                xchg    ax, bx
                call    LAB420
                xchg    ax, bx
                adc     al, 0
                loop    LAB360
                mov     cx, 1810h
                xchg    ax, dx
                call    LAB400
                xchg    ax, bx
                call    LAB380
                xchg    ax, bp
                call    LAB380
                pop     dx
                cmp     dx, 0
                jz      short LAB370
                call    LAB480

LAB370:
                ret

LAB380:
                push    ax
                mov     dl, ah
                call    LAB390
                pop     dx

LAB390:
                mov     dh, dl
                shr     dl, 1
                shr     dl, 1
                shr     dl, 1
                shr     dl, 1
                call    LAB400
                mov     dl, dh

LAB400:
                and     dl, 0Fh
                jz      short LAB410
                mov     cl, 0

LAB410:
                dec     ch
                and     cl, ch
                or      dl, 30h
                sub     dl, cl
                mov     ah, 2
                int     21h             ; DOS - DISPLAY OUTPUT
                                        ; DL = character to send to standard output
                ret

LAB420:
                adc     al, al
                daa
                xchg    al, ah
                adc     al, al
                daa
                xchg    al, ah

LAB430:
                ret

LAB440:
                shr     cx, 1
                jb      short LAB430
                shl     ax, 1
                rcl     dx, 1
                jmp     short LAB440

LAB450:
                mov     ax, ds:DAT130
                call    LAB460
                mov     ax, ds:DAT140
                call    LAB460
                mov     ax, ds:DAT150

LAB460:
                xor     dx, dx
                div     ds:DAT290
                or      dx, dx
                jz      short LAB470
                inc     ax

LAB470:
                mul     ds:DAT290
                add     ds:DAT300, ax
                ret

LAB480:
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                ret

LAB490:
                call    LAB520
                jb      short LAB560
                ; call    $+3
                call    LAB500

LAB500:
                mov     dx, offset STR040
                call    LAB480
                ret

LAB510:
                mov     ax, 0C08h
                int     21h             ; DOS - CLEAR KEYBOARD BUFFER
                                        ; AL must be 01h, 06h, 07h, 08h, or 0Ah.
                ret

LAB520:
                mov     dx, offset STR030
                call    LAB480
                mov     ax, 0C01h
                int     21h             ; DOS - CLEAR KEYBOARD BUFFER
                                        ; AL must be 01h, 06h, 07h, 08h, or 0Ah.
                and     al, 0DFh
                cmp     al, "Y"
                jz      short LAB540
                cmp     al, "N"
                jz      short LAB530
                call    LAB500
                jmp     short LAB520

LAB530:
                stc

LAB540:
                ret

LAB550:
                call    LAB480

LAB560:
                int     20h             ; DOS - PROGRAM TERMINATION
                                        ; returns to DOS--identical to INT 21/AH=00h

LAB570:
                mov     ah, 19h
                int     21h             ; DOS - GET DEFAULT DISK NUMBER
                add     al, "A"
                mov     byte ptr ds:[STR070+25], al
                mov     dx, offset STR070
                call    LAB480
                mov     dx, offset STR010
                call    LAB480
                call    LAB510
                call    LAB500
                ret

LAB580:
                mov     dl, ds:DRIVE
                inc     dl
                mov     ah, 1Ch
                int     21h             ; DOS - GET ALLOCATION TABLE INFORMATION FOR SPECIFIC DRIVE
                                        ; DL = drive number to check (0=default, 1=A, etc)
                or      bx, bx
                jz      short LAB590
                ret

LAB590:
                mov     dx, offset STR050
                jmp     short LAB550

LAB600:
                call    LAB580
                mov     cs:DAT260, bx
                mov     cs:DAT270, ds
                mov     al, byte ptr cs:FATID
                or      al, 0F8h
                mov     ah, 0FFh
                mov     di, cs:DAT260
                mov     [di], ax
                mov     [di+2], ah
                mov     byte ptr [di-1], 1
                push    es
                pop     ds
                mov     ah, 0Dh
                int     21h             ; DOS - DISK RESET
                call    LAB580
                push    ds
                push    es
                pop     ds
                mov     ds:DAT310, dx
                mov     ds:DAT290, cx
                mov     ah, 0
                mov     ds:DAT280, ax
                shr     dx, 1
                jnb     short LAB610
                inc     dx

LAB610:
                add     dx, ds:DAT310
                xor     ax, ax
                pop     es
                mov     cx, dx
                mov     di, ds:DAT260
                add     di, 3
                rep stosb
                mov     di, ds:DAT260
                mov     byte ptr es:[di-1], 1
                push    ds
                pop     es
                mov     al, ds:DRIVE
                inc     al
                mov     ds:DAT230, al
                mov     ah, 13h
                mov     dx, offset DAT220
                int     21h             ; DOS - DELETE FILE via FCB
                                        ; DS:DX -> FCB with filename field filled with
                                        ; template for  deletion ('?' wildcard allowed, but not '*')
                                        ; Return: AL = 00h file found, FFh file not found
                ret

LAB620:
                mov     ds:DAT160, 0FFFFh
                mov     ax, ds:2
                mov     cx, cs
                sub     ax, cx
                cmp     ax, 1000h
                jge     short LAB630
                mov     cl, 4
                shl     ax, cl
                mov     ds:DAT160, ax

LAB630:
                mov     ah, 0Fh
                mov     dx, offset DAT190
                int     21h             ; DOS - OPEN DISK FILE
                                        ; DS:DX -> FCB
                                        ; Return: AL = 00h file found, FFh file not found
                or      al, al
                jz      short LAB650

LAB640:
                stc
                ret

LAB650:
                mov     ax, word ptr ds:[DAT200+20]
                mov     ds:DAT060, ax
                mov     ax, word ptr ds:[DAT200+22]
                mov     ds:DAT100, ax
                mov     ah, 0Fh
                mov     dx, offset DAT170
                int     21h             ; DOS - OPEN DISK FILE
                                        ; DS:DX -> FCB
                                        ; Return: AL = 00h file found, FFh file not found
                or      al, al
                jnz     short LAB640
                mov     ax, word ptr ds:[DAT180+20]
                mov     ds:DAT050, ax
                mov     ax, word ptr ds:[DAT180+22]
                mov     ds:DAT090, ax
                mov     ah, 1Ah
                mov     dx, word ptr ds:FREESPACE
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     word ptr ds:[DAT200+33], 0
                mov     word ptr ds:[DAT200+35], 0
                mov     word ptr ds:[DAT180+33], 0
                mov     word ptr ds:[DAT180+35], 0
                mov     word ptr ds:[DAT200+14], 1
                mov     word ptr ds:[DAT180+14], 1
                mov     ah, 27h
                mov     dx, offset DAT190
                mov     cx, ds:DAT160
                sub     cx, word ptr ds:FREESPACE
                mov     ds:DAT160, cx
                int     21h             ; DOS - RANDOM BLOCK READ
                                        ; DS:DX -> FCB
                                        ; CX = number of records to be read
                cmp     al, 1
                jnz     short LAB660
                mov     word ptr ds:[DAT200+37], cx
                mov     ds:DAT140, cx
                mov     ah, 1Ah
                mov     dx, word ptr ds:FREESPACE
                add     dx, ds:DAT140
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     ah, 27h
                mov     dx, offset DAT170
                mov     cx, ds:DAT160
                sub     cx, ds:DAT140
                mov     ds:DAT160, cx
                int     21h             ; DOS - RANDOM BLOCK READ
                                        ; DS:DX -> FCB
                                        ; CX = number of records to be read
                cmp     al, 1
                jnz     short LAB660
                mov     word ptr ds:[DAT180+37], cx
                mov     ds:DAT130, cx
                ret

LAB660:
                jmp     LAB710

LAB670:
                mov     al, ds:DRIVE
                inc     al
                mov     ds:DAT200, al
                mov     ds:DAT180, al
                mov     dx, word ptr ds:FREESPACE
                add     dx, ds:DAT140
                mov     ah, 1Ah
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     ax, ds:DAT050
                mov     ds:DAT040, ax
                mov     ax, ds:DAT090
                mov     ds:DAT080, ax
                mov     bx, offset DAT170
                call    LAB680
                mov     ax, ds:DAT060
                mov     ds:DAT040, ax
                mov     ax, ds:DAT100
                mov     ds:DAT080, ax
                mov     dx, word ptr ds:FREESPACE
                mov     ah, 1Ah
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     bx, offset DAT190
                call    LAB680
                ret

LAB680:
                xor     ax, ax
                mov     [bx+28h], ax
                mov     [bx+2Ah], ax
                mov     dx, bx
                mov     ah, 16h
                int     21h             ; DOS - CREATE A DISK FILE
                                        ; DS:DX -> FCB
                mov     word ptr [bx+15h], 1
                mov     ah, 28h
                mov     cx, [bx+2Ch]
                int     21h             ; DOS - RANDOM BLOCK WRITE
                                        ; DS:DX -> FCB
                                        ; CX = number of records to be written
                                        ; if zero, truncate file to current random file position
                mov     ax, ds:DAT040
                mov     [bx+1Bh], ax
                mov     ax, ds:DAT080
                mov     [bx+1Dh], ax
                mov     ah, 10h
                int     21h             ; DOS - CLOSE DISK FILE
                                        ; DS:DX -> FCB
                                        ; Return: AL = 00h directory update successful
                                        ; FFh file not found in directory
                ret

LAB690:
                mov     dx, word ptr ds:FREESPACE
                add     dx, ds:DAT140
                add     dx, ds:DAT130
                mov     ds:DAT120, dx
                mov     ah, 1Ah
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     dx, offset DAT210
                mov     ah, 0Fh
                int     21h             ; DOS - OPEN DISK FILE
                                        ; DS:DX -> FCB
                                        ; Return: AL = 00h file found, FFh file not found
                or      al, al
                jnz     short LAB700
                mov     word ptr ds:[DAT210+33], 0
                mov     word ptr ds:[DAT210+35], 0
                mov     word ptr ds:[DAT210+14], 1
                mov     cx, ds:DAT160
                sub     cx, ds:DAT130
                mov     ax, word ptr ds:[DAT210+20]
                mov     ds:DAT070, ax
                mov     ax, word ptr ds:[DAT210+22]
                mov     ds:DAT110, ax
                mov     ah, 27h
                int     21h             ; DOS - RANDOM BLOCK READ
                                        ; DS:DX -> FCB
                                        ; CX = number of records to be read
                cmp     al, 1
                jnz     short LAB710
                mov     ds:DAT150, cx
                ret

LAB700:
                jmp     short LAB730
                nop

LAB710:
                mov     dx, offset STR120
                call    LAB480
                jmp     LAB560

LAB720:
                mov     al, ds:DRIVE
                inc     al
                mov     ds:DAT210, al
                mov     dx, offset DAT210
                mov     ah, 16h
                int     21h             ; DOS - CREATE A DISK FILE
                                        ; DS:DX -> FCB
                or      al, al
                jnz     short LAB730
                mov     dx, ds:DAT120
                mov     ah, 1Ah
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     word ptr ds:[DAT210+33], 0
                mov     word ptr ds:[DAT210+35], 0
                mov     word ptr ds:[DAT210+14], 1
                mov     dx, offset DAT210
                mov     ah, 28h
                mov     cx, ds:DAT150
                int     21h             ; DOS - RANDOM BLOCK WRITE
                                        ; DS:DX -> FCB
                                        ; CX = number of records to be written
                                        ; if zero, truncate file to current random file position
                or      al, al
                jnz     short LAB730
                mov     ax, ds:DAT070
                mov     word ptr ds:[DAT210+20], ax
                mov     ax, ds:DAT110
                mov     word ptr ds:[DAT210+22], ax
                mov     dx, offset DAT210
                mov     ah, 10h
                int     21h             ; DOS - CLOSE DISK FILE
                                        ; DS:DX -> FCB
                                        ; Return: AL = 00h directory update successful
                                        ; FFh file not found in directory
                or      al, al
                jnz     short LAB730
                ret

LAB730:
                stc
                ret


STR000          db "Insert new diskette for drive x:",13,10,"$"
STR010          db "and strike any key when ready$"
STR020          db "System transferred",13,10,"$"
STR030          db "Format another (Y/N)?$"
STR040          db 13,10,"$"
STR050          db "Invalid drive specification$"
STR060          db "Invalid parameter$"
STR070          db "Insert DOS disk in drive x:",13,10,"$"
STR080          db "Format failure",13,10,13,10,"$"
STR090          db "Fatal format error",13,10,"$"
STR100          db "Disk unsuitable for system disk",13,10,"$"
STR110          db "Track 0 bad - disk unusable",13,10,"$"
STR120          db "Insufficient memory for system transfer",13,10,"$"
STR130          db " bytes total disk space",13,10,"$"
STR140          db " bytes used by system",13,10,"$"
STR150          db " bytes in bad sectors",13,10,"$"
STR160          db " bytes available on disk",13,10,13,10,"$"
DAT000          db 0
DAT010          db 0
DRIVE           db 0
DAT030          db 0
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
DAT140          dw 0
DAT150          dw 0
DAT160          dw 0
DAT170          db -1,0,0,0,0,0,6
DAT180          db 0,"IO      SYS",4
                db 26 dup(?)
DAT190          db -1,0,0,0,0,0,6
DAT200          db 0,"MSDOS   SYS",4
                db 26 dup(?)
DAT210          db 0,"COMMAND COM"
                db 26 dup(?)
DAT220          db -1,0,0,0,0,0,6
DAT230          db 0,"???????????"
                db 26 dup(?)
SWITCHMAP       dw 0
DAT250          dw 0
DAT260          dw 0
DAT270          dw 0
DAT280          dw 0
DAT290          dw 0
DAT300          dw 0
DAT310          dw 0
DAT320          dw 0
DAT330          dw 0
DAT340          dw 0
DAT350          dw 0
DAT360          dw 0
                db 60h dup(?)
STACK           label byte
CODE            ends


                end     LAB000
