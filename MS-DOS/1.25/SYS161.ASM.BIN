TITLE   MS-DOS SYS Program
; SYS - Copies system programs IBMBIO.COM/IO.SYS and IBMDOS.COM/MSDOS.SYS
;   1.6     05/21/82  Added rev number message
;   1.61    06/04/82  Allow SYS to blank disk TimP at SCP

CODE            segment public 'CODE'
                assume cs:CODE, ds:CODE, es:CODE

                org     100h

LAB000:
                push    ax
                mov     dx, offset STR000
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                pop     ax
                jmp     short LAB040

LAB010:
                mov     dx, offset STR020
                jmp     LAB060

LAB020:
                mov     dx, offset STR010
                jmp     LAB060

LAB030:
                mov     al, DAT000
                add     al, 40h
                mov     byte ptr [STR030+28], al
                mov     dx, offset STR030
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                mov     ax, 0C08h
                int     21h             ; DOS - CLEAR KEYBOARD BUFFER
                                        ; AL must be 01h, 06h, 07h, 08h, or 0Ah.
                xor     al, al

LAB040:
                cmp     byte ptr ds:5Dh, 20h
                jnz     short LAB010
                cmp     al, 0FFh
                jz      short LAB020
                cmp     byte ptr ds:5Ch, 0
                jz      short LAB020
                mov     ah, 19h
                int     21h             ; DOS - GET DEFAULT DISK NUMBER
                inc     al
                mov     DAT000, al
                cmp     ds:5Ch, al
                jz      short LAB020
                mov     ah, 0Fh
                mov     dx, offset DAT010
                int     21h             ; DOS - OPEN DISK FILE
                                        ; DS:DX -> FCB
                                        ; Return: AL = 00h file found, FFh file not found
                or      al, al
                jnz     short LAB030
                mov     dx, offset DAT030
                mov     ah, 0Fh
                int     21h             ; DOS - OPEN DISK FILE
                                        ; DS:DX -> FCB
                                        ; Return: AL = 00h file found, FFh file not found
                or      al, al
                jnz     short LAB030
                mov     ah, 1Ah
                mov     dx, offset 2000h+STACK+1
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     ax, 1
                mov     word ptr [DAT020+14], ax
                mov     word ptr [DAT040+14], ax
                mov     bx, offset DAT010
                mov     cx, 8000h
                call    LAB090
                mov     dx, offset STACK+1
                mov     ah, 1Ah
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     bx, offset DAT030
                mov     cx, 2000h
                call    LAB090
                mov     al, ds:5Ch
                mov     DAT040, al
                mov     DAT020, al
                mov     DAT060, al
                mov     ah, 1Ah
                mov     dx, 80h
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     ah, 11h
                mov     dx, offset DAT050
                int     21h             ; DOS - SEARCH FIRST USING FCB
                                        ; DS:DX -> FCB
                or      al, al
                jnz     short LAB050
                mov     dl, ds:5Ch
                mov     ah, 1Ch
                int     21h             ; DOS - GET ALLOCATION TABLE INFORMATION FOR SPECIFIC DRIVE
                                        ; DL = drive number to check (0=default, 1=A, etc)
                push    cs
                pop     ds
                mov     ah, 0
                mul     cx
                xchg    ax, cx
                mov     bx, offset DAT010
                call    LAB100
                jnz     short LAB080
                mov     bx, offset DAT030
                call    LAB100
                ja      short LAB080

LAB050:
                mov     dx, offset 2000h+STACK+1
                mov     bx, offset DAT010
                call    LAB110
                mov     dx, offset STACK+1
                mov     bx, offset DAT030
                call    LAB110
                mov     dx, offset STR060

LAB060:
                mov     ah, 9
                int     21h             ; DOS - PRINT STRING
                                        ; DS:DX -> string terminated by "$"
                int     20h             ; DOS - PROGRAM TERMINATION
                                        ; returns to DOS--identical to INT 21/AH=00h

LAB070:
                mov     dx, offset STR040
                jmp     short LAB060

LAB080:
                mov     dx, offset STR050
                jmp     short LAB060

LAB090:
                mov     ah, 27h
                mov     dx, bx
                int     21h             ; DOS - RANDOM BLOCK READ
                                        ; DS:DX -> FCB
                                        ; CX = number of records to be read
                mov     [bx+30h], cx
                mov     ax, [bx+1Bh]
                mov     [bx+2Ch], ax
                mov     ax, [bx+1Dh]
                mov     [bx+2Eh], ax
                ret

LAB100:
                mov     ah, 0Fh
                mov     dx, bx
                int     21h             ; DOS - OPEN DISK FILE
                                        ; DS:DX -> FCB
                                        ; Return: AL = 00h file found, FFh file not found
                or      al, al
                jnz     short LAB070
                mov     ax, [bx+17h]
                xor     dx, dx
                add     ax, cx
                dec     ax
                div     cx
                push    ax
                mov     ax, [bx+30h]
                add     ax, cx
                dec     ax
                xor     dx, dx
                div     cx
                pop     dx
                cmp     ax, dx
                ret

LAB110:
                mov     ah, 1Ah
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     dx, bx
                mov     ah, 16h
                int     21h             ; DOS - CREATE A DISK FILE
                                        ; DS:DX -> FCB
                xor     ax, ax
                mov     [bx+28h], ax
                mov     [bx+2Ah], ax
                inc     ax
                mov     [bx+15h], ax
                mov     ah, 28h
                mov     cx, [bx+30h]
                int     21h             ; DOS - RANDOM BLOCK WRITE
                                        ; DS:DX -> FCB
                                        ; CX = number of records to be written
                                        ; if zero, truncate file to current random file position
                mov     ax, [bx+2Ch]
                mov     [bx+1Bh], ax
                mov     ax, [bx+2Eh]
                mov     [bx+1Dh], ax
                mov     ah, 10h
                int     21h             ; DOS - CLOSE DISK FILE
                                        ; DS:DX -> FCB
                                        ; Return: AL = 00h directory update successful
                                        ; FFh file not found in directory
                ret


STR000          db 13,10,"Sys version 1.6",13,10,13,10,"$"
STR010          db "Invalid drive specification$"
STR020          db "Invalid parameter$"
STR030          db "Insert system disk in drive A",13,10,"and strike any key when ready",13,10,"$"
STR040          db "No room for system on destination disk$"
STR050          db "Incompatible system size$"
STR060          db "System transferred$"
DAT000          db 0
DAT010          db -1,0,0,0,0,0,6
DAT020          db 0,"IO      SYS"
                db 31 dup(0)
DAT030          db -1,0,0,0,0,0,6
DAT040          db 0,"MSDOS   SYS"
                db 31 dup(0)
DAT050          db -1,0,0,0,0,0,6
DAT060          db 0,"???????????"
                db 21 dup(0)
STACK           label byte
CODE            ends


                end     LAB000
