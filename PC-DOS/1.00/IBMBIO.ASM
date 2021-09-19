; I/O System for PC-DOS version 1.00. Revised 7-22-81.
;
; Disassembled from IBMBIO.COM written by IBM. Label and constant
; names from Michal Necasek and Michael Steil.
;
; Use Seattle Computer Products 8086 Assembler 2.40 or later to
; assemble this file.


SECTSIZE        equ     200h            ; size of a sector
DOSSIZE         equ     10000           ; max size of IBMDOS.COM in bytes
PAUSEKEY        equ     7200h           ; scancode + charcode of PAUSE key
KEYBUFNEXT      equ     041Ah           ; next character in keyboard buffer
KEYBUFFREE      equ     041Ch           ; next free slot in keyboard buffer
LOGICALDRIVE    equ     0504h           ; linear address of logical drive byte
SEGDOSTEMP      equ     0E0h            ; segment in which DOS was loaded
SEGDOS          equ     0B1h            ; segment in which DOS will run
SEGBIO          equ     60h             ; segment in which BIO is running

                org     0

                jmp     INIT
                jmp     STATUS
                jmp     CONIN
                jmp     CONOUT
                jmp     PRINT
                jmp     AUXIN
                jmp     AUXOUT
                jmp     READ
                jmp     WRITE
                jmp     DSKCHG

                dw SEGDOS
                dw VERSION
VERSION         db "BIOS Version 1.00 22-Jul-81",0
NOPAPER         db 13,10,"Out of papeò",13,10,0
BADPRN          db 13,10,"Printer faulô",13,10,0
BADAUX          db 13,10,"Aux I/O erroò",13,10,0

STATUS:
                seg     cs
                mov     al, [LASTCHR]
                or      al, al
                jnz     STDONE
                push    dx
                xchg    ax, dx
                mov     ah, 1
                int     16h             ; KEYBOARD - CHECK BUFFER, DO NOT CLEAR
                                        ; Return: ZF clear if character in buffer
                                        ; AH = scan code, AL = character
                                        ; ZF set if no character in buffer
                jz      RESTORAH
                cmp     ax, PAUSEKEY
                jnz     RESTORAH
                mov     al, 10h
                or      al, al

RESTORAH:
                mov     ah, dh
                pop     dx

STDONE:
                ret     l

BREAK:
                seg     cs
                mov     b, [LASTCHR], 3

IRETINS:
                iret

DIVZERO:
                sti
                push    ax
                push    dx
                mov     dx, DIVOVER
                call    CONSTR
                pop     dx
                pop     ax
                int     23h             ; DOS - CONTROL "C" EXIT ADDRESS
                                        ; Return: return via ret  2 with CF set
                                        ; DOS will abort program with errorlevel 0
                                        ; else
                                        ; interrupted DOS call continues

IRET1:
                iret

DIVOVER         db 13,10,"Divide overflo÷",13,10,0

INAGAIN:
                xchg    ax, dx
                pop     dx

CONIN:
                mov     al, 0
                seg     cs
                xchg    al, [LASTCHR]
                or      al, al
                jnz     HAVECHR
                push    dx
                xchg    ax, dx
                mov     ah, 0
                int     16h             ; KEYBOARD - READ CHAR FROM BUFFER, WAIT IF EMPTY
                                        ; Return: AH = scan code, AL = character
                or      ax, ax
                jz      INAGAIN
                cmp     ax, PAUSEKEY
                jnz     NOXLAT
                mov     al, 10h

NOXLAT:
                cmp     al, 0
                jnz     GOTCHAR
                seg     cs
                mov     [LASTCHR], ah

GOTCHAR:
                mov     ah, dh
                pop     dx

HAVECHR:
                ret     l

CONOUT:
                push    bp
                push    ax
                push    bx
                push    si
                push    di
                mov     ah, 0Eh
                seg     cs              ; no use
                mov     bx, 7
                int     10h             ; - VIDEO - WRITE CHARACTER AND ADVANCE CURSOR (TTY WRITE)
                                        ; AL = character, BH = display page (alpha modes)
                                        ; BL = foreground color (graphics modes)
                pop     di
                pop     si
                pop     bx
                pop     ax
                pop     bp
                ret     l

PRINT:
                push    ax
                push    dx
                seg     cs
                mov     b, [PRTFLAG], 0

NXTPRCH:
                mov     dx, 0
                mov     ah, 0
                int     17h             ; PRINTER - OUTPUT CHARACTER
                                        ; AL = character, DX = printer port (0-3)
                                        ; Return: AH = status bits
                mov     dx, NOPAPER
                test    ah, 20h
                jnz     ERROUT
                mov     dx, BADPRN
                test    ah, 5
                jz      POPRET
                seg     cs
                xor     b, [PRTFLAG], 1
                jnz     NXTPRCH

ERROUT:
                call    CONSTR

POPRET:
                pop     dx
                pop     ax
                ret     l

CONSTR:
                xchg    dx, si

NEXTCS:
                seg     cs
                lodb
                and     al, 7Fh
                jz      CSDONE
                call    CONOUT, SEGBIO
                jp      NEXTCS


CSDONE:
                xchg    dx, si
                ret

AUXIN:
                push    dx
                push    ax
                mov     dx, 0
                mov     ah, 2
                int     14h             ; SERIAL I/O - RECEIVE CHARACTER
                                        ; DX = port number (0-3)
                                        ; Return: AL = character received, AH = RS-232 status code
                mov     dx, BADAUX
                test    ah, 0Eh
                jz      AINDONE
                call    CONSTR

AINDONE:
                pop     dx
                mov     ah, dh
                pop     dx
                ret     l

AUXOUT:
                push    ax
                push    dx
                mov     ah, 1
                mov     dx, 0
                int     14h             ; SERIAL I/O - TRANSMIT CHARACTER
                                        ; AL = character, DX = port number (0-3)
                                        ; Return: AH = RS-232 status code bits, AL = modem status bits
                test    ah, 80h
                jz      POPRET
                mov     dx, BADAUX
                jp      ERROUT

DSKCHG:
                mov     ah, 0
                ret     l

INIT:
                cli
                mov     ax, cs
                mov     ds, ax
                mov     ss, ax
                mov     sp, PRTFLAG
                sti
                xor     ah, ah
                int     13h             ; DISK - RESET DISK SYSTEM
                                        ; DL = drive (if bit 7 is set both hard disks and floppy disks reset)
                mov     al, 0A3h
                int     14h
                mov     ah, 1
                int     17h             ; PRINTER - INITIALIZE
                                        ; DX = printer port (0-3)
                                        ; Return: AH = status
                int     11h             ; EQUIPMENT DETERMINATION
                                        ; Return: AX = equipment flag bits
                and     ax, 0C0h
                mov     cx, 5
                shr     ax, cl
                add     ax, 2
                and     ax, 6
                jz      FOURFLP
                
                cmp     al, 2
                jnz     MANYFLP
                shl     ax
                mov     b, [SNGLDR], 1

MANYFLP:
                mov     bx, FLPLIST
                add     bx, ax
                mov     w, [bx], 0
                ; mov     w, [bx+2], 0
                db      0C7h,87h,02h,00h,00h,00h
                shr     ax
                mov     [FLPNUM], al

FOURFLP:
                push    ds
                mov     ax, 0
                mov     ds, ax
                mov     ax, 60h
                mov     [6Eh], ax
                mov     [6Ch], BREAK
                mov     [0], DIVZERO
                mov     [2], ax
                mov     bx, IRET1
                mov     [4], bx
                mov     [6], ax
                mov     [0Ch], bx
                mov     [0Eh], ax
                mov     [10h], bx
                mov     [12h], ax
                mov     ax, 50h
                mov     ds, ax
                mov     [0], 0
                push    es
                mov     ax, SEGDOS
                mov     es, ax
                mov     cx, DOSSIZE/2
                cld
                mov     ax, SEGDOSTEMP
                mov     ds, ax
                xor     di, di
                mov     si, di
                rep
                movsw
                pop     es
                pop     ds
                mov     si, FLPNUM
                call    0, SEGDOS
                sti
                mov     dx, 100h
                mov     ah, 1Ah
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                mov     cx, [6]
                sub     cx, 100h
                mov     bx, ds
                mov     ax, cs
                mov     ds, ax
                mov     dx, FCB
                mov     ah, 0Fh
                int     21h             ; DOS - OPEN DISK FILE
                                        ; DS:DX -> FCB
                                        ; Return: AL = 00h file found, FFh file not found
                or      al, al
                jnz     COMERR
                mov     [FCB+21h], 0
                mov     [FCB+23h], 0
                mov     [FCB+0Eh], 1
                mov     ah, 27h
                int     21h             ; DOS - RANDOM BLOCK READ
                                        ; DS:DX -> FCB
                                        ; CX = number of records to be read
                jcxz    COMERR
                cmp     al, 1
                jnz     COMERR
                mov     ds, bx
                mov     es, bx
                mov     ss, bx
                mov     sp, 40h
                xor     ax, ax
                push    ax
                mov     dx, [80h]
                mov     ah, 1Ah
                int     21h             ; DOS - SET DISK TRANSFER AREA ADDRESS
                                        ; DS:DX -> disk transfer buffer
                push    bx
                mov     ax, 100h
                push    ax
                ret     l


COMERR:
                mov     dx, BADCOM
                call    CONSTR

STALL:
                jpe     STALL

FCB             db 1,"COMMAND COÍ",0
                db 0
                dw 0
                dw 0
                dw 0
                dw 0
                dw 0
                dw 0
                dw 0
                dw 0
                dw 0
                db 0
                dw 0
                dw 0
BADCOM          db 13,10,"Bad or missing Command Interpreteò",13,10,0
FLPNUM          db 4
FLPLIST         dw PARAMS
                dw PARAMS
                dw PARAMS
                dw PARAMS
                dw 0
                dw 0
PARAMS          dw SECTSIZE
                db 1
                dw 1
                db 2
                dw 40h
                dw 140h
                ds INIT+512-$
PRTFLAG         db 0
LASTCHR         db 0
                db 0
SNGLDR          db 0

READ:
                mov     ah, 2
                jp      RWCMD

WRITE:
                mov     ah, 3

RWCMD:
                push    es
                push    ds
                push    ds
                pop     es
                push    cs
                pop     ds
                mov     [SAVSP], sp
                mov     [DSKOP], ah
                ; cmp     [SNGLDR], 1
                db      80h,3Eh
                dw      SNGLDR
                db      01h
                jnz     TWODRV
                push    ds
                xor     si, si
                mov     ds, si
                mov     ah, al
                xchg    ah, [LOGICALDRIVE]
                pop     ds
                cmp     al, ah
                jz      NOSWAP
                push    dx
                add     al, "A"
                mov     [CHGDSK+1Ch], al        ; drive letter
                mov     dx, CHGDSK
                call    CONSTR
                push    ds
                xor     bp, bp
                mov     ds, bp
                mov     b, [KEYBUFNEXT], 1Eh
                mov     b, [KEYBUFFREE], 1Eh
                pop     ds
                mov     ah, 0
                int     16h             ; KEYBOARD - READ CHAR FROM BUFFER, WAIT IF EMPTY
                                        ; Return: AH = scan code, AL = character
                pop     dx

NOSWAP:
                mov     al, 0

TWODRV:
                xchg    ax, dx
                mov     dh, 8
                div     ax, dh
                inc     ah
                xchg    al, ah
                xchg    ax, cx
                mov     [NUMSCT], ax
                mov     dh, 0
                mov     di, es
                shl     di
                shl     di
                shl     di
                shl     di
                add     di, bx
                add     di, SECTSIZE-1
                jb      NOCROS
                xchg    di, bx
                shr     bh
                mov     ah, 80h
                sub     ah, bh
                mov     bx, di
                cmp     ah, al
                jbe     NOMORE
                mov     ah, al

NOMORE:
                push    ax
                mov     al, ah
                call    DSKLOOP
                pop     ax
                sub     al, ah
                jz      DSKRET

NOCROS:
                dec     al
                push    ax
                cld
                push    bx
                push    es
                ; cmp     [DSKOP], 2
                db      80h,3Eh
                dw      DSKOP
                db      02h
                jz      DOREAD
                mov     si, bx
                push    cx
                mov     cx, SECTSIZE/2
                push    es
                pop     ds
                push    cs
                pop     es
                mov     di, INIT
                mov     bx, di
                rep
                movsw
                pop     cx
                push    cs
                pop     ds
                call    SCTRWONE
                pop     es
                pop     bx
                jp     MOREIO


DOREAD:
                mov     bx, INIT
                push    cs
                pop     es
                call    SCTRWONE
                mov     si, bx
                pop     es
                pop     bx
                mov     di, bx
                push    cx
                mov     cx, SECTSIZE/2
                rep
                movsw
                pop     cx

MOREIO:
                ; add     bh, 2
                db      80h,0C7h,02h
                pop     ax
                call    DSKLOOP

DSKRET:
                pop     ds
                pop     es
                clc
                ret     l

DSKLOOP:
                or      al, al
                jz      NOMORSCT
                mov     ah, 9
                sub     ah, cl
                cmp     ah, al
                jbe     DL1
                mov     ah, al

DL1:
                push    ax
                mov     al, ah
                call    SECTRW
                pop     ax
                sub     al, ah
                shl     ah
                add     bh, ah
                jp      DSKLOOP

DSKERR:
                xchg    ax, di
                mov     ah, 0
                int     13h             ; DISK - RESET DISK SYSTEM
                                        ; DL = drive (if bit 7 is set both hard disks and floppy disks reset)
                dec     si
                jz      XLATERR
                mov     ax, di
                cmp     ah, 80h
                jz      XLATERR
                pop     ax
                jp      DSKRETRY


XLATERR:
                push    cs
                pop     es
                mov     ax, di
                mov     al, ah
                mov     cx, 0Ah
                mov     di, BDSKERR
                repne
                scasb
                ; mov     al, [di+9]
                db      8Ah,85h,09h,00h
                ; mov     cx, NUMSCT
                db      8Bh,0Eh
                dw      NUMSCT
                ; mov     sp, SAVSP
                db      8Bh,26h
                dw      SAVSP
                pop     ds
                pop     es
                stc
                ret     l

SCTRWONE:
                mov     al, 1

SECTRW:
                mov     si, 5
                mov     ah, [DSKOP]

DSKRETRY:
                push    ax
                int     13h             ; DISK - VERIFY SECTORS
                                        ; AL = number of sectors to verify, CH = track, CL = sector
                                        ; DH = head, DL = drive
                                        ; Return: CF set on error, AH = status
                                        ; AL = number of sectors verified
                jb      DSKERR
                pop     ax
                sub     [NUMSCT], al
                add     cl, al
                ; cmp     cl, 8
                db      80h,0F9h,08
                jbe     NOMORSCT
                inc     ch
                mov     cl, 1

NOMORSCT:
                ret

CHGDSK          db 13,10,"Insert diskette for drive A: and strikå",13,10,"any key when readù",13,10,10,0
BDSKERR         db 80h
                db 40h
                db 20h
                db 10h
                db 9
                db 8
                db 4
                db 3
                db 2
                db 1
DDSKERR         db 2
                db 6
                db 0Ch
                db 4
                db 0Ch
                db 4
                db 8
                db 0
                db 0Ch
                db 0Ch
DSKOP           db 2
SAVSP           dw 0
NUMSCT          dw 0

                ds 256-($-(($/256)*256))
                ds 256
                dw 0C900h
                ds 128-2-1
                db 0                    ; end

                end

