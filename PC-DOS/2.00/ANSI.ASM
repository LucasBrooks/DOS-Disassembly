ESCCHR  EQU     1BH
BRKADR  EQU     6CH
ASNMAX  EQU     0C0H

CODE    SEGMENT BYTE PUBLIC 'CODE'
        ASSUME  CS:CODE,DS:CODE,ES:CODE,SS:CODE

CONDEV  DW      -1
        DW      -1
        DW      8013H
        DW      OFFSET STRATEGY
        DW      OFFSET ENTRY
        DB      "CON     "

CONTBL  DW      OFFSET CON$INIT
        DW      OFFSET EXIT
        DW      OFFSET EXIT
        DW      OFFSET ERR$EXIT
        DW      OFFSET CON$READ
        DW      OFFSET CON$RDND
        DW      OFFSET EXIT
        DW      OFFSET CON$FLSH
        DW      OFFSET CON$WRIT
        DW      OFFSET CON$WRIT
        DW      OFFSET EXIT
        DW      OFFSET EXIT

IODAT   STRUC
CMDLEN  DB      ?                       ; LENGTH OF THIS COMMAND
UNIT    DB      ?                       ; SUB UNIT SPECIFIER
CMD     DB      ?                       ; COMMAND CODE
STATUS  DW      ?                       ; STATUS
        DB      8 DUP (?)
MEDIA   DB      ?                       ; MEDIA DESCRIPTOR
TRANS   DD      ?                       ; TRANSFER ADDRESS
COUNT   DW      ?                       ; COUNT OF BLOCKS OR CHARACTERS
START   DW      ?                       ; FIRST BLOCK TO TRANSFER
IODAT   ENDS

CMDTABL DB      'A'
        DW      OFFSET CUU              ; cursor up
        DB      'B'
        DW      OFFSET CUD              ; cursor down
        DB      'C'
        DW      OFFSET CUF              ; cursor forward
        DB      'D'
        DW      OFFSET CUB              ; cursor back
        DB      'H'
        DW      OFFSET CUP              ; cursor position
        DB      'J'
        DW      OFFSET ED               ; erase display
        DB      'K'
        DW      OFFSET EL               ; erase line
        DB      'R'
        DW      OFFSET CPR              ; cursor postion report
        DB      'f'
        DW      OFFSET CUP              ; cursor position
        DB      'h'
        DW      OFFSET SM               ; set mode
        DB      'l'
        DW      OFFSET RM               ; reset mode
        DB      'm'
        DW      OFFSET SGR              ; select graphics rendition
        DB      'n'
        DW      OFFSET DSR              ; device status report
        DB      'p'
        DW      OFFSET KEYASN           ; key assignment
        DB      's'
        DW      OFFSET PSCP             ; save cursor postion
        DB      'u'
        DW      OFFSET PRCP             ; restore cursor position
        DB      0

GRMODE  DB      00,00000000B,00000111B
        DB      01,11111111B,00001000B
        DB      04,11111000B,00000001B
        DB      05,11111111B,10000000B
        DB      07,11111000B,01110000B
        DB      08,10001000B,00000000B
        DB      30,11111000B,00000000B
        DB      31,11111000B,00000100B
        DB      32,11111000B,00000010B
        DB      33,11111000B,00000110B
        DB      34,11111000B,00000001B
        DB      35,11111000B,00000101B
        DB      36,11111000B,00000011B
        DB      37,11111000B,00000111B
        DB      40,10001111B,00000000B
        DB      41,10001111B,01000000B
        DB      42,10001111B,00100000B
        DB      43,10001111B,01100000B
        DB      44,10001111B,00010000B
        DB      45,10001111B,01010000B
        DB      46,10001111B,00110000B
        DB      47,10001111B,01110000B
        DB      0FFH

PTRSAV  DD      0

STRATP  PROC    FAR

STRATEGY:
        MOV     WORD PTR CS:PTRSAV,BX
        MOV     WORD PTR CS:PTRSAV+2,ES
        RET
STRATP  ENDP

ENTRY:
        PUSH    SI
        PUSH    AX
        PUSH    CX
        PUSH    DX
        PUSH    DI
        PUSH    BP
        PUSH    DS
        PUSH    ES
        PUSH    BX
        LDS     BX,CS:PTRSAV            ; Retrieve pointer to I/O Packet.
        MOV     CX,[BX.COUNT]           ; CX = Contains byte/sector count.
        MOV     AL,[BX.CMD]             ; Retrieve Command type. (1 => 11)
        CBW
        MOV     SI,OFFSET CONTBL
        ADD     SI,AX                   ; Compute entry pointer in dispatch table.
        ADD     SI,AX
        CMP     AL,11                   ; Verify that not more than 11 commands.
        JA      ERR$EXIT                ; Ah, well, error out.
        LES     DI,[BX.TRANS]           ; DI contains addess of Transfer address.
                                        ; ES contains segment.
        PUSH    CS
        POP     DS                      ; Data segment same as Code segment.
        JMP     WORD PTR [SI]

BUS$EXIT:                               ; Device busy exit.
        MOV     AH,00000011B            ; Set busy and done bits.
        JMP     SHORT ERR1

ERR$EXIT:
        MOV     AL,00000011B            ; Set unknown command error #.
        MOV     AH,10000001B            ; Set error and done bits.
        JMP     SHORT ERR1              ; Quick way out.

EXITP   PROC    FAR

EXIT:
        MOV     AH,00000001B            ; Set done bit for MSDOS.

ERR1:
        LDS     BX,CS:PTRSAV
        MOV     [BX.STATUS],AX          ; Save operation compete and status.
        POP     BX
        POP     ES
        POP     DS
        POP     BP
        POP     DI
        POP     DX
        POP     CX
        POP     AX
        POP     SI
        RET                             ; RESTORE REGS AND RETURN
EXITP   ENDP

BRKKY:
        MOV     CS:ALTAH,3              ; INDICATE BREAK KEY SET
        IRET

WRAP    DB      0                       ; 0 = WRAP, 1 = NO WRAP
ASNPTR  DW      4
STATE   DW      OFFSET S1
MODE    DB      3
MAXCOL  DB      79
COL     DB      0
ROW     DB      0
SAVCR   DW      0
INQ     DB      0
PRMCNT  LABEL   BYTE
PRMCNTW DW      0
KEYCNT  DB      0
KEYPTR  DW      OFFSET BUF
REPORT  DB      ESCCHR,"[00;00R",13     ; CURSOR POSTION REPORT BUFFER
ALTAH   DB      0                       ; Special key handling
ATTRW   LABEL   WORD
ATTR    DB      00000111B               ; CHARACTER ATTRIBUTE
BPAGE   DB      0                       ; BASE PAGE
BASE    DW      0B800H
SCREEN_SEG DW   0

CHROUT:
        CMP     AL,13
        JNZ     TRYLF
        MOV     [COL],0
        JMP     SHORT SETIT

TRYLF:
        CMP     AL,10
        JZ      LF
        CMP     AL,7
        JNZ     TRYBACK

TOROM:
        MOV     BX,[ATTRW]
        AND     BL,7
        MOV     AH,14
        INT     10H

RET5:
        RET

TRYBACK:
        CMP     AL,8
        JNZ     OUTCHR
        CMP     [COL],0
        JZ      RET5
        DEC     [COL]
        JMP     SHORT SETIT

OUTCHR:
        MOV     BX,[ATTRW]
        MOV     CX,1
        MOV     AH,9
        INT     10H
        INC     [COL]
        MOV     AL,[COL]
        CMP     AL,[MAXCOL]
        JBE     SETIT
        CMP     [WRAP],0
        JZ      OUTCHR1
        DEC     [COL]
        RET

OUTCHR1:
        MOV     [COL],0

LF:
        INC     [ROW]
        CMP     [ROW],25
        JB      SETIT
        MOV     [ROW],24
        CALL    SCROLL

SETIT:
        MOV     DH,ROW
        MOV     DL,COL
        MOV     BH,[BPAGE]
        MOV     AH,2
        INT     10H
        RET

SCROLL:
        MOV     AL,MODE
        CMP     AL,2
        JZ      TMPLAB1
        CMP     AL,3
        JZ      TMPLAB1
        MOV     AL,10
        JMP     SHORT TOROM

TMPLAB1:
        MOV     BH,[ATTR]
        MOV     BL,32
        MOV     BP,50H
        MOV     AX,[BASE]
        ADD     AX,[SCREEN_SEG]
        MOV     ES,AX
        MOV     DS,AX
        XOR     DI,DI
        MOV     SI,0A0H
        MOV     CX,780H
        CLD
        CMP     CS:[BASE],0B800H
        JZ      TMPLAB3
        REP     MOVSW
        MOV     AX,BX
        MOV     CX,BP
        REP     STOSW

TMPLAB2:
        PUSH    CS
        POP     DS
ASSUME  DS:CODE
        RET

TMPLAB3:
        MOV     DX,3DAH

TMPLAB4:
        IN      AL,DX
        TEST    AL,8
        JZ      TMPLAB4
        MOV     AL,25H
        MOV     DX,3D8H
        OUT     DX,AL
        REP     MOVSW
        MOV     AX,BX
        MOV     CX,BP
        REP     STOSW
        MOV     AL,29H
        MOV     DX,3D8H
        OUT     DX,AL
        JMP     SHORT TMPLAB2

;
; Console Read Routine
;

CON$READ:
        JCXZ    CON$EXIT

CON$LOOP:
        PUSH    CX                      ; SAVE COUNT
        CALL    CHRIN                   ; GET CHAR IN AL
        POP     CX
        STOSB                           ; STORE CHAR AT ES:DI
        LOOP    CON$LOOP

CON$EXIT:
        JMP     EXIT

CHRIN:
        XOR     AX,AX
        XCHG    AL,ALTAH                ; GET CHARACTER & ZERO ALTAH
        OR      AL,AL
        JNZ     KEYRET

INAGN:
        CMP     KEYCNT,0
        JNZ     KEY5
        XOR     AH,AH
        INT     16H
        CALL    SCAN                    ; check for redefinition
        JNZ     ALT10                   ; IF NO MATCH JUST RETURN IT
        DEC     CX
        DEC     CX
        INC     BX
        INC     BX
        OR      AL,AL                   ; check whether keypacket is an extended one
        JNZ     TMPLAB6
        DEC     CX
        INC     BX

TMPLAB6:
        MOV     KEYCNT,CL
        MOV     KEYPTR,BX
        CALL    KEY5

ALT10:
        OR      AX,AX                   ; Check for non-key after BREAK
        JZ      INAGN
        OR      AL,AL                   ; SPECIAL CASE?
        JNZ     KEYRET
        MOV     ALTAH,AH                ; STORE SPECIAL KEY

KEYRET:
        RET

KEY5:
        MOV     BX,KEYPTR               ; GET A KEY FROM TRANSLATION TABLE
        MOV     AX,WORD PTR [BX]
        DEC     KEYCNT
        INC     BX
        OR      AL,AL
        JNZ     KEY6
        INC     BX
        DEC     KEYCNT

KEY6:
        MOV     KEYPTR,BX
        RET

SCAN:
        MOV     BX,OFFSET BUF

KEYLP:
        MOV     CL,[BX]
        XOR     CH,CH
        OR      CX,CX
        JZ      NOTFND
        OR      AL,AL                   ; check whether extended keypacket
        JNZ     TMPLAB9
        CMP     AX,WORD PTR [BX+1]      ; yes...compare the word
        JMP     SHORT TMPLAB10

TMPLAB9:
        CMP     AL,BYTE PTR [BX+1]      ; no...compare the byte

TMPLAB10:
        JZ      MATCH
        ADD     BX,CX
        JMP     SHORT KEYLP

NOTFND:
        OR      BX,BX

MATCH:
        RET

;
; Keyboard Non Destructive Read, No Wait
;

CON$RDND:
        MOV     AL,[ALTAH]
        OR      AL,AL
        JNZ     RDEXIT
        CMP     [KEYCNT],0
        JZ      RD1
        MOV     BX,[KEYPTR]
        MOV     AL,BYTE PTR [BX]
        JMP     SHORT RDEXIT

RD1:
        MOV     AH,1
        INT     16H
        JZ      CONBUS
        OR      AX,AX
        JNZ     RD2
        MOV     AH,0
        INT     16H
        JMP     SHORT CON$RDND

RD2:
        CALL    SCAN
        JNZ     RDEXIT
        MOV     AL,BYTE PTR [BX+2]
        CMP     BYTE PTR [BX+1],0
        JNZ     RDEXIT
        MOV     AL,BYTE PTR [BX+3]

RDEXIT:
        LDS     BX,PTRSAV
ASSUME  DS:NOTHING
        MOV     [BX.MEDIA],AL

EXVEC:
        JMP     EXIT

CONBUS:
        JMP     BUS$EXIT

;
; Keyboard Flush Routine
;

CON$FLSH:
        MOV     DS:[ALTAH],0               ; Clear out holding buffer
        MOV     DS:[KEYCNT],0
        PUSH    DS
        XOR     BP,BP
        MOV     DS,BP
ASSUME  DS:CODE
        MOV     BYTE PTR DS:[41AH],1EH
        MOV     BYTE PTR DS:[41CH],1EH
        POP     DS
ASSUME  DS:NOTHING
        JMP     SHORT EXVEC

;
; Console Write Routine
;

CON$WRIT:
        JCXZ    EXVEC

CON$LP:
        MOV     AL,ES:[DI]              ; GET CHAR
        INC     DI
        CALL    OUTC                    ; OUTPUT CHAR
        LOOP    CON$LP                  ; REPEAT UNTIL ALL THROUGH
        JMP     SHORT EXVEC

COUT:
        STI
        PUSH    DS
        PUSH    CS
        POP     DS
        CALL    OUTC
        POP     DS
        IRET

OUTC:
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DI
        PUSH    ES
        PUSH    BP
        MOV     DS:[BASE],0B800H
        XCHG    AX,SI                   ; SAVE CHARACTER TO STUFF
        MOV     AX,40H                  ; POINT TO ROS BIOS
        MOV     DS,AX
        MOV     AX,DS:[49H]             ; AL=MODE, AH=MAX COL
        DEC     AH                      ; ANSI NEEDS 0-79 OR 0-39
        MOV     WORD PTR CS:[MODE],AX   ; SAVE MODE and MAX COL
        CMP     AL,7
        JNZ     SHORT NOT_BW
        MOV     CS:[BASE],0B000H

NOT_BW:
        MOV     AL,DS:[62H]             ; GET ACTIVE PAGE
        MOV     CS:[BPAGE],AL
        CBW
        ADD     AX,AX
        MOV     BX,AX
        MOV     AX,[BX+50H]             ; AL=COL, AH=ROW
        MOV     WORD PTR CS:[COL],AX    ; SAVE ROW and COLUMN
        MOV     AX,DS:[4EH]             ; GET START OF SCREEN SEG
        MOV     CL,4
        SHR     AX,CL                   ; CONVERT TO A SEGMENT
        PUSH    CS
        POP     DS
ASSUME  DS:CODE
        MOV     SCREEN_SEG,AX
        XCHG    AX,SI                   ; GET BACK CHARACTER IN AL
        CALL    VIDEO
        POP     BP
        POP     ES
        POP     DI
        POP     SI
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET

;
; Output Single Char In AL To Video Device
;

VIDEO:
        MOV     SI,OFFSET STATE
        JMP     WORD PTR [SI]

S2:
        CMP     AL,"["
        JZ      S22
        JMP     S1

S22:
        MOV     WORD PTR [SI],OFFSET S3
        XOR     BX,BX
        MOV     WORD PTR INQ,BX
        JMP     SHORT S3B

S3:
        CMP     AL,";"
        JNZ     S3C

S3A:    
        INC     PRMCNT

S3B:    
        CALL    GETPTR
        XOR     AX,AX
        MOV     WORD PTR [BX],AX        ; DEFAULT VALUE IS ZERO
        RET

S3C:
        CMP     AL,"0"
        JB      S3D
        CMP     AL,"9"
        JA      S3D
        CALL    GETPTR
        SUB     AL,"0"
        XCHG    AL,BYTE PTR [BX]
        MOV     AH,10
        MUL     AH                      ; *10
        ADD     BYTE PTR [BX],AL        ; MOVE IN DIGIT
        RET

S3D:
        CMP     AL,"="
        JZ      S3RET
        CMP     AL,"?"
        JZ      S3RET
        CMP     AL,'"'
        JZ      S3E
        CMP     AL,"'"
        JNZ     S7

S3E:
        MOV     WORD PTR [SI],OFFSET S4
        MOV     [INQ],AL

S3RET:
        RET

S4:
        CMP     AL,[INQ]                ; CHECK FOR STRING TERMINATOR
        JNZ     S4A
        DEC     PRMCNT                  ; TERMINATE STRING
        MOV     WORD PTR [SI],OFFSET S3
        RET

S4A:
        CALL    GETPTR
        MOV     BYTE PTR [BX],AL
        MOV     WORD PTR [SI],OFFSET S4
        JMP     SHORT S3A

; LOOK FOR ANSI COMMAND SPECIFIED IN AL

S7:
        MOV     BX,OFFSET CMDTABL-3

S7A:
        ADD     BX,3
        CMP     BYTE PTR [BX],0
        JZ      S1B
        CMP     BYTE PTR [BX],AL
        JNZ     S7A

S7B:
        MOV     AX,WORD PTR [BX+1]      ; AX = JUMP ADDRESS
        MOV     BX,OFFSET BUF
        INC     BX
        ADD     BX,ASNPTR               ; BX = PTR TO PARM LIST
        MOV     DL,BYTE PTR [BX]
        XOR     DH,DH                   ; DX = FIRST PARAMETER
        MOV     CX,DX
        OR      CX,CX
        JNZ     S7C
        INC     CX                      ; CX = DX, CX=1 IF DX=0

S7C:
        JMP     AX                      ; AL = COMMAND

S1:
        CMP     AL,ESCCHR               ; ESCAPE SEQUENCE?
        JNZ     S1B
        MOV     WORD PTR [SI],OFFSET S2
        RET

S1B:
        CALL    CHROUT

S1A:
        MOV     STATE,OFFSET S1
        RET

MOVCUR:
        CMP     BYTE PTR [BX],AH
        JZ      SETCUR
        ADD     BYTE PTR [BX],AL
        LOOP    MOVCUR

SETCUR:
        MOV     DX,WORD PTR COL
        XOR     BX,BX
        MOV     AH,2
        INT     10H
        JMP     SHORT S1A

CUP:
        CMP     CL,25
        JA      SETCUR
        MOV     AL,MAXCOL
        MOV     CH,BYTE PTR [BX+1]
        OR      CH,CH
        JZ      CUP1
        DEC     CH

CUP1:
        CMP     AL,CH
        JA      CUP2
        MOV     CH,AL

CUP2:
        XCHG    CL,CH
        DEC     CH
        MOV     WORD PTR COL,CX
        JMP     SHORT SETCUR

CUF:
        MOV     AH,MAXCOL
        MOV     AL,1

CUF1:
        MOV     BX,OFFSET COL
        JMP     SHORT MOVCUR

CUB:
        MOV     AX,0FFH
        JMP     SHORT CUF1

CUU:
        MOV     AX,0FFH

CUU1:
        MOV     BX,OFFSET ROW
        JMP     SHORT MOVCUR

CUD:
        MOV     AX,1801H
        JMP     SHORT CUU1

PSCP:
        MOV     AX,WORD PTR COL
        MOV     SAVCR,AX
        JMP     SHORT SETCUR

PRCP:
        MOV     AX,SAVCR
        MOV     WORD PTR COL,AX
        JMP     SHORT SETCUR

SGR:
        XOR     CX,CX
        XCHG    CL,PRMCNT
        CALL    GETPTR
        INC     CX

SGR1:
        MOV     AL,BYTE PTR [BX]
        PUSH    BX
        MOV     BX,OFFSET GRMODE

SGR2:
        MOV     AH,BYTE PTR [BX]
        ADD     BX,3
        CMP     AH,0FFH
        JZ      SGR3
        CMP     AH,AL
        JNZ     SGR2
        MOV     AX,WORD PTR [BX-2]
        AND     ATTR,AL
        OR      ATTR,AH

SGR3:
        POP     BX
        INC     BX
        LOOP    SGR1
        JMP     SETCUR

ED:
        XOR     CX,CX
        MOV     WORD PTR COL,CX
        MOV     DH,25

ERASE:
        MOV     DL,MAXCOL
        MOV     BH,ATTR
        MOV     AX,600H
        INT     10H
        JMP     SETCUR

EL:
        MOV     CX,WORD PTR COL
        MOV     DH,CH
        JMP     SHORT ERASE

BIN2ASC:
        MOV     DL,10
        INC     AL
        XOR     AH,AH
        DIV     DL
        ADD     AX,"00"
        RET

DSR:
        MOV     AL,ROW
        CALL    BIN2ASC
        MOV     WORD PTR REPORT+2,AX
        MOV     AL,COL
        CALL    BIN2ASC
        MOV     WORD PTR REPORT+5,AX
        MOV     [KEYCNT],9
        MOV     [KEYPTR],OFFSET REPORT

CPR:
        JMP     S1A

RM:
        MOV     CL,1
        JMP     SHORT SM1

SM:
        XOR     CX,CX

SM1:
        MOV     AL,DL
        CMP     AL,7
        JA      CPR
        JB      SETMODE
        MOV     WRAP,CL
        RET

SETMODE:
        MOV     AH,0                    ; WGR yes....set mode..
        INT     10H
        JMP     SHORT CPR

KEYASN:
        XOR     DX,DX
        XCHG    DL,PRMCNT               ; GET CHARACTER COUNT
        INC     DX
        INC     DX
        CALL    GETPTR
        MOV     AX,WORD PTR [BX]        ; GET CHARACTER TO BE ASSIGNED
        CALL    SCAN                    ; LOOK IT UP
        JNZ     KEYAS1
        MOV     DI,BX                   ; DELETE OLD DEFINITION
        SUB     ASNPTR,CX
        MOV     SI,DI
        ADD     SI,CX
        MOV     CX,OFFSET BUF+ASNMAX+8
        SUB     CX,SI
        CLD
        PUSH    ES                      ; SAVE USER'S ES
        PUSH    CS
        POP     ES                      ; SET UP ES ADDRESSABILITY
        REP     MOVSB
        POP     ES                      ; RESTORE ES

KEYAS1:
        CALL    GETPTR
        CMP     DL,3
        JB      KEYAS3
        MOV     BYTE PTR [BX-1],DL      ; SET LENGTH
        ADD     ASNPTR,DX               ; REMEMBER END OF LIST
        ADD     BX,DX

KEYAS3:
        MOV     BYTE PTR [BX-1],0
        MOV     STATE,OFFSET S1         ; RETURN
        RET

GETPTR:
        MOV     BX,ASNPTR
        INC     BX
        ADD     BX,PRMCNTW
        CMP     BX,ASNMAX+8
        JB      GET1
        DEC     PRMCNT
        JMP     SHORT GETPTR

GET1:
        ADD     BX,OFFSET BUF
        RET

BUF     DB      4,0,72H,16,0
        DB      ASNMAX+8-5 DUP (0)

CON$INIT:
        INT     11H
        AND     AL,00110000B
        CMP     AL,00110000B
        JNZ     ISCOLOR
        MOV     [BASE],0B000H           ; look for bw card

ISCOLOR:
        CMP     AL,00010000B            ; look for 40 col mode
        JA      SETBRK
        MOV     [MODE],0
        MOV     [MAXCOL],39

SETBRK:
        XOR     BX,BX
        MOV     DS,BX
        MOV     BX,BRKADR
        MOV     WORD PTR [BX],OFFSET BRKKY
        MOV     WORD PTR [BX+2],CS
        MOV     BX,29H*4
        MOV     WORD PTR [BX],OFFSET COUT
        MOV     WORD PTR [BX+2],CS
        LDS     BX,CS:PTRSAV
        MOV     WORD PTR [BX.TRANS],OFFSET CON$INIT
        MOV     WORD PTR [BX.TRANS+2],CS
        JMP     EXIT

        DB      33H DUP (0)

CODE    ENDS
        END

