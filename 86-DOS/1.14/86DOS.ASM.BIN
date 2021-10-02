; 86-DOS  High-performance operating system for the 8086  version 1.14
;       by Tim Paterson


; ****************** Revision History *************************
;          >> EVERY change must noted below!! <<
;
; 0.34 12/29/80 General release, updating all past customers
; 0.42 02/25/81 32-byte directory entries added
; 0.56 03/23/81 Variable record and sector sizes
; 0.60 03/27/81 Ctrl-C exit changes, including register save on user stack
; 0.74 04/15/81 Recognize I/O devices with file names
; 0.75 04/17/81 Improve and correct buffer handling
; 0.76 04/23/81 Correct directory size when not 2^N entries
; 0.80 04/27/81 Add console input without echo, Functions 7 & 8
; 1.00 04/28/81 Renumber for general release
; 1.01 05/12/81 Fix bug in `STORE'
; 1.10 07/21/81 Fatal error trapping, NUL device, hidden files, date & time,
;               RENAME fix, general cleanup
; 1.11 09/03/81 Don't set CURRENT BLOCK to 0 on open; fix SET FILE SIZE
; 1.12 10/09/81 Zero high half of CURRENT BLOCK after all (CP/M programs don't)
; 1.13 10/29/81 Fix classic "no write-through" error in buffer handling
;
; *************************************************************


; Interrupt Entry Points:

; INTBASE:      ABORT
; INTBASE+4:    COMMAND
; INTBASE+8:    BASE EXIT ADDRESS
; INTBASE+C:    CONTROL-C ABORT
; INTBASE+10H:  FATAL ERROR ABORT
; INTBASE+14H:  BIOS DISK READ
; INTBASE+18H:  BIOS DISK WRITE
; INTBASE+40H:  Long jump to CALL entry point

FALSE	EQU	0
TRUE	EQU	1

ESCCH   EQU     1BH
CANCEL  EQU     "X"-"@"         ;Cancel with Ctrl-X
NUMDEV  EQU     5               ;Number of I/O device names

MAXCALL EQU     36
MAXCOM  EQU     45
INTBASE EQU     80H
INTTAB  EQU     20H
ENTRYPOINTSEG   EQU     0CH
ENTRYPOINT      EQU     INTBASE+40H
CONTC   EQU     INTTAB+3
EXIT    EQU     INTBASE+8
LONGJUMP EQU    0EAH
LONGCALL EQU    9AH
MAXDIF  EQU     0FFFH
SAVEXIT EQU     10

; Field definition for FCBs

FNAME   EQU     0       ;Drive code and name
EXTENT  EQU     12
RECSIZ  EQU     14      ;Size of record (user settable)
FILSIZ  EQU     16      ;Size of file in bytes
DRVBP   EQU     18      ;BP for SEARCH FIRST and SEARCH NEXT
FDATE   EQU     20      ;Date of last writing
FTIME   EQU     22      ;Time of last writing
DEVID   EQU     22      ;Device ID number, bits 0-5
                        ;bit 7=0 for file, bit 7=1 for I/O device
                        ;If file, bit 6=0 if dirty
                        ;If I/O device, bit 6=0 if EOF (input)
FIRCLUS EQU     24      ;First cluster of file
LSTCLUS EQU     26      ;Last cluster accessed
CLUSPOS EQU     28      ;Position of last cluster accessed
NR      EQU     32      ;Next record
RR      EQU     33      ;Random record
FILDIRENT       EQU 22          ;Used only by SEARCH FIRST and SEARCH NEXT

; Description of 32-byte directory entry (same as returned by SEARCH FIRST
; and SEARCH NEXT, functions 17 and 18).
;
; Location      bytes   Description
;
;    0          11      File name and extension ( 0E5H if empty)
;   11           1      Attributes. Bits 1 or 2 make file hidden
;   12          10      Zero field (for expansion)
;   22           2      Time. Bits 0-4=seconds/2, bits 5-10=minute, 11-15=hour
;   24           2      Date. Bits 0-4=day, bits 5-8=month, bits 9-15=year-1980
;   26           2      First allocation unit ( < 4080 )
;   28           4      File size, in bytes (LSB first, 30 bits max.)
;
; The File Allocation Table uses a 12-bit entry for each allocation unit on
; the disk. These entries are packed, two for every three bytes. The contents
; of entry number N is found by 1) multiplying N by 1.5; 2) adding the result
; to the base address of the Allocation Table; 3) fetching the 16-bit word at
; this address; 4) If N was odd (so that N*1.5 was not an integer), shift the
; word right four bits; 5) mask to 12 bits (AND with 0FFF hex). Entry number
; zero is used as an end-of-file trap in the OS and as a flag for directory
; entry size (if SMALLDIR selected). Entry 1 is reserved for future use. The
; first available allocation unit is assigned entry number two, and even
; though it is the first, is called cluster 2. Entries greater than 0FF8H are
; end of file marks; entries of zero are unallocated. Otherwise, the contents
; of a FAT entry is the number of the next cluster in the file.


; Field definition for Drive Parameter Block

DEVNUM  EQU     0       ;I/O driver number
DRVNUM  EQU     0       ;Physical Unit number
SECSIZ  EQU     1       ;Size of physical sector in bytes
CLUSMSK EQU     3       ;Sectors/cluster - 1
CLUSSHFT EQU    4       ;Log2 of sectors/cluster
FIRFAT  EQU     5       ;Starting record of FATs
FATCNT  EQU     7       ;Number of FATs for this drive
MAXENT  EQU     8       ;Number of directory entries
FIRREC  EQU     10      ;First sector of first cluster
MAXCLUS EQU     12      ;Number of clusters on drive + 1
FATSIZ  EQU     14      ;Number of records occupied by FAT
FIRDIR  EQU     15      ;Starting record of directory
FAT     EQU     26      ;Pointer to start of FAT

DPBSIZ  EQU     20      ;Size of the structure in bytes


; BOIS entry point definitions

BIOSSEG EQU     60H

BIOSINIT        EQU     0       ;Reserve room for jump to init code
BIOSSTAT        EQU     3       ;Console input status check
BIOSIN          EQU     6       ;Get console character
BIOSOUT         EQU     9       ;Output console character
BIOSPRINT       EQU     12      ;Output to printer
BIOSAUXIN       EQU     15      ;Get byte from auxilliary
BIOSAUXOUT      EQU     18      ;Output byte to auxilliary
BIOSREAD        EQU     21      ;Disk read
BIOSWRITE       EQU     24      ;Disk write
BIOSDSKCHG      EQU     27      ;Dsik-change status
BIOSSETDATE     EQU     30      ;Set date
BIOSSETTIME     EQU     33      ;Set time
BIOSGETTIME     EQU     36      ;Get time and date
BIOSFLUSH       EQU     39      ;Clear console input buffer
BIOSMAPDEV      EQU     42      ;Dynamic disk table mapper

; Location of user registers relative user stack pointer

AXSAVE  EQU     0
BXSAVE  EQU     2
CXSAVE  EQU     4
DXSAVE  EQU     6
SISAVE  EQU     8
DISAVE  EQU     10
BPSAVE  EQU     12
DSSAVE  EQU     14
ESSAVE  EQU     16
IPSAVE  EQU     18
CSSAVE  EQU     20
FSAVE   EQU     22

; Start of code

        ORG     0
CODSTRT EQU     $
        JMP     DOSINIT

ESCTAB: 
        DB      "SC"     ;Copy one character from template
        DB      "VN"     ;Skip over one character in template
        DB      "TA"     ;Copy up to specified character
        DB      "WB"     ;Skip up to specified character
        DB      "UH"     ;Copy rest of template
        DB      "HH"     ;Kill line with no change in template (Ctrl-X)
        DB      "RM"     ;Cancel line and update template
        DB      "DD"     ;Backspace (same as Ctrl-H)
        DB      "P@"     ;Enter Insert mode
        DB      "QL"     ;Exit Insert mode
        DB      1BH,1BH  ;Escape sequence to represent escape character
        DB      ESCCH,ESCCH

ESCTABLEN EQU   $-ESCTAB
HEADER  DB      13,10,"86-DOS version 1.14"
        DB      13,10
        DB      "Copyright 1980,81 Seattle Computer Products, Inc.",13,10,"$"

QUIT:
        MOV     AH,0
        JP      SAVREGS

COMMAND: ;Interrupt call entry point
        CMP     AH,MAXCOM
        JBE     SAVREGS
BADCALL:
        MOV     AL,0
IRET:   IRET

ENTRY:  ;System call entry point and dispatcher
        POP     AX              ;IP from the long call at 5
        POP     AX              ;Segment from the long call at 5
        SEG     CS
        POP     [TEMP]          ;IP from the CALL 5
        PUSHF                   ;Start re-ordering the stack
        CLI
        PUSH    AX              ;Save segment
        SEG     CS
        PUSH    [TEMP]          ;Stack now ordered as if INT had been used
        CMP     CL,MAXCALL      ;This entry point doesn't get as many calls
        JA      BADCALL
        MOV     AH,CL
SAVREGS:
        PUSH    ES
        PUSH    DS
        PUSH    BP
        PUSH    DI
        PUSH    SI
        PUSH    DX
        PUSH    CX
        PUSH    BX
        PUSH    AX
        SEG     CS
        MOV     [SPSAVE],SP
        SEG     CS
        MOV     [SSSAVE],SS
        MOV     SP,CS
        MOV     SS,SP
REDISP:
        MOV     SP,IOSTACK
        STI                     ;Stack OK now
        MOV     BL,AH
        MOV     BH,0
        SHL     BX
        CLD
        CMP     AH,12
        JLE     SAMSTK
        MOV     SP,DSKSTACK
SAMSTK:
        SEG     CS
        CALL    [BX+DISPATCH]
LEAVE:
        CLI
        SEG     CS
        MOV     SP,[SPSAVE]
        SEG     CS
        MOV     SS,[SSSAVE]
        MOV     BP,SP
        MOV     B,[BP+AXSAVE],AL
        POP     AX
        POP     BX
        POP     CX
        POP     DX
        POP     SI
        POP     DI
        POP     BP
        POP     DS
        POP     ES
        IRET
; Standard Functions
DISPATCH DW     ABORT           ;0
        DW      CONIN
        DW      CONOUT
        DW      READER
        DW      PUNCH
        DW      LIST            ;5
        DW      RAWIO
        DW      RAWINP
        DW      IN
        DW      PRTBUF
        DW      BUFIN           ;10
        DW      CONSTAT
        DW      FLUSHKB
        DW      DSKRESET
        DW      SELDSK
        DW      OPEN            ;15
        DW      CLOSE
        DW      SRCHFRST
        DW      SRCHNXT
        DW      DELETE
        DW      SEQRD           ;20
        DW      SEQWRT
        DW      CREATE
        DW      RENAME
        DW      INUSE
        DW      GETDRV          ;25
        DW      SETDMA
        DW      GETFATPT
        DW      GETFATPTDL
        DW      GETRDONLY
        DW      SETATTRIB       ;30
        DW      GETDSKPT
        DW      USERCODE
        DW      RNDRD
        DW      RNDWRT
        DW      FILESIZE        ;35
        DW      SETRNDREC
; Extended Functions
        DW      SETVECT
        DW      NEWBASE
        DW      BLKRD
        DW      BLKWRT          ;40
        DW      MAKEFCB
        DW      GETDATE
        DW      SETDATE
        DW      GETTIME
        DW      SETTIME         ;45

INUSE:
GETIO:
SETIO:
GETFATPTDL:
GETRDONLY:
SETATTRIB:
USERCODE:
        MOV     AL,0
        RET

FLUSHKB:
        MOV     AH,AL
        CMP     AL,1
        JZ      REDISPJ
        CMP     AL,6
        JZ      REDISPJ
        CMP     AL,7
        JZ      REDISPJ
        CMP     AL,8
        JZ      REDISPJ
        CMP     AL,10
        JZ      REDISPJ
        MOV     AL,0
        RET

REDISPJ:JMP     REDISP

READER:
AUXIN:
        CALL    STATCHK
        CALL    BIOSAUXIN,BIOSSEG
        RET

PUNCH:
        MOV     AL,DL
AUXOUT:
        PUSH    AX
        CALL    STATCHK
        POP     AX
        CALL    BIOSAUXOUT,BIOSSEG
        RET


UNPACK:

; Inputs:
;       DS = CS
;       BX = Cluster number
;       BP = Base of drive parameters
;       SI = Pointer to drive FAT
; Outputs:
;       DI = Contents of FAT for given cluster
;       Zero set means DI=0 (free cluster)
; No other registers affected. Fatal error if cluster too big.

        CMP     BX,[BP+12]
        JA      HURTFAT
        LEA     DI,[SI+BX]
        SHR     BX
        MOV     DI,[DI+BX]
        JNC     HAVCLUS
        SHR     DI
        SHR     DI
        SHR     DI
        SHR     DI
        STC
HAVCLUS:
        RCL     BX
        AND     DI,0FFFH
        RET
HURTFAT:
        PUSH    AX
        MOV     AH,80H          ;Signal Bad FAT to INT 24H handler
        MOV     DI,0FFFH        ;In case INT 24H returns (it shouldn't)
        CALL    FATAL
        POP     AX              ;Try to ignore bad FAT
        RET


PACK:

; Inputs:
;       DS = CS
;       BX = Cluster number
;       DX = Data
;       SI = Pointer to drive FAT
; Outputs:
;       The data is stored in the FAT at the given cluster.
;       BX,DX,DI all destroyed
;       No other registers affected

        MOV     DI,BX
        SHR     BX
        ADD     BX,SI
        ADD     BX,DI
        SHR     DI
        MOV     DI,[BX]
        JNC     ALIGNED
        SHL     DX
        SHL     DX
        SHL     DX
        SHL     DX
        AND     DI,0FH
        JP      PACKIN
ALIGNED:
        AND     DI,0F000H
PACKIN:
        OR      DI,DX
        MOV     [BX],DI
        RET

DEVNAME:
        MOV     SI,IONAME       ;List of I/O devices with file names
        MOV     BX,0FF05H               ;BH = number of device names
LOOKIO:
        MOV     DI,NAME1
        MOV     CX,3                    ;All devices are 4 letters
        REPE
        CMPSB                           ;Check for name in list
        JZ      IOCHK                   ;If first 3 letters OK, check for the rest
        ADD     SI,CX                   ;Point to next device name
        DEC     BL
        JNZ     LOOKIO
CRET:
        STC                             ;Not found
        RET

IOCHK:
        DEC     BL
        MOV     CX,5            ;Check rest of name but not extension
        MOV     AL,20H
        REPE
        SCASB                   ;Make sure rest of name is blanks
        JNZ     CRET
RET1:   RET                     ;Zero set so CREATE works

GETFILE:
; Same as GETNAME except ES:DI points to FCB on successful return
        CALL    MOVNAME
        JC      RET1
        PUSH    DX
        PUSH    DS
        CALL    FINDNAME
        POP     ES
        POP     DI
RET2:   RET


GETNAME:

; Inputs:
;       DS,DX point to FCB
; Function:
;       Find file name in disk directory. First byte is
;       drive number (0=current disk). "?" matches any
;       character.
; Outputs:
;       Carry set if file not found
;       ELSE
;       Zero set if attributes match (always except when creating)
;       BP = Base of drive parameters
;       DS = CS
;       ES = CS
;       BX = Pointer into directory buffer
;       SI = Pointer to First Cluster field in directory entry
;       [DIRBUF] has directory record with match
;       [NAME1] has file name
; All other registers destroyed.

        CALL    MOVNAME
        JC      RET2            ;Bad file name?
FINDNAME:
        MOV     AX,CS
        MOV     DS,AX
        CALL    DEVNAME
        JNC     RET2
        CALL    STARTSRCH
CONTSRCH:
        CALL    GETENTRY
        JC      RET2
SRCH:
        CMP     B,[BX],0E5H
        JZ      NEXTENT
        MOV     SI,BX
        MOV     DI,NAME1
        MOV     CX,11
WILDCRD:
        REPE
        CMPSB
        JZ      FREE
        CMP     B,[DI-1],"?"
        JZ      WILDCRD
NEXTENT:
        CALL    NEXTENTRY
        JNC     SRCH
RET3:   RET

FREE:
        CMP     B,[BP+26],-1            ;Found a free entry before?
        JZ      RET3
 
FOUND:
;Check if attributes allow finding it
        MOV     AH,[ATTRIB]             ;Attributes of search
        NOT     AH
        AND     AH,[SI]                 ;Compare with attributes of file
        ADD     SI,15   
        AND     AH,6                    ;Only look at bits 1 and 2
        JZ      RET3
        TEST    B,[CREATING],-1         ;Pass back mismatch if creating
        JZ      NEXTENT                 ;Otherwise continue searching
        RET


GETENTRY:

; Inputs:
;       [LASTENT] has previously searched directory entry
; Function:
;       Locates next sequential directory entry in preparation for search
; Outputs:
;       Carry set if none
;       ELSE
;       AL = Current directory block
;       BX = Pointer to next directory entry in [DIRBUF]
;       DX = Pointer to first byte after end of DIRBUF
;       [LASTENT] = New directory entry number

        MOV     AX,[LASTENT]
        INC     AX                      ;Start with next entry
        CMP     AX,[BP+8]
        JAE     NONE
GETENT:
        MOV     [LASTENT],AX
        MOV     CL,4
        SHL     AX,CL
        XOR     DX,DX
        CMP     B,[BP+26],-1
        JZ      LAB000
        SHL     AX
        RCL     DX                      ;Account for overflow in last shift
LAB000:
        MOV     BX,[BP+1]
        AND     BL,255-31               ;Must be multiple of 32
        DIV     AX,BX
        MOV     BX,DX                   ;Position within sector
        MOV     AH,[BP+DEVNUM]          ;AL=Directory sector no.
        CMP     AX,[DIRBUFID]
        JZ      HAVDIRBUF
        PUSH    BX
        CALL    DIRREAD
        POP     BX
HAVDIRBUF:
        MOV     DX,DIRBUF
        ADD     BX,DX
        ADD     DX,[BP+1]
        RET

NEXTENTRY:

; Inputs:
;       Same as outputs of GETENTRY, above
; Function:
;       Update AL, BX, and [LASTENT] for next directory entry.
;       Carry set if no more.

        MOV     DI,[LASTENT]
        INC     DI
        CMP     DI,[BP+MAXENT]
        JAE     NONE
        MOV     [LASTENT],DI
        ADD     BX,32
        CMP     B,[BP+26],-1
        JNZ     LAB010
        SUB     BX,16
LAB010:
        CMP     BX,DX
        JB      HAVIT
        INC     AL                      ;Next directory sector
        PUSH    DX                      ;Save limit
        CALL    DIRREAD
        POP     DX
        MOV     BX,DIRBUF
HAVIT:
        CLC
        RET

NONE:
        CALL    CHKDIRWRITE
        STC
RET4:   RET


DELETE: ; System call 19
        CALL    GETNAME
        MOV     AL,-1
        JC      RET4
        CMP     BH,AL
        JZ      RET4
DELFILE:
        MOV     B,[DIRTYDIR],-1
        MOV     B,[BX],0E5H
        MOV     BX,[SI]
        LEA     SI,[BP+FAT]
        OR      BX,BX
        JZ      DELNXT
        CMP     BX,[BP+MAXCLUS]
        JA      DELNXT
        CALL    RELEASE
DELNXT:
        CALL    CONTSRCH
        JNC     DELFILE
        CALL    FATWRT
        CALL    CHKDIRWRITE
        XOR     AL,AL
        RET


RENAME: ;System call 23
        CALL    MOVNAME
        JC      ERRET
        ADD     SI,5
        MOV     DI,NAME2
        CALL    LODNAME
        JC      ERRET           ;Report error if second name invalid
        CALL    FINDNAME
        JC      ERRET
        CMP     BH,-1           ;Check if I/O device name
        JZ      ERRET           ;If so, can't rename it
        MOV     SI,NAME1
        MOV     DI,NAME3
        MOV     CX,6            ;6 words (12 bytes)--include attribute byte
        REP
        MOVSW                   ;Copy name to search for
RENFIL:
        MOV     DI,NAME1
        MOV     SI,NAME2
        MOV     CX,11
NEWNAM:
        LODSB
        CMP     AL,"?"
        JNZ     NOCHG
        MOV     AL,[BX]
NOCHG:
        STOSB
        INC     BX
        LOOP    NEWNAM
        MOV     B,[DI],6        ;Stop duplicates with any attributes
        CALL    DEVNAME         ;Check if giving it a device name
        JNC     RENERR
        PUSH    [LASTENT]       ;Save position of match
        MOV     [LASTENT],-1    ;Search entire directory for duplicate
        CALL    CONTSRCH        ;See if new name already exists
        POP     AX
        JNC     RENERR                  ;Error if found
        CALL    GETENT                  ;Re-read matching entry
        MOV     DI,BX
        MOV     SI,NAME1
        MOV     CX,11
        REP
        MOVSB                           ;Replace old name with new one
        MOV     B,[DIRTYDIR],-1         ;Flag change in directory
        MOV     SI,NAME3
        MOV     DI,NAME1
        MOV     CX,6                    ;Include attribute byte
        REP
        MOVSW                           ;Copy name back into search buffer
        CALL    CONTSRCH
        JNC     RENFIL
        CALL    CHKDIRWRITE
        XOR     AL,AL
        RET

RENERR:
        CALL    CHKDIRWRITE
ERRET:
        MOV     AL,-1
RET5:   RET


MOVNAME:

; Inputs:
;       DS, DX point to FCB or extended FCB
; Outputs:
;       DS:DX point to normal FCB
;       ES = CS
;       If file name OK:
;       BP has base of driver parameters
;       [NAME1] has name in upper case
; All registers except DX destroyed
; Carry set if bad file name or drive

        SEG     CS
        MOV     B,[EXTFCB+1],0
        MOV     AX,CS
        MOV     ES,AX
        MOV     DI,NAME1
        MOV     SI,DX
        LODSB
        SEG     CS
        MOV     [EXTFCB],AL     ;Set flag if extended FCB in use
        MOV     AH,0            ;Set default attributes
        CMP     AL,-1           ;Is it an extended FCB?
        JNZ     HAVATTRB
        ADD     DX,7            ;Adjust to point to normal FCB
        ADD     SI,6            ;Point to drive select byte
        MOV     AH,[SI-1]       ;Get attribute byte
        LODSB           ;Get drive select byte
HAVATTRB:
        SEG     CS
        MOV     [ATTRIB],AH     ;Save attributes
        CALL    GETTHISDRV
        JB      RET5
LODNAME:
; This entry point copies a file name from DS,SI
; to ES,DI converting to upper case.
        CMP     B,[SI]," "      ;Don't allow blank as first letter
        STC                     ;In case of error
        JZ      RET5
        MOV     CX,11
MOVCHK:
        CALL    GETLET
        JNZ     STOLET          ;Is it a delimiter?
        CMP     AL," "          ;This is the only delimiter allowed
        STC                     ;In case of error
        JNZ     RET5
STOLET:
        CMP     AL," "
        JB      RET5
        STOSB
        LOOP    MOVCHK
RET6:   RET

GETTHISDRV:
        SEG     CS
        CMP     [NUMIO],AL
        JC      RET6
        CBW
        XCHG    AX,BP
        SHL     BP
        MOV     BP,[BP+CURDRV]
        RET
        

OPEN:   ;System call 15
        CALL    GETFILE
DOOPEN:
; Enter here to perform OPEN on file already found
; in directory. DS=CS, BX points to directory
; entry in DIRBUF, SI points to First Cluster field, and
; ES:DI point to the FCB to be opened. This entry point
; is used by CREATE.
        JC      ERRET
        CMP     BH,-1
        JZ      LAB030
        MOV     AL,[BP+0]
        INC     AL
        STOSB
        ADD     DI,12
        XOR     AL,AL
        STOSB
        MOV     AX,128
        STOSW
        LODSW
        MOV     DX,AX
        MOVSW
        MOVSW
        MOV     AX,[SI-8]
        CMP     B,[BP+26],-1
        JNZ     LAB020
        SEG     ES
        MOV     W,[DI-1],0
        XOR     AX,AX
LAB020:
        STOSW
        MOV     AX,[LASTENT]
        STOSW
        MOV     AX,DX
        STOSW
        STOSW
        XOR     AX,AX
        STOSW
        STOSB
        RET
LAB030:
        SEG     ES
        MOV     [DI+22],BX
        XOR     AL,AL
LAB040: RET
STARTSRCH:
        MOV     [LASTENT],-1
LAB050:
        MOV     AL,[BP+0]
        CALL    BIOSDSKCHG,BIOSSEG      ;See what BIOS has to say
        MOV     AL,[BP+0]
        OR      AH,[BP+25]
        JS      NEWDSK          ;If either say new disk, then it's so
        DEC     AH
        JZ      LAB040
        MOV     AH,1
        CMP     W,AX,[BUFDRVNO] ;Does buffer have dirty sector of this drive?
        JZ      LAB040
NEWDSK:
        CMP     AL,[BUFDRVNO]   ;See if buffer is for this drive
        JNZ     BUFOK           ;If not, don't touch it
        MOV     [BUFSECNO],0    ;Flag buffers invalid
        MOV     W,[BUFDRVNO],00FFH
BUFOK:
        MOV     [DIRBUFID],-1
        CALL    FIGFAT
NEXTFAT:
        PUSH    AX
        CALL    DSKREAD
        POP     AX
        JC      BADFAT
        MOV     DL,AL
        LEA     SI,[BP+17]
        CMP     B,[BP+26],-1
        JZ      LAB060
        ADD     SI,4
LAB060:
        LODSW
        MOV     [BP+10],AX
        LODSW
        MOV     [BP+12],AX
        MOV     AL,DL
        SUB     AL,[BP+7]
        JZ      LAB040
        NEG     AL
        JMP     FATWRT

BADFAT:
        MOV     CX,DI
        ADD     DX,CX
        DEC     AL
        JNZ     NEXTFAT
        CALL    FIGFAT                          ;Reset registers
        JMP     DREAD

OKRET1:
        MOV     AL,0
        RET

CLOSE:  ;System call 16
        MOV     DI,DX
        CMP     B,[DI],-1                       ;Check for extended FCB
        JNZ     NORMFCB3
        ADD     DI,7
NORMFCB3:
        CMP     B,[DI+23],-1
        JZ      OKRET1
        TEST    B,[DI+30],-1
        JZ      OKRET1
        MOV     AL,[DI]
        CALL    GETTHISDRV
        JC      BADCLOSEJ
        MOV     AL,[bp+0]
        MOV     AH,1                            ;Look for dirty buffer
        SEG     CS
        CMP     W,AX,[BUFDRVNO]
        JNZ     FNDDIR
;Write back dirty buffer if on same drive
        PUSH    DX
        PUSH    DS
        PUSH    CS
        POP     DS
        MOV     B,[DIRTYBUF],0
        MOV     BX,[BUFFER]
        MOV     CX,1
        MOV     DX,[BUFSECNO]
        CALL    DWRITE
        POP     DS
        POP     DX
FNDDIR:
        CALL    GETFILE
BADCLOSEJ:
        JC      BADCLOSE
        MOV     AX,[LASTENT]
        SEG     ES
        CMP     AX,[DI+22]
        JNZ     BADCLOSE
        SEG     ES
        MOV     CX,[DI+24]
        MOV     [SI],CX
        SEG     ES
        MOV     DX,[DI+16]
        MOV     [SI+2],DX
        SEG     ES
        MOV     DX,[DI+18]
        CMP     B,[BP+26],-1
        JNZ     LAB070
        MOV     [SI+4],DL
        JP      LAB080
LAB070:
        mov     [SI+4],DX
        SEG     ES
        mov     DX,[DI+20]
        mov     [SI-2],DX
LAB080:
        CALL    DIRWRITE

CHKFATWRT:
; Do FATWRT only if FAT is dirty and uses same I/O driver
        CMP     B,[BP+25],1
        JNZ     OKRET

FATWRT:

; Inputs:
;       DS = CS
;       BP = Base of drive parameter table
; Function:
;       Write the FAT back to disk and reset FAT
;       dirty bit.
; Outputs:
;       AL = 0
;       BP unchanged
; All other registers destroyed

        MOV     B,[BP+25],0
        CALL    FIGFAT
EACHFAT:
        PUSH    DX
        PUSH    CX
        PUSH    BX
        PUSH    AX
        CALL    DWRITE
        POP     AX
        POP     BX
        POP     CX
        POP     DX
        ADD     DX,CX
        DEC     AL
        JNZ     EACHFAT
OKRET:
        MOV     AL,0
        RET

BADCLOSE:
        MOV     B,[BP+25],0
        MOV     AL,-1
        RET


FIGFAT:
; Loads registers with values needed to read or
; write a FAT.
        MOV     AL,[BP+FATCNT]
        LEA     BX,[BP+FAT]
        MOV     CL,[BP+FATSIZ]  ;No. of records occupied by FAT
        MOV     CH,0
        MOV     DX,[BP+FIRFAT]  ;Record number of start of FATs
        RET


DIRCOMP:
; Prepare registers for directory read or write
        CBW
        ADD     AX,[BP+FIRDIR]
        MOV     DX,AX
        MOV     BX,DIRBUF
        MOV     CX,1
        RET


CREATE: ;System call 22
        CALL    MOVNAME
        JC      ERRET3
        MOV     DI,NAME1
        MOV     CX,11
        MOV     AL,"?"
        REPNE
        SCASB
        JZ      ERRET3
        SEG     CS
        MOV     B,[CREATING],-1
        PUSH    DX
        PUSH    DS
        CALL    FINDNAME
        JNC     EXISTENT
        CALL    STARTSRCH
        CALL    GETENTRY
LAB090:
        CMP     B,[BX],0E5H
        JZ      FREESPOT
        CALL    NEXTENTRY
        JNB     LAB090
ERRPOP:
        POP     DS
        POP     DX
ERRET3:
        MOV     AL,-1
        RET

EXISTENT:
        JNZ     ERRPOP          ;Error if attributes don't match
        CMP     BH,-1           ;Check if file is I/O device
        JZ      OPENJMP         ;If so, no action
        MOV     CX,[SI]         ;Get pointer to clusters
        JCXZ    FREESPOT
        CMP     CX,[BP+MAXCLUS]
        JA      FREESPOT
        PUSH    BX
        MOV     BX,CX
        LEA     SI,[BP+FAT]
        CALL    RELEASE         ;Free any data already allocated
        CALL    FATWRT
        POP     BX
FREESPOT:
        MOV     DI,BX
        MOV     SI,NAME1
        MOV     CX,5
        MOVSB
        REP
        MOVSW
        CMP     B,[BP+26],-1
        JNZ     LAB100
        PUSH    DI
        MOV     CL,5
        XOR     AX,AX
        JP      SMALLENT
LAB100:
        MOV     AL,[ATTRIB]
        STOSB
        XOR     AX,AX
        MOV     CL,6
        REP
        STOSW
        CALL    DATE16
        STOSW
        XOR     AX,AX
        PUSH    DI
        MOV     CL,6
SMALLENT:
        REP
        STOSB
        PUSH    BX
        CALL    DIRWRITE
        POP     BX
        POP     SI
OPENJMP:
        CLC                     ;Clear carry so OPEN won't fail
        POP     ES
        POP     DI
        JMP     DOOPEN


DIRREAD:

; Inputs:
;       DS = CS
;       AL = Directory block number
;       BP = Base of drive parameters
; Function:
;       Read the directory block into DIRBUF.
; Outputs:
;       AX,BP unchanged
; All other registers destroyed.

        PUSH    AX
        CALL    CHKDIRWRITE
        POP     AX
        PUSH    AX
        MOV     AH,[BP+DEVNUM]
        MOV     [DIRBUFID],AX
        CALL    DIRCOMP
        CALL    DREAD
        POP     AX
RET8:   RET


DREAD:

; Inputs:
;       BX,DS = Transfer address
;       CX = Number of sectors
;       DX = Absolute record number
;       BP = Base of drive parameters
; Function:
;       Calls BIOS to perform disk read. If BIOS reports
;       errors, will call HARDERR for further action.
; BP preserved. All other registers destroyed.

        CALL    DSKREAD
        JNC     RET8
        SEG     CS
        MOV     B,[READOP],0
        CALL    HARDERR
        CMP     AL,1            ;Check for retry
        JZ      DREAD
        RET                     ;Ignore otherwise


HARDERR:

;Hard disk error handler. Entry conditions:
;       DS:BX = Original disk transfer address
;       DX = Original logical sector number
;       CX = Number of sectors to go (first one gave the error)
;       AX = Hardware error code
;       DI = Original sector transfer count
;       BP = Base of drive parameters
;       [READOP] = 0 for read, 1 for write

        XCHG    AX,DI           ;Error code in DI, count in AX
        SUB     AX,CX           ;Number of sectors successfully transferred
        ADD     DX,AX           ;First sector number to retry
        PUSH    DX
        MUL     AX,[BP+SECSIZ]  ;Number of bytes transferred
        POP     DX
        ADD     BX,AX           ;First address for retry
        MOV     AH,0            ;Flag disk section in error
        CMP     DX,[BP+FIRFAT]  ;In reserved area?
        JB      ERRINT
        INC     AH              ;Flag for FAT
        CMP     DX,[BP+FIRDIR]  ;In FAT?
        JB      ERRINT
        INC     AH
        CMP     DX,[BP+FIRREC]  ;In directory?
        JB      ERRINT
        INC     AH              ;Must be in data area
ERRINT:
        SHL     AH              ;Make room for read/write bit
        SEG     CS
        OR      AH,[READOP]
FATAL:
        MOV     AL,[BP+DRVNUM]  ;Get drive number
FATAL1:
        PUSH    BP              ;The only thing we preserve
        SEG     CS
        MOV     [CONTSTK],SP
        CLI                     ;Prepare to play with stack
        SEG     CS
        MOV     SS,[SSSAVE]
        SEG     CS
        MOV     SP,[SPSAVE]     ;User stack pointer restored
        INT     24H             ;Fatal error interrupt vector
        SEG     CS
        MOV     [SPSAVE],SP
        SEG     CS
        MOV     [SSSAVE],SS
        MOV     SP,CS
        MOV     SS,SP
        SEG     CS
        MOV     SP,[CONTSTK]
        STI
        POP     BP
        CMP     AL,2
        JZ      ERROR
        RET

DSKREAD:
        MOV     AL,[BP+DEVNUM]
        PUSH    BP
        PUSH    BX
        PUSH    CX
        PUSH    DX
        CALL    BIOSREAD,BIOSSEG 
        POP     DX
        POP     DI
        POP     BX
        POP     BP
RET9:   RET


CHKDIRWRITE:
        TEST    B,[DIRTYDIR],-1
        JZ      RET9

DIRWRITE:

; Inputs:
;       DS = CS
;       AL = Directory block number
;       BP = Base of drive parameters
; Function:
;       Write the directory block into DIRBUF.
; Outputs:
;       BP unchanged
; All other registers destroyed.

        MOV     B,[DIRTYDIR],0
        MOV     W,AL,[DIRBUFID]
        CALL    DIRCOMP


DWRITE:

; Inputs:
;       BX,DS = Transfer address
;       CX = Number of sectors
;       DX = Absolute record number
;       BP = Base of drive parameters
; Function:
;       Calls BIOS to perform disk write. If BIOS reports
;       errors, will call HARDERR for further action.
; BP preserved. All other registers destroyed.

        MOV     AL,[BP+DEVNUM]
        PUSH    BP
        PUSH    BX
        PUSH    CX
        PUSH    DX
        CALL    BIOSWRITE,BIOSSEG
        POP     DX
        POP     DI
        POP     BX
        POP     BP
        JNC     RET9
        SEG     CS
        MOV     B,[READOP],1
        CALL    HARDERR
        CMP     AL,1            ;Check for retry
        JZ      DWRITE
        RET


ABORT:
        SEG     CS
        LDS     SI,[SPSAVE]
        MOV     DS,[SI+CSSAVE]
        XOR     AX,AX
        MOV     ES,AX
        MOV     SI,SAVEXIT
        MOV     DI,EXIT
        MOVSW
        MOVSW
        MOVSW
        MOVSW
        MOVSW
        MOVSW
ERROR:
        MOV     AX,CS
        MOV     DS,AX
        MOV     ES,AX
        CALL    WRTFATS
        XOR     AX,AX
        CLI
        MOV     SS,[SSSAVE]
        MOV     SP,[SPSAVE]
        MOV     DS,AX
        MOV     SI,EXIT
        MOV     DI,EXITHOLD
        MOVSW
        MOVSW
        POP     AX
        POP     BX
        POP     CX
        POP     DX
        POP     SI
        POP     DI
        POP     BP
        POP     DS
        POP     ES
        STI             ;Stack OK now
        SEG     CS
        JMP     L,[EXITHOLD]


SEQRD:  ;System call 20
        CALL    GETREC
        CALL    LOAD
        JP      FINSEQ

SEQWRT: ;System call 21
        CALL    GETREC
        CALL    STORE
FINSEQ:
        JCXZ    SETNREX
        ADD     AX,1
        ADC     DX,0
        JP      SETNREX

RNDRD:  ;System call 33
        CALL    GETRRPOS1
        CALL    LOAD
        JP      FINRND

RNDWRT: ;System call 34
        CALL    GETRRPOS1
        CALL    STORE
        JP      FINRND

BLKRD:  ;System call 39
        CALL    GETRRPOS
        CALL    LOAD
        JP      FINBLK

BLKWRT: ;System call 40
        CALL    GETRRPOS
        CALL    STORE
FINBLK:
        LDS     SI,[SPSAVE]
        MOV     [SI+CXSAVE],CX
        JCXZ    FINRND
        ADD     AX,1
        ADC     DX,0
FINRND:
        SEG     ES
        MOV     W,[DI+RR],AX
        SEG     ES
        MOV     [DI+RR+2],DL
        OR      DH,DH
        JZ      SETNREX
        SEG     ES
        MOV     [DI+RR+3],DH    ;Save 4 byte of RECPOS only if significant
SETNREX:
        MOV     CX,AX
        AND     AL,7FH
        SEG     ES
        MOV     [DI+NR],AL
        AND     CL,80H
        SHL     CX
        RCL     DX
        MOV     AL,CH
        MOV     AH,DL
        SEG     ES
        MOV     [DI+EXTENT],AX
        SEG     CS
        MOV     AL,[DSKERR]
        RET

GETRRPOS1:
        MOV     CX,1
GETRRPOS:
        MOV     DI,DX
        CMP     B,[DI],-1
        JNZ     NORMFCB1
        ADD     DI,7
NORMFCB1:
        MOV     W,AX,[DI+RR]
        MOV     W,DX,[DI+RR+2]
        RET

NOFILERR:
        XOR     CX,CX
        MOV     B,[DSKERR],4
        POP     BX
        RET

SETUP:

; Inputs:
;       DS:DI point to FCB
;       DX:AX = Record position in file of disk transfer
;       CX = Record count
; Outputs:
;       DS = CS
;       ES:DI point to FCB
;       BL = DEVID from FCB
;       CX = No. of bytes to transfer
;       BP = Base of drive parameters
;       SI = FAT pointer
;       [RECCNT] = Record count
;       [RECPOS] = Record position in file
;       [FCB] = DI
;       [NEXTADD] = Displacement of disk transfer within segment
;       [SECPOS] = Position of first sector
;       [BYTPOS] = Byte position in file
;       [BYTSECPOS] = Byte position in first sector
;       [CLUSNUM] = First cluster
;       [SECCLUSPOS] = Sector within first cluster
;       [DSKERR] = 0 (no errors yet)
;       [TRANS] = 0 (No transfers yet)
;       [THISDRV] = Physical drive unit number
; If SETUP detects no records will be transfered, it returns 1 level up 
; with CX = 0.

        PUSH    AX
        MOV     AL,[DI]
        MOV     SI,[DI+RECSIZ]
        OR      SI,SI
        JNZ     HAVRECSIZ
        MOV     SI,128
        MOV     [DI+RECSIZ],SI
HAVRECSIZ:
        MOV     BX,DS
        MOV     ES,BX
        MOV     BX,CS
        MOV     DS,BX
        CALL    GETTHISDRV
        POP     AX
        JC      NOFILERR
        CMP     SI,64           ;Check if highest byte of RECPOS is significant
        JB      SMALREC
        MOV     DH,0            ;Ignore MSB if record >= 64 bytes
SMALREC:
        MOV     [RECCNT],CX
        MOV     W,[RECPOS],AX
        MOV     W,[RECPOS+2],DX
        MOV     [FCB],DI
        MOV     BX,[DMAADD]
        MOV     [NEXTADD],BX
        MOV     B,[DSKERR],0
        MOV     B,[TRANS],0
        MOV     BX,DX
        MUL     AX,SI
        MOV     W,[BYTPOS],AX
        PUSH    DX
        MOV     AX,BX
        MUL     AX,SI
        POP     BX
        ADD     AX,BX
        ADC     DX,0            ;Ripple carry
        JNZ     EOFERR
        MOV     W,[BYTPOS+2],AX
        MOV     DX,AX
        MOV     W,AX,[BYTPOS]
        MOV     BX,[BP+SECSIZ]
        CMP     DX,BX           ;See if divide will overflow
        JNC     EOFERR
        DIV     AX,BX
        MOV     [SECPOS],AX
        MOV     [BYTSECPOS],DX
        MOV     DX,AX
        AND     AL,[BP+CLUSMSK]
        MOV     [SECCLUSPOS],AL
        MOV     AX,CX           ;Record count
        MOV     CL,[BP+CLUSSHFT]
        SHR     DX,CL
        MOV     [CLUSNUM],DX
        MUL     AX,SI           ;Multiply by bytes per record
        MOV     CX,AX
        ADD     AX,[DMAADD]     ;See if it will fit in one segment
        ADC     DX,0
        JZ      OK              ;Must be less than 64K
        MOV     AX,[DMAADD]
        NEG     AX              ;Amount of room left in segment
        JNZ     PARTSEG         ;All 64K available?
        DEC     AX              ;If so, reduce by one
PARTSEG:
        XOR     DX,DX
        DIV     AX,SI           ;How many records will fit?
        MOV     [RECCNT],AX
        MUL     AX,SI           ;Translate that back into bytes
        MOV     B,[DSKERR],2    ;Flag that trimming took place
        MOV     CX,AX
        JCXZ    NOROOM
OK:
        SEG     ES
        MOV     BX,[DI+DEVID]
        LEA     SI,[BP+FAT]
        RET

EOFERR:
        MOV     B,[DSKERR],1
        XOR     CX,CX
NOROOM:
        POP     BX              ;Kill return address
        RET

BREAKDOWN:

;Inputs:
;       DS = CS
;       CX = Length of disk transfer in bytes
;       BP = Base of drive parameters
;       [BYTSECPOS] = Byte position witin first sector
;Outputs:
;       [BYTCNT1] = Bytes to transfer in first sector
;       [SECCNT] = No. of whole sectors to transfer
;       [BYTCNT2] = Bytes to transfer in last sector
;AX, BX, DX destroyed. No other registers affected.

        MOV     AX,[BYTSECPOS]
        MOV     BX,CX
        OR      AX,AX
        JZ      SAVFIR          ;Partial first sector?
        SUB     AX,[BP+SECSIZ]
        NEG     AX              ;Max number of bytes left in first sector
        SUB     BX,AX           ;Subtract from total length
        JAE     SAVFIR
        ADD     AX,BX           ;Don't use all of the rest of the sector
        XOR     BX,BX           ;And no bytes are left
SAVFIR:
        MOV     [BYTCNT1],AX
        MOV     AX,BX
        XOR     DX,DX
        DIV     AX,[BP+SECSIZ]  ;How many whole sectors?
        MOV     [SECCNT],AX
        MOV     [BYTCNT2],DX    ;Bytes remaining for last sector
RET10:  RET


FNDCLUS:

; Inputs:
;       DS = CS
;       CX = No. of clusters to skip
;       BP = Base of drive parameters
;       SI = FAT pointer
;       ES:DI point to FCB
; Outputs:
;       BX = Last cluster skipped to
;       CX = No. of clusters remaining (0 unless EOF)
;       DX = Position of last cluster
; DI destroyed. No other registers affected.

        SEG     ES
        MOV     BX,[DI+LSTCLUS]
        SEG     ES
        MOV     DX,[DI+CLUSPOS]
        OR      BX,BX
        JZ      NOCLUS
        SUB     CX,DX
        JNB     FINDIT
        ADD     CX,DX
        XOR     DX,DX
        SEG     ES
        MOV     BX,[DI+FIRCLUS]
FINDIT:
        JCXZ    RET10
SKPCLP:
        CALL    UNPACK
        CMP     DI,0FF8H
        JAE     RET10
        XCHG    BX,DI
        INC     DX
        LOOP    SKPCLP
        RET
NOCLUS:
        INC     CX
        DEC     DX
        RET


BUFSEC:
; Inputs:
;       AL = 0 if buffer must be read, 1 if no pre-read needed
;       BP = Base of drive parameters
;       [CLUSNUM] = Physical cluster number
;       [SECCLUSPOS] = Sector position of transfer within cluster
;       [BYTCNT1] = Size of transfer
; Function:
;       Insure specified sector is in buffer, flushing buffer before
;       read if necessary.
; Outputs:
;       SI = Pointer to buffer
;       DI = Pointer to transfer address
;       CX = Number of bytes
;       [NEXTADD] updated
;       [TRANS] set to indicate a transfer will occur

        MOV     DX,[CLUSNUM]
        MOV     BL,[SECCLUSPOS]
        CALL    FIGREC
        MOV     [PREREAD],AL
        CMP     DX,[BUFSECNO]
        JNZ     GETSEC
        MOV     AL,[BUFDRVNO]
        CMP     AL,[BP+0]
        JZ      FINBUF          ;Already have it?
GETSEC:
        XOR     AL,AL
        XCHG    [DIRTYBUF],AL   ;Read dirty flag and reset it
        OR      AL,AL
        JZ      RDSEC
        PUSH    DX
        PUSH    BP
        MOV     BP,[BUFDRVBP]
        MOV     BX,[BUFFER]
        MOV     CX,1
        MOV     DX,[BUFSECNO]
        CALL    DWRITE
        POP     BP
        POP     DX
RDSEC:
        TEST    B,[PREREAD],-1
        JNZ     SETBUF
        XOR     AX,AX
        MOV     [BUFSECNO],AX           ;Set buffer valid in case of disk error
        DEC     AX
        MOV     [BUFDRVNO],AL
        MOV     BX,[BUFFER]
        MOV     CX,1
        PUSH    DX
        CALL    DREAD
        POP     DX
SETBUF:
        MOV     [BUFSECNO],DX
        MOV     AL,[BP+0]
        MOV     [BUFDRVNO],AL
        MOV     [BUFDRVBP],BP
FINBUF:
        MOV     B,[TRANS],1             ;A transfer is taking place
        MOV     DI,[NEXTADD]
        MOV     SI,DI
        MOV     CX,[BYTCNT1]
        ADD     SI,CX
        MOV     [NEXTADD],SI
        MOV     SI,[BUFFER]
        ADD     SI,[BYTSECPOS]
        RET

BUFRD:
        XOR     AL,AL           ;Pre-read necessary
        CALL    BUFSEC
        PUSH    ES
        MOV     ES,[DMAADD+2]
        SHR     CX
        JNC     EVENRD
        MOVSB
EVENRD:
        REP
        MOVSW
        POP     ES
        RET

BUFWRT:
        MOV     AX,[SECPOS]
        INC     AX              ;Set for next sector
        MOV     [SECPOS],AX
        CMP     AX,[VALSEC]     ;Has sector been written before?
        MOV     AL,1
        JA      NOREAD          ;Skip preread if SECPOS>VALSEC
        MOV     AL,0
NOREAD:
        CALL    BUFSEC
        XCHG    DI,SI
        PUSH    DS
        PUSH    ES
        PUSH    CS
        POP     ES
        MOV     DS,[DMAADD+2]
        SHR     CX
        JNC     EVENWRT
        MOVSB
EVENWRT:
        REP
        MOVSW
        POP     ES
        POP     DS
        MOV     B,[DIRTYBUF],1
        RET

NEXTSEC:
        TEST    B,[TRANS],-1
        JZ      CLRET
        MOV     AL,[SECCLUSPOS]
        INC     AL
        CMP     AL,[BP+CLUSMSK]
        JBE     SAVPOS
        MOV     BX,[CLUSNUM]
        CMP     BX,0FF8H
        JAE     NONEXT
        LEA     SI,[BP+FAT]
        CALL    UNPACK
        MOV     [CLUSNUM],DI
        INC     [LASTPOS]
        MOV     AL,0
SAVPOS:
        MOV     [SECCLUSPOS],AL
CLRET:
        CLC
        RET
NONEXT:
        STC
        RET

TRANBUF:
        LODSB
        STOSB
        CMP     AL,13           ;Check for carriage return
        JNZ     NORMCH
        MOV     B,[SI],10
NORMCH:
        CMP     AL,10
        LOOPNZ  TRANBUF
        JNZ     ENDRDCON
        CALL    OUT             ;Transmit linefeed
        XOR     SI,SI
        OR      CX,CX
        JNZ     GETBUF
        OR      AL,1            ;Clear zero flag--not end of file
ENDRDCON:
        MOV     [CONTPOS],SI
ENDRDDEV:
        MOV     [NEXTADD],DI
        POP     ES
        JNZ     SETFCBJ         ;Zero set if Ctrl-Z found in input
        MOV     DI,[FCB]
        SEG     ES
        OR      B,[DI+DEVID],80H        ;Mark as no more data available
SETFCBJ:
        JMP     SETFCB

READDEV:
        PUSH    ES
        LES     DI,[DMAADD]
        OR      BL,BL
        JZ      READCON
        DEC     BL
        JNZ     ENDRDDEV
READAUX:
        CALL    AUXIN
        STOSB
        CMP     AL,1AH
        LOOPNZ  READAUX
        JP      ENDRDDEV

READCON:
        PUSH    CS
        POP     DS
        MOV     SI,[CONTPOS]
        OR      SI,SI
        JNZ     TRANBUF
        CMP     B,[CONBUF],128
        JZ      GETBUF
        MOV     W,[CONBUF],0FF80H       ;Set up 128-byte buffer with no template
GETBUF:
        PUSH    CX
        PUSH    ES
        PUSH    DI
        MOV     DX,CONBUF
        CALL    BUFIN           ;Get input buffer
        POP     DI
        POP     ES
        POP     CX
        MOV     SI,2 + CONBUF
        CMP     B,[SI],1AH      ;Check for Ctrl-Z in first character
        JNZ     TRANBUF
        MOV     AL,1AH
        STOSB
        MOV     AL,10
        CALL    OUT             ;Send linefeed
        XOR     SI,SI
        JP      ENDRDCON

RDERR:
        XOR     CX,CX
        JMP     WRTERR

RDLASTJ:JMP     RDLAST

LOAD:

; Inputs:
;       DS:DI point to FCB
;       DX:AX = Position in file to read
;       CX = No. of records to read
; Outputs:
;       DX:AX = Position of last record read
;       CX = No. of bytes read
;       ES:DI point to FCB
;       LSTCLUS, CLUSPOS fields in FCB set

        CALL    SETUP
        CMP     BH,-1           ;Check for named device I/O
        JZ      READDEV
        SEG     ES
        MOV     W,AX,[DI+FILSIZ]
        SEG     ES
        MOV     W,BX,[DI+FILSIZ+2]
        SUB     W,AX,[BYTPOS]
        SBB     W,BX,[BYTPOS+2]
        JB      RDERR
        JNZ     ENUF
        OR      AX,AX
        JZ      RDERR
        CMP     AX,CX
        JAE     ENUF
        MOV     CX,AX
ENUF:
        CALL    BREAKDOWN
        MOV     CX,[CLUSNUM]
        CALL    FNDCLUS
        OR      CX,CX
        JNZ     RDERR
        MOV     [LASTPOS],DX
        MOV     [CLUSNUM],BX
        CMP     [BYTCNT1],0
        JZ      RDMID
        CALL    BUFRD
RDMID:
        CMP     [SECCNT],0
        JZ      RDLASTJ
        CALL    NEXTSEC
        JC      SETFCB
        MOV     B,[TRANS],1     ;A transfer is taking place
ONSEC:
        MOV     DL,[SECCLUSPOS]
        MOV     CX,[SECCNT]
        MOV     BX,[CLUSNUM]
RDLP:
        CALL    OPTIMIZE
        PUSH    DI
        PUSH    AX
        PUSH    DS
        MOV     DS,[DMAADD+2]
        PUSH    DX
        PUSH    BX
        PUSHF                   ;Save carry flag
        CALL DREAD
        POPF                    ;Restore carry flag
        POP     DI              ;Initial transfer address
        POP     AX              ;First sector transfered
        POP     DS
        JC      NOTBUFFED       ;Was one of those sectors in the buffer?
        CMP     B,[DIRTYBUF],0  ;Is buffer dirty?
        JZ      NOTBUFFED       ;If not no problem
;We have transfered in a sector from disk when a dirty copy of it is in the buffer.
;We must transfer the sector from the buffer to correct memory address
        SUB     AX,[BUFSECNO]   ;How many sectors into the transfer?
        NEG     AX
        MOV     CX,[BP+SECSIZ]
        MUL     AX,CX           ;How many bytes into the transfer?
        ADD     DI,AX
        MOV     SI,[BUFFER]
        PUSH    ES
        MOV     ES,[DMAADD+2]   ;Get disk transfer segment
        SHR     CX
        REP
        MOVSW
        JNC     EVENMOV
        MOVSB
EVENMOV:
        POP     ES
NOTBUFFED:
        POP     CX
        POP     BX
        JCXZ    RDLAST
        CMP     BX,0FF8H
        JAE     SETFCB
        MOV     DL,0
        INC     [LASTPOS]       ;We'll be using next cluster
        JP      RDLP

SETFCB:
        MOV     SI,[FCB]
        MOV     AX,[NEXTADD]
        MOV     DI,AX
        SUB     AX,[DMAADD]     ;Number of bytes transfered
        XOR     DX,DX
        SEG     ES
        MOV     CX,[SI+RECSIZ]
        DIV     AX,CX           ;Number of records
        CMP     AX,[RECCNT]     ;Check if all records transferred
        JZ      FULLREC
        MOV     B,[DSKERR],1
        OR      DX,DX
        JZ      FULLREC         ;If remainder 0, then full record transfered
        MOV     B,[DSKERR],3    ;Flag partial last record
        SUB     CX,DX           ;Bytes left in last record
        PUSH    ES
        MOV     ES,[DMAADD+2]
        XCHG    AX,BX           ;Save the record count temporarily
        XOR     AX,AX           ;Fill with zeros
        SHR     CX
        JNC     EVENFIL
        STOSB
EVENFIL:
        REP
        STOSW
        XCHG    AX,BX           ;Restore record count to AX
        POP     ES
        INC     AX              ;Add last (partial) record to total
FULLREC:
        MOV     CX,AX
        MOV     DI,SI           ;ES:DI point to FCB
SETCLUS:
        MOV     AX,[CLUSNUM]
        SEG     ES
        MOV     [DI+LSTCLUS],AX
        MOV     AX,[LASTPOS]
        SEG     ES
        MOV     [DI+CLUSPOS],AX
ADDREC:
        MOV     W,AX,[RECPOS]
        MOV     W,DX,[RECPOS+2]
        JCXZ    RET28           ;If no records read, don't change position
        DEC     CX
        ADD     AX,CX           ;Update current record position
        ADC     DX,0
        INC     CX      
RET28:  RET

RDLAST:
        MOV     AX,[BYTCNT2]
        OR      AX,AX
        JZ      SETFCB
        MOV     [BYTCNT1],AX
        CALL    NEXTSEC
        JC      SETFCB
        MOV     [BYTSECPOS],0
        CALL    BUFRD
        JP      SETFCB

WRTDEV:
        PUSH    DS
        LDS     SI,[DMAADD]
        AND     BL,7FH
        OR      BL,BL
        JZ      WRTCON
        DEC     BL
        JZ      WRTAUX
        DEC     BL
        JZ      ENDWRDEV        ;Done if device is NUL
WRTLST:
        LODSB
        CMP     AL,1AH
        JZ      ENDWRDEV
        CALL    LISTOUT
        LOOP    WRTLST
        JP      ENDWRDEV

WRTAUX:
        LODSB
        CALL    AUXOUT
        CMP     AL,1AH
        LOOPNZ  WRTAUX
        JP      ENDWRDEV

WRTCON:
        LODSB
        CMP     AL,1AH
        JZ      ENDWRDEV
        CALL    OUT
        LOOP    WRTCON
ENDWRDEV:
        POP     DS
        MOV     CX,[RECCNT]
        MOV     DI,[FCB]
        JP      ADDREC

HAVSTART:
        MOV     CX,AX
        CALL    SKPCLP
        JCXZ    DOWRTJ
        CALL    ALLOCATE
        JNC     DOWRTJ
WRTERR:
        MOV     B,[DSKERR],1
LVDSK:
        MOV     W,AX,[RECPOS]
        MOV     W,DX,[RECPOS+2]
        MOV     DI,[FCB]
        RET

DOWRTJ: JMP     DOWRT

WRTEOFJ:
        JMP     WRTEOF

STORE:

; Inputs:
;       DS:DI point to FCB
;       DX:AX = Position in file of disk transfer
;       CX = Record count
; Outputs:
;       DX:AX = Position of last record written
;       CX = No. of records written
;       ES:DI point to FCB
;       LSTCLUS, CLUSPOS fields in FCB set

        CALL    SETUP
        CALL    DATE16
        SEG     ES
        MOV     [DI+FDATE],AX
        CMP     BH,-1
        JZ      WRTDEV
        SEG     ES
        MOV     B,[DI+30],1
        CALL    BREAKDOWN
        MOV     W,AX,[BYTPOS]
        MOV     W,DX,[BYTPOS+2]
        JCXZ    WRTEOFJ
        DEC     CX
        ADD     AX,CX
        ADC     DX,0            ;AX:DX=last byte accessed
        DIV     AX,[BP+SECSIZ]  ;AX=last sector accessed
        MOV     CL,[BP+CLUSSHFT]
        SHR     AX,CL           ;Last cluster to be accessed
        PUSH    AX
        SEG     ES
        MOV     W,AX,[DI+FILSIZ]
        SEG     ES
        MOV     W,DX,[DI+FILSIZ+2]
        DIV     AX,[BP+SECSIZ]
        OR      DX,DX
        JZ      NORNDUP
        INC     AX              ;Round up if any remainder
NORNDUP:
        MOV     [VALSEC],AX     ;Number of sectors that have been written
        POP     AX
        MOV     CX,[CLUSNUM]    ;First cluster accessed
        CALL    FNDCLUS
        MOV     [CLUSNUM],BX
        MOV     [LASTPOS],DX
        SUB     AX,DX           ;Last cluster minus current cluster
        JZ      DOWRT           ;If we have last clus, we must have first
        JCXZ    HAVSTART        ;See if no more data
        PUSH    CX              ;No. of clusters of first
        MOV     CX,AX
        CALL    ALLOCATE
        POP     AX
        JC      WRTERR
        MOV     CX,AX
        MOV     DX,[LASTPOS]
        INC     DX
        DEC     CX
        JZ      NOSKIP
        CALL    SKPCLP
NOSKIP:
        MOV     [CLUSNUM],BX
        MOV     [LASTPOS],DX
DOWRT:
        CMP     [BYTCNT1],0
        JZ      WRTMID
        MOV     BX,[CLUSNUM]
        CALL    BUFWRT  
WRTMID:
        MOV     AX,[SECCNT]
        OR      AX,AX
        JZ      WRTLAST
        ADD     [SECPOS],AX
        CALL    NEXTSEC
        MOV     B,[TRANS],1     ;A transfer is taking place
        MOV     DL,[SECCLUSPOS]
        MOV     BX,[CLUSNUM]
        MOV     CX,[SECCNT]
WRTLP:
        CALL    OPTIMIZE
        JC      NOTINBUF        ;Is one of the sectors buffered?
        MOV     [BUFSECNO],0    ;If so, invalidate the buffer since we're
        MOV     W,[BUFDRVNO],0FFH       ;completely rewritting it
NOTINBUF:
        PUSH    DI
        PUSH    AX
        PUSH    DS
        MOV     DS,[DMAADD+2]
        CALL    DWRITE
        POP     DS
        POP     CX
        POP     BX
        JCXZ    WRTLAST
        MOV     DL,0
        INC     [LASTPOS]       ;We'll be using next cluster
        JP      WRTLP
WRTLAST:
        MOV     AX,[BYTCNT2]
        OR      AX,AX
        JZ      FINWRT
        MOV     [BYTCNT1],AX
        CALL    NEXTSEC
        MOV     [BYTSECPOS],0
        CALL    BUFWRT
FINWRT:
        MOV     AX,[NEXTADD]
        SUB     AX,[DMAADD]
        ADD     W,AX,[BYTPOS]
        MOV     W,DX,[BYTPOS+2]
        ADC     DX,0
        MOV     CX,DX
        MOV     DI,[FCB]
        SEG     ES
        CMP     W,AX,[DI+FILSIZ]
        SEG     ES
        SBB     W,CX,[DI+FILSIZ+2]
        JB      SAMSIZ
        SEG     ES
        MOV     W,[DI+FILSIZ],AX
        SEG     ES
        MOV     W,[DI+FILSIZ+2],DX
SAMSIZ:
        MOV     CX,[RECCNT]
        JMP     SETCLUS


WRTERRJ:JMP     WRTERR

WRTEOF:
        MOV     CX,AX
        OR      CX,DX
        JZ      KILLFIL
        SUB     AX,1
        SBB     DX,0
        DIV     AX,[BP+SECSIZ]
        MOV     CL,[BP+CLUSSHFT]
        SHR     AX,CL
        MOV     CX,AX
        CALL    FNDCLUS
        JCXZ    RELFILE
        CALL    ALLOCATE
        JC      WRTERRJ
UPDATE:
        MOV     DI,[FCB]
        MOV     W,AX,[BYTPOS]
        SEG     ES
        MOV     W,[DI+FILSIZ],AX
        MOV     W,AX,[BYTPOS+2]
        SEG     ES
        MOV     W,[DI+FILSIZ+2],AX
        XOR     CX,CX
        JMP     ADDREC

RELFILE:
        MOV     DX,0FFFH
        CALL    RELBLKS
SETDIRT:
        MOV     B,[BP+25],1
        JP      UPDATE

KILLFIL:
        XOR     BX,BX
        SEG     ES
        XCHG    BX,[DI+FIRCLUS]
        OR      BX,BX
        JZ      UPDATE
        CALL    RELEASE
        JP      SETDIRT


OPTIMIZE:

; Inputs:
;       DS = CS
;       BX = Physical cluster
;       CX = No. of records
;       DL = sector within cluster
;       BP = Base of drives parameters
;       [NEXTADD] = transfer address
; Outputs:
;       AX = No. of records remaining
;       BX = Transfer address
;       CX = No. or records to be transferred
;       DX = Physical sector address
;       DI = Next cluster
;       Carry clear if a sector to transfer is in the buffer
;       Carry set otherwise
;       [CLUSNUM] = Last cluster accessed
;       [NEXTADD] updated
; BP unchanged. Note that segment of transfer not set.

        PUSH    DX
        PUSH    BX
        MOV     AL,[BP+CLUSMSK]
        INC     AL              ;Number of sectors per cluster
        MOV     AH,AL
        SUB     AL,DL           ;AL = Number of sectors left in first cluster
        MOV     DX,CX
        LEA     SI,[BP+FAT]
        MOV     CX,0
OPTCLUS:
;AL has number of sectors available in current cluster
;AH has number of sectors available in next cluster
;BX has current physical cluster
;CX has number of sequential sectors found so far
;DX has number of sectors left to transfer
;SI has FAT pointer
        CALL    UNPACK
        ADD     CL,AL
        ADC     CH,0
        CMP     CX,DX
        JAE     BLKDON
        MOV     AL,AH
        INC     BX
        CMP     DI,BX
        JZ      OPTCLUS
        DEC     BX
FINCLUS:
        MOV     [CLUSNUM],BX    ;Last cluster accessed
        SUB     DX,CX           ;Number of sectors still needed
        PUSH    DX
        MOV     AX,CX
        MUL     AX,[BP+SECSIZ]  ;Number of sectors times sector size
        MOV     SI,[NEXTADD]
        ADD     AX,SI           ;Adjust by size of transfer
        MOV     [NEXTADD],AX
        POP     AX              ;Number of sectors still needed
        POP     DX              ;Starting cluster
        SUB     BX,DX           ;Number of new clusters accessed
        ADD     [LASTPOS],BX
        POP     BX              ;BL = sector postion within cluster
        CALL    FIGREC
        MOV     BX,SI
;Now let's see if any of these sectors are already in the buffer
        CMP     [BUFSECNO],DX
        JC      RET100          ;If DX > [BUFSECNO] then not in buffer
        MOV     SI,DX
        ADD     SI,CX           ;Last sector + 1
        CMP     [BUFSECNO],SI
        CMC
        JC      RET100          ;If SI <= [BUFSECNO] then not in buffer
        PUSH    AX
        MOV     AL,[BP+DEVNUM]
        CMP     AL,[BUFDRVNO]   ;Is buffer for this drive?
        POP     AX
        JZ      RET100          ;If so, then we match 
        STC                     ;No match
RET100: RET
BLKDON:
        SUB     CX,DX           ;Number of sectors in cluster we don't want
        SUB     AH,CL           ;Number of sectors in cluster we accepted
        DEC     AH              ;Adjust to mean position within cluster
        MOV     [SECCLUSPOS],AH
        MOV     CX,DX           ;Anyway, make the total equal to the request
        JP      FINCLUS


FIGREC:

;Inputs:
;       DX = Physical cluster number
;       BL = Sector postion within cluster
;       BP = Base of drive parameters
;Outputs:
;       DX = physical sector number
;No other registers affected.

        PUSH    CX
        MOV     CL,[BP+CLUSSHFT]
        DEC     DX
        DEC     DX
        SHL     DX,CL
        OR      DL,BL
        ADD     DX,[BP+FIRREC]
        POP     CX
        RET

GETREC:

; Inputs:
;       DS:DX point to FCB
; Outputs:
;       CX = 1
;       DX:AX = Record number determined by EXTENT and NR fields
;       DS:DI point to FCB
; No other registers affected.

        MOV     DI,DX
        CMP     B,[DI],-1       ;Check for extended FCB
        JNZ     NORMFCB2
        ADD     DI,7
NORMFCB2:
        MOV     CX,1
        MOV     AL,[DI+NR]
        MOV     DX,[DI+EXTENT]
        SHL     AL
        SHR     DX
        RCR     AL
        MOV     AH,DL
        MOV     DL,DH
        MOV     DH,0
        RET


ALLOCATE:

; Inputs:
;       DS = CS
;       ES = Segment of FCB
;       BX = Last cluster of file (0 if null file)
;       CX = No. of clusters to allocate
;       DX = Position of cluster BX
;       BP = Base of drive parameters
;       SI = FAT pointer
;       [FCB] = Displacement of FCB within segment
; Outputs:
;       IF insufficient space
;         THEN
;       Carry set
;       CX = max. no. of records that could be added to file
;         ELSE
;       Carry clear
;       BX = First cluster allocated
;       FAT is fully updated including dirty bit
;       FIRCLUS field of FCB set if file was null
; SI,BP unchanged. All other registers destroyed.

        PUSH    [SI]
        PUSH    DX
        PUSH    CX
        PUSH    BX
        MOV     AX,BX
ALLOC:
        MOV     DX,BX
FINDFRE:
        INC     BX
        CMP     BX,[BP+MAXCLUS]
        JLE     TRYOUT
        CMP     AX,1
        JG      TRYIN
        POP     BX
        MOV     DX,0FFFH
        CALL    RELBLKS
        POP     AX              ;No. of clusters requested
        SUB     AX,CX           ;AX=No. of clusters allocated
        POP     DX
        POP     [SI]
        INC     DX              ;Position of first cluster allocated
        ADD     AX,DX           ;AX=max no. of cluster in file
        MOV     DL,[BP+CLUSMSK]
        MOV     DH,0
        INC     DX              ;DX=records/cluster
        MUL     AX,DX           ;AX=max no. of records in file
        MOV     CX,AX
        SUB     W,CX,[RECPOS]   ;CX=max no. of records that could be written
        JA      MAXREC
        XOR     CX,CX           ;If CX was negative, zero it
MAXREC:
        STC
RET11:  RET

TRYOUT:
        CALL    UNPACK
        JZ      HAVFRE
TRYIN:
        DEC     AX
        JLE     FINDFRE
        XCHG    AX,BX
        CALL    UNPACK
        JZ      HAVFRE
        XCHG    AX,BX
        JP      FINDFRE
HAVFRE:
        XCHG    BX,DX
        MOV     AX,DX
        CALL    PACK
        MOV     BX,AX
        LOOP    ALLOC
        MOV     DX,0FFFH
        CALL    PACK
        MOV     B,[BP+25],1
        POP     BX
        POP     CX              ;Don't need this stuff since we're successful
        POP     DX
        CALL    UNPACK
        POP     [SI]
        XCHG    BX,DI
        OR      DI,DI
        JNZ     RET11
        MOV     DI,[FCB]
        SEG     ES
        MOV     [DI+FIRCLUS],BX
RET12:  RET


RELEASE:

; Inputs:
;       DS = CS
;       BX = Cluster in file
;       SI = FAT pointer
;       BP = Base of drive parameters
; Function:
;       Frees cluster chain starting with [BX]
; AX,BX,DX,DI all destroyed. Other registers unchanged.

        XOR     DX,DX
RELBLKS:
; Enter here with DX=0FFFH to put an end-of-file mark
; in the first cluster and free the rest in the chain.
        CALL    UNPACK
        JZ      RET12
        MOV     AX,DI
        CALL    PACK
        CMP     AX,0FF8H
        MOV     BX,AX
        JB      RELEASE
RET13:  RET


GETEOF:

; Inputs:
;       BX = Cluster in a file
;       SI = Base of drive FAT
;       DS = CS
; Outputs:
;       BX = Last cluster in the file
; DI destroyed. No other registers affected.

        CALL    UNPACK
        CMP     DI,0FF8H
        JAE     RET13
        MOV     BX,DI
        JP      GETEOF


SRCHFRST: ;System call 17
        CALL    GETFILE
SAVPLCE:
; Search-for-next enters here to save place and report
; findings.
        JC      KILLSRCH
        CMP     BH,-1
        JZ      SRCHDEV
        MOV     AX,[LASTENT]
        SEG     ES
        MOV     [DI+FILDIRENT],AX
;Information in directory entry must be copied into the first
; 33 bytes starting at the disk transfer address.
        MOV     SI,BX
        LES     DI,[DMAADD]
        MOV     AX,00FFH
        CMP     AL,[EXTFCB]
        JNZ     NORMFCB
        STOSW
        INC     AL
        STOSW
        STOSW
        MOV     AL,[ATTRIB]
        STOSB
NORMFCB:
        MOV     AL,[BP+0]
        INC     AL
        STOSB
        MOVSB
        MOV     CX,5
        REP
        MOVSW
        XOR     AX,AX
        CMP     B,[BP+26],-1
        JNZ     LAB110
        STOSB
        MOV     CX,7
        REP
        STOSW
        MOVSW
        MOVSW
        MOVSB
        STOSB
        RET

LAB110:
        MOV     CX,10
        REP
        MOVSW
        MOVSB
        RET

KILLSRCH:
        SEG     ES
KILLSRCH1:
;The purpose of the KILLSRCH1 label is to provide a jump label to the following
;   instruction which leaves out the segment override.
        MOV     W,[DI+FILDIRENT],-2
        MOV     AL,-1
        RET

SRCHDEV:
        SEG     ES
        MOV     [DI+FILDIRENT],BX
        LES     DI,[DMAADD]
        XOR     AX,AX
        STOSB           ;Zero drive byte
        SUB     SI,3            ;Point to device name
        MOVSW
        MOVSB
        MOV     AX,2020H
        MOV     CX,4
        REP
        STOSW
        XOR     AX,AX
        MOV     CX,10
        REP
        STOSW
        STOSB
RET14:  RET

SRCHNXT: ;System call 18
        CALL    MOVNAME
        MOV     DI,DX
        JC      KILLSRCH1
        PUSH    DX
        PUSH    DS
        MOV     AX,[DI+22]
        PUSH    CS
        POP     DS
        MOV     [LASTENT],AX
        CALL    CONTSRCH
        POP     ES
        POP     DI
        JMP     SAVPLCE


FILESIZE: ;System call 35
        CALL    GETFILE
        MOV     AL,-1
        JC      RET14
        ADD     DI,33           ;Write size in RR field
        SEG     ES
        MOV     CX,[DI+RECSIZ-33]
        OR      CX,CX
        JNZ     RECOK
        MOV     CX,128
RECOK:
        XOR     AX,AX
        XOR     DX,DX           ;Intialize size to zero
        CMP     BH,-1           ;Check for named I/O device
        JZ      DEVSIZ
        INC     SI
        INC     SI              ;Point to length field
        MOV     AX,[SI+2]       ;Get high word of size
        CMP     B,[BP+26],-1
        JNZ     LAB120
        MOV     AH,0
LAB120:
        DIV     AX,CX
        PUSH    AX              ;Save high part of result
        LODSW           ;Get low word of size
        DIV     AX,CX
        OR      DX,DX           ;Check for zero remainder
        POP     DX
        JZ      DEVSIZ
        INC     AX              ;Round up for partial record
        JNZ     DEVSIZ          ;Propagate carry?
        INC     DX
DEVSIZ:
        STOSW
        MOV     AX,DX
        STOSB
        MOV     AL,0
        CMP     CX,64
        JAE     RET14           ;Only 3-byte field if RECSIZ >= 64
        SEG     ES
        MOV     [DI],AH
        RET


SETDMA: ;System call 26
        SEG     CS
        MOV     [DMAADD],DX
        SEG     CS
        MOV     [DMAADD+2],DS
        RET

GETFATPT: ;System call 27
        MOV     AX,CS
        MOV     DS,AX
        MOV     BP,[CURDRV]
        CALL    LAB050
        LEA     BX,[BP+26]
        MOV     AL,[BP+3]
        INC     AL
        MOV     DX,[BP+12]
        DEC     DX
        MOV     B,[BP+25],1
        MOV     CX,[BP+1]
        LDS     SI,[SPSAVE]
        MOV     [SI+2],BX
        MOV     [SI+6],DX
        MOV     [SI+4],CX
        MOV     W,[SI+14],CS
        RET


GETDSKPT: ;System call 31
        SEG     CS
        MOV     BX,[CURDRV]
        SEG     CS
        LDS     SI,[SPSAVE]
        MOV     [SI+2],BX
        MOV     W,[SI+14],CS
        RET


DSKRESET: ;System call 13
        PUSH    CS
        POP     DS
WRTFATS:
; DS=CS. Writes back all dirty FATs. All registers destroyed.
        MOV     W,AX,[BUFDRVNO]
        OR      AH,AH
        JZ      NOBUF
        INC     AX
        CALL    GETTHISDRV
        MOV     B,[DIRTYBUF],0
        MOV     DX,[BUFSECNO]
        MOV     BX,[BUFFER]
        MOV     CX,1
        CALL    DWRITE
NOBUF:
        MOV     CL,[NUMIO]
        MOV     CH,0
        MOV     SI,DRVTAB
WRTFAT:
        LODSW
        PUSH    CX
        PUSH    SI
        MOV     BP,AX
        CALL    CHKFATWRT
        POP     SI
        POP     CX
        LOOP    WRTFAT
        RET


GETDRV: ;System call 25
        SEG     CS
        MOV     BP,[CURDRV]
        MOV     AL,[BP+0]
RET15:  RET


SETRNDREC: ;System call 36
        CALL    GETREC
        MOV     [DI+33],AX
        MOV     [DI+35],DL
        CMP     [DI+RECSIZ],64
        JAE     RET15
        MOV     [DI+36],DH      ;Set 4th byte only if record size < 64
RET16:  RET


SELDSK: ;System call 14
        MOV     DH,0
        MOV     BX,DX
        PUSH    CS
        POP     DS
        MOV     AL,[NUMIO]
        CMP     BL,AL
        JNB     RET16
        SHL     BX
        MOV     DX,[BX+DRVTAB]
        MOV     [CURDRV],DX
RET17:  RET

BUFIN:  ;System call 10
        MOV     AX,CS
        MOV     ES,AX
        MOV     SI,DX
        MOV     CH,0
        LODSW
        OR      AL,AL
        JZ      RET17
        MOV     BL,AH
        MOV     BH,CH
        CMP     AL,BL
        JBE     NOEDIT
        CMP     B,[BX+SI],0DH
        JZ      EDITON
NOEDIT:
        MOV     BL,CH
EDITON:
        MOV     DL,AL
        DEC     DX
NEWLIN:
        SEG     CS
        MOV     AL,[CARPOS]
        SEG     CS
        MOV     [STARTPOS],AL
        PUSH    SI
        MOV     DI,INBUF
        MOV     AH,CH
        MOV     BH,CH
        MOV     DH,CH
GETCH:
        CALL    IN
        CMP     AL,7FH
        JZ      BACKSP
        CMP     AL,8
        JZ      BACKSP
        CMP     AL,13
        JZ      ENDLIN
        CMP     AL,10
        JZ      PHYCRLF
        CMP     AL,CANCEL
        JZ      KILNEW
        CMP     AL,27
        JZ      ESC
SAVCH:
        CMP     DH,DL
        JNB     GETCH
        STOSB
        INC     DH
        CALL    BUFOUT
        OR      AH,AH
        JNZ     GETCH
        CMP     BH,BL
        JAE     GETCH
        INC     SI
        INC     BH
        JP      GETCH

ESC:
        CALL    IN
        MOV     CL,ESCTABLEN
        PUSH    DI
        MOV     DI,ESCTAB
        REPNE
        SCASB
        POP     DI
        AND     CL,0FEh
        MOV     BP,CX
        SEG     CS
        JMP     [BP+ESCFUNC]

ENDLIN:
        STOSB
        CALL    OUT
        POP     DI
        MOV     [DI-1],DH
        INC     DH
COPYNEW:
        MOV     BP,ES
        MOV     BX,DS
        MOV     ES,BX
        MOV     DS,BP
        MOV     SI,INBUF
        MOV     CL,DH
        REP
        MOVSB
        RET
CRLF:
        MOV     AL,13
        CALL    OUT
        MOV     AL,10
        JMP     OUT

PHYCRLF:
        CALL    CRLF
        JP      GETCH

KILNEW:
        MOV     AL,"\"
        CALL    OUT
        POP     SI
PUTNEW:
        CALL    CRLF
        SEG     CS
        MOV     AL,[STARTPOS]
        CALL    TAB
        JMP     NEWLIN

BACKSP:
        OR      DH,DH
        JZ      OLDBAK
        CALL    BACKUP
        SEG     ES
        MOV     AL,[DI]
        CMP     AL," "
        JAE     OLDBAK
        CMP     AL,9
        JZ      BAKTAB
        CALL    BACKMES
OLDBAK:
        OR      AH,AH
        JNZ     GETCH1
        OR      BH,BH
        JZ      GETCH1
        DEC     BH
        DEC     SI
GETCH1:
        JMP     GETCH
BAKTAB:
        PUSH    DI
        DEC     DI
        STD
        MOV     CL,DH
        MOV     AL," "
        PUSH    BX
        MOV     BL,7
        JCXZ    FIGTAB
FNDPOS:
        SCASB
        JNA     CHKCNT
        SEG     ES
        CMP     B,[DI+1],9
        JZ      HAVTAB
        DEC     BL
CHKCNT:
        LOOP    FNDPOS
FIGTAB:
        SEG     CS
        SUB     BL,[STARTPOS]
HAVTAB:
        SUB     BL,DH
        ADD     CL,BL
        AND     CL,7
        CLD
        POP     BX
        POP     DI
        JZ      OLDBAK
TABBAK:
        CALL    BACKMES
        LOOP    TABBAK
        JP      OLDBAK
BACKUP:
        DEC     DH
        DEC     DI
BACKMES:
        MOV     AL,8
        CALL    OUT
        MOV     AL," "
        CALL    OUT
        MOV     AL,8
        JMP     OUT

TWOESC:
        MOV     AL,ESCCH
        JMP     SAVCH

COPYLIN:
        MOV     CL,BL
        SUB     CL,BH
        JP      COPYEACH

COPYSTR:
        CALL    FINDOLD
        JP      COPYEACH

COPYONE:
        MOV     CL,1
COPYEACH:
        CMP     DH,DL
        JZ      GETCH2
        CMP     BH,BL
        JZ      GETCH2
        LODSB
        STOSB
        CALL    BUFOUT
        INC     BH
        INC     DH
        LOOP    COPYEACH
GETCH2:
        JMP     GETCH

SKIPONE:
        CMP     BH,BL
        JZ      GETCH2
        INC     BH
        INC     SI
        JMP     GETCH

SKIPSTR:
        CALL    FINDOLD
        ADD     SI,CX
        ADD     BH,CL
        JMP     GETCH

FINDOLD:
        CALL    IN
        MOV     CL,BL
        SUB     CL,BH
        JZ      NOTFND
        DEC     CX
        JZ      NOTFND
        PUSH    ES
        PUSH    DS
        POP     ES
        PUSH    DI
        MOV     DI,SI
        INC     DI
        REPNE
        SCASB
        POP     DI
        POP     ES
        JNZ     NOTFND
        NOT     CL
        ADD     CL,BL
        SUB     CL,BH
RET30:  RET
NOTFND:
        POP     BP
        JMP     GETCH

REEDIT:
        MOV     AL,"@"
        CALL    OUT
        POP     DI
        PUSH    DI
        PUSH    ES
        PUSH    DS
        CALL    COPYNEW
        POP     DS
        POP     ES
        POP     SI
        MOV     BL,DH
        JMP     PUTNEW

ENTERINS:
        MOV     AH,-1
        JMP     GETCH

EXITINS:
        MOV     AH,0
        JMP     GETCH

ESCFUNC DW      GETCH
        DW      TWOESC
        DW      EXITINS
        DW      ENTERINS
        DW      BACKSP
        DW      REEDIT
        DW      KILNEW
        DW      COPYLIN
        DW      SKIPSTR
        DW      COPYSTR
        DW      SKIPONE
        DW      COPYONE

BUFOUT:
        CMP     AL," "
        JAE     OUT
        CMP     AL,9
        JZ      OUT
        PUSH    AX
        MOV     AL,"^"
        CALL    OUT
        POP     AX
        OR      AL,40H
        JP      OUT

CONOUT: ;System call 2
        MOV     AL,DL
OUT:
        CMP     AL,20H
        JB      CTRLOUT
        CMP     AL,7FH
        JZ      OUTCH
        SEG     CS
        INC     B,[CARPOS]
OUTCH:
        PUSH    AX
        CALL    STATCHK
        POP     AX
        CALL    BIOSOUT,BIOSSEG
        SEG     CS
        TEST    B,[PFLAG],-1
        JZ      RET30
        CALL    BIOSPRINT,BIOSSEG
RET18:  RET

STATCHK:
        CALL    BIOSSTAT,BIOSSEG
        JZ      RET18
INCHK:
        CALL    BIOSIN,BIOSSEG
        CMP     AL,19
        JNZ     LAB130
        CALL    BIOSIN,BIOSSEG
LAB130:
        CMP     AL,'P'-'@'
        JZ      PRINTON
        CMP     AL,'N'-'@'
        JZ      PRINTOFF
        CMP     AL,'C'-'@'
        JNZ     RET18
; Ctrl-C handler.
; "^C" and CR/LF is printed. Then the user registers are restored and the
; user CTRL-C handler is executed. At this point the top of the stack has
; 1) the interrupt return address should the user CTRL-C handler wish to
; allow processing to continue; 2) the original interrupt return address
; to the code that performed the function call in the first place. If the
; user CTRL-C handler wishes to continue, it must leave all registers
; unchanged and IRET. The function that was interrupted will simply be
; repeated.
        MOV     AL,3            ;Display "^C"
        CALL    BUFOUT
        CALL    CRLF
        CLI                     ;Prepare to play with stack
        SEG     CS
        MOV     SS,[SSSAVE]
        SEG     CS
        MOV     SP,[SPSAVE]     ;User stack now restored
        POP     AX
        POP     BX
        POP     CX
        POP     DX
        POP     SI
        POP     DI
        POP     BP
        POP     DS
        POP     ES              ;User registers now restored
        INT     CONTC           ;Execute user Ctrl-C handler
        JMP     COMMAND         ;Repeat command otherwise

PRINTON:
        SEG     CS
        MOV     B,[PFLAG],1
        RET

PRINTOFF:
        SEG     CS
        MOV     B,[PFLAG],0
        RET

CTRLOUT:
        CMP     AL,13
        JZ      ZERPOS
        CMP     AL,8
        JZ      BACKPOS
        CMP     AL,9
        JNZ     OUTCHJ
        SEG     CS
        MOV     AL,[CARPOS]
        OR      AL,0F8H
        NEG     AL
TAB:
        PUSH    CX
        MOV     CL,AL
        MOV     CH,0
        JCXZ    POPTAB
TABLP:
        MOV     AL," "
        CALL    OUT
        LOOP    TABLP
POPTAB:
        POP     CX
RET19:  RET

ZERPOS:
        SEG     CS
        MOV     B,[CARPOS],0
OUTCHJ: JMP     OUTCH

BACKPOS:
        SEG     CS
        DEC     B,[CARPOS]
        JMP     OUTCH


CONSTAT: ;System call 11
        CALL    BIOSSTAT,BIOSSEG
        JZ      RET19
        OR      AL,-1
        RET


CONIN:  ;System call 1
        CALL    IN
        PUSH    AX
        CALL    OUT
        POP     AX
        RET


IN:     ;System call 8
        CALL    INCHK
        JZ      IN
RET29:  RET

RAWIO:  ;System call 6
        MOV     AL,DL
        CMP     AL,-1
        JNZ     RAWOUT
        CALL    BIOSSTAT,BIOSSEG
        JZ      RET29
RAWINP: ;System call 7
        CALL    BIOSIN,BIOSSEG
        RET
RAWOUT:
        CALL    BIOSOUT,BIOSSEG
        RET

LIST:   ;System call 5
        MOV     AL,DL
LISTOUT:
        PUSH    AX
        CALL    STATCHK
        POP     AX
        CALL    BIOSPRINT,BIOSSEG
RET20:  RET

PRTBUF: ;System call 9
        MOV     SI,DX
OUTSTR:
        LODSB
        CMP     AL,"$"
        JZ      RET20
        CALL    OUT
        JP      OUTSTR

OUTMES: ;String output for internal messages
        SEG     CS
        LODSB
        CMP     AL,"$"
        JZ      RET20
        CALL    OUT
        JP      OUTMES


MAKEFCB: ;Interrupt call 41
DRVBIT  EQU     2
NAMBIT  EQU     4
EXTBIT  EQU     8
        MOV     DL,0            ;Flag--not ambiguous file name
        OR      AL,AL
        JZ      LAB140
        CALL    SCANB
        CALL    DELIM
        JNZ     LAB150
        INC     SI

LAB140:
        CALL    SCANB

LAB150:
        MOV     AL,0
        CMP     B,[SI]," "
        JB      HAVDRV
        CMP     B,[SI+1],":"
        JNZ     HAVDRV
        CALL    GETLET
        INC     SI
        SUB     AL, 64
        JBE     BADDRV
        SEG     CS
        CMP     AL,[NUMIO]
        JBE     HAVDRV
BADDRV:
        MOV     DL,-1
HAVDRV:
        STOSB           ;Put drive specifier in first byte
        MOV     CX,8
        CALL    GETWORD         ;Get 8-letter file name
        CMP     B,[SI],"."
        JNZ     NODOT
        INC     SI              ;Skip over dot if present
NODOT:
        MOV     CX,3            ;Get 3-letter extension
        CALL    GETWORD
        SEG     CS
        LDS     BX,[SPSAVE]
        MOV     [BX+SISAVE],SI
        XOR     AX,AX
        STOSW
        STOSW
        MOV     AL,DL
        RET

GETWORD:
        CALL    GETLET
        JZ      FILLNAM
        CMP     AL," "
        JBE     FILLNAM
        CMP     AL,"*"
        JNZ     NOSTAR
        MOV     AL,"?"
        DEC     CX
        REP
        STOSB
        INC     CX
NOSTAR:
        STOSB
        CMP     AL,"?"
        JNZ     LAB160
        OR      DL,1
LAB160:
        LOOP    GETWORD
        INC     SI
FILLNAM:
        MOV     AL," "
        REP
        STOSB
        DEC     SI
        RET

SCANB:
        LODSB
        CALL    SPCHK
        JZ      SCANB
        DEC     SI
RET21:  RET

GETLET:
;Get a byte from [SI], convert it to upper case, and compare for delimiter.
;ZF set if a delimiter, CY set if a control character (other than TAB).
        LODSB
        AND     AL,7FH
        CMP     AL,"a"
        JB      CHK
        CMP     AL,"z"
        JA      CHK
        SUB     AL,20H          ;Convert to upper case
CHK:
        CMP     AL,"."
        JZ      RET21
        CMP     AL,":"
        JZ      RET21
        CMP     AL,'"'
        JZ      RET21
        CMP     AL,"/"
        JZ      RET21
        CMP     AL,"["
        JZ      RET21
        CMP     AL,"]"
        JZ      RET21

DELIM:
        CMP     AL,"+"
        JZ      RET21
        CMP     AL,"="
        JZ      RET21
        CMP     AL,";"
        JZ      RET21
        CMP     AL,","
        JZ      RET21
SPCHK:
        CMP     AL," "
        JZ      RET21
        CMP     AL,9
        RET

SETVECT: ; Interrupt call 37
        XOR     BX,BX
        MOV     ES,BX
        MOV     BL,AL
        SHL     BX
        SHL     BX
        SEG     ES
        MOV     [BX],DX
        SEG     ES
        MOV     [BX+2],DS
        RET


NEWBASE: ; Interrupt call 38
        MOV     ES,DX
        SEG     CS
        LDS     SI,[SPSAVE]
        MOV     DS,[SI+CSSAVE]
        XOR     SI,SI
        MOV     DI,SI
        MOV     CX,80H
        REP
        MOVSW

SETMEM:

; Inputs:
;       AX = Size of memory in paragraphs
;       DX = Segment
; Function:
;       Completely prepares a program base at the 
;       specified segment.
; Outputs:
;       DS = DX
;       ES = DX
;       [0] has INT 20H
;       [2] = First unavailable segment ([ENDMEM])
;       [5] to [9] form a long call to the entry point
;       [10] to [13] have exit address (from INT 22H)
;       [14] to [17] have ctrl-C exit address (from INT 23H)
;       [18] to [21] have fatal error address (from INT 24H)
; DX,BP unchanged. All other registers destroyed.

        XOR     CX,CX
        MOV     DS,CX
        MOV     ES,DX
        MOV     SI,EXIT
        MOV     DI,SAVEXIT
        MOVSW
        MOVSW
        MOVSW
        MOVSW
        MOVSW
        MOVSW
        SEG     CS
        MOV     CX,[ENDMEM]
        SEG     ES
        MOV     [2],CX
        SUB     CX,DX
        CMP     CX,0FFFH
        JBE     HAVDIF
        MOV     CX,0FFFH
HAVDIF:
        MOV     BX,ENTRYPOINTSEG
        SUB     BX,CX
        SHL     CX
        SHL     CX
        SHL     CX
        SHL     CX
        MOV     DS,DX
        MOV     [6],CX
        MOV     [8],BX
        MOV     [0],20CDH       ;"INT INTTAB"
        MOV     B,[5],LONGCALL
        RET

DATE16:
        PUSH    CX
        PUSH    DX
        CALL    READTIME
        POP     DX
        POP     CX
        MOV     W,AX,[MONTH]            ;Fetch month and year
        SHL     AL                      ;Push month to left to make room for day
        SHL     AL
        SHL     AL
        SHL     AL
        SHL     AX
        OR      AL,[DAY]
RET22:  RET

FOURYEARS       EQU     3*365+366

READTIME:
;Gets time in CX:DX. Figures new date if it has changed.
;Uses AX, CX, DX.
        CALL    BIOSGETTIME,BIOSSEG
        CMP     AX,[DAYCNT]     ;See if day count is the same
        JZ      RET22
        CMP     AX,FOURYEARS*30 ;Number of days in 120 years
        JAE     RET22           ;Ignore if too large
        MOV     [DAYCNT],AX
        PUSH    SI
        PUSH    CX
        PUSH    DX              ;Save time
        XOR     DX,DX
        MOV     CX,FOURYEARS    ;Number of days in 4 years
        DIV     AX,CX           ;Compute number of 4-year units
        SHL     AX
        SHL     AX
        SHL     AX              ;Multiply by 8 (no. of half-years)
        MOV     CX,AX           ;<240 implies AH=0
        MOV     SI,YRTAB        ;Table of days in each year
        CALL    DSLIDE          ;Find out which of four years we're in
        SHR     CX              ;Convert half-years to whole years
        JNC     SK              ;Extra half-year?
        ADD     DX,200
SK:
        CALL    SETYEAR
        MOV     CL,1            ;At least at first month in year
        MOV     SI,MONTAB       ;Table of days in each month
        CALL    DSLIDE          ;Find out which month we're in
        MOV     [MONTH],CL
        INC     DX              ;Remainder is day of month (start with one)
        MOV     [DAY],DL
        CALL    WKDAY           ;Set day of week
        POP     DX
        POP     CX
        POP     SI
RET23:  RET

DSLIDE:
        MOV     AH,0
DSLIDE1:
        LODSB           ;Get count of days
        CMP     DX,AX           ;See if it will fit
        JB      RET23           ;If not, done
        SUB     DX,AX
        INC     CX              ;Count one more month/year
        JP      DSLIDE1

SETYEAR:
;Set year with value in CX. Adjust length of February for this year.
        MOV     B,[YEAR],CL
CHKYR:
        TEST    CL,3            ;Check for leap year
        MOV     AL,28
        JNZ     SAVFEB          ;28 days if no leap year
        INC     AL              ;Add leap day
SAVFEB:
        MOV     [MONTAB+1],AL   ;Store for February
        RET

;Days in year
YRTAB   DB      200,166         ;Leap year
        DB      200,165
        DB      200,165
        DB      200,165

;Days of each month
MONTAB  DB      31              ;January
        DB      28              ;February--reset each time year changes
        DB      31              ;March
        DB      30              ;April
        DB      31              ;May
        DB      30              ;June
        DB      31              ;July
        DB      31              ;August
        DB      30              ;September
        DB      31              ;October
        DB      30              ;November
        DB      31              ;December

GETDATE: ;Function call 42
        PUSH    CS
        POP     DS
        CALL    READTIME        ;Check for rollover to next day
        MOV     AX,[YEAR]
        MOV     W,BX,[DAY]
        LDS     W,SI,[SPSAVE]   ;Get pointer to user registers
        MOV     [SI+DXSAVE],BX  ;DH=month, DL=day
        ADD     AX,1980         ;Put bias back
        MOV     [SI+CXSAVE],AX  ;CX=year
        SEG     CS
        MOV     AL,[WEEKDAY]
RET24:  RET

SETDATE: ;Function call 43
        MOV     AL,-1           ;Be ready to flag error
        SUB     CX,1980         ;Fix bias in year
        JC      RET24           ;Error if not big enough
        CMP     CX,119          ;Year must be less than 2100
        JA      RET24
        OR      DH,DH
        JZ      RET24
        OR      DL,DL
        JZ      RET24           ;Error if either month or day is 0
        CMP     DH,12           ;Check against max. month
        JA      RET24
        PUSH    CS
        POP     DS
        CALL    CHKYR           ;Set Feb. up for new year
        MOV     AL,DH
        MOV     BX,MONTAB-1
        XLAT                    ;Look up days in month
        CMP     AL,DL
        MOV     AL,-1           ;Restore error flag, just in case
        JB      RET24           ;Error if too many days
        CALL    SETYEAR
        MOV     W,[DAY],DX      ;Set both day and month
        SHR     CX
        SHR     CX
        MOV     AX,FOURYEARS
        MOV     BX,DX
        MUL     AX,CX
        MOV     B,CL,[YEAR]
        AND     CL,3
        MOV     SI,YRTAB
        MOV     DX,AX
        SHL     CX              ;Two entries per year, so double count
        CALL    DSUM            ;Add up the days in each year
        MOV     CL,BH           ;Month of year
        MOV     SI,MONTAB
        DEC     CX              ;Account for months starting with one
        CALL    DSUM            ;Add up days in each month
        MOV     CL,BL           ;Day of month
        DEC     CX              ;Account for days starting with one
        ADD     DX,CX           ;Add in to day total
        XCHG    AX,DX           ;Get day count in AX
        MOV     [DAYCNT],AX
        CALL    BIOSSETDATE,BIOSSEG
WKDAY:
        MOV     AX,[DAYCNT]
        XOR     DX,DX
        MOV     CX,7
        INC     AX
        INC     AX              ;First day was Tuesday
        DIV     AX,CX           ;Compute day of week
        MOV     [WEEKDAY],DL
        XOR     AL,AL           ;Flag OK
RET25:  RET

DSUM:
        MOV     AH,0
        JCXZ    RET25
DSUM1:
        LODSB
        ADD     DX,AX
        LOOP    DSUM1
        RET

GETTIME: ;Function call 44
        PUSH    CS
        POP     DS
        CALL    READTIME
        LDS     SI,[SPSAVE]     ;Get pointer to user registers
        MOV     [SI+DXSAVE],DX
        MOV     [SI+CXSAVE],CX
        XOR     AL,AL
RET26:  RET

SETTIME: ;Function call 45
;Time is in CX:DX in hours, minutes, seconds, 1/100 sec.
        MOV     AL,-1           ;Flag in case of error
        CMP     CH,24           ;Check hours
        JAE     RET26
        CMP     CL,60           ;Check minutes
        JAE     RET26
        CMP     DH,60           ;Check seconds
        JAE     RET26
        CMP     DL,100          ;Check 1/100's
        JAE     RET26
        CALL    BIOSSETTIME,BIOSSEG
        XOR     AL,AL
        RET


; Default handler for division overflow trap
DIVOV:
        PUSH    SI
        MOV     SI,DIVMES
        CALL    OUTMES
        POP     SI
        INT     23H             ;Use Ctrl-C abort on divide overflow
        IRET

CODSIZ  EQU     $-CODSTRT       ;Size of code segment


;***** DATA AREA *****
        ; ORG     0
CONSTRT EQU     $               ;Start of constants segment

IONAME  DB      "PRN","LST","NUL","AUX","CON"
DIVMES  DB      13,10,"Divide overflow",13,10,"$"
CARPOS  DB      0
STARTPOS DB     0
PFLAG   DB      0
DIRTYDIR DB     0               ;Dirty buffer flag
NUMIO   DB      0               ;Number of disk tables
CONTPOS DW      0
DMAADD  DW      80H             ;User's disk transfer address (disp/seg)
        DW      0
ENDMEM  DW      0
MAXSEC  DW      0
BUFFER  DW      0
BUFSECNO DW     0
BUFDRVNO DB     -1
DIRTYBUF DB     0
BUFDRVBP DW     0
DIRBUFID DW     -1
DAY     DB      0
MONTH   DB      0
YEAR    DW      0
DAYCNT  DW      -1
WEEKDAY DB      0
CURDRV  DW      0               ;Default to drive A
DRVTAB  DW      0               ;Address of start of DPBs
        DS      28
DOSLEN  EQU     CODSIZ+($-CONSTRT)      ;Size of CODE + CONSTANTS segments

; Init code overlaps with data area below

        ORG     DOSLEN
        PUT     DOSLEN+100H
INBUF   DS      128
CONBUF  DS      131                     ;The rest of INBUF and console buffer
LASTENT DW      0
EXITHOLD DS     4
FATBASE DW      0
NAME1   DS      11                      ;File name buffer
ATTRIB  DB      0
NAME2   DS      11
NAME3   DS      12
EXTFCB  DB      0
;WARNING - the following two items are accessed as a word
CREATING DB     0
TEMP:
SPSAVE  DW      0
SSSAVE  DW      0
CONTSTK DW      0
SECCLUSPOS DB   0       ;Position of first sector within cluster
DSKERR  DB      0
TRANS   DB      0
PREREAD DB      0       ;0 means preread; 1 means optional
READOP  DB      0
THISDRV DB      0

        ALIGN
FCB     DW      0       ;Address of user FCB
NEXTADD DW      0
RECPOS  DS      4
RECCNT  DW      0
LASTPOS DW      0
CLUSNUM DW      0
SECPOS  DW      0       ;Position of first sector accessed
VALSEC  DW      0       ;Number of valid (previously written) sectors
BYTSECPOS DW    0       ;Position of first byte within sector
BYTPOS  DS      4       ;Byte position in file of access
BYTCNT1 DW      0       ;No. of bytes in first sector
BYTCNT2 DW      0       ;No. of bytes in last sector
SECCNT  DW      0       ;No. of whole sectors
ENTFREE DW      0

        DS      26H     ;Stack space
IOSTACK DS      3CH
DSKSTACK:

DIRBUF:

;Init code below overlaps with data area above

        ORG     DOSLEN
        PUT     DOSLEN+100H

MOVFAT:
        SEG     ES
        REP
        MOVSB
        CLD

FLGFAT:
        CALL    SETMEM
        RET     L

DOSINIT:
        CLI
        CLD
        PUSH    CS
        POP     ES
        LODSB
        SEG     ES
        MOV     B,[NUMIO],AL
        MOV     BX,DRVTAB
        MOV     DI,MEMSTRT+2

LAB200:
        SEG     ES
        MOV     [BX],DI
        MOV     BP,DI
        INC     BX
        INC     BX
        SEG     ES
        MOV     AL,[DRVCNT]
        STOSB
        LODSW
        PUSH    SI
        MOV     SI,AX
        LODSW
        STOSW
        MOV     DX,AX
        SEG     ES
        CMP     W,AX,[MAXSEC]
        JBE     NOTMAX
        SEG     ES
        MOV     W,[MAXSEC],AX

NOTMAX:
        LODSB
        DEC     AL
        STOSB
        JZ      HAVSHFT
        CBW

FIGSHFT:
        INC     AH
        SAR     AL
        JNZ     FIGSHFT
        MOV     AL,AH

HAVSHFT:
        STOSB
        MOVSW
        MOVSB
        MOVSW
        MOV     AX,DX
        MOV     CL,5
        SHR     AX,CL
        MOV     CX,AX
        DEC     AX
        SEG     ES
        ADD     AX,[BP+8]
        XOR     DX,DX
        DIV     AX,CX
        STOSW
        SHR     AX
        ADC     AX,0
        SEG     ES
        MOV     [MEMSTRT],AX
        MOVSW

FNDFATSIZ:
        MOV     AL,1
        MOV     DX,1

GETFATSIZ:
        PUSH    DX
        CALL    FIGFATSIZ
        POP     DX
        CMP     AL,DL
        JZ      HAVFATSIZ
        CMP     AL,DH
        MOV     DH,DL
        MOV     DL,AL
        JNZ     GETFATSIZ
        SEG     ES
        DEC     W,[BP+12]
        JP      FNDFATSIZ

HAVFATSIZ:
        STOSB
        SEG     ES
        MUL     AL,[BP+7]
        SEG     ES
        ADD     AX,[BP+5]
        STOSW
        MOV     DX,AX
        SEG     ES
        ADD     AX,[MEMSTRT]
        STOSW
        XCHG    AX,CX
        STOSW
        XCHG    AX,DX
        SEG     ES
        ADD     AX,[BP+10]
        STOSW
        CALL    FIGMAX
        XCHG    AX,CX
        STOSW
        MOV     AX,0FFH
        STOSB
        SEG     ES
        MOV     AL,[BP+14]
        SEG     ES
        MUL     W,AX,[BP+1]
        ADD     DI,AX
        POP     SI
        SEG     ES
        MOV     AL,[DRVCNT]
        INC     AL
        SEG     ES
        MOV     [DRVCNT],AL
        SEG     ES
        CMP     B,AL,[NUMIO]
        JNB     LAB210
        JMP     LAB200

LAB210:
        LODSW
        SEG     ES
        MOV     W,BX,[MAXSEC]
        MOV     AX,DSKSTACK
        ADD     AX,BX
        SEG     ES
        MOV     W,[BUFFER],AX
        PUSH    DI
        ADD     DI,BX
        ADD     DI,BX
        ADD     DI,VAR000
        MOV     CL,4
        SHR     DI,CL
        MOV     BP,DI
        XOR     AX,AX
        MOV     DS,AX
        MOV     ES,AX
        MOV     DI,INTBASE
        MOV     AX,QUIT
        STOSW
        MOV     AX,CS
        MOV     B,[ENTRYPOINT],LONGJUMP
        MOV     W,[ENTRYPOINT+1],ENTRY
        MOV     [ENTRYPOINT+3],AX
        MOV     W,[0],DIVOV
        MOV     [2],AX
        MOV     CX,9
        REP
        STOSW
        MOV     W,[INTBASE+4],COMMAND
        MOV     W,[INTBASE+12],IRET
        MOV     W,[INTBASE+16],IRET
        MOV     AX,BIOSREAD
        STOSW
        MOV     AX,BIOSSEG
        STOSW
        STOSW
        STOSW
        MOV     W,[INTBASE+18H],BIOSWRITE
        MOV     DX,CS
        MOV     DS,DX
        ADD     DX,BP
        MOV     W,[DMAADD],128
        MOV     W,[DMAADD+2],DX
        MOV     W,AX,[DRVTAB]
        MOV     W,[CURDRV],AX
        MOV     CX,DX
        MOV     BX,15

LAB220:
        INC     CX
        JZ      LAB230
        MOV     DS,CX
        MOV     AL,[BX]
        NOT     AL
        MOV     [BX],AL
        CMP     AL,[BX]
        NOT     AL
        MOV     [BX],AL
        JZ      LAB220

LAB230:
        SEG     CS
        MOV     W,[ENDMEM],CX
        XOR     CX,CX
        MOV     DS,CX
        MOV     W,[EXIT],100H
        MOV     W,[EXIT+2],DX
        MOV     SI,HEADER
        CALL    OUTMES
        PUSH    CS
        POP     DS
        PUSH    CS
        POP     ES
        POP     SI
        MOV     W,AX,[MAXSEC]
        SHL     AX
        ADD     AX,0FFE0H
        JZ      LAB250
        MOV     DI,DRVTAB
        MOV     CX,15

LAB240:
        ADD     [DI],AX
        INC     DI
        INC     DI
        LOOP    LAB240
        MOV     CX,SI
        MOV     SI,MEMSTRT+2
        SUB     CX,SI
        MOV     DI,AX
        ADD     DI,SI
        OR      AX,AX
        JS      MOVJMP
        DEC     CX
        ADD     DI,CX
        ADD     SI,CX
        INC     CX
        STD

MOVJMP:
        JMP     MOVFAT

LAB250:
        JMP     FLGFAT

FIGFATSIZ:
        SEG     ES
        MUL     AL,[BP+7]
        SEG     ES
        ADD     AX,[BP+5]
        SEG     ES
        ADD     AX,[MEMSTRT]

FIGMAX:
        SEG     ES
        SUB     AX,[BP+12]
        NEG     AX
        SEG     ES
        MOV     CL,[BP+4]
        SHR     AX,CL
        INC     AX
        MOV     CX,AX
        INC     AX
        MOV     DX,AX
        SHR     DX
        ADC     AX,DX
        SEG     ES
        MOV     SI,[BP+1]
        ADD     AX,SI
        DEC     AX
        XOR     DX,DX
        DIV     AX,SI
        RET

DRVCNT  DB      0

MEMSTRT:
ADJFAC  EQU     DIRBUF-MEMSTRT
VAR000  EQU     0FFEFH
        END
