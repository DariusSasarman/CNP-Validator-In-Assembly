; *******************************************************************************************************
; File:        MACLIB.ASM
; Author:      Sasarman Darius-Eric
; Date:        2025-04-28
; Description:
;   MACLIB.ASM contains several DOS 'interrupt 21h' macros for 
;   16-bit assembly programs in MS-DOS, for x86 architecture. 
;   These macros handle console I/O operations and basic file 
;   management tasks such as opening, closing, reading, and writing files.
;
;   Key Features:
;     - Console I/O:
;         - NEW_LINE                  : output carriage return + line feed
;         - WRITE_CHARACTER <char>    : print a single character
;         - SCAN_STRING_FROM_KEYBOARD : DOS buffered string input 
;         - SCAN_CHAR                 : DOS single character input 
;     - File Operations:
;         - OPEN_FILE <name>          : open existing file 
;         - CLOSE_FILE <handle>       : close already open file handle 
;         - GET_CNP_FROM_FILE         : read fixed-length record from file
;         - FINISH_READING_FILE       : close file via handle pointer
;         - RECREATE_FILE_ON_TOP_OF_ORIGINAL : create/overwrite file, store new handle
;         - STORE_FROM_MEMORY_TO_FILE : write a memory buffer to file 
;     - System Date:
;         - GET_TODAY_DATE            : retrieve current date
;         - SET_TODAY_DATE            : set system date
; *******************************************************************************************************

NEW_LINE MACRO          ;MACRO USED FOR WRITING A NEW LINE AFTER EACH INPUT/OUTPUT
    PUSH    AX          ;SAVE AX VALUE
    PUSH    DX          ;SAVE DX VALUE
    MOV     AH, 02H     ;SET OPERATION TO WRITE
    MOV     DL, 0DH     ;'CARRIAGE RETURN' CHARACTER TO BE WRITTEN
    INT     21H         ;EXECUTE
                        ;CURSOR MOVED TO BEGINING OF LINE
    MOV     DL, 0AH     ;'LINE FEED' CHARACTER TO BE WRITTEN
    INT     21H         ;EXECUTE
                        ;CURSOR MOVED TO NEXT LINE
    POP     DX          ;RESTORE DX VALUE
    POP     AX          ;RESTORE AX VALUE
ENDM

WRITE_CHARACTER MACRO CHARACTER:REQ ;MACRO USED FOR WRITING A SINGLE CHARACTER
    PUSH    AX                      ; SAVE AX
    PUSH    DX                      ; SAVE DX 
    MOV     DL, CHARACTER           ; LOAD CHARACTER TO PRINT
    MOV     AH, 02h                 ; WRITE CHARACTER TO STDOUT
    INT     21H                     ; CALL DOS SUB-ROUTINE
    POP     DX                      ; RESTORE DX
    POP     AX                      ; RESTORE AX
ENDM

SCAN_STRING_FROM_KEYBOARD MACRO LOCATION:REQ    ;MACRO USED FOR READING A LINE FROM KEYBOARD
    PUSH    AX                                  ;SAVE AX
    PUSH    DX                                  ;SAVE DX
    MOV     DX,LOCATION                         ;LOAD POINTER TO BUFFER
    MOV     AH,0AH                              ;SELECT THE READ 
    INT     21H                                 ;CALL DOS SUB-ROUTINE 
    POP     DX                                  ;RESTORE DX
    POP     AX                                  ;RESTORE AX
ENDM

SCAN_CHAR MACRO                                 ;READ ONE CHARACTER FROM THE SCREEN
        MOV AH,1                                ;STORES IT IN AL AFTER EXECUTION
        INT 21H                                 ;NO BUFFER NEEDED
ENDM

OPEN_FILE MACRO FILE_NAME_OFFSET:REQ            ;OPENING FILE FOR READ AND WRITE    
    XOR AX,AX                                   ;(CH-13 PAGE 725)
    MOV AH,3DH                                  ;OPEN FILE FUNCTION CALL
    MOV AL,00H                                  ;READ ACCESS VALUE
    MOV DX, FILE_NAME_OFFSET                    ;LOAD FILENAME POINTER
    INT 21H                                     ;CALL SYSTEM INTERRUPT
ENDM

CLOSE_FILE MACRO FILE_HANDLE_VAL:REQ            ;MACRO USED FOR CLOSING THE FILE
    MOV AH,3EH                                  ;SELECT CLOSING FILE OPERATION
    MOV BX,FILE_HANDLE_VAL                      ;SELECT FILE HANDLE
    INT 21H                                     ;CALL SYSTEM INTERUPT
ENDM

SET_TODAY_DATE MACRO OFFSET_YEAR:REQ, OFFSET_MONTH:REQ, OFFSET_DAY:REQ;MACRO USED FOR SETTING THE DATE
    MOV CX, WORD PTR [OFFSET_YEAR]                ;LOAD YEAR
    MOV DH, BYTE PTR [OFFSET_MONTH]               ;LOAD MONTH
    MOV DL, BYTE PTR [OFFSET_DAY]                 ;LOAD DAY
    MOV AH, 2BH                                   ;SET SYSTEM DATE FUNCTION CALL
    INT 21H                                       ;CALL SYSTEM INTERRUPT
ENDM

GET_TODAY_DATE MACRO
    MOV AH,2AH                                  ;GET SYSTEM DATE
    INT 21H                                     ;CALL SYSTEM INTERUPT
ENDM

FINISH_READING_FILE MACRO OFFSET_FILE_HANDLE:REQ;MACRO USED FOR CLOSING A TARGET FILE
    MOV BX, WORD PTR [OFFSET_FILE_HANDLE]       ;TELL DOS WHAT FILE TO CLOSE
    MOV AH,3EH                                  ;TELL DOS THAT IT'S CLOSING A FILE
    INT 21H
ENDM

RECREATE_FILE_ON_TOP_OF_ORIGINAL MACRO OFFSET_STRING_CHOICE :REQ, OFFSET_FILE_HANDLE:REQ
    MOV AH,3CH                          ;CREATE FILE MODE
    MOV CX,0                            ;TELL IT NOT TO WRITE ANY BYTES
    MOV DX, OFFSET_STRING_CHOICE        ;WHAT THE NAME OF THE NEW HANDLE IS
    INT 21H
    MOV SI, OFFSET_FILE_HANDLE          ;WHERE TO STORE THE NEW HANDLE
    MOV WORD PTR [SI], AX               ;SAVE THE NEW FILE HANDLE
ENDM

STORE_FROM_MEMORY_TO_FILE MACRO OFFSET_FILE_HANDLE:REQ, OFFSET_FINAL_FILE_POINTER:REQ,OFFSET_FINAL_FILE:REQ
    MOV AH,40H                                  ;WRITE TO FILE MODE
    MOV SI, OFFSET_FILE_HANDLE
    MOV BX, WORD PTR [SI]                       ;SET FILE HANDLE
    MOV SI, OFFSET_FINAL_FILE_POINTER
    MOV CX, WORD PTR [SI]                       ;SET HOW MANY CHARACTERS ARE WRITTEN
    MOV DX, OFFSET_FINAL_FILE                   ;WHERE TO TAKE THE CHARACTERS FROM
    INT 21H
ENDM

GET_CNP_FROM_FILE MACRO OFFSET_FILE_HANDLE:REQ,OFFSET_CNP:REQ;MACRO USED FOR READING A LINE FROM THE FILE
        MOV AH,3FH                              ;SET READING MODE
        MOV SI, OFFSET_FILE_HANDLE
        MOV BX,WORD PTR [SI]                    ;SET FILE HANDLE
        MOV CX,0FH                              ;SET CHARACTER READ COUNT (13 DIGITS + CRLF)
        MOV DX,OFFSET_CNP                       ;WHERE TO STORE SAID CHARACTERS
        INT 21H                                 ;CALL SYSTEM INTERUPT
ENDM
