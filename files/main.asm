; *******************************************************************************************************
; File:        MAIN.ASM
; Author:      Sasarman Darius-Eric
; Date:        2025-04-28
; Course:      Assembly Language Programming (E. Cebuc)
; Target:      x86 Real/DOS (16-bit) using MASM/TASM-compatible syntax
; Assembler:   TASM
; *******************************************************************************************************
; Description:
;   CNP Verificator – used for validating Romanian Personal Numeric Codes in two ways:
;     1) Interactive mode: user enters a single CNP, verifies it, and the program
;        displays “Valid.” or “Invalid.”. Does this until the user quits with ‘Q’.
;     2) Batch mode: reads a file with CNPs on each line, appends ‘V’ (valid) or ‘N’ (invalid),
;         keeps track of results in memory then rewrites the original file.
;         Also collects the following statistics:
;         - total valid / invalid counts
;         - count of male / female people
;         - age groups: under 18, 18–65, over 65
;
;   Implements the official CNP check algorithm:
;     - Ensure input length is 13 digits
;     - Parse fields: S (sex/century), YY (year), MM (month), DD (day),
;       JJ (county), NNN (sequence), C (control digit)
;     - Compute weighted checksum using weights [2,7,9,1,4,6,3,5,8,2,7,9]
;     - Check if county code is valid
;
; *******************************************************************************************************
; External Macros  (from MACLIB.ASM)
; *******************************************************************************************************
; NEW_LINE                      ; write CR+LF on screen
; WRITE_CHARACTER <char>        ; output one character on screen
; SCAN_STRING_FROM_KEYBOARD     ; buffered line input 
; SCAN_CHAR                     ; single-char input 
; OPEN_FILE <name>              ; open existing file for read 
; CLOSE_FILE <handle>           ; close file, indicted by a given handle
; GET_TODAY_DATE                ; load system date 
; FINISH_READING_FILE <hptr>    ; close file via handle pointer
; RECREATE_FILE_ON_TOP_OF_ORIGINAL <name>,<hptr>
;                               ; overwrite file by creating on top, store new handle
; STORE_FROM_MEMORY_TO_FILE <hptr>,<ptrCnt>,<bufPtr>
;                               ; write `ptrCnt` bytes from `bufPtr` to 'hptr' file
; GET_CNP_FROM_FILE <hptr>,<dest>; read 15 bytes (CNP+CRLF) into `dest`
; *******************************************************************************************************
; External Procedures (from PROCLIB.ASM)
; *******************************************************************************************************
;   - String & Console I/O:
;       - WRITE_STRING            : print a '$'-terminated string with newline
;       - READ_STRING             : read a line into a buffer and convert to '$'-string
;       - STORE_STRING            : transform DOS input buffer to '$'-terminated string
;       - SCAN_STRING             : get raw buffered input (DOS AH=0Ah)
;       - READ_CNP_FROM_KEYBOARD  : read/edit exactly 13 CNP characters, handle backspace/Q/ENTER options
;   - CNP Processing:
;       - VALIDATE_CNP            : checks the given string and calculates the validity of that CNP
;       - APPEND_RESULT_TO_STRING : embed 'V'/'N' result into CNP buffer replacing CR/LF
;   - File Buffering & Statistics:
;       - STORE_LINE_FILE         : write n given bytes plus '$' terminator to in-memory file buffer
;       - MALE_OR_FEMALE          : increment gender counters based on CNP gender digit
;       - AGE_COUNT               : calculate age from CNP + system date, increment age-group counters
;       - WRITE_NUM_TO_MEMORY     : convert numeric values to ASCII and append to file buffer
; *******************************************************************************************************
; Error Handling:
;   Invalid menu choice loops until ‘1’ or ‘2’ entered.
;   Batch mode handles file-not-found, no-permission, too-many-open,
;   invalid-access errors and retries filename input.
;
; Usage:
;   TASM /M2 MAIN.ASM
;   TASM /M2 PROCLIB.ASM
;   TLINK /v/3 MAIN.OBJ PROCLIB.OBJ
;   LINK, then run under DOS:
;   MAIN.EXE
; *******************************************************************************************************

INCLUDE MACLIB.ASM

DATA SEGMENT PARA PUBLIC 'DATA'
    BUFF                    DB 64, ? , 64 DUP (?)
    STRING1                 DB "Welcome to CNP validator.$"
    STRING2                 DB "There are two modes:$"
    STRING3                 DB "1.Interactive mode (write CNP, get result)$"
    STRING4                 DB "2.Batch processing mode (input file name with CNP's, get a different file with results)$"
    STRING5                 DB "What is your choice?(1/2)$"
    STRING_CHOICE           DB 64 DUP (?),'0','$'
    STRING7                 DB "Invalid choice, try again.$"
    STRING8                 DB "You picked : 1$"
    STRING_INSERT_VALUE     DB "Please insert the CNP code you wish to check.Or press 'Q' to quit.$"
    CNP                     DB "################$"
    VALIDATOR_CODE          DB 2,7,9,1,4,6,3,5,8,2,7,9
    VALIDATION_RESULT       DB 00H
    STRING_VALID            DB "Valid.$"
    STRING_INVALID          DB "Invalid.$"
    STRING_EXECUTION_DONE   DB "Execution finished.$"
    STRING9                 DB "You picked : 2$"
    FILE_NAME_REQUEST       DB "Please input the file name and extension.$"
    FILE_NOT_FOUND          DB "ERROR! File not found. Please retry.$"
    FILE_NOT_ACCESS         DB "ERROR! No permision to access file. Please retry.$"
    FILE_TOO_MANY_OPEN      DB "ERROR! Too many files are open.Please retry.$"
    FILE_INVALID_ACCESS     DB "ERROR! Invalid file access.Please retry.$"
    FILE_HANDLE             DW ?
    VALID_COUNT             DW 0H
    INVALID_COUNT           DW 0H
    MALE_COUNT              DW 0H
    FEMALE_COUNT            DW 0H
    COUNT_14TO17            DW 0H
    COUNT_18TO65            DW 0H
    COUNT_66TOELSE          DW 0H
    DISPLAY_NUMBER          DB '0','0','0','0','0',0DH,0AH,'$'
    APPEND_CHARACTER        DB 'N'
    FINAL_FILE_POINTER      DW 0H
    FINAL_FILE              DB 3000 DUP (?)
    STATISTICS1             DB "Here are the statistics:",0DH,0AH
    STATISTICS2             DB "Valid count:",0DH,0AH
    STATISTICS3             DB "Invalid count:",0DH,0AH
    STATISTICS4             DB "Male count:",0DH,0AH
    STATISTICS5             DB "Female count:", 0DH, 0AH
    STATISTICS6             DB "Under 18 count:",0DH, 0AH
    STATISTICS7             DB "In between 18 and 65:", 0DH, 0AH
    STATISTICS8             DB "Over 65 count",0DH, 0AH
    TODAY_YEAR              DW 2024
    TODAY_MONTH             DB 04
    TODAY_DAY               DB 29
    AGE_TEST                DW ?
DATA ENDS


CODE SEGMENT PARA PUBLIC 'CODE'
    ASSUME CS:CODE, DS:DATA
    EXTRN WRITE_STRING : NEAR
    EXTRN READ_STRING : NEAR
    EXTRN STORE_STRING : NEAR
    EXTRN SCAN_STRING : NEAR
    EXTRN READ_CNP_FROM_KEYBOARD : NEAR
    EXTRN VALIDATE_CNP : NEAR
    EXTRN APPEND_RESULT_TO_STRING : NEAR
    EXTRN STORE_LINE_FILE : NEAR
    EXTRN MALE_OR_FEMALE : NEAR
    EXTRN AGE_COUNT : NEAR
    EXTRN WRITE_NUM_TO_MEMORY : NEAR
    START PROC FAR
    PUSH DS
    XOR AX, AX
    MOV DS, AX
    PUSH AX
    MOV AX, DATA
    MOV DS, AX

    LEA SI, TODAY_YEAR                  
    LEA DI, TODAY_MONTH
    LEA BX, TODAY_DAY
    SET_TODAY_DATE SI, DI, BX           ;SET SYSTEM DATE - 

    PUSH OFFSET STRING1                 ;INTRODUCTORY LINES
    CALL WRITE_STRING
    PUSH OFFSET STRING2
    CALL WRITE_STRING
    PUSH OFFSET STRING3
    CALL WRITE_STRING
    PUSH OFFSET STRING4
    CALL WRITE_STRING

    NOT_OK_CHOICE:                      ;MODE SELECT 'LOOP' - WILL END WHEN A GOOD ONE IS CHOSEN
    PUSH OFFSET STRING5
    CALL WRITE_STRING
    PUSH OFFSET BUFF                    ;READ BUFFER FOR MSDOS
    PUSH OFFSET STRING_CHOICE           ;WHERE THE RESULT WILL BE STORED
    CALL READ_STRING            
    CMP BYTE PTR [STRING_CHOICE], '1'   ;USER HAS CHOSEN MODE 1
    JE OK_CHOICE_1
    CMP BYTE PTR [STRING_CHOICE], '2'   ;USER HAS CHOSEN MODE 2
    JNE NOK_CHOICE_2
    JMP OK_CHOICE_2                     ;FAR JUMPS CAN'T BE CONDITIONAL
    NOK_CHOICE_2:
    PUSH OFFSET STRING7                 ;INVALID INPUT FEEDBACK
    CALL WRITE_STRING
    JMP NOT_OK_CHOICE                   ;RESTART CHOICE PICK LOOP

    OK_CHOICE_1:                        ;MODE 1 PROCESSING
    PUSH OFFSET STRING8
    CALL WRITE_STRING
    
    OP_1:    
        XOR BX,BX
        PUSH OFFSET STRING_INSERT_VALUE ;COMMUNICATE THE FACT
        CALL WRITE_STRING               ;THAT INPUT CAN BE RECIEVED

        PUSH OFFSET CNP                 ;GET THAT INPUT
        CALL READ_CNP_FROM_KEYBOARD     ;STORE IT IN CNP
        
        CMP BL,'Q'                      ;IN CASE THE USER
        JNE GOTO_VALIDATE_CNP           ;WANTS TO QUIT
        JMP DONE                        ;DO IT AS FAST AS POSSIBLE
        
        GOTO_VALIDATE_CNP:   
        PUSH OFFSET VALIDATION_RESULT   ;WHERE TO STORE THE RESULT 
        PUSH OFFSET VALIDATOR_CODE      ;THE VALIDATION DIGITS USED FOR CALCULATION
        PUSH OFFSET CNP                 ;CHECK THE GIVEN CNP
        CALL VALIDATE_CNP               ;SEE IF IT'S LEGIT OR NOT

        CMP VALIDATION_RESULT, 0H       ;IF IT IS LEGIT
        JNE GOOD1                       ;GOTO GOOD RESULT

        PUSH OFFSET STRING_INVALID      ;IF IT IS NOT
        CALL WRITE_STRING               ;GIVE FEEDBACK
    JMP OP_1
        GOOD1:
        PUSH OFFSET STRING_VALID        ;WRITE GOOT RESULT
        CALL WRITE_STRING               ;GIVE FEEDBACK
    JMP OP_1
    JMP DONE

    OK_CHOICE_2:                        ;MODE 2 PROCESSING
        PUSH OFFSET STRING9
        CALL WRITE_STRING

        PUSH OFFSET FILE_NAME_REQUEST       ;ASK FOR FILE-NAME
        CALL WRITE_STRING

        PUSH OFFSET BUFF                    ;MSDOS BUFFER FOR STRINGS READ
        PUSH OFFSET STRING_CHOICE           ;WHERE WE WILL STORE THE RESULT STRING
        CALL READ_STRING                    ;GETTING THE FILE-NAME

                                            ;PROCESSING FILE-NAME READ:
        LEA DI,STRING_CHOICE                ;TRANSFORM INPUT FILENAME
        LEA SI,BUFF                         ;TO ASCIIZ FORMAT : TERMINATE READ FILENAME WITH A '0' BYTE
        INC SI                              ;GET ACTUAL LENGTH OF STRING_CHOICE
        XOR AX,AX                           ;CLEAR AX
        MOV AL, BYTE PTR [SI]               ;MOVE LENGTH TO AX
        ADD DI, AX                          ;GO TO THE LAST CHARACTER IN STRING
        MOV BYTE PTR [DI], 0                ;REPLACE '$' WITH ZERO BYTE

        LEA BX, STRING_CHOICE               ;STORE FILE NAME TO BX
        OPEN_FILE BX                        ;OPEN THE FILE ENTERED 

        JC ERROR_FOUND                      ;CARRY FLAG TELLS US
        JMP FILE_VALID_ACCESS               ;IF THERE WAS ANY ERROR
        ERROR_FOUND:                        ;IN THE OPENING OF THE FILE

                                            ;THIS SECTION CHECKS FOR FILE OPENING ERRORS
        CMP AX,02H                          ;CHECK IF THE FILE WAS FOUND
    JNE FILE_FOUND                          ;IF YES, PROCEEED
        PUSH OFFSET FILE_NOT_FOUND          ;IF NOT, GIVE FEEDBACK
        CALL WRITE_STRING                   ;AND RESTART FILE-NAME INPUT
    JMP OK_CHOICE_2
    FILE_FOUND:

        CMP AX,05H                          ;CHECK IF THE FILE CAN BE ACCESSED
        JNE FILE_ACCESS_ALLOWED             ;IF YES, PROCEED
        PUSH OFFSET FILE_NOT_ACCESS         ;IF NOT, GIVE FEEDBACK
        CALL WRITE_STRING                   ;AND RESTART FILE-NAME INPUT
        JMP OK_CHOICE_2
        FILE_ACCESS_ALLOWED:

        CMP AX,04H                          ;CHECK IF THERE WAS ENOUGH MEMORY FOR THE FILE
        JNE FILE_ENOUGH_MEMORY              ;IF YES, PROCEED
        PUSH OFFSET FILE_TOO_MANY_OPEN      ;IF NOT,
        CALL WRITE_STRING                   ;GIVE FEEDBACK
        JMP OK_CHOICE_2                     ;AND RESTART FILE-NAME INPUT
        FILE_ENOUGH_MEMORY:

        CMP AX,0CH                          ;CHECK IF THE FILE OPENING OCCURED PROPERLY
        JNE FILE_VALID_ACCESS               ;IF YES, PROCEED
        PUSH OFFSET FILE_INVALID_ACCESS     ;IF NOT
        CALL WRITE_STRING                   ;GIVE FEEDBACK
        JMP OK_CHOICE_2                     ;AND RESTART FILE-NAME INPUT
        FILE_VALID_ACCESS:
                                            ;FILE OPENING ERROR SECTION ENDS HERE
                                            ;FILE IS NOW ACCESABLE IN READ-WRITE MODE
        MOV FILE_HANDLE, AX                 ;STORE FILE HANDLE CONTENTS

        READ_FILE_LOOP:
            LEA SI, FILE_HANDLE
            LEA DI, CNP
            GET_CNP_FROM_FILE SI,DI         ;READ 15 BYTES FROM FILE INTO CNP

            PUSH AX                         ;PUSH PARAMETERS FOR THE EOF DETECTION    
            PUSH CX                         ;PUSH CX FOR THE EOF CHECK
                                            ;IF EQUAL, THEN PROCESS LAST CNP AND QUIT
            JNC NERROR_READ_FILE            ;CHECK FOR ERRORS
            JMP ERROR_READ_FILE             ;FAR JUMP TO ERRORS IF FOUND
            NERROR_READ_FILE:               ;NEAR JUMP TO PROCESSING IF NOT FOUND

                                            ;VALIDATION CHECK SECTION BEGINS
            PUSH OFFSET VALIDATION_RESULT   ;RESULT STORE
            PUSH OFFSET VALIDATOR_CODE      ;CHECK SUM VALUES LOAD-UP
            PUSH OFFSET CNP                 ;CHECK VALIDITY CNP
            CALL VALIDATE_CNP               ;CALL VALIDATION PROCEDURE
            CMP VALIDATION_RESULT, 0        ;COMPARE RESULT WITH 0
        JE FILE_CNP_INVALID                 ;IF INVALID(0) GO TO INVALID SECTION
        JMP FILE_CNP_VALID                  ;ELSE, FAR JUMP TO VALID SECTION
            
        FILE_CNP_INVALID:                   ;INVALID CNP SECTION STARTS HERE
                                            ;IF NOT VALID:
                                            ;1.APPEND N TO LINE
            XOR AX,AX
            MOV AL, 'N'                     ;CHARACTER TO BE APENDED IS 'N'
            PUSH OFFSET CNP                 ;WHERE THE CNP IS FOUND
            CALL APPEND_RESULT_TO_STRING    ;ADD 'N' TO THE END OF CURRENT CNP
                                            ;WRITE THE MODIFICATION IN MEMORY
            PUSH OFFSET FINAL_FILE_POINTER
            PUSH OFFSET FINAL_FILE
            PUSH 10H                        ;HOW MANY CHARACTERS ARE WRITTEN
            PUSH OFFSET CNP                 ;WHERE TO TAKE THE CHARACTERS FROM
            CALL STORE_LINE_FILE            ;WRITE THEM TO MEMORY

            INC INVALID_COUNT               ;2.INCREASE INVALID COUNT
            
            POP CX                          ;POP AX PARAMETER FOR THE EOF DETECTION
            POP AX                          ;POP CX PARAMETER FOR THE EOF DETECTION
            CMP AX,CX                       ;CHECK FOR CHARACTERS READ COUNT
            JE NEOF1                        ;IF NOT 0, THEN EOF HAS NOT BEEN REACHED
            JMP EOF                         ;IF 0, FAR JUMP TO EOF PROCESSING
            NEOF1:                          ;(EOF = END OF FILE)    

        JMP READ_FILE_LOOP                  ;INVALID CNP SECTION ENDS HERE

        FILE_CNP_VALID:                     ;VALID CNP SECTION STARTS HERE
                                            ;IF VALID:

                                            ;1.APPEND V TO LINE
            XOR AX,AX
            MOV AL, 'V'                     ;CHARACTER TO BE APENDED IS 'V'
            PUSH OFFSET CNP                 ;WHERE THE CNP IS FOUND
            CALL APPEND_RESULT_TO_STRING    ;ADD 'V' TO THE END OF CURRENT CNP
                                            ;NOW WRITE THE MODIFICATION IN MEMORY
            PUSH OFFSET FINAL_FILE_POINTER  ;TELL THE PROCESS THE CHARACTER WRITE POSITION
            PUSH OFFSET FINAL_FILE          ;TELL WRITE PROCESS WHERE TO WRITE IT IN MEMORY
            PUSH 10H                        ;CHARACTER COUNT TO BE WRITTEN
            PUSH OFFSET CNP                 ;CHARACTER SOURCE
            CALL STORE_LINE_FILE            ;WRITE IT TO MEMORY

                                            ;2.INCREASE MALE/FEMALE COUNT
            PUSH OFFSET MALE_COUNT
            PUSH OFFSET FEMALE_COUNT
            PUSH OFFSET CNP                 ;TARGET CNP
            CALL MALE_OR_FEMALE             ;MALE/FEMALE COUNT INCREASE

                                            ;3.INCREASE [14,17]/[18,65]/[66,???] COUNT
            PUSH OFFSET COUNT_14TO17        ;LOAD COUNTER POSITION 1
            PUSH OFFSET COUNT_18TO65        ;LOAD COUNTER POSITION 2
            PUSH OFFSET COUNT_66TOELSE      ;LOAD COUNTER POSITION 3
            PUSH OFFSET CNP                 ;TARGET CNP LOAD
            CALL AGE_COUNT                  ;AGE COUNT INCREASE

            INC VALID_COUNT                 ;4.INCREASE VALID COUNT
            
            POP CX                          ;POP AX PARAMETER FOR THE EOF DETECTION
            POP AX                          ;POP CX PARAMETER FOR THE EOF DETECTION  
            CMP AX,CX                       ;CHECK FOR CHARACTERS READ COUNT
            JE NEOF2                         ;IF NOT 0, THEN EOF HAS NOT BEEN REACHED
            JMP EOF                         ;IF 0, FAR JUMP TO EOF PROCESSING
            NEOF2:                           ;(EOF = END OF FILE)    

        JMP READ_FILE_LOOP                  ;VALID CNP SECTION ENDS HERE

            ERROR_READ_FILE:                ;ERROR CORRECTING SECTION STARTS HERE
            PUSH OFFSET FILE_INVALID_ACCESS ;JUST RESTART THE WHOLE PROCESS
            CALL WRITE_STRING               ;GIVE FEEDBACK
            JMP OK_CHOICE_2                 ;LONG JUMP TO RESTART FILE OPEN PROCESS
            
            JMP READ_FILE_LOOP              ;ERROR CORRECTING SECTION ENDS HERE
        EOF:
                                            ;0.APPEND 'HERE ARE THE STATISTICS' TO FILE IN MEMORY
        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH 26D                            ;HERE ARE THE STATISTICS
        PUSH OFFSET STATISTICS1             ;IS WRITTEN TO NEW FILE
        CALL STORE_LINE_FILE                ;IN MEMORY
                                            ;1.APPEND VALID/INVALID COUNT TO FILE IN MEMORY
        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH 14D
        PUSH OFFSET STATISTICS2
        CALL STORE_LINE_FILE
                                            ;WRITE VALID COUNT VALUE
        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH OFFSET DISPLAY_NUMBER
        PUSH OFFSET VALID_COUNT
        CALL WRITE_NUM_TO_MEMORY

        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH 16D
        PUSH OFFSET STATISTICS3
        CALL STORE_LINE_FILE
                                            ;WRITE INVALID VALUE
        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH OFFSET DISPLAY_NUMBER
        PUSH OFFSET INVALID_COUNT
        CALL WRITE_NUM_TO_MEMORY

                                            ;2.APPEND MALE/FEMALE COUNT TO FILE IN MEMORY
        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH 13D
        PUSH OFFSET STATISTICS4
        CALL STORE_LINE_FILE
                                            ;WRITE MALE VALUE
        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH OFFSET DISPLAY_NUMBER
        PUSH OFFSET MALE_COUNT
        CALL WRITE_NUM_TO_MEMORY

        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH 15D
        PUSH OFFSET STATISTICS5
        CALL STORE_LINE_FILE
                                            ;WRITE FEMALE VALUE
        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH OFFSET DISPLAY_NUMBER
        PUSH OFFSET FEMALE_COUNT
        CALL WRITE_NUM_TO_MEMORY

                                            ;3.APPEND AGE COUNT TO FILE IN MEMORY
        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH 17D
        PUSH OFFSET STATISTICS6
        CALL STORE_LINE_FILE
                                            ;WRITE UNDER18 VALUE
        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH OFFSET DISPLAY_NUMBER
        PUSH OFFSET COUNT_14TO17
        CALL WRITE_NUM_TO_MEMORY

        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH 23D
        PUSH OFFSET STATISTICS7
        CALL STORE_LINE_FILE
                                            ;WRITE OVER 18 VALUE
        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH OFFSET DISPLAY_NUMBER
        PUSH OFFSET COUNT_18TO65
        CALL WRITE_NUM_TO_MEMORY

        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH 15D
        PUSH OFFSET STATISTICS8
        CALL STORE_LINE_FILE
                                            ;WRITE OVER 66 VALUE
        PUSH OFFSET FINAL_FILE_POINTER
        PUSH OFFSET FINAL_FILE
        PUSH OFFSET DISPLAY_NUMBER
        PUSH OFFSET COUNT_66TOELSE
        CALL WRITE_NUM_TO_MEMORY

                                            ;4.REPLACE TARGET FILE, WITH FILE FROM MEMORY
        LEA SI, FILE_HANDLE
        FINISH_READING_FILE SI              ;CLOSE FILE
        LEA SI, STRING_CHOICE
        LEA DI, FILE_HANDLE
        RECREATE_FILE_ON_TOP_OF_ORIGINAL SI,DI ;RECREATE FILE
        LEA SI, FILE_HANDLE
        LEA DI, FINAL_FILE_POINTER
        LEA BP, FINAL_FILE
        STORE_FROM_MEMORY_TO_FILE SI,DI,BP ;WRITE THE ENTIRE BUFFER FROM MEMORY TO IT

        MOV DX, FILE_HANDLE             ;SETUP
        CLOSE_FILE DX                   ;CALL CLOSE FILE MACRO 
    DONE:
    NEW_LINE
    PUSH OFFSET STRING_EXECUTION_DONE   ;'EXECUTION ENDED' FEEDBACK
    CALL WRITE_STRING
    RET
START ENDP
CODE ENDS
END START