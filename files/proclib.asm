; *******************************************************************************************************
; File:        PROCLIB.ASM
; Author:      Sasarman Darius-Eric
; Date:        2025-04-28
; Description:
;   PROCLIB.ASM provides reusable procedures, for console I/O, string handling,
;   CNP validation, file buffering, and statistical counters in this MS-DOS assembly project. 
;
; Implemented procedures:
;   - String & Console I/O:
;       - WRITE_STRING            : print a '$'-terminated string with newline
;       - READ_STRING             : read a line into a buffer and convert to '$'-string
;       - STORE_STRING            : transform DOS input buffer to '$'-terminated string
;       - SCAN_STRING             : get raw buffered input 
;       - READ_CNP_FROM_KEYBOARD  : read/edit exactly 13 CNP characters, handle backspace/Q/ENTER options
;   - CNP Processing:
;       - VALIDATE_CNP            : checks the given string and calculates the validity of that CNP
;       - APPEND_RESULT_TO_STRING : embed 'V'/'N' result into CNP buffer replacing CR/LF
;   - File Buffering & Statistics:
;       - STORE_LINE_FILE         : write n given bytes to in-memory file buffer
;       - MALE_OR_FEMALE          : increment gender counters based on CNP gender digit
;       - AGE_COUNT               : calculate age from CNP + system date, increment age-group counters
;       - WRITE_NUM_TO_MEMORY     : convert numeric values to ASCII and append to file buffer
;
; Usage:
;   ; Call any PUBLIC routine as documented below.
; *******************************************************************************************************

INCLUDE maclib.asm

CODE SEGMENT PARA PUBLIC 'CODE'
    PUBLIC WRITE_STRING            
    PUBLIC READ_STRING              
    PUBLIC STORE_STRING
    PUBLIC SCAN_STRING
    PUBLIC READ_CNP_FROM_KEYBOARD  
    PUBLIC VALIDATE_CNP
    PUBLIC APPEND_RESULT_TO_STRING
    PUBLIC STORE_LINE_FILE
    PUBLIC MALE_OR_FEMALE
    PUBLIC AGE_COUNT
    PUBLIC WRITE_NUM_TO_MEMORY
ASSUME CS:CODE


;PROCEDURE CALL TEMPLATE:
;PUSH OFFSET FINAL_FILE_POINTER     +8
;PUSH OFFSET FINAL_FILE             +6
;PUSH OFFSET DISPLAY_NUMBER         +4
;PUSH OFFSET VALUE                  +2
;CALL WRITE_NUM_TO_MEMORY
;PROCEDURE USAGE:
;FOR WRITING ASCII VALUES, IN MEMORY, OF THE GIVEN NUMBER
WRITE_NUM_TO_MEMORY PROC NEAR
    MOV BP,SP
    MOV SI,[BP+2]
    XOR BX,BX                   ;CLEAR BX
    MOV DI, [BP+4]              ;LOAD DISPLAY_NUMBER LOCATION
    MOV BYTE PTR [DI], '0'      ;CLEAR DISPLAY_NUMBER DIGIT 0
    INC DI                      ;GO NEXT
    MOV BYTE PTR [DI], '0'      ;CLEAR DISPLAY_NUMBER DIGIT 1
    INC DI                      ;GO NEXT
    MOV BYTE PTR [DI], '0'      ;CLEAR DISPLAY_NUMBER DIGIT 2
    INC DI                      ;GO NEXT
    MOV BYTE PTR [DI], '0'      ;CLEAR DISPLAY_NUMBER DIGIT 3
    INC DI                      ;GO NEXT
    MOV BYTE PTR [DI], '0'      ;CLEAR DISPLAY_NUMBER DIGIT 4

    MOV AX,[SI]                 ;GET VALUE TO BE WRITTEN
    MOV BL, 10                  ;EXECUTE DIGIT EXTRACTION WITH DIVISION BY 10
    WRITE_DIGIT:                ;WRITE EACH DIGIT IN 'DISPLAY_NUMBER'
    XOR DX,DX                   ;CLEAR THE DIVIDED HIGH SO IT DOESN'T LOOP
    DIV BX                      ;DIV BY 10
    ADD BYTE PTR [DI], DL       ;REMAINDER IS IN DX
    DEC DI                      ;GO NEXT DIGIT
    CMP AX,0                    ;IF THERE'S ANYTHING LEFT
    JA WRITE_DIGIT              ;CONTINUE THE LOOP

    PUSH [BP+8]                 ;PUSH FINAL_FILE CHARACTER COUNT
    PUSH [BP+6]                 ;PUSH FINAL_FILE POINTER
    PUSH 7                      ;WRITE THE 7 CHARACTERS
    PUSH [BP+4]                 ;OF DISPLAY NUMBER
    CALL STORE_LINE_FILE        ;TO MEMORY
    RET 8
WRITE_NUM_TO_MEMORY ENDP

;PROCEDURE CALL TEMPLATE
;PUSH OFFSET COUNT_14TO17       +8
;PUSH OFFSET COUNT_18TO65       +6
;PUSH OFFSET COUNT_66TOELSE     +4
;PUSH OFFSET CNP                +2
;CALL AGE_COUNT
;PURPOSE OF THIS PROCEDURE:
;TO RAISE THE AGE GROUP COUNTS
AGE_COUNT PROC NEAR
    MOV BP,SP
    MOV SI, [BP+2]              ;LOAD CNP OFFSET
    GET_TODAY_DATE              ;LOAD TODAY'S DATE
                                ;CX = TODAY YEAR
                                ;DH = TODAY MONTH
                                ;DL = TODAY DAY
    ;ACTUAL AGE = TODAY YEAR - YEAR BORN - 1(IF BIRTHDAY DID NOT PASS)
    ;YEAR BORN = PREFIX + YEAR_CNP
    ;ACTUAL AGE = TODAY YEAR - PREFIX - YEAR_CNP - 1(IF BIRTHDAY DID NOT PASS)
    MOV AL, BYTE PTR [SI]       ;FIRST CNP GIVES GENDER AND YEAR PREFIX
    CMP AL, '3'                 ;3 => PERSON FROM 1800S
    JNE GETPY1                  ;IF IT'S NOT THE CASE, GO NEXT COMPARISON
    SUB CX, 1800                ;PREFIX SUBTRACTION
    JMP GETY0                   ;SKIP OTHER COMPARISONS
    GETPY1:                     ;
    CMP AL, '4'                 ;4 => PERSON FROM 1800S
    JNE GETPY2                  ;IF IT'S NOT THE CASE, GO NEXT COMPARISON
    SUB CX, 1800                ;PREFIX SUBTRACTION
    JMP GETY0                   ;SKIP OTHER COMPARISONS
    GETPY2:                     ;
    CMP AL, '1'                 ;1 => PERSON FROM 1900S
    JNE GETPY3                  ;IF IT'S NOT THE CASE, GO NEXT COMPARISON
    SUB CX, 1900                ;PREFIX SUBTRACTION
    JMP GETY0                   ;SKIP OTHER COMPARISONS
    GETPY3:                     ;
    CMP AL, '2'                 ;2 => PERSON FROM 1900S
    JNE GETPY4                  ;IF IT'S NOT THE CASE, GO NEXT COMPARISON
    SUB CX, 1900                ;PREFIX SUBTRACTION
    JMP GETY0                   ;SKIP OTHER COMPARISONS
    GETPY4:                     ;
    CMP AL, '5'                 ;5 => PERSON FROM 2000S
    JNE GETPY5                  ;IF IT'S NOT THE CASE, GO NEXT COMPARISON
    SUB CX, 2000                ;PREFIX SUBTRACTION
    JMP GETY0                   ;SKIP OTHER COMPARISONS
    GETPY5:                     ;
    CMP AL, '6'                 ;6 => PERSON FROM 2000S
    JNE GETPY6                  ; IF IT'S NOT THE CASE, GO NEXT COMPARISON
    SUB CX, 2000                ;PREFIX SUBTRACTION
    JMP GETY0                   ;SKIP OTHER COMPARISONS
    GETPY6:
    SUB CX, 2000                ;7,8,9,0 -> FOREIGNER
    GETY0:                      ;SUPPOSE 2000S FOR NOW
    XOR AX,AX                   ;PROCEED TO CALCULATE YEAR_CNP
    INC SI                      ;GO NEXT DIGIT
    MOV AH, BYTE PTR [SI]       ;GET FIRST 'Y' CHAR
    SUB AH, '0'                 ;GET FIRST 'Y' VAL
    SHL AH, 1                   ;AH = Y*2
    ADD AL, AH                  ;AL = Y*2
    SHL AH, 2                   ;AH = Y*8
    ADD AL, AH                  ;AL = Y*2 + Y*8 = Y*10
    INC SI                      ;GO NEXT DIGIT
    MOV AH, BYTE PTR [SI]       ;GET SECOND 'Y' CHAR
    SUB AH, '0'                 ;GET SECOND 'Y' VAL
    ADD AL, AH                  ;AL = 10*Y1 + Y2
                                ;AL HAS YEAR CALCULATED YEAR CNP
                                ;CX HAS AGE  
                                ;NOW WE NEED TO CHECK IF BIRTHDAY PASSED OR NOT
    XOR AX,AX                   ;MONTH CALCULATION
    INC SI                      ;GO NEXT DIGIT
    MOV AH, BYTE PTR [SI]       ;GET FIRST 'M' CHAR
    SUB AH, '0'                 ;GET FIRST 'M' VAL
    SHL AH, 1                   ;AH = M1*2
    ADD AL, AH                  ;AL = M1*2
    SHL AH, 2                   ;AH = M1*8
    ADD AL, AH                  ;AL = M1*2 + M1*8 = M1*10
    INC SI                      ;GO NEXT DIGIT
    MOV AH, BYTE PTR [SI]       ;GET SECOND 'M' CHAR
    SUB AH, '0'                 ;GET SECOND 'M' VAL
    ADD AL, AH                  ;AL = 10*M1 + M2
    CMP AL, DH                  ;TARGET MONTH <> TODAY MONTH
    JBE NO_DEC_MONTH            ;IF IT'S SMALLER/EQUAL DON'T DECREMENT
    JMP DEC_AGE                 ;IF IT IS NOT, DECREMENT
    NO_DEC_MONTH:
    JE  CHECK_DAY               ;IF MONTHS ARE EQUAL, CHECK THE DAY
    JMP GET_RESULT_AGE          ;IF NOT, THEN BIRTHDAY PASED, SKIP TO RESULT
    CHECK_DAY:
    XOR AX,AX                   ;DAY CALCULATION
    INC SI                      ;GO NEXT DIGIT
    MOV AH, BYTE PTR [SI]       ;GET FIRST 'D' CHAR
    SUB AH, '0'                 ;GET FIRST 'D' VAL
    SHL AH, 1                   ;AH = D1*2
    ADD AL, AH                  ;AL = D1*2
    SHL AH, 2                   ;AH = D1*8
    ADD AL, AH                  ;AL = D1*2 + D1*8 = D1*10
    INC SI                      ;GO NEXT DIGIT
    MOV AH, BYTE PTR [SI]       ;GET SECOND 'D' CHAR
    SUB AH, '0'                 ;GET SECOND 'D' VAL
    ADD AL, AH                  ;AL = 10*D1 + D2
    CMP AL, DL                  ;TARGET DAY <> TODAY DAY 
    JBE NO_DEC_DAY              ;TARGET DAY < OR = TODAY DAY  => BIRTHDAY PASSED
    JMP DEC_AGE                 ;TARGET DAY > TODAY => BIRTHDAY DID NOT PASS
    NO_DEC_DAY:                 ;THIS IS SO THAT FAR JUMP IS ENABLED
    JMP GET_RESULT_AGE          ;IF IT PASSED, JUMP TO RESULT

    DEC_AGE:                    ;THIS IS FOR WHEN THE BIRTHDAY
    DEC CX                      ;DID NOT PASS YET

    GET_RESULT_AGE:
    CMP CX,18                   ;IF AGE IS LESS THAN 18
    JL  UNDER_18
    CMP CX,66                   ;ELSIF AGE IS LESS THAN 66
    JL UNDER_66
    JMP OVER_66                 ;ELSE IT'S JUST OLDER 

    UNDER_18:                   ;CONCLUSIONS
    MOV SI, [BP+8]              ;LOAD UNDER 18 COUNTER LOCATION
    INC WORD PTR [SI]           ;TARGET HAS UNDER 18 YEARS
    JMP END_AGE
    
    UNDER_66:
    MOV SI, [BP+6]              ;LOAD UNDER 66 COUNTER LOCATION
    INC WORD PTR [SI]           ;TARGET HAS IN BETWEEN 18 AND 65 YEARS
    JMP END_AGE
    
    OVER_66:
    MOV SI, [BP+4]
    INC WORD PTR [SI]           ;TARGET HAS MORE THAN 65 YEARS

    END_AGE:
    RET 8                       ;CONSUME PARAMETER PUSH
AGE_COUNT ENDP

;PROCEDURE CALL TEMPLATE
;PUSH OFFSET MALE_COUNT
;PUSH OFFSET FEMALE_COUNT
;PUSH OFFSET CNP
;CALL MALE_OR_FEMALE
;PURPOSE OF THIS PROCEDURE:
;TO RAISE THE MALE/FEMALE COUNT FOR VALID CNP
MALE_OR_FEMALE PROC NEAR
    MOV BP,SP
    MOV SI,[BP+2]               ;LOAD CNP OFFSET
    XOR AX,AX                   ;CLEAR AX
    MOV AL,BYTE PTR [SI]        ;LOAD FIRST DIGIT CHARACTER IN AL
    SUB AL,'0'                  ;GET ACTUAL DIGIT VALUE
    AND AX,01H                  ;BIT-MASK : KEEP LAST BIT ONLY
    CMP AX,0H                   ;IF EVEN(LAST BIT 0)=> FEMALE
    JNE MALE_INCREASE           ;IF ODD (LAST BIT 1)=> MALE
    MOV SI, [BP+4]              ;LOAD FEMALE COUNT POSITION
    INC WORD PTR [SI]           ;INCREASE FEMALE_COUNT
    JMP MALE_OR_FEMALE_END      ;TARGET IS FEMALE,INCREASE COUNT
    MALE_INCREASE:
    MOV SI, [BP+6]              ;LOAD MALE_COUNT POSITION
    INC WORD PTR [SI]           ;TARGET IS MALE, INCREASE COUNT
    MALE_OR_FEMALE_END:
    RET 6                       ;CLEAR LOADED CNP OFFSET
MALE_OR_FEMALE ENDP

;PROCEDURE CALL TEMPLATE
;PUSH OFFSET FINAL_FILE_POINTER +8
;PUSH OFFSET FINAL_FILE         +6
;PUSH CHARACTER_COUNT           +4
;PUSH OFFSET STRING             +2
;CALL STORE_LINE_FILE
;PURPOSE OF THIS PROCEDURE:
;TO BUILD THE FINAL FILE IN MEMORY
;BY WRITING THE SPECIFIED AMOUNT
;ALWAYS ENDS THE FILE WITH '$'
STORE_LINE_FILE PROC NEAR
    MOV BP,SP
    MOV SI,[BP+2]               ;LOAD STRING OFFSET
    MOV CX,[BP+4]               ;LOAD CHARACTER COUNT
    LOOP_STORE_LINE_FILE:
        XOR BX,BX
        MOV BL, BYTE PTR [SI]   ;LOAD CHARACTER TO BE WRITTEN
        MOV AX, [BP+6]          ;LOAD FILE POSITION
        MOV DI, [BP+8]
        ADD AX, WORD PTR [DI]   ;LOAD CURRENT POSITION
        MOV DI,AX
        MOV BYTE PTR [DI], BL   ;REWRITE FILE BYTE
        INC SI                  ;GOTO NEXT CHARACTER TO BE WRITTEN
        MOV DI, [BP+8]
        INC WORD PTR [DI]       ;GO TO NEXT POSITION TO INSERT CHARACTER
    LOOP LOOP_STORE_LINE_FILE
    MOV AX, [BP+6]              ;GO TO FILE
    MOV DI, [BP+8]
    ADD AX, WORD PTR [DI]       ;GO TO END OF IT
    MOV DI,AX
    MOV BYTE PTR [DI], '$'      ;THIS WILL BE USED AS END OF FILE CHARACTER
    RET 8
STORE_LINE_FILE ENDP


;PROCEDURE CALL TEMPLATE 
;XOR AX,AX
;MOV AL, 'X'                ;N FOR INVALID / V FOR VALID
;PUSH OFFSET CNP
;CALL APPEND_RESULT_TO_STRING
;PURPOSE OF THIS PROCEDURE:
;TO TAKE A CNP STRING FROM A FILE AND TO APPEND 
;TO IT THE VALIDATION RESULT CHARACTER
;EXECUTES OPERATION DIRECTLY ON MEMORY
APPEND_RESULT_TO_STRING PROC NEAR
    MOV BP,SP
    MOV SI,[BP+2]               ;GET CNP STRING
    ADD SI, 13                  ;JUMP OVER THE 13 DIGITS
    MOV BYTE PTR [SI], AL       ;APPEND RESULT TO END
    INC SI                      ;MOVE TO NEXT POSITION
    MOV BYTE PTR [SI], 0DH      ;APPEND CR BACK
    INC SI                      ;NEXT POSITION
    MOV BYTE PTR [SI], 0AH      ;APPEND LF BACK
    RET 2                       ;STRING HAS BEEN MODIFIED, END PROCESS
APPEND_RESULT_TO_STRING ENDP

;PROCEDURE CALL TEMPLATE
;PUSH OFFSET VALIDATION_RESULT  +6
;PUSH OFFSET VALIDATOR_CODE     +4
;PUSH OFFSET CNP                +2
;CALL VALIDATE_CNP
;PURPOSE OF THIS PROCEDURE:
;TO TAKE A CNP STRING AND CHECK IF
;IT IS VALID OR NOT
;STORES RESULT IN VALIDATION_RESULT VARIABLE
;1 FOR VALID
;0 FOR INVALID
VALIDATE_CNP PROC NEAR
    MOV BP,SP                   ; LOAD STACK POINTER
    MOV SI,[BP+2]               ; LOAD CNP-TO BE TESTED
    MOV DI,[BP+4]               ; LOAD VALIDATION VALUES
    MOV BX,[BP+6]               ; LOAD VALIDATION RESULT
    MOV BYTE PTR [BX], 01H      ; SUPPOSE IT IS TRUE AT FIRST
    XOR BX,BX                   ; USE BX TO STORE THE SUM
    MOV CX,12                   ; CALCULATE SUM FOR FIRST 12 DIGITS
    SUM_VALIDATE:
        XOR DX,DX
        XOR AX,AX
        MOV AL, BYTE PTR [SI]   ;DL<-'DIGIT'
        
        CMP AL, '0'             ;IF 'DIGIT' < '0' 
        JGE OK_DIGIT1           ;=> INVALID CHARACTER =>
        JMP CNP_NOT_VALID       ;=> CNP NOT VALID
        OK_DIGIT1:
        
        CMP AL, '9'             ;IF 'DIGIT' > '9'
        JLE OK_DIGIT2           ;=> INVALID CHARACTER =>
        JMP CNP_NOT_VALID       ;=> CNP NOT VALID
        OK_DIGIT2:
        
        SUB AL, '0'             ;DL<-CURRENT CNP DIGIT NUMERICAL VALUE
        MOV AH, BYTE PTR [DI]   ;STORE CURRENT VALIDATOR CONSTANT DIGIT
        MUL AH                  ;AX <- CNP DIGIT * VALIDATOR DIGIT
        ADD BX, AX              ;ADD IT TO THE SUM
        
        INC SI                  ;MOVE TO NEXT CNP DIGIT
        INC DI                  ;MOVE TO NEXT VALIDATOR DIGIT
    LOOP SUM_VALIDATE
    MOV AX,BX                   ;PREPARE SUM FOR DIVISION
    XOR BX,BX                   ;BX <- 0000H
    MOV BL,11D                  ;BX <- 000BH
    DIV BL                      ;EXECUTE DIVISION, STORE REMAINDER IN AH
    MOV BH, BYTE PTR [SI]       ;GET LAST DIGIT CHARACTER
    SUB BH, '0'                 ;GET LAST DIGIT VALUE
    CMP AH, 10                  ;IF AH == 10
    JNE NOT_TEN
        CMP BH,1D               ;   IF LAST_DIGIT =/= 1
        JNE CNP_NOT_VALID       ;       NOT_VALID
        JMP FINISH_VALIDATION   ;   ELSE VALID
    NOT_TEN:                    ;ELSE
        CMP AH,BH               ;   IF LAST_DIGIT =/= REMAINDER
        JNE CNP_NOT_VALID       ;       NOT_VALID
                                ;   ELSE VALID
    JMP FINISH_VALIDATION
    CNP_NOT_VALID:
    MOV BX, [BP+6]              ;LOAD VALIDATION RESULT ADDRESS
    MOV BYTE PTR [BX],00H       ;RESULT <- 0; NOT VALID CNP
    FINISH_VALIDATION:
    RET 6
VALIDATE_CNP ENDP


;PROCEDURE CALL TEMPLATE
;PUSH OFFSET CNP
;CALL READ_CNP
;PURPOSE OF THIS PROCEDURE:
;TO READ FROM KEYBOARD, CHARACTER BY CHARACTER, A CNP CODE
;ALLOWS FOR EARLY 'ENTER' FINISH
;ALLOWS FOR BACKSPACE CORRECTION
;ALLOWS FOR 'Q'/'q' press QUITTING 
;APPENDS '$' AT THE END FOR STRING WRITING
;STORES INPUT AT TARGET LOCATION
READ_CNP_FROM_KEYBOARD PROC NEAR
    MOV BP,SP
    MOV SI,[BP+2]               ;LOAD STORE LOCATION
    MOV CX,13                   ;START READING 13 CHARACTERS
    READ_CNP_LOOP:
        SCAN_CHAR SI            ;READ CHAR FROM KEYBOARD

        CMP AL, 'Q'             ;QUIT CASE 1
        JNE CONTINUE_CNP_1
        MOV BL, 'Q'
        RET 2
        CONTINUE_CNP_1:

        CMP AL, 'q'             ;QUIT CASE 2
        JNE CONTINUE_CNP_2
        MOV BL, 'Q'
        RET 2
        CONTINUE_CNP_2:
        
        CMP AL, 13D             ;INPUT IS DONE EARLY
        JNE NOT_ENTER           
        MOV CX,1                ;TERMINATE LOOP IF SO
        NOT_ENTER:
        
        CMP AL,08H              ;USER PRESSED BACK SPACE
        JE BACKSPACE            ;GO TO CASE HANDLER

        MOV BYTE PTR [SI], AL   ;PLACE IN MEMORY READ CHARACTER
        INC SI                  ;GO NEXT POINT IN MEMORY
        JMP NO_BACKSPACE_CHANGE2;NO SPECIAL CASE DETECTED

        BACKSPACE:              ;FIRST WE NEED TO COVER COUNTING
        CMP CX,13               ;IF IT IS AT THE BEGINING
        JE NO_BACKSPACE_CHANGE1 ;DON'T INCREASE IT MORE
        INC CX                  ;BACKSPACE CHARACTER COUNT COVER
        INC CX                  ;REPLACEMENT CHARACTER COUNT COVER
        NO_BACKSPACE_CHANGE1:   ;THEN WE NEED TO COVER MEMORY
        CMP SI, [BP+2]          ;IF WE'RE AT THE BEGINING
        JE NO_BACKSPACE_CHANGE2 ;DON'T DECREASE FURTHER
        DEC SI                  ;REPLACE DELETED CHARACTER
        NO_BACKSPACE_CHANGE2:   ;WHEN AN OTHER ONE IS READ FROM KEYBOARD(IN MEMORY)
    LOOP READ_CNP_LOOP
    MOV BYTE PTR [SI], '$'      ;APPEND '$' AT THE END
    NEW_LINE
    RET 2
READ_CNP_FROM_KEYBOARD ENDP


;PROCEDURE CALL TEMPLATE:
;PUSH OFFSET STRING  - POINTER TO TARGET STRING
;CALL WRITE_STRING   - ACTUAL PROCEDURE CALL
;PURPOSE OF THIS PROCEDURE:
;TO WRITE A STRING FROM MEMORY, ONTO THE SCREEN
WRITE_STRING PROC NEAR
    MOV BP,SP
    MOV SI,[BP+2]               ;LOAD POINTER TO STRING
    XOR CX,CX                   ;PREPARE PSEUDO-INFINITE LOOP- STOPS WHEN '$' IS MET
    LOOP1:
        MOV BL, BYTE PTR [SI]   ;SAVE CHARACTER INTO BX REGISTER - ALLOWS WRITING
        WRITE_CHARACTER BL      ;WRITE THE CURRENT CHARACTER
        INC SI                  ;GO TO NEXT CHARACTER
        CMP BYTE PTR [SI],'$'   ;IF IT'S ENDLINE - STOP THE LOOP
        JE STOP_LOOP
    LOOP LOOP1
    STOP_LOOP:
    NEW_LINE                    ;WRITE A NEW LINE
    RET 2                       ;ELIMINATE PASSED POINTER TO STRING FROM STACK
WRITE_STRING ENDP

;PROCEDURE CALL TEMPLATE
;PUSH OFFSET BUFFER
;CALL SCAN_STRING
;PURPOSE OF THIS PROCEDURE:
;USED FOR READING USER INPUT AND STORING IT INTO A BUFFER
SCAN_STRING PROC NEAR           
    MOV BP,SP                   ;STACK POINTER SAFE COPY
    MOV SI, [BP+2]
    SCAN_STRING_FROM_KEYBOARD SI;MACRO USED FOR LOADING THE INPUT INTO THE BUFFER
    RET 2                       ;ELIMINATE PASSED BUFFER FROM STRING
SCAN_STRING ENDP

;PROCEDURE CALL TEMPLATE:
;PUSH OFFSET BUFFER
;PUSH OFFSET STRING
;CALL STORE_STRING
;PURPOSE OF THIS PROCEDURE:
;TO TAKE A BUFFER-STORED STRING
;AND SAVE IT TO MEMORY AS A '$'-ENDED STRING
STORE_STRING PROC NEAR          ;TAKES IN A BUFFER THAT CONTAINS INFO ABOUT A STRING, AND STORES IT IN A STRING
    MOV BP,SP
    MOV SI, [BP+2]              ;BUFFER LOAD
    INC SI                      ;GO FROM 'MAX SIZE' COUNT TO 'ACTUAL SIZE'
    XOR CX,CX                   ;CLEAR CX
    MOV CL, BYTE PTR [SI]       ;LOAD 'ACTUAL SIZE' TO CX REGISTER
    INC SI                      ;MOVE ON TO STORED CHARACTERS
    MOV DI, [BP+4]              ;STRING LOCATION LOAD
    LOOP2:                      ;COPY FROM BUFFER TO STRING, UNTIL READING IS DONE
        MOV AL, BYTE PTR [SI]   ;TAKE CHARACTER FROM BUFFER
        MOV BYTE PTR [DI], AL   ;STORE IT TO STRING
        INC DI                  ;GO NEXT STORE LOCATION
        INC SI                  ;GO NEXT BUFFER CHARACTER
    LOOP LOOP2
    MOV BYTE PTR [DI] , '$'     ;ADD END OF LINE CHARACTER AT THE END OF THE STRING
    RET 4
STORE_STRING ENDP

;PROCEDURE CALL TEMPLATE:
;PUSH OFFSET BUFF
;PUSH OFFSET STRING
;CALL READ_STRING
;PURPOSE OF THIS PROCEDURE:
;TO READ A STRING REPLY FROM THE USER, AND STORE IT IN A PRINTABLE MANNER
;FIRST IT OBTAINS THE INPUT STORED IN A BUFFER
;THEN IT PROCESSES IT INTO A '$' ENDED STRING
READ_STRING PROC NEAR           ;PROCEDURE USED FOR READING A STRING FROM THE KEYBOARD
    MOV BP,SP
    MOV SI, [BP+4]              ;BUFFER LOAD
    MOV DI, [BP+2]              ;STRING LOAD
    PUSH SI                     ;GETTING READY TO READ FROM KEYBOARD
    CALL SCAN_STRING            ;SCAN_STRING CALL
    PUSH DI                     ;INFO IS STORED IN BUFFER
    PUSH SI                     ;NOW WE NEED TO TRANSLATE IT INTO PLAIN STRING
    CALL STORE_STRING
    NEW_LINE                    ;NEW LINE AFTER READ STRING SO THE CHARACTERS AREN'T DELETED
    RET 4                       ;CLEAR STACK TO PREVENT STACK OVERFLOW
READ_STRING ENDP

CODE ENDS
END