         TITLE 'MAINFRAME MAIL BATCH RPG EXIT PROGRAM'
* CHANGE HISTORY  ------------------------------------------------
* 06/07/2019 DLC0 ORIGINAL PROGRAM.
* END OF HISTORY  ------------------------------------------------
         EJECT                                                          00018300
* =================================================================== * 00018400
MAILPARM DSECT                         MAILBR PARMS DUMMY SECTION       00018500
* =================================================================== * 00018700
PARMDATA DS    XL64                                                     00018800
* =================================================================== * 00020500
         EJECT                                                          00020600
* =================================================================== * 00020700
*        REGISTER USAGES                                                00020800
* =================================================================== * 00020900
R0       EQU   0                       RPG "SPECIAL" FUNCTION CODE      00021000
R1       EQU   1                       RPG "SPECIAL" RECORD ADDRESS     00021100
R2       EQU   2                                                        00021200
R3       EQU   3                                                        00021300
R4       EQU   4                                                        00021400
R5       EQU   5                                                        00021500
R6       EQU   6                                                        00021600
R7       EQU   7                                                        00021700
R8       EQU   8                                                        00021800
R9       EQU   9                                                        00021900
R10      EQU   10                                                       00022000
R11      EQU   11                      MAILBR CSECT BASE REG            00022100
R12      EQU   12                                                       00022200
R13      EQU   13                      REGISTER SAVEAREA                00022300
R14      EQU   14                      RETURN REG                       00022400
R15      EQU   15                      ENTRY REG                        00022500
* =================================================================== * 00022600
         EJECT                                                          00012700
* =================================================================== * 00012800
*        MACROS                                                       * 00012900
* =================================================================== * 00013000
         PRINT OFF
         COPY  HEADER                  SUBROUTINE PROLOG
         COPY  TRAILER                 SUBROUTINE EPILOG
         COPY  IF                      STRUCTURED MACRO SET
         PRINT ON
* =================================================================== * 00012800
         EJECT                                                          00022700
* =================================================================== * 00022800
MAILBR   HEADER VERSION=' 1.0',AUTHOR=' DLC',DATE='(C)JUN19',          X
               BASE=R11,SVAREA=SAVEAREA
* ------------------------------------------------------------------- *
         ST    R0,SPFUNC               SAVE RPG "SPECIAL" FUNCTION
         ST    R1,RECADD               SAVE "SPECIAL" RECORD ADDRESS

         IF    R0,(EQ,C),=F'8'         IF F'8' = CLOSE
         AND   MAILCD,EQ,C'H'          AND NO MAIL STARTED
          B    RETURN                   SKIP FURTHER PROCESSING
         ENDIF

         IF    INITFLG,NE,C'Y'         IF MAILWRTB NOT LOADED
LOADWRTR  MVI  INITFLG,C'I'             INDICATE INITIALIZATION PHASE
          LA   R1,1                     ONE-TIME INIT OF COBOL STG
          L    R15,IGZERRE              GET ADDRESS OF LE ROUTINE
          BALR  R14,R15                 CALL LE ROUTINE
          CDLOAD MAILWRTB               DYNAMIC LOAD OF COBOL PHASE
          IF   R15,(NZ,LTR),R15         NON-ZERO RETURN CODE?
           WTO 'MAILBR: UNABLE TO LOAD MAIL BATCH WRITER'
           B   ABEND                     OOPS, ABEND
          ENDIF
          ST   R1,WRTR_EP               SAVE ENTRY POINT FOR WRITER
          LA   R1,MAILWR                CREATE WRITER PARM LIST
          ST   R1,PARMLIST              PUT ADDRESS IN PARM LIST
          OI   PARMLIST,X'80'           FLAG AS LAST ADDR. ENTRY
          MVI  INITFLG,C'Y'             INDICATE PHASE LOAD COMPLETE
         ENDIF

         IF    MAILCD,NE,C'B'          IF INVOKED VIA RPG "EXIT"
MAILHEAD  BAL  R9,MAILINIT              GO CLEAR THE DATA AREA
          L    R3,VMAILEX               GET ADDR. OF EXIT PARMS
          MVC  HMAILEX,0(R3)            PUT IN PARM AREA
          B    MAILWRTR                 GO PROCESS MAIL REQUEST
         ENDIF

         L     R0,SPFUNC               GET RPG "SPECIAL" FUNCTION

*        IF    R0,(NE,C),=F'0'         IF NOT F'0' = READ
         IF    R0,(NE,C),=F'4'         IF NOT F'4' = WRITE
         AND   R0,(NE,C),=F'8'         AND NOT F'8' = CLOSE
          WTO  'MAILBR: UNSUPPORTED RPG "SPECIAL" FUNCTION'
          B    ABEND                     OOPS, ABEND
         ENDIF

         IF    R0,(EQ,C),=F'8'         IF F'8' = CLOSE
MAILCOMP  MVI  MAILCD,C'C'              SET UP TO COMPLETE MAIL
          BAL  R9,MAILINIT              GO CLEAR THE DATA AREA
          B    MAILWRTR                 GO COMPLETE THE MAIL MESSAGE
         ENDIF                                                          QVSM5630

         B     MAILBODY                GO PROCESS BODY TEXT
* ------------------------------------------------------------------- * 00022800
MAILINIT MVI   MAILDATA,C'0'           INITIALIZE DATA AREA
         MVC   MAILDATA+1(L'MAILDATA-1),MAILDATA
         BR    R9                      RETURN TO CALL POINT
MAILBODY MVI   MAILCD,C'B'             SET UP FOR BODY TEXT
         L     R1,RECADD               GET "SPECIAL" RECORD ADDRESS
         MVC   MAILDATA,0(R1)          GET BODY TEXT
MAILWRTR LA    R1,PARMLIST             PASS ADDR. OF PLIST TO WRITER
         L     15,WRTR_EP              GET ENTRY POINT OF WRITER
         BALR  14,15                   INVOKE IT
         MVI   MAILCD,C'B'             SET UP FOR BODY TEXT
         B     RETURN                  GO RETURN TO THE CALLER
* ------------------------------------------------------------------- * 00022800
ABEND    EQU   *
         CANCEL ALL                    CANCEL THE JOB
RETURN   EQU   *                       RETURN TO CALLER
         XR    R15,R15                 SET ZERO RETURN CODE
         TRAILER RETCODE=R15,SVAREA=SAVEAREA
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* ------------------------------------------------------------------- *
SAVEAREA DS    9D
IGZERRE  DC    V(IGZERRE)
VMAILEX  DC    V(MAILEX)               FOR RPG RLABL
         ENTRY MAILWR                  FOR RPG ULABL
SPFUNC   DC    A(0)
RECADD   DC    A(0)
WRTR_EP  DC    A(0)
PARMLIST DC    A(0)
INITFLG  DC    CL1'N'
* ------------------------------------------------------------------- *
         LTORG
* ------------------------------------------------------------------- *
MAILWR   DS    0D                      WRITER PARAMETER AREA
MAILCD   DC    CL1'H'                  MAIL CODE 'H'EAD PARMS
*                                                'D'ISTRIBUTION
*                                                'B'ODY PARMS
*                                                'C'OMPLETE MSG
*                                                'P'URGE MSG
MAILID   DC    XL8'00'                 MAIL IN-FLIGHT IDENTIFIER
MAILSQ   DC    PL3'0'                  BODY IN-FLIGHT SEQUENCE
MAILDATA DC    CL240' '                MAIL DATA (REDEFINED)
         ORG   MAILDATA
HMAILEX  DS    0CL161
MAILGRP  DC    CL25' '                 MAIL TO-GROUP
MAILFRM  DC    CL64' '                 MAIL FROM
MAILSBJ  DC    CL64' '                 MAIL SUBJECT
MAILDLV  DC    CL1' '                  .... DELIVER OPTION
MAILPVT  DC    CL1' '                  .... PRIVATE OPTION
MAILSYS  DC    CL2' '                  .... SYSTEM CODE
MAILRSV  DC    CL4' '                  .... RESERVED
         ORG
* =================================================================== * 00022800
         END
