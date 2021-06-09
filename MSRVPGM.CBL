 CBL XOPTS(SP)
      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    MSRVPGM.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  JUNE 2019.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      TRANSACTION SECURITY, ONLY.
      *REMARKS.       MAINFRAME MAIL SERVER PROGRAM STARTED BY PLTPI
      *               OR VIA THE COMMAND LINE INTERFACE.

      * CHANGE HISTORY ------------------------------------------------
      * 06/05/2019 DLC ORIGINAL PROGRAM.
/DLC0/* 04/01/2021 DLC PUT MAIL ID IN JOB FOR DIAGNOSTIC PURPOSES.
      * END OF HISTORY ------------------------------------------------

      /*****************************************************************
      *                                                                *
      *    ENVIRONMENT DIVISION                                        *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.

      ******************************************************************
      *    CONFIGURATION SECTION                                       *
      ******************************************************************
       CONFIGURATION SECTION.

       SOURCE-COMPUTER. IBM-2086-A04-140.
       OBJECT-COMPUTER. IBM-2086-A04-140.

      /*****************************************************************
      *                                                                *
      *    DATA DIVISION                                               *
      *                                                                *
      ******************************************************************
       DATA DIVISION.

      ******************************************************************
      *    WORKING-STORAGE SECTION                                     *
      ******************************************************************
       WORKING-STORAGE SECTION.

       01  CONTROL-FIELDS.
      *
      * FIELDS TO MANAGE PROGRAM AND MAP RESOURCE NAMES
         COPY COMMWORK REPLACING =='TRAN'==    BY =='MSRV'==
                                 =='PROGRAM'== BY =='MSRVPGM'==.
      *
      * FIELDS TO MANAGE REFERENCED FILES
         03  MAILMID                   PIC  X(08)   VALUE 'MAILMID '.
         03  MAILMSG                   PIC  X(08)   VALUE 'MAILMSG '.
         03  MAILMDS                   PIC  X(08)   VALUE 'MAILMDS '.
         03  MAILDST                   PIC  X(08)   VALUE 'MAILDST '.
         03  IESCNTL                   PIC  X(08)   VALUE 'IESCNTL '.
         03  IESLDUV                   PIC  X(08)   VALUE 'IESLDUV '.
      *
      * FIELDS TO MANAGE REFERENCED EXTERNAL PROGRAM NAMES
         03  CICSINFO                  PIC  X(08)   VALUE 'CICSINFO'.
         03  IESLDGAC                  PIC  X(08)   VALUE 'IESLDGAC'.
      *
      * FIELDS TO MANAGE ERROR HANDLING
         03  ERROR-SWITCH              PIC S9(01)   VALUE ZEROES.
           88  NO-ERRORS-FOUND                      VALUE ZEROES.
           88  ERROR-AT-CURSOR                      VALUE -1.
           88  ERRORS-FOUND                         VALUES -9 THRU -1.
         03  FROM-SWITCH               PIC  X(01)   VALUE 'N'.
           88  FROM-A-PERSON                        VALUE 'Y'.
           88  FROM-A-PROGRAM                       VALUE 'N'.
         03  LDAP-SWITCH               PIC  X(01)   VALUE 'Y'.
           88  LDAP-CONFIRMED                       VALUE 'Y'.
           88  LDAP-FAILED                          VALUE 'N'.
         03  DIST-CNT                  PIC S9(04)   BINARY VALUE ZEROES.
         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.
         03  LEN                       PIC S9(04)   BINARY VALUE ZEROES.
         03  CNT                       PIC S9(04)   BINARY VALUE ZEROES.
         03  POS                       PIC S9(04)   BINARY VALUE ZEROES.
         03  PTR                       PIC S9(04)   BINARY VALUE ZEROES.
         03  SUB                       PIC S9(04)   BINARY VALUE ZEROES.
         03  JSUB                      PIC S9(04)   BINARY VALUE ZEROES.
         03  JCNT                      PIC S9(04)   BINARY VALUE ZEROES.
         03  MMID-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
         03  MMSG-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
         03  MDST-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
         03  CNTL-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
      *
         03  FROM-ID                   PIC  X(8).
         03  FROM-NAME                 PIC  X(64).
         03  FROM-ADDR                 PIC  X(64).
      *
         03  EVOKE-TIME                PIC S9(07)   COMP-3 VALUE +100.
         03  nbsp                      PIC  X(01)   value x'41'.
         03  EDIT-CREATET              PIC  X(08).
         03  EDIT-CREATED              PIC  X(10).
         03  EDIT-MSGID                PIC  Z(18).
         03  DIST-GROUP                PIC  X(25).
         03  LOWER-CASE  PIC  X(26)  VALUE 'abcdefghijklmnopqrstuvwxyz'.
         03  UPPER-CASE  PIC  X(26)  VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
         03  VAR-TEXTL                 PIC S9(4)    BINARY.
         03  VAR-TEXT                  PIC  X(245)  VALUE SPACES.
      *
         03  TS-QUEUE.
           05  TS-TRAN                 PIC  X(4).
           05  TS-TERM                 PIC  X(4).
         03  TS-TOTL                   PIC S9(4)    BINARY.
         03  TS-ITEM                   PIC S9(4)    BINARY.
         03  TS-RECD.
           05  TS-TYPE                 PIC  X(1).
           05  TS-NAME                 PIC  X(64).
           05  TS-ADDR                 PIC  X(64).
      *
         03  DIST-TABLE.
           05  DIST-NAMES              PIC  X(25)   OCCURS 128.
      *
      * TO CONVERT NUMBERS TO ZONED-DECIMAL CHARACTER
         03  NUM-WORK                  PIC  9(05)   VALUE ZEROES.
         03  NUM-WRK5                  REDEFINES    NUM-WORK.
           05  FILLER                  PIC  X(01).
           05  NUM-WRK4.
             07  FILLER                PIC  X(01).
             07  NUM-WRK3.
               09  FILLER              PIC  X(01).
               09  NUM-WRK2.
                 11  FILLER            PIC  X(01).
                 11  NUM-WRK1          PIC  X(01).
      *
      * TO CONVERT NUMERIC TO BINARY-CODED CHARACTER AND VICE VERSA
         03  DOUBLE-WORD               PIC S9(18)   BINARY.
         03  EIGHT-BYTES               REDEFINES    DOUBLE-WORD.
           05                          PIC  X(2).
           05  SIX-BYTES.
             07                        PIC  X(2).
             07  FULL-WORD             PIC S9(9)    BINARY.
             07  FOUR-BYTES            REDEFINES    FULL-WORD.
               09  HI-MSB              PIC  X.
               09  THREE-BYTES.
                 11  HI-LSB            PIC  X.
                 11  HALF-WORD         PIC S9(4)    BINARY.
                 11  TWO-BYTES         REDEFINES    HALF-WORD.
                   13  LO-MSB          PIC  X.
                   13  LO-LSB          PIC  X.
      *
      * TO CONVERT DATES AND TIMES
         03  EDIT-DATE                 PIC  9999/99/99.
         03  NUM-DATE                  PIC  9(8).
         03  WRK-DATE                  REDEFINES    NUM-DATE.
           05  DTE-GRMO                PIC  99.
           05  DTE-GRDA                PIC  99.
           05  DTE-GRYR                PIC  9999.
         03  WS-ABSTIME                PIC S9(15)   COMP-3 VALUE ZEROES.
         03  WS-WRKDATE.
           05  WS-MODATE               PIC  9(02).
           05                          PIC  X.
           05  WS-DADATE               PIC  9(02).
           05                          PIC  X.
           05  WS-CYDATE               PIC  9(04).
         03  WS-WRKTIME.
           05  WS-HRSTIME              PIC  9(02).
           05                          PIC  X(03).
           05  WS-XXXTIME              PIC  X(03).

      * SUBMISSION JOB CONTROL AREAS

       01  MAIL-AREA.
         03  MFMAIL                    PIC  X(08)   VALUE 'MFMAIL'.
       01  MAIL-JCL.
         03  PIC X(40) VALUE '* $$ JOB JNM=MFMAIL,DISP=D,CLASS=R      '.
         03  PIC X(40) VALUE '* $$ LST DISP=D,CLASS=Z,PURGE=0         '.
         03  PIC X(40) VALUE '// JOB    MFMAIL   Mail Distribution    '.
         03  PIC X(40) VALUE '// LIBDEF *,SEARCH=(PRD2.CONFIG,ESP.BSI)'.
         03  PIC X(40) VALUE '* $$ SLI SYSCODE                        '.
         03  PIC X(40) VALUE '// OPTION SYSPARM=''00''                '.
/DLC0/   03  PIC X(40) VALUE '*  MAILID=#MAILID#                      '.
         03  PIC X(40) VALUE '// EXEC REXX=BSISMTPC,SIZE=196K,SYSCODE '.
         03  PIC X(40) VALUE 'ID 00                                   '.
         03  PIC X(40) VALUE 'OPEN INTERNET.WINWHOLESALE.COM          '.
         03  PIC X(40) VALUE 'HELO LIBRA<SYSCODE>.WINWHOLESALE.COM    '.
         03  PIC X(40) VALUE '+++HEAD+++                              '.
         03  PIC X(40) VALUE 'DATA                                    '.
         03  PIC X(40) VALUE '+++BODY+++                              '.
         03  PIC X(40) VALUE 'QUIT                                    '.
         03  PIC X(40) VALUE '/* EOD                                  '.
         03  PIC X(40) VALUE '/* EOD                                  '.
         03  PIC X(40) VALUE '/& EOJ                                  '.
         03  PIC X(40) VALUE '* $$ EOJ                                '.
       01  MAIL-TABLE                  REDEFINES    MAIL-JCL.
/DLC0/   03  MAIL-ENTRY                PIC  X(40)   OCCURS 19.

      * IBM LDAP GET ATTRIBUTE INTERFACE
       COPY IESLDGAC.

      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS

       01  CICSINFO-PARMS.
         COPY CICSINFO.

       COPY CICSSORT.

       COPY HEXMAN.

       COPY LOGGING.

       COPY PRTMAN.

       COPY UNEXERRW.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

      * MAILMID RECORD I/O AREA
       COPY MAILMID.

      * MAILMSG RECORD I/O AREA
       COPY MAILMSG.

      * MAILDST RECORD I/O AREA
       COPY MAILDST.

      * MAILMDS RECORD I/O AREA
       COPY MAILMDS.

      * IESCNTL RECORD I/O AREA
       COPY IESCNTL.

      * IESLDUM RECORD I/O AREA
       COPY IESLDUM.

      * COMMAND LINE INTERFACE
       01  INPUT-AREA.
         03                            PIC  X(10).

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *    PROGRAM INITIALIZATION                                      *
      ******************************************************************
       A00-MAIL-INITIALIZATION.

           SET  DAPL-LOGGING           TO TRUE.

           EXEC CICS LINK
                     PROGRAM  (CICSINFO)
                     COMMAREA (CICSINFO-PARMS)
           END-EXEC.

           IF  EIBCALEN NOT > ZERO
               EVALUATE CICS-STATUS        ALSO TRUE
               WHEN DFHVALUE(FIRSTQUIESCE) ALSO TRUE
               WHEN DFHVALUE(FINALQUIESCE) ALSO TRUE
                   GO TO Z00-MAIL-TERMINATION
               WHEN DFHVALUE(STARTUP)      ALSO TRUE
                   SET  LOG-TO-BOTH    TO TRUE
                   MOVE 'MAINFRAME MAIL SERVER STARTING...'
                                       TO LOGF-MESG
                   PERFORM Q100-LOGIT THRU Q199-EXIT
                   GO TO C10-MAIL-RESTART
               WHEN DFHVALUE(ACTIVE)       ALSO EIBTRMID = 'CNSL'
                   EXEC CICS RETRIEVE
                             SET(ADDRESS OF INPUT-AREA)
                             LENGTH(UNEX-MSGL)
                             NOHANDLE
                   END-EXEC
                   IF (INPUT-AREA(1:5) = 'START'
                   OR  INPUT-AREA(1:4) = 'STOP' OR 'STRT'
                   OR  INPUT-AREA(2:4) = 'STOP' OR 'STRT'
                   OR  INPUT-AREA(2:5) = 'START')
                   AND EIBRESP = DFHRESP(NORMAL)
                       MOVE +1         TO IDX
                       GO TO P90-SERVER-CONTROL
                   END-IF
               WHEN DFHVALUE(ACTIVE)       ALSO CICS-FROM-START
                   GO TO C00-MAIL-DELIVERY
               WHEN DFHVALUE(ACTIVE)       ALSO CICS-FROM-STARTD
                   CONTINUE
               WHEN OTHER
                   IF  NOT CICS-FROM-TERMINAL
                       STRING 'START FROM ''' CICS-STARTCODE
                              ''' NOT SUPPORTED.' DELIMITED BY SIZE
                                     INTO LOGF-MESG
                       PERFORM Q100-LOGIT THRU Q199-EXIT
                       GO TO Z00-MAIL-TERMINATION
                   END-IF
                   EXEC CICS RECEIVE
                             SET(ADDRESS OF INPUT-AREA)
                             LENGTH(UNEX-MSGL)
                             NOHANDLE
                   END-EXEC
                   IF (INPUT-AREA(5:5) = 'START'
                   OR  INPUT-AREA(5:4) = 'STOP' OR 'STRT'
                   OR  INPUT-AREA(6:4) = 'STOP' OR 'STRT'
                   OR  INPUT-AREA(6:5) = 'START')
                       MOVE +5         TO IDX
                       GO TO P90-SERVER-CONTROL
                   END-IF
               END-EVALUATE
           END-IF.

           MOVE 1                      TO UNEX-MSGL.
           STRING 'This application has no screen interface.'
               DELIMITED BY SIZE INTO UNEX-MSG POINTER UNEX-MSGL.
           MOVE UNEX-IC                TO UNEX-MSG(UNEX-MSGL:1).

           GO TO X50-SEND-MSG.

      /*****************************************************************
      *    MAIL DELIVERY ROUTINE                                       *
      ******************************************************************
       C00-MAIL-DELIVERY.

           PERFORM Q20-INITKEY-MAILMID.
           PERFORM Q21-STARTBR-MAILMID.

           PERFORM WITH TEST BEFORE
             UNTIL EIBRESP NOT = DFHRESP(NORMAL)
               PERFORM Q23-READNEXT-MAILMID
               IF  EIBRESP = DFHRESP(NORMAL)
               AND MMID-SENT-TIMESTAMP <= ZEROES
               AND MMID-NORMAL-DELIVERY
               AND MMID-IS-COMPLETE
                   PERFORM C20-MAIL-PROCESS THRU C95-EXIT
               END-IF
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               SET  LOG-TO-BOTH        TO TRUE
               MOVE 'BROWSING FOR MAIL...'
                                       TO LOGF-MESG
               PERFORM Q200-UNEX-LOG
           END-IF.

           PERFORM Q25-ENDBR-MAILMID.

       C10-MAIL-RESTART.

           EXEC CICS START
                     REQID(THIS-PGM)
                     INTERVAL(EVOKE-TIME)
                     TRANSID(THIS-TRN)
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               SET  LOG-TO-BOTH        TO TRUE
               MOVE 'RESTARTING MAIL CYCLE...'
                                       TO LOGF-MESG
               PERFORM Q200-UNEX-LOG
           END-IF.

           IF  EIBTRMID > SPACES
               MOVE +1                 TO POS
               STRING 'MAIL SERVER STARTED.'
                   DELIMITED BY SIZE INTO UNEX-MSG
                                     WITH POINTER POS
               SUBTRACT 1 FROM POS GIVING UNEX-MSGL
               MOVE UNEX-MSG(1:POS)    TO LOGF-MESG
               SET  LOG-TO-LOGFILE     TO TRUE
               PERFORM Q100-LOGIT THRU Q199-EXIT
               GO TO X50-SEND-MSG
           END-IF.

           GO TO Z00-MAIL-TERMINATION.

       C20-MAIL-PROCESS.

           MOVE MMID-ID                TO EDIT-MSGID.
           MOVE EDIT-MSGID             TO UNEX-VARIABLE.
           PERFORM Q203-LEFT-JUSTIFY.
/DLC0/     MOVE UNEX-VARIABLE(1:UNEX-LENG)
/DLC0/                                 TO VAR-TEXT.

           EXEC CICS FORMATTIME
                     ABSTIME  (MMID-CREATED-TIMESTAMP)
                     YYYYMMDD (EDIT-CREATED) DATESEP
                     TIME     (EDIT-CREATET) TIMESEP
           END-EXEC.

           STRING 'FOUND: '               DELIMITED BY SIZE
/DLC0/            VAR-TEXT                DELIMITED BY SPACE
                  ' ... ' EDIT-CREATED
                  ' ' EDIT-CREATET
                  ' ' MMID-SUBJECT        DELIMITED BY SIZE
                                     INTO LOGF-MESG.
           PERFORM Q100-LOGIT THRU Q199-EXIT.

       C30-MAIL-START.

           MOVE ZEROES                 TO DIST-CNT.
           MOVE SPACES                 TO DIST-TABLE.
           PERFORM Q65-DELETEQ-TS.

           MOVE MMID-TO-GRP            TO DIST-GROUP.
           PERFORM P10-LOAD-DISTRIBUTION THRU P15-EXIT.

           IF  TS-TOTL = ZEROES
               SET  LOG-TO-BOTH        TO TRUE
               STRING 'DISTRIBUTION GROUP ' DELIMITED BY SIZE
                      DIST-GROUP            DELIMITED BY SPACE
                      ' HAS NO ENTRIES.'    DELIMITED BY SIZE
                                     INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
           END-IF.

           PERFORM Q50-INITKEY-MAILMDS.
           MOVE MMID-ID                TO MMDS-ID.
           PERFORM Q51-STARTBR-MAILMDS.

           PERFORM WITH TEST BEFORE
             UNTIL MMDS-ID NOT = MMID-ID
                OR EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q53-READNEXT-MAILMDS
             IF  EIBRESP = DFHRESP(NORMAL)
             AND MMDS-ID = MMID-ID
                 MOVE MMDS-TO-GRP      TO DIST-GROUP
                 PERFORM P10-LOAD-DISTRIBUTION THRU P15-EXIT
             END-IF
           END-PERFORM.

           PERFORM Q55-ENDBR-MAILMDS.

           IF  TS-TOTL = ZEROES
               SET  LOG-TO-BOTH        TO TRUE
               STRING 'MAIL MESSAGE ID '  DELIMITED BY SIZE
/DLC0/                VAR-TEXT            DELIMITED BY SPACE
                      ' HAS NO DISTRIBUTION.  DEFAULTING...'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT

               IF  MMID-SYSTEM > SPACES
                   MOVE SPACES         TO DIST-GROUP
                   STRING 'DEFAULT_'      DELIMITED BY SIZE
                          MMID-SYSTEM     DELIMITED BY SPACE
                          '_GROUP'        DELIMITED BY SIZE
                                     INTO DIST-GROUP
               ELSE
                   MOVE 'MAINFRAME_PROGRAMMING'
                                       TO DIST-GROUP
               END-IF
               PERFORM P10-LOAD-DISTRIBUTION THRU P15-EXIT
           END-IF.

           IF  TS-TOTL = ZEROES
               SET  LOG-TO-BOTH        TO TRUE
               STRING 'MAIL MESSAGE ID '  DELIMITED BY SIZE
/DLC0/                VAR-TEXT            DELIMITED BY SPACE
                      ' STILL HAS NO DISTRIBUTION.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               PERFORM P50-FLAG-INCOMPLETE THRU P55-EXIT
               GO TO C95-EXIT
           END-IF.

       C40-MAIL-BUILD.

           MOVE SPACE                  TO PRT-LNK-REQU.
           SET  PRT-PWR-RDRQ           TO TRUE.
           MOVE MFMAIL                 TO PRT-PWR-JOBN.
           MOVE CICS-USERID            TO PRT-PWR-USER.
           MOVE 80                     TO PRT-STR-LENG.

           COMPUTE JCNT = LENGTH OF MAIL-JCL
                        / LENGTH OF MAIL-ENTRY.

       C50-MAIL-HEADERS.

           PERFORM WITH TEST BEFORE
             VARYING JSUB FROM 1 BY 1
               UNTIL JSUB > JCNT
                  OR MAIL-ENTRY(JSUB)(1:10) = '+++HEAD+++'
                  OR EIBRESP NOT = DFHRESP(NORMAL)
                  OR NOT PRINT-COMPLETED
             MOVE MAIL-ENTRY(JSUB)     TO PRT-STR-DATA
/DLC0/       IF  PRT-STR-DATA(11:8) = '#MAILID#'
/DLC0/           MOVE VAR-TEXT         TO PRT-STR-DATA(11:)
/DLC0/       END-IF
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.

           IF  JSUB > JCNT
               SET  LOG-TO-BOTH        TO TRUE
               MOVE 'MAIL JCL MISSING HEAD INCLUSION POINT.'
                                       TO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               SET  PRINT-DELSPOOL     TO TRUE
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT
               GO TO C95-EXIT
           END-IF.

           ADD  1                      TO JSUB.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1                      TO VAR-TEXTL.
           MOVE SPACES                 TO VAR-TEXT.
           STRING 'MAIL From: '
               DELIMITED BY SIZE     INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL.
           IF  MMID-FROM > SPACES
               PERFORM P80-CHECK-FROM-PERSON THRU P85-EXIT
               IF  FROM-A-PERSON
               AND FROM-ADDR > SPACES
                 MOVE FROM-NAME        TO UNEX-VARIABLE
                 PERFORM Q202-SET-LENGTH
                 STRING UNEX-VARIABLE(1:UNEX-LENG)
                        ' <'              DELIMITED BY SIZE
                        FROM-ADDR         DELIMITED BY SPACE
                        '>'
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL
               ELSE
                 MOVE MMID-FROM        TO UNEX-VARIABLE
                 PERFORM Q202-SET-LENGTH
                 STRING UNEX-VARIABLE(1:UNEX-LENG)
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL
               END-IF
           ELSE
               STRING 'LIBRA<SYSCODE> Mainframe'
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL
           END-IF.
           PERFORM Q10-SEND-MAIL-HEADER THRU Q15-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1                      TO VAR-TEXTL.
           STRING 'ORGA Organization: Winsupply Group Services'
               DELIMITED BY SIZE     INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL.
           PERFORM Q10-SEND-MAIL-HEADER THRU Q15-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1                      TO VAR-TEXTL.
           STRING 'RPTO ReplyTo: '
               DELIMITED BY SIZE     INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL.
           IF  FROM-A-PERSON
           AND FROM-ADDR > SPACES
               MOVE FROM-NAME          TO UNEX-VARIABLE
               PERFORM Q202-SET-LENGTH
               STRING UNEX-VARIABLE(1:UNEX-LENG)
                      ' <'                DELIMITED BY SIZE
                      FROM-ADDR           DELIMITED BY SPACE
                      '>'
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL
           ELSE
               MOVE 1                  TO TS-ITEM
               PERFORM Q62-READQ-TS
               IF  TS-NAME > SPACES
                 MOVE TS-NAME          TO UNEX-VARIABLE
                 PERFORM Q202-SET-LENGTH
                 STRING UNEX-VARIABLE(1:UNEX-LENG) ' '
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL
               END-IF
               MOVE TS-ADDR            TO UNEX-VARIABLE
               PERFORM Q202-SET-LENGTH
               STRING '<' UNEX-VARIABLE(1:UNEX-LENG) '>'
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL
           END-IF.
           PERFORM Q10-SEND-MAIL-HEADER THRU Q15-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF  MMID-IS-PRIVATE
               MOVE 1                  TO VAR-TEXTL
               STRING 'SENS Sensitivity: Company-Confidential'
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL
               PERFORM Q10-SEND-MAIL-HEADER THRU Q15-EXIT
               IF  EIBRESP NOT = DFHRESP(NORMAL)
               OR  NOT PRINT-COMPLETED
                   GO TO C90-MAIL-RETURN
               END-IF
           END-IF.

       C60-MAIL-DISTRIBUTION.

      *    PERFORM P70-SORT-TS-QUEUE THRU P75-EXIT.

           PERFORM WITH TEST BEFORE
             VARYING TS-ITEM FROM 1 BY 1
               UNTIL TS-ITEM > TS-TOTL
                  OR EIBRESP NOT = DFHRESP(NORMAL)
                  OR NOT PRINT-COMPLETED

             PERFORM Q62-READQ-TS
             IF  EIBRESP = DFHRESP(NORMAL)

               MOVE 1                  TO VAR-TEXTL
               EVALUATE TS-TYPE
               WHEN 3
                 STRING 'RCPT Bcc: '
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL
               WHEN 2
                 STRING 'RCPT Cc: '
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL
               WHEN OTHER
                 STRING 'RCPT To: '
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL
               END-EVALUATE

               IF  TS-NAME > SPACES
                 MOVE TS-NAME          TO UNEX-VARIABLE
                 PERFORM Q202-SET-LENGTH
                 STRING UNEX-VARIABLE(1:UNEX-LENG) ' '
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL
               END-IF

               MOVE TS-ADDR            TO UNEX-VARIABLE
               PERFORM Q202-SET-LENGTH
               STRING '<' UNEX-VARIABLE(1:UNEX-LENG) '>'
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL

               PERFORM Q10-SEND-MAIL-HEADER THRU Q15-EXIT
             END-IF

           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ITEMERR)
               PERFORM Q65-DELETEQ-TS
               GO TO C90-MAIL-RETURN
           END-IF.

           PERFORM Q65-DELETEQ-TS.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1                      TO VAR-TEXTL.
           STRING 'SUBJ Subject: '
               DELIMITED BY SIZE     INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL.
           IF  MMID-SUBJECT > SPACES
               MOVE MMID-SUBJECT       TO UNEX-VARIABLE
               PERFORM Q202-SET-LENGTH
               STRING UNEX-VARIABLE(1:UNEX-LENG)
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL
           ELSE
               STRING '[no subject specified]'
                   DELIMITED BY SIZE INTO VAR-TEXT
                                     WITH POINTER VAR-TEXTL
           END-IF.
           PERFORM Q10-SEND-MAIL-HEADER THRU Q15-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.

       C70-MAIL-BODY.

           PERFORM WITH TEST BEFORE
             VARYING JSUB FROM JSUB BY 1
               UNTIL JSUB > JCNT
                  OR MAIL-ENTRY(JSUB)(1:10) = '+++BODY+++'
                  OR EIBRESP NOT = DFHRESP(NORMAL)
                  OR NOT PRINT-COMPLETED
             MOVE MAIL-ENTRY(JSUB)     TO PRT-STR-DATA
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.

           IF  JSUB > JCNT
               SET  LOG-TO-BOTH        TO TRUE
               MOVE 'MAIL JCL MISSING BODY INCLUSION POINT.'
                                       TO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               SET  PRINT-DELSPOOL     TO TRUE
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT
               GO TO C95-EXIT
           END-IF.

           ADD  1                      TO JSUB.

           PERFORM Q30-INITKEY-MAILMSG.
           MOVE MMID-ID                TO MMSG-ID.
           PERFORM Q31-STARTBR-MAILMSG.

           PERFORM WITH TEST BEFORE
             UNTIL MMSG-ID NOT = MMID-ID
                OR EIBRESP NOT = DFHRESP(NORMAL)
                OR NOT PRINT-COMPLETED

             PERFORM Q33-READNEXT-MAILMSG
             IF  EIBRESP = DFHRESP(NORMAL)
             AND MMSG-ID = MMID-ID

               COMPUTE LEN = MMSG-RECL - LENGTH OF MMSG-KEY
               MOVE LEN                TO VAR-TEXTL
               MOVE MMSG-BODY(1:LEN)   TO VAR-TEXT
               PERFORM P60-SET-TEXT-LENGTH THRU P65-EXIT
               IF  VAR-TEXTL > ZEROES
                 ADD  1                TO VAR-TEXTL
                 MOVE x'41'            TO VAR-TEXT(VAR-TEXTL:1)
               END-IF

               IF  VAR-TEXTL <= ZEROES
                 MOVE '<br>'           TO PRT-STR-DATA
               ELSE
                 PERFORM WITH TEST AFTER UNTIL VAR-TEXTL <= ZEROES
                   IF  VAR-TEXTL <= PRT-STR-LENG
                     MOVE VAR-TEXT(1:VAR-TEXTL)
                                       TO PRT-STR-DATA
                     MOVE ZEROES       TO VAR-TEXTL
                   ELSE
                     PERFORM WITH TEST BEFORE
                       VARYING POS FROM PRT-STR-LENG BY -1
                         UNTIL POS < 1 OR VAR-TEXT(POS:1) > SPACE
                     END-PERFORM
                     IF  POS < 1
                       MOVE PRT-STR-LENG TO POS
                     END-IF
                     MOVE VAR-TEXT(1:POS) TO PRT-STR-DATA
                     PERFORM P00-CALL-PRTMAN THRU P05-EXIT
                     SUBTRACT POS    FROM VAR-TEXTL
                     MOVE VAR-TEXT(POS + 1:)
                                       TO VAR-TEXT(1:VAR-TEXTL)
                   END-IF
                 END-PERFORM
               END-IF

               PERFORM P00-CALL-PRTMAN THRU P05-EXIT
             END-IF
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               GO TO C90-MAIL-RETURN
           END-IF.

           PERFORM Q35-ENDBR-MAILMSG.

      * add body content to all emails sent via this method

           STRING '<h1 style="font-family:helvetica;"><em><font '
                  'color=blue>Win</font>supply</em>' nbsp
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.

           MOVE '<span style="font-size:8pt;font-weight:bold;">'
                                       TO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.

           MOVE 'The Winsupply Family of Companies</span></h1>'
                                       TO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.

           STRING 'Sent by Mainframe Mail: '
                  '"A new <em>frame</em> of mind."&nbsp;' nbsp
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.

           IF  DIST-CNT = 1
               STRING 'Distribution group "' DELIMITED BY SIZE
                      MMID-TO-GRP            DELIMITED BY SPACE
                      '" used to send this email.<br>'
                   DELIMITED BY SIZE INTO PRT-STR-DATA
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT
           ELSE
               STRING 'The following distribution'
                      ' groups used to send this email:<br>'
                   DELIMITED BY SIZE INTO PRT-STR-DATA
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT

               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > DIST-CNT
                      OR NOT PRINT-COMPLETED
                      OR EIBRESP NOT = DFHRESP(NORMAL)
                 STRING '<br>' DIST-NAMES(IDX)
                   DELIMITED BY SIZE INTO PRT-STR-DATA
                 PERFORM P00-CALL-PRTMAN THRU P05-EXIT
               END-PERFORM
           END-IF.

           STRING '<p><b>Note:</b> Managers may request distribution'
                  ' changes by logging into the' nbsp
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.

           MOVE '<a href="https://cherwellprod.winwholesale.com/Cherwell
      -         'Portal/IT">'          TO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.

           STRING 'WinZone/Support/Request Help</a> link.&nbsp;'
                  ' As more than one group may be' nbsp
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.

           STRING 'listed above, please include the specific'
                  ' group name desired (and a' nbsp
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.

           STRING 'screenshot of the email) in your'
                  ' request.&nbsp; Thanks.</p>' nbsp
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.

       C80-MAIL-COMPLETE.

           PERFORM WITH TEST BEFORE
             VARYING JSUB FROM JSUB BY 1
               UNTIL JSUB > JCNT
                  OR NOT PRINT-COMPLETED
                  OR EIBRESP NOT = DFHRESP(NORMAL)
             MOVE MAIL-ENTRY(JSUB)       TO PRT-STR-DATA
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO C90-MAIL-RETURN
           END-IF.

      * on successful completion, flag all usage metrics

           PERFORM Q26-READUP-MAILMID
           IF  EIBRESP = DFHRESP(NORMAL)
               EXEC CICS ASKTIME
                         ABSTIME(MMID-SENT-TIMESTAMP)
               END-EXEC
               PERFORM Q27-REWRITE-MAILMID
           END-IF.

           IF  EIBRESP = DFHRESP(NORMAL)
               MOVE MMID-TO-GRP        TO DIST-GROUP
               PERFORM P30-UPDATE-DIST-METRICS THRU P35-EXIT

               IF  EIBRESP = DFHRESP(NORMAL)
                   PERFORM Q50-INITKEY-MAILMDS
                   MOVE MMID-ID            TO MMDS-ID
                   PERFORM Q51-STARTBR-MAILMDS

                   PERFORM WITH TEST BEFORE
                     UNTIL MMDS-ID NOT = MMID-ID
                        OR EIBRESP NOT = DFHRESP(NORMAL)
                     PERFORM Q53-READNEXT-MAILMDS
                     IF  EIBRESP = DFHRESP(NORMAL)
                     AND MMDS-ID = MMID-ID
                         MOVE MMDS-TO-GRP  TO DIST-GROUP
                         PERFORM P30-UPDATE-DIST-METRICS THRU P35-EXIT
                     END-IF
                   END-PERFORM

                   IF  EIBRESP = DFHRESP(NORMAL)
                              OR DFHRESP(NOTFND)
                              OR DFHRESP(ENDFILE)
                       PERFORM Q55-ENDBR-MAILMDS
                   END-IF
               END-IF
           END-IF.

       C90-MAIL-RETURN.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               SET  LOG-TO-BOTH        TO TRUE
               PERFORM Q200-UNEX-LOG
               PERFORM P50-FLAG-INCOMPLETE THRU P55-EXIT
               GO TO C95-EXIT
           END-IF.

           IF  NOT PRINT-COMPLETED
               SET  PRINT-DELSPOOL     TO TRUE
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT
               SET  LOG-TO-BOTH        TO TRUE
               MOVE 'MAINFRAME MAIL GENERATION FAILED.  SEE LOG.'
                                       TO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               PERFORM P50-FLAG-INCOMPLETE THRU P55-EXIT
               GO TO C95-EXIT
           END-IF.

       C95-EXIT.
           EXIT.

      /*****************************************************************
      *    PERFORMED ROUTINES                                          *
      ******************************************************************

       P00-CALL-PRTMAN.
           EXEC CICS LINK
                     PROGRAM  (PRT-PGM-NAME)
                     COMMAREA (PRT-PGM-COMM)
                     LENGTH   (PRT-PGM-COML)
           END-EXEC.
       P02-CHK-COMPLETE.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO P05-EXIT
           END-IF.
           IF  PRINT-COMPLETED
               MOVE SPACES             TO PRT-STR-DATA
           ELSE
               MOVE PRT-LNK-RESP       TO UNEX-RESP
               MOVE PRT-LNK-RSP2       TO UNEX-RESP2
               STRING PRT-PGM-NAME ' FAILURE: RC=' PRT-LNK-RETN
                      ', RS=' UNEX-RESP ', R2=' UNEX-RESP2
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               IF  NOT PRINT-ENDOFMSG
                   SET PRINT-ENDOFMSG  TO TRUE
                   PERFORM P00-CALL-PRTMAN
               END-IF
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       P05-EXIT.
           EXIT.

       P10-LOAD-DISTRIBUTION.
           PERFORM Q40-INITKEY-MAILDST.
           MOVE DIST-GROUP             TO MDST-GRP-NAME.
           PERFORM Q41-STARTBR-MAILDST.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               SET  LOG-TO-BOTH        TO TRUE
               STRING 'OPENING GROUP '      DELIMITED BY SIZE
                      DIST-GROUP            DELIMITED BY SPACE
                      '...'                 DELIMITED BY SIZE
                                     INTO LOGF-MESG
               PERFORM Q200-UNEX-LOG
               PERFORM P50-FLAG-INCOMPLETE THRU P55-EXIT
               GO TO P15-EXIT
           END-IF.

           ADD  1                      TO DIST-CNT.
           MOVE DIST-GROUP             TO DIST-NAMES(DIST-CNT).

           PERFORM WITH TEST BEFORE
             UNTIL MDST-GRP-NAME NOT = DIST-GROUP
                OR EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q43-READNEXT-MAILDST
             IF  EIBRESP = DFHRESP(NORMAL)
             AND MDST-GRP-NAME = DIST-GROUP
               IF  NOT MDST-GRP-HEADER
                 PERFORM P20-WRITE-TS-RECD THRU P25-EXIT
               END-IF
             END-IF
           END-PERFORM.

           IF  EIBRESP = DFHRESP(NORMAL)
                      OR DFHRESP(NOTFND)
                      OR DFHRESP(ENDFILE)
               PERFORM Q45-ENDBR-MAILDST
               PERFORM Q61-NUMITEMS-TS
           END-IF.
       P15-EXIT.
           EXIT.

       P20-WRITE-TS-RECD.
           IF (SPACES >= MDST-NAME OR MDST-ADDR)
               PERFORM P40-GET-MAIL-ADDRESS THRU P45-EXIT
               IF  MDST-ADDR NOT > SPACES
                   GO TO P25-EXIT
               END-IF
           END-IF.

           EVALUATE MDST-TYPE
           WHEN '*BCC'
               MOVE 3                  TO TS-TYPE
           WHEN '*CC'
               MOVE 2                  TO TS-TYPE
           WHEN OTHER
               MOVE 1                  TO TS-TYPE
           END-EVALUATE.
           MOVE MDST-NAME              TO TS-NAME.
           MOVE MDST-ADDR              TO TS-ADDR.

           PERFORM Q63-WRITEQ-TS.
       P25-EXIT.
           EXIT.

       P30-UPDATE-DIST-METRICS.
           PERFORM Q40-INITKEY-MAILDST.
           MOVE DIST-GROUP             TO MDST-GRP-NAME.
           PERFORM Q46-READUP-MAILDST.
           IF  EIBRESP = DFHRESP(NORMAL)
               ADD  1                  TO MDST-USAGE-COUNT
               MOVE CICS-CCYYMMDD      TO MDST-LAST-USE-DATE
               MOVE MMID-CREATED-BY    TO MDST-LAST-USED-BY
               PERFORM Q47-REWRITE-MAILDST
           END-IF.
       P35-EXIT.
           EXIT.

       P40-GET-MAIL-ADDRESS.
      * this code looks up LDAP attributes for a network user; but,
      * when you don't know the network user id, use the IESLDUM file
      * to translate a vse user id back into a network user id
           INITIALIZE IESLDGA-COMMAREA.
           MOVE LENGTH OF IESLDGA-COMMAREA
                                       TO LDGA-AREA-LENGTH.
           MOVE MDST-IDNT              TO LDGA-USER-ID.
           MOVE '&(objectClass=person)(objectClass=user)'
                                       TO LDGA-SEARCH-FILTER.
           MOVE 2                      TO LDGA-ATTR-COUNT.

           MOVE 'displayName'          TO LDGA-ATTR-NAME(1).
           MOVE LENGTH OF LDGA-ATTR-VALUE
                                       TO LDGA-VALUE-LENGTH(1).
           MOVE 8                      TO LDGA-VALUE-COUNT(1).

           MOVE 'mail'                 TO LDGA-ATTR-NAME(2).
           MOVE LENGTH OF LDGA-ATTR-VALUE
                                       TO LDGA-VALUE-LENGTH(2).
           MOVE 8                      TO LDGA-VALUE-COUNT(2).

           EXEC CICS LINK
                     PROGRAM  (IESLDGAC)
                     COMMAREA (IESLDGA-COMMAREA)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO P45-EXIT
           END-IF.

           IF  LDGA-RET-CODE = ZERO
               IF  MDST-NAME <= SPACES
                   MOVE LDGA-ATTR-VALUE(1 1)
                                       TO MDST-NAME
               END-IF
               IF  MDST-ADDR <= SPACES
                   MOVE LDGA-ATTR-VALUE(2 1)
                                       TO MDST-ADDR
               END-IF
           ELSE
               SET  LOG-TO-BOTH        TO TRUE
               MOVE 1                  TO PTR
               INSPECT LDGA-USER-ID       CONVERTING LOWER-CASE
                                                  TO UPPER-CASE
               MOVE LDGA-RET-CODE      TO UNEX-RESP
               MOVE UNEX-RESP          TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               STRING 'UNABLE TO RETRIEVE ' DELIMITED BY SIZE
                      LDGA-USER-ID        DELIMITED BY SPACE
                      ' - RC='            DELIMITED BY SIZE
                      UNEX-VARIABLE       DELIMITED BY SPACE
                      ', RSN='            DELIMITED BY SIZE
                   INTO LOGF-MESG    WITH POINTER PTR
               MOVE LDGA-LDAP-CODE     TO UNEX-RESP
               MOVE UNEX-RESP          TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               STRING UNEX-VARIABLE       DELIMITED BY SPACE
                   INTO LOGF-MESG    WITH POINTER PTR
               PERFORM Q100-LOGIT    THRU Q199-EXIT
           END-IF.
       P45-EXIT.
           EXIT.

       P50-FLAG-INCOMPLETE.
           PERFORM Q26-READUP-MAILMID.
           IF  EIBRESP = DFHRESP(NORMAL)
               SET  MMID-NOT-COMPLETE  TO TRUE
               PERFORM Q27-REWRITE-MAILMID
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               SET  LOG-TO-BOTH        TO TRUE
               PERFORM Q200-UNEX-LOG
           END-IF.
       P55-EXIT.
           EXIT.

       P60-SET-TEXT-LENGTH.
           IF  VAR-TEXT <= SPACES
             MOVE ZEROES               TO VAR-TEXTL
           ELSE
             PERFORM WITH TEST BEFORE
               VARYING VAR-TEXTL FROM VAR-TEXTL BY -1
                 UNTIL VAR-TEXTL < 1
                    OR VAR-TEXT(VAR-TEXTL:1) > SPACE
             END-PERFORM
           END-IF.
       P65-EXIT.
           EXIT.

       P70-SORT-TS-QUEUE.
           IF TS-TOTL <= 1
               GO TO P75-EXIT
           END-IF.

      * setup to call CICSSORT
           MOVE TS-QUEUE               TO CICSSORT-QNAME.
           MOVE TS-TOTL                TO CICSSORT-RECCNT.
           MOVE LENGTH OF TS-RECD      TO CICSSORT-RECLEN.
           MOVE 0                      TO CICSSORT-KEYOFF
           MOVE 30                     TO CICSSORT-KEYLEN
           SET CICSSORT-ASCENDING      TO TRUE.

           EXEC CICS LINK
                     PROGRAM  (CICSSORT)
                     COMMAREA (CICSSORT-PARMS)
           END-EXEC.

      * check if sort completed
           IF  CICSSORT-RC NOT = ZEROES
               SET  LOG-TO-BOTH        TO TRUE
               MOVE CICSSORT-RC        TO NUM-WORK
               STRING 'DISTRIBUTION SORT FAILURE, RC=' NUM-WRK2 '.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT    THRU Q199-EXIT
               GO TO P75-EXIT
           END-IF.

      * delete the old queue
           PERFORM Q65-DELETEQ-TS.

      * save new queue name
           MOVE CICSSORT-QNAME         TO TS-QUEUE.
           PERFORM Q61-NUMITEMS-TS.
       P75-EXIT.
           EXIT.

       P80-CHECK-FROM-PERSON.
           SET  FROM-A-PROGRAM         TO TRUE.
           MOVE SPACES                 TO FROM-ID
                                          FROM-NAME
                                          FROM-ADDR.

           MOVE MMID-FROM              TO UNEX-VARIABLE.
           PERFORM Q202-SET-LENGTH.
           IF  UNEX-LENG > 8
               GO TO P85-EXIT
           END-IF.

           MOVE MMID-FROM              TO FROM-ID.
           INSPECT FROM-ID                CONVERTING LOWER-CASE
                                                  TO UPPER-CASE.
           PERFORM Q70-INITKEY-IESCNTL.
           STRING FROM-ID                 DELIMITED BY SPACE
                                     INTO IUI-US-USRIDNT.
           PERFORM Q76-READEQ-IESCNTL.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
               GO TO P85-EXIT
           END-IF.
           SET  FROM-A-PERSON          TO TRUE.

           PERFORM Q80-INITKEY-IESLDUM.
           MOVE FROM-ID                TO LDUM-MFUSRID.
           PERFORM Q86-READEQ-IESLDUV.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
               GO TO P85-EXIT
           END-IF.

           MOVE LDUM-NETUSRID          TO MDST-IDNT.
           MOVE SPACES                 TO MDST-NAME
                                          MDST-ADDR.
           PERFORM P40-GET-MAIL-ADDRESS THRU P45-EXIT.
           IF  MDST-ADDR NOT > SPACES
               GO TO P85-EXIT
           END-IF.

           MOVE MDST-NAME              TO FROM-NAME.
           MOVE MDST-ADDR              TO FROM-ADDR.
       P85-EXIT.
           EXIT.

       P90-SERVER-CONTROL.
           MOVE +1                     TO POS.
           MOVE SPACES                 TO UNEX-MSG.

           EXEC CICS INQUIRE
                     REQID(THIS-PGM)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
               IF  INPUT-AREA(IDX:5)     = 'START'
               OR  INPUT-AREA(IDX:4)     = 'STRT'
               OR  INPUT-AREA(IDX + 1:4) = 'STRT'
               OR  INPUT-AREA(IDX + 1:5) = 'START'
                   MOVE +1             TO EVOKE-TIME
                   GO TO C10-MAIL-RESTART
               END-IF
               STRING 'MAIL SERVER IS STOPPED.'
                   DELIMITED BY SIZE INTO UNEX-MSG
                                     WITH POINTER POS
           ELSE
               IF  INPUT-AREA(IDX:5)     = 'START'
               OR  INPUT-AREA(IDX:4)     = 'STRT'
               OR  INPUT-AREA(IDX + 1:4) = 'STRT'
               OR  INPUT-AREA(IDX + 1:5) = 'START'
                 STRING 'MAIL SERVER IS RUNNING.'
                   DELIMITED BY SIZE INTO UNEX-MSG
                                     WITH POINTER POS
               ELSE
                 EXEC CICS CANCEL
                           REQID(THIS-PGM)
                 END-EXEC
                 STRING 'MAIL SERVER STOPPED.'
                   DELIMITED BY SIZE INTO UNEX-MSG
                                     WITH POINTER POS
                 MOVE UNEX-MSG(1:POS) TO LOGF-MESG
                 PERFORM Q100-LOGIT THRU Q199-EXIT
               END-IF
           END-IF.

           SUBTRACT 1     FROM POS GIVING UNEX-MSGL.
           GO TO X50-SEND-MSG.
       P95-EXIT.
           EXIT.

      /*****************************************************************
      *    MORE PERFORMED ROUTINES                                     *
      ******************************************************************

       Q10-SEND-MAIL-HEADER.
      * mail header cards can be continued up to three times
           SUBTRACT 1                FROM VAR-TEXTL.
           MOVE 1                      TO POS.
           PERFORM WITH TEST BEFORE VARYING CNT FROM 1 BY 1
             UNTIL CNT > 3 OR VAR-TEXTL <= 80
                OR NOT PRINT-COMPLETED
                OR EIBRESP NOT = DFHRESP(NORMAL)
             MOVE VAR-TEXT(POS:79)     TO PRT-STR-DATA
             MOVE '+'                  TO PRT-STR-DATA(80:1)
             PERFORM P00-CALL-PRTMAN   THRU P05-EXIT
             SUBTRACT 79             FROM VAR-TEXTL
             ADD  79                   TO POS
           END-PERFORM.
           MOVE VAR-TEXT(POS:)         TO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           MOVE ZEROES                 TO VAR-TEXTL.
           MOVE SPACES                 TO VAR-TEXT.
       Q15-EXIT.
           EXIT.

       Q20-INITKEY-MAILMID.
           IF  ADDRESS OF MAILMID-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF MAILMID-RECORD)
                         LENGTH   (LENGTH OF MAILMID-RECORD)
               END-EXEC
           END-IF.
           MOVE 1                      TO MMID-ID.
       Q21-STARTBR-MAILMID.
           EXEC CICS STARTBR
                     DATASET  (MAILMID)
                     RIDFLD   (MMID-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q23-READNEXT-MAILMID.
           MOVE LENGTH OF MAILMID-RECORD TO MMID-RECL.
           EXEC CICS READNEXT
                     DATASET  (MAILMID)
                     INTO     (MAILMID-RECORD)
                     LENGTH   (MMID-RECL)
                     RIDFLD   (MMID-KEY)
                     NOHANDLE
           END-EXEC.
       Q25-ENDBR-MAILMID.
           EXEC CICS ENDBR
                     DATASET  (MAILMID)
                     NOHANDLE
           END-EXEC.
       Q26-READUP-MAILMID.
           MOVE LENGTH OF MAILMID-RECORD TO MMID-RECL.
           EXEC CICS READ
                     DATASET  (MAILMID)
                     INTO     (MAILMID-RECORD)
                     LENGTH   (MMID-RECL)
                     RIDFLD   (MMID-KEY)
                     EQUAL
                     UPDATE
                     NOHANDLE
           END-EXEC.
       Q27-REWRITE-MAILMID.
           COMPUTE MMID-RECL = LENGTH OF MMID-KEY
                             + LENGTH OF MMID-HDR.
           EXEC CICS REWRITE
                     DATASET  (MAILMID)
                     FROM     (MAILMID-RECORD)
                     LENGTH   (MMID-RECL)
                     NOHANDLE
           END-EXEC.
       Q29-EXIT.
           EXIT.

       Q30-INITKEY-MAILMSG.
           IF  ADDRESS OF MAILMSG-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF MAILMSG-RECORD)
                         LENGTH   (LENGTH OF MAILMSG-RECORD)
               END-EXEC
           END-IF.
           MOVE ZEROES                 TO MMSG-ID
                                          MMSG-SEQ.
       Q31-STARTBR-MAILMSG.
           EXEC CICS STARTBR
                     DATASET  (MAILMSG)
                     RIDFLD   (MMSG-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q33-READNEXT-MAILMSG.
           MOVE LENGTH OF MAILMSG-RECORD TO MMSG-RECL.
           EXEC CICS READNEXT
                     DATASET  (MAILMSG)
                     INTO     (MAILMSG-RECORD)
                     LENGTH   (MMSG-RECL)
                     RIDFLD   (MMSG-KEY)
                     NOHANDLE
           END-EXEC.
       Q35-ENDBR-MAILMSG.
           EXEC CICS ENDBR
                     DATASET  (MAILMSG)
                     NOHANDLE
           END-EXEC.
       Q39-EXIT.
           EXIT.

       Q40-INITKEY-MAILDST.
           IF  ADDRESS OF MAILDST-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF MAILDST-RECORD)
                         LENGTH   (LENGTH OF MAILDST-RECORD)
               END-EXEC
           END-IF.
           MOVE SPACES                 TO MDST-KEY.
           SET  MDST-GRP-HEADER        TO TRUE.
       Q41-STARTBR-MAILDST.
           EXEC CICS STARTBR
                     DATASET  (MAILDST)
                     RIDFLD   (MDST-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q43-READNEXT-MAILDST.
           MOVE LENGTH OF MAILDST-RECORD TO MDST-RECL.
           EXEC CICS READNEXT
                     DATASET  (MAILDST)
                     INTO     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     RIDFLD   (MDST-KEY)
                     NOHANDLE
           END-EXEC.
       Q45-ENDBR-MAILDST.
           EXEC CICS ENDBR
                     DATASET  (MAILDST)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(INVREQ)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       Q46-READUP-MAILDST.
           MOVE LENGTH OF MAILDST-RECORD TO MDST-RECL.
           EXEC CICS READ
                     DATASET  (MAILDST)
                     INTO     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     RIDFLD   (MDST-KEY)
                     EQUAL
                     UPDATE
                     NOHANDLE
           END-EXEC.
       Q47-REWRITE-MAILDST.
           COMPUTE MDST-RECL = LENGTH OF MDST-KEY
                             + LENGTH OF MDST-HEADER.
           EXEC CICS REWRITE
                     DATASET  (MAILDST)
                     FROM     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     NOHANDLE
           END-EXEC.
       Q49-EXIT.
           EXIT.

       Q50-INITKEY-MAILMDS.
           IF  ADDRESS OF MAILMDS-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF MAILMDS-RECORD)
                         LENGTH   (LENGTH OF MAILMDS-RECORD)
               END-EXEC
           END-IF.
           MOVE ZEROES                 TO MMDS-ID
                                          MMDS-SEQ.
       Q51-STARTBR-MAILMDS.
           EXEC CICS STARTBR
                     DATASET  (MAILMDS)
                     RIDFLD   (MMDS-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q53-READNEXT-MAILMDS.
           EXEC CICS READNEXT
                     DATASET  (MAILMDS)
                     INTO     (MAILMDS-RECORD)
                     RIDFLD   (MMDS-KEY)
                     NOHANDLE
           END-EXEC.
       Q55-ENDBR-MAILMDS.
           EXEC CICS ENDBR
                     DATASET  (MAILMDS)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(INVREQ)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       Q59-EXIT.
           EXIT.

       Q61-NUMITEMS-TS.
           MOVE 1                      TO TS-ITEM.
           EXEC CICS READQ TS
                     QUEUE   (TS-QUEUE)
                     ITEM    (TS-ITEM)
                     INTO    (TS-RECD)
                     NUMITEMS(TS-TOTL)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(QIDERR)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
               MOVE ZEROES             TO TS-TOTL
           END-IF.
       Q62-READQ-TS.
           EXEC CICS READQ TS
                     QUEUE   (TS-QUEUE)
                     INTO    (TS-RECD)
                     ITEM    (TS-ITEM)
                     NOHANDLE
           END-EXEC.
       Q63-WRITEQ-TS.
           EXEC CICS WRITEQ TS
                     QUEUE   (TS-QUEUE)
                     FROM    (TS-RECD)
                     MAIN
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(NORMAL)
               MOVE SPACES             TO TS-RECD
           END-IF.
       Q65-DELETEQ-TS.
           MOVE ZEROES                 TO TS-ITEM.
           IF  TS-QUEUE <= SPACES
               MOVE THIS-TRN           TO TS-TRAN
               MOVE EIBTRMID           TO TS-TERM
           END-IF.
           EXEC CICS DELETEQ TS
                     QUEUE   (TS-QUEUE)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(QIDERR)
               MOVE DFHRESP(NORMAL) TO EIBRESP
           END-IF.
           IF  EIBRESP = DFHRESP(NORMAL)
               MOVE SPACES             TO TS-RECD
           END-IF.
       Q65-EXIT.
           EXIT.

       Q70-INITKEY-IESCNTL.
           IF  ADDRESS OF IESCNTL-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF IESCNTL-RECORD)
                         LENGTH   (LENGTH OF IESCNTL-RECORD)
               END-EXEC
               SET ADDRESS OF IUI-US-RECORD
                                       TO ADDRESS OF IESCNTL-RECORD
           END-IF.
           MOVE LOW-VALUES             TO IUI-KEY.
           SET  IUI-USER-PROFILE       TO TRUE.
       Q76-READEQ-IESCNTL.
           MOVE LENGTH OF IESCNTL-RECORD TO CNTL-RECL.
           EXEC CICS READ
                     DATASET  (IESCNTL)
                     INTO     (IUI-US-RECORD)
                     LENGTH   (CNTL-RECL)
                     RIDFLD   (IUI-US-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q79-EXIT.
           EXIT.

       Q80-INITKEY-IESLDUM.
           IF  ADDRESS OF IESLDUM-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF IESLDUM-RECORD)
                         LENGTH   (LENGTH OF IESLDUM-RECORD)
               END-EXEC
           END-IF.
           MOVE LOW-VALUES             TO LDUM-KEY.
           SET  LDUM-USRMAP-RECORD     TO TRUE.
       Q86-READEQ-IESLDUV.
           EXEC CICS READ
                     DATASET  (IESLDUV)
                     INTO     (IESLDUM-RECORD)
                     RIDFLD   (LDUM-MFUSRID)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q89-EXIT.
           EXIT.

      /*****************************************************************
      *    DAPL OR DTLG MESSAGE LOGGING ROUTINE                        *
      ******************************************************************
       COPY LOGGINGP.

      /*****************************************************************
      *    PROGRAM ERRORS (UNEXPECTED)                                 *
      ******************************************************************
       COPY UNEXERRP.

      /*****************************************************************
      *    PROGRAM TERMINATION (NORMAL)                                *
      ******************************************************************
       Z00-MAIL-TERMINATION.

           EXEC CICS SYNCPOINT NOHANDLE END-EXEC.
           EXEC CICS RETURN END-EXEC.
           GOBACK.
