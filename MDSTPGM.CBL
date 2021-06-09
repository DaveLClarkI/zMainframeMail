 CBL XOPTS(SP)
      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    MDSTPGM.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  MAY 2019.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      TRANSACTION SECURITY, ONLY.
      *REMARKS.       MF MAIL DISTRIBUTION GROUP MAINTENANCE.

      * CHANGE HISTORY ------------------------------------------------
      * 05/20/2019 DLC ORIGINAL PROGRAM.
/DLC0/* 12/23/2019 DLC REPLACE VOPERID FILE WITH VUSERID, INSTEAD.
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
       OBJECT-COMPUTER. IBM-2096-N03.
       SPECIAL-NAMES.
           CLASS ALPHAMERIC-UNDERSCORE IS ' ', '_',
                                          'A' THRU 'I',
                                          'J' THRU 'R',
                                          'S' THRU 'Z',
                                          '0' THRU '9',
           CLASS ALPHAMERIC-AND-PERIOD IS ' ', '&', '.', '-', '''',
                                          'a' THRU 'i',
                                          'j' THRU 'r',
                                          's' THRU 'z',
                                          'A' THRU 'I',
                                          'J' THRU 'R',
                                          'S' THRU 'Z',
                                          '0' THRU '9',
           CLASS NUMERIC-FILTER-CHARS IS ' ', '?', '*', '<', '>',
                                         '0' THRU '9'.

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
         COPY COMMWORK       REPLACING =='TRAN'==    BY =='MDST'==
                                       =='PROGRAM'== BY =='MDSTPGM'==
                                       =='LINKPGM'== BY =='VUSRPGM'==.
         03  MDSTMS                    PIC  X(08)   VALUE 'MDSTMS  '.
         03  LISTMAP                   PIC  X(08)   VALUE 'LISTMAP '.
         03  DETLMAP                   PIC  X(08)   VALUE 'DETLMAP '.
         03  WAITWIN                   PIC  X(08)   VALUE 'WAITWIN '.
      *
      * FIELDS TO MANAGE REFERENCED FILE NAMES
         03  MAILDST                   PIC  X(08)   VALUE 'MAILDST '.
/DLC0/*  03  VOPERPU                   PIC  X(08)   VALUE 'VOPERPU '.
/DLC0/*  03  VOPERID                   PIC  X(08)   VALUE 'VOPERID '.
/DLC0/   03  VUSERID                   PIC  X(08)   VALUE 'VUSERID '.
         03  IESLDUM                   PIC  X(08)   VALUE 'IESLDUM '.
         03  IESLDUV                   PIC  X(08)   VALUE 'IESLDUV '.
      *
      * FIELDS TO MANAGE REFERENCED EXTERNAL PROGRAM NAMES
         03  IESLDGAC                  PIC  X(08)   VALUE 'IESLDGAC'.
         03  JOBSSELT                  PIC  X(08)   VALUE 'JOBSSELT'.
         03  MMSGPGM                   PIC  X(08)   VALUE 'MMSGPGM'.
         03  MULTMESG                  PIC  X(08)   VALUE 'MULTMESG'.
      *
      * FIELDS TO MANAGE ERROR HANDLING
         03  ERROR-SWITCH              PIC S9(01)   VALUE ZEROES.
           88  NO-ERRORS-FOUND                      VALUE ZEROES.
           88  ERROR-AT-CURSOR                      VALUE -1.
           88  ERRORS-FOUND                         VALUES -9 THRU -1.
         03  RECORD-SWITCH             PIC  X(01)   VALUE 'N'.
           88  RECORD-NOT-SELECTED                  VALUE 'N'.
           88  RECORD-IS-SELECTED                   VALUE 'Y'.
         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.
         03  CNT                       PIC S9(04)   BINARY VALUE ZEROES.
         03  LEN                       PIC S9(04)   BINARY VALUE ZEROES.
         03  POS                       PIC S9(04)   BINARY VALUE ZEROES.
         03  PTR                       PIC S9(04)   BINARY VALUE ZEROES.
         03  SUB                       PIC S9(04)   BINARY VALUE ZEROES.
         03  JCNT                      PIC S9(04)   BINARY VALUE ZEROES.
         03  JSUB                      PIC S9(04)   BINARY VALUE ZEROES.
         03  MDST-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
      *
      * FIELDS TO CALCULATE CURSOR POSITION AND MANAGE MAPS W/ OCCURS
         03  ROW                       PIC S9(04)   BINARY VALUE ZEROES.
         03  COL                       PIC S9(04)   BINARY VALUE ZEROES.
         03  LISTHEAD                  PIC S9(04)   BINARY VALUE +06.
         03  LISTROWS                  PIC S9(04)   BINARY VALUE +18.
         03  DETLHEAD                  PIC S9(04)   BINARY VALUE +06.
         03  DETLROWS                  PIC S9(04)   BINARY VALUE +08.
         03  DETLDEEP                  PIC S9(04)   BINARY VALUE +02.
      *
         03  nbsp                      PIC  X(01)   value x'41'.
         03  EDIT-NUMBER               PIC  Z(8)9-.
         03  EDIT-NUMBERC              PIC  ZZZ,ZZZ,ZZZ-.
         03  POPUP-TEXT                PIC  X(15)   VALUE SPACES.
         03  LOWER-CASE  PIC  X(26)  VALUE 'abcdefghijklmnopqrstuvwxyz'.
         03  UPPER-CASE  PIC  X(26)  VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
         03  MULTI-EDIT-MSG            PIC  X(78)   VALUE SPACES.
         03  VAR-DATA                  PIC  X(98)   VALUE SPACES.
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
         03  TESTMAIL                  PIC  X(08)   VALUE 'TESTMAIL'.
       01  MAIL-JCL.
         03  PIC X(40) VALUE '* $$ JOB JNM=TESTMAIL,DISP=D,CLASS=U    '.
         03  PIC X(40) VALUE '* $$ LST DISP=D,CLASS=Z,PURGE=0         '.
         03  PIC X(40) VALUE '// JOB    TESTMAIL   Test Distribution  '.
         03  PIC X(40) VALUE '// LIBDEF *,SEARCH=(PRD2.CONFIG,ESP.BSI)'.
         03  PIC X(40) VALUE '* $$ SLI SYSCODE                        '.
         03  PIC X(40) VALUE '// OPTION SYSPARM=''00''                '.
         03  PIC X(40) VALUE '// EXEC REXX=BSISMTPC,SIZE=196K,SYSCODE '.
         03  PIC X(40) VALUE 'ID 00                                   '.
         03  PIC X(40) VALUE 'OPEN INTERNET.WINWHOLESALE.COM          '.
         03  PIC X(40) VALUE 'HELO LIBRA<SYSCODE>.WINWHOLESALE.COM    '.
         03  PIC X(40) VALUE 'MAIL From: LIBRA<SYSCODE> Mainframe     '.
         03  PIC X(40) VALUE '+++HEAD+++                              '.
         03  PIC X(40) VALUE 'SUBJ Subject: This is a Test            '.
         03  PIC X(40) VALUE 'DATA                                    '.
         03  PIC X(40) VALUE 'Run date: %D$T% %T$C% <br>              '.
         03  PIC X(40) VALUE 'This is just a test email to make sure  '.
         03  PIC X(40) VALUE 'the associated mail distribution group  '.
         03  PIC X(40) VALUE 'is set up correctly.  You may disregard '.
         03  PIC X(40) VALUE 'this email.                             '.
         03  PIC X(40) VALUE '+++TAIL+++                              '.
         03  PIC X(40) VALUE 'QUIT                                    '.
         03  PIC X(40) VALUE '/* EOD                                  '.
         03  PIC X(40) VALUE '/& EOJ                                  '.
         03  PIC X(40) VALUE '* $$ EOJ                                '.
       01  MAIL-TABLE                  REDEFINES    MAIL-JCL.
         03  MAIL-ENTRY                PIC  X(40)   OCCURS 24.

      * IBM LDAP GET ATTRIBUTE INTERFACE
       COPY IESLDGAC.

      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS

       01  CICSINFO-PARMS.
         COPY CICSINFO.

       COPY CICSSORT.

       COPY DTEMAN.

       COPY HEXMAN.

       COPY LOGGING.

       COPY NUMMAN.

       COPY PRTMAN.

       COPY TXTMAN.

       COPY UNEXERRW.
           05  CONFIRM-MSG   REDEFINES UNEX-MSG     PIC  X(79).

       COPY WILDCOMP.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

      * MDST CICS COMMUNICATION AREA
       01  DFHCOMMAREA.
         COPY COMMAREA.
      *
           05  MDST-SAVEKEY            PIC  X(25).
           05  SAVE-FILTERS.
             07  SAVE-GRPDESC-FILTER   PIC  X(50).
             07  SAVE-USECOUNT-FILTER  PIC  X(12).
             07  SAVE-LASTUSED-FILTER  PIC  X(10).
             07  SAVE-LASTUSEBY-FILTER PIC  X(08).
             07  SAVE-UPDATEDBY-FILTER PIC  X(08).
           05  MDST-PROCESS            PIC  X(01).
             88  VIEW-PROCESS                       VALUE 'V'.
             88  ADD-PROCESS                        VALUE 'A'.
             88  CHG-PROCESS                        VALUE 'C'.

           05  TS-QUEUE.
             07  TS-TRAN               PIC  X(4).
             07  TS-TERM               PIC  X(4).
           05  TS-TOTL                 PIC S9(4)    BINARY.
           05  TS-ITEM                 PIC S9(4)    BINARY.
           05  SS-ITEM                 PIC S9(4)    BINARY.
           05  TS-RECD.
             07  TS-TYPE               PIC  X(1).
             07  TS-IDNT               PIC  X(64).
             07  TS-NAME               PIC  X(64).
             07  TS-NAME-OVERRIDE      PIC  X(1).
             07  TS-ADDR               PIC  X(64).
             07  TS-ADDR-OVERRIDE      PIC  X(1).
             07  TS-SELECT             PIC  X(1).

           05  MDST-LIST-TIOA          PIC  X(3008).
           05  MDST-SAVE-TIOA          PIC  X(2282).

      * MDST TEMP COMMUNICATION AREA
       COPY COMMTEMP.

      * 3270 DATA STREAM CONTROL CHARACTERS
       COPY IBM3270.

      * MAILDST RECORD I/O AREA
       COPY MAILDST.

      * VOPERID RECORD I/O AREA
/DLC0/*COPY VOPERID.

/DLC0/* VUSERID RECORD I/O AREA
/DLC0/ COPY VUSERID.

      * IESLDUM RECORD I/O AREA
       COPY IESLDUM.

      * BMS TERMINAL I/O AREAS
       COPY MDSTMS.

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *    PROGRAM INITIALIZATION                                      *
      ******************************************************************
       A00-MDST-PROGRAM-ENTRY.

           COPY COMMENTR.

           EXEC CICS LINK
                     PROGRAM  ('CICSINFO')
                     COMMAREA (CICSINFO-PARMS)
           END-EXEC.
           MOVE CICS-FULLDATE          TO WS-WRKDATE.
           MOVE CICS-FULLTIME          TO WS-WRKTIME.
           PERFORM Q02-REFORMAT-TIME THRU Q05-EXIT.

           COPY COMMMAPT.

           COPY COMM3270.

           SET  DAPL-LOGGING           TO TRUE.

           EXEC CICS HANDLE CONDITION
                     DISABLED (Z70-MDST-CLOSED)
                     NOTOPEN  (Z70-MDST-CLOSED)
           END-EXEC.

           IF  COMM-CURRPGM = THIS-PGM
      * psuedo-conversational program continuation
               GO TO B50-MDST-ROUTINE-SELECTION
           END-IF.

       B00-MDST-FIRST-TIME-ONLY.

           COPY COMMINIT.

           IF  COMM-CURRRTN = 'RETN'
           AND COMM-SAVE-FUNC > SPACES
      * return from 2nd-level program (such as the HELP system)
               GO TO B50-MDST-ROUTINE-SELECTION
           END-IF.

      * the following executes one-time-only per user session

           IF  COMM-CURRRTN NOT = 'RETN'
                              AND 'EXIT'
               MOVE SPACES             TO MDST-SAVEKEY
           END-IF.

      * set default initial filter value
           MOVE '*'                    TO SAVE-GRPDESC-FILTER
                                          SAVE-USECOUNT-FILTER
                                          SAVE-LASTUSED-FILTER
                                          SAVE-LASTUSEBY-FILTER
                                          SAVE-UPDATEDBY-FILTER.

      * this program requires mixed-case terminal input
           EXEC CICS SET TERMINAL(EIBTRMID)
                     TRANIDONLY
                     NOHANDLE
           END-EXEC.

       B50-MDST-ROUTINE-SELECTION.

           IF (COMM-CURRRTN = 'RETN' OR 'EXIT')
           AND COMM-SAVE-FUNC > SPACES
               MOVE COMM-CURRRTN       TO INIT-FLAG
               MOVE COMM-SAVE-FUNC     TO COMM-CURRRTN
               MOVE INIT-FLAG          TO COMM-SAVE-FUNC
           END-IF.

           EVALUATE COMM-CURRRTN
           WHEN 'DETL' GO TO D00-DETL-ROUTINE
           WHEN 'LIST' GO TO C00-LIST-ROUTINE
           END-EVALUATE.

      /*****************************************************************
      *    LIST ROUTINE                                                *
      ******************************************************************
       C00-LIST-ROUTINE.

           IF  ADDRESS OF LISTMAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF LISTMAPI)
                         LENGTH   (LENGTH OF LISTMAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
           END-IF.

           IF  COMM-CURRRTN NOT = 'LIST'
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-CURRRTN = 'RETN'
                   MOVE MDST-LIST-TIOA TO LISTMAPO
                   MOVE LOW-VALUES     TO MDST-LIST-TIOA
               END-IF
               PERFORM C80-LIST-OUTPUT THRU C85-LIST-SENDMAP
               GO TO C50-LIST-DISPLAY
           END-IF.

       C10-LIST-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (LISTMAP)
                     INTO     (LISTMAPI)
                     MAPSET   (MDSTMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO C70-LIST-ERRORS
           END-IF.

       C20-LIST-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
               MOVE JOBSSELT           TO COMM-LINKPGM
               GO TO Z90-MDST-TERMINATION

           WHEN AIDPFK03
               GO TO Z90-MDST-TERMINATION

           WHEN AIDPFK01
               MOVE LISTMAPI           TO MDST-LIST-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDENTER
           WHEN AIDPFK06
           WHEN AIDPFK07
           WHEN AIDPFK08
           WHEN AIDPFK09
           WHEN AIDPFK10
               CONTINUE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO LIST-MESSAGE-O
               GO TO C80-LIST-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO LIST-MESSAGE-O
               GO TO C80-LIST-OUTPUT
           END-EVALUATE.

       C30-LIST-PROCESS.

           IF  EIBAID = AIDPFK10
               MOVE SPACES             TO MDST-SAVEKEY
               MOVE 'EXIT'             TO COMM-TRANSID
               MOVE MMSGPGM            TO COMM-LINKPGM
               MOVE LENGTH OF COMM-HEADER
                                       TO LINK-LEN
               GO TO T02-LINK-CONTINUE
           END-IF.

      * convert cursor position to row and column coordinates
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.
      *

           INSPECT LIST-GRPNAME-SKIPTO-I CONVERTING LOWER-CASE
                                                 TO UPPER-CASE
           INSPECT LIST-GRPDESC-FILTER-I CONVERTING LOWER-CASE
                                                 TO UPPER-CASE
           INSPECT LIST-LASTUSEBY-FILTER-I CONVERTING LOWER-CASE
                                                   TO UPPER-CASE
           INSPECT LIST-UPDATEDBY-FILTER-I CONVERTING LOWER-CASE
                                                   TO UPPER-CASE

           IF  LIST-GRPNAME-SKIPTO-L   > ZERO
           OR  LIST-GRPNAME-SKIPTO-F   = FLDMODFY OR FLDCURSM
           OR  LIST-GRPDESC-FILTER-L   > ZERO
           OR  LIST-GRPDESC-FILTER-F   = FLDMODFY OR FLDCURSM
           OR  LIST-USECOUNT-FILTER-L  > ZERO
           OR  LIST-USECOUNT-FILTER-F  = FLDMODFY OR FLDCURSM
           OR  LIST-LASTUSED-FILTER-L  > ZERO
           OR  LIST-LASTUSED-FILTER-F  = FLDMODFY OR FLDCURSM
           OR  LIST-LASTUSEBY-FILTER-L > ZERO
           OR  LIST-LASTUSEBY-FILTER-F = FLDMODFY OR FLDCURSM
           OR  LIST-UPDATEDBY-FILTER-L > ZERO
           OR  LIST-UPDATEDBY-FILTER-F = FLDMODFY OR FLDCURSM
               GO TO C50-LIST-DISPLAY
           END-IF.

      * scroll to top or bottom of the list
           IF  EIBAID = AIDPFK06 OR AIDPFK09
               PERFORM Q20-INITKEY-MAILDST
               IF  EIBAID = AIDPFK06
                   PERFORM Q21-STARTBR-MAILDST
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No distribution groups found.'
                                       TO LIST-MESSAGE-O
                         GO TO C50-LIST-DISPLAY
                       END-IF
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q23-READNX-DST-HDR THRU Q23-CHK-FILTER
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  MDST-GRP-NAME = LIST-GRPNAME-I(1)
                   OR  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first page.' TO LIST-MESSAGE-O
                   END-IF
               ELSE
                   MOVE HIGH-VALUES    TO MDST-KEY
                   PERFORM Q21-STARTEQ-MAILDST
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No distribution groups found.'
                                       TO LIST-MESSAGE-O
                         GO TO C50-LIST-DISPLAY
                       END-IF
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q24-READPR-DST-HDR THRU Q24-CHK-FILTER
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  MDST-GRP-NAME = LIST-GRPNAME-I(1)
                   OR  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the last page.' TO LIST-MESSAGE-O
                   END-IF
               END-IF
               MOVE MDST-GRP-NAME      TO LIST-GRPNAME-O(1)
               PERFORM Q25-ENDBR-MAILDST
               GO TO C50-LIST-DISPLAY
           END-IF.

      * adjust row and column coordinates for header/detail screens
           SUBTRACT LISTHEAD         FROM ROW.
      *    COMPUTE COL = (COL + ((SCRNWDTH - LISTCOLS) / LISTCOLS))
      *                       / ((SCRNWDTH - LISTCOLS) / LISTCOLS).

      * scroll to previous or next page
           IF  EIBAID = AIDPFK07 OR AIDPFK08
               PERFORM Q20-INITKEY-MAILDST
               MOVE LIST-GRPNAME-I(1) TO MDST-GRP-NAME
               IF  EIBAID = AIDPFK07
                   IF  ROW < 1 OR >= LISTROWS
                   OR  LIST-GRPNAME-I(ROW) <= SPACES
                       COMPUTE IDX = LISTROWS
                   ELSE
                       COMPUTE IDX = LISTROWS - ROW
                   END-IF
                   PERFORM Q21-STARTBR-MAILDST
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No distribution groups found.'
                                       TO LIST-MESSAGE-O
                         GO TO C50-LIST-DISPLAY
                       END-IF
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q23-READNX-DST-HDR
                   PERFORM Q24-READPR-DST-HDR THRU Q24-CHK-FILTER
                     VARYING IDX FROM IDX BY -1
                       UNTIL IDX < ZERO
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first entry.' TO LIST-MESSAGE-O
                       PERFORM Q23-READNX-DST-HDR THRU Q23-CHK-FILTER
                       IF  EIBRESP NOT = DFHRESP(NORMAL)
                                     AND DFHRESP(ENDFILE)
                           GO TO C70-LIST-ERRORS
                       END-IF
                   END-IF
               ELSE
                   IF  ROW <= 1 OR > LISTROWS
                   OR  LIST-GRPNAME-I(ROW) <= SPACES
                       COMPUTE IDX = LISTROWS
                   ELSE
                       COMPUTE IDX = ROW - 1
                   END-IF
                   PERFORM Q21-STARTBR-MAILDST
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No distribution groups found.'
                                       TO LIST-MESSAGE-O
                         GO TO C50-LIST-DISPLAY
                       END-IF
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q23-READNX-DST-HDR THRU Q23-CHK-FILTER
                     VARYING IDX FROM IDX BY -1
                       UNTIL IDX < ZERO
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the last entry.' TO LIST-MESSAGE-O
                       PERFORM Q24-READPR-DST-HDR THRU Q24-CHK-FILTER
                       IF  EIBRESP NOT = DFHRESP(NORMAL)
                                     AND DFHRESP(ENDFILE)
                           GO TO C70-LIST-ERRORS
                       END-IF
                   END-IF
               END-IF
               MOVE MDST-GRP-NAME      TO LIST-GRPNAME-O(1)
               PERFORM Q25-ENDBR-MAILDST
               GO TO C50-LIST-DISPLAY
           END-IF.

      * check if default selection needs to be made
           IF  ROW >= 1 AND <= LISTROWS
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > LISTROWS
                      OR LIST-SELECT-I(IDX) > SPACE
               END-PERFORM
               IF  IDX > LISTROWS
                   MOVE 'V'            TO LIST-SELECT-O(ROW)
               END-IF
           END-IF.

      * PERFORM DETAIL FIELD EDITING HERE

           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1
               UNTIL IDX > LISTROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

               INSPECT LIST-SELECT-I(IDX) CONVERTING LOWER-CASE
                                                  TO UPPER-CASE
               IF  LIST-SELECT-I(IDX) > SPACE
                   PERFORM C40-LIST-UPDATES THRU C45-EXIT
               END-IF

      * end of detail loop
           END-PERFORM.

           IF  ERRORS-FOUND
               GO TO C80-LIST-OUTPUT
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO C70-LIST-ERRORS
           END-IF.

           GO TO C50-LIST-DISPLAY.

       C40-LIST-UPDATES.

      * PROCESS ANY ALLOWED UPDATES HERE

           EVALUATE LIST-SELECT-I(IDX)
           WHEN '*'
           WHEN 'X'
               GO TO C45-EXIT

           WHEN 'V'
           WHEN 'A'
           WHEN 'C'
               IF  LIST-GRPNAME-I(IDX) NOT > SPACES
                   IF  LIST-SELECT-I(IDX) NOT = 'A'
                     MOVE LOW-VALUES   TO LIST-SELECT-I(IDX)
                   END-IF
                   GO TO C45-EXIT
               END-IF
               GO TO C45-EXIT

           WHEN 'D'
               IF  LIST-GRPNAME-I(IDX) NOT > SPACES
                   MOVE LOW-VALUES     TO LIST-SELECT-I(IDX)
                   GO TO C45-EXIT
               END-IF
      * confirm delete processing
               COMPUTE EIBCPOSN = (LISTHEAD + IDX - 1) * SCRNWDTH + 1
               EXEC CICS SEND CONTROL CURSOR(EIBCPOSN) END-EXEC
               MOVE SPACES             TO MULTI-EDIT-MSG
               MOVE 1                  TO LEN
               STRING 'Delete this group and all '
                      'of its entries?  '
                      'Press F2 to confirm.' DELIMITED BY SIZE
                   INTO MULTI-EDIT-MSG WITH POINTER LEN
               SUBTRACT 1            FROM LEN
               PERFORM V10-VERIFY-DELETE THRU V15-EXIT
               IF  EIBAID  NOT = AIDPFK02
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE LOW-VALUES     TO LIST-SELECT-I(IDX)
                   GO TO C45-EXIT
               END-IF
      * delete distribution group
               PERFORM Q20-INITKEY-MAILDST
               MOVE LIST-GRPNAME-I(IDX) TO MDST-GRP-NAME
               EXEC CICS DELETE
                         DATASET(MAILDST)
                         RIDFLD(MDST-GRP-NAME)
                         KEYLENGTH(LENGTH OF MDST-GRP-NAME)
                         GENERIC
                         NOHANDLE
               END-EXEC
               IF  EIBRESP = DFHRESP(NORMAL)
                   MOVE LOW-VALUES     TO LISTMAP-ROW-DETAIL(IDX)
               END-IF

           WHEN 'T'
               IF  LIST-GRPNAME-I(IDX) NOT > SPACES
                   MOVE LOW-VALUES     TO LIST-SELECT-I(IDX)
                   GO TO C45-EXIT
               END-IF
               MOVE LIST-GRPNAME-I(IDX) TO MDST-SAVEKEY
               PERFORM M00-MAIL-TEST-MESSAGE THRU M95-EXIT

           WHEN OTHER
               MOVE -1                 TO ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
               MOVE EXCRED             TO LIST-SELECT-C(IDX)
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               MOVE 'Unknown option selection.'
                                       TO LIST-MESSAGE-O
               GO TO C45-EXIT
           END-EVALUATE.

           IF  EIBRESP = DFHRESP(NORMAL)
           AND LIST-SELECT-I(IDX) > SPACE
               MOVE '*'                TO LIST-SELECT-O(IDX)
           END-IF.

       C45-EXIT.
           EXIT.

       C50-LIST-DISPLAY.

           MOVE 'LIST'                 TO COMM-CURRRTN.

      * skip if heading selections changed
           IF  LIST-GRPNAME-SKIPTO-L > ZERO
           OR  LIST-GRPNAME-SKIPTO-F = FLDMODFY OR FLDCURSM
               GO TO C60-LIST-DETAIL
           END-IF.

      * CHECK FOR OPTION SELECTIONS

           PERFORM WITH TEST BEFORE
             VARYING ROW FROM 1 BY 1
               UNTIL ROW > LISTROWS
                  OR LIST-SELECT-I(ROW) > SPACE AND NOT = '*'
           END-PERFORM.

           IF  ROW <= LISTROWS
           AND NO-ERRORS-FOUND
               EVALUATE LIST-SELECT-I(ROW)
      * ... transfer key data to internal function ...
               WHEN 'V'
               WHEN 'A'
               WHEN 'C'
                   IF  LIST-GRPNAME-I(ROW) NOT > SPACES
                   AND LIST-SELECT-I(ROW) NOT = 'A'
                       MOVE LOW-VALUES TO LIST-SELECT-O(ROW)
                       GO TO C60-LIST-DETAIL
                   END-IF
                   MOVE LIST-GRPNAME-I(ROW)
                                       TO MDST-SAVEKEY
                   MOVE LIST-SELECT-I(ROW)
                                       TO MDST-PROCESS
                   MOVE '*'            TO LIST-SELECT-O(ROW)
                   MOVE LISTMAPI       TO MDST-LIST-TIOA
                   GO TO D00-DETL-ROUTINE
      * ... return key data to previous program ...
               WHEN 'X'
                   IF  LIST-GRPNAME-I(ROW) NOT > SPACES
                   OR  COMM-LINKPGM NOT > SPACES
                   AND COMM-LINKPGM NOT = 'JOBSRETN'
                       MOVE LOW-VALUES TO LIST-SELECT-O(ROW)
                       GO TO C60-LIST-DETAIL
                   END-IF
                   MOVE 'RETN'         TO COMM-TRANSID
                   MOVE LIST-GRPNAME-I(ROW)
                                       TO MDST-SAVEKEY
                   COMPUTE LINK-LEN = LENGTH OF COMM-HEADER
                                    + LENGTH OF COMM-SHARED
                                    + LENGTH OF MDST-SAVEKEY
                   GO TO T00-LINK-TRANSFER
      *
               WHEN OTHER
                 MOVE     -1           TO ERROR-SWITCH
                                          LIST-SELECT-L(ROW)
                 MOVE  EXHREVRS        TO LIST-MESSAGE-H
                 MOVE 'Unknown option selection.'
                                       TO LIST-MESSAGE-O
               END-EVALUATE
               GO TO C80-LIST-OUTPUT
           END-IF.

       C60-LIST-DETAIL.

           PERFORM Q80-SAVE-FILTERS THRU Q85-EXIT.
           IF  ERRORS-FOUND
               GO TO C80-LIST-OUTPUT
           END-IF.

           PERFORM Q20-INITKEY-MAILDST.
           EVALUATE TRUE
           WHEN LIST-GRPNAME-SKIPTO-L > ZERO
            AND LIST-GRPNAME-SKIPTO-I > SPACES
               MOVE LIST-GRPNAME-SKIPTO-I TO MDST-GRP-NAME
               PERFORM Q21-STARTBR-MAILDST
               IF  EIBRESP = DFHRESP(NOTFND)
                   MOVE HIGH-VALUES    TO MDST-KEY
                   PERFORM Q21-STARTEQ-MAILDST
                   PERFORM Q24-READPR-DST-HDR THRU Q24-CHK-FILTER
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
                   PERFORM Q23-READNX-DST-HDR THRU Q23-CHK-FILTER
               END-IF
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO C70-LIST-ERRORS
               END-IF
               IF  LIST-GRPNAME-SKIPTO-I < MDST-GRP-NAME
                   PERFORM Q24-READPR-DST-HDR THRU Q24-CHK-FILTER
                     VARYING IDX FROM 1 BY 1 UNTIL IDX > 2
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first entry.'
                                       TO LIST-MESSAGE-O
                   END-IF
               END-IF
               PERFORM Q25-ENDBR-MAILDST
           WHEN MDST-SAVEKEY > SPACES
               MOVE MDST-SAVEKEY       TO MDST-GRP-NAME
           WHEN LIST-GRPNAME-I(1) > SPACES
               MOVE LIST-GRPNAME-I(1)  TO MDST-GRP-NAME
           END-EVALUATE.

           PERFORM Q21-STARTBR-MAILDST.
           IF  EIBRESP = DFHRESP(NOTFND)
               MOVE LOW-VALUES         TO MDST-KEY
               PERFORM Q21-STARTBR-MAILDST
               IF  EIBRESP = DFHRESP(NOTFND)
                   MOVE DFHRESP(NORMAL) TO EIBRESP
                   MOVE 2              TO IDX
                   GO TO C65-LIST-BLANKS
               END-IF
           END-IF.

           MOVE SPACES                 TO MDST-SAVEKEY.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1
               UNTIL IDX > LISTROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

               PERFORM Q23-READNX-DST-HDR THRU Q23-CHK-FILTER
               IF  EIBRESP = DFHRESP(NORMAL)

      * BUILD DETAIL LINE OUTPUT HERE
                   MOVE LOW-VALUES     TO LISTMAP-ROW-DETAIL(IDX)
                   MOVE MDST-GRP-NAME  TO LIST-GRPNAME-O(IDX)
                   MOVE MDST-GRP-DESC  TO LIST-GRPDESC-O(IDX)
                   IF  MDST-USAGE-COUNT > ZEROES
                       MOVE MDST-USAGE-COUNT
                                       TO LIST-USECOUNT-O(IDX)
                   ELSE
                       MOVE SPACES     TO LIST-USECOUNT-I(IDX)
                   END-IF
                   IF  MDST-LAST-USE-DATE > ZEROES
                       MOVE MDST-LAST-USE-DATE
                                       TO LIST-LASTUSED-O(IDX)
                   ELSE
                       MOVE SPACES     TO LIST-LASTUSED-I(IDX)
                   END-IF
                   MOVE MDST-LAST-USED-BY
                                       TO LIST-LASTUSEBY-O(IDX)
                   MOVE MDST-UPDATED-BY
                                       TO LIST-UPDATEDBY-O(IDX)

               ELSE
                   MOVE  -1            TO LIST-SELECT-L(IDX)
                   SUBTRACT 1        FROM IDX
               END-IF
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               PERFORM C65-LIST-BLANKS
               GO TO C70-LIST-ERRORS
           END-IF.

           PERFORM Q25-ENDBR-MAILDST.

       C65-LIST-BLANKS.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM IDX BY 1
               UNTIL IDX > LISTROWS
             MOVE LOW-VALUES           TO LISTMAP-ROW-DETAIL(IDX)
             MOVE FLDASDRK             TO LIST-SELECT-A(IDX)
           END-PERFORM.

       C70-LIST-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO LIST-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       C80-LIST-OUTPUT.

           MOVE MAPTITLE               TO LIST-COMPANY-O.
           MOVE WS-WRKDATE             TO LIST-SYSDATE-O.
           MOVE WS-WRKTIME             TO LIST-SYSTIME-O.
           MOVE SPACES                 TO LIST-NETNAME-O.
           EXEC CICS ASSIGN NETNAME(LIST-NETNAME-O) END-EXEC.
           IF  LIST-NETNAME-I NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO LIST-NETNAME-O
           END-IF.

       C85-LIST-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (LISTMAP)
                             DATAONLY
                             FROM     (LISTMAPO)
                             MAPSET   (MDSTMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      (LISTMAP)
                             DATAONLY
                             FROM     (LISTMAPO)
                             MAPSET   (MDSTMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               EXEC CICS SEND
                         MAP      (LISTMAP)
                         FROM     (LISTMAPO)
                         MAPSET   (MDSTMS)
                         TERMINAL
                         FREEKB
                         ERASE
               END-EXEC
           END-IF.

       C90-LIST-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (LISTMAPI)
           END-EXEC.

           GO TO Z80-MDST-RETURN.

      /*****************************************************************
      *    DETL ROUTINE                                                *
      ******************************************************************
       D00-DETL-ROUTINE.

           IF  ADDRESS OF DETLMAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF DETLMAPI)
                         LENGTH   (LENGTH OF DETLMAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
           END-IF.

           IF  COMM-CURRRTN NOT = 'DETL'
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-SAVE-FUNC = 'RETN'
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE MDST-SAVE-TIOA TO DETLMAPO
                   MOVE LOW-VALUES     TO MDST-SAVE-TIOA
                   MOVE ZEROES         TO ROW
                   IF  MDST-SAVEKEY(1:8) > SPACES
                   AND COMM-HELPAREA(1:4) NOT = 'HELP'
                   AND SS-ITEM > ZEROES AND SS-ITEM <= DETLROWS
                     MOVE AIDENTER     TO EIBAID
                     MOVE SS-ITEM      TO ROW
                     MOVE LENGTH OF DETL-NETWORKID-I
                                       TO DETL-NETWORKID-L(ROW)
                     MOVE FLDCURSM     TO DETL-NETWORKID-F(ROW)
                     MOVE MDST-SAVEKEY(1:8)
                                       TO DETL-NETWORKID-I(ROW)
                     PERFORM D80-DETL-OUTPUT THRU D85-DETL-SENDMAP
                     GO TO D30-DETL-PROCESS
                   END-IF
                   MOVE SPACES         TO COMM-HELPAREA
               ELSE
                   MOVE ZEROES         TO TS-TOTL
               END-IF
               PERFORM D80-DETL-OUTPUT THRU D85-DETL-SENDMAP
               GO TO D50-DETL-DISPLAY
           END-IF.

       D10-DETL-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (DETLMAP)
                     INTO     (DETLMAPI)
                     MAPSET   (MDSTMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO D70-DETL-ERRORS
           END-IF.

       D20-DETL-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
               MOVE JOBSSELT           TO COMM-LINKPGM
               GO TO Z90-MDST-TERMINATION

           WHEN AIDPFK03
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE 'RETN'             TO COMM-CURRRTN
               MOVE MDST-GRP-NAME      TO MDST-SAVEKEY
               GO TO C00-LIST-ROUTINE

           WHEN AIDPFK01
      *    ... save current screen data content in commarea ...
               MOVE DETLMAPI           TO MDST-SAVE-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDENTER
           WHEN AIDPFK04
           WHEN AIDPFK05
           WHEN AIDPFK06
           WHEN AIDPFK07
           WHEN AIDPFK08
           WHEN AIDPFK09
               CONTINUE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO DETL-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO DETL-MESSAGE-O
               GO TO D80-DETL-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO DETL-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO DETL-MESSAGE-O
               GO TO D80-DETL-OUTPUT
           END-EVALUATE.

       D30-DETL-PROCESS.

           IF (EIBAID = AIDPFK04 OR AIDPFK05)
           AND VIEW-PROCESS
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO DETL-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO DETL-MESSAGE-O
               GO TO D80-DETL-OUTPUT
           END-IF.

      * convert cursor position to row and column coordinates
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.
           COMPUTE ROW = (ROW - DETLHEAD) / DETLDEEP.

      * PERFORM DETAIL FIELD EDITING HERE

           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           IF  NOT VIEW-PROCESS

           IF  ADD-PROCESS
               INSPECT DETL-GRPNAME-I CONVERTING LOWER-CASE
                                              TO UPPER-CASE
           END-IF

           IF  DETL-GRPNAME-L      > ZERO
           OR  DETL-GRPNAME-F      = FLDMODFY OR FLDCURSM
             IF  DETL-GRPNAME-I NOT > SPACES
             AND NOT ADD-PROCESS
               MOVE MDST-SAVEKEY       TO DETL-GRPNAME-O
             END-IF
             IF  DETL-GRPNAME-I NOT > SPACES
               MOVE 'Group name is required.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-GRPNAME-L
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
             ELSE
               SET TXT-REQUEST-JUSTIFY-L TO TRUE
               MOVE LOW-VALUES         TO TXT-DLMS
               MOVE ' '                TO TXT-DLM1
               MOVE LENGTH OF DETL-GRPNAME-I
                                       TO TXT-PARM-STRLEN
               MOVE DETL-GRPNAME-I     TO TXT-PARM-STRING
               CALL 'TXTMAN'        USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
               MOVE TXT-PARM-STRING    TO DETL-GRPNAME-O
               IF  TXT-PARM-STRING NOT ALPHAMERIC-UNDERSCORE
                 MOVE 'Group name must be alpha-numeric and underscore c
      -               'haracters.'     TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-GRPNAME-L
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                 IF  DETL-GRPNAME-I > SPACES
                 AND ADD-PROCESS
                   PERFORM Q20-INITKEY-MAILDST
                   MOVE DETL-GRPNAME-I TO MDST-GRP-NAME
                   PERFORM Q26-READEQ-MAILDST
                   EVALUATE EIBRESP
                   WHEN DFHRESP(NOTFND)
                     MOVE DFHRESP(NORMAL) TO EIBRESP
                   WHEN DFHRESP(NORMAL)
                     MOVE 'Group name already exists.'
                                       TO MULTI-EDIT-MSG
                     CALL MULTMESG  USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-GRPNAME-L
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
                   WHEN OTHER
                     GO TO D70-DETL-ERRORS
                   END-EVALUATE
                 END-IF
               END-IF
               MOVE DETL-GRPNAME-I     TO MDST-SAVEKEY
             END-IF
           END-IF

           IF  DETL-GRPDESC-L      > ZERO
           OR  DETL-GRPDESC-F      = FLDMODFY OR FLDCURSM
             IF  DETL-GRPDESC-I NOT > SPACES
               MOVE 'Group description is required.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-GRPDESC-L
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
             END-IF
           END-IF

           IF  EIBAID = AIDPFK04
               IF  1 <= ROW AND ROW <= DETLROWS
               AND DETL-NETWORKID-F(ROW) = FLDCURSR OR FLDCURSM
                   MOVE 'EXIT'         TO COMM-TRANSID
                   MOVE FLDUNFST       TO DETL-NETWORKID-A(ROW)
                   IF  DETL-NETWORKID-I(ROW) > SPACES
                       MOVE DETL-NETWORKID-I(ROW)
                                       TO MDST-SAVEKEY
                   ELSE
                       COMPUTE SS-ITEM = TS-ITEM + (ROW - 1)
                       MOVE LOW-VALUES TO TS-RECD
                       PERFORM Q32-READQ-TS
                       MOVE TS-IDNT    TO MDST-SAVEKEY
                   END-IF
                   INSPECT MDST-SAVEKEY CONVERTING LOWER-CASE
                                                TO UPPER-CASE
                   MOVE ROW            TO SS-ITEM
                   COMPUTE LINK-LEN = LENGTH OF COMM-HEADER
                                    + LENGTH OF COMM-SHARED
                                    + LENGTH OF MDST-SAVEKEY
                   MOVE DETLMAPI       TO MDST-SAVE-TIOA
                   GO TO T00-LINK-TRANSFER
               END-IF
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO DETL-MESSAGE-H
               MOVE 'Cursor is not in a promptable field.'
                                       TO DETL-MESSAGE-O
               GO TO D80-DETL-OUTPUT
           END-IF

           MOVE 1                      TO IDX
           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM TS-ITEM BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR IDX > DETLROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q32-READQ-TS
             PERFORM P30-EDIT-DIST-LINE THRU P35-EXIT
             ADD  1                    TO IDX
           END-PERFORM
           IF  IDX <= DETLROWS
             PERFORM P30-EDIT-DIST-LINE THRU P35-EXIT
           END-IF

           END-IF.

           IF  ERRORS-FOUND
               GO TO D80-DETL-OUTPUT
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO D70-DETL-ERRORS
           END-IF.

      * END OF DETAIL FIELD EDITING

           IF  EIBAID = AIDPFK05
               GO TO D40-DETL-UPDATES
           END-IF.

           IF  EIBAID = AIDPFK06 OR AIDPFK09
               IF  EIBAID = AIDPFK06
                   IF  TS-ITEM = 1
                       MOVE 'This is the first page.' TO DETL-MESSAGE-O
                   ELSE
                       MOVE 1          TO TS-ITEM
                   END-IF
               ELSE
                   COMPUTE TS-ITEM = TS-TOTL - (DETLROWS - 1)
                   IF  TS-ITEM < 1
                       MOVE 1          TO TS-ITEM
                       MOVE 'This is the last page.' TO DETL-MESSAGE-O
                   END-IF
               END-IF
               GO TO D50-DETL-DISPLAY
           END-IF.

           IF  EIBAID = AIDPFK07 OR AIDPFK08
               IF  EIBAID = AIDPFK07
                   IF  (ROW < 1 OR >= DETLROWS)
                       MOVE ZEROES     TO ROW
                   END-IF
                   COMPUTE TS-ITEM = TS-ITEM - (DETLROWS - ROW)
                   IF  TS-ITEM < 1
                       MOVE +1         TO TS-ITEM
                       MOVE 'This is the first entry.'
                                       TO DETL-MESSAGE-O
                   END-IF
               ELSE
                   IF  (ROW <= 1 OR > DETLROWS)
                       COMPUTE ROW = DETLROWS + 1
                   END-IF
                   COMPUTE TS-ITEM = TS-ITEM + (ROW - 1)
                   IF  TS-ITEM > TS-TOTL
                       MOVE TS-TOTL    TO TS-ITEM
                       MOVE 'This is the last entry.'
                                       TO DETL-MESSAGE-O
                   END-IF
               END-IF
               GO TO D50-DETL-DISPLAY
           END-IF.

           GO TO D50-DETL-DISPLAY.

       D40-DETL-UPDATES.

           PERFORM Q20-INITKEY-MAILDST.
           MOVE DETL-GRPNAME-I         TO MDST-GRP-NAME.
           PERFORM Q26-READUP-MAILDST.
           IF  EIBRESP = DFHRESP(NOTFND)
               INITIALIZE MDST-HEADER
           END-IF.

           MOVE DETL-GRPDESC-I         TO MDST-GRP-DESC.
           MOVE CICS-USERID            TO MDST-UPDATED-BY.

           IF  EIBRESP = DFHRESP(NOTFND)
               PERFORM Q27-WRITE-MDST-HEADER
           ELSE
               PERFORM Q27-REWRITE-MDST-HEADER
           END-IF.

           IF  TS-TOTL > 1
               PERFORM P50-SORT-TS-QUEUE THRU P55-EXIT
           END-IF.

           MOVE 1                      TO IDX.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM 1 BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR EIBRESP NOT = DFHRESP(NORMAL)

               PERFORM Q32-READQ-TS
               IF  TS-SELECT NOT = 'D'

                   MOVE IDX            TO MDST-GRP-SEQU
                   PERFORM Q26-READUP-MAILDST

                   MOVE TS-IDNT        TO MDST-IDNT

                   IF  TS-NAME-OVERRIDE = 'Y'
                   OR  MDST-IDNT <= SPACES
                       MOVE TS-NAME    TO MDST-NAME
                   ELSE
                       MOVE SPACES     TO MDST-NAME
                   END-IF

                   IF  TS-ADDR-OVERRIDE = 'Y'
                   OR  MDST-IDNT <= SPACES
                       MOVE TS-ADDR    TO MDST-ADDR
                   ELSE
                       MOVE SPACES     TO MDST-ADDR
                   END-IF

                   EVALUATE TS-TYPE
                   WHEN '3'
                       MOVE '*BCC'     TO MDST-TYPE
                   WHEN '2'
                       MOVE '*CC'      TO MDST-TYPE
                   WHEN OTHER
                       MOVE '*PRI'     TO MDST-TYPE
                   END-EVALUATE

                   EVALUATE EIBRESP
                   WHEN DFHRESP(NOTFND)
                       PERFORM Q27-WRITE-MDST-ENTRY
                   WHEN DFHRESP(NORMAL)
                       PERFORM Q27-REWRITE-MDST-ENTRY
                   END-EVALUATE

                   ADD  1              TO IDX
               END-IF
           END-PERFORM.

           PERFORM WITH TEST BEFORE
             VARYING MDST-GRP-SEQU FROM IDX BY 1
               UNTIL EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q26-READUP-MAILDST
             IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q28-DELETE-MAILDST
             END-IF
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(NOTFND)
               GO TO D70-DETL-ERRORS
           END-IF.

           MOVE COMM-CURRRTN           TO COMM-SAVE-FUNC.
           MOVE 'RETN'                 TO COMM-CURRRTN.
           GO TO C00-LIST-ROUTINE.

       D50-DETL-DISPLAY.

           MOVE 'DETL'                 TO COMM-CURRRTN.

      * skip if heading selections changed
           IF  TS-TOTL <= ZERO
               GO TO D60-DETL-DETAIL
           END-IF.

      * PERFORM HEADER CONTROL FIELD EDITING HERE

       D60-DETL-DETAIL.

           IF  TS-TOTL <= ZERO
               PERFORM P10-EXTRACT-DIST-INFO THRU P15-EXIT
               IF  ERRORS-FOUND
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE -2             TO ERROR-SWITCH
                   MOVE EXHREVRS       TO DETL-MESSAGE-H
                   IF  DETL-MESSAGE-I <= SPACES
                       PERFORM X00-UNEX-ERR
                       MOVE UNEX-MSG   TO DETL-MESSAGE-O
                   END-IF
                   GO TO D80-DETL-OUTPUT
               END-IF
               IF  TS-TOTL <= ZERO
                   MOVE 2              TO IDX
                   GO TO D65-DETL-BLANKS
               END-IF
           END-IF.

           MOVE 1                      TO IDX.
           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM TS-ITEM BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR IDX > DETLROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM Q32-READQ-TS
             IF  EIBRESP = DFHRESP(NORMAL)

      * BUILD DETAIL LINE OUTPUT HERE
               MOVE LOW-VALUES         TO DETLMAP-ROW-DETAIL(IDX)
               MOVE TS-SELECT          TO DETL-SELECT-O(IDX)
               MOVE TS-IDNT            TO DETL-NETWORKID-O(IDX)
               IF (SPACES >= TS-NAME OR TS-ADDR)
                 PERFORM P40-GET-MAIL-ADDRESS THRU P45-EXIT
               END-IF
               MOVE TS-NAME            TO DETL-DISTNAME-O(IDX)
               MOVE TS-ADDR            TO DETL-DISTADDR-O(IDX)
               EVALUATE TS-TYPE
               WHEN '3'
                   MOVE '*BCC'         TO DETL-RECPTYPE-O(IDX)
               WHEN '2'
                   MOVE '*CC'          TO DETL-RECPTYPE-O(IDX)
               WHEN OTHER
                   MOVE '*PRI'         TO DETL-RECPTYPE-O(IDX)
               END-EVALUATE

             ELSE
               MOVE -1                 TO DETL-SELECT-L(IDX)
             END-IF
             ADD  1                    TO IDX
           END-PERFORM.

           IF  NOT VIEW-PROCESS
               ADD  1                  TO IDX
           END-IF.

       D65-DETL-BLANKS.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM IDX BY 1
               UNTIL IDX > DETLROWS
             MOVE LOW-VALUES           TO DETLMAP-ROW-DETAIL(IDX)
             MOVE FLDASDRK             TO DETL-SELECT-A(IDX)
                                          DETL-NETWORKID-A(IDX)
                                          DETL-DISTNAME-A(IDX)
                                          DETL-DISTADDR-A(IDX)
                                          DETL-RECPTYPE-LABEL-A(IDX)
                                          DETL-RECPTYPE-A(IDX)
           END-PERFORM.

       D70-DETL-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO DETL-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO DETL-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       D80-DETL-OUTPUT.

           MOVE MAPTITLE               TO DETL-COMPANY-O.
           MOVE WS-WRKDATE             TO DETL-SYSDATE-O.
           MOVE WS-WRKTIME             TO DETL-SYSTIME-O.
           MOVE SPACES                 TO DETL-NETNAME-O.
           EXEC CICS ASSIGN NETNAME(DETL-NETNAME-O) END-EXEC.
           IF  DETL-NETNAME-I NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO DETL-NETNAME-O
           END-IF.

           IF  NOT ADD-PROCESS
           AND NOT CHG-PROCESS
               MOVE FLDASFST           TO DETL-GRPNAME-A
               MOVE FLDASFST           TO DETL-GRPDESC-A
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > DETLROWS
                 IF  DETL-SELECT-A(IDX) NOT = FLDASDRK
                   MOVE FLDASKIP       TO DETL-SELECT-A(IDX)
                                          DETL-NETWORKID-A(IDX)
                                          DETL-DISTNAME-A(IDX)
                                          DETL-DISTADDR-A(IDX)
                                          DETL-RECPTYPE-A(IDX)
                 END-IF
               END-PERFORM
               MOVE FLDASDRK           TO DETL-OPTIONS-LABEL-A
                                          DETL-DELETE-OPT-A
                                          DETL-DELETE-TEXT-A
                                          DETL-PROMPT-KEY-A
                                          DETL-PROMPT-TEXT-A
                                          DETL-UPDATE-KEY-A
                                          DETL-UPDATE-TEXT-A
           ELSE
               IF  NOT ADD-PROCESS
                   MOVE FLDASFST       TO DETL-GRPNAME-A
               ELSE
                   MOVE EXHULINE       TO DETL-GRPNAME-H
               END-IF
               MOVE EXHULINE           TO DETL-GRPDESC-H
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > DETLROWS
                 IF  DETL-SELECT-A(IDX) NOT = FLDASDRK
                   MOVE EXHULINE       TO DETL-SELECT-H(IDX)
                                          DETL-NETWORKID-H(IDX)
                                          DETL-DISTNAME-H(IDX)
                                          DETL-DISTADDR-H(IDX)
                                          DETL-RECPTYPE-H(IDX)
                 END-IF
               END-PERFORM
           END-IF.

       D85-DETL-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (DETLMAP)
                             DATAONLY
                             FROM     (DETLMAPO)
                             MAPSET   (MDSTMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      (DETLMAP)
                             DATAONLY
                             FROM     (DETLMAPO)
                             MAPSET   (MDSTMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               IF  VIEW-PROCESS
                   MOVE -1             TO DETL-GRPNAME-L
               ELSE
                   EVALUATE TRUE
                   WHEN DETL-GRPNAME-I > SPACES
                    AND DETL-GRPDESC-I > SPACES
                    AND TS-TOTL > ZEROES
                       MOVE ZEROES     TO DETL-GRPNAME-L
                                          DETL-GRPDESC-L
                       MOVE -1         TO DETL-SELECT-L(1)
                   WHEN ADD-PROCESS
                       MOVE -1         TO DETL-GRPNAME-L
                   WHEN OTHER
                       MOVE -1         TO DETL-GRPDESC-L
                   END-EVALUATE
               END-IF
               EXEC CICS SEND
                         MAP      (DETLMAP)
                         FROM     (DETLMAPO)
                         MAPSET   (MDSTMS)
                         TERMINAL
                         FREEKB
                         ERASE
                         CURSOR
               END-EXEC
           END-IF.

       D90-DETL-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (DETLMAPI)
           END-EXEC.

           GO TO Z80-MDST-RETURN.

      /*****************************************************************
      *    MAIL ROUTINE                                                *
      ******************************************************************
       M00-MAIL-TEST-MESSAGE.
           MOVE 'Generating...  '      TO POPUP-TEXT.
           PERFORM Q10-DISPLAY-POPUP THRU Q15-EXIT.

       M10-MAIL-STARTBR.

           PERFORM Q20-INITKEY-MAILDST.
           MOVE MDST-SAVEKEY           TO MDST-GRP-NAME.
           PERFORM Q21-STARTBR-MAILDST.
           IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q23-READNEXT-MAILDST
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               PERFORM C70-LIST-ERRORS
               GO TO M95-EXIT
           END-IF.

           PERFORM Q23-READNEXT-MAILDST
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  MDST-GRP-NAME NOT = MDST-SAVEKEY
               PERFORM Q25-ENDBR-MAILDST
               MOVE -2                 TO ERROR-SWITCH
               MOVE 'No distribution entries found.'
                                       TO LIST-MESSAGE-O
               GO TO M95-EXIT
           END-IF.

       M20-MAIL-INITIALIZE.

           MOVE SPACE                  TO PRT-LNK-REQU.
           SET  PRT-PWR-RDRQ           TO TRUE.
           MOVE TESTMAIL               TO PRT-PWR-JOBN.
           MOVE CICS-USERID            TO PRT-PWR-USER.
           MOVE 80                     TO PRT-STR-LENG.

           COMPUTE JCNT = LENGTH OF MAIL-JCL
                        / LENGTH OF MAIL-ENTRY.

       M30-MAIL-HEADERS.

           PERFORM WITH TEST BEFORE
             VARYING JSUB FROM 1 BY 1
               UNTIL JSUB > JCNT
                  OR MAIL-ENTRY(JSUB)(1:10) = '+++HEAD+++'
                  OR EIBRESP NOT = DFHRESP(NORMAL)
                  OR NOT PRINT-COMPLETED
             MOVE MAIL-ENTRY(JSUB)     TO PRT-STR-DATA
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

           IF  JSUB > JCNT
             MOVE -2                   TO ERROR-SWITCH
             MOVE 'MAIL JCL missing HEAD inclusion point.'
                                       TO LIST-MESSAGE-O
             SET  PRINT-DELSPOOL       TO TRUE
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT
             GO TO M95-EXIT
           END-IF.

           ADD  1                      TO JSUB.

           MOVE 'ORGA Organization: Winsupply Group Services'
                                       TO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

           STRING 'RPTO ReplyTo: '
                  'Finance Programming '
                  '<FinanceProgramming@winsupplyinc.com>'
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

       M40-MAIL-DISTRIBUTION.

           PERFORM WITH TEST BEFORE
             UNTIL MDST-GRP-NAME NOT = MDST-SAVEKEY
                OR EIBRESP NOT = DFHRESP(NORMAL)
                OR NOT PRINT-COMPLETED

             PERFORM P20-FILL-TS-RECD THRU P25-EXIT
             IF  SPACES >= TS-NAME OR TS-ADDR
                 PERFORM P40-GET-MAIL-ADDRESS THRU P45-EXIT
             END-IF

             MOVE 1                    TO PTR
             EVALUATE TS-TYPE
             WHEN 3
                 STRING 'RCPT Bcc: '      DELIMITED BY SIZE
                   INTO PRT-STR-DATA WITH POINTER PTR
             WHEN 2
                 STRING 'RCPT Cc: '       DELIMITED BY SIZE
                   INTO PRT-STR-DATA WITH POINTER PTR
             WHEN OTHER
                 STRING 'RCPT To: '       DELIMITED BY SIZE
                   INTO PRT-STR-DATA WITH POINTER PTR
             END-EVALUATE

             IF  TS-NAME > SPACES
                 MOVE TS-NAME          TO UNEX-VARIABLE
                 PERFORM Q202-SET-LENGTH
                 MOVE UNEX-LENG        TO LEN
                 STRING TS-NAME(1:LEN) ' ' DELIMITED BY SIZE
                   INTO PRT-STR-DATA WITH POINTER PTR
             END-IF

             MOVE TS-ADDR              TO UNEX-VARIABLE
             PERFORM Q202-SET-LENGTH
             MOVE UNEX-LENG            TO LEN
             STRING '<' TS-ADDR(1:LEN) '>' DELIMITED BY SIZE
                 INTO PRT-STR-DATA   WITH POINTER PTR

             PERFORM P00-CALL-PRTMAN THRU P05-EXIT
             IF  EIBRESP = DFHRESP(NORMAL)
             AND PRINT-COMPLETED
                 PERFORM Q23-READNEXT-MAILDST
             END-IF

           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               GO TO M90-MAIL-RETURN
           END-IF.

           PERFORM Q25-ENDBR-MAILDST.

       M70-MAIL-TRAILER.

           PERFORM WITH TEST BEFORE
             VARYING JSUB FROM JSUB BY 1
               UNTIL JSUB > JCNT
                  OR MAIL-ENTRY(JSUB)(1:10) = '+++TAIL+++'
                  OR EIBRESP NOT = DFHRESP(NORMAL)
                  OR NOT PRINT-COMPLETED
             MOVE MAIL-ENTRY(JSUB)     TO PRT-STR-DATA
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

           IF  JSUB > JCNT
             MOVE -2                   TO ERROR-SWITCH
             MOVE 'MAIL JCL missing TAIL inclusion point.'
                                       TO LIST-MESSAGE-O
             SET  PRINT-DELSPOOL       TO TRUE
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT
             GO TO M95-EXIT
           END-IF.

           ADD  1                      TO JSUB.

           STRING '<h1 style="font-family:helvetica;"><em><font '
                  'color=blue>Win</font>supply</em>' nbsp
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

           MOVE '<span style="font-size:8pt;font-weight:bold;">'
                                       TO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

           MOVE 'The Winsupply Family of Companies</span></h1>'
                                       TO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

           STRING 'Sent by Mainframe Mail: '
                  '"A new <em>frame</em> of mind."&nbsp;' nbsp
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

           STRING 'Distribution group "'  DELIMITED BY SIZE
                  MDST-SAVEKEY            DELIMITED BY SPACE
                  '" used to send this email.<br>'
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

           STRING '<p><b>Note:</b> Managers may request distribution'
                  ' changes by logging into the' nbsp
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

           MOVE '<a href="https://cherwellprod.winwholesale.com/Cherwell
      -         'Portal/IT">'          TO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

           STRING 'WinZone/Support/Request Help</a> link.&nbsp;'
                  ' As more than one group may be' nbsp
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

           STRING 'listed above, please include the specific'
                  ' group name desired (and a' nbsp
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

           STRING 'screenshot of the email) in your'
                  ' request.&nbsp; Thanks.</p>' nbsp
               DELIMITED BY SIZE     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN THRU P05-EXIT.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT PRINT-COMPLETED
               GO TO M90-MAIL-RETURN
           END-IF.

       M80-MAIL-COMPLETE.

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
               GO TO M90-MAIL-RETURN
           END-IF.

           MOVE SPACES                 TO MDST-SAVEKEY.
           MOVE 'Test email submitted.'
                                       TO LIST-MESSAGE-O.
       M90-MAIL-RETURN.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               PERFORM C70-LIST-ERRORS
               GO TO M95-EXIT
           END-IF.

           IF  NOT PRINT-COMPLETED
               SET  PRINT-DELSPOOL     TO TRUE
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT
               MOVE -2                 TO ERROR-SWITCH
               MOVE 'TESTMAIL generation failed.  See log.'
                                       TO LIST-MESSAGE-O
               GO TO M95-EXIT
           END-IF.

       M95-EXIT.
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

       P10-EXTRACT-DIST-INFO.

           PERFORM Q35-DELETEQ-TS.

           PERFORM Q20-INITKEY-MAILDST.
           MOVE MDST-SAVEKEY           TO MDST-GRP-NAME.
           PERFORM Q21-STARTBR-MAILDST.

           PERFORM WITH TEST BEFORE
             UNTIL MDST-GRP-NAME NOT = MDST-SAVEKEY
                OR EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q23-READNEXT-MAILDST
             IF  EIBRESP = DFHRESP(NORMAL)
             AND MDST-GRP-NAME = MDST-SAVEKEY
               IF  MDST-GRP-HEADER
                 MOVE MDST-GRP-NAME    TO DETL-GRPNAME-O
                 MOVE MDST-GRP-DESC    TO DETL-GRPDESC-O
               ELSE
                 PERFORM P20-FILL-TS-RECD THRU P25-EXIT
                 PERFORM Q33-WRITEQ-TS
               END-IF
             END-IF
           END-PERFORM.

           IF  EIBRESP = DFHRESP(NORMAL)
                      OR DFHRESP(NOTFND)
                      OR DFHRESP(ENDFILE)
               PERFORM Q25-ENDBR-MAILDST
               PERFORM Q31-NUMITEMS-TS
           END-IF.

       P15-EXIT.
           EXIT.

       P20-FILL-TS-RECD.

           EVALUATE MDST-TYPE
           WHEN '*BCC'
               MOVE 3                  TO TS-TYPE
           WHEN '*CC'
               MOVE 2                  TO TS-TYPE
           WHEN OTHER
               MOVE 1                  TO TS-TYPE
           END-EVALUATE.

           MOVE MDST-IDNT              TO TS-IDNT.

           MOVE MDST-NAME              TO TS-NAME.
           IF  MDST-NAME > SPACES
             MOVE 'Y'                  TO TS-NAME-OVERRIDE
           ELSE
             MOVE 'N'                  TO TS-NAME-OVERRIDE
           END-IF.

           MOVE MDST-ADDR              TO TS-ADDR.
           IF  MDST-ADDR > SPACES
             MOVE 'Y'                  TO TS-ADDR-OVERRIDE
           ELSE
             MOVE 'N'                  TO TS-ADDR-OVERRIDE
           END-IF.

           MOVE SPACE                  TO TS-SELECT.
       P25-EXIT.
           EXIT.

       P30-EDIT-DIST-LINE.
           IF  DETL-SELECT-L(IDX) > ZERO
           OR  DETL-SELECT-F(IDX) = FLDMODFY OR FLDCURSM
               INSPECT DETL-SELECT-I(IDX) CONVERTING LOWER-CASE
                                                  TO UPPER-CASE
               IF  DETL-SELECT-I(IDX) > SPACES
               AND DETL-SELECT-I(IDX) NOT = 'D'
                 MOVE 'Option selection is invalid.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-SELECT-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                 MOVE DETL-SELECT-I(IDX)
                                       TO TS-SELECT
               END-IF
           END-IF.

           IF  TS-SELECT NOT = 'D'

               IF  SS-ITEM > TS-TOTL
               AND DETL-NETWORKID-I(IDX) <= SPACES
               AND DETL-DISTNAME-I(IDX)  <= SPACES
               AND DETL-DISTADDR-I(IDX)  <= SPACES
               AND DETL-RECPTYPE-I(IDX)  <= SPACES
                 GO TO P35-EXIT
               END-IF

               IF  DETL-NETWORKID-L(IDX) > ZERO
               OR  DETL-NETWORKID-F(IDX) = FLDMODFY OR FLDCURSM
                 IF  DETL-NETWORKID-I(IDX) <= SPACES
                   IF  DETL-DISTNAME-I(IDX) <= SPACES
                   AND DETL-DISTADDR-I(IDX) <= SPACES
                   AND TS-IDNT              <= SPACES
                   AND TS-NAME              <= SPACES
                   AND TS-ADDR              <= SPACES
                     MOVE 'Network id is required.'
                                       TO MULTI-EDIT-MSG
                     CALL MULTMESG  USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-NETWORKID-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
                   ELSE
                     MOVE TS-IDNT      TO DETL-NETWORKID-O(IDX)
                   END-IF
                 ELSE
                   MOVE DETL-NETWORKID-I(IDX) TO UNEX-VARIABLE
                   PERFORM Q202-SET-LENGTH
/DLC0/*            IF  UNEX-LENG <= LENGTH OF VOPERID-USERID
/DLC0/             IF  UNEX-LENG <= LENGTH OF VUSER-KEY
/DLC0/*              PERFORM Q40-INITKEY-VOPERPU
/DLC0/               PERFORM Q40-INITKEY-VUSERID
                     MOVE DETL-NETWORKID-I(IDX)
/DLC0/*                                TO VOPERID-USERID
/DLC0/                                 TO VUSER-KEY
/DLC0/*              INSPECT VOPERID-USERID CONVERTING LOWER-CASE
/DLC0/               INSPECT VUSER-KEY CONVERTING LOWER-CASE
                                               TO UPPER-CASE
/DLC0/*              PERFORM Q46-READEQ-VOPERPU
/DLC0/               PERFORM Q46-READEQ-VUSERID
                     IF  EIBRESP = DFHRESP(NORMAL)
                       PERFORM Q50-INITKEY-IESLDUM
/DLC0/*                MOVE VOPERID-USERID
/DLC0/                 MOVE VUSER-KEY
                                       TO LDUM-MFUSRID
                       PERFORM Q56-READEQ-IESLDUV
                       IF  EIBRESP = DFHRESP(NORMAL)
                         MOVE LENGTH OF LDUM-NETUSRID
                                       TO DETL-NETWORKID-L(IDX)
                         MOVE LDUM-NETUSRID
                                       TO DETL-NETWORKID-O(IDX)
                       ELSE
                         MOVE SPACES   TO IESLDUM-RECORD
                         MOVE DFHRESP(NORMAL)
                                       TO EIBRESP
                       END-IF
                     ELSE
/DLC0/*                MOVE SPACES     TO VOPERID-RECORD
/DLC0/                 MOVE SPACES     TO VUSERID-RECORD
                     END-IF
                   END-IF
                   MOVE DETL-NETWORKID-I(IDX) TO UNEX-VARIABLE
                   PERFORM Q202-SET-LENGTH
/DLC0/*            IF  UNEX-LENG > LENGTH OF VOPERID-USERID
/DLC0/             IF  UNEX-LENG > LENGTH OF VUSER-KEY
                   OR  EIBRESP = DFHRESP(NOTFND)
                     PERFORM Q50-INITKEY-IESLDUM
                     MOVE DETL-NETWORKID-I(IDX)
                                       TO LDUM-NETUSRID
                     PERFORM Q56-READEQ-IESLDUM
                     IF  EIBRESP = DFHRESP(NORMAL)
                       MOVE LDUM-NETUSRID
                                       TO DETL-NETWORKID-O(IDX)
                     ELSE
                       MOVE SPACES     TO IESLDUM-RECORD
                       MOVE DFHRESP(NORMAL)
                                       TO EIBRESP
                     END-IF
                   END-IF
                   MOVE DETL-NETWORKID-I(IDX)
                                       TO TS-IDNT
                 END-IF
                 PERFORM P40-GET-MAIL-ADDRESS THRU P45-EXIT
                 INSPECT LDGA-ATTR-VALUE(4 1) CONVERTING LOWER-CASE
                                                      TO UPPER-CASE
/DLC0/*          IF  VOPERID-USERID > SPACES
/DLC0/           IF  VUSER-KEY > SPACES
/DLC0/*          AND VOPERID-USERID NOT = LDGA-ATTR-VALUE(4 1)
/DLC0/           AND VUSER-KEY NOT = LDGA-ATTR-VALUE(4 1)
                 OR  LDUM-MFUSRID > SPACES
                 AND LDUM-MFUSRID NOT = LDGA-ATTR-VALUE(4 1)
                   MOVE 'Mainframe user id does not match Active Directo
      -                 'ry.'          TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-NETWORKID-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
                 END-IF
               END-IF

               IF  DETL-DISTNAME-L(IDX) > ZERO
               OR  DETL-DISTNAME-F(IDX) = FLDMODFY OR FLDCURSM
                 IF  DETL-DISTNAME-I(IDX) <= SPACES
                   MOVE TS-NAME        TO DETL-DISTNAME-O(IDX)
                 ELSE
                   IF  DETL-DISTNAME-I(IDX) NOT ALPHAMERIC-AND-PERIOD
                     MOVE 'Email name allows alpha-numeric, period, ampe
      -                   'rsand, and dash characters.'
                                       TO MULTI-EDIT-MSG
                     CALL MULTMESG  USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-DISTNAME-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
                   ELSE
                     MOVE DETL-DISTNAME-I(IDX)
                                       TO TS-NAME
                     MOVE 'Y'          TO TS-NAME-OVERRIDE
                   END-IF
                 END-IF
               END-IF

               IF  DETL-DISTADDR-L(IDX) > ZERO
               OR  DETL-DISTADDR-F(IDX) = FLDMODFY OR FLDCURSM
                 IF  DETL-DISTADDR-I(IDX) <= SPACES
                   MOVE TS-ADDR        TO DETL-DISTADDR-O(IDX)
                 ELSE
                   PERFORM P60-VALIDATE-EMAIL THRU P65-EXIT
                 END-IF
               END-IF

               IF  TS-IDNT <= SPACES
               AND TS-ADDR <= SPACES
                 MOVE 'If user id omitted, email address is required.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-DISTADDR-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
               END-IF

               IF  DETL-RECPTYPE-L(IDX) > ZERO
               OR  DETL-RECPTYPE-F(IDX) = FLDMODFY OR FLDCURSM
               OR  TS-TYPE <= SPACES
                 INSPECT DETL-RECPTYPE-I(IDX) CONVERTING LOWER-CASE
                                                      TO UPPER-CASE
                 IF  DETL-RECPTYPE-I(IDX) <= SPACES
                   EVALUATE TS-TYPE
                   WHEN 3
                       MOVE '*BCC'     TO DETL-RECPTYPE-O(IDX)
                   WHEN 2
                       MOVE '*CC'      TO DETL-RECPTYPE-O(IDX)
                   WHEN OTHER
                       MOVE '*PRI'     TO DETL-RECPTYPE-O(IDX)
                   END-EVALUATE
                 END-IF
                 IF  DETL-RECPTYPE-I(IDX) <= SPACES
                   MOVE '*PRI'         TO DETL-RECPTYPE-O(IDX)
                 END-IF
                 IF  DETL-RECPTYPE-I(IDX) NOT = '*PRI'
                                            AND '*CC'
                                            AND '*BCC'
                     MOVE 'Recipient type is invalid.'
                                       TO MULTI-EDIT-MSG
                     CALL MULTMESG  USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-RECPTYPE-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
                 ELSE
                   EVALUATE DETL-RECPTYPE-I(IDX)
                   WHEN '*BCC'
                       MOVE 3          TO TS-TYPE
                   WHEN '*CC'
                       MOVE 2          TO TS-TYPE
                   WHEN OTHER
                       MOVE 1          TO TS-TYPE
                   END-EVALUATE
                 END-IF
               END-IF

           END-IF.

           IF  SS-ITEM > TS-TOTL
               PERFORM Q33-WRITEQ-TS
               ADD  1                  TO TS-TOTL
           ELSE
               PERFORM Q34-REWRITEQ-TS
           END-IF.
       P35-EXIT.
           EXIT.

       P40-GET-MAIL-ADDRESS.
           IF  TS-IDNT <= SPACES
               GO TO P45-EXIT
           END-IF.

      * this code looks up LDAP attributes for a network user; but,
      * when you don't know the network user id, use the IESLDUM file
      * to translate a vse user id back into a network user id
           INITIALIZE IESLDGA-COMMAREA.
           MOVE LENGTH OF IESLDGA-COMMAREA
                                       TO LDGA-AREA-LENGTH.
           MOVE TS-IDNT                TO LDGA-USER-ID.
           MOVE '&(objectClass=person)(objectClass=user)'
                                       TO LDGA-SEARCH-FILTER.
           MOVE 4                      TO LDGA-ATTR-COUNT.

           MOVE 'displayName'          TO LDGA-ATTR-NAME(1).
           MOVE LENGTH OF LDGA-ATTR-VALUE
                                       TO LDGA-VALUE-LENGTH(1).
           MOVE 8                      TO LDGA-VALUE-COUNT(1).

           MOVE 'mail'                 TO LDGA-ATTR-NAME(2).
           MOVE LENGTH OF LDGA-ATTR-VALUE
                                       TO LDGA-VALUE-LENGTH(2).
           MOVE 8                      TO LDGA-VALUE-COUNT(2).

           MOVE 'name'                 TO LDGA-ATTR-NAME(3).
           MOVE LENGTH OF LDGA-ATTR-VALUE
                                       TO LDGA-VALUE-LENGTH(3).
           MOVE 8                      TO LDGA-VALUE-COUNT(3).

           MOVE 'mfid'                 TO LDGA-ATTR-NAME(4).
           MOVE LENGTH OF LDGA-ATTR-VALUE
                                       TO LDGA-VALUE-LENGTH(4).
           MOVE 8                      TO LDGA-VALUE-COUNT(4).

           EXEC CICS LINK
                     PROGRAM  (IESLDGAC)
                     COMMAREA (IESLDGA-COMMAREA)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO P45-EXIT
           END-IF.

           IF  LDGA-RET-CODE = ZERO
               IF  TS-NAME <= SPACES
                   MOVE LDGA-ATTR-VALUE(1 1)
                                       TO TS-NAME
                   MOVE 'N'            TO TS-NAME-OVERRIDE
               END-IF
               IF  TS-ADDR <= SPACES
                   MOVE LDGA-ATTR-VALUE(2 1)
                                       TO TS-ADDR
                   MOVE 'N'            TO TS-ADDR-OVERRIDE
               END-IF
               MOVE LDGA-ATTR-VALUE(3 1)
                                       TO TS-IDNT
           ELSE
               MOVE SPACES             TO MULTI-EDIT-MSG
                                          LOGF-MESG
               MOVE 1                  TO PTR
                                          CNT
               STRING 'Unable to retrieve user - rc='
                   DELIMITED BY SIZE INTO MULTI-EDIT-MSG
                                     WITH POINTER PTR
               STRING LDGA-USER-ID        DELIMITED BY SPACE
                      ' unable to retrieve (' EIBTRMID ') - rc='
                   DELIMITED BY SIZE     INTO LOGF-MESG
                                     WITH POINTER CNT
               MOVE LDGA-RET-CODE      TO UNEX-RESP
               MOVE UNEX-RESP          TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               STRING UNEX-VARIABLE       DELIMITED BY SPACE
                      ', rsn='
                   DELIMITED BY SIZE INTO MULTI-EDIT-MSG
                                     WITH POINTER PTR
               STRING UNEX-VARIABLE       DELIMITED BY SPACE
                      ', rsn='
                   DELIMITED BY SIZE     INTO LOGF-MESG
                                     WITH POINTER CNT
               MOVE LDGA-LDAP-CODE     TO UNEX-RESP
               MOVE UNEX-RESP          TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               STRING UNEX-VARIABLE       DELIMITED BY SPACE
                                     INTO MULTI-EDIT-MSG
                                     WITH POINTER PTR
               STRING UNEX-VARIABLE       DELIMITED BY SPACE
                                         INTO LOGF-MESG
                                     WITH POINTER CNT
               PERFORM Q100-LOGIT    THRU Q199-EXIT
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-NETWORKID-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
           END-IF.
       P45-EXIT.
           EXIT.

       P50-SORT-TS-QUEUE.
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

      * delete the old queue
           PERFORM Q35-DELETEQ-TS.

      * check if sort completed
           IF  CICSSORT-RC NOT = ZEROES
               MOVE SPACES             TO DETL-MESSAGE-O
               MOVE CICSSORT-RC        TO NUM-WORK
               MOVE -2                 TO ERROR-SWITCH
               STRING 'Report sort failure, rc=' NUM-WRK2 '.'
                 DELIMITED BY SIZE   INTO DETL-MESSAGE-O
               GO TO P55-EXIT
           END-IF.

      * save new queue name
           MOVE CICSSORT-QNAME         TO TS-QUEUE.
           PERFORM Q31-NUMITEMS-TS.
       P55-EXIT.
           EXIT.

       P60-VALIDATE-EMAIL.
      * eliminate embedded blanks
           SET  TXT-REQUEST-JUSTIFY-L  TO TRUE.
           MOVE LOW-VALUES             TO TXT-DLMS.
           MOVE ' '                    TO TXT-DLM1.
           MOVE LENGTH OF DETL-DISTADDR-I(IDX)
                                       TO TXT-PARM-STRLEN.
           MOVE DETL-DISTADDR-I(IDX)   TO TXT-PARM-STRING.
           CALL 'TXTMAN'            USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING.
           MOVE TXT-PARM-STRING        TO DETL-DISTADDR-I(IDX).
      * make sure there is an @-sign in the email address
           SET  TXT-REQUEST-FIND       TO TRUE.
           MOVE ZEROES                 TO TXT-PNTR.

           MOVE +1                     TO TXT-PARM-STRLEN.
           MOVE '@'                    TO TXT-PARM-STRING.
           MOVE LENGTH OF DETL-DISTADDR-I(IDX)
                                       TO TXT-MSTR-BUFLEN.
           MOVE DETL-DISTADDR-I(IDX)   TO TXT-MSTR-BUFFER.
           CALL 'TXTMAN'            USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING.
           IF  TXT-STRING-NOT-FOUND
               MOVE 'Missing "@" in email address.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-DISTADDR-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
               GO TO P65-EXIT
           END-IF.
      * save position of @-sign character
           MOVE TXT-PNTR               TO POS.
      * calculate characters before @-sign character
           COMPUTE CNT = POS - 1.
           IF  CNT < 3
               MOVE 'Local mailbox part of email address is too short.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-DISTADDR-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
               GO TO P65-EXIT
           END-IF.
      * followed by a period character
           MOVE '.'                    TO TXT-PARM-STRING.
           CALL 'TXTMAN'            USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING.
           IF  TXT-STRING-NOT-FOUND
               MOVE 'Missing "." in domain part of email address.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-DISTADDR-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
               GO TO P65-EXIT
           END-IF.
      * save position of period character
           MOVE TXT-PNTR               TO PTR.
      * calculate characters between @-sign and period
           COMPUTE CNT = PTR - POS - 1.
           IF  CNT < 3
               MOVE 'Local domain part of email address is too short.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-DISTADDR-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
               GO TO P65-EXIT
           END-IF.
      * calculate total length of email address
           MOVE DETL-DISTADDR-I(IDX)   TO UNEX-VARIABLE.
           PERFORM Q203-LEFT-JUSTIFY.
           MOVE UNEX-LENG              TO LEN.
      * calculate characters between period and end of email address
           COMPUTE CNT = LEN - PTR.
           IF  CNT < 2
               MOVE 'Top-level domain part of address is too short.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-DISTADDR-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
               GO TO P65-EXIT
           END-IF.
      * email address accepted
           MOVE DETL-DISTADDR-I(IDX)   TO TS-ADDR.
           MOVE 'Y'                    TO TS-ADDR-OVERRIDE.
       P65-EXIT.
           EXIT.

      /*****************************************************************
      *    MORE PERFORMED ROUTINES                                     *
      ******************************************************************

       Q00-FORMAT-DATE-AND-TIME.
           EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     FULLDATE (WS-WRKDATE)          DATESEP
                     TIME     (WS-WRKTIME)          TIMESEP
           END-EXEC.
       Q02-REFORMAT-TIME.
           IF  WS-HRSTIME < 12
               MOVE ' AM'              TO WS-XXXTIME
               IF  WS-HRSTIME = 00
                   MOVE 12             TO WS-HRSTIME
               END-IF
           ELSE
               MOVE ' PM'              TO WS-XXXTIME
               IF  WS-HRSTIME > 12
                   SUBTRACT 12       FROM WS-HRSTIME
               END-IF
           END-IF.
       Q05-EXIT.
           EXIT.

       Q10-DISPLAY-POPUP.

           EXEC CICS GETMAIN
                     SET      (ADDRESS OF WAITWINI)
                     LENGTH   (LENGTH OF WAITWINI)
                     INITIMG  (LOVALUE)
           END-EXEC.

           MOVE POPUP-TEXT             TO WAIT-MESSAGE-O.
           EXEC CICS SEND
                     MAP      (WAITWIN)
                     FROM     (WAITWINO)
                     MAPSET   (MDSTMS)
                     TERMINAL
                     CURSOR(+1655)
                     WAIT
                     NOHANDLE
           END-EXEC.

           EXEC CICS FREEMAIN
                     DATA     (WAITWINI)
           END-EXEC.

       Q15-EXIT.
           EXIT.

       Q20-INITKEY-MAILDST.
           IF  ADDRESS OF MAILDST-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF MAILDST-RECORD)
                         LENGTH   (LENGTH OF MAILDST-RECORD)
               END-EXEC
           END-IF.
           MOVE SPACES                 TO MDST-KEY.
           SET  MDST-GRP-HEADER        TO TRUE.
       Q21-STARTBR-MAILDST.
           EXEC CICS STARTBR
                     DATASET  (MAILDST)
                     RIDFLD   (MDST-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q21-STARTEQ-MAILDST.
           EXEC CICS STARTBR
                     DATASET  (MAILDST)
                     RIDFLD   (MDST-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q22-RESETBR-MAILDST.
           EXEC CICS RESETBR
                     DATASET  (MAILDST)
                     RIDFLD   (MDST-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q23-READNEXT-MAILDST.
           MOVE LENGTH OF MAILDST-RECORD TO MDST-RECL.
           EXEC CICS READNEXT
                     DATASET  (MAILDST)
                     INTO     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     RIDFLD   (MDST-KEY)
                     NOHANDLE
           END-EXEC.
       Q23-READNX-DST-HDR.
           MOVE LENGTH OF MAILDST-RECORD TO MDST-RECL.
           EXEC CICS READNEXT
                     DATASET  (MAILDST)
                     INTO     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     RIDFLD   (MDST-KEY)
                     NOHANDLE
           END-EXEC.
       Q23-CHK-DST-HDR.
           IF  EIBRESP = DFHRESP(NORMAL)
               IF  NOT MDST-GRP-HEADER
                   GO TO Q23-READNX-DST-HDR
               END-IF
           END-IF.
       Q23-CHK-FILTER.
           IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q90-CHECK-FILTERS THRU Q95-EXIT
               IF  RECORD-NOT-SELECTED
                   GO TO Q23-READNX-DST-HDR
               END-IF
           END-IF.
       Q24-READPR-DST-HDR.
           MOVE LENGTH OF MAILDST-RECORD TO MDST-RECL.
           EXEC CICS READPREV
                     DATASET  (MAILDST)
                     INTO     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     RIDFLD   (MDST-KEY)
                     NOHANDLE
           END-EXEC.
       Q24-CHK-DST-HDR.
           IF  EIBRESP = DFHRESP(NORMAL)
               IF  NOT MDST-GRP-HEADER
                   GO TO Q24-READPR-DST-HDR
               END-IF
           END-IF.
       Q24-CHK-FILTER.
           IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q90-CHECK-FILTERS THRU Q95-EXIT
               IF  RECORD-NOT-SELECTED
                   GO TO Q24-READPR-DST-HDR
               END-IF
           END-IF.
       Q25-ENDBR-MAILDST.
           EXEC CICS ENDBR
                     DATASET  (MAILDST)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(INVREQ)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       Q26-READEQ-MAILDST.
           MOVE LENGTH OF MAILDST-RECORD TO MDST-RECL.
           EXEC CICS READ
                     DATASET  (MAILDST)
                     INTO     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     RIDFLD   (MDST-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q26-READGE-MAILDST.
           MOVE LENGTH OF MAILDST-RECORD TO MDST-RECL.
           EXEC CICS READ
                     DATASET  (MAILDST)
                     INTO     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     RIDFLD   (MDST-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q26-READUP-MAILDST.
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
       Q27-WRITE-MDST-HEADER.
           COMPUTE MDST-RECL = LENGTH OF MDST-KEY
                             + LENGTH OF MDST-HEADER.
           EXEC CICS WRITE
                     DATASET  (MAILDST)
                     FROM     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     RIDFLD   (MDST-KEY)
                     NOHANDLE
           END-EXEC.
       Q27-REWRITE-MDST-HEADER.
           COMPUTE MDST-RECL = LENGTH OF MDST-KEY
                             + LENGTH OF MDST-HEADER.
           EXEC CICS REWRITE
                     DATASET  (MAILDST)
                     FROM     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     NOHANDLE
           END-EXEC.
       Q27-WRITE-MDST-ENTRY.
           COMPUTE MDST-RECL = LENGTH OF MDST-KEY
                             + LENGTH OF MDST-ENTRY.
           EXEC CICS WRITE
                     DATASET  (MAILDST)
                     FROM     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     RIDFLD   (MDST-KEY)
                     NOHANDLE
           END-EXEC.
       Q27-REWRITE-MDST-ENTRY.
           COMPUTE MDST-RECL = LENGTH OF MDST-KEY
                             + LENGTH OF MDST-ENTRY.
           EXEC CICS REWRITE
                     DATASET  (MAILDST)
                     FROM     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     NOHANDLE
           END-EXEC.
       Q28-DELETE-MAILDST.
           EXEC CICS DELETE
                     DATASET  (MAILDST)
                     NOHANDLE
           END-EXEC.
       Q28-UNLOCK-MAILDST.
           EXEC CICS UNLOCK
                     DATASET  (MAILDST)
                     NOHANDLE
           END-EXEC.
       Q29-EXIT.
           EXIT.

       Q31-NUMITEMS-TS.
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
       Q32-READQ-TS.
           EXEC CICS READQ TS
                     QUEUE   (TS-QUEUE)
                     INTO    (TS-RECD)
                     ITEM    (SS-ITEM)
                     NOHANDLE
           END-EXEC.
       Q33-WRITEQ-TS.
           EXEC CICS WRITEQ TS
                     QUEUE   (TS-QUEUE)
                     FROM    (TS-RECD)
                     MAIN
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(NORMAL)
               MOVE SPACES             TO TS-RECD
           END-IF.
       Q34-REWRITEQ-TS.
           EXEC CICS WRITEQ TS
                     QUEUE   (TS-QUEUE)
                     FROM    (TS-RECD)
                     ITEM    (SS-ITEM)
                     REWRITE
                     MAIN
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(NORMAL)
               MOVE SPACES             TO TS-RECD
           END-IF.
       Q35-DELETEQ-TS.
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
       Q35-EXIT.
           EXIT.

/DLC0/*Q40-INITKEY-VOPERPU.
/DLC0/ Q40-INITKEY-VUSERID.
/DLC0/*    IF  ADDRESS OF VOPERID-RECORD = NULL
/DLC0/     IF  ADDRESS OF VUSERID-RECORD = NULL
/DLC0/*                  SET      (ADDRESS OF VOPERID-RECORD)
/DLC0/*                  LENGTH   (LENGTH OF VOPERID-RECORD)
               EXEC CICS GETMAIN
/DLC0/                   SET      (ADDRESS OF VUSERID-RECORD)
/DLC0/                   LENGTH   (LENGTH OF VUSERID-RECORD)
               END-EXEC
           END-IF.
/DLC0/*    MOVE LOW-VALUES             TO VOPERID-USERID.
/DLC0/     MOVE SPACES                 TO VUSER-KEY.
/DLC0/*Q46-READEQ-VOPERPU.
/DLC0/ Q46-READEQ-VUSERID.
/DLC0/*              DATASET  (VOPERPU)
/DLC0/*              INTO     (VOPERID-RECORD)
/DLC0/*              RIDFLD   (VOPERID-USERID)
           EXEC CICS READ
/DLC0/               DATASET  (VUSERID)
/DLC0/               INTO     (VUSERID-RECORD)
/DLC0/               RIDFLD   (VUSER-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q49-EXIT.
           EXIT.

       Q50-INITKEY-IESLDUM.
           IF  ADDRESS OF IESLDUM-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF IESLDUM-RECORD)
                         LENGTH   (LENGTH OF IESLDUM-RECORD)
               END-EXEC
           END-IF.
           MOVE SPACES                 TO LDUM-KEY.
           SET  LDUM-USRMAP-RECORD     TO TRUE.
       Q56-READEQ-IESLDUM.
           EXEC CICS READ
                     DATASET  (IESLDUM)
                     INTO     (IESLDUM-RECORD)
                     RIDFLD   (LDUM-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q56-READEQ-IESLDUV.
           EXEC CICS READ
                     DATASET  (IESLDUV)
                     INTO     (IESLDUM-RECORD)
                     RIDFLD   (LDUM-MFUSRID)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q49-EXIT.
           EXIT.

       Q80-SAVE-FILTERS.
           IF  LIST-GRPDESC-FILTER-L > ZERO
           OR  LIST-GRPDESC-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-GRPDESC-FILTER-I <= SPACES
               AND LIST-GRPDESC-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-GRPDESC-FILTER
               ELSE
                   MOVE LIST-GRPDESC-FILTER-I
                                       TO SAVE-GRPDESC-FILTER
               END-IF
           END-IF.
           MOVE SAVE-GRPDESC-FILTER    TO LIST-GRPDESC-FILTER-O.

           IF  LIST-USECOUNT-FILTER-L > ZERO
           OR  LIST-USECOUNT-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-USECOUNT-FILTER-I <= SPACES
               AND LIST-USECOUNT-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-USECOUNT-FILTER
               ELSE
                   MOVE LIST-USECOUNT-FILTER-I
                                       TO SAVE-USECOUNT-FILTER
                   EVALUATE TRUE
                   WHEN SAVE-USECOUNT-FILTER NOT NUMERIC-FILTER-CHARS
                       MOVE 'Use count filter is invalid.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-USECOUNT-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                   WHEN SAVE-USECOUNT-FILTER(1:1) = '<' OR '>'
                     SET NUM-REQUEST-NORMAL-EDIT TO TRUE
                     MOVE 09           TO NUM-TOTL
                     MOVE 0            TO NUM-DECM
                     MOVE SAVE-USECOUNT-FILTER(2:)
                                       TO NUM-DATA
                     CALL 'NUMMAN'  USING NUMMAN-PARMS
                     IF  NOT NUM-REQUEST-COMPLETED
                     OR  NUM-T09-D0 < ZEROES
                       MOVE 'Use count filter not numeric.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-USECOUNT-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                     ELSE
                       MOVE NUM-T09-D0 TO EDIT-NUMBER
                       MOVE EDIT-NUMBER TO UNEX-VARIABLE
                       PERFORM Q203-LEFT-JUSTIFY
                       MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO SAVE-USECOUNT-FILTER(2:)
                     END-IF
                   END-EVALUATE
               END-IF
           END-IF.
           MOVE SAVE-USECOUNT-FILTER   TO LIST-USECOUNT-FILTER-O.

           IF  LIST-LASTUSED-FILTER-L > ZERO
           OR  LIST-LASTUSED-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-LASTUSED-FILTER-I <= SPACES
               AND LIST-LASTUSED-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-LASTUSED-FILTER
               ELSE
                   MOVE LIST-LASTUSED-FILTER-I
                                       TO SAVE-LASTUSED-FILTER
                   EVALUATE TRUE
                   WHEN SAVE-LASTUSED-FILTER NOT NUMERIC-FILTER-CHARS
                       MOVE 'Last used date filter is invalid.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-LASTUSED-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                   WHEN (SAVE-LASTUSED-FILTER(1:1) = '<' OR '>')
                   AND SAVE-LASTUSED-FILTER(2:) > SPACES
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE SAVE-LASTUSED-FILTER(2:)
                                       TO DTE-GREG
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     IF  NOT DTE-REQUEST-COMPLETED
                       MOVE 'Last used filter has invalid date format.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-LASTUSED-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                     END-IF
                   END-EVALUATE
               END-IF
           END-IF.
           MOVE SAVE-LASTUSED-FILTER   TO LIST-LASTUSED-FILTER-O.

           IF  LIST-LASTUSEBY-FILTER-L > ZERO
           OR  LIST-LASTUSEBY-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-LASTUSEBY-FILTER-I <= SPACES
               AND LIST-LASTUSEBY-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-LASTUSEBY-FILTER
               ELSE
                   MOVE LIST-LASTUSEBY-FILTER-I
                                       TO SAVE-LASTUSEBY-FILTER
               END-IF
           END-IF.
           MOVE SAVE-LASTUSEBY-FILTER  TO LIST-LASTUSEBY-FILTER-O.

           IF  LIST-UPDATEDBY-FILTER-L > ZERO
           OR  LIST-UPDATEDBY-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-UPDATEDBY-FILTER-I <= SPACES
               AND LIST-UPDATEDBY-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-UPDATEDBY-FILTER
               ELSE
                   MOVE LIST-UPDATEDBY-FILTER-I
                                       TO SAVE-UPDATEDBY-FILTER
               END-IF
           END-IF.
           MOVE SAVE-UPDATEDBY-FILTER  TO LIST-UPDATEDBY-FILTER-O.

       Q85-EXIT.
           EXIT.

       Q90-CHECK-FILTERS.
           SET  WILDCOMP-PGM           TO TRUE.
           SET  RECORD-NOT-SELECTED    TO TRUE.

           IF  SAVE-GRPDESC-FILTER NOT = '*'
      * too big
      *        MOVE LENGTH OF SAVE-GRPDESC-FILTER
               MOVE LENGTH OF WILDCOMP-STR1
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-GRPDESC-FILTER(1:1)
               WHEN '<'
                   IF  SAVE-GRPDESC-FILTER(2:) <= SPACES
                     IF  MDST-GRP-DESC > SPACES
                       GO TO Q95-EXIT
                     END-IF
                     GO TO Q91-ACCEPTED-GRPDESC
                   ELSE
                     IF  MDST-GRP-DESC <= SPACES
                       GO TO Q95-EXIT
                     END-IF
                     MOVE SAVE-GRPDESC-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE MDST-GRP-DESC
                                       TO WILDCOMP-STR2
                   END-IF
               WHEN '>'
                   IF  SAVE-GRPDESC-FILTER(2:) <= SPACES
                     IF  MDST-GRP-DESC <= SPACES
                       GO TO Q95-EXIT
                     END-IF
                     GO TO Q91-ACCEPTED-GRPDESC
                   ELSE
                     MOVE SAVE-GRPDESC-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE MDST-GRP-DESC
                                       TO WILDCOMP-STR2
                   END-IF
               WHEN OTHER
                   MOVE SAVE-GRPDESC-FILTER
                                       TO WILDCOMP-STR1
                   MOVE MDST-GRP-DESC  TO WILDCOMP-STR2
               END-EVALUATE
               INSPECT WILDCOMP-STR1 CONVERTING LOWER-CASE
                                             TO UPPER-CASE
               INSPECT WILDCOMP-STR2 CONVERTING LOWER-CASE
                                             TO UPPER-CASE
               CALL 'WILDCOMP'      USING WILDCOMP-PARMS
               EVALUATE SAVE-GRPDESC-FILTER(1:1)
               WHEN '<'
                   IF  NOT WILDCOMP-STR1-GT-STR2
                       GO TO Q95-EXIT
                   END-IF
               WHEN '>'
                   IF  NOT WILDCOMP-STR1-LT-STR2
                       GO TO Q95-EXIT
                   END-IF
               WHEN OTHER
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

       Q91-ACCEPTED-GRPDESC.

           IF  SAVE-USECOUNT-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-USECOUNT-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-USECOUNT-FILTER(1:1)
               WHEN '<'
                   SET NUM-REQUEST-NORMAL-EDIT TO TRUE
                   MOVE 09             TO NUM-TOTL
                   MOVE 0              TO NUM-DECM
                   MOVE SAVE-USECOUNT-FILTER(2:)
                                       TO NUM-DATA
                   CALL 'NUMMAN'    USING NUMMAN-PARMS
                   IF  MDST-USAGE-COUNT = ZEROES
                   OR  MDST-USAGE-COUNT NOT < NUM-T09-D0
                       GO TO Q95-EXIT
                   END-IF
               WHEN '>'
                   SET NUM-REQUEST-NORMAL-EDIT TO TRUE
                   MOVE 09             TO NUM-TOTL
                   MOVE 0              TO NUM-DECM
                   MOVE SAVE-USECOUNT-FILTER(2:)
                                       TO NUM-DATA
                   CALL 'NUMMAN'    USING NUMMAN-PARMS
                   IF  MDST-USAGE-COUNT NOT > NUM-T09-D0
                       GO TO Q95-EXIT
                   END-IF
               WHEN OTHER
                   MOVE SAVE-USECOUNT-FILTER
                                       TO WILDCOMP-STR1
                   MOVE MDST-USAGE-COUNT
                                       TO EDIT-NUMBERC
                   MOVE EDIT-NUMBERC   TO WILDCOMP-STR2
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

           IF  SAVE-LASTUSED-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-LASTUSED-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-LASTUSED-FILTER(1:1)
               WHEN '<'
                   IF  MDST-LAST-USE-DATE <= ZEROES
                     GO TO Q95-EXIT
                   ELSE
                     IF  SAVE-LASTUSED-FILTER = '<'
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE CICS-CCYYMMDD TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       SET DTE-REQUEST-TOTL-DAYS TO TRUE
                       SUBTRACT 365  FROM DTE-TOTD
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     ELSE
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE SAVE-LASTUSED-FILTER(2:)
                                       TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     END-IF
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE MDST-LAST-USE-DATE
                                       TO DTE-GNUM
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     MOVE DTE-CCYYMMDD TO WILDCOMP-STR2
                     IF  WILDCOMP-STR1 < WILDCOMP-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN '>'
                   IF  MDST-LAST-USE-DATE <= ZEROES
                     GO TO Q95-EXIT
                   ELSE
                     IF  SAVE-LASTUSED-FILTER = '>'
                       MOVE CICS-CCYYMMDD TO WILDCOMP-STR1
                     ELSE
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE SAVE-LASTUSED-FILTER(2:)
                                       TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     END-IF
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE MDST-LAST-USE-DATE
                                       TO DTE-GNUM
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     MOVE DTE-CCYYMMDD TO WILDCOMP-STR2
                     IF  WILDCOMP-STR1 > WILDCOMP-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN OTHER
                   MOVE SAVE-LASTUSED-FILTER
                                       TO WILDCOMP-STR1
                   MOVE MDST-LAST-USE-DATE
                                       TO EDIT-DATE
                   MOVE EDIT-DATE      TO WILDCOMP-STR2
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

           IF  SAVE-LASTUSEBY-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-LASTUSEBY-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-LASTUSEBY-FILTER(1:1)
               WHEN '<'
                   IF  SAVE-LASTUSEBY-FILTER(2:) <= SPACES
                     IF  MDST-LAST-USED-BY > SPACES
                       GO TO Q95-EXIT
                     END-IF
                   ELSE
                     IF  MDST-LAST-USED-BY <= SPACES
                       GO TO Q95-EXIT
                     END-IF
                     MOVE SAVE-LASTUSEBY-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE MDST-LAST-USED-BY
                                       TO WILDCOMP-STR2
                     CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                     IF  NOT WILDCOMP-STR1-GT-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN '>'
                   IF  SAVE-LASTUSEBY-FILTER(2:) <= SPACES
                     IF  MDST-LAST-USED-BY <= SPACES
                       GO TO Q95-EXIT
                     END-IF
                   ELSE
                     MOVE SAVE-LASTUSEBY-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE MDST-LAST-USED-BY
                                       TO WILDCOMP-STR2
                     CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                     IF  NOT WILDCOMP-STR1-LT-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN OTHER
                   MOVE SAVE-LASTUSEBY-FILTER
                                       TO WILDCOMP-STR1
                   MOVE MDST-LAST-USED-BY
                                       TO WILDCOMP-STR2
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

           IF  SAVE-UPDATEDBY-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-UPDATEDBY-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-UPDATEDBY-FILTER(1:1)
               WHEN '<'
                   IF  SAVE-UPDATEDBY-FILTER(2:) <= SPACES
                     IF  MDST-UPDATED-BY > SPACES
                       GO TO Q95-EXIT
                     END-IF
                   ELSE
                     IF  MDST-UPDATED-BY <= SPACES
                       GO TO Q95-EXIT
                     END-IF
                     MOVE SAVE-UPDATEDBY-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE MDST-UPDATED-BY
                                       TO WILDCOMP-STR2
                     CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                     IF  NOT WILDCOMP-STR1-GT-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN '>'
                   IF  SAVE-UPDATEDBY-FILTER(2:) <= SPACES
                     IF  MDST-UPDATED-BY <= SPACES
                       GO TO Q95-EXIT
                     END-IF
                   ELSE
                     MOVE SAVE-UPDATEDBY-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE MDST-UPDATED-BY
                                       TO WILDCOMP-STR2
                     CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                     IF  NOT WILDCOMP-STR1-LT-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN OTHER
                   MOVE SAVE-UPDATEDBY-FILTER
                                       TO WILDCOMP-STR1
                   MOVE MDST-UPDATED-BY
                                       TO WILDCOMP-STR2
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

           SET  RECORD-IS-SELECTED     TO TRUE.
       Q95-EXIT.
           EXIT.

      /*****************************************************************
      *    UNEXPECTED ERRORS AND DEBUG LOGGING                         *
      ******************************************************************
       COPY LOGGINGP.

      /*****************************************************************
      *    PROGRAM EXIT TO 2ND-LEVEL PGM OR RETURN TO 1ST-LEVEL PGM    *
      ******************************************************************
       COPY COMMLINK.
      * the above copybook ends with a return to cics

      ******************************************************************
      *    TRANSFER TO THE CICS HELP SYSTEM                            *
      ******************************************************************
       COPY COMMHELP.
      * the above copybook ends with a return to cics

      /*****************************************************************
      *    VERIFICATION ROUTINE                                        *
      ******************************************************************
       V10-VERIFY-DELETE.
           MOVE AIDENTER               TO EIBAID.
           MOVE 1                      TO POS.
      *                         row  adjust            col  adjust
           COMPUTE HALF-WORD = (025 - 1) * SCRNWDTH + (001 - 1).
           STRING SCRSETBA TWO-BYTES SCRSTFEX X'02'
                  TYPFIELD FLDASBRT  TYPHILIT EXHBLINK
               DELIMITED BY SIZE INTO VAR-DATA WITH POINTER POS.
           STRING MULTI-EDIT-MSG(1:LEN)
               DELIMITED BY SIZE INTO VAR-DATA WITH POINTER POS.
           EVALUATE TRUE
           WHEN LEN < LENGTH OF MULTI-EDIT-MSG - 2
      *                             row  adjust            col  adjust
               COMPUTE HALF-WORD = (025 - 1) * SCRNWDTH + (080 - 1)
               STRING SCREPEAT TWO-BYTES X'40'
                      SCRSTFLD FLDASKIP DELIMITED BY SIZE
                   INTO VAR-DATA WITH POINTER POS
           WHEN LEN < LENGTH OF MULTI-EDIT-MSG
               STRING SCRSTFLD FLDASKIP DELIMITED BY SIZE
                   INTO VAR-DATA WITH POINTER POS
           END-EVALUATE.
           SUBTRACT 1                FROM POS.

           EXEC CICS CONVERSE
                     FROM     (VAR-DATA)
                     FROMLENGTH(POS)
                     CTLCHAR  (WRTFKFSA)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(EOC)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       V15-EXIT.
           EXIT.

      /*****************************************************************
      *    PROGRAM ERRORS (UNEXPECTED)                                 *
      ******************************************************************
       COPY UNEXERRP.
      * the above copybook ends with a return to cics

      /*****************************************************************
      *    PROGRAM EXIT ROUTINES (NORMAL)                              *
      ******************************************************************

       Z70-MDST-CLOSED.

           EXEC CICS SET TERMINAL(EIBTRMID)
                     UCTRAN
                     NOHANDLE
           END-EXEC.

           COPY COMMXCTL.
      * the above transfers control to standard NOT OPEN screen

       Z80-MDST-RETURN.

           COPY COMMRETN.
      * the above copybook is a psuedo-conversational return to cics

       Z90-MDST-TERMINATION.

           EXEC CICS SET TERMINAL(EIBTRMID)
                     UCTRAN
                     NOHANDLE
           END-EXEC.

      * use the following if you don't want to pass back any data
           COPY COMMEXIT REPLACING DFHCOMMAREA BY COMM-HEADER.

      * the above copybook ends with a return to cics and GOBACK

