 CBL XOPTS(SP)
      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    MMSGPGM.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  MAY 2019.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      TRANSACTION SECURITY, ONLY.
      *REMARKS.       MAINFRAME MAIL MESSAGE MAINTENANCE.

      * CHANGE HISTORY ------------------------------------------------
      * 09/29/2006 DLC ORIGINAL PROGRAM.
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
           CLASS ALPHA-CHARACTERS IS 'A' THRU 'I',
                                     'J' THRU 'R',
                                     'S' THRU 'Z',
           CLASS LINE-COMMAND     IS ' ', 'I', 'C', 'M',
                                     'A', 'B', '"', 'D',
           CLASS NUMERIC-DIGIT    IS '0' THRU '9',
           CLASS NUMERIC-FILTER-CHARS IS ' ', '?', '*', '<', '>',
                                         '0' THRU '9',
           CLASS POSITIVE-DIGIT   IS '1' THRU '9'.

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
         COPY COMMWORK       REPLACING =='TRAN'==    BY =='MMSG'==
                                       =='PROGRAM'== BY =='MMSGPGM'==
                                       =='LINKPGM'== BY =='MDSTPGM'==.
         03  MMSGMS                    PIC  X(08)   VALUE 'MMSGMS  '.
         03  LISTMAP                   PIC  X(08)   VALUE 'LISTMAP '.
         03  DETLMAP                   PIC  X(08)   VALUE 'DETLMAP '.
         03  DISTMAP                   PIC  X(08)   VALUE 'DISTMAP '.
      *
      * FIELDS TO MANAGE REFERENCED FILE NAMES
         03  MAILMID                   PIC  X(08)   VALUE 'MAILMID '.
         03  MAILMSG                   PIC  X(08)   VALUE 'MAILMSG '.
         03  MAILMDS                   PIC  X(08)   VALUE 'MAILMDS '.
         03  MAILDST                   PIC  X(08)   VALUE 'MAILDST '.
      *
      * FIELDS TO MANAGE REFERENCED EXTERNAL PROGRAM NAMES
         03  JOBSSELT                  PIC  X(08)   VALUE 'JOBSSELT'.
         03  MDSTPGM                   PIC  X(08)   VALUE 'MDSTPGM'.
         03  MULTMESG                  PIC  X(08)   VALUE 'MULTMESG'.
      *
      * FIELDS TO MANAGE ERROR HANDLING
         03  ERROR-SWITCH              PIC S9(01)   VALUE ZEROES.
           88  NO-ERRORS-FOUND                      VALUE ZEROES.
           88  ERROR-AT-CURSOR                      VALUE -1.
           88  ERRORS-FOUND                         VALUES -9 THRU -1.
         03  EOD-SWITCH                PIC S9(01)   VALUE -1.
           88  EOD-NOT-MARKED                       VALUE -1.
           88  EOD-MARKED                           VALUE ZEROES.
         03  RECORD-SWITCH             PIC  X(01)   VALUE 'N'.
           88  RECORD-NOT-SELECTED                  VALUE 'N'.
           88  RECORD-IS-SELECTED                   VALUE 'Y'.
         03  NOTFOUND-SWITCH           PIC  X(01)   VALUE 'Y'.
           88  RECORD-WAS-FOUND                     VALUE 'Y'.
           88  RECORD-NOT-FOUND                     VALUE 'N'.
         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.
         03  CNT                       PIC S9(04)   BINARY VALUE ZEROES.
         03  LEN                       PIC S9(04)   BINARY VALUE ZEROES.
         03  POS                       PIC S9(04)   BINARY VALUE ZEROES.
         03  PTR                       PIC S9(04)   BINARY VALUE ZEROES.
         03  SUB                       PIC S9(04)   BINARY VALUE ZEROES.
         03  MMID-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
         03  MMSG-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
         03  MDST-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
      *
      * FIELDS TO CALCULATE CURSOR POSITION AND MANAGE MAPS W/ OCCURS
         03  ROW                       PIC S9(04)   BINARY VALUE ZEROES.
         03  COL                       PIC S9(04)   BINARY VALUE ZEROES.
         03  LISTHEAD                  PIC S9(04)   BINARY VALUE +06.
         03  LISTROWS                  PIC S9(04)   BINARY VALUE +18.
         03  DETLHEAD                  PIC S9(04)   BINARY VALUE +09.
         03  DETLROWS                  PIC S9(04)   BINARY VALUE +15.
         03  DETLPRFX                  PIC S9(04)   BINARY VALUE +07.
         03  DETL-MAX                  PIC S9(04)   BINARY VALUE +3000.
         03  DISTHEAD                  PIC S9(04)   BINARY VALUE +08.
         03  DISTROWS                  PIC S9(04)   BINARY VALUE +16.
      *
         03  NNN                       PIC  999.
         03  MMID-VAL                  PIC  9(18).
         03  MMID-EDIT                 PIC  Z(18).
         03  POPUP-TEXT                PIC  X(15)   VALUE SPACES.
         03  LOWER-CASE  PIC  X(26)  VALUE 'abcdefghijklmnopqrstuvwxyz'.
         03  UPPER-CASE  PIC  X(26)  VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
         03  MULTI-EDIT-MSG            PIC  X(78)   VALUE SPACES.
         03  VAR-DATA                  PIC  X(98)   VALUE SPACES.
         03  VAR-FINDL                 PIC S9(4)    BINARY.
         03  VAR-FIND                  PIC  X(124)  VALUE SPACES.
      *
      * TO CONVERT NUMBERS TO ZONED-DECIMAL CHARACTER
         03  NUM-EDIT                  PIC  Z(8)9.
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
         03  NUM-TIME                  PIC  9(6).
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

      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS

       01  CICSINFO-PARMS.
         COPY CICSINFO.

       COPY DTEMAN.

       COPY HEXMAN.

       COPY LOGGING.

       COPY NUMMAN.

       COPY TXTMAN.

       COPY UNEXERRW.
           05  CONFIRM-MSG   REDEFINES UNEX-MSG     PIC  X(79).

       COPY WILDCOMP.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

      * MMSG CICS COMMUNICATION AREA
       01  DFHCOMMAREA.
         COPY COMMAREA.
      *
           05  MMSG-SAVEAREA           PIC  X(25).
           05  MMSG-SAVEKEY            PIC  9(18).
           05  MMSG-FILTERS.
             07  SAVE-CREATED-FILTER   PIC  X(09).
             07  SAVE-CREATET-FILTER   PIC  X(07).
             07  SAVE-SENTDATE-FILTER  PIC  X(09).
             07  SAVE-SENTTIME-FILTER  PIC  X(07).
             07  SAVE-SYSTEM-FILTER    PIC  X(02).
             07  SAVE-TO-GRP-FILTER    PIC  X(25).
             07  SAVE-SUBJECT-FILTER   PIC  X(50).
           05  MMSG-PROCESS            PIC  X(01).
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
             07  TS-SELECT             PIC  X(5).
             07  TS-DATA.
               09  TS-GRPNAME          PIC  X(25).
               09  TS-GRPDESC          PIC  X(99).
           05  SAVED-DATA              PIC  X(124).

           05  T2-QUEUE.
             07  T2-TERM               PIC  X(4).
             07  T2-TRAN               PIC  X(4).
           05  T2-TOTL                 PIC S9(4)    BINARY.
           05  T2-ITEM                 PIC S9(4)    BINARY.
           05  T2-RECD.
             07  T2-SELECT             PIC  X(5).
             07  T2-DATA               PIC  X(124).

           05  MMSG-LIST-TIOA          PIC  X(3456).
           05  MMSG-SAVE-TIOA          PIC  X(2739).

      * MMSG TEMP COMMUNICATION AREA
       COPY COMMTEMP.

      * 3270 DATA STREAM CONTROL CHARACTERS
       COPY IBM3270.

      * MAILMID RECORD I/O AREA
       COPY MAILMID.

      * MAILMSG RECORD I/O AREA
       COPY MAILMSG.

      * MAILDST RECORD I/O AREA
       COPY MAILDST.

      * MAILMDS RECORD I/O AREA
       COPY MAILMDS.

      * BMS TERMINAL I/O AREAS
       COPY MMSGMS.

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *    PROGRAM INITIALIZATION                                      *
      ******************************************************************
       A00-MMSG-PROGRAM-ENTRY.

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
                     DISABLED (Z70-MMSG-CLOSED)
                     NOTOPEN  (Z70-MMSG-CLOSED)
           END-EXEC.

           IF  COMM-CURRPGM = THIS-PGM
      * psuedo-conversational program continuation
               GO TO B50-MMSG-ROUTINE-SELECTION
           END-IF.

       B00-MMSG-FIRST-TIME-ONLY.

           COPY COMMINIT.

           IF  COMM-CURRRTN = 'RETN'
           AND COMM-SAVE-FUNC > SPACES
      * return from 2nd-level program (such as the HELP system)
               GO TO B50-MMSG-ROUTINE-SELECTION
           END-IF.

      * the following executes one-time-only per user session

           IF  COMM-CURRRTN NOT = 'RETN'
                              AND 'EXIT'
               MOVE ZEROES             TO MMSG-SAVEKEY
           END-IF.

      * set default initial filter value
           MOVE '*'                    TO SAVE-CREATED-FILTER
                                          SAVE-CREATET-FILTER
                                          SAVE-SENTDATE-FILTER
                                          SAVE-SENTTIME-FILTER
                                          SAVE-SYSTEM-FILTER
                                          SAVE-TO-GRP-FILTER
                                          SAVE-SUBJECT-FILTER.

           MOVE LOW-VALUES             TO SAVED-DATA.

      * this program requires mixed-case terminal input
           EXEC CICS SET TERMINAL(EIBTRMID)
                     TRANIDONLY
                     NOHANDLE
           END-EXEC.

       B50-MMSG-ROUTINE-SELECTION.

           IF (COMM-CURRRTN = 'RETN' OR 'EXIT')
           AND COMM-SAVE-FUNC > SPACES
               MOVE COMM-CURRRTN       TO INIT-FLAG
               MOVE COMM-SAVE-FUNC     TO COMM-CURRRTN
               MOVE INIT-FLAG          TO COMM-SAVE-FUNC
           END-IF.

           EVALUATE COMM-CURRRTN
           WHEN 'DIST' GO TO E00-DIST-ROUTINE
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
                   MOVE MMSG-LIST-TIOA TO LISTMAPO
                   MOVE LOW-VALUES     TO MMSG-LIST-TIOA
               END-IF
               PERFORM C80-LIST-OUTPUT THRU C85-LIST-SENDMAP
               GO TO C50-LIST-DISPLAY
           END-IF.

       C10-LIST-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (LISTMAP)
                     INTO     (LISTMAPI)
                     MAPSET   (MMSGMS)
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
               GO TO Z90-MMSG-TERMINATION

           WHEN AIDPFK03
               GO TO Z90-MMSG-TERMINATION

           WHEN AIDPFK01
               MOVE LISTMAPI           TO MMSG-LIST-TIOA
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
               MOVE SPACES             TO MMSG-SAVEAREA
               MOVE 'EXIT'             TO COMM-TRANSID
               MOVE MDSTPGM            TO COMM-LINKPGM
               MOVE LENGTH OF COMM-HEADER
                                       TO LINK-LEN
               GO TO T02-LINK-CONTINUE
           END-IF.

      * convert cursor position to row and column coordinates
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.
      *

           INSPECT LIST-SYSTEM-FILTER-I CONVERTING LOWER-CASE
                                                TO UPPER-CASE
           INSPECT LIST-TO-GRP-FILTER-I CONVERTING LOWER-CASE
                                                TO UPPER-CASE

           IF  LIST-MSGID-SKIPTO-L    > ZERO
           OR  LIST-MSGID-SKIPTO-F    = FLDMODFY OR FLDCURSM
             IF  LIST-MSGID-SKIPTO-I > SPACES
               MOVE LIST-MSGID-SKIPTO-I
                                       TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               IF  UNEX-VARIABLE(1:UNEX-LENG) NOT NUMERIC-DIGIT
                 MOVE 'Message id skip-to must be a number.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-MSGID-SKIPTO-L
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
                 GO TO C80-LIST-OUTPUT
               END-IF
             END-IF
           END-IF.

           IF  LIST-MSGID-SKIPTO-L    > ZERO
           OR  LIST-MSGID-SKIPTO-F    = FLDMODFY OR FLDCURSM
           OR  LIST-CREATED-FILTER-L  > ZERO
           OR  LIST-CREATED-FILTER-F  = FLDMODFY OR FLDCURSM
           OR  LIST-CREATET-FILTER-L  > ZERO
           OR  LIST-CREATET-FILTER-F  = FLDMODFY OR FLDCURSM
           OR  LIST-SENTDATE-FILTER-L > ZERO
           OR  LIST-SENTDATE-FILTER-F = FLDMODFY OR FLDCURSM
           OR  LIST-SENTTIME-FILTER-L > ZERO
           OR  LIST-SENTTIME-FILTER-F = FLDMODFY OR FLDCURSM
           OR  LIST-SYSTEM-FILTER-L   > ZERO
           OR  LIST-SYSTEM-FILTER-F   = FLDMODFY OR FLDCURSM
           OR  LIST-TO-GRP-FILTER-L   > ZERO
           OR  LIST-TO-GRP-FILTER-F   = FLDMODFY OR FLDCURSM
           OR  LIST-SUBJECT-FILTER-L  > ZERO
           OR  LIST-SUBJECT-FILTER-F  = FLDMODFY OR FLDCURSM
               GO TO C50-LIST-DISPLAY
           END-IF.

      * scroll to top or bottom of the list
           IF  EIBAID = AIDPFK06 OR AIDPFK09
               PERFORM Q20-INITKEY-MAILMID
               IF  EIBAID = AIDPFK06
                   PERFORM Q21-STARTBR-MAILMID
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No mail message ids found.'
                                       TO LIST-MESSAGE-O
                         GO TO C50-LIST-DISPLAY
                       END-IF
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q23-READNEXT-MAILMID THRU Q23-CHK-FILTER
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   MOVE MMID-ID        TO MMID-EDIT
                   IF  MMID-EDIT = LIST-MSGID-I(1)
                   OR  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first page.'
                                       TO LIST-MESSAGE-O
                   END-IF
               ELSE
                   MOVE HIGH-VALUES    TO MMID-KEY
                   PERFORM Q21-STARTEQ-MAILMID
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No mail message ids found.'
                                       TO LIST-MESSAGE-O
                         GO TO C50-LIST-DISPLAY
                       END-IF
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q24-READPREV-MAILMID THRU Q24-CHK-FILTER
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   MOVE MMID-ID        TO MMID-EDIT
                   IF  MMID-EDIT = LIST-MSGID-I(1)
                   OR  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the last page.'
                                       TO LIST-MESSAGE-O
                   END-IF
               END-IF
               MOVE MMID-ID            TO LIST-MSGID-O(1)
               PERFORM Q25-ENDBR-MAILMID
               GO TO C50-LIST-DISPLAY
           END-IF.

      * adjust row and column coordinates for header/detail screens
           SUBTRACT LISTHEAD         FROM ROW.
      *    COMPUTE COL = (COL + ((SCRNWDTH - LISTCOLS) / LISTCOLS))
      *                       / ((SCRNWDTH - LISTCOLS) / LISTCOLS).

      * scroll to previous or next page
           IF  EIBAID = AIDPFK07 OR AIDPFK08
               PERFORM Q20-INITKEY-MAILMID
               MOVE LIST-MSGID-I(1)    TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMID-ID
               IF  EIBAID = AIDPFK07
                   IF  ROW < 1 OR >= LISTROWS
                   OR  LIST-MSGID-I(ROW) <= SPACES
                       COMPUTE IDX = LISTROWS
                   ELSE
                       COMPUTE IDX = LISTROWS - ROW
                   END-IF
                   PERFORM Q21-STARTBR-MAILMID
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No mail message ids found.'
                                       TO LIST-MESSAGE-O
                         GO TO C50-LIST-DISPLAY
                       END-IF
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q23-READNEXT-MAILMID
                   PERFORM Q24-READPREV-MAILMID THRU Q24-CHK-FILTER
                     VARYING IDX FROM IDX BY -1
                       UNTIL IDX < ZERO
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first entry.'
                                       TO LIST-MESSAGE-O
                       PERFORM Q23-READNEXT-MAILMID THRU Q23-CHK-FILTER
                       IF  EIBRESP NOT = DFHRESP(NORMAL)
                                     AND DFHRESP(ENDFILE)
                           GO TO C70-LIST-ERRORS
                       END-IF
                   END-IF
               ELSE
                   IF  ROW <= 1 OR > LISTROWS
                   OR  LIST-MSGID-I(ROW) <= SPACES
                       COMPUTE IDX = LISTROWS
                   ELSE
                       COMPUTE IDX = ROW - 1
                   END-IF
                   PERFORM Q21-STARTBR-MAILMID
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       IF  EIBRESP = DFHRESP(NOTFND)
                         MOVE 'No mail message ids found.'
                                       TO LIST-MESSAGE-O
                         GO TO C50-LIST-DISPLAY
                       END-IF
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q23-READNEXT-MAILMID THRU Q23-CHK-FILTER
                     VARYING IDX FROM IDX BY -1
                       UNTIL IDX < ZERO
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the last entry.'
                                       TO LIST-MESSAGE-O
                       PERFORM Q24-READPREV-MAILMID THRU Q24-CHK-FILTER
                       IF  EIBRESP NOT = DFHRESP(NORMAL)
                                     AND DFHRESP(ENDFILE)
                           GO TO C70-LIST-ERRORS
                       END-IF
                   END-IF
               END-IF
               MOVE MMID-ID            TO LIST-MSGID-O(1)
               PERFORM Q25-ENDBR-MAILMID
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
               GO TO C45-EXIT

           WHEN 'V'
           WHEN 'A'
           WHEN 'C'
               IF  LIST-MSGID-I(IDX) NOT > SPACES
                   IF  LIST-SELECT-I(IDX) NOT = 'A'
                     MOVE LOW-VALUES   TO LIST-SELECT-I(IDX)
                   END-IF
                   GO TO C45-EXIT
               END-IF
      * retrieve message attributes
               PERFORM Q20-INITKEY-MAILMID
               MOVE LIST-MSGID-I(IDX)  TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMID-ID
               PERFORM Q26-READEQ-MAILMID
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO C45-EXIT
               END-IF
      * private messages cannot be touched
               IF  MMID-IS-PRIVATE
               AND MMID-FROM NOT = CICS-USERID
               AND MMID-CREATED-BY NOT = CICS-USERID
                   MOVE 'Message is private.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
                   GO TO C45-EXIT
               END-IF
      * sent messages cannot be modified except by creator
               IF  LIST-SELECT-I(IDX) = 'C'
               AND MMID-SENT-TIMESTAMP > ZEROES
               AND MMID-CREATED-BY NOT = CICS-USERID
                   MOVE 'Message is already sent.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
                   GO TO C45-EXIT
               END-IF
               GO TO C45-EXIT

           WHEN 'D'
               IF  LIST-MSGID-I(IDX) NOT > SPACES
                   MOVE LOW-VALUES     TO LIST-SELECT-I(IDX)
                   GO TO C45-EXIT
               END-IF
      * retrieve message attributes
               PERFORM Q20-INITKEY-MAILMID
               MOVE LIST-MSGID-I(IDX)  TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMID-ID
               PERFORM Q26-READEQ-MAILMID
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO C45-EXIT
               END-IF
      * private messages cannot be deleted
               IF  MMID-IS-PRIVATE
               AND MMID-FROM NOT = CICS-USERID
               AND MMID-CREATED-BY NOT = CICS-USERID
                   MOVE 'Message is private.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
                   GO TO C45-EXIT
               END-IF
      * confirm delete processing
               COMPUTE EIBCPOSN = (LISTHEAD + IDX - 1) * SCRNWDTH + 1
               EXEC CICS SEND CONTROL CURSOR(EIBCPOSN) END-EXEC
               MOVE SPACES             TO MULTI-EDIT-MSG
               MOVE 1                  TO LEN
               STRING 'Delete this mail message?  '
                      'Press F2 to confirm.' DELIMITED BY SIZE
                   INTO MULTI-EDIT-MSG WITH POINTER LEN
               SUBTRACT 1            FROM LEN
               PERFORM V10-VERIFY-DELETE THRU V15-EXIT
               IF  EIBAID  NOT = AIDPFK02
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE LOW-VALUES     TO LIST-SELECT-I(IDX)
                   GO TO C45-EXIT
               END-IF
      * delete mail message body text
               PERFORM Q40-INITKEY-MAILMSG
               MOVE LIST-MSGID-I(IDX)  TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMSG-ID
               EXEC CICS DELETE
                         DATASET(MAILMSG)
                         RIDFLD(MMSG-ID)
                         KEYLENGTH(LENGTH OF MMSG-ID)
                         GENERIC
                         NOHANDLE
               END-EXEC
      * delete mail message distributions
               IF  EIBRESP = DFHRESP(NORMAL)
                          OR DFHRESP(NOTFND)
                   PERFORM Q70-INITKEY-MAILMDS
                   MOVE MMSG-ID        TO MMDS-ID
                   EXEC CICS DELETE
                             DATASET(MAILMDS)
                             RIDFLD(MMDS-ID)
                             KEYLENGTH(LENGTH OF MMDS-ID)
                             GENERIC
                             NOHANDLE
                   END-EXEC
               END-IF
      * delete mail message id header
               IF  EIBRESP = DFHRESP(NORMAL)
                          OR DFHRESP(NOTFND)
                   PERFORM Q20-INITKEY-MAILMID
                   MOVE MMSG-ID        TO MMID-ID
                   EXEC CICS DELETE
                             DATASET(MAILMID)
                             RIDFLD(MMID-KEY)
                             NOHANDLE
                   END-EXEC
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
                   MOVE LOW-VALUES     TO LISTMAP-ROW-DETAIL(IDX)
               END-IF

           WHEN 'R'
               IF  LIST-MSGID-I(IDX) NOT > SPACES
                   MOVE LOW-VALUES     TO LIST-SELECT-I(IDX)
                   GO TO C45-EXIT
               END-IF
               MOVE LIST-MSGID-I(IDX)  TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMSG-SAVEKEY
               PERFORM P90-RESEND-MAIL-MESSAGE THRU P95-EXIT

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
           IF  LIST-MSGID-SKIPTO-L > ZERO
           OR  LIST-MSGID-SKIPTO-F = FLDMODFY OR FLDCURSM
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
                   IF  LIST-MSGID-I(ROW) NOT > SPACES
                   AND LIST-SELECT-I(ROW) NOT = 'A'
                       MOVE LOW-VALUES TO LIST-SELECT-O(ROW)
                       GO TO C60-LIST-DETAIL
                   END-IF
                   MOVE LIST-MSGID-I(ROW) TO UNEX-VARIABLE
                   PERFORM Q203-LEFT-JUSTIFY
                   MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMSG-SAVEKEY
                   MOVE LIST-SELECT-I(ROW)
                                       TO MMSG-PROCESS
                   IF  MMSG-SAVEKEY = ZERO
                   AND ADD-PROCESS
                       MOVE 1          TO MMSG-SAVEKEY
                   END-IF
                   MOVE '*'            TO LIST-SELECT-O(ROW)
                   MOVE LISTMAPI       TO MMSG-LIST-TIOA
                   GO TO D00-DETL-ROUTINE
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

           PERFORM Q20-INITKEY-MAILMID.
           EVALUATE TRUE
           WHEN LIST-MSGID-SKIPTO-L > ZERO
            AND LIST-MSGID-SKIPTO-I > SPACES
               MOVE LIST-MSGID-SKIPTO-I TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMID-ID
                                          MMID-VAL
               PERFORM Q21-STARTBR-MAILMID
               IF  EIBRESP = DFHRESP(NOTFND)
                   MOVE HIGH-VALUES    TO MMID-KEY
                   PERFORM Q21-STARTEQ-MAILMID
                   PERFORM Q24-READPREV-MAILMID THRU Q24-CHK-FILTER
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
                   PERFORM Q23-READNEXT-MAILMID THRU Q23-CHK-FILTER
               END-IF
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO C70-LIST-ERRORS
               END-IF
               IF  MMID-VAL < MMID-ID
                   PERFORM Q24-READPREV-MAILMID THRU Q24-CHK-FILTER
                     VARYING IDX FROM 1 BY 1 UNTIL IDX > 2
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first entry.'
                                       TO LIST-MESSAGE-O
                   END-IF
               END-IF
               PERFORM Q25-ENDBR-MAILMID
           WHEN MMSG-SAVEKEY > ZEROES
               MOVE MMSG-SAVEKEY       TO MMID-ID
           WHEN LIST-MSGID-I(1) > SPACES
               MOVE LIST-MSGID-I(1)    TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMID-ID
           WHEN OTHER
               MOVE HIGH-VALUES        TO MMID-KEY
               PERFORM Q21-STARTEQ-MAILMID
               PERFORM Q24-READPREV-MAILMID THRU Q24-CHK-MAILMID
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > LISTROWS
                      OR EIBRESP NOT = DFHRESP(NORMAL)
               PERFORM Q25-ENDBR-MAILMID
               MOVE 'This is the last entry.'
                                       TO LIST-MESSAGE-O
           END-EVALUATE.

           PERFORM Q21-STARTBR-MAILMID.
           IF  EIBRESP = DFHRESP(NOTFND)
               MOVE 1                  TO MMID-ID
               PERFORM Q21-STARTBR-MAILMID
               IF  EIBRESP = DFHRESP(NOTFND)
                   MOVE DFHRESP(NORMAL) TO EIBRESP
                   MOVE 2              TO IDX
                   GO TO C65-LIST-BLANKS
               END-IF
           END-IF.

           MOVE ZEROES                 TO MMSG-SAVEKEY.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1
               UNTIL IDX > LISTROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

               PERFORM Q23-READNEXT-MAILMID THRU Q23-CHK-FILTER
               IF  EIBRESP = DFHRESP(NORMAL)

      * BUILD DETAIL LINE OUTPUT HERE
                   MOVE LOW-VALUES     TO LISTMAP-ROW-DETAIL(IDX)
                   MOVE MMID-ID        TO LIST-MSGID-O(IDX)
                   EXEC CICS FORMATTIME
                             ABSTIME(MMID-CREATED-TIMESTAMP)
                             YYYYMMDD(LIST-CREATED-O(IDX))
                             TIME(LIST-CREATET-O(IDX))
                   END-EXEC
                   MOVE MMID-COMPLETE  TO LIST-COMPLETE-O(IDX)
                   IF  MMID-SENT-TIMESTAMP > ZEROES
                       EXEC CICS FORMATTIME
                                 ABSTIME(MMID-SENT-TIMESTAMP)
                                 YYYYMMDD(LIST-SENTDATE-O(IDX))
                                 TIME(LIST-SENTTIME-O(IDX))
                       END-EXEC
                   END-IF
                   MOVE MMID-PRIVATE   TO LIST-PRIVATE-O(IDX)
                   MOVE MMID-SYSTEM    TO LIST-SYSTEM-O(IDX)
                   MOVE MMID-TO-GRP    TO LIST-TO-GRP-O(IDX)
                   MOVE MMID-SUBJECT   TO LIST-SUBJECT-O(IDX)

                   PERFORM Q70-INITKEY-MAILMDS
                   MOVE MMID-ID          TO MMDS-ID
                   PERFORM Q76-READGT-MAILMDS
                   IF  EIBRESP = DFHRESP(NORMAL)
                   AND MMDS-ID = MMID-ID
                     MOVE '...'        TO LIST-TO-GRP-O(IDX)(23:3)
                   ELSE
                     MOVE DFHRESP(NORMAL) TO EIBRESP
                   END-IF

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

           PERFORM Q25-ENDBR-MAILMID.

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
                             MAPSET   (MMSGMS)
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
                             MAPSET   (MMSGMS)
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
                         MAPSET   (MMSGMS)
                         TERMINAL
                         FREEKB
                         ERASE
               END-EXEC
           END-IF.

       C90-LIST-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (LISTMAPI)
           END-EXEC.

           GO TO Z80-MMSG-RETURN.

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
                   MOVE MMSG-SAVE-TIOA TO DETLMAPO
                   MOVE LOW-VALUES     TO MMSG-SAVE-TIOA
                   IF  MMSG-SAVEAREA > SPACES
                   AND COMM-HELPAREA(1:4) NOT = 'HELP'
                     MOVE AIDENTER     TO EIBAID
                     MOVE LENGTH OF DETL-TO-GRP-I
                                       TO DETL-TO-GRP-L
                     MOVE FLDCURSM     TO DETL-TO-GRP-F
                     MOVE MMSG-SAVEAREA
                                       TO DETL-TO-GRP-I
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
                     MAPSET   (MMSGMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO D70-DETL-ERRORS
           END-IF.

       D20-DETL-KEYS.

           IF  EIBAID = AIDPFK10
               MOVE DETL-MSGID-I       TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMSG-SAVEKEY
               PERFORM P90-RESEND-MAIL-MESSAGE THRU P95-EXIT
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO D70-DETL-ERRORS
               END-IF
               MOVE AIDPFK03           TO EIBAID
           END-IF.

           EVALUATE EIBAID
           WHEN AIDCLEAR
               MOVE JOBSSELT           TO COMM-LINKPGM
               GO TO Z90-MMSG-TERMINATION

           WHEN AIDPFK03
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE 'RETN'             TO COMM-CURRRTN
               MOVE DETL-MSGID-I       TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMSG-SAVEKEY
               GO TO C00-LIST-ROUTINE

           WHEN AIDPFK01
               MOVE DETLMAPI           TO MMSG-SAVE-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDENTER
           WHEN AIDPFK02
           WHEN AIDPFK04
           WHEN AIDPFK05
           WHEN AIDPFK06
           WHEN AIDPFK07
           WHEN AIDPFK08
           WHEN AIDPFK09
           WHEN AIDPFK14
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

           IF  EIBAID = AIDPFK05
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
           SUBTRACT DETLHEAD         FROM ROW.

      * PERFORM DETAIL FIELD EDITING HERE

           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           IF  NOT VIEW-PROCESS

           INSPECT DETL-SYSTEM-I CONVERTING LOWER-CASE
                                         TO UPPER-CASE
           INSPECT DETL-TO-GRP-I CONVERTING LOWER-CASE
                                         TO UPPER-CASE
           INSPECT DETL-COMPLETE-I CONVERTING LOWER-CASE
                                           TO UPPER-CASE
           INSPECT DETL-DELIVERY-I CONVERTING LOWER-CASE
                                           TO UPPER-CASE
           INSPECT DETL-PRIVATE-I CONVERTING LOWER-CASE
                                          TO UPPER-CASE

           IF  DETL-SYSTEM-L       > ZERO
           OR  DETL-SYSTEM-F       = FLDMODFY OR FLDCURSM
             IF  DETL-SYSTEM-I > SPACES
             AND DETL-SYSTEM-I NOT ALPHA-CHARACTERS
               MOVE 'System code must be alphabetic.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-SYSTEM-L
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
             END-IF
           END-IF

           IF  DETL-TO-GRP-L       > ZERO
           OR  DETL-TO-GRP-F       = FLDMODFY OR FLDCURSM
             IF  DETL-TO-GRP-I <= SPACES
               MOVE 'Send to group name is required.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-TO-GRP-L
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
             ELSE
               PERFORM Q50-INITKEY-MAILDST
               MOVE DETL-TO-GRP-I      TO MDST-GRP-NAME
               PERFORM Q56-READEQ-MAILDST
               EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
                 MOVE MDST-GRP-DESC    TO DETL-TO-DESC-O
                 PERFORM Q20-INITKEY-MAILMID
                 PERFORM Q70-INITKEY-MAILMDS
                 MOVE DETL-MSGID-I     TO UNEX-VARIABLE
                 PERFORM Q203-LEFT-JUSTIFY
                 MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMDS-ID
                                          MMID-ID
                 PERFORM Q76-READGT-MAILMDS
                 IF  EIBRESP = DFHRESP(NORMAL)
                 AND MMDS-ID = MMID-ID
                   MOVE '...'          TO DETL-TO-GRP-O(26:3)
                 ELSE
                   MOVE DFHRESP(NORMAL) TO EIBRESP
                 END-IF
               WHEN DFHRESP(NOTFND)
                 MOVE DFHRESP(NORMAL)  TO EIBRESP
                 MOVE 'Send to group name is not found.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-TO-GRP-L
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
               WHEN OTHER
                 GO TO D70-DETL-ERRORS
               END-EVALUATE
             END-IF
           END-IF

           IF  DETL-SUBJECT-L      > ZERO
           OR  DETL-SUBJECT-F      = FLDMODFY OR FLDCURSM
             IF  DETL-SUBJECT-I <= SPACES
               MOVE 'Subject is required.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-SUBJECT-L
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
             END-IF
           END-IF

           IF  DETL-COMPLETE-L     > ZERO
           OR  DETL-COMPLETE-F     = FLDMODFY OR FLDCURSM
             IF  DETL-COMPLETE-I > SPACES AND NOT = 'Y' AND 'N'
               MOVE 'Complete flag is invalid.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-COMPLETE-L
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
             END-IF
           END-IF

           IF  DETL-DELIVERY-L     > ZERO
           OR  DETL-DELIVERY-F     = FLDMODFY OR FLDCURSM
             IF  DETL-DELIVERY-I > SPACES AND NOT = 'Y' AND 'N'
               MOVE 'Deliver flag is invalid.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-DELIVERY-L
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
             END-IF
           END-IF

           IF  DETL-PRIVATE-L      > ZERO
           OR  DETL-PRIVATE-F      = FLDMODFY OR FLDCURSM
             IF  DETL-PRIVATE-I > SPACES AND NOT = 'Y' AND 'N'
               MOVE 'Private flag is invalid.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-PRIVATE-L
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
             END-IF
           END-IF

      * loop on body text lines to process them
           MOVE 1                      TO IDX
           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM TS-ITEM BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR IDX > DETLROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM Q32-READQ-TS

      *     save text lines, first
             IF  DETL-TEXT-L(IDX)  > ZERO
             OR  DETL-TEXT-F(IDX)  = FLDMODFY OR FLDCURSM
               IF  DETL-TEXT-I(IDX) > SPACES
                 MOVE DETL-TEXT-I(IDX) TO TS-DATA
               ELSE
                 MOVE SPACES           TO TS-DATA
               END-IF
             END-IF

      *     save line commands for later processing
             IF  DETL-LCA-I(IDX) > SPACES AND NOT = '*===*'
               INSPECT DETL-LCA-I(IDX) CONVERTING '*=' TO SPACES
               MOVE DETL-LCA-I(IDX)    TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               MOVE UNEX-VARIABLE      TO TS-SELECT
               INSPECT TS-SELECT CONVERTING LOWER-CASE
                                         TO UPPER-CASE
               MOVE TS-SELECT          TO DETL-LCA-I(IDX)
               EVALUATE TRUE
               WHEN TS-SELECT(1:1) IS NOT LINE-COMMAND
               AND  TS-SELECT(1:1) NOT = 'S'
                 MOVE 'Line command is unknown.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-LCA-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
               WHEN TS-SELECT(2:1) > SPACE
               AND (TS-SELECT(1:1) = 'C' OR 'M')
               OR   TS-SELECT(2:1) > SPACE
               AND  TS-SELECT(2:1) IS NOT POSITIVE-DIGIT
                 MOVE 'Line command is invalid.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DETL-LCA-L(IDX)
                                          DETL-MESSAGE-L
                                          MULTI-EDIT-MSG
               END-EVALUATE
               IF  NO-ERRORS-FOUND
                 IF  TS-SELECT(1:1) = 'C' OR 'M'
                   IF  SAVED-DATA = LOW-VALUES
                     MOVE TS-DATA      TO SAVED-DATA
                   ELSE
                     MOVE LOW-VALUES   TO TS-SELECT
                                          DETL-LCA-I(IDX)
                   END-IF
                 END-IF
               END-IF
             ELSE
               MOVE LOW-VALUES         TO TS-SELECT
             END-IF

      *     set up to split the current line at the cursor position
             IF  EIBAID = AIDPFK14
             AND IDX = ROW
               SUBTRACT DETLPRFX     FROM COL
               IF  1 <= COL AND COL <= LENGTH OF DETL-TEXT-I(1)
                 MOVE COL              TO NUM-WORK
                 STRING 'S' NUM-WRK3 ' '  DELIMITED BY SIZE
                                     INTO DETL-LCA-I(IDX)
                 MOVE DETL-LCA-I(IDX)  TO TS-SELECT
               END-IF
             END-IF

             PERFORM Q34-REWRITEQ-TS
             ADD  1                    TO IDX
             IF  SS-ITEM = 32767
                 MOVE DFHRESP(ENDFILE) TO EIBRESP
             END-IF
           END-PERFORM

      * flow the indicated paragraph to eliminate excess white space
           IF  NO-ERRORS-FOUND
           AND EIBAID = AIDPFK02
           AND EIBRESP = DFHRESP(NORMAL)
               IF  1 <= ROW AND ROW <= DETLROWS
                   PERFORM P40-FLOW-MAIL-BODY THRU P45-EXIT
               ELSE
                   MOVE -2             TO ERROR-SWITCH
                   MOVE EXHREVRS       TO DETL-MESSAGE-H
                   MOVE 'Cursor is not in the mail body section.'
                                       TO DETL-MESSAGE-O
                   GO TO D80-DETL-OUTPUT
               END-IF
           END-IF

           IF  NO-ERRORS-FOUND
             PERFORM P30-LINE-COMMANDS THRU P35-EXIT
           END-IF

           END-IF.

           IF  ERRORS-FOUND
               GO TO D80-DETL-OUTPUT
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               GO TO D70-DETL-ERRORS
           END-IF.

      * END OF DETAIL FIELD EDITING

           IF  EIBAID = AIDPFK04
               IF  DETL-TO-GRP-F = FLDCURSR OR FLDCURSM
                   MOVE DETL-TO-GRP-I  TO MMSG-SAVEAREA
                   IF  NOT ADD-PROCESS
                     MOVE DETL-MSGID-I TO UNEX-VARIABLE
                     PERFORM Q203-LEFT-JUSTIFY
                     MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMSG-SAVEKEY
                     GO TO E00-DIST-ROUTINE
                   END-IF
                   MOVE COMM-CURRRTN   TO COMM-SAVE-FUNC
                   MOVE FLDUNFST       TO DETL-TO-GRP-A
                   MOVE ZEROES         TO SS-ITEM
                   MOVE DETLMAPI       TO MMSG-SAVE-TIOA
                   PERFORM T40-SAVE-COMMAREA THRU T45-EXIT
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE 'EXIT'         TO COMM-TRANSID
                   MOVE LINK-PGM       TO COMM-LINKPGM
                   COMPUTE LINK-LEN = LENGTH OF COMM-HEADER
                                    + LENGTH OF COMM-SHARED
                                    + LENGTH OF MMSG-SAVEAREA
                   GO TO T02-LINK-CONTINUE
               END-IF
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO DETL-MESSAGE-H
               MOVE 'Cursor is not in a promptable field.'
                                       TO DETL-MESSAGE-O
               GO TO D80-DETL-OUTPUT
           END-IF

           IF  EIBAID = AIDPFK05
               GO TO D40-DETL-UPDATES
           END-IF

           IF  EIBAID = AIDPFK06 OR AIDPFK09
               IF  EIBAID = AIDPFK06
                   IF  TS-ITEM = 1
                       MOVE 'This is the first page.'
                                       TO DETL-MESSAGE-O
                   ELSE
                       MOVE 1          TO TS-ITEM
                   END-IF
               ELSE
                   COMPUTE TS-ITEM = TS-TOTL - (DETLROWS - 2)
                   IF  TS-ITEM < 1
                       MOVE 1          TO TS-ITEM
                       MOVE 'This is the last page.'
                                       TO DETL-MESSAGE-O
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
                   OR  TS-ITEM < 1
                       MOVE TS-TOTL    TO TS-ITEM
                       MOVE 'This is the last entry.'
                                       TO DETL-MESSAGE-O
                   END-IF
               END-IF
               GO TO D50-DETL-DISPLAY
           END-IF.

           GO TO D50-DETL-DISPLAY.

       D40-DETL-UPDATES.

      * PROCESS ANY ALLOWED UPDATES HERE

           PERFORM Q20-INITKEY-MAILMID.
           MOVE DETL-MSGID-I           TO UNEX-VARIABLE.
           PERFORM Q203-LEFT-JUSTIFY.
           MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMID-ID.
           PERFORM Q26-READUP-MAILMID.

           IF  EIBRESP = DFHRESP(NOTFND)
               SET  RECORD-NOT-FOUND   TO TRUE
               INITIALIZE MMID-HDR
               EXEC CICS ASKTIME
                         ABSTIME  (MMID-CREATED-TIMESTAMP)
               END-EXEC
               MOVE CICS-USERID        TO MMID-CREATED-BY
           ELSE
               EXEC CICS ASKTIME
                         ABSTIME  (MMID-CHANGED-TIMESTAMP)
               END-EXEC
           END-IF.
           MOVE ZEROES                 TO MMID-SENT-TIMESTAMP.

           MOVE DETL-COMPLETE-I        TO MMID-COMPLETE.
           MOVE DETL-DELIVERY-I        TO MMID-DELIVERY.
           MOVE DETL-PRIVATE-I         TO MMID-PRIVATE.
           MOVE DETL-SYSTEM-I          TO MMID-SYSTEM.
           MOVE DETL-TO-GRP-I          TO MMID-TO-GRP.
           MOVE DETL-FROM-I            TO MMID-FROM.
           MOVE DETL-SUBJECT-I         TO MMID-SUBJECT.
           MOVE CICS-USERID            TO MMID-UPDATED-BY.

           IF  RECORD-NOT-FOUND
               PERFORM Q27-WRITE-MAILMID
           ELSE
               PERFORM Q27-REWRITE-MAILMID
           END-IF.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO D70-DETL-ERRORS
           END-IF.

           PERFORM Q40-INITKEY-MAILMSG.
           MOVE MMID-ID                TO MMSG-ID.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM 1 BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR EIBRESP NOT = DFHRESP(NORMAL)

               EXEC CICS SUSPEND END-EXEC

               PERFORM Q32-READQ-TS

               MOVE SS-ITEM            TO MMSG-SEQ
               PERFORM Q46-READUP-MAILMSG

               MOVE TS-DATA            TO MMSG-BODY

               EVALUATE EIBRESP
               WHEN DFHRESP(NOTFND)
                   PERFORM Q47-WRITE-MAILMSG
               WHEN DFHRESP(NORMAL)
                   PERFORM Q47-REWRITE-MAILMSG
               END-EVALUATE

               IF  SS-ITEM = 32767
                   MOVE DFHRESP(ENDFILE) TO EIBRESP
               END-IF
           END-PERFORM.

           IF  EIBRESP = DFHRESP(ENDFILE)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.

           COMPUTE MMSG-SEQ = TS-TOTL + 1.

           PERFORM WITH TEST BEFORE
             VARYING MMSG-SEQ FROM MMSG-SEQ BY 1
               UNTIL EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q46-READUP-MAILMSG
             IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q48-DELETE-MAILMSG
             END-IF
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(NOTFND)
               GO TO D70-DETL-ERRORS
           END-IF.

           PERFORM Q35-DELETEQ-TS.

           MOVE COMM-CURRRTN           TO COMM-SAVE-FUNC.
           MOVE 'RETN'                 TO COMM-CURRRTN.
           GO TO C00-LIST-ROUTINE.

       D50-DETL-DISPLAY.

           MOVE 'DETL'                 TO COMM-CURRRTN.

      * skip if no detail lines present
           IF  TS-TOTL <= ZERO
               GO TO D60-DETL-DETAIL
           END-IF.

      * PERFORM HEADER CONTROL FIELD EDITING HERE

       D60-DETL-DETAIL.

           IF  TS-TOTL <= ZERO
               PERFORM P10-EXTRACT-MAIL-BODY THRU P15-EXIT
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
           END-IF.

           IF  MMSG-SAVEKEY > ZERO
               PERFORM Q20-INITKEY-MAILMID
               MOVE MMSG-SAVEKEY       TO MMID-ID
               PERFORM Q26-READEQ-MAILMID
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NOTFND)
                   OR  NOT ADD-PROCESS
                       GO TO D70-DETL-ERRORS
                   END-IF
                   MOVE '*INITMID'     TO UNEX-DS
               END-IF
               MOVE ZEROES             TO MMSG-SAVEKEY
               IF  ADD-PROCESS
                   PERFORM P20-CREATE-NEW-MAIL-ID THRU P25-EXIT
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       GO TO D70-DETL-ERRORS
                   END-IF
               END-IF
               MOVE MMID-ID            TO DETL-MSGID-O
               MOVE MMID-SYSTEM        TO DETL-SYSTEM-O
               MOVE MMID-TO-GRP        TO DETL-TO-GRP-O
               PERFORM Q70-INITKEY-MAILMDS
               MOVE MMID-ID            TO MMDS-ID
               PERFORM Q76-READGT-MAILMDS
               IF  EIBRESP = DFHRESP(NORMAL)
               AND MMDS-ID = MMID-ID
                 MOVE '...'            TO DETL-TO-GRP-O(26:3)
               END-IF
               PERFORM Q50-INITKEY-MAILDST
               MOVE MMID-TO-GRP        TO MDST-GRP-NAME
               PERFORM Q56-READEQ-MAILDST
               IF  EIBRESP = DFHRESP(NORMAL)
                 MOVE MDST-GRP-DESC    TO DETL-TO-DESC-O
               ELSE
                 MOVE ' Ýnot found¨ '  TO DETL-TO-DESC-O
               END-IF
               MOVE MMID-SUBJECT       TO DETL-SUBJECT-O
               MOVE MMID-COMPLETE      TO DETL-COMPLETE-O
               MOVE MMID-DELIVERY      TO DETL-DELIVERY-O
               MOVE MMID-PRIVATE       TO DETL-PRIVATE-O
               MOVE MMID-FROM          TO DETL-FROM-O
               EXEC CICS FORMATTIME
                         ABSTIME(MMID-CREATED-TIMESTAMP)
                         YYYYMMDD(DETL-CREATED-O) DATESEP
                         TIME(DETL-CREATET-O) TIMESEP
               END-EXEC
               IF  MMID-CHANGED-TIMESTAMP <= ZEROES
                   MOVE SPACES         TO DETL-CHANGED-O
                                          DETL-CHANGET-O
               ELSE
                   EXEC CICS FORMATTIME
                             ABSTIME(MMID-CHANGED-TIMESTAMP)
                             YYYYMMDD(DETL-CHANGED-O) DATESEP
                             TIME(DETL-CHANGET-O) TIMESEP
                   END-EXEC
               END-IF
               IF  MMID-SENT-TIMESTAMP <= ZEROES
                   MOVE SPACES         TO DETL-SENTDATE-O
                                          DETL-SENTTIME-O
               ELSE
                   EXEC CICS FORMATTIME
                             ABSTIME(MMID-SENT-TIMESTAMP)
                             YYYYMMDD(DETL-SENTDATE-O) DATESEP
                             TIME(DETL-SENTTIME-O) TIMESEP
                   END-EXEC
               END-IF
               MOVE MMID-CREATED-BY    TO DETL-CREATED-BY-O
               MOVE MMID-UPDATED-BY    TO DETL-UPDATED-BY-O
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
                 MOVE LOW-VALUES       TO DETLMAP-ROW-DETAIL(IDX)
                 MOVE TS-DATA          TO DETL-TEXT-O(IDX)
             ELSE
               MOVE -1                 TO DETL-LCA-L(IDX)
               SUBTRACT 1            FROM IDX
             END-IF
             ADD  1                    TO IDX
           END-PERFORM.

           IF  TS-TOTL >= DETL-MAX
               MOVE DFHRESP(ENDFILE)   TO EIBRESP
           END-IF.

       D65-DETL-BLANKS.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM IDX BY 1
               UNTIL IDX > DETLROWS
             MOVE LOW-VALUES           TO DETLMAP-ROW-DETAIL(IDX)
             MOVE FLDASDRK             TO DETL-LCA-A(IDX)
                                          DETL-TEXT-A(IDX)
             IF  EOD-NOT-MARKED
               SET  EOD-MARKED         TO TRUE
               MOVE FLDASBRT           TO DETL-TEXT-A(IDX)
               MOVE EXCNEUTR           TO DETL-TEXT-C(IDX)
               MOVE ' -- END OF DATA -- '
                                       TO DETL-TEXT-O(IDX)
             END-IF
           END-PERFORM.

       D70-DETL-ERRORS.

           IF  EIBRESP = DFHRESP(ENDFILE)
               MOVE 'Maximum size reached.'
                                       TO DETL-MESSAGE-O
               GO TO D80-DETL-OUTPUT
           END-IF.

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
               MOVE FLDASFST           TO DETL-SYSTEM-A
                                          DETL-TO-GRP-A
                                          DETL-SUBJECT-A
                                          DETL-COMPLETE-A
                                          DETL-DELIVERY-A
                                          DETL-PRIVATE-A
                                          DETL-FROM-A
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > DETLROWS
                 IF  DETL-LCA-A(IDX) NOT = FLDASDRK
                   MOVE FLDASKIP       TO DETL-LCA-A(IDX)
                                          DETL-TEXT-A(IDX)
                 END-IF
               END-PERFORM
               MOVE FLDASDRK           TO DETL-FLOW-KEY-A
                                          DETL-FLOW-TEXT-A
                                          DETL-UPDATE-KEY-A
                                          DETL-UPDATE-TEXT-A
                                          DETL-SPLIT-KEY-A
                                          DETL-SPLIT-TEXT-A
           ELSE
               MOVE EXHULINE           TO DETL-SYSTEM-H
                                          DETL-TO-GRP-H
                                          DETL-SUBJECT-H
                                          DETL-COMPLETE-H
                                          DETL-DELIVERY-H
                                          DETL-PRIVATE-H
                                          DETL-FROM-H
           END-IF.

           IF  TS-TOTL > ZERO
               MOVE TS-ITEM            TO DETL-CUR-LINE-O
               COMPUTE DETL-CUR-PAGE-O = (TS-ITEM - 1) / DETLROWS + 1
               COMPUTE DETL-TOT-PAGES-O = (TS-TOTL - 1) / DETLROWS + 1
           ELSE
               MOVE 1                  TO DETL-CUR-LINE-O
                                          DETL-CUR-PAGE-O
                                          DETL-TOT-PAGES-O
           END-IF.

       D85-DETL-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (DETLMAP)
                             DATAONLY
                             FROM     (DETLMAPO)
                             MAPSET   (MMSGMS)
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
                             MAPSET   (MMSGMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               DIVIDE EIBCPOSN         BY SCRNWDTH
                                   GIVING ROW
                                REMAINDER COL
               ADD  1                  TO ROW
                                          COL
               IF  ROW <= DETLHEAD
               OR (ROW - DETLHEAD) > (TS-TOTL - TS-ITEM + 1)
                   IF  VIEW-PROCESS
                       MOVE -1         TO DETL-MSGID-L
                   ELSE
                       EVALUATE TRUE
                       WHEN DETL-MSGID-I > SPACES
                        AND DETL-TO-GRP-I > SPACES
                        AND DETL-SUBJECT-I > SPACES
                        AND TS-TOTL > ZEROES
                           MOVE ZEROES TO DETL-SYSTEM-L
                                          DETL-TO-GRP-L
                                          DETL-SUBJECT-L
                           MOVE -1     TO DETL-LCA-L(1)
                       WHEN ADD-PROCESS
                           MOVE -1     TO DETL-SYSTEM-L
                       WHEN DETL-TO-GRP-I <= SPACES
                           MOVE -1     TO DETL-TO-GRP-L
                       WHEN OTHER
                           MOVE -1     TO DETL-SUBJECT-L
                       END-EVALUATE
                   END-IF
                   EXEC CICS SEND
                             MAP      (DETLMAP)
                             FROM     (DETLMAPO)
                             MAPSET   (MMSGMS)
                             TERMINAL
                             FREEKB
                             ERASE
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      (DETLMAP)
                             FROM     (DETLMAPO)
                             MAPSET   (MMSGMS)
                             TERMINAL
                             FREEKB
                             ERASE
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
               IF  VIEW-PROCESS
                 MOVE 1                TO PTR
      *                               row  adjust            col  adjust
                 COMPUTE HALF-WORD = (026 - 1) * SCRNWDTH + (001 - 1)
                 STRING SCRSETBA TWO-BYTES SCRSTFLD FLDASDRK
                   DELIMITED BY SIZE INTO VAR-DATA WITH POINTER PTR
      *                               row  adjust            col  adjust
                 COMPUTE HALF-WORD = (026 - 1) * SCRNWDTH + (099 - 1)
                 STRING SCREPEAT TWO-BYTES X'40' SCRSTFLD FLDASKIP
                   DELIMITED BY SIZE INTO VAR-DATA WITH POINTER PTR
                 SUBTRACT 1          FROM PTR
                 EXEC CICS SEND
                           FROM(VAR-DATA)
                           LENGTH(PTR)
                           CTLCHAR(WRTFKFST)
                           WAIT
                           NOHANDLE
                 END-EXEC
               END-IF
           END-IF.

       D90-DETL-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (DETLMAPI)
           END-EXEC.

           GO TO Z80-MMSG-RETURN.

      /*****************************************************************
      *    DIST ROUTINE                                                *
      ******************************************************************
       E00-DIST-ROUTINE.

           IF  ADDRESS OF DISTMAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF DISTMAPI)
                         LENGTH   (LENGTH OF DISTMAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
           END-IF.

           IF  COMM-CURRRTN NOT = 'DIST'
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-SAVE-FUNC = 'RETN'
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE MMSG-SAVE-TIOA TO DISTMAPO
                   MOVE LOW-VALUES     TO MMSG-SAVE-TIOA
                   IF  MMSG-SAVEAREA > SPACES
                   AND COMM-HELPAREA(1:4) NOT = 'HELP'
                     MOVE AIDENTER     TO EIBAID
                     IF  SS-ITEM > ZEROES
                     AND SS-ITEM <= DETLROWS
                       MOVE SS-ITEM    TO ROW
                       MOVE LENGTH OF DIST-GRPNAME-I(ROW)
                                       TO DIST-GRPNAME-L(ROW)
                       MOVE FLDCURSM   TO DIST-GRPNAME-F(ROW)
                       MOVE MMSG-SAVEAREA
                                       TO DIST-GRPNAME-I(ROW)
                     ELSE
                       MOVE LENGTH OF DIST-TO-GRP-I
                                       TO DIST-TO-GRP-L
                       MOVE FLDCURSM   TO DIST-TO-GRP-F
                       MOVE MMSG-SAVEAREA
                                       TO DIST-TO-GRP-I
                     END-IF
                     PERFORM E80-DIST-OUTPUT THRU E85-DIST-SENDMAP
                     GO TO E30-DIST-PROCESS
                   END-IF
                   MOVE SPACES         TO COMM-HELPAREA
               ELSE
                   MOVE ZEROES         TO TS-TOTL
               END-IF
               PERFORM E80-DIST-OUTPUT THRU E85-DIST-SENDMAP
               GO TO E50-DIST-DISPLAY
           END-IF.

       E10-DIST-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (DISTMAP)
                     INTO     (DISTMAPI)
                     MAPSET   (MMSGMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO E70-DIST-ERRORS
           END-IF.

       E20-DIST-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
               MOVE JOBSSELT           TO COMM-LINKPGM
               GO TO Z90-MMSG-TERMINATION

           WHEN AIDPFK03
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE 'RETN'             TO COMM-CURRRTN
               MOVE DIST-MSGID-I       TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMSG-SAVEKEY
               GO TO D00-DETL-ROUTINE

           WHEN AIDPFK01
               MOVE DISTMAPI           TO MMSG-SAVE-TIOA
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
               MOVE EXHREVRS           TO DIST-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO DIST-MESSAGE-O
               GO TO E80-DIST-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO DIST-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO DIST-MESSAGE-O
               GO TO E80-DIST-OUTPUT
           END-EVALUATE.

       E30-DIST-PROCESS.

           IF (EIBAID = AIDPFK04 OR AIDPFK05)
           AND VIEW-PROCESS
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO DIST-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO DIST-MESSAGE-O
               GO TO E80-DIST-OUTPUT
           END-IF.

      * convert cursor position to row and column coordinates
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.

      * PERFORM DETAIL FIELD EDITING HERE

           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           IF  NOT VIEW-PROCESS

           INSPECT DIST-TO-GRP-I CONVERTING LOWER-CASE
                                         TO UPPER-CASE

      * prompt in header area?
           IF  EIBAID = AIDPFK04
             IF  DIST-TO-GRP-F = FLDCURSR OR FLDCURSM
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE FLDUNFST           TO DIST-TO-GRP-A
               MOVE DIST-TO-GRP-I      TO MMSG-SAVEAREA
               MOVE ZEROES             TO SS-ITEM
               MOVE DISTMAPI           TO MMSG-SAVE-TIOA
               PERFORM T40-SAVE-COMMAREA THRU T45-EXIT
               MOVE SPACES             TO COMM-SAVE-FUNC
               MOVE 'EXIT'             TO COMM-TRANSID
               MOVE LINK-PGM           TO COMM-LINKPGM
               COMPUTE LINK-LEN = LENGTH OF COMM-HEADER
                                + LENGTH OF COMM-SHARED
                                + LENGTH OF MMSG-SAVEAREA
               GO TO T02-LINK-CONTINUE
             END-IF
           END-IF

           IF  DIST-TO-GRP-L       > ZERO
           OR  DIST-TO-GRP-F       = FLDMODFY OR FLDCURSM
             IF  DIST-TO-GRP-I <= SPACES
               MOVE 'Send to group name is required.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DIST-TO-GRP-L
                                          DIST-MESSAGE-L
                                          MULTI-EDIT-MSG
             ELSE
               PERFORM Q50-INITKEY-MAILDST
               MOVE DIST-TO-GRP-I      TO MDST-GRP-NAME
               PERFORM Q56-READEQ-MAILDST
               EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
                 MOVE MDST-GRP-DESC    TO DIST-TO-DESC-O
               WHEN DFHRESP(NOTFND)
                 MOVE DFHRESP(NORMAL)  TO EIBRESP
                 MOVE 'Send to group name is not found.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DIST-TO-GRP-L
                                          DIST-MESSAGE-L
                                          MULTI-EDIT-MSG
               WHEN OTHER
                 GO TO E70-DIST-ERRORS
               END-EVALUATE
             END-IF
           END-IF

      * adjust row and column coordinates for header/detail screens
           SUBTRACT DISTHEAD         FROM ROW

           IF  EIBAID = AIDPFK04
      * prompt a detail line?
               IF  1 <= ROW AND ROW <= DISTROWS
               AND DIST-GRPNAME-F(ROW) = FLDCURSR OR FLDCURSM
                   MOVE COMM-CURRRTN   TO COMM-SAVE-FUNC
                   MOVE FLDUNFST       TO DIST-GRPNAME-A(ROW)
                   IF  DIST-GRPNAME-I(ROW) > SPACES
                       MOVE DIST-GRPNAME-I(ROW)
                                       TO MMSG-SAVEAREA
                   ELSE
                       COMPUTE SS-ITEM = TS-ITEM + (ROW - 1)
                       PERFORM Q32-READQ-TS
                       MOVE TS-GRPNAME TO MMSG-SAVEAREA
                   END-IF
                   INSPECT MMSG-SAVEAREA CONVERTING LOWER-CASE
                                                 TO UPPER-CASE
                   MOVE ROW            TO SS-ITEM
                   MOVE DISTMAPI       TO MMSG-SAVE-TIOA
                   PERFORM T40-SAVE-COMMAREA THRU T45-EXIT
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE 'EXIT'         TO COMM-TRANSID
                   MOVE LINK-PGM       TO COMM-LINKPGM
                   COMPUTE LINK-LEN = LENGTH OF COMM-HEADER
                                    + LENGTH OF COMM-SHARED
                                    + LENGTH OF MMSG-SAVEAREA
                   GO TO T02-LINK-CONTINUE
               END-IF
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO DIST-MESSAGE-H
               MOVE 'Cursor is not in a promptable field.'
                                       TO DIST-MESSAGE-O
               GO TO E80-DIST-OUTPUT
           END-IF

      * loop on distribution lines to process them
           MOVE 1                      TO IDX
           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM TS-ITEM BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR IDX > DISTROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q32-READQ-TS
             PERFORM P70-EDIT-DIST-LINE THRU P75-EXIT
             ADD  1                    TO IDX
           END-PERFORM
           IF  IDX <= DISTROWS
             PERFORM P70-EDIT-DIST-LINE THRU P75-EXIT
           END-IF

           END-IF.

           IF  ERRORS-FOUND
               GO TO E80-DIST-OUTPUT
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               GO TO E70-DIST-ERRORS
           END-IF.

      * END OF DETAIL FIELD EDITING

           IF  EIBAID = AIDPFK05
               GO TO E40-DIST-UPDATES
           END-IF

           IF  EIBAID = AIDPFK06 OR AIDPFK09
               IF  EIBAID = AIDPFK06
                   IF  TS-ITEM = 1
                       MOVE 'This is the first page.'
                                       TO DIST-MESSAGE-O
                   ELSE
                       MOVE 1          TO TS-ITEM
                   END-IF
               ELSE
                   COMPUTE TS-ITEM = TS-TOTL - (DISTROWS - 1)
                   IF  TS-ITEM < 1
                       MOVE 1          TO TS-ITEM
                       MOVE 'This is the last page.'
                                       TO DIST-MESSAGE-O
                   END-IF
               END-IF
               GO TO E50-DIST-DISPLAY
           END-IF.

           IF  EIBAID = AIDPFK07 OR AIDPFK08
               IF  EIBAID = AIDPFK07
                   IF  (ROW < 1 OR >= DISTROWS)
                       MOVE ZEROES     TO ROW
                   END-IF
                   COMPUTE TS-ITEM = TS-ITEM - (DISTROWS - ROW)
                   IF  TS-ITEM < 1
                       MOVE +1         TO TS-ITEM
                       MOVE 'This is the first entry.'
                                       TO DIST-MESSAGE-O
                   END-IF
               ELSE
                   IF  (ROW <= 1 OR > DISTROWS)
                       COMPUTE ROW = DISTROWS + 1
                   END-IF
                   COMPUTE TS-ITEM = TS-ITEM + (ROW - 1)
                   IF  TS-ITEM > TS-TOTL
                   OR  TS-ITEM < 1
                       MOVE TS-TOTL    TO TS-ITEM
                       MOVE 'This is the last entry.'
                                       TO DIST-MESSAGE-O
                   END-IF
               END-IF
               GO TO E50-DIST-DISPLAY
           END-IF.

           GO TO E50-DIST-DISPLAY.

       E40-DIST-UPDATES.

      * PROCESS ANY ALLOWED UPDATES HERE

           PERFORM Q20-INITKEY-MAILMID.
           MOVE DIST-MSGID-I           TO UNEX-VARIABLE.
           PERFORM Q203-LEFT-JUSTIFY.
           MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMID-ID.
           PERFORM Q26-READUP-MAILMID.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO E70-DIST-ERRORS
           END-IF.

           EXEC CICS ASKTIME
                     ABSTIME  (MMID-CHANGED-TIMESTAMP)
           END-EXEC.
           MOVE ZEROES                 TO MMID-SENT-TIMESTAMP.

           MOVE DIST-TO-GRP-I          TO MMID-TO-GRP.
           MOVE CICS-USERID            TO MMID-UPDATED-BY.

           PERFORM Q27-REWRITE-MAILMID.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO E70-DIST-ERRORS
           END-IF.

           MOVE 1                      TO IDX.

           PERFORM Q70-INITKEY-MAILMDS.
           MOVE MMID-ID                TO MMDS-ID.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM 1 BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR EIBRESP NOT = DFHRESP(NORMAL)

               PERFORM Q32-READQ-TS
               IF  TS-SELECT NOT = 'D'

                   MOVE IDX            TO MMDS-SEQ
                   PERFORM Q76-READUP-MAILMDS

                   MOVE TS-GRPNAME     TO MMDS-TO-GRP

                   EVALUATE EIBRESP
                   WHEN DFHRESP(NOTFND)
                       PERFORM Q77-WRITE-MAILMDS
                   WHEN DFHRESP(NORMAL)
                       PERFORM Q77-REWRITE-MAILMDS
                   END-EVALUATE

                   ADD  1              TO IDX
               END-IF
           END-PERFORM.

           PERFORM WITH TEST BEFORE
             VARYING MMDS-SEQ FROM IDX BY 1
               UNTIL EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q76-READUP-MAILMDS
             IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q78-DELETE-MAILMDS
             END-IF
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(NOTFND)
               GO TO E70-DIST-ERRORS
           END-IF.

           PERFORM Q35-DELETEQ-TS.

           MOVE DIST-MSGID-I           TO UNEX-VARIABLE.
           PERFORM Q203-LEFT-JUSTIFY.
           MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO MMSG-SAVEKEY.

           MOVE COMM-CURRRTN           TO COMM-SAVE-FUNC.
           MOVE 'RETN'                 TO COMM-CURRRTN.
           GO TO D00-DETL-ROUTINE.

       E50-DIST-DISPLAY.

           MOVE 'DIST'                 TO COMM-CURRRTN.

      * skip if no detail lines present
           IF  TS-TOTL <= ZERO
               GO TO E60-DIST-DETAIL
           END-IF.

      * PERFORM HEADER CONTROL FIELD EDITING HERE

       E60-DIST-DETAIL.

           IF  TS-TOTL <= ZERO
               PERFORM P60-EXTRACT-MAIL-DIST THRU P65-EXIT
               IF  ERRORS-FOUND
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE -2             TO ERROR-SWITCH
                   MOVE EXHREVRS       TO DIST-MESSAGE-H
                   IF  DIST-MESSAGE-I <= SPACES
                       PERFORM X00-UNEX-ERR
                       MOVE UNEX-MSG   TO DIST-MESSAGE-O
                   END-IF
                   GO TO E80-DIST-OUTPUT
               END-IF
               IF  TS-TOTL <= ZERO
                   MOVE 2              TO IDX
                   GO TO E65-DIST-BLANKS
               END-IF
           END-IF.

           IF  MMSG-SAVEKEY > ZERO
               PERFORM Q20-INITKEY-MAILMID
               MOVE MMSG-SAVEKEY       TO MMID-ID
               PERFORM Q26-READEQ-MAILMID
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO E70-DIST-ERRORS
               END-IF
               MOVE ZEROES             TO MMSG-SAVEKEY
               MOVE MMID-ID            TO DIST-MSGID-O
               MOVE MMID-SUBJECT       TO DIST-SUBJECT-O
               MOVE MMID-TO-GRP        TO DIST-TO-GRP-O
               PERFORM Q50-INITKEY-MAILDST
               MOVE MMID-TO-GRP        TO MDST-GRP-NAME
               PERFORM Q56-READEQ-MAILDST
               IF  EIBRESP = DFHRESP(NORMAL)
                 MOVE MDST-GRP-DESC    TO DIST-TO-DESC-O
               ELSE
                 MOVE ' Ýnot found¨ '  TO DIST-TO-DESC-O
               END-IF
           END-IF.

           MOVE 1                      TO IDX.
           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM TS-ITEM BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR IDX > DISTROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM Q32-READQ-TS
             IF  EIBRESP = DFHRESP(NORMAL)

      * BUILD DETAIL LINE OUTPUT HERE
                 MOVE LOW-VALUES       TO DISTMAP-ROW-DETAIL(IDX)
                 MOVE TS-SELECT        TO DIST-SELECT-O(IDX)
                 MOVE TS-GRPNAME       TO DIST-GRPNAME-O(IDX)
                 MOVE TS-GRPDESC       TO DIST-GRPDESC-O(IDX)
             ELSE
                 MOVE -1               TO DIST-SELECT-L(IDX)
             END-IF
             ADD  1                    TO IDX
           END-PERFORM.

           IF  NOT VIEW-PROCESS
           AND DIST-GRPNAME-O(IDX - 1) > SPACES
               ADD  1                  TO IDX
           END-IF.

       E65-DIST-BLANKS.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM IDX BY 1
               UNTIL IDX > DISTROWS
             MOVE LOW-VALUES           TO DISTMAP-ROW-DETAIL(IDX)
             MOVE FLDASDRK             TO DIST-SELECT-A(IDX)
                                          DIST-GRPNAME-A(IDX)
                                          DIST-GRPDESC-A(IDX)
           END-PERFORM.

       E70-DIST-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO DIST-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO DIST-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       E80-DIST-OUTPUT.

           MOVE MAPTITLE               TO DIST-COMPANY-O.
           MOVE WS-WRKDATE             TO DIST-SYSDATE-O.
           MOVE WS-WRKTIME             TO DIST-SYSTIME-O.
           MOVE SPACES                 TO DIST-NETNAME-O.
           EXEC CICS ASSIGN NETNAME(DIST-NETNAME-O) END-EXEC.
           IF  DIST-NETNAME-I NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO DIST-NETNAME-O
           END-IF.

           IF  NOT ADD-PROCESS
           AND NOT CHG-PROCESS
               MOVE FLDASFST           TO DIST-TO-GRP-A
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > DISTROWS
                 IF  DIST-SELECT-A(IDX) NOT = FLDASDRK
                   MOVE FLDASKIP       TO DIST-SELECT-A(IDX)
                                          DIST-GRPNAME-A(IDX)
                                          DIST-GRPDESC-A(IDX)
                 END-IF
               END-PERFORM
               MOVE FLDASDRK           TO DIST-PROMPT-KEY-A
                                          DIST-PROMPT-TEXT-A
                                          DIST-UPDATE-KEY-A
                                          DIST-UPDATE-TEXT-A
           ELSE
               MOVE EXHULINE           TO DIST-TO-GRP-H
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > DISTROWS
                 IF  DIST-SELECT-A(IDX) NOT = FLDASDRK
                   MOVE EXHULINE       TO DIST-SELECT-H(IDX)
                                          DIST-GRPNAME-H(IDX)
                 END-IF
               END-PERFORM
           END-IF.

       E85-DIST-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (DISTMAP)
                             DATAONLY
                             FROM     (DISTMAPO)
                             MAPSET   (MMSGMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      (DISTMAP)
                             DATAONLY
                             FROM     (DISTMAPO)
                             MAPSET   (MMSGMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               IF  VIEW-PROCESS
                   MOVE -1             TO DIST-TO-GRP-L
               ELSE
                   IF  DIST-TO-GRP-I > SPACES
                   AND TS-TOTL > ZEROES
                       MOVE ZEROES     TO DIST-TO-GRP-L
                       MOVE -1         TO DIST-SELECT-L(1)
                   ELSE
                       MOVE -1         TO DIST-TO-GRP-L
                   END-IF
               END-IF
               EXEC CICS SEND
                         MAP      (DISTMAP)
                         FROM     (DISTMAPO)
                         MAPSET   (MMSGMS)
                         TERMINAL
                         FREEKB
                         ERASE
                         CURSOR
               END-EXEC
           END-IF.

       E90-DIST-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (DISTMAPI)
           END-EXEC.

           GO TO Z80-MMSG-RETURN.

      /*****************************************************************
      *    PERFORMED ROUTINES                                          *
      ******************************************************************

       P10-EXTRACT-MAIL-BODY.

           PERFORM Q35-DELETEQ-TS.

           PERFORM Q40-INITKEY-MAILMSG.
           MOVE MMSG-SAVEKEY           TO MMSG-ID.
           PERFORM Q41-STARTBR-MAILMSG.

           MOVE ZEROES                 TO SS-ITEM.
           PERFORM WITH TEST BEFORE
             UNTIL MMSG-ID NOT = MMSG-SAVEKEY
                OR EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q43-READNEXT-MAILMSG
             IF  EIBRESP = DFHRESP(NORMAL)
             AND MMSG-ID = MMSG-SAVEKEY
                 MOVE SPACES           TO TS-SELECT
      *        get length of body text line
                 COMPUTE LEN = MMSG-RECL - LENGTH OF MMSG-KEY
      *        if text fits in ts record
                 IF  LEN <= LENGTH OF TS-DATA
      *            put it into a single ts record
                     MOVE MMSG-BODY(1:LEN)
                                       TO TS-DATA
                 ELSE
      *            else split on word boundary between two ts records
                     COMPUTE POS = LENGTH OF TS-DATA + 1
                     PERFORM WITH TEST BEFORE
                       VARYING POS FROM POS BY -1
                         UNTIL POS < 1
                            OR MMSG-BODY(POS:1) = SPACE
                     END-PERFORM
                     IF  POS < 1
                       COMPUTE POS = LENGTH OF TS-DATA + 1
                     END-IF
                     MOVE MMSG-BODY(1:POS - 1)
                                       TO TS-DATA
                     PERFORM Q33-WRITEQ-TS
                     ADD  1            TO SS-ITEM
                     PERFORM WITH TEST BEFORE
                       VARYING POS FROM POS BY +1
                         UNTIL POS > LEN
                            OR MMSG-BODY(POS:1) NOT = SPACE
                     END-PERFORM
                     MOVE MMSG-BODY(POS:)
                                       TO TS-DATA
                 END-IF
                 PERFORM Q33-WRITEQ-TS
                 IF  SS-ITEM < 32767
                     ADD  1            TO SS-ITEM
                 ELSE
                     MOVE DFHRESP(ENDFILE)
                                       TO EIBRESP
                 END-IF
             END-IF
           END-PERFORM.

           IF  EIBRESP = DFHRESP(NORMAL)
                      OR DFHRESP(NOTFND)
                      OR DFHRESP(ENDFILE)
               PERFORM Q45-ENDBR-MAILMSG
               PERFORM Q31-NUMITEMS-TS
               IF  TS-TOTL <= ZERO
                   MOVE SPACES         TO TS-RECD
                   PERFORM Q33-WRITEQ-TS
                   PERFORM Q31-NUMITEMS-TS
               END-IF
           END-IF.

       P15-EXIT.
           EXIT.

       P20-CREATE-NEW-MAIL-ID.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC.
           PERFORM Q20-INITKEY-MAILMID.
           MOVE ZERO                   TO MMID-ID.
           PERFORM Q26-READUP-MAILMID.
           IF  EIBRESP = DFHRESP(NORMAL)
               ADD  1                  TO MMID-CONTROL-ID
               MOVE WS-ABSTIME         TO MMID-CONTROL-LAST-STMP
               PERFORM Q27-REWRITE-MAILMID
           END-IF.
           IF  EIBRESP = DFHRESP(NORMAL)
               MOVE MMID-CONTROL-ID    TO MMID-ID
               IF  UNEX-DS = '*INITMID'
                   INITIALIZE MMID-HDR
               END-IF
               MOVE WS-ABSTIME         TO MMID-CREATED-TIMESTAMP
               MOVE ZEROES             TO MMID-CHANGED-TIMESTAMP
                                          MMID-SENT-TIMESTAMP
               SET  MMID-NOT-COMPLETE  TO TRUE
           END-IF.
       P25-EXIT.
           EXIT.

       P30-LINE-COMMANDS.
           PERFORM Q65-DELETEQ-2-TS.
           MOVE TS-TOTL                TO T2-TOTL.
           MOVE ZEROES                 TO T2-ITEM.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM 1 BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM Q32-READQ-TS

             EVALUATE TS-SELECT(1:1)
             WHEN 'C'
               MOVE TS-DATA            TO T2-DATA
               PERFORM P32-LINE-WRITE
             WHEN 'M'
               SUBTRACT 1            FROM T2-TOTL
             WHEN 'I'
               MOVE TS-DATA            TO T2-DATA
               PERFORM P32-LINE-WRITE
               IF  TS-SELECT(2:1) IS POSITIVE-DIGIT
                 MOVE TS-SELECT(2:1)   TO NNN
               ELSE
                 MOVE 1                TO NNN
               END-IF
               MOVE SPACES             TO T2-DATA
               PERFORM WITH TEST BEFORE
                 VARYING CNT FROM 1 BY 1
                   UNTIL CNT > NNN
                      OR EIBRESP NOT = DFHRESP(NORMAL)
                 PERFORM P32-LINE-WRITE
               END-PERFORM
             WHEN 'A'
               MOVE TS-DATA            TO T2-DATA
               PERFORM P32-LINE-WRITE
               IF  SAVED-DATA > LOW-VALUES
                 IF  TS-SELECT(2:1) IS POSITIVE-DIGIT
                   MOVE TS-SELECT(2:1) TO NNN
                 ELSE
                   MOVE 1              TO NNN
                 END-IF
                 MOVE SAVED-DATA       TO T2-DATA
                 PERFORM WITH TEST BEFORE
                   VARYING CNT FROM 1 BY 1
                     UNTIL CNT > NNN
                        OR EIBRESP NOT = DFHRESP(NORMAL)
                   PERFORM P32-LINE-WRITE
                 END-PERFORM
                 MOVE LOW-VALUES       TO SAVED-DATA
               END-IF
             WHEN 'B'
               IF  SAVED-DATA > LOW-VALUES
                 IF  TS-SELECT(2:1) IS POSITIVE-DIGIT
                   MOVE TS-SELECT(2:1) TO NNN
                 ELSE
                   MOVE 1              TO NNN
                 END-IF
                 MOVE SAVED-DATA       TO T2-DATA
                 PERFORM WITH TEST BEFORE
                   VARYING CNT FROM 1 BY 1
                     UNTIL CNT > NNN
                        OR EIBRESP NOT = DFHRESP(NORMAL)
                   PERFORM P32-LINE-WRITE
                 END-PERFORM
                 MOVE LOW-VALUES       TO SAVED-DATA
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
                 MOVE TS-DATA          TO T2-DATA
                 PERFORM P32-LINE-WRITE
               END-IF
             WHEN '"'
               MOVE TS-DATA            TO T2-DATA
               PERFORM P32-LINE-WRITE
               IF  TS-SELECT(2:1) IS POSITIVE-DIGIT
                 MOVE TS-SELECT(2:1)   TO NNN
               ELSE
                 MOVE 1                TO NNN
               END-IF
               MOVE TS-DATA            TO T2-DATA
               PERFORM WITH TEST BEFORE
                 VARYING CNT FROM 1 BY 1
                   UNTIL CNT > NNN
                      OR EIBRESP NOT = DFHRESP(NORMAL)
                 PERFORM P32-LINE-WRITE
               END-PERFORM
             WHEN 'D'
               MOVE TS-SELECT(2:3) TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               IF  UNEX-VARIABLE(1:UNEX-LENG) IS NUMERIC
                 MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO NNN
               ELSE
                 MOVE 1                TO NNN
               END-IF
               IF  NNN > (TS-TOTL - SS-ITEM + 1)
                 COMPUTE NNN = TS-TOTL - SS-ITEM + 1
               END-IF
               SUBTRACT NNN          FROM T2-TOTL
               COMPUTE SS-ITEM = SS-ITEM + (NNN - 1)
             WHEN 'S'
               MOVE TS-SELECT(2:3)     TO LEN
               MOVE TS-DATA(1:LEN - 1) TO T2-DATA
               PERFORM P32-LINE-WRITE
               MOVE TS-DATA(LEN:)      TO T2-DATA
               PERFORM P32-LINE-WRITE
             WHEN OTHER
               MOVE TS-DATA            TO T2-DATA
               PERFORM P32-LINE-WRITE
             END-EVALUATE
             IF  SS-ITEM = 32767
               MOVE DFHRESP(ENDFILE)   TO EIBRESP
             END-IF
           END-PERFORM.

           EXEC CICS DELETEQ TS
                     QUEUE   (TS-QUEUE)
                     NOHANDLE
           END-EXEC.

           MOVE T2-QUEUE               TO TS-QUEUE.
           MOVE T2-ITEM                TO TS-TOTL.

           IF  TS-TOTL >= DETL-MAX
           OR  EIBRESP = DFHRESP(ITEMERR)
               MOVE DFHRESP(ENDFILE)   TO EIBRESP
           END-IF.

           GO TO P35-EXIT.

       P32-LINE-WRITE.
           EXEC CICS SUSPEND END-EXEC.
           MOVE LOW-VALUES             TO T2-SELECT.
           PERFORM Q63-WRITEQ-2-TS.

       P35-EXIT.
           EXIT.

       P40-FLOW-MAIL-BODY.
           COMPUTE SS-ITEM = TS-ITEM + ROW - 1.

      * locate the beginning of the paragraph as the first line,
      * a blank line, an HTML paragraph tag, or an HTML break tag
           PERFORM WITH TEST AFTER
             VARYING SS-ITEM FROM SS-ITEM BY -1
               UNTIL SS-ITEM <= 1
                  OR TS-DATA <= SPACES
                  OR TXT-STRING-WAS-FOUND
             PERFORM Q32-READQ-TS
             IF  TS-DATA > SPACES
               SET  TXT-REQUEST-FIND   TO TRUE
               MOVE ZEROES             TO TXT-PNTR
               MOVE +3                 TO TXT-PARM-STRLEN
               MOVE '<p>'              TO TXT-PARM-STRING
               MOVE LENGTH OF TS-DATA  TO TXT-MSTR-BUFLEN
               MOVE TS-DATA            TO TXT-MSTR-BUFFER
               INSPECT TXT-MSTR-BUFFER    CONVERTING UPPER-CASE
                                                  TO LOWER-CASE
               CALL 'TXTMAN'        USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
               IF  TXT-STRING-WAS-FOUND
                 MOVE TXT-PNTR         TO POS
                 INSPECT TS-DATA(POS:3)   CONVERTING UPPER-CASE
                                                  TO LOWER-CASE
               ELSE
                 MOVE ZEROES           TO TXT-PNTR
                 MOVE +4               TO TXT-PARM-STRLEN
                 MOVE '<br>'           TO TXT-PARM-STRING
                 CALL 'TXTMAN'      USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
                 IF  TXT-STRING-WAS-FOUND
                   MOVE TXT-PNTR       TO POS
                   INSPECT TS-DATA(POS:4) CONVERTING UPPER-CASE
                                                  TO LOWER-CASE
                 END-IF
               END-IF
             END-IF
           END-PERFORM.

           IF  TS-DATA <= SPACES
           OR  TS-DATA(POS:4) = '<br>'
               ADD  1                  TO SS-ITEM
               PERFORM Q32-READQ-TS
           END-IF.

      * paragraph starts here
           MOVE SS-ITEM                TO TS-ITEM.

      * extract words until end of paragraph as the last line,
      * a blank line, an HTML end paragraph tag, or an HTML break tag
           MOVE +5000                  TO TXT-MSTR-BUFLEN.
           MOVE SPACES                 TO TXT-MSTR-BUFFER.
           MOVE 1                      TO PTR.
           SET  TXT-STRING-NOT-FOUND   TO TRUE.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM TS-ITEM BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR TS-DATA <= SPACES
                  OR TXT-STRING-WAS-FOUND
             PERFORM Q32-READQ-TS
             IF  TS-DATA > SPACES
               MOVE SS-ITEM            TO T2-ITEM
      * ... parse out the words in the current line
               PERFORM P50-LEFT-JUSTIFY-TS-DATA THRU P55-EXIT
               MOVE 1                  TO POS
               PERFORM WITH TEST BEFORE
                 UNTIL POS > LEN
                   UNSTRING TS-DATA
                     DELIMITED BY ALL SPACES OR ALL LOW-VALUES
                     INTO VAR-FIND COUNT IN VAR-FINDL
                     WITH POINTER POS
                   END-UNSTRING
                   STRING VAR-FIND(1:VAR-FINDL) ' ' DELIMITED BY SIZE
                     INTO TXT-MSTR-BUFFER WITH POINTER PTR
                   END-STRING
                   IF  VAR-FIND(VAR-FINDL:1) = '.'
                     STRING ' ' DELIMITED BY SIZE
                       INTO TXT-MSTR-BUFFER WITH POINTER PTR
                     END-STRING
                   END-IF
               END-PERFORM
      * ... look for HTML end paragraph tag or HTML break tag
               SET  TXT-REQUEST-FIND   TO TRUE
               MOVE ZEROES             TO TXT-PNTR
               MOVE +4                 TO TXT-PARM-STRLEN
               MOVE '</p>'             TO TXT-PARM-STRING
               CALL 'TXTMAN'        USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
               IF  TXT-STRING-NOT-FOUND
                   MOVE ZEROES         TO TXT-PNTR
                   MOVE +4             TO TXT-PARM-STRLEN
                   MOVE '<br>'         TO TXT-PARM-STRING
                   CALL 'TXTMAN'    USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
               END-IF
             END-IF
      * end of extract
           END-PERFORM.

           SUBTRACT 2   FROM PTR   GIVING TXT-MSTR-BUFLEN.

      * reformat paragraph back into the same ts lines
           MOVE 1                      TO PTR.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM TS-ITEM BY 1
               UNTIL SS-ITEM > T2-ITEM
             PERFORM Q32-READQ-TS
             MOVE SPACES               TO TS-DATA
             IF  PTR <= TXT-MSTR-BUFLEN
                 MOVE LENGTH OF TS-DATA
                                       TO LEN
                 COMPUTE POS = PTR + LEN - 1
                 EVALUATE TRUE
                 WHEN TXT-MSTR-BUFFER(POS + 0:1) = SPACE
                     CONTINUE
                 WHEN TXT-MSTR-BUFFER(POS + 1:1) = SPACE
                     ADD  1            TO POS
                                          LEN
                 WHEN OTHER
                     PERFORM WITH TEST BEFORE
                       VARYING POS FROM POS BY -1
                         UNTIL TXT-MSTR-BUFFER(POS:1) = SPACE
                     END-PERFORM
                     SUBTRACT PTR    FROM POS
                                   GIVING LEN
                 END-EVALUATE
                 MOVE TXT-MSTR-BUFFER(PTR:LEN)
                                       TO TS-DATA(1:LEN)
                 COMPUTE PTR = PTR + LEN
                 IF  TXT-MSTR-BUFFER(PTR:1) = SPACE
                     ADD  1            TO PTR
                 END-IF
             END-IF
             PERFORM Q34-REWRITEQ-TS
           END-PERFORM.

           MOVE ZEROES                 TO EIBCPOSN.
       P45-EXIT.
           EXIT.

       P50-LEFT-JUSTIFY-TS-DATA.
           IF  TS-DATA <= SPACES
             MOVE ZEROES               TO LEN
           ELSE
             PERFORM WITH TEST BEFORE
               VARYING LEN FROM LENGTH OF TS-DATA BY -1
                 UNTIL LEN < 1 OR TS-DATA(LEN:1) > SPACE
             END-PERFORM
             PERFORM WITH TEST BEFORE
               VARYING LEN FROM LEN BY -1
                 UNTIL LEN < 1 OR TS-DATA(1:1) > SPACE
               MOVE TS-DATA(2:LEN - 1)   TO TS-DATA(1:LEN)
             END-PERFORM
           END-IF.
       P55-EXIT.
           EXIT.

       P60-EXTRACT-MAIL-DIST.

           PERFORM Q35-DELETEQ-TS.

           PERFORM Q70-INITKEY-MAILMDS.
           MOVE MMSG-SAVEKEY           TO MMDS-ID.
           PERFORM Q71-STARTBR-MAILMDS.

           MOVE ZEROES                 TO SS-ITEM.
           PERFORM WITH TEST BEFORE
             UNTIL MMDS-ID NOT = MMSG-SAVEKEY
                OR EIBRESP NOT = DFHRESP(NORMAL)
             PERFORM Q73-READNEXT-MAILMDS
             IF  EIBRESP = DFHRESP(NORMAL)
             AND MMDS-ID = MMSG-SAVEKEY
                 MOVE SPACES           TO TS-SELECT
                 MOVE MMDS-TO-GRP      TO TS-GRPNAME
                 PERFORM Q50-INITKEY-MAILDST
                 MOVE MMDS-TO-GRP      TO MDST-GRP-NAME
                 PERFORM Q56-READEQ-MAILDST
                 IF  EIBRESP = DFHRESP(NORMAL)
                   MOVE MDST-GRP-DESC  TO TS-GRPDESC
                 ELSE
                   MOVE ' Ýnot found¨' TO TS-GRPDESC
                 END-IF
                 PERFORM Q33-WRITEQ-TS
                 ADD  1                TO SS-ITEM
             END-IF
           END-PERFORM.

           IF  EIBRESP = DFHRESP(NORMAL)
                      OR DFHRESP(NOTFND)
                      OR DFHRESP(ENDFILE)
               PERFORM Q75-ENDBR-MAILMDS
               PERFORM Q31-NUMITEMS-TS
               IF  TS-TOTL <= ZERO
                   MOVE SPACES         TO TS-RECD
                   PERFORM Q33-WRITEQ-TS
                   PERFORM Q31-NUMITEMS-TS
               END-IF
           END-IF.

       P65-EXIT.
           EXIT.

       P70-EDIT-DIST-LINE.
           IF  DIST-SELECT-L(IDX) > ZERO
           OR  DIST-SELECT-F(IDX) = FLDMODFY OR FLDCURSM
               INSPECT DIST-SELECT-I(IDX) CONVERTING LOWER-CASE
                                                  TO UPPER-CASE
               IF  DIST-SELECT-I(IDX) > SPACES
               AND DIST-SELECT-I(IDX) NOT = 'D'
                 MOVE 'Option selection is invalid.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DIST-SELECT-L(IDX)
                                          DIST-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                 MOVE DIST-SELECT-I(IDX)
                                       TO TS-SELECT
               END-IF
           END-IF

           IF  TS-SELECT NOT = 'D'

               IF  SS-ITEM > TS-TOTL
               AND DIST-GRPNAME-I(IDX) <= SPACES
                 GO TO P75-EXIT
               END-IF

               IF  DIST-GRPNAME-L(IDX) > ZERO
               OR  DIST-GRPNAME-F(IDX) = FLDMODFY OR FLDCURSM
                 INSPECT DIST-GRPNAME-I(IDX) CONVERTING LOWER-CASE
                                                     TO UPPER-CASE
                 IF  DIST-GRPNAME-I(IDX) > SPACES
                   MOVE DIST-GRPNAME-I(IDX)
                                       TO TS-GRPNAME
                   PERFORM Q50-INITKEY-MAILDST
                   MOVE TS-GRPNAME     TO MDST-GRP-NAME
                   PERFORM Q56-READEQ-MAILDST
                   IF  EIBRESP = DFHRESP(NORMAL)
                     MOVE MDST-GRP-DESC TO TS-GRPDESC
                   ELSE
                     MOVE ' Ýnot found¨' TO TS-GRPDESC
                     MOVE DFHRESP(NORMAL) TO EIBRESP
                     MOVE 'Group name is not found.'
                                       TO MULTI-EDIT-MSG
                     CALL MULTMESG  USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          DIST-GRPNAME-L(IDX)
                                          DIST-MESSAGE-L
                                          MULTI-EDIT-MSG
                   END-IF
                 END-IF
               END-IF

           END-IF.

           IF  SS-ITEM > TS-TOTL
               PERFORM Q33-WRITEQ-TS
               ADD  1                  TO TS-TOTL
           ELSE
               PERFORM Q34-REWRITEQ-TS
           END-IF.
       P75-EXIT.
           EXIT.

       P90-RESEND-MAIL-MESSAGE.
           PERFORM Q20-INITKEY-MAILMID.
           MOVE MMSG-SAVEKEY           TO MMID-ID.
           PERFORM Q26-READUP-MAILMID.
           IF  EIBRESP = DFHRESP(NORMAL)
               MOVE ZEROES             TO MMID-SENT-TIMESTAMP
               SET  MMID-IS-COMPLETE   TO TRUE
               SET  MMID-NORMAL-DELIVERY TO TRUE
               PERFORM Q27-REWRITE-MAILMID
           END-IF.
       P95-EXIT.
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
       Q21-STARTEQ-MAILMID.
           EXEC CICS STARTBR
                     DATASET  (MAILMID)
                     RIDFLD   (MMID-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q22-RESETBR-MAILMID.
           EXEC CICS RESETBR
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
       Q23-CHK-MAILMID.
           IF  EIBRESP = DFHRESP(NORMAL)
               IF  MMID-ID = ZEROES
                   GO TO Q23-READNEXT-MAILMID
               END-IF
           END-IF.
       Q23-CHK-FILTER.
           IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q90-CHECK-FILTERS THRU Q95-EXIT
               IF  RECORD-NOT-SELECTED
                   GO TO Q23-READNEXT-MAILMID
               END-IF
           END-IF.
       Q24-READPREV-MAILMID.
           MOVE LENGTH OF MAILMID-RECORD TO MMID-RECL.
           EXEC CICS READPREV
                     DATASET  (MAILMID)
                     INTO     (MAILMID-RECORD)
                     LENGTH   (MMID-RECL)
                     RIDFLD   (MMID-KEY)
                     NOHANDLE
           END-EXEC.
       Q24-CHK-MAILMID.
           IF  EIBRESP = DFHRESP(NORMAL)
               IF  MMID-ID = ZEROES
                   GO TO Q24-READPREV-MAILMID
               END-IF
           END-IF.
       Q24-CHK-FILTER.
           IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q90-CHECK-FILTERS THRU Q95-EXIT
               IF  RECORD-NOT-SELECTED
                   GO TO Q24-READPREV-MAILMID
               END-IF
           END-IF.
       Q25-ENDBR-MAILMID.
           EXEC CICS ENDBR
                     DATASET  (MAILMID)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(INVREQ)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       Q26-READEQ-MAILMID.
           MOVE LENGTH OF MAILMID-RECORD TO MMID-RECL.
           EXEC CICS READ
                     DATASET  (MAILMID)
                     INTO     (MAILMID-RECORD)
                     LENGTH   (MMID-RECL)
                     RIDFLD   (MMID-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q26-READGE-MAILMID.
           MOVE LENGTH OF MAILMID-RECORD TO MMID-RECL.
           EXEC CICS READ
                     DATASET  (MAILMID)
                     INTO     (MAILMID-RECORD)
                     LENGTH   (MMID-RECL)
                     RIDFLD   (MMID-KEY)
                     GTEQ
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
       Q27-WRITE-MAILMID.
           COMPUTE MMID-RECL = LENGTH OF MMID-KEY
                             + LENGTH OF MMID-HDR.
           EXEC CICS WRITE
                     DATASET  (MAILMID)
                     FROM     (MAILMID-RECORD)
                     LENGTH   (MMID-RECL)
                     RIDFLD   (MMID-KEY)
                     NOHANDLE
           END-EXEC.
       Q27-REWRITE-MAILMID.
           IF  MMID-ID = ZERO
               COMPUTE MMID-RECL = LENGTH OF MMID-KEY
                                 + LENGTH OF MMID-CONTROL-ID
                                 + LENGTH OF MMID-CONTROL-LAST-STMP
           ELSE
               COMPUTE MMID-RECL = LENGTH OF MMID-KEY
                                 + LENGTH OF MMID-HDR
           END-IF.
           EXEC CICS REWRITE
                     DATASET  (MAILMID)
                     FROM     (MAILMID-RECORD)
                     LENGTH   (MMID-RECL)
                     NOHANDLE
           END-EXEC.
       Q28-DELETE-MAILMID.
           EXEC CICS DELETE
                     DATASET  (MAILMID)
                     NOHANDLE
           END-EXEC.
       Q28-UNLOCK-MAILMID.
           EXEC CICS UNLOCK
                     DATASET  (MAILMID)
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
       Q39-EXIT.
           EXIT.

       Q40-INITKEY-MAILMSG.
           IF  ADDRESS OF MAILMSG-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF MAILMSG-RECORD)
                         LENGTH   (LENGTH OF MAILMSG-RECORD)
               END-EXEC
           END-IF.
           MOVE ZEROES                 TO MMSG-ID
                                          MMSG-SEQ.
       Q41-STARTBR-MAILMSG.
           EXEC CICS STARTBR
                     DATASET  (MAILMSG)
                     RIDFLD   (MMSG-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q43-READNEXT-MAILMSG.
           MOVE LENGTH OF MAILMSG-RECORD TO MMSG-RECL.
           EXEC CICS READNEXT
                     DATASET  (MAILMSG)
                     INTO     (MAILMSG-RECORD)
                     LENGTH   (MMSG-RECL)
                     RIDFLD   (MMSG-KEY)
                     NOHANDLE
           END-EXEC.
       Q45-ENDBR-MAILMSG.
           EXEC CICS ENDBR
                     DATASET  (MAILMSG)
                     NOHANDLE
           END-EXEC.
       Q46-READUP-MAILMSG.
           MOVE LENGTH OF MAILMSG-RECORD TO MMSG-RECL.
           EXEC CICS READ
                     DATASET  (MAILMSG)
                     INTO     (MAILMSG-RECORD)
                     LENGTH   (MMSG-RECL)
                     RIDFLD   (MMSG-KEY)
                     EQUAL
                     UPDATE
                     NOHANDLE
           END-EXEC.
       Q47-WRITE-MAILMSG.
           PERFORM Q49-SET-BODY-LENGTH.
           COMPUTE MMSG-RECL = LENGTH OF MMSG-KEY + LEN.
           EXEC CICS WRITE
                     DATASET  (MAILMSG)
                     FROM     (MAILMSG-RECORD)
                     LENGTH   (MMSG-RECL)
                     RIDFLD   (MMSG-KEY)
                     NOHANDLE
           END-EXEC.
       Q47-REWRITE-MAILMSG.
           PERFORM Q49-SET-BODY-LENGTH.
           COMPUTE MMSG-RECL = LENGTH OF MMSG-KEY + LEN.
           EXEC CICS REWRITE
                     DATASET  (MAILMSG)
                     FROM     (MAILMSG-RECORD)
                     LENGTH   (MMSG-RECL)
                     NOHANDLE
           END-EXEC.
       Q48-DELETE-MAILMSG.
           EXEC CICS DELETE
                     DATASET  (MAILMSG)
                     NOHANDLE
           END-EXEC.
       Q49-SET-BODY-LENGTH.
           IF  MMSG-BODY > SPACES
               PERFORM WITH TEST BEFORE
                 VARYING LEN FROM LENGTH OF MMSG-BODY BY -1
                   UNTIL LEN < 1
                      OR MMSG-BODY(LEN:1) > SPACE
               END-PERFORM
           ELSE
               MOVE ZEROES             TO LEN
           END-IF.
       Q49-EXIT.
           EXIT.

       Q50-INITKEY-MAILDST.
           IF  ADDRESS OF MAILDST-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF MAILDST-RECORD)
                         LENGTH   (LENGTH OF MAILDST-RECORD)
               END-EXEC
           END-IF.
           MOVE SPACES                 TO MDST-KEY.
           SET  MDST-GRP-HEADER        TO TRUE.
       Q56-READEQ-MAILDST.
           MOVE LENGTH OF MAILDST-RECORD TO MDST-RECL.
           EXEC CICS READ
                     DATASET  (MAILDST)
                     INTO     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     RIDFLD   (MDST-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q59-EXIT.
           EXIT.

       Q63-WRITEQ-2-TS.
           EXEC CICS WRITEQ TS
                     QUEUE   (T2-QUEUE)
                     FROM    (T2-RECD)
                     ITEM    (T2-ITEM)
                     MAIN
                     NOHANDLE
           END-EXEC.
       Q65-DELETEQ-2-TS.
           MOVE TS-TRAN                TO T2-TRAN.
           MOVE TS-TERM                TO T2-TERM.
           EXEC CICS DELETEQ TS
                     QUEUE   (T2-QUEUE)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(QIDERR)
               MOVE DFHRESP(NORMAL) TO EIBRESP
           END-IF.
       Q69-EXIT.
           EXIT.

       Q70-INITKEY-MAILMDS.
           IF  ADDRESS OF MAILMDS-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF MAILMDS-RECORD)
                         LENGTH   (LENGTH OF MAILMDS-RECORD)
               END-EXEC
           END-IF.
           MOVE ZEROES                 TO MMDS-ID
                                          MMDS-SEQ.
       Q71-STARTBR-MAILMDS.
           EXEC CICS STARTBR
                     DATASET  (MAILMDS)
                     RIDFLD   (MMDS-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q73-READNEXT-MAILMDS.
           EXEC CICS READNEXT
                     DATASET  (MAILMDS)
                     INTO     (MAILMDS-RECORD)
                     RIDFLD   (MMDS-KEY)
                     NOHANDLE
           END-EXEC.
       Q75-ENDBR-MAILMDS.
           EXEC CICS ENDBR
                     DATASET  (MAILMDS)
                     NOHANDLE
           END-EXEC.
       Q76-READGT-MAILMDS.
           EXEC CICS READ
                     DATASET  (MAILMDS)
                     INTO     (MAILMDS-RECORD)
                     RIDFLD   (MMDS-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q76-READUP-MAILMDS.
           EXEC CICS READ
                     DATASET  (MAILMDS)
                     INTO     (MAILMDS-RECORD)
                     RIDFLD   (MMDS-KEY)
                     EQUAL
                     UPDATE
                     NOHANDLE
           END-EXEC.
       Q77-WRITE-MAILMDS.
           EXEC CICS WRITE
                     DATASET  (MAILMDS)
                     FROM     (MAILMDS-RECORD)
                     RIDFLD   (MMDS-KEY)
                     NOHANDLE
           END-EXEC.
       Q77-REWRITE-MAILMDS.
           EXEC CICS REWRITE
                     DATASET  (MAILMDS)
                     FROM     (MAILMDS-RECORD)
                     NOHANDLE
           END-EXEC.
       Q78-DELETE-MAILMDS.
           EXEC CICS DELETE
                     DATASET  (MAILMDS)
                     NOHANDLE
           END-EXEC.
       Q79-EXIT.
           EXIT.

       Q80-SAVE-FILTERS.
           IF  LIST-CREATED-FILTER-L > ZERO
           OR  LIST-CREATED-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-CREATED-FILTER-I <= SPACES
               AND LIST-CREATED-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-CREATED-FILTER
               ELSE
                   MOVE LIST-CREATED-FILTER-I
                                       TO SAVE-CREATED-FILTER
                   EVALUATE TRUE
                   WHEN SAVE-CREATED-FILTER NOT NUMERIC-FILTER-CHARS
                       MOVE 'Create date filter is invalid.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-CREATED-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                   WHEN (SAVE-CREATED-FILTER(1:1) = '<' OR '>')
                   AND SAVE-CREATED-FILTER(2:) > SPACES
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE SAVE-CREATED-FILTER(2:)
                                       TO DTE-GREG
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     IF  NOT DTE-REQUEST-COMPLETED
                      MOVE 'Create date filter has invalid date format.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-CREATED-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                     END-IF
                   END-EVALUATE
               END-IF
           END-IF.
           IF  SAVE-CREATED-FILTER(1:1) = '<' OR '>'
               MOVE SAVE-CREATED-FILTER
                                       TO LIST-CREATED-FILTER-O
           ELSE
               MOVE SPACES             TO LIST-CREATED-FILTER-O
               MOVE SAVE-CREATED-FILTER
                                       TO LIST-CREATED-FILTER-O(2:)
           END-IF.

           IF  LIST-CREATET-FILTER-L > ZERO
           OR  LIST-CREATET-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-CREATET-FILTER-I <= SPACES
               AND LIST-CREATET-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-CREATET-FILTER
               ELSE
                   MOVE LIST-CREATET-FILTER-I
                                       TO SAVE-CREATET-FILTER
                   EVALUATE TRUE
                   WHEN SAVE-CREATET-FILTER NOT NUMERIC-FILTER-CHARS
                       MOVE 'Create time filter is invalid.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-CREATET-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                   WHEN SAVE-CREATET-FILTER(1:1) = '<' OR '>'
                     SET NUM-REQUEST-NORMAL-EDIT TO TRUE
                     MOVE 06           TO NUM-TOTL
                     MOVE 0            TO NUM-DECM
                     MOVE SAVE-CREATET-FILTER(2:)
                                       TO NUM-DATA
                     CALL 'NUMMAN'  USING NUMMAN-PARMS
                     IF  NOT NUM-REQUEST-COMPLETED
                     OR  NUM-T06-D0 < ZEROES
                       MOVE 'Create time filter is invalid.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-CREATET-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                     ELSE
                       MOVE NUM-T06-D0 TO NUM-EDIT
                       MOVE NUM-EDIT   TO UNEX-VARIABLE
                       PERFORM Q203-LEFT-JUSTIFY
                       MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO SAVE-CREATET-FILTER(2:)
                     END-IF
                   END-EVALUATE
               END-IF
           END-IF.
           MOVE SAVE-CREATET-FILTER    TO LIST-CREATET-FILTER-O.

           IF  LIST-SENTDATE-FILTER-L > ZERO
           OR  LIST-SENTDATE-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-SENTDATE-FILTER-I <= SPACES
               AND LIST-SENTDATE-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-SENTDATE-FILTER
               ELSE
                   MOVE LIST-SENTDATE-FILTER-I
                                       TO SAVE-SENTDATE-FILTER
                   EVALUATE TRUE
                   WHEN SAVE-SENTDATE-FILTER NOT NUMERIC-FILTER-CHARS
                       MOVE 'Sent date filter is invalid.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-SENTDATE-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                   WHEN (SAVE-SENTDATE-FILTER(1:1) = '<' OR '>')
                   AND SAVE-SENTDATE-FILTER(2:) > SPACES
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE SAVE-SENTDATE-FILTER(2:)
                                       TO DTE-GREG
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     IF  NOT DTE-REQUEST-COMPLETED
                       MOVE 'Sent date filter has invalid date format.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-SENTDATE-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                     END-IF
                   END-EVALUATE
               END-IF
           END-IF.
           IF  SAVE-SENTDATE-FILTER(1:1) = '<' OR '>'
               MOVE SAVE-SENTDATE-FILTER
                                       TO LIST-SENTDATE-FILTER-O
           ELSE
               MOVE SPACES             TO LIST-SENTDATE-FILTER-O
               MOVE SAVE-SENTDATE-FILTER
                                       TO LIST-SENTDATE-FILTER-O(2:)
           END-IF.

           IF  LIST-SENTTIME-FILTER-L > ZERO
           OR  LIST-SENTTIME-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-SENTTIME-FILTER-I <= SPACES
               AND LIST-SENTTIME-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-SENTTIME-FILTER
               ELSE
                   MOVE LIST-SENTTIME-FILTER-I
                                       TO SAVE-SENTTIME-FILTER
                   EVALUATE TRUE
                   WHEN SAVE-SENTTIME-FILTER NOT NUMERIC-FILTER-CHARS
                       MOVE 'Sent time filter is invalid.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-SENTTIME-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                   WHEN SAVE-SENTTIME-FILTER(1:1) = '<' OR '>'
                     SET NUM-REQUEST-NORMAL-EDIT TO TRUE
                     MOVE 06           TO NUM-TOTL
                     MOVE 0            TO NUM-DECM
                     MOVE SAVE-SENTTIME-FILTER(2:)
                                       TO NUM-DATA
                     CALL 'NUMMAN'  USING NUMMAN-PARMS
                     IF  NOT NUM-REQUEST-COMPLETED
                     OR  NUM-T06-D0 < ZEROES
                       MOVE 'Sent time filter is invalid.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-SENTTIME-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                     ELSE
                       MOVE NUM-T06-D0 TO NUM-EDIT
                       MOVE NUM-EDIT   TO UNEX-VARIABLE
                       PERFORM Q203-LEFT-JUSTIFY
                       MOVE UNEX-VARIABLE(1:UNEX-LENG)
                                       TO SAVE-SENTTIME-FILTER(2:)
                     END-IF
                   END-EVALUATE
               END-IF
           END-IF.
           MOVE SAVE-SENTTIME-FILTER    TO LIST-SENTTIME-FILTER-O.

           IF  LIST-SYSTEM-FILTER-L > ZERO
           OR  LIST-SYSTEM-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-SYSTEM-FILTER-I <= SPACES
               AND LIST-SYSTEM-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-SYSTEM-FILTER
               ELSE
                   MOVE LIST-SYSTEM-FILTER-I
                                       TO SAVE-SYSTEM-FILTER
               END-IF
           END-IF.
           MOVE SAVE-SYSTEM-FILTER     TO LIST-SYSTEM-FILTER-O.

           IF  LIST-TO-GRP-FILTER-L > ZERO
           OR  LIST-TO-GRP-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-TO-GRP-FILTER-I <= SPACES
               AND LIST-TO-GRP-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-TO-GRP-FILTER
               ELSE
                   MOVE LIST-TO-GRP-FILTER-I
                                       TO SAVE-TO-GRP-FILTER
               END-IF
           END-IF.
           MOVE SAVE-TO-GRP-FILTER     TO LIST-TO-GRP-FILTER-O.

           IF  LIST-SUBJECT-FILTER-L > ZERO
           OR  LIST-SUBJECT-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-SUBJECT-FILTER-I <= SPACES
               AND LIST-SUBJECT-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-SUBJECT-FILTER
               ELSE
                   MOVE LIST-SUBJECT-FILTER-I
                                       TO SAVE-SUBJECT-FILTER
               END-IF
           END-IF.
           MOVE SAVE-SUBJECT-FILTER    TO LIST-SUBJECT-FILTER-O.
       Q85-EXIT.
           EXIT.

       Q90-CHECK-FILTERS.
           SET  WILDCOMP-PGM           TO TRUE.
           SET  RECORD-NOT-SELECTED    TO TRUE.

           IF  SAVE-CREATED-FILTER NOT = '*'
               EXEC CICS FORMATTIME
                         ABSTIME(MMID-CREATED-TIMESTAMP)
                         YYYYMMDD(NUM-DATE)
               END-EXEC
               MOVE LENGTH OF SAVE-CREATED-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-CREATED-FILTER(1:1)
               WHEN '<'
                   IF  MMID-CREATED-TIMESTAMP <= ZEROES
                     GO TO Q95-EXIT
                   ELSE
                     IF  SAVE-CREATED-FILTER = '<'
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE CICS-CCYYMMDD TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       SET DTE-REQUEST-TOTL-DAYS TO TRUE
                       SUBTRACT 365  FROM DTE-TOTD
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     ELSE
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE SAVE-CREATED-FILTER(2:)
                                       TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     END-IF
                     MOVE NUM-DATE     TO WILDCOMP-STR2
                     IF  WILDCOMP-STR1 < WILDCOMP-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN '>'
                   IF  MMID-CREATED-TIMESTAMP <= ZEROES
                     GO TO Q95-EXIT
                   ELSE
                     IF  SAVE-CREATED-FILTER = '>'
                       MOVE CICS-CCYYMMDD TO WILDCOMP-STR1
                     ELSE
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE SAVE-CREATED-FILTER(2:)
                                       TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     END-IF
                     MOVE NUM-DATE     TO WILDCOMP-STR2
                     IF  WILDCOMP-STR1 > WILDCOMP-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN OTHER
                   MOVE SAVE-CREATED-FILTER
                                       TO WILDCOMP-STR1
                   MOVE NUM-DATE       TO WILDCOMP-STR2
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

           IF  SAVE-CREATET-FILTER NOT = '*'
               EXEC CICS FORMATTIME
                         ABSTIME(MMID-CREATED-TIMESTAMP)
                         TIME(NUM-TIME)
               END-EXEC
               MOVE LENGTH OF SAVE-CREATET-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-CREATET-FILTER(1:1)
               WHEN '<'
                   SET NUM-REQUEST-NORMAL-EDIT TO TRUE
                   MOVE 06             TO NUM-TOTL
                   MOVE 0              TO NUM-DECM
                   MOVE SAVE-CREATET-FILTER(2:)
                                       TO NUM-DATA
                   CALL 'NUMMAN'    USING NUMMAN-PARMS
                   IF  NUM-TIME > NUM-T06-D0
                       GO TO Q95-EXIT
                   END-IF
               WHEN '>'
                   SET NUM-REQUEST-NORMAL-EDIT TO TRUE
                   MOVE 06             TO NUM-TOTL
                   MOVE 0              TO NUM-DECM
                   MOVE SAVE-CREATET-FILTER(2:)
                                       TO NUM-DATA
                   CALL 'NUMMAN'    USING NUMMAN-PARMS
                   IF  NUM-TIME < NUM-T06-D0
                       GO TO Q95-EXIT
                   END-IF
               WHEN OTHER
                   MOVE SAVE-CREATET-FILTER
                                       TO WILDCOMP-STR1
                   MOVE NUM-TIME       TO WILDCOMP-STR2
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

           IF  SAVE-SENTDATE-FILTER NOT = '*'
               EXEC CICS FORMATTIME
                         ABSTIME(MMID-SENT-TIMESTAMP)
                         YYYYMMDD(NUM-DATE)
               END-EXEC
               MOVE LENGTH OF SAVE-SENTDATE-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-SENTDATE-FILTER(1:1)
               WHEN '<'
                   IF  MMID-SENT-TIMESTAMP <= ZEROES
                     GO TO Q95-EXIT
                   ELSE
                     IF  SAVE-SENTDATE-FILTER = '<'
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE CICS-CCYYMMDD
                                       TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       SET DTE-REQUEST-TOTL-DAYS TO TRUE
                       SUBTRACT 365  FROM DTE-TOTD
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     ELSE
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE SAVE-SENTDATE-FILTER(2:)
                                       TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     END-IF
                     MOVE NUM-DATE     TO WILDCOMP-STR2
                     IF  WILDCOMP-STR1 < WILDCOMP-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN '>'
                   IF  MMID-SENT-TIMESTAMP <= ZEROES
                     GO TO Q95-EXIT
                   ELSE
                     IF  SAVE-SENTDATE-FILTER = '>'
                       MOVE CICS-CCYYMMDD TO WILDCOMP-STR1
                     ELSE
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE SAVE-SENTDATE-FILTER(2:)
                                       TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     END-IF
                     MOVE NUM-DATE     TO WILDCOMP-STR2
                     IF  WILDCOMP-STR1 > WILDCOMP-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN OTHER
                   MOVE SAVE-SENTDATE-FILTER
                                       TO WILDCOMP-STR1
                   IF  MMID-SENT-TIMESTAMP = ZEROES
                       MOVE SPACES     TO WILDCOMP-STR2
                   ELSE
                       MOVE NUM-DATE   TO WILDCOMP-STR2
                   END-IF
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

           IF  SAVE-SENTTIME-FILTER NOT = '*'
               EXEC CICS FORMATTIME
                         ABSTIME(MMID-SENT-TIMESTAMP)
                         TIME(NUM-TIME)
               END-EXEC
               MOVE LENGTH OF SAVE-SENTTIME-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-SENTTIME-FILTER(1:1)
               WHEN '<'
                   SET NUM-REQUEST-NORMAL-EDIT TO TRUE
                   MOVE 06             TO NUM-TOTL
                   MOVE 0              TO NUM-DECM
                   MOVE SAVE-SENTTIME-FILTER(2:)
                                       TO NUM-DATA
                   CALL 'NUMMAN'    USING NUMMAN-PARMS
                   IF  NUM-TIME > NUM-T06-D0
                       GO TO Q95-EXIT
                   END-IF
               WHEN '>'
                   SET NUM-REQUEST-NORMAL-EDIT TO TRUE
                   MOVE 06             TO NUM-TOTL
                   MOVE 0              TO NUM-DECM
                   MOVE SAVE-SENTTIME-FILTER(2:)
                                       TO NUM-DATA
                   CALL 'NUMMAN'    USING NUMMAN-PARMS
                   IF  NUM-TIME < NUM-T06-D0
                       GO TO Q95-EXIT
                   END-IF
               WHEN OTHER
                   MOVE SAVE-SENTTIME-FILTER
                                       TO WILDCOMP-STR1
                   IF  MMID-SENT-TIMESTAMP = ZEROES
                       MOVE SPACES     TO WILDCOMP-STR2
                   ELSE
                       MOVE NUM-TIME   TO WILDCOMP-STR2
                   END-IF
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

           IF  SAVE-SYSTEM-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-SYSTEM-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-SYSTEM-FILTER(1:1)
               WHEN '<'
                   IF  SAVE-SYSTEM-FILTER(2:) <= SPACES
                     IF  MMID-SYSTEM > SPACES
                       GO TO Q95-EXIT
                     END-IF
                     GO TO Q92-ACCEPTED-SYSTEM
                   ELSE
                     MOVE SAVE-SYSTEM-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE MMID-SYSTEM  TO WILDCOMP-STR2
                     CALL 'WILDCOMP' USING WILDCOMP-PARMS
                     IF  NOT WILDCOMP-STR1-GT-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN '>'
                   IF  SAVE-SYSTEM-FILTER(2:) <= SPACES
                     IF  MMID-SYSTEM <= SPACES
                       GO TO Q95-EXIT
                     END-IF
                     GO TO Q92-ACCEPTED-SYSTEM
                   ELSE
                     MOVE SAVE-SYSTEM-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE MMID-SYSTEM  TO WILDCOMP-STR2
                     CALL 'WILDCOMP' USING WILDCOMP-PARMS
                     IF  NOT WILDCOMP-STR1-LT-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN OTHER
                   MOVE SAVE-SYSTEM-FILTER
                                       TO WILDCOMP-STR1
                   MOVE MMID-SYSTEM    TO WILDCOMP-STR2
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

       Q92-ACCEPTED-SYSTEM.

           IF  SAVE-TO-GRP-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-TO-GRP-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-TO-GRP-FILTER(1:1)
               WHEN '<'
                   IF  SAVE-TO-GRP-FILTER(2:) <= SPACES
                     IF  MMID-TO-GRP > SPACES
                       GO TO Q95-EXIT
                     END-IF
                     GO TO Q93-ACCEPTED-TO-GRP
                   ELSE
                     MOVE SAVE-TO-GRP-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE MMID-TO-GRP  TO WILDCOMP-STR2
                     CALL 'WILDCOMP' USING WILDCOMP-PARMS
                     IF  NOT WILDCOMP-STR1-GT-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN '>'
                   IF  SAVE-TO-GRP-FILTER(2:) <= SPACES
                     IF  MMID-TO-GRP <= SPACES
                       GO TO Q95-EXIT
                     END-IF
                     GO TO Q93-ACCEPTED-TO-GRP
                   ELSE
                     MOVE SAVE-TO-GRP-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE MMID-TO-GRP  TO WILDCOMP-STR2
                     CALL 'WILDCOMP' USING WILDCOMP-PARMS
                     IF  NOT WILDCOMP-STR1-LT-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN OTHER
                   MOVE SAVE-TO-GRP-FILTER
                                       TO WILDCOMP-STR1
                   MOVE MMID-TO-GRP    TO WILDCOMP-STR2
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

       Q93-ACCEPTED-TO-GRP.

           IF  SAVE-SUBJECT-FILTER NOT = '*'
      * too big
      *        MOVE LENGTH OF SAVE-SUBJECT-FILTER
               MOVE LENGTH OF WILDCOMP-STR1
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-SUBJECT-FILTER(1:1)
               WHEN '<'
                   IF  SAVE-SUBJECT-FILTER(2:) <= SPACES
                     IF  MMID-SUBJECT > SPACES
                       GO TO Q95-EXIT
                     END-IF
                     GO TO Q94-ACCEPTED-SUBJECT
                   ELSE
                     MOVE SAVE-SUBJECT-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE MMID-SUBJECT TO WILDCOMP-STR2
                   END-IF
               WHEN '>'
                   IF  SAVE-SUBJECT-FILTER(2:) <= SPACES
                     IF  MMID-SUBJECT <= SPACES
                       GO TO Q95-EXIT
                     END-IF
                     GO TO Q94-ACCEPTED-SUBJECT
                   ELSE
                     MOVE SAVE-SUBJECT-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE MMID-SUBJECT TO WILDCOMP-STR2
                   END-IF
               WHEN OTHER
                   MOVE SAVE-SUBJECT-FILTER
                                       TO WILDCOMP-STR1
                   MOVE MMID-SUBJECT   TO WILDCOMP-STR2
               END-EVALUATE
               INSPECT WILDCOMP-STR1 CONVERTING LOWER-CASE
                                             TO UPPER-CASE
               INSPECT WILDCOMP-STR2 CONVERTING LOWER-CASE
                                             TO UPPER-CASE
               CALL 'WILDCOMP'      USING WILDCOMP-PARMS
               EVALUATE SAVE-SUBJECT-FILTER(1:1)
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

       Q94-ACCEPTED-SUBJECT.

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

       Z70-MMSG-CLOSED.

           EXEC CICS SET TERMINAL(EIBTRMID)
                     UCTRAN
                     NOHANDLE
           END-EXEC.

           COPY COMMXCTL.
      * the above transfers control to standard NOT OPEN screen

       Z80-MMSG-RETURN.

           COPY COMMRETN.
      * the above copybook is a psuedo-conversational return to cics

       Z90-MMSG-TERMINATION.

           EXEC CICS SET TERMINAL(EIBTRMID)
                     UCTRAN
                     NOHANDLE
           END-EXEC.

      * use the following if you don't want to pass back any data
           COPY COMMEXIT REPLACING DFHCOMMAREA BY COMM-HEADER.

      * the above copybook ends with a return to cics and GOBACK

