      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    MAILWRTC.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  JUNE 2019.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      EXECUTED VIA CICS LINK, ONLY.
      *REMARKS.       MAINFRAME MAIL CICS WRITER PROGRAM.

      * CHANGE HISTORY ------------------------------------------------
      * 06/11/2019 DLC ORIGINAL PROGRAM.
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
         COPY COMMWORK REPLACING =='TRAN'==    BY =='NONE'==
                                 =='PROGRAM'== BY =='MAILWRTC'==.
      *
      * FIELDS TO MANAGE REFERENCED FILES
         03  MAILMID                   PIC  X(08)   VALUE 'MAILMID '.
         03  MAILMSG                   PIC  X(08)   VALUE 'MAILMSG '.
         03  MAILDST                   PIC  X(08)   VALUE 'MAILDST '.
         03  MAILMDS                   PIC  X(08)   VALUE 'MAILMDS '.
      *
      * FIELDS TO MANAGE REFERENCED EXTERNAL PROGRAM NAMES
         03  CICSINFO                  PIC  X(08)   VALUE 'CICSINFO'.
      *
      * FIELDS TO MANAGE ERROR HANDLING
         03  ERROR-SWITCH              PIC S9(01)   VALUE ZEROES.
           88  NO-ERRORS-FOUND                      VALUE ZEROES.
           88  ERROR-AT-CURSOR                      VALUE -1.
           88  ERRORS-FOUND                         VALUES -9 THRU -1.
         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.
         03  CNT                       PIC S9(04)   BINARY VALUE ZEROES.
         03  LEN                       PIC S9(04)   BINARY VALUE ZEROES.
         03  MMID-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
         03  MMSG-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
         03  MDST-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
         03  WS-ABSTIME                PIC S9(15)   COMP-3 VALUE ZEROES.
         03  VAR-TEXTL                 PIC S9(4)    BINARY.
         03  VAR-TEXT                  PIC  X(245)  VALUE SPACES.

      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS

       01  CICSINFO-PARMS.
         COPY CICSINFO.

       COPY HEXMAN.

       COPY LOGGING.

       COPY UNEXERRW.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA.
         COPY MAILWRTR.

      * MAILMID RECORD I/O AREA
       COPY MAILMID.

      * MAILMSG RECORD I/O AREA
       COPY MAILMSG.

      * MAILDST RECORD I/O AREA
       COPY MAILDST.

      * MAILMDS RECORD I/O AREA
       COPY MAILMDS.

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
           EXEC CICS ASSIGN
                     INVOKINGPROG(CICS-INVOKEDBY)
                     NOHANDLE
           END-EXEC.

           IF  EIBCALEN NOT = LENGTH OF DFHCOMMAREA
               STRING 'Caller '           DELIMITED BY SIZE
                      CICS-INVOKEDBY      DELIMITED BY SPACE
                      ' invalid commarea length.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               EXEC CICS ABEND
                         ABCODE('MCOM')
                         CANCEL
               END-EXEC
           END-IF.

           IF  NOT MAIL-HEAD-PARMS
           AND MAIL-ID NOT > ZEROES
               STRING 'Caller '           DELIMITED BY SIZE
                      CICS-INVOKEDBY      DELIMITED BY SPACE
                      ' link sequence error.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               EXEC CICS ABEND
                         ABCODE('MLNK')
                         CANCEL
               END-EXEC
           END-IF.

           IF  MAIL-DIST-PARMS
           AND MAIL-SEQ > ZEROES
               STRING 'Caller '           DELIMITED BY SIZE
                      CICS-INVOKEDBY      DELIMITED BY SPACE
                      ' distribution sequence error.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               EXEC CICS ABEND
                         ABCODE('MSEQ')
                         CANCEL
               END-EXEC
           END-IF.

           IF  MAIL-BODY-PARMS
           AND MAIL-SEQ >= 3000
               STRING 'Caller '           DELIMITED BY SIZE
                      CICS-INVOKEDBY      DELIMITED BY SPACE
                      ' body limit reached.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               EXEC CICS ABEND
                         ABCODE('MMAX')
                         CANCEL
               END-EXEC
           END-IF.

           EVALUATE TRUE
           WHEN MAIL-HEAD-PARMS     GO TO C00-MAIL-HEADER
           WHEN MAIL-DIST-PARMS     GO TO D00-MAIL-DISTRIBUTION
           WHEN MAIL-BODY-PARMS     GO TO E00-MAIL-BODY
           WHEN MAIL-MESG-COMPLETE  GO TO F00-MAIL-COMPLETE
           WHEN MAIL-PURGE-MESG     GO TO G00-MAIL-PURGE
           END-EVALUATE.

           STRING 'Caller '               DELIMITED BY SIZE
                  CICS-INVOKEDBY          DELIMITED BY SPACE
                  ' link control error.'
               DELIMITED BY SIZE     INTO LOGF-MESG.
           PERFORM Q100-LOGIT THRU Q199-EXIT.
           EXEC CICS ABEND
                     ABCODE('MCTL')
                     CANCEL
           END-EXEC.

      /*****************************************************************
      *    MAIL HEADER ROUTINE                                         *
      ******************************************************************
       C00-MAIL-HEADER.

           IF  MAIL-ID > ZEROES
               PERFORM F00-MAIL-COMPLETE
           END-IF.

           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC.

           PERFORM Q20-INITKEY-MAILMID.
      * get message id control record
           MOVE ZERO                   TO MMID-ID.
           PERFORM Q26-READUP-MAILMID.

           IF  EIBRESP = DFHRESP(NORMAL)
      * increment message id control record
               ADD  1                  TO MMID-CONTROL-ID
               MOVE WS-ABSTIME         TO MMID-CONTROL-LAST-STMP
      * rewrite message id control record
               PERFORM Q27-REWRITE-MAILMID
           END-IF.

           IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q40-INITKEY-MAILDST
      * get mail distribution header record
               MOVE MAIL-HEAD-TO-GRP   TO MDST-GRP-NAME
               PERFORM Q46-READEQ-MAILDST
               IF  EIBRESP = DFHRESP(NOTFND)
      * report errors and abend process
                   STRING 'Caller '         DELIMITED BY SIZE
                          CICS-INVOKEDBY    DELIMITED BY SPACE
                          ' primary group ' DELIMITED BY SIZE
                          MDST-GRP-NAME     DELIMITED BY SPACE
                          ' not found.'     DELIMITED BY SIZE
                                     INTO LOGF-MESG
                   PERFORM Q200-UNEX-LOG
                   EXEC CICS ABEND
                             ABCODE('MDST')
                             CANCEL
                   END-EXEC
               END-IF
           END-IF.

           IF  EIBRESP = DFHRESP(NORMAL)
      * build message id header
               MOVE MMID-CONTROL-ID    TO MMID-ID
                                          MAIL-ID
               MOVE ZEROES             TO MAIL-SEQ
               INITIALIZE MMID-HDR
               MOVE WS-ABSTIME         TO MMID-CREATED-TIMESTAMP
               MOVE ZEROES             TO MMID-CHANGED-TIMESTAMP
                                          MMID-SENT-TIMESTAMP
               SET  MMID-NOT-COMPLETE  TO TRUE
               IF  MAIL-HEAD-DELIVER = 'Y'
                 SET  MMID-NORMAL-DELIVERY TO TRUE
               END-IF
               IF  MAIL-HEAD-PRIVATE = 'Y'
                 SET  MMID-IS-PRIVATE  TO TRUE
               END-IF
               MOVE MAIL-HEAD-SYSTEM   TO MMID-SYSTEM
               MOVE MAIL-HEAD-TO-GRP   TO MMID-TO-GRP
               MOVE MAIL-HEAD-FROM     TO MMID-FROM
               MOVE MAIL-HEAD-SUBJECT  TO MMID-SUBJECT
               MOVE CICS-USERID        TO MMID-CREATED-BY
      * write message id header
               PERFORM Q27-WRITE-MAILMID
           END-IF.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
      * report errors and abend process
               STRING 'Caller '           DELIMITED BY SIZE
                      CICS-INVOKEDBY      DELIMITED BY SPACE
                      ' failure creating mail header.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q200-UNEX-LOG
               EXEC CICS ABEND
                         ABCODE('MLOG')
                         CANCEL
               END-EXEC
           END-IF.

           GO TO Z00-MAIL-TERMINATION.

      /*****************************************************************
      *    MAIL DISTRIBUTION ROUTINE                                   *
      ******************************************************************
       D00-MAIL-DISTRIBUTION.

           PERFORM Q40-INITKEY-MAILDST.
      * get mail distribution header record
           MOVE MAIL-DIST-TO-GRP       TO MDST-GRP-NAME.
           PERFORM Q46-READEQ-MAILDST.
           IF  EIBRESP = DFHRESP(NOTFND)
               STRING 'Caller '           DELIMITED BY SIZE
                      CICS-INVOKEDBY      DELIMITED BY SPACE
                      ' extended group '  DELIMITED BY SIZE
                      MDST-GRP-NAME       DELIMITED BY SPACE
                      ' not found.'       DELIMITED BY SIZE
                                     INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               GO TO Z00-MAIL-TERMINATION
           END-IF.

           IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q50-INITKEY-MAILMDS
      * get extended message distribution sequence number
               MOVE HIGH-VALUES        TO MMDS-KEY
      *     find last record for this message id
               MOVE MAIL-ID            TO MMDS-ID
               PERFORM Q51-STARTBR-MAILMDS
               IF  EIBRESP = DFHRESP(NOTFND)
      *     else find last record on file
                   MOVE HIGH-VALUES    TO MMDS-KEY
                   PERFORM Q52-STARTEQ-MAILMDS
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
      *     get that record
                   PERFORM Q54-READPREV-MAILMDS
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
                   PERFORM Q55-ENDBR-MAILMDS
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
                          OR DFHRESP(NOTFND)
      * build extended message distribution record
                   IF  MMDS-ID = MAIL-ID
                       ADD  1          TO MMDS-SEQ
                   ELSE
                       MOVE MAIL-ID    TO MMDS-ID
                       MOVE 1          TO MMDS-SEQ
                   END-IF
                   MOVE MAIL-DIST-TO-GRP
                                       TO MMDS-TO-GRP
      * write extended message distribution record
                   PERFORM Q57-WRITE-MAILMDS
               END-IF
           END-IF.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
      * report errors and abend process
               STRING 'Caller '           DELIMITED BY SIZE
                      CICS-INVOKEDBY      DELIMITED BY SPACE
                      ' failure extending mail distribution.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q200-UNEX-LOG
               EXEC CICS ABEND
                         ABCODE('MLOG')
                         CANCEL
               END-EXEC
           END-IF.

           GO TO Z00-MAIL-TERMINATION.

      /*****************************************************************
      *    MAIL BODY ROUTINE                                           *
      ******************************************************************
       E00-MAIL-BODY.

           PERFORM Q30-INITKEY-MAILMSG.
      * build message body record
           MOVE MAIL-ID                TO MMSG-ID.
           ADD  1                      TO MAIL-SEQ.
           MOVE MAIL-SEQ               TO MMSG-SEQ.
           MOVE MAIL-BODY-TEXT         TO MMSG-BODY.
      * write message body record
           PERFORM Q37-WRITE-MAILMSG.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
      * report errors and abend process
               STRING 'Caller '           DELIMITED BY SIZE
                      CICS-INVOKEDBY      DELIMITED BY SPACE
                      ' failure creating mail body.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q200-UNEX-LOG
               EXEC CICS ABEND
                         ABCODE('MLOG')
                         CANCEL
               END-EXEC
           END-IF.

           GO TO Z00-MAIL-TERMINATION.

      /*****************************************************************
      *    MAIL COMPLETE ROUTINE                                       *
      ******************************************************************
       F00-MAIL-COMPLETE.

           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC.

           PERFORM Q20-INITKEY-MAILMID.
      * get message id header record
           MOVE MAIL-ID                TO MMID-ID.
           PERFORM Q26-READUP-MAILMID.

           IF  EIBRESP = DFHRESP(NORMAL)
      * change message id header record
               MOVE WS-ABSTIME         TO MMID-CHANGED-TIMESTAMP
               SET  MMID-IS-COMPLETE   TO TRUE
               MOVE CICS-USERID        TO MMID-UPDATED-BY
      * rewrite message id header record
               PERFORM Q27-REWRITE-MAILMID
           END-IF.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
      * report errors and abend process
               STRING 'Caller '           DELIMITED BY SIZE
                      CICS-INVOKEDBY      DELIMITED BY SPACE
                      ' failure completing mail process.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q200-UNEX-LOG
               EXEC CICS ABEND
                         ABCODE('MLOG')
                         CANCEL
               END-EXEC
           END-IF.

           GO TO Z00-MAIL-TERMINATION.

      /*****************************************************************
      *    MAIL PURGE ROUTINE                                          *
      ******************************************************************
       G00-MAIL-PURGE.

           PERFORM Q20-INITKEY-MAILMID.
      * get message id header record
           MOVE MAIL-ID                TO MMID-ID.
           PERFORM Q26-READUP-MAILMID.

           IF  EIBRESP = DFHRESP(NORMAL)
      * delete message body records
               MOVE MMID-ID            TO MMSG-ID
               PERFORM Q38-DELETE-MAILMSG
           END-IF.

           IF  EIBRESP = DFHRESP(NORMAL)
      * delete message id header record
               PERFORM Q28-DELETE-MAILMID
           END-IF.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
      * report errors and abend process
               STRING 'Caller '           DELIMITED BY SIZE
                      CICS-INVOKEDBY      DELIMITED BY SPACE
                      ' failure purging mail message.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q200-UNEX-LOG
               EXEC CICS ABEND
                         ABCODE('MLOG')
                         CANCEL
               END-EXEC
           END-IF.

           GO TO Z00-MAIL-TERMINATION.

      /*****************************************************************
      *    PERFORMED ROUTINES                                          *
      ******************************************************************

      /*****************************************************************
      *    MORE PERFORMED ROUTINES                                     *
      ******************************************************************

       Q20-INITKEY-MAILMID.
           IF  ADDRESS OF MAILMID-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF MAILMID-RECORD)
                         LENGTH   (LENGTH OF MAILMID-RECORD)
               END-EXEC
           END-IF.
           MOVE 1                      TO MMID-ID.
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
       Q37-WRITE-MAILMSG.
           PERFORM Q39-SET-BODY-LENGTH.
           COMPUTE MMSG-RECL = LENGTH OF MMSG-KEY + LEN.
           EXEC CICS WRITE
                     DATASET  (MAILMSG)
                     FROM     (MAILMSG-RECORD)
                     LENGTH   (MMSG-RECL)
                     RIDFLD   (MMSG-KEY)
                     NOHANDLE
           END-EXEC.
       Q38-DELETE-MAILMSG.
           EXEC CICS DELETE
                     DATASET  (MAILMSG)
                     RIDFLD   (MMSG-ID)
                     KEYLENGTH(LENGTH OF MMSG-ID)
                     GENERIC
                     NOHANDLE
           END-EXEC.
       Q39-SET-BODY-LENGTH.
           IF  MMSG-BODY > SPACES
               PERFORM WITH TEST BEFORE
                 VARYING LEN FROM LENGTH OF MMSG-BODY BY -1
                   UNTIL LEN < 1
                      OR MMSG-BODY(LEN:1) > SPACE
               END-PERFORM
           ELSE
               MOVE ZEROES             TO LEN
           END-IF.
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
       Q46-READEQ-MAILDST.
           MOVE LENGTH OF MAILDST-RECORD TO MDST-RECL.
           EXEC CICS READ
                     DATASET  (MAILDST)
                     INTO     (MAILDST-RECORD)
                     LENGTH   (MDST-RECL)
                     RIDFLD   (MDST-KEY)
                     EQUAL
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
           INITIALIZE MAILMDS-RECORD.
       Q51-STARTBR-MAILMDS.
           EXEC CICS STARTBR
                     DATASET  (MAILMDS)
                     RIDFLD   (MMDS-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q52-STARTEQ-MAILMDS.
           EXEC CICS STARTBR
                     DATASET  (MAILMDS)
                     RIDFLD   (MMDS-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q54-READPREV-MAILMDS.
           EXEC CICS READPREV
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
       Q57-WRITE-MAILMDS.
           EXEC CICS WRITE
                     DATASET  (MAILMDS)
                     FROM     (MAILMDS-RECORD)
                     RIDFLD   (MMDS-KEY)
                     NOHANDLE
           END-EXEC.
       Q59-EXIT.
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

           EXEC CICS RETURN END-EXEC.
           GOBACK.
