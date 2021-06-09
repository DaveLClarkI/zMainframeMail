      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    MAILWRTB.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  JUNE 2019.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
      *REMARKS.       MAINFRAME MAIL BATCH WRITER PROGRAM.

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

      ******************************************************************
      *    INPUT-OUTPUT SECTION                                        *
      ******************************************************************
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT MAILMID
               ASSIGN TO MAILMID
               ACCESS IS RANDOM  INDEXED
               RECORD KEY IS MMID-KEY
               FILE STATUS IS FILE1-STAT
                              FILE1-FDBK.

           SELECT MAILMSG
               ASSIGN TO MAILMSG
               ACCESS IS DYNAMIC INDEXED
               RECORD KEY IS MMSG-KEY
               FILE STATUS IS FILE2-STAT
                              FILE2-FDBK.

           SELECT MAILDST
               ASSIGN TO MAILDST
               ACCESS IS RANDOM  INDEXED
               RECORD KEY IS MDST-KEY
               FILE STATUS IS FILE3-STAT
                              FILE3-FDBK.

           SELECT MAILMDS
               ASSIGN TO MAILMDS
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS MMDS-KEY
               FILE STATUS IS FILE4-STAT
                              FILE4-FDBK.

      /*****************************************************************
      *                                                                *
      *    DATA DIVISION                                               *
      *                                                                *
      ******************************************************************
       DATA DIVISION.

      ******************************************************************
      *    FILE SECTION                                                *
      ******************************************************************
       FILE SECTION.

       FD  MAILMID
           RECORD IS VARYING IN SIZE
             FROM 8 TO 255
             DEPENDING ON MMID-RECL.
       COPY MAILMID.

       FD  MAILMSG
           RECORD IS VARYING IN SIZE
             FROM 11 TO 255
             DEPENDING ON MMSG-RECL.
       COPY MAILMSG.

       FD  MAILDST
           RECORD IS VARYING IN SIZE
             FROM 28 TO 255
             DEPENDING ON MDST-RECL.
       COPY MAILDST.

       FD  MAILMDS.
       COPY MAILMDS.

      ******************************************************************
      *    WORKING-STORAGE SECTION                                     *
      ******************************************************************
       WORKING-STORAGE SECTION.

       01  CONTROL-FIELDS.
         03  FILLER                    PIC  X(11)   VALUE '**STORAGE**'.
         03  LOWVALUE                  PIC  X(1)    VALUE LOW-VALUE.
         03  THIS-PGM                  PIC  X(8)    VALUE 'MAILWRTB'.

         03  MMID                      PIC S9(4)    BINARY VALUE 1.
         03  MMSG                      PIC S9(4)    BINARY VALUE 2.
         03  MDST                      PIC S9(4)    BINARY VALUE 3.
         03  MMDS                      PIC S9(4)    BINARY VALUE 4.

         03  LEN                       PIC S9(4)    BINARY.
         03  VAR-TEXTL                 PIC S9(4)    BINARY.
         03  VAR-TEXT                  PIC  X(245)  VALUE SPACES.

         03  MMID-RECL                 PIC  9(5).
         03  MMSG-RECL                 PIC  9(5).
         03  MDST-RECL                 PIC  9(5).

         03  FILES-SWITCH              PIC  X       VALUE 'N'.
           88  FILES-CLOSED                         VALUE 'N'.
           88  FILES-OPEN                           VALUE 'Y'.

      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS

       01  JOBDATA                     PIC  X(8)    VALUE 'JOBDATA'.
       COPY JOBDATA.

       COPY RTCMAN.

       COPY VSMSTATW.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

       01  MAILWRTR-COMMAREA.
         COPY MAILWRTR.

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING MAILWRTR-COMMAREA.

      ******************************************************************
      *    MAINLINE ROUTINE                                            *
      ******************************************************************
       A00-MAINLINE-ROUTINE.

           PERFORM B10-INITIALIZATION THRU B15-EXIT.

           IF  RTC-CODE < +8
               EVALUATE TRUE
                 WHEN MAIL-HEAD-PARMS
                   PERFORM C00-MAIL-HEADER THRU C90-EXIT
                 WHEN MAIL-DIST-PARMS
                   PERFORM D00-MAIL-DISTRIBUTION THRU D90-EXIT
                 WHEN MAIL-BODY-PARMS
                   PERFORM E00-MAIL-BODY   THRU E90-EXIT
                 WHEN MAIL-MESG-COMPLETE
                   PERFORM F00-MAIL-COMPLETE THRU F90-EXIT
                 WHEN MAIL-PURGE-MESG
                   PERFORM G00-MAIL-PURGE  THRU G90-EXIT
               END-EVALUATE
           END-IF.

           PERFORM B20-TERMINATION THRU B25-EXIT.

           GOBACK.

      /*****************************************************************
      *    PROGRAM INITIALIZATION ROUTINE                              *
      ******************************************************************
       B10-INITIALIZATION.

           MOVE ZEROES                 TO RTC-CODE.

           IF  NOT MAIL-HEAD-PARMS
           AND NOT MAIL-DIST-PARMS
           AND NOT MAIL-BODY-PARMS
           AND NOT MAIL-MESG-COMPLETE
           AND NOT MAIL-PURGE-MESG
               MOVE 1                  TO VAR-TEXTL
               STRING THIS-PGM            DELIMITED BY SPACE
                      ': CALL CONTROL ERROR' DELIMITED BY SIZE
                   INTO VAR-TEXT     WITH POINTER VAR-TEXTL
               SUBTRACT 1            FROM VAR-TEXTL
               DISPLAY VAR-TEXT(1:VAR-TEXTL)
                                     UPON CONSOLE
               MOVE +12                TO RTC-CODE
               PERFORM B80-CALL-RTCMAN THRU B85-EXIT
           END-IF.

           IF  RTC-CODE = ZERO
               IF  NOT MAIL-HEAD-PARMS
               AND MAIL-ID NOT > ZEROES
                   MOVE 1              TO VAR-TEXTL
                   STRING THIS-PGM        DELIMITED BY SPACE
                          ': CALL SEQUENCE ERROR' DELIMITED BY SIZE
                       INTO VAR-TEXT WITH POINTER VAR-TEXTL
                   SUBTRACT 1        FROM VAR-TEXTL
                   DISPLAY VAR-TEXT(1:VAR-TEXTL)
                                     UPON CONSOLE
                   MOVE +12            TO RTC-CODE
                   PERFORM B80-CALL-RTCMAN THRU B85-EXIT
               END-IF
           END-IF.

           IF  RTC-CODE = ZERO
               IF  MAIL-DIST-PARMS
               AND MAIL-SEQ > ZEROES
                   MOVE 1              TO VAR-TEXTL
                   STRING THIS-PGM        DELIMITED BY SPACE
                          ': DIST SEQUENCE ERROR' DELIMITED BY SIZE
                       INTO VAR-TEXT WITH POINTER VAR-TEXTL
                   SUBTRACT 1        FROM VAR-TEXTL
                   DISPLAY VAR-TEXT(1:VAR-TEXTL)
                                     UPON CONSOLE
                   MOVE +12            TO RTC-CODE
                   PERFORM B80-CALL-RTCMAN THRU B85-EXIT
               END-IF
           END-IF.

           IF  RTC-CODE = ZERO
               IF  MAIL-BODY-PARMS
               AND MAIL-SEQ >= 3000
                   MOVE 1              TO VAR-TEXTL
                   STRING THIS-PGM        DELIMITED BY SPACE
                          ': BODY LIMIT REACHED' DELIMITED BY SIZE
                       INTO VAR-TEXT WITH POINTER VAR-TEXTL
                   SUBTRACT 1        FROM VAR-TEXTL
                   DISPLAY VAR-TEXT(1:VAR-TEXTL)
                                     UPON CONSOLE
                   MOVE +12            TO RTC-CODE
                   PERFORM B80-CALL-RTCMAN THRU B85-EXIT
               END-IF
           END-IF.

           IF  RTC-CODE = ZERO
           AND FILES-CLOSED
           AND MAIL-HEAD-PARMS
               CALL JOBDATA         USING JOBDATA-PARMS

               COMPUTE STAT-TOTL        = LENGTH OF STAT-DATA
                                        / LENGTH OF STAT-ENTRY

               MOVE 'MAILMID'          TO VSAM-FILE(MMID)
               MOVE 'MAILMSG'          TO VSAM-FILE(MMSG)
               MOVE 'MAILDST'          TO VSAM-FILE(MDST)
               MOVE 'MAILMDS'          TO VSAM-FILE(MMDS)

               MOVE LENGTH OF MMID-KEY TO VSAM-KEYL(MMID)
               MOVE LENGTH OF MMSG-KEY TO VSAM-KEYL(MMSG)
               MOVE LENGTH OF MDST-KEY TO VSAM-KEYL(MDST)
               MOVE LENGTH OF MMDS-KEY TO VSAM-KEYL(MMDS)

               PERFORM WITH TEST BEFORE
                 VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
                   SET FUNC-OPEN(VSUB) TO TRUE
               END-PERFORM

               OPEN I-O    MAILMID
                           MAILMSG
                           MAILDST
                           MAILMDS

               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                 VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL

               IF  RTC-CODE = ZERO
                   SET  FILES-OPEN     TO TRUE
                   PERFORM WITH TEST BEFORE
                     VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
                       SET FUNC-READ(VSUB) TO TRUE
                   END-PERFORM
               END-IF
           END-IF.

       B15-EXIT.
           EXIT.

      /*****************************************************************
      *    PROGRAM TERMINATION ROUTINE                                 *
      ******************************************************************
       B20-TERMINATION.

           IF  RTC-CODE = ZERO
           AND FILES-OPEN
           AND (MAIL-MESG-COMPLETE OR MAIL-PURGE-MESG)
               PERFORM WITH TEST BEFORE
                 VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
                   SET FUNC-CLOSE(VSUB) TO TRUE
               END-PERFORM

               CLOSE MAILMID
                     MAILMSG
                     MAILDST
                     MAILMDS

               SET  FILES-CLOSED       TO TRUE

               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                 VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL

               IF  RTC-CODE > +4
                 MOVE RTC-CODE         TO RETURN-CODE
                 IF  RTC-CODE < +16
                   SET RTC-REQUEST-CANCEL TO TRUE
                 ELSE
                   SET RTC-REQUEST-DUMP TO TRUE
                 END-IF
                 PERFORM B80-CALL-RTCMAN THRU B85-EXIT
               END-IF
           END-IF.

           MOVE RTC-CODE               TO RETURN-CODE.

       B25-EXIT.
           EXIT.

      /*****************************************************************
      *    CHECK A VSAM OR WORK FILE'S STATUS                          *
      ******************************************************************
       COPY VSMSTATP.

      /*****************************************************************
      *    MAIL HEADER ROUTINE                                         *
      ******************************************************************
       C00-MAIL-HEADER.

           IF  MAIL-ID > ZEROES
               PERFORM F00-MAIL-COMPLETE THRU F90-EXIT
           END-IF.

      * get mail distribution header record
      *    MOVE MAIL-HEAD-TO-GRP       TO MDST-GRP-NAME
      *    SET  MDST-GRP-HEADER        TO TRUE

      *    MOVE MDST                   TO VSUB
      *    SET  FUNC-READ(VSUB)        TO TRUE
      *    MOVE MDST-KEY               TO VSAM-KEYD(VSUB)
      *    READ MAILDST RECORD KEY IS MDST-KEY END-READ

      *    IF  STAT-NOTFND(VSUB)
      * report errors and abend process
      *        MOVE 1                  TO VAR-TEXTL
      *        STRING THIS-PGM            DELIMITED BY SPACE
      *               ': GROUP '          DELIMITED BY SIZE
      *               MAIL-HEAD-TO-GRP    DELIMITED BY SPACE
      *               ' NOT FOUND'        DELIMITED BY SIZE
      *            INTO VAR-TEXT     WITH POINTER VAR-TEXTL
      *        SUBTRACT 1            FROM VAR-TEXTL
      *        DISPLAY VAR-TEXT(1:VAR-TEXTL)
      *                              UPON CONSOLE
      *        MOVE +12                TO RTC-CODE
      *        PERFORM B80-CALL-RTCMAN THRU B85-EXIT
      *    END-IF.

      *    IF  STAT-NORMAL(VSUB)
      * get message id control record
               MOVE ZERO               TO MMID-ID

               MOVE MMID               TO VSUB
               SET  FUNC-READUPD(VSUB) TO TRUE
               MOVE MMID-KEY           TO VSAM-KEYD(VSUB)
               READ MAILMID RECORD KEY IS MMID-KEY END-READ
      *    END-IF.

           IF  STAT-NORMAL(VSUB)
      * increment message id control record
               ADD  1                  TO MMID-CONTROL-ID
               CALL 'ASKTIME'       USING MMID-CONTROL-LAST-STMP
      * rewrite message id control record
               SET  FUNC-REWRITE(VSUB) TO TRUE
               REWRITE MAILMID-RECORD END-REWRITE
           END-IF.

           IF  STAT-NORMAL(VSUB)
      * build message id header
               MOVE MMID-CONTROL-ID    TO MMID-ID
                                          MAIL-ID
               MOVE ZEROES             TO MAIL-SEQ
               INITIALIZE MMID-HDR
               CALL 'ASKTIME'       USING MMID-CREATED-TIMESTAMP
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
               MOVE JOBD-EXEC-PGM      TO MMID-CREATED-BY
      * write message id header
               MOVE MMID               TO VSUB
               SET  FUNC-WRITE(VSUB)   TO TRUE
               MOVE MMID-KEY           TO VSAM-KEYD(VSUB)
               COMPUTE MMID-RECL = LENGTH OF MMID-KEY
                                 + LENGTH OF MMID-HDR
               WRITE MAILMID-RECORD END-WRITE
           END-IF.

           IF  NOT STAT-NORMAL(VSUB)
           AND RTC-CODE < +8
      * report errors and abend process
               MOVE 1                  TO VAR-TEXTL
               STRING THIS-PGM            DELIMITED BY SPACE
                      ': CREATING '
                      'MAIL HEADER...'    DELIMITED BY SIZE
                   INTO VAR-TEXT     WITH POINTER VAR-TEXTL
               SUBTRACT 1            FROM VAR-TEXTL
               DISPLAY VAR-TEXT(1:VAR-TEXTL)
                                     UPON CONSOLE
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
           END-IF.

       C90-EXIT.
           EXIT.

      /*****************************************************************
      *    MAIL DISTRIBUTION ROUTINE                                   *
      ******************************************************************
       D00-MAIL-DISTRIBUTION.

      * get mail distribution header record
      *    MOVE MAIL-DIST-TO-GRP       TO MDST-GRP-NAME
      *    SET  MDST-GRP-HEADER        TO TRUE

      *    MOVE MDST                   TO VSUB
      *    SET  FUNC-READ(VSUB)        TO TRUE
      *    MOVE MDST-KEY               TO VSAM-KEYD(VSUB)
      *    READ MAILDST RECORD KEY IS MDST-KEY END-READ

      *    IF  STAT-NOTFND(VSUB)
      * report errors and abend process
      *        MOVE 1                  TO VAR-TEXTL
      *        STRING THIS-PGM            DELIMITED BY SPACE
      *               ': EXTENDED GROUP ' DELIMITED BY SIZE
      *               MAIL-HEAD-TO-GRP    DELIMITED BY SPACE
      *               ' NOT FOUND'        DELIMITED BY SIZE
      *            INTO VAR-TEXT     WITH POINTER VAR-TEXTL
      *        SUBTRACT 1            FROM VAR-TEXTL
      *        DISPLAY VAR-TEXT(1:VAR-TEXTL)
      *                              UPON CONSOLE
      *        MOVE +12                TO RTC-CODE
      *        PERFORM B80-CALL-RTCMAN THRU B85-EXIT
      *    END-IF.

      *    IF  STAT-NORMAL(VSUB)
      * get extended message distribution sequence number
               MOVE MAIL-ID            TO MMDS-ID
               MOVE ZEROES             TO MMDS-SEQ
               MOVE MMDS               TO VSUB
               SET  FUNC-START(VSUB)   TO TRUE
               MOVE MMDS-KEY           TO VSAM-KEYD(VSUB)
               START MAILMDS KEY >= MMDS-KEY END-START
      *comperr READ MAILMDS PREVIOUS RECORD END-READ
               PERFORM WITH TEST BEFORE
                 UNTIL MMDS-ID NOT = MAIL-ID
                    OR NOT STAT-NORMAL(VSUB)
                   SET  FUNC-READNEXT(VSUB) TO TRUE
                   READ MAILMDS NEXT RECORD END-READ
                   MOVE MMDS-KEY       TO VSAM-KEYD(VSUB)
                   IF  STAT-NORMAL(VSUB)
                   AND MMDS-ID = MAIL-ID
                       MOVE MMDS-SEQ   TO MAIL-SEQ
                   END-IF
               END-PERFORM
               IF  STAT-NORMAL(VSUB)
               OR  STAT-NOTFND(VSUB)
               OR  STAT-EOFILE(VSUB)
                   IF  MMDS-ID = MAIL-ID
                       ADD  1          TO MAIL-SEQ
                                   GIVING MMDS-SEQ
                   ELSE
                       MOVE MAIL-ID    TO MMDS-ID
                       MOVE 1          TO MMDS-SEQ
                   END-IF
                   MOVE ZEROES         TO MAIL-SEQ
                   MOVE MAIL-HEAD-TO-GRP
                                       TO MMDS-TO-GRP
               END-IF
      * write extended message distribution record
               IF  STAT-NORMAL(VSUB)
               OR  STAT-NOTFND(VSUB)
               OR  STAT-EOFILE(VSUB)
                   SET FUNC-WRITE(VSUB) TO TRUE
                   MOVE MMDS-KEY       TO VSAM-KEYD(VSUB)
                   WRITE MAILMDS-RECORD END-WRITE
               END-IF
      *    END-IF.

           IF  NOT STAT-NORMAL(VSUB)
           AND RTC-CODE < +8
      * report errors and abend process
               MOVE 1                  TO VAR-TEXTL
               STRING THIS-PGM            DELIMITED BY SPACE
                      ': EXTENDING '
                      'MAIL DISTIBUTION...' DELIMITED BY SIZE
                   INTO VAR-TEXT     WITH POINTER VAR-TEXTL
               SUBTRACT 1            FROM VAR-TEXTL
               DISPLAY VAR-TEXT(1:VAR-TEXTL)
                                     UPON CONSOLE
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
           END-IF.

       D90-EXIT.
           EXIT.

      /*****************************************************************
      *    MAIL BODY ROUTINE                                           *
      ******************************************************************
       E00-MAIL-BODY.

      * build message body record
           MOVE MAIL-ID                TO MMSG-ID.
           ADD  1                      TO MAIL-SEQ.
           MOVE MAIL-SEQ               TO MMSG-SEQ.
           MOVE MAIL-BODY-TEXT         TO MMSG-BODY.
      * set length of body text
           IF  MMSG-BODY > SPACES
               PERFORM WITH TEST BEFORE
                 VARYING LEN FROM LENGTH OF MMSG-BODY BY -1
                   UNTIL LEN < 1
                      OR MMSG-BODY(LEN:1) > SPACE
               END-PERFORM
           ELSE
               MOVE ZEROES             TO LEN
           END-IF.
      * write message body record
           MOVE MMSG                   TO VSUB.
           SET  FUNC-WRITE(VSUB)       TO TRUE.
           MOVE MMSG-KEY               TO VSAM-KEYD(VSUB).
           COMPUTE MMSG-RECL = LENGTH OF MMSG-KEY + LEN.
           WRITE MAILMSG-RECORD END-WRITE.

           IF  NOT STAT-NORMAL(VSUB)
      * report errors and abend process
               MOVE 1                  TO VAR-TEXTL
               STRING THIS-PGM            DELIMITED BY SPACE
                      ': CREATING '
                      'MAIL BODY...'      DELIMITED BY SIZE
                   INTO VAR-TEXT     WITH POINTER VAR-TEXTL
               SUBTRACT 1            FROM VAR-TEXTL
               DISPLAY VAR-TEXT(1:VAR-TEXTL)
                                     UPON CONSOLE
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
           END-IF.

       E90-EXIT.
           EXIT.

      /*****************************************************************
      *    MAIL COMPLETE ROUTINE                                       *
      ******************************************************************
       F00-MAIL-COMPLETE.

      * get message id header record
           MOVE MAIL-ID                TO MMID-ID.

           MOVE MMID                   TO VSUB.
           SET  FUNC-READUPD(VSUB)     TO TRUE.
           MOVE MMID-KEY               TO VSAM-KEYD(VSUB).
           READ MAILMID RECORD KEY IS MMID-KEY END-READ.

           IF  STAT-NORMAL(VSUB)
      * change message id header record
               CALL 'ASKTIME'       USING MMID-CHANGED-TIMESTAMP
               SET  MMID-IS-COMPLETE   TO TRUE
               MOVE JOBD-EXEC-PGM      TO MMID-UPDATED-BY
      * rewrite message id header record
               SET  FUNC-REWRITE(VSUB) TO TRUE
               REWRITE MAILMID-RECORD END-REWRITE
           END-IF.

           IF  NOT STAT-NORMAL(VSUB)
      * report errors and abend process
               MOVE 1                  TO VAR-TEXTL
               STRING THIS-PGM            DELIMITED BY SPACE
                      ': COMPLETING '
                      'MAIL MESSAGE...'   DELIMITED BY SIZE
                   INTO VAR-TEXT     WITH POINTER VAR-TEXTL
               SUBTRACT 1            FROM VAR-TEXTL
               DISPLAY VAR-TEXT(1:VAR-TEXTL)
                                     UPON CONSOLE
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
           END-IF.

       F90-EXIT.
           EXIT.

      /*****************************************************************
      *    MAIL PURGE ROUTINE                                          *
      ******************************************************************
       G00-MAIL-PURGE.

      * get message id header record
           MOVE MAIL-ID                TO MMID-ID.

           MOVE MMID                   TO VSUB.
           SET  FUNC-READUPD(VSUB)     TO TRUE.
           MOVE MMID-KEY               TO VSAM-KEYD(VSUB).
           READ MAILMID RECORD KEY IS MMID-KEY END-READ.

           IF  STAT-NORMAL(VSUB)
      * delete message body records
               MOVE MMID-ID            TO MMSG-ID
               MOVE ZEROES             TO MMSG-SEQ
               MOVE MMSG               TO VSUB
               SET  FUNC-START(VSUB)   TO TRUE
               MOVE MMSG-KEY           TO VSAM-KEYD(VSUB)
               START MAILMSG KEY >= MMSG-KEY END-START
               PERFORM WITH TEST BEFORE
                 UNTIL MMSG-ID NOT = MMID-ID
                    OR NOT STAT-NORMAL(VSUB)
                   SET  FUNC-READNEXT(VSUB) TO TRUE
                   READ MAILMSG NEXT RECORD END-READ
                   MOVE MMSG-KEY       TO VSAM-KEYD(VSUB)
                   IF  STAT-NORMAL(VSUB)
                   AND MMSG-ID = MMID-ID
                       SET  FUNC-DELETE(VSUB)
                                       TO TRUE
                       DELETE MAILMSG RECORD END-DELETE
                   END-IF
               END-PERFORM
               IF  STAT-EOFILE(VSUB)
                 SET STAT-NORMAL(VSUB) TO TRUE
               END-IF
           END-IF.

           IF  STAT-NORMAL(VSUB)
      * delete message id header record
               MOVE MMID               TO VSUB
               SET  FUNC-DELETE(VSUB)  TO TRUE
               MOVE MMID-KEY           TO VSAM-KEYD(VSUB)
               DELETE MAILMID RECORD END-DELETE
           END-IF.

           IF  NOT STAT-NORMAL(VSUB)
      * report errors and abend process
               MOVE 1                  TO VAR-TEXTL
               STRING THIS-PGM            DELIMITED BY SPACE
                      ': PURGING '
                      'MAIL MESSAGE...'   DELIMITED BY SIZE
                   INTO VAR-TEXT     WITH POINTER VAR-TEXTL
               SUBTRACT 1            FROM VAR-TEXTL
               DISPLAY VAR-TEXT(1:VAR-TEXTL)
                                     UPON CONSOLE
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
           END-IF.

       G90-EXIT.
           EXIT.

