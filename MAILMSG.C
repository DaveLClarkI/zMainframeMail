      * MAINFRAME MAIL MESSAGE BODIES RECORD                                    
       01  MAILMSG-RECORD.                                                      
         03  MMSG-KEY.                                                          
           05  MMSG-ID                 PIC S9(18)   BINARY.                     
           05  MMSG-SEQ                PIC S9(5)    PACKED-DECIMAL.             
         03  MMSG-BODY                 PIC  X(244).                             
