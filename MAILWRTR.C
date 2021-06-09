      * MAINFRAME MAIL WRITER COMMUNICATION AREA                                
         03  MAIL-CONTROL              PIC  X(01).                              
           88  MAIL-HEAD-PARMS                      VALUE 'H'.                  
           88  MAIL-DIST-PARMS                      VALUE 'D'.                  
           88  MAIL-BODY-PARMS                      VALUE 'B'.                  
           88  MAIL-MESG-COMPLETE                   VALUE 'C'.                  
           88  MAIL-PURGE-MESG                      VALUE 'P'.                  
         03  MAIL-ID                   PIC S9(18)   BINARY.                     
         03  MAIL-SEQ                  PIC S9(5)    PACKED-DECIMAL.             
         03  MAIL-PARMS                PIC  X(240).                             
         03  MAIL-HEAD                 REDEFINES    MAIL-PARMS.                 
           05  MAIL-HEAD-TO-GRP        PIC  X(25).                              
           05  MAIL-HEAD-FROM          PIC  X(64).                              
           05  MAIL-HEAD-SUBJECT       PIC  X(64).                              
           05  MAIL-HEAD-DELIVER       PIC  X(01).                              
             88  MAIL-DELIVERY-ON                   VALUE 'Y'.                  
             88  MAIL-DELIVERY-OFF                  VALUE SPACE.                
           05  MAIL-HEAD-PRIVATE       PIC  X(01).                              
             88  MAIL-PRIVACY-ON                    VALUE 'Y'.                  
             88  MAIL-PRIVACY-OFF                   VALUE SPACE.                
           05  MAIL-HEAD-SYSTEM        PIC  X(02).                              
           05  MAIL-HEAD-RESERVED      PIC  X(04).                              
         03  MAIL-DIST                 REDEFINES    MAIL-PARMS.                 
           05  MAIL-DIST-TO-GRP        PIC  X(25).                              
         03  MAIL-BODY                 REDEFINES    MAIL-PARMS.                 
           05  MAIL-BODY-TEXT          PIC  X(240).                             
