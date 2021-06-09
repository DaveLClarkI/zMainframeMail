      * MAINFRAME MAIL MESSAGE IDs RECORD                                       
       01  MAILMID-RECORD.                                                      
         03  MMID-KEY.                                                          
           05  MMID-ID                 PIC S9(18)   BINARY.                     
         03  MMID-DATA                 PIC  X(247).                             
         03  MMID-HDR                  REDEFINES    MMID-DATA.                  
           05  MMID-CREATED-TIMESTAMP  PIC S9(15)   PACKED-DECIMAL.             
           05  MMID-CONTROL-ID         REDEFINES MMID-CREATED-TIMESTAMP         
                                       PIC S9(18)   BINARY.                     
           05  MMID-CHANGED-TIMESTAMP  PIC S9(15)   PACKED-DECIMAL.             
           05  MMID-CONTROL-LAST-STMP  REDEFINES MMID-CHANGED-TIMESTAMP         
                                       PIC S9(15)   PACKED-DECIMAL.             
           05  MMID-SENT-TIMESTAMP     PIC S9(15)   PACKED-DECIMAL.             
           05  MMID-COMPLETE           PIC  X(1).                               
             88  MMID-IS-COMPLETE                   VALUE 'Y'.                  
             88  MMID-NOT-COMPLETE                  VALUES 'N', SPACE.          
           05  MMID-DELIVERY           PIC  X(1).                               
             88  MMID-NORMAL-DELIVERY               VALUE 'Y'.                  
             88  MMID-DO-NOT-DELIVER                VALUES 'N', SPACE.          
           05  MMID-PRIVATE            PIC  X(1).                               
             88  MMID-IS-PRIVATE                    VALUE 'Y'.                  
             88  MMID-NOT-PRIVATE                   VALUES 'N', SPACE.          
           05  MMID-SYSTEM             PIC  X(2).                               
           05  MMID-TO-GRP             PIC  X(25).                              
           05  MMID-FROM               PIC  X(64).                              
           05  MMID-SUBJECT            PIC  X(64).                              
           05  MMID-CREATED-BY         PIC  X(8).                               
           05  MMID-UPDATED-BY         PIC  X(8).                               
