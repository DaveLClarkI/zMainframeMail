      * MAINFRAME MAIL DISTRIBUTION GROUPS RECORD                               
       01  MAILDST-RECORD.                                                      
         03  MDST-KEY.                                                          
           05  MDST-GRP-NAME           PIC  X(25).                              
           05  MDST-GRP-SEQU           PIC S9(5)    PACKED-DECIMAL.             
             88  MDST-GRP-HEADER                    VALUE ZEROES.               
             88  MDST-GRP-ENTRY                     VALUES 1 THRU 99999.        
         03  MDST-DATA                 PIC  X(227).                             
         03  MDST-HEADER               REDEFINES    MDST-DATA.                  
           05  MDST-GRP-DESC           PIC  X(50).                              
           05  MDST-USAGE-COUNT        PIC S9(9)    PACKED-DECIMAL.             
           05  MDST-LAST-USE-DATE      PIC S9(8)    PACKED-DECIMAL.             
           05  MDST-LAST-USED-BY       PIC  X(8).                               
           05                          PIC  X(4).                               
           05  MDST-UPDATED-BY         PIC  X(8).                               
         03  MDST-ENTRY                REDEFINES    MDST-DATA.                  
           05  MDST-IDNT               PIC  X(64).                              
           05  MDST-NAME               PIC  X(64).                              
           05  MDST-ADDR               PIC  X(64).                              
           05  MDST-TYPE               PIC  X(4).                               
             88  MDST-PRIMARY-RECIPIENT             VALUE '*PRI'.               
             88  MDST-CARBON-COPY                   VALUE '*CC '.               
             88  MDST-BLIND-COPY                    VALUE '*BCC'.               
