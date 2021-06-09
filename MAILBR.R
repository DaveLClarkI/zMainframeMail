     I* MAIL WRITER VALUES (USE WITH ULABL)
     IMAILWR      DS
     I*   CONTROL CODE: 'H'EAD, 'D'IST, 'B'ODY, 'C'OMPLETE, 'P'URGE
     I                                        1   1 MAILCD
     I*   MAIL IN-FLIGHT IDENTIFIER (8-BYTE BINARY)
     I                                        2   9 MAILID
     I*   BODY IN-FLIGHT SEQUENCE (PACKED)
     I                                    P  10  120MAILSQ
     I* MAIL EXIT VALUES (USE WITH RLABL)
     IMAILEX      DS
     I*   MAIL TO DISTRIBUTION GROUP NAME
     I                                        1  25 MAILTO
     I*   MAIL FROM (OPTIONAL)
     I                                       26  89 MAILFR
     I*   MAIL SUBJECT
     I                                       90 153 MAILSB
     I*   MAIL DELIVERY ('Y' TO DELIVER, ELSE BLANK)
     I                                      154 154 MAILDV
     I*   MAIL PRIVATE ('Y' IS PRIVATE, ELSE BLANK)
     I                                      155 155 MAILPV
     I*   MAIL SYSTEM CODE (OPTIONAL)
     I                                      156 157 MAILSY
     I*   MAIL RESERVED (IGNORED)
     I                                      158 161 MAILRV
