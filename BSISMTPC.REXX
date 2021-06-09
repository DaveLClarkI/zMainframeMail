/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* REXX: BSISMTPC   Proc Created 12/12/2007   By Dave Clark        */
/*                  Last Revised 01/04/2021                        */
/*                                                                 */
/*      This procedure sends an email to an SMTP mail server.  The */
/* body content is flagged  with  a TEXT/HTML type.   All input is */
/* via card.  See below.                                           */
/*                                                                 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* No warranty is expressed or implied.    Written by Dave L Clark */
/* Neither the author nor the company is   Winsupply Group Svcs.   */
/* responsible for any loss or damage      3110 Kettering Blvd.    */
/* resulting from the use of this code.    Dayton, OH  45439       */
/* Source is provided on an as-is basis.   Phone (937)294-5331     */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* The following is example JCL for running this procedure:        */
/*                                                                 */
/*   // JOB    BSISMTPC   SEND MAIL VIA SOCKET                     */
/*   // LIBDEF *,SEARCH=(PRD2.CONFIG,ESP.BSI),TEMP                 */
/*   // EXEC   REXX=BSISMTPC,SIZE=200K                             */
/*   ID   stack_id                                                 */
/*   OPEN host_address                                             */
/*   HELO domain_name                                              */
/*   MAIL From: from_mail_address                                  */
/*   ORGA Organization: organization_name                          */
/*   RPTO ReplyTo: replyto_mail_address                            */
/*   SENS Sensitivity: {Personal|Private|Company-Confidential}     */
/*   RCPT To: to_mail_address                                      */
/*    ...                                                          */
/*   RCPT Cc: cc_mail_address                                      */
/*    ...                                                          */
/*   RCPT Bcc: bcc_mail_address                                    */
/*    ...                                                          */
/*   SUBJ Subject: mail_subject                                    */
/*   DATA                                                          */
/*   ...content...                                                 */
/*   QUIT                                                          */
/*   /* EOD */                                                     */
/*   /& EOJ &/                                                     */
/*                                                                 */
/* Where input cards must be presented in the following order:     */
/*        ID         is a required keyword tag if the local TCP/IP */
/*                     stack has an id other than 00;              */
/*        stack_id   is the TCP/IP stack id in the range of 00 to  */
/*                     99 (if card omitted, 00 is the default);    */
/*        OPEN       is a required keyword tag;                    */
/*        host_address is the fully-qualified host name or IP ad-  */
/*                     dress for the SMTP mail server to contact;  */
/*        HELO       is a required keyword tag;                    */
/*        domain_name is the fully-qualified domain name of the    */
/*                     sender;                                     */
/*        MAIL From: is a required keyword tag;                    */
/*        from_mail_address is the fully-qualified email address   */
/*                     of the sender;                              */
/*        ORGA Organization: is an optional keyword tag if no      */
/*                     "Organization" name is specified;           */
/*        organization_name is the free-text specification of the  */
/*                     sending organization;                       */
/*        RPTO ReplyTo: is an optional keyword tag if no           */
/*                     "ReplyTo" recipient is specified;           */
/*        replyto_mail_address is the fully-qualified email        */
/*                     address of the "ReplyTo" recipient;         */
/*        SENS Sensitivity: is an optional keyword tag if no       */
/*                     mail "Sensitivity" is required;             */
/*        {Personal|Private|Company-Confidential}                  */
/*                     are mutually-exclusive sensitivity options; */
/*        RCPT To:   is an optional keyword tag if no "TO"         */
/*                     recipients are specified;                   */
/*        to_email_address is the fully-qualified "TO" email       */
/*                     address of the recipient;                   */
/*        RCPT Cc:   is an optional keyword tag if no "CC"         */
/*                     recipients are specified;                   */
/*        cc_email_address is the fully-qualified "CC" email       */
/*                     address of the recipient;                   */
/*        RCPT Bcc:  is an optional keyword tag if no "BCC"        */
/*                     recipients are specified;                   */
/*        bcc_email_address is the fully-qualified "BCC" email     */
/*                     address of the recipient;                   */
/*        SUBJ Subject: is a required keyword tag;                 */
/*        mail_subject is the subject text for the email;          */
/*        DATA       is a required keyword tag;                    */
/*        ...content... represents the body of the email;          */
/*        QUIT       is a required keyword tag;                    */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/INCLUDE COPYBOOKS.XINITIAL

  rc = ChrCount('S',_pgm);        /* force procedure to compile */

  chunk_size = 8000;
  eof_cards = _False;

  Debug = 'Normal';    /* Specify 'Verbose', 'Normal', or 'Silent' */

/* ----------------------------------------------------------------- */
/* Get PARM argument (dummy -- not actually used by this procedure)  */
/* ----------------------------------------------------------------- */

  rc  = 0;
  Arg msg;

/* ----------------------------------------------------------------- */
/* Get SYSIPT data                                                   */
/* ----------------------------------------------------------------- */

  "EXECIO" chunk_size "DISKR SYSIPT ( OPEN STEM card. )";
  If rc ¬= 0 Then Do
    If rc == 2 Then Do
      eof_cards = _True;
    End
    Else Do
      Say 'Error: EXECIO returned rc =' rc;
      Exit 16;
    End
  End
  rc  = 0;

/* ----------------------------------------------------------------- */
/* Process SYSIPT data                                               */
/* ----------------------------------------------------------------- */

  tcpid     = '';
  MTPserver = '';
  MTPsender = '';
  MTPfraddr = '';
  MTPfrorga = '';
  MTPreply2 = '';
  MTPsense  = '';
  MTPsubjct = '';

  MTPtoaddr.  = '';
  MTPtoaddr.0 = 0;
  MTPccaddr.  = '';
  MTPccaddr.0 = 0;
  MTPbccadr.  = '';
  MTPbccadr.0 = 0;
  MTPtext.    = '';
  MTPtext.0   = 0;

  Do i = 1 To card.0
    If Left(card.i,1) == '*' Then Iterate;
    Call Logit 'Input: "'||Strip(card.i,'T')||'"', Debug;
    Select
      When Translate(Left(card.i,3)) == 'ID '   Then
        Parse Var card.i . tcpid .;
      When Translate(Left(card.i,5)) == 'OPEN ' Then
        Parse Var card.i . MTPserver .;
      When Translate(Left(card.i,5)) == 'HELO ' Then
        Do
          Parse Var card.i . MTPsender .;
          MTPsender = ReplVars(MTPsender,'SENDER');
        End
      When Translate(Left(card.i,5)) == 'MAIL ' Then
        Do
          fullcard = GetContinuedCard();
          Parse Var fullcard . pfx MTPfraddr;
          If Translate(pfx) <> 'FROM:' Then Do
            Say ' 'fullcard;
            Say ' Missing ''From:'' prefix.'
            rc = 8;
          End
          MTPfraddr = Strip(MTPfraddr);
          MTPfraddr = ReplVars(MTPfraddr,'FRADDR');
          If Pos('@', MTPfraddr) == 0 Then Do
            MTPfraddr = MTPfraddr '<no-reply@winsupplyinc.com>';
          End
          Call Logit 'Saved: "FROM:' MTPfraddr||'"', Debug;
        End
      When Translate(Left(card.i,5)) == 'ORGA ' Then
        Do
          fullcard = GetContinuedCard();
          Parse Var fullcard . pfx MTPfrorga;
          If Translate(pfx) <> 'ORGANIZATION:' Then Do
            Say ' 'fullcard;
            Say ' Missing ''Organization:'' prefix.'
            rc = 8;
          End
          MTPfrorga = Strip(MTPfrorga);
          Call Logit 'Saved: "ORGA:' MTPfrorga||'"', Debug;
        End
      When Translate(Left(card.i,5)) == 'RPTO ' Then
        Do
          fullcard = GetContinuedCard();
          Parse Var fullcard . pfx MTPreply2;
          If Translate(pfx) <> 'REPLYTO:' Then Do
            Say ' 'fullcard;
            Say ' Missing ''ReplyTo:'' prefix.'
            rc = 8;
          End
          MTPreply2 = Strip(MTPreply2);
          Call Logit 'Saved: "RPTO:' MTPreply2||'"', Debug;
        End
      When Translate(Left(card.i,5)) == 'SENS ' Then
        Do
          fullcard = GetContinuedCard();
          Parse Var fullcard . pfx MTPsense;
          If Translate(pfx) <> 'SENSITIVITY:' Then Do
            Say ' 'fullcard;
            Say ' Missing ''Sensitivity:'' prefix.'
            rc = 8;
          End
          MTPsense = Strip(MTPsense);
          If WordPos(Translate(MTPSense),,
                     'PERSONAL PRIVATE,
                      COMPANY-CONFIDENTIAL') == 0 Then Do
            Say ' 'fullcard;
            Say ' Invalid ''Sensitivity:'' option.'
            rc = 8;
          End
          Call Logit 'Saved: "SENS:' MTPsense||'"', Debug;
        End
      When Translate(Left(card.i,5)) == 'RCPT ' Then
        Do
          fullcard = GetContinuedCard();
          Parse Var fullcard . pfx addr;
          addr = Strip(addr);
          Select
            When Translate(pfx) == 'TO:'  Then
              Do
                n = MTPtoaddr.0 + 1;
                MTPtoaddr.n = addr;
                MTPtoaddr.0 = n;
              End
            When Translate(pfx) == 'CC:'  Then
              Do
                n = MTPccaddr.0 + 1;
                MTPccaddr.n = addr;
                MTPccaddr.0 = n;
              End
            When Translate(pfx) == 'BCC:' Then
              Do
                n = MTPbccadr.0 + 1;
                MTPbccadr.n = addr;
                MTPbccadr.0 = n;
              End
          Otherwise
            Do
              Say ' 'fullcard;
              Say ' Missing ''To:'', ''Cc:'', or ''Bcc:'' prefix.'
              rc = 8;
            End
          End
          Call Logit 'Saved: "RCPT:' pfx addr||'"', Debug;
        End
      When Translate(Left(card.i,5)) == 'SUBJ ' Then
        Do
          fullcard = GetContinuedCard();
          Parse Var fullcard . pfx MTPsubjct;
          If Translate(pfx) <> 'SUBJECT:' Then Do
            Say ' 'fullcard;
            Say ' Missing ''Subject:'' prefix.'
            rc = 8;
          End
          MTPsubjct = Strip(MTPsubjct);
          Call Logit 'Saved: "SUBJ:' MTPsubjct||'"', Debug;
        End
      When Translate(Left(card.i,5)) == 'DATA ' Then
        Do i = (i+1) To card.0
          If Translate(card.i) == 'QUIT' Then Leave;
          n = MTPtext.0 + 1;
          MTPtext.n = card.i;
          MTPtext.0 = n;
        End
      Otherwise
        Do
          Say ' 'card.i;
          Say ' Unknown input keyword specified.';
          rc = 8;
        End
    End
  End

  If rc > 0 Then Do
    Signal ABEND;
  End

  Drop card.;

  If tcpid <> '',
  & \DataType(tcpid,'W') Then Do
    Say _pgm': TCP/IP stack ID must be a whole number';
    rc = 8;
  End
  If MTPserver = '' Then Do
    Say _pgm': No HOST NAME or IP ADDRESS specified';
    rc = 8;
  End
  If MTPsender = '' Then Do
    Say _pgm': No SENDER domain name specified';
    rc = 8;
  End
  If MTPfraddr = '' Then Do
    Say _pgm': No SENDER mail address specified';
    rc = 8;
  End
  If MTPtoaddr.0 = 0,
  &  MTPccaddr.0 = 0,
  &  MTPbccadr.0 = 0 Then Do
    Say _pgm': No RECIPIENT mail address specified';
    rc = 8;
  End
  If MTPtext.0 = 0 Then Do
    Say _pgm': No EMAIL text content specified';
    rc = 8;
  End

  If rc > 0 Then Do
    Signal ABEND;
  End

  If tcpid  = '' Then Do
    Say Time() _pgm': TCP/IP STACK ID DEFAULTS TO 00';
    tcpid = '00';
    Say ' ';
  End
  tcpname = 'SOCKET'||Right(tcpid,2,'0');

/* ----------------------------------------------------------------- */
/* Setup translation tables                                          */
/* ----------------------------------------------------------------- */

  bad = '000102030405060708090A0B0C0D0E0F'x||,
        '101112131415161718191A1B1C1D1E1F'x||,
        '202122232425262728292A2B2C2D2E2F'x||,
        '303132333435363738393A3B3C3D3E3FFF'x;
  msk = '################################'||,
        '#################################';

  crlf= '0D0A'x;
  asc = '202122232425262728292A2B2C2D2E2F303132'x||,
        '333435363738393A3B3C3D3E3F404142434445'x||,
        '464748494A4B4C4D4E4F505152535455565758'x||,
        '595A5B5C5D5E5F606162636465666768696A6B'x||,
        '6C6D6E6F707172737475767778797A7B7C7D7E'x;
  ebc =' !"#$%&''()*+,-./012'||,
        '3456789:;<=>?@ABCDE'||,
        'FGHIJKLMNOPQRSTUVWX'||,
        'YZ[\]^_`abcdefghijk'||,
        'lmnopqrstuvwxyz{|}~';

  B64 = 'A B C D E F G H I J K L M N O P Q R S T U V W X Y Z',
        'a b c d e f g h i j k l m n o p q r s t u v w x y z',
        '0 1 2 3 4 5 6 7 8 9 + /';
  B64 = Translate(B64,SubStr(asc,2),SubStr(ebc,2));

/* ----------------------------------------------------------------- */
/* Begin processing                                                  */
/* ----------------------------------------------------------------- */

/*oldmsg = RexxMsg("OFF"); */      /* we'll get the messages ourself */
  Call ASSGN 'STDOUT','SYSLST';

  Say Time() _pgm': Start on' Date();
  Say Time() _pgm': Subject:' MTPsubjct;

/* ----------------------------------------------------------------- */
/* Allocate socket                                                   */
/* ----------------------------------------------------------------- */

  msg = Socket('Initialize', _pgm, 1, tcpname);
  msg = Socket('Socket');
  Parse Var msg sid .;

/* ----------------------------------------------------------------- */
/* Establish connection                                              */
/* ----------------------------------------------------------------- */

  iphost = MTPserver;
  If DataType(Left(iphost,1)) = 'CHAR' Then Do
    msg = Socket('GetHostByName', iphost);
    Parse Var msg iphost .;
  End

  sname  = 'AF_INET 25' iphost;
  Say Time() _pgm': Connecting...' sname;

  msg = Socket('Connect', sid, sname);
  Call RecvBuffers sid, Debug;
/*
  msg = Socket('Fcntl', sid, 'F_SETFL', 'NON-BLOCKING');
*/
/* ----------------------------------------------------------------- */
/* Send control info                                                 */
/* ----------------------------------------------------------------- */

  Say Time() _pgm': Handskaking...';

/*buffer = 'HELO' MTPsender; */
  buffer = 'EHLO' MTPsender;

  Call SendBuffers sid, buffer||crlf, Debug;
  Call RecvBuffers sid, Debug;
  If Left(smtp.1,3) >= 400 Then Signal NOGO;

  Select
    When Pos('<', MTPfraddr) > 0 Then
      buffer = SubStr(MTPfraddr, Pos('<', MTPfraddr));
    When Pos('@', MTPfraddr) > 0 Then
      buffer = '<'MTPfraddr'>';
    When Left(MTPfraddr,1) <> '"',
    &    Pos(',', MTPfraddr) > 0 Then
      buffer = '"'MTPfraddr'"';
    Otherwise
      buffer = MTPfraddr;
  End
  buffer = 'MAIL From:' buffer; /* 'SIZE=9000000'; */

  Call SendBuffers sid, buffer||crlf, Debug;
  Call RecvBuffers sid, Debug;
  If Left(smtp.1,3) >= 400 Then Signal NOGO;

/* ----------------------------------------------------------------- */
/* Set recipients                                                    */
/* ----------------------------------------------------------------- */

  Say Time() _pgm': Recipients...';

  Do i = 1 To MTPtoaddr.0
    Call SetRecipient MTPtoaddr.i;
    If Left(smtp.1,3) >= 400 Then Signal NOGO;
  End

  Do i = 1 To MTPccaddr.0
    Call SetRecipient MTPccaddr.i;
    If Left(smtp.1,3) >= 400 Then Signal NOGO;
  End

  Do i = 1 To MTPbccadr.0
    Call SetRecipient MTPbccadr.i;
    If Left(smtp.1,3) >= 400 Then Signal NOGO;
  End

/* ----------------------------------------------------------------- */
/* Signal ready to send data                                         */
/* ----------------------------------------------------------------- */

  buffer = 'DATA';

  Call SendBuffers sid, buffer||crlf, Debug;
  Call RecvBuffers sid, Debug;
  If Left(smtp.1,3) >= 400 Then Signal NOGO;

/* ----------------------------------------------------------------- */
/* Send mail headers                                                 */
/* ----------------------------------------------------------------- */

  Say Time() _pgm': Headers...';

  Call SendData 'Mime-Version: 1.0;'||crlf;
  If Length(MTPsense) > 0 Then Do
    Call SendData 'Sensitivity:' MTPsense';'||crlf;
  End
  Call SendData 'Content-Type: text/html; charset=UTF8;'||crlf;
  Call SendData 'Content-Transfer-Encoding: 8bit;'||crlf;
  Call SendData 'X-Mailer: Winsupply Socket Mailer;'||crlf;

  Call SendData 'From:' HandleComma(MTPfraddr)||crlf;

  If Length(MTPfrorga) > 0 Then Do
    Call SendData 'Organization:' MTPfrorga||crlf;
  End

  If Length(MTPreply2) > 0 Then Do
    Call SendData 'ReplyTo:' HandleComma(MTPreply2)||crlf;
  End

  Select
    When MTPtoaddr.0 > 0 Then Do
      Call SayAll 'Rcpt:' MTPtoaddr.1;
      Do i = 1 To MTPtoaddr.0
        If Length(MTPtoaddr.i) > 0 Then Do
          Call SendData 'To:' HandleComma(MTPtoaddr.i)||crlf;
        End
      End
    End
    Otherwise Do
      If MTPccaddr.0 > 0 Then
        Call SendData 'To: [see CC list]'||crlf;
      Else Do
        Call SendData 'To: [undisclosed recipients]'||crlf;
      End
    End
  End

  Do i = 1 To MTPccaddr.0
    If Length(MTPccaddr.i) > 0 Then Do
      Call SendData 'Cc:' HandleComma(MTPccaddr.i)||crlf;
    End
  End

  Call SayAll 'Subj:' MTPsubjct;
  Call SendData 'Subject:' MTPsubjct||crlf;

/* ----------------------------------------------------------------- */
/* Send mail body content                                            */
/* ----------------------------------------------------------------- */

  Say Time() _pgm': Body content...';

  n = 0;
  buffer = '';

  Do Forever                      /* stream text into body of email */
    n = n + 1;
    If n <= MTPtext.0,
    &  MTPtext.n ¬= 'QUIT' Then Do /* accumulate text into a buffer */
      chunk  = Translate(Strip(MTPtext.n,'Trailing'),'40'x,'41'x);
      buffer = buffer||chunk;
    End

    If Length(buffer) > chunk_size,
    |  n >= MTPtext.0 Then Do     /* when buffer ready, send it */
      If Length(buffer) > 0 Then Do
        buffer = ReplVars(buffer,'BODY');
        Call SendData buffer;
      End
      buffer = '';                /* clear the buffer for more */
      If n >= MTPtext.0 Then Do
        Drop MTPtext.;
        If eof_cards Then Leave;  /* get out if no more text cards */
        "EXECIO" chunk_size "DISKR SYSIPT ( STEM MTPtext. )";
        If rc ¬= 0 Then Do
          If rc == 2 Then Do
            eof_cards = _True;     /* set flag at end of text cards */
          End
          Else Do
            Say 'Error: EXECIO returned rc =' rc;
            Exit 16;
          End
        End
        n = 0;
      End
    End
  End

  "EXECIO 0 DISKR SYSIPT ( FINIS )";
  rc = 0;

  Call SendData crlf;             /* ending with a single cr/lf */
  buffer = '.';
  Call SendBuffers sid, buffer||crlf, Debug;
  Call RecvBuffers sid, Debug;
  If Left(smtp.1,3) >= 400 Then Signal NOGO;

/* ----------------------------------------------------------------- */
/* Sign off                                                          */
/* ----------------------------------------------------------------- */

  Say Time() _pgm': Complete...';

  rc = 0;
  Signal QUIT;

NOGO:
  Say Time() _pgm': Aborting...';

  buffer = 'RSET';

  Call SendBuffers sid, buffer||crlf, Debug;
  Call RecvBuffers sid, Debug;

  rc = 10;

QUIT:
  buffer = 'QUIT';

  Call SendBuffers sid, buffer||crlf, Debug;
  Call RecvBuffers sid, Debug;

/* ----------------------------------------------------------------- */
/* Terminate connection                                              */
/* ----------------------------------------------------------------- */

HALT:
  msg = Socket('Close', sid);
  msg = Socket('Terminate');

/* ----------------------------------------------------------------- */
/* Processing complete                                               */
/* ----------------------------------------------------------------- */

  Say Time() _pgm': Ended on' Date();

  If rc == 0,
  &  Translate(Left(Debug,1)) <> 'S' Then Do
    rc = 2;
  End

Exit rc;

/* ----------------------------------------------------------------- */
/* Abnormal termination occurred                                     */
/* ----------------------------------------------------------------- */

ABEND:
  Call RXDMPVAR;
  oldout = ASSGN('STDOUT','SYSLOG');
  Say _pgm': SEE SYSLST OUTPUT FOR ERRORS';
Exit rc;

/* ----------------------------------------------------------------- */
/* Internal functions follow                                         */
/* ----------------------------------------------------------------- */

Convert2ASCII: Procedure Expose asc ebc;
  Parse Arg buffer;
Return Translate(buffer,asc,ebc);

Convert2EBCDIC: Procedure Expose ebc asc;
  Parse Arg buffer;
Return Translate(buffer,ebc,asc);

Decode64: Procedure Expose B64 ebc asc;
  Parse Arg inp; out = '';
  If Right(inp,2) == '==' Then pad = 'AA'; Else
  If Right(inp,1) ==  '=' Then pad =  'A'; Else pad = '';
  inp = Convert2ASCII(SubStr(inp,1,Length(inp)-Length(pad))||pad);
  Do c = Length(inp) to 1 by -1
    If WordPos(SubStr(inp,c,1), B64) = 0 Then inp = DelStr(inp,c,1);
  End
  Do c = 1 to Length(inp) by 4
    b = Right(X2B(D2X(((WordPos(SubStr(inp,c+0,1),B64)-1)* (2 ** 18)) +,
                      ((WordPos(SubStr(inp,c+1,1),B64)-1)* (2 ** 12)) +,
                      ((WordPos(SubStr(inp,c+2,1),B64)-1)* (2 ** 6)) +,
                       (WordPos(SubStr(inp,c+3,1),B64)-1))), 24, '0');
    out = out||X2C(B2X(SubStr(b, 1,8))),
             ||X2C(B2X(SubStr(b, 9,8))),
             ||X2C(B2X(SubStr(b,17,8)));
  End
Return Convert2EBCDIC(SubStr(out,1,Length(out)-Length(pad)));

Encode64: Procedure Expose B64 ebc asc crlf;
  Parse Arg inp; out = '';
  c = Length(inp) // 3; If c > 0 Then pad = Copies('=', 3 - c);
  inp = Convert2ASCII(inp)||Copies('00'x, Length(pad));
  Do c = 1 to Length(inp) by 3
    If c > 1 & ((c-1) / 3 * 4) // 76 = 0 Then out = out||crlf;
    b = Right(X2B(D2X((C2D(SubStr(inp,c+0,1)) * (2 ** 16)) +,
                      (C2D(SubStr(inp,c+1,1)) * (2 ** 8)) +,
                       C2D(SubStr(inp,c+2,1)) )), 24, '0');
    out = out||Word(B64, X2D(B2X(SubStr(b, 1,6))) + 1),
             ||Word(B64, X2D(B2X(SubStr(b, 7,6))) + 1),
             ||Word(B64, X2D(B2X(SubStr(b,13,6))) + 1),
             ||Word(B64, X2D(B2X(SubStr(b,19,6))) + 1);
  End
Return Convert2EBCDIC(SubStr(out,1,Length(out)-Length(pad)))||pad;

GetContinuedCard: Procedure Expose card. i;
/* mail header cards can be continued up to three times */
  _card = Strip(card.i,'Trailing');
  If Right(_card,1) == '+' Then Do
    i = i + 1;
    _card = SubStr(_card,1,Length(_card)-1)||Strip(card.i,'Trailing');
  End
  If Right(_card,1) == '+' Then Do
    i = i + 1;
    _card = SubStr(_card,1,Length(_card)-1)||Strip(card.i,'Trailing');
  End
  If Right(_card,1) == '+' Then Do
    i = i + 1;
    _card = SubStr(_card,1,Length(_card)-1)||Strip(card.i,'Trailing');
  End
Return _card;

HandleComma: Procedure;
  Parse Arg addr;

  Parse Var addr addr2 '<' addr3 '>' .;
  addr2 = Strip(addr2);
  addr3 = Strip(addr3);

  If Pos('@', addr2) > 0,
  &  Length(addr3) == 0 Then Do
    addr3 = addr2;
    addr2 = '';
  End
  Else Do
    Parse Var addr2 addr1 '"' addr2 '"' .;
    addr1 = Strip(addr1);
    addr2 = Strip(addr2);
    If Length(addr1) > 0,
    &  Length(addr2) == 0 Then Do
      addr2 = addr1;
      addr1 = '';
    End
    If Pos(',', addr2) > 0 Then Do
      addr2 = '"'||addr2||'"';
    End
  End

  addr = addr2 '<'||addr3||'>';
Return Strip(addr);

Logit: Procedure;
  Parse Arg data, _debug;
  _debug = Translate(Left(_debug,1));
  If _debug <> 'S' Then Do
    Say data;
  End
Return;

MaskNonDisplay: Procedure Expose msk bad;
  Parse Arg buffer;
Return Translate(buffer,msk,bad);

MD5: Procedure;
/*Parse Arg inp; out = '';
//Note: All variables are unsigned 32 bits and wrap modulo 2^32 when
calculating
var int(64) r, k

//r specifies the per-round shift amounts
r( 0..15) := {7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17,
22}
r(16..31) := {5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14,
20}
r(32..47) := {4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16,
23}
r(48..63) := {6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15,
21}

//Use binary integer part of the sines of integers as constants:
for i from 0 to 63
    k(i) := floor(abs(sin(i + 1)) * (2 pow 32))

//Initialize variables:
var int h0 := 0x67452301
var int h1 := 0xEFCDAB89
var int h2 := 0x98BADCFE
var int h3 := 0x10325476

//Pre-processing:
append "1" bit to message
append "0" bits until message length in bits = 448 (mod 512)
append bit (bit, not byte) length of unpadded message as 64-bit
little-endian integer to message

//Process the message in successive 512-bit chunks:
for each 512-bit chunk of message
    break chunk into sixteen 32-bit little-endian words w(i), 0<=i<=15

    //Initialize hash value for this chunk:
    var int a := h0
    var int b := h1
    var int c := h2
    var int d := h3

    //Main loop:
    for i from 0 to 63
        if 0 = i = 15 then
            f := (b and c) or ((not b) and d)
            g := i
        else if 16 = i = 31
            f := (d and b) or ((not d) and c)
            g := (5 * i + 1) mod 16
        else if 32 = i = 47
            f := b xor c xor d
            g := (3 * i + 5) mod 16
        else if 48 = i = 63
            f := c xor (b or (not d))
            g := (7 * i) mod 16

        temp := d
        d := c
        c := b
        b := b + leftrotate((a + f + k(i) + w(g)) , r(i))
        a := temp

    //Add this chunk's hash to result so far:
    h0 := h0 + a
    h1 := h1 + b
    h2 := h2 + c
    h3 := h3 + d

var int digest := h0 append h1 append h2 append h3 //(expressed as
little-endian)
*/
Return out;
/*
//leftrotate function definition
leftrotate (x, c)
return (x << c) or (x >> (32-c));
*/

RecvBuffers: Procedure Expose _pgm ebc asc crlf msk bad smtp.;
  Parse Arg sockid, debug;
  debug = Translate(Left(debug,1));

  smtp.  = '';
  smtp.0 = 0;

  msg = Socket('Select', 'Read' sockid 'Write Exception', 30);
  Parse Var msg numsocks .;
  If numsocks = 0 Then Do
    Say Time() _pgm': Select for read timed out.';
    Return;
  End

  msg = Socket('Recv', sockid);
  Parse Var msg numbytes buffer;
  If debug == 'V' Then Say '-------- recv='numbytes '--------';
  If numbytes > 0 Then Do
    buffer = Convert2EBCDIC(buffer);
    Do n = (smtp.0 + 1) Until Length(buffer) = 0
      smtp.0 = n;                            /* parse buffer into */
      Parse Var buffer smtp.n (crlf) buffer; /* stem variable     */
      smtp.n = MaskNonDisplay(smtp.n);
      If debug <> 'S',
      |  Left(smtp.n,3) >= 400,
      Then Do
        If Left(smtp.n,3) >= 400 Then
          Call
        Else Do
          Say Time() smtp.n;
        End
      End
    End
  End
Return;

SendBuffers:  /* accepts either a single buffer or stem name for data */
  Parse Arg __sid, __buffer, __debug;
  If Length(__buffer)  > 1,
  &  Length(__buffer) <= 9,
  &  Right(__buffer,1) == '.',
  Then
    result = __buffer;
  Else Do
    __out.  = '';
    __out.0 = 1;
    __out.1 = __buffer;
    result = '__out.';
  End
  Call __SendBuffer __sid, result, __debug;
Return;

__SendBuffer: Procedure Expose _pgm asc ebc (result);
  Parse Arg sockid, stem, debug;
  debug = Translate(Left(debug,1));
  Do n = 1 to Value(stem||0)
    buffer  = Value(stem||n);
    If debug == 'V' Then
      Say '-------- send='||(Length(buffer)+2) '--------';
    If debug <> 'S' Then Do
      Select
        When Translate(Left(buffer,5)) == 'USER ' Then
          Say Time() 'USER ????????';
        When Translate(Left(buffer,5)) == 'PASS ' Then
          Say Time() 'PASS ????????';
        Otherwise
          Say Time() buffer;
      End
    End
    msg = Socket('Select', 'Read Write' sockid 'Exception', 30);
    Parse Var msg numsocks .;
    If numsocks = 0 Then Do
      Say Time() _pgm': Select for write timed out.';
      Return;
    End
    msg = Socket('Send', sockid, Convert2ASCII(buffer));
  End
Return;

SendData:
  Parse Arg buffer;
  Call SendBuffers sid, buffer, Debug;
Return;

SetRecipient:
  Parse Arg rcpt;

  Select
    When Pos('<', rcpt) > 0 Then
      buffer = SubStr(rcpt, Pos('<', rcpt));
    When Pos('@', rcpt) > 0 Then
      buffer = '<'rcpt'>';
    Otherwise Do
      buffer = 'VRFY' rcpt;
      Call SendBuffers sid, buffer, Debug;
      Call RecvBuffers sid, Debug;

      If Length(rcpt) > 0,
      &  Left(rcpt,1) <> '"',
      &  Pos(',', rcpt) > 0,
      Then
        buffer = '"'rcpt'"';
      Else Do
        buffer = Strip(rcpt);
      End
    End
  End

  If Length(buffer) > 0 Then Do
    buffer = 'RCPT To:' buffer;
    Call SendBuffers sid, buffer||crlf, Debug;
    Call RecvBuffers sid, Debug;
  End
Return;

Socket: Procedure Expose _pgm;
  Parse Value 'SOCKET'(Arg(1),Arg(2),Arg(3),Arg(4),Arg(5),,
                       Arg(6),Arg(7),Arg(8),Arg(9),Arg(10)),
        With src buffer;
  If src <> 0 Then Do
    Say Time() _pgm':' Arg(1) 'failed:' src buffer;
    Exit 8;
  End
Return buffer;

/INCLUDE COPYBOOKS.XFUNCTNS
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
