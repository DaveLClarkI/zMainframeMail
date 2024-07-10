/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* REXX: BSIMAILC   Proc Created 09/12/2007   By Dave Clark        */
/*                  Last Revised 07/10/2024                        */
/*                                                                 */
/*      This procedure checks a POP3  mail server on behalf of the */
/* specified mailbox user.  All input is via card.  See below.     */
/*                                                                 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* No warranty is expressed or implied.    Written by Dave L Clark */
/* Neither the author nor the company is   WinWholesale Group Svcs */
/* responsible for any loss or damage      3110 Kettering Blvd.    */
/* resulting from the use of this code.    Dayton, OH  45439       */
/* Source is provided on an as-is basis.   Phone (937)294-5331     */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* The following is example JCL for running this procedure:        */
/*                                                                 */
/*   // JOB    BSIMAILC   POP3 Socket to Mail Server               */
/*   // LIBDEF *,SEARCH=(PRD2.CONFIG,ESP.BSI),TEMP                 */
/*   // EXEC   REXX=BSIMAILC                                       */
/*   ID   stack_id                                                 */
/*   OPEN host_address                                             */
/*   USER email_address                                            */
/*   PASS password                                                 */
/*   <{READ|DELE} {ALL|mailid}>                                    */
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
/*                     dress for the POP3 mail server to contact;  */
/*        AUSR       is a required keyword tag if the server       */
/*                     requires POP3 authentication;               */
/*        user_name  is the fully-qualified user name for POP3     */
/*                     server authentication processing;           */
/*        APWD       is a required keyword tag if the server       */
/*                     requires POP3 authentication;               */
/*        password   is the password for POP3 server               */
/*                     authentication processing;                  */
/*        USER       is a required keyword tag;                    */
/*        email_address is the fully-qualified email address       */
/*                     for the POP3 mailbox to be accessed;        */
/*        PASS       is a required keyword tag;                    */
/*        password   is the password for the POP3 mailbox          */
/*                     to be accessed;                             */
/*        READ       is the default keyword tag for READ-only      */
/*                     retrieval of available emails;              */
/*        DELE       is a required keyword tag if emails are to be */
/*                     deleted after retrieval (if omitted, the    */
/*                     default is for READ-only retrieval);        */
/*        ALL        requests that all emails are to be retrieved  */
/*                     (and optionally deleted); and               */
/*        mailid     is the UIDL identifier of the specific email  */
/*                     to be retrieved (and optionally deleted).   */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

  Parse Source sys env pgm tokens;

  False = (0=1);
  True  = (1=1);

  Debug = 'Verbose';         /* Specify 'Verbose' or 'Silent' */

/* ----------------------------------------------------------------- */
/* Get PARM argument (dummy)                                         */
/* ----------------------------------------------------------------- */

  rc  = 0
  Arg msg

/* ----------------------------------------------------------------- */
/* Get SYSIPT data                                                   */
/* ----------------------------------------------------------------- */

  EXECIO "* DISKR SYSIPT ( OPEN FINIS STEM card. )";
  If rc <> 0 Then Do
    Say 'Error: EXECIO returned rc =' rc;
    Exit 16;
  End

/* ----------------------------------------------------------------- */
/* Process SYSIPT data                                               */
/* ----------------------------------------------------------------- */

  tcpid     = '';
  POPserver = '';
  POPauthid = '';
  POPauthpw = '';
  POPuserid = '';
  POPuserpw = '';
  func      = '';
  fopt      = '';

  Do i = 1 To card.0
    If Left(card.i,1) == '*' Then Iterate;
    Select
      When Left(card.i,3) == 'ID '   Then
        Parse Var card.i . tcpid .;
      When Left(card.i,5) == 'OPEN ' Then
        Parse Var card.i . POPserver .;
      When Left(card.i,5) == 'AUSR ' Then
        Parse Var card.i . POPauthid .;
      When Left(card.i,5) == 'APWD ' Then
        Parse Var card.i . POPauthpw .;
      When Left(card.i,5) == 'USER ' Then
        Parse Var card.i . POPuserid .;
      When Left(card.i,5) == 'PASS ' Then
        Parse Var card.i . POPuserpw .;
      When Left(card.i,5) == 'READ ',
      |    Left(card.i,5) == 'DELE ' Then
        Parse Var card.i func fopt .;
      Otherwise
        Do
          Say ' 'card.i;
          Say ' Unknown input keyword specified.';
        End
    End
  End

  Drop card.;

  If tcpid <> '',
  & \DataType(tcpid,'W') Then Do
    Say pgm': TCP/IP stack ID must be a whole number';
    rc = 8;
  End
  If POPserver = '' Then Do
    Say pgm': No HOST NAME or IP ADDRESS specified';
    rc = 8;
  End
  If POPauthid <> '',
  &  POPauthpw  = '' Then Do
    Say pgm': AUSR specified without APWD';
    rc = 8;
  End
  If POPauthpw <> '',
  If POPauthid  = '' Then Do
    Say pgm': APWD specified without AUSR';
    rc = 8;
  End
  If POPuserid = '' Then Do
    Say pgm': No mailbox USER ID specified';
    rc = 8;
  End
  If POPuserpw = '' Then Do
    Say pgm': No mailbox USER PASSWORD specified';
    rc = 8;
  End
  If func   = '' Then Do
    func = 'READ';
  End
  If fopt   = '' Then Do
    If func = 'DELE' Then Do
      Say pgm': DELE ALL must be explicit, not defaulted';
      rc = 8;
    End
    fopt = 'ALL';
  End

  If rc > 0 Then Do
    Signal ABEND;
  End

  If tcpid  = '' Then Do
    Say Time() pgm': TCP/IP STACK ID DEFAULTS TO 00';
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
        'YZÝ\¨^_`abcdefghijk'||,
        'lmnopqrstuvwxyz{|}~';

  B64 = 'A B C D E F G H I J K L M N O P Q R S T U V W X Y Z',
        'a b c d e f g h i j k l m n o p q r s t u v w x y z',
        '0 1 2 3 4 5 6 7 8 9 + /';
  B64 = Translate(B64,SubStr(asc,2),SubStr(ebc,2));

/* ----------------------------------------------------------------- */
/* Begin processing                                                  */
/* ----------------------------------------------------------------- */

  oldmsg = RexxMsg("OFF");         /* we'll get the messages ourself */
  Call ASSGN 'STDOUT','SYSLST';

  Say Time() pgm': Start on' Date();

/* ----------------------------------------------------------------- */
/* Allocate socket                                                   */
/* ----------------------------------------------------------------- */

  msg = Socket('Initialize', pgm, 1, tcpname);
  If src <> 0 Then Exit 8;

  msg = Socket('Socket');
  If src <> 0 Then Exit 8;

  Parse Var msg sid .;

/* ----------------------------------------------------------------- */
/* Establish connection                                              */
/* ----------------------------------------------------------------- */

  iphost = POPserver;
  If DataType(Left(iphost,1)) = 'CHAR' Then Do
    msg = Socket('GetHostByName', iphost);
    If src <> 0 Then Exit 8;
    Parse Var msg iphost .;
  End

  sname  = 'AF_INET 110' iphost;
  Say Time() pgm': Connecting...' sname;

  msg = Socket('Connect', sid, sname);
  If src <> 0 Then Exit 8;

  src = RecvBuffers(sid, Debug);
  If src <> 0 Then Exit 8;
/*
  msg = Socket('Fcntl', sid, 'F_SETFL', 'NON-BLOCKING');
  If src <> 0 Then Exit 8;
*/
/* ----------------------------------------------------------------- */
/* Authorize                                                         */
/* ----------------------------------------------------------------- */

  If Length(POPauthid) > 0 Then Do

    buffer = 'APOP' POPauthid;

    src = SendBuffers(sid, buffer, Debug);
    If src <> 0 Then Exit 8;
    src = RecvBuffers(sid, Debug);
    If src <> 0 Then Exit 8;
    If Left(pop3.1,4) == '-ERR' Then Signal QUIT;

    buffer = POPauthpw;

    src = SendBuffers(sid, buffer, Debug);
    If src <> 0 Then Exit 8;
    src = RecvBuffers(sid, Debug);
    If src <> 0 Then Exit 8;
    If Left(pop3.1,4) == '-ERR' Then Signal QUIT;

  End

/* ----------------------------------------------------------------- */
/* Sign on                                                           */
/* ----------------------------------------------------------------- */

  buffer = 'USER' POPuserid;

  src = SendBuffers(sid, buffer, Debug);
  If src <> 0 Then Exit 8;
  src = RecvBuffers(sid, Debug);
  If src <> 0 Then Exit 8;
  If Left(pop3.1,4) == '-ERR' Then Signal QUIT;

  buffer = 'PASS' POPuserpw;

  src = SendBuffers(sid, buffer, Debug);
  If src <> 0 Then Exit 8;
  src = RecvBuffers(sid, Debug);
  If src <> 0 Then Exit 8;
  If Left(pop3.1,4) == '-ERR' Then Signal QUIT;

/* ----------------------------------------------------------------- */
/* Check for email                                                   */
/* ----------------------------------------------------------------- */

  buffer = 'STAT';

  src = SendBuffers(sid, buffer, Debug);
  If src <> 0 Then Exit 8;
  src = RecvBuffers(sid, Debug);
  If src <> 0 Then Exit 8;

  Parse Var pop3.1 . count total_size .;
  If count == 0 Then Signal QUIT;

  buffer = 'UIDL';

  src = SendBuffers(sid, buffer, Debug);
  If src <> 0 Then Exit 8;
  src = RecvBuffers(sid, Debug);
  If src <> 0 Then Exit 8;

/* ----------------------------------------------------------------- */
/* Process email                                                     */
/* ----------------------------------------------------------------- */

  list.  = '';
  list.0 = 0;
  Do c = 2 to (pop3.0 - 1)
    n = list.0 + 1;
    list.n = pop3.c;
    list.0 = n;
  End

  Say ' ';
  Do n = 1 to list.0
    Parse Var list.n mid uid .;
    If fopt <> 'ALL',
    &  fopt <> mid,
    &  fopt <> uid,
    Then Do
      Iterate;
    End
    buffer = 'RETR' mid;
    src = SendBuffers(sid, buffer, Debug);
    If src <> 0 Then Exit 8;
    src = RecvBuffers(sid, Debug);
    If src <> 0 Then Exit 8;

    /*                                                               */
    /* process pop3. stem var now for email content plus attachments */
    /*                                                               */

    If func == 'DELE' Then Do
      buffer = func mid;
      src = SendBuffers(sid, buffer, Debug);
      If src <> 0 Then Exit 8;
      src = RecvBuffers(sid, Debug);
      If src <> 0 Then Exit 8;
    End
    Say ' ';
  End

/* ----------------------------------------------------------------- */
/* Sign off                                                          */
/* ----------------------------------------------------------------- */

QUIT:
  buffer = 'QUIT';

  src = SendBuffers(sid, buffer, Debug);
  If src <> 0 Then Exit 8;
  src = RecvBuffers(sid, Debug);
  If src <> 0 Then Exit 8;

/* ----------------------------------------------------------------- */
/* Terminate connection                                              */
/* ----------------------------------------------------------------- */

HALT:
  msg = Socket('Close', sid);
  msg = Socket('Terminate');

/* ----------------------------------------------------------------- */
/* Processing complete                                               */
/* ----------------------------------------------------------------- */

  Say Time() pgm': Ended on' Date();

Exit 0;

/* ----------------------------------------------------------------- */
/* Abnormal termination occurred                                     */
/* ----------------------------------------------------------------- */

ABEND:
  Call RXDMPVAR;
  oldout = ASSGN('STDOUT','SYSLOG');
  Say pgm': SEE SYSLST OUTPUT FOR ERRORS';
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

MaskNonDisplay: Procedure Expose msk bad;
  Parse Arg buffer;
Return Translate(buffer,msk,bad);

MD5: Procedure;   /* unconverted procedure content is commented out */
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

RecvBuffers: Procedure Expose pgm ebc asc crlf msk bad pop3.;
  Parse Arg sockid, debug;
  debug = Translate(Left(debug,1));
  pop3.  = '';
  pop3.0 = 0;
  Do c = 1 Until src <> 0
    msg = Socket('Select', 'Read' sockid 'Write Exception', 120)
    If src <> 0 Then Return 8;
    Parse Var msg numsocks .;
    If numsocks = 0 Then Do
      Say Time() pgm': Select for read timed out.';
      src = -1;
      Leave;
    End
    msg = Socket('Recv', sockid);
    If src <> 0 Then Return 8;
    Parse Var msg numbytes buffer;
    If debug == 'V' Then Say '-------- recv='numbytes '--------';
    If numbytes = 0 Then Leave;
    buffer = Convert2EBCDIC(buffer);
    Do n = (pop3.0 + 1) Until Length(buffer) = 0
      pop3.0 = n;                           /* parse buffer into */
      Parse Var buffer pop3.n (crlf) buffer; /* stem variable     */
      pop3.n = MaskNonDisplay(pop3.n);
      If debug == 'V' Then Say Time() pop3.n;
    End
    n = pop3.0;
    If pop3.n == '.',
    |  Left(pop3.n,3) == '+OK',
    |  Left(pop3.n,4) == '-ERR',
    Then Do
      Leave;
    End
  End
Return src;

SendBuffers:  /* accepts either a single buffer or stem name for data */
  Parse Arg __sid, __buffer, __debug;
  If Length(__buffer) <= 9,
  &  Right(__buffer,1) == '.' Then
    result = __buffer;
  Else Do
    __out.  = '';
    __out.0 = 1;
    __out.1 = __buffer;
    result = '__out.';
  End
Return __SendBuffer(__sid, result, __debug);

__SendBuffer: Procedure Expose pgm asc ebc crlf (result);
  Parse Arg sockid, stem, debug;
  debug = Translate(Left(debug,1));
  Do n = 1 to Value(stem||0) Until src <> 0
    buffer  = Value(stem||n);
    If debug == 'V' Then Do
      Say '-------- send='||(Length(buffer)+2) '--------';
      Select
      When Translate(Left(buffer,5)) == 'USER ' Then
        Say Time() 'USER ????????';
      When Translate(Left(buffer,5)) == 'PASS ' Then
        Say Time() 'PASS ????????';
      Otherwise
        Say Time() buffer;
      End
    End
    msg = Socket('Select', 'Read Write' sockid 'Exception', 120)
    If src <> 0 Then Return 8;
    Parse Var msg numsocks .;
    If numsocks = 0 Then Do
      Say Time() pgm': Select for write timed out.';
      src = -1;
      Leave;
    End
    msg = Socket('Send', sockid, Convert2ASCII(buffer)||crlf);
    If src <> 0 Then Return 8;
  End
Return src;

Socket: Procedure Expose pgm src;
  Parse Value 'SOCKET'(Arg(1),Arg(2),Arg(3),Arg(4),Arg(5),,
                       Arg(6),Arg(7),Arg(8),Arg(9),Arg(10)),
        With src buffer;
  If src <> 0 Then Say Time() pgm':' Arg(1) 'failed:' src buffer;
Return buffer;

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
