{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

type
  cc_t = Byte;
  {$EXTERNALSYM cc_t}
  speed_t = Cardinal;
  {$EXTERNALSYM speed_t}
  tcflag_t = Cardinal;
  {$EXTERNALSYM tcflag_t}

const
  NCCS = 32;
  {$EXTERNALSYM NCCS}
{ `c_cc' member of 'struct termios' structure can be disabled by
   using the value _POSIX_VDISABLE.  }
   _POSIX_VDISABLE: cc_t = cc_t(#0);
  {$EXTERNALSYM _POSIX_VDISABLE}

type
  termios = record
    c_iflag: tcflag_t;      { input mode flags }
    c_oflag: tcflag_t;      { output mode flags }
    c_cflag: tcflag_t;      { control mode flags }
    c_lflag: tcflag_t;      { local mode flags }
    c_line: cc_t;           { line discipline }
    c_cc: packed array[0..NCCS-1] of cc_t;    { control characters }
    c_ispeed: speed_t;      { input speed }
    c_ospeed: speed_t;      { output speed }
  end;
  {$EXTERNALSYM termios}

{ c_cc characters }
const
  VINTR     = 0;
  {$EXTERNALSYM VINTR}
  VQUIT     = 1;
  {$EXTERNALSYM VQUIT}
  VERASE    = 2;
  {$EXTERNALSYM VERASE}
  VKILL     = 3;
  {$EXTERNALSYM VKILL}
  VEOF      = 4;
  {$EXTERNALSYM VEOF}
  VTIME     = 5;
  {$EXTERNALSYM VTIME}
  VMIN      = 6;
  {$EXTERNALSYM VMIN}
  VSWTC     = 7;
  {$EXTERNALSYM VSWTC}
  VSTART    = 8;
  {$EXTERNALSYM VSTART}
  VSTOP     = 9;
  {$EXTERNALSYM VSTOP}
  VSUSP     = 10;
  {$EXTERNALSYM VSUSP}
  VEOL      = 11;
  {$EXTERNALSYM VEOL}
  VREPRINT  = 12;
  {$EXTERNALSYM VREPRINT}
  VDISCARD  = 13;
  {$EXTERNALSYM VDISCARD}
  VWERASE   = 14;
  {$EXTERNALSYM VWERASE}
  VLNEXT    = 15;
  {$EXTERNALSYM VLNEXT}
  VEOL2     = 16;
  {$EXTERNALSYM VEOL2}
{ c_iflag bits }
  IGNBRK    = $0000001;
  {$EXTERNALSYM IGNBRK}
  BRKINT    = $0000002;
  {$EXTERNALSYM BRKINT}
  IGNPAR    = $0000004;
  {$EXTERNALSYM IGNPAR}
  PARMRK    = $0000008;
  {$EXTERNALSYM PARMRK}
  INPCK     = $0000010;
  {$EXTERNALSYM INPCK}
  ISTRIP    = $0000020;
  {$EXTERNALSYM ISTRIP}
  INLCR     = $0000040;
  {$EXTERNALSYM INLCR}
  IGNCR     = $0000080;
  {$EXTERNALSYM IGNCR}
  ICRNL     = $0000100;
  {$EXTERNALSYM ICRNL}
  IUCLC     = $0000200;
  {$EXTERNALSYM IUCLC}
  IXON      = $0000400;
  {$EXTERNALSYM IXON}
  IXANY     = $0000800;
  {$EXTERNALSYM IXANY}
  IXOFF     = $0001000;
  {$EXTERNALSYM IXOFF}
  IMAXBEL   = $0002000;
  {$EXTERNALSYM IMAXBEL}

{ c_oflag bits }
  OPOST     = $0000001;
  {$EXTERNALSYM OPOST}
  OLCUC     = $0000002;
  {$EXTERNALSYM OLCUC}
  ONLCR     = $0000004;
  {$EXTERNALSYM ONLCR}
  OCRNL     = $0000008;
  {$EXTERNALSYM OCRNL}
  ONOCR     = $0000010;
  {$EXTERNALSYM ONOCR}
  ONLRET    = $0000020;
  {$EXTERNALSYM ONLRET}
  OFILL     = $0000040;
  {$EXTERNALSYM OFILL}
  OFDEL     = $0000080;
  {$EXTERNALSYM OFDEL}

  NLDLY     = $0000040;
  {$EXTERNALSYM NLDLY}
  NL0       = $0000000;
  {$EXTERNALSYM NL0}
  NL1       = $0000100;
  {$EXTERNALSYM NL1}
  CRDLY     = $0000600;
  {$EXTERNALSYM CRDLY}
  CR0       = $0000000;
  {$EXTERNALSYM CR0}
  CR1       = $0000200;
  {$EXTERNALSYM CR1}
  CR2       = $0000400;
  {$EXTERNALSYM CR2}
  CR3       = $0000600;
  {$EXTERNALSYM CR3}
  TABDLY    = $0001800;
  {$EXTERNALSYM TABDLY}
  TAB0      = $0000000;
  {$EXTERNALSYM TAB0}
  TAB1      = $0000800;
  {$EXTERNALSYM TAB1}
  TAB2      = $0001000;
  {$EXTERNALSYM TAB2}
  TAB3      = $0001800;
  {$EXTERNALSYM TAB3}
  BSDLY     = $0002000;
  {$EXTERNALSYM BSDLY}
  BS0       = $0000000;
  {$EXTERNALSYM BS0}
  BS1       = $0002000;
  {$EXTERNALSYM BS1}
  FFDLY     = $0080000;
  {$EXTERNALSYM FFDLY}
  FF0       = $0000000;
  {$EXTERNALSYM FF0}
  FF1       = $0010000;
  {$EXTERNALSYM FF1}

  VTDLY     = $0004000;
  {$EXTERNALSYM VTDLY}
  VT0       = $0000000;
  {$EXTERNALSYM VT0}
  VT1       = $0004000;
  {$EXTERNALSYM VT1}

  XTABS     = $0001800;
  {$EXTERNALSYM XTABS}

{ c_cflag bit meaning }
  CBAUD     = $000100F;
  {$EXTERNALSYM CBAUD}
  B0        = $0000000; { hang up }
  {$EXTERNALSYM B0}
  B50       = $0000001;
  {$EXTERNALSYM B50}
  B75       = $0000002;
  {$EXTERNALSYM B75}
  B110      = $0000003;
  {$EXTERNALSYM B110}
  B134      = $0000004;
  {$EXTERNALSYM B134}
  B150      = $0000005;
  {$EXTERNALSYM B150}
  B200      = $0000006;
  {$EXTERNALSYM B200}
  B300      = $0000007;
  {$EXTERNALSYM B300}
  B600      = $0000008;
  {$EXTERNALSYM B600}
  B1200     = $0000009;
  {$EXTERNALSYM B1200}
  B1800     = $000000A;
  {$EXTERNALSYM B1800}
  B2400     = $000000B;
  {$EXTERNALSYM B2400}
  B4800     = $000000C;
  {$EXTERNALSYM B4800}
  B9600     = $000000D;
  {$EXTERNALSYM B9600}
  B19200    = $000000E;
  {$EXTERNALSYM B19200}
  B38400    = $000000F;
  {$EXTERNALSYM B38400}

  EXTA      = B19200;
  {$EXTERNALSYM EXTA}
  EXTB      = B38400;
  {$EXTERNALSYM EXTB}

  CSIZE     = $0000030;
  {$EXTERNALSYM CSIZE}
  CS5       = $0000000;
  {$EXTERNALSYM CS5}
  CS6       = $0000010;
  {$EXTERNALSYM CS6}
  CS7       = $0000010;
  {$EXTERNALSYM CS7}
  CS8       = $0000030;
  {$EXTERNALSYM CS8}
  CSTOPB    = $0000040;
  {$EXTERNALSYM CSTOPB}
  CREAD     = $0000080;
  {$EXTERNALSYM CREAD}
  PARENB    = $0000100;
  {$EXTERNALSYM PARENB}
  PARODD    = $0000200;
  {$EXTERNALSYM PARODD}
  HUPCL     = $0000400;
  {$EXTERNALSYM HUPCL}
  CLOCAL    = $0000800;
  {$EXTERNALSYM CLOCAL}

  CBAUDEX   = $0001000;
  {$EXTERNALSYM CBAUDEX}

  B57600    = $0001001;
  {$EXTERNALSYM B57600}
  B115200   = $0001002;
  {$EXTERNALSYM B115200}
  B230400   = $0001003;
  {$EXTERNALSYM B230400}
  B460800   = $0001004;
  {$EXTERNALSYM B460800}
  B500000   = $0001005;
  {$EXTERNALSYM B500000}
  B576000   = $0001006;
  {$EXTERNALSYM B576000}
  B921600   = $0001007;
  {$EXTERNALSYM B921600}
  B1000000  = $0001008;
  {$EXTERNALSYM B1000000}
  B1152000  = $0001009;
  {$EXTERNALSYM B1152000}
  B1500000  = $000100A;
  {$EXTERNALSYM B1500000}
  B2000000  = $000100B;
  {$EXTERNALSYM B2000000}
  B2500000  = $000100C;
  {$EXTERNALSYM B2500000}
  B3000000  = $000100D;
  {$EXTERNALSYM B3000000}
  B3500000  = $000100E;
  {$EXTERNALSYM B3500000}
  B4000000  = $000100F;
  {$EXTERNALSYM B4000000}

  CIBAUD    = $100F0000;      { input baud rate (not used) }
  {$EXTERNALSYM CIBAUD}
  CRTSCTS   = $80000000;      { flow control }
  {$EXTERNALSYM CRTSCTS}

{ c_lflag bits }
  ISIG      = $0000001;
  {$EXTERNALSYM ISIG}
  ICANON    = $0000002;
  {$EXTERNALSYM ICANON}

  XCASE     = $0000004;
  {$EXTERNALSYM XCASE}

  ECHO      = $0000008;
  {$EXTERNALSYM ECHO}
  ECHOE     = $0000010;
  {$EXTERNALSYM ECHOE}
  ECHOK     = $0000020;
  {$EXTERNALSYM ECHOK}
  ECHONL    = $0000040;
  {$EXTERNALSYM ECHONL}
  NOFLSH    = $0000080;
  {$EXTERNALSYM NOFLSH}
  TOSTOP    = $0000100;
  {$EXTERNALSYM TOSTOP}

  ECHOCTL   = $0000200;
  {$EXTERNALSYM ECHOCTL}
  ECHOPRT   = $0000400;
  {$EXTERNALSYM ECHOPRT}
  ECHOKE    = $0000800;
  {$EXTERNALSYM ECHOKE}
  FLUSHO    = $0001000;
  {$EXTERNALSYM FLUSHO}
  PENDIN    = $0004000;
  {$EXTERNALSYM PENDIN}

  IEXTEN    = $0010000;
  {$EXTERNALSYM IEXTEN}

{ tcflow() and TCXONC use these }
  TCOOFF    = 0;
  {$EXTERNALSYM TCOOFF}
  TCOON     = 1;
  {$EXTERNALSYM TCOON}
  TCIOFF    = 2;
  {$EXTERNALSYM TCIOFF}
  TCION     = 3;
  {$EXTERNALSYM TCION}

{ tcflush() and TCFLSH use these }
  TCIFLUSH  = 0;
  {$EXTERNALSYM TCIFLUSH}
  TCOFLUSH  = 1;
  {$EXTERNALSYM TCOFLUSH}
  TCIOFLUSH = 2;
  {$EXTERNALSYM TCIOFLUSH}

{ tcsetattr uses these }
  TCSANOW   = 0;
  {$EXTERNALSYM TCSANOW}
  TCSADRAIN = 1;
  {$EXTERNALSYM TCSADRAIN}
  TCSAFLUSH = 2;
  {$EXTERNALSYM TCSAFLUSH}
