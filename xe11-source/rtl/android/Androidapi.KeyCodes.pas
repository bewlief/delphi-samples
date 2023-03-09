{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.KeyCodes;

interface

(*$HPPEMIT '#include <android/keycodes.h>' *)

const
  /// <symmary>Unknown key code.</summary>
  AKEYCODE_UNKNOWN                       =   0; // $00
  {$EXTERNALSYM AKEYCODE_UNKNOWN}

  /// <symmary>Soft Left key.<br />
  /// Usually situated below the display on phones and used as a multi-function
  /// feature key for selecting a software defined function shown on the bottom
  /// left of the display.</summary>
  AKEYCODE_SOFT_LEFT                     =   1; // $01
  {$EXTERNALSYM AKEYCODE_SOFT_LEFT}

  /// <symmary>Soft Right key.<br />
  /// Usually situated below the display on phones and used as a multi-function
  /// feature key for selecting a software defined function shown on the bottom
  /// right of the display.</summary>
  AKEYCODE_SOFT_RIGHT                    =   2; // $02
  {$EXTERNALSYM AKEYCODE_SOFT_RIGHT}

  /// <symmary>Home key.<br />
  /// This key is handled by the framework and is never delivered to
  /// applications.</summary>
  AKEYCODE_HOME                          =   3; // $03
  {$EXTERNALSYM AKEYCODE_HOME}

  /// <symmary>Back key.</summary>
  AKEYCODE_BACK                          =   4; // $04
  {$EXTERNALSYM AKEYCODE_BACK}

  /// <symmary>Call key.</summary>
  AKEYCODE_CALL                          =   5; // $05
  {$EXTERNALSYM AKEYCODE_CALL}

  /// <symmary>End Call key.</summary>
  AKEYCODE_ENDCALL                       =   6; // $06
  {$EXTERNALSYM AKEYCODE_ENDCALL}

  /// <summary>'0' key.</summary>
  AKEYCODE_0                             =   7; // $07
  {$EXTERNALSYM AKEYCODE_0}

  /// <summary>'1' key.</summary>
  AKEYCODE_1                             =   8; // $08
  {$EXTERNALSYM AKEYCODE_1}

  /// <summary>'2' key.</summary>
  AKEYCODE_2                             =   9; // $09
  {$EXTERNALSYM AKEYCODE_2}

  /// <summary>'3' key.</summary>
  AKEYCODE_3                             =  10; // $0A
  {$EXTERNALSYM AKEYCODE_3}

  /// <summary>'4' key.</summary>
  AKEYCODE_4                             =  11; // $0B
  {$EXTERNALSYM AKEYCODE_4}

  /// <summary>'5' key.</summary>
  AKEYCODE_5                             =  12; // $0C
  {$EXTERNALSYM AKEYCODE_5}

  /// <summary>'6' key.</summary>
  AKEYCODE_6                             =  13; // $0D
  {$EXTERNALSYM AKEYCODE_6}

  /// <summary>'7' key.</summary>
  AKEYCODE_7                             =  14; // $0E
  {$EXTERNALSYM AKEYCODE_7}

  /// <summary>'8' key.</summary>
  AKEYCODE_8                             =  15; // $0F
  {$EXTERNALSYM AKEYCODE_8}

  /// <summary>'9' key.</summary>
  AKEYCODE_9                             =  16; // $10
  {$EXTERNALSYM AKEYCODE_9}

  /// <summary>'*' key.</summary>
  AKEYCODE_STAR                          =  17; // $11
  {$EXTERNALSYM AKEYCODE_STAR}

  /// <summary>'#' key.</summary>
  AKEYCODE_POUND                         =  18; // $12
  {$EXTERNALSYM AKEYCODE_POUND}

  /// <summary>Directional Pad Up key.<br />
  /// May also be synthesized from trackball motions.</summary>
  AKEYCODE_DPAD_UP                       =  19; // $13
  {$EXTERNALSYM AKEYCODE_DPAD_UP}

  /// <summary>Directional Pad Down key.<br />
  /// May also be synthesized from trackball motions.</summary>
  AKEYCODE_DPAD_DOWN                     =  20; // $14
  {$EXTERNALSYM AKEYCODE_DPAD_DOWN}

  /// <summary>Directional Pad Left key.<br />
  /// May also be synthesized from trackball motions.</summary>
  AKEYCODE_DPAD_LEFT                     =  21; // $15
  {$EXTERNALSYM AKEYCODE_DPAD_LEFT}

  /// <summary>Directional Pad Right key.<br />
  /// May also be synthesized from trackball motions.</summary>
  AKEYCODE_DPAD_RIGHT                    =  22; // $16
  {$EXTERNALSYM AKEYCODE_DPAD_RIGHT}

  /// <summary>Directional Pad Center key.<br />
  /// May also be synthesized from trackball motions.</summary>
  AKEYCODE_DPAD_CENTER                   =  23; // $17
  {$EXTERNALSYM AKEYCODE_DPAD_CENTER}

  /// <summary>Volume Up key.<br />
  /// Adjusts the speaker volume up.</summary>
  AKEYCODE_VOLUME_UP                     =  24; // $18
  {$EXTERNALSYM AKEYCODE_VOLUME_UP}

  /// <summary>Volume Down key.<br />
  /// Adjusts the speaker volume down.</summary>
  AKEYCODE_VOLUME_DOWN                   =  25; // $19
  {$EXTERNALSYM AKEYCODE_VOLUME_DOWN}

  /// <summary>Power key.</summary>
  AKEYCODE_POWER                         =  26; // $1A
  {$EXTERNALSYM AKEYCODE_POWER}

  /// <summary>Camera key.<br />
  /// Used to launch a camera application or take pictures.</summary>
  AKEYCODE_CAMERA                        =  27; // $1B
  {$EXTERNALSYM AKEYCODE_CAMERA}

  /// <summary>Clear key.</summary>
  AKEYCODE_CLEAR                         =  28; // $1C
  {$EXTERNALSYM AKEYCODE_CLEAR}

  /// <summary>'A' key.</summary>
  AKEYCODE_A                             =  29; // $1D
  {$EXTERNALSYM AKEYCODE_A}

  /// <summary>'B' key.</summary>
  AKEYCODE_B                             =  30; // $1E
  {$EXTERNALSYM AKEYCODE_B}

  /// <summary>'C' key.</summary>
  AKEYCODE_C                             =  31; // $1F
  {$EXTERNALSYM AKEYCODE_C}

  /// <summary>'D' key.</summary>
  AKEYCODE_D                             =  32; // $20
  {$EXTERNALSYM AKEYCODE_D}

  /// <summary>'E' key.</summary>
  AKEYCODE_E                             =  33; // $21
  {$EXTERNALSYM AKEYCODE_E}

  /// <summary>'F' key.</summary>
  AKEYCODE_F                             =  34; // $22
  {$EXTERNALSYM AKEYCODE_F}

  /// <summary>'G' key.</summary>
  AKEYCODE_G                             =  35; // $23
  {$EXTERNALSYM AKEYCODE_G}

  /// <summary>'H' key.</summary>
  AKEYCODE_H                             =  36; // $24
  {$EXTERNALSYM AKEYCODE_H}

  /// <summary>'I' key.</summary>
  AKEYCODE_I                             =  37; // $25
  {$EXTERNALSYM AKEYCODE_I}

  /// <summary>'J' key.</summary>
  AKEYCODE_J                             =  38; // $26
  {$EXTERNALSYM AKEYCODE_J}

  /// <summary>'K' key.</summary>
  AKEYCODE_K                             =  39; // $27
  {$EXTERNALSYM AKEYCODE_K}

  /// <summary>'L' key.</summary>
  AKEYCODE_L                             =  40; // $28
  {$EXTERNALSYM AKEYCODE_L}

  /// <summary>'M' key.</summary>
  AKEYCODE_M                             =  41; // $29
  {$EXTERNALSYM AKEYCODE_M}

  /// <summary>'N' key.</summary>
  AKEYCODE_N                             =  42; // $2A
  {$EXTERNALSYM AKEYCODE_N}

  /// <summary>'O' key.</summary>
  AKEYCODE_O                             =  43; // $2B
  {$EXTERNALSYM AKEYCODE_O}

  /// <summary>'P' key.</summary>
  AKEYCODE_P                             =  44; // $2C
  {$EXTERNALSYM AKEYCODE_P}

  /// <summary>'Q' key.</summary>
  AKEYCODE_Q                             =  45; // $2D
  {$EXTERNALSYM AKEYCODE_Q}

  /// <summary>'R' key.</summary>
  AKEYCODE_R                             =  46; // $2E
  {$EXTERNALSYM AKEYCODE_R}

  /// <summary>'S' key.</summary>
  AKEYCODE_S                             =  47; // $2F
  {$EXTERNALSYM AKEYCODE_S}

  /// <summary>'T' key.</summary>
  AKEYCODE_T                             =  48; // $30
  {$EXTERNALSYM AKEYCODE_T}

  /// <summary>'U' key.</summary>
  AKEYCODE_U                             =  49; // $31
  {$EXTERNALSYM AKEYCODE_U}

  /// <summary>'V' key.</summary>
  AKEYCODE_V                             =  50; // $32
  {$EXTERNALSYM AKEYCODE_V}

  /// <summary>'W' key.</summary>
  AKEYCODE_W                             =  51; // $33
  {$EXTERNALSYM AKEYCODE_W}

  /// <summary>'X' key.</summary>
  AKEYCODE_X                             =  52; // $34
  {$EXTERNALSYM AKEYCODE_X}

  /// <summary>'Y' key.</summary>
  AKEYCODE_Y                             =  53; // $35
  {$EXTERNALSYM AKEYCODE_Y}

  /// <summary>'Z' key.</summary>
  AKEYCODE_Z                             =  54; // $36
  {$EXTERNALSYM AKEYCODE_Z}

  /// <summary>',' key.</summary>
  AKEYCODE_COMMA                         =  55; // $37
  {$EXTERNALSYM AKEYCODE_COMMA}

  /// <summary>'.' key.</summary>
  AKEYCODE_PERIOD                        =  56; // $38
  {$EXTERNALSYM AKEYCODE_PERIOD}

  /// <summary>Left Alt modifier key.</summary>
  AKEYCODE_ALT_LEFT                      =  57; // $39
  {$EXTERNALSYM AKEYCODE_ALT_LEFT}

  /// <summary>Right Alt modifier key.</summary>
  AKEYCODE_ALT_RIGHT                     =  58; // $3A
  {$EXTERNALSYM AKEYCODE_ALT_RIGHT}

  /// <summary>Left Shift modifier key.</summary>
  AKEYCODE_SHIFT_LEFT                    =  59; // $3B
  {$EXTERNALSYM AKEYCODE_SHIFT_LEFT}

  /// <summary>Right Shift modifier key.</summary>
  AKEYCODE_SHIFT_RIGHT                   =  60; // $3C
  {$EXTERNALSYM AKEYCODE_SHIFT_RIGHT}

  /// <summary>Tab key.</summary>
  AKEYCODE_TAB                           =  61; // $3D
  {$EXTERNALSYM AKEYCODE_TAB}

  /// <summary>Space key.</summary>
  AKEYCODE_SPACE                         =  62; // $3E
  {$EXTERNALSYM AKEYCODE_SPACE}

  /// <summary>Symbol modifier key.<br />
  /// Used to enter alternate symbols.</summary>
  AKEYCODE_SYM                           =  63; // $3F
  {$EXTERNALSYM AKEYCODE_SYM}

  /// <summary>Explorer special function key.<br />
  /// Used to launch a browser application.</summary>
  AKEYCODE_EXPLORER                      =  64; // $40
  {$EXTERNALSYM AKEYCODE_EXPLORER}

  /// <summary>Envelope special function key.<br />
  /// Used to launch a mail application.</summary>
  AKEYCODE_ENVELOPE                      =  65; // $41
  {$EXTERNALSYM AKEYCODE_ENVELOPE}

  /// <summary>Enter key.</summary>
  AKEYCODE_ENTER                         =  66; // $42
  {$EXTERNALSYM AKEYCODE_ENTER}

  /// <summary>Backspace key.<br />
  /// Deletes characters before the insertion point, unlike
  /// KEYCODE_FORWARD_DEL.</summary>
  AKEYCODE_DEL                           =  67; // $43
  {$EXTERNALSYM AKEYCODE_DEL}

  /// <summary>'`' (backtick) key.</summary>
  AKEYCODE_GRAVE                         =  68; // $44
  {$EXTERNALSYM AKEYCODE_GRAVE}

  /// <summary>'-'.</summary>
  AKEYCODE_MINUS                         =  69; // $45
  {$EXTERNALSYM AKEYCODE_MINUS}

  /// <summary>'=' key.</summary>
  AKEYCODE_EQUALS                        =  70; // $46
  {$EXTERNALSYM AKEYCODE_EQUALS}

  /// <summary>'[' key.</summary>
  AKEYCODE_LEFT_BRACKET                  =  71; // $47
  {$EXTERNALSYM AKEYCODE_LEFT_BRACKET}

  /// <summary>']' key.</summary>
  AKEYCODE_RIGHT_BRACKET                 =  72; // $48
  {$EXTERNALSYM AKEYCODE_RIGHT_BRACKET}

  /// <summary>'\' key.</summary>
  AKEYCODE_BACKSLASH                     =  73; // $49
  {$EXTERNALSYM AKEYCODE_BACKSLASH}

  /// <summary>';' key.</summary>
  AKEYCODE_SEMICOLON                     =  74; // $4A
  {$EXTERNALSYM AKEYCODE_SEMICOLON}

  /// <summary>''' (apostrophe) key.</summary>
  AKEYCODE_APOSTROPHE                    =  75; // $4B
  {$EXTERNALSYM AKEYCODE_APOSTROPHE}

  /// <summary>'/' key.</summary>
  AKEYCODE_SLASH                         =  76; // $4C
  {$EXTERNALSYM AKEYCODE_SLASH}

  /// <summary>'@' key.</summary>
  AKEYCODE_AT                            =  77; // $4D
  {$EXTERNALSYM AKEYCODE_AT}

  /// <summary>Number modifier key.<br />
  /// Used to enter numeric symbols. This key is not Num Lock; it is more like 
  /// KEYCODE_ALT_LEFTand is interpreted as an ALT key by
  /// MetaKeyKeyListener.</summary>
  AKEYCODE_NUM                           =  78; // $4E
  {$EXTERNALSYM AKEYCODE_NUM}

  /// <summary>Headset Hook key.<br />
  /// Used to hang up calls and stop media.</summary>
  AKEYCODE_HEADSETHOOK                   =  79; // $4F
  {$EXTERNALSYM AKEYCODE_HEADSETHOOK}

  /// <summary>Camera Focus key.<br />
  /// Used to focus the camera.</summary>
  AKEYCODE_FOCUS                         =  80; // $50
  {$EXTERNALSYM AKEYCODE_FOCUS}

  /// <summary>'+' key.</summary>
  AKEYCODE_PLUS                          =  81; // $51
  {$EXTERNALSYM AKEYCODE_PLUS}

  /// <summary>Menu key.</summary>
  AKEYCODE_MENU                          =  82; // $52
  {$EXTERNALSYM AKEYCODE_MENU}

  /// <summary>Notification key.</summary>
  AKEYCODE_NOTIFICATION                  =  83; // $53
  {$EXTERNALSYM AKEYCODE_NOTIFICATION}

  /// <summary>Search key.</summary>
  AKEYCODE_SEARCH                        =  84; // $54
  {$EXTERNALSYM AKEYCODE_SEARCH}

  /// <summary>Play/Pause media key.</summary>
  AKEYCODE_MEDIA_PLAY_PAUSE              =  85; // $55
  {$EXTERNALSYM AKEYCODE_MEDIA_PLAY_PAUSE}

  /// <summary>Stop media key.</summary>
  AKEYCODE_MEDIA_STOP                    =  86; // $56
  {$EXTERNALSYM AKEYCODE_MEDIA_STOP}

  /// <summary>Play Next media key.</summary>
  AKEYCODE_MEDIA_NEXT                    =  87; // $57
  {$EXTERNALSYM AKEYCODE_MEDIA_NEXT}

  /// <summary>Play Previous media key.</summary>
  AKEYCODE_MEDIA_PREVIOUS                =  88; // $58
  {$EXTERNALSYM AKEYCODE_MEDIA_PREVIOUS}

  /// <summary>Rewind media key.</summary>
  AKEYCODE_MEDIA_REWIND                  =  89; // $59
  {$EXTERNALSYM AKEYCODE_MEDIA_REWIND}

  /// <summary>Fast Forward media key.</summary>
  AKEYCODE_MEDIA_FAST_FORWARD            =  90; // $5A
  {$EXTERNALSYM AKEYCODE_MEDIA_FAST_FORWARD}

  /// <summary>Mute key.<br />
  /// Mutes the microphone, unlike KEYCODE_VOLUME_MUTE.</summary>
  AKEYCODE_MUTE                          =  91; // $5B
  {$EXTERNALSYM AKEYCODE_MUTE}

  /// <summary>Page Up key.</summary>
  AKEYCODE_PAGE_UP                       =  92; // $5C
  {$EXTERNALSYM AKEYCODE_PAGE_UP}

  /// <summary>Page Down key.</summary>
  AKEYCODE_PAGE_DOWN                     =  93; // $5D
  {$EXTERNALSYM AKEYCODE_PAGE_DOWN}

  /// <summary>Picture Symbols modifier key.<br />
  /// Used to switch symbol sets (Emoji, Kao-moji).</summary>
  AKEYCODE_PICTSYMBOLS                   =  94; // $5E
  {$EXTERNALSYM AKEYCODE_PICTSYMBOLS}

  /// <summary>Switch Charset modifier key.<br />
  /// Used to switch character sets (Kanji, Katakana).</summary>
  AKEYCODE_SWITCH_CHARSET                =  95; // $5F
  {$EXTERNALSYM AKEYCODE_SWITCH_CHARSET}

  /// <summary>A Button key.<br />
  /// On a game controller, the A button should be either the button labeled A
  /// or the first button on the bottom row of controller buttons.</summary>
  AKEYCODE_BUTTON_A                      =  96; // $60
  {$EXTERNALSYM AKEYCODE_BUTTON_A}

  /// <summary>B Button key.<br />
  /// On a game controller, the B button should be either the button labeled B
  /// or the second button on the bottom row of controller buttons.</summary>
  AKEYCODE_BUTTON_B                      =  97; // $61
  {$EXTERNALSYM AKEYCODE_BUTTON_B}

  /// <summary>C Button key.<br />
  /// On a game controller, the C button should be either the button labeled C
  /// or the third button on the bottom row of controller buttons.</summary>
  AKEYCODE_BUTTON_C                      =  98; // $62
  {$EXTERNALSYM AKEYCODE_BUTTON_C}

  /// <summary>X Button key.<br />
  /// On a game controller, the X button should be either the button labeled X
  /// or the first button on the upper row of controller buttons.</summary>
  AKEYCODE_BUTTON_X                      =  99; // $63
  {$EXTERNALSYM AKEYCODE_BUTTON_X}

  /// <summary>Y Button key.<br />
  /// On a game controller, the Y button should be either the button labeled Y
  /// or the second button on the upper row of controller buttons.</summary>
  AKEYCODE_BUTTON_Y                      = 100; // $64
  {$EXTERNALSYM AKEYCODE_BUTTON_Y}

  /// <summary>Z Button key.<br />
  /// On a game controller, the Z button should be either the button labeled Z
  /// or the third button on the upper row of controller buttons.</summary>
  AKEYCODE_BUTTON_Z                      = 101; // $65
  {$EXTERNALSYM AKEYCODE_BUTTON_Z}

  /// <summary>L1 Button key.<br />
  /// On a game controller, the L1 button should be either the button labeled
  /// L1 (or L) or the top left trigger button.</summary>
  AKEYCODE_BUTTON_L1                     = 102; // $66
  {$EXTERNALSYM AKEYCODE_BUTTON_L1}

  /// <summary>R1 Button key.<br />
  /// On a game controller, the R1 button should be either the button labeled
  /// R1 (or R) or the top right trigger button.</summary>
  AKEYCODE_BUTTON_R1                     = 103; // $67
  {$EXTERNALSYM AKEYCODE_BUTTON_R1}

  /// <summary>L2 Button key.<br />
  /// On a game controller, the L2 button should be either the button labeled
  /// L2 or the bottom left trigger button.</summary>
  AKEYCODE_BUTTON_L2                     = 104; // $68
  {$EXTERNALSYM AKEYCODE_BUTTON_L2}

  /// <summary>R2 Button key. On a game controller, the R2 button should be
  /// either the button labeled R2 or the bottom right trigger button.</summary>
  AKEYCODE_BUTTON_R2                     = 105; // $69
  {$EXTERNALSYM AKEYCODE_BUTTON_R2}

  /// <summary>Left Thumb Button key.<br />
  /// On a game controller, the left thumb button indicates that the left (or
  /// only) joystick is pressed.</summary>
  AKEYCODE_BUTTON_THUMBL                 = 106; // $6A
  {$EXTERNALSYM AKEYCODE_BUTTON_THUMBL}

  /// <summary>Right Thumb Button key.<br />
  /// On a game controller, the right thumb button indicates that the right
  /// joystick is pressed.</summary>
  AKEYCODE_BUTTON_THUMBR                 = 107; // $6B
  {$EXTERNALSYM AKEYCODE_BUTTON_THUMBR}

  /// <summary>Start Button key.<br />
  /// On a game controller, the button labeled Start.</summary>
  AKEYCODE_BUTTON_START                  = 108; // $6C
  {$EXTERNALSYM AKEYCODE_BUTTON_START}

  /// <summary>Select Button key.<br />
  /// On a game controller, the button labeled Select.</summary>
  AKEYCODE_BUTTON_SELECT                 = 109; // $6D
  {$EXTERNALSYM AKEYCODE_BUTTON_SELECT}

  /// <summary>Mode Button key.<br />
  /// On a game controller, the button labeled Mode.</summary>
  AKEYCODE_BUTTON_MODE                   = 110; // $6E
  {$EXTERNALSYM AKEYCODE_BUTTON_MODE}

  /// <summary>Escape key.</summary>
  AKEYCODE_ESCAPE                        = 111; // $6F
  {$EXTERNALSYM AKEYCODE_ESCAPE}

  /// <summary>Forward Delete key.<br />
  /// Deletes characters ahead of the insertion point, unlike
  /// KEYCODE_DEL.</summary>
  AKEYCODE_FORWARD_DEL                   = 112; // $70
  {$EXTERNALSYM AKEYCODE_FORWARD_DEL}

  /// <summary>Left Control modifier key.</summary>
  AKEYCODE_CTRL_LEFT                     = 113; // $71
  {$EXTERNALSYM AKEYCODE_CTRL_LEFT}

  /// <summary>Right Control modifier key.</summary>
  AKEYCODE_CTRL_RIGHT                    = 114; // $72
  {$EXTERNALSYM AKEYCODE_CTRL_RIGHT}

  /// <summary>Caps Lock key.</summary>
  AKEYCODE_CAPS_LOCK                     = 115; // $73
  {$EXTERNALSYM AKEYCODE_CAPS_LOCK}

  /// <summary>Scroll Lock key.</summary>
  AKEYCODE_SCROLL_LOCK                   = 116; // $74
  {$EXTERNALSYM AKEYCODE_SCROLL_LOCK}

  /// <summary>Left Meta modifier key.</summary>
  AKEYCODE_META_LEFT                     = 117; // $75
  {$EXTERNALSYM AKEYCODE_META_LEFT}

  /// <summary>Right Meta modifier key.</summary>
  AKEYCODE_META_RIGHT                    = 118; // $76
  {$EXTERNALSYM AKEYCODE_META_RIGHT}

  /// <summary>Function modifier key.</summary>
  AKEYCODE_FUNCTION                      = 119; // $77
  {$EXTERNALSYM AKEYCODE_FUNCTION}

  /// <summary>System Request / Print Screen key.</summary>
  AKEYCODE_SYSRQ                         = 120; // $78
  {$EXTERNALSYM AKEYCODE_SYSRQ}

  /// <summary>Break / Pause key.</summary>
  AKEYCODE_BREAK                         = 121; // $79
  {$EXTERNALSYM AKEYCODE_BREAK}

  /// <summary>Home Movement key.<br />
  /// Used for scrolling or moving the cursor around to the start of a line or
  /// to the top of a list.</summary>
  AKEYCODE_MOVE_HOME                     = 122; // $7A
  {$EXTERNALSYM AKEYCODE_MOVE_HOME}

  /// <summary>End Movement key.<br />
  /// Used for scrolling or moving the cursor around to the end of a line or
  /// to the bottom of a list.</summary>
  AKEYCODE_MOVE_END                      = 123; // $7B
  {$EXTERNALSYM AKEYCODE_MOVE_END}

  /// <summary>Insert key.<br />
  /// Toggles insert / overwrite edit mode.</summary>
  AKEYCODE_INSERT                        = 124; // $7C
  {$EXTERNALSYM AKEYCODE_INSERT}

  /// <summary>Forward key.<br />
  /// Navigates forward in the history stack. Complement of
  /// KEYCODE_BACK.</summary>
  AKEYCODE_FORWARD                       = 125; // $7D
  {$EXTERNALSYM AKEYCODE_FORWARD}

  /// <summary>Play media key.</summary>
  AKEYCODE_MEDIA_PLAY                    = 126; // $7E
  {$EXTERNALSYM AKEYCODE_MEDIA_PLAY}

  /// <summary>Pause media key.</summary>
  AKEYCODE_MEDIA_PAUSE                   = 127; // $7F
  {$EXTERNALSYM AKEYCODE_MEDIA_PAUSE}

  /// <summary>Close media key.<br />
  /// May be used to close a CD tray, for example.</summary>
  AKEYCODE_MEDIA_CLOSE                   = 128; // $80
  {$EXTERNALSYM AKEYCODE_MEDIA_CLOSE}

  /// <summary>Eject media key.<br />
  /// May be used to eject a CD tray, for example.</summary>
  AKEYCODE_MEDIA_EJECT                   = 129; // $81
  {$EXTERNALSYM AKEYCODE_MEDIA_EJECT}

  /// <summary>Record media key.</summary>
  AKEYCODE_MEDIA_RECORD                  = 130; // $82
  {$EXTERNALSYM AKEYCODE_MEDIA_RECORD}

  /// <summary>F1 key.</summary>
  AKEYCODE_F1                            = 131; // $83
  {$EXTERNALSYM AKEYCODE_F1}

  /// <summary>F2 key.</summary>
  AKEYCODE_F2                            = 132; // $84
  {$EXTERNALSYM AKEYCODE_F2}

  /// <summary>F3 key.</summary>
  AKEYCODE_F3                            = 133; // $85
  {$EXTERNALSYM AKEYCODE_F3}

  /// <summary>F4 key.</summary>
  AKEYCODE_F4                            = 134; // $86
  {$EXTERNALSYM AKEYCODE_F4}

  /// <summary>F5 key.</summary>
  AKEYCODE_F5                            = 135; // $87
  {$EXTERNALSYM AKEYCODE_F5}

  /// <summary>F6 key.</summary>
  AKEYCODE_F6                            = 136; // $88
  {$EXTERNALSYM AKEYCODE_F6}

  /// <summary>F7 key.</summary>
  AKEYCODE_F7                            = 137; // $89
  {$EXTERNALSYM AKEYCODE_F7}

  /// <summary>F8 key.</summary>
  AKEYCODE_F8                            = 138; // $8A
  {$EXTERNALSYM AKEYCODE_F8}

  /// <summary>F9 key.</summary>
  AKEYCODE_F9                            = 139; // $8B
  {$EXTERNALSYM AKEYCODE_F9}

  /// <summary>F10 key.</summary>
  AKEYCODE_F10                           = 140; // $8C
  {$EXTERNALSYM AKEYCODE_F10}

  /// <summary>F11 key.</summary>
  AKEYCODE_F11                           = 141; // $8D
  {$EXTERNALSYM AKEYCODE_F11}

  /// <summary>F12 key.</summary>
  AKEYCODE_F12                           = 142; // $8E
  {$EXTERNALSYM AKEYCODE_F12}

  /// <summary>Num Lock key.<br />
  /// This is the Num Lock key; it is different from KEYCODE_NUM. This key
  /// alters the behavior of other keys on the numeric keypad.</summary>
  AKEYCODE_NUM_LOCK                      = 143; // $8F
  {$EXTERNALSYM AKEYCODE_NUM_LOCK}

  /// <summary>Numeric keypad '0' key.</summary>
  AKEYCODE_NUMPAD_0                      = 144; // $90
  {$EXTERNALSYM AKEYCODE_NUMPAD_0}

  /// <summary>Numeric keypad '1' key.</summary>
  AKEYCODE_NUMPAD_1                      = 145; // $91
  {$EXTERNALSYM AKEYCODE_NUMPAD_1}

  /// <summary>Numeric keypad '2' key.</summary>
  AKEYCODE_NUMPAD_2                      = 146; // $92
  {$EXTERNALSYM AKEYCODE_NUMPAD_2}

  /// <summary>Numeric keypad '3' key.</summary>
  AKEYCODE_NUMPAD_3                      = 147; // $93
  {$EXTERNALSYM AKEYCODE_NUMPAD_3}

  /// <summary>Numeric keypad '4' key.</summary>
  AKEYCODE_NUMPAD_4                      = 148; // $94
  {$EXTERNALSYM AKEYCODE_NUMPAD_4}

  /// <summary>Numeric keypad '5' key.</summary>
  AKEYCODE_NUMPAD_5                      = 149; // $95
  {$EXTERNALSYM AKEYCODE_NUMPAD_5}

  /// <summary>Numeric keypad '6' key.</summary>
  AKEYCODE_NUMPAD_6                      = 150; // $96
  {$EXTERNALSYM AKEYCODE_NUMPAD_6}

  /// <summary>Numeric keypad '7' key.</summary>
  AKEYCODE_NUMPAD_7                      = 151; // $97
  {$EXTERNALSYM AKEYCODE_NUMPAD_7}

  /// <summary>Numeric keypad '8' key.</summary>
  AKEYCODE_NUMPAD_8                      = 152; // $98
  {$EXTERNALSYM AKEYCODE_NUMPAD_8}

  /// <summary>Numeric keypad '9' key.</summary>
  AKEYCODE_NUMPAD_9                      = 153; // $99
  {$EXTERNALSYM AKEYCODE_NUMPAD_9}

  /// <summary>Numeric keypad '/' key (for division).</summary>
  AKEYCODE_NUMPAD_DIVIDE                 = 154; // $9A
  {$EXTERNALSYM AKEYCODE_NUMPAD_DIVIDE}

  /// <summary>Numeric keypad '*' key (for multiplication).</summary>
  AKEYCODE_NUMPAD_MULTIPLY               = 155; // $9B
  {$EXTERNALSYM AKEYCODE_NUMPAD_MULTIPLY}

  /// <summary>Numeric keypad '-' key (for subtraction).</summary>
  AKEYCODE_NUMPAD_SUBTRACT               = 156; // $9C
  {$EXTERNALSYM AKEYCODE_NUMPAD_SUBTRACT}

  /// <summary>Numeric keypad '+' key (for addition).</summary>
  AKEYCODE_NUMPAD_ADD                    = 157; // $9D
  {$EXTERNALSYM AKEYCODE_NUMPAD_ADD}

  /// <summary>Numeric keypad '.' key (for decimals or digit grouping).</summary>
  AKEYCODE_NUMPAD_DOT                    = 158; // $9E
  {$EXTERNALSYM AKEYCODE_NUMPAD_DOT}

  /// <summary>Numeric keypad ',' key (for decimals or digit grouping).</summary>
  AKEYCODE_NUMPAD_COMMA                  = 159; // $9F
  {$EXTERNALSYM AKEYCODE_NUMPAD_COMMA}

  /// <summary>Numeric keypad Enter key.</summary>
  AKEYCODE_NUMPAD_ENTER                  = 160; // $A0
  {$EXTERNALSYM AKEYCODE_NUMPAD_ENTER}

  /// <summary>Numeric keypad '=' key.</summary>
  AKEYCODE_NUMPAD_EQUALS                 = 161; // $A1
  {$EXTERNALSYM AKEYCODE_NUMPAD_EQUALS}

  /// <summary>Numeric keypad '(' key.</summary>
  AKEYCODE_NUMPAD_LEFT_PAREN             = 162; // $A2
  {$EXTERNALSYM AKEYCODE_NUMPAD_LEFT_PAREN}

  /// <summary>Numeric keypad ')' key.</summary>
  AKEYCODE_NUMPAD_RIGHT_PAREN            = 163; // $A3
  {$EXTERNALSYM AKEYCODE_NUMPAD_RIGHT_PAREN}

  /// <summary>Volume Mute key.<br />
  /// Mutes the speaker, unlike KEYCODE_MUTE. This key should normally be
  /// implemented as a toggle such that the first press mutes the speaker and the second press restores the original volume.</summary>
  AKEYCODE_VOLUME_MUTE                   = 164; // $A4
  {$EXTERNALSYM AKEYCODE_VOLUME_MUTE}

  /// <summary>Info key.<br />
  /// Common on TV remotes to show additional information related to what is
  /// currently being viewed.</summary>
  AKEYCODE_INFO                          = 165; // $A5
  {$EXTERNALSYM AKEYCODE_INFO}

  /// <summary>Channel up key.<br />
  /// On TV remotes, increments the television channel.</summary>
  AKEYCODE_CHANNEL_UP                    = 166; // $A6
  {$EXTERNALSYM AKEYCODE_CHANNEL_UP}

  /// <summary>Channel down key.<br />
  /// On TV remotes, decrements the television channel.</summary>
  AKEYCODE_CHANNEL_DOWN                  = 167; // $A7
  {$EXTERNALSYM AKEYCODE_CHANNEL_DOWN}

  /// <summary>Zoom in key.</summary>
  AKEYCODE_ZOOM_IN                       = 168; // $A8
  {$EXTERNALSYM AKEYCODE_ZOOM_IN}

  /// <summary>Zoom out key.</summary>
  AKEYCODE_ZOOM_OUT                      = 169; // $A9
  {$EXTERNALSYM AKEYCODE_ZOOM_OUT}

  /// <summary>TV key.<br />
  /// On TV remotes, switches to viewing live TV.</summary>
  AKEYCODE_TV                            = 170; // $AA
  {$EXTERNALSYM AKEYCODE_TV}

  /// <summary>Window key.<br />
  /// On TV remotes, toggles picture-in-picture mode or other windowing
  /// functions.</summary>
  AKEYCODE_WINDOW                        = 171; // $AB
  {$EXTERNALSYM AKEYCODE_WINDOW}

  /// <summary>Guide key.<br />
  /// On TV remotes, shows a programming guide.</summary>
  AKEYCODE_GUIDE                         = 172; // $AC
  {$EXTERNALSYM AKEYCODE_GUIDE}

  /// <summary>DVR key.<br />
  /// On some TV remotes, switches to a DVR mode for recorded shows.</summary>
  AKEYCODE_DVR                           = 173; // $AD
  {$EXTERNALSYM AKEYCODE_DVR}

  /// <summary>Bookmark key.<br />
  /// On some TV remotes, bookmarks content or web pages.</summary>
  AKEYCODE_BOOKMARK                      = 174; // $AE
  {$EXTERNALSYM AKEYCODE_BOOKMARK}

  /// <summary>Toggle captions key.<br />
  /// Switches the mode for closed-captioning text, for example during
  /// television shows.</summary>
  AKEYCODE_CAPTIONS                      = 175; // $AF
  {$EXTERNALSYM AKEYCODE_CAPTIONS}

  /// <summary>Settings key.<br />
  /// Starts the system settings activity.</summary>
  AKEYCODE_SETTINGS                      = 176; // $B0
  {$EXTERNALSYM AKEYCODE_SETTINGS}

  /// <summary>TV power key.<br />
  /// On TV remotes, toggles the power on a television screen.</summary>
  AKEYCODE_TV_POWER                      = 177; // $B1
  {$EXTERNALSYM AKEYCODE_TV_POWER}

  /// <summary>TV input key.<br />
  /// On TV remotes, switches the input on a television screen.</summary>
  AKEYCODE_TV_INPUT                      = 178; // $B2
  {$EXTERNALSYM AKEYCODE_TV_INPUT}

  /// <summary>Set-top-box power key.<br />
  /// On TV remotes, toggles the power on an external Set-top-box.</summary>
  AKEYCODE_STB_POWER                     = 179; // $B3
  {$EXTERNALSYM AKEYCODE_STB_POWER}

  /// <summary>Set-top-box input key.<br />
  /// On TV remotes, switches the input mode on an external
  /// Set-top-box.</summary>
  AKEYCODE_STB_INPUT                     = 180; // $B4
  {$EXTERNALSYM AKEYCODE_STB_INPUT}

  /// <summary>A/V Receiver power key.<br />
  /// On TV remotes, toggles the power on an external A/V Receiver.</summary>
  AKEYCODE_AVR_POWER                     = 181; // $B5
  {$EXTERNALSYM AKEYCODE_AVR_POWER}

  /// <summary>A/V Receiver input key.<br />
  /// On TV remotes, switches the input mode on an external A/V
  /// Receiver.</summary>
  AKEYCODE_AVR_INPUT                     = 182; // $B6
  {$EXTERNALSYM AKEYCODE_AVR_INPUT}

  /// <summary>Red "programmable" key.<br />
  /// On TV remotes, acts as a contextual/programmable key.</summary>
  AKEYCODE_PROG_RED                      = 183; // $B7
  {$EXTERNALSYM AKEYCODE_PROG_RED}

  /// <summary>Green "programmable" key.<br />
  /// On TV remotes, actsas a contextual/programmable key.</summary>
  AKEYCODE_PROG_GREEN                    = 184; // $B8
  {$EXTERNALSYM AKEYCODE_PROG_GREEN}

  /// <summary>Yellow "programmable" key.<br />
  /// On TV remotes, acts as a contextual/programmable key.</summary>
  AKEYCODE_PROG_YELLOW                   = 185; // $B9
  {$EXTERNALSYM AKEYCODE_PROG_YELLOW}

  /// <summary>Blue "programmable" key.<br />
  /// On TV remotes, acts as a contextual/programmable key.</summary>
  AKEYCODE_PROG_BLUE                     = 186; // $BA
  {$EXTERNALSYM AKEYCODE_PROG_BLUE}

  /// <summary>App switch key.<br />
  /// Should bring up the application switcher dialog.</summary>
  AKEYCODE_APP_SWITCH                    = 187; // $BB
  {$EXTERNALSYM AKEYCODE_APP_SWITCH}

  /// <summary>Generic Game Pad Button #1.</summary>
  AKEYCODE_BUTTON_1                      = 188; // $BC
  {$EXTERNALSYM AKEYCODE_BUTTON_1}

  /// <summary>Generic Game Pad Button #2.</summary>
  AKEYCODE_BUTTON_2                      = 189; // $BD
  {$EXTERNALSYM AKEYCODE_BUTTON_2}

  /// <summary>Generic Game Pad Button #3.</summary>
  AKEYCODE_BUTTON_3                      = 190; // $BE
  {$EXTERNALSYM AKEYCODE_BUTTON_3}

  /// <summary>Generic Game Pad Button #4.</summary>
  AKEYCODE_BUTTON_4                      = 191; // $BF
  {$EXTERNALSYM AKEYCODE_BUTTON_4}

  /// <summary>Generic Game Pad Button #5.</summary>
  AKEYCODE_BUTTON_5                      = 192; // $C0
  {$EXTERNALSYM AKEYCODE_BUTTON_5}

  /// <summary>Generic Game Pad Button #6.</summary>
  AKEYCODE_BUTTON_6                      = 193; // $C1
  {$EXTERNALSYM AKEYCODE_BUTTON_6}

  /// <summary>Generic Game Pad Button #7.</summary>
  AKEYCODE_BUTTON_7                      = 194; // $C2
  {$EXTERNALSYM AKEYCODE_BUTTON_7}

  /// <summary>Generic Game Pad Button #8.</summary>
  AKEYCODE_BUTTON_8                      = 195; // $C3
  {$EXTERNALSYM AKEYCODE_BUTTON_8}

  /// <summary>Generic Game Pad Button #9.</summary>
  AKEYCODE_BUTTON_9                      = 196; // $C4
  {$EXTERNALSYM AKEYCODE_BUTTON_9}

  /// <summary>Generic Game Pad Button #10.</summary>
  AKEYCODE_BUTTON_10                     = 197; // $C5
  {$EXTERNALSYM AKEYCODE_BUTTON_10}

  /// <summary>Generic Game Pad Button #11.</summary>
  AKEYCODE_BUTTON_11                     = 198; // $C6
  {$EXTERNALSYM AKEYCODE_BUTTON_11}

  /// <summary>Generic Game Pad Button #12.</summary>
  AKEYCODE_BUTTON_12                     = 199; // $C7
  {$EXTERNALSYM AKEYCODE_BUTTON_12}

  /// <summary>Generic Game Pad Button #13.</summary>
  AKEYCODE_BUTTON_13                     = 200; // $C8
  {$EXTERNALSYM AKEYCODE_BUTTON_13}

  /// <summary>Generic Game Pad Button #14.</summary>
  AKEYCODE_BUTTON_14                     = 201; // $C9
  {$EXTERNALSYM AKEYCODE_BUTTON_14}

  /// <summary>Generic Game Pad Button #15.</summary>
  AKEYCODE_BUTTON_15                     = 202; // $CA
  {$EXTERNALSYM AKEYCODE_BUTTON_15}

  /// <summary>Generic Game Pad Button #16.</summary>
  AKEYCODE_BUTTON_16                     = 203; // $CB
  {$EXTERNALSYM AKEYCODE_BUTTON_16}

  /// <summary>Language Switch key.<br />
  /// Toggles the current input language such as switching between English and
  /// Japanese on a QWERTY keyboard. On some devices, the same function may be
  /// performed by pressing Shift+Spacebar.</summary>
  AKEYCODE_LANGUAGE_SWITCH               = 204; // $CC
  {$EXTERNALSYM AKEYCODE_LANGUAGE_SWITCH}

  /// <summary>Manner Mode key.<br />
  /// Toggles silent or vibrate mode on and off to make the device behave more
  /// politely in certain settings such as on a crowded train. On some devices,
  /// the key may only operate when long-pressed.</summary>
  AKEYCODE_MANNER_MODE                   = 205; // $CD
  {$EXTERNALSYM AKEYCODE_MANNER_MODE}

  /// <summary>3D Mode key.<br />
  /// Toggles the display between 2D and 3D mode.</summary>
  AKEYCODE_3D_MODE                       = 206; // $CE
  {$EXTERNALSYM AKEYCODE_3D_MODE}

  /// <summary>Contacts special function key.<br />
  /// Used to launch an address book application.</summary>
  AKEYCODE_CONTACTS                      = 207; // $CF
  {$EXTERNALSYM AKEYCODE_CONTACTS}

  /// <summary>Calendar special function key.<br />
  /// Used to launch a calendar application.</summary>
  AKEYCODE_CALENDAR                      = 208; // $D0
  {$EXTERNALSYM AKEYCODE_CALENDAR}

  /// <summary>Music special function key.<br />
  /// Used to launch a music player application.</summary>
  AKEYCODE_MUSIC                         = 209; // $D1
  {$EXTERNALSYM AKEYCODE_MUSIC}

  /// <summary>Calculator special function key.<br />
  /// Used to launch a calculator application.</summary>
  AKEYCODE_CALCULATOR                    = 210; // $D2
  {$EXTERNALSYM AKEYCODE_CALCULATOR}

  /// <summary>Japanese full-width / half-width key.</summary>
  AKEYCODE_ZENKAKU_HANKAKU               = 211; // $D3
  {$EXTERNALSYM AKEYCODE_ZENKAKU_HANKAKU}

  /// <summary>Japanese alphanumeric key.</summary>
  AKEYCODE_EISU                          = 212; // $D4
  {$EXTERNALSYM AKEYCODE_EISU}

  /// <summary>Japanese non-conversion key.</summary>
  AKEYCODE_MUHENKAN                      = 213; // $D5
  {$EXTERNALSYM AKEYCODE_MUHENKAN}

  /// <summary>Japanese conversion key.</summary>
  AKEYCODE_HENKAN                        = 214; // $D6
  {$EXTERNALSYM AKEYCODE_HENKAN}

  /// <summary>Japanese katakana / hiragana key.</summary>
  AKEYCODE_KATAKANA_HIRAGANA             = 215; // $D7
  {$EXTERNALSYM AKEYCODE_KATAKANA_HIRAGANA}

  /// <summary>Japanese Yen key.</summary>
  AKEYCODE_YEN                           = 216; // $D8
  {$EXTERNALSYM AKEYCODE_YEN}

  /// <summary>Japanese Ro key.</summary>
  AKEYCODE_RO                            = 217; // $D9
  {$EXTERNALSYM AKEYCODE_RO}

  /// <summary>Japanese kana key.</summary>
  AKEYCODE_KANA                          = 218; // $DA
  {$EXTERNALSYM AKEYCODE_KANA}

  /// <summary>Assist key.<br />
  /// Launches the global assist activity. Not delivered to
  /// applications.</summary>
  AKEYCODE_ASSIST                        = 219; // $DB
  {$EXTERNALSYM AKEYCODE_ASSIST}

  /// <summary>Brightness Down key.<br />
  /// Adjusts the screen brightness down.</summary>
  AKEYCODE_BRIGHTNESS_DOWN               = 220; // $DC
  {$EXTERNALSYM AKEYCODE_BRIGHTNESS_DOWN}

  /// <summary>Brightness Up key.<br />
  /// Adjusts the screen brightness up.</summary>
  AKEYCODE_BRIGHTNESS_UP                 = 221; // $DD
  {$EXTERNALSYM AKEYCODE_BRIGHTNESS_UP}

  /// <summary>Audio Track key.<br />
  /// Switches the audio tracks.</summary>
  AKEYCODE_MEDIA_AUDIO_TRACK             = 222; // $DE
  {$EXTERNALSYM AKEYCODE_MEDIA_AUDIO_TRACK}

  /// <summary>Sleep key.<br />
  /// Puts the device to sleep.  Behaves somewhat like AKEYCODE_POWER but it
  /// has no effect if the device is already asleep.</summary>
  AKEYCODE_SLEEP                         = 223; // $DF
  {$EXTERNALSYM AKEYCODE_SLEEP}

  /// <summary>Wakeup key.<br />
  /// Wakes up the device. Behaves somewhat like AKEYCODE_POWER but it has no
  /// effect if the device is already awake.</summary>
  AKEYCODE_WAKEUP                        = 224; // $E0
  {$EXTERNALSYM AKEYCODE_WAKEUP}

  /// <summary>Pairing key.<br />
  /// Initiates peripheral pairing mode. Useful for pairing remote control
  /// devices or game controllers, especially if no other input mode is
  /// available.</summary>
  AKEYCODE_PAIRING                       = 225; // $E1
  {$EXTERNALSYM AKEYCODE_PAIRING}

  /// <summary>Media Top Menu key.<br />
  /// Goes to the top of media menu.</summary>
  AKEYCODE_MEDIA_TOP_MENU                = 226; // $E2
  {$EXTERNALSYM AKEYCODE_MEDIA_TOP_MENU}

  /// <summary>'11' key.</summary>
  AKEYCODE_11                            = 227; // $E3
  {$EXTERNALSYM AKEYCODE_11}

  /// <summary>'12' key.</summary>
  AKEYCODE_12                            = 228; // $E4
  {$EXTERNALSYM AKEYCODE_12}

  /// <summary>Last Channel key.<br />
  /// Goes to the last viewed channel.</summary>
  AKEYCODE_LAST_CHANNEL                  = 229; // $E5
  {$EXTERNALSYM AKEYCODE_LAST_CHANNEL}

  /// <summary>TV data service key.<br />
  /// Displays data services like weather, sports.</summary>
  AKEYCODE_TV_DATA_SERVICE               = 230; // $E6
  {$EXTERNALSYM AKEYCODE_TV_DATA_SERVICE}

  /// <summary>Voice Assist key.<br />
  /// Launches the global voice assist activity. Not delivered to
  /// applications.</summary>
  AKEYCODE_VOICE_ASSIST                  = 231; // $E7
  {$EXTERNALSYM AKEYCODE_VOICE_ASSIST}

  /// <summary>Radio key.<br />
  /// Toggles TV service / Radio service.</summary>
  AKEYCODE_TV_RADIO_SERVICE              = 232; // $E8
  {$EXTERNALSYM AKEYCODE_TV_RADIO_SERVICE}

  /// <summary>Teletext key.<br />
  /// Displays Teletext service.</summary>
  AKEYCODE_TV_TELETEXT                   = 233; // $E9
  {$EXTERNALSYM AKEYCODE_TV_TELETEXT}

  /// <summary>Number entry key.<br />
  /// Initiates to enter multi-digit channel nubmber when each digit key is
  /// assigned for selecting separate channel. Corresponds to Number Entry Mode
  /// (0x1D) of CEC User Control Code.</summary>
  AKEYCODE_TV_NUMBER_ENTRY               = 234; // $EA
  {$EXTERNALSYM AKEYCODE_TV_NUMBER_ENTRY}

  /// <summary>Analog Terrestrial key.<br />
  /// Switches to analog terrestrial broadcast service.</summary>
  AKEYCODE_TV_TERRESTRIAL_ANALOG         = 235; // $EB
  {$EXTERNALSYM AKEYCODE_TV_TERRESTRIAL_ANALOG}

  /// <summary>Digital Terrestrial key.<br />
  /// Switches to digital terrestrial broadcast service.</summary>
  AKEYCODE_TV_TERRESTRIAL_DIGITAL        = 236; // $EC
  {$EXTERNALSYM AKEYCODE_TV_TERRESTRIAL_DIGITAL}

  /// <summary>Satellite key.<br />
  /// Switches to digital satellite broadcast service.</summary>
  AKEYCODE_TV_SATELLITE                  = 237; // $ED
  {$EXTERNALSYM AKEYCODE_TV_SATELLITE}

  /// <summary>BS key.<br />
  /// Switches to BS digital satellite broadcasting service available in
  /// Japan.</summary>
  AKEYCODE_TV_SATELLITE_BS               = 238; // $EE
  {$EXTERNALSYM AKEYCODE_TV_SATELLITE_BS}

  /// <summary>CS key.<br />
  /// Switches to CS digital satellite broadcasting service available in
  /// Japan.</summary>
  AKEYCODE_TV_SATELLITE_CS               = 239; // $EF
  {$EXTERNALSYM AKEYCODE_TV_SATELLITE_CS}

  /// <summary>BS/CS key.<br />
  /// Toggles between BS and CS digital satellite services.</summary>
  AKEYCODE_TV_SATELLITE_SERVICE          = 240; // $F0
  {$EXTERNALSYM AKEYCODE_TV_SATELLITE_SERVICE}

  /// <summary>Toggle Network key.<br />
  /// Toggles selecting broacast services.</summary>
  AKEYCODE_TV_NETWORK                    = 241; // $F1
  {$EXTERNALSYM AKEYCODE_TV_NETWORK}

  /// <summary>Antenna/Cable key.<br />
  /// Toggles broadcast input source between antenna and cable.</summary>
  AKEYCODE_TV_ANTENNA_CABLE              = 242; // $F2
  {$EXTERNALSYM AKEYCODE_TV_ANTENNA_CABLE}

  /// <summary>HDMI #1 key.<br />
  /// Switches to HDMI input #1.</summary>
  AKEYCODE_TV_INPUT_HDMI_1               = 243; // $F3
  {$EXTERNALSYM AKEYCODE_TV_INPUT_HDMI_1}

  /// <summary>HDMI #2 key.<br />
  /// Switches to HDMI input #2.</summary>
  AKEYCODE_TV_INPUT_HDMI_2               = 244; // $F4
  {$EXTERNALSYM AKEYCODE_TV_INPUT_HDMI_2}

  /// <summary>HDMI #3 key.<br />
  /// Switches to HDMI input #3.</summary>
  AKEYCODE_TV_INPUT_HDMI_3               = 245; // $F5
  {$EXTERNALSYM AKEYCODE_TV_INPUT_HDMI_3}

  /// <summary>HDMI #4 key.<br />
  /// Switches to HDMI input #4.</summary>
  AKEYCODE_TV_INPUT_HDMI_4               = 246; // $F6
  {$EXTERNALSYM AKEYCODE_TV_INPUT_HDMI_4}

  /// <summary>Composite #1 key.<br />
  /// Switches to composite video input #1.</summary>
  AKEYCODE_TV_INPUT_COMPOSITE_1          = 247; // $F7
  {$EXTERNALSYM AKEYCODE_TV_INPUT_COMPOSITE_1}

  /// <summary>Composite #2 key.<br />
  /// Switches to composite video input #2.</summary>
  AKEYCODE_TV_INPUT_COMPOSITE_2          = 248; // $F8
  {$EXTERNALSYM AKEYCODE_TV_INPUT_COMPOSITE_2}

  /// <summary>Component #1 key.<br />
  /// Switches to component video input #1.</summary>
  AKEYCODE_TV_INPUT_COMPONENT_1          = 249; // $F9
  {$EXTERNALSYM AKEYCODE_TV_INPUT_COMPONENT_1}

  /// <summary>Component #2 key.<br />
  /// Switches to component video input #2.</summary>
  AKEYCODE_TV_INPUT_COMPONENT_2          = 250; // $FA
  {$EXTERNALSYM AKEYCODE_TV_INPUT_COMPONENT_2}

  /// <summary>VGA #1 key.<br />
  /// Switches to VGA (analog RGB) input #1.</summary>
  AKEYCODE_TV_INPUT_VGA_1                = 251; // $FB
  {$EXTERNALSYM AKEYCODE_TV_INPUT_VGA_1}

  /// <summary>Audio description key.<br />
  /// Toggles audio description off / on.</summary>
  AKEYCODE_TV_AUDIO_DESCRIPTION          = 252; // $FC
  {$EXTERNALSYM AKEYCODE_TV_AUDIO_DESCRIPTION}

  /// <summary>Audio description mixing volume up key.<br />
  /// Louden audio description volume as compared with normal audio
  /// volume.</summary>
  AKEYCODE_TV_AUDIO_DESCRIPTION_MIX_UP   = 253; // $FD
  {$EXTERNALSYM AKEYCODE_TV_AUDIO_DESCRIPTION_MIX_UP}

  /// <summary>Audio description mixing volume down key.<br />
  /// Lessen audio description volume as compared with normal audio
  /// volume.</summary>
  AKEYCODE_TV_AUDIO_DESCRIPTION_MIX_DOWN = 254; // $FE
  {$EXTERNALSYM AKEYCODE_TV_AUDIO_DESCRIPTION_MIX_DOWN}

  /// <summary>Zoom mode key.<br />
  /// Changes Zoom mode (Normal, Full, Zoom, Wide-zoom, etc.)</summary>
  AKEYCODE_TV_ZOOM_MODE                  = 255; // $FF
  {$EXTERNALSYM AKEYCODE_TV_ZOOM_MODE}

  /// <summary>Contents menu key.<br />
  /// Goes to the title list. Corresponds to Contents Menu (0x0B) of CEC User
  /// Control Code</summary>
  AKEYCODE_TV_CONTENTS_MENU              = 256; // $100
  {$EXTERNALSYM AKEYCODE_TV_CONTENTS_MENU}

  /// <summary>Media context menu key.<br />
  /// Goes to the context menu of media contents. Corresponds to Media
  /// Context-sensitive Menu (0x11) of CEC User Control Code.</summary>
  AKEYCODE_TV_MEDIA_CONTEXT_MENU         = 257; // $101
  {$EXTERNALSYM AKEYCODE_TV_MEDIA_CONTEXT_MENU}

  /// <summary>Timer programming key.<br />
  /// Goes to the timer recording menu. Corresponds to Timer Programming
  /// (0x54) of CEC User Control Code.</summary>
  AKEYCODE_TV_TIMER_PROGRAMMING          = 258; // $102
  {$EXTERNALSYM AKEYCODE_TV_TIMER_PROGRAMMING}

  /// <summary>Help key.</summary>
  AKEYCODE_HELP                          = 259; // $103
  {$EXTERNALSYM AKEYCODE_HELP}

  /// <summary>Help key.</summary>
  AKEYCODE_NAVIGATE_PREVIOUS             = 260; // $104
  {$EXTERNALSYM AKEYCODE_NAVIGATE_PREVIOUS}

  /// <summary>Help key.</summary>
  AKEYCODE_NAVIGATE_NEXT                 = 261; // $105
  {$EXTERNALSYM AKEYCODE_NAVIGATE_NEXT}

  /// <summary>Help key.</summary>
  AKEYCODE_NAVIGATE_IN                   = 262; // $106
  {$EXTERNALSYM AKEYCODE_NAVIGATE_IN}

  /// <summary>Help key.</summary>
  AKEYCODE_NAVIGATE_OUT                  = 263; // $107
  {$EXTERNALSYM AKEYCODE_NAVIGATE_OUT}

  /// <summary>Primary stem key for Wear Main power/reset button on
  /// watch.</summary>
  AKEYCODE_STEM_PRIMARY                  = 264; // $108
  {$EXTERNALSYM AKEYCODE_STEM_PRIMARY}

  /// <summary>Generic stem key 1 for Wear</summary>
  AKEYCODE_STEM_1                        = 265; // $109
  {$EXTERNALSYM AKEYCODE_STEM_1}

  /// <summary>Generic stem key 2 for Wear</summary>
  AKEYCODE_STEM_2                        = 266; // $10A
  {$EXTERNALSYM AKEYCODE_STEM_2}

  /// <summary>Generic stem key 3 for Wear</summary>
  AKEYCODE_STEM_3                        = 267; // $10B
  {$EXTERNALSYM AKEYCODE_STEM_3}

  /// <summary>Directional Pad Up-Left</summary>
  AKEYCODE_DPAD_UP_LEFT                  = 268; // $10C
  {$EXTERNALSYM AKEYCODE_DPAD_UP_LEFT}

  /// <summary>Directional Pad Down-Left</summary>
  AKEYCODE_DPAD_DOWN_LEFT                = 269; // $10D
  {$EXTERNALSYM AKEYCODE_DPAD_DOWN_LEFT}

  /// <summary>Directional Pad Up-Right</summary>
  AKEYCODE_DPAD_UP_RIGHT                 = 270; // $10E
  {$EXTERNALSYM AKEYCODE_DPAD_UP_RIGHT}

  /// <summary>Directional Pad Down-Right</summary>
  AKEYCODE_DPAD_DOWN_RIGHT               = 271; // $10F
  {$EXTERNALSYM AKEYCODE_DPAD_DOWN_RIGHT}

  /// <summary>Skip forward media key</summary>
  AKEYCODE_MEDIA_SKIP_FORWARD            = 272; // $110
  {$EXTERNALSYM AKEYCODE_MEDIA_SKIP_FORWARD}

  /// <summary>Skip backward media key</summary>
  AKEYCODE_MEDIA_SKIP_BACKWARD           = 273; // $111
  {$EXTERNALSYM AKEYCODE_MEDIA_SKIP_BACKWARD}

  /// <summary>Step forward media key.<br />
  /// Steps media forward one from at a time. */</summary>
  AKEYCODE_MEDIA_STEP_FORWARD            = 274; // $112
  {$EXTERNALSYM AKEYCODE_MEDIA_STEP_FORWARD}

  /// <summary>Step backward media key.<br />
  /// Steps media backward one from at a time. */</summary>
  AKEYCODE_MEDIA_STEP_BACKWARD           = 275; // $113
  {$EXTERNALSYM AKEYCODE_MEDIA_STEP_BACKWARD}

  /// <summary>Put device to sleep unless a wakelock is held.</summary>
  AKEYCODE_SOFT_SLEEP                    = 276; // $114
  {$EXTERNALSYM AKEYCODE_SOFT_SLEEP}

  /// <summary>Cut key.</summary>
  AKEYCODE_CUT                           = 277; // $115
  {$EXTERNALSYM AKEYCODE_CUT}

  /// <summary>Copy key.</summary>
  AKEYCODE_COPY                          = 278; // $116
  {$EXTERNALSYM AKEYCODE_COPY}

  /// <summary>Paste key.</summary>
  AKEYCODE_PASTE                         = 279; // $117
  {$EXTERNALSYM AKEYCODE_PASTE}

  /// <summary>fingerprint navigation key, up.</summary>
  AKEYCODE_SYSTEM_NAVIGATION_UP          = 280; // $118
  {$EXTERNALSYM AKEYCODE_SYSTEM_NAVIGATION_UP}

  /// <summary>fingerprint navigation key, down.</summary>
  AKEYCODE_SYSTEM_NAVIGATION_DOWN        = 281; // $119
  {$EXTERNALSYM AKEYCODE_SYSTEM_NAVIGATION_DOWN}

  /// <summary>fingerprint navigation key, left.</summary>
  AKEYCODE_SYSTEM_NAVIGATION_LEFT        = 282; // $11A
  {$EXTERNALSYM AKEYCODE_SYSTEM_NAVIGATION_LEFT}

  /// <summary>fingerprint navigation key, right.</summary>
  AKEYCODE_SYSTEM_NAVIGATION_RIGHT       = 283; // $11B
  {$EXTERNALSYM AKEYCODE_SYSTEM_NAVIGATION_RIGHT}

  /// <summary>all apps</summary>
  AKEYCODE_ALL_APPS                      = 284; // $11C
  {$EXTERNALSYM AKEYCODE_ALL_APPS}

  /// <summary>refresh key</summary>
  AKEYCODE_REFRESH                       = 285; // $11D
  {$EXTERNALSYM AKEYCODE_REFRESH}

implementation

end.

