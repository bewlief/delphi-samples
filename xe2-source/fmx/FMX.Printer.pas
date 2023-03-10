{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.Printer;

interface

uses
{$IFDEF FPC}
  fgl,
{$ELSE}
  System.Generics.Collections,
{$ENDIF}
  System.UITypes, System.Types, System.SysUtils, System.Classes, FMX.Types;

{$SCOPEDENUMS ON}

type

{ TPrinterDevice }

  EPrinter = class(Exception);
  EPrinterSettingsError = class(EPrinter);

  TPrinterDevice = class
  private
    FDevice: string;
    FDriver: string;
    FPort: string;

    function GetActiveDPI: TPoint; inline;
    function GetDPI(Index: Integer): TPoint;
    function GetDPICount: Integer; inline;
    procedure SetActiveDPIIndex(const Value: Integer);
  protected
{$IFNDEF FPC}
    FDPIList: TList<TPoint>; // list of DPI resolutions
{$ENDIF}
    FActiveDPIIndex: Integer;

    procedure DPIChangeError;

    function GetTitle: string; virtual;

    // refreshes the list of pixel-per-inch (DPI) resolutions
    procedure RefreshDPIList; virtual; abstract;
    // notified when the user changed the ActiveDPI property
    procedure ActiveDPIChanged; virtual; abstract;
  public
    constructor Create(const ADriver, ADevice, APort: string);
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
    function Equals(const ADriver, ADevice, APort: string): Boolean; reintroduce; overload;
    procedure ShowDeviceOptions; virtual;

    // selects the DPI that best fits the requested DPI and activates it;
    // returns the index of the selected DPI; you can use these routines to
    // activate the best fit DPI and change the value of ActiveDPIIndex
    function SelectDPI(X, Y: Integer): Integer; overload;
    function SelectDPI(DPI: TPoint): Integer; overload; inline;

    property Device: string read FDevice;
    property Driver: string read FDriver;
    property Port: string read FPort write FPort;
    property Title: string read GetTitle;

    // the DPI that is currently active
    property ActiveDPI: TPoint read GetActiveDPI;
    // the index of the current DPI used by this printer device; you must always
    // set ActiveDPIIndex before using the printer device for printing;
    // a value of -1 will use the last selected DPI; the last selected DPI when
    // starting the application is the default DPI provided by the system; some
    // platform APIs do not work when queried for the default DPI for the printer
    // device, therefore we can use the default DPI through -1 only at the start
    // of the application; the problem in this case is that, for some platforms,
    // the size of the Canvas will not be adapted to correspond to the default
    // DPI because it may not be possible to be queried from the APIs;
    // the general rule and best practice is to always set the DPI before printing;
    // manually setting ActiveDPIIndex to -1 will cause the DPI property to return (0, 0);
    // trying to set a new value to ActiveDPIIndex of the ActiveDevice while printing
    // will cause an error
    property ActiveDPIIndex: Integer read FActiveDPIIndex write SetActiveDPIIndex;
    // supported dots-per-inch for this printer device
    property DPI[Index: Integer]: TPoint read GetDPI;
    // number of supported DPI resolutions
    property DPICount: Integer read GetDPICount;
  end;

{ TPrinter }

{$IFDEF FPC}
  TPrinterDeviceList = TFPGObjectList<TPrinterDevice>;
{$ENDIF}

  TPrinter = class(TAbstractPrinter)
{$IFNDEF FPC}
  private type
    TPrinterDeviceList = TObjectList<TPrinterDevice>;
{$ENDIF}
  protected
    FCanvas: TCanvas;
    FAborted: Boolean;
    FFonts: TStrings;
    FPrinters: TPrinterDeviceList;
    FActivePrinter: Integer;
    FTitle: string;
    FPrinting: Boolean;
    FPageNumber: Integer;

    procedure ActivePrinterChanged; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoAbortDoc; virtual; abstract;
    procedure DoBeginDoc; virtual; abstract;
    procedure DoEndDoc; virtual; abstract;
    procedure DoNewPage; virtual; abstract;
    procedure FreeFonts;
    function GetActivePrinter: TPrinterDevice;
    function GetCanvas: TCanvas; virtual; abstract;
    function GetCapabilities: TPrinterCapabilities; virtual; abstract;
    function GetCount: Integer;
    function GetFonts: TStrings;
    function GetNumCopies: Integer; virtual; abstract;
    function GetOrientation: TPrinterOrientation; virtual; abstract;
    function GetPageHeight: Integer; virtual; abstract;
    function GetPageWidth: Integer; virtual; abstract;
    function GetPrinter(Index: Integer): TPrinterDevice;
    procedure RaiseError(const Msg: string);
    procedure RefreshFonts; virtual; abstract;
    procedure RefreshPrinterDevices; virtual; abstract;
    procedure SetActivePrinter(APrinter: TPrinterDevice);
    procedure SetDefaultPrinter; virtual; abstract;
    procedure SetNumCopies(Value: Integer); virtual;
    procedure SetOrientation(Value: TPrinterOrientation); virtual;
    procedure CheckPrinting(Printing: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Abort;
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;
    procedure Refresh;
    property Aborted: Boolean read FAborted;
    property ActivePrinter: TPrinterDevice read GetActivePrinter write SetActivePrinter;
    property Canvas: TCanvas read GetCanvas;
    property Capabilities: TPrinterCapabilities read GetCapabilities;
    property Copies: Integer read GetNumCopies write SetNumCopies;
    property Count: Integer read GetCount;
    property Fonts: TStrings read GetFonts;
    property Orientation: TPrinterOrientation read GetOrientation write SetOrientation;
    property PageHeight: Integer read GetPageHeight;
    property PageNumber: Integer read FPageNumber;
    property PageWidth: Integer read GetPageWidth;
    property Printing: Boolean read FPrinting;
    property Printers[Index: Integer]: TPrinterDevice read GetPrinter;
    property Title: string read FTitle write FTitle;
  end;

  TPrinterClass = class of TPrinter;

function PrinterClass: TPrinterClass;
function Printer: TPrinter;
function SetPrinter(NewPrinter: TPrinter): TPrinter;

{ AssignPrn - Assigns a Text variable to the currently selected printer.  Any
  Write or Writeln's going to that file variable will be written on the
  printer using the Canvas property's font.  A new page is automatically
  started if a CR is encountered on (or a Writeln is written to) the last
  line on the page.  Closing the text file will imply a call to the
  Printer.EndDoc method. Note: only one Text variable can be open on the
  printer at a time.  Opening a second will cause an exception. }

procedure AssignPrn(var F: Text);

implementation

uses
{$IFDEF MSWINDOWS}
  System.Character, FMX.Consts, FMX.Printer.Win, FMX.Canvas.GDIP;
{$ENDIF}

{$IFDEF MACOS}
  System.Character, FMX.Consts, FMX.Printer.Mac;
{$ENDIF}

{$IFDEF FPC}
  FMX.Consts;
{$ENDIF}

function PrinterClass: TPrinterClass;
begin
{$IFNDEF FPC}
  Result := ActualPrinterClass;
{$ELSE}
  Result := nil;
{$ENDIF}
end;

var
  GPrinter: TPrinter;

function Printer : TPrinter;
begin
{$IFNDEF FPC}
  if (GPrinter = nil) and (ActualPrinterClass <> nil) then
    GPrinter := ActualPrinterClass.Create;
{$ENDIF}
  Result := GPrinter;
end;

function SetPrinter(NewPrinter: TPrinter): TPrinter;
begin
  Result := GPrinter;
  GPrinter := NewPrinter;
end;

{ TPrinter }

constructor TPrinter.Create;
begin
  inherited Create;
  FPrinters := TPrinterDeviceList.Create(True);
  FActivePrinter := -1;
end;

destructor TPrinter.Destroy;
begin
  if Printing then
    DoEndDoc;
  FPrinters.Free;
  FFonts.Free;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TPrinter.Abort;
begin
  CheckPrinting(True);
  DoAbortDoc;
  FAborted := True;
  EndDoc;
  FAborted := True;
end;

procedure TPrinter.AssignTo(Dest: TPersistent);
var
  LDest: TStrings;
  I: Integer;
  Dev: TPrinterDevice;
begin
  inherited AssignTo(Dest);
  if Dest is TStrings then
  begin
    LDest := TStrings(Dest);
    LDest.Clear;
    for I := 0 to Count - 1 do
    begin
      Dev := Printers[I];
      LDest.AddObject(Dev.Title, Dev);
    end;
  end;
end;

procedure TPrinter.BeginDoc;
begin
  CheckPrinting(False);
  DoBeginDoc;
  FPrinting := True;
  FAborted := False;
  FPageNumber := 1;
end;

procedure TPrinter.CheckPrinting(Printing: Boolean);
begin
  if Self.Printing <> Printing then
    if Printing then
      RaiseError(SNotPrinting)
    else
      RaiseError(SPrinting);
end;

procedure TPrinter.EndDoc;
begin
  CheckPrinting(True);
  DoEndDoc;
  FPrinting := False;
  FAborted := False;
  FPageNumber := 0;
end;

procedure TPrinter.FreeFonts;
begin
  FreeAndNil(FFonts);
end;

function TPrinter.GetActivePrinter: TPrinterDevice;
begin
  if FActivePrinter = -1 then
  begin
    if FPrinters.Count = 0 then
    begin
      RefreshPrinterDevices;
      SetDefaultPrinter;
    end;
  end;
  Result := GetPrinter(FActivePrinter);
end;

function TPrinter.GetCount: Integer;
begin
  if FPrinters.Count = 0 then
  begin
    RefreshPrinterDevices;
    // Only call SetDefaultPrinter if there are printers instaled.
    if FPrinters.Count <> 0 then
    begin
      SetDefaultPrinter;
    end;
  end;
  Result := FPrinters.Count;
end;

function TPrinter.GetFonts: TStrings;
begin
  if FFonts = nil then
  try
    FFonts := TStringList.Create;
    RefreshFonts;
  except
    FreeAndNil(FFonts);
    raise;
  end;
  Result := FFonts;
end;

function TPrinter.GetPrinter(Index: Integer): TPrinterDevice;
begin
  if Index < 0 then
    RaiseError(SPrinterIndexError);
  if FPrinters.Count = 0 then
  begin
    RefreshPrinterDevices;
    SetDefaultPrinter;
  end;
  if Index >= FPrinters.Count then
    RaiseError(SPrinterIndexError);
  Result := FPrinters[Index];
end;

procedure TPrinter.NewPage;
begin
  CheckPrinting(True);
  DoNewPage;
  Inc(FPageNumber);
end;

procedure TPrinter.RaiseError(const Msg: string);
begin
  raise EPrinter.Create(Msg);
end;

procedure TPrinter.Refresh;
begin
  FreeFonts;
  FPrinters.Clear;
end;

procedure TPrinter.SetActivePrinter(APrinter: TPrinterDevice);
var
  NewIndex: Integer;
  Changed: Boolean;
begin
  CheckPrinting(False);
  if (FPrinters <> nil) and (APrinter <> nil) then
  begin
    NewIndex := FPrinters.IndexOf(APrinter);
    if NewIndex > -1 then
    begin
      Changed := FActivePrinter <> NewIndex;
      FActivePrinter := NewIndex;
      if Changed then
        ActivePrinterChanged;
    end else
      RaiseError(sInvalidPrinter);
  end;
end;

procedure TPrinter.SetNumCopies(Value: Integer);
begin
  CheckPrinting(False);
end;

procedure TPrinter.SetOrientation(Value: TPrinterOrientation);
begin
  CheckPrinting(False);
end;

{ TPrinterDevice }

constructor TPrinterDevice.Create(const ADriver, ADevice, APort: string);
begin
  inherited Create;
  FDriver := ADriver;
  FDevice := ADevice;
  FPort := APort;
  FActiveDPIIndex := -1;
{$IFNDEF FPC}
  FDPIList := TList<TPoint>.Create;
{$ENDIF}
  RefreshDPIList;
end;

function TPrinterDevice.Equals(Obj: TObject): Boolean;
begin
  Result := (Obj is TPrinterDevice) and SameText(TPrinterDevice(Obj).Device, Device) and
    SameText(TPrinterDevice(Obj).Driver, Driver) and SameText(TPrinterDevice(Obj).Port, Port);
end;

destructor TPrinterDevice.Destroy;
begin
{$IFNDEF FPC}
  FDPIList.Free;
{$ENDIF}
  inherited;
end;

procedure TPrinterDevice.DPIChangeError;
begin
  raise EPrinterSettingsError.Create(SPrinterDPIChangeError);
end;

function TPrinterDevice.Equals(const ADriver, ADevice, APort: string): Boolean;
begin
  Result := (Device = ADevice) and ((Port = '') or (Port = APort));
end;

function TPrinterDevice.GetActiveDPI: TPoint;
begin
{$IFNDEF FPC}
  Result := DPI[ActiveDPIIndex];
{$ELSE}
  Result.X := 0;
  Result.Y := 0;
{$ENDIF}
end;

function TPrinterDevice.GetDPI(Index: Integer): TPoint;
begin
{$IFNDEF FPC}
  if Index = -1 then
    Result := TPoint.Create(0, 0)
  else
    Result := FDPIList[Index];
{$ELSE}
  Result.X := 0;
  Result.Y := 0;
{$ENDIF}
end;

function TPrinterDevice.GetDPICount: Integer;
begin
{$IFNDEF FPC}
  Result := FDPIList.Count;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function TPrinterDevice.GetTitle: string;
begin
  Result := Format(sDeviceOnPort, [FDevice, FPort]);
end;

function TPrinterDevice.SelectDPI(X, Y: Integer): Integer;
var
  Area: Integer;
  MinArea: Integer;
  MinIndex: Integer;
  i: Integer;
begin
  MinArea := MaxInt;
  MinIndex := -1;
  // calculate the DPI rectangle that covers most of the requeste DPI rectangle
  for i := 0 to DPICount - 1 do
  begin
    Area := Abs(X - DPI[i].X) * Abs(Y - DPI[i].Y);
    if MinArea > Area then
    begin
      MinArea := Area;
      MinIndex := i;
    end;
  end;

  ActiveDPIIndex := MinIndex;
  Result := MinIndex;
end;

function TPrinterDevice.SelectDPI(DPI: TPoint): Integer;
begin
  Result := SelectDPI(DPI.X, DPI.Y);
end;

procedure TPrinterDevice.SetActiveDPIIndex(const Value: Integer);
begin
  if FActiveDPIIndex <> Value then
  begin
    // can't set new DPI values while printing
    if (Printer.Printing) and (Self = Printer.ActivePrinter) then
      DPIChangeError;

    FActiveDPIIndex := Value;
    if FActiveDPIIndex <> -1 then
      ActiveDPIChanged;
  end;
end;

procedure TPrinterDevice.ShowDeviceOptions;
begin
  // Descendants can display the options dialog for printer device
end;

{ AssignPrn support }

type
  PrnRec = record
    case Integer of
      1:
        (Cur: TPointF;
         Finish: TPointF; { End of the printable area }
         Height: Single;  { Height of the current line }
         CodePage: Word); { CodePage to use on incoming data }
      2:
        (Tmp: array [1..32] of AnsiChar
        );
  end;

procedure NewPage(var Prn: PrnRec);
begin
  with Prn do
  begin
    Cur.X := 0;
    Cur.Y := 0;
    Printer.NewPage;
  end;
end;

{ Start a new line on the current page, if no more lines left start a new
  page. }
procedure NewLine(var Prn: PrnRec);

  function CharHeight: Word;
  begin
    Result := Trunc(Printer.Canvas.TextHeight('Ij')); // do not localize;
  end;

begin
  with Prn do
  begin
    Cur.X := 0;
    if Height = 0 then
      Cur.Y := Cur.Y + CharHeight
    else
      Cur.Y := Cur.Y + Height;
    if Cur.Y > (Finish.Y - (Height * 2)) then
      NewPage(Prn);
    Height := 0;
  end;
end;

{ Print a string to the printer without regard to special characters.  These
  should handled by the caller. }

procedure PrnOutStr(var Prn: PrnRec; Text: PAnsiChar; Len: Integer);
var
  R: TRectF;
  Extent: TSizeF;
  L: Integer;
  Path: TPathData;
  S: string;
  LText: PChar;

{$IFNDEF FPC}
  function CharPrev(Start, Current: PChar): PChar;
  begin
    if Current <> Start then
    begin
      if TCharacter.IsSurrogate(Current^) then
        Result := Current - 2
      else
        Result := Current - 1;
    end else
      Result := Start;
  end;
{$ENDIF}

  procedure TextExtent(Text: PChar; Len: Integer; var R: TRectF);
  var
    S: string;
  begin
    SetString(S, Text, Len);
    Printer.Canvas.MeasureText(R, S, False, [], TTextAlign.taLeading);
  end;

begin
{$IFNDEF FPC}
  with Prn, Printer.Canvas do
  begin
    L := UnicodeFromLocaleChars(CodePage, 0, Text, Len, nil, 0);
    SetLength(S, L);
    Len := UnicodeFromLocaleChars(CodePage, 0, Text, Len, PChar(S), Length(S));
    LText := PChar(S);
    while Len > 0 do
    begin
      L := Len;
      R := TRectF.Empty;
      TextExtent(LText, L, R);
    //Keep the width and the height calculated by TextExtent, but place the rectangle
    //where we need it to be (modify the starting point).	
      R.SetLocation(Cur.X, Cur.Y);
      Extent := R.Size;

      while (L > 0) and (Extent.cX + Cur.X > Finish.X) do
      begin
        L := CharPrev(LText, LText + L) - LText;
        TextExtent(LText, L, R);
        Extent := R.Size;
      end;

      if Extent.cY > Prn.Height then
        Prn.Height := Extent.cY + 2;
      SetString(S, LText, L);
      Printer.Canvas.FillText(R, S, False, 1, [], TTextAlign.taLeading);
      Dec(Len, L);
      Inc(LText, L);
      if Len > 0 then
        NewLine(Prn)
      else
        Cur.X := Cur.X + Extent.cX;
    end;
  end;
{$ENDIF}
end;

{ Print a string to the printer handling special characters. }
procedure PrnString(var Prn: PrnRec; Text: PAnsiChar; Len: Integer);
var
  L: Integer;
  TabWidth: Single;

  procedure Flush;
  begin
    if L <> 0 then
      PrnOutStr(Prn, Text, L);
    Inc(Text, L + 1);
    Dec(Len, L + 1);
    L := 0;
  end;

  function AvgCharWidth: Single;
  begin
    Result := Printer.Canvas.TextWidth('ABCDEFGHIJKLMNOPQRSTUVWXYZ') / 26; // do not localize
  end;

begin
  L := 0;
  with Prn do
  begin
    while L < Len do
    begin
      case Text[L] of
        #9:
          begin
            Flush;
            TabWidth := AvgCharWidth * 8;
            Cur.X := Cur.X + TabWidth - ((Round(Cur.X + TabWidth + 1) mod Round(TabWidth)) + 1);
            if Cur.X > Finish.X then
              NewLine(Prn);
          end;
        #13:
          Flush;
        #10:
          begin
            Flush;
            NewLine(Prn);
          end;
        ^L:
          begin
            Flush;
            NewPage(Prn);
          end;
      else
        Inc(L);
      end;
    end;
  end;
  Flush;
end;

{ Called when a Read or Readln is applied to a printer file. Since reading is
  illegal this routine tells the I/O system that no characters where read, which
  generates a runtime error. }
function PrnInput(var F: TTextRec): Integer;
begin
  with F do
  begin
    BufPos := 0;
    BufEnd := 0;
  end;
  Result := 0;
end;

{ Called when a Write or Writeln is applied to a printer file. The calls
  PrnString to write the text in the buffer to the printer. }
function PrnOutput(var F: TTextRec): Integer;
begin
  with F do
  begin
    PrnString(PrnRec(UserData), PAnsiChar(BufPtr), BufPos);
    BufPos := 0;
    Result := 0;
  end;
end;

{ Will ignore certain requests by the I/O system such as flush while doing an
  input. }
function PrnIgnore(var F: TTextRec): Integer;
begin
  Result := 0;
end;

{ Deallocates the resources allocated to the printer file. }
function PrnClose(var F: TTextRec): Integer;
begin
  with PrnRec(F.UserData) do
  begin
    Printer.EndDoc;
    Result := 0;
  end;
end;

{ Called to open I/O on a printer file.  Sets up the TTextFile to point to
  printer I/O functions. }
function PrnOpen(var F: TTextRec): Integer;
begin
  with F, PrnRec(UserData) do
  begin
    if Mode = fmInput then
    begin
      InOutFunc := @PrnInput;
      FlushFunc := @PrnIgnore;
      CloseFunc := @PrnIgnore;
    end
    else
    begin
      Mode := fmOutput;
      InOutFunc := @PrnOutput;
      FlushFunc := @PrnOutput;
      CloseFunc := @PrnClose;
      Printer.BeginDoc;

      Printer.Canvas.Font.Family := 'Courier New';
      Printer.Canvas.Fill.Kind := TBrushKind.bkSolid;
      Printer.Canvas.Fill.Color := claBlack;

      Cur.X := 0;
      Cur.Y := 0;
      Finish.X := Printer.PageWidth;
      Finish.Y := Printer.PageHeight;
      Height := 0;
{$IFNDEF FPC}
      PrnRec(UserData).CodePage := F.CodePage;
{$ENDIF}
    end;
    Result := 0;
  end;
end;

procedure AssignPrn(var F: Text);
begin
  with TTextRec(F), PrnRec(UserData) do
  begin
    Printer;
    FillChar(F, sizeof(F), 0);
    Mode := fmClosed;
    BufSize := sizeof(Buffer);
    BufPtr := @Buffer;
    OpenFunc := @PrnOpen;
  end;
end;

initialization

finalization
  Printer.Free;

end.
