
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MV.TgaBitmap;

{$POINTERMATH ON}

interface

uses
  System.SysUtils, System.Classes, FMX.Types;

type
  TTgaBitmap = class(TBitmap)
  private
    procedure LoadTargaFile(const AFileName: String);
  public
    constructor CreateFromFile(const AFileName: String); override;
  end;

implementation

type
  TTargaHeader = packed record
    IDLength      : UInt8;
    ColorMapType  : UInt8;
    ImageType     : UInt8;
    ColorMapOff   : UInt16;
    ColorMapLength: UInt16;
    ColorEntrySize: UInt8;
    XOrg          : Int16;
    YOrg          : Int16;
    Width         : Int16;
    Height        : Int16;
    PixelSize     : UInt8;
    Desc          : UInt8;
  end;

{ TTgaBitmap }

constructor TTgaBitmap.CreateFromFile(const AFileName: String);
begin
  if SameText(ExtractFileExt(AFileName), '.TGA') then
  begin
    Create(0, 0);
    LoadTargaFile(AFileName);
  end
  else
    raise EFilerError.Create('Unsupported targa file format.');
end;

procedure TTgaBitmap.LoadTargaFile(const AFileName: String);
var
  f: TFileStream;
  LHeader: TTargaHeader;
  LBuffer, LSource, LDest: ^Int32;
  LSize, i, j: Integer;
begin
  try
    f := TFileStream.Create(AFileName, fmOpenRead);

    f.ReadBuffer(LHeader, SizeOf(LHeader));
    f.Seek(LHeader.IDLength, soFromCurrent);

    // only supported uncompressed ARGB
    if (LHeader.ImageType <> 2) or (LHeader.PixelSize <> 32) then
      raise EFilerError.Create('Unsupported targa file format.');

    LSize := LHeader.Width * LHeader.Height * 4;
    SetSize(LHeader.Width, LHeader.Height);
    GetMem(LBuffer, LSize);

    f.ReadBuffer(LBuffer^, LSize);

    LDest := Pointer(StartLine);

    for i := LHeader.Height - 1 downto 0 do
    begin
      LSource := @LBuffer[(i * (LHeader.Width))];
      Move(LSource^, LDest^, LHeader.Width * 4);
      Inc(LDest, LHeader.Width);
    end;

    FreeMem(LBuffer);
  finally
    f.Free;
  end;
end;

end.
