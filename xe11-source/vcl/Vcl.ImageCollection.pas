{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 2018-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Vcl.ImageCollection;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, System.Messaging,
  Vcl.Graphics, Vcl.Controls, Winapi.Wincodec,
  Vcl.BaseImageCollection;

type
  TImageCollection = class;
  TImageCollectionItem = class;

  /// <summary>
  /// Item to store one image.
  /// </summary>
  TImageCollectionSourceItem = class(TCollectionItem)
  private
    FImage: TWICImage;
    procedure SetImage(Value: TWICImage);
    function GetImageCollectionItem: TImageCollectionItem;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Update;
  published
   /// <summary>
   /// TWICImage property to store image in native format.
   /// </summary>
    property Image: TWICImage
      read FImage write SetImage;
  end;

  /// <summary>
  /// Collection to store images with different sizes for one item from TImageCollection.
  /// </summary>
  TImageCollectionItemSources = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TImageCollectionSourceItem;
    procedure SetItem(Index: Integer; Value: TImageCollectionSourceItem);
  public
    function Add: TImageCollectionSourceItem;
    property Items[Index: Integer]: TImageCollectionSourceItem read GetItem write SetItem; default;
  end;

  /// <summary>
  /// Item for TImageCollection, which has name and list of source images.
  /// </summary>
  TImageCollectionItem = class(TCollectionItem)
  private
    FName: String;
    FSourceImages: TImageCollectionItemSources;
    FData: TCustomData;
    FDescription: String;
    procedure SetName(const Value: String);
    procedure SetDescription(const Value: String);
    procedure SetSourceImages(const Value: TImageCollectionItemSources);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    /// <summary>
    /// Use CheckSources to sort images by size after you loading them.
    /// Function return True if it reordered items in SourceImages property.
    /// </summary>
    function CheckSources: Boolean;
    /// <summary>
    /// Property to link some abstract data for item.
    /// </summary>
    property Data: TCustomData read FData write FData;
    /// <summary>
    /// Call Change method from specific item to process messaging that item was changed.
    /// CheckSources method is called in this method automatically.
    /// </summary>
    procedure Change;
  published
    /// <summary>
    /// Item name, which can includes Category.
    /// Category name placed at the beginning of the name and separated by the symbol "\".
    /// </summary>
    property Name: String read FName write SetName;
    /// <summary>
    /// Item description.
    /// </summary>
    property Description: String read FDescription write SetDescription;
    /// <summary>
    /// Collection of source images.
    /// TImageCollection chooses one source image for optimal scaling.
    /// </summary>
    property SourceImages: TImageCollectionItemSources
      read FSourceImages write SetSourceImages;
  end;

  /// <summary>
  /// Collection of items for TImageCollection
  /// </summary>
  TImageCollectionItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TImageCollectionItem;
    procedure SetItem(Index: Integer; Value: TImageCollectionItem);
  public
    function Add: TImageCollectionItem;
    property Items[Index: Integer]: TImageCollectionItem read GetItem write SetItem; default;
  end;

  /// <summary>
  /// Interpolation modes for TImageCollection.
  /// </summary>
  TImageCollectionInterpolationMode = (icIMModeHighQualityCubic, icIMFant,
    icIMLinear, icIMCubic, icIMModeNearestNeighbor);

  /// <summary>
  /// Event type to create create TBitmap from TWICImage with custom algorithm.
  /// </summary>
  TImageCollectionOnGetBitmapEvent = procedure(ASourceImage: TWICImage;
    AWidth, AHeight: Integer; out ABitmap: TBitmap) of object;

  /// <summary>
  /// Event type to draw image from collection with custom code.
  /// </summary>
  TImageCollectionOnDrawEvent = procedure(ASourceImage: TWICImage;
    ACanvas: TCanvas; ARect: TRect; AProportional: Boolean = False) of object;

  /// <summary>
  /// Component to store, scale and draw images.
  /// </summary>
  TImageCollection = class(TCustomImageCollection)
  private
    FImages: TImageCollectionItems;
    FInterpolationMode: TImageCollectionInterpolationMode;
    FOnGetBitmap: TImageCollectionOnGetBitmapEvent;
    FOnDraw: TImageCollectionOnDrawEvent;
    procedure SetImages(Value: TImageCollectionItems);
    procedure SetInterpolationMode(Value: TImageCollectionInterpolationMode);
  protected
    function CreateSourceItem(AName: String): TImageCollectionSourceItem;
    function GetCount: Integer; override;
    function GetSourceImageIndex(AIndex: Integer; AWidth, AHeight: Integer): Integer;
    function GetSourceImageByIndex(AIndex, ASourceIndex: Integer): TWICImage;
    procedure DoDraw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean);
    function GetScaledImage(ASourceImage: TWICImage; ANewWidth, ANewHeight: Integer): TWICImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsIndexAvailable(AIndex: Integer): Boolean; override;
    function GetIndexByName(const AName: String): Integer; override;
    function GetNameByIndex(AIndex: Integer): String; override;
    function GetDescriptionByIndex(AIndex: Integer): String; override;
    /// <summary>
    /// Get source TWICImage, which optimal for scaling to AWdith and AHeight sizes.
    /// </summary>
    function GetSourceImage(AIndex: Integer; AWidth, AHeight: Integer): TWICImage;
    /// <summary>
    /// Get scaled to specific size TBitmap from item with specific index.
    /// </summary>
    function GetBitmap(AIndex: Integer; AWidth, AHeight: Integer): TBitmap; overload; override;
    /// <summary>
    /// Get scaled to specific size TBitmap from item with specific name.
    /// </summary>
    function GetBitmap(const AName: String; AWidth, AHeight: Integer; AEnabled: Boolean = True): TBitmap; overload;
    /// <summary>
    /// Directly draw specific image from Item.SourceImages collection.
    /// </summary>
    procedure DrawSource(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; ASourceIndex: Integer; AProportional: Boolean = False);
    /// <summary>
    /// Draw image from collection item with specific index to specific rect and proportional parameter.
    /// </summary>
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean = False); overload; override;
    /// <summary>
    /// Draw image from collection item with specific name to specific rect and proportional parameter.
    /// </summary>
    procedure Draw(ACanvas: TCanvas; ARect: TRect; const AName: String; AProportional: Boolean = False); overload;
    /// <summary>
    /// Add new item with specified name if it does not exist in collection. Then add new source image loaded from the specified file.
    /// </summary>
    procedure Add(AName: String; const AFileName: String); overload;
    /// <summary>
    /// Add new item with specified name if it does not exist in collection. Then add new source image loaded from the stream.
    /// </summary>
    procedure Add(AName: String; Stream: TStream); overload;
    /// <summary>
    /// Add new item with specified name if it does not exist in collection. Then add new source image loaded from the resource name.
    /// </summary>
    procedure Add(AName: String; AInstance: THandle; const AResourceName: String); overload;
    /// <summary>
    /// Add new item with specified name if it does not exist in collection. Then add new source image loaded from the resource names, which defined by array of suffixes.
    /// </summary>
    procedure Add(AName: String; AInstance: THandle; const AResourceName: String; ASuffixes: array of string); overload;
    /// <summary>
    /// Delete item with specific index from Images property.
    /// </summary>
    procedure Delete(AIndex: Integer); overload;
    /// <summary>
    /// Delete item with specific name from Images property.
    /// </summary>
    procedure Delete(const AName: String); overload;
     /// <summary>
    /// Delete items from specific category using start and end item indexes.
    /// </summary>
    procedure Delete(const ACategory: String; AStartIndex, AEndIndex: Integer); overload;
  published
    /// <summary>
    /// Collection of items with source images.
    /// </summary>
    property Images: TImageCollectionItems read FImages write SetImages;
    /// <summary>
    /// Interpolation mode, which will be used to scale images.
    /// </summary>
    property InterpolationMode: TImageCollectionInterpolationMode
      read FInterpolationMode write SetInterpolationMode default icIMModeHighQualityCubic;
    /// <summary>
    /// Use OnDraw event with your code to draw image.
    /// </summary>
    property OnDraw: TImageCollectionOnDrawEvent
      read FOnDraw write FOnDraw;
    /// <summary>
    /// Use OnGetBitmap event with your code to get TBitmap from item.
    /// </summary>
    property OnGetBitmap: TImageCollectionOnGetBitmapEvent
      read FOnGetBitmap write FOnGetBitmap;
  end;

implementation

uses
  System.Math, System.SysUtils, Vcl.Themes;

const
  IMode: array[TImageCollectionInterpolationMode] of TWICImageInterpolationMode =
    (wipmHighQualityCubic, wipmFant, wipmLinear, wipmCubic, wipmNearestNeighbor);


function UpdateRectForProportionalSize(ARect: TRect; AWidth, AHeight: Integer; AStretch: Boolean): TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  Result := ARect;
  if AWidth * AHeight = 0 then
    Exit;

  w := AWidth;
  h := AHeight;
  cw := ARect.Width;
  ch := ARect.Height;

  if AStretch or ((w > cw) or (h > ch)) then
  begin
    xyaspect := w / h;
    if w > h then
    begin
      w := cw;
      h := Trunc(cw / xyaspect);
      if h > ch then
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
      end;
     end
     else
     begin
       h := ch;
       w := Trunc(ch * xyaspect);
       if w > cw then
       begin
         w := cw;
         h := Trunc(cw / xyaspect);
       end;
     end;
  end;

  Result := Rect(0, 0, w, h);
  OffsetRect(Result, ARect.Left + (cw - w) div 2, ARect.Top + (ch - h) div 2);
end;

constructor TImageCollectionSourceItem.Create(Collection: TCollection);
begin
  inherited;
  FImage := TWICImage.Create;
end;

destructor TImageCollectionSourceItem.Destroy;
begin
  FImage.Free;
  inherited;
end;

procedure TImageCollectionSourceItem.Assign(Source: TPersistent);
begin
  if Source is TImageCollectionSourceItem then
  begin
    Image := TImageCollectionSourceItem(Source).Image;
  end
  else
    inherited;
end;

function TImageCollectionSourceItem.GetImageCollectionItem: TImageCollectionItem;
begin
  if (Collection <> nil) and (Collection.Owner <> nil) and (Collection.Owner is TImageCollectionItem) then
    Result := TImageCollectionItem(Collection.Owner)
  else
    Result := nil;
end;

procedure TImageCollectionSourceItem.Update;
var
  LItem: TImageCollectionItem;
begin
  LItem := GetImageCollectionItem;
  if LItem <> nil then
    LItem.CheckSources;
end;

procedure TImageCollectionSourceItem.SetImage(Value: TWICImage);
begin
  if FImage <> Value then
    FImage.Assign(Value);
end;

function TImageCollectionItemSources.GetItem(Index: Integer): TImageCollectionSourceItem;
begin
  Result := TImageCollectionSourceItem(inherited GetItem(Index));
end;

procedure TImageCollectionItemSources.SetItem(Index: Integer; Value: TImageCollectionSourceItem);
begin
  inherited SetItem(Index, Value);
end;

function TImageCollectionItemSources.Add: TImageCollectionSourceItem;
begin
  Result := TImageCollectionSourceItem(inherited Add);
end;

constructor TImageCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSourceImages := TImageCollectionItemSources.Create(Self, TImageCollectionSourceItem);
  FName := '';
  FDescription := '';
end;

destructor TImageCollectionItem.Destroy;
begin
  FSourceImages.Free;
  inherited;
end;

procedure TImageCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TImageCollectionItem then
  begin
    Name := TImageCollectionItem(Source).Name;
    Description := TImageCollectionItem(Source).Description;
    SourceImages := TImageCollectionItem(Source).SourceImages;
  end
  else
    inherited;
end;

procedure TImageCollectionItem.Change;
begin
  CheckSources;
  if Collection.Owner is TImageCollection then
    TMessageManager.DefaultManager.SendMessage(nil,
      TImageCollectionChangedMessage.Create(TImageCollection(Collection.Owner), Index, Name, Description));
end;

procedure TImageCollectionItem.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TImageCollectionItem.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

procedure TImageCollectionItem.SetSourceImages(const Value: TImageCollectionItemSources);
begin
  if FSourceImages <> Value then
    FSourceImages.Assign(Value);
end;

function TImageCollectionItem.CheckSources: Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if SourceImages.Count < 2 then
    Exit;

  for I := 0 to SourceImages.Count - 2 do
    for J := 0 to SourceImages.Count - 2 - I do
      if Max(SourceImages[J].Image.Width, SourceImages[J].Image.Height) >
         Max(SourceImages[J + 1].Image.Width, SourceImages[J + 1].Image.Height) then
      begin
       Result := True;
       SourceImages[J].Index := J + 1;
      end;
end;

function TImageCollectionItems.GetItem(Index: Integer): TImageCollectionItem;
begin
  Result := TImageCollectionItem(inherited GetItem(Index));
end;

procedure TImageCollectionItems.SetItem(Index: Integer; Value: TImageCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TImageCollectionItems.Add: TImageCollectionItem;
begin
  Result := TImageCollectionItem(inherited Add);
end;

type
  TImageCollectionHelper = class helper for TImageCollection
  private
    class var FProportional: Boolean;
  public
    class property Proportional: Boolean read FProportional write FProportional;
  end;

constructor TImageCollection.Create(AOwner: TComponent);
begin
  inherited;
  FImages := TImageCollectionItems.Create(Self, TImageCollectionItem);
  FInterpolationMode := icIMModeHighQualityCubic;
end;

destructor TImageCollection.Destroy;
begin
  FImages.Free;
  inherited;
end;

procedure TImageCollection.SetInterpolationMode(Value: TImageCollectionInterpolationMode);
begin
  FInterpolationMode := Value;
end;

procedure TImageCollection.SetImages(Value: TImageCollectionItems);
begin
  if FImages <> Value then
    FImages.Assign(Value);
end;

function TImageCollection.GetCount: Integer;
begin
  Result := FImages.Count;
end;

function TImageCollection.GetNameByIndex(AIndex: Integer): String;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := Images[AIndex].Name
  else
    Result := '';
end;

function TImageCollection.GetDescriptionByIndex(AIndex: Integer): String;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := Images[AIndex].Description
  else
    Result := '';
end;

function TImageCollection.GetIndexByName(const AName: String): Integer;
var
  I: Integer;
begin
  for I := 0 to FImages.Count - 1 do
    if SameText(FImages[I].Name, AName) then
      Exit(I);
  Result := -1;
end;

function TImageCollection.IsIndexAvailable(AIndex: Integer): Boolean;
begin
  Result := (Count > 0) and (AIndex >= 0) and (AIndex < Count) and
    (FImages[AIndex].SourceImages.Count > 0);
end;

function TImageCollection.GetSourceImageByIndex(AIndex, ASourceIndex: Integer): TWICImage;
begin
  if IsIndexAvailable(AIndex) and (ASourceIndex >= 0) and (ASourceIndex < Images[AIndex].SourceImages.Count) then
    Result := Images[AIndex].SourceImages[ASourceIndex].Image
  else
    Result := nil;
end;

function TImageCollection.GetSourceImageIndex(AIndex: Integer; AWidth, AHeight: Integer): Integer;
var
  I, ImageCount: Integer;
  Image: TWICImage;
begin
  Result := -1;
  if IsIndexAvailable(AIndex) then
  begin
    ImageCount := Images[AIndex].SourceImages.Count;
    if (Pred(ImageCount) = 0) and not Images[AIndex].SourceImages[0].Image.Empty then
      Result := 0
    else
    begin
      Images[AIndex].CheckSources;
      for I := 0 to Pred(ImageCount) do
      begin
        Image := Images[AIndex].SourceImages[I].Image;
        if Proportional then
        begin
          if (Image.Width >= AWidth) or (Image.Height >= AHeight) or (I = Pred(ImageCount)) then
            Exit(I);
        end
        else if (Max(Image.Width, Image.Height) >= Max(AWidth, AHeight)) or (I = Pred(ImageCount)) then
          Exit(I);
      end;
    end;
  end;
end;

function TImageCollection.GetSourceImage(AIndex: Integer; AWidth, AHeight: Integer): TWICImage;
var
  FIndex: Integer;
begin
  if AIndex < 0 then
    Result := nil
  else
  begin
    FIndex := GetSourceImageIndex(AIndex, AWidth, AHeight);
    if FIndex >= 0 then
      Result := Images[AIndex].SourceImages[FIndex].FImage
    else
      Result := nil;
  end;
end;

function TImageCollection.GetScaledImage(ASourceImage: TWICImage; ANewWidth, ANewHeight: Integer): TWICImage;
begin
  Result := ASourceImage.CreateScaledCopy(ANewWidth, ANewHeight, IMode[FInterpolationMode]);
end;

function TImageCollection.GetBitmap(AIndex: Integer; AWidth, AHeight: Integer): TBitmap;
var
  SourceImage: TWICImage;
  BufferImage: TWICImage;
begin
  Result := nil;
  if (AIndex < 0) or (AIndex > FImages.Count - 1) then
    Exit;

  SourceImage := GetSourceImage(AIndex, AWidth, AHeight);
  if SourceImage = nil then
    Exit;

  if Assigned(FOnGetBitmap) then
    FOnGetBitmap(SourceImage, AWidth, AHeight, Result)
  else
  begin
    Result := TBitmap.Create;
    if not TStyleManager.ActiveStyle.Enabled then
    begin
      Result.PixelFormat := pf32bit;
      Result.Canvas.Brush.Color := clBtnFace;
      Result.SetSize(AWidth, AHeight);
      Result.Canvas.StretchDraw(Rect(0, 0, AWidth, AHeight), SourceImage);
    end
    else if (SourceImage.Width = AWidth) and (SourceImage.Height = AHeight) then
      Result.Assign(SourceImage)
    else
    begin
      BufferImage := GetScaledImage(SourceImage, AWidth, AHeight);
      try
        Result.Assign(BufferImage);
      finally
        BufferImage.Free;
      end;
    end;
    if Result.PixelFormat = pf32bit then
      Result.AlphaFormat := afIgnored;
  end;
end;

function TImageCollection.GetBitmap(const AName: String; AWidth, AHeight: Integer; AEnabled: Boolean = True): TBitmap;
begin
  Result := GetBitmap(GetIndexByName(AName), AWidth, AHeight);
end;

procedure TImageCollection.DrawSource(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; ASourceIndex: Integer; AProportional: Boolean = False);
var
  SourceImage: TWICImage;
begin
  if ARect.IsEmpty then
    Exit;

  SourceImage := GetSourceImageByIndex(AIndex, ASourceIndex);
  if SourceImage <> nil then
  begin
    if AProportional then
      ARect := UpdateRectForProportionalSize(ARect, SourceImage.Width, SourceImage.Height, True);
    SourceImage.InterpolationMode := IMode[FInterpolationMode];
    ACanvas.StretchDraw(ARect, SourceImage);
  end;
end;

procedure TImageCollection.DoDraw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean);
var
  SourceImage: TWICImage;
begin
  if ARect.IsEmpty then
    Exit;

  SourceImage := GetSourceImage(AIndex, ARect.Width, ARect.Height);
  if SourceImage <> nil then
  begin
    if AProportional then
      ARect := UpdateRectForProportionalSize(ARect, SourceImage.Width, SourceImage.Height, True);
    SourceImage.InterpolationMode := IMode[FInterpolationMode];
    ACanvas.StretchDraw(ARect, SourceImage);
  end;
end;

procedure TImageCollection.Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean = False);
begin
  Proportional := AProportional;
  if Assigned(FOnDraw) then
    FOnDraw(GetSourceImage(AIndex, ARect.Width, ARect.Height), ACanvas, ARect, AProportional)
  else
    DoDraw(ACanvas, ARect, AIndex, AProportional);
end;

procedure TImageCollection.Draw(ACanvas: TCanvas; ARect: TRect; const AName: String; AProportional: Boolean = False);
begin
  if Assigned(FOnDraw) then
    FOnDraw(GetSourceImage(GetIndexByName(AName), ARect.Width, ARect.Height),
      ACanvas, ARect, AProportional)
  else
    DoDraw(ACanvas, ARect, GetIndexByName(AName), AProportional);
end;

procedure TImageCollection.Assign(Source: TPersistent);
begin
  if Source is TImageCollection then
  begin
    FInterpolationMode := TImageCollection(Source).InterpolationMode;
    FImages.Assign(TImageCollection(Source).Images);
    Change;
  end
  else
    inherited;
end;

function TImageCollection.CreateSourceItem(AName: String): TImageCollectionSourceItem;
var
  LIndex: Integer;
  LItem: TImageCollectionItem;
begin
  LIndex := GetIndexByName(AName);
  if LIndex = -1 then
  begin
    LItem := FImages.Add;
    LItem.Name := AName;
  end
  else
    LItem := FImages[LIndex];
  Result := LItem.SourceImages.Add;
end;

procedure TImageCollection.Add(AName: String; const AFileName: String);
var
  LSourceItem: TImageCollectionSourceItem;
begin
  LSourceItem := CreateSourceItem(AName);
  try
    LSourceItem.Image.LoadFromFile(AFileName);
  finally
    LSourceItem.Update;
  end;
end;

procedure TImageCollection.Add(AName: String; AInstance: THandle; const AResourceName: String);
var
  LSourceItem: TImageCollectionSourceItem;
begin
  LSourceItem := CreateSourceItem(AName);
  try
    LSourceItem.Image.LoadFromResourceName(AInstance, AResourceName);
  finally
    LSourceItem.Update;
  end;
end;

procedure TImageCollection.Add(AName: String; AInstance: THandle; const AResourceName: String; ASuffixes: array of string);
var
  I: Integer;
begin
  for I := Low(ASuffixes) to High(ASuffixes) do
    Add(AName, AInstance, AResourceName + ASuffixes[I]);
end;

procedure TImageCollection.Add(AName: String; Stream: TStream);
var
  LSourceItem: TImageCollectionSourceItem;
begin
  LSourceItem := CreateSourceItem(AName);
  try
    LSourceItem.Image.LoadFromStream(Stream);
  finally
    LSourceItem.Update;
  end;
end;

procedure TImageCollection.Delete(AIndex: Integer);
begin
  if (AIndex > 0) and (AIndex < FImages.Count) then
  begin
    FImages.Delete(AIndex);
    TMessageManager.DefaultManager.SendMessage(nil,
      TImageCollectionChangedMessage.Create(Self, -1, '', ''));
  end;
end;

procedure TImageCollection.Delete(const AName: String);
begin
  Delete(GetIndexByName(AName));
end;

procedure TImageCollection.Delete(const ACategory: String; AStartIndex, AEndIndex: Integer);
var
  I: Integer;
begin
  if FImages.Count = 0 then
    Exit;

  if (ACategory = '') and (AStartIndex <= 0) and ((AEndIndex < 0) or (AEndIndex >= FImages.Count - 1)) then
  begin
    FImages.Clear;
    TMessageManager.DefaultManager.SendMessage(nil,
      TImageCollectionChangedMessage.Create(Self, -1, '', ''));
    Exit;
  end;

  if AStartIndex < 0 then
    AStartIndex := 0;
  if (AEndIndex < 0) or (AEndIndex > FImages.Count - 1) then
    AEndIndex := FImages.Count - 1;

  for I := AEndIndex downto AStartIndex do
    if (ACategory = '') or SameText(ACategory, ExtractImageCollectionCategory(FImages[I].Name)) then
      FImages.Delete(I);

  TMessageManager.DefaultManager.SendMessage(nil,
    TImageCollectionChangedMessage.Create(Self, -1, '', ''));
end;

initialization

  StartClassGroup(TControl);
  ActivateClassGroup(TControl);
  GroupDescendentsWith(TImageCollection, Vcl.Controls.TControl);

end.
