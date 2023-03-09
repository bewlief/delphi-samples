{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2014-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.EditBox;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Rtti, System.UITypes, System.SysUtils, FMX.Controls, FMX.Edit,
  FMX.StdActns, FMX.ActnList, FMX.Presentation.Messages, FMX.Text, FMX.Types, FMX.Controls.Model;

const
  MM_DECIMALDIGITS_CHANGED = MM_EDIT_USER + 1;
  MM_VALUETYPE_CHANGED = MM_EDIT_USER + 2;
  MM_VALUERANGE_CHANGED = MM_EDIT_USER + 3;
  MM_HORZ_INCREMENT_CHANGED = MM_EDIT_USER + 4;
  MM_EDITBOX_USER = MM_EDIT_USER + 5;

type

{ TCustomEditBox }

  TEditBoxModel = class(TCustomEditModel)
  public const
    DefaultDecimalDigits = 2;
    DefaultHorzIncrement = 1;
    DefaultValueType = TNumValueType.Integer;
    DefaultKeyboardType = TVirtualKeyboardType.NumberPad;
  private
    FValueRange: TValueRange;
    FValueType: TNumValueType;
    FDecimalDigits: Integer;
    FHorzIncrement: Single;
    FDefaultValueRange: TBaseValueRange;
    procedure SetDecimalDigits(const Value: Integer);
    procedure SetValueType(const Value: TNumValueType);
    function GetValue: Double;
    procedure SetValue(const Value: Double);
    function GetCurrentIncrement: Single;
    procedure SetHorzIncrement(const Value: Single);
    function HorzIncrementStored: Boolean;
  protected
    function DoFiltering(const Value: string): string; override;
    function DoValidating(const Value: string): string; override;
    function DoValidate(const Value: string): string; override;
    function GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass; override;
    function ConvertToText(const AValue: Double; const Source: string = ''): string;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ConvertValueToText: string;
    function ConvertTextToValue(const AText: string): Double;
    property CurrentIncrement: Single read GetCurrentIncrement;
    property DefaultValueRange: TBaseValueRange read FDefaultValueRange;
    property DecimalDigits: Integer read FDecimalDigits write SetDecimalDigits;
    property HorzIncrement: Single read FHorzIncrement write SetHorzIncrement stored HorzIncrementStored nodefault;
    property Value: Double read GetValue write SetValue;
    property ValueType: TNumValueType read FValueType write SetValueType;
    property ValueRange: TValueRange read FValueRange;
  end;

  TCustomEditBox = class(TCustomEdit, IValueRange)
  private
    procedure SetMax(const AValue: Double);
    function GetMax: Double;
    function MaxStored: Boolean;
    procedure SetMin(const AValue: Double);
    function GetMin: Double;
    function MinStored: Boolean;
    function GetValue: Double;
    procedure SetDecimalDigits(const ADecimalDigits: Integer);
    function GetDecimalDigits: Integer;
    procedure SetValueType(const AValueType: TNumValueType);
    function GetValueType: TNumValueType;
    procedure SetValueRange(const AValue: TCustomValueRange);
    function GetValueRange: TCustomValueRange;
    procedure SetValueRange_(const Value: TValueRange);
    function GetValueRange_: TValueRange;
    procedure SetHorzIncrement(const Value: Single);
    function GetHorzIncrement: Single;
    function DefStored: Boolean;
    function GetDefaultValueRange: TBaseValueRange;
  private
    function GetModel: TEditBoxModel; overload;
  protected
    /// <summary>
    ///   Return False in case <c>Value</c> has default value which means we don't need to store it
    /// </summary>
    function ValueStored: Boolean;
    function HorzIncrementStored: Boolean; virtual;
    procedure Loaded; override;
    procedure SetValue(const AValue: Double); virtual;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    { Actions }
    function GetActionLinkClass: TActionLinkClass; override;
    procedure ActionChange(Sender: TBasicAction; CheckDefaults: Boolean); override;
    property DefaultValueRange: TBaseValueRange read GetDefaultValueRange;
  protected
    function DefineModelClass: TDataModelClass; override;
    function DefinePresentationName: string; override;
    property Model: TEditBoxModel read GetModel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    procedure ValueInc;
    procedure ValueDec;
    property HorzIncrement: Single read GetHorzIncrement write SetHorzIncrement stored HorzIncrementStored nodefault;
    property ValueRange: TValueRange read GetValueRange_ write SetValueRange_ stored ValueStored;
    property Font;
    property FontColor;
    property TextSettings;
  published
    property Cursor default crIBeam;
    property DecimalDigits: Integer read GetDecimalDigits write SetDecimalDigits default TEditBoxModel.DefaultDecimalDigits;
    property Min: Double read GetMin write SetMin stored MinStored nodefault;
    property Max: Double read GetMax write SetMax stored MaxStored nodefault;
    property Value: Double read GetValue write SetValue stored ValueStored nodefault;
    property ValueType: TNumValueType read GetValueType write SetValueType default TNumValueType.Integer;
    property Text stored False;
  end;

implementation

uses
  System.Actions, System.Math, System.Character, System.Math.Vectors, FMX.Consts, FMX.EditBox.Style;

type
  TValueRangeCustomEditBox = class(TValueRange)
  private
    [Weak] FCustomEditBox: TCustomEditBox;
    FValueChanged: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoBeforeChange; override;
    procedure DoAfterChange; override;
    function NeedActionChange: Boolean;
    property CustomEditBox: TCustomEditBox read FCustomEditBox;
  end;

{ TValueRangeCustomEditBox }

constructor TValueRangeCustomEditBox.Create(AOwner: TComponent);
begin
  if not (AOwner is TCustomEditBox) then
    raise EActionError.CreateFMT(SEUseHeirs, [TCustomEditBox.ClassName]);
  inherited;
  FCustomEditBox := TCustomEditBox(AOwner);
end;

function TValueRangeCustomEditBox.NeedActionChange: Boolean;
begin
  Result := CustomEditBox.ActionClient and
           (not CustomEditBox.DefStored) and
           (not TValueRangeAction(CustomEditBox.Action).ValueRange.Changing);
end;

procedure TValueRangeCustomEditBox.DoAfterChange;
begin
  if FValueChanged then
  try
    if not CustomEditBox.Model.Validating then
    begin
      CustomEditBox.Model.Text := CustomEditBox.Model.ConvertValueToText;
      CustomEditBox.Model.Change;
    end;
  finally
    FValueChanged := False;
  end;
end;

procedure TValueRangeCustomEditBox.DoBeforeChange;
const
  DoubleFraction = 15;
var
  Sigma: Double;
begin
  if CustomEditBox.DecimalDigits < DoubleFraction then
    Sigma := Power(10, -CustomEditBox.DecimalDigits) / 2
  else
    Sigma := 0;
  if not SameValue(New.Value, Value, Sigma) then
    FValueChanged := True;
  if NeedActionChange then
    TValueRangeAction(CustomEditBox.Action).ValueRange.Assign(New);
  inherited;
end;

{ TCustomEditBox }

constructor TCustomEditBox.Create(AOwner: TComponent);
begin
  inherited;
  Text := '0';
end;

type
  TCustomEditBoxSettings = class (TTextSettingsInfo.TCustomTextSettings)
  public
    constructor Create(const AOwner: TPersistent); override;
  published
    property Font;
    property FontColor;
    property HorzAlign default TTextAlign.Leading;
    property VertAlign default TTextAlign.Center;
  end;

{ TCustomEditBoxSettings }

constructor TCustomEditBoxSettings.Create(const AOwner: TPersistent);
begin
  inherited;
  WordWrap := False;
  HorzAlign := TTextAlign.Leading;
  VertAlign := TTextAlign.Center;
end;

procedure TCustomEditBox.AfterConstruction;
begin
  inherited;
  DefaultValueRange.Assign(Model.ValueRange.New);
end;

function TCustomEditBox.GetData: TValue;
begin
  if ValueType = TNumValueType.Integer then
    Result := TValue.From<Integer>(Round(Value))
  else
    Result := TValue.From<Double>(Value);
end;

function TCustomEditBox.GetDecimalDigits: Integer;
begin
  Result := Model.DecimalDigits;
end;

function TCustomEditBox.GetDefaultValueRange: TBaseValueRange;
begin
  Result := Model.DefaultValueRange;
end;

function TCustomEditBox.GetHorzIncrement: Single;
begin
  Result := Model.HorzIncrement;
end;

function TCustomEditBox.DefineModelClass: TDataModelClass;
begin
  Result := TEditBoxModel;
end;

function TCustomEditBox.DefinePresentationName: string;
begin
  Result := 'EditBox-' + GetPresentationSuffix;
end;

function TCustomEditBox.DefStored: boolean;
begin
  Result := not (ActionClient and (Action is TCustomValueRangeAction));
end;

function TCustomEditBox.MaxStored: Boolean;
begin
  Result := DefStored and (not SameValue(Max, DefaultValueRange.Max));
end;

function TCustomEditBox.MinStored: Boolean;
begin
  Result := DefStored and (not SameValue(Min, DefaultValueRange.Min));
end;

function TCustomEditBox.ValueStored: Boolean;
begin
  Result := DefStored and (not SameValue(Value, DefaultValueRange.Value));
end;

function TCustomEditBox.GetValueRange: TCustomValueRange;
begin
  Result := Model.ValueRange;
end;

function TCustomEditBox.GetValueRange_: TValueRange;
begin
  Result := Model.ValueRange
end;

function TCustomEditBox.GetValueType: TNumValueType;
begin
  Result := Model.ValueType;
end;

procedure TCustomEditBox.SetValueRange(const AValue: TCustomValueRange);
begin
  Model.ValueRange.Assign(AValue);
end;

procedure TCustomEditBox.SetValueRange_(const Value: TValueRange);
begin
  Model.ValueRange.Assign(Value);
end;

function TCustomEditBox.GetActionLinkClass: TActionLinkClass;
begin
  Result := TValueRangeActionLink;
end;

procedure TCustomEditBox.ActionChange(Sender: TBasicAction; CheckDefaults: Boolean);
begin
  if Sender is TValueRangeAction then
  begin
    if (not CheckDefaults) or (Model.ValueRange.IsEmpty) then
      Model.ValueRange.Assign(TValueRangeAction(Sender).ValueRange);
  end;
  inherited;
end;

function TCustomEditBox.HorzIncrementStored: Boolean;
begin
  Result := not SameValue(Model.HorzIncrement, 1);
end;

procedure TCustomEditBox.SetData(const Value: TValue);
begin
  if Value.IsType<TNotifyEvent> then
    OnChange := Value.AsType<TNotifyEvent>()
  else if Value.IsOrdinal then
    Self.Value := Value.AsInteger
  else
    Self.Value := Value.AsType<Double>;
end;

procedure TCustomEditBox.ValueDec;
begin
  ValueRange.Value := Model.ConvertTextToValue(Text);
  ValueRange.Increment := Model.CurrentIncrement;
  ValueRange.Dec;
end;

procedure TCustomEditBox.ValueInc;
begin
  ValueRange.Value := Model.ConvertTextToValue(Text);
  ValueRange.Increment := Model.CurrentIncrement;
  ValueRange.Inc;
end;

procedure TCustomEditBox.Loaded;
begin
  if Model.ValueRange.IsChanged then
    Model.ValueRange.Changed(True);
  inherited;
end;

procedure TCustomEditBox.SetDecimalDigits(const ADecimalDigits: Integer);
begin
  Model.DecimalDigits := ADecimalDigits;
end;

procedure TCustomEditBox.SetHorzIncrement(const Value: Single);
begin
  Model.HorzIncrement := Value;
end;

function TCustomEditBox.GetMax: Double;
begin
  Result := Model.ValueRange.Max;
end;

procedure TCustomEditBox.SetMax(const AValue: Double);
begin
  Model.ValueRange.Max := AValue;
end;

function TCustomEditBox.GetMin: Double;
begin
  Result := Model.ValueRange.Min;
end;

function TCustomEditBox.GetModel: TEditBoxModel;
begin
  Result := GetModel<TEditBoxModel>;
end;

procedure TCustomEditBox.SetMin(const AValue: Double);
begin
  Model.ValueRange.Min := AValue;
end;

function TCustomEditBox.GetValue: Double;
begin
  Result := Model.Value;
end;

procedure TCustomEditBox.SetValue(const AValue: Double);
begin
  Model.Value := AValue;
end;

procedure TCustomEditBox.SetValueType(const AValueType: TNumValueType);
begin
  Model.ValueType := AValueType;
end;

{ TEditBoxModel }

function FilterCharByValueType(const AValueType: TNumValueType): string;
begin
  if AValueType = TNumValueType.Integer then
    Result := '0123456789-+'
  else
  begin
    Result := '0123456789.,-+';
    if Pos(FormatSettings.DecimalSeparator, Result) = 0 then
      Result := Result + FormatSettings.DecimalSeparator;
  end;
end;

function TEditBoxModel.ConvertTextToValue(const AText: string): Double;
var
  TempValue: Double;
begin
  if TryTextToValue(AText, TempValue, Value) then
  begin
    if (Frac(TempValue) = 0) or (ValueType = TNumValueType.Integer) then
      Result := Round(TempValue)
    else
      Result := TempValue;
  end
  else
    Result := Value;
end;

function TEditBoxModel.ConvertToText(const AValue: Double; const Source: string): string;
begin
  if ValueType = TNumValueType.Integer then
    Result := IntToStr(Round(AValue))
  else
    Result := FloatToStrF(AValue, ffFixed, 20 - DecimalDigits, DecimalDigits);
  if (ValueType = TNumValueType.Float) and not Source.IsEmpty and (Source.EndsWith('.') or Source.EndsWith(',')) then
    Result := Result + FormatSettings.DecimalSeparator;
end;

function TEditBoxModel.ConvertValueToText: string;
begin
  Result := ConvertToText(Self.Value);
end;

constructor TEditBoxModel.Create;
begin
  inherited;
  FValueRange := TValueRangeCustomEditBox.Create(Owner);
  FDefaultValueRange := TBaseValueRange.Create;
  FDecimalDigits := DefaultDecimalDigits;
  FHorzIncrement := DefaultHorzIncrement;
  FValueType := DefaultValueType;
  KeyboardType := DefaultKeyboardType;
  FilterChar := FilterCharByValueType(ValueType);
  Text := '0';
end;

destructor TEditBoxModel.Destroy;
begin
  FreeAndNil(FDefaultValueRange);
  FreeAndNil(FValueRange);
  inherited;
end;

function TEditBoxModel.DoFiltering(const Value: string): string;
begin
  Result := Value;
end;

function TEditBoxModel.DoValidating(const Value: string): string;
const
  NonBreakingSpace = #160;
var
  S: string;
  I: Integer;
  SpaceAware: TArray<Char>;
begin
  S := '';
  SpaceAware := [NonBreakingSpace];
  if not FormatSettings.ThousandSeparator.IsInArray([',', '.']) then
    SpaceAware := SpaceAware + [FormatSettings.ThousandSeparator];
  for I := 0 to Value.Length - 1 do
    if (Value.Chars[I] > ' ') and (not Char.IsInArray(Value, I, SpaceAware)) then
      S := S + Value.Chars[I];
  Result := inherited DoValidating(S);
end;

function TEditBoxModel.DoValidate(const Value: string): string;
var
  TempValue: Double;
  LValue: string;
begin
  if not Value.IsEmpty then
    LValue := Value
  else if IsInfinite(ValueRange.Min) then
    Exit('')
  else
    LValue := ConvertToText(ValueRange.Min);
  if TryTextToValue(LValue, TempValue, ValueRange.Value) and (ValueType = TNumValueType.Integer) then
    TempValue := Round(TempValue);
  Result := inherited DoValidate(ConvertToText(TempValue, LValue));
  if TryTextToValue(Result, TempValue, ValueRange.Value) then
  begin
    if ValueType = TNumValueType.Integer then
      TempValue := Round(TempValue);
    ValueRange.Value := TempValue;
  end;
  Result := ConvertToText(ValueRange.Value, LValue);
end;

function TEditBoxModel.GetCurrentIncrement: Single;
begin
  Result := System.Math.Max(HorzIncrement, ValueRange.Frequency);
end;

function TEditBoxModel.GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
begin
  Result := TCustomEditBoxSettings;
end;

function TEditBoxModel.GetValue: Double;
begin
  if ValueType = TNumValueType.Integer then
    Result := Round(ValueRange.Value)
  else
    Result := ValueRange.Value;
end;

procedure TEditBoxModel.SetDecimalDigits(const Value: Integer);
begin
  if (FDecimalDigits <> Value) and (Value >= 0) then
  begin
    FDecimalDigits := Value;
    SendMessage(MM_DECIMALDIGITS_CHANGED);
  end;
end;

function TEditBoxModel.HorzIncrementStored: Boolean;
begin
  Result := not SameValue(HorzIncrement, DefaultHorzIncrement);
end;

procedure TEditBoxModel.SetHorzIncrement(const Value: Single);
var
  LValue: Single;
begin
  LValue := Max(0, Value);
  if not SameValue(FHorzIncrement, LValue, TEpsilon.Vector) then
  begin
    FHorzIncrement := LValue;
    SendMessage<Single>(MM_HORZ_INCREMENT_CHANGED, FHorzIncrement);
  end;
end;

procedure TEditBoxModel.SetValue(const Value: Double);
begin
  ValueRange.Value := Value;
end;

procedure TEditBoxModel.SetValueType(const Value: TNumValueType);
const
  DefaultVirtualKeyboardTypes = [TVirtualKeyboardType.NumberPad, TVirtualKeyboardType.NumbersAndPunctuation];
begin
  if FValueType <> Value then
  begin
    FValueType := Value;
    FilterChar := FilterCharByValueType(FValueType);
    // If user specify non standard virtual keybaord type, we don't automatically reset it.
    if KeyboardType in DefaultVirtualKeyboardTypes then
      case FValueType of
        TNumValueType.Integer:
          KeyboardType := TVirtualKeyboardType.NumberPad;
        TNumValueType.Float:
          KeyboardType := TVirtualKeyboardType.NumbersAndPunctuation;
      end;
    SendMessage(MM_VALUETYPE_CHANGED);
  end;
end;

end.
