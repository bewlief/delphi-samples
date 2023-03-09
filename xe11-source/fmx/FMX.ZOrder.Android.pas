{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2018-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.ZOrder.Android;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, Androidapi.JNI.GraphicsContentViewText, FMX.Controls, FMX.Forms, FMX.ZOrder;

type

  /// <summary>Helper class used to manage platform controls</summary>
  TAndroidZOrderManager = class(TCustomZOrderManager<JView>)
  private
    function AbsoluteToParentView(const AControl: TControl; const AParentView: JView): TRect;
    function IsFormLayout(const AView: JView): Boolean;
  protected
    function GetParentView(const AView: JView): JView; override;
    procedure RemoveFromParent(const AView: JView); override;
    procedure InsertSubview(const AParent, AView, APreviousSiblingView: JView); override;
    function IndexOfView(const AView: JView): Integer; override;
    function IsFirstView(const AView: JView): Boolean; override;
    procedure UpdateBounds(const AView: JView; const AControl: TControl; const AParentView: JView); override;
    procedure UpdateVisible(const AView: JView; const AVisible: Boolean); override;
    function GetFormView(const AForm: TCommonCustomForm): JView; override;
    function SameView(const AViewLeft, AViewRight: JView): Boolean; override;
    function NullView: JView; override;
  end;

implementation

uses
  System.SysUtils, System.Math, System.Math.Vectors, System.Classes, Androidapi.JNI.Widget, Androidapi.Helpers,
  Androidapi.JNIBridge, Androidapi.Jni, AndroidApi.JNI.JavaTypes, FMX.Types, FMX.Consts, FMX.Platform.Android,
  FMX.Platform.UI.Android;

function TAndroidZOrderManager.AbsoluteToParentView(const AControl: TControl; const AParentView: JView): TRect;
var
  Bounds: TRectF;
  ParentTmp: JView;
  Form: TCommonCustomForm;
  ParentOrigin: TPoint;
  ParentParams: JRelativeLayout_LayoutParams;
begin
  if FindForm(AControl, Form) then
  begin
    // Control can be placed in native scene.
    if (AControl.Scene = nil) or (AControl.Scene.GetObject = Form) then
    begin
      ParentOrigin := TPoint.Zero;

      ParentTmp := AParentView;
      while (ParentTmp <> nil) and not TJNIResolver.IsSameObject(ParentTmp, WindowHandleToPlatform(Form.Handle).FormLayout) and
        (ParentTmp.getLayoutParams <> nil) and (ParentTmp.getParent <> nil) do
      begin
        ParentParams := TJRelativeLayout_LayoutParams.Wrap(ParentTmp.getLayoutParams);
        ParentOrigin := ParentOrigin + TPoint.Create(ParentParams.leftMargin, ParentParams.topMargin);
        ParentTmp := TJView.Wrap(ParentTmp.getParent);
      end;
      Bounds := AControl.AbsoluteRect;
      Result.Left := Ceil(Bounds.Left * Owner.Scale);
      Result.Top := Ceil(Bounds.Top * Owner.Scale);
      Result.Right := Ceil(Bounds.Right * Owner.Scale);
      Result.Bottom := Ceil(Bounds.Bottom * Owner.Scale);
      Result.Offset(-ParentOrigin.X, -ParentOrigin.Y);
    end
    else
    begin
      Bounds := GetControlBoundsInParentView(AControl, AParentView);
      Result.Left := Ceil(Bounds.Left * Owner.Scale);
      Result.Top := Ceil(Bounds.Top * Owner.Scale);
      Result.Right := Ceil(Bounds.Right * Owner.Scale);
      Result.Bottom := Ceil(Bounds.Bottom * Owner.Scale);
    end;
  end
  else
    Result := TRect.Empty;
end;

function TAndroidZOrderManager.SameView(const AViewLeft, AViewRight: JView): Boolean;
begin
  Result := TJNIResolver.IsSameObject(AViewLeft, AViewRight);
end;

function TAndroidZOrderManager.GetFormView(const AForm: TCommonCustomForm): JView;
var
  FormHandle: TAndroidWindowHandle;
begin
  FormHandle := WindowHandleToPlatform(AForm.Handle);
  Result := FormHandle.FormLayout;
end;

function TAndroidZOrderManager.GetParentView(const AView: JView): JView;
var
  ParentView: JViewParent;
begin
  ParentView := AView.getParent;
  if ParentView = nil then
    Result := nil
  else
    Result := TJView.Wrap(AView.getParent);
end;

function TAndroidZOrderManager.NullView: JView;
begin
  Result := nil;
end;

function TAndroidZOrderManager.IndexOfView(const AView: JView): Integer;
var
  Parent: JViewParent;
  ParentGroup: JViewGroup;
begin
  Parent := AView.getParent;
  if Parent = nil then
    Result := -1
  else
  begin
    ParentGroup := TJViewGroup.Wrap(Parent);
    Result := ParentGroup.indexOfChild(AView);
  end;
end;

procedure TAndroidZOrderManager.InsertSubview(const AParent, AView, APreviousSiblingView: JView);
var
  ParentGroup: JViewGroup;
  Param: JRelativeLayout_LayoutParams;
  PreviousSiblingViewIdx: Integer;
  InsertIndex: Integer;
begin
  ParentGroup := TJViewGroup.Wrap(AParent);
  if AView.getLayoutParams = nil then
    Param := TJRelativeLayout_LayoutParams.JavaClass.init(0, 0)
  else
    Param := TJRelativeLayout_LayoutParams.Wrap(AView.getLayoutParams);

  // FMX form consists from two parts: form's view and layout for other native controls. Form's view embedded into Layout.
  // So it should always holds 0 index. So when we rearrange native controls, we should keep in mind, that for native
  // controls first index is 1.
  if IsFormLayout(AParent) then
    InsertIndex := 1
  else
    InsertIndex := 0;

  if APreviousSiblingView = nil then
    ParentGroup.addView(AView, InsertIndex, Param)
  else
  begin
    PreviousSiblingViewIdx := ParentGroup.indexOfChild(APreviousSiblingView);
    ParentGroup.addView(AView, PreviousSiblingViewIdx + 1, Param);
  end;
end;

function TAndroidZOrderManager.IsFirstView(const AView: JView): Boolean;
var
  ParentGroup: JViewGroup;
  ViewIndex: Integer;
begin
  if AView.getParent = nil then
    Exit(False);

  ParentGroup := TJViewGroup.Wrap(AView.getParent);
  ViewIndex := IndexOfView(AView);

  if IsFormLayout(ParentGroup) then
    Result := ViewIndex = 1
  else
    Result := ViewIndex = 0;
end;

function TAndroidZOrderManager.IsFormLayout(const AView: JView): Boolean;
var
  FormTag: JObject;
begin
  FormTag := AView.getTag;
  Result := (FormTag <> nil) and TJNIResolver.IsInstanceOf(FormTag, TJString.GetClsID) and
            TJString.Wrap(FormTag).equals(StringToJString('FMXForm'));
end;

procedure TAndroidZOrderManager.RemoveFromParent(const AView: JView);
var
  Parent: JViewParent;
begin
  Parent := AView.getParent;
  if Parent <> nil then
    TJViewGroup.Wrap(Parent).removeView(AView);
end;

procedure TAndroidZOrderManager.UpdateBounds(const AView: JView; const AControl: TControl; const AParentView: JView);
var
  Param: JRelativeLayout_LayoutParams;
  R: TRect;
begin
  Param := TJRelativeLayout_LayoutParams.Wrap(AView.getLayoutParams);
  R := AbsoluteToParentView(AControl, AParentView);
  Param.width := R.Width;
  Param.height := R.Height;
  Param.leftMargin := R.Left;
  Param.topMargin := R.Top;
  if not AView.isInLayout then
    AView.requestLayout
  else
    TThread.ForceQueue(nil, procedure
    begin
      AView.requestLayout;
    end);
end;

procedure TAndroidZOrderManager.UpdateVisible(const AView: JView; const AVisible: Boolean);
var
  Visibility: Integer;
begin
  if AVisible then
    Visibility := TJView.JavaClass.VISIBLE
  else
    Visibility := TJView.JavaClass.GONE;
  AView.setVisibility(Visibility)
end;

end.
