{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2012-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Media.Android;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Messaging, System.Types, Androidapi.JNI.Media, Androidapi.JNI.VideoView, Androidapi.JNI.App,
  Androidapi.JNI.Widget, Androidapi.JNI.Embarcadero, Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes, FMX.Media, FMX.Platform.Android, FMX.ZOrder.Android;

const
  AndroidMediaTimeScale = TMediaTime(MediaTimeScale div MSecsPerSec);

type
  TAndroidCaptureDeviceManager = class(TCaptureDeviceManager)
  public
    constructor Create; override;
  end;

  TAndroidMedia = class(TMedia)
  private
    FPlayer: JMediaPlayer;
    FVolume: Single;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    procedure SeekToBegin;
    function GetDuration: TMediaTime; override;
    function GetCurrent: TMediaTime; override;
    procedure SetCurrent(const Value: TMediaTime); override;
    function GetVideoSize: TPointF; override;
    function GetMediaState: TMediaState; override;
    function GetVolume: Single; override;
    procedure SetVolume(const Value: Single); override;
    procedure UpdateMediaFromControl; override;
    procedure DoPlay; override;
    procedure DoStop; override;
  public
    constructor Create(const AFileName: string); override;
    destructor Destroy; override;
  end;

  TAndroidMediaCodec = class(TCustomMediaCodec)
  public
    function CreateFromFile(const AFileName: string): TMedia; override;
  end;

  TAndroidVideo = class(TMedia)
  private
    type
      TCommonVolume = class
      strict private
        FAudioService: JObject;
        FAudioManager: JAudioManager;
        FMaxVolume: Integer;
        procedure SetVolume(const Value: Single);
        function GetVolume: Single;
      public
        constructor Create;
        property Value: Single read GetVolume write SetVolume;
      end;
  private
    FVolume: TCommonVolume;
    FScale: Single;
    FJustAudio: TAndroidMedia;
    FVideoPlayer: JVideoView;
    FVideoSize: TSize;
    FVideoEnabled: Boolean;
    function AllAssigned: Boolean;
    procedure RealignView;
    procedure RetreiveVideoSize;
    procedure CheckVideo;
    procedure InitInstance;
    function InstanceCreated: Boolean;
    function IsVideoEnabled: Boolean;
    function GetZOrderManager: TAndroidZOrderManager;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    procedure SeekToBegin;
    function GetDuration: TMediaTime; override;
    function GetCurrent: TMediaTime; override;
    procedure SetCurrent(const Value: TMediaTime); override;
    function GetVideoSize: TPointF; override;
    function GetMediaState: TMediaState; override;
    function GetVolume: Single; override;
    procedure SetVolume(const Value: Single); override;
    procedure UpdateMediaFromControl; override;
    procedure DoPlay; override;
    procedure DoStop; override;
  public
    constructor Create(const AFileName: string); override;
    destructor Destroy; override;
    property ZOrderManager: TAndroidZOrderManager read GetZOrderManager;
  end;

  TAndroidVideoCodec = class(TCustomMediaCodec)
  public
    function CreateFromFile(const AFileName: string): TMedia; override;
  end;

implementation

uses
  System.Classes, System.RTLConsts, System.Threading, System.Math, System.Permissions, System.SyncObjs, System.Generics.Collections, System.UITypes,
  Androidapi.Bitmap, Androidapi.JNI.Hardware, Androidapi.Gles, Androidapi.JNI.Os, Androidapi.JNI.RenderScript, Androidapi.Helpers,
  FMX.Consts, FMX.Types, FMX.Surfaces, FMX.Graphics, FMX.Helpers.Android, FMX.Forms, FMX.Platform;

const
  THEME_DARK_NOTITLE_FULLSCREEN = $0103000A;

type
  TVideoInstance = record
    VideoPlayer: JVideoView;
  end;

  TVideoPool = class
  private
    FUsed: TList<TVideoInstance>;
    FReadyToUse: TList<TVideoInstance>;
    procedure CreateOneMoreInstance;
    procedure UIFreezeInstance(const AInstance: TVideoInstance);
  public
    function UIGetInstance: TVideoInstance;
    procedure UIReturnInstance(const AInstance: TVideoInstance);
    destructor Destroy; override;
    constructor Create;
  end;

var
  VideoPool: TVideoPool;

{$REGION 'Local Class Declarations'}

type
  TAndroidAudioCaptureDevice = class(TAudioCaptureDevice)
  private
    FRecorder: JMediaRecorder;

    procedure RecorderRequestPermissionsResult(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray);
  protected
    procedure DoStartCapture; override;
    procedure DoStopCapture; override;
    function GetDeviceState: TCaptureDeviceState; override;
    procedure DoRequestPermission; override;
  public
    destructor Destroy; override;
  end;

  TAndroidVideoCaptureDevice = class;

  TAndroidVideoCaptureCallback = class(TJavaLocal, JCamera_PreviewCallback)
  private
    FCaptureDevice: TAndroidVideoCaptureDevice;
  public
    constructor Create(const ACaptureDevice: TAndroidVideoCaptureDevice); overload;
    procedure onPreviewFrame(AData: TJavaArray<Byte>; ACamera: JCamera); cdecl;
  end;

  TAndroidVideoCaptureDevice = class(TVideoCaptureDevice)
  private type
    TNullable<T: record> = record
    private
      FValue: T;
      FHasValue: Boolean;

      procedure SetValue(AValue: T);
    public
      property Value: T read FValue write SetValue;
      property HasValue: Boolean read FHasValue;
    end;
  private const
    CallbackBufferLength = 3;
  private
    FPreviewCriticalSection: TCriticalSection;
    FOrientationChangedId: Integer;
    FCamera: JCamera;
    FCameraThread: JHandlerThread;
    FCameraId: Integer;
    FCapturing: Boolean;
    FOutputBufferRotation: Integer;
    FCaptureSetting: TNullable<TVideoCaptureSetting>;
    FFlashMode: TNullable<TFlashMode>;
    FFocusMode: TNullable<TFocusMode>;
    FTorchMode: TNullable<TTorchMode>;
    FCameraSize: TSize;
    FPreviewSize: TSize;
    FSurfaceTexture: JSurfaceTexture;
    FCallbackBuffers: array[0..CallbackBufferLength - 1] of TJavaArray<Byte>;
    FRenderScriptContext: JRenderScript;
    FRenderScript: JScriptC_YUVtoRGBA;
    FInputAllocation: JAllocation;
    FOutputAllocation: JAllocation;
    FOutputBufferLength: Integer;
    FOutputBuffer: TJavaArray<Byte>;
    FPreviewCallback: TAndroidVideoCaptureCallback;

    function GetOutputBufferRotation: Integer;
    procedure UpdateOutputBuffer(const AData: TJavaArray<Byte>; const ACamera: JCamera);
    function GetCamera: JCamera;
    procedure OrientationChangedHandler(const Sender: TObject; const Msg: TMessage);
    procedure UpdateQualityParameters(const AParams: JCamera_Parameters);
    procedure UpdateFlashModeParameter(const AParams: JCamera_Parameters);
    procedure UpdateFocusModeParameter(const AParams: JCamera_Parameters);
    procedure UpdateTorchModeParameter(const AParams: JCamera_Parameters);
    procedure SetAutoFocus;
    procedure CreateInputAllocation;
    procedure CreateOutputAllocation;
    procedure CreateCallbackBuffers;
    procedure UpdatePreviewSize;
    procedure DestroyInputAllocation;
    procedure DestroyOutputAllocation;
    procedure DestroyCallbackBuffers;
    procedure CopyOutputBufferToBitmap(const ABitmap: TBitmap);
    procedure CameraRequestPermissionsResult(const Permissions: TClassicStringDynArray; const GrantResults: TClassicPermissionStatusDynArray);

    class procedure CopyBitmap(const ASrcBitmap, ADstBitmap: TBitmap); static;
  protected
    procedure DoStartCapture; override;
    procedure DoStopCapture; override;
    procedure DoSampleBufferToBitmap(const ABitmap: TBitmap; const ASetSize: Boolean); override;
    function GetDeviceProperty(const Prop: TCaptureDevice.TProperty): string; override;
    function GetDeviceState: TCaptureDeviceState; override;
    function GetPosition: TDevicePosition; override;
    procedure DoSetQuality(const AQuality: TVideoCaptureQuality); override;
    function GetHasFlash: Boolean; override;
    function GetFlashMode: TFlashMode; override;
    procedure SetFlashMode(const AFlashMode: TFlashMode); override;
    function GetHasTorch: Boolean; override;
    function GetTorchMode: TTorchMode; override;
    procedure SetTorchMode(const ATorchMode: TTorchMode); override;
    function GetFocusMode: TFocusMode; override;
    procedure SetFocusMode(const AFocusMode: TFocusMode); override;
    function GetCaptureSetting: TVideoCaptureSetting; override;
    function DoSetCaptureSetting(const ACaptureSetting: TVideoCaptureSetting): Boolean; override;
    function DoGetAvailableCaptureSettings: TArray<TVideoCaptureSetting>; override;
    procedure DoRequestPermission; override;
  public
    property CameraId: Integer read FCameraId;
    property Camera: JCamera read GetCamera;

    destructor Destroy; override;
  end;

{$ENDREGION}
{$REGION 'TAndroidCaptureDeviceManager'}

constructor TAndroidCaptureDeviceManager.Create;
var
  I: Integer;
  CameraDevice: TAndroidVideoCaptureDevice;
begin
  inherited;

  TAndroidAudioCaptureDevice.Create(Self, True);

  for I := 0 to TJCamera.JavaClass.getNumberOfCameras - 1 do
  begin
    CameraDevice := TAndroidVideoCaptureDevice.Create(Self, I = 0);
    CameraDevice.FCameraId := I;
  end;
end;

{$ENDREGION}
{$REGION 'TAndroidAudioCaptureDevice'}

destructor TAndroidAudioCaptureDevice.Destroy;
begin
  FRecorder := nil;

  inherited;
end;

procedure TAndroidAudioCaptureDevice.DoStartCapture;
begin
  if PermissionsService.IsPermissionGranted(JStringToString(TJManifest_permission.JavaClass.RECORD_AUDIO)) then
  begin
    FRecorder := TJMediaRecorder.JavaClass.init;
    FRecorder.setAudioSource(TJMediaRecorder_AudioSource.JavaClass.MIC);
    FRecorder.setOutputFormat(TJMediaRecorder_OutputFormat.JavaClass.THREE_GPP);
    FRecorder.setAudioEncoder(TJMediaRecorder_AudioEncoder.JavaClass.AMR_NB);
    FRecorder.setOutputFile(StringToJString(FileName));
    FRecorder.prepare;
    FRecorder.start;
  end
  else
    raise EPermissionException.CreateFmt(SRequiredPermissionsAreAbsent, ['RECORD_AUDIO']);
end;

procedure TAndroidAudioCaptureDevice.DoStopCapture;
begin
  if FRecorder <> nil then
  begin
    FRecorder.stop;
    FRecorder := nil;
  end;
end;

function TAndroidAudioCaptureDevice.GetDeviceState: TCaptureDeviceState;
begin
  if FRecorder <> nil then
    Result := TCaptureDeviceState.Capturing
  else
    Result := TCaptureDeviceState.Stopped;
end;

procedure TAndroidAudioCaptureDevice.RecorderRequestPermissionsResult(const Permissions: TClassicStringDynArray;
  const GrantResults: TClassicPermissionStatusDynArray);
begin
  if Length(GrantResults) = 1 then
  begin
    if GrantResults[0] = TPermissionStatus.Granted then
      DoPermissionRequest('', True)
    else
      DoPermissionRequest(SUserRejectedCaptureDevicePermission, False);
  end
  else
    DoPermissionRequest(SPermissionsRequestHasBeenCancelled, False);
end;

procedure TAndroidAudioCaptureDevice.DoRequestPermission;
begin
  PermissionsService.RequestPermissions([JStringToString(TJManifest_permission.JavaClass.RECORD_AUDIO)], RecorderRequestPermissionsResult);
end;

{$ENDREGION}
{$REGION 'TAndroidVideoCaptureDevice'}

constructor TAndroidVideoCaptureCallback.Create(const ACaptureDevice: TAndroidVideoCaptureDevice);
begin
  inherited Create;

  FCaptureDevice := ACaptureDevice;
end;

procedure TAndroidVideoCaptureCallback.onPreviewFrame(AData: TJavaArray<Byte>; ACamera: JCamera);
begin
  // This function is called on the camera thread.
  FCaptureDevice.UpdateOutputBuffer(AData, ACamera);
  ACamera.addCallbackBuffer(AData);
end;

procedure TAndroidVideoCaptureDevice.TNullable<T>.SetValue(AValue: T);
begin
  FValue := AValue;
  FHasValue := True;
end;

destructor TAndroidVideoCaptureDevice.Destroy;
begin
  DoStopCapture;

  inherited;
end;

function TAndroidVideoCaptureDevice.GetCamera: JCamera;

  function OpenCamera: JCamera;
  var
    WaitEvent: TEvent;
    Runnable: JRunnable;
    Camera: JCamera;
    Handler: JHandler;
  begin
    WaitEvent := TEvent.Create;

    Runnable := TRunnable.Create(
      procedure
      begin
        // It is needed to call the 'Camera.open' function on the camera thread, as it causes the
        // 'Camera.PreviewCallback.onPreviewFrame' function to be also called on the camera thread.
        // This approach is aimed to reduce the pressure on the main thread.
        Camera := TJCamera.JavaClass.open(FCameraId);

        WaitEvent.SetEvent;
      end);

    Handler := TJHandler.JavaClass.init(FCameraThread.getLooper);
    Handler.post(Runnable);

    WaitEvent.WaitFor;
    WaitEvent.Free;

    Result := Camera;
  end;

begin
  if not PermissionsService.IsPermissionGranted(JStringToString(TJManifest_permission.JavaClass.CAMERA)) then
    raise EPermissionException.CreateFmt(SRequiredPermissionsAreAbsent, ['CAMERA']);

  if FCameraThread = nil then
  begin
    FCameraThread := TJHandlerThread.JavaClass.init(StringToJString('CameraThread'));
    FCameraThread.start;
  end;

  if FCamera = nil then
    FCamera := OpenCamera;

  Result := FCamera;
end;

function TAndroidVideoCaptureDevice.GetDeviceProperty(const Prop: TCaptureDevice.TProperty): string;
begin
  case Prop of
    TCaptureDevice.TProperty.UniqueID:
      Result := FCameraId.ToString;
    else
      Result := string.Empty;
  end;
end;

function TAndroidVideoCaptureDevice.GetDeviceState: TCaptureDeviceState;
begin
  if FCapturing then
    Result := TCaptureDeviceState.Capturing
  else
    Result := TCaptureDeviceState.Stopped;
end;

procedure TAndroidVideoCaptureDevice.DoSetQuality(const AQuality: TVideoCaptureQuality);
var
  Params: JCamera_Parameters;
  SettingsList: TArray<TVideoCaptureSetting>;
  SavedPriority: TVideoCaptureSettingPriority;
begin
  if AQuality <> TVideoCaptureQuality.CaptureSettings then
  begin
    SavedPriority := CaptureSettingPriority;
    try
      CaptureSettingPriority := TVideoCaptureSettingPriority.Resolution;
      SettingsList := AvailableCaptureSettings;
    finally
      CaptureSettingPriority := SavedPriority;
    end;

    Params := Camera.getParameters;
    if Params = nil then
      Exit;

    inherited;

    if Length(SettingsList) > 0 then
      case AQuality of
        TVideoCaptureQuality.PhotoQuality: FCaptureSetting.Value := SettingsList[0];
        TVideoCaptureQuality.HighQuality: FCaptureSetting.Value := SettingsList[0];
        TVideoCaptureQuality.MediumQuality: FCaptureSetting.Value := SettingsList[Length(SettingsList) div 2];
        TVideoCaptureQuality.LowQuality: FCaptureSetting.Value := SettingsList[High(SettingsList)];
      end;

    UpdateQualityParameters(Params);
    Camera.setParameters(Params);
  end;
end;

function TAndroidVideoCaptureDevice.GetPosition: TDevicePosition;
var
  CameraInfo: JCamera_CameraInfo;
begin
  CameraInfo := TJCamera_CameraInfo.JavaClass.init;
  TJCamera.JavaClass.getCameraInfo(CameraId, CameraInfo);

  if CameraInfo.facing = TJCamera_CameraInfo.JavaClass.CAMERA_FACING_BACK then
    Result := TDevicePosition.Back
  else if CameraInfo.facing = TJCamera_CameraInfo.JavaClass.CAMERA_FACING_FRONT then
    Result := TDevicePosition.Front
  else
    Result := TDevicePosition.Unspecified;
end;

function TAndroidVideoCaptureDevice.GetCaptureSetting: TVideoCaptureSetting;
var
  Params: JCamera_Parameters;
  Size: JCamera_Size;
begin
  if FCaptureSetting.HasValue then
    Result := FCaptureSetting.Value
  else
  begin
    Result := TVideoCaptureSetting.Create;
    Params := Camera.getParameters;
    if Params <> nil then
    begin
      Size := Params.getPreviewSize;
      Result := TVideoCaptureSetting.Create(Size.width, Size.height, Params.getPreviewFrameRate);
    end;
  end;
end;

function TAndroidVideoCaptureDevice.GetHasFlash: Boolean;
var
  Params: JCamera_Parameters;
  ModeList: JList;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit(False);

  ModeList := Params.getSupportedFlashModes;

  if ModeList = nil then
    Exit(False);

  Result := ModeList.contains(TJCamera_Parameters.JavaClass.FLASH_MODE_ON) or
    ModeList.contains(TJCamera_Parameters.JavaClass.FLASH_MODE_AUTO);
end;

function TAndroidVideoCaptureDevice.GetHasTorch: Boolean;
var
  Params: JCamera_Parameters;
  ModeList: JList;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit(False);

  ModeList := Params.getSupportedFlashModes;

  if ModeList = nil then
    Exit(False);

  Result := ModeList.contains(TJCamera_Parameters.JavaClass.FLASH_MODE_TORCH);
end;

function TAndroidVideoCaptureDevice.GetFlashMode: TFlashMode;
var
  Params: JCamera_Parameters;
  FlashMode: JString;
  FlashModeText: string;
begin
  if FFlashMode.HasValue then
    Result := FFlashMode.Value
  else
  begin
    Params := Camera.getParameters;
    if Params = nil then
      Exit(inherited);

    FlashMode := Params.getFlashMode;
    if FlashMode = nil then
      Exit(inherited);

    FlashModeText := JStringToString(FlashMode);

    if SameText(FlashModeText, JStringToString(TJCamera_Parameters.JavaClass.FLASH_MODE_ON)) then
      Result := TFlashMode.FlashOn
    else if SameText(FlashModeText, JStringToString(TJCamera_Parameters.JavaClass.FLASH_MODE_AUTO)) then
      Result := TFlashMode.AutoFlash
    else
      Result := TFlashMode.FlashOff;
  end;
end;

procedure TAndroidVideoCaptureDevice.SetFlashMode(const AFlashMode: TFlashMode);
var
  Params: JCamera_Parameters;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit;

  FFlashMode.Value := AFlashMode;

  UpdateFlashModeParameter(Params);
  Camera.setParameters(Params);
end;

function TAndroidVideoCaptureDevice.GetFocusMode: TFocusMode;
var
  Params: JCamera_Parameters;
  FocusMode: JString;
  FocusModeText: string;
begin
  if FFocusMode.HasValue then
    Result := FFocusMode.Value
  else
  begin
    Params := Camera.getParameters;
    if Params = nil then
      Exit(inherited);

    FocusMode := Params.getFocusMode;
    if FocusMode = nil then
      Exit(inherited);

    FocusModeText := JStringToString(FocusMode);

    if SameText(FocusModeText, JStringToString(TJCamera_Parameters.JavaClass.FOCUS_MODE_AUTO)) then
      Result := TFocusMode.AutoFocus
    else if SameText(FocusModeText, JStringToString(TJCamera_Parameters.JavaClass.FOCUS_MODE_CONTINUOUS_VIDEO)) then
      Result := TFocusMode.ContinuousAutoFocus
    else if SameText(FocusModeText, JStringToString(TJCamera_Parameters.JavaClass.FOCUS_MODE_CONTINUOUS_PICTURE)) then
      Result := TFocusMode.ContinuousAutoFocus
    else
      Result := TFocusMode.Locked;
  end;
end;

procedure TAndroidVideoCaptureDevice.SetFocusMode(const AFocusMode: TFocusMode);
var
  Params: JCamera_Parameters;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit;

  FFocusMode.Value := AFocusMode;

  UpdateFocusModeParameter(Params);
  Camera.setParameters(Params);

  SetAutoFocus;
end;

function TAndroidVideoCaptureDevice.GetTorchMode: TTorchMode;
var
  Params: JCamera_Parameters;
  FlashMode: JString;
begin
  if FTorchMode.HasValue then
    Result := FTorchMode.Value
  else
  begin
    Params := Camera.getParameters;
    if Params = nil then
      Exit(inherited);

    FlashMode := Params.getFlashMode;
    if FlashMode = nil then
      Exit(inherited);

    if SameText(JStringToString(FlashMode), JStringToString(TJCamera_Parameters.JavaClass.FLASH_MODE_TORCH)) then
      Result := TTorchMode.ModeOn
    else
      Result := TTorchMode.ModeOff
  end;
end;

procedure TAndroidVideoCaptureDevice.SetTorchMode(const ATorchMode: TTorchMode);
var
  Params: JCamera_Parameters;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit;

  if ATorchMode = TTorchMode.ModeAuto then
    Exit; // The 'TTorchMode.ModeAuto' enum case is not supported on the Android platform.

  FTorchMode.Value := ATorchMode;

  UpdateTorchModeParameter(Params);
  Camera.setParameters(Params);
end;

function TAndroidVideoCaptureDevice.GetOutputBufferRotation: Integer;
var
  CameraInfo: JCamera_CameraInfo;
  Display: JDisplay;
  DisplayOrientation: Integer;
begin
  CameraInfo := TJCamera_CameraInfo.JavaClass.init;
  TJCamera.JavaClass.getCameraInfo(FCameraId, CameraInfo);

  Display := TAndroidHelper.Display;
  if Display = nil then
    Exit(0);

  case Display.getRotation of
    0: // TJSurface.JavaClass.ROTATION_0
      DisplayOrientation := 0;
    1: // TJSurface.JavaClass.ROTATION_90
      DisplayOrientation := 90;
    2: // TJSurface.JavaClass.ROTATION_180
      DisplayOrientation := 180;
    3: // TJSurface.JavaClass.ROTATION_270
      DisplayOrientation := 270;
  else
    Exit(0);
  end;

  if CameraInfo.facing = TJCamera_CameraInfo.JavaClass.CAMERA_FACING_FRONT then
    Result := (DisplayOrientation + CameraInfo.orientation) mod 360
  else
    Result := (360 + CameraInfo.orientation - DisplayOrientation) mod 360;
end;

procedure TAndroidVideoCaptureDevice.CreateInputAllocation;
var
  InputTypeBuilder: JType_Builder;
begin
  InputTypeBuilder := TJType_Builder.JavaClass.init(FRenderScriptContext, TJrenderscript_Element.JavaClass.U8(FRenderScriptContext))
    .setX(FCameraSize.Width)
    .setY(FCameraSize.Height)
    .setYuvFormat(TJImageFormat.JavaClass.NV21);
  FInputAllocation := TJAllocation.JavaClass.createTyped(FRenderScriptContext, InputTypeBuilder.create, TJAllocation.JavaClass.USAGE_SCRIPT);
end;

procedure TAndroidVideoCaptureDevice.CreateOutputAllocation;
var
  OutputTypeBuilder: JType_Builder;
begin
  OutputTypeBuilder := TJType_Builder.JavaClass.init(FRenderScriptContext, TJrenderscript_Element.JavaClass.RGBA_8888(FRenderScriptContext))
    .setX(FPreviewSize.Width)
    .setY(FPreviewSize.Height);
  FOutputAllocation := TJAllocation.JavaClass.createTyped(FRenderScriptContext, OutputTypeBuilder.create, TJAllocation.JavaClass.USAGE_SCRIPT);
end;

procedure TAndroidVideoCaptureDevice.CreateCallbackBuffers;
var
  Length: Integer;
  I: Integer;
begin
  Length := FCameraSize.Width * FCameraSize.Height * TJImageFormat.JavaClass.getBitsPerPixel(TJImageFormat.JavaClass.NV21);
  for I := 0 to CallbackBufferLength - 1 do
  begin
    FCallbackBuffers[I] := TJavaArray<Byte>.Create(Length);
    Camera.addCallbackBuffer(FCallbackBuffers[I]);
  end;
end;

procedure TAndroidVideoCaptureDevice.UpdatePreviewSize;
begin
  FOutputBufferRotation := GetOutputBufferRotation;
  if (FOutputBufferRotation = 90) or (FOutputBufferRotation = 270) then
    FPreviewSize := TSize.Create(FCameraSize.Height, FCameraSize.Width)
  else
    FPreviewSize := FCameraSize;
end;

procedure TAndroidVideoCaptureDevice.DoStartCapture;

  procedure UpdateCameraParameters(const AParams: JCamera_Parameters);
  begin
    UpdateQualityParameters(AParams);
    UpdateFlashModeParameter(AParams);
    UpdateFocusModeParameter(AParams);
    UpdateTorchModeParameter(AParams);
  end;

var
  Params: JCamera_Parameters;
  NativePreviewSize: JCamera_Size;
begin
  if FCapturing then
    Exit;

  Params := Camera.getParameters;
  if Params = nil then
    Exit;

  // Workaround for Google Glass
  if TPlatformServices.Current.GlobalFlags.ContainsKey(EnableGlassFPSWorkaround) and
    TPlatformServices.Current.GlobalFlags[EnableGlassFPSWorkaround] then
    Params.setPreviewFpsRange(30000, 30000);

  UpdateCameraParameters(Params);
  Camera.setParameters(Params);

  FOrientationChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TOrientationChangedMessage, OrientationChangedHandler);

  FPreviewCriticalSection := TCriticalSection.Create;

  NativePreviewSize := Params.getPreviewSize;
  FCameraSize := TSize.Create(NativePreviewSize.width, NativePreviewSize.height);
  UpdatePreviewSize;

  FOutputBufferLength := FCameraSize.Width * FCameraSize.Height * 4;
  FOutputBuffer := TJavaArray<Byte>.Create(FOutputBufferLength);

  FRenderScriptContext := TJRenderScript.JavaClass.create(TAndroidHelper.Context);
  FRenderScript := TJScriptC_YUVtoRGBA.JavaClass.init(FRenderScriptContext);

  CreateInputAllocation;
  CreateOutputAllocation;

  FRenderScript.set_Input(FInputAllocation);
  FRenderScript.set_Width(FCameraSize.Width);
  FRenderScript.set_Height(FCameraSize.Height);

  FSurfaceTexture := TJSurfaceTexture.JavaClass.init(0);

  FPreviewCallback := TAndroidVideoCaptureCallback.Create(Self);

  Camera.setPreviewTexture(FSurfaceTexture);
  CreateCallbackBuffers;
  Camera.setPreviewCallbackWithBuffer(FPreviewCallback);
  Camera.startPreview;
  SetAutoFocus;

  FCapturing := True;
end;

procedure TAndroidVideoCaptureDevice.DestroyInputAllocation;
begin
  FInputAllocation.destroy;
  FInputAllocation := nil;
end;

procedure TAndroidVideoCaptureDevice.DestroyOutputAllocation;
begin
  FOutputAllocation.destroy;
  FOutputAllocation := nil;
end;

procedure TAndroidVideoCaptureDevice.DestroyCallbackBuffers;
var
  I: Integer;
begin
  for I := 0 to CallbackBufferLength - 1 do
    FCallbackBuffers[I].Free;
end;

procedure TAndroidVideoCaptureDevice.DoStopCapture;
begin
  if not FCapturing then
    Exit;

  FCamera.stopPreview;
  FCamera.setPreviewCallbackWithBuffer(nil);
  FCamera.setPreviewTexture(nil);
  FCamera.release;
  FCamera := nil;

  FCameraThread.quit;
  FCameraThread := nil;

  FreeAndNil(FOutputBuffer);
  FOutputBufferLength := 0;

  FSurfaceTexture.release;
  FSurfaceTexture := nil;

  DestroyCallbackBuffers;
  DestroyOutputAllocation;
  DestroyInputAllocation;

  FRenderScript.destroy;
  FRenderScript := nil;
  FRenderScriptContext := nil;

  FreeAndNil(FPreviewCriticalSection);

  TMessageManager.DefaultManager.Unsubscribe(TOrientationChangedMessage, FOrientationChangedId);

  FCapturing := False;
end;

procedure TAndroidVideoCaptureDevice.UpdateQualityParameters(const AParams: JCamera_Parameters);
const
  JPEGQualityKey = 'jpeg-quality';
  HighestJpegQuality = 100;
  MediumJpegQuality = 75;
  LowestJpegQuality = 50;
begin
  case Quality of
    TVideoCaptureQuality.PhotoQuality: ;
    TVideoCaptureQuality.HighQuality:
    begin
      AParams.setPictureFormat(TJImageFormat.JavaClass.JPEG);
      AParams.&set(StringToJString(JPEGQualityKey), HighestJpegQuality);
      AParams.setJpegQuality(HighestJpegQuality);
    end;
    TVideoCaptureQuality.MediumQuality:
    begin
      AParams.setPictureFormat(TJImageFormat.JavaClass.JPEG);
      AParams.&set(StringToJString(JPEGQualityKey), MediumJpegQuality);
      AParams.setJpegQuality(MediumJpegQuality);
    end;
    TVideoCaptureQuality.LowQuality:
    begin
      AParams.setPictureFormat(TJImageFormat.JavaClass.JPEG);
      AParams.&set(StringToJString(JPEGQualityKey), LowestJpegQuality);
      AParams.setJpegQuality(LowestJpegQuality);
    end;
  end;

  if FCaptureSetting.HasValue then
  begin
    AParams.setPreviewSize(FCaptureSetting.Value.Width, FCaptureSetting.Value.Height);
    AParams.setPreviewFrameRate(Round(FCaptureSetting.Value.FrameRate));
  end;
end;

procedure TAndroidVideoCaptureDevice.UpdateFlashModeParameter(const AParams: JCamera_Parameters);
begin
  if FFlashMode.HasValue then
  begin
    case FFlashMode.Value of
      TFlashMode.AutoFlash: AParams.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_AUTO);
      TFlashMode.FlashOff: AParams.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_OFF);
      TFlashMode.FlashOn: AParams.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_ON);
    end;
  end;
end;

procedure TAndroidVideoCaptureDevice.UpdateFocusModeParameter(const AParams: JCamera_Parameters);
begin
  if FFocusMode.HasValue then
  begin
    case FFocusMode.Value of
      TFocusMode.AutoFocus: AParams.setFocusMode(TJCamera_Parameters.JavaClass.FOCUS_MODE_AUTO);
      TFocusMode.ContinuousAutoFocus: AParams.setFocusMode(TJCamera_Parameters.JavaClass.FOCUS_MODE_CONTINUOUS_PICTURE);
      TFocusMode.Locked: AParams.setFocusMode(TJCamera_Parameters.JavaClass.FOCUS_MODE_FIXED);
    else
      AParams.setFocusMode(TJCamera_Parameters.JavaClass.FOCUS_MODE_CONTINUOUS_PICTURE);
    end;
  end;
end;

procedure TAndroidVideoCaptureDevice.UpdateTorchModeParameter(const AParams: JCamera_Parameters);
begin
  if FTorchMode.HasValue then
  begin
    case FTorchMode.Value of
      TTorchMode.ModeOff: AParams.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_OFF);
      TTorchMode.ModeOn: AParams.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_TORCH);
    end;
  end;
end;

procedure TAndroidVideoCaptureDevice.SetAutoFocus;
begin
  if FFocusMode.HasValue then
  begin
    try
      if FFocusMode.Value = TFocusMode.AutoFocus then
        Camera.autoFocus(nil)
      else
        Camera.cancelAutoFocus;
    except on E: EJNIException do
      Log.d(SMediaCannotUseAutofocus, [E.ToString]);
    end;
  end;
end;

procedure TAndroidVideoCaptureDevice.UpdateOutputBuffer(const AData: TJavaArray<Byte>; const ACamera: JCamera);
begin
  FInputAllocation.copyFromUnchecked(AData);

  FPreviewCriticalSection.Acquire;
  try
    case FOutputBufferRotation of
      0: FRenderScript.forEach_ProcessFrameRotated0Degrees(FOutputAllocation);
      90: FRenderScript.forEach_ProcessFrameRotated90Degrees(FOutputAllocation);
      180: FRenderScript.forEach_ProcessFrameRotated180Degrees(FOutputAllocation);
      270: FRenderScript.forEach_ProcessFrameRotated270Degrees(FOutputAllocation);
    end;

    FOutputAllocation.copyTo(FOutputBuffer);
  finally
    FPreviewCriticalSection.Release;
  end;

  TThread.Synchronize(nil,
    procedure
    begin
      SampleBufferReady(0);
    end);
end;

procedure TAndroidVideoCaptureDevice.CopyOutputBufferToBitmap(const ABitmap: TBitmap);
var
  BitmapData: TBitmapData;
begin
  if ABitmap.Map(TMapAccess.Write, BitmapData) then
    try
      Assert(BitmapData.Pitch * BitmapData.Height = FOutputBufferLength,
        'Output buffer and bitmap are not compatible for copying');

      Move(FOutputBuffer.Data^, BitmapData.Data^, FOutputBufferLength);
    finally
      ABitmap.Unmap(BitmapData);
    end;
end;

class procedure TAndroidVideoCaptureDevice.CopyBitmap(const ASrcBitmap, ADstBitmap: TBitmap);
var
  BitmapScale: Single;
  SrcRect, DstRect: TRectF;
begin
  BitmapScale := ADstBitmap.BitmapScale;
  ADstBitmap.BitmapScale := 1;
  try
    if ADstBitmap.Canvas.BeginScene then
      try
        SrcRect := TRectF.Create(0, 0, ASrcBitmap.Width, ASrcBitmap.Height);
        DstRect := TRectF.Create(0, 0, ADstBitmap.Width, ADstBitmap.Height);
        ADstBitmap.Canvas.DrawBitmap(ASrcBitmap, SrcRect, DstRect, 1);
      finally
        ADstBitmap.Canvas.EndScene;
      end;
  finally
    ADstBitmap.BitmapScale := BitmapScale;
  end;
end;

procedure TAndroidVideoCaptureDevice.DoSampleBufferToBitmap(const ABitmap: TBitmap; const ASetSize: Boolean);
var
  ScaledBitmap: TBitmap;
begin
  if not FCapturing then
    Exit;

  FPreviewCriticalSection.Acquire;
  try
    if ASetSize then
      ABitmap.SetSize(FPreviewSize.Width, FPreviewSize.Height);

    if (ABitmap.Width = FPreviewSize.Width) and (ABitmap.Height = FPreviewSize.Height) then
      CopyOutputBufferToBitmap(ABitmap)
    else
    begin
      ScaledBitmap := TBitmap.Create(FPreviewSize.Width, FPreviewSize.Height);
      try
        CopyOutputBufferToBitmap(ScaledBitmap);
        CopyBitmap(ScaledBitmap, ABitmap);
      finally
        ScaledBitmap.Free;
      end;
    end;
  finally
    FPreviewCriticalSection.Release;
  end;
end;

function TAndroidVideoCaptureDevice.DoSetCaptureSetting(const ACaptureSetting: TVideoCaptureSetting): Boolean;
var
  Params: JCamera_Parameters;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit(False);

  FCaptureSetting.Value := ACaptureSetting;

  UpdateQualityParameters(Params);
  Camera.setParameters(Params);

  Result := True;
end;

procedure TAndroidVideoCaptureDevice.OrientationChangedHandler(const Sender: TObject; const Msg: TMessage);
begin
  FPreviewCriticalSection.Acquire;
  try
    UpdatePreviewSize;

    // Upon detecting that the application-user rotated the device, it is needed to recreate the
    // 'FOutputAllocation' field to use the correct dimensions for the camera preview.
    DestroyOutputAllocation;
    CreateOutputAllocation;
  finally
    FPreviewCriticalSection.Release;
  end;
end;

function TAndroidVideoCaptureDevice.DoGetAvailableCaptureSettings: TArray<TVideoCaptureSetting>;
var
  Params: JCamera_Parameters;
  Size: JCamera_Size;
  SizeList, FramerateList: JList;
  I, J: Integer;
  List: TList<TVideoCaptureSetting>;
  Setting: TVideoCaptureSetting;
begin
  SetLength(Result, 0);
  Params := Camera.getParameters;
  if Params <> nil then
  begin
    List := TList<TVideoCaptureSetting>.Create;
    try
      SizeList := Params.getSupportedPreviewSizes;
      FramerateList := Params.getSupportedPreviewFrameRates;
      for I := 0 to SizeList.size - 1 do
      begin
        Size := TJCamera_Size.Wrap(SizeList.get(I));
        for J := 0 to FramerateList.size - 1 do
        begin
          Setting := TVideoCaptureSetting.Create(Size.width, Size.height, TJInteger.Wrap(FramerateList.get(J)).intValue);
          List.Add(Setting);
        end;
      end;
      Result := List.ToArray;
    finally
      List.Free;
    end;
  end;
end;

procedure TAndroidVideoCaptureDevice.CameraRequestPermissionsResult(const Permissions: TClassicStringDynArray;
  const GrantResults: TClassicPermissionStatusDynArray);
begin
  if Length(GrantResults) = 1 then
  begin
    if GrantResults[0] = TPermissionStatus.Granted then
      DoPermissionRequest('', True)
    else
      DoPermissionRequest(SUserRejectedCaptureDevicePermission, False);
  end
  else
    DoPermissionRequest(SPermissionsRequestHasBeenCancelled, False);
end;

procedure TAndroidVideoCaptureDevice.DoRequestPermission;
begin
  PermissionsService.RequestPermissions([JStringToString(TJManifest_permission.JavaClass.CAMERA)], CameraRequestPermissionsResult);
end;

{$ENDREGION}
{$REGION 'TAndroidMedia'}

constructor TAndroidMedia.Create(const AFileName: string);
var
  AudioService: JObject;
  AudioManager: JAudioManager;
  MaxVolume: Integer;
begin
  inherited Create(AFileName);
  FPlayer := TJMediaPlayer.JavaClass.init;
  FPlayer.setDataSource(StringToJString(FileName));
  FPlayer.prepare;
  AudioService := TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE);
  if AudioService <> nil then
    AudioManager := TJAudioManager.Wrap(TAndroidHelper.JObjectToID(AudioService));
  if AudioManager <> nil then
  begin
    MaxVolume := AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
    FVolume := AudioManager.getStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
    if MaxVolume > 0 then
      FVolume := FVolume / MaxVolume;
    FVolume := Min(FVolume, 1);
  end;
end;

destructor TAndroidMedia.Destroy;
begin
  FPlayer.release;
  FPlayer := nil;
  inherited Destroy;
end;

function TAndroidMedia.GetCurrent: TMediaTime;
begin
  Result := AndroidMediaTimeScale * FPlayer.getCurrentPosition;
end;

function TAndroidMedia.GetDuration: TMediaTime;
begin
  Result := AndroidMediaTimeScale * FPlayer.getDuration;
end;

function TAndroidMedia.GetMediaState: TMediaState;
begin
  if FPlayer.isPlaying then
    Result := TMediaState.Playing
  else
    Result := TMediaState.Stopped;
end;

function TAndroidMedia.GetVideoSize: TPointF;
begin
  Result := TPointF.Create(FPlayer.getVideoWidth, FPlayer.getVideoHeight)
end;

function TAndroidMedia.GetVolume: Single;
begin
  Result := FVolume
end;

function TAndroidMedia.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := FPlayer.QueryInterface(IID, Obj);
end;

procedure TAndroidMedia.SeekToBegin;
begin
  FPlayer.seekTo(0);
end;

procedure TAndroidMedia.SetCurrent(const Value: TMediaTime);
begin
  FPlayer.seekTo(Max(0, Round(Value / AndroidMediaTimeScale)));
end;

procedure TAndroidMedia.SetVolume(const Value: Single);
begin
  FVolume := EnsureRange(Value, 0, 1);
  FPlayer.setVolume(FVolume, FVolume);
end;

procedure TAndroidMedia.UpdateMediaFromControl;
begin
end;

procedure TAndroidMedia.DoStop;
begin
  FPlayer.pause;
end;

procedure TAndroidMedia.DoPlay;
begin
  FPlayer.start;
end;

{$ENDREGION}
{$REGION 'TAndroidMediaCodec'}

function TAndroidMediaCodec.CreateFromFile(const AFileName: string): TMedia;
begin
  Result := TAndroidMedia.Create(AFileName);
end;

{$ENDREGION}

{$REGION 'TAndroidVideoCodec'}

function TAndroidVideoCodec.CreateFromFile(const AFileName: string): TMedia;
begin
  Result := TAndroidVideo.Create(AFileName);
end;
{$ENDREGION}

{$REGION 'TAndroidVideo'}

constructor TAndroidVideo.Create(const AFileName: string);
const
  DefaultScale = 1;
var
  ScreenSrv: IFMXScreenService;
begin
  FVolume := TCommonVolume.Create;
  FVideoEnabled := False;
  inherited Create(AFileName);

  CheckVideo;

  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenSrv) then
    FScale := ScreenSrv.GetScreenScale
  else
    FScale := DefaultScale;
end;

function TAndroidVideo.AllAssigned: Boolean;
begin
  Result := FVideoPlayer <> nil;
end;

procedure TAndroidVideo.CheckVideo;
const
  CYes = 'yes';
var
  MMR : JMediaMetadataRetriever;
  HasVideoValue: string;
begin
  if TOSVersion.Check(4, 0) and FileExists(FileName) then
  begin
    MMR := TJMediaMetadataRetriever.JavaClass.init;
    MMR.setDataSource(StringToJString(FileName));
    HasVideoValue := JStringTOString(MMR.extractMetadata(TJMediaMetadataRetriever.JavaClass.METADATA_KEY_HAS_VIDEO));
    FVideoEnabled := CYes = HasVideoValue;
    MMR := nil;
  end
  else
    FVideoEnabled := Control <> nil;
end;

procedure TAndroidVideo.RealignView;

  procedure UpdateViewBounds;
  var
    OriginalVideoRect: TRectF;
    FitRect: TRectF;
    HMargins: Single;
    VMargins: Single;
    LP: JRelativeLayout_LayoutParams;
  begin
    OriginalVideoRect := TRectF.Create(0, 0, FVideoSize.Width, FVideoSize.Height);
    FitRect := OriginalVideoRect.FitInto(Control.LocalRect);
    if not FitRect.IsEmpty then
    begin
      HMargins := (Control.Width - FitRect.Width) / 2;
      VMargins := (Control.Height - FitRect.Height) / 2;

      LP := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT,
                                                         TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_TOP, TJRelativeLayout.JavaClass.TRUE);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_BOTTOM, TJRelativeLayout.JavaClass.TRUE);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_LEFT, TJRelativeLayout.JavaClass.TRUE);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_RIGHT, TJRelativeLayout.JavaClass.TRUE);

      if not FitRect.IsEmpty then
      begin
        LP.leftMargin := Round(HMargins * FScale);
        LP.topMargin := Round(VMargins * FScale);
        LP.rightMargin := Round(HMargins * FScale);
        LP.bottomMargin := Round(VMargins * FScale);
      end;

      FVideoPlayer.setLayoutParams(LP);
    end;
  end;

var
  SavedCurrentPosition: Integer;
  SavedIsStarted: Boolean;
begin
  SavedCurrentPosition := FVideoPlayer.getCurrentPosition;
  SavedIsStarted := FVideoPlayer.isPlaying;

  if ZOrderManager <> nil then
  begin
    ZOrderManager.UpdateOrderAndBounds(Control);
    // VideoView is placed inside Container. ZOrderManager controls visibility of Container based on TMediaPlayerControl.
    // But the Android doesn't hide VideoView, if parent is hidden. It's a bug in Android VideoView, so we need to update
    // visibility of VideoView also.
    if Control.ParentedVisible then
      FVideoPlayer.setVisibility(TJView.JavaClass.VISIBLE)
    else
      FVideoPlayer.setVisibility(TJView.JavaClass.GONE);
  end;
  FVideoPlayer.seekTo(SavedCurrentPosition);
  if SavedIsStarted then
    FVideoPlayer.start;

  UpdateViewBounds;
end;

procedure TAndroidVideo.RetreiveVideoSize;
var
  MediaPlayer: JMediaPlayer;
begin
  MediaPlayer := TJMediaPlayer.JavaClass.init;
  MediaPlayer.setDataSource(StringToJString(FileName));
  MediaPlayer.prepare;
  FVideoSize := TSize.Create(MediaPlayer.getVideoWidth, MediaPlayer.getVideoHeight);
  MediaPlayer := nil;
end;

destructor TAndroidVideo.Destroy;

  procedure RemoveContainer;
  var
    Container: JViewGroup;
  begin
    if FVideoPlayer.getParent <> nil then
    begin
      Container := TJViewGroup.Wrap(FVideoPlayer.getParent);
      Container.removeView(FVideoPlayer);

      if Container.getParent <> nil then
        TJViewGroup.Wrap(Container.getParent).removeView(FVideoPlayer);
    end;
  end;

var
  LInstance: TVideoInstance;
begin
  if ZOrderManager <> nil then
    ZOrderManager.RemoveLink(Control);
  if FVideoPlayer <> nil then
  begin
    RemoveContainer;

    LInstance.VideoPlayer := FVideoPlayer;
    VideoPool.UIReturnInstance(LInstance);
  end;
  inherited Destroy;
end;

procedure TAndroidVideo.DoPlay;
begin
  inherited;
  if IsVideoEnabled then
    FVideoPlayer.start
  else
    FJustAudio.DoPlay;
end;

procedure TAndroidVideo.DoStop;
begin
  inherited;
  if IsVideoEnabled then
    FVideoPlayer.pause
  else
    FJustAudio.DoStop;
end;

function TAndroidVideo.GetCurrent: TMediaTime;
begin
  Result := 0;
  if IsVideoEnabled then
  begin
    if AllAssigned then
      Result := FVideoPlayer.getCurrentPosition * AndroidMediaTimeScale;
  end
  else
    Result := FJustAudio.GetCurrent;
end;

function TAndroidVideo.GetDuration: TMediaTime;
begin
  Result := 0;
  if IsVideoEnabled then
  begin
    if AllAssigned then
      Result := FVideoPlayer.getDuration * AndroidMediaTimeScale;
  end
  else
    Result := FJustAudio.GetDuration;
end;

function TAndroidVideo.GetMediaState: TMediaState;
begin
  if IsVideoEnabled then
  begin
    if FVideoPlayer <> nil then
    begin
      if FVideoPlayer.isPlaying then
        Result := TMediaState.Playing
      else
        Result := TMediaState.Stopped;
    end
    else
      Result := TMediaState.Unavailable;
  end
  else
    Result := FJustAudio.GetMediaState;
end;

function TAndroidVideo.GetVideoSize: TPointF;
begin
  if IsVideoEnabled then
    Result := TPointF.Create(FVideoSize.Width, FVideoSize.Height)
  else
    Result := TPointF.Zero;
end;

function TAndroidVideo.GetVolume: Single;
begin
  if IsVideoEnabled then
    Result := FVolume.Value
  else
    Result := FJustAudio.GetVolume;
end;

function TAndroidVideo.GetZOrderManager: TAndroidZOrderManager;
var
  Form: TCommonCustomForm;
begin
  if (Control <> nil) and (Control.Root <> nil) and (Control.Root.GetObject is TCommonCustomForm) then
  begin
    Form := TCommonCustomForm(Control.Root);
    Result := WindowHandleToPlatform(Form.Handle).ZOrderManager;
  end
  else
    Result := nil;
end;

procedure TAndroidVideo.InitInstance;
var
  LFileName: string;
  LInstance: TVideoInstance;
  Container: JRelativeLayout;
  LP: JRelativeLayout_LayoutParams;
begin
  LFileName := FileName;
  if FVideoEnabled then
  begin
    RetreiveVideoSize;
    if FVideoPlayer = nil then
    begin
      LInstance := VideoPool.UIGetInstance;
      FVideoPlayer := LInstance.VideoPlayer;
      FVideoPlayer.setVisibility(TJView.JavaClass.VISIBLE);
      FVideoPlayer.setVideoPath(StringToJString(LFileName));

      Container := TJRelativeLayout.JavaClass.init(TAndroidHelper.Context);
      LP := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT,
                                                         TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_TOP, TJRelativeLayout.JavaClass.TRUE);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_BOTTOM, TJRelativeLayout.JavaClass.TRUE);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_LEFT, TJRelativeLayout.JavaClass.TRUE);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_RIGHT, TJRelativeLayout.JavaClass.TRUE);
      Container.addView(FVideoPlayer, LP);

      ZOrderManager.AddOrSetLink(Control, Container, nil);
      ZOrderManager.UpdateOrderAndBounds(Control);
    end;
    RealignView;
  end
  else
    FJustAudio := TAndroidMedia.Create(FileName);
end;

function TAndroidVideo.InstanceCreated: Boolean;
begin
  Result := (FJustAudio <> nil) or (FVideoPlayer <> nil);
end;

procedure TAndroidVideo.SeekToBegin;
begin
  if IsVideoEnabled then
  begin
    if AllAssigned then
    begin
      FVideoPlayer.stopPlayback;
      FVideoPlayer.seekTo(0);
    end;
  end
  else
    FJustAudio.SeekToBegin;
end;

procedure TAndroidVideo.SetCurrent(const Value: TMediaTime);
begin
  inherited;
  if IsVideoEnabled then
  begin
    if AllAssigned then
      FVideoPlayer.seekTo(Round(Value / AndroidMediaTimeScale));
  end
  else
    FJustAudio.SetCurrent(Value);
end;

procedure TAndroidVideo.SetVolume(const Value: Single);
begin
  inherited;
  if IsVideoEnabled then
    FVolume.Value := Value
  else
    FJustAudio.SetVolume(Value);
end;

procedure TAndroidVideo.UpdateMediaFromControl;
begin
  inherited;
  if IsVideoEnabled then
    RealignView
  else
    FJustAudio.UpdateMediaFromControl;
end;

function TAndroidVideo.IsVideoEnabled: Boolean;
begin
  if not InstanceCreated then
    InitInstance;
  Result := FVideoEnabled and (Control <> nil);
end;

function TAndroidVideo.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if Supports(FVideoPlayer, IID, Obj) then
    Result := S_OK
  else
    Result := E_NOTIMPL;
end;

{$ENDREGION}

{ TAndroidVideo.TVolume }

constructor TAndroidVideo.TCommonVolume.Create;
begin
  FAudioService := TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE);
  if FAudioService <> nil then
    FAudioManager := TJAudioManager.Wrap(TAndroidHelper.JObjectToID(FAudioService));
  if FAudioManager <> nil then
    FMaxVolume := FAudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
end;

function TAndroidVideo.TCommonVolume.GetVolume: Single;
begin
  if FMaxVolume = 0 then
    Result := 0
  else
    Result := Min(1, FAudioManager.getStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC) / FMaxVolume);
end;

procedure TAndroidVideo.TCommonVolume.SetVolume(const Value: Single);
begin
  if FAudioManager <> nil then
    FAudioManager.setStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC, Round(Value * FMaxVolume), 0);
end;

{ TVideoPool }

constructor TVideoPool.Create;
begin
  FUsed := TList<TVideoInstance>.Create;
  FReadyToUse := TList<TVideoInstance>.Create;
end;

procedure TVideoPool.CreateOneMoreInstance;
var
  LItem: TVideoInstance;
begin
  LItem.VideoPlayer := TJVideoView.JavaClass.init(TAndroidHelper.Activity);
  LItem.VideoPlayer.requestFocus(0);
  LItem.VideoPlayer.setZOrderOnTop(True);
  LItem.VideoPlayer.setZOrderMediaOverlay(True);
  FReadyToUse.Add(LItem);
end;

destructor TVideoPool.Destroy;
var
  LItem: TVideoInstance;
begin
  while FUsed.Count > 0 do
  begin
    LItem := FUsed.First;
    VideoPool.UIReturnInstance(LItem);
  end;
  FUsed.Free;
  FReadyToUse.Free;
  inherited;
end;

procedure TVideoPool.UIFreezeInstance(const AInstance: TVideoInstance);
begin
  AInstance.VideoPlayer.stopPlayback;
  AInstance.VideoPlayer.setVisibility(TJView.JavaClass.INVISIBLE);
end;

function TVideoPool.UIGetInstance: TVideoInstance;
begin
  if FReadyToUse.Count = 0 then
    CreateOneMoreInstance;
  Result := FReadyToUse.First;
  FReadyToUse.Remove(Result);
  FUsed.Add(Result);
end;

procedure TVideoPool.UIReturnInstance(const AInstance: TVideoInstance);
begin
  UIFreezeInstance(AInstance);
  FUsed.Remove(AInstance);
  FReadyToUse.Add(AInstance);
end;

initialization
  VideoPool := TVideoPool.Create;
  TMediaCodecManager.RegisterMediaCodecClass('.mov', SVMOVFiles, TMediaType.Video, TAndroidVideoCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.m4v', SVM4VFiles, TMediaType.Video, TAndroidVideoCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.mp4', SVMP4Files, TMediaType.Video, TAndroidVideoCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.3gp', SV3GPFiles, TMediaType.Video, TAndroidVideoCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.mp3', SVMP3Files, TMediaType.Audio, TAndroidMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.caf', SVCAFFiles, TMediaType.Audio, TAndroidMediaCodec);

  TMediaCodecManager.RegisterMediaCodecClass(SAllFilesExt, SDefault, TMediaType.Video, TAndroidMediaCodec);

finalization
  VideoPool.Free;

  TJRenderScript.JavaClass.releaseAllContexts;

end.
