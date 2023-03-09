{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2012-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Media.Mac;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, Macapi.ObjectiveC, Macapi.CocoaTypes, Macapi.AVFoundation, Macapi.CoreMedia,
  FMX.Helpers.Mac, FMX.Media, FMX.Media.AVFoundation, FMX.Graphics;

type
  /// <summary>AVFoundation delegate specialization for OSX.</summary>
  TAVVideoSampleDelegate = class(TVideoSampleDelegateBase, AVCaptureVideoDataOutputSampleBufferDelegate)
  public
    /// <summary>Constructor with a device.</summary>
    constructor Create(const ACaptureDevice: TAVVideoCaptureDeviceBase);
    /// <summary>Delegate selector for captured video buffer.</summary>
    [MethodName('captureOutput:didOutputSampleBuffer:fromConnection:')]
    procedure captureOutput(captureOutput: AVCaptureOutput; didOutputSampleBuffer: CMSampleBufferRef;
      fromConnection: AVCaptureConnection); cdecl;
    [MethodName('captureOutput:didDropSampleBuffer:fromConnection:')]
    procedure captureOutputDidDropSampleBuffer(output: AVCaptureOutput; sampleBuffer: CMSampleBufferRef; fromConnection: AVCaptureConnection); cdecl;
  end;

implementation

uses
  Macapi.Foundation, Macapi.Dispatch, Macapi.CoreVideo, Macapi.CoreGraphics, Macapi.CoreFoundation, Macapi.AppKit,
  Macapi.Helpers, Macapi.QuartzCore, FMX.Consts, FMX.Types, FMX.Types3D, FMX.Forms, FMX.Canvas.Mac, FMX.Platform.Mac,
  System.Variants, System.UITypes, System.Classes, System.SysUtils, System.Math, System.Generics.Collections,
  System.RTLConsts, System.IOUtils, Posix.StdDef;

const
  PresetCaptureWidth: array[0..7] of Integer = (160, 320, 352, 640, 720, 960, 1280, 1920);
  PresetCaptureHeight: array[0..7] of Integer = (120, 240, 288, 480, 480, 540, 720, 1080);
  PresetCaptureFramerate: array[0..7] of Integer = (30, 30, 30, 30, 30, 30, 30, 30);
  DefaultPresetCaptureWidthIndex = 3;
  DefaultPresetCaptureHeightIndex = 3;
  DefaultPresetCaptureFramerateIndex = 3;

type
  TMacMedia = class(TMedia)
  private
    FPixelBufferBitmap: TBitmap;
    FPlayer: AVPlayer;
    FPlayerItem: AVPlayerItem;
    FPlayerLayer: AVPlayerLayer;
    FPlayerVideoOutput: AVPlayerItemVideoOutput;
    FVideoView: NSView;
    function GetAspectRatio: Single;
    function GetVideoTrack: AVAssetTrack;
    procedure SetupVideoOutput;
  protected
    procedure DoPlay; override;
    procedure DoStop; override;
    function GetCurrent: TMediaTime; override;
    function GetDuration: TMediaTime; override;
    function GetMediaState: TMediaState; override;
    function GetVideoSize: TPointF; override;
    function GetVolume: Single; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    procedure SetCurrent(const Value: TMediaTime); override;
    procedure SetVolume(const Value: Single); override;
    procedure UpdateMediaFromControl; override;
    property PlayerVideoOutput: AVPlayerItemVideoOutput read FPlayerVideoOutput;
  public
    constructor Create(const AFileName: string); override;
    destructor Destroy; override;
  end;

  TMacMediaCodec = class(TCustomMediaCodec)
  public
    function CreateFromFile(const AFileName: string): TMedia; override;
  end;

{ TVideoSampleDelegate }

constructor TAVVideoSampleDelegate.Create(const ACaptureDevice: TAVVideoCaptureDeviceBase);
begin
  inherited;
  FIsProcessingCapture := False;
end;

procedure TAVVideoSampleDelegate.captureOutput(captureOutput: AVCaptureOutput; didOutputSampleBuffer: CMSampleBufferRef;
  fromConnection: AVCaptureConnection);
begin
  FIsProcessingCapture := True;
  try
    FCaptureDevice.SampleDelegate(didOutputSampleBuffer);
  finally
    FIsProcessingCapture := False;
  end;
end;

procedure TAVVideoSampleDelegate.captureOutputDidDropSampleBuffer(output: AVCaptureOutput; sampleBuffer: CMSampleBufferRef; fromConnection: AVCaptureConnection);
begin
  // Ignore dropped frame message
end;

{ TMacMedia }

constructor TMacMedia.Create(const AFileName: string);
var
  LURL: NSUrl;
  LAbsoluteFileName: string;
begin
  inherited Create(AFileName);
  AVMediaTypeAudio; // Force load the framework
  if FileExists(FileName) then
  begin
    if ExtractFilePath(FileName).IsEmpty then
      LAbsoluteFileName := TPath.Combine(TPath.GetHomePath, FileName)
    else
      LAbsoluteFileName := FileName;
    LURL := TNSUrl.Wrap(TNSUrl.OCClass.fileURLWithPath(StrToNSStr(LAbsoluteFileName)));
  end
  else
    LURL := StrToNSUrl(FileName);
  if LURL = nil then
    raise EFileNotFoundException.Create(SSpecifiedFileNotFound);
  FPixelBufferBitmap := TBitmap.Create;
  FPlayerItem := TAVPlayerItem.Wrap(TAVPlayerItem.OCClass.playerItemWithURL(LURL));
  FPlayerItem.retain;
  FPlayer := TAVPlayer.Wrap(TAVPlayer.OCClass.playerWithPlayerItem(FPlayerItem));
  FPlayer.retain;
  FPlayerLayer := TAVPlayerLayer.Wrap(TAVPlayerLayer.OCClass.playerLayerWithPlayer(FPlayer));
  FPlayerLayer.retain;
  FPlayerLayer.setVideoGravity(CocoaNSStringConst(libAVFoundation, 'AVLayerVideoGravityResizeAspectFill'));
  FPlayerLayer.setAutoresizingMask(kCALayerWidthSizable or kCALayerHeightSizable);
  FVideoView := TNSView.Create;
  FVideoView.retain;
  FVideoView.setWantsLayer(True);
  FVideoView.layer.addSublayer(FPlayerLayer);
  SetupVideoOutput;
end;

destructor TMacMedia.Destroy;
begin
  if FPlayerVideoOutput <> nil then
    FPlayerVideoOutput.release;
  FPlayerVideoOutput := nil;
  FPixelBufferBitmap.Free;
  if FVideoView <> nil then
  begin
    FVideoView.setHidden(True);
    FVideoView.release;
  end;
  FVideoView := nil;
  if FPlayerLayer <> nil then
  begin
    FPlayerLayer.removeFromSuperlayer;
    FPlayerLayer.release;
  end;
  FPlayerLayer := nil;
  if FPlayer <> nil then
    FPlayer.release;
  FPlayer := nil;
  if FPlayerItem <> nil then
    FPlayerItem.release;
  FPlayerItem := nil;
  inherited;
end;

procedure TMacMedia.DoPlay;
begin
  if FPlayer <> nil then
  begin
    FPlayer.play;
    UpdateMediaFromControl;
  end;
end;

procedure TMacMedia.DoStop;
begin
  if FPlayer <> nil then
    FPlayer.pause;
end;

function TMacMedia.GetAspectRatio: Single;
var
  LTrack: AVAssetTrack;
begin
  Result := 0;
  LTrack := GetVideoTrack;
  if LTrack <> nil then
    Result := LTrack.naturalSize.width / LTrack.naturalSize.height;
end;

function TMacMedia.GetCurrent: TMediaTime;
begin
  if FPlayerItem.currentTime.timeScale <> 0 then
    Result := Trunc(FPlayerItem.currentTime.Value / FPlayerItem.currentTime.timeScale * MediaTimeScale)
  else
    Result := 0;
end;

function TMacMedia.GetDuration: TMediaTime;
begin
  if FPlayerItem.duration.timeScale <> 0 then
    Result := Trunc(FPlayerItem.duration.Value / FPlayerItem.duration.timeScale * MediaTimeScale)
  else
    Result := 0;
end;

function TMacMedia.GetMediaState: TMediaState;
begin
  if FPlayer.status <> AVPlayerStatusFailed then
  begin
    if (FPlayer.Rate > 0) then
      Result := TMediaState.Playing
    else
      Result := TMediaState.Stopped
  end
  else
    Result := TMediaState.Unavailable;
end;

function TMacMedia.GetVideoSize: TPointF;
begin
  Result := PointF(FPlayerItem.presentationSize.width, FPlayerItem.presentationSize.height)
end;

function TMacMedia.GetVideoTrack: AVAssetTrack;
var
  LTracks: NSArray;
begin
  Result := nil;
  if FPlayer.currentItem <> nil then
  begin
    LTracks := FPlayer.currentItem.asset.tracksWithMediaType(AVMediaTypeVideo);
    if LTracks.count > 0 then
      Result := TAVAssetTrack.Wrap(LTracks.objectAtIndex(0));
  end;
end;

function TMacMedia.GetVolume: Single;
begin
  Result := FPlayer.volume;
end;

function TMacMedia.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOTIMPL;
  if (FVideoView <> nil) and (FVideoView.QueryInterface(IID, Obj) = S_OK) then
    Exit(S_OK);
  if (FPlayer <> nil) and (FPlayer.QueryInterface(IID, Obj) = S_OK) then
    Exit(S_OK);
  if (FPlayerLayer <> nil) and (FPlayerLayer.QueryInterface(IID, Obj) = S_OK) then
    Exit(S_OK);
  if (FPlayerItem <> nil) and (FPlayerItem.QueryInterface(IID, Obj) = S_OK) then
    Exit(S_OK);
end;

procedure TMacMedia.SetCurrent(const Value: TMediaTime);
begin
  FPlayer.seekToTime(CMTimeMake(Value, MediaTimeScale));
end;

procedure TMacMedia.SetupVideoOutput;
var
  LPixelBufferAttributes: NSDictionary;
  LPixelFormatType: Pointer;
begin
  LPixelFormatType := TNSNumber.OCClass.numberWithInt(kCVPixelFormatType_32BGRA);
  LPixelBufferAttributes := TNSDictionary.Wrap(TNSDictionary.OCClass.dictionaryWithObject(LPixelFormatType, Pointer(kCVPixelBufferPixelFormatTypeKey)));
  FPlayerVideoOutput := TAVPlayerItemVideoOutput.Create;
  FPlayerVideoOutput.retain;
  FPlayerVideoOutput.initWithPixelBufferAttributes(LPixelBufferAttributes);
  FPlayerVideoOutput.requestNotificationOfMediaDataChangeWithAdvanceInterval(0.1);
  FPlayerItem.addOutput(FPlayerVideoOutput);
end;

procedure TMacMedia.SetVolume(const Value: Single);
begin
  FPlayer.setVolume(Value);
end;

procedure TMacMedia.UpdateMediaFromControl;
var
  LForm: TCommonCustomForm;
  LFormView: NSView;
  LBounds: TRectF;
  LMultiplier, LDiff: Single;
begin
  if (Control <> nil) and (Control.Root <> nil) and (Control.Root.GetObject is TCommonCustomForm) then
  begin
    LForm := TCommonCustomForm(Control.Root.GetObject);
    if Control.ParentedVisible then
      LBounds := Control.AbsoluteRect
    else
      // Move it into invisible area - Making it invisible by calling setHidden will stop playing media
      LBounds := TRectF.Create(-2, -2, -1, -1);
    if GetAspectRatio <> 0 then
    begin
      LMultiplier := (LBounds.Width / LBounds.Height) / GetAspectRatio;
      if LMultiplier < 1 then
      begin
        LDiff := (LBounds.Height - (LBounds.Height * LMultiplier)) / 2;
        LBounds.Top := LBounds.Top + LDiff;
        LBounds.Bottom := LBounds.Bottom - LDiff;
      end
      else if LMultiplier > 1 then
      begin
        LDiff := (LBounds.Width - (LBounds.Width * (1 / LMultiplier))) / 2;
        LBounds.Left := LBounds.Left + LDiff;
        LBounds.Right := LBounds.Right - LDiff;
      end;
    end;
    LFormView := WindowHandleToPlatform(LForm.Handle).View;
    LFormView.addSubview(FVideoView);
    FVideoView.setFrame(MakeNSRect(LBounds.Left, LForm.ClientHeight - LBounds.Bottom, LBounds.Width, LBounds.Height));
    FPlayerLayer.setFrame(FVideoView.bounds);
    FVideoView.setHidden(False);
  end
  else
    FVideoView.setHidden(True);
end;

{ TMacMediaCodec }

function TMacMediaCodec.CreateFromFile(const AFileName: string): TMedia;
begin
  Result := TMacMedia.Create(AFileName);
end;

initialization
  TMediaCodecManager.RegisterMediaCodecClass('.mov', SVMOVFiles, TMediaType.Video, TMacMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.m4v', SVM4VFiles, TMediaType.Video, TMacMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.mp4', SVMP4Files, TMediaType.Video, TMacMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.avi', SVAviFiles, TMediaType.Video, TMacMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.mpg', SVMPGFiles, TMediaType.Video, TMacMediaCodec);

  TMediaCodecManager.RegisterMediaCodecClass('.wav', SVWAVFiles, TMediaType.Audio, TMacMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.m4a', SVM4AFiles, TMediaType.Audio, TMacMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.caf', SVCAFFiles, TMediaType.Audio, TMacMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.mp3', SVMP3Files, TMediaType.Audio, TMacMediaCodec);

  TMediaCodecManager.RegisterMediaCodecClass(SAllFilesExt, SDefault, TMediaType.Video, TMacMediaCodec);
end.
