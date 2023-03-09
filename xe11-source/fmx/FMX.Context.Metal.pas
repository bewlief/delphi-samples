{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Context.Metal;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.UITypes, System.Math.Vectors, System.Generics.Collections, Macapi.Metal, Macapi.MetalKit,
{$IF defined(IOS)}
  iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.QuartzCore, FMX.Platform.iOS,
{$ELSE}
  Macapi.Foundation, Macapi.CocoaTypes, Macapi.QuartzCore, FMX.Platform.Mac,
{$ENDIF}
  FMX.Types, FMX.Types3D, FMX.Graphics;

type

  TCustomContextMetal = class(TContext3D)
  private
    class var FMaxTextureSize: Integer;
    class var FSharedDevice: MTLDevice;
    class procedure InitMaxTextureSize;
  protected
    class function GetSharedDevice: MTLDevice; static;
    class procedure CreateSharedDevice; virtual;
    class procedure DestroySharedDevice; virtual;
  public
    class property SharedDevice: MTLDevice read GetSharedDevice;
    class function PixelFormat: TPixelFormat; override;
    class function MaxTextureSize: Integer; override;
    class function Valid: Boolean; override;
    class function MaxLightCount: Integer; override;
    class function IsMetalSupported: Boolean; virtual;
  end;

{ TContextMetal }

  TPipelineStateConfiguration = record
    ColorWriteMask: MTLColorWriteMask;
    SampleCount: Integer;
    BlendingEnabled: Boolean;
    VertexShaderHandle: THandle;
    PixelShaderHandle: THandle;
    VertexDeclaration: array [0..15] of TVertexElement;
    procedure Reset;
  end;

  TDepthStencilStateConfiguration = record
    DepthCompareFunction: MTLCompareFunction;
    DepthWriteEnabled: Boolean;
    StencilEnabled: Boolean;
    StencilFailureOperation: MTLStencilOperation;
    DepthFailureOperation: MTLStencilOperation;
    DepthStencilPassOperation: MTLStencilOperation;
    StencilCompareFunction: MTLCompareFunction;
    StencilMask: LongWord;
    procedure Reset;
  end;

  TSamplerStateConfiguration = record
    MinFilter: NSUInteger;
    MagFilter: NSUInteger;
    MipFilter: NSUInteger;
    procedure Reset;
  end;

  TContextMetal = class(TCustomContextMetal)
  private
    class var FShaderHandleGenerator: Integer;
    // Metal state objects are immutable and safe to use across threads.
    class var FPipelineStates: TDictionary<TPipelineStateConfiguration, MTLRenderPipelineState>;
    class var FDepthStencilStates: TDictionary<TDepthStencilStateConfiguration, MTLDepthStencilState>;
    class var FSamplerStates: TDictionary<TSamplerStateConfiguration, MTLSamplerState>;
  private
    FPipelineStateConfiguration: TPipelineStateConfiguration;
    FDepthStencilStateConfiguration: TDepthStencilStateConfiguration;
    FCommandQueue: MTLCommandQueue;
    FCommandBuffer: MTLCommandBuffer;
    FRenderPassDescriptor: MTLRenderPassDescriptor;
    FRenderCommandEncoder: MTLRenderCommandEncoder;
    FCurrentDrawable: CAMetalDrawable;
    FOnScreenTexture: MTLTexture;
    FStencilReferenceValue: LongWord;
    FSampleCount: NSUInteger;
    class function GetPipelineState(const AConfiguration: TPipelineStateConfiguration): MTLRenderPipelineState;
    class function CreatePipelineState(const AConfiguration: TPipelineStateConfiguration): MTLRenderPipelineState;
    class function GetDepthStencilState(const AConfiguration: TDepthStencilStateConfiguration): MTLDepthStencilState;
    class function CreateDepthStencilState(const AConfiguration: TDepthStencilStateConfiguration): MTLDepthStencilState;
    class function GetSamplerState(const AConfiguration: TSamplerStateConfiguration): MTLSamplerState;
    class function CreateSamplerState(const AConfiguration: TSamplerStateConfiguration): MTLSamplerState;
    procedure CreateRenderPassDescriptor;
    procedure DestroyRenderPassDescriptor;
    procedure CreateRenderCommandEncoder;
    function GetCurrentDrawable: CAMetalDrawable;
  protected
    { buffer }
    FDepthStencilBufferTexture: MTLTexture;
    FMultisampleBufferTexture: MTLTexture;
    procedure DoResize; override;
    procedure DoCreateBuffer; override;
    procedure DoFreeBuffer; override;
    procedure DoCopyToBitmap(const Dest: TBitmap; const ARect: TRect); override;
    procedure DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect); override;
    { rendering }
    function DoBeginScene: Boolean; override;
    procedure DoEndScene; override;
    procedure DoClear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: Single;
      const AStencil: Cardinal); override;
    { states }
    class procedure CreateSharedStates;
    class procedure DestroySharedStates;
    procedure DoSetContextState(AState: TContextState); override;
    procedure DoSetStencilOp(const Fail, ZFail, ZPass: TStencilOp); override;
    procedure DoSetStencilFunc(const Func: TStencilfunc; Ref, Mask: Cardinal); override;
    { scissor }
    procedure DoSetScissorRect(const ScissorRect: TRect); override;
    { drawing }
    procedure DoDrawPrimitivesBatch(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
      const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize,
      IndexCount: Integer); override;
    { texture }
    class procedure DoInitializeTexture(const Texture: TTexture); override;
    class procedure DoFinalizeTexture(const Texture: TTexture); override;
    class procedure DoUpdateTexture(const Texture: TTexture; const Bits: Pointer; const Pitch: Integer); override;
    { bitmap }
    class function DoBitmapToTexture(const Bitmap: TBitmap): TTexture; override;
    { shaders }
    class procedure DoInitializeShader(const Shader: TContextShader); override;
    class procedure DoFinalizeShader(const Shader: TContextShader); override;
    procedure DoSetShaders(const VertexShader, PixelShader: TContextShader); override;
    procedure DoSetShaderVariable(const Name: string; const Data: array of TVector3D); override;
    procedure DoSetShaderVariable(const Name: string; const Texture: TTexture); override;
    procedure DoSetShaderVariable(const Name: string; const Matrix: TMatrix3D); override;
    { constructors }
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AMultisample: TMultisample; const ADepthStencil: Boolean); override;
    constructor CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
      const ADepthStencil: Boolean); override;
    procedure InitContext; override;
  public
    { destructor }
    destructor Destroy; override;
  end;

function MultisampleTypeToNumber(const AMultisample: TMultisample): Integer;
function GetSupportedMultisample(const ADesiredMultisample: Integer): Integer;
function PixelFormatToMTLPixelFormat(const ASource: TPixelFormat): MTLPixelFormat;

procedure RegisterContextClasses;
procedure UnregisterContextClasses;

implementation

uses
  System.SysUtils, System.Math, Macapi.ObjectiveC, Macapi.Helpers, FMX.Consts, FMX.Canvas.GPU;

function MultisampleTypeToNumber(const AMultisample: TMultisample): Integer;
begin
  case AMultisample of
    TMultisample.TwoSamples:
      Result := 2;

    TMultisample.FourSamples:
      Result := 4;

    else
      Result := 1;
  end;
end;

function GetSupportedMultisample(const ADesiredMultisample: Integer): Integer;
begin
  Result := ADesiredMultisample;
  if Result <= 1 then
    Exit;
  while not TCustomContextMetal.SharedDevice.supportsTextureSampleCount(Result) do
  begin
    if Result > 8 then
      Result := 8
    else if Result > 4 then
      Result := 4
    else if Result > 2 then
      Result := 2
    else
    begin
      Result := 1;
      Break;
    end;
  end;
end;

function PixelFormatToMTLPixelFormat(const ASource: TPixelFormat): MTLPixelFormat;
begin
  case ASource of
    TPixelFormat.RGBA: Result := MTLPixelFormatRGBA8Unorm;
    TPixelFormat.BGRA: Result := MTLPixelFormatBGRA8Unorm;
    TPixelFormat.RGBA16: Result := MTLPixelFormatRGBA16Unorm;
    TPixelFormat.BGR_565: Result := MTLPixelFormatB5G6R5Unorm;
    TPixelFormat.BGR5_A1: Result := MTLPixelFormatBGR5A1Unorm;
    TPixelFormat.BGR10_A2: Result := MTLPixelFormatBGR10A2Unorm;
    TPixelFormat.RGB10_A2: Result := MTLPixelFormatRGB10A2Unorm;
    TPixelFormat.A: Result := MTLPixelFormatA8Unorm;
    TPixelFormat.R16F: Result := MTLPixelFormatR16Float;
    TPixelFormat.RG16F: Result := MTLPixelFormatRG16Float;
    TPixelFormat.RGBA16F: Result := MTLPixelFormatRGBA16Float;
    TPixelFormat.R32F: Result := MTLPixelFormatR32Float;
    TPixelFormat.RG32F: Result := MTLPixelFormatRG32Float;
    TPixelFormat.RGBA32F: Result := MTLPixelFormatRGBA32Float;
    else
      Result := MTLPixelFormatInvalid;
  end;
end;

function MakeMTLClearColor(const ARed: Double; const AGreen: Double; const ABlue: Double;
  const AAlpha: Double): MTLClearColor;
begin
  Result.red := ARed;
  Result.green := AGreen;
  Result.blue := ABlue;
  Result.alpha := AAlpha;
end;

function MakeMTLScissorRect(const ARect: TRect; const AScale: Single): MTLScissorRect;
begin
  Result.x := Round(ARect.Left * AScale);
  Result.y := Round(ARect.Top * AScale);
  Result.width :=  Round(ARect.Width * AScale);
  Result.height := Round(ARect.Height * AScale);
end;

{ TCustomContextMetal }

class procedure TCustomContextMetal.CreateSharedDevice;
begin
  if FSharedDevice = nil then
  begin
    FSharedDevice := TMTLDevice.Wrap(MTLCreateSystemDefaultDevice);
    if FSharedDevice = nil then
      raise EContext3DException.CreateResFmt(@SCannotCreateMetalContext, [ClassName]);
    InitMaxTextureSize;
  end;
end;

class procedure TCustomContextMetal.DestroySharedDevice;
begin
  if FSharedDevice <> nil then
  begin
    FSharedDevice.release;
    FSharedDevice := nil;
  end;
end;

class function TCustomContextMetal.GetSharedDevice: MTLDevice;
begin
  CreateSharedDevice;
  Result := FSharedDevice;
end;

class function TCustomContextMetal.PixelFormat: TPixelFormat;
begin
  Result := TPixelFormat.BGRA;
end;

class function TCustomContextMetal.Valid: Boolean;
begin
  Result := FSharedDevice <> nil;
end;

class procedure TCustomContextMetal.InitMaxTextureSize;
var
  LFeatureSet: MTLFeatureSet;
begin
  // https://developer.apple.com/metal/Metal-Feature-Set-Tables.pdf
  // if a GPU supports a feature set, it supports all features provided by
  // earlier members of the same family, and all features in earlier software revisions.
  {$IF defined(IOS)}
  LFeatureSet := MTLFeatureSet_iOS_GPUFamily3_v1;
  {$ELSE}
  LFeatureSet := MTLFeatureSet_macOS_GPUFamily1_v1;
  {$ENDIF}

  if FSharedDevice.supportsFeatureSet(LFeatureSet) then
    FMaxTextureSize := 16384
  else
    FMaxTextureSize := 8192;
end;

class function TCustomContextMetal.MaxTextureSize: Integer;
begin
  Result := FMaxTextureSize;
end;

class function TCustomContextMetal.MaxLightCount: Integer;
begin
  // the number of parameters we can pass to a shader are limited to 30. With more than 4 lights we receive the error
  // 'buffer' attribute parameter is out of bounds: must be between 0 and 30
  Result := 4;
end;

class function TCustomContextMetal.IsMetalSupported: Boolean;
{$IF defined(MACOS64)}
var
  LDevice: MTLDevice;
  LFeatureSet: MTLFeatureSet;
{$ENDIF}
begin
  {$IF not defined(MACOS64)}
    Result := False;
  {$ELSE}
    {$IF defined(IOS)}
    LFeatureSet := MTLFeatureSet_iOS_GPUFamily1_v1;
    {$ELSE}
    LFeatureSet := MTLFeatureSet_macOS_GPUFamily1_v1;
    {$ENDIF}

    LDevice := TMTLDevice.Wrap(MTLCreateSystemDefaultDevice);
    try
      Result := (LDevice <> nil) and
                (LDevice.supportsFeatureSet(LFeatureSet));
    finally
      LDevice.release;
      LDevice := nil;
    end;
  {$ENDIF}
end;

{ TPipelineStateConfiguration }

procedure TPipelineStateConfiguration.Reset;
begin
  ColorWriteMask := MTLColorWriteMaskAll;
  SampleCount := 1;
  BlendingEnabled := True;
  VertexShaderHandle := 0;
  PixelShaderHandle := 0;
  FillChar(VertexDeclaration, SizeOf(VertexDeclaration), 0);
end;

{ TDepthStencilStateConfiguration }

procedure TDepthStencilStateConfiguration.Reset;
begin
  // The default value is MTLCompareFunctionAlways, which indicates that the depth test always passes and
  // the fragment remains a candidate to replace the data at the specified location.
  DepthCompareFunction := MTLCompareFunctionAlways;
  // The default value is NO, which indicates the depth attachment is read-only.
  DepthWriteEnabled := False;
  // The default value is NO, which indicates the stencil test is disabled for the front-facing & back-facing
  // primitives.
  StencilEnabled := False;
  // The default value is MTLStencilOperationKeep, which does not change the current stencil value.
  StencilFailureOperation := MTLStencilOperationKeep;
  // The default value is MTLStencilOperationKeep, which does not change the current stencil value.
  DepthFailureOperation := MTLStencilOperationKeep;
  // The default value is MTLStencilOperationKeep, which does not change the current stencil value.
  DepthStencilPassOperation := MTLStencilOperationKeep;
  // The default value is MTLCompareFunctionAlways, which indicates that the stencil test always passes.
  StencilCompareFunction := MTLCompareFunctionAlways;
  // The default value is all ones. A logical AND operation with the default readMask does not change the value.
  StencilMask := 255;
end;

{ TSamplerStateConfiguration }

procedure TSamplerStateConfiguration.Reset;
begin
  MinFilter := MTLSamplerMinMagFilterNearest;
  MagFilter := MTLSamplerMinMagFilterNearest;
  MipFilter := MTLSamplerMipFilterNotMipmapped;
end;

{ TContextMetal }

class function TContextMetal.CreatePipelineState(const AConfiguration: TPipelineStateConfiguration):
  MTLRenderPipelineState;

  procedure CheckError(const AErrorPtr: Pointer; const ARaiseException: Boolean);
  var
    LError: NSError;
  begin
    if (AErrorPtr <> nil) then
    begin
      LError := TNSError.Wrap(AErrorPtr);
      if ARaiseException then
        raise EContext3DException.Create(NSStrToStr(LError.localizedDescription))
      else
        Log.d(NSStrToStr(LError.localizedDescription));
    end;
  end;

  function BuildVertexDeclarationStr: string;
  var
    LVertexElement: TVertexElement;
    LMetalVertexDeclaration: TStringBuilder;
    LCurrOffset: Integer;
    I: Integer;
  begin
    LMetalVertexDeclaration := TStringBuilder.Create;
    try
      LCurrOffset := 0;
      for I := Low(AConfiguration.VertexDeclaration) to High(AConfiguration.VertexDeclaration) do
      begin
        LVertexElement := AConfiguration.VertexDeclaration[i];
        if LCurrOffset <> LVertexElement.Offset then
        begin
          while LVertexElement.Offset - LCurrOffset > 0 do
          begin
            LMetalVertexDeclaration.Append('uchar4 pad');                   // Size = 4  bytes | Alignment = 4 bytes
            LMetalVertexDeclaration.Append(LCurrOffset);
            LMetalVertexDeclaration.Append(';');
            Inc(LCurrOffset, 4);
          end;
          if LCurrOffset <> LVertexElement.Offset then
          begin
            if LVertexElement.Offset = 0 then
              break
            else
              raise EContext3DException.CreateResFmt(@SCannotCreateShader, [ClassName]);
          end;
        end;
        case LVertexElement.Format of
          TVertexFormat.Vertex:
          begin
            LMetalVertexDeclaration.Append('packed_float3 position;');      // Size = 12 bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 12);
          end;
          TVertexFormat.Normal:
          begin
            LMetalVertexDeclaration.Append('packed_float3 normal;');        // Size = 12 bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 12);
          end;
          TVertexFormat.Color0:
          begin
            LMetalVertexDeclaration.Append('uchar4 color0;');               // Size = 4  bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 4);
          end;
          TVertexFormat.Color1:
          begin
            LMetalVertexDeclaration.Append('uchar4 color1;');               // Size = 4  bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 4);
          end;
          TVertexFormat.Color2:
          begin
            LMetalVertexDeclaration.Append('uchar4 color2;');               // Size = 4  bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 4);
          end;
          TVertexFormat.Color3:
          begin
            LMetalVertexDeclaration.Append('uchar4 color3;');               // Size = 4  bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 4);
          end;
          TVertexFormat.ColorF0:
          begin
            if LVertexElement.Offset mod 16 = 0 then
              LMetalVertexDeclaration.Append('float4 colorf0;')             // Size = 16 bytes | Alignment = 16 bytes
            else
              LMetalVertexDeclaration.Append('packed_float4 colorf0;');     // Size = 16 bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 16);
          end;
          TVertexFormat.ColorF1:
          begin
            if LVertexElement.Offset mod 16 = 0 then
              LMetalVertexDeclaration.Append('float4 colorf1;')             // Size = 16 bytes | Alignment = 16 bytes
            else
              LMetalVertexDeclaration.Append('packed_float4 colorf1;');     // Size = 16 bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 16);
          end;
          TVertexFormat.ColorF2:
          begin
            if LVertexElement.Offset mod 16 = 0 then
              LMetalVertexDeclaration.Append('float4 colorf2;')             // Size = 16 bytes | Alignment = 16 bytes
            else
              LMetalVertexDeclaration.Append('packed_float4 colorf2;');     // Size = 16 bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 16);
          end;
          TVertexFormat.ColorF3:
          begin
            if LVertexElement.Offset mod 16 = 0 then
              LMetalVertexDeclaration.Append('float4 colorf3;')             // Size = 16 bytes | Alignment = 16 bytes
            else
              LMetalVertexDeclaration.Append('packed_float4 colorf3;');     // Size = 16 bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 16);
          end;
          TVertexFormat.TexCoord0:
          begin
            if LVertexElement.Offset mod 8 = 0 then
              LMetalVertexDeclaration.Append('float2 texcoord0;')           // Size = 8  bytes | Alignment = 8 bytes
            else
              LMetalVertexDeclaration.Append('packed_float2 texcoord0;');   // Size = 8  bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 8);
          end;
          TVertexFormat.TexCoord1:
          begin
            if LVertexElement.Offset mod 8 = 0 then
              LMetalVertexDeclaration.Append('float2 texcoord1;')           // Size = 8  bytes | Alignment = 8 bytes
            else
              LMetalVertexDeclaration.Append('packed_float2 texcoord1;');   // Size = 8  bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 8);
          end;
          TVertexFormat.TexCoord2:
          begin
            if LVertexElement.Offset mod 8 = 0 then
              LMetalVertexDeclaration.Append('float2 texcoord2;')           // Size = 8  bytes | Alignment = 8 bytes
            else
              LMetalVertexDeclaration.Append('packed_float2 texcoord2;');   // Size = 8  bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 8);
          end;
          TVertexFormat.TexCoord3:
          begin
            if LVertexElement.Offset mod 8 = 0 then
              LMetalVertexDeclaration.Append('float2 texcoord3;')           // Size = 8  bytes | Alignment = 8 bytes
            else
              LMetalVertexDeclaration.Append('packed_float2 texcoord3;');   // Size = 8  bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 8);
          end;
          TVertexFormat.BiNormal:
          begin
            LMetalVertexDeclaration.Append('packed_float3 binormal;');      // Size = 12 bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 12);
          end;
          TVertexFormat.Tangent:
          begin
            LMetalVertexDeclaration.Append('packed_float3 tangent;');       // Size = 12 bytes | Alignment = 4 bytes
            Inc(LCurrOffset, 12);
          end
          else
            raise EContext3DException.CreateResFmt(@SCannotCreateShader, [ClassName]);
        end
      end;
      Result := LMetalVertexDeclaration.ToString;
    finally
      LMetalVertexDeclaration.Free;
    end;
  end;

  function CreateLibrary(const AShader: TcontextShader): MTLLibrary;
  var
    LContextShaderSource: TContextShaderSource;
    LSourceStr: string;
    LErrorPtr: Pointer;
  begin
    if AShader = nil then
      Exit(nil);
    LContextShaderSource := AShader.GetSourceByArch(TContextShaderArch.Metal);
    if not LContextShaderSource.IsDefined then
      Exit(nil);
    LSourceStr := TEncoding.UTF8.GetString(LContextShaderSource.Code);
    LSourceStr := StringReplace(LSourceStr, '<#VertexDeclaration#>', BuildVertexDeclarationStr, []);
    Result := SharedDevice.newLibraryWithSourceOptionsError(
                StrToNSStr(LSourceStr){source},
                nil{options},
                @LErrorPtr{error});
    CheckError(LErrorPtr, Result = nil);
  end;

var
  LPipelineDescriptor: MTLRenderPipelineDescriptor;
  LVertexShaderlibrary, LPixelShaderlibrary: MTLLibrary;
  LRenderPipelineColorAttachmentDescriptor: MTLRenderPipelineColorAttachmentDescriptor;
  LErrorPtr: Pointer;
  LFunction: MTLFunction;
begin
  LPipelineDescriptor := TMTLRenderPipelineDescriptor.Create;
  try
    LVertexShaderlibrary := CreateLibrary(CurrentVertexShader);
    try
      LPixelShaderlibrary := CreateLibrary(CurrentPixelShader);
      try
        // fail is their is no library defined
        if (LVertexShaderlibrary = nil) and
           (LPixelShaderlibrary = nil) then
          raise EContext3DException.CreateResFmt(@SCannotCreateShader, [ClassName]);

        // set the vertex function
        if LVertexShaderlibrary <> nil then
          LFunction := LVertexShaderlibrary.newFunctionWithName(StrToNsStr('vertexShader'))
        else
          LFunction := LPixelShaderlibrary.newFunctionWithName(StrToNsStr('vertexShader'));
        if LFunction = nil then
          raise EContext3DException.CreateResFmt(@SCannotCreateVertexShader, [ClassName]);
        try
          LpipelineDescriptor.setvertexFunction(LFunction);
        finally
          LFunction.release;
        end;

        // set the fragment function
        if LPixelShaderlibrary <> nil then
          LFunction := LPixelShaderlibrary.newFunctionWithName(StrToNsStr('fragmentShader'))
        else
          LFunction := LVertexShaderlibrary.newFunctionWithName(StrToNsStr('fragmentShader'));
        if LFunction = nil then
          raise EContext3DException.CreateResFmt(@SCannotCreatePixelShader, [ClassName]);
        try
          LpipelineDescriptor.setfragmentFunction(LFunction);
        finally
          LFunction.release;
        end;
      finally
        if LPixelShaderlibrary <> nil then
          LPixelShaderlibrary.release;
      end;
    finally
      if LVertexShaderlibrary <> nil then
        LVertexShaderlibrary.release;
    end;

    // The textures and actions are set in colorAttachments[0], which represents the first color
    // attachment (at index 0 in the array)
    LRenderPipelineColorAttachmentDescriptor := LPipelineDescriptor.colorAttachments.objectAtIndexedSubscript(0);

    // Enable Blending
    LRenderPipelineColorAttachmentDescriptor.setBlendingEnabled(AConfiguration.BlendingEnabled);
    if AConfiguration.BlendingEnabled then
    begin
      LRenderPipelineColorAttachmentDescriptor.setRgbBlendOperation(MTLBlendOperationAdd);
      LRenderPipelineColorAttachmentDescriptor.setalphaBlendOperation(MTLBlendOperationAdd);
      LRenderPipelineColorAttachmentDescriptor.setsourceRGBBlendFactor(MTLBlendFactorOne);
      LRenderPipelineColorAttachmentDescriptor.setsourceAlphaBlendFactor(MTLBlendFactorOne);
      LRenderPipelineColorAttachmentDescriptor.setdestinationRGBBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
      LRenderPipelineColorAttachmentDescriptor.setdestinationAlphaBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
    end;

    // Setup the output pixel format
    LRenderPipelineColorAttachmentDescriptor.setPixelFormat(PixelFormatToMTLPixelFormat(PixelFormat));
    LRenderPipelineColorAttachmentDescriptor.setWriteMask(AConfiguration.ColorWriteMask);

    // Setup the number of samples in each fragment.
    LPipelineDescriptor.setSampleCount(AConfiguration.SampleCount);

    // The pixel format of the attachment that stores depth data.
    LPipelineDescriptor.setDepthAttachmentPixelFormat(MTLPixelFormatDepth32Float_Stencil8);

    // The pixel format of the attachment that stores stencil data.
    LPipelineDescriptor.setStencilAttachmentPixelFormat(MTLPixelFormatDepth32Float_Stencil8);

    // Compile the configured pipeline descriptor to a pipeline state object
    Result := SharedDevice.newRenderPipelineStateWithDescriptorError(LpipelineDescriptor, @LErrorPtr);
    CheckError(LErrorPtr, Result = nil);
  finally
    LPipelineDescriptor.release;
  end;
end;

class function TContextMetal.GetPipelineState(const AConfiguration: TPipelineStateConfiguration):
  MTLRenderPipelineState;
begin
  CreateSharedStates;
  TMonitor.Enter(FPipelineStates);
  try
    if not FPipelineStates.TryGetValue(AConfiguration, Result) then
    begin
      Result := CreatePipelineState(AConfiguration);
      FPipelineStates.Add(AConfiguration, Result);
    end;
  finally
    TMonitor.Exit(FPipelineStates);
  end;
end;

class function TContextMetal.CreateDepthStencilState(const AConfiguration: TDepthStencilStateConfiguration):
  MTLDepthStencilState;
var
  LStencilDescriptor: MTLStencilDescriptor;
  LDepthStencilDescriptor: MTLDepthStencilDescriptor;
begin
  LDepthStencilDescriptor := TMTLDepthStencilDescriptor.Create;
  try
    // csZTestOn / csZTestOff
    // The comparison that is performed between a fragment's depth value and the depth
    // value in the attachment, which determines whether to discard the fragment.
    // The default value is MTLCompareFunctionAlways, which indicates that the depth test
    // always passes and the fragment remains a candidate to replace the data at the specified location.
    LDepthStencilDescriptor.setDepthCompareFunction(AConfiguration.DepthCompareFunction);

    // csZWriteOn / csZWriteOff
    // A Boolean value that indicates whether depth values can be written to the depth attachment.
    LDepthStencilDescriptor.setDepthWriteEnabled(AConfiguration.DepthWriteEnabled);

    // csStencilOn / csStencilOff
    if AConfiguration.StencilEnabled then
    begin
      LStencilDescriptor := TMTLStencilDescriptor.Create;
      try
        // The operation that is performed to update the values in the stencil attachment when the stencil test fails.
        // When the stencil test fails for a pixel, its incoming color, depth, or stencil values are discarded.
        LStencilDescriptor.setStencilFailureOperation(AConfiguration.StencilFailureOperation);
        // The operation that is performed to update the values in the stencil attachment when the stencil
        // test passes, but the depth test fails.
        LStencilDescriptor.setDepthFailureOperation(AConfiguration.DepthFailureOperation);
        // The operation that is performed to update the values in the stencil attachment when both the
        // stencil test and the depth test pass.
        LStencilDescriptor.setDepthStencilPassOperation(AConfiguration.DepthStencilPassOperation);
        // The comparison that is performed between the masked reference value and a masked value in
        // the stencil attachment.
        LStencilDescriptor.setStencilCompareFunction(AConfiguration.StencilCompareFunction);
        // A bitmask that determines from which bits that stencil comparison tests can read.
        LStencilDescriptor.setReadMask(AConfiguration.StencilMask);
        // A bitmask that determines to which bits that stencil operations can write.
        LStencilDescriptor.setWriteMask(AConfiguration.StencilMask);
        // The stencil descriptor for front-facing primitives.
        LDepthStencilDescriptor.setFrontFaceStencil(LStencilDescriptor);
        // The stencil descriptor for back-facing primitives.
        LDepthStencilDescriptor.setBackFaceStencil(LStencilDescriptor);
      finally
        LStencilDescriptor.release;
      end;
    end
    else
    begin
      // Nil indicates the stencil test is disabled for the front-facing primitives.
      LDepthStencilDescriptor.setFrontFaceStencil(nil);
      // Nil indicates the stencil test is disabled for the back-facing primitives.
      LDepthStencilDescriptor.setBackFaceStencil(nil);
    end;

    // Creates a new object that contains depth and stencil test state
    Result := SharedDevice.newDepthStencilStateWithDescriptor(LDepthStencilDescriptor);
  finally
    LDepthStencilDescriptor.release;
  end;
end;

class function TContextMetal.GetDepthStencilState(const AConfiguration: TDepthStencilStateConfiguration):
  MTLDepthStencilState;
begin
  CreateSharedStates;
  TMonitor.Enter(FDepthStencilStates);
  try
    if not FDepthStencilStates.TryGetValue(AConfiguration, Result) then
    begin
      Result := CreateDepthStencilState(AConfiguration);
      FDepthStencilStates.Add(AConfiguration, Result);
    end;
  finally
    TMonitor.Exit(FDepthStencilStates);
  end;
end;

class function TContextMetal.CreateSamplerState(const AConfiguration: TSamplerStateConfiguration): MTLSamplerState;
var
  LSamplerDescriptor: MTLSamplerDescriptor;
begin
  LSamplerDescriptor := TMTLSamplerDescriptor.Create;
  try
    // The filtering option for combining pixels within one mipmap level when
    // the sample footprint is larger than a pixel (minification).
    LSamplerDescriptor.setMinFilter(AConfiguration.MinFilter);

    // The filtering operation for combining pixels within one mipmap level when
    // the sample footprint is smaller than a pixel (magnification).
    LSamplerDescriptor.setMagFilter(AConfiguration.MagFilter);

    // The filtering option for combining pixels between two mipmap levels.
    LSamplerDescriptor.setMipFilter(AConfiguration.MipFilter);

    LSamplerDescriptor.setSAddressMode(MTLSamplerAddressModeRepeat);
    LSamplerDescriptor.setTAddressMode(MTLSamplerAddressModeRepeat);

    // Creates a sampler state object.
    Result := SharedDevice.newSamplerStateWithDescriptor(LSamplerDescriptor);
  finally
    LSamplerDescriptor.release;
  end;
end;

class function TContextMetal.GetSamplerState(const AConfiguration: TSamplerStateConfiguration): MTLSamplerState;
begin
  CreateSharedStates;
  TMonitor.Enter(FSamplerStates);
  try
    if not FSamplerStates.TryGetValue(AConfiguration, Result) then
    begin
      Result := CreateSamplerState(AConfiguration);
      FSamplerStates.Add(AConfiguration, Result);
    end;
  finally
    TMonitor.Exit(FSamplerStates);
  end;
end;

class procedure TContextMetal.CreateSharedStates;
begin
  if FPipelineStates = nil then
    FPipelineStates := TDictionary<TPipelineStateConfiguration, MTLRenderPipelineState>.Create;
  if FDepthStencilStates = nil then
    FDepthStencilStates := TDictionary<TDepthStencilStateConfiguration, MTLDepthStencilState>.Create;
  if FSamplerStates = nil then
    FSamplerStates := TDictionary<TSamplerStateConfiguration, MTLSamplerState>.Create;
end;

class procedure TContextMetal.DestroySharedStates;
var
  LRenderPipelineState: MTLRenderPipelineState;
  LDepthStencilState: MTLDepthStencilState;
  LSamplerState: MTLSamplerState;
begin
  if FPipelineStates <> nil then
  begin
    for LRenderPipelineState in FPipelineStates.values do
      LRenderPipelineState.release;
    FreeAndNil(FPipelineStates);
  end;
  if FDepthStencilStates <> nil then
  begin
    for LDepthStencilState in FDepthStencilStates.values do
      LDepthStencilState.release;
    FreeAndNil(FDepthStencilStates);
  end;
  if FSamplerStates <> nil then
  begin
    for LSamplerState in FSamplerStates.values do
      LSamplerState.release;
    FreeAndNil(FSamplerStates);
  end;
end;

constructor TContextMetal.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AMultisample: TMultisample; const ADepthStencil: Boolean);
begin
  inherited;
  CreateSharedDevice;
  CreateBuffer;
end;

constructor TContextMetal.CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
  const ADepthStencil: Boolean);
begin
  inherited;
  CreateSharedDevice;
  CreateBuffer;
end;

destructor TContextMetal.Destroy;
begin
  if FCommandQueue <> nil then
  begin
    FCommandQueue.release;
    FCommandQueue := nil;
  end;
  inherited Destroy;
end;

procedure TContextMetal.InitContext;
begin
  // this method is call every time we call
  // CreateFromWindow or CreateFromTexture
  inherited;
  FCommandQueue := SharedDevice.NewCommandQueue;
end;

procedure TContextMetal.DoCreateBuffer;
var
  LTextureDescriptor: MTLTextureDescriptor;
  LWidth: NSUInteger;
  LHeight: NSUInteger;
  LPixelFormat: MTLPixelFormat;
begin
  // this method is call every time we call
  // CreateFromWindow, CreateFromTexture, resize or SetMultisample
  FSampleCount := GetSupportedMultisample(MultisampleTypeToNumber(Multisample));
  if (fSampleCount > 1) or DepthStencil then
  begin
    if Texture <> nil then
    begin
      LWidth := Texture.Width;
      LHeight := Texture.Height;
      LPixelFormat := PixelFormatToMTLPixelFormat(Texture.PixelFormat);
    end
    else
    begin
      LWidth := Round(Width * Scale);
      LHeight := Round(Height * Scale);
      LPixelFormat := PixelFormatToMTLPixelFormat(PixelFormat);
    end;

    if (LWidth = 0) or (LHeight = 0) then
      Exit;

    if FSampleCount > 1 then
    begin
      LTextureDescriptor := TMTLTextureDescriptor.OCClass.texture2DDescriptorWithPixelFormat(
                              LPixelFormat, //pixelFormat: MTLPixelFormat;
                              LWidth, // width: NSUInteger;
                              LHeight, // height: NSUInteger;
                              False); //mipmapped: Boolean

      LTextureDescriptor.setTextureType(MTLTextureType2DMultisample);
      LTextureDescriptor.setStorageMode(MTLStorageModePrivate);
      LTextureDescriptor.setSampleCount(FSampleCount);
      FMultisampleBufferTexture := SharedDevice.newTextureWithDescriptor(LTextureDescriptor);
    end;

    if DepthStencil then
    begin
      LTextureDescriptor := TMTLTextureDescriptor.OCClass.texture2DDescriptorWithPixelFormat(
                              MTLPixelFormatDepth32Float_Stencil8, //pixelFormat: MTLPixelFormat;
                              LWidth, // width: NSUInteger;
                              LHeight, // height: NSUInteger;
                              False); //mipmapped: Boolean
      if FSampleCount > 1 then
      begin
        LTextureDescriptor.setTextureType(MTLTextureType2DMultisample);
        LTextureDescriptor.setSampleCount(FSampleCount);
      end
      else
        LTextureDescriptor.setTextureType(MTLTextureType2D);
      LTextureDescriptor.setStorageMode(MTLStorageModePrivate);
      FDepthStencilBufferTexture := SharedDevice.newTextureWithDescriptor(LTextureDescriptor);
    end;
  end;
end;

procedure TContextMetal.DoFreeBuffer;
begin
  // In pair with DoCreateBuffer
  if FMultisampleBufferTexture <> nil then
    FMultisampleBufferTexture.release;
  FMultisampleBufferTexture := nil;
  //
  if FDepthStencilBufferTexture <> nil then
    FDepthStencilBufferTexture.release;
  FDepthStencilBufferTexture := nil;
end;

procedure TContextMetal.DoResize;
begin
end;

class function TContextMetal.DoBitmapToTexture(const Bitmap: TBitmap): TTexture;
begin
  if Bitmap.CanvasClass.InheritsFrom(TCustomCanvasGpu) then
    Result := TBitmapCtx(Bitmap.Handle).PaintingTexture
  else
    Result := inherited DoBitmapToTexture(Bitmap);
end;

procedure TContextMetal.DoSetContextState(AState: TContextState);
begin
  case AState of
    // 2D screen matrix
    TContextState.cs2DScene:;
    // 3D camera matrix
    TContextState.cs3DScene:;
    // Depth
    TContextState.csZTestOn: FDepthStencilStateConfiguration.depthCompareFunction := MTLCompareFunctionLessEqual;
    TContextState.csZTestOff: FDepthStencilStateConfiguration.depthCompareFunction := MTLCompareFunctionAlways;
    TContextState.csZWriteOn: FDepthStencilStateConfiguration.DepthWriteEnabled := True;
    TContextState.csZWriteOff: FDepthStencilStateConfiguration.DepthWriteEnabled := False;
    // Alpha Blending
    TContextState.csAlphaBlendOn: FPipelineStateConfiguration.BlendingEnabled := True;
    TContextState.csAlphaBlendOff: FPipelineStateConfiguration.BlendingEnabled := False;
    // Stencil
    TContextState.csStencilOn: FDepthStencilStateConfiguration.StencilEnabled := True;
    TContextState.csStencilOff: FDepthStencilStateConfiguration.StencilEnabled := False;
    // Color
    TContextState.csColorWriteOn: FPipelineStateConfiguration.ColorWriteMask := MTLColorWriteMaskAll;
    TContextState.csColorWriteOff: FPipelineStateConfiguration.ColorWriteMask := MTLColorWriteMaskNone;
    // Scissor
    TContextState.csScissorOn:
    begin
      if FRenderCommandEncoder <> nil then
        FRenderCommandEncoder.setScissorRect(MakeMTLScissorRect(CurrentScissorRect, Scale));
    end;
    TContextState.csScissorOff:
    begin
      if FRenderCommandEncoder <> nil then
        FRenderCommandEncoder.setScissorRect(MakeMTLScissorRect(Rect(0, 0, width, height), Scale));
    end;
    // Faces
    TContextState.csFrontFace:
    begin
      if FRenderCommandEncoder <> nil then
        FRenderCommandEncoder.setCullMode(MTLCullModeBack);
    end;
    TContextState.csBackFace:
    begin
      if FRenderCommandEncoder <> nil then
        FRenderCommandEncoder.setCullMode(MTLCullModeFront);
    end;
    TContextState.csAllFace:
    begin
      if FRenderCommandEncoder <> nil then
        FRenderCommandEncoder.setCullMode(MTLCullModeNone);
    end;
  end;
end;

procedure TContextMetal.DoSetStencilOp(const Fail, ZFail, ZPass: TStencilOp);
begin
  case Fail of
    TStencilOp.Keep: FDepthStencilStateConfiguration.StencilFailureOperation := MTLStencilOperationKeep;
    TStencilOp.Zero: FDepthStencilStateConfiguration.StencilFailureOperation := MTLStencilOperationZero;
    TStencilOp.Replace: FDepthStencilStateConfiguration.StencilFailureOperation := MTLStencilOperationReplace;
    TStencilOp.Increase: FDepthStencilStateConfiguration.StencilFailureOperation := MTLStencilOperationIncrementClamp;
    TStencilOp.Decrease: FDepthStencilStateConfiguration.StencilFailureOperation := MTLStencilOperationDecrementClamp;
    TStencilOp.Invert: FDepthStencilStateConfiguration.StencilFailureOperation := MTLStencilOperationInvert;
  end;
  case ZFail of
    TStencilOp.Keep: FDepthStencilStateConfiguration.DepthFailureOperation := MTLStencilOperationKeep;
    TStencilOp.Zero: FDepthStencilStateConfiguration.DepthFailureOperation := MTLStencilOperationZero;
    TStencilOp.Replace: FDepthStencilStateConfiguration.DepthFailureOperation := MTLStencilOperationReplace;
    TStencilOp.Increase: FDepthStencilStateConfiguration.DepthFailureOperation := MTLStencilOperationIncrementClamp;
    TStencilOp.Decrease: FDepthStencilStateConfiguration.DepthFailureOperation := MTLStencilOperationDecrementClamp;
    TStencilOp.Invert: FDepthStencilStateConfiguration.DepthFailureOperation := MTLStencilOperationInvert;
  end;
  case ZPass of
    TStencilOp.Keep: FDepthStencilStateConfiguration.DepthStencilPassOperation := MTLStencilOperationKeep;
    TStencilOp.Zero: FDepthStencilStateConfiguration.DepthStencilPassOperation := MTLStencilOperationZero;
    TStencilOp.Replace: FDepthStencilStateConfiguration.DepthStencilPassOperation := MTLStencilOperationReplace;
    TStencilOp.Increase: FDepthStencilStateConfiguration.DepthStencilPassOperation := MTLStencilOperationIncrementClamp;
    TStencilOp.Decrease: FDepthStencilStateConfiguration.DepthStencilPassOperation := MTLStencilOperationDecrementClamp;
    TStencilOp.Invert: FDepthStencilStateConfiguration.DepthStencilPassOperation := MTLStencilOperationInvert;
  end;
end;

procedure TContextMetal.DoSetStencilFunc(const Func: TStencilfunc; Ref, Mask: cardinal);
begin
  case Func of
    TStencilfunc.Never: FDepthStencilStateConfiguration.StencilCompareFunction := MTLCompareFunctionNever;
    TStencilfunc.Less: FDepthStencilStateConfiguration.StencilCompareFunction := MTLCompareFunctionLess;
    TStencilfunc.Lequal: FDepthStencilStateConfiguration.StencilCompareFunction := MTLCompareFunctionLessEqual;
    TStencilfunc.Greater: FDepthStencilStateConfiguration.StencilCompareFunction := MTLCompareFunctionGreater;
    TStencilfunc.Gequal: FDepthStencilStateConfiguration.StencilCompareFunction := MTLCompareFunctionGreaterEqual;
    TStencilfunc.Equal: FDepthStencilStateConfiguration.StencilCompareFunction := MTLCompareFunctionEqual;
    TStencilfunc.NotEqual: FDepthStencilStateConfiguration.StencilCompareFunction := MTLCompareFunctionNotEqual;
    TStencilfunc.Always: FDepthStencilStateConfiguration.StencilCompareFunction := MTLCompareFunctionAlways;
  end;
  FDepthStencilStateConfiguration.StencilMask := Mask;
  FStencilReferenceValue := Ref;
end;

procedure TContextMetal.DoSetScissorRect(const ScissorRect: TRect);
begin
  if FRenderCommandEncoder = nil then
    Exit;
  if CurrentStates[TContextState.csScissorOn] then
    FRenderCommandEncoder.setScissorRect(MakeMTLScissorRect(ScissorRect, Scale));
end;

procedure TContextMetal.DoClear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: single;
  const AStencil: Cardinal);
var
{$IF defined(IOS)}
  LRenderPassColorAttachmentDescriptor: MTLRenderPassColorAttachmentDescriptor;
  LColorF: TAlphaColorF;
{$ELSE}
  LPrevContextStateCsZTestOn: Boolean;
  LPrevContextStateCsZWriteOn: Boolean;
  LPrevContextStateCsAlphaBlendOn: Boolean;
  LPrevContextStateCsStencilOn: Boolean;
  LPrevContextStateCsColorWriteOff: Boolean;
  LPrevContextStateCsScissorOn: Boolean;
  LPrevContextStateCsAllFace: Boolean;
  LPrevContextStateCsFrontFace: Boolean;
  LPrevContextStateCsBackFace: Boolean;
{$ENDIF}
  LRenderPassDepthAttachmentDescriptor: MTLRenderPassDepthAttachmentDescriptor;
  LRenderPassStencilAttachmentDescriptor: MTLRenderPassStencilAttachmentDescriptor;
begin
  if FRenderPassDescriptor = nil then
    Exit;
  if (FRenderCommandEncoder <> nil) {$IF not defined(IOS)}and (ATarget <> [TClearTarget.Color]){$ENDIF} then
  begin
    if (Multisample <> TMultisample.none) and (FMultisampleBufferTexture <> nil) then
      FRenderCommandEncoder.setColorStoreAction(MTLStoreActionStoreAndMultisampleResolve, 0);
    //
    if DepthStencil and (FDepthStencilBufferTexture <> nil) then
    begin
      FRenderCommandEncoder.setDepthStoreAction(MTLStoreActionStore);
      FRenderCommandEncoder.setStencilStoreAction(MTLStoreActionStore);
    end;
    //
    FRenderCommandEncoder.endEncoding;
    FRenderCommandEncoder := nil;
    //
    FRenderPassDescriptor.colorAttachments.objectAtIndexedSubscript(0).setLoadAction(MTLLoadActionLoad);
    if DepthStencil then
    begin
      FRenderPassDescriptor.DepthAttachment.setLoadAction(MTLLoadActionLoad);
      FRenderPassDescriptor.StencilAttachment.setLoadAction(MTLLoadActionLoad);
    end;
  end;

  if DepthStencil and (TClearTarget.Depth in ATarget) then
  begin
    LRenderPassDepthAttachmentDescriptor := FRenderPassDescriptor.DepthAttachment;
    LRenderPassDepthAttachmentDescriptor.setClearDepth(ADepth);
    LRenderPassDepthAttachmentDescriptor.setLoadAction(MTLLoadActionClear);
  end;

  if DepthStencil and (TClearTarget.Stencil in ATarget) then
  begin
    LRenderPassStencilAttachmentDescriptor := FRenderPassDescriptor.StencilAttachment;
    LRenderPassStencilAttachmentDescriptor.setClearStencil(AStencil);
    LRenderPassStencilAttachmentDescriptor.setLoadAction(MTLLoadActionClear);
  end;

  if (TClearTarget.Color in ATarget) then
  begin
  {$IF defined(IOS)}
    LColorF := TAlphaColorF.Create(AColor);
    LRenderPassColorAttachmentDescriptor := FRenderPassDescriptor.colorAttachments.objectAtIndexedSubscript(0);
    LRenderPassColorAttachmentDescriptor.setClearColor(MakeMTLClearColor(LColorF.r,LColorF.G,LColorF.b,LColorF.A));
    LRenderPassColorAttachmentDescriptor.setLoadAction(MTLLoadActionClear);
  {$ELSE}
    LPrevContextStateCsZTestOn := CurrentStates[TContextState.CsZTestOn];
    LPrevContextStateCsZWriteOn := CurrentStates[TContextState.CsZWriteOn];
    LPrevContextStateCsAlphaBlendOn := CurrentStates[TContextState.CsAlphaBlendOn];
    LPrevContextStateCsStencilOn := CurrentStates[TContextState.CsStencilOn];
    LPrevContextStateCsColorWriteOff := CurrentStates[TContextState.CsColorWriteOff];
    LPrevContextStateCsScissorOn := CurrentStates[TContextState.CsScissorOn];
    LPrevContextStateCsAllFace := CurrentStates[TContextState.CsAllFace];
    LPrevContextStateCsFrontFace := CurrentStates[TContextState.CsFrontFace];
    LPrevContextStateCsBackFace := CurrentStates[TContextState.CsBackFace];
    //
    if LPrevContextStateCsZTestOn then
      SetContextState(TContextState.csZTestOff);
    if LPrevContextStateCsZWriteOn then
      SetContextState(TContextState.csZWriteOff);
    if LPrevContextStateCsAlphaBlendOn then
      SetContextState(TContextState.csAlphaBlendOff);
    if LPrevContextStateCsStencilOn then
      SetContextState(TContextState.csStencilOff);
    if LPrevContextStateCsColorWriteOff then
      SetContextState(TContextState.csColorWriteOn);
    if LPrevContextStateCsScissorOn then
      SetContextState(TContextState.csScissorOff);
    if not LPrevContextStateCsAllFace then
      SetContextState(TContextState.csAllFace);
    try
      // Rounding error due to which, when setting the projection matrix at certain dimensions,
      // there may be an incomplete surface painting. Therefore, we expand the area size.
      FillRect(TPoint3D.Create(-1, -1, 1), TPoint3D.Create(Width + 1, Height + 1, 1), 1, AColor);
    finally
      if LPrevContextStateCsZTestOn then
        SetContextState(TContextState.csZTestOn);
      if LPrevContextStateCsZWriteOn then
        SetContextState(TContextState.csZWriteOn);
      if LPrevContextStateCsAlphaBlendOn then
        SetContextState(TContextState.csAlphaBlendOn);
      if LPrevContextStateCsStencilOn then
        SetContextState(TContextState.csStencilOn);
      if LPrevContextStateCsColorWriteOff then
        SetContextState(TContextState.csColorWriteOff);
      if LPrevContextStateCsScissorOn then
        SetContextState(TContextState.csScissorOn);
      if LPrevContextStateCsAllFace then begin
        if LPrevContextStateCsFrontFace then
          SetContextState(TContextState.csFrontFace)
        else if LPrevContextStateCsBackFace then
          SetContextState(TContextState.csBackFace)
      end
    end;
  {$ENDIF}
  end;
end;

procedure TContextMetal.DoCopyToBitmap(const Dest: TBitmap; const ARect: TRect);
var
  LCopyRect: TRect;
  LSourceTexture: MTLTexture;
  LDestTexture: MTLTexture;
  LTexture: TTexture;
  LCommandBuffer: MTLCommandBuffer;
  LBlitCommandEncoder: MTLBlitCommandEncoder;
  LSourceOrigin: MTLOrigin;
  LDestOrigin: MTLOrigin;
  LSourceSize: MTLSize;
begin
  if (TCanvasStyle.NeedGPUSurface in Dest.CanvasClass.GetCanvasStyle) and
     ((Texture <> nil) or (FOnScreenTexture <> nil)) then
  begin
    LCopyRect := TRect.Intersect(ARect, TRect.Create(0, 0, Width, Height));
    if (Texture <> nil) then
    begin
      LSourceTexture := TMTLTexture.Wrap(Pointer(Texture.Handle));
      LSourceOrigin.x := LCopyRect.left;
      LSourceOrigin.y := LCopyRect.top;
      LSourceOrigin.z := 0;
      LSourceSize.width := LCopyRect.Width;
      LSourceSize.height := LCopyRect.Height;
      LSourceSize.depth := 1;
    end
    else
    begin
      LSourceTexture := FOnScreenTexture;
      LSourceOrigin.x := Round(LCopyRect.left * Scale);
      LSourceOrigin.y := Round(LCopyRect.top * Scale);
      LSourceOrigin.z := 0;
      LSourceSize.width := Round(LCopyRect.Width * Scale);
      LSourceSize.height := Round(LCopyRect.Height * Scale);
      LSourceSize.depth := 1;
    end;

    LTexture := BitmapToTexture(Dest);
    if LTexture.Handle = 0 then
      LTexture.Initialize;
    LDestTexture := TMTLTexture.Wrap(Pointer(LTexture.Handle));
    LDestOrigin.x := 0;
    LDestOrigin.y := 0;
    LDestOrigin.z := 0;

    LCommandBuffer := FCommandQueue.CommandBuffer;
    if LCommandBuffer = nil then
      Exit;
    LBlitCommandEncoder := LCommandBuffer.blitCommandEncoder;
    LBlitCommandEncoder.
      copyFromTextureSourceSliceSourceLevelSourceOriginSourceSizeToTextureDestinationSliceDestinationLevelDestinationOrigin(
        NSObjectToId(LSourceTexture), // sourceTexture: MTLTexture;
        0, // sourceSlice: NSUInteger;
        0, // sourceLevel: NSUInteger;
        LSourceOrigin, // sourceOrigin: MTLOrigin;
        LSourceSize, // sourceSize: MTLSize;
        NSObjectToId(LDestTexture), // toTexture: MTLTexture;
        0, // destinationSlice: NSUInteger;
        0, // destinationLevel: NSUInteger;
        LDestOrigin);// destinationOrigin: MTLOrigin);
    LBlitCommandEncoder.endEncoding;
    LCommandBuffer.commit;
    LCommandBuffer.waitUntilCompleted;
  end
  else
    inherited DoCopyToBitmap(Dest, ARect);
end;

procedure TContextMetal.DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect);

  function CreateRegion(const ARect: TRect; const AScale: Single): MTLRegion;
  begin
    if SameValue(AScale, 1, TEpsilon.Scale) then
    begin
      Result.origin.x := ARect.left;
      Result.origin.y := ARect.top;
      Result.size.width := ARect.Width;
      Result.size.height := ARect.Height;
    end
    else
    begin
      Result.origin.x := Round(ARect.Left * Scale);
      Result.origin.y := Round(ARect.Top * Scale);
      Result.size.width := Round(ARect.Width * Scale);
      Result.size.height := Round(ARect.Height * Scale);
    end;
    Result.origin.z := 0;
    Result.size.depth := 1;
  end;

  procedure SynchronizeResources(const ATexture: MTLTexture);
  var
    CommandBuffer: MTLCommandBuffer;
    LBlitCommandEncoder: MTLBlitCommandEncoder;
  begin
    CommandBuffer := FCommandQueue.CommandBuffer;

    if CommandBuffer = nil then
      Exit;

    LBlitCommandEncoder := CommandBuffer.blitCommandEncoder;
    LBlitCommandEncoder.synchronizeResource(ATexture);
    LBlitCommandEncoder.endEncoding;
    CommandBuffer.commit;
    CommandBuffer.waitUntilCompleted;
  end;

var
  LCopyRect: TRect;
  LTexture: MTLTexture;
  LRegion: MTLRegion;
begin
  LTexture := nil;

  if FCommandBuffer <> nil then
    FCommandBuffer.waitUntilCompleted;

  LCopyRect := TRect.Intersect(ARect, TRect.Create(0, 0, Width, Height));
  if Texture <> nil then
  begin
    LTexture := TMTLTexture.Wrap(Pointer(Texture.Handle));
    LRegion := CreateRegion(LCopyRect, 1);
  end
  else if FOnScreenTexture <> nil then
  begin
    LTexture := FOnScreenTexture;
    LRegion := CreateRegion(LCopyRect, Scale);
  end;

  if LTexture <> nil then
  begin
    // Synchronizing a Managed Resource between GPU and CPU
    if LTexture.storageMode = MTLStorageModeManaged then
      SynchronizeResources(LTexture);
    // Get texture data
    LTexture.getBytesBytesPerRowFromRegionMipmapLevel(
      Bits, // pixelBytes: Pointer;
      Pitch, // bytesPerRow: NSUInteger;
      LRegion, // fromRegion: MTLRegion;
      0); // mipmapLevel: NSUInteger
  end;
end;

procedure TContextMetal.DoDrawPrimitivesBatch(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
  const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize, IndexCount: Integer);
var
  LVertexBuffer: MTLBuffer;
  LIndicesBuffer: MTLBuffer;
  LPrimitiveType: MTLPrimitiveType;
  LIndexType: MTLIndexType;
begin
  CreateRenderCommandEncoder;
  if FRenderCommandEncoder = nil then
    Exit;

  if IndexCount = 0 then
    Exit;

  case AKind of
    TPrimitivesKind.Points: LPrimitiveType := MTLPrimitiveTypePoint;
    TPrimitivesKind.Lines: LPrimitiveType := MTLPrimitiveTypeLine;
    else
      LPrimitiveType := MTLPrimitiveTypeTriangle;
  end;
  if IndexSize = 2 then
    LIndexType := MTLIndexTypeUInt16
  else if IndexSize = 4 then
    LIndexType := MTLIndexTypeUInt32
  else
    raise EContext3DException.CreateResFmt(@SCannotCreateIndexBuffer, [ClassName]);

  // Update FPipelineStateConfiguration.VertexDeclaration. Silently exit if overflow
  FillChar(FPipelineStateConfiguration.VertexDeclaration, SizeOf(FPipelineStateConfiguration.VertexDeclaration), 0);
  if Length(VertexDeclaration) > Length(FPipelineStateConfiguration.VertexDeclaration) then
    raise EContext3DException.CreateResFmt(@SCannotCreateVertexDeclaration, [ClassName]);
  Move(Pointer(VertexDeclaration)^,
       FPipelineStateConfiguration.VertexDeclaration[0],
       Length(VertexDeclaration) * SizeOf(TVertexElement));

  // Allocates vertex & indices buffer
  LVertexBuffer := SharedDevice.newBufferWithBytes(Vertices, VertexSize * VertexCount, 0);
  LIndicesBuffer := SharedDevice.newBufferWithBytes(Indices, IndexSize * IndexCount, 0);
  try
    // Sets a buffer for the vertex function
    FRenderCommandEncoder.setVertexBuffer(LVertexBuffer{buffer}, 0{offset}, 0{atIndex});

    // Sets the current render pipeline state object
    FRenderCommandEncoder.setRenderPipelineState(GetpipelineState(FPipelineStateConfiguration));

    if DepthStencil then
    begin
      // Sets the depth and stencil test state.
      FRenderCommandEncoder.setDepthStencilState(GetDepthStencilState(FDepthStencilStateConfiguration));
      // Sets a stencil reference value for both front and back stencil comparison tests
      FRenderCommandEncoder.setStencilReferenceValue(FStencilReferenceValue);
    end;

    // Encodes a command to render a number of instances of primitives using an index list specified in a buffer
    FRenderCommandEncoder.drawIndexedPrimitivesIndexCountIndexTypeIndexBufferIndexBufferOffset(
      LPrimitiveType, {primitiveType}
      IndexCount, {indexCount}
      LIndexType, {indexType}
      LIndicesBuffer, {indexBuffer}
      0); {indexBufferOffset}
  finally
    if LVertexBuffer <> nil then
      LVertexBuffer.release;
    if LIndicesBuffer <> nil then
      LIndicesBuffer.release;
  end;
end;

class procedure TContextMetal.DoInitializeTexture(const Texture: TTexture);
var
  LTextureDescriptor: MTLTextureDescriptor;
  LTexturePixelFormat: MTLPixelFormat;
  LTexture: MTLTexture;
begin
  LTexturePixelFormat := PixelFormatToMTLPixelFormat(Texture.PixelFormat);
  if LTexturePixelFormat = MTLPixelFormatInvalid then
    LTexturePixelFormat := PixelFormatToMTLPixelFormat(PixelFormat);
  LTextureDescriptor := TMTLTextureDescriptor.OCClass.texture2DDescriptorWithPixelFormat(
                          LTexturePixelFormat, //pixelFormat: MTLPixelFormat;
                          Texture.Width, // width: NSUInteger;
                          Texture.Height, // height: NSUInteger;
                          TTextureStyle.MipMaps in Texture.Style); //mipmapped: Boolean
  LTexture := SharedDevice.newTextureWithDescriptor(LTextureDescriptor);
  ITextureAccess(Texture).Handle := THandle(NSObjectToID(LTexture));
end;

class procedure TContextMetal.DoFinalizeTexture(const Texture: TTexture);
var
  LTexture: MTLTexture;
begin
  if Texture.Handle <> 0 then
  begin
    LTexture := TMTLTexture.Wrap(Pointer(Texture.Handle));
    LTexture.release;
    ITextureAccess(Texture).Handle := 0;
  end;
end;

class procedure TContextMetal.DoUpdateTexture(const Texture: TTexture; const Bits: Pointer; const Pitch: Integer);
var
  LTexture: MTLTexture;
  LRegion: MTLRegion;
  LCommandQueue: MTLCommandQueue;
  LCommandBuffer: MTLCommandBuffer;
  LBlitCommandEncoder: MTLBlitCommandEncoder;
begin
  if Texture.Handle <> 0 then
  begin
    LTexture := TMTLTexture.Wrap(Pointer(Texture.Handle));
    LRegion.origin.x := 0;
    LRegion.origin.y := 0;
    LRegion.origin.z := 0;
    LRegion.size.width := Texture.Width;
    LRegion.size.height := Texture.Height;
    LRegion.size.depth := 1;
    LTexture.replaceRegionMipmapLevelWithBytesBytesPerRow(
      LRegion, // region: MTLRegion;
      0, // mipmapLevel: NSUInteger;
      Bits, // withBytes: Pointer;
      Texture.BytesPerPixel * Texture.Width); // bytesPerRow: NSUInteger
    if TTextureStyle.MipMaps in Texture.Style then
    begin
      LCommandQueue := SharedDevice.NewCommandQueue;
      try
        LCommandBuffer := LCommandQueue.CommandBuffer;
        if LCommandBuffer = nil then
          Exit;
        LBlitCommandEncoder := LCommandBuffer.blitCommandEncoder;
        LBlitCommandEncoder.generateMipmapsForTexture(LTexture);
        LBlitCommandEncoder.endEncoding;
        LCommandBuffer.commit;
      finally
        LCommandQueue.release;
      end;
    end;
  end;
end;

class procedure TContextMetal.DoInitializeShader(const Shader: TContextShader);
var
  LShaderSource: TContextShaderSource;
begin
  LShaderSource := Shader.GetSourceByArch(TContextShaderArch.Metal);
  if LShaderSource.Arch = TContextShaderArch.Undefined then
    LShaderSource := Shader.GetSourceByArch(TContextShaderArch.Metal);
  if LShaderSource.IsDefined then
    Shader.Handle := AtomicIncrement(FShaderHandleGenerator)
  else
    Shader.Handle := 0;
end;

class procedure TContextMetal.DoFinalizeShader(const Shader: TContextShader);
var
  LItems: TArray<TPair<TPipelineStateConfiguration, MTLRenderPipelineState>>;
  LItem: TPair<TPipelineStateConfiguration, MTLRenderPipelineState>;
  I: Integer;
begin
  CreateSharedStates;
  if Shader.Handle = 0 then
    Exit;
  TMonitor.Enter(FPipelineStates);
  try
    LItems := FPipelineStates.ToArray;
    for I := Low(LItems) to High(LItems) do
    begin
      LItem := LItems[I];
      if (LItem.Key.VertexShaderHandle = Shader.Handle) or
         (LItem.Key.PixelShaderHandle = Shader.Handle) then
      begin
        FPipelineStates.Remove(LItem.Key);
        Litem.Value.release;
        LItem.Value := nil;
      end;
    end;
  finally
    TMonitor.Exit(FPipelineStates);
  end;
  Shader.Handle := 0;
end;

procedure TContextMetal.DoSetShaders(const VertexShader, PixelShader: TContextShader);
begin
  FPipelineStateConfiguration.VertexShaderHandle := VertexShader.Handle;
  FPipelineStateConfiguration.PixelShaderHandle := PixelShader.Handle;
end;

procedure TContextMetal.DoSetShaderVariable(const Name: string; const Data: array of TVector3D);
var
  LShaderSource: TContextShaderSource;
  LShaderVariable: TContextShaderVariable;
begin
  CreateRenderCommandEncoder;
  if FRenderCommandEncoder = nil then
    Exit;

  if (CurrentVertexShader <> nil) then
  begin
    LShaderSource := CurrentVertexShader.GetSourceByArch(TContextShaderArch.Metal);
    if LShaderSource.FindVariable(Name, LShaderVariable) then
    begin
      FRenderCommandEncoder.setVertexBytes(
        @Data[0], // bytes: Pointer;
        Length(Data) * SizeOf(TVector3D), // length: NSUInteger;
        LShaderVariable.Index); // atIndex: NSUInteger)
      Exit;
    end;
  end;

  if (CurrentPixelShader <> nil) then
  begin
    LShaderSource := CurrentPixelShader.GetSourceByArch(TContextShaderArch.Metal);
    if LShaderSource.FindVariable(Name, LShaderVariable) then
    begin
      FRenderCommandEncoder.setFragmentBytes(
        @Data[0], // bytes: Pointer;
        Length(Data) * SizeOf(TVector3D), // length: NSUInteger;
        LShaderVariable.Index); // atIndex: NSUInteger)
      Exit;
    end;
  end;
end;

procedure TContextMetal.DoSetShaderVariable(const Name: string; const Texture: TTexture);
var
  LShaderSource: TContextShaderSource;
  LSamplerStateConfiguration: TSamplerStateConfiguration;
  LTexture: MTLTexture;
  LShaderVariable: TContextShaderVariable;
begin
  if (Texture = nil) or Texture.IsEmpty then
    Exit;
  CreateRenderCommandEncoder;
  if FRenderCommandEncoder = nil then
    Exit;

  //
  // When MinFilter is MTLSamplerMinMagFilterNearest and MipFilter is MTLSamplerMipFilterNearest,
  // the closest-matching mipmap level is selected, and a single texel from it is used as the sample.
  //
  // When MinFilter is MTLSamplerMinMagFilterNearest and MipFilter is MTLSamplerMipFilterLinear, the
  // two closest-matching mipmap levels are selected, and one sample from each is taken. These two
  // samples are then averaged to produce the final sample.
  //
  // When MinFilter is MTLSamplerMinMagFilterLinear and MipFilter is MTLSamplerMipFilterNearest,
  // the closest-matching mipmap level is selected, and four texels are averaged to produce the sample.
  //
  // When MinFilter is MTLSamplerMinMagFilterLinear and MipFilter is MTLSamplerMipFilterLinear, the
  // two closest-matching mipmap levels are selected, and four samples from each are averaged to
  // create a sample for the level. These two averaged samples are then averaged again to produce
  // the final sample.
  //

  case Texture.MinFilter of
    TTextureFilter.Nearest: LSamplerStateConfiguration.MinFilter := MTLSamplerMinMagFilterNearest;
    TTextureFilter.Linear: LSamplerStateConfiguration.MinFilter := MTLSamplerMinMagFilterLinear;
  end;
  case Texture.MagFilter of
    TTextureFilter.Nearest: LSamplerStateConfiguration.MagFilter := MTLSamplerMinMagFilterNearest;
    TTextureFilter.Linear: LSamplerStateConfiguration.MagFilter := MTLSamplerMinMagFilterLinear;
  end;
  if TTextureStyle.MipMaps in Texture.Style then
  begin
    case Texture.MinFilter of
      TTextureFilter.Nearest: LSamplerStateConfiguration.MipFilter := MTLSamplerMipFilterNearest;
      TTextureFilter.Linear: LSamplerStateConfiguration.MipFilter := MTLSamplerMipFilterLinear;
    end;
  end
  else
    LSamplerStateConfiguration.MipFilter := MTLSamplerMipFilterNotMipmapped;

  if (CurrentVertexShader <> nil) then
  begin
    LShaderSource := CurrentVertexShader.GetSourceByArch(TContextShaderArch.Metal);
    if LShaderSource.FindVariable(Name, LShaderVariable) then
    begin
      if Texture.Handle = 0 then
        Texture.Initialize;
      LTexture := TMTLTexture.Wrap(Pointer(Texture.Handle));
      FRenderCommandEncoder.setVertexTexture(
        LTexture,
        LShaderVariable.Index);
      FRenderCommandEncoder.setFragmentSamplerStateAtIndex(
        GetSamplerState(LSamplerStateConfiguration),
        LShaderVariable.Index);
      Exit;
    end;
  end;

  if (CurrentPixelShader <> nil) then
  begin
    LShaderSource := CurrentPixelShader.GetSourceByArch(TContextShaderArch.Metal);
    if LShaderSource.FindVariable(Name, LShaderVariable) then
    begin
      if Texture.Handle = 0 then
        Texture.Initialize;
      LTexture := TMTLTexture.Wrap(Pointer(Texture.Handle));
      FRenderCommandEncoder.setFragmentTexture(
        LTexture,
        LShaderVariable.Index);
      FRenderCommandEncoder.setFragmentSamplerStateAtIndex(
        GetSamplerState(LSamplerStateConfiguration),
        LShaderVariable.Index);
      Exit;
    end;
  end;
end;

procedure TContextMetal.DoSetShaderVariable(const Name: string; const Matrix: TMatrix3D);
var
  LTransposedMatrix: TMatrix3D;
begin
  LTransposedMatrix := Matrix.Transpose;
  DoSetShaderVariable(Name, LTransposedMatrix.M);
end;

procedure TContextMetal.CreateRenderPassDescriptor;
var
  LRenderPassColorAttachmentDescriptor: MTLRenderPassColorAttachmentDescriptor;
  LTexture: MTLTexture;
begin
  if FRenderPassDescriptor = nil then
  begin
    if FCurrentDrawable <> nil then
      LTexture := FCurrentDrawable.texture
    else if Texture <> nil then
      LTexture := TMTLTexture.Wrap(Pointer(Texture.Handle))
    else
      LTexture := nil;

    if (LTexture <> nil) then
    begin
      FRenderPassDescriptor := TMTLRenderPassDescriptor.Create;
      //
      LRenderPassColorAttachmentDescriptor := FRenderPassDescriptor.colorAttachments.objectAtIndexedSubscript(0);
      if (Multisample <> TMultisample.none) and (FMultisampleBufferTexture <> nil) then
      begin
        LRenderPassColorAttachmentDescriptor.setTexture(FMultisampleBufferTexture);
        LRenderPassColorAttachmentDescriptor.setResolveTexture(LTexture);
        LRenderPassColorAttachmentDescriptor.setLoadAction(MTLLoadActionLoad);
        LRenderPassColorAttachmentDescriptor.setStoreAction(MTLStoreActionUnknown);
      end
      else
      begin
        LRenderPassColorAttachmentDescriptor.setTexture(LTexture);
        LRenderPassColorAttachmentDescriptor.setLoadAction(MTLLoadActionLoad);
        LRenderPassColorAttachmentDescriptor.setStoreAction(MTLStoreActionStore);
      end;
      //
      if DepthStencil and (FDepthStencilBufferTexture <> nil) then
      begin
        FRenderPassDescriptor.DepthAttachment.setTexture(FDepthStencilBufferTexture);
        FRenderPassDescriptor.DepthAttachment.setLoadAction(MTLLoadActionDontCare);
        FRenderPassDescriptor.DepthAttachment.setStoreAction(MTLStoreActionUnknown);
        //
        FRenderPassDescriptor.StencilAttachment.setTexture(FDepthStencilBufferTexture);
        FRenderPassDescriptor.StencilAttachment.setLoadAction(MTLLoadActionDontCare);
        FRenderPassDescriptor.StencilAttachment.setStoreAction(MTLStoreActionUnknown);
      end;
    end;
  end;
end;

procedure TContextMetal.DestroyRenderPassDescriptor;
begin
  if FRenderPassDescriptor <> nil then
  begin
    FRenderPassDescriptor.release;
    FRenderPassDescriptor := nil;
  end;
end;

procedure TContextMetal.CreateRenderCommandEncoder;
var
  LContextState: TContextState;
begin
  if FRenderCommandEncoder = nil then
  begin
    if (FCommandBuffer = nil) or (FRenderPassDescriptor = nil) then
      Exit;
    FRenderCommandEncoder := FCommandBuffer.renderCommandEncoderWithDescriptor(FRenderPassDescriptor);
    for LContextState := Low(TContextState) to High(TContextState) do
      if CurrentStates[LContextState] then
        DoSetContextState(LContextState);
  end;
end;

function TContextMetal.GetCurrentDrawable: CAMetalDrawable;
begin
  if Parent <> nil then
  begin
    {$IF defined(IOS)}
    Result := TiOSWindowHandle(Parent).MTView.currentDrawable;
    {$ELSE}
    Result := TMacWindowHandle(Parent).MTView.currentDrawable;
    {$ENDIF}
  end
  else
    Result := nil;
end;

function TContextMetal.DoBeginScene: Boolean;
begin
  Result := False;
  if not Valid then
    Exit;
  if FCommandQueue = nil then
    Exit;

  FOnScreenTexture := nil;
  FCurrentDrawable := GetCurrentDrawable;
  if (Texture = nil) and (FCurrentDrawable = nil) then
    Exit;

  FCommandBuffer := FCommandQueue.CommandBuffer;
  if FCommandBuffer = nil then
    Exit;

  FPipelineStateConfiguration.reset;
  FPipelineStateConfiguration.SampleCount := FSampleCount;
  FDepthStencilStateConfiguration.reset;
  FStencilReferenceValue := 0;

  CreateRenderPassDescriptor;
  if FRenderPassDescriptor = nil then
    Exit;

  Result := inherited DoBeginScene;
end;

procedure TContextMetal.DoEndScene;
begin
  // Finalizes the encoding of drawing commands.
  CreateRenderCommandEncoder;
  //
  if (Multisample <> TMultisample.none) and (FMultisampleBufferTexture <> nil) then
    FRenderCommandEncoder.setColorStoreAction(MTLStoreActionMultisampleResolve, 0);
  //
  if DepthStencil and (FDepthStencilBufferTexture <> nil) then
  begin
    FRenderCommandEncoder.setDepthStoreAction(MTLStoreActionDontCare);
    FRenderCommandEncoder.setStencilStoreAction(MTLStoreActionDontCare);
  end;
  //
  FRenderCommandEncoder.endEncoding;
  FRenderCommandEncoder := nil;

  // Tell Metal to send the rendering Result to the MTKView when rendering completes
  if FCurrentDrawable <> nil then
    FCommandBuffer.presentDrawable(FCurrentDrawable);

  // Finally, send the encoded command buffer to the GPU.
  FCommandBuffer.commit;
  FCommandBuffer.waitUntilCompleted;

  // Keep a reference to the CurrentDrawable.texture but not to CurrentDrawable
  DestroyRenderPassDescriptor;
  if FCurrentDrawable <> nil then
    FOnScreenTexture := FCurrentDrawable.texture;
  FCurrentDrawable := nil;
  FCommandBuffer := nil;

  inherited;
end;

procedure RegisterContextClasses;
begin
  TContextMetal.CreateSharedDevice;
  TContextMetal.CreateSharedStates;
  TContextManager.RegisterContext(TContextMetal, True);
end;

procedure UnregisterContextClasses;
begin
  TContextMetal.DestroySharedStates;
  TContextMetal.DestroySharedDevice;
end;

end.
