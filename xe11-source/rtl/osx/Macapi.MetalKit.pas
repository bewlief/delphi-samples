{**********************************************************}
{                                                          }
{           CodeGear Delphi Runtime Library                }
{                                                          }
{ Delphi-Objective-C Bridge                                }
{ Interfaces for Cocoa framework MetalKit                  }
{                                                          }
{ Copyright (c) 2008-2011 Apple Inc. All rights reserved.  }
{                                                          }
{ Translator: Embarcadero Technologies, Inc.               }
{   Copyright(c) 2012-2022 Embarcadero Technologies, Inc.  }
{              All rights reserved                         }
{                                                          }
{**********************************************************}

unit Macapi.MetalKit;

interface

uses
  {$IF defined(IOS)}
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.Foundation,
  iOSapi.QuartzCore,
  iOSapi.UIKit,
  {$ELSEIF defined(MACOS)}
  Macapi.CocoaTypes,
  Macapi.CoreGraphics,
  Macapi.Foundation,
  Macapi.QuartzCore,
  Macapi.AppKit,
  {$ENDIF}
  Macapi.CoreFoundation,
  Macapi.CoreServices,
  Macapi.Dispatch,
  Macapi.Mach,
  Macapi.ObjCRuntime,
  Macapi.ObjectiveC,
  Macapi.Metal;

type

// ===== Forward declarations =====

{$M+}

  MTKViewDelegate = interface;
  MTKView = interface;
  MTKTextureLoader = interface;
  MTKMeshBufferAllocator = interface;
  MTKMeshBuffer = interface;
  MTKMesh = interface;
  MTKSubmesh = interface;
  CAMetalDrawable = interface;


// ===== Framework typedefs =====

{$M+}

  MTKTextureLoaderError = NSString;
  PMTKTextureLoaderError = ^MTKTextureLoaderError;
  MTKTextureLoaderOption = NSString;
  PMTKTextureLoaderOption = ^MTKTextureLoaderOption;
  MTKTextureLoaderCubeLayout = NSString;
  PMTKTextureLoaderCubeLayout = ^MTKTextureLoaderCubeLayout;
  MTKTextureLoaderOrigin = NSString;
  PMTKTextureLoaderOrigin = ^MTKTextureLoaderOrigin;
  MTKTextureLoaderCallback = procedure(texture: MTLTexture; error: NSError) of object;
  MTKTextureLoaderArrayCallback = procedure(textures: NSArray; error: NSError) of object;
  MTKModelError = NSString;
  PMTKModelError = ^MTKModelError;


// ===== Interface declarations =====

  CAMetalLayerClass = interface(CALayerClass)
    ['{2307B5AA-EE64-4981-9458-D815A7678A1B}']
  end;
  CAMetalLayer = interface(CALayer)
    ['{31555584-6361-469C-962C-08E1C1BC1060}']
    procedure setDevice(device: MTLDevice); cdecl;
    function device : MTLDevice; cdecl;
    procedure setPixelFormat(pixelFormat: MTLPixelFormat); cdecl;
    function pixelFormat : MTLPixelFormat; cdecl;
    procedure setFramebufferOnly(framebufferOnly: Boolean); cdecl;
    function framebufferOnly : Boolean; cdecl;
    procedure setDrawableSize(drawableSize: CGSize); cdecl;
    function drawableSize : CGSize; cdecl;
    function nextDrawable : CAMetalDrawable; cdecl;
    procedure setMaximumDrawableCount(maximumDrawableCount: NSUInteger); cdecl;
    function maximumDrawableCount : NSUInteger; cdecl;
    procedure setPresentsWithTransaction(presentsWithTransaction: Boolean); cdecl;
    function presentsWithTransaction : Boolean; cdecl;
    procedure setAllowsNextDrawableTimeout(allowsNextDrawableTimeout: Boolean); cdecl;
    function allowsNextDrawableTimeout : Boolean; cdecl;
  end;
  TCAMetalLayer = class(TOCGenericImport<CAMetalLayerClass, CAMetalLayer>)  end;

  CAMetalDrawable = interface(MTLDrawable)
    ['{21E0F785-4690-4358-8401-787213A049BD}']
    function texture : MTLTexture; cdecl;
    function layer : CAMetalLayer; cdecl;
  end;

  {$IF defined(IOS)}
  MTKViewClass = interface(UIViewClass)
    ['{11EF065F-51D2-4B57-8700-B720B030CB27}']
  end;
  MTKView = interface(UIView)
  {$ELSE}
  MTKViewClass = interface(NSViewClass)
    ['{00E38C39-5F1E-4953-8107-0489FE3307A5}']
  end;
  MTKView = interface(NSView)
  {$ENDIF}
    ['{B1CA7942-7BD4-41C1-A928-4AA9D2229C1A}']
    function initWithFrame(frameRect: CGRect; device: MTLDevice) : MTKView {instancetype}; cdecl;
    function initWithCoder(coder: NSCoder) : MTKView {instancetype}; cdecl;
    procedure setDelegate(delegate: MTKViewDelegate); cdecl;
    function delegate : MTKViewDelegate; cdecl;
    procedure setDevice(device: MTLDevice); cdecl;
    function device : MTLDevice; cdecl;
    function currentDrawable : CAMetalDrawable; cdecl;
    procedure setFramebufferOnly(framebufferOnly: Boolean); cdecl;
    function framebufferOnly : Boolean; cdecl;
    procedure setPresentsWithTransaction(presentsWithTransaction: Boolean); cdecl;
    function presentsWithTransaction : Boolean; cdecl;
    procedure setColorPixelFormat(colorPixelFormat: MTLPixelFormat); cdecl;
    function colorPixelFormat : MTLPixelFormat; cdecl;
    procedure setDepthStencilPixelFormat(depthStencilPixelFormat: MTLPixelFormat); cdecl;
    function depthStencilPixelFormat : MTLPixelFormat; cdecl;
    procedure setSampleCount(sampleCount: NSUInteger); cdecl;
    function sampleCount : NSUInteger; cdecl;
    procedure setClearColor(clearColor: MTLClearColor); cdecl;
    function clearColor : MTLClearColor; cdecl;
    procedure setClearDepth(clearDepth: Double); cdecl;
    function clearDepth : Double; cdecl;
    procedure setClearStencil(clearStencil: LongWord); cdecl;
    function clearStencil : LongWord; cdecl;
    function depthStencilTexture : MTLTexture; cdecl;
    function multisampleColorTexture : MTLTexture; cdecl;
    procedure releaseDrawables; cdecl;
    function currentRenderPassDescriptor : MTLRenderPassDescriptor; cdecl;
    procedure setPreferredFramesPerSecond(preferredFramesPerSecond: NSInteger); cdecl;
    function preferredFramesPerSecond : NSInteger; cdecl;
    procedure setEnableSetNeedsDisplay(enableSetNeedsDisplay: Boolean); cdecl;
    function enableSetNeedsDisplay : Boolean; cdecl;
    procedure setAutoResizeDrawable(autoResizeDrawable: Boolean); cdecl;
    function autoResizeDrawable : Boolean; cdecl;
    procedure setDrawableSize(drawableSize: CGSize); cdecl;
    function drawableSize : CGSize; cdecl;
    procedure setPaused(paused: Boolean); cdecl;
    function isPaused : Boolean; cdecl;
    procedure setColorspace(colorspace: CGColorSpaceRef); cdecl;
    function colorspace : CGColorSpaceRef; cdecl;
    procedure draw; cdecl;
  end;
  TMTKView = class(TOCGenericImport<MTKViewClass, MTKView>) end;
  PMTKView = Pointer;

  MTKTextureLoaderClass = interface(NSObjectClass)
    ['{026B6EB0-0189-4D90-A7B1-DE09E924E6B0}']
  end;
  MTKTextureLoader = interface(NSObject)
    ['{FF17ED92-6D41-462F-93E6-CD6BB30C31DC}']
    function device : MTLDevice; cdecl;
    function initWithDevice(device: MTLDevice) : MTKTextureLoader {instancetype}; cdecl;
    [MethodName('newTextureWithContentsOfURL:options:completionHandler:')]
    procedure newTextureWithContentsOfURLOptionsCompletionHandler(URL: NSURL; options: NSDictionary; completionHandler: MTKTextureLoaderCallback); cdecl;
    [MethodName('newTextureWithName:scaleFactor:bundle:options:completionHandler:')]
    procedure newTextureWithNameScaleFactorBundleOptionsCompletionHandler(name: NSString; scaleFactor: CGFloat; bundle: NSBundle; options: NSDictionary; completionHandler: MTKTextureLoaderCallback); cdecl;
    [MethodName('newTexturesWithContentsOfURLs:options:completionHandler:')]
    procedure newTexturesWithContentsOfURLsOptionsCompletionHandler(URLs: NSArray; options: NSDictionary; completionHandler: MTKTextureLoaderArrayCallback); cdecl;
    procedure newTexturesWithNames(names: NSArray; scaleFactor: CGFloat; bundle: NSBundle; options: NSDictionary; completionHandler: MTKTextureLoaderArrayCallback); cdecl;
    [MethodName('newTextureWithData:options:completionHandler:')]
    procedure newTextureWithDataOptionsCompletionHandler(data: NSData; options: NSDictionary; completionHandler: MTKTextureLoaderCallback); cdecl;
    [MethodName('newTextureWithCGImage:options:completionHandler:')]
    procedure newTextureWithCGImageOptionsCompletionHandler(cgImage: CGImageRef; options: NSDictionary; completionHandler: MTKTextureLoaderCallback); cdecl;
    //[MethodName('newTextureWithMDLTexture:options:completionHandler:')]
    //procedure newTextureWithMDLTextureOptionsCompletionHandler(texture: MDLTexture; options: NSDictionary; completionHandler: MTKTextureLoaderCallback); cdecl;
    [MethodName('newTextureWithContentsOfURL:options:error:')]
    function newTextureWithContentsOfURLOptionsError(URL: NSURL; options: NSDictionary; error: NSError) : MTLTexture; cdecl;
    [MethodName('newTexturesWithContentsOfURLs:options:error:')]
    function newTexturesWithContentsOfURLsOptionsError(URLs: NSArray; options: NSDictionary; error: NSError) : NSArray; cdecl;
    [MethodName('newTextureWithData:options:error:')]
    function newTextureWithDataOptionsError(data: NSData; options: NSDictionary; error: NSError) : MTLTexture; cdecl;
    [MethodName('newTextureWithCGImage:options:error:')]
    function newTextureWithCGImageOptionsError(cgImage: CGImageRef; options: NSDictionary; error: NSError) : MTLTexture; cdecl;
    //[MethodName('newTextureWithMDLTexture:options:error:')]
    //function newTextureWithMDLTextureOptionsError(texture: MDLTexture; options: NSDictionary; error: NSError) : MTLTexture; cdecl;
    [MethodName('newTextureWithName:scaleFactor:bundle:options:error:')]
    function newTextureWithNameScaleFactorBundleOptionsError(name: NSString; scaleFactor: CGFloat; bundle: NSBundle; options: NSDictionary; error: NSError) : MTLTexture; cdecl;
  end;
  TMTKTextureLoader = class(TOCGenericImport<MTKTextureLoaderClass, MTKTextureLoader>) end;
  PMTKTextureLoader = Pointer;

  MTKMeshBufferAllocatorClass = interface(NSObjectClass)
    ['{D8C9C4B7-69D5-4228-831D-21297EC54825}']
  end;
  MTKMeshBufferAllocator = interface(NSObject)
    ['{6268F2C2-09C9-4EE5-B044-2AC06E70CB4B}']
    function initWithDevice(device: MTLDevice) : MTKMeshBufferAllocator {instancetype}; cdecl;
    function device : MTLDevice; cdecl;
  end;
  TMTKMeshBufferAllocator = class(TOCGenericImport<MTKMeshBufferAllocatorClass, MTKMeshBufferAllocator>)  end;
  PMTKMeshBufferAllocator = Pointer;

  MTKMeshBufferClass = interface(NSObjectClass)
    ['{79D15C52-086C-4712-88A7-3E301CF05F2C}']
  end;
  MTKMeshBuffer = interface(NSObject)
    ['{C6EE4B6C-BED5-4D66-852B-6C9FE84CBCC7}']
    function length : NSUInteger; cdecl;
    function allocator : MTKMeshBufferAllocator; cdecl;
    //function zone : MDLMeshBufferZone; cdecl;
    function buffer : MTLBuffer; cdecl;
    function offset : NSUInteger; cdecl;
    function &type : Integer; cdecl;
  end;
  TMTKMeshBuffer = class(TOCGenericImport<MTKMeshBufferClass, MTKMeshBuffer>)  end;
  PMTKMeshBuffer = Pointer;

  MTKMeshClass = interface(NSObjectClass)
    ['{D7BA7E0A-003F-4B22-9B80-B738A1C0C46E}']
    //{class} function newMeshesFromAsset(asset: MDLAsset; device: MTLDevice; sourceMeshes: NSArray; error: NSError) : NSArray; cdecl;
  end;
  MTKMesh = interface(NSObject)
    ['{EFD4A437-5B0E-4897-BD57-D3C854908D48}']
    //function initWithMesh(mesh: MDLMesh; device: MTLDevice; error: NSError) : MTKMesh {instancetype}; cdecl;
    function vertexBuffers : NSArray; cdecl;
    function vertexDescriptor : PInteger; cdecl;
    function submeshes : NSArray; cdecl;
    function vertexCount : NSUInteger; cdecl;
    procedure setName(name: NSString); cdecl;
    function name : NSString; cdecl;
  end;
  TMTKMesh = class(TOCGenericImport<MTKMeshClass, MTKMesh>)  end;
  PMTKMesh = Pointer;

  MTKSubmeshClass = interface(NSObjectClass)
    ['{EB3325F1-09EF-437A-809A-7390B52B6816}']
  end;
  MTKSubmesh = interface(NSObject)
    ['{D4576A09-D9E2-4388-8F92-718A34A1A23E}']
    function primitiveType : MTLPrimitiveType; cdecl;
    function indexType : MTLIndexType; cdecl;
    function indexBuffer : MTKMeshBuffer; cdecl;
    function indexCount : NSUInteger; cdecl;
    function mesh : MTKMesh; cdecl;
    procedure setName(name: NSString); cdecl;
    function name : NSString; cdecl;
  end;
  TMTKSubmesh = class(TOCGenericImport<MTKSubmeshClass, MTKSubmesh>)  end;
  PMTKSubmesh = Pointer;


  // ===== Protocol declarations =====

  MTKViewDelegate = interface(IObjectiveC)
    ['{5FF37054-5EAA-46DB-B278-60C97C9EDCB3}']
    procedure mtkView(view: MTKView; drawableSizeWillChange: CGSize); cdecl;
    procedure drawInMTKView(view: MTKView); cdecl;
  end;


// ===== Exported string consts =====

function MTKTextureLoaderErrorDomain: MTKTextureLoaderError;
function MTKTextureLoaderErrorKey: MTKTextureLoaderError;
function MTKTextureLoaderOptionAllocateMipmaps: MTKTextureLoaderOption;
function MTKTextureLoaderOptionGenerateMipmaps: MTKTextureLoaderOption;
function MTKTextureLoaderOptionSRGB: MTKTextureLoaderOption;
function MTKTextureLoaderOptionTextureUsage: MTKTextureLoaderOption;
function MTKTextureLoaderOptionTextureCPUCacheMode: MTKTextureLoaderOption;
function MTKTextureLoaderOptionTextureStorageMode: MTKTextureLoaderOption;
function MTKTextureLoaderOptionCubeLayout: MTKTextureLoaderOption;
function MTKTextureLoaderCubeLayoutVertical: MTKTextureLoaderOption;
function MTKTextureLoaderOptionOrigin: MTKTextureLoaderOption;
function MTKTextureLoaderOriginTopLeft: MTKTextureLoaderOption;
function MTKTextureLoaderOriginBottomLeft: MTKTextureLoaderOption;
function MTKTextureLoaderOriginFlippedVertically: MTKTextureLoaderOption;
function MTKModelErrorDomain: MTKModelError;
function MTKModelErrorKey: MTKModelError;


// ===== External functions =====

const
  libMetalKit = '/System/Library/Frameworks/MetalKit.framework/MetalKit';

function MTKModelIOVertexDescriptorFromMetal(metalDescriptor: Pointer {MTLVertexDescriptor}): PInteger; cdecl; external libMetalKit name _PU + 'MTKModelIOVertexDescriptorFromMetal';
function MTKModelIOVertexDescriptorFromMetalWithError(metalDescriptor: Pointer {MTLVertexDescriptor}; error: Pointer {NSError}): PInteger; cdecl; external libMetalKit name _PU + 'MTKModelIOVertexDescriptorFromMetalWithError';
function MTKMetalVertexDescriptorFromModelIO(modelIODescriptor: PInteger): Pointer {MTLVertexDescriptor}; cdecl; external libMetalKit name _PU + 'MTKMetalVertexDescriptorFromModelIO';
function MTKMetalVertexDescriptorFromModelIOWithError(modelIODescriptor: PInteger; error: Pointer {NSError}): Pointer {MTLVertexDescriptor}; cdecl; external libMetalKit name _PU + 'MTKMetalVertexDescriptorFromModelIOWithError';
function MTKModelIOVertexFormatFromMetal(vertexFormat: MTLVertexFormat): Integer; cdecl; external libMetalKit name _PU + 'MTKModelIOVertexFormatFromMetal';
function MTKMetalVertexFormatFromModelIO(vertexFormat: Integer): MTLVertexFormat; cdecl; external libMetalKit name _PU + 'MTKMetalVertexFormatFromModelIO';


implementation

{$IF defined(MACOS64)}

uses
  Posix.Dlfcn;

var
  MetalKitModule: THandle;

{$ENDIF}


function MTKTextureLoaderErrorDomain: MTKTextureLoaderError;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderErrorDomain');
end;

function MTKTextureLoaderErrorKey: MTKTextureLoaderError;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderErrorKey');
end;

function MTKTextureLoaderOptionAllocateMipmaps: MTKTextureLoaderOption;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderOptionAllocateMipmaps');
end;

function MTKTextureLoaderOptionGenerateMipmaps: MTKTextureLoaderOption;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderOptionGenerateMipmaps');
end;

function MTKTextureLoaderOptionSRGB: MTKTextureLoaderOption;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderOptionSRGB');
end;

function MTKTextureLoaderOptionTextureUsage: MTKTextureLoaderOption;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderOptionTextureUsage');
end;

function MTKTextureLoaderOptionTextureCPUCacheMode: MTKTextureLoaderOption;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderOptionTextureCPUCacheMode');
end;

function MTKTextureLoaderOptionTextureStorageMode: MTKTextureLoaderOption;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderOptionTextureStorageMode');
end;

function MTKTextureLoaderOptionCubeLayout: MTKTextureLoaderOption;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderOptionCubeLayout');
end;

function MTKTextureLoaderCubeLayoutVertical: MTKTextureLoaderOption;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderCubeLayoutVertical');
end;

function MTKTextureLoaderOptionOrigin: MTKTextureLoaderOption;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderOptionOrigin');
end;

function MTKTextureLoaderOriginTopLeft: MTKTextureLoaderOption;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderOriginTopLeft');
end;

function MTKTextureLoaderOriginBottomLeft: MTKTextureLoaderOption;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderOriginBottomLeft');
end;

function MTKTextureLoaderOriginFlippedVertically: MTKTextureLoaderOption;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKTextureLoaderOriginFlippedVertically');
end;

function MTKModelErrorDomain: MTKModelError;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKModelErrorDomain');
end;

function MTKModelErrorKey: MTKModelError;
begin
  Result := CocoaNSStringConst(libMetalKit, 'MTKModelErrorKey');
end;

{$IF defined(MACOS64)}

initialization
  MetalKitModule := dlopen(MarshaledAString(libMetalKit), RTLD_LAZY);

finalization
  dlclose(MetalKitModule);

{$ENDIF}

end.
