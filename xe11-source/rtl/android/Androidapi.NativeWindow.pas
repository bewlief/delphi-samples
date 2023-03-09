{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.NativeWindow;

interface

uses Androidapi.Rect;

(*$HPPEMIT '#include <android/native_window.h>' *)
(*$HPPEMIT '#include <android/hardware_buffer.h>' *)

{$I Androidapi.inc}

{ Buffer pixel format }
const
  /// <summary>Corresponding formats:<br />
  /// Vulkan: VK_FORMAT_R8G8B8A8_UNORM<br />
  /// OpenGL ES: GL_RGBA8</summary>
  AHARDWAREBUFFER_FORMAT_R8G8B8A8_UNORM     = 1;
  {$EXTERNALSYM AHARDWAREBUFFER_FORMAT_R8G8B8A8_UNORM}

  /// <summary>32 bits per pixel, 8 bits per channel format where alpha values
  /// are ignored (always opaque).<br />
  /// Corresponding formats:<br />
  /// Vulkan: VK_FORMAT_R8G8B8A8_UNORM<br />
  /// OpenGL ES: GL_RGB8</summary>
  AHARDWAREBUFFER_FORMAT_R8G8B8X8_UNORM     = 2;
  {$EXTERNALSYM AHARDWAREBUFFER_FORMAT_R8G8B8X8_UNORM}

  /// <summary>Corresponding formats:<br />
  /// Vulkan: VK_FORMAT_R8G8B8_UNORM<br />
  /// OpenGL ES: GL_RGB8</summary>
  AHARDWAREBUFFER_FORMAT_R8G8B8_UNORM       = 3;
  {$EXTERNALSYM AHARDWAREBUFFER_FORMAT_R8G8B8_UNORM}

  /// <summary>Corresponding formats:<br />
  /// Vulkan: VK_FORMAT_R5G6B5_UNORM_PACK16<br />
  /// OpenGL ES: GL_RGB565</summary>
  AHARDWAREBUFFER_FORMAT_R5G6B5_UNORM       = 4;
  {$EXTERNALSYM AHARDWAREBUFFER_FORMAT_R5G6B5_UNORM}

  /// <summary>Corresponding formats:<br />
  /// Vulkan: VK_FORMAT_R16G16B16A16_SFLOAT<br />
  /// OpenGL ES: GL_RGBA16F</summary>
  AHARDWAREBUFFER_FORMAT_R16G16B16A16_FLOAT = $16;
  {$EXTERNALSYM AHARDWAREBUFFER_FORMAT_R16G16B16A16_FLOAT}

  /// <summary>Corresponding formats:<br />
  /// Vulkan: VK_FORMAT_A2B10G10R10_UNORM_PACK32<br />
  /// OpenGL ES: GL_RGB10_A2</summary>
  AHARDWAREBUFFER_FORMAT_R10G10B10A2_UNORM  = $2b;
  {$EXTERNALSYM AHARDWAREBUFFER_FORMAT_R10G10B10A2_UNORM}

  /// <summary>An opaque binary blob format that must have height 1, with width
  /// equal to the buffer size in bytes.</summary>
  AHARDWAREBUFFER_FORMAT_BLOB               = $21;
  {$EXTERNALSYM AHARDWAREBUFFER_FORMAT_BLOB}

  /// <summary>Corresponding formats:<br />
  /// Vulkan: VK_FORMAT_D16_UNORM<br />
  /// OpenGL ES: GL_DEPTH_COMPONENT16</summary>
  AHARDWAREBUFFER_FORMAT_D16_UNORM          = $30;
  {$EXTERNALSYM AHARDWAREBUFFER_FORMAT_D16_UNORM}

  /// <summary>Corresponding formats:<br />
  /// Vulkan: VK_FORMAT_X8_D24_UNORM_PACK32<br />
  /// OpenGL ES: GL_DEPTH_COMPONENT24</summary>
  AHARDWAREBUFFER_FORMAT_D24_UNORM          = $31;
  {$EXTERNALSYM AHARDWAREBUFFER_FORMAT_D24_UNORM}

  /// <summary>Corresponding formats:<br />
  /// Vulkan: VK_FORMAT_D24_UNORM_S8_UINT<br />
  /// OpenGL ES: GL_DEPTH24_STENCIL8</summary>
  AHARDWAREBUFFER_FORMAT_D24_UNORM_S8_UINT  = $32;
  {$EXTERNALSYM AHARDWAREBUFFER_FORMAT_D24_UNORM_S8_UINT}

  /// <summary>Corresponding formats:<br />
  /// Vulkan: VK_FORMAT_D32_SFLOAT<br />
  /// OpenGL ES: GL_DEPTH_COMPONENT32F</summary>
  AHARDWAREBUFFER_FORMAT_D32_FLOAT          = $33;
  {$EXTERNALSYM AHARDWAREBUFFER_FORMAT_D32_FLOAT}

  /// <summary>Corresponding formats:<br />
  /// Vulkan: VK_FORMAT_D32_SFLOAT_S8_UINT<br />
  /// OpenGL ES: GL_DEPTH32F_STENCIL8</summary>
  AHARDWAREBUFFER_FORMAT_D32_FLOAT_S8_UINT  = $34;
  {$EXTERNALSYM AHARDWAREBUFFER_FORMAT_D32_FLOAT_S8_UINT}

  /// <summary>Corresponding formats:<br />
  /// Vulkan: VK_FORMAT_S8_UINT<br />
  /// OpenGL ES: GL_STENCIL_INDEX8</summary>
  AHARDWAREBUFFER_FORMAT_S8_UINT            = $35;
  {$EXTERNALSYM AHARDWAREBUFFER_FORMAT_S8_UINT}

{ Pixel formats that a window can use }
const
  /// <summary>Red: 8 bits, Green: 8 bits, Blue: 8 bits, Alpha: 8 bits.</summary>
  WINDOW_FORMAT_RGBA_8888 = AHARDWAREBUFFER_FORMAT_R8G8B8A8_UNORM;
  {$EXTERNALSYM WINDOW_FORMAT_RGBA_8888}

  /// <summary>Red: 8 bits, Green: 8 bits, Blue: 8 bits, Unused: 8 bits.</summary>
  WINDOW_FORMAT_RGBX_8888 = AHARDWAREBUFFER_FORMAT_R8G8B8X8_UNORM;
  {$EXTERNALSYM WINDOW_FORMAT_RGBX_8888}

  /// <summary>Red: 5 bits, Green: 6 bits, Blue: 5 bits.</summary>
  WINDOW_FORMAT_RGB_565   = AHARDWAREBUFFER_FORMAT_R5G6B5_UNORM;
  {$EXTERNALSYM WINDOW_FORMAT_RGB_565}

{ Transforms that can be applied to buffers as they are displayed to a window.

  Supported transforms are any combination of horizontal mirror, vertical
  mirror, and clockwise 90 degree rotation, in that order. Rotations of 180
  and 270 degrees are made up of those basic transforms. }
const
  ANATIVEWINDOW_TRANSFORM_IDENTITY            = 0;
  {$EXTERNALSYM ANATIVEWINDOW_TRANSFORM_IDENTITY}

  ANATIVEWINDOW_TRANSFORM_MIRROR_HORIZONTAL   = 1;
  {$EXTERNALSYM ANATIVEWINDOW_TRANSFORM_MIRROR_HORIZONTAL}

  ANATIVEWINDOW_TRANSFORM_MIRROR_VERTICAL     = 2;
  {$EXTERNALSYM ANATIVEWINDOW_TRANSFORM_MIRROR_VERTICAL}

  ANATIVEWINDOW_TRANSFORM_ROTATE_90           = 4;
  {$EXTERNALSYM ANATIVEWINDOW_TRANSFORM_ROTATE_90}

  ANATIVEWINDOW_TRANSFORM_ROTATE_180          = ANATIVEWINDOW_TRANSFORM_MIRROR_HORIZONTAL or
                                                ANATIVEWINDOW_TRANSFORM_MIRROR_VERTICAL;
  {$EXTERNALSYM ANATIVEWINDOW_TRANSFORM_ROTATE_180}

  ANATIVEWINDOW_TRANSFORM_ROTATE_270          = ANATIVEWINDOW_TRANSFORM_ROTATE_180 or
                                                ANATIVEWINDOW_TRANSFORM_ROTATE_90;
  {$EXTERNALSYM ANATIVEWINDOW_TRANSFORM_ROTATE_270}

type
  /// <summary>Opaque type that provides access to a native window. A pointer
  /// can be obtained using ANativeWindow_fromSurface()</summary>
  ANativeWindow = record end;
  {$EXTERNALSYM ANativeWindow}
  PANativeWindow = ^ANativeWindow;

  /// <summary>Struct that represents a windows buffer. A pointer can be
  /// obtained using ANativeWindow_lock()</summary>
  ANativeWindow_Buffer = record
    /// <summary>The number of pixels that are show horizontally</summary>
    width: Int32;
    /// <summary>The number of pixels that are shown vertically.</summary>
    height: Int32;
    /// <summary>The number of *pixels* that a line in the buffer takes in
    /// memory. This may be >= width.</summary>
    stride: Int32;
    /// <summary>The format of the buffer. One of AHARDWAREBUFFER_FORMAT_*
    /// </summary>
    format: Int32;
    /// <summary>The actual bits.</summary>
    bits: Pointer;
    /// <summary>Do not touch.</summary>
    reserved : array [0..5] of UInt32;
  end;
  {$EXTERNALSYM ANativeWindow_Buffer}
  PANativeWindow_Buffer = ^ANativeWindow_Buffer;

/// <summary>Acquire a reference on the given ANativeWindow object. This
/// prevents the object from being deleted until the reference is removed.
/// </summary>
procedure ANativeWindow_acquire(Window: PANativeWindow); cdecl;
  external AndroidLib name 'ANativeWindow_acquire';
{$EXTERNALSYM ANativeWindow_acquire}

/// <summary>Remove a reference that was previously acquired with
/// ANativeWindow_acquire().</summary>
procedure ANativeWindow_release(Window: PANativeWindow); cdecl;
  external AndroidLib name 'ANativeWindow_release';
{$EXTERNALSYM ANativeWindow_release}

/// <summary>Return the current width in pixels of the window surface. Returns a
/// negative value on error.</summary>
function ANativeWindow_getWidth(Window: PANativeWindow): Int32; cdecl;
  external AndroidLib name 'ANativeWindow_getWidth';
{$EXTERNALSYM ANativeWindow_getWidth}

/// <summary>Return the current height in pixels of the window surface. Returns
/// a negative value on error.</summary>
function ANativeWindow_getHeight(Window: PANativeWindow): Int32; cdecl;
  external AndroidLib name 'ANativeWindow_getHeight';
{$EXTERNALSYM ANativeWindow_getHeight}

/// <summary>Return the current pixel format (AHARDWAREBUFFER_FORMAT_*) of the
/// window surface. Returns a negative value on error.</summary>
function ANativeWindow_getFormat(Window: PANativeWindow): Int32; cdecl;
  external AndroidLib name 'ANativeWindow_getFormat';
{$EXTERNALSYM ANativeWindow_getFormat}

/// <summary>Change the format and size of the window buffers.<br />
/// The width and height control the number of pixels in the buffers, not the
/// dimensions of the window on screen. If these are different than the window's
/// physical size, then its buffer will be scaled to match that size when
/// compositing it to the screen. The width and height must be either both zero
/// or both non-zero.<br />
/// For all of these parameters, if 0 is supplied then the window's base value
/// will come back in force.<br />
/// Width: width of the buffers in pixels.<br />
/// Height: height of the buffers in pixels.<br />
/// Format: one of AHARDWAREBUFFER_FORMAT_* constants.<br />
/// Returns 0 for success, or a negative value on error.</summary>
function ANativeWindow_setBuffersGeometry(Window: PANativeWindow; Width, Height, Format: Int32): Int32; cdecl;
  external AndroidLib name 'ANativeWindow_setBuffersGeometry';
{$EXTERNALSYM ANativeWindow_setBuffersGeometry}

/// <summary>Lock the window's next drawing surface for writing.
/// InOutDirtyBounds is used as an in/out parameter, upon entering the function,
/// it contains the dirty region, that is, the region the caller intends to
/// redraw. When the function returns, InOutDirtyBounds is updated with the
/// actual area the caller needs to redraw -- this region is often extended by
/// ANativeWindow_lock.<br />
/// Returns 0 for success, or a negative value on error.</summary>
function ANativeWindow_lock(Window: PANativeWindow; OutBuffer: PANativeWindow_Buffer; InOutDirtyBounds: PARect): Int32; cdecl;
  external AndroidLib name 'ANativeWindow_lock';
{$EXTERNALSYM ANativeWindow_lock}

/// <summary>Unlock the window's drawing surface after previously locking it,
/// posting the new buffer to the display.<br />
/// Returns 0 for success, or a negative value on error.</summary>
function ANativeWindow_unlockAndPost(Window: PANativeWindow): Int32; cdecl;
  external AndroidLib name 'ANativeWindow_unlockAndPost';
{$EXTERNALSYM ANativeWindow_unlockAndPost}

(*
/// <remarks>Introduced in API 26</remarks>
/// <summary>Set a transform that will be applied to future buffers posted to
/// the window.<br />
/// Transform: combination of ANATIVEWINDOW_TRANSFORM_* constants<br />
/// Returns 0 for success, or -EINVAL if Transform is invalid</summary>
function ANativeWindow_setBuffersTransform(Window: PANativeWindow; Transform: Int32): Int32; cdecl;
  external AndroidNativeWindowLib name 'ANativeWindow_setBuffersTransform';
{$EXTERNALSYM ANativeWindow_setBuffersTransform}

/// <remarks>Introduced in API 28</remarks>
/// <summary>All buffers queued after this call will be associated with the
/// DataSpace parameter specified.<br />
/// DataSpace specifies additional information about the buffer. For example, it
/// can be used to convey the color space of the image data in the buffer, or it
/// can be used to indicate that the buffers contain depth measurement data
/// instead of color images. The default dataSpace is 0, ADATASPACE_UNKNOWN,
/// unless it has been overridden by the producer.<br />
/// DataSpace: data space of all buffers queued after this call.<br />
/// Returns 0 for success, -EINVAL if window is invalid or the dataspace is not
/// supported.</summary>
function ANativeWindow_setBuffersDataSpace(Window: PANativeWindow; DataSpace: Int32): Int32; cdecl;
  external AndroidNativeWindowLib name 'ANativeWindow_setBuffersDataSpace';
{$EXTERNALSYM ANativeWindow_setBuffersDataSpace}

/// <remarks>Introduced in API 28</remarks>
/// <summary>Get the dataspace of the buffers in window.<br />
/// Returns the dataspace of buffers in window, ADATASPACE_UNKNOWN is returned
/// if dataspace is unknown, or -EINVAL if window is invalid.</summary>
function ANativeWindow_getBuffersDataSpace(Window: PANativeWindow): Int32; cdecl;
  external AndroidNativeWindowLib name 'ANativeWindow_getBuffersDataSpace';
{$EXTERNALSYM ANativeWindow_getBuffersDataSpace}
*)

implementation

end.
