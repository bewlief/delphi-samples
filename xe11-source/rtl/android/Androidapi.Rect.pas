{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.Rect;

interface

(*$HPPEMIT '#include <android/rect.h>' *)

{$I Androidapi.inc}

type
  /// <summary>Rectangular window area.</summary>
  /// <remarks>This is the NDK equivalent of the android.graphics.Rect class
  /// in Java. It is used with ANativeActivityCallbacks::onContentRectChanged
  /// event callback and the ANativeWindow_lock() function.<br />
  /// In a valid ARect, left &lt;= right and top &lt;= bottom. ARect with left=0,
  /// top=10, right=1, bottom=11 contains only one pixel at x=0, y=10.</remarks>
  ARect = record
    /// <summary>
    ///   Minimum X coordinate of the rectangle.
    /// </summary>
    left: Int32;
    /// <summary>
    ///   Minimum Y coordinate of the rectangle.
    /// </summary>
    top: Int32;
    /// <summary>
    ///   Maximum X coordinate of the rectangle.
    /// </summary>
    right: Int32;
    /// <summary>
    ///   Maximum Y coordinate of the rectangle.
    /// </summary>
    bottom: Int32;
  end;
  {$EXTERNALSYM ARect}
  PARect = ^ARect;
  
implementation

end.

