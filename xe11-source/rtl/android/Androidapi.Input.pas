{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.Input;

interface

uses Posix.SysTypes, Posix.StdDef, Androidapi.Looper, Androidapi.KeyCodes;

(*$HPPEMIT '#include <android/input.h>' *)

{$I Androidapi.inc}

{ Structures and functions to receive and process input events in
  native code.

  NOTE: These functions MUST be implemented by /system/lib/libui.so }

{ Key states (may be returned by queries about the current state of a
  particular key code, scan code or switch). }
const
  /// <summary>The key state is unknown or the requested key itself is not
  /// supported.</summary>
  AKEY_STATE_UNKNOWN = - 1;
  {$EXTERNALSYM AKEY_STATE_UNKNOWN}
  
  /// <summary>The key is up.</summary>
  AKEY_STATE_UP = 0;
  {$EXTERNALSYM AKEY_STATE_UP}

  /// <summary>The key is down.</summary>
  AKEY_STATE_DOWN = 1;
  {$EXTERNALSYM AKEY_STATE_DOWN}

  /// <summary>The key is down but is a virtual key press that is being emulated
  /// by the system.</summary>
  AKEY_STATE_VIRTUAL = 2;
  {$EXTERNALSYM AKEY_STATE_VIRTUAL}

{ Meta key / modifer state. }
const
  /// <summary>No meta keys are pressed.</summary>
  AMETA_NONE = 0;
  {$EXTERNALSYM AMETA_NONE}

  /// <summary>This mask is used to check whether one of the ALT meta keys is
  /// pressed.</summary>
  AMETA_ALT_ON = $02;
  {$EXTERNALSYM AMETA_ALT_ON}

  /// <summary>This mask is used to check whether the left ALT meta key is
  // pressed.</summary>
  AMETA_ALT_LEFT_ON = $10;
  {$EXTERNALSYM AMETA_ALT_LEFT_ON}

  /// <summary>This mask is used to check whether the right ALT meta key is
  /// pressed.</summary>
  AMETA_ALT_RIGHT_ON = $20;
  {$EXTERNALSYM AMETA_ALT_RIGHT_ON}

  /// <summary>This mask is used to check whether one of the SHIFT meta keys is
  /// pressed.</summary>
  AMETA_SHIFT_ON = $01;
  {$EXTERNALSYM AMETA_SHIFT_ON}

  /// <summary>This mask is used to check whether the left SHIFT meta key is
  /// pressed.</summary>
  AMETA_SHIFT_LEFT_ON = $40;
  {$EXTERNALSYM AMETA_SHIFT_LEFT_ON}
  
  /// <summary>This mask is used to check whether the right SHIFT meta key is
  /// pressed.</summary>
  AMETA_SHIFT_RIGHT_ON = $80;
  {$EXTERNALSYM AMETA_SHIFT_RIGHT_ON}

  /// <summary>This mask is used to check whether the SYM meta key is pressed.
  /// </summary>
  AMETA_SYM_ON = $04;
  {$EXTERNALSYM AMETA_SYM_ON}

  /// <summary> This mask is used to check whether the FUNCTION meta key is
  /// pressed.</summary>
  AMETA_FUNCTION_ON = $08;
  {$EXTERNALSYM AMETA_FUNCTION_ON}

  /// <summary> This mask is used to check whether one of the CTRL meta keys is
  /// pressed.</summary>
  AMETA_CTRL_ON = $1000;
  {$EXTERNALSYM AMETA_CTRL_ON}

  /// <summary>This mask is used to check whether the left CTRL meta key is
  /// pressed.</summary>
  AMETA_CTRL_LEFT_ON = $2000;
  {$EXTERNALSYM AMETA_CTRL_LEFT_ON}

  /// <summary>This mask is used to check whether the right CTRL meta key is
  /// pressed.</summary>
  AMETA_CTRL_RIGHT_ON = $4000;
  {$EXTERNALSYM AMETA_CTRL_RIGHT_ON}

  /// <summary>This mask is used to check whether one of the META meta keys is
  /// pressed.</summary>
  AMETA_META_ON = $10000;
  {$EXTERNALSYM AMETA_META_ON}

  /// <summary>This mask is used to check whether the left META meta key is
  /// pressed.</summary>
  AMETA_META_LEFT_ON = $20000;
  {$EXTERNALSYM AMETA_META_LEFT_ON}

  /// <summary>This mask is used to check whether the right META meta key is
  /// pressed.</summary>
  AMETA_META_RIGHT_ON = $40000;
  {$EXTERNALSYM AMETA_META_RIGHT_ON}

  /// <summary>This mask is used to check whether the CAPS LOCK meta key is
  /// pressed.</summary>
  AMETA_CAPS_LOCK_ON = $100000;
  {$EXTERNALSYM AMETA_CAPS_LOCK_ON}

  /// <summary>This mask is used to check whether the NUM LOCK meta key is
  /// pressed.</summary>
  AMETA_NUM_LOCK_ON = $200000;
  {$EXTERNALSYM AMETA_NUM_LOCK_ON}

  /// <summary>This mask is used to check whether the SCROLL LOCK meta key is
  /// pressed.</summary>
  AMETA_SCROLL_LOCK_ON = $400000;
  {$EXTERNALSYM AMETA_SCROLL_LOCK_ON}

{ Input event types. }
const
  /// <summary>Indicates that the input event is a key event.</summary>
  AINPUT_EVENT_TYPE_KEY = 1;
  {$EXTERNALSYM AINPUT_EVENT_TYPE_KEY}

  /// <summary>Indicates that the input event is a motion event.</summary>
  AINPUT_EVENT_TYPE_MOTION = 2;
  {$EXTERNALSYM AINPUT_EVENT_TYPE_MOTION}

{ Key event actions. }
const
  /// <summary>The key has been pressed down.</summary>
  AKEY_EVENT_ACTION_DOWN = 0;
  {$EXTERNALSYM AKEY_EVENT_ACTION_DOWN}

  /// <summary>The key has been released.</summary>
  AKEY_EVENT_ACTION_UP = 1;
  {$EXTERNALSYM AKEY_EVENT_ACTION_UP}

  /// <summary>Multiple duplicate key events have occurred in a row, or a
  /// complex string is being delivered.  The repeat_count property of the key
  /// event contains the number of times the given key code should be
  /// executed.</summary>
  AKEY_EVENT_ACTION_MULTIPLE = 2;
  {$EXTERNALSYM AKEY_EVENT_ACTION_MULTIPLE}

{ Key event flags. }
const
  /// <summary>This mask is set if the device woke because of this key
  /// event.</summary>
  AKEY_EVENT_FLAG_WOKE_HERE = $1;
  {$EXTERNALSYM AKEY_EVENT_FLAG_WOKE_HERE}

  /// <summary>This mask is set if the key event was generated by a software
  /// keyboard.</summary>
  AKEY_EVENT_FLAG_SOFT_KEYBOARD = $2;
  {$EXTERNALSYM AKEY_EVENT_FLAG_SOFT_KEYBOARD}

  /// <summary>This mask is set if we don't want the key event to cause us to
  /// leave touch mode.</summary>
  AKEY_EVENT_FLAG_KEEP_TOUCH_MODE = $4;
  {$EXTERNALSYM AKEY_EVENT_FLAG_KEEP_TOUCH_MODE}

  /// <summary>This mask is set if an event was known to come from a trusted
  /// part of the system.  That is, the event is known to come from the user,
  /// and could not have been spoofed by a third party component.</summary>
  AKEY_EVENT_FLAG_FROM_SYSTEM = $8;
  {$EXTERNALSYM AKEY_EVENT_FLAG_FROM_SYSTEM}

  /// <summary>This mask is used for compatibility, to identify enter keys that
  /// are coming from an IME whose enter key has been auto-labelled "next" or
  /// "done".  This allows TextView to dispatch these as normal enter keys for
  /// old applications, but still do the appropriate action when receiving them.
  /// </summary>
  AKEY_EVENT_FLAG_EDITOR_ACTION = $10;
  {$EXTERNALSYM AKEY_EVENT_FLAG_EDITOR_ACTION}
  
  /// <summary>When associated with up key events, this indicates that the key
  /// press has been canceled.  Typically this is used with virtual touch screen
  /// keys, where the user can slide from the virtual key area on to the
  /// display: in that case, the application will receive a canceled up event
  /// and should not perform the action normally associated with the key.  Note
  /// that for this to work, the application can not perform an action for a key
  /// until it receives an up or the long press timeout has expired.</summary>
  AKEY_EVENT_FLAG_CANCELED = $20;
  {$EXTERNALSYM AKEY_EVENT_FLAG_CANCELED}

  /// <summary>This key event was generated by a virtual (on-screen) hard key
  /// area. Typically this is an area of the touchscreen, outside of the regular
  /// display, dedicated to "hardware" buttons.</summary>
  AKEY_EVENT_FLAG_VIRTUAL_HARD_KEY = $40;
  {$EXTERNALSYM AKEY_EVENT_FLAG_VIRTUAL_HARD_KEY}

  /// <summary>This flag is set for the first key repeat that occurs after the
  /// long press timeout.</summary>
  AKEY_EVENT_FLAG_LONG_PRESS = $80;
  {$EXTERNALSYM AKEY_EVENT_FLAG_LONG_PRESS}

  /// <summary>Set when a key event has AKEY_EVENT_FLAG_CANCELED set because a
  /// long press action was executed while it was down.</summary>
  AKEY_EVENT_FLAG_CANCELED_LONG_PRESS = $100;
  {$EXTERNALSYM AKEY_EVENT_FLAG_CANCELED_LONG_PRESS}

  /// <summary>Set for AKEY_EVENT_ACTION_UP when this event's key code is still
  /// being tracked from its initial down.  That is, somebody requested that
  /// tracking started on the key down and a long press has not caused
  /// the tracking to be canceled.</summary>
  AKEY_EVENT_FLAG_TRACKING = $200;
  {$EXTERNALSYM AKEY_EVENT_FLAG_TRACKING}

  /// <summary>Set when a key event has been synthesized to implement default
  /// behavior for an event that the application did not handle. Fallback key
  /// events are generated by unhandled trackball motions (to emulate a
  /// directional keypad) and by certain unhandled key presses that are declared
  /// in the key map (such as special function numeric keypad keys when numlock
  /// is off).</summary>
  AKEY_EVENT_FLAG_FALLBACK = $400;
  {$EXTERNALSYM AKEY_EVENT_FLAG_FALLBACK}

const
  /// <summary>Bit shift for the action bits holding the pointer index as
  /// defined by AMOTION_EVENT_ACTION_PointerIndex_MASK.</summary>
  AMOTION_EVENT_ACTION_PointerIndex_SHIFT = 8;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_PointerIndex_SHIFT}

{ Motion event actions. }
const
  /// <summary>Bit mask of the parts of the action code that are the action
  /// itself.</summary>
  AMOTION_EVENT_ACTION_MASK = $ff;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_MASK}

  /// <summary>Bits in the action code that represent a pointer index, used with
  /// AMOTION_EVENT_ACTION_POINTER_DOWN and AMOTION_EVENT_ACTION_POINTER_UP.
  /// Shifting down by AMOTION_EVENT_ACTION_PointerIndex_SHIFT provides the
  /// actual pointer index where the data for the pointer going up or down can
  /// be found.</summary>
  AMOTION_EVENT_ACTION_PointerIndex_MASK = $ff00;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_PointerIndex_MASK}

  /// <summary>A pressed gesture has started, the motion contains the initial
  /// starting location.</summary>
  AMOTION_EVENT_ACTION_DOWN = 0;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_DOWN}

  /// <summary>A pressed gesture has finished, the motion contains the final
  /// release location as well as any intermediate points since the last down or
  /// move event.</summary>
  AMOTION_EVENT_ACTION_UP = 1;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_UP}

  /// <summary>A change has happened during a press gesture (between
  /// AMOTION_EVENT_ACTION_DOWN and AMOTION_EVENT_ACTION_UP).  The motion
  /// contains the most recent point, as well as any intermediate points since
  /// the last down or move event.</summary>
  AMOTION_EVENT_ACTION_MOVE = 2;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_MOVE}

  /// <summary>The current gesture has been aborted. You will not receive any
  /// more points in it.  You should treat this as an up event, but not perform
  // /any action that you normally would.</summary>
  AMOTION_EVENT_ACTION_CANCEL = 3;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_CANCEL}

  /// <summary>A movement has happened outside of the normal bounds of the UI
  /// element. This does not provide a full gesture, but only the initial
  /// location of the movement/touch.</summary>
  AMOTION_EVENT_ACTION_OUTSIDE = 4;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_OUTSIDE}

  /// <summary>A non-primary pointer has gone down. The bits in
  /// AMOTION_EVENT_ACTION_PointerIndex_MASK indicate which pointer changed.</summary>
  AMOTION_EVENT_ACTION_POINTER_DOWN = 5;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_POINTER_DOWN}

  /// <summary>A non-primary pointer has gone up. The bits in
  /// AMOTION_EVENT_ACTION_PointerIndex_MASK indicate which pointer changed.</summary>
  AMOTION_EVENT_ACTION_POINTER_UP = 6;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_POINTER_UP}

  /// <summary>A change happened but the pointer is not down (unlike
  /// AMOTION_EVENT_ACTION_MOVE). The motion contains the most recent point, as
  /// well as any intermediate points since the last hover move event.</summary>
  AMOTION_EVENT_ACTION_HOVER_MOVE = 7;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_HOVER_MOVE}

  { The motion event contains relative vertical and/or horizontal scroll offsets.
    Use getAxisValue to retrieve the information from AMOTION_EVENT_AXIS_VSCROLL
    and AMOTION_EVENT_AXIS_HSCROLL.
    The pointer may or may not be down when this event is dispatched.
    This action is always delivered to the winder under the pointer, which
    may not be the window currently touched. }
  AMOTION_EVENT_ACTION_SCROLL = 8;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_SCROLL}

  /// <summary>The pointer is not down but has entered the boundaries of a
  /// window or view.</summary>
  AMOTION_EVENT_ACTION_HOVER_ENTER = 9;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_HOVER_ENTER}

  { Th/// <summary>pointer is not down but has exited the boundaries of a window
  /// or view.</summary>
  AMOTION_EVENT_ACTION_HOVER_EXIT = 10;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_HOVER_EXIT}

  /// <summary>One or more buttons have been pressed.</summary>
  AMOTION_EVENT_ACTION_BUTTON_PRESS = 11;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_BUTTON_PRESS}

  /// <summary>One or more buttons have been released.</summary>
  AMOTION_EVENT_ACTION_BUTTON_RELEASE = 12;
  {$EXTERNALSYM AMOTION_EVENT_ACTION_BUTTON_RELEASE}

{ Motion event flags. }
const
  /// <summary>This flag indicates that the window that received this motion
  /// event is partly or wholly obscured by another visible window above it.
  /// This flag is set to true even if the event did not directly pass through
  /// the obscured area. A security sensitive application can check this flag
  /// to identify situations in which a malicious application may have covered
  /// up part of its content for the purpose of misleading the user or hijacking
  /// touches.  An appropriate response might be to drop the suspect touches or
  /// to take additional precautions to confirm the user's actual intent.</summary>
  AMOTION_EVENT_FLAG_WINDOW_IS_OBSCURED = $1;
  {$EXTERNALSYM AMOTION_EVENT_FLAG_WINDOW_IS_OBSCURED}

{ Motion event edge touch flags. }
const
  /// <summary>No edges intersected</summary>
  AMOTION_EVENT_EDGE_FLAG_NONE = 0;
  {$EXTERNALSYM AMOTION_EVENT_EDGE_FLAG_NONE}

  /// <summary>Flag indicating the motion event intersected the top edge of the
  /// screen.</summary>
  AMOTION_EVENT_EDGE_FLAG_TOP = $01;
  {$EXTERNALSYM AMOTION_EVENT_EDGE_FLAG_TOP}

  /// <summary>Flag indicating the motion event intersected the bottom edge of
  /// the screen.</summary>
  AMOTION_EVENT_EDGE_FLAG_BOTTOM = $02;
  {$EXTERNALSYM AMOTION_EVENT_EDGE_FLAG_BOTTOM}

  /// <summary>Flag indicating the motion event intersected the left edge of the
  /// screen.</summary>
  AMOTION_EVENT_EDGE_FLAG_LEFT = $04;
  {$EXTERNALSYM AMOTION_EVENT_EDGE_FLAG_LEFT}

  /// <summary>Flag indicating the motion event intersected the right edge of
  /// the screen.</summary>
  AMOTION_EVENT_EDGE_FLAG_RIGHT = $08;
  {$EXTERNALSYM AMOTION_EVENT_EDGE_FLAG_RIGHT}

{ Constants that identify each individual axis of a motion event. }
const
  /// <summary>Axis constant: X axis of a motion event.<br />
  /// - For a touch screen, reports the absolute X screen position of the center
  ///   of the touch contact area.  The units are display pixels.<br />
  /// - For a touch pad, reports the absolute X surface position of the center
  ///   of the touch contact area. The units are device-dependent.<br />
  /// - For a mouse, reports the absolute X screen position of the mouse pointer.
  ///   The units are display pixels.<br />
  /// - For a trackball, reports the relative horizontal displacement of the
  ///   trackball. The value is normalized to a range from -1.0 (left) to 1.0
  //    (right).<br />
  /// - For a joystick, reports the absolute X position of the joystick. The
  ///   value is normalized to a range from -1.0 (left) to 1.0 (right).</summary>
  AMOTION_EVENT_AXIS_X = 0;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_X}

  /// <summary>Axis constant: Y axis of a motion event.<br />
  /// - For a touch screen, reports the absolute Y screen position of the center
  ///   of the touch contact area.  The units are display pixels.<br />
  /// - For a touch pad, reports the absolute Y surface position of the center
  ///   of the touch contact area. The units are device-dependent.<br />
  /// - For a mouse, reports the absolute Y screen position of the mouse
  ///   pointer. The units are display pixels.<br />
  /// - For a trackball, reports the relative vertical displacement of the
  ///   trackball. The value is normalized to a range from -1.0 (up) to 1.0
  ///   (down).<br />
  /// - For a joystick, reports the absolute Y position of the joystick. The
  ///   value is normalized to a range from -1.0 (up or far) to 1.0 (down or
  ///   near).</summary>
  AMOTION_EVENT_AXIS_Y = 1;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_Y}

  /// <summary>Axis constant: Pressure axis of a motion event.<br />
  /// - For a touch screen or touch pad, reports the approximate pressure
  ///   applied to the surface by a finger or other tool.  The value is
  ///   normalized to a range from 0 (no pressure at all) to 1 (normal
  ///   pressure), although values higher than 1 may be generated depending on
  ///   the calibration of the input device.<br />
  /// - For a trackball, the value is set to 1 if the trackball button is
  ///   pressed or 0 otherwise.<br />
  /// - For a mouse, the value is set to 1 if the primary mouse button is
  ///   pressed or 0 otherwise.</summary>
  AMOTION_EVENT_AXIS_PRESSURE = 2;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_PRESSURE}

  /// <summary>Axis constant: Size axis of a motion event.<br />
  /// - For a touch screen or touch pad, reports the approximate size of the
  ///   contact area in relation to the maximum detectable size for the device.
  ///   The value is normalized to a range from 0 (smallest detectable size) to
  ///   1 (largest detectable size), although it is not a linear scale. This
  ///   value is of limited use. To obtain calibrated size information, see
  ///   AMOTION_EVENT_AXIS_TOUCH_MAJOR or AMOTION_EVENT_AXIS_TOOL_MAJOR.
  /// </summary>
  AMOTION_EVENT_AXIS_SIZE = 3;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_SIZE}

  /// <summary>Axis constant: TouchMajor axis of a motion event.<br />
  /// - For a touch screen, reports the length of the major axis of an ellipse
  ///   that represents the touch area at the point of contact. The units are
  ///   display pixels.<br />
  /// - For a touch pad, reports the length of the major axis of an ellipse that
  ///   represents the touch area at the point of contact. The units are
  ///   device-dependent.</summary>
  AMOTION_EVENT_AXIS_TOUCH_MAJOR = 4;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_TOUCH_MAJOR}

  /// <summary>Axis constant: TouchMinor axis of a motion event.<br />
  /// - For a touch screen, reports the length of the minor axis of an ellipse
  ///   that represents the touch area at the point of contact. The units are
  ///   display pixels.<br />
  /// - For a touch pad, reports the length of the minor axis of an ellipse that
  ///   represents the touch area at the point of contact. The units are
  ///   device-dependent.<br />
  /// When the touch is circular, the major and minor axis lengths will be equal
  /// to one another.</summary>
  AMOTION_EVENT_AXIS_TOUCH_MINOR = 5;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_TOUCH_MINOR}

  /// <summary>Axis constant: ToolMajor axis of a motion event.<br />
  /// - For a touch screen, reports the length of the major axis of an ellipse
  ///   that represents the size of the approaching finger or tool used to make
  ///   contact.<br />
  /// - For a touch pad, reports the length of the major axis of an ellipse that
  ///   represents the size of the approaching finger or tool used to make
  ///   contact. The units are device-dependent.<br />
  /// When the touch is circular, the major and minor axis lengths will be equal
  /// to one another.<br />
  /// The tool size may be larger than the touch size since the tool may not be
  /// fully in contact with the touch sensor.</summary>
  AMOTION_EVENT_AXIS_TOOL_MAJOR = 6;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_TOOL_MAJOR}

  /// <summary>Axis constant: ToolMinor axis of a motion event.<br />
  /// - For a touch screen, reports the length of the minor axis of an ellipse
  ///   that represents the size of the approaching finger or tool used to make
  ///   contact.<br />
  /// - For a touch pad, reports the length of the minor axis of an ellipse that
  ///   represents the size of the approaching finger or tool used to make
  ///   contact. The units are device-dependent.<br />
  /// When the touch is circular, the major and minor axis lengths will be
  /// equal to one another.<br />
  /// The tool size may be larger than the touch size since the tool may not be
  /// fully in contact with the touch sensor.</summary>
  AMOTION_EVENT_AXIS_TOOL_MINOR = 7;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_TOOL_MINOR}

  /// <summary>Axis constant: Orientation axis of a motion event.<br />
  /// - For a touch screen or touch pad, reports the orientation of the finger
  ///   or tool in radians relative to the vertical plane of the device. An
  ///   angle of 0 radians indicates that the major axis of contact is oriented
  ///   upwards, is perfectly circular or is of unknown orientation. A positive
  ///   angle indicates that the major axis of contact is oriented to the right.
  ///   A negative angle indicates that the major axis of contact is oriented to
  ///   the left. The full range is from -PI/2 radians (finger pointing fully
  ///   left) to PI/2 radians (finger pointing fully right).<br />
  /// - For a stylus, the orientation indicates the direction in which the
  ///   stylus is pointing in relation to the vertical axis of the current
  ///   orientation of the screen. The range is from -PI radians to PI radians,
  ///   where 0 is pointing up, -PI/2 radians is pointing left, -PI or PI
  ///   radians is pointing down, and PI/2 radians is pointing right. See also
  ///   AMOTION_EVENT_AXIS_TILT.</summary>
  AMOTION_EVENT_AXIS_ORIENTATION = 8;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_ORIENTATION}

  /// <summary>Axis constant: Vertical Scroll axis of a motion event.<br />
  /// - For a mouse, reports the relative movement of the vertical scroll wheel.
  ///   The value is normalized to a range from -1.0 (down) to 1.0 (up).<br />
  /// This axis should be used to scroll views vertically.</summary>
  AMOTION_EVENT_AXIS_VSCROLL = 9;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_VSCROLL}

  /// <summary>Axis constant: Horizontal Scroll axis of a motion event.<br />
  /// - For a mouse, reports the relative movement of the horizontal scroll
  ///   wheel. The value is normalized to a range from -1.0 (left) to
  ///   1.0 (right).<br />
  /// This axis should be used to scroll views horizontally.</summary>
  AMOTION_EVENT_AXIS_HSCROLL = 10;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_HSCROLL}

  /// <summary>Axis constant: Z axis of a motion event.<br />
  /// - For a joystick, reports the absolute Z position of the joystick. The
  ///   value is normalized to a range from -1.0 (high) to 1.0 (low). <em>On
  ///   game pads with two analog joysticks, this axis is often reinterpreted to
  ///   report the absolute X position of the second joystick instead.</em>
  /// </summary>
  AMOTION_EVENT_AXIS_Z = 11;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_Z}

  /// <summary>Axis constant: X Rotation axis of a motion event.<br />
  /// - For a joystick, reports the absolute rotation angle about the X axis.
  ///   The value is normalized to a range from -1.0 (counter-clockwise) to 1.0
  ///   (clockwise).</summary>
  AMOTION_EVENT_AXIS_RX = 12;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_RX}

  /// <summary>Axis constant: Y Rotation axis of a motion event.<br />
  /// - For a joystick, reports the absolute rotation angle about the Y axis.
  ///   The value is normalized to a range from -1.0 (counter-clockwise) to 1.0
  ///   (clockwise).</summary>
  AMOTION_EVENT_AXIS_RY = 13;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_RY}

  /// <summary>Axis constant: Z Rotation axis of a motion event.<br />
  /// - For a joystick, reports the absolute rotation angle about the Z axis.
  ///   The value is normalized to a range from -1.0 (counter-clockwise) to 1.0
  ///   (clockwise). On game pads with two analog joysticks, this axis is often
  ///   reinterpreted to report the absolute Y position of the second joystick
  ///   instead.</summary>
  AMOTION_EVENT_AXIS_RZ = 14;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_RZ}

  /// <summary>Axis constant: Hat X axis of a motion event.<br />
  /// - For a joystick, reports the absolute X position of the directional hat
  ///   control. The value is normalized to a range from -1.0 (left) to 1.0
  ///   (right).</summary>
  AMOTION_EVENT_AXIS_HAT_X = 15;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_HAT_X}

  /// <summary>Axis constant: Hat Y axis of a motion event.<br />
  /// - For a joystick, reports the absolute Y position of the directional hat
  ///   control. The value is normalized to a range from -1.0 (up) to 1.0
  ///   (down).</summary>
  AMOTION_EVENT_AXIS_HAT_Y = 16;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_HAT_Y}

  /// <summary>Axis constant: Left Trigger axis of a motion event.<br />
  /// - For a joystick, reports the absolute position of the left trigger
  ///   control. The value is normalized to a range from 0.0 (released) to 1.0
  ///   (fully pressed).</summary>
  AMOTION_EVENT_AXIS_LTRIGGER = 17;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_LTRIGGER}

  /// <summary>Axis constant: Right Trigger axis of a motion event.<br />
  /// - For a joystick, reports the absolute position of the right trigger
  ///   control. The value is normalized to a range from 0.0 (released) to 1.0
  ///   (fully pressed).</summary>
  AMOTION_EVENT_AXIS_RTRIGGER = 18;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_RTRIGGER}

  /// <summary>Axis constant: Throttle axis of a motion event.<br />
  /// - For a joystick,reports the absolute position of the throttle control.
  ///   The value is normalized to a range from 0.0 (fully open) to 1.0 (fully
  ///   closed).</summary>
  AMOTION_EVENT_AXIS_THROTTLE = 19;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_THROTTLE}

  /// <summary>Axis constant: Rudder axis of a motion event.<br />
  /// - For a joystick, reports the absolute position of the rudder control. The
  ///   value is normalized to a range from -1.0 (turn left) to 1.0 (turn
  ///   right).</summary>
  AMOTION_EVENT_AXIS_RUDDER = 20;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_RUDDER}

  /// <summary>Axis constant: Wheel axis of a motion event.<br />
  /// - For a joystick, reports the absolute position of the steering wheel
  ///   control. The value is normalized to a range from -1.0 (turn left) to 1.0
  ///   (turn right).</summary>
  AMOTION_EVENT_AXIS_WHEEL = 21;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_WHEEL}

  /// <summary>Axis constant: Gas axis of a motion event.<br />
  /// - For a joystick, reports the absolute position of the gas (accelerator)
  ///   control. The value is normalized to a range from 0.0 (no acceleration)
  ///   to 1.0 (maximum acceleration).</summary>
  AMOTION_EVENT_AXIS_GAS = 22;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GAS}

  /// <summary>Axis constant: Brake axis of a motion event.<br />
  /// - For a joystick, reports the absolute position of the brake control. The
  ///   value is normalized to a range from 0.0 (no braking) to 1.0 (maximum
  ///   braking).</summary>
  AMOTION_EVENT_AXIS_BRAKE = 23;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_BRAKE}

  /// <summary>Axis constant: Distance axis of a motion event.<br />
  /// - For a stylus, reports the distance of the stylus from the screen. A
  ///   value of 0.0 indicates direct contact and larger values indicate
  ///   increasing distance from the surface.</summary>
  AMOTION_EVENT_AXIS_DISTANCE = 24;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_DISTANCE}

  /// <summary>Axis constant: Tilt axis of a motion event.<br />
  /// - For a stylus, reports the tilt angle of the stylus in radians where 0
  ///   radians indicates that the stylus is being held perpendicular to the
  ///   surface, and PI/2 radians indicates that the stylus is being held flat
  ///   against the surface.</summary>
  AMOTION_EVENT_AXIS_TILT = 25;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_TILT}

  /// <summary>Axis constant: Generic scroll axis of a motion event.<br />
  /// - This is used for scroll axis motion events that can't be classified as
  ///   strictly vertical or horizontal. The movement of a rotating scroller is
  ///   an example of this.</summary>
  AMOTION_EVENT_AXIS_SCROLL = 26;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_SCROLL}

  /// <summary>Axis constant: The movement of x position of a motion event.<br />
  /// - For a mouse, reports a difference of x position between the previous
  ///   position. This is useful when pointer is captured, in that case the
  ///   mouse pointer doesn't change the location but this axis reports the
  ///   difference which allows the app to see how the mouse is moved.</summary>
  AMOTION_EVENT_AXIS_RELATIVE_X = 27;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_RELATIVE_X}

  /// <summary>Axis constant: The movement of y position of a motion event.<br />
  /// Same as RELATIVE_X, but for y position.</summary>
  AMOTION_EVENT_AXIS_RELATIVE_Y = 28;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_RELATIVE_Y}

  /// <summary>Axis constant: Generic 1 axis of a motion event.<br />
  /// The interpretation of a generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_1 = 32;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_1}

  /// <summary>Axis constant: Generic 2 axis of a motion event.<br />
  /// The interpretation of a generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_2 = 33;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_2}

  /// <summary>Axis constant: Generic 3 axis of a motion event.<br />
  /// The interpretation of a generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_3 = 34;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_3}

  /// <summary>Axis constant: Generic 4 axis of a motion event.<br />
  /// The interpretation of a generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_4 = 35;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_4}

  /// <summary>Axis constant: Generic 5 axis of a motion event.<br />
  /// The interpretation of a generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_5 = 36;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_5}

  /// <summary>Axis constant: Generic 6 axis of a motion event.<br />
  /// The interpretation of a generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_6 = 37;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_6}

  /// <summary>Axis constant: Generic 7 axis of a motion event.<br />
  /// The interpretation of a generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_7 = 38;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_7}

  /// <summary>Axis constant: Generic 8 axis of a motion event.<br />
  /// The interpretation of a generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_8 = 39;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_8}

  /// <summary>Axis constant: Generic 9 axis of a motion event.<br />
  /// The interpretation of a generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_9 = 40;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_9}

  /// <summary>Axis constant: Generic 10 axis of a motion event.<br />
  /// The interpretation of a generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_10 = 41;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_10}

  /// <summary>Axis constant: Generic 11 axis of a motion event.<br />
  /// The interpretation of a generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_11 = 42;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_11}

  /// <summary>Axis constant: Generic 12 axis of a motion event. The
  /// interpretation ofa generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_12 = 43;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_12}

  /// <summary>Axis constant: Generic 13 axis of a motion event. The
  /// interpretation ofa generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_13 = 44;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_13}

  /// <summary>Axis constant: Generic 14 axis of a motion event. The
  /// interpretation ofa generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_14 = 45;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_14}

  /// <summary>Axis constant: Generic 15 axis of a motion event. The
  /// interpretation of a generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_15 = 46;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_15}

  /// <summary>Axis constant: Generic 16 axis of a motion event. The
  /// interpretation of a generic axis is device-specific.</summary>
  AMOTION_EVENT_AXIS_GENERIC_16 = 47;
  {$EXTERNALSYM AMOTION_EVENT_AXIS_GENERIC_16}

{ Constants that identify buttons that are associated with motion events.
  Refer to the documentation on the MotionEvent class for descriptions of each button. }
const
  AMOTION_EVENT_BUTTON_PRIMARY = 1 shl 0;
  {$EXTERNALSYM AMOTION_EVENT_BUTTON_PRIMARY}

  AMOTION_EVENT_BUTTON_SECONDARY = 1 shl 1;
  {$EXTERNALSYM AMOTION_EVENT_BUTTON_SECONDARY}

  AMOTION_EVENT_BUTTON_TERTIARY = 1 shl 2;
  {$EXTERNALSYM AMOTION_EVENT_BUTTON_TERTIARY}

  AMOTION_EVENT_BUTTON_BACK = 1 shl 3;
  {$EXTERNALSYM AMOTION_EVENT_BUTTON_BACK}

  AMOTION_EVENT_BUTTON_FORWARD = 1 shl 4;
  {$EXTERNALSYM AMOTION_EVENT_BUTTON_FORWARD}

  AMOTION_EVENT_BUTTON_STYLUS_PRIMARY = 1 shl 5;
  {$EXTERNALSYM AMOTION_EVENT_BUTTON_STYLUS_PRIMARY}

  AMOTION_EVENT_BUTTON_STYLUS_SECONDARY = 1 shl 6;
  {$EXTERNALSYM AMOTION_EVENT_BUTTON_STYLUS_SECONDARY}

{ Constants that identify tool types.
  Refer to the documentation on the MotionEvent class for descriptions of each tool type. }
const
  AMOTION_EVENT_TOOL_TYPE_UNKNOWN = 0;
  {$EXTERNALSYM AMOTION_EVENT_TOOL_TYPE_UNKNOWN}

  AMOTION_EVENT_TOOL_TYPE_FINGER = 1;
  {$EXTERNALSYM AMOTION_EVENT_TOOL_TYPE_FINGER}

  AMOTION_EVENT_TOOL_TYPE_STYLUS = 2;
  {$EXTERNALSYM AMOTION_EVENT_TOOL_TYPE_STYLUS}

  AMOTION_EVENT_TOOL_TYPE_MOUSE = 3;
  {$EXTERNALSYM AMOTION_EVENT_TOOL_TYPE_MOUSE}

  AMOTION_EVENT_TOOL_TYPE_ERASER = 4;
  {$EXTERNALSYM AMOTION_EVENT_TOOL_TYPE_ERASER}

{ Input source masks.

  Refer to the documentation on Androidapi.view.InputDevice for more details about input sources
  and their correct interpretation. }
const
  AINPUT_SOURCE_CLASS_MASK = $000000ff;
  {$EXTERNALSYM AINPUT_SOURCE_CLASS_MASK}

  AINPUT_SOURCE_CLASS_NONE = $00000000;
  {$EXTERNALSYM AINPUT_SOURCE_CLASS_NONE}

  AINPUT_SOURCE_CLASS_BUTTON = $00000001;
  {$EXTERNALSYM AINPUT_SOURCE_CLASS_BUTTON}

  AINPUT_SOURCE_CLASS_POINTER = $00000002;
  {$EXTERNALSYM AINPUT_SOURCE_CLASS_POINTER}

  AINPUT_SOURCE_CLASS_NAVIGATION = $00000004;
  {$EXTERNALSYM AINPUT_SOURCE_CLASS_NAVIGATION}

  AINPUT_SOURCE_CLASS_POSITION = $00000008;
  {$EXTERNALSYM AINPUT_SOURCE_CLASS_POSITION}

  AINPUT_SOURCE_CLASS_JOYSTICK = $00000010;
  {$EXTERNALSYM AINPUT_SOURCE_CLASS_JOYSTICK}

{ Input sources }
const
  AINPUT_SOURCE_UNKNOWN = $00000000;
  {$EXTERNALSYM AINPUT_SOURCE_UNKNOWN}

  AINPUT_SOURCE_KEYBOARD = $00000100 or AINPUT_SOURCE_CLASS_BUTTON;
  {$EXTERNALSYM AINPUT_SOURCE_KEYBOARD}

  AINPUT_SOURCE_DPAD = $00000200 or AINPUT_SOURCE_CLASS_BUTTON;
  {$EXTERNALSYM AINPUT_SOURCE_DPAD}

  AINPUT_SOURCE_GAMEPAD = $00000400 or AINPUT_SOURCE_CLASS_BUTTON;
  {$EXTERNALSYM AINPUT_SOURCE_GAMEPAD}

  AINPUT_SOURCE_TOUCHSCREEN = $00001000 or AINPUT_SOURCE_CLASS_POINTER;
  {$EXTERNALSYM AINPUT_SOURCE_TOUCHSCREEN}

  AINPUT_SOURCE_MOUSE = $00002000 or AINPUT_SOURCE_CLASS_POINTER;
  {$EXTERNALSYM AINPUT_SOURCE_MOUSE}

  AINPUT_SOURCE_STYLUS = $00004000 or AINPUT_SOURCE_CLASS_POINTER;
  {$EXTERNALSYM AINPUT_SOURCE_STYLUS}

  AINPUT_SOURCE_BLUETOOTH_STYLUS = $00008000 or AINPUT_SOURCE_STYLUS;
  {$EXTERNALSYM AINPUT_SOURCE_BLUETOOTH_STYLUS}

  AINPUT_SOURCE_TRACKBALL = $00010000 or AINPUT_SOURCE_CLASS_NAVIGATION;
  {$EXTERNALSYM AINPUT_SOURCE_TRACKBALL}

  AINPUT_SOURCE_MOUSE_RELATIVE = $00020000 or AINPUT_SOURCE_CLASS_NAVIGATION;
  {$EXTERNALSYM AINPUT_SOURCE_MOUSE}

  AINPUT_SOURCE_TOUCHPAD = $00100000 or AINPUT_SOURCE_CLASS_POSITION;
  {$EXTERNALSYM AINPUT_SOURCE_TOUCHPAD}

  AINPUT_SOURCE_TOUCH_NAVIGATION = $00200000 or AINPUT_SOURCE_CLASS_NONE;
  {$EXTERNALSYM AINPUT_SOURCE_TOUCH_NAVIGATION}

  AINPUT_SOURCE_JOYSTICK = $01000000 or AINPUT_SOURCE_CLASS_JOYSTICK;
  {$EXTERNALSYM AINPUT_SOURCE_JOYSTICK}

  AINPUT_SOURCE_ROTARY_ENCODER = $00400000 or AINPUT_SOURCE_CLASS_NONE;
  {$EXTERNALSYM AINPUT_SOURCE_TOUCHPAD}

  AINPUT_SOURCE_ANY = $ffffff00;
  {$EXTERNALSYM AINPUT_SOURCE_ANY}

{ Keyboard types.
  
  Refer to the documentation on Androidapi.view.InputDevice for more details. }
const
  AINPUT_KEYBOARD_TYPE_NONE = 0;
  {$EXTERNALSYM AINPUT_KEYBOARD_TYPE_NONE}

  AINPUT_KEYBOARD_TYPE_NON_ALPHABETIC = 1;
  {$EXTERNALSYM AINPUT_KEYBOARD_TYPE_NON_ALPHABETIC}

  AINPUT_KEYBOARD_TYPE_ALPHABETIC = 2;
  {$EXTERNALSYM AINPUT_KEYBOARD_TYPE_ALPHABETIC}

{ Constants used to retrieve information about the range of motion for a particular
  coordinate of a motion event.
  
  Refer to the documentation on Androidapi.view.InputDevice for more details about input sources
  and their correct interpretation.

  These constants are deprecated. Use AMOTION_EVENT_AXIS AMOTION_EVENT_AXIS_* constants instead.}

const
  AINPUT_MOTION_RANGE_X = AMOTION_EVENT_AXIS_X deprecated 'Use AMOTION_EVENT_AXIS_X';
  {$EXTERNALSYM AINPUT_MOTION_RANGE_X}

  AINPUT_MOTION_RANGE_Y = AMOTION_EVENT_AXIS_Y deprecated 'Use AMOTION_EVENT_AXIS_Y';
  {$EXTERNALSYM AINPUT_MOTION_RANGE_Y}

  AINPUT_MOTION_RANGE_PRESSURE = AMOTION_EVENT_AXIS_PRESSURE deprecated 'Use AMOTION_EVENT_AXIS_Y';
  {$EXTERNALSYM AINPUT_MOTION_RANGE_PRESSURE}

  AINPUT_MOTION_RANGE_SIZE = AMOTION_EVENT_AXIS_SIZE deprecated 'Use AMOTION_EVENT_AXIS_SIZE';
  {$EXTERNALSYM AINPUT_MOTION_RANGE_SIZE}

  AINPUT_MOTION_RANGE_TOUCH_MAJOR = AMOTION_EVENT_AXIS_TOUCH_MAJOR deprecated 'Use AMOTION_EVENT_AXIS_TOUCH_MAJOR';
  {$EXTERNALSYM AINPUT_MOTION_RANGE_TOUCH_MAJOR}

  AINPUT_MOTION_RANGE_TOUCH_MINOR = AMOTION_EVENT_AXIS_TOUCH_MINOR deprecated 'Use AMOTION_EVENT_AXIS_TOUCH_MINOR';
  {$EXTERNALSYM AINPUT_MOTION_RANGE_TOUCH_MINOR}

  AINPUT_MOTION_RANGE_TOOL_MAJOR = AMOTION_EVENT_AXIS_TOOL_MAJOR deprecated 'Use AMOTION_EVENT_AXIS_TOOL_MAJOR';
  {$EXTERNALSYM AINPUT_MOTION_RANGE_TOOL_MAJOR}

  AINPUT_MOTION_RANGE_TOOL_MINOR = AMOTION_EVENT_AXIS_TOOL_MINOR deprecated 'Use AMOTION_EVENT_AXIS_TOOL_MINOR';
  {$EXTERNALSYM AINPUT_MOTION_RANGE_TOOL_MINOR}

  AINPUT_MOTION_RANGE_ORIENTATION = AMOTION_EVENT_AXIS_ORIENTATION deprecated 'AMOTION_EVENT_AXIS_ORIENTATION';
  {$EXTERNALSYM AINPUT_MOTION_RANGE_ORIENTATION}

type
  /// <summary>An input queue is the facility through which you retrieve input
  /// events.</summary>
  AInputQueue = record end;
  {$EXTERNALSYM AInputQueue}

  PAInputQueue = ^AInputQueue;

  /// <summary>Input events are opaque structures.<br />
  /// Use the provided accessors functions to read their properties.</summary>
  AInputEvent = record end;
  {$EXTERNALSYM AInputEvent}

  PAInputEvent = ^AInputEvent;
  PPAInputEvent = ^PAInputEvent;


{ Input event accessors.

  Note that most functions can only be used on input events that are of a given type.
  Calling these functions on input events of other types will yield undefined behavior. }

{ Accessors for all input events. }

/// <summary>Get the input event type.</summary>
function AInputEvent_getType(Event: PAinputEvent): Int32; cdecl;
  external AndroidLib name 'AInputEvent_getType';
{$EXTERNALSYM AInputEvent_getType}

/// <summary>Get the id for the device that an input event came from.<br />
/// Input events can be generated by multiple different input devices. Use the
/// input device id to obtain information about the input device that was
/// responsible for generating a particular event.<br />
/// An input device id of 0 indicates that the event didn't come from a physical
/// device; other numbers are arbitrary and you shouldn't depend on the values.
/// Use the provided input device query API to obtain information about input
/// devices.</summary>
function AInputEvent_getDeviceId(Event: PAinputEvent): Int32; cdecl;
  external AndroidLib name 'AInputEvent_getDeviceId';
{$EXTERNALSYM AInputEvent_getDeviceId}

/// <summary>Get the input event source.</summary>
function AInputEvent_getSource(Event: PAinputEvent): Int32; cdecl;
  external AndroidLib name 'AInputEvent_getSource';
{$EXTERNALSYM AInputEvent_getSource}

{ Accessors for key events only. }

/// <summary>Get the key event action.</summary>
function AKeyEvent_getAction(KeyEvent: PAInputEvent): Int32; cdecl;
  external AndroidLib name 'AKeyEvent_getAction';
{$EXTERNALSYM AKeyEvent_getAction}

/// <summary>Get the key event flags.</summary>
function AKeyEvent_getFlags(KeyEvent: PAInputEvent): Int32; cdecl;
  external AndroidLib name 'AKeyEvent_getFlags';
{$EXTERNALSYM AKeyEvent_getFlags}

/// <summary>Get the key code of the key event. This is the physical key that
/// was pressed, not the Unicode character.</summary>
function AKeyEvent_getKeyCode(KeyEvent: PAInputEvent): Int32; cdecl;
  external AndroidLib name 'AKeyEvent_getKeyCode';
{$EXTERNALSYM AKeyEvent_getKeyCode}

/// <summary>Get the hardware key id of this key event. These values are not
/// reliable and vary from device to device.</summary>
function AKeyEvent_getScanCode(KeyEvent: PAInputEvent): Int32; cdecl;
  external AndroidLib name 'AKeyEvent_getScanCode';
{$EXTERNALSYM AKeyEvent_getScanCode}

/// <summary>Get the meta key state.</summary>
function AKeyEvent_getMetaState(KeyEvent: PAInputEvent): Int32; cdecl;
  external AndroidLib name 'AKeyEvent_getMetaState';
{$EXTERNALSYM AKeyEvent_getMetaState}

/// <summary>Get the repeat count of the event. For both key up an key down
/// events, this is the number of times the key has repeated with the first down
/// starting at 0 and counting up from there. For multiple key events, this
/// is the number of down/up pairs that have occurred.</summary>
function AKeyEvent_getRepeatCount(KeyEvent: PAInputEvent): Int32; cdecl;
  external AndroidLib name 'AKeyEvent_getRepeatCount';
{$EXTERNALSYM AKeyEvent_getRepeatCount}

/// <summary>Get the time of the most recent key down event, in the
/// java.lang.System.nanoTime() time base. If this is a down event, this will
/// be the same as eventTime. Note that when chording keys, this value is the
/// down time of the most recently pressed key, which may not be the same
/// physical key of this event.</summary>
function AKeyEvent_getDownTime(KeyEvent: PAInputEvent): Int64; cdecl;
  external AndroidLib name 'AKeyEvent_getDownTime';
{$EXTERNALSYM AKeyEvent_getDownTime}

/// <summary>Get the time this event occurred, in the
/// java.lang.System.nanoTime() time base.</summary>
function AKeyEvent_getEventTime(KeyEvent: PAInputEvent): Int64; cdecl;
  external AndroidLib name 'AKeyEvent_getEventTime';
{$EXTERNALSYM AKeyEvent_getEventTime}

{ Accessors for motion events only. }

/// <summary>Get the combined motion event action code and pointer index.</summary>
function AMotionEvent_getAction(MotionEvent: PAInputEvent): Int32; cdecl;
  external AndroidLib name 'AMotionEvent_getAction';
{$EXTERNALSYM AMotionEvent_getAction}

/// <summary>Get the motion event flags.</summary>
function AMotionEvent_getFlags(MotionEvent: PAInputEvent): Int32; cdecl;
  external AndroidLib name 'AMotionEvent_getFlags';
{$EXTERNALSYM AMotionEvent_getFlags}

/// <summary>Get the state of any meta / modifier keys that were in effect when
/// the event was generated.</summary>
function AMotionEvent_getMetaState(MotionEvent: PAInputEvent): Int32; cdecl;
  external AndroidLib name 'AMotionEvent_getMetaState';
{$EXTERNALSYM AMotionEvent_getMetaState}

/// <remarks>Introduced in API 14.</remarks>
/// <summary>Get the button state of all buttons that are pressed.</summary>
function AMotionEvent_getButtonState(MotionEvent: PAInputEvent): Int32; cdecl;
  external AndroidLib name 'AMotionEvent_getButtonState';
{$EXTERNALSYM AMotionEvent_getButtonState}

/// <summary>Get a bitfield indicating which edges, if any, were touched by this
/// motion event. For touch events, clients can use this to determine if the
/// user's finger was touching the edge of the display.</summary>
function AMotionEvent_getEdgeFlags(MotionEvent: PAInputEvent): Int32; cdecl;
  external AndroidLib name 'AMotionEvent_getEdgeFlags';
{$EXTERNALSYM AMotionEvent_getEdgeFlags}

/// <summary>Get the time when the user originally pressed down to start a
/// stream of position events, in the java.lang.System.nanoTime() time base.
/// </summary>
function AMotionEvent_getDownTime(MotionEvent: PAInputEvent): Int64; cdecl;
  external AndroidLib name 'AMotionEvent_getDownTime';
{$EXTERNALSYM AMotionEvent_getDownTime}

/// <summary>Get the time when this specific event was generated, in the
///   java.lang.System.nanoTime() time base.</summary>
function AMotionEvent_getEventTime(MotionEvent: PAInputEvent): Int64; cdecl;
  external AndroidLib name 'AMotionEvent_getEventTime';
{$EXTERNALSYM AMotionEvent_getEventTime}

/// <summary>Get the X coordinate offset. For touch events on the screen, this
/// is the delta that was added to the raw screen coordinates to adjust for the
/// absolute position of the containing windows and views.</summary>
function AMotionEvent_getXOffset(MotionEvent: PAInputEvent): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getXOffset';
{$EXTERNALSYM AMotionEvent_getXOffset}

/// <summary>Get the precision of the Y coordinates being reported. For touch
/// events on the screen, this is the delta that was added to the raw screen
/// coordinates to adjust for the absolute position of the containing windows
/// and views.</summary>
function AMotionEvent_getYOffset(MotionEvent: PAInputEvent): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getYOffset';
{$EXTERNALSYM AMotionEvent_getYOffset}

/// <summary>Get the precision of the X coordinates being reported. You can
/// multiply this number with an X coordinate sample to find the actual hardware
/// value of the X coordinate.</summary>
function AMotionEvent_getXPrecision(MotionEvent: PAInputEvent): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getXPrecision';
{$EXTERNALSYM AMotionEvent_getXPrecision}

/// <summary>Get the precision of the Y coordinates being reported. You can
/// multiply this number with an Y coordinate sample to find the actual hardware
/// value of the Y coordinate.</summary>
function AMotionEvent_getYPrecision(MotionEvent: PAInputEvent): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getYPrecision';
{$EXTERNALSYM AMotionEvent_getYPrecision}

/// <summary>Get the number of pointers of data contained in this event.
/// Always >= 1.</summary>
function AMotionEvent_getPointerCount(MotionEvent: PAInputEvent): size_t; cdecl;
  external AndroidLib name 'AMotionEvent_getPointerCount';
{$EXTERNALSYM AMotionEvent_getPointerCount}

/// <summary>Get the pointer identifier associated with a particular pointer
/// data index is this event. The identifier tells you the actual pointer number
/// associated with the data, accounting for individual pointers going up and
/// down since the start of the current gesture.</summary>
function AMotionEvent_getPointerId(MotionEvent: PAInputEvent; PointerIndex: size_t): Int32; cdecl;
  external AndroidLib name 'AMotionEvent_getPointerId';
{$EXTERNALSYM AMotionEvent_getPointerId}

/// <remarks>Introduced in API 14.</remarks>
/// <summary>Get the tool type of a pointer for the given pointer index. The
/// tool type indicates the type of tool used to make contact such as a finger
/// or stylus, if known.</summary>
function AMotionEvent_getToolType(MotionEvent: PAInputEvent; PointerIndex: size_t): Int32; cdecl;
  external AndroidLib name 'AMotionEvent_getToolType';
{$EXTERNALSYM AMotionEvent_getToolType}

/// <summary>Get the original raw X coordinate of this event. For touch events
/// on the screen, this is the original location of the event on the screen,
/// before it had been adjusted for the containing window and views.</summary>
function AMotionEvent_getRawX(MotionEvent: PAInputEvent; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getRawX';
{$EXTERNALSYM AMotionEvent_getRawX}

/// <summary>Get the original raw Y coordinate of this event. For touch events
/// on the screen, this is the original location of the event on the screen,
/// before it had been adjusted for the containing window and views.</summary>
function AMotionEvent_getRawY(MotionEvent: PAInputEvent; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getRawY';
{$EXTERNALSYM AMotionEvent_getRawY}

/// <summary>Get the current X coordinate of this event for the given pointer
/// index. Whole numbers are pixels; the value may have a fraction for input
/// devices that are sub-pixel precise.</summary>
function AMotionEvent_getX(MotionEvent: PAInputEvent; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getX';
{$EXTERNALSYM AMotionEvent_getX}

/// <summary>Get the current Y coordinate of this event for the given pointer
/// index. Whole numbers are pixels; the value may have a fraction for input
/// devices that are sub-pixel precise.</summary>
function AMotionEvent_getY(MotionEvent: PAInputEvent; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getY';
{$EXTERNALSYM AMotionEvent_getY}

/// <summary>Get the current pressure of this event for the given pointer index.
/// The pressure generally ranges from 0 (no pressure at all) to 1 (normal
/// pressure), however values higher than 1 may be generated depending on the
/// calibration of the input device.</summary>
function AMotionEvent_getPressure(MotionEvent: PAInputEvent; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getPressure';
{$EXTERNALSYM AMotionEvent_getPressure}

/// <summary>Get the current scaled value of the approximate size for the given
/// pointer index. This represents some approximation of the area of the
/// screen being pressed; the actual value in pixels corresponding to the
/// touch is normalized with the device specific range of values and scaled
/// to a value between 0 and 1. The value of size can be used to determine fat
/// touch events.</summary>
function AMotionEvent_getSize(MotionEvent: PAInputEvent; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getSize';
{$EXTERNALSYM AMotionEvent_getSize}

/// <summary>Get the current length of the major axis of an ellipse that
/// describes the touch area at the point of contact for the given pointer
/// index.</summary>
function AMotionEvent_getTouchMajor(MotionEvent: PAInputEvent; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getTouchMajor';
{$EXTERNALSYM AMotionEvent_getTouchMajor}

/// <summary>Get the current length of the minor axis of an ellipse that
/// describes the touch area at the point of contact for the given pointer
/// index.</summary>
function AMotionEvent_getTouchMinor(MotionEvent: PAInputEvent; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getTouchMinor';
{$EXTERNALSYM AMotionEvent_getTouchMinor}

/// <summary>Get the current length of the major axis of an ellipse that
/// describes the size of the approaching tool for the given pointer index. The
/// tool area represents the estimated size of the finger or pen that is
/// touching the device independent of its actual touch area at the point of
/// contact.</summary>
function AMotionEvent_getToolMajor(MotionEvent: PAInputEvent; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getToolMajor';
{$EXTERNALSYM AMotionEvent_getToolMajor}

/// <summary>Get the current length of the minor axis of an ellipse that
/// describes the size of the approaching tool for the given pointer index. The
/// tool area represents the estimated size of the finger or pen that is
/// touching the device independent of its actual touch area at the point of
/// contact.</summary>
function AMotionEvent_getToolMinor(MotionEvent: PAInputEvent; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getToolMinor';
{$EXTERNALSYM AMotionEvent_getToolMinor}

/// <summary>Get the current orientation of the touch area and tool area in
/// radians clockwise from vertical for the given pointer index. An angle of 0
/// degrees indicates that the major axis of contact is oriented upwards, is
/// perfectly circular or is of unknown orientation. A positive angle indicates
/// that the major axis of contact is oriented to the right. A negative angle
/// indicates that the major axis of contact is oriented to the left.
/// The full range is from -PI/2 radians (finger pointing fully left) to PI/2
/// radians (finger pointing fully right).</summary>
function AMotionEvent_getOrientation(MotionEvent: PAInputEvent; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getOrientation';
{$EXTERNALSYM AMotionEvent_getOrientation}

/// <remarks>Introduced in API 13.</remarks>
/// <summary>Get the value of the request axis for the given pointer index.</summary>
function AMotionEvent_getAxisValue(MotionEvent: PAInputEvent; Axis: Int32; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getAxisValue';
{$EXTERNALSYM AMotionEvent_getAxisValue}

/// <summary>Get the number of historical points in this event. These are
/// movements that have occurred between this event and the previous event. This
/// only applies to AMOTION_EVENT_ACTION_MOVE events -- all other actions will
/// have a size of 0. Historical samples are indexed from oldest to newest.
/// </summary>
function AMotionEvent_getHistorySize(MotionEvent: PAInputEvent): size_t; cdecl;
  external AndroidLib name 'AMotionEvent_getHistorySize';
{$EXTERNALSYM AMotionEvent_getHistorySize}

/// <summary>Get the time that a historical movement occurred between this event
/// and the previous event, in the java.lang.System.nanoTime() time base.</summary>
function AMotionEvent_getHistoricalEventTime(MotionEvent: PAInputEvent; HistoryIndex: size_t): Int64; cdecl;
  external AndroidLib name 'AMotionEvent_getHistoricalEventTime';
{$EXTERNALSYM AMotionEvent_getHistoricalEventTime}

/// <summary>Get the historical raw X coordinate of this event for the given
/// pointer index that occurred between this event and the previous motion
/// event. For touch events on the screen, this is the original location of the
/// event on the screen, before it had been adjusted for the containing window
/// and views. Whole numbers are pixels; the value may have a fraction for input
/// devices that are sub-pixel precise.</summary>
function AMotionEvent_getHistoricalRawX(MotionEvent: PAInputEvent; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getHistoricalRawX';
{$EXTERNALSYM AMotionEvent_getHistoricalRawX}

/// <summary>Get the historical raw Y coordinate of this event for the given
/// pointer index that occurred between this event and the previous motion
/// event. For touch events on the screen, this is the original location of the
/// event on the screen, before it had been adjusted for the containing window
/// and views. Whole numbers are pixels; the value may have a fraction for input
/// devices that are sub-pixel precise.</summary>
function AMotionEvent_getHistoricalRawY(MotionEvent: PAInputEvent; PointerIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getHistoricalRawY';
{$EXTERNALSYM AMotionEvent_getHistoricalRawY}

/// <summary>Get the historical X coordinate of this event for the given pointer
/// index that occurred between this event and the previous motion event. Whole
/// numbers are pixels; the value may have a fraction for input devices that are
/// sub-pixel precise.</summary>
function AMotionEvent_getHistoricalX(MotionEvent: PAInputEvent; PointerIndex, HistoryIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getHistoricalX';
{$EXTERNALSYM AMotionEvent_getHistoricalX}

/// <summary>Get the historical Y coordinate of this event for the given pointer
/// index that occurred between this event and the previous motion event. Whole
/// numbers are pixels; the value may have a fraction for input devices that are
/// sub-pixel precise.</summary>
function AMotionEvent_getHistoricalY(MotionEvent: PAInputEvent; PointerIndex, HistoryIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getHistoricalY';
{$EXTERNALSYM AMotionEvent_getHistoricalY}

/// <summary>Get the historical pressure of this event for the given pointer
/// indexthat occurred between this event and the previous motion event. The
/// pressure generally ranges from 0 (no pressure at all) to 1 (normal
/// pressure), however values higher than 1 may be generated depending on the
/// calibration of the input device.</summary>
function AMotionEvent_getHistoricalPressure(MotionEvent: PAInputEvent; PointerIndex, HistoryIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getHistoricalPressure';
{$EXTERNALSYM AMotionEvent_getHistoricalPressure}

/// <summary>Get the current scaled value of the approximate size for the given
/// pointer index that occurred between this event and the previous motion
/// event. This represents some approximation of the area of the screen being
/// pressed; the actual value in pixels corresponding to the touch is
/// normalized with the device specific range of values and scaled to a value
/// between 0 and 1. The value of size can be used to determine fat touch
/// events.</summary>
function AMotionEvent_getHistoricalSize(MotionEvent: PAInputEvent; PointerIndex, HistoryIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getHistoricalSize';
{$EXTERNALSYM AMotionEvent_getHistoricalSize}

/// <summary>Get the historical length of the major axis of an ellipse that
/// describes the touch area at the point of contact for the given pointer index
/// that occurred between this event and the previous motion event.</summary>
function AMotionEvent_getHistoricalTouchMajor(MotionEvent: PAInputEvent; PointerIndex, HistoryIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getHistoricalTouchMajor';
{$EXTERNALSYM AMotionEvent_getHistoricalTouchMajor}

/// <summary>Get the historical length of the minor axis of an ellipse that
/// describes the touch area at the point of contact for the given pointer index
/// that occurred between this event and the previous motion event.</summary>
function AMotionEvent_getHistoricalTouchMinor(MotionEvent: PAInputEvent; PointerIndex, HistoryIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getHistoricalTouchMinor';
{$EXTERNALSYM AMotionEvent_getHistoricalTouchMinor}

/// <summary>Get the historical length of the major axis of an ellipse that
/// describes the size of the approaching tool for the given pointer index that
/// occurred between this event and the previous motion event. The tool area
/// represents the estimated size of the finger or pen that is touching the
/// device independent of its actual touch area at the point of contact.</summary>
function AMotionEvent_getHistoricalToolMajor(MotionEvent: PAInputEvent; PointerIndex, HistoryIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getHistoricalToolMajor';
{$EXTERNALSYM AMotionEvent_getHistoricalToolMajor}

/// <summary>Get the historical length of the minor axis of an ellipse that
/// describes the size of the approaching tool for the given pointer index that
/// occurred between this event and the previous motion event. The tool area
/// represents the estimated size of the finger or pen that is touching the
/// device independent of its actual touch area at the point of contact.</summary>
function AMotionEvent_getHistoricalToolMinor(MotionEvent: PAInputEvent; PointerIndex, HistoryIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getHistoricalToolMinor';
{$EXTERNALSYM AMotionEvent_getHistoricalToolMinor}

/// <summary>Get the historical orientation of the touch area and tool area in
/// radians clockwise from vertical for the given pointer index that occurred
/// between this event and the previous motion event. An angle of 0 degrees
/// indicates that the major axis of contact is oriented upwards, is perfectly
/// circular or is of unknown orientation. A positive angle indicates that the
/// major axis of contact is oriented to the right. A negative angle indicates
/// that the major axis of contact is oriented to the left. The full range is
/// from -PI/2 radians (finger pointing fully left) to PI/2 radians (finger
/// pointing fully right).</summary>
function AMotionEvent_getHistoricalOrientation(MotionEvent: PAInputEvent; PointerIndex, HistoryIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getHistoricalOrientation';
{$EXTERNALSYM AMotionEvent_getHistoricalOrientation}

/// <remarks>Introduced in API 13</remarks>
/// <summary>Get the historical value of the request axis for the given pointer
/// index that occurred between this event and the previous motion event.
/// </summary>
function AMotionEvent_getHistoricalAxisValue(MotionEvent: PAInputEvent; Axis: Int32; PointerIndex, HistoryIndex: size_t): Single; cdecl;
  external AndroidLib name 'AMotionEvent_getHistoricalAxisValue';
{$EXTERNALSYM AMotionEvent_getHistoricalAxisValue}

/// <summary>Add this input queue to a looper for processing. See
/// ALooper_addFd() for information on the ident, callback, and data params.
/// </summary>
procedure AInputQueue_attachLooper(const Queue: PAInputQueue; Looper: PALooper; Ident: Integer; Callback: ALooper_callbackFunc; Data: Pointer); cdecl;
  external AndroidLib name 'AInputQueue_attachLooper';
{$EXTERNALSYM AInputQueue_attachLooper}

/// <summary>Remove the input queue from the looper it is currently attached to.
/// </summary>
procedure AInputQueue_detachLooper(const Queue: PAInputQueue); cdecl;
  external AndroidLib name 'AInputQueue_detachLooper';
{$EXTERNALSYM AInputQueue_detachLooper}

/// <summary>Returns true if there are one or more events available in the input
/// queue. Returns 1 if the queue has events; 0 if it does not have events; and
/// a negative value if there is an error. </summary>
function AInputQueue_hasEvents(const Queue: PAInputQueue): Int32; cdecl;
  external AndroidLib name 'AInputQueue_hasEvents';
{$EXTERNALSYM AInputQueue_hasEvents}

/// <summary>Returns the next available event from the queue. Returns a negative
/// value if no events are available or an error has occurred.</summary>
function AInputQueue_getEvent(const Queue: PAInputQueue; OutEvent: PPAInputEvent): Int32; cdecl;
  external AndroidLib name 'AInputQueue_getEvent';
{$EXTERNALSYM AInputQueue_getEvent}

/// <summary>Sends the key for standard pre-dispatching -- that is, possibly
/// deliver it to the current IME to be consumed before the app. Returns 0 if it
/// was not pre-dispatched, meaning you can process it right now. If non-zero is
/// returned, you must abandon the current event processing and allow the event
/// to appear again in the event queue (if it does not get consumed during
/// pre-dispatching).</summary>
function AInputQueue_preDispatchEvent(const Queue: PAInputQueue; Event: PAinputEvent): Int32; cdecl;
  external AndroidLib name 'AInputQueue_preDispatchEvent';
{$EXTERNALSYM AInputQueue_preDispatchEvent}

/// <summary>Report that dispatching has finished with the given event.
/// This must be called after receiving an event with AInputQueue_get_event().
/// </summary>
procedure AInputQueue_finishEvent(const Queue: PAInputQueue; Event: PAinputEvent; Handled: Integer); cdecl;
  external AndroidLib name 'AInputQueue_finishEvent';
{$EXTERNALSYM AInputQueue_finishEvent}

implementation

end.
