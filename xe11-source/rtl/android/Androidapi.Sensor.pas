{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.Sensor;

interface

uses Posix.SysTypes, Androidapi.Looper;

(*$HPPEMIT '#include <android/sensor.h>' *)

{$I Androidapi.inc}

const
  /// <summary>Invalid sensor type. Returned by ASensor_getType as error value.
  /// </summary>
  ASENSOR_TYPE_INVALID             = -1;
  {$EXTERNALSYM ASENSOR_TYPE_INVALID}

  /// <summary>All values are in SI units (m/s^2) and measure the acceleration
  /// of the device minus the force of gravity.<br />
  /// reporting-mode: continuous</summary>
  ASENSOR_TYPE_ACCELEROMETER       = 1;
  {$EXTERNALSYM ASENSOR_TYPE_ACCELEROMETER}

  /// <summary>All values are in micro-Tesla (uT) and measure the geomagnetic
  /// field in the X, Y and Z axis.<br />
  /// reporting-mode: continuous</summary>
  ASENSOR_TYPE_MAGNETIC_FIELD      = 2;
  {$EXTERNALSYM ASENSOR_TYPE_MAGNETIC_FIELD}

  /// <summary>All values are in radians/second and measure the rate of
  /// rotation around the X, Y and Z axis.<br />
  /// reporting-mode: continuous</summary>
  ASENSOR_TYPE_GYROSCOPE           = 4;
  {$EXTERNALSYM ASENSOR_TYPE_GYROSCOPE}

  /// <summary>The light sensor value is returned in SI lux units.<br />
  /// reporting-mode: on-change</summary>
  ASENSOR_TYPE_LIGHT               = 5;
  {$EXTERNALSYM ASENSOR_TYPE_LIGHT}

  /// <summary>The pressure sensor value is returned in hPa (millibar).
  /// </summary>
  ASENSOR_TYPE_PRESSURE            = 6;
  {$EXTERNALSYM ASENSOR_TYPE_PRESSURE}

  /// <summary>The proximity sensor which turns the screen off and back on
  /// during calls is the wake-up proximity sensor. Implement wake-up proximity
  /// sensor before implementing a non wake-up proximity sensor.<br />
  /// For the wake-up proximity sensor set the flag SENSOR_FLAG_WAKE_UP.<br />
  /// The value corresponds to the distance to the nearest object in
  /// centimeters.<br />
  /// reporting-mode: on-change</summary>
  ASENSOR_TYPE_PROXIMITY           = 8;
  {$EXTERNALSYM ASENSOR_TYPE_PROXIMITY}

  /// <summary>All values are in SI units (m/s^2) and measure the direction and
  /// magnitude of gravity. When the device is at rest, the output of the
  /// gravity sensor should be identical to that of the accelerometer.</summary>
  ASENSOR_TYPE_GRAVITY             = 9;
  {$EXTERNALSYM ASENSOR_TYPE_GRAVITY}

  /// <summary>All values are in SI units (m/s^2) and measure the acceleration
  /// of the device not including the force of gravity.<br />
  /// reporting-mode: continuous</summary>
  ASENSOR_TYPE_LINEAR_ACCELERATION = 10;
  {$EXTERNALSYM ASENSOR_TYPE_LINEAR_ACCELERATION}

  ASENSOR_TYPE_ROTATION_VECTOR     = 11;
  {$EXTERNALSYM ASENSOR_TYPE_ROTATION_VECTOR}

  /// <summary>The relative humidity sensor value is returned in percent.
  /// </summary>
  ASENSOR_TYPE_RELATIVE_HUMIDITY   = 12;
  {$EXTERNALSYM ASENSOR_TYPE_RELATIVE_HUMIDITY}

  /// <summary>The ambient temperature sensor value is returned in Celcius.
  /// </summary>
  ASENSOR_TYPE_AMBIENT_TEMPERATURE = 13;
  {$EXTERNALSYM ASENSOR_TYPE_AMBIENT_TEMPERATURE}

  ASENSOR_TYPE_MAGNETIC_FIELD_UNCALIBRATED = 14;
  {$EXTERNALSYM ASENSOR_TYPE_MAGNETIC_FIELD_UNCALIBRATED}

  ASENSOR_TYPE_GAME_ROTATION_VECTOR = 15;
  {$EXTERNALSYM ASENSOR_TYPE_GAME_ROTATION_VECTOR}

  ASENSOR_TYPE_GYROSCOPE_UNCALIBRATED = 16;
  {$EXTERNALSYM ASENSOR_TYPE_GYROSCOPE_UNCALIBRATED}

  ASENSOR_TYPE_SIGNIFICANT_MOTION = 17;
  {$EXTERNALSYM ASENSOR_TYPE_SIGNIFICANT_MOTION}

  ASENSOR_TYPE_STEP_DETECTOR = 18;
  {$EXTERNALSYM ASENSOR_TYPE_STEP_DETECTOR}

  ASENSOR_TYPE_STEP_COUNTER = 19;
  {$EXTERNALSYM ASENSOR_TYPE_STEP_COUNTER}

  ASENSOR_TYPE_GEOMAGNETIC_ROTATION_VECTOR = 20;
  {$EXTERNALSYM ASENSOR_TYPE_GEOMAGNETIC_ROTATION_VECTOR}

  ASENSOR_TYPE_HEART_RATE = 21;
  {$EXTERNALSYM ASENSOR_TYPE_HEART_RATE}

  ASENSOR_TYPE_POSE_6DOF = 28;
  {$EXTERNALSYM ASENSOR_TYPE_POSE_6DOF}

  ASENSOR_TYPE_STATIONARY_DETECT = 29;
  {$EXTERNALSYM ASENSOR_TYPE_STATIONARY_DETECT}

  ASENSOR_TYPE_MOTION_DETECT = 30;
  {$EXTERNALSYM ASENSOR_TYPE_MOTION_DETECT}

  ASENSOR_TYPE_HEART_BEAT = 31;
  {$EXTERNALSYM ASENSOR_TYPE_HEART_BEAT}

  ASENSOR_TYPE_LOW_LATENCY_OFFBODY_DETECT = 34;
  {$EXTERNALSYM ASENSOR_TYPE_LOW_LATENCY_OFFBODY_DETECT}

  ASENSOR_TYPE_ACCELEROMETER_UNCALIBRATED = 35;
  {$EXTERNALSYM ASENSOR_TYPE_ACCELEROMETER_UNCALIBRATED}

{ Sensor accuracy measure }
const
  ASENSOR_STATUS_NO_CONTACT       = -1;
  {$EXTERNALSYM ASENSOR_STATUS_NO_CONTACT}

  ASENSOR_STATUS_UNRELIABLE       = 0;
  {$EXTERNALSYM ASENSOR_STATUS_UNRELIABLE}

  ASENSOR_STATUS_ACCURACY_LOW     = 1;
  {$EXTERNALSYM ASENSOR_STATUS_ACCURACY_LOW}

  ASENSOR_STATUS_ACCURACY_MEDIUM  = 2;
  {$EXTERNALSYM ASENSOR_STATUS_ACCURACY_MEDIUM}

  ASENSOR_STATUS_ACCURACY_HIGH    = 3;
  {$EXTERNALSYM ASENSOR_STATUS_ACCURACY_HIGH}

{ Sensor reporting modes }
const
  /// <summary>invalid reporting mode</summary>
  AREPORTING_MODE_INVALID = -1;
  {$EXTERNALSYM AREPORTING_MODE_INVALID}

  /// <summary>continuous reporting</summary>
  AREPORTING_MODE_CONTINUOUS = 0;
  {$EXTERNALSYM AREPORTING_MODE_CONTINUOUS}

  /// <summary>reporting on change</summary>
  AREPORTING_MODE_ON_CHANGE = 1;
  {$EXTERNALSYM AREPORTING_MODE_ON_CHANGE}

  /// <summary>one shot reporting</summary>
  AREPORTING_MODE_ONE_SHOT = 2;
  {$EXTERNALSYM AREPORTING_MODE_ONE_SHOT}

  /// <summary>special trigger reporting</summary>
  AREPORTING_MODE_SPECIAL_TRIGGER = 3;
  {$EXTERNALSYM AREPORTING_MODE_SPECIAL_TRIGGER}

{ Sensor Direct Report Rates }
const
  /// <summary>stopped</summary>
  ASENSOR_DIRECT_RATE_STOP = 0;
  {$EXTERNALSYM ASENSOR_DIRECT_RATE_STOP}

  /// <summary>nominal 50Hz</summary>
  ASENSOR_DIRECT_RATE_NORMAL = 1;
  {$EXTERNALSYM ASENSOR_DIRECT_RATE_NORMAL}

  /// <summary>nominal 200Hz</summary>
  ASENSOR_DIRECT_RATE_FAST = 2;
  {$EXTERNALSYM ASENSOR_DIRECT_RATE_FAST}

  /// <summary>nominal 800Hz</summary>
  ASENSOR_DIRECT_RATE_VERY_FAST = 3;
  {$EXTERNALSYM ASENSOR_DIRECT_RATE_VERY_FAST}

{ Sensor Direct Channel Type }
const
  /// <summary>shared memory created by ASharedMemory_create</summary>
  ASENSOR_DIRECT_CHANNEL_TYPE_SHARED_MEMORY = 1;
  {$EXTERNALSYM ASENSOR_DIRECT_CHANNEL_TYPE_SHARED_MEMORY}

  /// <summary>AHardwareBuffer_</summary>
  ASENSOR_DIRECT_CHANNEL_TYPE_HARDWARE_BUFFER = 2;
  {$EXTERNALSYM ASENSOR_DIRECT_CHANNEL_TYPE_HARDWARE_BUFFER}


{ A few useful constants }
const
  /// <summary>Earth's gravity in m/s^2</summary>
  ASENSOR_STANDARD_GRAVITY         = 9.80665;
  {$EXTERNALSYM ASENSOR_STANDARD_GRAVITY}

  /// <summary>Maximum magnetic field on Earth's surface in uT</summary>
  ASENSOR_MAGNETIC_FIELD_EARTH_MAX = 60.0;
  {$EXTERNALSYM ASENSOR_MAGNETIC_FIELD_EARTH_MAX}

  /// <summary>Minimum magnetic field on Earth's surface in uT</summary>
  ASENSOR_MAGNETIC_FIELD_EARTH_MIN = 30.0;
  {$EXTERNALSYM ASENSOR_MAGNETIC_FIELD_EARTH_MIN}

type
{ A sensor event. }
  ASensorVector = record
    case Integer of
      0: (v: array[0..2] of Single;
          status: Int8;
          reserved: array[0..2] of UInt8);
      1: (x, y, z: Single);
      2: (azimuth, pitch, roll: Single);
  end;
  {$EXTERNALSYM ASensorVector}
  PASensorVector = ^ASensorVector;

  AMetaDataEvent = record
    what: Int32;
    sensor: Int32;
  end;
  {$EXTERNALSYM AMetaDataEvent}
  PAMetaDataEvent = ^AMetaDataEvent;

  AUncalibratedEvent = record
    case Integer of
      0: (uncalib: array[0..2] of Single;
          bias: array[0..2] of Single);
      1: (x_uncalib: Single;
          y_uncalib: Single;
          z_uncalib: Single;
          x_bias: Single;
          y_bias: Single;
          z_bias: Single);
  end;
  {$EXTERNALSYM AUncalibratedEvent}
  PAUncalibratedEvent = ^AUncalibratedEvent;

  AHeartRateEvent = record
    bpm: Single;
    status: Int8;
  end;
  {$EXTERNALSYM AHeartRateEvent}
  PAHeartRateEvent = ^AHeartRateEvent;

  ADynamicSensorEvent = record
    connected: Int32;
    handle: Int32;
  end;
  {$EXTERNALSYM ADynamicSensorEvent}
  PADynamicSensorEvent = ^ADynamicSensorEvent;

  AAdditionalInfoEvent = record
    &type: Int32;
    serial: Int32;
    case Integer of
      0: (data_int32: array[0..13] of Int32);
      1: (data_float: array[0..13] of Single);
  end;
  {$EXTERNALSYM AAdditionalInfoEvent}
  PAAdditionalInfoEvent = ^AAdditionalInfoEvent;

  ASensorEvent = record
    version: Int32; { SizeOf(ASensorEvent) }
    sensor: Int32;
    __type: Int32;
    reserved0: Int32;
    timestamp: Int64;

    case Integer of
      0:(data: array[0..15] of Single;
         flags: UInt32;
         reserved1: array[0..2] of Int32);
      1:(vector: ASensorVector);
      2:(acceleration: ASensorVector);
      3:(magnetic: ASensorVector);
      4:(temperature: Single);
      5:(distance: Single);
      6:(light: Single);
      7:(pressure: Single);
      8:(relative_humidity: Single);
      9:(uncalibrated_gyro: AUncalibratedEvent);
      10:(uncalibrated_magnetic: AUncalibratedEvent);
      11:(meta_data: AMetaDataEvent);
      12:(heart_rate: AHeartRateEvent);
      13:(dynamic_sensor_meta: ADynamicSensorEvent);
      14:(additional_info: AAdditionalInfoEvent);
      15:(data_: array[0..7] of UInt64;
          step_counter: UInt64);
  end;
  {$EXTERNALSYM ASensorEvent}
  PASensorEvent = ^ASensorEvent;

  /// <summary>ASensorManager is an opaque type to manage sensors and events
  /// queues.<br />
  /// ASensorManager is a singleton that can be obtained
  /// using ASensorManager_getInstance().<br />
  /// This file provides a set of functions that uses ASensorManager to access
  /// and list hardware sensors, and create and destroy event queues:<br />
  /// - ASensorManager_getSensorList()<br />
  /// - ASensorManager_getDefaultSensor()<br />
  /// - ASensorManager_getDefaultSensorEx()<br />
  /// - ASensorManager_createEventQueue()<br />
  /// - ASensorManager_destroyEventQueue()</summary>
  ASensorManager = record end;
  {$EXTERNALSYM ASensorManager}
  PASensorManager = ^ASensorManager;

  /// <summary>ASensorEventQueue is an opaque type that provides access to
  /// ASensorEvent from hardware sensors.<br />
  /// A new ASensorEventQueue can be obtained using
  /// ASensorManager_createEventQueue().<br />
  /// This file provides a set of functions to enable and disable sensors,
  /// check and get events, and set event rates on aASensorEventQueue:<br />
  /// - ASensorEventQueue_enableSensor()<br />
  /// - ASensorEventQueue_disableSensor()<br />
  /// - ASensorEventQueue_hasEvents()<br />
  /// - ASensorEventQueue_getEvents()<br />
  /// - ASensorEventQueue_setEventRate()</summary>
  ASensorEventQueue = record end;
  {$EXTERNALSYM ASensorEventQueue}
  PASensorEventQueue = ^ASensorEventQueue;

  /// <summary>ASensor is an opaque type that provides information about
  /// hardware sensors.<br />
  /// A ASensor pointer can be obtained using
  /// ASensorManager_getDefaultSensor(), ASensorManager_getDefaultSensorEx()
  /// or from a ASensorList.<br />
  /// This file provides a set of functions to access properties of
  /// a ASensor:<br />
  /// - ASensor_getName()<br />
  /// - ASensor_getVendor()<br />
  /// - ASensor_getType()<br />
  /// - ASensor_getResolution()<br />
  /// - ASensor_getMinDelay()<br />
  /// - ASensor_getFifoMaxEventCount()<br />
  /// - ASensor_getFifoReservedEventCount()<br />
  /// - ASensor_getStringType()<br />
  /// - ASensor_getReportingMode()<br />
  /// - ASensor_isWakeUpSensor()</summary>
  ASensor = record end;
  {$EXTERNALSYM ASensor}
  PASensor = ^ASensor;

  /// <summary>ASensorRef is a type for constant pointers to Sensor.<br />
  /// This is used to define entry in SensorList arrays.</summary>
  ASensorRef = ^ASensor;
  {$EXTERNALSYM ASensorRef}

  /// <summary>ASensorList is an array of reference to ASensor.<br />
  /// A ASensorList can be initialized using
  /// ASensorManager_getSensorList().</summary>
  ASensorList = ^ASensorRef;
  {$EXTERNALSYM ASensorList}

  AHardwareBuffer_ = record end;
  {$EXTERNALSYM ASensor}
  PAHardwareBuffer = ^AHardwareBuffer_;

/// <remarks>Deprecated in API 26: use ASensorManager_getInstanceForPackage()
/// instead on API 26 or higher</remarks>
/// <summary>Get a reference to the sensor manager. ASensorManager is a
/// singleton per package as different packages may have access to different
/// sensors.
/// <code lang="Delphi">var sensorManager: PASensorManager;
/// ...
/// sensorManager := ASensorManager_getInstance();</code></summary>
function ASensorManager_getInstance: PASensorManager; cdecl;
  external AndroidLib name 'ASensorManager_getInstance';
{$EXTERNALSYM ASensorManager_getInstance}

{$IFDEF API_26}
/// <remarks>Introduced in API 26</remarks>
/// <summary>Get a reference to the sensor manager. ASensorManager is a
/// singleton per package as different packages may have access to different
/// sensors.
/// <code lang="Delphi">var sensorManager: PASensorManager;
/// ...
/// sensorManager := ASensorManager_getInstanceForPackage('foo.bar.baz');</code>
/// </summary>
function ASensorManager_getInstanceForPackage(const packageName: MarshaledAString): PASensorManager; cdecl;
  external AndroidLib name 'ASensorManager_getInstanceForPackage';
{$EXTERNALSYM ASensorManager_getInstanceForPackage}
{$ENDIF API_26}

/// <summary>Returns the list of available sensors.</summary>
function ASensorManager_getSensorList(SensorManager: PASensorManager; List: ASensorList): Integer; cdecl;
  external AndroidLib name 'ASensorManager_getSensorList';
{$EXTERNALSYM ASensorManager_getSensorList}

/// <summary>Returns the default sensor for the given type, or nil if no sensor
/// ofthat type exist.</summary>
function ASensorManager_getDefaultSensor(SensorManager: PASensorManager; SensorType: Integer): PASensor; cdecl;
  external AndroidLib name 'ASensorManager_getDefaultSensor';
{$EXTERNALSYM ASensorManager_getDefaultSensor}

{$IFDEF API_26}
/// <remarks>Introduced in API 26</remarks>
/// <summary>Returns the default sensor with the given type and wakeUp
/// properties or nil if no sensor of this type and wakeUp properties exists.
/// </summary>
function ASensorManager_getDefaultSensorEx(SensorManager: PASensorManager; SensorType: Integer;
  WakeUp: Boolean): PASensor; cdecl;
  external AndroidLib name 'ASensorManager_getDefaultSensorEx';
{$EXTERNALSYM ASensorManager_getDefaultSensorEx}
{$ENDIF API_26}

/// <summary>Creates a new sensor event queue and associates it with a looper.
/// <br />
/// "Ident" is an identifier for the events that will be returned when
/// calling ALooper_pollOnce(). The identifier must be >= 0, or
/// ALOOPER_POLL_CALLBACK if providing a non-nil callback.</summary>
function ASensorManager_createEventQueue(SensorManager: PASensorManager; Looper: PALooper; Ident: Integer; Callback: ALooper_callbackFunc; Data: Pointer): PASensorEventQueue; cdecl;
  external AndroidLib name 'ASensorManager_createEventQueue';
{$EXTERNALSYM ASensorManager_createEventQueue}

/// <summary>Destroys the event queue and frees all resources associated to it.
/// </summary>
function ASensorManager_destroyEventQueue(SensorManager: PASensorManager; Queue: PASensorEventQueue): Integer; cdecl;
  external AndroidLib name 'ASensorManager_destroyEventQueue';
{$EXTERNALSYM ASensorManager_destroyEventQueue}

{$IFDEF API_26}
/// <remarks>Introduced in API 26</remarks>
/// <summary>Create direct channel based on shared memory<br />
/// Create a direct channel of ASENSOR_DIRECT_CHANNEL_TYPE_SHARED_MEMORY to be
/// used for configuring sensor direct report.<br />
/// SensorManager: the ASensorManager instance obtained from
///   ASensorManager_getInstanceForPackage.<br />
/// FD: file descriptor representing a shared memory created by
///   ASharedMemory_create<br />
/// Size: size to be used, must be less or equal to size of shared memory.<br />
/// Returns a positive integer as a channel id to be used in
///   ASensorManager_destroyDirectChannel and
///   ASensorManager_configureDirectReport, or value less or equal to 0 for
///     failures.</summary>
function ASensorManager_createSharedMemoryDirectChannel(SensorManager: PASensorManager; FD: Integer;
  Size: size_t): PASensor; cdecl;
  external AndroidLib name 'ASensorManager_createSharedMemoryDirectChannel';
{$EXTERNALSYM ASensorManager_createSharedMemoryDirectChannel}

/// <remarks>Introduced in API 26</remarks>
/// <summary>Create direct channel based on AHardwareBuffer_<br />
/// Create a direct channel of ASENSOR_DIRECT_CHANNEL_TYPE_HARDWARE_BUFFER type
/// to be used for configuring sensor direct report.<br />
/// SensorManager: the ASensorManager instance obtained from
///   ASensorManager_getInstanceForPackage.<br />
/// Buffer: AHardwareBuffer_ instance created by AHardwareBuffer_allocate.<br />
/// Size: the intended size to be used, must be less or equal to size of
///   buffer.<br />
/// Returns a positive integer as a channel id to be used in
///   ASensorManager_destroyDirectChannel and
///   ASensorManager_configureDirectReport, or value less or equal to 0 for
///   failures.<br /></summary>
function ASensorManager_createHardwareBufferDirectChannel(SensorManager: PASensorManager; Buffer: PAHardwareBuffer;
  Size: size_t): PASensor; cdecl;
  external AndroidLib name 'ASensorManager_createHardwareBufferDirectChannel';
{$EXTERNALSYM ASensorManager_createHardwareBufferDirectChannel}

/// <remarks>Introduced in API 26</remarks>
/// <summary>Destroy a direct channel<br />
/// Destroy a direct channel previously created using
/// ASensorManager_createDirectChannel. The buffer used for creating direct
/// channel does not get destroyed with ASensorManager_destroy and has to be
/// close or released separately.<br />
/// SensorManager: the ASensorManager instance obtained from
///   ASensorManager_getInstanceForPackage.<br />
/// ChannelId: channel id (a positive integer) returned from
///   ASensorManager_createSharedMemoryDirectChannel or
///   ASensorManager_createHardwareBufferDirectChannel.</summary>
procedure ASensorManager_destroyDirectChannel(SensorManager: PASensorManager; ChannelId: Integer); cdecl;
  external AndroidLib name 'ASensorManager_destroyDirectChannel';
{$EXTERNALSYM ASensorManager_destroyDirectChannel}

/// <remarks>Introduced in API 26</remarks>
/// <summary>Configure direct report on channel<br />
/// Configure sensor direct report on a direct channel: set rate to value other
/// than ASENSOR_DIRECT_RATE_STOP so that sensor event can be directly written
/// into the shared memory region used for creating the buffer. It returns a
/// positive token which can be used for identify sensor events from different
/// sensors on success. Calling with rate ASENSOR_DIRECT_RATE_STOP will stop
/// direct report of the sensor specified in the channel.<br />
/// To stop all active sensor direct report configured to a channel, set sensor
/// to nil and rate to ASENSOR_DIRECT_RATE_STOP.<br />
/// In order to successfully configure a direct report, the sensor has to
/// support the specified rate and the channel type, which can be checked by
/// ASensor_getHighestDirectReportRateLevel and
/// ASensor_isDirectChannelTypeSupported, respectively.<br />
/// SensorManager: the ASensorManager instance obtained from
///   ASensorManager_getInstanceForPackage.<br />
/// Sensor: a ASensor to denote which sensor to be operate. It can be nil if
///   rate is ASENSOR_DIRECT_RATE_STOP, denoting stopping of all active sensor
///   direct report.<br />
/// ChannelId: channel id (a positive integer) returned from
///   ASensorManager_createSharedMemoryDirectChannel or
///   ASensorManager_createHardwareBufferDirectChannel.<br />
/// Returns positive token for success or negative error code.
/// <code lang="Delphi">var
///   Manager: PASensorManager;
///   Sensor: PASensor;
///   ChannelId: Integer;
/// ...
/// Manager := ...
/// Sensor := ...
/// ChannelId := ...
/// ASensorManager_configureDirectReport(Manager, Sensor, ChannelId,
///   ASENSOR_DIRECT_RATE_FAST);</code></summary>
function ASensorManager_configureDirectReport(SensorManager: PASensorManager; const Sensor: PASensor;
  ChannelId, Rate: Integer): Integer; cdecl;
  external AndroidLib name 'ASensorManager_configureDirectReport';
{$EXTERNALSYM ASensorManager_configureDirectReport}

/// <remarks>Introduced in API 26</remarks>
/// <summary>Enable the selected sensor with sampling and report parameters
/// <br />
/// Enable the selected sensor at a specified sampling period and max
/// batch report latency.<br />
/// To disable sensor, use ASensorEventQueue_disableSensor.<br />
/// SensorEventQueue: ASensorEventQueue for sensor event to be reported to.<br />
/// Sensor: ASensor to be enabled.<br />
/// SamplingPeriodUs: sampling period of sensor in microseconds.<br />
/// MaxBatchReportLatencyUs: maximum time interval between two batch of sensor
///   events are delivered in microseconds. For sensor streaming, set to 0.<br />
/// Returns 0 on success or a negative error code on failure.</summary>
function ASensorEventQueue_registerSensor(SensorEventQueue: PASensorEventQueue; const Sensor: PASensor;
  SamplingPeriodUs: Int32; MaxBatchReportLatencyUs: Int64): Integer; cdecl;
  external AndroidLib name 'ASensorEventQueue_registerSensor';
{$EXTERNALSYM ASensorEventQueue_registerSensor}
{$ENDIF API_26}

/// <summary>Enable the selected sensor at default sampling rate.<br />
/// Start event reports of a sensor to specified sensor event queue at
/// a default rate.<br />
/// SensorEventQueue: ASensorEventQueue for sensor event to be reported to.<br />
/// Sensor: ASensor to be enabled.<br />
/// Returns 0 on success or a negative error code on failure.</summary>
function ASensorEventQueue_enableSensor(SensorEventQueue: PASensorEventQueue; const Sensor: PASensor): Integer; cdecl;
  external AndroidLib name 'ASensorEventQueue_enableSensor';
{$EXTERNALSYM ASensorEventQueue_enableSensor}

/// <summary>Disable the selected sensor.<br />
/// Stop event reports from the sensor to specified sensor event queue.<br />
/// SensorEventQueue: ASensorEventQueue to be changed<br />
/// Sensor: ASensor to be disabled<br />
/// Returns 0 on success or a negative error code on failure.</summary>
function ASensorEventQueue_disableSensor(SensorEventQueue: PASensorEventQueue; const Sensor: PASensor): Integer; cdecl;
  external AndroidLib name 'ASensorEventQueue_disableSensor';
{$EXTERNALSYM ASensorEventQueue_disableSensor}

/// <summary>Sets the delivery rate of events in microseconds for the given
/// sensor.<br />
/// This function has to be called after ASensorEventQueue_enableSensor.<br />
/// Note that this is a hint only, generally event will arrive at a higher rate.
/// It is an error to set a rate inferior to the value returned by
/// ASensor_getMinDelay().<br />
/// SensorEventQueue: ASensorEventQueue to which sensor event is delivered.<br />
/// Sensor: ASensor of which sampling rate to be updated.<br />
/// USeconds: sensor sampling period (1/sampling rate) in microseconds<br />
/// Returns 0 on sucess or a negative error code on failure.</summary>
function ASensorEventQueue_setEventRate(SensorEventQueue: PASensorEventQueue; const Sensor: PASensor; USeconds: Int32): Integer; cdecl;
  external AndroidLib name 'ASensorEventQueue_setEventRate';
{$EXTERNALSYM ASensorEventQueue_setEventRate}

/// <summary>Determine if a sensor event queue has pending event to be
/// processed.<br />
/// SensorManager: ASensorEventQueue to be queried<br />
/// Returns 1 if the queue has events; 0 if it does not have events; or a
/// negative value if there is an error.</summary>
function ASensorEventQueue_hasEvents(SensorManager: PASensorManager): Integer; cdecl;
  external AndroidLib name 'ASensorEventQueue_hasEvents';
{$EXTERNALSYM ASensorEventQueue_hasEvents}

/// <summary>Retrieve pending events in sensor event queue<br />
/// Retrieve next available events from the queue to a specified event
/// array.<br />
/// SensorEventQueue: ASensorEventQueue to get events from<br />
/// SensorEvents: pointer to an array of ASensorEvents.<br />
/// Count: max number of events that can be filled into array event.<br />
/// Returns number of events on success; negative error code when no events are
/// pending or an error has occurred.</summary>
/// <example><code lang="Delphi">var
///   event: ASensorEvent;
///   numEvent: ssize_t;
/// ...
/// numEvent :=ASensorEventQueue_getEvents(queue, @event, 1); </code>
/// <code lang="Delphi">var
///   eventBuffer: array[0..7] of ASensorEvent;
///   numEvent: ssize_t;
/// ...
/// numEvent := ASensorEventQueue_getEvents(queue,@eventBuffer[0], 8); </code>
/// </example>
function ASensorEventQueue_getEvents(SensorEventQueue: PASensorEventQueue; SensorEvents: PASensorEvent; Count: size_t): ssize_t; cdecl;
  external AndroidLib name 'ASensorEventQueue_getEvents';
{$EXTERNALSYM ASensorEventQueue_getEvents}

/// <summary>Returns this sensor's name (non localized)</summary>
function ASensor_getName(const Sensor: PASensor): MarshaledAString; cdecl;
  external AndroidLib name 'ASensor_getName';
{$EXTERNALSYM ASensor_getName}

/// <summary>Returns this sensor's vendor's name (non localized)</summary>
function ASensor_getVendor(const Sensor: PASensor): MarshaledAString; cdecl;
  external AndroidLib name 'ASensor_getVendor';
{$EXTERNALSYM ASensor_getVendor}

/// <summary>Return this sensor's type</summary>
function ASensor_getType(const Sensor: PASensor): Integer; cdecl;
  external AndroidLib name 'ASensor_getType';
{$EXTERNALSYM ASensor_getType}

/// <summary>Returns this sensors's resolution</summary>
function ASensor_getResolution(const Sensor: PASensor): Single; cdecl;
  external AndroidLib name 'ASensor_getResolution';
{$EXTERNALSYM ASensor_getResolution}

/// <summary>Returns the minimum delay allowed between events in microseconds.
/// A value of zero means that this sensor doesn't report events at a constant
/// rate, but rather only when a new data is available.</summary>
function ASensor_getMinDelay(const Sensor: PASensor): Integer; cdecl;
  external AndroidLib name 'ASensor_getMinDelay';
{$EXTERNALSYM ASensor_getMinDelay}

/// <remarks>Introduced in API 21</remarks>
/// <summary>Returns the maximum size of batches for this sensor. Batches will
/// often be smaller, as the hardware fifo might be used for other sensors.
/// </summary>
function ASensor_getFifoMaxEventCount(const Sensor: PASensor): Integer; cdecl;
  external AndroidLib name 'ASensor_getFifoMaxEventCount';
{$EXTERNALSYM ASensor_getFifoMaxEventCount}

/// <remarks>Introduced in API 21</remarks>
/// <summary>Returns the hardware batch fifo size reserved to this sensor.</summary>
function ASensor_getFifoReservedEventCount(const Sensor: PASensor): Integer; cdecl;
  external AndroidLib name 'ASensor_getFifoReservedEventCount';
{$EXTERNALSYM ASensor_getFifoReservedEventCount}

/// <remarks>Introduced in API 21</remarks>
/// <summary>Returns this sensor's string type.</summary>
function ASensor_getStringType(const Sensor: PASensor): MarshaledAString; cdecl;
  external AndroidLib name 'ASensor_getStringType';
{$EXTERNALSYM ASensor_getStringType}

/// <remarks>Introduced in API 21</remarks>
/// <summary>Returns the reporting mode for this sensor. One of
/// AREPORTING_MODE_* constants.</summary>
function ASensor_getReportingMode(const Sensor: PASensor): Integer; cdecl;
  external AndroidLib name 'ASensor_getReportingMode';
{$EXTERNALSYM ASensor_getReportingMode}

/// <remarks>Introduced in API 21</remarks>
/// <summary>Returns true if this is a wake up sensor, false otherwise.</summary>
function ASensor_isWakeUpSensor(const Sensor: PASensor): Boolean; cdecl;
  external AndroidLib name 'ASensor_isWakeUpSensor';
{$EXTERNALSYM ASensor_isWakeUpSensor}

{$IFDEF API_26}
/// <remarks>Introduced in API 26</remarks>
/// <summary>Test if sensor supports a certain type of direct channel.<br />
/// Sensor: a ASensor to denote the sensor to be checked.<br />
/// ChannelType: Channel type constant, either
///   ASENSOR_DIRECT_CHANNEL_TYPE_SHARED_MEMORY or
///   ASENSOR_DIRECT_CHANNEL_TYPE_HARDWARE_BUFFER.<br />
/// Returns True if sensor supports the specified direct channel type.</summary>
function ASensor_isDirectChannelTypeSupported(const Sensor: PASensor; ChannelType: Integer): Boolean; cdecl;
  external AndroidLib name 'ASensor_isDirectChannelTypeSupported';
{$EXTERNALSYM ASensor_isDirectChannelTypeSupported}

/// <remarks>Introduced in API 26</remarks>
/// <summary>Get the highest direct rate level that a sensor support.<br />
/// Sensor: a ASensor to denote the sensor to be checked.<br />
/// Returns a ASENSOR_DIRECT_RATE_... constant denoting the highest rate level
///   supported by the sensor. If return value is ASENSOR_DIRECT_RATE_STOP, it
///   means the sensor does not support direct report.</summary>
function ASensor_getHighestDirectReportRateLevel(const Sensor: PASensor): Integer; cdecl;
  external AndroidLib name 'ASensor_getHighestDirectReportRateLevel';
{$EXTERNALSYM ASensor_getHighestDirectReportRateLevel}
{$ENDIF API_26}

implementation

end.
