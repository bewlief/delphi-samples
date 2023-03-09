{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Devices.Midi;

{$HPPEMIT NOUSINGNAMESPACE}

{$WARN SYMBOL_DEPRECATED OFF}

interface

{$MINENUMSIZE 4}

uses 
  Winapi.Windows, 
  Winapi.WinRT, 
  System.Types, 
  System.Win.WinRT, 
  Winapi.CommonTypes, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type

  // Forward declarations for interfaces

  // Windows.Devices.Midi.IMidiMessage
  IMidiMessage = interface;
  PIMidiMessage = ^IMidiMessage;

  // Windows.Devices.Midi.IMidiChannelPressureMessage
  IMidiChannelPressureMessage = interface;
  PIMidiChannelPressureMessage = ^IMidiChannelPressureMessage;

  // Windows.Devices.Midi.IMidiChannelPressureMessageFactory
  IMidiChannelPressureMessageFactory = interface;
  PIMidiChannelPressureMessageFactory = ^IMidiChannelPressureMessageFactory;

  // Windows.Devices.Midi.IMidiControlChangeMessage
  IMidiControlChangeMessage = interface;
  PIMidiControlChangeMessage = ^IMidiControlChangeMessage;

  // Windows.Devices.Midi.IMidiControlChangeMessageFactory
  IMidiControlChangeMessageFactory = interface;
  PIMidiControlChangeMessageFactory = ^IMidiControlChangeMessageFactory;

  // Windows.Devices.Midi.IMidiMessageReceivedEventArgs
  IMidiMessageReceivedEventArgs = interface;
  PIMidiMessageReceivedEventArgs = ^IMidiMessageReceivedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Midi.IMidiInPort,Windows.Devices.Midi.IMidiMessageReceivedEventArgs>
  TypedEventHandler_2__IMidiInPort__IMidiMessageReceivedEventArgs = interface;
  PTypedEventHandler_2__IMidiInPort__IMidiMessageReceivedEventArgs = ^TypedEventHandler_2__IMidiInPort__IMidiMessageReceivedEventArgs;

  // Windows.Devices.Midi.IMidiInPort
  IMidiInPort = interface;
  PIMidiInPort = ^IMidiInPort;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Midi.IMidiInPort>
  AsyncOperationCompletedHandler_1__IMidiInPort = interface;
  PAsyncOperationCompletedHandler_1__IMidiInPort = ^AsyncOperationCompletedHandler_1__IMidiInPort;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Midi.IMidiInPort>
  IAsyncOperation_1__IMidiInPort = interface;
  PIAsyncOperation_1__IMidiInPort = ^IAsyncOperation_1__IMidiInPort;

  // Windows.Devices.Midi.IMidiInPortStatics
  IMidiInPortStatics = interface;
  PIMidiInPortStatics = ^IMidiInPortStatics;

  // Windows.Devices.Midi.IMidiNoteOffMessage
  IMidiNoteOffMessage = interface;
  PIMidiNoteOffMessage = ^IMidiNoteOffMessage;

  // Windows.Devices.Midi.IMidiNoteOffMessageFactory
  IMidiNoteOffMessageFactory = interface;
  PIMidiNoteOffMessageFactory = ^IMidiNoteOffMessageFactory;

  // Windows.Devices.Midi.IMidiNoteOnMessage
  IMidiNoteOnMessage = interface;
  PIMidiNoteOnMessage = ^IMidiNoteOnMessage;

  // Windows.Devices.Midi.IMidiNoteOnMessageFactory
  IMidiNoteOnMessageFactory = interface;
  PIMidiNoteOnMessageFactory = ^IMidiNoteOnMessageFactory;

  // Windows.Devices.Midi.IMidiOutPort
  IMidiOutPort = interface;
  PIMidiOutPort = ^IMidiOutPort;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Midi.IMidiOutPort>
  AsyncOperationCompletedHandler_1__IMidiOutPort = interface;
  PAsyncOperationCompletedHandler_1__IMidiOutPort = ^AsyncOperationCompletedHandler_1__IMidiOutPort;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Midi.IMidiOutPort>
  IAsyncOperation_1__IMidiOutPort = interface;
  PIAsyncOperation_1__IMidiOutPort = ^IAsyncOperation_1__IMidiOutPort;

  // Windows.Devices.Midi.IMidiOutPortStatics
  IMidiOutPortStatics = interface;
  PIMidiOutPortStatics = ^IMidiOutPortStatics;

  // Windows.Devices.Midi.IMidiPitchBendChangeMessage
  IMidiPitchBendChangeMessage = interface;
  PIMidiPitchBendChangeMessage = ^IMidiPitchBendChangeMessage;

  // Windows.Devices.Midi.IMidiPitchBendChangeMessageFactory
  IMidiPitchBendChangeMessageFactory = interface;
  PIMidiPitchBendChangeMessageFactory = ^IMidiPitchBendChangeMessageFactory;

  // Windows.Devices.Midi.IMidiPolyphonicKeyPressureMessage
  IMidiPolyphonicKeyPressureMessage = interface;
  PIMidiPolyphonicKeyPressureMessage = ^IMidiPolyphonicKeyPressureMessage;

  // Windows.Devices.Midi.IMidiPolyphonicKeyPressureMessageFactory
  IMidiPolyphonicKeyPressureMessageFactory = interface;
  PIMidiPolyphonicKeyPressureMessageFactory = ^IMidiPolyphonicKeyPressureMessageFactory;

  // Windows.Devices.Midi.IMidiProgramChangeMessage
  IMidiProgramChangeMessage = interface;
  PIMidiProgramChangeMessage = ^IMidiProgramChangeMessage;

  // Windows.Devices.Midi.IMidiProgramChangeMessageFactory
  IMidiProgramChangeMessageFactory = interface;
  PIMidiProgramChangeMessageFactory = ^IMidiProgramChangeMessageFactory;

  // Windows.Devices.Midi.IMidiSongPositionPointerMessage
  IMidiSongPositionPointerMessage = interface;
  PIMidiSongPositionPointerMessage = ^IMidiSongPositionPointerMessage;

  // Windows.Devices.Midi.IMidiSongPositionPointerMessageFactory
  IMidiSongPositionPointerMessageFactory = interface;
  PIMidiSongPositionPointerMessageFactory = ^IMidiSongPositionPointerMessageFactory;

  // Windows.Devices.Midi.IMidiSongSelectMessage
  IMidiSongSelectMessage = interface;
  PIMidiSongSelectMessage = ^IMidiSongSelectMessage;

  // Windows.Devices.Midi.IMidiSongSelectMessageFactory
  IMidiSongSelectMessageFactory = interface;
  PIMidiSongSelectMessageFactory = ^IMidiSongSelectMessageFactory;

  // Windows.Devices.Midi.IMidiSynthesizer
  IMidiSynthesizer = interface;
  PIMidiSynthesizer = ^IMidiSynthesizer;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Midi.IMidiSynthesizer>
  AsyncOperationCompletedHandler_1__IMidiSynthesizer = interface;
  PAsyncOperationCompletedHandler_1__IMidiSynthesizer = ^AsyncOperationCompletedHandler_1__IMidiSynthesizer;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Midi.IMidiSynthesizer>
  IAsyncOperation_1__IMidiSynthesizer = interface;
  PIAsyncOperation_1__IMidiSynthesizer = ^IAsyncOperation_1__IMidiSynthesizer;

  // Windows.Devices.Midi.IMidiSynthesizerStatics
  IMidiSynthesizerStatics = interface;
  PIMidiSynthesizerStatics = ^IMidiSynthesizerStatics;

  // Windows.Devices.Midi.IMidiSystemExclusiveMessageFactory
  IMidiSystemExclusiveMessageFactory = interface;
  PIMidiSystemExclusiveMessageFactory = ^IMidiSystemExclusiveMessageFactory;

  // Windows.Devices.Midi.IMidiTimeCodeMessage
  IMidiTimeCodeMessage = interface;
  PIMidiTimeCodeMessage = ^IMidiTimeCodeMessage;

  // Windows.Devices.Midi.IMidiTimeCodeMessageFactory
  IMidiTimeCodeMessageFactory = interface;
  PIMidiTimeCodeMessageFactory = ^IMidiTimeCodeMessageFactory;

  // Windows.Devices.Midi Enums

  // Windows.Devices.Midi.MidiMessageType
  MidiMessageType = (
    None = 0,
    NoteOff = 128,
    NoteOn = 144,
    PolyphonicKeyPressure = 160,
    ControlChange = 176,
    ProgramChange = 192,
    ChannelPressure = 208,
    PitchBendChange = 224,
    SystemExclusive = 240,
    MidiTimeCode = 241,
    SongPositionPointer = 242,
    SongSelect = 243,
    TuneRequest = 246,
    EndSystemExclusive = 247,
    TimingClock = 248,
    Start = 250,
    Continue = 251,
    Stop = 252,
    ActiveSensing = 254,
    SystemReset = 255
  );
  PMidiMessageType = ^MidiMessageType;

  // Windows.Devices.Midi Interfaces

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiMessage
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiTuneRequestMessage)]
  IMidiMessage = interface(IInspectable)
  ['{79767945-1094-4283-9BE0-289FC0EE8334}']
    function get_Timestamp: TimeSpan; safecall;
    function get_RawData: IBuffer; safecall;
    function get_Type: MidiMessageType; safecall;
    property RawData: IBuffer read get_RawData;
    property Timestamp: TimeSpan read get_Timestamp;
    property &Type: MidiMessageType read get_Type;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiChannelPressureMessage
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiChannelPressureMessage)]
  IMidiChannelPressureMessage = interface(IInspectable)
  ['{BE1FA860-62B4-4D52-A37E-92E54D35B909}']
    function get_Channel: Byte; safecall;
    function get_Pressure: Byte; safecall;
    property Channel: Byte read get_Channel;
    property Pressure: Byte read get_Pressure;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiChannelPressureMessageFactory
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiChannelPressureMessage)]
  IMidiChannelPressureMessageFactory = interface(IInspectable)
  ['{6218ED2F-2284-412A-94CF-10FB04842C6C}']
    function CreateMidiChannelPressureMessage(channel: Byte; pressure: Byte): IMidiChannelPressureMessage; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiControlChangeMessage
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiControlChangeMessage)]
  IMidiControlChangeMessage = interface(IInspectable)
  ['{B7E15F83-780D-405F-B781-3E1598C97F40}']
    function get_Channel: Byte; safecall;
    function get_Controller: Byte; safecall;
    function get_ControlValue: Byte; safecall;
    property Channel: Byte read get_Channel;
    property ControlValue: Byte read get_ControlValue;
    property Controller: Byte read get_Controller;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiControlChangeMessageFactory
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiControlChangeMessage)]
  IMidiControlChangeMessageFactory = interface(IInspectable)
  ['{2AB14321-956C-46AD-9752-F87F55052FE3}']
    function CreateMidiControlChangeMessage(channel: Byte; controller: Byte; controlValue: Byte): IMidiControlChangeMessage; safecall;
  end;

  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiMessageReceivedEventArgs
  IMidiMessageReceivedEventArgs = interface(IInspectable)
  ['{76566E56-F328-4B51-907D-B3A8CE96BF80}']
    function get_Message: IMidiMessage; safecall;
    property &Message: IMidiMessage read get_Message;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Midi.IMidiInPort,Windows.Devices.Midi.IMidiMessageReceivedEventArgs>
  TypedEventHandler_2__IMidiInPort__IMidiMessageReceivedEventArgs_Delegate_Base = interface(IUnknown)
  ['{50017240-CC39-5775-8A6B-F6F22386BFCA}']
    procedure Invoke(sender: IMidiInPort; args: IMidiMessageReceivedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Midi.IMidiInPort,Windows.Devices.Midi.IMidiMessageReceivedEventArgs>
  TypedEventHandler_2__IMidiInPort__IMidiMessageReceivedEventArgs = interface(TypedEventHandler_2__IMidiInPort__IMidiMessageReceivedEventArgs_Delegate_Base)
  ['{405CE118-C844-59EB-91B5-61568095BA74}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiInPort
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiInPort)]
  IMidiInPort = interface(IInspectable)
  ['{D5C1D9DB-971A-4EAF-A23D-EA19FE607FF9}']
    function add_MessageReceived(handler: TypedEventHandler_2__IMidiInPort__IMidiMessageReceivedEventArgs): EventRegistrationToken; safecall;
    procedure remove_MessageReceived(token: EventRegistrationToken); safecall;
    function get_DeviceId: HSTRING; safecall;
    property DeviceId: HSTRING read get_DeviceId;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Midi.IMidiInPort>
  AsyncOperationCompletedHandler_1__IMidiInPort_Delegate_Base = interface(IUnknown)
  ['{6C090FB2-8099-558F-8A92-9A8EA806E6FB}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMidiInPort; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Midi.IMidiInPort>
  AsyncOperationCompletedHandler_1__IMidiInPort = interface(AsyncOperationCompletedHandler_1__IMidiInPort_Delegate_Base)
  ['{C57FE1CC-0C94-540B-8223-7E4F273FD7F2}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Midi.IMidiInPort>
  IAsyncOperation_1__IMidiInPort_Base = interface(IInspectable)
  ['{CC664F0E-EDB1-55C8-9EF7-EC9007E4561C}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMidiInPort); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMidiInPort; safecall;
    function GetResults: IMidiInPort; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMidiInPort read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Midi.IMidiInPort>
  IAsyncOperation_1__IMidiInPort = interface(IAsyncOperation_1__IMidiInPort_Base)
  ['{18E03B23-441E-5EDE-8E92-A457ABECF3DD}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiInPortStatics
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiInPort)]
  IMidiInPortStatics = interface(IInspectable)
  ['{44C439DC-67FF-4A6E-8BAC-FDB6610CF296}']
    function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IMidiInPort; safecall;
    function GetDeviceSelector: HSTRING; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiNoteOffMessage
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiNoteOffMessage)]
  IMidiNoteOffMessage = interface(IInspectable)
  ['{16FD8AF4-198E-4D8F-A654-D305A293548F}']
    function get_Channel: Byte; safecall;
    function get_Note: Byte; safecall;
    function get_Velocity: Byte; safecall;
    property Channel: Byte read get_Channel;
    property Note: Byte read get_Note;
    property Velocity: Byte read get_Velocity;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiNoteOffMessageFactory
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiNoteOffMessage)]
  IMidiNoteOffMessageFactory = interface(IInspectable)
  ['{A6B240E0-A749-425F-8AF4-A4D979CC15B5}']
    function CreateMidiNoteOffMessage(channel: Byte; note: Byte; velocity: Byte): IMidiNoteOffMessage; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiNoteOnMessage
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiNoteOnMessage)]
  IMidiNoteOnMessage = interface(IInspectable)
  ['{E0224AF5-6181-46DD-AFA2-410004C057AA}']
    function get_Channel: Byte; safecall;
    function get_Note: Byte; safecall;
    function get_Velocity: Byte; safecall;
    property Channel: Byte read get_Channel;
    property Note: Byte read get_Note;
    property Velocity: Byte read get_Velocity;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiNoteOnMessageFactory
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiNoteOnMessage)]
  IMidiNoteOnMessageFactory = interface(IInspectable)
  ['{9B4280A0-59C1-420E-B517-15A10AA9606B}']
    function CreateMidiNoteOnMessage(channel: Byte; note: Byte; velocity: Byte): IMidiNoteOnMessage; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiOutPort
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiOutPort)]
  IMidiOutPort = interface(IInspectable)
  ['{931D6D9F-57A2-4A3A-ADB8-4640886F6693}']
    procedure SendMessage(midiMessage: IMidiMessage); safecall;
    procedure SendBuffer(midiData: IBuffer); safecall;
    function get_DeviceId: HSTRING; safecall;
    property DeviceId: HSTRING read get_DeviceId;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Midi.IMidiOutPort>
  AsyncOperationCompletedHandler_1__IMidiOutPort_Delegate_Base = interface(IUnknown)
  ['{EED37805-2A49-59B4-B4D4-1188C6819122}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMidiOutPort; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Midi.IMidiOutPort>
  AsyncOperationCompletedHandler_1__IMidiOutPort = interface(AsyncOperationCompletedHandler_1__IMidiOutPort_Delegate_Base)
  ['{EED37805-2A49-59B4-B4D4-1188C6819122}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Midi.IMidiOutPort>
  IAsyncOperation_1__IMidiOutPort_Base = interface(IInspectable)
  ['{32699A4D-1CC0-5A1C-9DA6-875197875086}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMidiOutPort); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMidiOutPort; safecall;
    function GetResults: IMidiOutPort; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMidiOutPort read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Midi.IMidiOutPort>
  IAsyncOperation_1__IMidiOutPort = interface(IAsyncOperation_1__IMidiOutPort_Base)
  ['{32699A4D-1CC0-5A1C-9DA6-875197875086}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiOutPortStatics
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiOutPort)]
  IMidiOutPortStatics = interface(IInspectable)
  ['{065CC3E9-0F88-448B-9B64-A95826C65B8F}']
    function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IMidiOutPort; safecall;
    function GetDeviceSelector: HSTRING; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiPitchBendChangeMessage
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiPitchBendChangeMessage)]
  IMidiPitchBendChangeMessage = interface(IInspectable)
  ['{29DF4CB1-2E9F-4FAF-8C2B-9CB82A9079CA}']
    function get_Channel: Byte; safecall;
    function get_Bend: Word; safecall;
    property Bend: Word read get_Bend;
    property Channel: Byte read get_Channel;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiPitchBendChangeMessageFactory
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiPitchBendChangeMessage)]
  IMidiPitchBendChangeMessageFactory = interface(IInspectable)
  ['{F5EEDF55-CFC8-4926-B30E-A3622393306C}']
    function CreateMidiPitchBendChangeMessage(channel: Byte; bend: Word): IMidiPitchBendChangeMessage; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiPolyphonicKeyPressureMessage
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiPolyphonicKeyPressureMessage)]
  IMidiPolyphonicKeyPressureMessage = interface(IInspectable)
  ['{1F7337FE-ACE8-48A0-868E-7CDBF20F04D6}']
    function get_Channel: Byte; safecall;
    function get_Note: Byte; safecall;
    function get_Pressure: Byte; safecall;
    property Channel: Byte read get_Channel;
    property Note: Byte read get_Note;
    property Pressure: Byte read get_Pressure;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiPolyphonicKeyPressureMessageFactory
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiPolyphonicKeyPressureMessage)]
  IMidiPolyphonicKeyPressureMessageFactory = interface(IInspectable)
  ['{E98F483E-C4B3-4DD2-917C-E349815A1B3B}']
    function CreateMidiPolyphonicKeyPressureMessage(channel: Byte; note: Byte; pressure: Byte): IMidiPolyphonicKeyPressureMessage; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiProgramChangeMessage
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiProgramChangeMessage)]
  IMidiProgramChangeMessage = interface(IInspectable)
  ['{9CBB3C78-7A3E-4327-AA98-20B8E4485AF8}']
    function get_Channel: Byte; safecall;
    function get_Program: Byte; safecall;
    property Channel: Byte read get_Channel;
    property &Program: Byte read get_Program;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiProgramChangeMessageFactory
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiProgramChangeMessage)]
  IMidiProgramChangeMessageFactory = interface(IInspectable)
  ['{D6B04387-524B-4104-9C99-6572BFD2E261}']
    function CreateMidiProgramChangeMessage(channel: Byte; &program: Byte): IMidiProgramChangeMessage; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiSongPositionPointerMessage
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiSongPositionPointerMessage)]
  IMidiSongPositionPointerMessage = interface(IInspectable)
  ['{4CA50C56-EC5E-4AE4-A115-88DC57CC2B79}']
    function get_Beats: Word; safecall;
    property Beats: Word read get_Beats;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiSongPositionPointerMessageFactory
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiSongPositionPointerMessage)]
  IMidiSongPositionPointerMessageFactory = interface(IInspectable)
  ['{9C00E996-F10B-4FEA-B395-F5D6CF80F64E}']
    function CreateMidiSongPositionPointerMessage(beats: Word): IMidiSongPositionPointerMessage; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiSongSelectMessage
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiSongSelectMessage)]
  IMidiSongSelectMessage = interface(IInspectable)
  ['{49F0F27F-6D83-4741-A5BF-4629F6BE974F}']
    function get_Song: Byte; safecall;
    property Song: Byte read get_Song;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiSongSelectMessageFactory
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiSongSelectMessage)]
  IMidiSongSelectMessageFactory = interface(IInspectable)
  ['{848878E4-8748-4129-A66C-A05493F75DAA}']
    function CreateMidiSongSelectMessage(song: Byte): IMidiSongSelectMessage; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiSynthesizer
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiSynthesizer)]
  IMidiSynthesizer = interface(IInspectable)
  ['{F0DA155E-DB90-405F-B8AE-21D2E17F2E45}']
    function get_AudioDevice: IDeviceInformation; safecall;
    function get_Volume: Double; safecall;
    procedure put_Volume(value: Double); safecall;
    property AudioDevice: IDeviceInformation read get_AudioDevice;
    property Volume: Double read get_Volume write put_Volume;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Midi.IMidiSynthesizer>
  AsyncOperationCompletedHandler_1__IMidiSynthesizer_Delegate_Base = interface(IUnknown)
  ['{5D716335-D087-516F-AD0A-63F61CBCF342}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMidiSynthesizer; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Midi.IMidiSynthesizer>
  AsyncOperationCompletedHandler_1__IMidiSynthesizer = interface(AsyncOperationCompletedHandler_1__IMidiSynthesizer_Delegate_Base)
  ['{D26AED51-C3FB-55A7-B983-E077BFC95748}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Midi.IMidiSynthesizer>
  IAsyncOperation_1__IMidiSynthesizer_Base = interface(IInspectable)
  ['{9388B978-13F1-5E37-8133-94430D90DD50}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMidiSynthesizer); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMidiSynthesizer; safecall;
    function GetResults: IMidiSynthesizer; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMidiSynthesizer read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Midi.IMidiSynthesizer>
  IAsyncOperation_1__IMidiSynthesizer = interface(IAsyncOperation_1__IMidiSynthesizer_Base)
  ['{6CF9602B-DAAF-568D-AEAE-24B0B87A64E1}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiSynthesizerStatics
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiSynthesizer)]
  IMidiSynthesizerStatics = interface(IInspectable)
  ['{4224EAA8-6629-4D6B-AA8F-D4521A5A31CE}']
    function CreateAsync: IAsyncOperation_1__IMidiSynthesizer; overload; safecall;
    function CreateAsync(audioDevice: IDeviceInformation): IAsyncOperation_1__IMidiSynthesizer; overload; safecall;
    function IsSynthesizer(midiDevice: IDeviceInformation): Boolean; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiSystemExclusiveMessageFactory
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiSystemExclusiveMessage)]
  IMidiSystemExclusiveMessageFactory = interface(IInspectable)
  ['{083DE222-3B74-4320-9B42-0CA8545F8A24}']
    function CreateMidiSystemExclusiveMessage(rawData: IBuffer): IMidiMessage; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiTimeCodeMessage
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiTimeCodeMessage)]
  IMidiTimeCodeMessage = interface(IInspectable)
  ['{0BF7087D-FA63-4A1C-8DEB-C0E87796A6D7}']
    function get_FrameType: Byte; safecall;
    function get_Values: Byte; safecall;
    property FrameType: Byte read get_FrameType;
    property Values: Byte read get_Values;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Midi.IMidiTimeCodeMessageFactory
  [WinRTClassNameAttribute(SWindows_Devices_Midi_MidiTimeCodeMessage)]
  IMidiTimeCodeMessageFactory = interface(IInspectable)
  ['{EB3099C5-771C-40DE-B961-175A7489A85E}']
    function CreateMidiTimeCodeMessage(frameType: Byte; values: Byte): IMidiTimeCodeMessage; safecall;
  end;

  // Windows.Devices.Midi.MidiActiveSensingMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Instantiable: "IMidiMessage"
  TMidiActiveSensingMessage = class(TWinRTGenericImportI<IMidiMessage>) end;

  // Windows.Devices.Midi.MidiChannelPressureMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiChannelPressureMessage
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Factory: "Windows.Devices.Midi.IMidiChannelPressureMessageFactory"
  TMidiChannelPressureMessage = class(TWinRTGenericImportF<IMidiChannelPressureMessageFactory>)
  public
    // -> IMidiChannelPressureMessageFactory
    class function CreateMidiChannelPressureMessage(channel: Byte; pressure: Byte): IMidiChannelPressureMessage; static; inline;
  end;

  // Windows.Devices.Midi.MidiContinueMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Instantiable: "IMidiMessage"
  TMidiContinueMessage = class(TWinRTGenericImportI<IMidiMessage>) end;

  // Windows.Devices.Midi.MidiControlChangeMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiControlChangeMessage
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Factory: "Windows.Devices.Midi.IMidiControlChangeMessageFactory"
  TMidiControlChangeMessage = class(TWinRTGenericImportF<IMidiControlChangeMessageFactory>)
  public
    // -> IMidiControlChangeMessageFactory
    class function CreateMidiControlChangeMessage(channel: Byte; controller: Byte; controlValue: Byte): IMidiControlChangeMessage; static; inline;
  end;

  // Windows.Devices.Midi.MidiInPort
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiInPort
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.Devices.Midi.IMidiInPortStatics"
  TMidiInPort = class(TWinRTGenericImportS<IMidiInPortStatics>)
  public
    // -> IMidiInPortStatics
    class function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IMidiInPort; static; inline;
    class function GetDeviceSelector: HSTRING; static; inline;
  end;

  // Windows.Devices.Midi.MidiNoteOffMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiNoteOffMessage
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Factory: "Windows.Devices.Midi.IMidiNoteOffMessageFactory"
  TMidiNoteOffMessage = class(TWinRTGenericImportF<IMidiNoteOffMessageFactory>)
  public
    // -> IMidiNoteOffMessageFactory
    class function CreateMidiNoteOffMessage(channel: Byte; note: Byte; velocity: Byte): IMidiNoteOffMessage; static; inline;
  end;

  // Windows.Devices.Midi.MidiNoteOnMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiNoteOnMessage
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Factory: "Windows.Devices.Midi.IMidiNoteOnMessageFactory"
  TMidiNoteOnMessage = class(TWinRTGenericImportF<IMidiNoteOnMessageFactory>)
  public
    // -> IMidiNoteOnMessageFactory
    class function CreateMidiNoteOnMessage(channel: Byte; note: Byte; velocity: Byte): IMidiNoteOnMessage; static; inline;
  end;

  // Windows.Devices.Midi.MidiOutPort
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiOutPort
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.Devices.Midi.IMidiOutPortStatics"
  TMidiOutPort = class(TWinRTGenericImportS<IMidiOutPortStatics>)
  public
    // -> IMidiOutPortStatics
    class function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IMidiOutPort; static; inline;
    class function GetDeviceSelector: HSTRING; static; inline;
  end;

  // Windows.Devices.Midi.MidiPitchBendChangeMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiPitchBendChangeMessage
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Factory: "Windows.Devices.Midi.IMidiPitchBendChangeMessageFactory"
  TMidiPitchBendChangeMessage = class(TWinRTGenericImportF<IMidiPitchBendChangeMessageFactory>)
  public
    // -> IMidiPitchBendChangeMessageFactory
    class function CreateMidiPitchBendChangeMessage(channel: Byte; bend: Word): IMidiPitchBendChangeMessage; static; inline;
  end;

  // Windows.Devices.Midi.MidiPolyphonicKeyPressureMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiPolyphonicKeyPressureMessage
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Factory: "Windows.Devices.Midi.IMidiPolyphonicKeyPressureMessageFactory"
  TMidiPolyphonicKeyPressureMessage = class(TWinRTGenericImportF<IMidiPolyphonicKeyPressureMessageFactory>)
  public
    // -> IMidiPolyphonicKeyPressureMessageFactory
    class function CreateMidiPolyphonicKeyPressureMessage(channel: Byte; note: Byte; pressure: Byte): IMidiPolyphonicKeyPressureMessage; static; inline;
  end;

  // Windows.Devices.Midi.MidiProgramChangeMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiProgramChangeMessage
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Factory: "Windows.Devices.Midi.IMidiProgramChangeMessageFactory"
  TMidiProgramChangeMessage = class(TWinRTGenericImportF<IMidiProgramChangeMessageFactory>)
  public
    // -> IMidiProgramChangeMessageFactory
    class function CreateMidiProgramChangeMessage(channel: Byte; &program: Byte): IMidiProgramChangeMessage; static; inline;
  end;

  // Windows.Devices.Midi.MidiSongPositionPointerMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiSongPositionPointerMessage
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Factory: "Windows.Devices.Midi.IMidiSongPositionPointerMessageFactory"
  TMidiSongPositionPointerMessage = class(TWinRTGenericImportF<IMidiSongPositionPointerMessageFactory>)
  public
    // -> IMidiSongPositionPointerMessageFactory
    class function CreateMidiSongPositionPointerMessage(beats: Word): IMidiSongPositionPointerMessage; static; inline;
  end;

  // Windows.Devices.Midi.MidiSongSelectMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiSongSelectMessage
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Factory: "Windows.Devices.Midi.IMidiSongSelectMessageFactory"
  TMidiSongSelectMessage = class(TWinRTGenericImportF<IMidiSongSelectMessageFactory>)
  public
    // -> IMidiSongSelectMessageFactory
    class function CreateMidiSongSelectMessage(song: Byte): IMidiSongSelectMessage; static; inline;
  end;

  // Windows.Devices.Midi.MidiStartMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Instantiable: "IMidiMessage"
  TMidiStartMessage = class(TWinRTGenericImportI<IMidiMessage>) end;

  // Windows.Devices.Midi.MidiStopMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Instantiable: "IMidiMessage"
  TMidiStopMessage = class(TWinRTGenericImportI<IMidiMessage>) end;

  // Windows.Devices.Midi.MidiSynthesizer
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiSynthesizer
  // Implements: Windows.Devices.Midi.IMidiOutPort
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.Devices.Midi.IMidiSynthesizerStatics"
  TMidiSynthesizer = class(TWinRTGenericImportS<IMidiSynthesizerStatics>)
  public
    // -> IMidiSynthesizerStatics
    class function CreateAsync: IAsyncOperation_1__IMidiSynthesizer; overload; static; inline;
    class function CreateAsync(audioDevice: IDeviceInformation): IAsyncOperation_1__IMidiSynthesizer; overload; static; inline;
    class function IsSynthesizer(midiDevice: IDeviceInformation): Boolean; static; inline;
  end;

  // Windows.Devices.Midi.MidiSystemExclusiveMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Factory: "Windows.Devices.Midi.IMidiSystemExclusiveMessageFactory"
  TMidiSystemExclusiveMessage = class(TWinRTGenericImportF<IMidiSystemExclusiveMessageFactory>)
  public
    // -> IMidiSystemExclusiveMessageFactory
    class function CreateMidiSystemExclusiveMessage(rawData: IBuffer): IMidiMessage; static; inline;
  end;

  // Windows.Devices.Midi.MidiSystemResetMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Instantiable: "IMidiMessage"
  TMidiSystemResetMessage = class(TWinRTGenericImportI<IMidiMessage>) end;

  // Windows.Devices.Midi.MidiTimeCodeMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiTimeCodeMessage
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Factory: "Windows.Devices.Midi.IMidiTimeCodeMessageFactory"
  TMidiTimeCodeMessage = class(TWinRTGenericImportF<IMidiTimeCodeMessageFactory>)
  public
    // -> IMidiTimeCodeMessageFactory
    class function CreateMidiTimeCodeMessage(frameType: Byte; values: Byte): IMidiTimeCodeMessage; static; inline;
  end;

  // Windows.Devices.Midi.MidiTimingClockMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Instantiable: "IMidiMessage"
  TMidiTimingClockMessage = class(TWinRTGenericImportI<IMidiMessage>) end;

  // Windows.Devices.Midi.MidiTuneRequestMessage
  // DualAPI
  // Implements: Windows.Devices.Midi.IMidiMessage
  // Instantiable: "IMidiMessage"
  TMidiTuneRequestMessage = class(TWinRTGenericImportI<IMidiMessage>) end;

implementation

{ TMidiActiveSensingMessage }

{ TMidiChannelPressureMessage }
// Factories for : "MidiChannelPressureMessage"
// Factory: "Windows.Devices.Midi.IMidiChannelPressureMessageFactory"
// -> IMidiChannelPressureMessageFactory

class function TMidiChannelPressureMessage.CreateMidiChannelPressureMessage(channel: Byte; pressure: Byte): IMidiChannelPressureMessage;
begin
  Result := Factory.CreateMidiChannelPressureMessage(channel, pressure);
end;


{ TMidiContinueMessage }

{ TMidiControlChangeMessage }
// Factories for : "MidiControlChangeMessage"
// Factory: "Windows.Devices.Midi.IMidiControlChangeMessageFactory"
// -> IMidiControlChangeMessageFactory

class function TMidiControlChangeMessage.CreateMidiControlChangeMessage(channel: Byte; controller: Byte; controlValue: Byte): IMidiControlChangeMessage;
begin
  Result := Factory.CreateMidiControlChangeMessage(channel, controller, controlValue);
end;


{ TMidiInPort }

class function TMidiInPort.FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IMidiInPort;
begin
  Result := Statics.FromIdAsync(deviceId);
end;

class function TMidiInPort.GetDeviceSelector: HSTRING;
begin
  Result := Statics.GetDeviceSelector;
end;


{ TMidiNoteOffMessage }
// Factories for : "MidiNoteOffMessage"
// Factory: "Windows.Devices.Midi.IMidiNoteOffMessageFactory"
// -> IMidiNoteOffMessageFactory

class function TMidiNoteOffMessage.CreateMidiNoteOffMessage(channel: Byte; note: Byte; velocity: Byte): IMidiNoteOffMessage;
begin
  Result := Factory.CreateMidiNoteOffMessage(channel, note, velocity);
end;


{ TMidiNoteOnMessage }
// Factories for : "MidiNoteOnMessage"
// Factory: "Windows.Devices.Midi.IMidiNoteOnMessageFactory"
// -> IMidiNoteOnMessageFactory

class function TMidiNoteOnMessage.CreateMidiNoteOnMessage(channel: Byte; note: Byte; velocity: Byte): IMidiNoteOnMessage;
begin
  Result := Factory.CreateMidiNoteOnMessage(channel, note, velocity);
end;


{ TMidiOutPort }

class function TMidiOutPort.FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IMidiOutPort;
begin
  Result := Statics.FromIdAsync(deviceId);
end;

class function TMidiOutPort.GetDeviceSelector: HSTRING;
begin
  Result := Statics.GetDeviceSelector;
end;


{ TMidiPitchBendChangeMessage }
// Factories for : "MidiPitchBendChangeMessage"
// Factory: "Windows.Devices.Midi.IMidiPitchBendChangeMessageFactory"
// -> IMidiPitchBendChangeMessageFactory

class function TMidiPitchBendChangeMessage.CreateMidiPitchBendChangeMessage(channel: Byte; bend: Word): IMidiPitchBendChangeMessage;
begin
  Result := Factory.CreateMidiPitchBendChangeMessage(channel, bend);
end;


{ TMidiPolyphonicKeyPressureMessage }
// Factories for : "MidiPolyphonicKeyPressureMessage"
// Factory: "Windows.Devices.Midi.IMidiPolyphonicKeyPressureMessageFactory"
// -> IMidiPolyphonicKeyPressureMessageFactory

class function TMidiPolyphonicKeyPressureMessage.CreateMidiPolyphonicKeyPressureMessage(channel: Byte; note: Byte; pressure: Byte): IMidiPolyphonicKeyPressureMessage;
begin
  Result := Factory.CreateMidiPolyphonicKeyPressureMessage(channel, note, pressure);
end;


{ TMidiProgramChangeMessage }
// Factories for : "MidiProgramChangeMessage"
// Factory: "Windows.Devices.Midi.IMidiProgramChangeMessageFactory"
// -> IMidiProgramChangeMessageFactory

class function TMidiProgramChangeMessage.CreateMidiProgramChangeMessage(channel: Byte; &program: Byte): IMidiProgramChangeMessage;
begin
  Result := Factory.CreateMidiProgramChangeMessage(channel, &program);
end;


{ TMidiSongPositionPointerMessage }
// Factories for : "MidiSongPositionPointerMessage"
// Factory: "Windows.Devices.Midi.IMidiSongPositionPointerMessageFactory"
// -> IMidiSongPositionPointerMessageFactory

class function TMidiSongPositionPointerMessage.CreateMidiSongPositionPointerMessage(beats: Word): IMidiSongPositionPointerMessage;
begin
  Result := Factory.CreateMidiSongPositionPointerMessage(beats);
end;


{ TMidiSongSelectMessage }
// Factories for : "MidiSongSelectMessage"
// Factory: "Windows.Devices.Midi.IMidiSongSelectMessageFactory"
// -> IMidiSongSelectMessageFactory

class function TMidiSongSelectMessage.CreateMidiSongSelectMessage(song: Byte): IMidiSongSelectMessage;
begin
  Result := Factory.CreateMidiSongSelectMessage(song);
end;


{ TMidiStartMessage }

{ TMidiStopMessage }

{ TMidiSynthesizer }

class function TMidiSynthesizer.CreateAsync: IAsyncOperation_1__IMidiSynthesizer;
begin
  Result := Statics.CreateAsync;
end;

class function TMidiSynthesizer.CreateAsync(audioDevice: IDeviceInformation): IAsyncOperation_1__IMidiSynthesizer;
begin
  Result := Statics.CreateAsync(audioDevice);
end;

class function TMidiSynthesizer.IsSynthesizer(midiDevice: IDeviceInformation): Boolean;
begin
  Result := Statics.IsSynthesizer(midiDevice);
end;


{ TMidiSystemExclusiveMessage }
// Factories for : "MidiSystemExclusiveMessage"
// Factory: "Windows.Devices.Midi.IMidiSystemExclusiveMessageFactory"
// -> IMidiSystemExclusiveMessageFactory

class function TMidiSystemExclusiveMessage.CreateMidiSystemExclusiveMessage(rawData: IBuffer): IMidiMessage;
begin
  Result := Factory.CreateMidiSystemExclusiveMessage(rawData);
end;


{ TMidiSystemResetMessage }

{ TMidiTimeCodeMessage }
// Factories for : "MidiTimeCodeMessage"
// Factory: "Windows.Devices.Midi.IMidiTimeCodeMessageFactory"
// -> IMidiTimeCodeMessageFactory

class function TMidiTimeCodeMessage.CreateMidiTimeCodeMessage(frameType: Byte; values: Byte): IMidiTimeCodeMessage;
begin
  Result := Factory.CreateMidiTimeCodeMessage(frameType, values);
end;


{ TMidiTimingClockMessage }

{ TMidiTuneRequestMessage }

end.
