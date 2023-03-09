{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2014-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Tether.Consts;

interface

resourcestring
  SProfileAlreadyRegistered = 'Profile "%s" is already registered';
  SProfileNotRegistered = 'Profile "%s" is not registered';
  SProtocolNotRegistered = 'Protocol "%s" is not registered';
  SProfileWithoutAdapters = 'Profile "%s" without registered adapters';
  SProfileWithoutProtocols = 'Profile "%s" without registered protocols';
  SProfileWithoutProtocolAdapters = 'Profile "%s" without registered protocol-adapters';
  SCanNotSetStorage = 'Can not set storage path. Manager already started.';
  SCannotSetAllowedAdapters = 'Cannot change AllowedAdapters, Manager already started.';
  SManagerUDPCreation = 'Error Opening UDP Server';
  SManagerNetworkCreation = 'Error Opening Network Server';
  SManagerBluetoothCreation = 'Error Opening Bluetooth Server';
  SProtocolCreation = 'Error Opening %s Server';

  SInvalidBluetoothTargetFormat = 'Invalid Bluetooth Target format "%s", expected Device$GUID';
  SInvalidNetworkTargetFormat = 'Invalid Network Target format "%s", expected IP$Port';

  SNoConnections = 'No available connections to %s';
  SCanNotGetConnection = 'Can''t get a connection to profile %s';
  SCanNotConnect = 'Can''t connect to profile %s';
  SNoProfile = 'Can''t find profile %s';
  SProfileNotConnected = 'Profile %s is not connected';
  SCanNotSendFile = 'Can''t send file %s';
  SNoResourceList = 'Can''t find resource list for profile %s';
  SRemoteResNotFound = 'Remote resource not found';
  SNoProfileFor = 'Can''t find profile for resource %s';
  SNoProfileForAction = 'Can''t find profile for action %s';
  SNoProfileCommand = 'AppProfile Command not Handled: "%s"';
  SNoManager = 'Manager property not assigned';
  SNoResourceValue = 'No resource value';
  SLocalAction = 'Action %d';
  SCustomLocalAction = 'Custom Local Item %d';
  SLocalResource = 'Resource %d';
  SCanNotSendResource = 'Can''t send resource %s';
  SNoProtocolAndAdapter = 'You need to add a protocol and an adapter connecting to %s';
  SDisabledManager = 'Manager %s is disabled';

  SRemoteResNotPublished = 'The specified remote resource (%s) was not found among the resources published by the specified remote profile (%s).';
  SRemoteActNotPublished = 'The specified remote action (%s) was not found among the actions published by the specified remote profile (%s).';
  SProfileNotFoundRes = 'Could not find a connected (TTetheringAppProfile.Connect) remote profile that published the specified resource (%s).';
  SProfileNotFoundAct = 'Could not find a connected (TTetheringAppProfile.Connect) remote profile that published the specified action (%s).';
  SRemoteProfileNotSpecifiedRes = 'You must connect (TTetheringAppProfile.Connect) to the specified remote profile (%s) before you can request a remote resource published by that remote profile.';
  SRemoteProfileNotSpecifiedAct = 'You must connect (TTetheringAppProfile.Connect) to the specified remote profile (%s) before you can request a remote action published by that remote profile.';

  SActionNameEmpty = 'Action name is empty';
  SResourceNameEmpty = 'Resource name is empty';
  SDuplicateResourceName = 'Duplicate resource name: "%s"';

// Common constants for tethering
const
  TetheringSeparator = '|';
  TetheringCommandSeparator = '~';
  TetheringBlockSeparator: Char = #10; // LF
  TetheringConnectionSeparator = '$';
  TetheringMULTIHOMED = 'MULTIHOMED';


implementation

end.
