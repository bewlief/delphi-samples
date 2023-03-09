{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit PlatformConst;

interface

uses System.Generics.Collections;

const
  { Universal platform ids }
  // If you add new platform ID, you have to update
  //  app\delphicore\PasMgr.pas
  //
  cWin32Platform = 'Win32';
  cWinNX32Platform = 'WinNX32';
  cWinIoT32Platform = 'WinIoT32';
  cWinArm32Platform = 'WinARM32';
  cWinArmPlatform = cWinArm32Platform deprecated 'use cWinArm32Platform';
  cOSX32Platform = 'OSX32';
  cWin64Platform = 'Win64';
  cLinux32Platform = 'Linux32';
  cAndroidArm32Platform = 'Android';
  cAndroid32ArmPlatform = cAndroidArm32Platform deprecated 'use cAndroidArm32Platform';
  cAndroidPlatform = cAndroidArm32Platform deprecated 'use cAndroidArm32Platform';
  ciOSSimulator32Platform = 'iOSSimulator';
  ciOSSimulator64Platform = 'iOSSimulator64';
  ciOSSimulatorPlatform = ciOSSimulator32Platform deprecated 'use ciOSSimulator32Platform';
  ciOSDevice32Platform = 'iOSDevice32';
  ciOSDevice64Platform = 'iOSDevice64';
  ciOSDevicePlatform = ciOSDevice32Platform deprecated 'use ciOSDevice32Platform';
  cLinux64Platform = 'Linux64';
  cOSX64Platform = 'OSX64';
  cLinuxArm32Platform = 'LinuxARM32';
  cLinuxArm64Platform = 'LinuxARM64';
  cAndroidArm64Platform = 'Android64';
  cAndroid64ArmPlatform = cAndroidArm64Platform deprecated 'use cAndroidArm64Platform';

  cOSXArm64Platform = 'OSXARM64';
  cWinArm64Platform = 'WinARM64';
  ciOSSimulatorArm64Platform = 'iOSSimARM64';

  cUndefinedFamilyName = 'Unknown';
  cWindowsFamilyName = 'Windows';
  cOSXFamilyName = 'macOS';
  ciOSFamilyName = 'iOS';
  cAndroidFamilyName = 'Android';
  cLinuxFamilyName = 'Linux';

function GetAllPlatforms: TArray<string>;

implementation

function GetAllPlatforms: TArray<string>;
begin
  // Dont' add duplicated platforms constants like ciOSDevicePlatform here
  Result := TArray<string>.Create(cWin32Platform, cWinNX32Platform,
    cWinIoT32Platform, cWinArm32Platform, cWin64Platform,
    cOSX32Platform, cOSX64Platform, cOSXArm64Platform, 
    cLinux32Platform, cLinuxArm32Platform, cLinux64Platform, cLinuxArm64Platform, 
    cAndroidArm32Platform, cAndroidArm64Platform, 
    ciOSDevice32Platform, ciOSDevice64Platform, 
    ciOSSimulator32Platform, ciOSSimulator64Platform, ciOSSimulatorArm64Platform);
end;

end.
