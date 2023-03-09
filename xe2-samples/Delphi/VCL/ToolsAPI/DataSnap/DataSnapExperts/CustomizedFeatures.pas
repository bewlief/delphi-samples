
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit CustomizedFeatures;

interface

uses DSServerFeatures, DSServerFeatureManager;

const

  // Value used in feature page

  cCustomFeatureGroup = TDSServerFeature.dsCustom1;
  cCustomFeatureCommentModule = TDSServerFeature.dsCustom2;
  cCustomFeatureTimeStampModule = TDSServerFeature.dsCustom3;
  cCustomFeatureSampleMethods = TDSServerFeature.dsCustom4;
  cCustomFeatureAddFiles = TDSServerFeature.dsCustom5;

  // Values used in code templates

  // Boolean values
  sCustomFeatureCommentModule = 'Customize_CommentModule';
  sCustomFeatureTimeStamp = 'Customize_TimeStampModule';
  sCustomSampleMethods = 'Customize_IncludeSampleMethods';
  // Text values
  sCommentModuleText = 'Customize_CommentModuleText';
  sTimeStampText = 'Customize_TimeStampText';

function CustomFeatureDescriptions: TArray<TFeatureDescription>;

implementation

uses Generics.Collections;

function CustomFeatureDescriptions: TArray<TFeatureDescription>;
var
  LList: TList<TFeatureDescription>;
  LDescription: TFeatureDescription;
begin
  LList := TList<TFeatureDescription>.Create;
  try
    // Add some more features
    LDescription := TFeatureDescription.Create(
      cCustomFeatureGroup, dsNull, 'Comments (new)', 'Add comments to modules',
         [wtAll]);
    LList.Add(LDescription);
    LDescription := TFeatureDescription.Create(
      cCustomFeatureGroup, cCustomFeatureCommentModule, 'Comment Server Container (new)', 'Add a comment to the server module',
         [wtAll]);
    LList.Add(LDescription);
    LDescription := TFeatureDescription.Create(
      cCustomFeatureGroup, cCustomFeatureTimeStampModule, 'Time Stamps (new)', 'Add a stamp to all modules',
         [wtAll]);
    LList.Add(LDescription);

//    Uncomment to remove TCP protocol
    for LDescription in DefaultFeatureDescriptions do
    begin
//      case LDescription.Group of
//        dsProtocols:
//        begin
//          if LDescription.Feature = dsNull then
//            LList.Add(TFeatureDescription.Create(
//              LDescription.Group, LDescription.Feature,
//              LDescription.Name  + ' (TCP Removed)', // Change text of Protocols group
//              LDescription.Description,
//              LDescription.WizardTypes))
//          else  if LDescription.Feature <> dsTCPProtocol then  // Don't add TCP to
            LList.Add(LDescription);
//        end;
//      else
//        LList.Add(LDescription);
//      end;
    end;
    // Add to an existing group
    LDescription := TFeatureDescription.Create(
      dsServerMethodClass, cCustomFeatureSampleMethods, 'More sample methods (new)', 'Add CustomEchoString, CustomReverseString',
         [wtAll]);
    LList.Add(LDescription);
    // Add new top level check box
    LDescription := TFeatureDescription.Create(
      dsNull, cCustomFeatureAddFiles, 'Add files to the project (new)', 'Choose some files to add to the project',
         [wtWebBrokerRest]);  // Note that this option is REST project only
    LList.Add(LDescription);
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

end.
