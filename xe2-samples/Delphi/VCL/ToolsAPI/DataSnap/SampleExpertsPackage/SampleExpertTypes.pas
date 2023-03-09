
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit SampleExpertTypes;

interface

type
  TSampleApplicationType = (appVCl, appConsole);
  TSampleApplicationTypes = set of TSampleApplicationType;

  TFeature = (feNone, feFiles, feUnit, feForm, feDataModule, feTextFile);
  TFeatures = set of TFeature;

implementation

end.
