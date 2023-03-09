{*******************************************************}
{                                                       }
{             Delphi LiveBindings Framework             }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{$HPPEMIT LINKUNIT}
unit Vcl.Samples.Bind.Editors;

interface

uses
  Data.Bind.Components, Vcl.Bind.Consts;

implementation

uses
  Vcl.Samples.Spin;

initialization
  Data.Bind.Components.RegisterObservableMember(TArray<TClass>.Create(
    TSpinEdit
    ),
    'Value', sDfm);

  Data.Bind.Components.RegisterObservableMemberOptions(TArray<TClass>.Create(
    TSpinEdit
    ), [moTrack]);

finalization
  Data.Bind.Components.UnRegisterObservableMember(TArray<TClass>.Create(
    TSpinEdit
  ));

  Data.Bind.Components.UnRegisterObservableMemberOptions(TArray<TClass>.Create(
    TSpinEdit));

end.
