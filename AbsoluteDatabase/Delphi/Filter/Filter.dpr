program Filter;

uses
  Forms,
  CustView in 'CustView.pas' {fmCustView},
  Filter1 in 'Filter1.pas' {fmFilterFrm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmCustView, fmCustView);
  Application.CreateForm(TfmFilterFrm, fmFilterFrm);
  Application.Run;
end.
