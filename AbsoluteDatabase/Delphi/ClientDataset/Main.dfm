�
 TFORM1 0�
  TPF0TForm1Form1Left� Top� Width�Height�CaptionClientDataset DemoColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderPositionpoScreenCenterOnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel2LeftTophWidth1HeightCaption	Employee:  TLabelLabel1LefthTop`Width(HeightCaption:You can Browse and Insert/Update/Delete Data in the DBGrid  TDBGridDBGrid1LeftTopxWidth�Height� 
DataSource
DataSourceTabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.Height�TitleFont.NameMS Sans SerifTitleFont.Style   TRadioGroup	rgDataSetLeftTopWidth	HeightECaptionEmployee Table Data: 	ItemIndex Items.Strings	Via Table"Via Query:  select * from employee TabOrder  TButton	btnReOpenLeft� Top/Width=HeightCaptionReOpenTabOrderOnClickbtnReOpenClick  	TGroupBox	GroupBox1LeftTop8Width�Height9CaptionInfo: TabOrder TLabelLabel3Left� TopWidthlHeightCaptionOThis demo illustrates how to work with ClientDataset using IProvider interface.   	TGroupBox	GroupBox2LeftTopWidth�HeightECaptionUpdate via Query: TabOrder TEdit
eUpdateSqlLeftTopWidthqHeightTabOrder TextDupdate employee set phoneext=cast(phoneext, integer)+1 where empno=2  TButtonButton2LeftPTop)Width)HeightCaptionExecTabOrderOnClickButton2Click   TButtonButton1Left� TopWidth>HeightCaptionCloseTabOrderOnClickButton1Click  TDBNavigatorDBNavigator1Left�TopXWidth� Height
DataSource
DataSourceTabOrder  TABSDatabasedbDemosCurrentVersion4.76 DatabaseFileName..\..\Data\Demos.absDatabaseNamedemos	ExclusiveMaxConnections	MultiUserSessionNameDefaultLeft(Top�   	TABSTableABSTableCurrentVersion4.76 DatabaseNamedemosInMemoryReadOnly	TableNameemployee	ExclusiveLeftHTop�   	TABSQueryABSQueryCurrentVersion4.76 DatabaseNamedemosInMemoryReadOnlyRequestLive	SQL.Stringsselect * from employee LeftHTop�   TDataSource
DataSourceDataSetClientDataSetLeft� Top�   TDataSetProviderDataSetProviderDataSetABSTableConstraints	LeftpTop�   TClientDataSetClientDataSet
Aggregates Params ProviderNameDataSetProvider	AfterPostApplyUpdatesAfterDeleteApplyUpdatesLeft� Top�   TDataSetProviderDataSetProviderUpdateDataSetABSQueryUpdateConstraints	OptionspoAllowCommandText Left�Top2  TClientDataSetClientDataSetUpdate
Aggregates Params ProviderNameDataSetProviderUpdateLeft�Top2  	TABSQueryABSQueryUpdateCurrentVersion4.76 DatabaseNamedemosInMemoryReadOnlyLeft�Top2   