�
 TFMCUSTVIEW 0�	  TPF0TfmCustView
fmCustViewLeft� Top� BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionFilter DemoClientHeightPClientWidth�Color	clBtnFace
ParentFont	OldCreateOrder	PositionpoScreenCenterOnCreate
FormCreate
DesignSize�P PixelsPerInch`
TextHeight TSpeedButtonSpeedButton1LefthTop� WidthBHeightCaption< Go >	NumGlyphsOnClickSpeedButton1Click  TLabelLabel1Left	Top� Width� Height	AlignmenttaCenterAutoSizeCaption
UnfilteredFont.CharsetDEFAULT_CHARSET
Font.ColorclMaroonFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFont  TRadioGroup	rgDataSetLeftTopWidth� HeightMCaptionDataset	ItemIndexItems.Strings&Query-Based&Table-Based TabOrderOnClickrgDataSetClick  TDBGridDBGrid1Left� TopWidth	Height� 
DataSourceEmployeeSourceTabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.Height�TitleFont.NameMS Sans SerifTitleFont.Style   TDBGridDBGrid2Left� Top� WidthHeight� 
DataSourceEventsSource1EnabledTabOrderTitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.Height�TitleFont.NameMS Sans SerifTitleFont.Style   TRadioGroupRadioGroup1LeftTopbWidth� HeightUCaptionFilter	ItemIndex Items.StringsUsing Filter propertyUsing OnFilterRecord event TabOrderOnClickRadioGroup1Click  	TGroupBox	GroupBox2LeftTopWidth� HeightCAnchorsakLeftakBottom Caption Info TabOrder TLabelLabel2LeftTopWidthHeight'CaptionGThis sample shows how to filter records via table and query  componentsWordWrap	   TDataSourceEmployeeSourceDataSetEmployeeLeft_Top	  	TABSTableEmployeeCurrentVersion5.05 DatabaseNamedbDemosInMemoryReadOnlyFilter	EmpNo > 5Filtered	OnFilterRecordEmployeeFilterRecord	TableNameemployee	ExclusiveLeft Top  TABSDatabasedbDemosCurrentVersion5.05 DatabaseFileName..\..\Data\Demos.absDatabaseNamedbDemos	ExclusiveMaxConnections�	MultiUserSessionNameDefaultLeftHTop  	TABSQuerySQLEmployeeCurrentVersion5.05 DatabaseNamedbDemosInMemoryReadOnlyFilter	EmpNo = 5Filtered	OnFilterRecordSQLEmployeeFilterRecordSQL.Stringsselect * from employee Left@Top  TDataSourceEventsSource1DataSetEventsTable1LeftxTop�   	TABSTableEventsTable1CurrentVersion5.05 DatabaseNamedbDemosInMemoryReadOnly	TableNameevents	ExclusiveLeft�Top�    