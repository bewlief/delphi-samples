?
 TFRMMAIN 0?  TPF0TfrmMainfrmMainLeft? Top? Width<HeightFCaptionBlob Compression DemoColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.Style OldCreateOrderPositionpoScreenCenterOnCreate
FormCreatePixelsPerInch`
TextHeight TDBNavigatorDBNavigator1LeftTopWidth? Height
DataSourceDataSource1TabOrder   TDBGridDBGrid1LeftTop(WidthYHeight? 
DataSourceDataSource1TabOrderTitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.Height?TitleFont.NameMS Sans SerifTitleFont.Style ColumnsExpanded	FieldNameVenueNoVisible	 Expanded	FieldNameVenueVisible	 Expanded	FieldNameCapacityVisible	    	TGroupBox	GroupBox1LeftfTop? Width? HeightICaptionInfo TabOrder TLabelLabel3LeftTopWidth? HeightCaption3This application shows how to compress BLOB fields.WordWrap	   TButtonbtCreateLeftTophWidth9HeightCaption&Create Database with Compressed  BLOBsTabOrderOnClickbtCreateClick  	TGroupBox	GroupBox2LeftTopWidthHeightUCaption"Graphic Field Compression Params: TabOrder TLabelLabel2LeftTopWidthsHeightCaption Compression Algorithm:   TLabelLabel1LeftTop8Width`HeightCaptionCompression Mode:   TLabelLabel4Left? Top8WidthAHeightCaption(1-min, 9-max)  	TComboBoxcbGraphicAlgorithmLeft? TopWidthiHeightStylecsDropDownList
ItemHeightItems.StringsNoneZLIBBZIPPPM TabOrder   	TSpinEditseGraphicModeLeft? Top4Width)HeightMaxValue	MinValueTabOrderValue   	TGroupBox	GroupBox3LeftTopWidthHeightVCaptionMemo Field Compression Params: TabOrder TLabelLabel5LeftTopWidthsHeightCaption Compression Algorithm:   TLabelLabel6LeftTop8Width`HeightCaptionCompression Mode:   TLabelLabel7Left? Top8WidthAHeightCaption(1-min, 9-max)  	TComboBoxcbMemoalgorithmLeft? TopWidthiHeightStylecsDropDownList
ItemHeightItems.StringsNoneZLIBBZIPPPM TabOrder   	TSpinEdit
seMemoModeLeft? Top4Width)HeightMaxValue	MinValueTabOrderValue   	TGroupBox	GroupBox4LeftTop? WidthQHeightnCaptionCompressed Database "Venues" : TabOrder TLabelLabel8LeftTopWidth|HeightCaptionNew Database File Name:  TLabellbNewDbFileNameLeft? TopWidthGHeightCaptionVenuesDB.abs  TLabelLabel9Left	Top1Width? HeightCaption&Source Paradox "Venues" Database Size:  TLabelLabel10Left? Top2Width(HeightCaption1070 Kb  TLabelLabel11Left	TopGWidth? HeightCaption'Compressed Absolute Database File Size:Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabellbVenuesDBSizeLeft? TopGWidth)HeightCaption120 KbFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height?	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFont   TDBImageDBImage1LeftkTop(Width? Height? 	DataField	Venue_Map
DataSourceDataSource1Stretch	TabOrder  TDBMemoDBMemo1LeftmTop?Width? HeightA	DataFieldRemarks
DataSourceDataSource1
ScrollBars
ssVerticalTabOrder  TABSDatabasedbDemosCurrentVersion4.76 DatabaseFileName..\..\Data\Demos.absDatabaseNamedemos	ExclusiveMaxConnections	MultiUserSessionNameDefaultLeft`Tope  	TABSTabletVenuesCurrentVersion4.76 DatabaseNamedemosInMemoryReadOnly	TableNamevenues	ExclusiveLeft?Tope TAutoIncFieldtVenuesVenueNo	FieldNameVenueNo  TStringFieldtVenuesVenue	FieldNameVenueSize  TIntegerFieldtVenuesCapacity	FieldNameCapacity  TGraphicFieldtVenuesVenue_Map	FieldName	Venue_MapBlobType	ftGraphic  
TMemoFieldtVenuesRemarks	FieldNameRemarksBlobTypeftMemo   TDataSourceDataSource1DataSet
tNewVenuesLeft? Toph  TABSDatabaseVenuesDBCurrentVersion4.76 DatabaseFileNameVenuesDB.absDatabaseNameVenuesDB	ExclusiveMaxConnections?	MultiUserSessionNameDefaultLeftXToph  	TABSTable
tNewVenuesCurrentVersion4.76 DatabaseNameVenuesDBInMemoryReadOnly	TableNamevenues	ExclusiveLeftxToph   