�
 TFORM1 0  TPF0TForm1Form1Left� Top� WidthHeightdCaptionLastAutoInc DemoColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderPositionpoScreenCenterOnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel1LeftTopWidthHeightCaptionOrders  TLabelLabel2LeftTop� WidthDHeightCaptionOrderPositions  TDBGrid	dbgOrdersLeftTopWidthiHeight� 
DataSourcedsOrdersTabOrder TitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.Height�TitleFont.NameMS Sans SerifTitleFont.Style   TDBGridDBGrid2LeftTop� WidthiHeight� 
DataSourcedsOrderPositionsTabOrderTitleFont.CharsetDEFAULT_CHARSETTitleFont.ColorclWindowTextTitleFont.Height�TitleFont.NameMS Sans SerifTitleFont.Style   	TGroupBox	GroupBox2Left�Top� WidthyHeightACaption Info TabOrder TLabelLabel3LeftTopWidth� HeightCaption6This demo illustrates  how to use LastAutoInc functionWordWrap	   	TGroupBox	GroupBox1Left�TopWidthyHeight� CaptionAdd order with 3 positionsTabOrder TLabelLabel4LeftTop WidthIHeightCaptionCustomer name  TLabelLabel5Left� Top Width2HeightCaption
Order date  TLabellblQuantity1LeftTopXWidth'HeightCaptionQuantity  TLabelLabel10Left TopXWidth.HeightCaption	Product 1  TLabellblProduct2Left TopxWidth.HeightCaption	Product 2  TLabellblProduct3Left Top� Width.HeightCaption	Product 3  TLabellblQuantity2LeftTopxWidth'HeightCaptionQuantity  TLabellblQuantity3LeftTop� Width'HeightCaptionQuantity  TEditedCustomerNameLeft`TopWidthaHeightTabOrder TextJoe Fop  TEdit
edProduct1Left`TopPWidth� HeightTabOrderTextBow  TEdit
edProduct2Left`ToppWidth� HeightTabOrderTextCompass  TEdit
edProduct3Left`Top� Width� HeightTabOrderTextLantern  	TSpinEditseQuantity1LeftHTopPWidth)HeightMaxValue MinValue TabOrderValue  	TSpinEditseQuantity2LeftHToppWidth)HeightMaxValue MinValue TabOrderValue  	TSpinEditseQuantity3LeftHTop� Width)HeightMaxValue MinValue TabOrderValue  TButtonbtnAddUsingQueryLeft� Top� Width� HeightCaptionAdd using SQL queryTabOrderOnClickbtnAddUsingQueryClick  TButtonbtnAddUsingTableLeft8Top� Width� HeightCaptionAdd using TABSTableTabOrderOnClickbtnAddUsingTableClick   TDateTimePickerdtOrderDateLeft�Top3WidthqHeightCalAlignmentdtaLeftDate ��j�<�@Time ��j�<�@
DateFormatdfShortDateMode
dmComboBoxKinddtkDate
ParseInputTabOrder  	TABSTable	tblOrdersCurrentVersion4.76 DatabaseNameMEMORYInMemory	ReadOnly	StoreDefs		IndexDefsNameIndexFieldsID  	FieldDefsNameIDDataType	ftAutoInc NameCustomerNameDataTypeftStringSize Name	OrderDateDataTypeftDate  	TableNameOrders	ExclusiveLeft� TopP TAutoIncFieldtblOrdersID	FieldNameID  TStringFieldtblOrdersCustomerName	FieldNameCustomerNameSize  
TDateFieldtblOrdersOrderDate	FieldName	OrderDate   	TABSTabletblOrderPositionsCurrentVersion4.76 DatabaseNameMEMORYInMemory	ReadOnly	StoreDefs		IndexDefsNameIndexFieldsOrderID  	IndexNameIndex	FieldDefsNameOrderIDDataType	ftInteger NameProductNameDataTypeftStringSize NameQuantityDataType	ftInteger  	TableNameOrderPositions	ExclusiveMasterFieldsIDMasterSourcedsOrdersLeft� Top�  TIntegerFieldtblOrderPositionsOrderID	FieldNameOrderID  TStringFieldtblOrderPositionsProductName	FieldNameProductNameSize  TIntegerFieldtblOrderPositionsQuantity	FieldNameQuantity   TDataSourcedsOrdersDataSet	tblOrdersLeft� TopP  TDataSourcedsOrderPositionsDataSettblOrderPositionsLeft� Top�   	TABSQueryqryAddOrderPositionCurrentVersion4.76 DatabaseNameMEMORYInMemory	ReadOnlySQL.Strings!INSERT INTO OrderPositions VALUES7( LASTAUTOINC('Orders', 'ID'), :ProductName, :Quantity) Left�Top� 	ParamDataDataTypeftStringNameProductName	ParamType	ptUnknown DataType	ftIntegerNameQuantity	ParamType	ptUnknown    	TABSQueryqryAddOrderCurrentVersion4.76 DatabaseNameMEMORYInMemory	ReadOnlySQL.Strings.INSERT INTO Orders  (CustomerName, OrderDate) "VALUES (:CustomerName, :OrderDate) LeftXTop� 	ParamDataDataType	ftUnknownNameCustomerName	ParamType	ptUnknown DataType	ftUnknownName	OrderDate	ParamType	ptUnknown     