?
 TFMFILTERFRM 0?  TPF0TfmFilterFrmfmFilterFrmLeftnTopqActiveControlMemo1BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionFilter ConditionClientHeight*ClientWidth1Color	clBtnFace
ParentFont	OldCreateOrder	OnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel1LeftTop? WidthHeightCaption&FieldsFocusControlListBox1  TLabelLabel2LeftTop,Width,HeightCaption
&Condition  TLabelLabel3Left? Top? Width.HeightCaption
&Operators  TLabelLabel4LeftTopWidthHeightCaption&ListFocusControl	ComboBox1  TListBoxListBox1LeftTop? Width? HeightIHintDoubleclick to addTabStop
ItemHeightParentShowHintShowHint	Sorted	TabOrder
OnDblClickAddFieldName  TListBoxListBox2Left? Top? Width5Height? HintDouble-click to addTabStop
ItemHeightItems.Strings><= >=<=<>ANDOR() ParentShowHintShowHint	TabOrder
OnDblClickListBox2DblClick  TMemoMemo1LeftTop<Width!HeightATabOrderOnChangeMemo1Change  	TGroupBox	GroupBox1LeftTop? Width? Height=CaptionF&ilter OptionsTabOrder 	TCheckBoxcbCaseSensitiveLeftTopWidthaHeightCaptionCase SensitiveTabOrder OnClickcbCaseSensitiveClick  	TCheckBoxcbNoPartialCompareLeftTop$Width}HeightHint<Disable partial string comparisons (strings ending with '*')CaptionNo Partial CompareTabOrderOnClickcbNoPartialCompareClick   	TComboBox	ComboBox1LeftTopWidth!HeightCursorcrArrow
ItemHeightTabOrder OnChangeComboBox1ChangeItems.StringsLastName = 'R*'FirstName <> 'Roger'PhoneExt > 200   TButtonBtnApplyFilterLeft? Top? WidthKHeightCaption&ApplyTabOrderOnClickApplyFilter  TButtonBtnClearLeft? Top? WidthKHeightCaptionCl&earTabOrderOnClickSBtnClearClick  TButtonBtnCloseLeft? Top WidthKHeightCaptionCl&oseTabOrderOnClickSBtnCloseClick   