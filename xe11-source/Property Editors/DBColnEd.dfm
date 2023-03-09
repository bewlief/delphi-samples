inherited DBGridColumnsEditor: TDBGridColumnsEditor
  Top = 121
  Caption = 'DBGridColumnsEditor'
  OnBeforeMonitorDpiChanged = FormBeforeMonitorDpiChanged
  PixelsPerInch = 96
  TextHeight = 15
  inherited Splitter1: TSplitter
    ExplicitWidth = 264
  end
  inherited ToolBar1: TToolBar
    ExplicitWidth = 120
    object ToolButton6: TToolButton
      Left = 104
      Top = 0
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 4
      ImageName = 'Collections\AddAllField'
      Style = tbsSeparator
      Visible = False
    end
    object ToolButton7: TToolButton
      Left = 112
      Top = 0
      Action = AddAllFieldsCmd
    end
    object ToolButton8: TToolButton
      Left = 135
      Top = 0
      Action = RestoreDefaultsCmd
    end
  end
  inherited Panel3: TPanel
    ExplicitTop = 38
    ExplicitWidth = 120
    ExplicitHeight = 84
    inherited ListView1: TListView
      Width = 120
      Height = 84
      ExplicitWidth = 120
      ExplicitHeight = 84
    end
  end
  inherited PopupMenu1: TPopupActionBar
    Left = 40
    Top = 40
    object AddAllFields1: TMenuItem [6]
      Action = AddAllFieldsCmd
    end
    object Restoredefaults1: TMenuItem [7]
      Action = RestoreDefaultsCmd
    end
    object N1: TMenuItem [8]
      Caption = '-'
    end
  end
  inherited ActionList1: TActionList
    inherited MoveUpCmd: TAction
      Visible = False
    end
    inherited MoveDownCmd: TAction
      Visible = False
    end
    object AddAllFieldsCmd: TAction
      Caption = 'Add All &Fields'
      Enabled = False
      Hint = 'Add All Fields'
      ImageIndex = 4
      ImageName = 'Collections\AddAllField'
      OnExecute = AddAllFieldsClick
      OnUpdate = AddAllFieldsCmdUpdate
    end
    object RestoreDefaultsCmd: TAction
      Caption = '&Restore Defaults'
      Enabled = False
      Hint = 'Restore Defaults'
      ImageIndex = 5
      ImageName = 'Collections\RestoreDefaults'
      OnExecute = RestoreDefaultsClick
      OnUpdate = RestoreDefaultsCmdUpdate
    end
  end
  inherited ImageList1: TVirtualImageList
    Images = <
      item
        CollectionIndex = 402
        CollectionName = 'Collections\Add'
        Name = 'Collections\Add'
        Description = 
          'BuildGroupFrame.GroupImageList, ConfigManagerForm.ImageList1, Ob' +
          'jectTreeView.ImageList, CollectionEditor.ImageList1, ActionListD' +
          'esigner.ImageList1, CustomizeActnEditDesigner.ImageList1, BindAd' +
          'apterColumnsEditor.ImageList1, DataBindingListDesigner.ImageList' +
          '1, BindCompExprDesigner.ImageList1, BindDBGridColumnsEditor.Imag' +
          'eList1, BindGridColumnsEditor.ImageList1, ConstraintEditor.Image' +
          'List1, DBGridColumnsEditor.ImageList1, ImageListDesignerForm.Ima' +
          'geList1, MultiResBitmapForm.ImageList1, HTMLTableEditor.ImageLis' +
          't1, FrInformationEditor.ImageList1'
      end
      item
        CollectionIndex = 411
        CollectionName = 'Collections\Delete'
        Name = 'Collections\Delete'
        Description = 
          'BuildGroupFrame.GroupImageList, ConfigManagerForm.ImageList1, Ob' +
          'jectTreeView.ImageList, CollectionEditor.ImageList1, ActionListD' +
          'esigner.ImageList1, CustomizeActnEditDesigner.ImageList1, BindAd' +
          'apterColumnsEditor.ImageList1, DataBindingListDesigner.ImageList' +
          '1, BindCompExprDesigner.ImageList1, BindDBGridColumnsEditor.Imag' +
          'eList1, BindGridColumnsEditor.ImageList1, ConstraintEditor.Image' +
          'List1, DBGridColumnsEditor.ImageList1, ImageListDesignerForm.Ima' +
          'geList1, MultiResBitmapForm.ImageList1, HTMLTableEditor.ImageLis' +
          't1, FrInformationEditor.ImageList1'
      end
      item
        CollectionIndex = 412
        CollectionName = 'Collections\MoveUp'
        Name = 'Collections\MoveUp'
        Description = 
          'ClassExplorerResources.ToolbarImages, ConfigManagerForm.ImageLis' +
          't1, ObjectTreeView.ImageList, CollectionEditor.ImageList1, EditW' +
          'indow.SearchBarImages, ActionListDesigner.ImageList1, CustomizeA' +
          'ctnEditDesigner.ImageList1, BindAdapterColumnsEditor.ImageList1,' +
          ' DataBindingListDesigner.ImageList1, BindCompExprDesigner.ImageL' +
          'ist1, BindDBGridColumnsEditor.ImageList1, BindGridColumnsEditor.' +
          'ImageList1, ConstraintEditor.ImageList1, DBGridColumnsEditor.Ima' +
          'geList1, ImageListDesignerForm.ImageList1, HTMLTableEditor.Image' +
          'List1'
      end
      item
        CollectionIndex = 414
        CollectionName = 'Collections\MoveDown'
        Name = 'Collections\MoveDown'
        Description = 'DataExplorer'
      end
      item
        CollectionIndex = 467
        CollectionName = 'Collections\AddAllField'
        Name = 'Collections\AddAllField'
        Description = 'ObjectTreeView.ListItemImages, UDDIBrowMainForm.Images'
      end
      item
        CollectionIndex = 845
        CollectionName = 'Collections\RestoreDefaults'
        Name = 'Collections\RestoreDefaults'
        Description = 'PasProSkuCommands.ImageList1'
      end>
  end
end
