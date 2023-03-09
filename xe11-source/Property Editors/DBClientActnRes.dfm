object ClientDatasetActions: TClientDatasetActions
  OldCreateOrder = False
  Left = 217
  Top = 85
  Height = 150
  Width = 215
  object ActionList1: TActionList
    Images = ImageList1
    Left = 136
    Top = 32
    object ClientDataSetApply1: TClientDataSetApply
      Category = 'Dataset'
      Caption = 'Apply Updates'
      ImageIndex = 2
      MaxErrors = -1
    end
    object ClientDataSetRevert1: TClientDataSetRevert
      Category = 'Dataset'
      Caption = 'Revert'
      ImageIndex = 1
    end
    object ClientDataSetUndo1: TClientDataSetUndo
      Category = 'Dataset'
      Caption = 'Undo'
      ImageIndex = 0
      FollowChange = False
    end
  end
  object ImageList1: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 892
        CollectionName = 'AppBuilderActions\UndoBlack'
        Disabled = False
        Name = 'AppBuilderActions\UndoBlack'
      end
      item
        CollectionIndex = 893
        CollectionName = 'AppBuilderActions\RedoBlack'
        Disabled = False
        Name = 'AppBuilderActions\RedoBlack'
      end
      item
        CollectionIndex = 39
        CollectionName = 'AppBuilderActions\Apply'
        Disabled = False
        Name = 'AppBuilderActions\Apply'
      end>
    ImageCollection = IDEImageResourcesFrm.IDEImageCollection
    Left = 48
    Top = 32
  end
end
