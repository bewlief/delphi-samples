object StandardDatasetActions: TStandardDatasetActions
  Left = 192
  Top = 106
  Height = 151
  Width = 211
  object ActionList1: TActionList
    Images = ImageList1
    Left = 80
    Top = 8
    object DataSetFirst1: TDataSetFirst
      Category = 'Dataset'
      Caption = '&First'
      Hint = 'First'
      ImageIndex = 0
    end
    object DataSetPrior1: TDataSetPrior
      Category = 'Dataset'
      Caption = '&Prior'
      Hint = 'Prior'
      ImageIndex = 1
    end
    object DataSetNext1: TDataSetNext
      Category = 'Dataset'
      Caption = '&Next'
      Hint = 'Next'
      ImageIndex = 2
    end
    object DataSetLast1: TDataSetLast
      Category = 'Dataset'
      Caption = '&Last'
      Hint = 'Last'
      ImageIndex = 3
    end
    object DataSetInsert1: TDataSetInsert
      Category = 'Dataset'
      Caption = '&Insert'
      Hint = 'Insert'
      ImageIndex = 4
    end
    object DataSetDelete1: TDataSetDelete
      Category = 'Dataset'
      Caption = '&Delete'
      Hint = 'Delete'
      ImageIndex = 5
    end
    object DataSetEdit1: TDataSetEdit
      Category = 'Dataset'
      Caption = '&Edit'
      Hint = 'Edit'
      ImageIndex = 6
    end
    object DataSetPost1: TDataSetPost
      Category = 'Dataset'
      Caption = 'P&ost'
      Hint = 'Post'
      ImageIndex = 7
    end
    object DataSetCancel1: TDataSetCancel
      Category = 'Dataset'
      Caption = '&Cancel'
      Hint = 'Cancel'
      ImageIndex = 8
    end
    object DataSetRefresh1: TDataSetRefresh
      Category = 'Dataset'
      Caption = '&Refresh'
      Hint = 'Refresh'
      ImageIndex = 9
    end
  end
  object ImageList1: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 853
        CollectionName = 'DatasetActions\DatasetFirst'
        Disabled = False
        Name = 'DatasetActions\DatasetFirst'
      end
      item
        CollectionIndex = 858
        CollectionName = 'DatasetActions\DatasetPrior'
        Disabled = False
        Name = 'DatasetActions\DatasetPrior'
      end
      item
        CollectionIndex = 856
        CollectionName = 'DatasetActions\DatasetNext'
        Disabled = False
        Name = 'DatasetActions\DatasetNext'
      end
      item
        CollectionIndex = 855
        CollectionName = 'DatasetActions\DatasetLast'
        Disabled = False
        Name = 'DatasetActions\DatasetLast'
      end
      item
        CollectionIndex = 854
        CollectionName = 'DatasetActions\DatasetInsert'
        Disabled = False
        Name = 'DatasetActions\DatasetInsert'
      end
      item
        CollectionIndex = 851
        CollectionName = 'DatasetActions\DatasetDelete'
        Disabled = False
        Name = 'DatasetActions\DatasetDelete'
      end
      item
        CollectionIndex = 852
        CollectionName = 'DatasetActions\DatasetEdit'
        Disabled = False
        Name = 'DatasetActions\DatasetEdit'
      end
      item
        CollectionIndex = 857
        CollectionName = 'DatasetActions\DatasetPost'
        Disabled = False
        Name = 'DatasetActions\DatasetPost'
      end
      item
        CollectionIndex = 850
        CollectionName = 'DatasetActions\DatasetCancel'
        Disabled = False
        Name = 'DatasetActions\DatasetCancel'
      end
      item
        CollectionIndex = 859
        CollectionName = 'DatasetActions\DatasetRefresh'
        Disabled = False
        Name = 'DatasetActions\DatasetRefresh'
      end>
    ImageCollection = IDEImageResourcesFrm.IDEImageCollection
    Left = 20
    Top = 8
  end
end
