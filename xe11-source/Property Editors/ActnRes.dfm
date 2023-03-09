object StandardActions: TStandardActions
  OldCreateOrder = True
  Left = 229
  Top = 423
  Height = 308
  Width = 499
  object ActionList1: TActionList
    Images = ImageList1
    Left = 72
    Top = 8
    object EditCut1: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object EditCopy1: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object EditPaste1: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 2
      ShortCut = 16470
    end
    object WindowArrange1: TWindowArrange
      Category = 'Window'
      Caption = '&Arrange'
    end
    object WindowCascade1: TWindowCascade
      Category = 'Window'
      Caption = '&Cascade'
      Hint = 'Cascade'
      ImageIndex = 17
    end
    object WindowClose1: TWindowClose
      Category = 'Window'
      Caption = 'C&lose'
      Hint = 'Close'
    end
    object WindowMinimizeAll1: TWindowMinimizeAll
      Category = 'Window'
      Caption = '&Minimize All'
      Hint = 'Minimize All'
    end
    object WindowTileHorizontal1: TWindowTileHorizontal
      Category = 'Window'
      Caption = 'Tile &Horizontally'
      Hint = 'Tile Horizontal'
      ImageIndex = 15
    end
    object WindowTileVertical1: TWindowTileVertical
      Category = 'Window'
      Caption = '&Tile Vertically'
      Hint = 'Tile Vertical'
      ImageIndex = 16
    end
    object EditDelete1: TEditDelete
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete|Erases the selection'
      ImageIndex = 5
      ShortCut = 46
    end
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object EditUndo1: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 3
      ShortCut = 16474
    end
    object HelpContents1: THelpContents
      Category = 'Help'
      Caption = '&Contents'
      Hint = 'Help Contents'
      ImageIndex = 40
    end
    object HelpOnHelp1: THelpOnHelp
      Category = 'Help'
      Caption = '&Help on Help'
      Hint = 'Help on help'
    end
    object HelpTopicSearch1: THelpTopicSearch
      Category = 'Help'
      Caption = '&Topic Search'
      Hint = 'Topic Search'
      ImageIndex = 9
    end
    object FileExit1: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 43
    end
    object FileOpen1: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 7
      ShortCut = 16463
    end
    object FilePrintSetup1: TFilePrintSetup
      Category = 'File'
      Caption = 'Print Set&up...'
      Hint = 'Print Setup'
    end
    object FileSaveAs1: TFileSaveAs
      Category = 'File'
      Caption = 'Save &As...'
      Hint = 'Save As|Saves the active file with a new name'
      ImageIndex = 30
    end
    object RichEditBold1: TRichEditBold
      Category = 'Format'
      AutoCheck = True
      Caption = '&Bold'
      Hint = 'Bold'
      ImageIndex = 31
      ShortCut = 16450
    end
    object RichEditItalic1: TRichEditItalic
      Category = 'Format'
      AutoCheck = True
      Caption = '&Italic'
      Hint = 'Italic'
      ImageIndex = 29
      ShortCut = 16457
    end
    object RichEditUnderline1: TRichEditUnderline
      Category = 'Format'
      AutoCheck = True
      Caption = '&Underline'
      Hint = 'Underline'
      ImageIndex = 28
      ShortCut = 16469
    end
    object SearchFind1: TSearchFind
      Category = 'Search'
      Caption = '&Find...'
      Hint = 'Find|Finds the specified text'
      ImageIndex = 34
      ShortCut = 16454
    end
    object SearchFindNext1: TSearchFindNext
      Category = 'Search'
      Caption = 'Find &Next'
      Hint = 'Find Next|Repeats the last find'
      ImageIndex = 33
      ShortCut = 114
    end
    object SearchReplace1: TSearchReplace
      Category = 'Search'
      Caption = '&Replace'
      Hint = 'Replace|Replaces specific text with different text'
      ImageIndex = 32
    end
    object RichEditAlignCenter1: TRichEditAlignCenter
      Category = 'Format'
      AutoCheck = True
      Caption = '&Center'
      Hint = 'Center|Centers text between margins'
      ImageIndex = 37
    end
    object RichEditAlignLeft1: TRichEditAlignLeft
      Category = 'Format'
      AutoCheck = True
      Caption = 'Align &Left'
      Hint = 'Align Left|Aligns text at the left indent'
      ImageIndex = 35
    end
    object RichEditAlignRight1: TRichEditAlignRight
      Category = 'Format'
      AutoCheck = True
      Caption = 'Align &Right'
      Hint = 'Align Right|Aligns text at the right indent'
      ImageIndex = 36
    end
    object RichEditBullets1: TRichEditBullets
      Category = 'Format'
      AutoCheck = True
      Caption = '&Bullets'
      Hint = 'Bullets|Inserts a bullet on the current line'
      ImageIndex = 38
    end
    object RichEditStrikeOut1: TRichEditStrikeOut
      Category = 'Format'
      AutoCheck = True
      Caption = '&Strikeout'
      Hint = 'Strikeout'
      ImageIndex = 44
    end
    object ColorSelect1: TColorSelect
      Category = 'Dialog'
      Caption = 'Select &Color...'
      Hint = 'Color Select'
    end
    object FontEdit1: TFontEdit
      Category = 'Dialog'
      Caption = 'Select &Font...'
      Dialog.MinFontSize = 0
      Dialog.MaxFontSize = 0
      Hint = 'Font Select'
    end
    object OpenPicture1: TOpenPicture
      Category = 'Dialog'
      Caption = '&Open Picture...'
      Hint = 'Open Picture'
      ShortCut = 16463
    end
    object SavePicture1: TSavePicture
      Category = 'Dialog'
      Caption = '&Save Picture...'
      Hint = 'Save Picture'
      ShortCut = 16467
    end
    object FileRun1: TFileRun
      Category = 'File'
      Browse = False
      BrowseDlg.Title = 'Run'
      Caption = '&Run...'
      Hint = 'Run|Runs an application'
      Operation = 'open'
      ShowCmd = scShowNormal
    end
    object PreviousTab1: TPreviousTab
      Category = 'Tab'
      Caption = '&Previous'
      Hint = 'Previous|Go back to the previous tab'
    end
    object NextTab1: TNextTab
      Category = 'Tab'
      LastTabCaption = '&Finish'
      Caption = '&Next'
      Hint = 'Next|Go to the next tab'
    end
    object SearchFindFirst1: TSearchFindFirst
      Category = 'Search'
      Caption = 'F&ind First'
      Hint = 'Find First|Finds the first occurance of specified text'
    end
    object HelpContextAction1: THelpContextAction
      Category = 'Help'
      Caption = 'HelpContextAction1'
      ImageIndex = 11
    end
    object ListControlCopySelection1: TListControlCopySelection
      Category = 'List'
      Caption = '&Copy Selection'
      Hint = 'Copy Selection'
    end
    object ListControlDeleteSelection1: TListControlDeleteSelection
      Category = 'List'
      Caption = '&Delete Selection'
      Hint = 'Delete Selection'
    end
    object ListControlSelectAll1: TListControlSelectAll
      Category = 'List'
      Caption = '&Select All'
      Hint = 'Select All'
    end
    object ListControlClearSelection1: TListControlClearSelection
      Category = 'List'
      Caption = 'Clear Selection'
      Hint = 'Clear Selection'
    end
    object ListControlMoveSelection1: TListControlMoveSelection
      Category = 'List'
      Caption = '&Move Selection'
      Hint = 'Move Selection'
    end
    object BrowseURL1: TBrowseURL
      Category = 'Internet'
      Caption = '&Browse URL'
      Hint = 'Browse URL'
    end
    object DownLoadURL1: TDownLoadURL
      Category = 'Internet'
      Caption = '&Download URL'
      Hint = 'Download from URL'
    end
    object SendMail1: TSendMail
      Category = 'Internet'
      Caption = '&Send Mail...'
      Hint = 'Send email'
    end
    object PrintDlg1: TPrintDlg
      Category = 'Dialog'
      Caption = '&Print...'
      ImageIndex = 14
      ShortCut = 16464
    end
    object FileOpenWith1: TFileOpenWith
      Category = 'File'
      Caption = 'Open with...'
    end
    object FilePageSetup1: TFilePageSetup
      Category = 'File'
      Caption = 'Page Set&up...'
      Dialog.MinMarginLeft = 0
      Dialog.MinMarginTop = 0
      Dialog.MinMarginRight = 0
      Dialog.MinMarginBottom = 0
      Dialog.MarginLeft = 1000
      Dialog.MarginTop = 1000
      Dialog.MarginRight = 1000
      Dialog.MarginBottom = 1000
      Dialog.PageWidth = 8500
      Dialog.PageHeight = 11000
    end
  end
  object ImageList1: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 203
        CollectionName = 'AppBuilderActions\Cut'
        Disabled = False
        Name = 'AppBuilderActions\Cut'
      end
      item
        CollectionIndex = 204
        CollectionName = 'AppBuilderActions\Copy'
        Disabled = False
        Name = 'AppBuilderActions\Copy'
      end
      item
        CollectionIndex = 205
        CollectionName = 'AppBuilderActions\Paste'
        Disabled = False
        Name = 'AppBuilderActions\Paste'
      end
      item
        CollectionIndex = 106
        CollectionName = 'AppBuilderActions\Undo'
        Disabled = False
        Name = 'AppBuilderActions\Undo'
      end
      item
        CollectionIndex = 107
        CollectionName = 'AppBuilderActions\Redo'
        Disabled = False
        Name = 'AppBuilderActions\Redo'
      end
      item
        CollectionIndex = 505
        CollectionName = 'AppBuilderActions\Clear'
        Disabled = False
        Name = 'AppBuilderActions\Clear'
      end
      item
        CollectionIndex = 780
        CollectionName = 'AppBuilder\Document'
        Disabled = False
        Name = 'AppBuilder\Document'
      end
      item
        CollectionIndex = 641
        CollectionName = 'AppBuilderActions\OpenFile'
        Disabled = False
        Name = 'AppBuilderActions\OpenFile'
      end
      item
        CollectionIndex = 642
        CollectionName = 'AppBuilderActions\SaveFile'
        Disabled = False
        Name = 'AppBuilderActions\SaveFile'
      end
      item
        CollectionIndex = 845
        CollectionName = 'StandardActions\HelpTopicSearch'
        Disabled = False
        Name = 'StandardActions\HelpTopicSearch'
      end
      item
        CollectionIndex = 369
        CollectionName = 'ClassBrowser\Item574'
        Disabled = False
        Name = 'ClassBrowser\Item574'
      end
      item
        CollectionIndex = 781
        CollectionName = 'AppBuilderHelp\SelectHelp'
        Disabled = False
        Name = 'AppBuilderHelp\SelectHelp'
      end
      item
        CollectionIndex = 8
        CollectionName = 'AppBuilderActions\Search'
        Disabled = False
        Name = 'AppBuilderActions\Search'
      end
      item
        CollectionIndex = 846
        CollectionName = 'StandardActions\Item2381'
        Disabled = False
        Name = 'StandardActions\Item2381'
      end
      item
        CollectionIndex = 648
        CollectionName = 'AppBuilderActions\Print'
        Disabled = False
        Name = 'AppBuilderActions\Print'
      end
      item
        CollectionIndex = 847
        CollectionName = 'StandardActions\WindowTileHorizontal'
        Disabled = False
        Name = 'StandardActions\WindowTileHorizontal'
      end
      item
        CollectionIndex = 848
        CollectionName = 'StandardActions\WindowTileVertical'
        Disabled = False
        Name = 'StandardActions\WindowTileVertical'
      end
      item
        CollectionIndex = 849
        CollectionName = 'StandardActions\WindowCascade'
        Disabled = False
        Name = 'StandardActions\WindowCascade'
      end
      item
        CollectionIndex = 850
        CollectionName = 'DatasetActions\DatasetCancel'
        Disabled = False
        Name = 'DatasetActions\DatasetCancel'
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
        CollectionIndex = 853
        CollectionName = 'DatasetActions\DatasetFirst'
        Disabled = False
        Name = 'DatasetActions\DatasetFirst'
      end
      item
        CollectionIndex = 854
        CollectionName = 'DatasetActions\DatasetInsert'
        Disabled = False
        Name = 'DatasetActions\DatasetInsert'
      end
      item
        CollectionIndex = 855
        CollectionName = 'DatasetActions\DatasetLast'
        Disabled = False
        Name = 'DatasetActions\DatasetLast'
      end
      item
        CollectionIndex = 856
        CollectionName = 'DatasetActions\DatasetNext'
        Disabled = False
        Name = 'DatasetActions\DatasetNext'
      end
      item
        CollectionIndex = 857
        CollectionName = 'DatasetActions\DatasetPost'
        Disabled = False
        Name = 'DatasetActions\DatasetPost'
      end
      item
        CollectionIndex = 858
        CollectionName = 'DatasetActions\DatasetPrior'
        Disabled = False
        Name = 'DatasetActions\DatasetPrior'
      end
      item
        CollectionIndex = 859
        CollectionName = 'DatasetActions\DatasetRefresh'
        Disabled = False
        Name = 'DatasetActions\DatasetRefresh'
      end
      item
        CollectionIndex = 110
        CollectionName = 'Font\FontUnderline'
        Disabled = False
        Name = 'Font\FontUnderline'
      end
      item
        CollectionIndex = 109
        CollectionName = 'Font\FontIntalic'
        Disabled = False
        Name = 'Font\FontIntalic'
      end
      item
        CollectionIndex = 718
        CollectionName = 'AppBuilderActions\SaveAs'
        Disabled = False
        Name = 'AppBuilderActions\SaveAs'
      end
      item
        CollectionIndex = 108
        CollectionName = 'Font\FontBold'
        Disabled = False
        Name = 'Font\FontBold'
      end
      item
        CollectionIndex = 676
        CollectionName = 'AppBuilderSearch\Replace'
        Disabled = False
        Name = 'AppBuilderSearch\Replace'
      end
      item
        CollectionIndex = 860
        CollectionName = 'AppBuilderSearch\FindNext'
        Disabled = False
        Name = 'AppBuilderSearch\FindNext'
      end
      item
        CollectionIndex = 264
        CollectionName = 'Libraries_ActiveX_Objects\View'
        Disabled = False
        Name = 'Libraries_ActiveX_Objects\View'
      end
      item
        CollectionIndex = 111
        CollectionName = 'Font\FontAlignLeft'
        Disabled = False
        Name = 'Font\FontAlignLeft'
      end
      item
        CollectionIndex = 112
        CollectionName = 'Font\FontAlignRight'
        Disabled = False
        Name = 'Font\FontAlignRight'
      end
      item
        CollectionIndex = 113
        CollectionName = 'Font\FontAlignCenter'
        Disabled = False
        Name = 'Font\FontAlignCenter'
      end
      item
        CollectionIndex = 114
        CollectionName = 'HTMLDesigner\ListBullet'
        Disabled = False
        Name = 'HTMLDesigner\ListBullet'
      end
      item
        CollectionIndex = 649
        CollectionName = 'AppBuilderActions\SaveAllFiles'
        Disabled = False
        Name = 'AppBuilderActions\SaveAllFiles'
      end
      item
        CollectionIndex = 782
        CollectionName = 'AppBuilderHelp\HelpBook'
        Disabled = False
        Name = 'AppBuilderHelp\HelpBook'
      end
      item
        CollectionIndex = 861
        CollectionName = 'StandardActions\Item2409'
        Disabled = False
        Name = 'StandardActions\Item2409'
      end
      item
        CollectionIndex = 862
        CollectionName = 'StandardActions\Item2410'
        Disabled = False
        Name = 'StandardActions\Item2410'
      end
      item
        CollectionIndex = 704
        CollectionName = 'AppBuilderFile\Exit'
        Disabled = False
        Name = 'AppBuilderFile\Exit'
      end
      item
        CollectionIndex = 863
        CollectionName = 'Font\FontStrikeOut'
        Disabled = False
        Name = 'Font\FontStrikeOut'
      end>
    ImageCollection = IDEImageResourcesFrm.IDEImageCollection
    Left = 16
    Top = 8
  end
end
