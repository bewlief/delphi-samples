object CommentFrame: TCommentFrame
  Left = 0
  Top = 0
  Width = 222
  Height = 135
  TabOrder = 0
  DesignSize = (
    222
    135)
  object Label1: TLabel
    Left = 3
    Top = 3
    Width = 49
    Height = 13
    Caption = 'Comment:'
  end
  object Memo1: TMemo
    Left = 3
    Top = 22
    Width = 214
    Height = 107
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Sample comment')
    TabOrder = 0
  end
end
