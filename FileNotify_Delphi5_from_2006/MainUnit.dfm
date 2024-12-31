object Form1: TForm1
  Left = 297
  Top = 108
  Width = 786
  Height = 907
  Caption = 'FileNotify'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 20
    Width = 116
    Height = 13
    Caption = 'Verzeichnis überwachen'
  end
  object Memo1: TMemo
    Left = 12
    Top = 52
    Width = 752
    Height = 393
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object DirectoryEdit1: TDirectoryEdit
    Left = 148
    Top = 16
    Width = 361
    Height = 21
    DialogKind = dkWin32
    NumGlyphs = 1
    TabOrder = 1
    Text = '\\M46348\f\dh'
    OnChange = DirectoryEdit1Change
  end
  object CheckBox1: TCheckBox
    Left = 528
    Top = 20
    Width = 97
    Height = 17
    Caption = 'Rekursiv'
    TabOrder = 2
  end
  object RxDBGrid1: TRxDBGrid
    Left = 12
    Top = 452
    Width = 753
    Height = 417
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Aktion'
        Width = 36
        Visible = True
      end
      item
        Alignment = taCenter
        Expanded = False
        FieldName = 'Zeitstempel'
        Title.Alignment = taCenter
        Visible = True
      end
      item
        Alignment = taCenter
        Expanded = False
        FieldName = 'Aktionsname'
        Title.Alignment = taCenter
        Width = 90
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Aktionstext'
        Title.Alignment = taCenter
        Width = 479
        Visible = True
      end>
  end
  object DataSource: TDataSource
    DataSet = RxMemoryData
    Left = 60
    Top = 520
  end
  object RxMemoryData: TRxMemoryData
    Active = True
    FieldDefs = <
      item
        Name = 'Aktion'
        DataType = ftInteger
      end
      item
        Name = 'Zeitstempel'
        DataType = ftDateTime
      end
      item
        Name = 'Aktionsname'
        DataType = ftString
        Size = 64
      end
      item
        Name = 'Aktionstext'
        DataType = ftString
        Size = 4096
      end>
    Left = 28
    Top = 520
    object RxMemoryDataAktion: TIntegerField
      FieldName = 'Aktion'
    end
    object RxMemoryDataZeitstempel: TDateTimeField
      FieldName = 'Zeitstempel'
    end
    object RxMemoryDataAktionsname: TStringField
      FieldName = 'Aktionsname'
      Size = 64
    end
    object RxMemoryDataAktionstext: TStringField
      FieldName = 'Aktionstext'
      Size = 4096
    end
  end
end
