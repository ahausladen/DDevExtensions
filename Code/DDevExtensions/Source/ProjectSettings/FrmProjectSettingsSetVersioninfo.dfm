inherited FormProjectSettingsSetVersioninfo: TFormProjectSettingsSetVersioninfo
  Left = 428
  Top = 271
  BorderStyle = bsDialog
  Caption = 'Set Project Versioninfo'
  ClientHeight = 589
  ClientWidth = 704
  Constraints.MinWidth = 708
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 237
    Height = 546
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 0
    object pnlProjects: TPanel
      Left = 5
      Top = 5
      Width = 227
      Height = 536
      Align = alClient
      BevelOuter = bvNone
      BorderStyle = bsSingle
      TabOrder = 0
      object lblProjects: TLabel
        Left = 0
        Top = 0
        Width = 223
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = ' Projects'
        Color = clNavy
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Style = []
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        Transparent = False
      end
      object lvwProjects: TListView
        Left = 0
        Top = 13
        Width = 223
        Height = 519
        Align = alClient
        BorderStyle = bsNone
        Checkboxes = True
        Columns = <
          item
            AutoSize = True
          end>
        ColumnClick = False
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        SmallImages = DataModuleImages.imlIcons
        TabOrder = 0
        ViewStyle = vsReport
        OnCustomDrawItem = lvwProjectsCustomDrawItem
        OnSelectItem = lvwProjectsSelectItem
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 546
    Width = 704
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      704
      43)
    object bvlDivider: TBevel
      Left = 0
      Top = 0
      Width = 704
      Height = 9
      Align = alTop
      Shape = bsTopLine
    end
    object lblApplied: TLabel
      Left = 202
      Top = 7
      Width = 56
      Height = 13
      Caption = 'lblApplied'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
    end
    object btnClose: TButton
      Left = 624
      Top = 14
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Close'
      ModalResult = 1
      TabOrder = 0
    end
    object cbxApplyToSelectedOnly: TCheckBox
      Left = 7
      Top = 6
      Width = 180
      Height = 17
      Caption = 'Apply to &selected'
      TabOrder = 1
      OnClick = cbxApplyToSelectedOnlyClick
    end
    object cbxApplyToAllPlatforms: TCheckBox
      Left = 7
      Top = 23
      Width = 251
      Height = 17
      Caption = 'Apply VersionInfo to all platforms'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object pnlClient: TPanel
    Left = 237
    Top = 0
    Width = 467
    Height = 546
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 2
    object pgcPages: TPageControl
      Left = 5
      Top = 5
      Width = 457
      Height = 536
      ActivePage = tsSetVersionInfo
      Align = alClient
      TabHeight = 21
      TabOrder = 0
      OnChange = pgcPagesChange
      object tsSetVersionInfo: TTabSheet
        Caption = 'Set Versioninfo'
        DesignSize = (
          449
          505)
        object lblStartDay: TLabel
          Left = 8
          Top = 463
          Width = 105
          Height = 13
          Caption = '&Project creation date:'
          FocusControl = dtpStartDay
        end
        object lblFileVersionMajor: TLabel
          Left = 8
          Top = 420
          Width = 27
          Height = 13
          Caption = '&Major'
          FocusControl = edtMajor
        end
        object lblFileVersionMinor: TLabel
          Left = 80
          Top = 420
          Width = 26
          Height = 13
          Caption = 'Min&or'
          FocusControl = edtMinor
        end
        object lblFileVersionRelease: TLabel
          Left = 152
          Top = 420
          Width = 38
          Height = 13
          Caption = '&Release'
          FocusControl = edtRelease
        end
        object lblProductVersion: TLabel
          Left = 8
          Top = 51
          Width = 94
          Height = 13
          Caption = 'Product &version:'
          FocusControl = edtProductVersion
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblFileVersion: TLabel
          Left = 8
          Top = 404
          Width = 70
          Height = 13
          Caption = 'File version:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblProductName: TLabel
          Left = 8
          Top = 8
          Width = 83
          Height = 13
          Caption = 'Product &name:'
          FocusControl = edtProductName
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblFileVersionBuild: TLabel
          Left = 302
          Top = 420
          Width = 22
          Height = 13
          Caption = '&Build'
          FocusControl = edtBuild
        end
        object lblCompanyName: TLabel
          Left = 8
          Top = 94
          Width = 90
          Height = 13
          Caption = '&Company name:'
          FocusControl = edtCompanyName
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblLegalCopyright: TLabel
          Left = 8
          Top = 137
          Width = 92
          Height = 13
          Caption = 'Legal copyright:'
          FocusControl = edtLegalCopyright
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblLegalTrademarks: TLabel
          Left = 8
          Top = 180
          Width = 102
          Height = 13
          Caption = 'Legal trademarks:'
          FocusControl = edtLegalTrademarks
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LblFileDescription: TLabel
          Left = 8
          Top = 223
          Width = 91
          Height = 13
          Caption = 'File description:'
          FocusControl = edtFileDescription
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LblInternalName: TLabel
          Left = 8
          Top = 266
          Width = 84
          Height = 13
          Caption = 'Internal Name:'
          FocusControl = edtInternalName
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LblOriginalFilename: TLabel
          Left = 8
          Top = 352
          Width = 102
          Height = 13
          Caption = 'Original Filename:'
          FocusControl = edtOriginalFilename
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LblComment: TLabel
          Left = 8
          Top = 309
          Width = 62
          Height = 13
          Caption = 'Comments:'
          FocusControl = edtComments
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edtMajor: TEdit
          Left = 8
          Top = 436
          Width = 41
          Height = 21
          TabOrder = 18
          Text = '1'
          OnExit = edtFileVersionExit
        end
        object udMajor: TUpDown
          Left = 49
          Top = 436
          Width = 16
          Height = 21
          Associate = edtMajor
          Max = 32767
          Position = 1
          TabOrder = 19
          Thousands = False
        end
        object edtMinor: TEdit
          Left = 80
          Top = 436
          Width = 41
          Height = 21
          TabOrder = 20
          Text = '0'
          OnExit = edtFileVersionExit
        end
        object udMinor: TUpDown
          Left = 121
          Top = 436
          Width = 16
          Height = 21
          Associate = edtMinor
          Max = 32767
          TabOrder = 21
          Thousands = False
        end
        object edtRelease: TEdit
          Left = 152
          Top = 436
          Width = 41
          Height = 21
          TabOrder = 22
          Text = '0'
          OnExit = edtFileVersionExit
        end
        object udRelease: TUpDown
          Left = 193
          Top = 436
          Width = 16
          Height = 21
          Associate = edtRelease
          Max = 32767
          TabOrder = 23
          Thousands = False
        end
        object edtProductVersion: TEdit
          Left = 8
          Top = 67
          Width = 353
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
        end
        object btnApplyProductVersion: TButton
          Left = 367
          Top = 65
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Apply to all'
          TabOrder = 3
          OnClick = btnApplyStringClick
        end
        object btnApplyFileVersion: TButton
          Left = 215
          Top = 434
          Width = 75
          Height = 25
          Caption = 'Apply to all'
          TabOrder = 24
          OnClick = btnApplyFileVersionClick
        end
        object edtProductName: TEdit
          Left = 8
          Top = 24
          Width = 353
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object btnApplyProductName: TButton
          Left = 367
          Top = 22
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Apply to all'
          TabOrder = 1
          OnClick = btnApplyStringClick
        end
        object edtBuild: TEdit
          Left = 302
          Top = 436
          Width = 41
          Height = 21
          TabOrder = 25
          Text = '1'
          OnExit = edtFileVersionExit
        end
        object udBuild: TUpDown
          Left = 343
          Top = 436
          Width = 16
          Height = 21
          Associate = edtBuild
          Max = 32767
          Position = 1
          TabOrder = 26
          Thousands = False
        end
        object btnApplyBuild: TButton
          Left = 365
          Top = 434
          Width = 75
          Height = 25
          Caption = 'Apply to all'
          TabOrder = 27
          OnClick = btnApplyBuildClick
        end
        object dtpStartDay: TDateTimePicker
          Left = 8
          Top = 479
          Width = 82
          Height = 21
          Date = 38918.802541412000000000
          Time = 38918.802541412000000000
          TabOrder = 28
        end
        object btnDaysbetween: TButton
          Left = 97
          Top = 477
          Width = 79
          Height = 25
          Caption = 'Days &between'
          TabOrder = 29
          OnClick = btnDaysbetweenClick
        end
        object edtDaysBetween: TEdit
          Left = 182
          Top = 479
          Width = 49
          Height = 21
          TabOrder = 30
        end
        object edtCompanyName: TEdit
          Left = 8
          Top = 110
          Width = 353
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
        end
        object btnApplyCompanyName: TButton
          Left = 367
          Top = 108
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Apply to all'
          TabOrder = 5
          OnClick = btnApplyStringClick
        end
        object edtLegalCopyright: TEdit
          Left = 8
          Top = 153
          Width = 353
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = False
          TabOrder = 6
        end
        object btnApplyLegalCopyright: TButton
          Left = 367
          Top = 151
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Apply to all'
          TabOrder = 7
          OnClick = btnApplyStringClick
        end
        object edtLegalTrademarks: TEdit
          Left = 8
          Top = 196
          Width = 353
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = False
          TabOrder = 8
        end
        object btnApplyLegalTrademarks: TButton
          Left = 367
          Top = 194
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Apply to all'
          TabOrder = 9
          OnClick = btnApplyStringClick
        end
        object edtFileDescription: TEdit
          Left = 8
          Top = 239
          Width = 353
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = False
          TabOrder = 10
        end
        object btnApplyFileDescription: TButton
          Left = 366
          Top = 237
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Apply to all'
          TabOrder = 11
          OnClick = btnApplyStringClick
        end
        object edtInternalName: TEdit
          Left = 8
          Top = 282
          Width = 353
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = False
          TabOrder = 12
        end
        object btnApplyInternalName: TButton
          Left = 366
          Top = 280
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Apply to all'
          TabOrder = 13
          OnClick = btnApplyStringClick
        end
        object edtOriginalFilename: TEdit
          Left = 8
          Top = 368
          Width = 271
          Height = 21
          Anchors = [akTop, akRight]
          ParentShowHint = False
          ShowHint = False
          TabOrder = 16
        end
        object btnApplyOriginalFilename: TButton
          Left = 285
          Top = 366
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Apply to this'
          TabOrder = 17
          OnClick = btnApplyStringClick
        end
        object edtComments: TEdit
          Left = 8
          Top = 325
          Width = 353
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = False
          TabOrder = 14
        end
        object btnApplyComments: TButton
          Left = 366
          Top = 323
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Apply to all'
          TabOrder = 15
          OnClick = btnApplyStringClick
        end
        object btnApplyAutoSetOriginalFilename: TButton
          Left = 366
          Top = 366
          Width = 75
          Height = 25
          Hint = 
            'Sets the Original Filename field to the'#13#10'Project'#39's target filena' +
            'me.'
          Anchors = [akTop, akRight]
          Caption = 'Auto set to all'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 31
          OnClick = btnApplyAutoSetOriginalFilenameClick
        end
      end
      object tsIncrementVersionInfo: TTabSheet
        Caption = 'Increment Versioninfo'
        ImageIndex = 1
        object cbxIncMajor: TCheckBox
          Left = 8
          Top = 8
          Width = 170
          Height = 17
          Caption = 'Increment &Major version'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnClick = cbxIncMajorClick
        end
        object cbxIncMinor: TCheckBox
          Left = 8
          Top = 88
          Width = 170
          Height = 17
          Caption = 'Increment mi&nor version'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 5
          OnClick = cbxIncMinorClick
        end
        object cbxIncRelease: TCheckBox
          Left = 8
          Top = 145
          Width = 170
          Height = 17
          Caption = 'Increment &Release version'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 9
          OnClick = cbxIncReleaseClick
        end
        object cbxIncBuild: TCheckBox
          Left = 8
          Top = 178
          Width = 170
          Height = 17
          Caption = 'Increment &Build version'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 12
          OnClick = cbxIncBuildClick
        end
        object cbxZeroMinor: TCheckBox
          Left = 24
          Top = 32
          Width = 160
          Height = 17
          Caption = 'Set Minor version to &zero'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object cbxZeroRelease: TCheckBox
          Left = 24
          Top = 56
          Width = 160
          Height = 17
          Caption = 'Set Release version to z&ero'
          Checked = True
          State = cbChecked
          TabOrder = 4
        end
        object cbxZeroRelease2: TCheckBox
          Left = 24
          Top = 112
          Width = 160
          Height = 17
          Caption = 'Set Release version to ze&ro'
          Checked = True
          State = cbChecked
          TabOrder = 8
        end
        object btnExecuteIncrement: TButton
          Left = 109
          Top = 211
          Width = 75
          Height = 25
          Caption = '&Execute'
          TabOrder = 15
          OnClick = btnExecuteIncrementClick
        end
        object edtIncMajor: TEdit
          Left = 227
          Top = 6
          Width = 41
          Height = 21
          TabOrder = 1
          Text = '1'
          OnExit = edtFileVersionExit
        end
        object udIncMajor: TUpDown
          Left = 268
          Top = 6
          Width = 16
          Height = 21
          Associate = edtIncMajor
          Min = 1
          Max = 32767
          Position = 1
          TabOrder = 2
          Thousands = False
        end
        object edtIncMinor: TEdit
          Left = 227
          Top = 86
          Width = 41
          Height = 21
          TabOrder = 6
          Text = '1'
          OnExit = edtFileVersionExit
        end
        object udIncMinor: TUpDown
          Left = 268
          Top = 86
          Width = 16
          Height = 21
          Associate = edtIncMinor
          Min = 1
          Max = 32767
          Position = 1
          TabOrder = 7
          Thousands = False
        end
        object edtIncRelease: TEdit
          Left = 227
          Top = 143
          Width = 41
          Height = 21
          TabOrder = 10
          Text = '1'
          OnExit = edtFileVersionExit
        end
        object udIncRelease: TUpDown
          Left = 268
          Top = 143
          Width = 16
          Height = 21
          Associate = edtIncRelease
          Min = 1
          Max = 32767
          Position = 1
          TabOrder = 11
          Thousands = False
        end
        object edtIncBuild: TEdit
          Left = 227
          Top = 176
          Width = 41
          Height = 21
          TabOrder = 13
          Text = '1'
          OnExit = edtFileVersionExit
        end
        object udIncBuild: TUpDown
          Left = 268
          Top = 176
          Width = 16
          Height = 21
          Associate = edtIncBuild
          Min = 1
          Max = 32767
          Position = 1
          TabOrder = 14
          Thousands = False
        end
      end
      object tsMainIcon: TTabSheet
        Caption = 'Change Main Icon'
        ImageIndex = 2
        DesignSize = (
          449
          505)
        object lblMainIcons: TLabel
          Left = 8
          Top = 63
          Width = 32
          Height = 13
          Caption = '&Icons'
          FocusControl = lbxMainIcons
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
        end
        object pbxMainIcon: TPaintBox
          Left = 127
          Top = 79
          Width = 266
          Height = 266
          OnPaint = pbxMainIconPaint
        end
        object lblChangeMainIcon: TLabel
          Left = 8
          Top = 6
          Width = 104
          Height = 13
          Caption = 'Change Main Icon'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Style = [fsBold]
          ParentFont = False
        end
        object btnApplyMainIcon: TButton
          Left = 366
          Top = 22
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Apply to all'
          TabOrder = 2
          OnClick = btnApplyMainIconClick
        end
        object btnLoadMainIcon: TBitBtn
          Left = 8
          Top = 22
          Width = 98
          Height = 25
          Hint = 'Browse...'
          Caption = 'Load Icon...'
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FF078DBE
            078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078D
            BE078DBEFF00FFFF00FF078DBE25A1D172C7E785D7FA66CDF965CDF965CDF965
            CDF965CDF865CDF965CDF866CEF939ADD8078DBEFF00FFFF00FF078DBE4CBCE7
            39A8D1A0E2FB6FD4FA6FD4F96ED4FA6FD4F96FD4FA6FD4FA6FD4FA6ED4F93EB1
            D984D7EB078DBEFF00FF078DBE72D6FA078DBEAEEAFC79DCFB79DCFB79DCFB79
            DCFB79DCFB7ADCFB79DCFA79DCFA44B5D9AEF1F9078DBEFF00FF078DBE79DDFB
            1899C79ADFF392E7FB84E4FB83E4FC83E4FC84E4FC83E4FC83E4FB84E5FC48B9
            DAB3F4F9078DBEFF00FF078DBE82E3FC43B7DC65C3E0ACF0FD8DEBFC8DEBFC8D
            EBFD8DEBFD8DEBFC8DEBFD0C85184CBBDAB6F7F96DCAE0078DBE078DBE8AEAFC
            77DCF3229CC6FDFFFFC8F7FEC9F7FEC9F7FEC9F7FEC8F7FE0C85183CBC5D0C85
            18DEF9FBD6F6F9078DBE078DBE93F0FE93F0FD1697C5078DBE078DBE078DBE07
            8DBE078DBE0C851852D97F62ED9741C4650C8518078DBE078DBE078DBE9BF5FE
            9AF6FE9AF6FE9BF5FD9BF6FE9AF6FE9BF5FE0C851846CE6C59E48858E18861EB
            9440C1650C8518FF00FF078DBEFEFEFEA0FBFFA0FBFEA0FBFEA1FAFEA1FBFE0C
            85180C85180C85180C851856E18447CD6E0C85180C85180C8518FF00FF078DBE
            FEFEFEA5FEFFA5FEFFA5FEFF078CB643B7DC43B7DC43B7DC0C85184EDD7936BA
            540C8518FF00FFFF00FFFF00FFFF00FF078DBE078DBE078DBE078DBEFF00FFFF
            00FFFF00FFFF00FF0C851840D0650C8518FF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0C85182AB7432DBA490C85
            18FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FF0C851821B5380C8518FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FF0C85180C85180C85180C8518FF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0C85180C85180C
            85180C8518FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = btnLoadMainIconClick
        end
        object lbxMainIcons: TListBox
          Left = 7
          Top = 79
          Width = 114
          Height = 266
          Style = lbOwnerDrawFixed
          TabOrder = 3
          OnClick = lbxMainIconsClick
        end
        object btnRemoveMainIcon: TBitBtn
          Left = 112
          Top = 22
          Width = 98
          Height = 25
          Caption = '&Remove Icon'
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FF0732DE0732DEFF00FF0732DE0732DEFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FF0732DE0732DEFF00FFFF00FF0732DE
            0732DE0732DEFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0732
            DE0732DEFF00FFFF00FFFF00FF0732DE0732DD0732DE0732DEFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FF0732DE0732DEFF00FFFF00FFFF00FFFF00FFFF00FF
            0534ED0732DF0732DE0732DEFF00FFFF00FFFF00FFFF00FF0732DE0732DEFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0732DE0732DE0732DDFF
            00FF0732DD0732DE0732DEFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FF0732DD0633E60633E60633E90732DCFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0633E307
            32E30534EFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FF0732DD0534ED0533E90434EF0434F5FF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0434F40534EF0533EBFF
            00FFFF00FF0434F40335F8FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FF0335FC0534EF0434F8FF00FFFF00FFFF00FFFF00FF0335FC0335FBFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FF0335FB0335FB0335FCFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FF0335FB0335FBFF00FFFF00FFFF00FFFF00FF0335FB
            0335FB0335FBFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FF0335FBFF00FFFF00FF0335FB0335FB0335FBFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0335FB0335FB
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
          ParentShowHint = False
          ShowHint = False
          TabOrder = 1
          OnClick = btnRemoveMainIconClick
        end
      end
    end
  end
  object TimerAppliedHide: TTimer
    Enabled = False
    OnTimer = TimerAppliedHideTimer
    Left = 160
    Top = 296
  end
  object dlgOpenMainIcon: TOpenPictureDialog
    DefaultExt = 'ico'
    Filter = 'Icon file (*.ico)|*.ico|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open Icon'
    Left = 608
    Top = 8
  end
end
