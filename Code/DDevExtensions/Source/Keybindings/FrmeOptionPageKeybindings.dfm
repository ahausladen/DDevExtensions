inherited FrameOptionPageKeybindings: TFrameOptionPageKeybindings
  Width = 317
  Height = 266
  TabStop = True
  inherited pnlClient: TPanel
    Width = 317
    Height = 217
    object cbxActive: TCheckBox
      Left = 8
      Top = 8
      Width = 57
      Height = 17
      Caption = '&Active'
      TabOrder = 0
      OnClick = cbxActiveClick
    end
    object cbxExtendedHome: TCheckBox
      Left = 24
      Top = 77
      Width = 161
      Height = 17
      Hint = 
        'Pressing the HOME key in the first column moves the '#13#10'caret to t' +
        'he first non-whitespace char in the line.'
      Caption = 'Extended HOME'
      TabOrder = 3
      OnClick = cbxExtendedHomeClick
    end
    object cbxSwitchExtendedHome: TCheckBox
      Left = 40
      Top = 100
      Width = 225
      Height = 17
      Hint = 
        'Pressing the HOME key will position the caret at the first'#13#10'non-' +
        'whitespace char in the line. Pressing the HOME key'#13#10'again will m' +
        'ove it to the begin of the line.'
      Caption = 'First jump to first non-whitespace char'
      TabOrder = 4
    end
    object cbxTabIndent: TCheckBox
      Left = 24
      Top = 31
      Width = 161
      Height = 17
      Hint = 
        'Pressing TAB/Shift-TAB in a selected text block indents/unindent' +
        's'#13#10'the block.'
      Caption = 'Indent text block with TAB'
      TabOrder = 1
      OnClick = cbxTabIndentClick
    end
    object cbxIndentSingleLine: TCheckBox
      Left = 40
      Top = 54
      Width = 225
      Height = 17
      Hint = 
        'Indent/Unindent a selected text block that doesn'#39't include a lin' +
        'e break '
      Caption = 'Allow single line indention'
      TabOrder = 2
    end
    object cbxExtendedCtrlLeftRight: TCheckBox
      Left = 24
      Top = 123
      Width = 161
      Height = 17
      Hint = 'Ctrl+Left/Right like in VisualStudio'
      Caption = 'Extended Ctrl+Left/Right'
      TabOrder = 5
      OnClick = cbxTabIndentClick
    end
    object cbxShiftF3: TCheckBox
      Left = 24
      Top = 192
      Width = 174
      Height = 17
      Hint = 
        'Adds support for the Shift-F3 shortcut to the code editor.'#13#10'It a' +
        'llows to reverse the current text search.'
      Caption = 'Shift-F3 for reverse search'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      Visible = False
    end
    object chkMoveLineBlock: TCheckBox
      Left = 24
      Top = 146
      Width = 241
      Height = 17
      Hint = 'Alt+Up/Down move the current line or selected block up/down.'
      Caption = 'Shift+Ctrl+Alt+Up/Down move line/block'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
    end
    object chkFindDeclOnCaret: TCheckBox
      Left = 24
      Top = 169
      Width = 241
      Height = 17
      Hint = 
        'Find the declaration of the word under the caret on Ctrl+Alt+PgU' +
        'p'
      Caption = 'Find declaration on Ctrl+Alt+PgUp'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
    end
  end
  inherited pnlDescription: TPanel
    Width = 317
    inherited bvlSplitter: TBevel
      Width = 317
    end
    inherited lblDescription: TLabel
      Width = 182
      Caption = 'Configure the enhanced key bindings.'
    end
  end
end
