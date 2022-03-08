{-------------------------------------------------------------------------------

Copyright (C) 2015 Helltar <mail@helltar.com>

This file is part of AMPASIDE.

AMPASIDE is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

AMPASIDE is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with AMPASIDE.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------------}

unit uEditorOptionsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, ColorBox, Dialogs, Forms, Spin, StdCtrls, ExtCtrls, EditBtn, SysUtils;

type

  { TEditorOptionsFrame }

  TEditorOptionsFrame = class(TFrame)
    cbScrollPastEol: TCheckBox;
    clrbEditorColor: TColorBox;
    clrbRightEdgeColor: TColorBox;
    clrbComment: TColorBox;
    clrbKey: TColorBox;
    clrbNumber: TColorBox;
    clrbString: TColorBox;
    clrbIdentifier: TColorBox;
    clrbSymbol: TColorBox;
    cbbRightEdge: TComboBox;
    clrbSelectedColor: TColorBox;
    edtbtnSelectFont: TEditButton;
    edtbtnSchemeFile: TEditButton;
    gbFont: TGroupBox;
    gbMix: TGroupBox;
    gbColors: TGroupBox;
    lblSelectedColor: TLabel;
    lblFontSize: TLabel;
    lblSymbol: TLabel;
    lblRightEdge: TLabel;
    lblEditorColor: TLabel;
    lblRigthEdge: TLabel;
    lblComment: TLabel;
    lblKey: TLabel;
    lblNumber: TLabel;
    lblString: TLabel;
    lblIdentifier: TLabel;
    sedtFontSize: TSpinEdit;
    splLeft: TSplitter;
    procedure cbbRightEdgeChange(Sender: TObject);
    procedure cbScrollPastEolChange(Sender: TObject);
    procedure clrbCommentChange(Sender: TObject);
    procedure clrbEditorColorChange(Sender: TObject);
    procedure clrbIdentifierChange(Sender: TObject);
    procedure clrbKeyChange(Sender: TObject);
    procedure clrbNumberChange(Sender: TObject);
    procedure clrbRightEdgeColorChange(Sender: TObject);
    procedure clrbSelectedColorChange(Sender: TObject);
    procedure clrbStringChange(Sender: TObject);
    procedure clrbSymbolChange(Sender: TObject);
    procedure edtbtnSchemeFileButtonClick(Sender: TObject);
    procedure edtbtnSchemeFileChange(Sender: TObject);
    procedure edtbtnSelectFontButtonClick(Sender: TObject);
    procedure edtbtnSelectFontChange(Sender: TObject);
    procedure sedtFontSizeChange(Sender: TObject);
  private
    { private declarations }
    procedure GetColorScheme;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  uEditorConfig,
  uIDEConfig;

{$R *.lfm}

{ TEditorOptionsFrame }

constructor TEditorOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  edtbtnSelectFont.Text := EditorConfig.FontName;
  cbbRightEdge.Caption := IntToStr(EditorConfig.RightEdge);
  cbScrollPastEol.Checked := EditorConfig.ScrollPastEol;
  sedtFontSize.Value := EditorConfig.FontSize;
  GetColorScheme;
end;

procedure TEditorOptionsFrame.GetColorScheme;
begin
  edtbtnSchemeFile.Text := IDEConfig.ColorSchemeFile;
  edtbtnSchemeFile.Hint := edtbtnSchemeFile.Text;
  clrbComment.Selected := EditorConfig.CommentColor;
  clrbEditorColor.Selected := EditorConfig.EditorColor;
  clrbIdentifier.Selected := EditorConfig.IdentifierColor;
  clrbKey.Selected := EditorConfig.KeyColor;
  clrbNumber.Selected := EditorConfig.NumberColor;
  clrbRightEdgeColor.Selected := EditorConfig.RightEdgeColor;
  clrbSelectedColor.Selected := EditorConfig.SelectedColor;
  clrbString.Selected := EditorConfig.StringColor;
  clrbSymbol.Selected := EditorConfig.SymbolColor;
end;

procedure TEditorOptionsFrame.cbbRightEdgeChange(Sender: TObject);
begin
  EditorConfig.RightEdge := StrToInt(cbbRightEdge.Caption);
end;

procedure TEditorOptionsFrame.cbScrollPastEolChange(Sender: TObject);
begin
  EditorConfig.ScrollPastEol := cbScrollPastEol.Checked;
end;

procedure TEditorOptionsFrame.clrbCommentChange(Sender: TObject);
begin
  EditorConfig.CommentColor := clrbComment.Selected;
end;

procedure TEditorOptionsFrame.clrbEditorColorChange(Sender: TObject);
begin
  EditorConfig.EditorColor := clrbEditorColor.Selected;
end;

procedure TEditorOptionsFrame.clrbIdentifierChange(Sender: TObject);
begin
  EditorConfig.IdentifierColor := clrbIdentifier.Selected;
end;

procedure TEditorOptionsFrame.clrbKeyChange(Sender: TObject);
begin
  EditorConfig.KeyColor := clrbKey.Selected;
end;

procedure TEditorOptionsFrame.clrbNumberChange(Sender: TObject);
begin
  EditorConfig.NumberColor := clrbNumber.Selected;
end;

procedure TEditorOptionsFrame.clrbRightEdgeColorChange(Sender: TObject);
begin
  EditorConfig.RightEdgeColor := clrbRightEdgeColor.Selected;
end;

procedure TEditorOptionsFrame.clrbSelectedColorChange(Sender: TObject);
begin
  EditorConfig.SelectedColor := clrbSelectedColor.Selected;
end;

procedure TEditorOptionsFrame.clrbStringChange(Sender: TObject);
begin
  EditorConfig.StringColor := clrbString.Selected;
end;

procedure TEditorOptionsFrame.clrbSymbolChange(Sender: TObject);
begin
  EditorConfig.SymbolColor := clrbSymbol.Selected;
end;

procedure TEditorOptionsFrame.edtbtnSchemeFileButtonClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  begin
    try
      if Execute then
        if EditorConfig.LoadColorScheme(FileName) then
        begin
          edtbtnSchemeFile.Text := FileName;
          GetColorScheme;
        end;
    finally
      Free;
    end;
  end;
end;

procedure TEditorOptionsFrame.edtbtnSchemeFileChange(Sender: TObject);
begin
  IDEConfig.ColorSchemeFile := edtbtnSchemeFile.Text;
end;

procedure TEditorOptionsFrame.edtbtnSelectFontButtonClick(Sender: TObject);
begin
  with TFontDialog.Create(Self) do
  begin
    try
      if Execute then
      begin
        edtbtnSelectFont.Text := Font.Name;
        sedtFontSize.Value := Font.Size;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TEditorOptionsFrame.edtbtnSelectFontChange(Sender: TObject);
begin
  EditorConfig.FontName := edtbtnSelectFont.Text;
end;

procedure TEditorOptionsFrame.sedtFontSizeChange(Sender: TObject);
begin
  EditorConfig.FontSize := sedtFontSize.Value;
end;

end.

