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

unit uCodeEditorDM;

{$mode objfpc}{$H+}

interface

uses
  ActnList, Classes, Menus, StdActns, Controls, SynHighlighterPas, SynHighlighterJava,
  SynHighlighterHTML, SynHighlighterPHP;

type

  { TdmCodeEditor }

  TdmCodeEditor = class(TDataModule)
    actlEditorMenu: TActionList;
    edtCopy: TEditCopy;
    edtCut: TEditCut;
    edtPaste: TEditPaste;
    edtSelectAll: TEditSelectAll;
    ilEditor: TImageList;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    MenuItem4: TMenuItem;
    miSelectAll: TMenuItem;
    pmEditor: TPopupMenu;
    synHTMLHighlighter: TSynHTMLSyn;
    synJavaHighlighter: TSynJavaSyn;
    synPasHighlighter: TSynPasSyn;
    synPHPHighlighter: TSynPHPSyn;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure LoadHighlighter(const FileName: string);
  end;

var
  dmCodeEditor: TdmCodeEditor;

implementation

uses
  uAMPASCore,
  uIDEConfig;

{$R *.lfm}

{ TdmCodeEditor }

procedure TdmCodeEditor.DataModuleCreate(Sender: TObject);
begin
  if LoadImages('editor', ilEditor) then
  begin
    edtCopy.ImageIndex := 0;
    edtCut.ImageIndex := 1;
    edtPaste.ImageIndex := 2;
    edtSelectAll.ImageIndex := 4;
  end;
end;

procedure TdmCodeEditor.DataModuleDestroy(Sender: TObject);
begin
  synPasHighlighter.SaveToFile(IDEConfig.ColorSchemeFile);
end;

procedure TdmCodeEditor.LoadHighlighter(const FileName: string);
begin
  synPasHighlighter.LoadFromFile(FileName);
end;

end.

