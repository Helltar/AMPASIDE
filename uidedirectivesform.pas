{-------------------------------------------------------------------------------

Copyright (C) 2015 Taras Adamchuk <helltar.live@gmail.com>

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

unit uIDEDirectivesForm;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel, Menus, SynEdit, SynHighlighterPas, Classes;

type

  { TfrmIDEDirectives }

  TfrmIDEDirectives = class(TForm)
    btnpnlMain: TButtonPanel;
    miCopy: TMenuItem;
    pmMain: TPopupMenu;
    synedtDirectives: TSynEdit;
    synPasHighlighter: TSynPasSyn;
    procedure FormCreate(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

uses
  uAMPASCore,
  uProjectManager,
  uEditorConfig,
  uIDEConfig;

{$R *.lfm}

{ TfrmIDEDirectives }

procedure TfrmIDEDirectives.FormCreate(Sender: TObject);
var
  JARFileName: string;

begin
  Height := 320;
  Width := 700;

  synedtDirectives.Font.Name := EditorConfig.FontName;
  synedtDirectives.Font.Size := EditorConfig.FontSize;

  JARFileName := IDEConfig.DirectiveReplace('{$JAR_FILENAME}');

  if ProjManager.JARFile = '' then
  {$IFDEF UNIX}
    JARFileName := '/home/user/development/myproject/bin/myproject.jar';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  JARFileName := 'C:\development\myproject\bin\myproject.jar';
  {$ENDIF}

  { TODO : в константы }
  synedtDirectives.Text := (
    '{$APP_NAME} - ' + IDEConfig.DirectiveReplace('{$APP_NAME}') + LE +
    '{$APP_VERSION} - ' + IDEConfig.DirectiveReplace('{$APP_VERSION}') + LE +
    '{$DATE_TIME} - ' + IDEConfig.DirectiveReplace('{$DATE_TIME}') + LE +
    '{$JAR_FILENAME} - ' + JARFileName + LE +
    '{$USERNAME} - ' + IDEConfig.DirectiveReplace('{$USERNAME}'));
end;

procedure TfrmIDEDirectives.miCopyClick(Sender: TObject);
begin
  synedtDirectives.CopyToClipboard;
end;

procedure TfrmIDEDirectives.OKButtonClick(Sender: TObject);
begin
  Close;
end;

end.

