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

unit uIDEMacrosForm;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel, Menus, SynEdit, SynHighlighterPas;

type

  { TfrmIDEMacros }

  TfrmIDEMacros = class(TForm)
    btnpnlMain: TButtonPanel;
    miCopy: TMenuItem;
    pmMain: TPopupMenu;
    synedtMacros: TSynEdit;
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
  uEditorConfig,
  uIDEConfig;

{$R *.lfm}

{ TfrmIDEMacros }

procedure TfrmIDEMacros.FormCreate(Sender: TObject);
var
  APKFileName: string;
  JARFileName, JADFileName: string;
  ProjPath: string;

begin
  Height := 320;
  Width := 700;

  synedtMacros.Font.Name := EditorConfig.FontName;
  synedtMacros.Font.Size := EditorConfig.FontSize;

  APKFileName := IDEConfig.MacrosReplace(M_APK_FILENAME);
  JADFileName := IDEConfig.MacrosReplace(M_JAD_FILENAME);
  JARFileName := IDEConfig.MacrosReplace(M_JAR_FILENAME);
  ProjPath := IDEConfig.MacrosReplace(M_PROJ_PATH);

  if APKFileName = '' then
    APKFileName := 'абсолютный путь к .apk';

  if JADFileName = '' then
    JADFileName := 'абсолютный путь к .jad';

  if JARFileName = '' then
    JARFileName := 'абсолютный путь к .jar';

  if ProjPath = '' then
    ProjPath := 'абсолютный путь к домашнему каталогу проекта ' + DIR_SEP;

  with IDEConfig do
    synedtMacros.Text := (
      '//--------- IDE ---------' + LE + LE +
      M_APP_NAME + ' - ' + MacrosReplace(M_APP_NAME) + LE +
      M_APP_VERSION + ' - ' + MacrosReplace(M_APP_VERSION) + LE + LE +
      '//--------- Проект ---------' + LE + LE +
      M_PROJ_PATH + ' - ' + ProjPath + LE +
      M_APK_FILENAME + ' - ' + APKFileName + LE +
      M_JAD_FILENAME + ' - ' + JADFileName + LE +
      M_JAR_FILENAME + ' - ' + JARFileName + LE + LE +
      '//--------- Другое ---------' + LE + LE +
      M_DATE_TIME + ' - ' + MacrosReplace(M_DATE_TIME) + LE +
      M_USERNAME + ' - ' + MacrosReplace(M_USERNAME)
      );
end;

procedure TfrmIDEMacros.miCopyClick(Sender: TObject);
begin
  synedtMacros.CopyToClipboard;
end;

procedure TfrmIDEMacros.OKButtonClick(Sender: TObject);
begin
  Close;
end;

end.

