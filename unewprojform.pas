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

unit uNewProjForm;

{$mode objfpc}{$H+}

interface

uses
  ButtonPanel, Controls, Dialogs, EditBtn, FileUtil, SysUtils, Forms, StdCtrls,
  ExtCtrls;

type

  { TfrmNewProj }

  TfrmNewProj = class(TForm)
    btnpnlMain: TButtonPanel;
    diredtPath: TDirectoryEdit;
    edtName: TEdit;
    lblName: TLabel;
    lblPath: TLabel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    function IsNameValid(const AName: string): boolean;
    function IsPathValid(const ParentPath, ProjPath: string): boolean;
  public
    { public declarations }
  end;

implementation

uses
  uAMPASCore,
  uProjectManager,
  uMainForm;

{$R *.lfm}

{ TfrmNewProj }

procedure TfrmNewProj.FormCreate(Sender: TObject);
begin
  Height := 180;
  Width := 320;
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

procedure TfrmNewProj.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmNewProj.OKButtonClick(Sender: TObject);
var
  ProjPath: string;

begin
  ProjPath := diredtPath.Text + DIR_SEP + edtName.Text;
  if IsNameValid(edtName.Text) then
    if IsPathValid(diredtPath.Text, ProjPath) then
    begin
      if ProjManager.CreateProject(diredtPath.Text, edtName.Text) then
        frmMain.LoadFile(ProjManager.ConfigFile);
      Close;
    end;
end;

function TfrmNewProj.IsNameValid(const AName: string): boolean;
begin
  Result := False;

  if AName = '' then
    MessageDlg('Пустое значение', 'Введите название проекта', mtError, [mbOK], 0)
  else
  if Length(AName) < 3 then
    MessageDlg('Неверное значение', 'Название проекта должно состоять минимум из 3-х символов', mtWarning, [mbOK], 0)
  else
    Result := True;
end;

function TfrmNewProj.IsPathValid(const ParentPath, ProjPath: string): boolean;
begin
  Result := False;

  if ParentPath = '' then
    MessageDlg('Пустое значение', 'Выберите каталог сохранения', mtError, [mbOK], 0)
  else
  if not DirectoryExists(ParentPath) then
    MessageDlg('Ошибка', 'Каталога не существует', mtError, [mbOK], 0)
  else
  if DirectoryExists(ProjPath) then
  begin
    case MessageDlg('Подтвердите действие', 'Проект с таким именем уже существует на диске, перезаписать его?',
        mtWarning, [mbYes, mbNo], 0) of
      mrYes:
      begin
        if DeleteDirectory(ProjPath, False) then
          Result := True
        else
          MessageDlg('Ошибка', 'Не удалось удалить каталог: ' + ProjPath, mtError, [mbOK], 0);
      end;
    end;
  end
  else
    Result := True;
end;

end.

