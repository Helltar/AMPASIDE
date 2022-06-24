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

unit uNewProjForm;

{$mode objfpc}{$H+}

interface

uses
  ButtonPanel, Controls, Dialogs, EditBtn, FileUtil, SysUtils, Forms, StdCtrls;

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

resourcestring
  CAPTION_CONFIRM_ACTION = 'Confirm the action';
  MSG_ENTER_PROJNAME = 'Enter project name';
  MSG_PROJ_NAME_MUST_LEAST = 'Project name must be at least 3 characters long';
  MSG_PROJ_EXISTS = 'Project with this name already exists, overwrite it?';
  MSG_SELECT_SAVE_DIR = 'Select a save directory';

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
    MessageDlg(ERR_EMPTY_VALUE, MSG_ENTER_PROJNAME, mtError, [mbOK], 0)
  else
  if Length(AName) < 3 then
    MessageDlg(ERR_BAD_VALUE, MSG_PROJ_NAME_MUST_LEAST, mtWarning, [mbOK], 0)
  else
    Result := True;
end;

function TfrmNewProj.IsPathValid(const ParentPath, ProjPath: string): boolean;
begin
  Result := False;

  if ParentPath = '' then
    MessageDlg(ERR_EMPTY_VALUE, MSG_SELECT_SAVE_DIR, mtError, [mbOK], 0)
  else
  if not DirectoryExists(ParentPath) then
    MessageDlg(ERR_ERR, ERR_DIR_404, mtError, [mbOK], 0)
  else
  if DirectoryExists(ProjPath) then
  begin
    case MessageDlg(CAPTION_CONFIRM_ACTION, MSG_PROJ_EXISTS,
        mtWarning, [mbYes, mbNo], 0) of
      mrYes:
      begin
        if DeleteDirectory(ProjPath, False) then
          Result := True
        else
          MessageDlg(ERR_ERR, ERR_DEL_DIR + ': ' + ProjPath, mtError, [mbOK], 0);
      end;
    end;
  end
  else
    Result := True;
end;

end.
