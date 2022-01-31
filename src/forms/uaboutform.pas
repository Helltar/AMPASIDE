{-------------------------------------------------------------------------------

Copyright (C) 2015-2022 Helltar <mail@helltar.com>

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

unit uAboutForm;

{$mode objfpc}{$H+}

interface

uses
  ButtonPanel, Graphics, ExtCtrls, Forms, StdCtrls, LCLIntf;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnpnlMain: TButtonPanel;
    imgLogo: TImage;
    lblGitHubURL: TLabel;
    memAbout: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure lblURLClick(Sender: TObject);
    procedure lblURLMouseEnter(Sender: TObject);
    procedure lblURLMouseLeave(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

uses
  uAMPASCore;

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Caption := 'О проекте ' + APP_NAME;

  Height := 252;
  Width := 340;

  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;

  lblGitHubURL.Caption := URL_SRC_CODE;

  memAbout.Lines.Text :=
    'Advanced MIDletPascal IDE' + LE +
    'Автор: Helltar' + LE +
    'Версия: ' + GetProgramVersion + LE +
    'Лицензия: GPLv3 (см. COPYING)';
end;

procedure TfrmAbout.lblURLClick(Sender: TObject);
begin
  OpenURL(TLabel(Sender).Caption);
end;

procedure TfrmAbout.lblURLMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsUnderline];
end;

procedure TfrmAbout.lblURLMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [];
end;

procedure TfrmAbout.OKButtonClick(Sender: TObject);
begin
  Close;
end;

end.

