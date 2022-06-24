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

unit uIDEOptionsForm;

{$mode objfpc}{$H+}

interface

uses
  ButtonPanel, ComCtrls, Controls, Forms;

type

  { TfrmIDEOptions }

  TfrmIDEOptions = class(TForm)
    btnpnlMain: TButtonPanel;
    pgcMain: TPageControl;
    tsEditor: TTabSheet;
    tsGeneral: TTabSheet;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

uses
  uMainForm,
  uIDEConfig,
  uEditorOptionsFrame,
  uGeneralOptionsFrame;

var
  EditorOptionsFrame: TEditorOptionsFrame;
  GeneralOptionsFrame: TGeneralOptionsFrame;

{$R *.lfm}

{ TfrmIDEOptions }

procedure TfrmIDEOptions.FormCreate(Sender: TObject);
begin
  Color := IDEConfig.FColor;

  Height := 700;
  Width := 1024;

  EditorOptionsFrame := TEditorOptionsFrame.Create(tsEditor);
  EditorOptionsFrame.Parent := tsEditor;
  EditorOptionsFrame.Align := alClient;

  GeneralOptionsFrame := TGeneralOptionsFrame.Create(tsGeneral);
  GeneralOptionsFrame.Parent := tsGeneral;
  GeneralOptionsFrame.Align := alClient;
end;

procedure TfrmIDEOptions.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmIDEOptions.OKButtonClick(Sender: TObject);
begin
  frmMain.UpdateSettings;
  Close;
end;

end.

