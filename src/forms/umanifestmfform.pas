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

unit uManifestMfForm;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, ButtonPanel, SynEdit, SynHighlighterAny;

type

  { TfrmManifestMf }

  TfrmManifestMf = class(TForm)
    btnpnlMain: TButtonPanel;
    cbExtraOptions: TCheckBox;
    edtlMInfoURL: TEdit;
    edtMDelConfirm: TEdit;
    edtMInstallNotify: TEdit;
    lblMInfoURL: TLabel;
    lblMDelConfirm: TLabel;
    lblMInstallNotify: TLabel;
    synAnyHighlighter: TSynAnySyn;
    synedtExtraOptions: TSynEdit;
    procedure cbExtraOptionsChange(Sender: TObject);
    procedure edtlMInfoURLChange(Sender: TObject);
    procedure edtMDelConfirmChange(Sender: TObject);
    procedure edtMInstallNotifyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure synedtExtraOptionsChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

uses
  uEditorConfig,
  uProjectConfig;

{$R *.lfm}

{ TfrmManifestMf }

procedure TfrmManifestMf.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmManifestMf.synedtExtraOptionsChange(Sender: TObject);
begin
  ProjConfig.MnfExtraOptions := synedtExtraOptions.Text;
end;

procedure TfrmManifestMf.FormCreate(Sender: TObject);
begin
  Height := 420;
  Width := 500;

  cbExtraOptions.Checked := ProjConfig.MnfExtraOptionsEnabled;
  edtlMInfoURL.Text := ProjConfig.MIDletInfoURL;
  edtMDelConfirm.Text := ProjConfig.MIDletDeleteConfirm;
  edtMInstallNotify.Text := ProjConfig.MIDletInstallNotify;

  synedtExtraOptions.Enabled := ProjConfig.MnfExtraOptionsEnabled;
  synedtExtraOptions.Font.Name := EditorConfig.FontName;
  synedtExtraOptions.Font.Size := EditorConfig.FontSize;
  synedtExtraOptions.Text := ProjConfig.MnfExtraOptions;
end;

procedure TfrmManifestMf.cbExtraOptionsChange(Sender: TObject);
begin
  ProjConfig.MnfExtraOptionsEnabled := cbExtraOptions.Checked;
  synedtExtraOptions.Enabled := cbExtraOptions.Checked;
end;

procedure TfrmManifestMf.edtlMInfoURLChange(Sender: TObject);
begin
  ProjConfig.MIDletInfoURL := edtlMInfoURL.Text;
end;

procedure TfrmManifestMf.edtMDelConfirmChange(Sender: TObject);
begin
  ProjConfig.MIDletDeleteConfirm := edtMDelConfirm.Text;
end;

procedure TfrmManifestMf.edtMInstallNotifyChange(Sender: TObject);
begin
  ProjConfig.MIDletInstallNotify := edtMInstallNotify.Text;
end;

end.

