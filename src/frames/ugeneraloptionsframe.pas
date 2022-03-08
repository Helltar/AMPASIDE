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

unit uGeneralOptionsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, Spin, SynEdit;

type

  { TGeneralOptionsFrame }

  TGeneralOptionsFrame = class(TFrame)
    cbPrevStretch: TCheckBox;
    edtModulePrefix: TEdit;
    edtEmulatorCmd: TEdit;
    gbEditorHeaders: TGroupBox;
    gbEmulator: TGroupBox;
    gbFileManager: TGroupBox;
    gbModulePrefix: TGroupBox;
    lblCmdLine: TLabel;
    lblPrevHeight: TLabel;
    lblPrevWidth: TLabel;
    sedtY: TSpinEdit;
    sedtX: TSpinEdit;
    synedtHeaders: TSynEdit;
    procedure cbPrevStretchChange(Sender: TObject);
    procedure edtModulePrefixChange(Sender: TObject);
    procedure edtEmulatorCmdChange(Sender: TObject);
    procedure sedtXChange(Sender: TObject);
    procedure sedtYChange(Sender: TObject);
    procedure synedtHeadersChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  uEditorConfig,
  uIDEConfig;

{$R *.lfm}

{ TGeneralOptionsFrame }

constructor TGeneralOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  synedtHeaders.Font.Name := EditorConfig.FontName;
  synedtHeaders.Font.Size := EditorConfig.FontSize;
  synedtHeaders.Text := EditorConfig.EditorHeadersText;
  cbPrevStretch.Checked := IDEConfig.FilManPrevStretch;
  edtEmulatorCmd.Font.Name := EditorConfig.FontName;
  edtEmulatorCmd.Font.Size := EditorConfig.FontSize;
  edtEmulatorCmd.Text := IDEConfig.EmulatorCmd;
  edtModulePrefix.Font.Name := EditorConfig.FontName;
  edtModulePrefix.Font.Size := EditorConfig.FontSize;
  edtModulePrefix.Text := IDEConfig.ModulePrefix;
  sedtX.Value := IDEConfig.FilManPrevSizeX;
  sedtY.Value := IDEConfig.FilManPrevSizeY;
end;

procedure TGeneralOptionsFrame.edtEmulatorCmdChange(Sender: TObject);
begin
  IDEConfig.EmulatorCmd := edtEmulatorCmd.Text;
end;

procedure TGeneralOptionsFrame.cbPrevStretchChange(Sender: TObject);
begin
  IDEConfig.FilManPrevStretch := cbPrevStretch.Checked;
end;

procedure TGeneralOptionsFrame.edtModulePrefixChange(Sender: TObject);
begin
  IDEConfig.ModulePrefix := edtModulePrefix.Text;
end;

procedure TGeneralOptionsFrame.sedtXChange(Sender: TObject);
begin
  IDEConfig.FilManPrevSizeX := sedtX.Value;
end;

procedure TGeneralOptionsFrame.sedtYChange(Sender: TObject);
begin
  IDEConfig.FilManPrevSizeY := sedtY.Value;
end;

procedure TGeneralOptionsFrame.synedtHeadersChange(Sender: TObject);
begin
  EditorConfig.EditorHeadersText := synedtHeaders.Text;
end;

end.

