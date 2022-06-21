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

unit uProjectOptionsFrame;

{$mode objfpc}{$H+}

interface

uses
  Forms, Spin, StdCtrls, Classes, LCLIntf,
  uAMPASCore;

type

  { TProjectOptionsFrame }

  TProjectOptionsFrame = class(TFrame)
    btnMoarMoarMoar111: TButton;
    cbAutoIncBuildVer: TCheckBox;
    cbbCanvasType: TComboBox;
    cbbMathType: TComboBox;
    edtAPackage: TEdit;
    edtMName: TEdit;
    edtMVendor: TEdit;
    edtMDesc: TEdit;
    edtMIcon: TEdit;
    gbManifest: TGroupBox;
    gbVersions: TGroupBox;
    gbConfiguration: TGroupBox;
    gbAndroidManifest: TGroupBox;
    lblAndroidManifestFile: TLabel;
    lblAPackage: TLabel;
    lblMName: TLabel;
    lblMVendor: TLabel;
    lblMDesc: TLabel;
    lblMIcon: TLabel;
    lblVMajor: TLabel;
    lblVMinor: TLabel;
    lblVBuild: TLabel;
    lblMType: TLabel;
    lblMathType: TLabel;
    sedtVMajor: TSpinEdit;
    sedtVMinor: TSpinEdit;
    sedtVBuild: TSpinEdit;
    procedure btnMoarMoarMoar111Click(Sender: TObject);
    procedure cbAutoIncBuildVerChange(Sender: TObject);
    procedure cbbCanvasTypeChange(Sender: TObject);
    procedure cbbMathTypeChange(Sender: TObject);
    procedure edtAPackageChange(Sender: TObject);
    procedure edtMDescChange(Sender: TObject);
    procedure edtMIconChange(Sender: TObject);
    procedure edtMNameChange(Sender: TObject);
    procedure edtMVendorChange(Sender: TObject);
    procedure gbAndroidManifestClick(Sender: TObject);
    procedure lblAndroidManifestFileClick(Sender: TObject);
    procedure sedtVBuildChange(Sender: TObject);
    procedure sedtVMajorChange(Sender: TObject);
    procedure sedtVMinorChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure GetSettings;
    procedure UpdateVers;
  end;

implementation

uses
  uManifestMfForm,
  uProjectConfig,
  uProjectManager;

{$R *.lfm}

{ TProjectOptionsFrame }

procedure TProjectOptionsFrame.cbAutoIncBuildVerChange(Sender: TObject);
begin
  ProjConfig.AutoIncBuildVers := cbAutoIncBuildVer.Checked;
end;

procedure TProjectOptionsFrame.btnMoarMoarMoar111Click(Sender: TObject);
begin
  with TfrmManifestMf.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TProjectOptionsFrame.cbbCanvasTypeChange(Sender: TObject);
begin
  ProjConfig.CanvasType := cbbCanvasType.ItemIndex;
end;

procedure TProjectOptionsFrame.cbbMathTypeChange(Sender: TObject);
begin
  ProjConfig.MathType := cbbMathType.ItemIndex;
end;

procedure TProjectOptionsFrame.edtAPackageChange(Sender: TObject);
begin
  ProjConfig.APackage := edtAPackage.Text;
end;

procedure TProjectOptionsFrame.edtMDescChange(Sender: TObject);
begin
  ProjConfig.MIDletDescription := edtMDesc.Text;
end;

procedure TProjectOptionsFrame.edtMIconChange(Sender: TObject);
begin
  ProjConfig.MIDletIcon := edtMIcon.Text;
end;

procedure TProjectOptionsFrame.edtMNameChange(Sender: TObject);
begin
  ProjConfig.MIDletName := edtMName.Text;
end;

procedure TProjectOptionsFrame.edtMVendorChange(Sender: TObject);
begin
  ProjConfig.MIDletVendor := edtMVendor.Text;
end;

procedure TProjectOptionsFrame.gbAndroidManifestClick(Sender: TObject);
begin

end;

procedure TProjectOptionsFrame.lblAndroidManifestFileClick(Sender: TObject);
begin
  OpenURL(ProjManager.ProjDirHome + ANDROID_MANIFEST);
end;

procedure TProjectOptionsFrame.sedtVBuildChange(Sender: TObject);
begin
  ProjConfig.VersBuild := sedtVBuild.Value;
end;

procedure TProjectOptionsFrame.sedtVMajorChange(Sender: TObject);
begin
  ProjConfig.VersMajor := sedtVMajor.Value;
end;

procedure TProjectOptionsFrame.sedtVMinorChange(Sender: TObject);
begin
  ProjConfig.VersMinor := sedtVMinor.Value;
end;

procedure TProjectOptionsFrame.UpdateVers;
begin
  sedtVMajor.Value := ProjConfig.VersMajor;
  sedtVMinor.Value := ProjConfig.VersMinor;
  sedtVBuild.Value := ProjConfig.VersBuild;
end;

procedure TProjectOptionsFrame.GetSettings;
begin
  cbAutoIncBuildVer.Checked := ProjConfig.AutoIncBuildVers;
  cbbCanvasType.ItemIndex := ProjConfig.CanvasType;
  cbbMathType.ItemIndex := ProjConfig.MathType;
  edtAPackage.Text := ProjConfig.APackage;
  edtMDesc.Text := ProjConfig.MIDletDescription;
  edtMIcon.Text := ProjConfig.MIDletIcon;
  edtMName.Text := ProjConfig.MIDletName;
  edtMVendor.Text := ProjConfig.MIDletVendor;
  lblAndroidManifestFile.Caption := ANDROID_MANIFEST;
  lblAndroidManifestFile.Hint := GetAppPath + APP_DIR_CONFIG + ANDROID_MANIFEST;
  UpdateVers;
end;

end.
