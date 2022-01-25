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

unit uProjectConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IniFiles;

type

  { TProjectConfig }

  TProjectConfig = class
  private
    ProjIniFile: TIniFile;
    function GetAPackage: string;
    function GetAutoIncBuildVers: boolean;
    function GetCanvasType: integer;
    function GetConfigFileName: string;
    function GetExtraOptionsFileName: string;
    function GetMainModuleName: string;
    function GetMathType: integer;
    function GetMIDletDeleteConfirm: string;
    function GetMIDletDescription: string;
    function GetMIDletIcon: string;
    function GetMIDletInfoURL: string;
    function GetMIDletInstallNotify: string;
    function GetMIDletName: string;
    function GetMIDletVendor: string;
    function GetMnfExtraOptions: string;
    function GetMnfExtraOptionsEnabled: boolean;
    function GetVersBuild: integer;
    function GetVersMajor: integer;
    function GetVersMinor: integer;
    procedure SetAPackage(AValue: string);
    procedure SetAutoIncBuildVers(AValue: boolean);
    procedure SetCanvasType(AValue: integer);
    procedure SetConfigFileName(AValue: string);
    procedure SetMainModuleName(AValue: string);
    procedure SetMathType(AValue: integer);
    procedure SetMIDletDeleteConfirm(AValue: string);
    procedure SetMIDletDescription(AValue: string);
    procedure SetMIDletIcon(AValue: string);
    procedure SetMIDletInfoURL(AValue: string);
    procedure SetMIDletInstallNotify(AValue: string);
    procedure SetMIDletName(AValue: string);
    procedure SetMIDletVendor(AValue: string);
    procedure SetMnfExtraOptions(AValue: string);
    procedure SetMnfExtraOptionsEnabled(AValue: boolean);
    procedure SetVersBuild(AValue: integer);
    procedure SetVersMajor(AValue: integer);
    procedure SetVersMinor(AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;

    property ConfigFileName: string read GetConfigFileName write SetConfigFileName;

    property CanvasType: integer read GetCanvasType write SetCanvasType;
    property MainModuleName: string read GetMainModuleName write SetMainModuleName;
    property MathType: integer read GetMathType write SetMathType;

    property MIDletDeleteConfirm: string read GetMIDletDeleteConfirm write SetMIDletDeleteConfirm;
    property MIDletDescription: string read GetMIDletDescription write SetMIDletDescription;
    property MIDletIcon: string read GetMIDletIcon write SetMIDletIcon;
    property MIDletInfoURL: string read GetMIDletInfoURL write SetMIDletInfoURL;
    property MIDletInstallNotify: string read GetMIDletInstallNotify write SetMIDletInstallNotify;
    property MIDletName: string read GetMIDletName write SetMIDletName;
    property MIDletVendor: string read GetMIDletVendor write SetMIDletVendor;
    property MnfExtraOptions: string read GetMnfExtraOptions write SetMnfExtraOptions;
    property MnfExtraOptionsEnabled: boolean read GetMnfExtraOptionsEnabled write SetMnfExtraOptionsEnabled;

    // android
    property APackage: string read GetAPackage write SetAPackage;

    property AutoIncBuildVers: boolean read GetAutoIncBuildVers write SetAutoIncBuildVers;
    property VersBuild: integer read GetVersBuild write SetVersBuild;
    property VersMajor: integer read GetVersMajor write SetVersMajor;
    property VersMinor: integer read GetVersMinor write SetVersMinor;
  end;

var
  ProjConfig: TProjectConfig;

implementation

uses
  uAMPASCore;

{ TProjectConfig }

constructor TProjectConfig.Create;
begin
  ProjIniFile := TIniFile.Create('');
end;

destructor TProjectConfig.Destroy;
begin
  FreeAndNil(ProjIniFile);
  inherited Destroy;
end;

function TProjectConfig.GetAutoIncBuildVers: boolean;
begin
  Result := ProjIniFile.ReadBool('VERSIONS', 'AutoIncBuildVers', True);
end;

function TProjectConfig.GetAPackage: string;
begin
  Result := ProjIniFile.ReadString('AMANIFEST', 'Package', 'org.microemu.android.' + MIDletName);
end;

function TProjectConfig.GetCanvasType: integer;
begin
  Result := ProjIniFile.ReadInteger('MAIN', 'CanvasType', 1);
end;

function TProjectConfig.GetConfigFileName: string;
begin
  Result := ProjIniFile.FileName;
end;

function TProjectConfig.GetMainModuleName: string;
begin
  Result := ProjIniFile.ReadString('MAIN', 'MainModule', '');
end;

function TProjectConfig.GetMathType: integer;
begin
  Result := ProjIniFile.ReadInteger('MAIN', 'MathType', 0);
end;

function TProjectConfig.GetMIDletDeleteConfirm: string;
begin
  Result := ProjIniFile.ReadString('MANIFEST', 'MIDletDeleteConfirm', '');
end;

function TProjectConfig.GetMIDletDescription: string;
begin
  Result := ProjIniFile.ReadString('MANIFEST', 'MIDletDescription', '');
end;

function TProjectConfig.GetMIDletIcon: string;
begin
  Result := ProjIniFile.ReadString('MANIFEST', 'MIDletIcon', '/icon.png');
end;

function TProjectConfig.GetMIDletInfoURL: string;
begin
  Result := ProjIniFile.ReadString('MANIFEST', 'MIDletInfoURL', '');
end;

function TProjectConfig.GetMIDletInstallNotify: string;
begin
  Result := ProjIniFile.ReadString('MANIFEST', 'MIDletInstallNotify', '');
end;

function TProjectConfig.GetMIDletName: string;
begin
  Result := ProjIniFile.ReadString('MANIFEST', 'MIDletName', '');
end;

function TProjectConfig.GetMIDletVendor: string;
begin
  Result := ProjIniFile.ReadString('MANIFEST', 'MIDletVendor', GetCurrentUserName);
end;

function TProjectConfig.GetMnfExtraOptions: string;
begin
  if FileExists(GetExtraOptionsFileName) then
    with TStringList.Create do
      try
        LoadFromFile(GetExtraOptionsFileName);
        Result := Text;
      finally
        Free;
      end;
end;

function TProjectConfig.GetMnfExtraOptionsEnabled: boolean;
begin
  Result := ProjIniFile.ReadBool('MANIFEST', 'ExtraOptionsEnabled', False);
end;

function TProjectConfig.GetVersBuild: integer;
begin
  Result := ProjIniFile.ReadInteger('VERSIONS', 'VersBuild', 0);
end;

function TProjectConfig.GetVersMajor: integer;
begin
  Result := ProjIniFile.ReadInteger('VERSIONS', 'VersMajor', 1);
end;

function TProjectConfig.GetVersMinor: integer;
begin
  Result := ProjIniFile.ReadInteger('VERSIONS', 'VersMinor', 0);
end;

procedure TProjectConfig.SetAPackage(AValue: string);
begin
  ProjIniFile.WriteString('AMANIFEST', 'Package', AValue);
end;

procedure TProjectConfig.SetAutoIncBuildVers(AValue: boolean);
begin
  ProjIniFile.WriteBool('VERSIONS', 'AutoIncBuildVers', AValue);
end;

procedure TProjectConfig.SetCanvasType(AValue: integer);
begin
  ProjIniFile.WriteInteger('MAIN', 'CanvasType', AValue);
end;

procedure TProjectConfig.SetConfigFileName(AValue: string);
begin
  FreeAndNil(ProjIniFile);
  ProjIniFile := TIniFile.Create(AValue);
end;

procedure TProjectConfig.SetMainModuleName(AValue: string);
begin
  ProjIniFile.WriteString('MAIN', 'MainModule', AValue);
end;

procedure TProjectConfig.SetMathType(AValue: integer);
begin
  ProjIniFile.WriteInteger('MAIN', 'MathType', AValue);
end;

procedure TProjectConfig.SetMIDletDeleteConfirm(AValue: string);
begin
  ProjIniFile.WriteString('MANIFEST', 'MIDletDeleteConfirm', AValue);
end;

procedure TProjectConfig.SetMIDletDescription(AValue: string);
begin
  ProjIniFile.WriteString('MANIFEST', 'MIDletDescription', AValue);
end;

procedure TProjectConfig.SetMIDletIcon(AValue: string);
begin
  ProjIniFile.WriteString('MANIFEST', 'MIDletIcon', AValue);
end;

procedure TProjectConfig.SetMIDletInfoURL(AValue: string);
begin
  ProjIniFile.WriteString('MANIFEST', 'MIDletInfoURL', AValue);
end;

procedure TProjectConfig.SetMIDletInstallNotify(AValue: string);
begin
  ProjIniFile.WriteString('MANIFEST', 'MIDletInstallNotify', AValue);
end;

procedure TProjectConfig.SetMIDletName(AValue: string);
begin
  ProjIniFile.WriteString('MANIFEST', 'MIDletName', AValue);
end;

procedure TProjectConfig.SetMIDletVendor(AValue: string);
begin
  ProjIniFile.WriteString('MANIFEST', 'MIDletVendor', AValue);
end;

procedure TProjectConfig.SetMnfExtraOptions(AValue: string);
begin
  with TStringList.Create do
    try
      Text := AValue;
      SaveToFile(GetExtraOptionsFileName);
    finally
      Free;
    end;
end;

procedure TProjectConfig.SetMnfExtraOptionsEnabled(AValue: boolean);
begin
  ProjIniFile.WriteBool('MANIFEST', 'ExtraOptionsEnabled', AValue);
end;

procedure TProjectConfig.SetVersBuild(AValue: integer);
begin
  ProjIniFile.WriteInteger('VERSIONS', 'VersBuild', AValue);
end;

procedure TProjectConfig.SetVersMajor(AValue: integer);
begin
  ProjIniFile.WriteInteger('VERSIONS', 'VersMajor', AValue);
end;

procedure TProjectConfig.SetVersMinor(AValue: integer);
begin
  ProjIniFile.WriteInteger('VERSIONS', 'VersMinor', AValue);
end;

function TProjectConfig.GetExtraOptionsFileName: string;
begin
  Result := ExtractFilePath(GetConfigFileName) + ExtractFileNameWithoutExt(GetConfigFileName) + '.mf';
end;

end.

