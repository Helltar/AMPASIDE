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

unit uIDEConfig;

{$mode objfpc}{$H+}

interface

uses
  Graphics, IniFiles, SysUtils;

type

  { TIDEConfig }

  TIDEConfig = class
  private
    FMainConfig: TIniFile;
    function GetColorSchemeFile: string;
    function GetCompactViewMode: boolean;
    function GetEmulatorCmd: string;
    function GetFColor: TColor;
    function GetFHeight: integer;
    function GetFilManPrevSizeX: integer;
    function GetFilManPrevSizeY: integer;
    function GetFilManPrevStretch: boolean;
    function GetFLeft: integer;
    function GetFsplBottom: integer;
    function GetFsplLeft: integer;
    function GetFTop: integer;
    function GetFWidth: integer;
    function GetLastProj: string;
    function GetModulePrefix: string;
    procedure SetColorSchemeFile(AValue: string);
    procedure SetCompactViewMode(AValue: boolean);
    procedure SetEmulatorCmd(AValue: string);
    procedure SetFColor(AValue: TColor);
    procedure SetFHeight(AValue: integer);
    procedure SetFilManPrevSizeX(AValue: integer);
    procedure SetFilManPrevSizeY(AValue: integer);
    procedure SetFilManPrevStretch(AValue: boolean);
    procedure SetFLeft(AValue: integer);
    procedure SetFSplBottom(AValue: integer);
    procedure SetFSplLeft(AValue: integer);
    procedure SetFTop(AValue: integer);
    procedure SetFWidth(AValue: integer);
    procedure SetLastProj(AValue: string);
    procedure SetModulePrefix(AValue: string);
  public
    constructor Create(FileName: string);
    destructor Destroy; override;

    function DirectiveReplace(const AValue: string): string;

    property MainConfig: TIniFile read FMainConfig;

    // mainform
    property CompactViewMode: boolean read GetCompactViewMode write SetCompactViewMode;
    property FColor: TColor read GetFColor write SetFColor;
    property FHeight: integer read GetFHeight write SetFHeight;
    property FLeft: integer read GetFLeft write SetFLeft;
    property FSplBottom: integer read GetFSplBottom write SetFSplBottom;
    property FSplLeft: integer read GetFSplLeft write SetFSplLeft;
    property FTop: integer read GetFTop write SetFTop;
    property FWidth: integer read GetFWidth write SetFWidth;

    // file manager
    property FilManPrevSizeX: integer read GetFilManPrevSizeX write SetFilManPrevSizeX;
    property FilManPrevSizeY: integer read GetFilManPrevSizeY write SetFilManPrevSizeY;
    property FilManPrevStretch: boolean read GetFilManPrevStretch write SetFilManPrevStretch;

    // other
    property ColorSchemeFile: string read GetColorSchemeFile write SetColorSchemeFile;
    property EmulatorCmd: string read GetEmulatorCmd write SetEmulatorCmd;
    property LastProj: string read GetLastProj write SetLastProj;
    property ModulePrefix: string read GetModulePrefix write SetModulePrefix;
  end;

var
  IDEConfig: TIDEConfig;

implementation

uses
  uAMPASCore,
  uProjectManager;

{ TIDEConfig }

constructor TIDEConfig.Create(FileName: string);
begin
  FMainConfig := TIniFile.Create(FileName);
end;

destructor TIDEConfig.Destroy;
begin
  FreeAndNil(FMainConfig);
  inherited Destroy;
end;

function TIDEConfig.DirectiveReplace(const AValue: string): string;
begin
  Result := AValue;
  { TODO : в константы }
  Result := StringReplace(Result, '{$APP_NAME}', APP_NAME, [rfReplaceAll]);
  Result := StringReplace(Result, '{$APP_VERSION}', APP_VERSION, [rfReplaceAll]);
  Result := StringReplace(Result, '{$DATE_TIME}', FormatDateTime('dd.mm.yyyy hh:mm:ss', Now), [rfReplaceAll]);
  Result := StringReplace(Result, '{$JAR_FILENAME}', ProjManager.JARFile, [rfReplaceAll]);
  Result := StringReplace(Result, '{$USERNAME}', GetCurrentUsername, [rfReplaceAll]);
end;

procedure TIDEConfig.SetEmulatorCmd(AValue: string);
begin
  FMainConfig.WriteString('OTHER', 'EmulatorCmd', AValue);
end;

procedure TIDEConfig.SetFColor(AValue: TColor);
begin
  FMainConfig.WriteString('FORM', 'Color', ColorToString(AValue));
end;

procedure TIDEConfig.SetFHeight(AValue: integer);
begin
  FMainConfig.WriteInteger('FORM', 'Height', AValue);
end;

procedure TIDEConfig.SetFilManPrevSizeX(AValue: integer);
begin
  FMainConfig.WriteInteger('FILEMANAGER', 'PrevSizeX', AValue);
end;

procedure TIDEConfig.SetFilManPrevSizeY(AValue: integer);
begin
  FMainConfig.WriteInteger('FILEMANAGER', 'PrevSizeY', AValue);
end;

procedure TIDEConfig.SetFilManPrevStretch(AValue: boolean);
begin
  FMainConfig.WriteBool('FILEMANAGER', 'PrevStretch', AValue);
end;

procedure TIDEConfig.SetFLeft(AValue: integer);
begin
  FMainConfig.WriteInteger('FORM', 'Left', AValue);
end;

procedure TIDEConfig.SetFSplBottom(AValue: integer);
begin
  FMainConfig.WriteInteger('FORM', 'SplBottom', AValue);
end;

procedure TIDEConfig.SetFSplLeft(AValue: integer);
begin
  FMainConfig.WriteInteger('FORM', 'SplLeft', AValue);
end;

procedure TIDEConfig.SetFTop(AValue: integer);
begin
  FMainConfig.WriteInteger('FORM', 'Top', AValue);
end;

procedure TIDEConfig.SetFWidth(AValue: integer);
begin
  FMainConfig.WriteInteger('FORM', 'Width', AValue);
end;

procedure TIDEConfig.SetLastProj(AValue: string);
begin
  FMainConfig.WriteString('OTHER', 'LastProj', AValue);
end;

procedure TIDEConfig.SetModulePrefix(AValue: string);
begin
  FMainConfig.WriteString('EDITOR', 'ModulePrefix', AValue);
end;

function TIDEConfig.GetLastProj: string;
begin
  Result := FMainConfig.ReadString('OTHER', 'LastProj', '');
end;

function TIDEConfig.GetModulePrefix: string;
begin
  Result := FMainConfig.ReadString('EDITOR', 'ModulePrefix', 'u');
end;

procedure TIDEConfig.SetColorSchemeFile(AValue: string);
begin
  FMainConfig.WriteString('EDITOR', 'ColorSchemeFile', AValue);
end;

procedure TIDEConfig.SetCompactViewMode(AValue: boolean);
begin
  FMainConfig.WriteBool('FORM', 'CompactViewMode', AValue);
end;

function TIDEConfig.GetEmulatorCmd: string;
begin
  Result := FMainConfig.ReadString('OTHER', 'EmulatorCmd', 'java -jar ' + EMULATOR + ' {$JAR_FILENAME}');
end;

function TIDEConfig.GetFColor: TColor;
begin
  Result := StringToColor(FMainConfig.ReadString('FORM', 'Color', 'clDefault'));
end;

function TIDEConfig.GetCompactViewMode: boolean;
begin
  Result := FMainConfig.ReadBool('FORM', 'CompactViewMode', False);
end;

function TIDEConfig.GetColorSchemeFile: string;
begin
  Result := FMainConfig.ReadString('EDITOR', 'ColorSchemeFile', APP_DIR_DATA + 'editorcolorscheme');
end;

function TIDEConfig.GetFHeight: integer;
begin
  Result := FMainConfig.ReadInteger('FORM', 'Height', 600);
end;

function TIDEConfig.GetFilManPrevSizeX: integer;
begin
  Result := FMainConfig.ReadInteger('FILEMANAGER', 'PrevSizeX', 64);
end;

function TIDEConfig.GetFilManPrevSizeY: integer;
begin
  Result := FMainConfig.ReadInteger('FILEMANAGER', 'PrevSizeY', 64);
end;

function TIDEConfig.GetFilManPrevStretch: boolean;
begin
  Result := FMainConfig.ReadBool('FILEMANAGER', 'PrevStretch', False);
end;

function TIDEConfig.GetFLeft: integer;
begin
  Result := FMainConfig.ReadInteger('FORM', 'Left', 200);
end;

function TIDEConfig.GetFsplBottom: integer;
begin
  Result := FMainConfig.ReadInteger('FORM', 'SplBottom', 380);
end;

function TIDEConfig.GetFsplLeft: integer;
begin
  Result := FMainConfig.ReadInteger('FORM', 'SplLeft', 290);
end;

function TIDEConfig.GetFTop: integer;
begin
  Result := FMainConfig.ReadInteger('FORM', 'Top', 120);
end;

function TIDEConfig.GetFWidth: integer;
begin
  Result := FMainConfig.ReadInteger('FORM', 'Width', 900);
end;

end.

