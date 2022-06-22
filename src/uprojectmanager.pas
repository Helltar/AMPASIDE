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

unit uProjectManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, FileUtil, SysUtils, LazFileUtils;

type

  { TProjectManager }

  TProjectManager = class
  private
    FConfigFile: string;
    FMainModule: string;
    FNotesFile: string;
    FProjDirAndroid: string;
    FProjDirBin: string;
    FProjDirHome: string;
    FProjDirJavaME: string;
    FProjDirLibs: string;
    FProjDirPreBuild: string;
    FProjDirRes: string;
    FProjDirSrc: string;
    FProjectOpen: boolean;
    function GetApkFile: string;
    function GetJadFile: string;
    function GetJarFile: string;
    function GetMIDletVersion: string;
  public
    constructor Create;
    destructor Destroy; override;

    function CreateModule(const FileName: string): string;
    function CreateProjDir(const MainDir: string): boolean;
    function CreateProject(APath, AName: string): boolean;
    function OpenProject(const FileName: string): boolean;

    property ProjectOpen: boolean read FProjectOpen;
    property MIDletVersion: string read GetMIDletVersion;

    property ApkFile: string read GetApkFile;
    property ConfigFile: string read FConfigFile;
    property JadFile: string read GetJadFile;
    property JarFile: string read GetJarFile;
    property MainModule: string read FMainModule;
    property NotesFile: string read FNotesFile;

    property ProjDirAndroid: string read FProjDirAndroid;
    property ProjDirBin: string read FProjDirBin;
    property ProjDirHome: string read FProjDirHome;
    property ProjDirJavaME: string read FProjDirJavaME;
    property ProjDirLibs: string read FProjDirLibs;
    property ProjDirPreBuild: string read FProjDirPreBuild;
    property ProjDirRes: string read FProjDirRes;
    property ProjDirSrc: string read FProjDirSrc;
  end;

var
  ProjManager: TProjectManager;

implementation

uses
  uAMPASCore,
  uEditorConfig,
  uProjectConfig;

{ TProjectManager }

constructor TProjectManager.Create;
begin
  ProjConfig := TProjectConfig.Create;
end;

destructor TProjectManager.Destroy;
begin
  FreeAndNil(ProjConfig);
  inherited Destroy;
end;

function TProjectManager.GetMIDletVersion: string;
begin
  with ProjConfig do
    Result := IntToStr(VersMajor) + '.' + IntToStr(VersMinor) + '.' + IntToStr(VersBuild);
end;

function TProjectManager.CreateProjDir(const MainDir: string): boolean;
begin
  Result := False;
  if MakeDir(MainDir) then
    if MakeDir(MainDir + PROJ_DIR_BIN) then
      if MakeDir(MainDir + PROJ_DIR_BIN + DIR_SEP + PROJ_DIR_ANDROID) then
        if MakeDir(MainDir + PROJ_DIR_BIN + DIR_SEP + PROJ_DIR_JAVAME) then
          if MakeDir(MainDir + PROJ_DIR_LIBS) then
            if MakeDir(MainDir + PROJ_DIR_PRE_BUILD) then
              if MakeDir(MainDir + PROJ_DIR_RES) then
                if MakeDir(MainDir + PROJ_DIR_SRC) then
                  Result := True;
end;

function TProjectManager.CreateModule(const FileName: string): string;
var
  ModuleName: string;

begin
  ModuleName := ExtractFileName(FileName);

  Result := ExtractFilePath(FileName) + LowerCase(ModuleName + EXT_MODULE);

  with TStringList.Create do
  begin
    try
      Add(EditorConfig.EditorHeaders);
      Add('unit ' + ModuleName + ';' + LE);
      Add('interface' + LE);
      Add('{ public declarations }' + LE);
      Add('implementation' + LE);
      Add('{ add unit functions & procedures here }' + LE);
      Add('initialization' + LE);
      Add('{ add initialization code here }' + LE);
      Add('end.');
      try
        SaveToFile(Result);
      except
        AddLogMsg(ERR_FAILED_CREATE_MODULE + ': ' + Result, lmtErr);
      end;
    finally
      Free;
    end;
  end;
end;

function TProjectManager.GetJarFile: string;
begin
  if ProjConfig.MIDletName <> '' then
    Result := FProjDirJavaME + ProjConfig.MIDletName + EXT_JAR;
end;

function TProjectManager.GetApkFile: string;
begin
  if ProjConfig.MIDletName <> '' then
    Result := FProjDirAndroid + ProjConfig.MIDletName + EXT_APK;
end;

function TProjectManager.GetJadFile: string;
begin
  if ProjConfig.MIDletName <> '' then
    Result := FProjDirJavaME + ProjConfig.MIDletName + EXT_JAD;
end;

function TProjectManager.CreateProject(APath, AName: string): boolean;

  procedure CreateConfigFile(FileName: string);
  var
    MIDletNameOnly: string;

  begin
    MIDletNameOnly := ExtractFileNameOnly(FileName);

    with ProjConfig do
    begin
      ConfigFileName := FileName;

      CanvasType := 1;
      MathType := 0;
      MainModuleName := LowerCase(MIDletNameOnly);

      MIDletName := MIDletNameOnly;
      MIDletVendor := GetCurrentUsername;
      MIDletIcon := '/icon.png';
      MIDletDescription := '';
      MIDletInfoURL := '';
      MIDletDeleteConfirm := '';
      MIDletInstallNotify := '';
      MnfExtraOptionsEnabled := False;

      VersMajor := 1;
      VersMinor := 0;
      VersBuild := 0;
      AutoIncBuildVers := False;
    end;
  end;

  procedure CreateMainModule(FileName: string);
  var
    ModuleName: string;

  begin
    ModuleName := ExtractFileNameOnly(FileName);
    FileName := ExtractFilePath(FileName) + LowerCase(ModuleName + EXT_MODULE);

    with TStringList.Create do
    begin
      try
        Add(EditorConfig.EditorHeaders);
        Add('program ' + ModuleName + ';' + LE);
        Add('var');
        Add('  s: string;');
        Add('  x, y: integer;' + LE);
        Add('begin');
        Add('  s := ''Hello, World!'';');
        Add('  x := (GetWidth - GetStringWidth(s)) div 2;');
        Add('  y := (GetHeight - GetStringHeight(s)) div 2;' + LE);
        Add('  DrawText(s, x, y);');
        Add('  Repaint;' + LE);
        Add('  Delay(9000);');
        Add('end.');
        SaveToFile(FileName);
      finally
        Free;
      end;
    end;
  end;

  procedure CreateNotesFile(FileName: string);
  begin
    with TStringList.Create do
    begin
      try
        Add(MSG_DEFAULT_NOTES);
        SaveToFile(FileName + EXT_NOTES);
      finally
        Free;
      end;
    end;
  end;

  procedure CreateAndroidManifest(const AppName, filename: string);
  begin
    with TStringList.Create do
      try
        LoadFromFile(GetAppPath + APP_DIR_CONFIG + ANDROID_MANIFEST);
        Text := StringReplace(Text, '$(APP_NAME)', LowerCase(AppName), [rfReplaceAll]);
        try
          SaveToFile(filename);
        except
          AddLogMsg(ERR_FAILED_SAVE + ': ' + FileName, lmtErr);
        end;
      finally
        Free;
      end;
  end;

begin
  Result := False;

  APath := APath + DIR_SEP + AName + DIR_SEP;
  FConfigFile := APath + AName + EXT_PROJECT;
  FMainModule := APath + PROJ_DIR_SRC + DIR_SEP + AName;
  FNotesFile := APath + AName;
  FProjDirRes := APath + PROJ_DIR_RES + DIR_SEP;

  if CreateProjDir(APath) then
  begin
    CreateConfigFile(FConfigFile);
    CreateMainModule(FMainModule);
    CreateNotesFile(FNotesFile);
    CreateAndroidManifest(AName, APath + ANDROID_MANIFEST);
    CopyFile(GetAppPath + APP_DIR_IMG + 'main' + DIR_SEP + 'icon.png', FProjDirRes + 'icon.png');
    Result := True;
  end;
end;

function TProjectManager.OpenProject(const FileName: string): boolean;
begin
  Result := False;

  if not CheckFile(FileName) then
    Exit;

  ProjConfig.ConfigFileName := FileName;
  FProjDirHome := ExtractFilePath(FileName);
  FMainModule := FProjDirHome + PROJ_DIR_SRC + DIR_SEP + ProjConfig.MainModuleName + EXT_MODULE;

  if not CheckFile(FMainModule) then
    Exit;

  FConfigFile := FileName;
  FNotesFile := FProjDirHome + ExtractFileNameOnly(FileName) + EXT_NOTES;
  FProjDirBin := FProjDirHome + PROJ_DIR_BIN + DIR_SEP;
  FProjDirAndroid := FProjDirBin + PROJ_DIR_ANDROID + DIR_SEP;
  FProjDirJavaME := FProjDirBin + PROJ_DIR_JAVAME + DIR_SEP;
  FProjDirLibs := FProjDirHome + PROJ_DIR_LIBS + DIR_SEP;
  FProjDirPreBuild := FProjDirHome + PROJ_DIR_PRE_BUILD + DIR_SEP;
  FProjDirRes := FProjDirHome + PROJ_DIR_RES + DIR_SEP;
  FProjDirSrc := FProjDirHome + PROJ_DIR_SRC + DIR_SEP;

  AddLogMsg(UpperCase(ProjConfig.MIDletName) + ' ' + GetMIDletVersion);

  FProjectOpen := True;
  Result := True;
end;

end.
