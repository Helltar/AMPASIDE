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

unit uProjectManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, FileUtil, SysUtils;

type

  { TProjectManager }

  TProjectManager = class
  private
    FConfigFile: string;
    FMainModule: string;
    FNotesFile: string;
    FProjDirBin: string;
    FProjDirHome: string;
    FProjDirLibs: string;
    FProjDirPreBuild: string;
    FProjDirRes: string;
    FProjDirSrc: string;
    FProjectOpen: boolean;
    function GetJARFile: string;
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

    property ConfigFile: string read FConfigFile;
    property JARFile: string read GetJARFile;
    property MainModule: string read FMainModule;
    property NotesFile: string read FNotesFile;

    property ProjDirBin: string read FProjDirBin;
    property ProjDirHome: string read FProjDirHome;
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
        AddLogMsg('Не удалось создать модуль: ' + Result, lmtErr);
      end;
    finally
      Free;
    end;
  end;
end;

function TProjectManager.GetJARFile: string;
begin
  if ProjConfig.MIDletName <> '' then
    Result := FProjDirHome + PROJ_DIR_BIN + DIR_SEP + ProjConfig.MIDletName + EXT_JAR;
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

      VersMajor := 1;
      VersMinor := 0;
      VersBuild := 0;
      AutoIncBuildVers := True;
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
        Add('program ' + ModuleName + ';');
        Add('begin');
        Add('  DrawText(''Hello, Linux!'', 0, 0);');
        Add('  Repaint;');
        Add('  Delay(5000);');
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
        Add('Здесь можно писать что угодно, используйте это для заметок и быстрых записей');
        SaveToFile(FileName + EXT_NOTES);
      finally
        Free;
      end;
    end;
  end;

begin
  Result := False;

  APath := APath + DIR_SEP + AName + DIR_SEP;
  FConfigFile := APath + AName + EXT_PROJECT;
  FMainModule := APath + PROJ_DIR_SRC + DIR_SEP + AName;
  FNotesFile := APath + AName;

  if CreateProjDir(APath) then
  begin
    CreateConfigFile(FConfigFile);
    CreateMainModule(FMainModule);
    CreateNotesFile(FNotesFile);
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
  FProjDirLibs := FProjDirHome + PROJ_DIR_LIBS + DIR_SEP;
  FProjDirPreBuild := FProjDirHome + PROJ_DIR_PRE_BUILD + DIR_SEP;
  FProjDirRes := FProjDirHome + PROJ_DIR_RES + DIR_SEP;
  FProjDirSrc := FProjDirHome + PROJ_DIR_SRC + DIR_SEP;

  AddLogMsg(UpperCase(ProjConfig.MIDletName) + ' ' + GetMIDletVersion);

  FProjectOpen := True;
  Result := True;
end;

end.

