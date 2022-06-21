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

unit uProjectBuilding;

{$mode objfpc}{$H+}

interface

uses
  Classes, FileUtil, RegExpr, SysUtils, LazFileUtils;

type

  { TProjectBuilding }

  TProjectBuilding = class
  private
    ManifestFileName: string;
    function CompileFile(FileName: string): boolean;
    function CreateManifest(const FileName: string): boolean;
    function DeleteCharacters(const AValue: string): string;
    function ExecPreBuildAct: boolean;
    function IsErr(const AValue: string): boolean;
    function RebuildPreBuildDir: boolean;
    procedure CreateJad;
  public
    constructor Create;
    function Build: boolean;
    function CompileMainModule: boolean;
    procedure Run;
  end;

  { TBuildingThread }

  TBuildingThread = class(TThread)
  private
    FMode: integer;
    ProjBuilding: TProjectBuilding;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
    property Mode: integer read FMode write FMode;
  end;

implementation

uses
  uAMPASCore,
  uProjectManager,
  uProjectConfig,
  uIDEConfig;

{ TBuildingThread }

constructor TBuildingThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  ProjBuilding := TProjectBuilding.Create;
end;

destructor TBuildingThread.Destroy;
begin
  FreeAndNil(ProjBuilding);
  inherited Destroy;
end;

procedure TBuildingThread.Execute;
begin
  with ProjBuilding do
    case FMode of
      0: CompileMainModule;
      1: Build;
      2: Run;
    end;
end;

{ TProjectBuilding }

constructor TProjectBuilding.Create;
begin
  ManifestFileName := ProjManager.ProjDirPreBuild + 'META-INF' + DIR_SEP + 'MANIFEST.MF';
end;

function TProjectBuilding.CreateManifest(const FileName: string): boolean;
var
  ManifestExtraOptions: string;
  mName, mDeleteConfirm, mDescription, mInfoURL: string;

begin
  Result := False;

  with ProjConfig do
  begin
    ManifestExtraOptions := MnfExtraOptions;
    mDeleteConfirm := MIDletDeleteConfirm;
    mDescription := MIDletDescription;
    mInfoURL := MIDletInfoURL;
    mName := MIDletName;
  end;

  with TStringList.Create do
  begin
    try
      Add('Manifest-Version: 1.0');
      Add('Created-By: ' + APP_NAME + ' ' + GetProgramVersion);
      Add('MIDlet-1: ' + mName + ', ' + ProjConfig.MIDletIcon + ', FW');
      Add('MIDlet-Name: ' + mName);
      Add('MIDlet-Version: ' + ProjManager.MIDletVersion);
      Add('MIDlet-Vendor: ' + ProjConfig.MIDletVendor);

      if mDescription <> '' then
        Add('MIDlet-Description: ' + mDescription);

      if mInfoURL <> '' then
        Add('MIDlet-Info-URL: ' + mInfoURL);

      if mDeleteConfirm <> '' then
        Add('MIDlet-Delete-Confirm: ' + mDeleteConfirm);

      Add('MicroEdition-Configuration: CLDC-1.1');

      if ProjConfig.CanvasType <= 0 then
        Add('MicroEdition-Profile: MIDP-1.0')
      else
        Add('MicroEdition-Profile: MIDP-2.0');

      if ProjConfig.MnfExtraOptionsEnabled then
        if ManifestExtraOptions <> '' then
          Add(ManifestExtraOptions);

      Text := DelEmptyLines(Text);

      try
        SaveToFile(FileName);
        Result := True;
      except
        AddLogMsg(ERR_SAVING + ': ' + FileName, lmtErr);
      end;
    finally
      Free;
    end;
  end;
end;

function TProjectBuilding.DeleteCharacters(const AValue: string): string;
var
  i: integer;

begin
  with TStringList.Create do
  begin
    try
      Text := AValue;
      for i := Count - 1 downto 0 do
        if Pos('@', Strings[i]) > 0 then
          Delete(i);
      Text := StringReplace(Text, '^1', 'Lib: ', [rfReplaceAll]);
      Text := StringReplace(Text, '^2', '', [rfReplaceAll]);
      Text := StringReplace(Text, '^3', '', [rfReplaceAll]);
      Text := StringReplace(Text, '[Pascal Error] ', '', [rfReplaceAll]);
      Result := Text;
    finally
      Free;
    end;
  end;
end;

function TProjectBuilding.CompileFile(FileName: string): boolean;
var
  P: TProcFunc;

  procedure CopyClass;
  var
    StubFileName: string;

  begin
    with TRegExpr.Create do
    begin
      try
        Expression := '\^2(.*?)' + EXT_CLASS;
        if Exec(P.Output) then
          repeat
            StubFileName := GetAppPath + APP_DIR_STUBS + Match[1] + EXT_CLASS;
            if CheckFile(StubFileName) then
              CopyFile(StubFileName, ProjManager.ProjDirPreBuild + ExtractFileName(StubFileName));
          until not ExecNext;
      finally
        Free;
      end;
    end;
  end;

  procedure CopyLib;
  const
    LibPrefix = 'Lib_';

  var
    LibFileName, NewLibFileName: string;

  begin
    with TRegExpr.Create do
    begin
      try
        Expression := '\^1(.*?)' + LE;
        if Exec(P.Output) then
          repeat
            LibFileName := ProjManager.ProjDirLibs + LibPrefix + Match[1] + EXT_CLASS;
            NewLibFileName := ProjManager.ProjDirPreBuild + ExtractFileName(LibFileName);
            if FileExists(LibFileName) then
            begin
              if CheckFile(LibFileName) then
                CopyFile(LibFileName, NewLibFileName);
            end
            else
            begin
              LibFileName := GetAppPath + APP_DIR_LIBS + ExtractFileName(LibFileName);
              if CheckFile(LibFileName) then
                CopyFile(LibFileName, NewLibFileName);
            end;
          until not ExecNext;
      finally
        Free;
      end;
    end;
  end;

var
  MPCompiler, CmdParameters: string;
  OrigName: string;

begin
  Result := False;

  if not FileExists(FileName) then
  begin
    AddLogMsg(ERR_FAILED_FIND_LIB + ': "' +
      ExtractFileNameOnly(FileName) + '"', lmtErr);
    Exit;
  end;

  MPCompiler := GetAppPath + MP3CC;
  CmdParameters :=
    ' -s "' + FileName + '"' +
    ' -o "' + ExcludeTrailingPathDelimiter(ProjManager.ProjDirPreBuild) + '"' +
    ' -l "' + ExcludeTrailingPathDelimiter(GetAppPath + APP_DIR_LIBS) + '"' +
    ' -p "' + ExcludeTrailingPathDelimiter(ProjManager.ProjDirLibs) + '"' +
    ' -m ' + IntToStr(ProjConfig.MathType) +
    ' -c ' + IntToStr(ProjConfig.CanvasType);

  P := ProcStart(MPCompiler, CmdParameters + ' -d');

  if not P.Completed then
    Exit;

  OrigName := ExtractFileName(FileName);

  with TRegExpr.Create do
  begin
    try
      Expression := '\^0(.*?)' + LE;
      if Exec(P.Output) then
      begin
        repeat
          if not FileExists(ProjManager.ProjDirPreBuild + Match[1] + EXT_CLASS) then // ._.
          begin
            FileName := ProjManager.ProjDirSrc + Match[1] + EXT_MODULE;
            if not CompileFile(FileName) then
              Exit;
          end;
        until not ExecNext;
      end;
    finally
      Free;
    end;
  end;

  AddLogMsg(MSG_COMPILATION_PROGRESS + ' ' + OrigName + '...');
  P := ProcStart(MPCompiler, CmdParameters);

  if P.Completed then
  begin
    CopyClass;
    CopyLib;
    if not IsErr(P.Output) then
    begin
      AddLogMsg(DeleteCharacters(P.Output) + MSG_COMPLETED, lmtOk);
      Result := True;
    end
    else
      AddLogMsg(DeleteCharacters(P.Output), lmtErr);
  end;
end;

function TProjectBuilding.IsErr(const AValue: string): boolean;
begin
  Result := False;
  if Pos('[Pascal Error]', AValue) > 0 then
    Result := True;
end;

function TProjectBuilding.ExecPreBuildAct: boolean;
var
  FW_Class: string;

begin
  Result := False;

  if ProjManager.CreateProjDir(ProjManager.ProjDirHome) then
    if RebuildPreBuildDir then
    begin
      FW_Class := GetAppPath + APP_DIR_STUBS + CLASS_FW;
      if CheckFile(FW_Class) then
        if CopyFile(FW_Class, ProjManager.ProjDirPreBuild + CLASS_FW) then
          if MakeDir(ExtractFilePath(ManifestFileName)) then
            if CreateManifest(ManifestFileName) then
              Result := True;
    end;
end;

function TProjectBuilding.RebuildPreBuildDir: boolean;
var
  PreBuildDir: string;

begin
  Result := False;

  PreBuildDir := ProjManager.ProjDirPreBuild;

  if CheckDir(PreBuildDir) then
    if DeleteDirectory(PreBuildDir, False) then
      if MakeDir(PreBuildDir) then
        Result := True;
end;

procedure TProjectBuilding.CreateJad;
var
  JadFile: string;

begin
  JadFile := ProjManager.JadFile;

  if not CopyFile(ManifestFileName, JadFile) then
    Exit;

  with TStringList.Create do
  begin
    try
      LoadFromFile(JadFile);

      Delete(0);
      Delete(0);

      if ProjConfig.MIDletInstallNotify <> '' then
        Add('MIDlet-Install-Notify: ' + ProjConfig.MIDletInstallNotify);

      Add('MIDlet-Jar-URL: ' + ExtractFileName(ProjManager.JARFile));
      Add('MIDlet-Jar-Size: ' + IntToStr(FileSize(ProjManager.JARFile)));

      Text := DelEmptyLines(Text);

      SaveToFile(JadFile);
    finally
      Free;
    end;
  end;
end;

function TProjectBuilding.Build: boolean;

  procedure IncBuildVers;
  var
    vMajor, vMinor, vBuild: integer;

  begin
    vMajor := ProjConfig.VersMajor;
    vMinor := ProjConfig.VersMinor;
    vBuild := ProjConfig.VersBuild;

    Inc(vBuild);

    // >= 100 not working (Nokia)
    if vBuild > 99 then
    begin
      vBuild := 0;
      Inc(vMinor);
    end;

    if vMinor > 99 then
    begin
      vMinor := 0;
      Inc(vMajor);
    end;

    ProjConfig.VersMajor := vMajor;
    ProjConfig.VersMinor := vMinor;
    ProjConfig.VersBuild := vBuild;
  end;

var
  FileArchiver, CmdParameters: string;
  JARFileName: string;

begin
  Result := False;

  if not ExecPreBuildAct then
    Exit;

  if not CompileFile(ProjManager.MainModule) then
    Exit;

  JARFileName := ProjManager.JarFile;

  if FileExists(JARFileName) then
    if not DeleteFile(JARFileName) then
    begin
      AddLogMsg(ERR_FAILED_DEL + ' ' + ExtractFileName(JARFileName) +
        ' ' + MSG_BUSY_PROC, lmtErr);
      Exit;
    end;

  AddLogMsg(MSG_ARCHIVING_START + ' ' + ExtractFileName(JARFileName) + '...' + LE);

  FileArchiver := GetAppPath + FILE_ARCHIVER;
  CmdParameters := 'a "' + JARFileName + '" "';

  if ProcStart(FileArchiver, CmdParameters + ProjManager.ProjDirPreBuild + '*"', False).Completed then
  begin
    if ProcStart(FileArchiver, CmdParameters + ProjManager.ProjDirRes + '*"', False).Completed then
    begin
      CreateJad;

      if ProjConfig.AutoIncBuildVers then
        IncBuildVers;

      AddLogMsg(
        MSG_SUCCESSFULLY_ASSEMBLED + LE +
        MSG_VERSION + ': ' + ProjManager.MIDletVersion + LE +
        MSG_SIZE + ': ' + GetFileSize(JARFileName) + LE +
        MSG_PLATFORM + ': JavaME', lmtOk);

      Result := True;
    end;
  end;
end;

function TProjectBuilding.CompileMainModule: boolean;
begin
  Result := False;

  if ProjManager.CreateProjDir(ProjManager.ProjDirHome) then
    if RebuildPreBuildDir then
      if CompileFile(ProjManager.MainModule) then
        Result := True;
end;

procedure TProjectBuilding.Run;
begin
  if not Build then
    Exit;

  AddLogMsg('Emulator: ' + MSG_LAUCHING + ' ' + ExtractFileName(ProjManager.JarFile) + '...');

  if ProcStart(IDEConfig.MacrosReplace(IDEConfig.EmulatorCmd), False).Completed then
    AddLogMsg(MSG_EMULATOR_STOP);
end;

end.
