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

unit uProjectBuilding;

{$mode objfpc}{$H+}

interface

uses
  Classes, FileUtil, RegExpr, SysUtils;

type

  { TProjectBuilding }

  TProjectBuilding = class
  private
    FAutoIncBuildVers: boolean;
    function CompileFile(FileName: string): boolean;
    function CreateManifest(const APath: string): boolean;
    function DeleteCharacters(const AValue: string): string;
    function IsErr(const AValue: string): boolean;
  public
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
  ProjBuilding := TProjectBuilding.Create;
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
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

function TProjectBuilding.CreateManifest(const APath: string): boolean;
var
  FileName: string;
  mName, mVendor, mVersion, mIcon: string;
  mDeleteConfirm, mDescription, mInfoURL: string;

begin
  Result := False;

  with ProjConfig do
  begin
    mDeleteConfirm := MIDletDeleteConfirm;
    mDescription := MIDletDescription;
    mIcon := MIDletIcon;
    mInfoURL := MIDletInfoURL;
    mName := MIDletName;
    mVendor := MIDletVendor;
  end;

  mVersion := ProjManager.MIDletVersion;

  with TStringList.Create do
  begin
    try
      Add('Manifest-Version: 1.0');
      Add('MIDlet-1: ' + mName + ', ' + mIcon + ', FW');
      Add('MIDlet-Name: ' + mName);
      Add('MIDlet-Version: ' + mVersion);
      Add('MIDlet-Vendor: ' + mVendor);
      Add('MicroEdition-Configuration: CLDC-1.0');

      if ProjConfig.CanvasType <= 0 then
        Add('MicroEdition-Profile: MIDP-1.0')
      else
        Add('MicroEdition-Profile: MIDP-2.0');

      if mDescription <> '' then
        Add('MIDlet-Description: ' + mDescription);

      if mInfoURL <> '' then
        Add('MIDlet-Info-URL: ' + mInfoURL);

      if mDeleteConfirm <> '' then
        Add('MIDlet-Delete-Confirm: ' + mDeleteConfirm);

      FileName := APath + 'MANIFEST.MF';

      try
        SaveToFile(FileName);
        Result := True;
      except
        AddLogMsg('Ошибка при сохранении: ' + FileName, lmtErr);
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
  PCompiler: TProcFunc;

  procedure CopyClass;
  var
    StubFileName: string;

  begin
    with TRegExpr.Create do
    begin
      try
        Expression := '\^2(.*?)' + EXT_CLASS;
        if Exec(PCompiler.Output) then
          repeat
            StubFileName := APP_DIR_STUBS + Match[1] + EXT_CLASS;
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
        if Exec(PCompiler.Output) then
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
              LibFileName := APP_DIR_LIBS + ExtractFileName(LibFileName);
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
  CmdLine: string;

begin
  Result := False;

  if not CheckFile(FileName) then
    Exit;

  CmdLine :=
    MP3CC +
    ' -s "' + FileName + '"' +
    ' -o "' + ExcludeTrailingPathDelimiter(ProjManager.ProjDirPreBuild) + '"' +
    ' -l "' + ExcludeTrailingPathDelimiter(APP_DIR_LIBS) + '"' +
    ' -p "' + ExcludeTrailingPathDelimiter(ProjManager.ProjDirLibs) + '"' +
    ' -m ' + IntToStr(ProjConfig.MathType) +
    ' -c ' + IntToStr(ProjConfig.CanvasType);

  PCompiler := ProcStart(CmdLine + ' -d');

  if not PCompiler.Completed then
    Exit;

  with TRegExpr.Create do
  begin
    try
      Expression := '\^0(.*?)' + LE;
      if Exec(PCompiler.Output) then
      begin
        repeat
          FileName := ProjManager.ProjDirSrc + Match[1] + EXT_MODULE;
          CompileFile(FileName);
        until not ExecNext;
      end;
    finally
      Free;
    end;
  end;

  PCompiler := ProcStart(CmdLine);

  if PCompiler.Completed then
  begin
    CopyClass;
    CopyLib;
    if not IsErr(PCompiler.Output) then
    begin
      AddLogMsg(DeleteCharacters(PCompiler.Output) + 'Завершено', lmtOk);
      Result := True;
    end
    else
      AddLogMsg(DeleteCharacters(PCompiler.Output), lmtErr);
  end;
end;

function TProjectBuilding.IsErr(const AValue: string): boolean;
begin
  Result := False;
  if Pos('[Pascal Error]', AValue) > 0 then
    Result := True;
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

    // >= 100 не работает (Nokia)
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
  PreBuildDir, ManifestDir: string;
  CmdLine, JARFileName: string;

begin
  Result := False;

  PreBuildDir := ProjManager.ProjDirPreBuild;
  ManifestDir := PreBuildDir + 'META-INF' + DIR_SEP;

  if not CheckDir(PreBuildDir) then
    Exit;

  { TODO : if }
  DeleteDirectory(PreBuildDir, False);

  if not MakeDir(PreBuildDir) then
    Exit;

  MakeDir(ManifestDir);

  if not CreateManifest(ManifestDir) then
    Exit;

  if not CompileMainModule then
    Exit;

  JARFileName := ProjManager.JARFile;
  CmdLine := FILE_ARCHIVER + ' a "' + JARFileName + '" "';

  if FileExists(JARFileName) then
    if not DeleteFile(JARFileName) then
    begin
      AddLogMsg('Не удалось удалить ' + ExtractFileName(JARFileName) +
        ' возможно файл занят другим процессом', lmtErr);
      Exit;
    end;

  AddLogMsg('Начало архивации ' + ExtractFileName(JARFileName) + '...' + LE);

  if ProcStart(CmdLine + ProjManager.ProjDirPreBuild + '*"', False).Completed then
  begin
    if ProcStart(CmdLine + ProjManager.ProjDirRes + '*"', False).Completed then
    begin
      if ProjConfig.AutoIncBuildVers then
        IncBuildVers;
      AddLogMsg(
        'Проект успешно собран' + LE +
        'Версия: ' + ProjManager.MIDletVersion + LE +
        'Размер: ' + GetFileSize(JARFileName), lmtOk);
      Result := True;
    end;
  end;
end;

function TProjectBuilding.CompileMainModule: boolean;
const
  FW_Class = APP_DIR_STUBS + CLASS_FW;

begin
  Result := False;

  if ProjManager.CreateProjDir(ProjManager.ProjDirHome) then
  begin
    if CheckFile(FW_Class) then
      CopyFile(FW_Class, ProjManager.ProjDirPreBuild + CLASS_FW);

    AddLogMsg('Компиляция ' + ExtractFileName(ProjManager.MainModule) + '...');

    if CompileFile(ProjManager.MainModule) then
      Result := True;
  end;
end;

procedure TProjectBuilding.Run;
begin
  if Build then
  begin
    { TODO : костыль }
    Sleep(10);
    AddLogMsg('Emulator: запуск ' + ExtractFileName(ProjManager.JARFile) + '...');
    if ProcStart(IDEConfig.DirectiveReplace(IDEConfig.EmulatorCmd), False).Completed then
      AddLogMsg('Работа эмулятора завершена');
  end;
end;

end.

