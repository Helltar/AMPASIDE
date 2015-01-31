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
  Classes, FileUtil, RegExpr, SysUtils, Forms;

type

  { TProjectBuilding }

  TProjectBuilding = class
  public
    procedure Build;
    procedure CompileMainModule;
    procedure Run;
  end;

implementation

uses
  uAMPASCore,
  uProjectManager,
  uProjectConfig,
  uIDEConfig;

type

  { TLogMsg }

  TLogMsg = record
    Text: string;
    MsgType: TLogMsgType;
  end;

  { TBaseThread }

  TBaseThread = class(TThread)
  private
    LogMsg: TLogMsg;
    procedure ShowMsg;
  public
    procedure AddLog(const AValue: string; MsgType: TLogMsgType = lmtText);
  end;

  { TCompileThread }

  TCompileThread = class(TBaseThread)
  private
    FModuleName: string;
    procedure CompileFile(FileName: string);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
    property ModuleName: string read FModuleName write FModuleName;
  end;

  { TBuildThread }

  TBuildThread = class(TBaseThread)
  private
    procedure Build;
  protected
    procedure Execute; override;
  public
    WaitForCompile: PRtlEvent;
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
  end;

  { TRunThread }

  TRunThread = class(TBaseThread)
  private
    procedure RunEmu;
  protected
    procedure Execute; override;
  public
    WaitForBuild: PRtlEvent;
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
  end;

var
  CompileThread: TCompileThread;
  BuildThread: TBuildThread;
  RunThread: TRunThread;
  CompileCompleted, BuildCompleted: boolean;
  BuildStart, RunStart: boolean;

function CreateManifest(const APath: string): boolean;
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

function DeleteCharacters(const AValue: string): string;
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

function IsErr(const AValue: string): boolean;
begin
  Result := False;
  if Pos('[Pascal Error]', AValue) > 0 then
    Result := True;
end;

procedure IncBuildVers;
begin
  with ProjConfig do
  begin
    VersBuild := VersBuild + 1;

    // >= 100 не работает (Nokia)
    if VersBuild > 99 then
    begin
      VersBuild := 0;
      VersMinor := VersMinor + 1;
    end;

    if VersMinor > 99 then
    begin
      VersMinor := 0;
      VersMajor := VersMajor + 1;
    end;
  end;
end;

{ TBaseThread }

procedure TBaseThread.ShowMsg;
begin
  AddLogMsg(LogMsg.Text, LogMsg.MsgType);
end;

procedure TBaseThread.AddLog(const AValue: string; MsgType: TLogMsgType);
begin
  if not Application.Terminated then
  begin
    LogMsg.Text := AValue;
    LogMsg.MsgType := MsgType;
    Synchronize(@ShowMsg);
  end;
end;

{ TCompileThread }

constructor TCompileThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

destructor TCompileThread.Destroy;
begin
  if BuildStart then
    RtlEventSetEvent(BuildThread.WaitForCompile);
  inherited Destroy;
end;

procedure TCompileThread.Execute;
begin
  CompileFile(FModuleName);
end;

procedure TCompileThread.CompileFile(FileName: string);
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
  CompileCompleted := False;

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
      AddLog(DeleteCharacters(PCompiler.Output) + 'Завершено', lmtOk);
      CompileCompleted := True;
    end
    else
      AddLog(DeleteCharacters(PCompiler.Output), lmtErr);
  end;
end;

{ TBuildThread }

constructor TBuildThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  BuildStart := True;
  inherited Create(CreateSuspended);
end;

destructor TBuildThread.Destroy;
begin
  BuildStart := False;
  if RunStart then
    RtlEventSetEvent(RunThread.WaitForBuild);
  RTLeventdestroy(WaitForCompile);
  inherited Destroy;
end;

procedure TBuildThread.Execute;
begin
  WaitForCompile := RTLEventCreate;
  RtlEventWaitFor(WaitForCompile);
  if CompileCompleted then
    Build;
end;

procedure TBuildThread.Build;
var
  CmdLine, JARFileName: string;

begin
  BuildCompleted := False;

  JARFileName := ProjManager.JARFile;
  CmdLine := FILE_ARCHIVER + ' a "' + JARFileName + '" "';

  if FileExists(JARFileName) then
    DeleteFile(JARFileName);

  AddLog('Начало архивации ' + ExtractFileName(JARFileName) + '...' + LE);

  if ProcStart(CmdLine + ProjManager.ProjDirPreBuild + '*"', False).Completed then
  begin
    if ProcStart(CmdLine + ProjManager.ProjDirRes + '*"', False).Completed then
    begin
      if ProjConfig.AutoIncBuildVers then
        IncBuildVers;
      AddLog(
        'Проект успешно собран' + LE +
        'Версия: ' + ProjManager.MIDletVersion + LE +
        'Размер: ' + GetFileSize(JARFileName), lmtOk);
      BuildCompleted := True;
    end;
  end;
end;

{ TRunThread }

constructor TRunThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  RunStart := True;
  inherited Create(CreateSuspended);
end;

destructor TRunThread.Destroy;
begin
  RunStart := False;
  RTLeventdestroy(WaitForBuild);
  inherited Destroy;
end;

procedure TRunThread.Execute;
begin
  WaitForBuild := RTLEventCreate;
  RtlEventWaitFor(WaitForBuild);
  if BuildCompleted then
    RunEmu;
end;

procedure TRunThread.RunEmu;
begin
  AddLog('Emulator: запуск ' + ExtractFileName(ProjManager.JARFile) + '...');
  if ProcStart(IDEConfig.DirectiveReplace(IDEConfig.EmulatorCmd), False).Completed then
    AddLog('Работа эмулятора завершена');
end;

{ TProjectBuilding }

procedure TProjectBuilding.Build;
var
  PreBuildDir, ManifestDir: string;

begin
  BuildCompleted := False;

  PreBuildDir := ProjManager.ProjDirPreBuild;
  ManifestDir := PreBuildDir + 'META-INF' + DIR_SEP;

  if CheckDir(PreBuildDir) then
    { TODO : if }
    if DeleteDirectory(PreBuildDir, False) then
      if MakeDir(PreBuildDir) then
        if MakeDir(ManifestDir) then
          if CreateManifest(ManifestDir) then
          begin
            CompileMainModule;
            BuildThread := TBuildThread.Create(False);
          end;
end;

procedure TProjectBuilding.CompileMainModule;
const
  FW_Class = APP_DIR_STUBS + CLASS_FW;

begin
  CompileCompleted := False;

  if ProjManager.CreateProjDir(ProjManager.ProjDirHome) then
  begin
    if CheckFile(FW_Class) then
      CopyFile(FW_Class, ProjManager.ProjDirPreBuild + CLASS_FW);

    AddLogMsg('Компиляция ' + ExtractFileName(ProjManager.MainModule) + '...');

    CompileThread := TCompileThread.Create(True);
    CompileThread.ModuleName := ProjManager.MainModule;
    CompileThread.Start;
  end;
end;

procedure TProjectBuilding.Run;
begin
  Build;
  RunThread := TRunThread.Create(False);
end;

end.

