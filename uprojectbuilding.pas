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
  private
    function CompileFile(FileName: string): boolean;
    function CreateManifest: boolean;
    function DeleteCharacters(const AValue: string): string;
    function IsErr(const AValue: string): boolean;
  public
    function Build: boolean;
    function CompileMainModule: boolean;
    procedure Run;
  end;

implementation

uses
  uAMPASCore,
  uProjectManager,
  uProjectConfig,
  uIDEConfig;

type

  { TRunThread }

  TRunThread = class(TThread)
  private
    LogMsg: string;
    IsBuild: boolean;
    procedure AddMsg;
    procedure RunEmu;
    procedure ProjBuild;
  protected
    procedure Execute; override;
  end;

{ TRunThread }

procedure TRunThread.RunEmu;
begin
  with TProjectBuilding.Create do
    try
      Synchronize(@ProjBuild);
      if IsBuild then
      begin
        LogMsg := 'Emulator: запуск ' + ExtractFileName(ProjManager.JARFile) + ' ...';
        Synchronize(@AddMsg);
        if ProcStart(IDEConfig.DirectiveReplace(IDEConfig.EmulatorCmd)).Completed then
        begin
          if not Application.Terminated then
          begin
            LogMsg := 'Работа эмулятора завершена';
            Synchronize(@AddMsg);
          end;
        end;
      end;
    finally
      Free;
    end;
end;

procedure TRunThread.ProjBuild;
begin
  with TProjectBuilding.Create do
  begin
    try
      IsBuild := Build;
    finally
      Free;
    end;
  end;
end;

procedure TRunThread.AddMsg;
begin
  AddLogMsg(LogMsg);
end;

procedure TRunThread.Execute;
begin
  RunEmu;
end;

{ TProjectBuilding }

function TProjectBuilding.CreateManifest: boolean;
var
  Path, FileName: string;
  DeleteConfirm, Description, Icon, InfoURL, Name, Vendor, Version: string;

begin
  Result := False;

  Path := ProjManager.ProjDirPreBuild + 'META-INF';

  if not CreateDir(Path) then
  begin
    AddLogMsg('Не удалось создать каталог: ' + Path, lmtErr);
    Exit;
  end;

  with ProjConfig do
  begin
    DeleteConfirm := MIDletDeleteConfirm;
    Description := MIDletDescription;
    Icon := MIDletIcon;
    InfoURL := MIDletInfoURL;
    Name := MIDletName;
    Vendor := MIDletVendor;
  end;

  Version := ProjManager.MIDletVersion;

  with TStringList.Create do
  begin
    try
      Add('Manifest-Version: 1.0');
      Add('MIDlet-1: ' + Name + ', ' + Icon + ', FW');
      Add('MIDlet-Name: ' + Name);
      Add('MIDlet-Version: ' + Version);
      Add('MIDlet-Vendor: ' + Vendor);
      Add('MicroEdition-Configuration: CLDC-1.0');

      if ProjConfig.CanvasType <= 0 then
        Add('MicroEdition-Profile: MIDP-1.0')
      else
        Add('MicroEdition-Profile: MIDP-2.0');

      if Description <> '' then
        Add('MIDlet-Description: ' + Description);

      if InfoURL <> '' then
        Add('MIDlet-Info-URL: ' + InfoURL);

      if DeleteConfirm <> '' then
        Add('MIDlet-Delete-Confirm: ' + DeleteConfirm);

      FileName := Path + DIR_SEP + 'MANIFEST.MF';

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

function TProjectBuilding.CompileMainModule: boolean;
const
  FW = APP_DIR_STUBS + CLASS_FW;

begin
  Result := False;

  if CheckFile(FW) then
    CopyFile(FW, ProjManager.ProjDirPreBuild + CLASS_FW);

  if ProjManager.CreateProjDir(ProjManager.ProjDirHome) then
    if CompileFile(ProjManager.MainModule) then
      Result := True;
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
      AddLogMsg(DeleteCharacters(PCompiler.Output + 'Компиляция завершена'), lmtOk);
      Result := True;
    end
    else
      AddLogMsg(DeleteCharacters(PCompiler.Output), lmtErr);
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
      Text := StringReplace(Text, '^3', 'Добавлен: ', [rfReplaceAll]);
      Text := StringReplace(Text, '[Pascal Error] ', '', [rfReplaceAll]);

      Result := Text;
    finally
      Free;
    end;
  end;
end;

function TProjectBuilding.IsErr(const AValue: string): boolean;
begin
  Result := False;
  if Pos('[Pascal Error]', AValue) > 0 then
    Result := True;
end;

procedure TProjectBuilding.Run;
var
  RunThread: TRunThread;

begin
  RunThread := TRunThread.Create(False);
  RunThread.FreeOnTerminate := True;
end;

function TProjectBuilding.Build: boolean;

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

var
  CmdLine, JARFileName: string;

begin
  Result := False;

  if DirectoryExists(ProjManager.ProjDirPreBuild) then
    if not DirectoryIsWritable(ProjManager.ProjDirPreBuild) then
    begin
      AddLogMsg('Недостаточно прав для чтения каталога: ' + ProjManager.ProjDirPreBuild, lmtErr);
      Exit;
    end;

  if DeleteDirectory(ProjManager.ProjDirPreBuild, False) then
    CreateDir(ProjManager.ProjDirPreBuild);

  if not CompileMainModule then
    Exit;

  if not CreateManifest then
    Exit;

  JARFileName := ProjManager.JARFile;
  CmdLine := FILE_ARCHIVER + ' a "' + JARFileName + '" "';

  if FileExists(JARFileName) then
    DeleteFile(JARFileName);

  AddLogMsg('Начало архивации ' + ExtractFileName(JARFileName) + ' ...' + LE);

  if ProcStart(CmdLine + ProjManager.ProjDirPreBuild + '*"').Completed then
  begin
    if ProcStart(CmdLine + ProjManager.ProjDirRes + '*"').Completed then
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

end.

