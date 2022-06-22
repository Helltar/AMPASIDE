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

unit uAndroidBuilding;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

type

  { TAndroidBuildingThread }

  TAndroidBuildingThread = class(TThread)
  private
    FAntBuildFile: string;
    function CreateBuildFile(const FileName, JadName, ApkName, Outdir: string): boolean;
    function CreateStringsFile(const FileName, AppName, MainClass, JadName: string): boolean;
    procedure BuildAPK(const AntBuildFile: string);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
    property AntBuildFile: string write FAntBuildFile;
  end;

implementation

uses
  uAMPASCore,
  uProjectBuilding,
  uProjectConfig,
  uProjectManager;

{ TAndroidBuildingThread }

constructor TAndroidBuildingThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TAndroidBuildingThread.Execute;
begin
  BuildAPK(FAntBuildFile);
end;

function TAndroidBuildingThread.CreateStringsFile(const FileName, AppName, MainClass, JadName: string): boolean;
begin
  Result := False;
  with TStringList.Create do
  begin
    try
      Add('<?xml version="1.0" encoding="utf-8"?>');
      Add('<resources>');
      Add('<string name="app_name">' + AppName + '</string>');
      Add('<string name="class_name">' + MainClass + '</string>');
      Add('<string name="jad_name">' + JadName + '</string>');
      Add('</resources>');
      try
        SaveToFile(FileName);
        Result := True;
      except
        AddLogMsg(ERR_FAILED_SAVE + ': ' + FileName, lmtErr);
      end;
    finally
      Free;
    end;
  end;
end;

function TAndroidBuildingThread.CreateBuildFile(const FileName, JadName, ApkName, Outdir: string): boolean;
var
  sList: TStringList;

begin
  Result := False;
  sList := TStringList.Create;
  try
    sList.Add('<project name="microemu-android" default="package-apk">' + LE);
    sList.Add('<property name="midlet.jad" value="' + JadName + '" />');
    sList.Add('<property name="midlet.package" value="' + ApkName + '" />');
    sList.Add('<property name="outdir" value="' + Outdir + '" />');
    sList.Add('<property name="sdk-folder" value="' + GetAppPath + APP_DIR_ANDROID + 'sdk" />');
    sList.Add('<property name="root-folder" value="' + GetAppPath + APP_DIR_ANDROID + '" />');

    with TStringList.Create do
      try
        try
          LoadFromFile(FileName);
          sList.Add(Text);
        except
          AddLogMsg(ERR_FAILED_DOWNLOAD + ': ' + FileName, lmtErr);
        end;
      finally
        Free;
      end;
    try
      sList.SaveToFile(FileName);
      Result := True;
    except
      AddLogMsg(ERR_FAILED_SAVE + ': ' + FileName, lmtErr);
    end;
  finally
    FreeAndNil(sList);
  end;
end;

procedure TAndroidBuildingThread.BuildAPK(const AntBuildFile: string);
var
  ApkName, MIDletName: string;
  ProjBuildFile: string;

  function PreBuildAct: boolean;
  begin
    Result := False;

    with TProjectBuilding.Create do
      try
        if not Build then
          Exit;
      finally
        Free;
      end;

    if CheckFile(AntBuildFile) then
      if MakeDir(GetAppPath + APP_DIR_TMP) and MakeDir(GetAppPath + APP_DIR_LOGS) then
        if CopyFile(AntBuildFile, ProjBuildFile) then
          if CreateBuildFile(ProjBuildFile, ProjManager.JadFile,
            ApkName, ProjManager.ProjDirPreBuild) then
            if CreateStringsFile(GetAppPath + APP_DIR_TMP + 'strings.xml', MIDletName, 'FW', MIDletName + EXT_JAD) then
              Result := True;
  end;

  procedure DelTempFiles;
  begin
    DeleteFile(ProjBuildFile);
    DeleteDirectory(GetAppPath + APP_DIR_ANDROID + 'src' + DIR_SEP + 'org' + DIR_SEP + 'microemu' + DIR_SEP + 'android' + DIR_SEP + MIDletName, False);
    DeleteDirectory(GetAppPath + APP_DIR_TMP, False);
  end;

var
  ApkFileName: string;
  AntOutput, AntCmd: string;
  P: TProcFunc;

begin
  MIDletName := ProjConfig.MIDletName;
  ProjBuildFile := APP_DIR_TMP + 'build.' + MIDletName + '.xml';
  ApkName := MIDletName + EXT_APK;

  if not PreBuildAct then
    Exit;

  AddLogMsg('Apache Ant (' + ApkName + '), ' + MSG_GOING_ASSEMBLED + '...');

  AntCmd := GetAppPath + APACHE_ANT + ProjBuildFile;

  {$IFDEF MSWINDOWS}
  AntCmd := AntCmd + ' -logfile ' + GetAppPath + ANT_LOG;
  {$ENDIF}

  P := ProcStart(AntCmd);

  if not P.Completed then
    Exit;

  // ant.log
  if P.Output <> EmptyStr then
  begin
    with TStringList.Create do
    begin
      try
        Text := P.Output;
        AntOutput := Text;
        SaveToFile(GetAppPath + ANT_LOG);
      finally
        Free;
      end;
    end;
  end
  else
    with TStringList.Create do
      try
        LoadFromFile(GetAppPath + ANT_LOG);
        AntOutput := Text;
      finally
        Free;
      end;

  if Pos('BUILD FAILED', AntOutput) = 0 then  // if Pos('BUILD SUCCESSFUL', AntOutput) > 0 then (windows output bug)
  begin
    AddLogMsg(ERR_ANT_COMPLETED_WORK + ': ./' + ANT_LOG);

    ApkFileName := ProjManager.ProjDirPreBuild + ApkName;

    // ../pre-build/midlet.apk -> ../bin/android/midlet.apk
    if RenameFile(ApkFileName, ProjManager.ProjDirAndroid + ApkName) then
      ApkFileName := ProjManager.ApkFile;

    AddLogMsg(MSG_SUCCESSFULLY_ASSEMBLED + LE +
      MSG_VERSION + ': ' + ProjManager.MIDletVersion + LE + MSG_SIZE + ': ' +
      GetFileSize(ApkFileName) + LE + MSG_PLATFORM + ': Android', lmtOk);

    DelTempFiles;
  end
  else
    AddLogMsg(ERR_FAILDED_BUILD_APK + ': ./' + ANT_LOG, lmtErr);
end;

end.
