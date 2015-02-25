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

{ TODO : --> XMLWrite, etc... }

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
    function CreateAndroidManifest(const FileName, Package, NameVers: string; CodeVers: integer): boolean;
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

function TAndroidBuildingThread.CreateAndroidManifest(const FileName, Package, NameVers: string; CodeVers: integer): boolean;
begin
  Result := False;
  with TStringList.Create do
  begin
    try
      Add('<?xml version="1.0" encoding="utf-8"?>');
      Add('<manifest xmlns:android="http://schemas.android.com/apk/res/android"');
      Add('    package="' + Package + '"');
      Add('    android:versionCode="' + IntToStr(CodeVers) + '"');
      Add('    android:versionName="' + NameVers + '" >');
      Add('    <uses-permission android:name="android.permission.INTERNET" />');
      Add('    <uses-permission android:name="android.permission.VIBRATE" />');
      Add('    <uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />');
      Add('    <supports-screens android:smallScreens="true" android:normalScreens="true" android:largeScreens="true" android:anyDensity="true" />');
      Add('    <uses-sdk');
      Add('        android:minSdkVersion="4"');
      Add('        android:targetSdkVersion="20" />');
      Add('    <application android:label="@string/app_name" android:icon="@drawable/app_icon">');
      Add('        <activity');
      Add('            android:name="org.microemu.android.MicroEmulator"');
      Add('            android:configChanges="orientation|keyboardHidden">');
      Add('            <intent-filter>');
      Add('                <action android:name="android.intent.action.MAIN" />');
      Add('                <category android:name="android.intent.category.LAUNCHER" />');
      Add('            </intent-filter>');
      Add('        </activity>');
      Add('    </application>');
      Add('</manifest>');
      try
        SaveToFile(FileName);
        Result := True;
      except
        AddLogMsg('Не удалось сохранить: ' + FileName, lmtErr);
      end;
    finally
      Free;
    end;
  end;
end;

function TAndroidBuildingThread.CreateStringsFile(const FileName, AppName, MainClass, JadName: string): boolean;
begin
  Result := False;
  with TStringList.Create do
  begin
    try
      Add('<?xml version="1.0" encoding="utf-8"?>');
      Add('<resources>');
      Add('    <string name="app_name">' + AppName + '</string>');
      Add('    <string name="class_name">' + MainClass + '</string>');
      Add('    <string name="jad_name">' + JadName + '</string>');
      Add('</resources>');
      try
        SaveToFile(FileName);
        Result := True;
      except
        AddLogMsg('Не удалось сохранить: ' + FileName, lmtErr);
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
    sList.Add('    <property name="midlet.jad" value="' + JadName + '" />');
    sList.Add('    <property name="midlet.package" value="' + ApkName + '" />');
    sList.Add('    <property name="outdir" value="' + Outdir + '" />');
    with TStringList.Create do
      try
        try
          LoadFromFile(FileName);
          sList.Add(Text);
        except
          AddLogMsg('Не удалось загрузить: ' + FileName, lmtErr);
        end;
      finally
        Free;
      end;
    try
      sList.SaveToFile(FileName);
      Result := True;
    except
      AddLogMsg('Не удалось сохранить: ' + FileName, lmtErr);
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
      if MakeDir(GetAppPath + APP_DIR_TMP) then
        if CopyFile(AntBuildFile, ProjBuildFile) then
          if CreateBuildFile(ProjBuildFile, ProjManager.JadFile, ApkName, ProjManager.ProjDirPreBuild) then
            if CreateAndroidManifest(GetAppPath + APP_DIR_TMP + 'AndroidManifest.xml',
              'org.microemu.android.' + MIDletName, ProjManager.MIDletVersion, ProjConfig.VersMajor) then
              if CreateStringsFile(GetAppPath + APP_DIR_TMP + 'strings.xml', MIDletName, 'FW', MIDletName + EXT_JAD) then
                Result := True;
  end;

  procedure DelTempFiles;
  begin
    // tools/android/build.myMIDlet.xml
    DeleteFile(ProjBuildFile);
    // tools/android/src/org/microemu/android/myMIDlet (R.java)
    DeleteDirectory(GetAppPath + APP_DIR_ANDROID + 'src' + DIR_SEP + 'org' + DIR_SEP + 'microemu' + DIR_SEP + 'android' + DIR_SEP + MIDletName, False);
    DeleteDirectory(GetAppPath + APP_DIR_TMP, False);
  end;

var
  ApkFileName: string;
  P: TProcFunc;

begin
  MIDletName := ProjConfig.MIDletName;
  ProjBuildFile := ExtractFilePath(AntBuildFile) + 'build.' + MIDletName + '.xml';
  ApkName := MIDletName + EXT_APK;

  if not PreBuildAct then
    Exit;

  AddLogMsg('Apache Ant (' + ApkName + '), идет сборка, это займет около минуты...');

  P := ProcStart('ant -buildfile ' + ProjBuildFile);

  if not P.Completed then
    Exit;

  // ant.log
  with TStringList.Create do
  begin
    try
      Text := P.Output;
      SaveToFile(GetAppPath + ANT_LOG);
    finally
      Free;
    end;
  end;

  if Pos('BUILD SUCCESSFUL', P.Output) > 0 then
  begin
    ApkFileName := ProjManager.ProjDirPreBuild + ApkName;

    // pre-build/myMIDlet.apk -> bin/android/myMIDlet.apk
    if RenameFile(ApkFileName, ProjManager.ProjDirAndroid + ApkName) then
      ApkFileName := ProjManager.ApkFile;

    AddLogMsg('Проект успешно собран' + LE +
      'Версия: ' + ProjManager.MIDletVersion + LE +
      'Размер: ' + GetFileSize(ApkFileName) + LE +
      'Платформа: Android', lmtOk);

    DelTempFiles;
  end
  else
  if Pos('BUILD FAILED', P.Output) > 0 then
    AddLogMsg('Не удалось собрать APK файл, подробности: ' + ANT_LOG, lmtErr)
  else
    AddLogMsg('Ant завершил работу, подробности: ' + ANT_LOG);
end;

end.

