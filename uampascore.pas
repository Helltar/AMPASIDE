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

unit uAMPASCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics, SysUtils, FileUtil, process, versionresource,
  versiontypes {$IFDEF MSWINDOWS}, Windows  {$ENDIF};

type

  { TLogMsgType }

  TLogMsgType = (lmtText, lmtOk, lmtErr);

  { TLogThread }

  TLogThread = class(TThread)
  private
    FMsgType: TLogMsgType;
    FText: string;
    procedure AddMsg;
  protected
    procedure Execute; override;
  public
    property Text: string read FText write FText;
    property MsgType: TLogMsgType read FMsgType write FMsgType;
  end;

  { TProcFunc }

  TProcFunc = record
    Completed: boolean;
    Output: string;
  end;

  { TFileType }

  TFileType = (ftImage, ftSound, ftPascal, ftJava, ftHTML, ftPHP);

const
  {$I ampasideconsts.inc}

function CheckDir(const DirName: string): boolean;
function CheckFile(const FileName: string): boolean;
function GetCurrentUsername: string;
function GetFileSize(const FileName: string): string;
function GetFileType(const FileName: string): TFileType;
function GetProgramVersion: string;
function IsProcRunning: boolean;
function LoadImages(const APath: string; ImgList: TImageList): boolean;
function MakeDir(const DirName: string): boolean;
function ProcStart(const CmdLine: string; UsePipes: boolean = True): TProcFunc;
procedure AddLogMsg(const AValue: string; MsgType: TLogMsgType = lmtText);
procedure CheckConfig(var FileName: string);
procedure TerminateProc;

implementation

uses
  uMainForm;

var
  ProcessTerminate, ProcessRunning: boolean;

procedure CheckConfig(var FileName: string);
var
  ErrMsg: string;

begin
  if FileExists(FileName) then
  begin
    if not FileIsWritable(FileName) then
    begin
      ErrMsg := 'Недостаточно прав для записи в файл: ' + FileName;
      FileName := '';
    end;
  end
  else
  if not CheckDir(ExtractFilePath(FileName)) then
    FileName := '';

  if ErrMsg <> '' then
    AddLogMsg(ErrMsg, lmtErr);
end;

function CheckFile(const FileName: string): boolean;
var
  ErrMsg: string;

begin
  Result := False;

  if FileName = '' then
    ErrMsg := 'Ошибка, пустое значение в имени файла'
  else
  if not FileExists(FileName) then
    ErrMsg := 'Файл не найден: ' + FileName
  else
  if not FileIsWritable(FileName) then
    ErrMsg := 'Недостаточно прав для чтения файла: ' + FileName
  else
    Result := True;

  if not Result then
    AddLogMsg(ErrMsg, lmtErr);
end;

function GetProgramVersion: string;
var
  ResStream: TResourceStream;
  vRes: TVersionResource;
  vFixInf: TVersionFixedInfo;

begin
  try
    ResStream := TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION));
    try
      vRes := TVersionResource.Create;
      try
        vRes.SetCustomRawDataStream(ResStream);
        vFixInf := vRes.FixedInfo;
        Result := IntToStr(vFixInf.FileVersion[0]) + '.' + IntToStr(vFixInf.FileVersion[1]) + '.' + IntToStr(vFixInf.FileVersion[2]);
        vRes.SetCustomRawDataStream(nil);
      finally
        FreeAndNil(vRes);
      end
    finally
      FreeAndNil(ResStream);
    end
  except
    Result := APP_VERSION;
  end;
end;

function CheckDir(const DirName: string): boolean;
begin
  Result := False;
  if DirectoryExists(DirName) then
  begin
    if DirectoryIsWritable(DirName) then
      Result := True
    else
      AddLogMsg('Недостаточно прав для чтения каталога: ' + DirName, lmtErr);
  end
  else
    AddLogMsg('Каталога не существует: ' + DirName, lmtErr);
end;

function GetCurrentUsername: string;
begin
  {$IFDEF UNIX}
  Result := 'USER';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := 'USERNAME';
  {$ENDIF}
  Result := GetEnvironmentVariableUTF8(Result);
end;

function GetFileSize(const FileName: string): string;
var
  IntSize: integer;

begin
  IntSize := FileSize(FileName);

  if IntSize < 1024 then
    Result := IntToStr(IntSize) + ' байт'
  else
  if (IntSize div 1024) >= 1024 then
    Result := IntToStr((IntSize div 1024) div 1024) + ' MB'
  else
    Result := IntToStr(IntSize div 1024) + ' KB';
end;

function ProcStart(const CmdLine: string; UsePipes: boolean): TProcFunc;
var
  P: TProcess;

begin
  Result.Completed := False;

  P := TProcess.Create(nil);
  P.CommandLine := CmdLine;
  P.Options := [poStderrToOutPut, poNoConsole];

  if UsePipes then
    P.Options := [poUsePipes] + P.Options;

  try
    try
      P.Execute;

      while P.Running do
      begin
        ProcessRunning := True;
        if ProcessTerminate then
          P.Terminate(0);
        Sleep(1);
      end;

      if UsePipes then
        with TStringList.Create do
          try
            LoadFromStream(P.Output);
            Result.Output := Text;
          finally
            Free;
          end;

      Result.Completed := True;
    except
      AddLogMsg('Ошибка при запуске процесса: ' + CmdLine, lmtErr);
    end;
  finally
    ProcessRunning := False;
    ProcessTerminate := False;
    FreeAndNil(P);
  end;
end;

procedure TerminateProc;
begin
  ProcessTerminate := True;
end;

function MakeDir(const DirName: string): boolean;
begin
  Result := True;
  if not DirectoryExists(DirName) then
    if not CreateDir(DirName) then
    begin
      Result := False;
      AddLogMsg('Ошибка создания каталога: ' + DirName, lmtErr);
    end;
end;

function LoadImages(const APath: string; ImgList: TImageList): boolean;
var
  StrList: TStringList;
  Img: TPortableNetworkGraphic;
  FileName: string;
  i: integer;

begin
  Result := False;

  StrList := TStringList.Create;
  Img := TPortableNetworkGraphic.Create;
  FileName := APP_DIR_IMG + APath + DIR_SEP + 'img_list';

  try
    try
      StrList.LoadFromFile(FileName);
    except
      AddLogMsg('Не удалось получить список изображений: ' + FileName, lmtErr);
    end;

    for i := 0 to StrList.Count - 1 do
    begin
      FileName := APP_DIR_IMG + APath + DIR_SEP + StrList.Strings[i] + '.png';
      if CheckFile(FileName) then
      begin
        try
          Img.LoadFromFile(FileName);
          ImgList.Add(Img, nil);
        except
          AddLogMsg('Не удалось открыть файл как PNG: ' + FileName, lmtErr);
        end;
      end;
    end;

    Result := True;
  finally
    FreeAndNil(Img);
    FreeAndNil(StrList);
  end;
end;

function IsProcRunning: boolean;
begin
  Result := ProcessRunning;
end;

function GetFileType(const FileName: string): TFileType;

  function InAr(StringArray: array of string): boolean;
  var
    i: integer;

  begin
    Result := False;
    for i := 0 to High(StringArray) do
      if ExtractFileExt(FileName) = '.' + StringArray[i] then
      begin
        Result := True;
        Break;
      end;
  end;

var
  ImgExt: array[0..3] of string = ('png', 'jpg', 'gif', 'ico');
  SndExt: array[0..4] of string = ('amr', 'mp3', 'wav', 'mid', 'aac');
  PasExt: array[0..3] of string = ('pas', 'pp', 'lpr', 'dpr');
  JavaExt: array[0..0] of string = ('java');
  HTMLExt: array[0..1] of string = ('html', 'htm');
  PHPExt: array[0..0] of string = ('php');

begin
  if InAr(ImgExt) then
    Result := ftImage
  else
  if InAr(SndExt) then
    Result := ftSound
  else
  if InAr(PasExt) then
    Result := ftPascal
  else
  if InAr(JavaExt) then
    Result := ftJava
  else
  if InAr(PHPExt) then
    Result := ftPHP
  else
  if InAr(HTMLExt) then
    Result := ftHTML;
end;

procedure AddLogMsg(const AValue: string; MsgType: TLogMsgType);
var
  LogThread: TLogThread;

begin
  LogThread := TLogThread.Create(True);
  LogThread.FreeOnTerminate := True;
  LogThread.Text := AValue;
  LogThread.MsgType := MsgType;
  LogThread.Start;
end;

{ TLogThread }

procedure TLogThread.AddMsg;
begin
  frmMain.AddLogMsg(FText, FMsgType);
end;

procedure TLogThread.Execute;
begin
  Synchronize(@AddMsg);
end;

end.

