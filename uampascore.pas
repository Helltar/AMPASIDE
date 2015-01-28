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
  Classes, Controls, Graphics, FileUtil, SysUtils, process, versionresource,
  versiontypes {$IFDEF MSWINDOWS}, Windows  {$ENDIF};

type

  { TProcFunc }

  TProcFunc = record
    Completed: boolean;
    Output: string;
  end;

  { TLogMsgType }

  TLogMsgType = (lmtText, lmtOk, lmtErr);

  { TFileType }

  TFileType = (ftImage, ftSound, ftPascal, ftJava, ftHTML, ftPHP);

const
  {$I ampasideconsts.inc}

function CheckFile(const FileName: string): boolean;
function GetCurrentUsername: string;
function GetFileSize(const FileName: string): string;
function GetFileType(const FileName: string): TFileType;
function GetProgramVersion: string;
function IsProcRunning: boolean;
function LoadImages(const APath: string; ImgList: TImageList): boolean;
function ProcStart(const CmdLine: string): TProcFunc;
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
  begin
    if not DirectoryIsWritable(ExtractFilePath(FileName)) then
    begin
      ErrMsg := 'Недостаточно прав для записи в каталог: ' + ExtractFilePath(FileName);
      FileName := '';
    end;
  end;

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

function ProcStart(const CmdLine: string): TProcFunc;
const
  READ_BYTES = 2048;

var
  MemStream: TMemoryStream;
  BytesRead: longint;
  P: TProcess;
  NumBytes: longint;

begin
  Result.Completed := False;

  MemStream := TMemoryStream.Create;
  BytesRead := 0;

  P := TProcess.Create(nil);
  P.CommandLine := CmdLine;
  P.Options := [poUsePipes, poStderrToOutPut, poNoConsole];

  try
    try
      P.Execute;

      while P.Running do
      begin
        ProcessRunning := True;
        if ProcessTerminate then
          P.Terminate(0);
        //AddLogMsg('Процесс (PID: ' + IntToStr(P.ProcessID) + ') завершен', lmtOk);
        Sleep(1);
      end;

      while True do
      begin
        MemStream.SetSize(BytesRead + READ_BYTES);
        NumBytes := P.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes)
        else
          Break;
      end;

      MemStream.SetSize(BytesRead);

      with TStringList.Create do
        try
          LoadFromStream(MemStream);
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
    FreeAndNil(MemStream);
  end;
end;

procedure TerminateProc;
begin
  ProcessTerminate := True;
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
begin
  frmMain.AddLogMsg(AValue, MsgType);
end;

end.

