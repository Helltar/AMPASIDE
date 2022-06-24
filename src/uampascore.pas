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

unit uAMPASCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics, SysUtils, FileUtil, Forms, process, LazFileUtils, LazUTF8,
  versionresource, versiontypes {$IFDEF MSWINDOWS}, Windows  {$ENDIF};

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

  TFileType = (ftImage, ftSound, ftPascal, ftJava, ftHTML, ftPHP, ftBinary);

const
  {$I ampasideconsts.inc}

resourcestring
  ERR_BAD_VALUE = 'Invalid value';
  ERR_COPY_DIR = 'Failed to copy directory';
  ERR_COPY_FILE = 'Failed to copy file';
  ERR_CREATE_DIR = 'Failed to create directory';
  ERR_DEL_DIR = 'Error when deleting a directory';
  ERR_DIR_404 = 'Directory not found';
  ERR_EMPTY_FILENAME = 'Error, empty value in filename';
  ERR_EMPTY_VALUE = 'Empty value';
  ERR_ERR = 'Error';
  ERR_FAILDED_BUILD_APK = 'Failed to build APK file, details';
  ERR_FAILED_CREATE_MODULE = 'Failed to create module';
  ERR_FAILED_DEL = 'Failed to delete';
  ERR_FAILED_DOWNLOAD = 'Failed to download';
  ERR_FAILED_FIND_LIB = 'Failed to find library/module';
  ERR_FAILED_OPEN = 'Failed to open';
  ERR_FAILED_OPEN_FILE_AS_PNG = 'Failed to open file as PNG';
  ERR_FAILED_RENAME = 'Failed to rename';
  ERR_FAILED_SAVE = 'Failed to save';
  ERR_FAILED_SAVE_NOTES_FILE = 'Failed to save notes file';
  ERR_FILE_404 = 'File not found';
  ERR_JEDI_CF = 'An error occurred during code formatting';
  ERR_LOAD_AUTOCOMPELE_FILE = 'Failed to load the auto-complete file';
  ERR_LOAD_IMG = 'Failed to get image list';
  ERR_LOAD_NOTES = 'Failed to load notes file';
  ERR_NO_READ_ACCESS = 'No read permissions';
  ERR_NO_WRITE_ACCESS = 'No write permissions';
  ERR_RUN_PROC = 'Process startup error';
  ERR_SAVING = 'Saving error';

  TEXT_BYTE = 'b.';
  TEXT_COMPLETED = 'completed';
  TEXT_PROC = 'Process';

function CheckDir(const DirName: string): boolean;
function CheckFile(const FileName: string): boolean;
function DelEmptyLines(const AValue: string): string;
function GetAppPath: string;
function GetCurrentUsername: string;
function GetFileSize(const FileName: string): string;
function GetFileType(const FileName: string): TFileType;
function GetProgramVersion: string;
function IsProcRunning: boolean;
function LoadImages(APath: string; ImgList: TImageList): boolean;
function MakeDir(const DirName: string): boolean;
function ProcStart(const AExecutable, AParameters: string; UsePipes: boolean = True): TProcFunc;
function ProcStart(const AParameters: string; UsePipes: boolean = True): TProcFunc;
procedure AddLogMsg(const AValue: string; MsgType: TLogMsgType = lmtText);
procedure CheckConfig(var FileName: string);
procedure TerminateProc;

implementation

uses
  uMainForm;

var
  IsProcTerminate, IsProcessRunning: boolean;

procedure CheckConfig(var FileName: string);
var
  ErrMsg: string;

begin
  if FileExists(FileName) then
  begin
    if not FileIsWritable(FileName) then
    begin
      ErrMsg := ERR_NO_WRITE_ACCESS + ': ' + FileName;
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
    ErrMsg := ERR_EMPTY_FILENAME
  else
  if not FileExists(FileName) then
    ErrMsg := ERR_FILE_404 + ': ' + FileName
  else
  if not FileIsWritable(FileName) then
    ErrMsg := ERR_NO_READ_ACCESS + ': ' + FileName
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
      AddLogMsg(ERR_NO_READ_ACCESS + ': ' + DirName, lmtErr);
  end
  else
    AddLogMsg(ERR_DIR_404 + ': ' + DirName, lmtErr);
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
    Result := IntToStr(IntSize) + ' ' + TEXT_BYTE
  else
  if (IntSize div 1024) >= 1024 then
    Result := IntToStr((IntSize div 1024) div 1024) + ' MB'
  else
    Result := IntToStr(IntSize div 1024) + ' KB';
end;

function ProcStart(const AExecutable, AParameters: string; UsePipes: boolean): TProcFunc;
var
  P: TProcess;

begin
  Result.Completed := False;

  P := TProcess.Create(nil);

  //P.Executable := AExecutable;
  //P.Parameters.Add(AParameters);

  if AExecutable <> '' then
    P.CommandLine := '"' + AExecutable + '" ' + AParameters
  else
    P.CommandLine := AParameters;

  P.Options := [poStderrToOutPut, poNoConsole];

  if UsePipes then
    P.Options := P.Options + [poUsePipes];

  try
    try
      P.Execute;

      while P.Running do
      begin
        IsProcessRunning := True;
        if IsProcTerminate then
          if P.Terminate(0) then
            AddLogMsg(TEXT_PROC + ' (PID: ' + IntToStr(P.ProcessID) + ') ' + TEXT_COMPLETED);
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
      AddLogMsg(ERR_RUN_PROC + ': ' + P.CommandLine, lmtErr);
    end;
  finally
    IsProcessRunning := False;
    IsProcTerminate := False;
    FreeAndNil(P);
  end;
end;

function ProcStart(const AParameters: string; UsePipes: boolean): TProcFunc;
begin
  Result := ProcStart('', AParameters, UsePipes);
end;

procedure TerminateProc;
begin
  IsProcTerminate := True;
end;

function DelEmptyLines(const AValue: string): string;
var
  i: integer;

begin
  with TStringList.Create do
    try
      Text := AValue;
      for i := Count - 1 downto 0 do
        if Strings[i] = '' then
          Delete(i);
      Result := Text;
    finally
      Free;
    end;
end;

function MakeDir(const DirName: string): boolean;
begin
  Result := True;
  if not DirectoryExists(DirName) then
    if not CreateDir(DirName) then
    begin
      Result := False;
      AddLogMsg(ERR_CREATE_DIR + ': ' + DirName, lmtErr);
    end;
end;

function LoadImages(APath: string; ImgList: TImageList): boolean;
var
  sList: TStringList;
  Img: TPortableNetworkGraphic;
  FileName: string;
  i: integer;

begin
  Result := False;

  sList := TStringList.Create;
  Img := TPortableNetworkGraphic.Create;

  APath := GetAppPath + APP_DIR_IMG + APath + DIR_SEP;
  FileName := APath + 'img_list';

  try
    try
      sList.LoadFromFile(FileName);
    except
      AddLogMsg(ERR_LOAD_IMG + ': ' + FileName, lmtErr);
    end;

    for i := 0 to sList.Count - 1 do
    begin
      FileName := APath + sList.Strings[i] + '.png';
      if CheckFile(FileName) then
      begin
        try
          Img.LoadFromFile(FileName);
          ImgList.Add(Img, nil);
        except
          AddLogMsg(ERR_FAILED_OPEN_FILE_AS_PNG + ': ' + FileName, lmtErr);
        end;
      end;
    end;

    Result := True;
  finally
    FreeAndNil(Img);
    FreeAndNil(sList);
  end;
end;

function IsProcRunning: boolean;
begin
  Result := IsProcessRunning;
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
  Result := ftBinary;

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

function GetAppPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
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
  if not Application.Terminated then
    Synchronize(@AddMsg);
end;

end.
