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

unit uFileManager;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, FileUtil, ComCtrls, Controls, Dialogs, LCLIntf, LazUTF8;

type

  { TFileManager }

  TFileManager = class
  private
    FOwner: TTreeView;
    function GetFileName(ANode: TTreeNode): string;
    function GetParentNode: TTreeNode;
    function GetSelectedNode: TTreeNode;
    function TreeViewAddChild(ParentNode: TTreeNode; const NodeName: string): TTreeNode;
    procedure AddChildNode(ParentNode: TTreeNode; const NodeName: string; IsDir: boolean = False);
    procedure DeleteSelectedNode;
  public
    constructor Create(AOwner: TTreeView);
    destructor Destroy; override;

    function GetDirNameOnly(const FileName: string): string;
    function GetPath: string;
    function IsDirectory(const DirName: string): boolean;
    function SelectedFileName: string;

    procedure AddFile(const FileName: string);
    procedure AddFilesFromDir(const DirName: string; ParentNode: TTreeNode);
    procedure OpenFile(const FileName: string);
    procedure UpdateFileList;

    procedure ShowAddFilesDialog;
    procedure ShowDeleteDialog;
    procedure ShowNewDirDialog;
    procedure ShowRenFileDialog;
  end;

var
  FileManager: TFileManager;

implementation

uses
  uAMPASCore,
  uCodeEditor,
  uProjectManager;

{ TFileManager }

constructor TFileManager.Create(AOwner: TTreeView);
begin
  FOwner := AOwner;
end;

destructor TFileManager.Destroy;
begin
  inherited Destroy;
end;

function TFileManager.SelectedFileName: string;
begin
  Result := GetFileName(GetSelectedNode);
end;

procedure TFileManager.AddFile(const FileName: string);
var
  DestFileName: string;

begin
  if not Assigned(GetSelectedNode) then
    Exit;

  DestFileName := GetPath + ExtractFileName(FileName);

  if FileExists(DestFileName) then
    case MessageDlg('Подтвердите действие', 'Перезаписать файл "' + ExtractFileName(DestFileName) + '"?',
        mtInformation, [mbYes, mbNo], 0) of
      mrNo: Exit;
    end;

  if CopyFile(FileName, DestFileName) then
    AddChildNode(GetParentNode, ExtractFileName(FileName))
  else
    AddLogMsg('Не удалось скопировать файл: ' + FileName);
end;

function TFileManager.IsDirectory(const DirName: string): boolean;
begin
  Result := False;
  if DirectoryExists(DirName) then
    Result := True;
end;

function TFileManager.GetDirNameOnly(const FileName: string): string;
begin
  Result := ExtractFileName(ExtractFileDir(FileName));
end;

function TFileManager.GetPath: string;
var
  FileName: string;

begin
  FileName := SelectedFileName;

  if IsDirectory(FileName) then
    Result := FileName + DIR_SEP
  else
    Result := ExtractFilePath(FileName);
end;

function TFileManager.GetFileName(ANode: TTreeNode): string;
begin
  Result := ANode.Text;

  while ANode.Parent <> nil do
  begin
    ANode := ANode.Parent;
    Result := ANode.Text + DIR_SEP + Result;
  end;

  UTF8Delete(Result, 1, UTF8Length(ANode.Text));

  case ANode.Index of
    0: Result := ExtractFileDir(ProjManager.ProjDirSrc) + Result;
    1: Result := ExtractFileDir(ProjManager.ProjDirRes) + Result;
  end;
end;

function TFileManager.GetParentNode: TTreeNode;
begin
  if IsDirectory(SelectedFileName) then
    Result := GetSelectedNode
  else
    Result := GetSelectedNode.Parent;
end;

function TFileManager.GetSelectedNode: TTreeNode;
begin
  Result := FOwner.Selected;
end;

procedure TFileManager.DeleteSelectedNode;
begin
  FOwner.Selected.Delete;
end;

function TFileManager.TreeViewAddChild(ParentNode: TTreeNode; const NodeName: string): TTreeNode;
begin
  Result := FOwner.Items.AddChild(ParentNode, NodeName);
end;

procedure TFileManager.AddChildNode(ParentNode: TTreeNode; const NodeName: string; IsDir: boolean);
var
  ChildNode: TTreeNode;
  ImgIndex: integer;

begin
  ChildNode := TreeViewAddChild(ParentNode, NodeName);

  if IsDir then
  begin
    ChildNode.Index := 0;
    ImgIndex := 5;
  end
  else
    case GetFileType(NodeName) of
      ftImage: ImgIndex := 8;
      ftSound: ImgIndex := 11;
      ftPascal: ImgIndex := 12;
      else
        ImgIndex := 1;
    end;

  ChildNode.ImageIndex := ImgIndex;
  ChildNode.SelectedIndex := ImgIndex;
  ChildNode.Selected := True;
end;

procedure TFileManager.AddFilesFromDir(const DirName: string; ParentNode: TTreeNode);
var
  SR: TSearchRec;

  procedure AddDirs;
  begin
    if FindFirst(DirName + '*', faDirectory, SR) = 0 then
    begin
      repeat
        if (SR.Attr and faDirectory) <> 0 then
        begin
          ParentNode.ImageIndex := 5;
          ParentNode.SelectedIndex := 5;
          if (SR.Name <> '.') and (SR.Name <> '..') then
            AddFilesFromDir(DirName + SR.Name + DIR_SEP, TreeViewAddChild(ParentNode, SR.Name));
        end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  end;

  procedure AddFiles;
  begin
    if FindFirst(DirName + '*', faAnyFile, SR) = 0 then
    begin
      repeat
        if (SR.Attr and faDirectory) = 0 then
          { TODO : сделать .ampasignore или сохранение в xml, etc... }
          if ExtractFileExt(SR.Name) <> '.bak' then
            AddChildNode(ParentNode, SR.Name);
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  end;

begin
  AddDirs;
  AddFiles;
end;

procedure TFileManager.OpenFile(const FileName: string);
begin
  if ExtractFileExt(FileName) = EXT_MODULE then
    CodeEditor.LoadFile(FileName)
  else
    OpenDocument(FileName);
end;

procedure TFileManager.UpdateFileList;
begin
  with FOwner do
  begin
    Items.Clear;

    BeginUpdate;

    AddFilesFromDir(ProjManager.ProjDirSrc, Items.Add(nil, 'Модули'));
    Items.GetLastNode.ImageIndex := 6;
    Items.GetLastNode.SelectedIndex := 6;

    AddFilesFromDir(ProjManager.ProjDirRes, Items.Add(nil, 'Ресурсы'));
    Items.GetLastNode.ImageIndex := 10;
    Items.GetLastNode.SelectedIndex := 10;

    Items.Item[0].Selected := True;
    Items.Item[1].Selected := True;

    EndUpdate;
  end;
end;

procedure TFileManager.ShowAddFilesDialog;
var
  i: integer;

begin
  with TOpenDialog.Create(nil) do
  begin
    try
      Title := 'Добавить файлы в "' + GetDirNameOnly(GetPath) + '"';
      Filter := 'Все файлы *|*';
      Options := [ofAllowMultiSelect, ofEnableSizing, ofViewDetail];
      if Execute then
        for i := 0 to Files.Count - 1 do
          AddFile(Files.Strings[i]);
    finally
      Free;
    end;
  end;
end;

procedure TFileManager.ShowDeleteDialog;
var
  FileName: string;

begin
  FileName := SelectedFileName;

  if IsDirectory(FileName) then
  begin
    case MessageDlg('Подтвердите действие', 'Удалить каталог "' + GetDirNameOnly(GetPath) + '"?',
        mtInformation, [mbYes, mbNo], 0) of
      mrYes:
      begin
        if DeleteDirectory(FileName, False) then
          DeleteSelectedNode
        else
          MessageDlg('Ошибка', 'Не удалось удалить каталог: ' + FileName, mtError, [mbOK], 0);
      end;
    end;
  end
  else
  begin
    case MessageDlg('Подтвердите действие', 'Удалить файл "' + ExtractFileName(FileName) + '"?',
        mtInformation, [mbYes, mbNo], 0) of
      mrYes:
      begin
        if DeleteFile(FileName) then
          DeleteSelectedNode
        else
          MessageDlg('Ошибка', 'Не удалось удалить файл: ' + FileName, mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TFileManager.ShowNewDirDialog;
var
  DirName: string;

begin
  if InputQuery('Создание каталога', 'Введите название:', DirName) then
  begin
    if DirName = '' then
      MessageDlg('Ошибка', 'Пустое значение', mtError, [mbOK], 0)
    else
    if CreateDir(GetPath + DirName) then
      AddChildNode(GetParentNode, DirName, True)
    else
      MessageDlg('Ошибка', 'Не удалось создать каталог', mtError, [mbOK], 0);
  end;
end;

procedure TFileManager.ShowRenFileDialog;
var
  FileName, NewName: string;

begin
  FileName := SelectedFileName;
  NewName := ExtractFileName(FileName);

  if InputQuery('Переименование', 'Новое имя:', NewName) then
  begin
    if NewName = '' then
      MessageDlg('Ошибка', 'Пустое значение', mtError, [mbOK], 0)
    else
    if RenameFile(FileName, ExtractFilePath(FileName) + NewName) then
      FOwner.Selected.Text := NewName
    else
      MessageDlg('Ошибка', 'Не удалось переименовать: ' + FileName, mtError, [mbOK], 0);
  end;
end;

end.

