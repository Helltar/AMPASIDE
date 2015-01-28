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

unit uFileManagerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, ActnList, ComCtrls, Controls, ExtCtrls, Forms, Menus, StdCtrls, SysUtils, TreeFilterEdit;

type

  { TFileManagerFrame }

  TFileManagerFrame = class(TFrame)
    actExpandCollapse: TAction;
    actAddFiles: TAction;
    actCreateDir: TAction;
    actUpdateFileList: TAction;
    actOpenFile: TAction;
    actRenameFile: TAction;
    actDeleteFile: TAction;
    actlMain: TActionList;
    gbFileInfo: TGroupBox;
    ilMain: TImageList;
    ilPreview: TImageList;
    imgPreview: TImage;
    lblFileName: TLabel;
    lblFileSize: TLabel;
    lblImgWidth: TLabel;
    lblImgHeight: TLabel;
    miCreateDir: TMenuItem;
    miAddFiles: TMenuItem;
    MenuItem2: TMenuItem;
    miOpenFile: TMenuItem;
    miDeleteFile: TMenuItem;
    miiRenameFile: TMenuItem;
    MenuItem4: TMenuItem;
    pmFileManager: TPopupMenu;
    splBottom: TSplitter;
    tlbManagerActions: TToolBar;
    tbbExpandCollapse: TToolButton;
    tbbUpdateFileList: TToolButton;
    tfedtSearchFile: TTreeFilterEdit;
    tvFileList: TTreeView;
    procedure actCreateDirExecute(Sender: TObject);
    procedure actDeleteFileExecute(Sender: TObject);
    procedure actExpandCollapseExecute(Sender: TObject);
    procedure actAddFilesExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actOpenFileUpdate(Sender: TObject);
    procedure actRenameFileExecute(Sender: TObject);
    procedure actUpdateFileListExecute(Sender: TObject);
    procedure tvFileListChange(Sender: TObject; Node: TTreeNode);
    procedure tvFileListDblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateFileList;
    procedure UpdateImgPreview;
  end;

implementation

uses
  uAMPASCore,
  uIDEConfig,
  uFileManager,
  uProjectManager;

var
  FileManager: TFileManager;

{$R *.lfm}

{ TFileManagerFrame }

constructor TFileManagerFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FileManager := TFileManager.Create(tvFileList);

  if LoadImages('filemanager', ilMain) then
  begin
    actAddFiles.ImageIndex := 0;
    actCreateDir.ImageIndex := 14;
    actDeleteFile.ImageIndex := 4;
    actExpandCollapse.ImageIndex := 2;
    actOpenFile.ImageIndex := 7;
    actRenameFile.ImageIndex := 9;
    actUpdateFileList.ImageIndex := 13;
  end;

  LoadImages('filemanager' + DIR_SEP + 'preview', ilPreview);

  UpdateImgPreview;
end;

destructor TFileManagerFrame.Destroy;
begin
  FreeAndNil(FileManager);
  inherited Destroy;
end;

procedure TFileManagerFrame.actDeleteFileExecute(Sender: TObject);
begin
  FileManager.ShowDeleteDialog;
end;

procedure TFileManagerFrame.actCreateDirExecute(Sender: TObject);
begin
  FileManager.ShowNewDirDialog;
end;

procedure TFileManagerFrame.actExpandCollapseExecute(Sender: TObject);
begin
  tvFileList.AutoExpand := not tvFileList.AutoExpand;

  if tvFileList.AutoExpand then
  begin
    tvFileList.FullExpand;
    actExpandCollapse.ImageIndex := 2;
    actExpandCollapse.Hint := 'Свернуть';
  end
  else
  begin
    tvFileList.FullCollapse;
    actExpandCollapse.ImageIndex := 3;
    actExpandCollapse.Hint := 'Развернуть';
  end;
end;

procedure TFileManagerFrame.actAddFilesExecute(Sender: TObject);
begin
  FileManager.ShowAddFilesDialog;
end;

procedure TFileManagerFrame.actOpenFileExecute(Sender: TObject);
begin
  FileManager.OpenFile(FileManager.SelectedFileName);
end;

procedure TFileManagerFrame.actOpenFileUpdate(Sender: TObject);
var
  IsSelected: boolean;

begin
  IsSelected := Assigned(tvFileList.Selected);

  TAction(Sender).Enabled := IsSelected;

  if IsSelected then
  begin
    if tvFileList.Selected.Parent = nil then
    begin
      actRenameFile.Enabled := False;
      actDeleteFile.Enabled := False;
    end;

    actAddFiles.Caption := 'Добавить файлы в "' + FileManager.GetDirNameOnly(FileManager.GetPath) + '"';
  end;
end;

procedure TFileManagerFrame.actRenameFileExecute(Sender: TObject);
begin
  FileManager.ShowRenFileDialog;
end;

procedure TFileManagerFrame.actUpdateFileListExecute(Sender: TObject);
begin
  UpdateFileList;
end;

procedure TFileManagerFrame.tvFileListChange(Sender: TObject; Node: TTreeNode);
var
  FileName: string;

begin
  if not Assigned(Node) then
  begin
    lblFileName.Caption := 'Файл не выбран';
    lblFileName.Hint := '';
    lblFileSize.Caption := '';
    lblImgHeight.Caption := '';
    lblImgWidth.Caption := '';
    imgPreview.Picture.Clear;
    Exit;
  end;

  FileName := FileManager.SelectedFileName;

  lblFileName.Caption := ExtractFileName(FileName);
  lblFileName.Hint := lblFileName.Caption;
  lblFileSize.Caption := 'Размер: ' + GetFileSize(FileName);
  lblImgHeight.Caption := '';
  lblImgWidth.Caption := '';

  case GetFileType(Node.Text) of
    ftPascal: ilPreview.GetBitmap(2, imgPreview.Picture.Bitmap);
    ftSound: ilPreview.GetBitmap(3, imgPreview.Picture.Bitmap);
    ftImage:
    begin
      try
        imgPreview.Picture.LoadFromFile(FileName);
        lblImgWidth.Caption := 'Высота: ' + IntToStr(imgPreview.Picture.Height);
        lblImgHeight.Caption := 'Ширина: ' + IntToStr(imgPreview.Picture.Width);
      except
        imgPreview.Picture.Clear;
      end;
    end
    else
      if FileManager.IsDirectory(FileName) then
      begin
        ilPreview.GetBitmap(1, imgPreview.Picture.Bitmap);
        lblFileSize.Caption := '';
      end
      else
        ilPreview.GetBitmap(0, imgPreview.Picture.Bitmap);
  end;
end;

procedure TFileManagerFrame.tvFileListDblClick(Sender: TObject);
begin
  FileManager.OpenFile(FileManager.SelectedFileName);
end;

procedure TFileManagerFrame.UpdateFileList;
begin
  tvFileList.Items.Clear;

  tvFileList.BeginUpdate;

  FileManager.AddFilesFromDir(ProjManager.ProjDirSrc, tvFileList.Items.Add(nil, 'Модули'));
  tvFileList.Items.GetLastNode.ImageIndex := 6;
  tvFileList.Items.GetLastNode.SelectedIndex := 6;

  FileManager.AddFilesFromDir(ProjManager.ProjDirRes, tvFileList.Items.Add(nil, 'Ресурсы'));
  tvFileList.Items.GetLastNode.ImageIndex := 10;
  tvFileList.Items.GetLastNode.SelectedIndex := 10;

  tvFileList.Items.Item[0].Selected := True;
  tvFileList.Items.Item[1].Selected := True;

  tvFileList.EndUpdate;
end;

procedure TFileManagerFrame.UpdateImgPreview;
begin
  imgPreview.Height := IDEConfig.FilManPrevSizeY;
  imgPreview.Width := IDEConfig.FilManPrevSizeX;
  imgPreview.Stretch := IDEConfig.FilManPrevStretch;
end;

end.
