{-------------------------------------------------------------------------------

Copyright (C) 2015 Helltar <mail@helltar.com>

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
  Classes, ActnList, ComCtrls, Controls, ExtCtrls, Forms, Menus,
  StdCtrls, SysUtils, FileUtil, TreeFilterEdit, LazFileUtils;

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
    procedure tvFileListMouseEnter(Sender: TObject);
    procedure tvFileListMouseLeave(Sender: TObject);
  private
    { private declarations }
    FIsMouseEnter: boolean;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateImgPreview;
    property IsMouseEnter: boolean read FIsMouseEnter;
  end;

implementation

uses
  uAMPASCore,
  uFileManager,
  uIDEConfig;

{$R *.lfm}

{ TFileManagerFrame }

constructor TFileManagerFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FileManager := TFileManager.Create(tvFileList);

  if LoadImages('filemanager', ilMain) then
  begin
    actExpandCollapse.ImageIndex := 2;
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
    actExpandCollapse.Hint := MSG_MINIMIZE;
  end
  else
  begin
    tvFileList.FullCollapse;
    actExpandCollapse.ImageIndex := 3;
    actExpandCollapse.Hint := MSG_EXPAND;
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
    actAddFiles.Caption := TITLE_ADD_FILES_TO + ' "' +
      FileManager.GetDirNameOnly(FileManager.GetPath) + '"';
  end;
end;

procedure TFileManagerFrame.actRenameFileExecute(Sender: TObject);
begin
  FileManager.ShowRenFileDialog;
end;

procedure TFileManagerFrame.actUpdateFileListExecute(Sender: TObject);
begin
  FileManager.UpdateFileList;
end;

procedure TFileManagerFrame.tvFileListChange(Sender: TObject; Node: TTreeNode);
var
  FileName: string;

begin
  if not Assigned(Node) then
  begin
    lblFileName.Caption := CAPTION_FILE_NO_SELECTED;
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
  lblFileSize.Caption := MSG_SIZE + ': ' + GetFileSize(FileName);
  lblImgHeight.Caption := '';
  lblImgWidth.Caption := '';

  case GetFileType(Node.Text) of
    ftPascal: ilPreview.GetBitmap(2, imgPreview.Picture.Bitmap);
    ftSound: ilPreview.GetBitmap(3, imgPreview.Picture.Bitmap);
    ftImage:
    begin
      try
        imgPreview.Picture.LoadFromFile(FileName);
        lblImgWidth.Caption := MSG_HEIGHT + ': ' + IntToStr(imgPreview.Picture.Height);
        lblImgHeight.Caption := MSG_WIDTH + ': ' + IntToStr(imgPreview.Picture.Width);
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
      if FileIsText(FileName) then
        ilPreview.GetBitmap(4, imgPreview.Picture.Bitmap)
      else
        ilPreview.GetBitmap(0, imgPreview.Picture.Bitmap);
  end;
end;

procedure TFileManagerFrame.tvFileListDblClick(Sender: TObject);
begin
  FileManager.OpenFile(FileManager.SelectedFileName);
end;

procedure TFileManagerFrame.tvFileListMouseEnter(Sender: TObject);
begin
  FIsMouseEnter := True;
end;

procedure TFileManagerFrame.tvFileListMouseLeave(Sender: TObject);
begin
  FIsMouseEnter := False;
end;

procedure TFileManagerFrame.UpdateImgPreview;
begin
  imgPreview.Height := IDEConfig.FilManPrevSizeY;
  imgPreview.Width := IDEConfig.FilManPrevSizeX;
  imgPreview.Stretch := IDEConfig.FilManPrevStretch;
end;

end.
