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

unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, Controls, Forms, Dialogs, ExtCtrls, ActnList, FileUtil,
  LCLIntf, Menus, SynEdit, SysUtils, uAMPASCore;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actCreateModule: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actCloseActiveTab: TAction;
    actBuildAndroid: TAction;
    actCodeInsDateTime: TAction;
    actCodeInsUserName: TAction;
    actExportAsHTML: TAction;
    actCompactViewMode: TAction;
    actSaveAll: TAction;
    actTerminateProc: TAction;
    actJCFCurrentTab: TAction;
    actRun: TAction;
    actCompile: TAction;
    actBuild: TAction;
    actlProjActions: TActionList;
    actUpdateStatusBar: TAction;
    actlMain: TActionList;
    ilMain: TImageList;
    ilProjActions: TImageList;
    miSaveAll: TMenuItem;
    miCloseActivePage: TMenuItem;
    miCloseAllOtherPages: TMenuItem;
    miExamples: TMenuItem;
    miSettings: TMenuItem;
    miIDESettings: TMenuItem;
    miIDEMacros: TMenuItem;
    MenuItem5: TMenuItem;
    miView: TMenuItem;
    miCompactViewMode: TMenuItem;
    miExportAsHTML: TMenuItem;
    miJCF: TMenuItem;
    miJCFCurrentTAB: TMenuItem;
    miCreateProject: TMenuItem;
    miCodeInsDateTime: TMenuItem;
    miCodeInsUserName: TMenuItem;
    miCodeIns: TMenuItem;
    miCode: TMenuItem;
    miFile: TMenuItem;
    miExit: TMenuItem;
    miInfo: TMenuItem;
    miDocumentation: TMenuItem;
    miJavaLibs: TMenuItem;
    MenuItem14: TMenuItem;
    miAbout: TMenuItem;
    miCreateModule: TMenuItem;
    MenuItem3: TMenuItem;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    MenuItem7: TMenuItem;
    miCloseActiveTab: TMenuItem;
    MenuItem9: TMenuItem;
    mmMain: TMainMenu;
    pgcEditor: TPageControl;
    pgcMsgNotes: TPageControl;
    pgcProject: TPageControl;
    pmEditor: TPopupMenu;
    splBottom: TSplitter;
    splLeft: TSplitter;
    stbEditor: TStatusBar;
    synedtNotes: TSynEdit;
    tbbRun64: TToolButton;
    tbbCompile64: TToolButton;
    tbbBuild64: TToolButton;
    tbbCreateModule: TToolButton;
    tbbDivider1: TToolButton;
    tbbOpenFile: TToolButton;
    tbbSaveFile: TToolButton;
    tbbRun: TToolButton;
    tbbCompile: TToolButton;
    tbbDivider2: TToolButton;
    tbbCloseEditorTab: TToolButton;
    tbbBuild: TToolButton;
    tbbDivider4: TToolButton;
    tbbTermProc64: TToolButton;
    tbbDivider3: TToolButton;
    tbbTermProc: TToolButton;
    tbbDivider5: TToolButton;
    tbbBuildAndroid64: TToolButton;
    tbbSaveAll: TToolButton;
    tsLogMsg: TTabSheet;
    tsNotes: TTabSheet;
    tsProjFiles: TTabSheet;
    tsProjSettings: TTabSheet;
    tlbMain: TToolBar;
    tlbRCB: TToolBar;
    tvMsg: TTreeView;
    procedure actBuildAndroidExecute(Sender: TObject);
    procedure actBuildExecute(Sender: TObject);
    procedure actCloseActiveTabExecute(Sender: TObject);
    procedure actCloseActiveTabUpdate(Sender: TObject);
    procedure actCodeInsDateTimeExecute(Sender: TObject);
    procedure actCodeInsUserNameExecute(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure actCreateModuleExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveUpdate(Sender: TObject);
    procedure actExportAsHTMLExecute(Sender: TObject);
    procedure actJCFCurrentTabExecute(Sender: TObject);
    procedure actSaveAllExecute(Sender: TObject);
    procedure actTerminateProcExecute(Sender: TObject);
    procedure actlProjActionsUpdate(AAction: TBasicAction; var Handled: boolean);
    procedure actRunExecute(Sender: TObject);
    procedure actRunUpdate(Sender: TObject);
    procedure actCompactViewModeExecute(Sender: TObject);
    procedure actUpdateStatusBarExecute(Sender: TObject);
    procedure actUpdateStatusBarUpdate(Sender: TObject);
    procedure BuildingThreadTerminate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure miCloseAllOtherPagesClick(Sender: TObject);
    procedure miDocumentationClick(Sender: TObject);
    procedure miExamplesClick(Sender: TObject);
    procedure miIDESettingsClick(Sender: TObject);
    procedure miIDEMacrosClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miCreateProjectClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miJavaLibsClick(Sender: TObject);
    procedure pgcEditorCloseTabClicked(Sender: TObject);
    procedure synedtNotesChange(Sender: TObject);
  private
    { private declarations }
    function CheckFileModified(ATabSheet: TTabSheet): boolean;
    procedure CloseTab(ATabSheet: TTabSheet);
    procedure EnableProjUIControls;
    procedure GetSettings;
    procedure InitControls;
    procedure LoadControlsImages;
    type TProjBuildMode = (pbmRun, pbmCompile, pbmBuild);
    procedure ProjLaunchMode(ProjBuildMode: TProjBuildMode);
    procedure SetSettings;
    procedure UpdateStatusBar;
  public
    { public declarations }
    procedure AddLogMsg(const AValue: string; MsgType: TLogMsgType = lmtText);
    procedure LoadFile(FileName: string);
    procedure UpdateSettings;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uAboutForm,
  uAndroidBuilding,
  uCodeEditor,
  uEditorConfig,
  uFileManager,
  uFileManagerFrame,
  uIDEConfig,
  uIDEMacrosForm,
  uIDEOptionsForm,
  uNewProjForm,
  uProjectBuilding,
  uProjectConfig,
  uProjectManager,
  uProjectOptionsFrame;

var
  FileManagerFrame: TFileManagerFrame;
  ProjectOptionsFrame: TProjectOptionsFrame;
  frmNewProj: TfrmNewProj;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  FileName: string;
  i: integer;

begin
  Caption := APP_NAME + ' ' + GetProgramVersion;

  AddLogMsg(Caption);

  Constraints.MinHeight := 360;
  Constraints.MinWidth := 640;

  FileName := GetAppPath + APP_CONFIG;
  CheckConfig(FileName);
  IDEConfig := TIDEConfig.Create(FileName);

  FileName := IDEConfig.ColorSchemeFile;
  CheckConfig(FileName);
  EditorConfig := TEditorConfig.Create(FileName);

  CodeEditor := TCodeEditor.Create(pgcEditor);
  ProjManager := TProjectManager.Create;

  InitControls;

  if Paramcount <> 0 then
    for i := 1 to Paramcount do
      CodeEditor.LoadFile(ParamStr(i));
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  SetSettings;
  FreeAndNil(ProjManager);
  FreeAndNil(CodeEditor);
  FreeAndNil(EditorConfig);
  FreeAndNil(IDEConfig);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: integer;

begin
  if IsProcRunning then
    case MessageDlg('Запущен процесс', 'Прервать выполнение?', mtWarning, [mbYes, mbCancel], 0) of
      mrYes: TerminateProc;
      mrCancel:
      begin
        CanClose := False;
        Exit;
      end;
    end;

  if pgcEditor.PageCount > 0 then
  begin
    for i := 0 to pgcEditor.PageCount - 1 do
    begin
      CanClose := CheckFileModified(pgcEditor.Pages[i]);
      if not CanClose then
        Break;
    end;
  end;
end;

procedure TfrmMain.actRunUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ProjManager.ProjectOpen and not IsProcRunning;
end;

procedure TfrmMain.actUpdateStatusBarExecute(Sender: TObject);
begin
  UpdateStatusBar;
end;

procedure TfrmMain.actUpdateStatusBarUpdate(Sender: TObject);
begin
  UpdateStatusBar;
end;

procedure TfrmMain.BuildingThreadTerminate(Sender: TObject);
begin
  ProjectOptionsFrame.UpdateVers;
end;

procedure TfrmMain.actCreateModuleExecute(Sender: TObject);
begin
  with TSaveDialog.Create(Self) do
  begin
    try
      Title := 'Сохранить модуль как ...';
      FileName := IDEConfig.ModulePrefix;
      if ProjManager.ProjectOpen then
        InitialDir := ProjManager.ProjDirSrc;
      if Execute then
      begin
        if Length(ExtractFileNameOnly(FileName)) < 3 then
          MessageDlg('Неверное значение', 'Название модуля должно состоять минимум из 3-х символов',
            mtWarning, [mbOK], 0)
        else
        begin
          if FileExists(FileName) then
            case MessageDlg('Подтверждение', 'Модуль с таким именем уже существует, заменить?',
                mtWarning, [mbYes, mbNo], 0) of
              mrNo: Exit;
            end;
          FileName := ProjManager.CreateModule(FileName);
          if FileName <> '' then
            CodeEditor.LoadFile(FileName);
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actCloseActiveTabExecute(Sender: TObject);
begin
  CloseTab(pgcEditor.ActivePage);
end;

procedure TfrmMain.actBuildExecute(Sender: TObject);
begin
  ProjLaunchMode(pbmBuild);
end;

procedure TfrmMain.actBuildAndroidExecute(Sender: TObject);
begin
  with TAndroidBuildingThread.Create(True) do
  begin
    AntBuildFile := GetAppPath + APP_DIR_ANDROID + 'build.xml';
    OnTerminate := @BuildingThreadTerminate;
    Start;
  end;
end;

procedure TfrmMain.actCloseActiveTabUpdate(Sender: TObject);
var
  IsEditorActive: boolean;

begin
  IsEditorActive := CodeEditor.IsEditorActive;
  TAction(Sender).Enabled := IsEditorActive;
  miCloseAllOtherPages.Enabled := IsEditorActive;
end;

procedure TfrmMain.actCodeInsDateTimeExecute(Sender: TObject);
begin
  CodeEditor.InsDateTime;
end;

procedure TfrmMain.actCodeInsUserNameExecute(Sender: TObject);
begin
  CodeEditor.InsUsername;
end;

procedure TfrmMain.actCompileExecute(Sender: TObject);
begin
  ProjLaunchMode(pbmCompile);
end;

procedure TfrmMain.actFileOpenExecute(Sender: TObject);
var
  i: integer;

begin
  with TOpenDialog.Create(Self) do
  begin
    try
      Title := 'Открыть файл';
      Filter :=
        'Все файлы *|*|' +
        APP_NAME + ' Project|*' + EXT_PROJECT + '|' +
        'Pascal Source Code|*' + EXT_MODULE;
      Options := [ofAllowMultiSelect, ofEnableSizing, ofViewDetail];
      if Execute then
        for i := 0 to Files.Count - 1 do
          LoadFile(Files.Strings[i]);
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actFileSaveExecute(Sender: TObject);
begin
  CodeEditor.SaveCurrentFile;
end;

procedure TfrmMain.actFileSaveUpdate(Sender: TObject);
begin
  actFileSave.Enabled := CodeEditor.IsCurrentFileModified;
end;

procedure TfrmMain.actExportAsHTMLExecute(Sender: TObject);
begin
  with TSaveDialog.Create(Self) do
  begin
    try
      Title := 'Сохранить как ...';
      Filter := 'HTML|*.html';
      if Execute then
        CodeEditor.ExportToHTML(FileName + '.html');
    finally
      Free;
    end;
  end;
end;

procedure TfrmMain.actJCFCurrentTabExecute(Sender: TObject);
begin
  with CodeEditor do
  begin
    SaveCurrentFile;
    case MessageDlg('Подтверждение', 'Форматировать код ' + ActiveFileName + '? ',
        mtInformation, [mbYes, mbNo], 0) of
      mrYes: JCFormat;
    end;
  end;
end;

procedure TfrmMain.actSaveAllExecute(Sender: TObject);
begin
  CodeEditor.SaveAllFiles;
end;

procedure TfrmMain.actTerminateProcExecute(Sender: TObject);
begin
  TerminateProc;
end;

procedure TfrmMain.actlProjActionsUpdate(AAction: TBasicAction; var Handled: boolean);
begin
  actTerminateProc.Enabled := IsProcRunning;
end;

procedure TfrmMain.actRunExecute(Sender: TObject);
begin
  ProjLaunchMode(pbmRun);
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  i: integer;

begin
  for i := 0 to High(FileNames) do
    if FileManagerFrame.IsMouseEnter then
      FileManager.AddFile(FileNames[i])
    else
      LoadFile(FileNames[i]);
end;

procedure TfrmMain.miCloseAllOtherPagesClick(Sender: TObject);
var
  TabClose: boolean;
  i: integer;

begin
  if pgcEditor.PageCount <= 0 then
    Exit;

  for i := pgcEditor.PageCount - 1 downto 0 do
  begin
    TabClose := CheckFileModified(pgcEditor.Pages[i]);
    if not TabClose then
      Break;
  end;

  if TabClose then
    for i := pgcEditor.PageCount - 1 downto 0 do
      if pgcEditor.Pages[i] <> pgcEditor.ActivePage then
        CodeEditor.CloseTabSheet(pgcEditor.Pages[i]);
end;

procedure TfrmMain.miDocumentationClick(Sender: TObject);
begin
  OpenURL(URL_TUTORIAL);
end;

procedure TfrmMain.miExamplesClick(Sender: TObject);
begin
  OpenDocument(GetAppPath + 'examples');
end;

procedure TfrmMain.miIDESettingsClick(Sender: TObject);
begin
  with TfrmIDEOptions.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmMain.miIDEMacrosClick(Sender: TObject);
begin
  with TfrmIDEMacros.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmMain.miAboutClick(Sender: TObject);
begin
  with TfrmAbout.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfrmMain.miCreateProjectClick(Sender: TObject);
begin
  if not Assigned(frmNewProj) then
    Application.CreateForm(TfrmNewProj, frmNewProj);
  frmNewProj.Show;
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.miJavaLibsClick(Sender: TObject);
begin
  OpenURL(URL_JAVA_LIB);
end;

procedure TfrmMain.pgcEditorCloseTabClicked(Sender: TObject);
begin
  if Sender is TTabSheet then
    CloseTab(TTabSheet(Sender));
end;

procedure TfrmMain.synedtNotesChange(Sender: TObject);
begin
  try
    synedtNotes.Lines.SaveToFile(ProjManager.NotesFile);
  except
    synedtNotes.Lines.Add('Не удалось сохранить файл заметок');
  end;
end;

procedure TfrmMain.CloseTab(ATabSheet: TTabSheet);
begin
  if CheckFileModified(ATabSheet) then
    CodeEditor.CloseTabSheet(ATabSheet);
end;

function TfrmMain.CheckFileModified(ATabSheet: TTabSheet): boolean;
var
  AEditor: TSynEdit;
  FileName: string;

begin
  Result := True;

  AEditor := CodeEditor.GetEditor(ATabSheet);
  FileName := CodeEditor.GetFileName(ATabSheet);

  if CodeEditor.IsFileModified(AEditor) then
  begin
    case MessageDlg('Подтверждение', 'Сохранить изменения в файле "' + ExtractFileName(FileName) + '"?',
        mtInformation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: CodeEditor.SaveFile(FileName, AEditor);
      mrCancel: Result := False;
    end;
  end;
end;

procedure TfrmMain.GetSettings;
begin
  Color := IDEConfig.FColor;
  Left := IDEConfig.FLeft;
  Top := IDEConfig.FTop;
  Height := IDEConfig.FHeight;
  Width := IDEConfig.FWidth;

  splBottom.Top := IDEConfig.FsplBottom;
  splLeft.Left := IDEConfig.FsplLeft;

  if IDEConfig.CompactViewMode then
    actCompactViewMode.Execute;
end;

procedure TfrmMain.SetSettings;
begin
  IDEConfig.FColor := Color;
  IDEConfig.FLeft := Left;
  IDEConfig.FTop := Top;
  IDEConfig.FHeight := Height;
  IDEConfig.FWidth := Width;

  IDEConfig.FSplBottom := splBottom.Top + 21;
  IDEConfig.FSplLeft := splLeft.Left;

  IDEConfig.CompactViewMode := miCompactViewMode.Checked;

  if ProjManager.ProjectOpen then
    IDEConfig.LastProj := ProjManager.ConfigFile;
end;

procedure TfrmMain.UpdateStatusBar;
begin
  stbEditor.Panels[0].Text := CodeEditor.CaretPos;

  if CodeEditor.IsCurrentFileModified then
    stbEditor.Panels[1].Text := 'Изменён'
  else
    stbEditor.Panels[1].Text := '';

  stbEditor.Panels[2].Text := CodeEditor.ActiveFileName;
  stbEditor.Hint := stbEditor.Panels[2].Text;
end;

procedure TfrmMain.AddLogMsg(const AValue: string; MsgType: TLogMsgType);
var
  i, ImgIndex: integer;

begin
  if AValue = '' then
    Exit;

  if tvMsg.Items.Count > 200 then
    tvMsg.Items.Clear;

  pgcMsgNotes.ActivePage := tsLogMsg;

  case MsgType of
    lmtText: ImgIndex := 8;
    lmtOk: ImgIndex := 11;
    lmtErr: ImgIndex := 5;
    else
      ImgIndex := 8;
  end;

  with TStringList.Create do
  begin
    try
      DelimitedText := LE;
      Text := AValue;
      tvMsg.Items.Add(nil, FormatDateTime('[hh:mm:ss]: ', Now) + Strings[0]);
      for i := 1 to Count - 1 do
      begin
        tvMsg.Items.AddChild(tvMsg.Items.GetLastNode, Strings[i]);
        Application.ProcessMessages; // не нужно
        tvMsg.Items.GetLastSubNode.Selected := True;
      end;
    finally
      Free;
    end;
  end;

  tvMsg.Items.GetLastNode.Selected := True;
  tvMsg.Items.GetLastNode.ImageIndex := ImgIndex;
  tvMsg.Items.GetLastNode.SelectedIndex := ImgIndex;
end;

procedure TfrmMain.UpdateSettings;
var
  i: integer;

begin
  synedtNotes.Font.Name := EditorConfig.FontName;
  synedtNotes.Font.Size := EditorConfig.FontSize;

  for i := 0 to pgcEditor.PageCount - 1 do
    CodeEditor.UpdateEditorSettings(CodeEditor.GetEditor(pgcEditor.Pages[i]));

  FileManagerFrame.UpdateImgPreview;
end;

procedure TfrmMain.InitControls;
begin
  pgcProject.ActivePage := tsProjSettings;

  FileManagerFrame := TFileManagerFrame.Create(tsProjFiles);
  FileManagerFrame.Parent := tsProjFiles;
  FileManagerFrame.Enabled := False;
  FileManagerFrame.Align := alClient;

  ProjectOptionsFrame := TProjectOptionsFrame.Create(tsProjSettings);
  ProjectOptionsFrame.Parent := tsProjSettings;
  ProjectOptionsFrame.Enabled := False;
  ProjectOptionsFrame.Align := alClient;

  synedtNotes.Enabled := False;
  synedtNotes.Font.Name := EditorConfig.FontName;
  synedtNotes.Font.Size := EditorConfig.FontSize;
  synedtNotes.Lines.Add('Недоступно, нет открытого проекта');

  tbbBuild.Visible := False;
  tbbCompile.Visible := False;
  tbbDivider2.Visible := False;
  tbbRun.Visible := False;
  tbbTermProc.Visible := False;

  {$IFDEF MSWINDOWS}
  tbbDivider3.Visible := True;
  tbbCloseEditorTab.Visible := True;
  {$ELSE}
  tbbDivider3.Visible := False;
  tbbCloseEditorTab.Visible := False;
  {$ENDIF}

  GetSettings;
  LoadControlsImages;

  if FileExists(IDEConfig.LastProj) then
    LoadFile(IDEConfig.LastProj);
end;

procedure TfrmMain.LoadControlsImages;
begin
  if LoadImages('main', ilMain) then
  begin
    actCloseActiveTab.ImageIndex := 2;
    actCodeInsDateTime.ImageIndex := 4;
    actCodeInsUserName.ImageIndex := 16;
    actCreateModule.ImageIndex := 10;
    actExportAsHTML.ImageIndex := 7;
    actFileOpen.ImageIndex := 12;
    actFileSave.ImageIndex := 15;
    actSaveAll.ImageIndex := 19;

    miAbout.ImageIndex := 8;
    miCreateProject.ImageIndex := 13;
    miDocumentation.ImageIndex := 0;
    miExamples.ImageIndex := 18;
    miExit.ImageIndex := 6;
    miJavaLibs.ImageIndex := 9;

    tbbBuild.ImageIndex := 1;
    tbbCompile.ImageIndex := 3;
    tbbRun.ImageIndex := 14;
    tbbTermProc.ImageIndex := 17;
  end;

  if LoadImages('actions', ilProjActions) then
  begin
    actBuild.ImageIndex := 0;
    actBuildAndroid.ImageIndex := 1;
    actCompile.ImageIndex := 2;
    actRun.ImageIndex := 4;
    actTerminateProc.ImageIndex := 3;
  end;
end;

procedure TfrmMain.ProjLaunchMode(ProjBuildMode: TProjBuildMode);
var
  BuildingThread: TBuildingThread;

begin
  CodeEditor.SaveCurrentFile;

  BuildingThread := TBuildingThread.Create(True);
  BuildingThread.OnTerminate := @BuildingThreadTerminate;

  case ProjBuildMode of
    pbmCompile: BuildingThread.Mode := 0;
    pbmBuild: BuildingThread.Mode := 1;
    pbmRun: BuildingThread.Mode := 2;
  end;

  BuildingThread.Start;
end;

procedure TfrmMain.EnableProjUIControls;
begin
  Caption := APP_NAME + ' ' + GetProgramVersion + ' - ' + ProjConfig.MIDletName;

  FileManager.UpdateFileList;
  FileManagerFrame.Enabled := True;

  ProjectOptionsFrame.GetSettings;
  ProjectOptionsFrame.Enabled := True;

  synedtNotes.Enabled := True;
  try
    synedtNotes.Lines.LoadFromFile(ProjManager.NotesFile);
  except
    synedtNotes.Lines.Text := 'Не удалось загрузить файл заметок';
  end;
end;

procedure TfrmMain.actCompactViewModeExecute(Sender: TObject);
var
  IsChecked: boolean;

begin
  IsChecked := miCompactViewMode.Checked;
  miCompactViewMode.Checked := not IsChecked;

  pgcProject.Visible := IsChecked;
  splLeft.Visible := IsChecked;
  tlbRCB.Visible := IsChecked;
  tbbBuild.Visible := not IsChecked;
  tbbCompile.Visible := not IsChecked;
  tbbDivider2.Visible := not IsChecked;
  tbbRun.Visible := not IsChecked;
  tbbTermProc.Visible := not IsChecked;

  if IsChecked then
  begin
    pgcEditor.AnchorSideLeft.Control := tlbRCB;
    pgcEditor.AnchorSideLeft.Side := asrBottom;
    pgcMsgNotes.AnchorSideLeft.Control := tlbRCB;
    pgcMsgNotes.AnchorSideLeft.Side := asrBottom;
    splBottom.AnchorSideLeft.Control := tlbRCB;
    splBottom.AnchorSideLeft.Side := asrBottom;
    stbEditor.AnchorSideLeft.Control := tlbRCB;
    stbEditor.AnchorSideLeft.Side := asrBottom;
    tlbMain.AnchorSideLeft.Control := tlbRCB;
    tlbMain.AnchorSideLeft.Side := asrBottom;
  end
  else
  begin
    pgcEditor.AnchorSideLeft.Control := Self;
    pgcEditor.AnchorSideLeft.Side := asrLeft;
    pgcMsgNotes.AnchorSideLeft.Control := Self;
    pgcMsgNotes.AnchorSideLeft.Side := asrLeft;
    splBottom.AnchorSideLeft.Control := Self;
    splBottom.AnchorSideLeft.Side := asrLeft;
    stbEditor.AnchorSideLeft.Control := Self;
    stbEditor.AnchorSideLeft.Side := asrLeft;
    tlbMain.AnchorSideLeft.Control := Self;
    tlbMain.AnchorSideLeft.Side := asrLeft;
  end;
end;

procedure TfrmMain.LoadFile(FileName: string);
begin
  if ExtractFileExt(FileName) = EXT_PROJECT then
  begin
    if ProjManager.OpenProject(FileName) then
    begin
      FileName := ProjManager.MainModule;
      EnableProjUIControls;
    end;
  end;

  CodeEditor.LoadFile(FileName);
end;

end.

