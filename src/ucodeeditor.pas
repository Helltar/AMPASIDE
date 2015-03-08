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

unit uCodeEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, Controls, Graphics, SysUtils, LCLIntf, SynEdit, SynEditMarkupHighAll,
  SynEditMouseCmds, SynExportHTML, SynEditHighlighter, SynPluginSyncroEdit;

type

  { TCodeEditor }

  TCodeEditor = class
  private
    FileNameList: array of string;
    FOwner: TPageControl;
    function GetActiveFileName: string;
    function GetActiveTabSheet: TTabSheet;
    function GetActiveTabTag: integer;
    function GetCaretPos: string;
    function GetCurrentEditor: TSynEdit;
    function GetHighlighterFromName(const FileName: string): TSynCustomHighlighter;
  public
    constructor Create(AOwner: TPageControl);
    destructor Destroy; override;

    function GetEditor(ATabSheet: TTabSheet): TSynEdit;
    function GetFileName(ATabSheet: TTabSheet): string;
    function IsCurrentFileModified: boolean;
    function IsEditorActive: boolean;
    function IsFileModified(AEditor: TSynEdit): boolean;
    function LoadFile(const FileName: string): boolean;

    procedure CloseTabSheet(ATabSheet: TTabSheet);
    procedure ExportToHTML(const FileName: string);
    procedure InsDateTime;
    procedure InsUsername;
    procedure JCFormat;
    procedure SaveCurrentFile;
    procedure SaveFile(const FileName: string; AEditor: TSynEdit);
    procedure UpdateEditorSettings(AEditor: TSynEdit);

    property ActiveFileName: string read GetActiveFileName;
    property CaretPos: string read GetCaretPos;
    property CurrentEditor: TSynEdit read GetCurrentEditor;
  end;

var
  CodeEditor: TCodeEditor;

implementation

uses
  uAMPASCore,
  uAutocomplete,
  uCodeEditorDM,
  uEditorConfig;

var
  EditorAutocomplete: TAutocomplete;

{ TCodeEditor }

constructor TCodeEditor.Create(AOwner: TPageControl);
begin
  FOwner := AOwner;
  EditorAutocomplete := TAutocomplete.Create(AOwner);
end;

destructor TCodeEditor.Destroy;
begin
  FileNameList := nil;
  FreeAndNil(EditorAutocomplete);
  inherited Destroy;
end;

function TCodeEditor.LoadFile(const FileName: string): boolean;
var
  FileListLength: integer;
  synEditor: TSynEdit;
  synHighlightCaret: TSynEditMarkupHighlightAllCaret;
  synSyncEdit: TSynPluginSyncroEdit;
  tsEditor: TTabSheet;
  i: integer;

begin
  Result := False;

  if not CheckFile(FileName) then
    Exit;

  if IsEditorActive then
    for i := 0 to FOwner.PageCount - 1 do
      if FileName = FileNameList[FOwner.Pages[i].Tag] then
      begin
        FOwner.ActivePage := FOwner.Pages[i];
        Exit;
      end;

  tsEditor := TTabSheet.Create(FOwner);
  synEditor := TSynEdit.Create(tsEditor);

  try
    synEditor.Lines.LoadFromFile(FileName);
    FileListLength := Length(FileNameList);
    SetLength(FileNameList, FileListLength + 1);
    FileNameList[FileListLength] := FileName;
  except
    FreeAndNil(tsEditor);
    AddLogMsg('Не удалось открыть файл: ' + FileName, lmtErr);
    Exit;
  end;

  tsEditor.Align := alClient;
  tsEditor.Caption := ExtractFileName(FileName);
  tsEditor.Parent := FOwner;
  tsEditor.Tag := FileListLength;

  synEditor.Align := alClient;
  synEditor.Font.Quality := fqCleartype;
  synEditor.Highlighter := GetHighlighterFromName(FileName);
  synEditor.MouseOptions := [emAltSetsColumnMode, emDragDropEditing, emCtrlWheelZoom];
  synEditor.Options := [eoAutoIndent, eoBracketHighlight, eoGroupUndo, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces];
  synEditor.Parent := tsEditor;
  synEditor.PopupMenu := dmCodeEditor.pmEditor;

  UpdateEditorSettings(synEditor);

  EditorAutocomplete.AddEditor(synEditor);

  synSyncEdit := TSynPluginSyncroEdit.Create(synEditor);
  synSyncEdit.Editor := synEditor;
  dmCodeEditor.ilEditor.GetBitmap(5, synSyncEdit.GutterGlyph);

  synHighlightCaret := TSynEditMarkupHighlightAllCaret(synEditor.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
  synHighlightCaret.FullWord := True;
  synHighlightCaret.IgnoreKeywords := False;
  synHighlightCaret.MarkupInfo.Background := clYellow;
  synHighlightCaret.Trim := True;
  synHighlightCaret.WaitTime := 1000;

  FOwner.ActivePage := tsEditor;

  Result := True;
end;

procedure TCodeEditor.SaveCurrentFile;
begin
  if IsEditorActive then
    SaveFile(GetActiveFileName, GetCurrentEditor);
end;

procedure TCodeEditor.SaveFile(const FileName: string; AEditor: TSynEdit);
begin
  try
    AEditor.Lines.SaveToFile(FileName);
    AEditor.Modified := False;
  except
    AddLogMsg('Не удалось сохранить файл: ' + FileName, lmtErr);
  end;
end;

procedure TCodeEditor.UpdateEditorSettings(AEditor: TSynEdit);
begin
  if Assigned(AEditor) then
  begin
    with AEditor do
    begin
      Color := EditorConfig.EditorColor;
      Font.Name := EditorConfig.FontName;
      Font.Size := EditorConfig.FontSize;
      RightEdge := EditorConfig.RightEdge;
      RightEdgeColor := EditorConfig.RightEdgeColor;
      SelectedColor.Background := EditorConfig.SelectedColor;
      if EditorConfig.ScrollPastEol then
        Options := Options + [eoScrollPastEol]
      else
        Options := Options - [eoScrollPastEol];
    end;
  end;
end;

function TCodeEditor.GetActiveFileName: string;
begin
  Result := 'нет открытого файла';
  if IsEditorActive then
    Result := FileNameList[GetActiveTabTag];
end;

function TCodeEditor.GetActiveTabSheet: TTabSheet;
begin
  Result := FOwner.ActivePage;
end;

function TCodeEditor.GetActiveTabTag: integer;
begin
  Result := FOwner.ActivePage.Tag;
end;

function TCodeEditor.GetCaretPos: string;
var
  XY: TPoint;

begin
  Result := '0: 0';
  if IsEditorActive then
  begin
    XY := GetCurrentEditor.LogicalCaretXY;
    Result := IntToStr(XY.Y) + ': ' + IntToStr(XY.X);
  end;
end;

function TCodeEditor.GetEditor(ATabSheet: TTabSheet): TSynEdit;
var
  i: integer;

begin
  Result := nil;
  if IsEditorActive then
    for i := 0 to ATabSheet.ComponentCount - 1 do
      if ATabSheet.Components[i] is TSynEdit then
        Result := TSynEdit(ATabSheet.Components[i]);
end;

function TCodeEditor.GetFileName(ATabSheet: TTabSheet): string;
begin
  Result := FileNameList[ATabSheet.Tag];
end;

function TCodeEditor.GetCurrentEditor: TSynEdit;
begin
  Result := GetEditor(GetActiveTabSheet);
end;

function TCodeEditor.GetHighlighterFromName(const FileName: string): TSynCustomHighlighter;
begin
  case GetFileType(FileName) of
    ftPascal: Result := dmCodeEditor.synPasHighlighter;
    ftJava: Result := dmCodeEditor.synJavaHighlighter;
    ftPHP: Result := dmCodeEditor.synPHPHighlighter;
    ftHTML: Result := dmCodeEditor.synHTMLHighlighter;
    else
      Result := nil;
  end;
end;

function TCodeEditor.IsCurrentFileModified: boolean;
begin
  Result := False;
  if IsEditorActive then
    if IsFileModified(GetCurrentEditor) then
      Result := True;
end;

function TCodeEditor.IsEditorActive: boolean;
begin
  Result := False;
  if FOwner.PageCount > 0 then
    Result := True;
end;

function TCodeEditor.IsFileModified(AEditor: TSynEdit): boolean;
begin
  Result := False;
  if AEditor.Modified then
    Result := True;
end;

procedure TCodeEditor.ExportToHTML(const FileName: string);
var
  AEditor: TSynEdit;

begin
  AEditor := GetCurrentEditor;

  with TSynExporterHTML.Create(nil) do
  begin
    try
      try
        CreateHTMLFragment := True;
        Highlighter := AEditor.Highlighter;
        ExportAll(AEditor.Lines);
        SaveToFile(FileName);
      except
        AddLogMsg('Ошибка при сохранении файла: ' + FileName, lmtErr);
      end;
    finally
      Free;
    end;
  end;
end;

procedure TCodeEditor.JCFormat;
var
  AEditor: TSynEdit;
  FileName: string;
  P: TProcFunc;
  XY: TPoint;

begin
  FileName := GetActiveFileName;

  if not CheckFile(FileName) then
    Exit;

  P := ProcStart(GetAppPath + JCF, '"' + FileName + '" -backup -y -config=' + GetAppPath + JCF_SETTINGS);

  if P.Completed then
  begin
    P.Output := StringReplace(P.Output, FileName, ExtractFileName(FileName), [rfReplaceAll]);
    if Pos('Aborted due to error', P.Output) > 0 then
      AddLogMsg(P.Output, lmtErr)
    else
    begin
      with TStringList.Create do
      begin
        try
          try
            LoadFromFile(FileName);
            AEditor := GetCurrentEditor;
            XY.X := Length(AEditor.Lines.Strings[AEditor.Lines.Count - 1]) + 1;
            XY.Y := AEditor.Lines.Count;
            AEditor.TextBetweenPoints[Point(1, 1), XY] := Text;
            AddLogMsg(P.Output);
          except
            AddLogMsg('При форматировании кода возникла ошибка', lmtErr);
          end;
        finally
          Free;
        end;
      end;
    end;
  end;
end;

procedure TCodeEditor.CloseTabSheet(ATabSheet: TTabSheet);
begin
  FileNameList[ATabSheet.Tag] := '';
  FreeAndNil(ATabSheet);
  if not IsEditorActive then
    FileNameList := nil;
end;

procedure TCodeEditor.InsDateTime;
begin
  GetCurrentEditor.SelText := FormatDateTime('dd.mm.yyyy hh:mm:ss', Now);
end;

procedure TCodeEditor.InsUsername;
begin
  GetCurrentEditor.SelText := GetCurrentUsername;
end;

end.

