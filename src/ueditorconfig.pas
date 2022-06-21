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

unit uEditorConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IniFiles;

type

  { TEditorConfig }

  TEditorConfig = class
  private
    MainConfig: TIniFile;
    HighlighterConfig: TIniFile;
    function GetCommentColor: TColor;
    function GetEditorColor: TColor;
    function GetEditorCompletionWidth: integer;
    function GetEditorHeaders: string;
    function GetEditorHeadersText: string;
    function GetFontName: string;
    function GetFontSize: integer;
    function GetIdentifierColor: TColor;
    function GetKeyColor: TColor;
    function GetNumberColor: TColor;
    function GetRightEdge: integer;
    function GetRightEdgeColor: TColor;
    function GetScrollPastEol: boolean;
    function GetSelectedColor: TColor;
    function GetStringColor: TColor;
    function GetSymbolColor: TColor;
    procedure SetCommentColor(AValue: TColor);
    procedure SetEditorColor(AValue: TColor);
    procedure SetEditorCompletionWidth(AValue: integer);
    procedure SetEditorHeadersText(AValue: string);
    procedure SetFontName(AValue: string);
    procedure SetFontSize(AValue: integer);
    procedure SetIdentifierColor(AValue: TColor);
    procedure SetKeyColor(AValue: TColor);
    procedure SetNumberColor(AValue: TColor);
    procedure SetRightEdge(AValue: integer);
    procedure SetRightEdgeColor(AValue: TColor);
    procedure SetScrollPastEol(AValue: boolean);
    procedure SetSelectedColor(AValue: TColor);
    procedure SetStringColor(AValue: TColor);
    procedure SetSymbolColor(AValue: TColor);
  public
    constructor Create(HighlighterFile: string);
    destructor Destroy; override;

    function LoadColorScheme(const FileName: string): boolean;

    property EditorColor: TColor read GetEditorColor write SetEditorColor;
    property EditorCompletionWidth: integer read GetEditorCompletionWidth write SetEditorCompletionWidth;
    property EditorHeaders: string read GetEditorHeaders;
    property EditorHeadersText: string read GetEditorHeadersText write SetEditorHeadersText;
    property FontName: string read GetFontName write SetFontName;
    property FontSize: integer read GetFontSize write SetFontSize;
    property RightEdge: integer read GetRightEdge write SetRightEdge;
    property RightEdgeColor: TColor read GetRightEdgeColor write SetRightEdgeColor;
    property ScrollPastEol: boolean read GetScrollPastEol write SetScrollPastEol;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;

    property CommentColor: TColor read GetCommentColor write SetCommentColor;
    property IdentifierColor: TColor read GetIdentifierColor write SetIdentifierColor;
    property KeyColor: TColor read GetKeyColor write SetKeyColor;
    property NumberColor: TColor read GetNumberColor write SetNumberColor;
    property StringColor: TColor read GetStringColor write SetStringColor;
    property SymbolColor: TColor read GetSymbolColor write SetSymbolColor;
  end;

var
  EditorConfig: TEditorConfig;

implementation

uses
  uAMPASCore,
  uCodeEditorDM,
  uIDEConfig;

{ TEditorConfig }

constructor TEditorConfig.Create(HighlighterFile: string);
begin
  MainConfig := IDEConfig.MainConfig;
  HighlighterConfig := TIniFile.Create(HighlighterFile);
  dmCodeEditor := TdmCodeEditor.Create(nil);
  dmCodeEditor.LoadHighlighter(HighlighterFile);
end;

destructor TEditorConfig.Destroy;
begin
  FreeAndNil(dmCodeEditor);
  FreeAndNil(HighlighterConfig);
  inherited Destroy;
end;

function TEditorConfig.LoadColorScheme(const FileName: string): boolean;
begin
  Result := False;

  if CheckFile(FileName) then
  begin
    FreeAndNil(HighlighterConfig);
    dmCodeEditor.LoadHighlighter(FileName);
    HighlighterConfig := TIniFile.Create(FileName);
    Result := True;
  end;
end;

function TEditorConfig.GetCommentColor: TColor;
begin
  Result := dmCodeEditor.synPasHighlighter.CommentAttri.Foreground;
end;

function TEditorConfig.GetEditorColor: TColor;
begin
  Result := StringToColor(HighlighterConfig.ReadString('EDITOR', 'EditorColor', '$FFFFFF'));
end;

function TEditorConfig.GetEditorCompletionWidth: integer;
begin
  Result := MainConfig.ReadInteger('EDITOR', 'CompletionWidth', 480);
end;

function TEditorConfig.GetEditorHeaders: string;
begin
  Result := IDEConfig.MacrosReplace(GetEditorHeadersText);
end;

function TEditorConfig.GetEditorHeadersText: string;
begin
  if not FileExists(GetAppPath + EDITOR_HEADERS) then
    SetEditorHeadersText(
      '// Author:  ' + M_USERNAME + LE +
      '// Created: ' + M_DATE_TIME);
  with TStringList.Create do
  begin
    try
      try
        LoadFromFile(GetAppPath + EDITOR_HEADERS);
        Result := Text;
      except
        AddLogMsg(ERR_FAILED_DOWNLOAD + ': ' + EDITOR_HEADERS, lmtErr);
      end;
    finally
      Free;
    end;
  end;
end;

function TEditorConfig.GetFontName: string;
var
  FName: string;

begin
  {$IFDEF MSWINDOWS}
  FName := 'Consolas';
  {$ENDIF}
  {$IFDEF UNIX}
  FName := 'Dejavu Sans Mono';
  {$ENDIF}
  Result := MainConfig.ReadString('EDITOR', 'FontName', FName);
end;

function TEditorConfig.GetFontSize: integer;
var
  FSize: integer;

begin
  {$IFDEF MSWINDOWS}
  FSize := 10;
  {$ENDIF}
  {$IFDEF UNIX}
  FSize := 9;
  {$ENDIF}
  Result := MainConfig.ReadInteger('EDITOR', 'FontSize', FSize);
end;

function TEditorConfig.GetIdentifierColor: TColor;
begin
  Result := dmCodeEditor.synPasHighlighter.IdentifierAttri.Foreground;
end;

function TEditorConfig.GetKeyColor: TColor;
begin
  Result := dmCodeEditor.synPasHighlighter.KeyAttri.Foreground;
end;

function TEditorConfig.GetNumberColor: TColor;
begin
  Result := dmCodeEditor.synPasHighlighter.NumberAttri.Foreground;
end;

function TEditorConfig.GetRightEdge: integer;
begin
  Result := MainConfig.ReadInteger('EDITOR', 'RightEdge', 80);
end;

function TEditorConfig.GetRightEdgeColor: TColor;
begin
  Result := StringToColor(HighlighterConfig.ReadString('EDITOR', 'RightEdgeColor', '$FAFAFA'));
end;

function TEditorConfig.GetScrollPastEol: boolean;
begin
  Result := MainConfig.ReadBool('EDITOR', 'ScrollPastEol', False);
end;

function TEditorConfig.GetSelectedColor: TColor;
begin
  Result := StringToColor(HighlighterConfig.ReadString('EDITOR', 'SelectedColor', 'clDefault'));
end;

function TEditorConfig.GetStringColor: TColor;
begin
  Result := dmCodeEditor.synPasHighlighter.StringAttri.Foreground;
end;

function TEditorConfig.GetSymbolColor: TColor;
begin
  Result := dmCodeEditor.synPasHighlighter.SymbolAttri.Foreground;
end;

procedure TEditorConfig.SetCommentColor(AValue: TColor);
begin
  dmCodeEditor.synPasHighlighter.CommentAttri.Foreground := AValue;
end;

procedure TEditorConfig.SetEditorColor(AValue: TColor);
begin
  HighlighterConfig.WriteString('EDITOR', 'EditorColor', ColorToString(AValue));
end;

procedure TEditorConfig.SetEditorCompletionWidth(AValue: integer);
begin
  MainConfig.WriteInteger('EDITOR', 'CompletionWidth', AValue);
end;

procedure TEditorConfig.SetEditorHeadersText(AValue: string);
begin
  with TStringList.Create do
  begin
    try
      try
        Text := AValue;
        SaveToFile(GetAppPath + EDITOR_HEADERS);
      except
        AddLogMsg(ERR_SAVING + ': ' + EDITOR_HEADERS, lmtErr);
      end;
    finally
      Free;
    end;
  end;
end;

procedure TEditorConfig.SetFontName(AValue: string);
begin
  MainConfig.WriteString('EDITOR', 'FontName', AValue);
end;

procedure TEditorConfig.SetFontSize(AValue: integer);
begin
  MainConfig.WriteInteger('EDITOR', 'FontSize', AValue);
end;

procedure TEditorConfig.SetIdentifierColor(AValue: TColor);
begin
  dmCodeEditor.synPasHighlighter.IdentifierAttri.Foreground := AValue;
end;

procedure TEditorConfig.SetKeyColor(AValue: TColor);
begin
  dmCodeEditor.synPasHighlighter.KeyAttri.Foreground := AValue;
end;

procedure TEditorConfig.SetNumberColor(AValue: TColor);
begin
  dmCodeEditor.synPasHighlighter.NumberAttri.Foreground := AValue;
end;

procedure TEditorConfig.SetRightEdge(AValue: integer);
begin
  MainConfig.WriteInteger('EDITOR', 'RightEdge', AValue);
end;

procedure TEditorConfig.SetRightEdgeColor(AValue: TColor);
begin
  HighlighterConfig.WriteString('EDITOR', 'RightEdgeColor', ColorToString(AValue));
end;

procedure TEditorConfig.SetScrollPastEol(AValue: boolean);
begin
  MainConfig.WriteBool('EDITOR', 'ScrollPastEol', AValue);
end;

procedure TEditorConfig.SetSelectedColor(AValue: TColor);
begin
  HighlighterConfig.WriteString('EDITOR', 'SelectedColor', ColorToString(AValue));
end;

procedure TEditorConfig.SetStringColor(AValue: TColor);
begin
  dmCodeEditor.synPasHighlighter.StringAttri.Foreground := AValue;
end;

procedure TEditorConfig.SetSymbolColor(AValue: TColor);
begin
  dmCodeEditor.synPasHighlighter.SymbolAttri.Foreground := AValue;
end;

end.

