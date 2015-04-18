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

unit uAutocomplete;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, LCLType, Graphics, SynEdit, SynCompletion;

type

  { TAutocomplete }

  TAutocomplete = class
  private
    CompletionList: TStringList;
    EditorCompletion: TSynCompletion;
    function EditorCompletionPaintItem(const AKey: string; ACanvas: TCanvas; X, Y: integer; Selected: boolean; Index: integer): boolean;
    function GetCompletionKeyFromIndex(Index: integer; ACanvas: TCanvas): string;
    procedure DelCompletionKey(var AValue: TStringList);
    procedure EditorCompletionCodeCompletion(var AValue: string; SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function AddEditor(AValue: TCustomSynEdit): integer;
  end;

implementation

uses
  uAMPASCore,
  uEditorConfig;

{ TAutocomplete }

constructor TAutocomplete.Create(AOwner: TComponent);
var
  sList: TStringList;

begin
  CompletionList := TStringList.Create;
  try
    CompletionList.LoadFromFile(GetAppPath + EDITOR_COMPLETION);
  except
    AddLogMsg('Не удалось загрузить файл автодополнения: ' + EDITOR_COMPLETION, lmtErr);
  end;

  EditorCompletion := TSynCompletion.Create(AOwner);
  EditorCompletion.LinesInWindow := 12;
  EditorCompletion.OnCodeCompletion := @EditorCompletionCodeCompletion;
  EditorCompletion.OnPaintItem := @EditorCompletionPaintItem;
  EditorCompletion.SelectedColor := $E0E0E0;
  EditorCompletion.ShowSizeDrag := True;
  EditorCompletion.Width := 520;

  sList := TStringList.Create;
  try
    sList.Text := CompletionList.Text;
    DelCompletionKey(sList);
    EditorCompletion.ItemList := sList;
  finally
    FreeAndNil(sList);
  end;
end;

destructor TAutocomplete.Destroy;
begin
  FreeAndNil(CompletionList);
  inherited Destroy;
end;

function TAutocomplete.AddEditor(AValue: TCustomSynEdit): integer;
begin
  Result := EditorCompletion.AddEditor(AValue);
end;

procedure TAutocomplete.EditorCompletionCodeCompletion(var AValue: string; SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
begin
  with TRegExpr.Create do
  begin
    try
      Expression := '\w*\(';
      if Exec(AValue) then
      begin
        AValue := Match[0];
        EditorCompletion.AddCharAtCursor(')');
      end
      else
      begin
        Expression := '(\w*):';
        if Exec(AValue) then
          AValue := Match[1]
        else
        begin
          Expression := '(\w*);';
          if Exec(AValue) then
            AValue := Match[1];
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function TAutocomplete.EditorCompletionPaintItem(const AKey: string; ACanvas: TCanvas; X, Y: integer; Selected: boolean; Index: integer): boolean;
begin
  ACanvas.Font.Name := EditorConfig.FontName;
  ACanvas.Font.Size := EditorConfig.FontSize;

  ACanvas.TextOut(0, Y, GetCompletionKeyFromIndex(Index, ACanvas));

  ACanvas.Font.Color := $303030;
  ACanvas.TextOut(90, Y, AKey);

  Result := True;
end;

function TAutocomplete.GetCompletionKeyFromIndex(Index: integer; ACanvas: TCanvas): string;
const
  KEY_CONST = 'const';
  KEY_FUNCTION = 'function';
  KEY_MIMETYPE = 'mimetype';
  KEY_PROCEDURE = 'procedure';
  KEY_PROPERTY = 'property';
  KEY_TYPE = 'type';

begin
  Result := CompletionList.Strings[Index];

  with TRegExpr.Create do
  begin
    try
      Expression := '\w*';
      if Exec(Result) then
        Result := Match[0];
    finally
      Free;
    end;
  end;

  with ACanvas.Font do
  begin
    case Result of
      KEY_CONST: Color := clMaroon;
      KEY_FUNCTION: Color := clGreen;
      KEY_MIMETYPE: Color := clOlive;
      KEY_PROCEDURE: Color := clNavy;
      KEY_PROPERTY: Color := clTeal;
      KEY_TYPE: Color := clPurple;
    end;
  end;
end;

procedure TAutocomplete.DelCompletionKey(var AValue: TStringList);
var
  i: integer;

begin
  with TRegExpr.Create do
  begin
    try
      Expression := '\s(.*?)$';
      for i := 0 to AValue.Count - 1 do
        if Exec(AValue.Strings[i]) then
          AValue.Strings[i] := Match[1];
    finally
      Free;
    end;
  end;
end;

end.

