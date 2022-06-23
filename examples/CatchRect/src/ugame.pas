// Author:  helltar
// Created: 21.02.2015 03:45:14

unit uGame;

interface

{ public declarations }

procedure RunGame;

implementation

{ add unit functions & procedures here }

uses
  uCore,
  uRect,
  uPlatfrm;

var
  Level: integer;

procedure Cls;
begin
  SetColor(0, 0, 0);
  FillRect(0, 0, SW, SH);
end;

procedure CheckLevel(AScore: integer);
begin
  if AScore = 100 then
    Level := 3;
  if AScore = 200 then
    Level := 4;
  if AScore = 300 then
    Level := 5;
  if AScore = 400 then
    Level := 6;
  if AScore = 500 then
    Level := 7;
  if AScore = 600 then
    Level := 8;
end;

procedure DrawGameInfo;
const
  L_MARG = 32;

var
  sLife, sScore, sLevel: string;
  T_MARG: integer;

begin
  sLife := 'Life: ' + IntegerToString(Life);
  sScore := 'Score: ' + IntegerToString(Score);
  sLevel := 'Level: ' + IntegerToString(Level);

  SetColor(255, 255, 255);

  T_MARG := 24;

  if isAndroid then T_MARG := 48;

  DrawText(sLife, SW - GetStringWidth(sLife) - L_MARG, T_MARG * 2);
  DrawText(sScore, SW - GetStringWidth(sScore) - L_MARG, T_MARG * 3);
  DrawText(sLevel, SW - GetStringWidth(sLevel) - L_MARG, T_MARG * 4);
end;

procedure DrawCubes;
begin
  DrawBox(Level);
  DrawBomb(Level);
  DrawBonus(Level + 1);
end;

function IsGameOver: boolean;
begin
  Result := Life <= 0;
end;

procedure start;
var
  key: integer;
  out: boolean;

begin
  out := false;

  repeat
    SetColor(255, 255, 255);
    FillRect(0, 0, getWidth, getHeight);

    Repaint;
    Delay(20);

    key := getKeyClicked;

    if KEY = KE_KEY0 then
      out := true;

    key := keyToAction(key);
  until out;
end;

procedure DrawGOS; // game over screen
const
  GameOverText = 'Game Over';

begin
  Cls;

  SetColor(255, 20, 20);
  SetFont(FONT_FACE_PROPORTIONAL, FONT_STYLE_BOLD, FONT_SIZE_LARGE);
  DrawText(GameOverText, (GetWidth - GetStringWidth(GameOverText)) div 2, 100);

  Refresh;
  Delay(5000);

  GAME_OVER := true;
end;

procedure RunGame;
begin
  Cls;
  DrawCubes;
  MovePlatfrm;
  DrawGameInfo;
  CheckLevel(Score);

  if IsGameOver then
    DrawGOS;

  Refresh;
end;

initialization

  { add initialization code here }

  Level := 4;

end.

