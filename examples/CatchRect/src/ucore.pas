// Author:  helltar
// Created: 21.02.2015 03:13:33

unit uCore;

interface

{ public declarations }

var
  SH, SW: integer;
  GAME_OVER: boolean;

procedure Refresh;
function isAndroid(): boolean;

implementation

{ add unit functions & procedures here }

procedure Refresh;
begin
  Repaint;
  Delay(30);
end;

function isAndroid(): boolean;
begin
  Result := GetWidth > 240;
end;

initialization

  { add initialization code here }

  SH := GetHeight;
  SW := GetWidth;
  GAME_OVER := false;

end.

