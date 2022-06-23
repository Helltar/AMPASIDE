// Author:  helltar
// Created: 24.02.2015 05:37:21

unit uPlatfrm;

interface

{ public declarations }

type

  TPlatfrm = record
    X, Y: integer;
    H, W: integer;
  end;

var
  Platfrm: TPlatfrm;

procedure MovePlatfrm;

implementation

{ add unit functions & procedures here }

uses
  uCore,
  Sensor;

procedure MovePlatfrm;
begin
  Platfrm.X := pointer_dragged_x - (Platfrm.W div 2);
  SetColor(210, 210, 210);
  FillRect(Platfrm.X, Platfrm.Y, Platfrm.W, Platfrm.H);
end;

initialization

  { add initialization code here }

  Sensor.Init;

  Platfrm.H := 16;
  Platfrm.W := 48;

  if isAndroid then
    begin
      Platfrm.H := 32;
      Platfrm.W := 90;
    end;

  Platfrm.Y := SH - Platfrm.H - 6;

end.

