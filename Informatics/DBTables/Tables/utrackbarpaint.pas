unit UTrackBarPaint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics;

type
  TPaintTrackBar = class(TPersistent)
  protected
    Width: integer;
    Height: integer;
    Top: integer;
    Left: integer;
  public
    constructor Create(
      ACanvas: TCanvas; AWidth, AHeight, ATop, ALeft: integer); overload;
    procedure OnPaint(ACanvas: TCanvas);
  end;

implementation
  constructor TPaintTrackBar.Create(
    ACanvas: TCanvas; AWidth, AHeight, ATop, ALeft: integer);
  begin
    Width := AWidth;
    Height := AHeight;
    Top := ATop;
    Left := ALeft;
  end;

  procedure TPaintTrackBar.OnPaint(ACanvas: TCanvas);
  begin
    ACanvas.Brush.Color := clGreen;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Rectangle(Left, Top, Left + Width, Top + Height);
  end;

end.

