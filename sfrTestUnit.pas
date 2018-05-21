unit sfrTestUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, JvExComCtrls, JvStatusBar, TripackTypes, SfrInterpolatorUnit;

type
  TfrmSfrtest = class(TForm)
    JvStatusBar1: TJvStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    FSfrInterpolator: TSfrInterpolator;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSfrtest: TfrmSfrtest;

implementation

{$R *.dfm}

uses TripackProcedures, SfrProcedures;

{ SfrInterpolator }







procedure TfrmSfrtest.FormCreate(Sender: TObject);
var
  X, Y, Z: TSingleArray;
  Index: Integer;
begin
   FSfrInterpolator := TSfrInterpolator.Create;
   SetLength(X, 100);
   SetLength(Y, 100);
   SetLength(Z, 100);
   Randomize;
   for Index := 0 to Length(X) - 1 do
   begin
     X[Index] := Random * ClientWidth;
     Y[Index] := Random * ClientHeight;
     Z[Index] := Random;
   end;
   FSfrInterpolator.Initialize(X, Y, Z);
end;

procedure TfrmSfrtest.FormDestroy(Sender: TObject);
begin
  FSfrInterpolator.Free;
end;

procedure TfrmSfrtest.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  JvStatusBar1.SimpleText :=
    FloatToStr(FSfrInterpolator.Interpolate1(X, Y)) + ' '
    + FloatToStr(FSfrInterpolator.Interpolate2(X, Y))
end;

end.
