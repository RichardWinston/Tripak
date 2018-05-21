unit frmMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, RealListUnit, IntListUnit;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    memoFortran: TMemo;
    memoPascal: TMemo;
    Splitter1: TSplitter;
    btnTripackFortran: TButton;
    Panel2: TPanel;
    Splitter2: TSplitter;
    Panel3: TPanel;
    Splitter3: TSplitter;
    memoFortranEPS: TMemo;
    memoPascalEPS: TMemo;
    btnTripackPascal: TButton;
    btnTripackTest: TButton;
    btnSfrpackFortran: TButton;
    btnSfrpackPascal: TButton;
    btnSfrpackTest: TButton;
    sfrButton2: TButton;
    btn1: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSfrpackTestClick(Sender: TObject);
    procedure btnSfrpackPascalClick(Sender: TObject);
    procedure btnSfrpackFortranClick(Sender: TObject);
    procedure btnTripackTestClick(Sender: TObject);
    procedure btnTripackPascalClick(Sender: TObject);
    procedure btnTripackFortranClick(Sender: TObject);
    procedure sfrButton2Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    FortranReals, PascalReals: TRealList;
    FortranIntegers, PascalIntegers: TIntegerList;

    { Public declarations }
  end;

var
  frmMain: TfrmMain;

procedure TripackDemo(); stdcall; external 'tripack.dll';

implementation

{$R *.dfm}

uses TripackTypes, TripackPascalDriver, SfrFortranInterface,
  SfrDriver, sfrTestUnit;

procedure TfrmMain.btn1Click(Sender: TObject);
begin
  ModifiedTripackDemoPascal;
end;

procedure TfrmMain.btnSfrpackFortranClick(Sender: TObject);
begin
  SfrDemo;
  // The fortran procedure doesn;t give the same result the second time.  
  btnSfrpackTest.Enabled := False;
end;

procedure TfrmMain.btnSfrpackPascalClick(Sender: TObject);
begin
  SfrDemoPascal;
end;

procedure TfrmMain.btnSfrpackTestClick(Sender: TObject);
var
  Same: boolean;
  I: Integer;
begin
  // The fortran procedure doesn;t give the same result the second time.  
  btnSfrpackTest.Enabled := False;
  
  memoFortran.Lines.Clear;
  memoFortranEPS.Lines.Clear;
  memoPascal.Lines.Clear;
  memoPascalEPS.Lines.Clear;
  FortranIntegers.Clear;
  PascalIntegers.Clear;
  FortranReals.Clear;
  PascalReals.Clear;
  SfrDemo;
  SfrDemoPascal;
  Same := FortranIntegers.Count = PascalIntegers.Count;
  if Same then
  begin
    for I := 0 to FortranIntegers.Count - 1 do
    begin
      Same := FortranIntegers[I] = PascalIntegers[I];
      if not Same then
      begin
        break;
      end;
    end;
  end;
  if Same then
  begin
    Same := FortranReals.Count = PascalReals.Count;
    if Same then
    begin
      for I := 0 to FortranReals.Count - 1 do
      begin
        Same := (FortranReals[I] = PascalReals[I]);// < 1e-5;
        if not Same then
        begin
          Same := Abs(FortranReals[I] - PascalReals[I]) < 7e-5;
        end;
        if not Same then
        begin
          Same := Abs(FortranReals[I] - PascalReals[I])/
            (Abs(FortranReals[I]) + Abs(PascalReals[I])) < 7e-5;
        end;
        if not Same then
        begin
          ShowMessage(IntToStr(I) + ' '
            + FloatToStr(FortranReals[I]) + ' ' + FloatToStr(PascalReals[I]));
          break;
        end;
      end;
    end;
  end;
{  Same := memoFortran.Lines.Count = memoPascal.Lines.Count;
  if Same then
  begin
    for I := 0 to memoFortran.Lines.Count - 1 do
    begin
      Same := memoFortran.Lines[I] = memoPascal.Lines[I];
      if not Same then
      begin
        break;
      end;
    end;
  end;
  if Same then
  begin
    Same := memoFortranEPS.Lines.Count = memoPascalEPS.Lines.Count;
    if Same then
    begin
      for I := 0 to memoFortranEPS.Lines.Count - 1 do
      begin
        Same := memoFortranEPS.Lines[I] = memoPascalEPS.Lines[I];
        if not Same then
        begin
          break;
        end;
      end;
    end;
  end;}
  if Same then
  begin
    ShowMessage('Success');
  end
  else
  begin
    ShowMessage('Failure');
  end;
end;

procedure TfrmMain.btnTripackFortranClick(Sender: TObject);
begin
  TripackDemo;
  // The fortran routine doesn;t give the same result the second time.
  btnTripackTest.Enabled := False;
end;

procedure TfrmMain.btnTripackPascalClick(Sender: TObject);
begin
  TripackDemoPascal;
end;

procedure TfrmMain.btnTripackTestClick(Sender: TObject);
var
  Same: boolean;
  I: Integer;
begin
  // The fortran routine doesn;t give the same result the second time.
  btnTripackTest.Enabled := False;

  memoFortran.Lines.Clear;
  memoFortranEPS.Lines.Clear;
  memoPascal.Lines.Clear;
  memoPascalEPS.Lines.Clear;
  TripackDemo;
  TripackDemoPascal;
  Same := memoFortran.Lines.Count = memoPascal.Lines.Count;
  if Same then
  begin
    for I := 0 to memoFortran.Lines.Count - 1 do
    begin
      Same := memoFortran.Lines[I] = memoPascal.Lines[I];
      if not Same then
      begin
        break;
      end;
    end;
  end;
  if Same then
  begin
    Same := memoFortranEPS.Lines.Count = memoPascalEPS.Lines.Count;
    if Same then
    begin
      for I := 0 to memoFortranEPS.Lines.Count - 1 do
      begin
        Same := memoFortranEPS.Lines[I] = memoPascalEPS.Lines[I];
        if not Same then
        begin
          break;
        end;
      end;
    end;
  end;
  if Same then
  begin
    ShowMessage('Success');
  end
  else
  begin
    ShowMessage('Failure');
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FortranReals := TRealList.Create;
  PascalReals := TRealList.Create;
  FortranIntegers := TIntegerList.Create;
  PascalIntegers := TIntegerList.Create;

end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FortranReals.Free;
  PascalReals.Free;
  FortranIntegers.Free;
  PascalIntegers.Free;
end;

procedure TfrmMain.sfrButton2Click(Sender: TObject);
begin
  frmSfrtest.ShowModal;
end;

end.
