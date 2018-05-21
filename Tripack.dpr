program Tripack;

{%TogetherDiagram 'ModelSupport_Tripack\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\SfrFortranInterface\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\frmMainUnit\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\SfrProcedures\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\sfrTestUnit\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\TripackProcedures\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\Tripack\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\TripackMessages\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\RealListUnit\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\SfrDriver\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\IntListUnit\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\SfrInterpolatorUnit\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\TripackPascalDriver\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\SfrMessages\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tripack\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tripack\sfrTestUnit\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tripack\Tripack\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tripack\SfrInterpolatorUnit\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tripack\SfrFortranInterface\default.txvpck'}

uses
  Forms,
  frmMainUnit in 'frmMainUnit.pas' {frmMain},
  TripackMessages in 'TripackMessages.pas',
  TripackTypes in 'TripackTypes.pas',
  TripackPascalDriver in 'TripackPascalDriver.pas',
  TripackProcedures in 'TripackProcedures.pas',
  SfrMessages in 'SfrMessages.pas',
  SfrProcedures in 'SfrProcedures.pas',
  SfrFortranInterface in 'SfrFortranInterface.pas',
  SfrDriver in 'SfrDriver.pas',
  IntListUnit in 'IntListUnit.pas',
  RealListUnit in 'RealListUnit.pas',
  sfrTestUnit in 'sfrTestUnit.pas' {frmSfrtest},
  SfrInterpolatorUnit in 'SfrInterpolatorUnit.pas',
  FastGEO in 'FastGEO.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSfrtest, frmSfrtest);
  Application.Run;
end.
