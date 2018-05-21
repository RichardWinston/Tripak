unit SfrMessages;

interface

{$IFDEF UseSfrMessages}

uses SysUtils, Classes, FastGeo;

Procedure Sfr400(var Ifortran: longint; var N: longint); stdcall;
Procedure Sfr205(var Ifortran: longint; var N, NCC: longint); stdcall;
Procedure Sfr210(var Ifortran: longint); stdcall;
Procedure Sfr212(var Ifortran: longint); stdcall;
Procedure Sfr214(var Ifortran: longint); stdcall;
Procedure Sfr220(var Ifortran: longint; var IER: longint); stdcall;
Procedure Sfr230(var Ifortran: longint; var IER: longint); stdcall;
Procedure Sfr240(var Ifortran: longint; var IER, NIT: longint); stdcall;
Procedure Sfr250(var Ifortran: longint; var IER: longint); stdcall;
Procedure Sfr260(var Ifortran: longint; var IER: longint); stdcall;
Procedure Sfr300(var Ifortran: longint; var BoundaryNodes, Arcs,
  Triangles : longint; var Areap, Area2, AreaaExcluding: TFloat); stdcall;
Procedure Sfr310(var Ifortran: longint; var MaxError: TFloat); stdcall;
Procedure Sfr320(var Ifortran: longint; var MaxError: TFloat); stdcall;
Procedure Sfr330(var Ifortran: longint; var MaxError: TFloat); stdcall;
Procedure Sfr340(var Ifortran: longint; var MaxError: TFloat; var Count, Points: longint); stdcall;
Procedure Sfr350(var Ifortran: longint; var MaxError: TFloat); stdcall;
Procedure Sfr360(var Ifortran: longint); stdcall;
Procedure Cntour100(var Ifortran: longint; var NCON: longint); stdcall;
Procedure Cntour110(var Ifortran: longint; var NCON: longint); stdcall;
Procedure Crplot100(var Ifortran: longint; var IPX1, IPY1, IPX2, IPY2: longint); stdcall;
Procedure Crplot110(var Ifortran: longint; var X, Y: longint); stdcall;
Procedure Crplot120(var Ifortran: longint; var X, Y: longint); stdcall;
Procedure Crplot130(var Ifortran: longint); stdcall;
Procedure Crplot140(var Ifortran: longint); stdcall;
Procedure Crplot150(var Ifortran: longint; var TX, TY, SFX, SFY: TFloat); stdcall;
Procedure Crplot160(var Ifortran: longint; var T: TFloat); stdcall;
Procedure Crplot170(var Ifortran: longint; var X, Y: TFloat); stdcall;
Procedure Crplot180(var Ifortran: longint; var X, Y: TFloat); stdcall;
Procedure Crplot200(var Ifortran: longint); stdcall;
procedure Crplot210(var IFortran: longint; var AChar: Char); stdcall;
procedure Getsig100(var IFortran: longint; var N: longint; var TOL: TFloat); stdcall;
procedure Getsig110(var IFortran: longint; var N1, N2: longint); stdcall;
Procedure Getsig120(var Ifortran: longint; var SIG, FSIG, FPSIG: TFloat); stdcall;
procedure Getsig130(var IFortran: longint; var DSIG: TFloat); stdcall;
procedure Getsig140(var IFortran: longint; var NIT: longint; var SIG, F: TFloat); stdcall;
procedure Sgprnt100(var IFortran: longint; var N: longint); stdcall;
procedure Sgprnt110(var IFortran: longint; var N1, N2: longint; var Tension: TFloat); stdcall;
procedure Sgprnt120(var IFortran: longint; var N1, N2: longint; var S1, S2: TFloat); stdcall;
procedure Sgprnt130(var IFortran: longint); stdcall;
procedure Sgprnt140(var IFortran: longint; var NA: longint); stdcall;
procedure Sgprnt200(var IFortran: longint; var Count: longint); stdcall;
procedure Sgprnt210(var IFortran: longint; var Value: longint); stdcall;
procedure Sgprnt220(var IFortran: longint; var NMAX: longint); stdcall;
procedure Sig0_100(var IFortran: longint; var N1, N2: longint; var BND: TFloat); stdcall;
procedure Sig0_110(var IFortran: longint; var N1, N2: longint; var BND: TFloat); stdcall;
procedure Sig0_120(var IFortran: longint; var SIG, SNEG, F0, FMAX: TFloat); stdcall;
procedure Sig0_130(var IFortran: longint; var NIT: longint; var SIG, F: TFloat); stdcall;
procedure Sig0_140(var IFortran: longint; var DSIG: TFloat); stdcall;
procedure Sig1_100(var IFortran: longint; var N1, N2: longint; var BND: TFloat); stdcall;
procedure Sig1_110(var IFortran: longint; var N1, N2: longint; var BND: TFloat); stdcall;
procedure Sig1_120(var IFortran: longint; var F0, FMAX, SIG: TFloat); stdcall;
procedure Sig1_130(var IFortran: longint; var NIT: longint; var SIG, F: TFloat); stdcall;
procedure Sig1_140(var IFortran: longint; var DSIG: TFloat); stdcall;
procedure Sig2_100(var IFortran: longint; var N1, N2: longint); stdcall;
procedure Sig2_110(var IFortran: longint; var NIT: longint; var SIG, F, FP: TFloat); stdcall;
procedure Smsurf100(var IFortran: longint); stdcall;
procedure Smsurf110(var IFortran: longint; var SM, TOL: TFloat; var NITMAX: longint; var G0: TFloat); stdcall;
procedure Smsurf120(var IFortran: longint; var ITER: longint;  var P, G: TFloat;
  var NIT: longint; var DFMAX: TFloat); stdcall;
procedure Smsurf130(var IFortran: longint; var DP: TFloat); stdcall;

exports Sfr400, Sfr205, Sfr210, Sfr212, Sfr214, Sfr220, Sfr230, Sfr240, Sfr250,
  Sfr260, Sfr300, Sfr310, Sfr320, Sfr330, Sfr340, Sfr350, Sfr360, Cntour100,
  Cntour110, Crplot100, Crplot110, Crplot120, Crplot130, Crplot140, Crplot150,
  Crplot160, Crplot170, Crplot180, Crplot200, Crplot210, Getsig100, Getsig110,
  Getsig120, Getsig130, Getsig140, Sgprnt100, Sgprnt110, Sgprnt120, Sgprnt130,
  Sgprnt140, Sgprnt200, Sgprnt210, Sgprnt220, Sig0_100, Sig0_110, Sig0_120,
  Sig0_130, Sig0_140, Sig1_100, Sig1_110, Sig1_120, Sig1_130, Sig1_140,
  Sig2_100, Sig2_110, Smsurf100, Smsurf110, Smsurf120, Smsurf130;

procedure Getsig100P(const IFortran: longint; const N: longint; const TOL: TFloat);
procedure Sgprnt100P(const IFortran: longint; const N: longint);

{$ENDIF}

implementation

{$IFDEF UseSfrMessages}

uses frmMainUnit, IntListUnit, RealListUnit;

function GetStrings(const IFortran: longint): TStrings;
begin
  result := nil;
  if IFortran = 1 then
  begin
    result := frmMain.memoFortran.Lines;
  end
  else if IFortran = 2 then
  begin
    result := frmMain.memoPascal.Lines;
  end
  else
  if IFortran = 3 then
  begin
    result := frmMain.memoFortranEPS.Lines;
  end
  else if IFortran = 4 then
  begin
    result := frmMain.memoPascalEPS.Lines;
  end
  else
  begin
    Assert(False);
  end;
end;

Procedure GetLists(const IFortran: longint; out Reals: TRealList;
  out Integers: TIntegerList);
begin
  case IFortran of
    1, 3:
      begin
        Reals := frmMain.FortranReals;
        Integers := frmMain.FortranIntegers;
      end;
    2, 4:
      begin
        Reals := frmMain.PascalReals;
        Integers := frmMain.PascalIntegers;
      end;
    else
      begin
        Assert(False);
      end;
  end;
end;

//C Heading:
//C
Procedure Sfr400(var Ifortran: longint; var N: longint); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('SRFPACK Test:  N = ' + IntToStr(N));
//  400 FORMAT (///1X,21X,'SRFPACK Test:  N =',I4///)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(N);
end;

//C
//C Error message formats:
//C
Procedure Sfr205(var Ifortran: longint; var N, NCC: longint); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('*** N or NCC is outside its valid '
                 + 'range:  N =' + IntToStr(N) + ', NCC = '
                 + IntToStr(NCC) + ' ***');
//  205 FORMAT (//5X,'*** N or NCC is outside its valid ',
//     .             'range:  N =',I5,', NCC = ',I4,' ***'/)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(N);
  Integers.Add(NCC);
end;

Procedure Sfr210(var Ifortran: longint); stdcall;
begin
  GetStrings(IFortran).Add('*** Error in TRMESH:  the first three '
     +        'nodes are collinear ***');
//  210 FORMAT (//5X,'*** Error in TRMESH:  the first three ',
//     .        'nodes are collinear ***'/)
end;

Procedure Sfr212(var Ifortran: longint); stdcall;
begin
  GetStrings(IFortran).Add('*** Error in TRMESH:  invalid '
     +      'triangulation ***');
//  212 FORMAT (//5X,'*** Error in TRMESH:  invalid ',
//     .        'triangulation ***'/)
end;

Procedure Sfr214(var Ifortran: longint); stdcall;
begin
  GetStrings(IFortran).Add('*** Error in TRMESH:  duplicate nodes '
     +        'encountered ***');
//  214 FORMAT (//5X,'*** Error in TRMESH:  duplicate nodes ',
//     .        'encountered ***'/)
end;

Procedure Sfr220(var Ifortran: longint; var IER: longint); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('*** Error in ADDCST:  IER = ' + IntToStr(IER)
     +        ' ***');
//  220 FORMAT (//5X,'*** Error in ADDCST:  IER = ',I1,
//     .        ' ***'/)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(IER);
end;

Procedure Sfr230(var Ifortran: longint; var IER: longint); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('*** Error in GRADG (invalid flag '
     +        'returned):  IER = ' + IntToStr(IER) + ' ***');
//  230 FORMAT (//5X,'*** Error in GRADG (invalid flag ',
//     .        'returned):  IER = ',I2,' ***'/)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(IER);
end;

Procedure Sfr240(var Ifortran: longint; var IER, NIT: longint); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('*** Error in ZGRADG:  IER = '
              + IntToStr(IER) + ', NIT = ' + IntToStr(NIT) + ' ***');
//  240 FORMAT (//5X,'*** Error in ZGRADG:  IER = ',
//     .        I2,', NIT = ',I3,' ***'/)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(IER);
  Integers.Add(NIT);
end;

Procedure Sfr250(var Ifortran: longint; var IER: longint); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('*** Error in INTRC1 (invalid flag '
     +        'returned):  IER = '+ IntToStr(IER) +' ***');
//  250 FORMAT (//5X,'*** Error in INTRC1 (invalid flag ',
//     .        'returned):  IER = ',I2,' ***'/)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(IER);
end;

Procedure Sfr260(var Ifortran: longint; var IER: longint); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('*** Error in CRPLOT:  IER = '+ IntToStr(IER) +
             ' ***');
//  260 FORMAT (//5X,'*** Error in CRPLOT:  IER = ',I1,
//     .        ' ***'/)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(IER);
end;

//C
//C Test message formats:
//C
Procedure Sfr300(var Ifortran: longint; var BoundaryNodes, Arcs,
  Triangles : longint; var Areap, Area2, AreaaExcluding: TFloat); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  with GetStrings(IFortran) do
  begin
    Add('Output from BNODES, AREA, and VOLUME');
    Add('     BNODES:  '+ IntToStr(BoundaryNodes) +' Boundary Nodes,  '+ IntToStr(Arcs) +
             ' Arcs,  '+ IntToStr(Triangles) +' Triangles');
    Add('     AREAP:   Area of convex hull = ' + FloatToStr(Areap));
    Add('     VOLUME:  Area of convex hull = ' + FloatToStr(Area2));
    Add('     VOLUME:  Area excluding constraint regions = ' + FloatToStr(AreaaExcluding));
  end;
//  300 FORMAT (//5X,'Output from BNODES, AREA, and VOLUME'//
//     .        5X,'BNODES:  ',I4,' Boundary Nodes,  ',I4,
//     .        ' Arcs,  ',I4,' Triangles'/5X,
//     .        'AREAP:   Area of convex hull = ',E15.8/5X,
//     .        'VOLUME:  Area of convex hull = ',E15.8/5X,
//     .        'VOLUME:  Area excluding constraint regions',
//     .        ' = ',E15.8//)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(BoundaryNodes);
  Integers.Add(Arcs);
  Integers.Add(Triangles);
  Reals.Add(Areap);
  Reals.Add(Area2);
  Reals.Add(AreaaExcluding);
end;

Procedure Sfr310(var Ifortran: longint; var MaxError: TFloat); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('GRADG Test:  Maximum error = '+ FloatToStr(MaxError));
//  310 FORMAT (5X,'GRADG Test:  Maximum error = ',
//     .        E15.8//)
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(MaxError);
end;

Procedure Sfr320(var Ifortran: longint; var MaxError: TFloat); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('INTRC0 Test:  Maximum error = '+ FloatToStr(MaxError));
//  320 FORMAT (5X,'INTRC0 Test:  Maximum error = ',
//     .        E15.8//)
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(MaxError);
end;

Procedure Sfr330(var Ifortran: longint; var MaxError: TFloat); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('GRADL Test:  Maximum error = '+ FloatToStr(MaxError));
//  330 FORMAT (5X,'GRADL Test:  Maximum error = ',
//     .        E15.8//)
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(MaxError);
end;

Procedure Sfr340(var Ifortran: longint; var MaxError: TFloat; var Count, Points: longint); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('UNIF (INTRC1) Test:  Maximum error = '+ FloatToStr(MaxError));
  GetStrings(IFortran).Add(IntToStr(Count) + ' of ' + IntToStr(Points) +
           ' points required extrapolation');
//  340 FORMAT (5X,'UNIF (INTRC1) Test:  Maximum ',
//     .        'error = ',E15.8/5X,I2,' of ',I3,
//     .        ' points required extrapolation'//)
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(MaxError);
  Integers.Add(Count);
  Integers.Add(Points);
end;

Procedure Sfr350(var Ifortran: longint; var MaxError: TFloat); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('ZGRADG/GETSIG Test:  Maximum interpolation error = '+ FloatToStr(MaxError));
//  350 FORMAT (5X,'ZGRADG/GETSIG Test:  Maximum ',
//     .        'interpolation error = ',E15.8//)
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(MaxError);
end;

Procedure Sfr360(var Ifortran: longint); stdcall;
begin
  GetStrings(IFortran).Add('A contour plot file was '
     +            'successfully created.')
//  360 FORMAT (/5X,'A contour plot file was ',
//     .            'successfully created.')
end;

Procedure Cntour100(var Ifortran: longint; var NCON: longint); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('Error in CNTOUR:  Contour line L '
     +        'begins on the boundary and terminates '
     +        'in the interior for L =' + IntToStr(NCON));
//  100 FORMAT (///5X,'Error in CNTOUR:  Contour line L ',
//     .        'begins on the boundary'/5X,'and terminates ',
//     .        'in the interior for L =',I4/)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(NCON);
end;

Procedure Cntour110(var Ifortran: longint; var NCON: longint); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('Error in CNTOUR:  Contour line L '
     +        'is open but does not intersect the '
     +        'boundary for L =' + IntToStr(NCON));
//  110   FORMAT (///5X,'Error in CNTOUR:  Contour line L ',
//     .          'is open but'/5X,'does not intersect the ',
//     .          'boundary for L =',I4/)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(NCON);
end;

Procedure Crplot100(var Ifortran: longint; var IPX1, IPY1, IPX2, IPY2: longint); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  with GetStrings(IFortran) do
  begin
    Add('%!PS-Adobe-3.0 EPSF-3.0');
    Add('%%BoundingBox:' + IntToStr(IPX1) + ' ' + IntToStr(IPY1) + ' '
      + IntToStr(IPX2) + ' ' + IntToStr(IPY2));
    Add('%%Title:  Contour Plot');
    Add('%%Creator:  SRFPACK');
    Add('%%EndComments');
  end;
//  100 FORMAT ('%!PS-Adobe-3.0 EPSF-3.0'/
//     .        '%%BoundingBox:',4I4/
//     .        '%%Title:  Contour Plot'/
//     .        '%%Creator:  SRFPACK'/
//     .        '%%EndComments')
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(IPX1);
  Integers.Add(IPY1);
  Integers.Add(IPX2);
  Integers.Add(IPY2);
end;

Procedure Crplot110(var Ifortran: longint; var X, Y: longint); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add(IntToStr(X) + ' ' + IntToStr(Y) +' moveto');
//  110 FORMAT (2I4,' moveto')
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(X);
  Integers.Add(Y);
end;

Procedure Crplot120(var Ifortran: longint; var X, Y: longint); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add(IntToStr(X) + ' ' + IntToStr(Y) +' lineto');
//  120 FORMAT (2I4,' lineto')
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(X);
  Integers.Add(Y);
end;

Procedure Crplot130(var Ifortran: longint); stdcall;
begin
  GetStrings(IFortran).Add('closepath');
//  130 FORMAT ('closepath')
end;

Procedure Crplot140(var Ifortran: longint); stdcall;
begin
  GetStrings(IFortran).Add('stroke');
//  140 FORMAT ('stroke')
end;

Procedure Crplot150(var Ifortran: longint; var TX, TY, SFX, SFY: TFloat); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  with GetStrings(IFortran) do
  begin
    Add(FloatToStr(TX) + ' ' + FloatToStr(TY) +' translate');
    Add(FloatToStr(SFX) + ' ' + FloatToStr(SFY) +' scale');
  end;
//  150 FORMAT (2F12.6,' translate'/
//     .        2F12.6,' scale')
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(TX);
  Reals.Add(TY);
  Reals.Add(SFX);
  Reals.Add(SFY);
end;

Procedure Crplot160(var Ifortran: longint; var T: TFloat); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add(FloatToStr(T) + ' setlinewidth');
//  160 FORMAT (F12.6,' setlinewidth')
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(T);
end;

Procedure Crplot170(var Ifortran: longint; var X, Y: TFloat); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add(FloatToStr(X) + ' ' + FloatToStr(Y) +' moveto');
//  170     FORMAT (2F12.6,' moveto')
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(X);
  Reals.Add(Y);
end;

Procedure Crplot180(var Ifortran: longint; var X, Y: TFloat); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add(FloatToStr(X) + ' ' + FloatToStr(Y) +' lineto');
//  180       FORMAT (2F12.6,' lineto')
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(X);
  Reals.Add(Y);
end;

Procedure Crplot200(var Ifortran: longint); stdcall;
begin
  with GetStrings(IFortran) do
  begin
    Add('showpage');
    Add('%%EOF');
  end;
//  200 FORMAT ('showpage'/
//     .        '%%EOF')
end;

procedure Crplot210(var IFortran: longint; var AChar: Char);
begin
  GetStrings(IFortran).Add(AChar);
//  210 FORMAT (A1)
end;

procedure Getsig100P(const IFortran: longint; const N: longint; const TOL: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('GETSIG:  N = '+ IntToStr(N) +', TOL = ' + FloatToStr(TOL));
//  100 FORMAT (///13X,'GETSIG:  N =',I4,', TOL = ',E10.3//)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(N);
  Reals.Add(TOL);
end;

procedure Getsig100(var IFortran: longint; var N: longint; var TOL: TFloat);
begin
  Getsig100P(IFortran, N, TOL);
//  GetStrings(IFortran).Add('GETSIG:  N = '+ IntToStr(N) +', TOL = ' + FloatToStr(TOL));
////  100 FORMAT (///13X,'GETSIG:  N =',I4,', TOL = ',E10.3//)
//  GetLists(Ifortran, Reals, Integers);
//  Integers.Add(N);
//  Reals.Add(TOL);
end;

procedure Getsig110(var IFortran: longint; var N1, N2: longint);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('Arc '+ IntToStr(N1) +' - '+ IntToStr(N2));
//  110   FORMAT (/1X,'Arc',I4,' -',I4)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(N1);
  Integers.Add(N2);
end;

Procedure Getsig120(var Ifortran: longint; var SIG, FSIG, FPSIG: TFloat); stdcall;
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  with GetStrings(IFortran) do
  begin
    Add('Convexity:  SIG = '+ FloatToStr(SIG) +', F(SIG) = '+ FloatToStr(FSIG));
    Add('       FP(SIG) = '+ FloatToStr(FPSIG));
  end;
//  120   FORMAT (1X,'Convexity:  SIG = ',E15.8,
//     .          ', F(SIG) = ',E15.8/1X,35X,'FP(SIG) = ',
//     .          E15.8)
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(SIG);
  Reals.Add(FSIG);
  Reals.Add(FPSIG);
end;

procedure Getsig130(var IFortran: longint; var DSIG: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('Monotonicity:  DSIG = ' + FloatToStr(DSIG));
//  130   FORMAT (1X,'Monotonicity:  DSIG = ',E15.8)
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(DSIG);
end;

procedure Getsig140(var IFortran: longint; var NIT: longint; var SIG, F: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add(IntToStr(NIT) + ' -- SIG = ' + FloatToStr(SIG)
    + ', F = ' + FloatToStr(F));
//  140   FORMAT (1X,11X,I2,' -- SIG = ',E15.8,', F = ',
//     .          E15.8)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(NIT);
  Reals.Add(SIG);
  Reals.Add(F);
end;

//C Print formats:
//C
procedure Sgprnt100P(const IFortran: longint; const N: longint);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  with GetStrings(IFortran) do
  begin
    Add('Tension Factors,  N =' + IntToStr(N) + ' Nodes');
    Add('N1     N2        Tension');
  end;
//  100 FORMAT (///14X,'Tension Factors,  N =',I5,
//     .        ' Nodes'//1X,18X,'N1',5X,'N2',8X,'Tension'//)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(N);
end;

procedure Sgprnt100(var IFortran: longint; var N: longint);
begin
  Sgprnt100P(IFortran, N);
//  with GetStrings(IFortran) do
//  begin
//    Add('Tension Factors,  N =' + IntToStr(N) + ' Nodes');
//    Add('N1     N2        Tension');
//  end;
////  100 FORMAT (///14X,'Tension Factors,  N =',I5,
////     .        ' Nodes'//1X,18X,'N1',5X,'N2',8X,'Tension'//)
//  GetLists(Ifortran, Reals, Integers);
//  Integers.Add(N);
end;

procedure Sgprnt110(var IFortran: longint; var N1, N2: longint; var Tension: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add(IntToStr(N1) + '   ' + IntToStr(N2) + '     ' + FloatToStr(Tension));
//  110 FORMAT (1X,16X,I4,3X,I4,5X,F12.8)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(N1);
  Integers.Add(N2);
  Reals.Add(Tension);
end;

procedure Sgprnt120(var IFortran: longint; var N1, N2: longint; var S1, S2: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add(IntToStr(N1) + '   ' + IntToStr(N2) + '     '
  + FloatToStr(S1)+ '   ' + FloatToStr(S2));
//  120 FORMAT (1X,16X,I4,3X,I4,5X,F12.8,3X,F12.8,' *')
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(N1);
  Integers.Add(N2);
  Reals.Add(S1);
  Reals.Add(S2);
end;

procedure Sgprnt130(var IFortran: longint);
begin
  GetStrings(IFortran).Add('');
//  130 FORMAT (///)
end;

procedure Sgprnt140(var IFortran: longint; var NA: longint);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('NA ='+ IntToStr(NA) +' Arcs');
//  140 FORMAT (//1X,10X,'NA =',I5,' Arcs')
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(NA);
end;

//C
//C Error messages:
//C
procedure Sgprnt200(var IFortran: longint; var Count: longint);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('* '+ IntToStr(Count) +' Errors in SIGMA');
//  200 FORMAT (//1X,10X,'*',I5,' Errors in SIGMA')
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(Count);
end;

procedure Sgprnt210(var IFortran: longint; var Value: longint);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('*** Error in triangulation:  '
     +        '3N-NB-3 = '+ IntToStr(Value) +' ***');
//  210 FORMAT (/1X,10X,'*** Error in triangulation:  ',
//     .        '3N-NB-3 = ',I5,' ***')
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(Value);
end;

procedure Sgprnt220(var IFortran: longint; var NMAX: longint);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('*** N is outside its valid range:  '
     +        'NMAX = '+ IntToStr(NMAX) +' ***');
//  220 FORMAT (1X,10X,'*** N is outside its valid range:  ',
//     .        'NMAX = ',I4,' ***')
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(NMAX);
end;

procedure Sig0_100(var IFortran: longint; var N1, N2: longint; var BND: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('SIG0 -- N1 ='+ IntToStr(N1) +', N2 ='+ IntToStr(N2) +
             ', Lower bound = ' + FloatToStr(BND));
//  100 FORMAT (//1X,'SIG0 -- N1 =',I4,', N2 =',I4,
//     .        ', Lower bound = ',E15.8)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(N1);
  Integers.Add(N2);
  Reals.Add(BND);
end;

procedure Sig0_110(var IFortran: longint; var N1, N2: longint; var BND: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('SIG0 -- N1 ='+ IntToStr(N1) +', N2 ='+ IntToStr(N2) +
             ', Upper bound = ' + FloatToStr(BND));
//  110 FORMAT (//1X,'SIG0 -- N1 =',I4,', N2 =',I4,
//     .        ', Upper bound = ',E15.8)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(N1);
  Integers.Add(N2);
  Reals.Add(BND);
end;

procedure Sig0_120(var IFortran: longint; var SIG, SNEG, F0, FMAX: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  with GetStrings(IFortran) do
  begin
    Add('SIG = '+ FloatToStr(SIG) +', SNEG = '+ FloatToStr(SNEG));
    Add('F0 = '+ FloatToStr(F0) +', FMAX = ' + FloatToStr(FMAX));
  end;
//  120 FORMAT (1X,8X,'SIG = ',E15.8,', SNEG = ',E15.8/
//     .        1X,9X,'F0 = ',E15.8,', FMAX = ',E15.8/)
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(SIG);
  Reals.Add(SNEG);
  Reals.Add(F0);
  Reals.Add(FMAX);
end;

procedure Sig0_130(var IFortran: longint; var NIT: longint; var SIG, F: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  Getsig140(IFortran, NIT, SIG, F);
//  130 FORMAT (1X,3X,I2,' -- SIG = ',E15.8,', F = ',
//     .        E15.8)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(NIT);
  Reals.Add(SIG);
  Reals.Add(F);
end;

procedure Sig0_140(var IFortran: longint; var DSIG: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('DSIG = ' + FloatToStr(DSIG));
//  140 FORMAT (1X,8X,'DSIG = ',E15.8)
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(DSIG);
end;

procedure Sig1_100(var IFortran: longint; var N1, N2: longint; var BND: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('SIG1 -- N1 ='+ IntToStr(N1) +', N2 ='+ IntToStr(N2) +
             ', Lower bound = ' + FloatToStr(BND));
//  100 FORMAT (//1X,'SIG1 -- N1 =',I4,', N2 =',I4,
//     .        ', Lower bound = ',E15.8)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(N1);
  Integers.Add(N2);
  Reals.Add(BND);
end;

procedure Sig1_110(var IFortran: longint; var N1, N2: longint; var BND: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('SIG1 -- N1 ='+ IntToStr(N1) +', N2 ='+ IntToStr(N2) +
             ', Upper bound = ' + FloatToStr(BND));
//  110 FORMAT (//1X,'SIG1 -- N1 =',I4,', N2 =',I4,
//     .        ', Upper bound = ',E15.8)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(N1);
  Integers.Add(N2);
  Reals.Add(BND);
end;

procedure Sig1_120(var IFortran: longint; var F0, FMAX, SIG: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  with GetStrings(IFortran) do
  begin
    Add('F0 = '+ FloatToStr(F0) +', FMAX = ' + FloatToStr(FMAX));
    Add('SIG = '+ FloatToStr(SIG));
  end;
//  120 FORMAT (1X,9X,'F0 = ',E15.8,', FMAX = ',E15.8/
//     .        1X,8X,'SIG = ',E15.8/)
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(F0);
  Reals.Add(FMAX);
  Reals.Add(SIG);
end;

procedure Sig1_130(var IFortran: longint; var NIT: longint; var SIG, F: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  Getsig140(IFortran, NIT, SIG, F);
//  130 FORMAT (1X,3X,I2,' -- SIG = ',E15.8,', F = ',
//     .        E15.8)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(NIT);
  Reals.Add(SIG);
  Reals.Add(F);
end;

procedure Sig1_140(var IFortran: longint; var DSIG: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('DSIG = ' + FloatToStr(DSIG));
//  140 FORMAT (1X,8X,'DSIG = ',E15.8)
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(DSIG);
end;

procedure Sig2_100(var IFortran: longint; var N1, N2: longint);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('SIG1 -- N1 ='+ IntToStr(N1) +', N2 ='+ IntToStr(N2));
//  100 FORMAT (//1X,'SIG2 -- N1 =',I4,', N2 =',I4)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(N1);
  Integers.Add(N2);
end;

procedure Sig2_110(var IFortran: longint; var NIT: longint; var SIG, F, FP: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  with GetStrings(IFortran) do
  begin
    Add(IntToStr(NIT) +' -- SIG = '+ FloatToStr(SIG) +', F = '
     + FloatToStr(F));
    Add('FP = ' + FloatToStr(FP));
  end;
//  110 FORMAT (1X,3X,I2,' -- SIG = ',E15.8,', F = ',
//     .        E15.8/1X,31X,'FP = ',E15.8)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(NIT);
  Reals.Add(SIG);
  Reals.Add(F);
  Reals.Add(FP);
end;

procedure Smsurf100(var IFortran: longint);
begin
  GetStrings(IFortran).Add('SMSURF:  The constraint is not '
     +          'active and the surface is linear.');
//  100   FORMAT (///1X,'SMSURF:  The constraint is not ',
//     .          'active and the surface is linear.'/)
end;

procedure Smsurf110(var IFortran: longint; var SM, TOL: TFloat; var NITMAX: longint; var G0: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('SMSURF -- SM = '+ FloatToStr(SM) +', GSTOL = '
             + FloatToStr(TOL) +', NITMAX = '+ IntToStr(NITMAX) +', G(0) = '+ FloatToStr(G0));
//  110 FORMAT (///1X,'SMSURF -- SM = ',E10.4,', GSTOL = ',
//     .        E7.1,', NITMAX = ',I2,', G(0) = ',E15.8)
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(SM);
  Reals.Add(TOL);
  Integers.Add(NITMAX);
end;

procedure Smsurf120(var IFortran: longint; var ITER: longint;  var P, G: TFloat;
  var NIT: longint; var DFMAX: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add(IntToStr(ITER) +' -- P = '+ FloatToStr(P) +', G = '+ FloatToStr(G) +
             ', NIT = '+ IntToStr(NIT) +', DFMAX = '+ FloatToStr(DFMAX));
//  120 FORMAT (/1X,I2,' -- P = ',E15.8,', G = ',E15.8,
//     .        ', NIT = ',I2,', DFMAX = ',E12.6)
  GetLists(Ifortran, Reals, Integers);
  Integers.Add(ITER);
  Reals.Add(P);
  Reals.Add(G);
  Integers.Add(NIT);
  Reals.Add(DFMAX);
end;

procedure Smsurf130(var IFortran: longint; var DP: TFloat);
var
  Reals: TRealList;
  Integers: TIntegerList;
begin
  GetStrings(IFortran).Add('DP = ' + FloatToStr(DP));
//  130 FORMAT (1X,5X,'DP = ',E15.8)
  GetLists(Ifortran, Reals, Integers);
  Reals.Add(DP);
end;

initialization
{$IFNDEF FASTGEO_SINGLE_PRECISION}
  Assert(False);
{$ENDIF}

{$ENDIF}

end.
