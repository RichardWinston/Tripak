unit TripackMessages;

interface

{$IFDEF UseTripackMessages}
uses Classes, SysUtils, FastGeo;

Type
  TChar80 = array[0..79] of Char;

// Error messages
procedure Tripack205(var IFortran: longint; var N, NC: longint); stdcall;
procedure Tripack210(var IFortran: longint); stdcall;
procedure Tripack212(var IFortran: longint); stdcall;
procedure Tripack214(var IFortran: longint); stdcall;
procedure Tripack220(var IFortran: longint; var IER: longint); stdcall;
procedure Tripack230(var IFortran: longint); stdcall;
procedure Tripack240(var IFortran: longint); stdcall;
procedure Tripack250(var IFortran: longint; var IER: longint); stdcall;
procedure Tripack260(var IFortran: longint; var IER: longint); stdcall;
procedure Tripack270(var IFortran: longint; var IER: longint); stdcall;
// Informative messages
procedure Tripack400(var IFortran: longint; var N: longint); stdcall;
procedure Tripack410(var IFortran: longint; var AspectRatio: TFloat); stdcall;
procedure Tripack420(var IFortran: longint; var BoundaryNodes, Edges, Triangles: longint;
  var Area: TFloat); stdcall;
procedure Tripack430(var IFortran: longint); stdcall;
procedure Tripack440(var IFortran: longint); stdcall;
procedure Tripack450(var IFortran: longint); stdcall;
procedure Tripack460(var IFortran: longint); stdcall;
procedure Tripack470(var IFortran: longint); stdcall;
procedure Trmtst100(var IFortran: longint; var NullTriangles: longint); stdcall;
procedure Trmtst110(var IFortran: longint; var Node, Neighbors: longint); stdcall;
procedure Trmtst120(var IFortran: longint; var ListLP, LP, Neighbor: longint); stdcall;
procedure Trmtst130(var IFortran: longint; var LP, LNEW, Neighbor: longint); stdcall;
procedure Trmtst140(var IFortran: longint; var N, LNEW, NB, NT, NA: longint); stdcall;
procedure Trmtst150(var IFortran: longint; var Nodes: longint); stdcall;
procedure Delnod100(var IFortran: longint; var NIT, IER: longint); stdcall;
procedure Edge130(var IFortran: longint; var IN1, IN2: longint); stdcall;
procedure Edge140(var IFortran: longint; var NIT, IERR: longint); stdcall;
procedure Trlprt100(var IFortran: longint); stdcall;
procedure Trlprt101(var IFortran: longint); stdcall;
procedure Trlprt102(var IFortran: longint; var I1: longint; var F1, F2: TFloat); stdcall;
procedure Trlprt103(var IFortran: longint); stdcall;
procedure Trlprt104(var IFortran: longint); stdcall;
procedure Trlprt105A(var IFortran: longint; var K: longint ); stdcall;
procedure Trlprt105B(var IFortran: longint; var LTRI: longint ); stdcall;
procedure Trlprt106(var IFortran: longint); stdcall;
procedure Trlprt107(var IFortran: longint; var NB, NA, NT: longint); stdcall;
procedure Trlprt108(var IFortran: longint; var NCC: longint); stdcall;
procedure Trlprt109(var IFortran: longint; var LCT: longint); stdcall;
procedure Trlprt110(var IFortran: longint; var N, NROW, NT: longint); stdcall;
procedure Trplot100(var IFortran: longint; var IPX1, IPY1, IPX2, IPY2: longint); stdcall;
procedure Trplot110(var IFortran: longint; var Value: TFloat); stdcall;
procedure Trplot120(var IFortran: longint; var X, Y: longint); stdcall;
procedure Trplot130(var IFortran: longint; var X, Y: longint); stdcall;
procedure Trplot140(var IFortran: longint); stdcall;
procedure Trplot150(var IFortran: longint); stdcall;
procedure Trplot160(var IFortran: longint; var Value1, Value2, Value3, Value4: TFloat); stdcall;
procedure Trplot170(var IFortran: longint); stdcall;
procedure Trplot180(var IFortran: longint; var X, Y: TFloat); stdcall;
procedure Trplot190(var IFortran: longint; var X, Y: TFloat); stdcall;
procedure Trplot200(var IFortran: longint); stdcall;
procedure Trplot210(var IFortran: longint; var X1, Y1, X2, Y2: TFloat); stdcall;
procedure Trplot220(var IFortran: longint; var Value: TFloat); stdcall;
procedure Trplot230(var IFortran: longint); stdcall;
procedure Trplot240(var IFortran: longint; var Value: TFloat); stdcall;
procedure Trplot250(var IFortran: longint; var Value: longint); stdcall;
procedure Trplot260(var IFortran: longint; //var Title: TChar80;
  var Value1, Value2: TFloat); stdcall;
procedure Trplot270(var IFortran: longint{; var Title: TChar80}); stdcall;
procedure Trplot280(var IFortran: longint; var Value1, Value2: TFloat); stdcall;
procedure Trplot290(var IFortran: longint; var Value1, Value2, Value3, Value4: TFloat); stdcall;
procedure Trplot300(var IFortran: longint); stdcall;
procedure Trplot310(var IFortran: longint; var AChar: Char); stdcall;
procedure Trprnt100(var IFortran: longint; var N: longint); stdcall;
procedure Trprnt101(var IFortran: longint); stdcall;
procedure Trprnt102(var IFortran: longint); stdcall;
procedure Trprnt103A(var IFortran: longint; var Node: longint); stdcall;
procedure Trprnt103B(var IFortran: longint; var Neighbor: longint); stdcall;
procedure Trprnt104A(var IFortran: longint; var Node: longint; var X, Y: TFloat); stdcall;
procedure Trprnt104B(var IFortran: longint; var Neighbor: longint); stdcall;
procedure Trprnt105(var IFortran: longint); stdcall;
procedure Trprnt106(var IFortran: longint); stdcall;
procedure Trprnt107(var IFortran: longint; var NB, NA, NT: longint); stdcall;
procedure Trprnt108(var IFortran: longint; var NCC: longint); stdcall;
procedure Trprnt109(var IFortran: longint; var Value: longint); stdcall;
procedure Trprnt110(var IFortran: longint); stdcall;

exports Tripack205, Tripack210, Tripack212, Tripack214, Tripack220,
  Tripack230, Tripack240, Tripack250, Tripack260, Tripack270, Tripack400,
  Tripack410, Tripack420, Tripack430, Tripack440, Tripack450, Tripack460,
  Tripack470, Trmtst100, Trmtst110, Trmtst120, Trmtst130, Trmtst140,
  Trmtst150, Delnod100, Edge130, Edge140, Trlprt100, Trlprt101, Trlprt102,
  Trlprt103, Trlprt104, Trlprt105A, Trlprt105B, Trlprt106, Trlprt107,
  Trlprt108, Trlprt109, Trlprt110, Trplot100, Trplot110, Trplot120,
  Trplot130, Trplot140, Trplot150, Trplot160, Trplot170, Trplot180,
  Trplot190, Trplot200, Trplot210, Trplot220, Trplot230, Trplot240,
  Trplot250, Trplot260, Trplot270, Trplot280, Trplot290, Trplot300,
  Trplot310, Trprnt100, Trprnt101, Trprnt102, Trprnt103A, Trprnt103B,
  Trprnt104A, Trprnt104B, Trprnt105, Trprnt106, Trprnt107, Trprnt108,
  Trprnt109, Trprnt110;

procedure Edge130P(const IFortran: longint; const IN1, IN2: longint);
procedure Trlprt110P(const IFortran: longint; const N, NROW, NT: longint);
procedure Trlprt107P(const IFortran: longint; const NB, NA, NT: longint);
procedure Trlprt108P(const IFortran: longint; const NCC: longint);
procedure Trplot180P(const IFortran: longint; const X, Y: TFloat);
procedure Trplot190P(const IFortran: longint; const X, Y: TFloat);
procedure Trplot280P(const IFortran: longint; const Value1, Value2: TFloat);
procedure Trplot290P(const IFortran: longint; const Value1, Value2, Value3, Value4: TFloat);
procedure Trprnt108P(const IFortran: longint; const NCC: longint);

{$ENDIF}
implementation

{$IFDEF UseTripackMessages}
uses frmMainUnit;

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

// Error messages

procedure Tripack205(var IFortran: longint; var N, NC: longint);
begin
  GetStrings(IFortran).Add('*** N or NCC is outside its valid ' +
                  'range:  N =' + IntToStr(N) + ', NCC = ' + IntToStr(NC) + ' ***');
end;

procedure Tripack210(var IFortran: longint);
begin
  GetStrings(IFortran).Add('*** Error in TRMESH:  the first three '
     +        'nodes are collinear ***');
end;

procedure Tripack212(var IFortran: longint);
begin
  GetStrings(IFortran).Add('*** Error in TRMESH:  invalid '+
             'triangulation ***');
end;

procedure Tripack214(var IFortran: longint);
begin
  GetStrings(IFortran).Add('*** Error in TRMESH:  duplicate nodes '
     +        'encountered ***');
end;

procedure Tripack220(var IFortran: longint; var IER: longint);
begin
  GetStrings(IFortran).Add('*** Error in ADDCST:  IER = '+ IntToStr(IER)
     +        ' ***');
end;

procedure Tripack230(var IFortran: longint);
begin
  GetStrings(IFortran).Add('*** Error in GETNP ***');
end;

procedure Tripack240(var IFortran: longint);
begin
  GetStrings(IFortran).Add('*** Error in NEARND ***');
end;


procedure Tripack250(var IFortran: longint; var IER: longint);
begin
  GetStrings(IFortran).Add('*** Error in DELARC:  IER = '+ IntToStr(IER)
     +        ' ***');
end;

procedure Tripack260(var IFortran: longint; var IER: longint);
begin
  GetStrings(IFortran).Add('*** Error in DELNOD:  IER = '+ IntToStr(IER)
     +        ' ***');
end;

procedure Tripack270(var IFortran: longint; var IER: longint);
begin
  GetStrings(IFortran).Add('*** Error in TRPLOT:  IER = '+ IntToStr(IER)
     +        ' ***');
end;

// Informative messages

procedure Tripack400(var IFortran: longint; var N: longint);
begin
  GetStrings(IFortran).Add(' Test:  N ='+ IntToStr(N));
end;

procedure Tripack410(var IFortran: longint; var AspectRatio: TFloat);
begin
  GetStrings(IFortran).Add('Maximum triangle aspect ratio = '+ FloatToStr(AspectRatio));
end;

procedure Tripack420(var IFortran: longint; var BoundaryNodes, Edges, Triangles: longint;
  var Area: TFloat);
begin
  with GetStrings(IFortran) do
  begin
    Add('Output from BNODES and AREAP');
    Add('BNODES:  ' + IntToStr(BoundaryNodes) + ' boundary nodes,  '
      + IntToStr(Edges) +  ' edges,  ' + IntToStr(Triangles) + ' triangles');
    Add('AREAP:  area of convex hull = ' + FloatToStr(Area));
  end;
end;

procedure Tripack430(var IFortran: longint);
begin
  with GetStrings(IFortran) do
  begin
    Add('Subroutine EDGE not tested:');
    Add('  No edges were swapped by ADDCST');
  end;
end;

procedure Tripack440(var IFortran: longint);
begin
  with GetStrings(IFortran) do
  begin
    Add('Subroutine DELARC not tested:');
    Add('  Nodes 1 and 2 do not form a '
     +          'removable boundary edge.');
  end;
end;

procedure Tripack450(var IFortran: longint);
begin
  with GetStrings(IFortran) do
  begin
    Add('Subroutine DELNOD not tested:');
    Add('  N cannot be reduced below 3');
  end;
end;

procedure Tripack460(var IFortran: longint);
begin
  GetStrings(IFortran).Add('No triangulation errors encountered.');
end;

procedure Tripack470(var IFortran: longint);
begin
  GetStrings(IFortran).Add('A triangulation plot file was '
     +             'successfully created.');
end;

procedure Trmtst100(var IFortran: longint; var NullTriangles: longint);
begin
   with GetStrings(IFortran) do
   begin
     Add('*** TRMTST -- ' + IntToStr(NullTriangles) + ' NULL TRIANGLES '
     +        'ARE PRESENT');
     Add('(NULL TRIANGLES '
     +        'ON THE BOUNDARY ARE UNAVOIDABLE) ***');
   end;
end;

procedure Trmtst110(var IFortran: longint; var Node, Neighbors: longint);
begin
  GetStrings(IFortran).Add('*** TRMTST -- NODE '  + IntToStr(Node) +
             ' HAS ONLY '  + IntToStr(Neighbors) + ' NEIGHBORS ***');
end;

procedure Trmtst120(var IFortran: longint; var ListLP, LP, Neighbor: longint);
begin
  GetStrings(IFortran).Add('*** TRMTST -- LIST(LP) =' + IntToStr(ListLP) +
             ', FOR LP ='+ IntToStr(LP) +','+
     'IS NOT A VALID NEIGHBOR OF '+ IntToStr(Neighbor) +' ***');
end;

procedure Trmtst130(var IFortran: longint; var LP, LNEW, Neighbor: longint);
begin
  GetStrings(IFortran).Add('*** TRMTST -- LP ='+ IntToStr(LP) +' IS NOT IN THE'
            + ' RANGE 1 TO LNEW-1 FOR LNEW = ' + IntToStr(LNEW) +
            'LP POINTS TO A NEIGHBOR OF ' + IntToStr(Neighbor) +
             ' ***');
end;

procedure Trmtst140(var IFortran: longint; var N, LNEW, NB, NT, NA: longint);
begin
   with GetStrings(IFortran) do
   begin
     Add('*** TRMTST -- INCONSISTENT PARAMETERS');
     Add(' *** N = '+ IntToStr(N) +' NODES;   LNEW ='+ IntToStr(LNEW));
     Add('NB = '+ IntToStr(NB) +' BOUNDARY NODES');
     Add('NT = '+ IntToStr(NT) +' TRIANGLES');
     Add('NA = '+ IntToStr(NA) +' ARCS');
   end;
end;

procedure Trmtst150(var IFortran: longint; var Nodes: longint);
begin
  GetStrings(IFortran).Add('*** TRMTST -- '+ IntToStr(Nodes) +' CIRCUMCIRCLES '
     +        'CONTAIN NODES IN THEIR INTERIORS ***');
end;

procedure Delnod100(var IFortran: longint; var NIT, IER: longint);
begin
  GetStrings(IFortran).Add('*** Error in OPTIM:  NIT = '+ IntToStr(NIT) +
             ', IER = '+ IntToStr(IER) +' ***');
end;

procedure Edge130P(const IFortran: longint; const IN1, IN2: longint);
begin
  GetStrings(IFortran).Add('*** Error in EDGE:  Invalid triangula'+
             'tion or null triangles on boundary; '+
             'IN1 ='+ IntToStr(IN1) +', IN2='+ IntToStr(IN2) );
end;

procedure Edge130(var IFortran: longint; var IN1, IN2: longint);
begin
  Edge130P(IFortran, IN1, IN2);
//  GetStrings(IFortran).Add('*** Error in EDGE:  Invalid triangula'+
//             'tion or null triangles on boundary; '+
//             'IN1 ='+ IntToStr(IN1) +', IN2='+ IntToStr(IN2) );
end;

procedure Edge140(var IFortran: longint; var NIT, IERR: longint);
begin
  Delnod100(IFortran, NIT, IERR);
end;

procedure Trlprt100(var IFortran: longint);
begin
  GetStrings(IFortran).Add('TRIPACK (TRLIST) Output');
end;

procedure Trlprt101(var IFortran: longint);
begin
  GetStrings(IFortran).Add('Node; X(Node); Y(Node)');
end;

procedure Trlprt102(var IFortran: longint; var I1: longint; var F1, F2: TFloat);
begin
  GetStrings(IFortran).Add(IntToStr(I1) + ' '
    + FloatToStr(F1) + ' ' + FloatToStr(F2));
end;

procedure Trlprt103(var IFortran: longint);
begin
  GetStrings(IFortran).Add(
    'Triangle; Vertices; Neighbors; KT; N1; N2; N3; KT1; KT2; KT3');
end;

procedure Trlprt104(var IFortran: longint);
begin
  GetStrings(IFortran).Add(
    'Triangle; Vertices; Neighbors; Arcs; KT; N1; N2; N3; KT1; KT2; KT3; KA1; KA2; KA3');
end;

procedure Trlprt105A(var IFortran: longint; var K: longint );
begin
  GetStrings(IFortran).Add(IntToStr(K));
end;

procedure Trlprt105B(var IFortran: longint; var LTRI: longint );
begin
  with GetStrings(IFortran) do
  begin
    Strings[Count-1] := Strings[Count-1] + '   ' + IntToStr(LTRI);
  end;
end;

procedure Trlprt106(var IFortran: longint);
begin
  GetStrings(IFortran).Add('');
end;

procedure Trlprt107P(const IFortran: longint; const NB, NA, NT: longint);
begin
  GetStrings(IFortran).Add('NB = '+ IntToStr(NB)+ ' Boundary Nodes'
     +        'NA = '+ IntToStr(NA)+' Arcs     NT = '+ IntToStr(NT)
     +        ' Triangles');
end;

procedure Trlprt107(var IFortran: longint; var NB, NA, NT: longint);
begin
  Trlprt107P(IFortran,NB, NA, NT);
end;

procedure Trlprt108P(const IFortran: longint; const NCC: longint);
begin
  GetStrings(IFortran).Add('NCC ='+ IntToStr(NCC)+' Constraint Curves');
end;

procedure Trlprt108(var IFortran: longint; var NCC: longint);
begin
  Trlprt108P(IFortran, NCC);
//  GetStrings(IFortran).Add('NCC ='+ IntToStr(NCC)+' Constraint Curves');
end;

procedure Trlprt109(var IFortran: longint; var LCT: longint);
begin
  GetStrings(IFortran).Add(IntToStr(LCT));
end;

procedure Trlprt110P(const IFortran: longint; const N, NROW, NT: longint);
begin
  GetStrings(IFortran).Add('*** Invalid Parameter:  N ='+ IntToStr(N)+
             ', NROW ='+ IntToStr(NROW)+', NT ='+ IntToStr(NT)+' ***');
end;

procedure Trlprt110(var IFortran: longint; var N, NROW, NT: longint);
begin
  Trlprt110P(IFortran, N, NROW, NT);
//  GetStrings(IFortran).Add('*** Invalid Parameter:  N ='+ IntToStr(N)+
//             ', NROW ='+ IntToStr(NROW)+', NT ='+ IntToStr(NT)+' ***');
end;

procedure Trplot100(var IFortran: longint; var IPX1, IPY1, IPX2, IPY2: longint);
begin
  with GetStrings(IFortran) do
  begin
    Add('%!PS-Adobe-3.0 EPSF-3.0');
    Add('%%BoundingBox: ' + IntToStr(IPX1) + ' '
       + IntToStr(IPY1) + ' '
       + IntToStr(IPX2) + ' '
       + IntToStr(IPY2) + ' ');
    Add('%%Title:  Triangulation');
    Add('%%Creator:  TRIPACK');
    Add('%%EndComments');
  end;
end;

procedure Trplot110(var IFortran: longint; var Value: TFloat);
begin
  GetStrings(IFortran).Add(FloatToStr(Value) + ' setlinewidth');
end;

procedure Trplot120(var IFortran: longint; var X, Y: longint);
begin
  GetStrings(IFortran).Add(IntToStr(X) + ' ' + IntToStr(Y) + ' moveto');
end;

procedure Trplot130(var IFortran: longint; var X, Y: longint);
begin
  GetStrings(IFortran).Add(IntToStr(X) + ' ' + IntToStr(Y) + ' lineto');
end;

procedure Trplot140(var IFortran: longint);
begin
  GetStrings(IFortran).Add('closepath');
end;

procedure Trplot150(var IFortran: longint);
begin
  GetStrings(IFortran).Add('stroke');
end;

procedure Trplot160(var IFortran: longint; var Value1, Value2, Value3, Value4: TFloat);
begin
  with GetStrings(IFortran) do
  begin
    Add(FloatToStr(Value1) + ' ' + FloatToStr(Value2) + ' translate');
    Add(FloatToStr(Value3) + ' ' + FloatToStr(Value4) + ' scale');
  end;
end;

procedure Trplot170(var IFortran: longint);
begin
  GetStrings(IFortran).Add('gsave');
end;

procedure Trplot180P(const IFortran: longint; const X, Y: TFloat);
begin
  GetStrings(IFortran).Add(FloatToStr(X) + ' ' + FloatToStr(Y) + ' moveto');
end;

procedure Trplot180(var IFortran: longint; var X, Y: TFloat);
begin
  Trplot180P(IFortran, X, Y);
//  GetStrings(IFortran).Add(FloatToStr(X) + ' ' + FloatToStr(Y) + ' moveto');
end;

procedure Trplot190P(const IFortran: longint; const X, Y: TFloat);
begin
  GetStrings(IFortran).Add(FloatToStr(X) + ' ' + FloatToStr(Y) + ' lineto');
end;

procedure Trplot190(var IFortran: longint; var X, Y: TFloat);
begin
  Trplot190P(IFortran, X, Y);
//  GetStrings(IFortran).Add(FloatToStr(X) + ' ' + FloatToStr(Y) + ' lineto');
end;

procedure Trplot200(var IFortran: longint);
begin
  GetStrings(IFortran).Add('closepath clip newpath');
end;

procedure Trplot210(var IFortran: longint; var X1, Y1, X2, Y2: TFloat);
begin
  GetStrings(IFortran).Add(FloatToStr(X1) + ' ' + FloatToStr(Y1) + ' moveto '
    + FloatToStr(X2) + ' ' + FloatToStr(Y2) + ' lineto');
end;

procedure Trplot220(var IFortran: longint; var Value: TFloat);
begin
  GetStrings(IFortran).Add('['+ FloatToStr(Value) +'] 0 setdash');
end;

procedure Trplot230(var IFortran: longint);
begin
  GetStrings(IFortran).Add('grestore');
end;

procedure Trplot240(var IFortran: longint; var Value: TFloat);
begin
  with GetStrings(IFortran) do
  begin
    Add('/Helvetica findfont');
    Add(FloatToStr(Value) +' scalefont setfont');
  end;
end;

procedure Trplot250(var IFortran: longint; var Value: longint);
begin
  GetStrings(IFortran).Add('(' + IntToStr(Value) + ') show');
end;

procedure Trplot260(var IFortran: longint; {var Title: TChar80;}
  var Value1, Value2: TFloat);
var
  ATitle: string;
begin
  ATitle := Trim('(Triangulation created by TRITEST)                                              ');
  GetStrings(IFortran).Add(ATitle);
  GetStrings(IFortran).Add('  stringwidth pop 2 div neg ' + FloatToStr(Value1)
    + ' add '+ FloatToStr(Value2) + ' moveto');
end;

procedure Trplot270(var IFortran: longint{; var Title: TChar80});
var
  ATitle: string;
begin
  ATitle := Trim('(Triangulation created by TRITEST)                                              ');
  GetStrings(IFortran).Add(ATitle);
  GetStrings(IFortran).Add('  show');
end;

procedure Trplot280P(const IFortran: longint; const Value1, Value2: TFloat);
begin
  GetStrings(IFortran).Add('(Window:   WX1 = '+ FloatToStr(Value1)
    +',   WX2 = '+ FloatToStr(Value2) +
               ') show');
end;

procedure Trplot280(var IFortran: longint; var Value1, Value2: TFloat);
begin
  Trplot280P(IFortran, Value1, Value2);
//  GetStrings(IFortran).Add('(Window:   WX1 = '+ FloatToStr(Value1)
//    +',   WX2 = '+ FloatToStr(Value2) +
//               ') show');
end;

procedure Trplot290P(const IFortran: longint; const Value1, Value2, Value3, Value4: TFloat);
begin
  with GetStrings(IFortran) do
  begin
    Add('(Window:  ) stringwidth pop '+ FloatToStr(Value1) +' add '
      + FloatToStr(Value2) +' moveto');
    Add('( WY1 = '+ FloatToStr(Value3) +',   WY2 = '+ FloatToStr(Value4) +') show');
  end;
end;

procedure Trplot290(var IFortran: longint; var Value1, Value2, Value3, Value4: TFloat);
begin
  Trplot290P(IFortran, Value1, Value2, Value3, Value4);
//  with GetStrings(IFortran) do
//  begin
//    Add('(Window:  ) stringwidth pop '+ FloatToStr(Value1) +' add '
//      + FloatToStr(Value2) +' moveto');
//    Add('( WY1 = '+ FloatToStr(Value3) +',   WY2 = '+ FloatToStr(Value4) +') show');
//  end;
end;

procedure Trplot300(var IFortran: longint);
begin
  with GetStrings(IFortran) do
  begin
    Add('stroke');
    Add('showpage');
    Add('%%EOF');
  end;
end;

procedure Trplot310(var IFortran: longint; var AChar: Char);
begin
  GetStrings(IFortran).Add(AChar);
end;

procedure Trprnt100(var IFortran: longint; var N: longint);
begin
  GetStrings(IFortran).Add('Adjacency Sets,    N = ' +IntToStr(N));
end;

procedure Trprnt101(var IFortran: longint);
begin
  GetStrings(IFortran).Add('Node                   Neighbors of Node');
end;

procedure Trprnt102(var IFortran: longint);
begin
  GetStrings(IFortran).Add('Node     X(Node)        Y(Node)'
    + '                     Neighbors of Node');
end;

procedure Trprnt103A(var IFortran: longint; var Node: longint);
begin
  GetStrings(IFortran).Add(IntToStr(Node));
end;

procedure Trprnt103B(var IFortran: longint; var Neighbor: longint);
begin
  with GetStrings(IFortran) do
  begin
    Strings[Count-1] := Strings[Count-1] + '     ' + IntToStr(Neighbor);
  end;
end;

procedure Trprnt104A(var IFortran: longint; var Node: longint; var X, Y: TFloat);
begin
  GetStrings(IFortran).Add(IntToStr(Node) + ' ' + FloatToStr(X) + ' ' + FloatToStr(Y));
end;

procedure Trprnt104B(var IFortran: longint; var Neighbor: longint);
begin
  with GetStrings(IFortran) do
  begin
    Strings[Count-1] := Strings[Count-1] + '     ' + IntToStr(Neighbor);
  end;
//  GetStrings(IFortran).Add('                                          ' + IntToStr(Neighbor));
end;

procedure Trprnt105(var IFortran: longint);
begin
  GetStrings(IFortran).Add(' ');
end;

procedure Trprnt106(var IFortran: longint);
begin
  with GetStrings(IFortran) do
  begin
    Add('');
    Add('');
    Add('');
  end;
end;

procedure Trprnt107(var IFortran: longint; var NB, NA, NT: longint);
begin
  GetStrings(IFortran).Add('NB = ' + IntToStr(NB) + ' Boundary Nodes     '
     + 'NA = ' + IntToStr(NA) + ' Arcs     '+ 'NT = '+ IntToStr(NT) +
             ' Triangles');
end;

procedure Trprnt108P(const IFortran: longint; const NCC: longint);
begin
  GetStrings(IFortran).Add('NCC =' + IntToStr(NCC) + ' Constraint Curves');
end;

procedure Trprnt108(var IFortran: longint; var NCC: longint);
begin
  Trprnt108P(IFortran, NCC);
//  GetStrings(IFortran).Add('NCC =' + IntToStr(NCC) + ' Constraint Curves');
end;

procedure Trprnt109(var IFortran: longint; var Value: longint);
begin
  GetStrings(IFortran).Add(IntToStr(Value));
end;

procedure Trprnt110(var IFortran: longint);
begin
  GetStrings(IFortran).Add('*** N is outside its valid range ***');
end;

initialization
{$IFNDEF FASTGEO_SINGLE_PRECISION}
  Assert(False);
{$ENDIF}
{$ENDIF}


end.
