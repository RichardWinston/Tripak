unit TripackPascalDriver;

interface

uses FastGeo, TripackTypes;

procedure TripackDemoPascal;
procedure ModifiedTripackDemoPascal;

procedure TRMTST (var N: longint; var X,Y: TNmaxSingleArray;
  var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray; var LNEW: longint;
  var TOL: TFloat; var LUN:longint; var ARMAX: TFloat; var IER:longint);

implementation

uses
{$IFDEF UseTripackMessages}
  TripackMessages,
{$ENDIF}
  Math, TripackProcedures;

procedure TripackDemoPascal;
//      subroutine TripackDemo
//      dll_export TripackDemo
//      dll_import Tripack205, Tripack210, Tripack212, Tripack214,
//     1  Tripack220, Tripack230, Tripack240, Tripack250, Tripack260,
//     2  Tripack270, Tripack400, Tripack410, Tripack420, Tripack430,
//     3  Tripack440, Tripack450, Tripack460, Tripack470
//C
//C
//C        TRITEST:  Portable Test Driver for TRIPACK
//C                        07/02/98
//C
//C
//C   This driver tests software package TRIPACK for construc-
//C ting a constrained Delaunay triangulation of a set of
//C points in the plane.  All modules other than TRMSHR are
//C tested unless an error is encountered, in which case the
//C program terminates immediately.
//C
//C   By default, tests are performed on a simple data set
//C consisting of 12 nodes whose convex hull covers the unit
//C square.  The data set includes a single constraint region
//C consisting of four nodes forming a smaller square at the
//C center of the unit square.  However, by enabling the READ
//C statements below (C# in the first two columns), testing
//C may be performed on an arbitrary set of up to NMAX nodes
//C with up to NCMAX constraint curves.  (Refer to the
//C PARAMETER statements below.)  A data set consists of the
//C following sequence of records:
//C
//C    N = Number of nodes (format I4) -- 3 to NMAX.
//C    NCC = Number of constraint curves (format I4) -- 0 to
//C          NCMAX.
//C    (LCC(I), I = 1,NCC) = Indexes of the first node in each
//C             constraint curve (format I4).  1 .LE. LCC(1)
//C             and, for I .GT. 1, LCC(I-1) + 3 .LE. LCC(I)
//C             .LE. N-2. (Each constraint curve has at least
//C             three nodes.)
//C    (X(I),Y(I), I = 1,N) = Nodal coordinates with non-
//C                constraint nodes followed by the NCC
//C                sequences of constraint nodes (format
//C                2F13.8).
//C
//C   The I/O units may be changed by altering LIN (input) and
//C LOUT (output) in the DATA statement below.
//C
//C   This driver must be linked to TRIPACK.
//C
var
  TITLE: string;
  IER, IO1, IO2, K, KSUM, LIN, LNEW, LOUT, LPLT,
  LW, {LWK,} N, N0, {N6,} NA, NB, NCC, {NCMAX, NMAX,}
  NN, NROW, NT {, NTMX}, K_Temp: longint;
  NUMBR, PRNTX: longbool;
  A, ARMAX, DSQ, PLTSIZ, TOL, WX1, WX2, WY1, WY2: TFloat;
  IFORTRAN: longint;
  LCC: TNcmaxIntarray;
  LCT: TNcmaxIntarray;
  LEND: TNmaxIntArray;
  LIST: TN6IntArray;
  LPTR: TN6IntArray;
  LTRI: TNtmx_LwkIntArray;
  NODES: TLwkIntArray;
  DS: TNmaxSingleArray;
  X: TNmaxSingleArray;
  Y: TNmaxSingleArray;
  I: Integer;
  IST_Temp: Integer;

//      CHARACTER*80 TITLE
//      INTEGER IER, IO1, IO2, K, KSUM, LIN, LNEW, LOUT, LPLT,
//     .        LW, LWK, N, N0, N6, NA, NB, NCC, NCMAX, NMAX,
//     .        NN, NROW, NT, NTMX
//      INTEGER NEARND
//      LOGICAL NUMBR, PRNTX
//      REAL    A, ARMAX, DSQ, PLTSIZ, TOL, WX1, WX2, WY1, WY2
//      REAL    AREAP
//      INTEGER IFORTRAN
//C
//      PARAMETER (NMAX=100, NCMAX=5, NTMX=2*NMAX, N6=6*NMAX,
//     .           LWK=2*NMAX, NROW=9)
//C
//C Array storage:
//C
//      INTEGER LCC(NCMAX), LCT(NCMAX), LEND(NMAX), LIST(N6),
//     .        LPTR(N6), LTRI(NROW,NTMX), NODES(LWK)
//      REAL    DS(NMAX), X(NMAX), Y(NMAX)
//C
//C Tolerance for TRMTST and NEARND:  upper bound on squared
//C   distances.
//C
begin
  SetLength(LCC, NCMAX);
  SetLength(LCT, NCMAX);
  SetLength(LEND,NMAX);
  SetLength(LIST,N6);
  SetLength(LPTR,N6);
  SetLength(NODES,LWK);
  SetLength(LTRI,NTMX,TripackTypes.NROW);
  SetLength(DS,NMAX);
  SetLength(X,NMAX);
  SetLength(Y,NMAX);


{$IFDEF UseTripackMessages}
  IFORTRAN :=2;
{$ENDIF}  
  NROW:=9;
  Tol := 1e-2;
//      DATA    TOL/1.E-2/
//C
//C Plot size for the triangulation plot.
//C
  PLTSIZ := 7.5;
//      DATA    PLTSIZ/7.5/
//C
//C Default data set:
//C
  N:= 12;
  NCC := 1;
  LCC[0] := 9;
  X[0] := 0;
  X[1] := 1;
  X[2] := 0.5;
  X[3] := 0.15;
  X[4] := 0.85;
  X[5] := 0.5;
  X[6] := 0;
  X[7] := 1;
  X[8] := 0.35;
  X[9] := 0.65;
  X[10] := 0.65;
  X[11] := 0.35;
  Y[0] := 0;
  Y[1] := 0;
  Y[2] := 0.15;
  Y[3] := 0.5;
  Y[4] := 0.5;
  Y[5] := 0.85;
  Y[6] := 1;
  Y[7] := 1;
  Y[8] := 0.35;
  Y[9] := 0.35;
  Y[10] := 0.65;
  Y[11] := 0.65;
//      DATA   N/12/, NCC/1/, LCC(1)/9/
//      DATA    X(1),  X(2),  X(3),  X(4),  X(5),  X(6),  X(7)
//     .       / 0.,    1.,    .5,   .15,   .85,    .5,   0./,
//     .        X(8),  X(9), X(10), X(11), X(12)
//     .       / 1.,   .35,   .65,   .65,   .35/,
//     .        Y(1),  Y(2),  Y(3),  Y(4),  Y(5),  Y(6),  Y(7)
//     .       / 0.,    0.,   .15,    .5,    .5,   .85,   1./,
//     .        Y(8),  Y(9), Y(10), Y(11), Y(12)
//     .       / 1.,   .35,   .35,   .65,   .65/
//C
//C Logical unit numbers for I/O:
//C
  LIN := 1;
  LOUT := 2;
  LPLT := 3;
//      DATA    LIN/1/,  LOUT/2/,  LPLT/3/
//      IFORTRAN = 1
//      OPEN (LOUT,FILE='RES')
//      OPEN (LPLT,FILE='RES.eps')
//C
//C Store a plot title.  It must be enclosed in parentheses.
//C
  TITLE := '(Triangulation created by TRITEST)';
//      TITLE = '(Triangulation created by TRITEST)'
//C
//C *** Read triangulation parameters -- N, NCC, LCC, X, Y.
//C
//C#    OPEN (LIN,FILE='tritest.dat',STATUS='OLD')
//C#    READ (LIN,100,ERR=30) N, NCC
      IF (N < 3)  OR  (N > NMAX)  OR  (NCC < 0)
         OR  (NCC > NCMAX) then
      begin
{$IFDEF UseTripackMessages}
        Tripack205(IFORTRAN, N, NCC);
{$ENDIF}
        Exit;
      end;
//C#    IF (NCC .GT. 0) READ (LIN,100,ERR=30)
//C#   .                     (LCC(K), K = 1,NCC)
//C#    READ (LIN,110,ERR=30) (X(K),Y(K), K = 1,N)
//C#100 FORMAT (I4)
//C#110 FORMAT (2F13.8)
//C
//C Print a heading.
//C
//!      WRITE (LOUT,400) N
//      call Tripack400(IFORTRAN, N)
{$IFDEF UseTripackMessages}
      Tripack400(IFORTRAN, N);
{$ENDIF}
//C
//C *** Create the Delaunay triangulation (TRMESH), and test
//C     for errors (refer to TRMTST below).  NODES and DS are
//C     used as work space.
//C
      for I := 0 to Length(List) - 1 do
      begin
        List[I] := 0;
      end;
      for I := 0 to Length(LPTR) - 1 do
      begin
        LPTR[I] := 0;
      end;
      for I := 0 to Length(LEND) - 1 do
      begin
        LEND[I] := 0;
      end;
      LNEW := 0;
      IER := 0;
      TRMESH (N,X,Y, LIST,LPTR,LEND,LNEW,NODES,
                  NODES,DS,IER,N);
//      CALL TRMESH (N,X,Y, LIST,LPTR,LEND,LNEW,NODES,
//     .             NODES(N+1),DS,IER)

      IF (IER = -2) THEN
      begin
{$IFDEF UseTripackMessages}
        Tripack210(IFORTRAN);
{$ENDIF}
        Exit;
      end
      ELSE IF (IER = -4) THEN
      begin
{$IFDEF UseTripackMessages}
        Tripack212(IFORTRAN);
{$ENDIF}
        Exit;
      end
      ELSE IF (IER > 0) THEN
      begin
{$IFDEF UseTripackMessages}
        Tripack214(IFORTRAN);
{$ENDIF}
        Exit;
      end;

//      IF (IER .EQ. -2) THEN
//!        WRITE (LOUT,210)
//        CALL Tripack210(IFORTRAN)
//        return
//      ELSEIF (IER .EQ. -4) THEN
//        CALL Tripack212(IFORTRAN)
//!        WRITE (LOUT,212)
//        return
//      ELSEIF (IER .GT. 0) THEN
//        CALL Tripack214(IFORTRAN)
//!        WRITE (LOUT,214)
//        return
//      ENDIF
      TRMTST(N,X,Y,LIST,LPTR,LEND,LNEW,TOL,LOUT, ARMAX,IER);
//      CALL TRMTST (N,X,Y,LIST,LPTR,LEND,LNEW,TOL,
//     .             LOUT, ARMAX,IER)
{$IFDEF UseTripackMessages}
      Tripack410(IFORTRAN, ARMAX);
{$ENDIF}
//      CALL Tripack410(IFORTRAN, ARMAX)
//!      WRITE (LOUT,410) ARMAX
      IF (IER > 0) then Exit;
//      IF (IER .GT. 0) return
//C
//C *** Add the constraint curves (ADDCST).  Note that edges
//C     and triangles are not removed from constraint regions.
//C     ADDCST forces the inclusion of triangulation edges
//C     connecting the sequences of constraint nodes.  If it
//C     is necessary to alter the triangulation, the empty
//C     circumcircle property is no longer satisfied.
//C
      LW := LWK;
      ADDCST (NCC,LCC,N,X,Y, LW,NODES,LIST,LPTR, LEND, IER);
//      LW = LWK
//      CALL ADDCST (NCC,LCC,N,X,Y, LW,NODES,LIST,LPTR,
//     .             LEND, IER)
      IF (IER <> 0) THEN
      begin
{$IFDEF UseTripackMessages}
        Tripack220(IFORTRAN, IER);
{$ENDIF}
        Exit;
      END;
//      IF (IER .NE. 0) THEN
//        CALL Tripack220(IFORTRAN, IER)
//!        WRITE (LOUT,220) IER
//        return
//      ENDIF
//!      IF (LW .EQ. 0) WRITE (LOUT,430)
{$IFDEF UseTripackMessages}
      IF (LW = 0) then Tripack430(IFORTRAN);
{$ENDIF}
//      IF (LW .EQ. 0) CALL Tripack430(IFORTRAN)
//C
//C *** Test TRPRNT, TRLIST, and TRLPRT, and TRPLOT.
//C
      PRNTX := TRUE;
      TRPRNT (NCC,LCC,N,X,Y,LIST,LPTR,LEND,LOUT,PRNTX);
//      PRNTX = .TRUE.
//      CALL TRPRNT (NCC,LCC,N,X,Y,LIST,LPTR,LEND,LOUT,PRNTX)
      TRLIST (NCC,LCC,N,LIST,LPTR,LEND,NROW, NT,LTRI, LCT,IER);
//      CALL TRLIST (NCC,LCC,N,LIST,LPTR,LEND,NROW, NT,LTRI,
//     .             LCT,IER)
      TRLPRT (NCC,LCT,N,X,Y,NROW,NT,LTRI,LOUT,PRNTX);
//      CALL TRLPRT (NCC,LCT,N,X,Y,NROW,NT,LTRI,LOUT,PRNTX)
//C
//C   Set the plot window [WX1,WX2] X [WY1,WY2] to the
//C     smallest rectangle that contains the nodes.
//C     NUMBR = TRUE iff nodal indexes are to be displayed.
//C
      WX1 := X[0];
      WX2 := WX1;
      WY1 := Y[0];
      WY2 := WY1;
//      WX1 = X(1)
//      WX2 = WX1
//      WY1 = Y(1)
//      WY2 = WY1
      for K := 1 to N - 1 do
      begin
        IF (X[K] < WX1) then WX1 := X[K];
        IF (X[K] > WX2) then WX2 := X[K];
        IF (Y[K] < WY1) then WY1 := Y[K];
        IF (Y[K] > WY2) then WY2 := Y[K];
      end;
//      DO 1 K = 2,N
//        IF (X(K) .LT. WX1) WX1 = X(K)
//        IF (X(K) .GT. WX2) WX2 = X(K)
//        IF (Y(K) .LT. WY1) WY1 = Y(K)
//        IF (Y(K) .GT. WY2) WY2 = Y(K)
//    1   CONTINUE
      NUMBR := N <= 200;
      TRPLOT (LPLT,PLTSIZ,WX1,WX2,WY1,WY2,NCC,LCC,N,
                  X,Y,LIST,LPTR,LEND,NUMBR, IER);
//      NUMBR = N .LE. 200
//      CALL TRPLOT (LPLT,PLTSIZ,WX1,WX2,WY1,WY2,NCC,LCC,N,
//     .             X,Y,LIST,LPTR,LEND,TITLE,NUMBR, IER)
{$IFDEF UseTripackMessages}
      IF (IER = 0) THEN
      begin
          Tripack470(IFORTRAN);
      end
      ELSE
      begin
        Tripack270(IFORTRAN, IER);
      end;
{$ENDIF}
//      IF (IER .EQ. 0) THEN
//          CALL Tripack470(IFORTRAN)
// !       WRITE (LOUT,470)
//      ELSE
//        CALL Tripack270(IFORTRAN, IER)
//!      WRITE (LOUT,270) IER
//      ENDIF
//C
//C *** Test BNODES and AREAP.
//C
      BNODES (N,LIST,LPTR,LEND, NODES,NB,NA,NT);
//      CALL BNODES (N,LIST,LPTR,LEND, NODES,NB,NA,NT)
{$IFDEF UseTripackMessages}
      A := AREAP(X,Y,NB,NODES);
//      A = AREAP(X,Y,NB,NODES)
      Tripack420(IFORTRAN, NB, NA, NT, A);
{$ENDIF}
//      CALL Tripack420(IFORTRAN, NB, NA, NT, A)
//!      WRITE (LOUT,420) NB, NA, NT, A
//C
//C *** Test GETNP by ordering the nodes on distance from N0
//C                and verifying the ordering.
//C

      N0 := N div 2;
      NODES[0] := N0;
      DS[0] := 0.0;
      KSUM := N0;
      for K := 2 to N do
      begin
        K_Temp := K;
        GETNP (NCC,LCC,N,X,Y,LIST,LPTR,LEND, K_Temp, NODES,DS, IER);
        IF (IER <> 0)  OR  (DS[K-1] < DS[K-2]) THEN
        begin
{$IFDEF UseTripackMessages}
          Tripack230(IFORTRAN);
{$ENDIF}
          Exit;
        END;
        KSUM := KSUM + NODES[K-1];
      end;

//      N0 = N/2
//      NODES(1) = N0
//      DS(1) = 0.
//      KSUM = N0
//      DO 2 K = 2,N
//        CALL GETNP (NCC,LCC,N,X,Y,LIST,LPTR,LEND,
//     .              K, NODES,DS, IER)
//        IF (IER .NE. 0  .OR.  DS(K) .LT. DS(K-1)) THEN
//          CALL Tripack230(IFORTRAN)
//!          WRITE (LOUT,230)
//          return
//        ENDIF
//        KSUM = KSUM + NODES(K)
//    2   CONTINUE
//C
//C   Test for all nodal indexes included in NODES.
//C
      IF (KSUM <> (N*(N+1)) div 2) THEN
      begin
{$IFDEF UseTripackMessages}
        Tripack230(IFORTRAN);
{$ENDIF}
        Exit;
      END;
//      IF (KSUM .NE. (N*(N+1))/2) THEN
//        CALL Tripack230(IFORTRAN)
//!        WRITE (LOUT,230)
//        return
//      ENDIF
//C
//C *** Test NEARND by verifying that the nearest node to K is
//C                 node K for K = 1 to N.
//C
      for K := 1 to N do
      begin
        IST_Temp := 1;
        N0 := NEARND (X[K-1],Y[K-1],IST_Temp,N,X,Y,LIST,LPTR,LEND, DSQ);
        IF (N0 <> K)  OR  (DSQ > TOL) THEN
        begin
{$IFDEF UseTripackMessages}
          Tripack240(IFORTRAN);
{$ENDIF}
          Exit;
        END;

      end;
//      DO 3 K = 1,N
//        N0 = NEARND (X(K),Y(K),1,N,X,Y,LIST,LPTR,LEND, DSQ)
//        IF (N0 .NE. K  .OR.  DSQ .GT. TOL) THEN
//        CALL Tripack240(IFORTRAN)
//!!         WRITE (LOUT,240)
//          return
//        ENDIF
//    3 CONTINUE
//C
//C *** Test DELARC by removing a boundary arc if possible.
//C                 The first two nodes define a boundary arc
//C                 in the default data set.
//C
      IO1 := 1;
      IO2 := 2;
      DELARC (N,IO1,IO2, LIST,LPTR,LEND,LNEW, IER);
      IF (IER = 1)  OR  (IER = 4) THEN
      begin
{$IFDEF UseTripackMessages}
        Tripack250(IFORTRAN, IER);
{$ENDIF}
        Exit;
      END;
{$IFDEF UseTripackMessages}
      IF (IER <> 0) then Tripack440(IFORTRAN);
{$ENDIF}
//      IO1 = 1
//      IO2 = 2
//      CALL DELARC (N,IO1,IO2, LIST,LPTR,LEND,LNEW, IER)
//      IF (IER .EQ. 1  .OR.  IER .EQ. 4) THEN
//        CALL Tripack250(IFORTRAN, IER)
//!       WRITE (LOUT,250) IER
//        return
//      ENDIF
//!      IF (IER .NE. 0) WRITE (LOUT,440)
//      IF (IER .NE. 0) CALL Tripack440(IFORTRAN)
//CC
//C   Recreate the triangulation without constraints.
//C
      TRMESH (N,X,Y, LIST,LPTR,LEND,LNEW,NODES, NODES,DS,IER,N);
      NCC := 0;
//      CALL TRMESH (N,X,Y, LIST,LPTR,LEND,LNEW,NODES,
//     .             NODES(N+1),DS,IER)
//      NCC = 0
//C
//C *** Test DELNOD by removing nodes 4 to N (in reverse
//C                 order).
//C

      IF (N <= 3) THEN
      begin
{$IFDEF UseTripackMessages}
         Tripack450(IFORTRAN);
{$ENDIF}
      end
      ELSE
      begin
        NN := N;
        repeat
          LW := LWK div 2;
          DELNOD (NN,NCC, LCC,NN,X,Y,LIST,LPTR,LEND, LNEW,LW,NODES, IER);
          IF (IER <> 0) THEN
          begin
{$IFDEF UseTripackMessages}
            Tripack260(IFORTRAN, IER);
{$ENDIF}
            Exit;
          END;
        until NN <= 3;
      END;

//      IF (N .LE. 3) THEN
//        CALL Tripack450(IFORTRAN)
//!        WRITE (LOUT,450)
//      ELSE
//        NN = N
//    4   LW = LWK/2
//          CALL DELNOD (NN,NCC, LCC,NN,X,Y,LIST,LPTR,LEND,
//     .                 LNEW,LW,NODES, IER)
//          IF (IER .NE. 0) THEN
//            CALL Tripack260(IFORTRAN, IER)
//!            WRITE (LOUT,260) IER
//            return
//          ENDIF
//          IF (NN .GT. 3) GO TO 4
//      ENDIF
//C
//C Successful test.
{$IFDEF UseTripackMessages}
      Tripack460(IFORTRAN);
{$ENDIF}
//      CALL Tripack460(IFORTRAN)
//!      WRITE (LOUT,460)
//      return
//C
//C Error reading the data set.
//C
//C# 30 WRITE (*,200)
//!      STOP
//C
//C Invalid value of N or NCC.
//C
//!   31 WRITE (*,205) N, NCC
//   31 CALL Tripack205(IFORTRAN, N, NCC)
//      return
//C
//C Error message formats:
//C
//C#200 FORMAT (//5X,'*** Input data set invalid ***'/)
//!  205 FORMAT (//5X,'*** N or NCC is outside its valid ',
//!     .             'range:  N =',I5,', NCC = ',I4,' ***'/)
//!  210 FORMAT (//5X,'*** Error in TRMESH:  the first three ',
//!     .        'nodes are collinear ***'/)
//!  212 FORMAT (//5X,'*** Error in TRMESH:  invalid ',
//!     .        'triangulation ***'/)
//!  214 FORMAT (//5X,'*** Error in TRMESH:  duplicate nodes ',
//!     .        'encountered ***'/)
//!  220 FORMAT (//5X,'*** Error in ADDCST:  IER = ',I1,
//!     .        ' ***'/)
//!  230 FORMAT (//5X,'*** Error in GETNP ***'/)
//!  240 FORMAT (//5X,'*** Error in NEARND ***'/)
//!  250 FORMAT (//5X,'*** Error in DELARC:  IER = ',I1,
//!     .        ' ***'/)
//!  260 FORMAT (//5X,'*** Error in DELNOD:  IER = ',I1,
//!     .        ' ***'/)
//!  270 FORMAT (//5X,'*** Error in TRPLOT:  IER = ',I1,
//!     .        ' ***'/)
//C
//C Informative message formats:
//C
//!  400 FORMAT (///1X,21X,'TRIPACK Test:  N =',I5///)
//!  410 FORMAT (5X,'Maximum triangle aspect ratio = ',E10.3//)
//!  420 FORMAT (/5X,'Output from BNODES and AREAP'//
//!     .        5X,'BNODES:  ',I4,' boundary nodes,  ',I5,
//!     .        ' edges,  ',I5,' triangles'/5X,
//!     .        'AREAP:  area of convex hull = ',E15.8//)
//!  430 FORMAT (5X,'Subroutine EDGE not tested:'/
//!     .        5X,'  No edges were swapped by ADDCST'//)
//! 440 FORMAT (5X,'Subroutine DELARC not tested:'/
//!     .        5X,'  Nodes 1 and 2 do not form a ',
//!     .           'removable boundary edge.'//)
//!  450 FORMAT (5X,'Subroutine DELNOD not tested:'/
//!     .        5X,'  N cannot be reduced below 3'//)
//!  460 FORMAT (5X,'No triangulation errors encountered.'/)
//!  470 FORMAT (/5X,'A triangulation plot file was ',
//!     .             'successfully created.'/)
//      END
end;

procedure ModifiedTripackDemoPascal;
//      subroutine TripackDemo
//      dll_export TripackDemo
//      dll_import Tripack205, Tripack210, Tripack212, Tripack214,
//     1  Tripack220, Tripack230, Tripack240, Tripack250, Tripack260,
//     2  Tripack270, Tripack400, Tripack410, Tripack420, Tripack430,
//     3  Tripack440, Tripack450, Tripack460, Tripack470
//C
//C
//C        TRITEST:  Portable Test Driver for TRIPACK
//C                        07/02/98
//C
//C
//C   This driver tests software package TRIPACK for construc-
//C ting a constrained Delaunay triangulation of a set of
//C points in the plane.  All modules other than TRMSHR are
//C tested unless an error is encountered, in which case the
//C program terminates immediately.
//C
//C   By default, tests are performed on a simple data set
//C consisting of 12 nodes whose convex hull covers the unit
//C square.  The data set includes a single constraint region
//C consisting of four nodes forming a smaller square at the
//C center of the unit square.  However, by enabling the READ
//C statements below (C# in the first two columns), testing
//C may be performed on an arbitrary set of up to NMAX nodes
//C with up to NCMAX constraint curves.  (Refer to the
//C PARAMETER statements below.)  A data set consists of the
//C following sequence of records:
//C
//C    N = Number of nodes (format I4) -- 3 to NMAX.
//C    NCC = Number of constraint curves (format I4) -- 0 to
//C          NCMAX.
//C    (LCC(I), I = 1,NCC) = Indexes of the first node in each
//C             constraint curve (format I4).  1 .LE. LCC(1)
//C             and, for I .GT. 1, LCC(I-1) + 3 .LE. LCC(I)
//C             .LE. N-2. (Each constraint curve has at least
//C             three nodes.)
//C    (X(I),Y(I), I = 1,N) = Nodal coordinates with non-
//C                constraint nodes followed by the NCC
//C                sequences of constraint nodes (format
//C                2F13.8).
//C
//C   The I/O units may be changed by altering LIN (input) and
//C LOUT (output) in the DATA statement below.
//C
//C   This driver must be linked to TRIPACK.
//C
var
  TITLE: string;
  IER, IO1, IO2, K, KSUM, LIN, LNEW, LOUT, LPLT,
  LW, {LWK,} N, N0, {N6,} NA, NB, NCC, {NCMAX, NMAX,}
  NN, NROW, NT {, NTMX}, K_Temp: longint;
  NUMBR, PRNTX: longbool;
  A, ARMAX, DSQ, PLTSIZ, TOL, WX1, WX2, WY1, WY2: TFloat;
  IFORTRAN: longint;
  LCC: TNcmaxIntarray;
  LCT: TNcmaxIntarray;
  LEND: TNmaxIntArray;
  LIST: TN6IntArray;
  LPTR: TN6IntArray;
  LTRI: TNtmx_LwkIntArray;
  NODES: TLwkIntArray;
  DS: TNmaxSingleArray;
  X: TNmaxSingleArray;
  Y: TNmaxSingleArray;
  I: Integer;
  IST_Temp: Integer;

//      CHARACTER*80 TITLE
//      INTEGER IER, IO1, IO2, K, KSUM, LIN, LNEW, LOUT, LPLT,
//     .        LW, LWK, N, N0, N6, NA, NB, NCC, NCMAX, NMAX,
//     .        NN, NROW, NT, NTMX
//      INTEGER NEARND
//      LOGICAL NUMBR, PRNTX
//      REAL    A, ARMAX, DSQ, PLTSIZ, TOL, WX1, WX2, WY1, WY2
//      REAL    AREAP
//      INTEGER IFORTRAN
//C
//      PARAMETER (NMAX=100, NCMAX=5, NTMX=2*NMAX, N6=6*NMAX,
//     .           LWK=2*NMAX, NROW=9)
//C
//C Array storage:
//C
//      INTEGER LCC(NCMAX), LCT(NCMAX), LEND(NMAX), LIST(N6),
//     .        LPTR(N6), LTRI(NROW,NTMX), NODES(LWK)
//      REAL    DS(NMAX), X(NMAX), Y(NMAX)
//C
//C Tolerance for TRMTST and NEARND:  upper bound on squared
//C   distances.
//C
begin
  SetLength(LCC, NCMAX);
  SetLength(LCT, NCMAX);
  SetLength(LEND,NMAX);
  SetLength(LIST,N6);
  SetLength(LPTR,N6);
  SetLength(NODES,LWK);
  SetLength(LTRI,NTMX,TripackTypes.NROW);
  SetLength(DS,NMAX);
  SetLength(X,NMAX);
  SetLength(Y,NMAX);


{$IFDEF UseTripackMessages}
  IFORTRAN :=2;
{$ENDIF}
  NROW:=9;
  Tol := 1e-2;
//      DATA    TOL/1.E-2/
//C
//C Plot size for the triangulation plot.
//C
  PLTSIZ := 7.5;
//      DATA    PLTSIZ/7.5/
//C
//C Default data set:
//C
  N:= 12;
  NCC := 1;
  LCC[0] := 9;
  X[0] := 0;
  X[1] := 1;
  X[2] := 0.5;
  X[3] := 0.15;
  X[4] := 0.85;
  X[5] := 0.5;
  X[6] := 0;
  X[7] := 1;
  X[8] := 0.05;
  X[9] := 0.65;
  X[10] := 0.65;
  X[11] := 0.35;
  Y[0] := 0;
  Y[1] := 0;
  Y[2] := 0.15;
  Y[3] := 0.5;
  Y[4] := 0.5;
  Y[5] := 0.85;
  Y[6] := 1;
  Y[7] := 1;
  Y[8] := 0.05;
  Y[9] := 0.35;
  Y[10] := 0.65;
  Y[11] := 0.65;
//      DATA   N/12/, NCC/1/, LCC(1)/9/
//      DATA    X(1),  X(2),  X(3),  X(4),  X(5),  X(6),  X(7)
//     .       / 0.,    1.,    .5,   .15,   .85,    .5,   0./,
//     .        X(8),  X(9), X(10), X(11), X(12)
//     .       / 1.,   .35,   .65,   .65,   .35/,
//     .        Y(1),  Y(2),  Y(3),  Y(4),  Y(5),  Y(6),  Y(7)
//     .       / 0.,    0.,   .15,    .5,    .5,   .85,   1./,
//     .        Y(8),  Y(9), Y(10), Y(11), Y(12)
//     .       / 1.,   .35,   .35,   .65,   .65/
//C
//C Logical unit numbers for I/O:
//C
  LIN := 1;
  LOUT := 2;
  LPLT := 3;
//      DATA    LIN/1/,  LOUT/2/,  LPLT/3/
//      IFORTRAN = 1
//      OPEN (LOUT,FILE='RES')
//      OPEN (LPLT,FILE='RES.eps')
//C
//C Store a plot title.  It must be enclosed in parentheses.
//C
  TITLE := '(Triangulation created by TRITEST)';
//      TITLE = '(Triangulation created by TRITEST)'
//C
//C *** Read triangulation parameters -- N, NCC, LCC, X, Y.
//C
//C#    OPEN (LIN,FILE='tritest.dat',STATUS='OLD')
//C#    READ (LIN,100,ERR=30) N, NCC
      IF (N < 3)  OR  (N > NMAX)  OR  (NCC < 0)
         OR  (NCC > NCMAX) then
      begin
{$IFDEF UseTripackMessages}
        Tripack205(IFORTRAN, N, NCC);
{$ENDIF}
        Exit;
      end;
//C#    IF (NCC .GT. 0) READ (LIN,100,ERR=30)
//C#   .                     (LCC(K), K = 1,NCC)
//C#    READ (LIN,110,ERR=30) (X(K),Y(K), K = 1,N)
//C#100 FORMAT (I4)
//C#110 FORMAT (2F13.8)
//C
//C Print a heading.
//C
//!      WRITE (LOUT,400) N
//      call Tripack400(IFORTRAN, N)
{$IFDEF UseTripackMessages}
      Tripack400(IFORTRAN, N);
{$ENDIF}
//C
//C *** Create the Delaunay triangulation (TRMESH), and test
//C     for errors (refer to TRMTST below).  NODES and DS are
//C     used as work space.
//C
      for I := 0 to Length(List) - 1 do
      begin
        List[I] := 0;
      end;
      for I := 0 to Length(LPTR) - 1 do
      begin
        LPTR[I] := 0;
      end;
      for I := 0 to Length(LEND) - 1 do
      begin
        LEND[I] := 0;
      end;
      LNEW := 0;
      IER := 0;
      TRMESH (N,X,Y, LIST,LPTR,LEND,LNEW,NODES,
                  NODES,DS,IER,N);
//      CALL TRMESH (N,X,Y, LIST,LPTR,LEND,LNEW,NODES,
//     .             NODES(N+1),DS,IER)

      IF (IER = -2) THEN
      begin
{$IFDEF UseTripackMessages}
        Tripack210(IFORTRAN);
{$ENDIF}
        Exit;
      end
      ELSE IF (IER = -4) THEN
      begin
{$IFDEF UseTripackMessages}
        Tripack212(IFORTRAN);
{$ENDIF}
        Exit;
      end
      ELSE IF (IER > 0) THEN
      begin
{$IFDEF UseTripackMessages}
        Tripack214(IFORTRAN);
{$ENDIF}
        Exit;
      end;

//      IF (IER .EQ. -2) THEN
//!        WRITE (LOUT,210)
//        CALL Tripack210(IFORTRAN)
//        return
//      ELSEIF (IER .EQ. -4) THEN
//        CALL Tripack212(IFORTRAN)
//!        WRITE (LOUT,212)
//        return
//      ELSEIF (IER .GT. 0) THEN
//        CALL Tripack214(IFORTRAN)
//!        WRITE (LOUT,214)
//        return
//      ENDIF
      TRMTST(N,X,Y,LIST,LPTR,LEND,LNEW,TOL,LOUT, ARMAX,IER);
//      CALL TRMTST (N,X,Y,LIST,LPTR,LEND,LNEW,TOL,
//     .             LOUT, ARMAX,IER)

      PRNTX := TRUE;
      TRPRNT (NCC,LCC,N,X,Y,LIST,LPTR,LEND,LOUT,PRNTX);
//      PRNTX = .TRUE.
//      CALL TRPRNT (NCC,LCC,N,X,Y,LIST,LPTR,LEND,LOUT,PRNTX)
      TRLIST (NCC,LCC,N,LIST,LPTR,LEND,NROW, NT,LTRI, LCT,IER);
//      CALL TRLIST (NCC,LCC,N,LIST,LPTR,LEND,NROW, NT,LTRI,
//     .             LCT,IER)
      TRLPRT (NCC,LCT,N,X,Y,NROW,NT,LTRI,LOUT,PRNTX);

{$IFDEF UseTripackMessages}
      Tripack410(IFORTRAN, ARMAX);
{$ENDIF}
//      CALL Tripack410(IFORTRAN, ARMAX)
//!      WRITE (LOUT,410) ARMAX
      IF (IER > 0) then Exit;
//      IF (IER .GT. 0) return
//C
//C *** Add the constraint curves (ADDCST).  Note that edges
//C     and triangles are not removed from constraint regions.
//C     ADDCST forces the inclusion of triangulation edges
//C     connecting the sequences of constraint nodes.  If it
//C     is necessary to alter the triangulation, the empty
//C     circumcircle property is no longer satisfied.
//C
      LW := LWK;
      ADDCST (NCC,LCC,N,X,Y, LW,NODES,LIST,LPTR, LEND, IER);
//      LW = LWK
//      CALL ADDCST (NCC,LCC,N,X,Y, LW,NODES,LIST,LPTR,
//     .             LEND, IER)
      IF (IER <> 0) THEN
      begin
{$IFDEF UseTripackMessages}
        Tripack220(IFORTRAN, IER);
{$ENDIF}
        Exit;
      END;
//      IF (IER .NE. 0) THEN
//        CALL Tripack220(IFORTRAN, IER)
//!        WRITE (LOUT,220) IER
//        return
//      ENDIF
//!      IF (LW .EQ. 0) WRITE (LOUT,430)
{$IFDEF UseTripackMessages}
      IF (LW = 0) then Tripack430(IFORTRAN);
{$ENDIF}
//      IF (LW .EQ. 0) CALL Tripack430(IFORTRAN)
//C
//C *** Test TRPRNT, TRLIST, and TRLPRT, and TRPLOT.
//C
      PRNTX := TRUE;
      TRPRNT (NCC,LCC,N,X,Y,LIST,LPTR,LEND,LOUT,PRNTX);
//      PRNTX = .TRUE.
//      CALL TRPRNT (NCC,LCC,N,X,Y,LIST,LPTR,LEND,LOUT,PRNTX)
      TRLIST (NCC,LCC,N,LIST,LPTR,LEND,NROW, NT,LTRI, LCT,IER);
//      CALL TRLIST (NCC,LCC,N,LIST,LPTR,LEND,NROW, NT,LTRI,
//     .             LCT,IER)
      TRLPRT (NCC,LCT,N,X,Y,NROW,NT,LTRI,LOUT,PRNTX);
//      CALL TRLPRT (NCC,LCT,N,X,Y,NROW,NT,LTRI,LOUT,PRNTX)
//C
//C   Set the plot window [WX1,WX2] X [WY1,WY2] to the
//C     smallest rectangle that contains the nodes.
//C     NUMBR = TRUE iff nodal indexes are to be displayed.
//C
      WX1 := X[0];
      WX2 := WX1;
      WY1 := Y[0];
      WY2 := WY1;
//      WX1 = X(1)
//      WX2 = WX1
//      WY1 = Y(1)
//      WY2 = WY1
      for K := 1 to N - 1 do
      begin
        IF (X[K] < WX1) then WX1 := X[K];
        IF (X[K] > WX2) then WX2 := X[K];
        IF (Y[K] < WY1) then WY1 := Y[K];
        IF (Y[K] > WY2) then WY2 := Y[K];
      end;
//      DO 1 K = 2,N
//        IF (X(K) .LT. WX1) WX1 = X(K)
//        IF (X(K) .GT. WX2) WX2 = X(K)
//        IF (Y(K) .LT. WY1) WY1 = Y(K)
//        IF (Y(K) .GT. WY2) WY2 = Y(K)
//    1   CONTINUE
      NUMBR := N <= 200;
      TRPLOT (LPLT,PLTSIZ,WX1,WX2,WY1,WY2,NCC,LCC,N,
                  X,Y,LIST,LPTR,LEND,NUMBR, IER);
//      NUMBR = N .LE. 200
//      CALL TRPLOT (LPLT,PLTSIZ,WX1,WX2,WY1,WY2,NCC,LCC,N,
//     .             X,Y,LIST,LPTR,LEND,TITLE,NUMBR, IER)
{$IFDEF UseTripackMessages}
      IF (IER = 0) THEN
      begin
          Tripack470(IFORTRAN);
      end
      ELSE
      begin
        Tripack270(IFORTRAN, IER);
      end;
{$ENDIF}
//      IF (IER .EQ. 0) THEN
//          CALL Tripack470(IFORTRAN)
// !       WRITE (LOUT,470)
//      ELSE
//        CALL Tripack270(IFORTRAN, IER)
//!      WRITE (LOUT,270) IER
//      ENDIF
//C
//C *** Test BNODES and AREAP.
//C
      BNODES (N,LIST,LPTR,LEND, NODES,NB,NA,NT);
//      CALL BNODES (N,LIST,LPTR,LEND, NODES,NB,NA,NT)
{$IFDEF UseTripackMessages}
      A := AREAP(X,Y,NB,NODES);
//      A = AREAP(X,Y,NB,NODES)
      Tripack420(IFORTRAN, NB, NA, NT, A);
{$ENDIF}
//      CALL Tripack420(IFORTRAN, NB, NA, NT, A)
//!      WRITE (LOUT,420) NB, NA, NT, A
//C
//C *** Test GETNP by ordering the nodes on distance from N0
//C                and verifying the ordering.
//C

      N0 := N div 2;
      NODES[0] := N0;
      DS[0] := 0.0;
      KSUM := N0;
      for K := 2 to N do
      begin
        K_Temp := K;
        GETNP (NCC,LCC,N,X,Y,LIST,LPTR,LEND, K_Temp, NODES,DS, IER);
        IF (IER <> 0)  OR  (DS[K-1] < DS[K-2]) THEN
        begin
{$IFDEF UseTripackMessages}
          Tripack230(IFORTRAN);
{$ENDIF}
          Exit;
        END;
        KSUM := KSUM + NODES[K-1];
      end;

//      N0 = N/2
//      NODES(1) = N0
//      DS(1) = 0.
//      KSUM = N0
//      DO 2 K = 2,N
//        CALL GETNP (NCC,LCC,N,X,Y,LIST,LPTR,LEND,
//     .              K, NODES,DS, IER)
//        IF (IER .NE. 0  .OR.  DS(K) .LT. DS(K-1)) THEN
//          CALL Tripack230(IFORTRAN)
//!          WRITE (LOUT,230)
//          return
//        ENDIF
//        KSUM = KSUM + NODES(K)
//    2   CONTINUE
//C
//C   Test for all nodal indexes included in NODES.
//C
      IF (KSUM <> (N*(N+1)) div 2) THEN
      begin
{$IFDEF UseTripackMessages}
        Tripack230(IFORTRAN);
{$ENDIF}
        Exit;
      END;
//      IF (KSUM .NE. (N*(N+1))/2) THEN
//        CALL Tripack230(IFORTRAN)
//!        WRITE (LOUT,230)
//        return
//      ENDIF
//C
//C *** Test NEARND by verifying that the nearest node to K is
//C                 node K for K = 1 to N.
//C
      for K := 1 to N do
      begin
        IST_Temp := 1;
        N0 := NEARND (X[K-1],Y[K-1],IST_Temp,N,X,Y,LIST,LPTR,LEND, DSQ);
        IF (N0 <> K)  OR  (DSQ > TOL) THEN
        begin
{$IFDEF UseTripackMessages}
          Tripack240(IFORTRAN);
{$ENDIF}
          Exit;
        END;

      end;
//      DO 3 K = 1,N
//        N0 = NEARND (X(K),Y(K),1,N,X,Y,LIST,LPTR,LEND, DSQ)
//        IF (N0 .NE. K  .OR.  DSQ .GT. TOL) THEN
//        CALL Tripack240(IFORTRAN)
//!!         WRITE (LOUT,240)
//          return
//        ENDIF
//    3 CONTINUE
//C
//C *** Test DELARC by removing a boundary arc if possible.
//C                 The first two nodes define a boundary arc
//C                 in the default data set.
//C
      IO1 := 1;
      IO2 := 2;
      DELARC (N,IO1,IO2, LIST,LPTR,LEND,LNEW, IER);
      IF (IER = 1)  OR  (IER = 4) THEN
      begin
{$IFDEF UseTripackMessages}
        Tripack250(IFORTRAN, IER);
{$ENDIF}
        Exit;
      END;
{$IFDEF UseTripackMessages}
      IF (IER <> 0) then Tripack440(IFORTRAN);
{$ENDIF}
//      IO1 = 1
//      IO2 = 2
//      CALL DELARC (N,IO1,IO2, LIST,LPTR,LEND,LNEW, IER)
//      IF (IER .EQ. 1  .OR.  IER .EQ. 4) THEN
//        CALL Tripack250(IFORTRAN, IER)
//!       WRITE (LOUT,250) IER
//        return
//      ENDIF
//!      IF (IER .NE. 0) WRITE (LOUT,440)
//      IF (IER .NE. 0) CALL Tripack440(IFORTRAN)
//CC
//C   Recreate the triangulation without constraints.
//C
      TRMESH (N,X,Y, LIST,LPTR,LEND,LNEW,NODES, NODES,DS,IER,N);
      NCC := 0;
//      CALL TRMESH (N,X,Y, LIST,LPTR,LEND,LNEW,NODES,
//     .             NODES(N+1),DS,IER)
//      NCC = 0
//C
//C *** Test DELNOD by removing nodes 4 to N (in reverse
//C                 order).
//C

      IF (N <= 3) THEN
      begin
{$IFDEF UseTripackMessages}
         Tripack450(IFORTRAN);
{$ENDIF}
      end
      ELSE
      begin
        NN := N;
        repeat
          LW := LWK div 2;
          DELNOD (NN,NCC, LCC,NN,X,Y,LIST,LPTR,LEND, LNEW,LW,NODES, IER);
          IF (IER <> 0) THEN
          begin
{$IFDEF UseTripackMessages}
            Tripack260(IFORTRAN, IER);
{$ENDIF}
            Exit;
          END;
        until NN <= 3;
      END;

//      IF (N .LE. 3) THEN
//        CALL Tripack450(IFORTRAN)
//!        WRITE (LOUT,450)
//      ELSE
//        NN = N
//    4   LW = LWK/2
//          CALL DELNOD (NN,NCC, LCC,NN,X,Y,LIST,LPTR,LEND,
//     .                 LNEW,LW,NODES, IER)
//          IF (IER .NE. 0) THEN
//            CALL Tripack260(IFORTRAN, IER)
//!            WRITE (LOUT,260) IER
//            return
//          ENDIF
//          IF (NN .GT. 3) GO TO 4
//      ENDIF
//C
//C Successful test.
{$IFDEF UseTripackMessages}
      Tripack460(IFORTRAN);
{$ENDIF}
//      CALL Tripack460(IFORTRAN)
//!      WRITE (LOUT,460)
//      return
//C
//C Error reading the data set.
//C
//C# 30 WRITE (*,200)
//!      STOP
//C
//C Invalid value of N or NCC.
//C
//!   31 WRITE (*,205) N, NCC
//   31 CALL Tripack205(IFORTRAN, N, NCC)
//      return
//C
//C Error message formats:
//C
//C#200 FORMAT (//5X,'*** Input data set invalid ***'/)
//!  205 FORMAT (//5X,'*** N or NCC is outside its valid ',
//!     .             'range:  N =',I5,', NCC = ',I4,' ***'/)
//!  210 FORMAT (//5X,'*** Error in TRMESH:  the first three ',
//!     .        'nodes are collinear ***'/)
//!  212 FORMAT (//5X,'*** Error in TRMESH:  invalid ',
//!     .        'triangulation ***'/)
//!  214 FORMAT (//5X,'*** Error in TRMESH:  duplicate nodes ',
//!     .        'encountered ***'/)
//!  220 FORMAT (//5X,'*** Error in ADDCST:  IER = ',I1,
//!     .        ' ***'/)
//!  230 FORMAT (//5X,'*** Error in GETNP ***'/)
//!  240 FORMAT (//5X,'*** Error in NEARND ***'/)
//!  250 FORMAT (//5X,'*** Error in DELARC:  IER = ',I1,
//!     .        ' ***'/)
//!  260 FORMAT (//5X,'*** Error in DELNOD:  IER = ',I1,
//!     .        ' ***'/)
//!  270 FORMAT (//5X,'*** Error in TRPLOT:  IER = ',I1,
//!     .        ' ***'/)
//C
//C Informative message formats:
//C
//!  400 FORMAT (///1X,21X,'TRIPACK Test:  N =',I5///)
//!  410 FORMAT (5X,'Maximum triangle aspect ratio = ',E10.3//)
//!  420 FORMAT (/5X,'Output from BNODES and AREAP'//
//!     .        5X,'BNODES:  ',I4,' boundary nodes,  ',I5,
//!     .        ' edges,  ',I5,' triangles'/5X,
//!     .        'AREAP:  area of convex hull = ',E15.8//)
//!  430 FORMAT (5X,'Subroutine EDGE not tested:'/
//!     .        5X,'  No edges were swapped by ADDCST'//)
//! 440 FORMAT (5X,'Subroutine DELARC not tested:'/
//!     .        5X,'  Nodes 1 and 2 do not form a ',
//!     .           'removable boundary edge.'//)
//!  450 FORMAT (5X,'Subroutine DELNOD not tested:'/
//!     .        5X,'  N cannot be reduced below 3'//)
//!  460 FORMAT (5X,'No triangulation errors encountered.'/)
//!  470 FORMAT (/5X,'A triangulation plot file was ',
//!     .             'successfully created.'/)
//      END
end;

procedure TRMTST (var N: longint; var X,Y: TNmaxSingleArray;
  var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray; var LNEW: longint;
  var TOL: TFloat; var LUN:longint; var ARMAX: TFloat; var IER:longint);
//      SUBROUTINE TRMTST (N,X,Y,LIST,LPTR,LEND,LNEW,TOL,
//     .                   LUN, ARMAX,IER,IFORTRAN)
//      dll_import Trmtst100, Trmtst110, Trmtst120, Trmtst130, Trmtst140,
//     1  Trmtst150
//      INTEGER N, LIST(*), LPTR(*), LEND(N), LNEW, LUN, IER
//      REAL    X(N), Y(N), TOL, ARMAX
//C
//C***********************************************************
//C
//C                                               From TRPTEST
//C					     Robert J. Renka
//C				   Dept. of Computer Science
//C					Univ. of North Texas
//C					      (817) 565-2767
//C                                                   09/01/91
//C
//C   This subroutine tests the validity of the data structure
//C representing a Delaunay triangulation created by subrou-
//C tine TRMESH.  The following properties are tested:
//C
//C   1)  Each interior node has at least three neighbors, and
//C       each boundary node has at least two neighbors.
//C
//C   2)  abs(LIST(LP)) is a valid nodal index in the range
//C       1 to N and LIST(LP) > 0 unless LP = LEND(K) for some
//C       nodal index K.
//C
//C   3)  Each pointer LEND(K) for K = 1 to N and LPTR(LP) for
//C       LP = 1 to LNEW-1 is a valid LIST index in the range
//C       1 to LNEW-1.
//C
//C   4)  N .GE. NB .GE. 3, NT = 2*N-NB-2, and NA = 3*N-NB-3 =
//C       (LNEW-1)/2, where NB, NT, and NA are the numbers of
//C       boundary nodes, triangles, and arcs, respectively.
//C
//C   5)  Each circumcircle defined by the vertices of a tri-
//C       angle contains no nodes in its interior.  This prop-
//C       erty distinguishes a Delaunay triangulation from an
//C       arbitrary triangulation of the nodes.
//C
//C Note that no test is made for the property that a triangu-
//C lation covers the convex hull of the nodes.
//C
//C On input:
//C
//C       N = Number of nodes.  N .GE. 3.
//C
//C       X,Y = Arrays of length N containing the nodal
//C             coordinates.
//C
//C       LIST,LPTR,LEND = Data structure containing the tri-
//C                        angulation.  Refer to subroutine
//C                        TRMESH.
//C
//C       TOL = Nonnegative tolerance to allow for floating-
//C             point errors in the circumcircle test.  An
//C             error situation is defined as (R**2 - D**2)/
//C             R**2 > TOL, where R is the radius of a circum-
//C             circle and D is the distance from the
//C             circumcenter to the nearest node.  A reason-
//C             able value for TOL is 10*EPS, where EPS is the
//C             machine precision.  The test is effectively
//C             bypassed by making TOL large.  If TOL < 0, the
//C             tolerance is taken to be 0.
//C
//C       LUN = Logical unit number for printing error mes-
//C             sages.  If LUN < 0 or LUN > 99, no messages
//C             are printed.
//C
//C Input parameters are not altered by this routine.
//C
//C On output:
//C
//C       ARMAX = Maximum aspect ratio (radius of inscribed
//C               circle divided by circumradius) of a trian-
//C               gle in the triangulation unless IER > 0.
//C
//C       IER = Error indicator:
//C             IER = -1 if one or more null triangles (area =
//C                      0) are present but no (other) errors
//C                      were encountered.  A null triangle is
//C                      an error only if it occurs in the
//C                      the interior.
//C             IER = 0 if no errors or null triangles were
//C                     encountered.
//C             IER = 1 if a node has too few neighbors.
//C             IER = 2 if a LIST entry is outside its valid
//C                     range.
//C             IER = 3 if a LPTR or LEND entry is outside its
//C                     valid range.
//C             IER = 4 if the triangulation parameters (N,
//C                     NB, NT, NA, and LNEW) are inconsistent
//C                     (or N < 3 or LNEW is invalid).
//C             IER = 5 if a triangle contains a node interior
//C                     to its circumcircle.
//C
//C Module required by TRMTST:  CIRCUM
//C
//C Intrinsic function called by TRMTST:  MAX, ABS
//C
//C***********************************************************
//C
var
  K, LP, LPL, LPMAX, LPN, N1, N2, N3, NA, NB, NFAIL, NN, NNB, NT, NULL: longint;
  RATIO, RITE: longbool;
  AR, CR, CX, CY, RS, RTOL, SA: TFloat;
  IFORTRAN: longint;
  N1_Temp: Integer;
//      INTEGER K, LP, LPL, LPMAX, LPN, N1, N2, N3, NA, NB,
//     .        NFAIL, NN, NNB, NT, NULL
//      LOGICAL RATIO, RITE
//      REAL    AR, CR, CX, CY, RS, RTOL, SA
//      integer IFORTRAN
begin
//C
//C Store local variables, test for errors in input, and
//C   initialize counts.
//C
      IFORTRAN := 2;
      NN := N;
      LPMAX := LNEW - 1;
      RTOL := TOL;
      IF (RTOL < 0.0) then RTOL := 0.0;
      RATIO := TRUE;
      ARMAX := 0.0;
      RITE := (LUN >= 0)  AND  (LUN <= 99);
      IF (NN < 3) then
      begin
//C Inconsistent triangulation parameters encountered.
//C
        IER := 4;
{$IFDEF UseTripackMessages}
        IF (RITE) then Trmtst140(IFORTRAN,  N, LNEW, NB, NT, NA);
{$ENDIF}
        Exit;
      end;
      NB := 0;
      NT := 0;
      NULL := 0;
      NFAIL := 0;
//      NN = N
//      LPMAX = LNEW - 1
//      RTOL = TOL
//      IF (RTOL .LT. 0.) RTOL = 0.
//      RATIO = .TRUE.
//      ARMAX = 0.
//      RITE = LUN .GE. 0  .AND.  LUN .LE. 99
//      IF (NN .LT. 3) GO TO 14
//      NB = 0
//      NT = 0
//      NULL = 0
//      NFAIL = 0
//C
//C Loop on triangles (N1,N2,N3) such that N2 and N3 index
//C   adjacent neighbors of N1 and are both larger than N1
//C   (each triangle is associated with its smallest index).
//C   NNB is the neighbor count for N1.
//C
      for N1 := 1 to NN do
      begin

        NNB := 0;
        LPL := LEND[N1-1];
        IF (LPL < 1)  OR  (LPL > LPMAX) THEN
        begin
          LP := LPL;
//C LIST pointer LP is outside its valid range.
//C
          IER := 3;
{$IFDEF UseTripackMessages}
          N1_Temp := N1;
          IF (RITE) then Trmtst130(IFORTRAN,  LP, LNEW, N1_Temp);
{$ENDIF}
          Exit;
        end;
        LP := LPL;
//      DO 5 N1 = 1,NN
//        NNB = 0
//        LPL = LEND(N1)
//        IF (LPL .LT. 1  .OR.  LPL .GT. LPMAX) THEN
//          LP = LPL
//          GO TO 13
//        ENDIF
//        LP = LPL
//C
//C   Loop on neighbors of N1.
//C
        repeat
          LP := LPTR[LP-1];
          NNB := NNB + 1;
          IF (LP < 1)  OR  (LP > LPMAX) then
          begin
//C LIST pointer LP is outside its valid range.
//C
            IER := 3;
{$IFDEF UseTripackMessages}
            N1_Temp := N1;
            IF (RITE) then Trmtst130(IFORTRAN,  LP, LNEW, N1_Temp);
{$ENDIF}
            Exit;
          end;
//    1   LP = LPTR(LP)
//          NNB = NNB + 1
//          IF (LP .LT. 1  .OR.  LP .GT. LPMAX) GO TO 13
          N2 := LIST[LP-1];
          IF (N2 < 0) THEN
          begin
            IF (LP <> LPL) then
            begin
//C N2 = LIST(LP) is outside its valid range.
//C
              IER := 2;
{$IFDEF UseTripackMessages}
              N1_Temp := N1;
              IF (RITE) then Trmtst120(IFORTRAN,  N2, LP, N1_Temp);
{$ENDIF}
              Exit;
            end;
            IF (N2 = 0)  OR  (-N2 > NN) then
            begin
//C N2 = LIST(LP) is outside its valid range.
//C
              IER := 2;
{$IFDEF UseTripackMessages}
              N1_Temp := N1;
              IF (RITE) then Trmtst120(IFORTRAN,  N2, LP, N1_Temp);
{$ENDIF}
              Exit;
            end;
            NB := NB + 1;
          END
          else
          begin
//          N2 = LIST(LP)
//          IF (N2 .LT. 0) THEN
//            IF (LP .NE. LPL) GO TO 12
//            IF (N2 .EQ. 0  .OR.  -N2 .GT. NN) GO TO 12
//            NB = NB + 1
//            GO TO 4
//          ENDIF
            IF (N2 < 1)  OR  (N2 > NN) then
            begin
//C N2 = LIST(LP) is outside its valid range.
//C
              IER := 2;
{$IFDEF UseTripackMessages}
              N1_Temp := N1;
              IF (RITE) then Trmtst120(IFORTRAN,  N2, LP, N1_Temp);
{$ENDIF}              
              Exit;
            end;
            LPN := LPTR[LP-1];
            N3 := ABS(LIST[LPN-1]);
            IF (N2 >= N1)  AND  (N3 >= N1) then
            begin
            NT := NT + 1;
//          IF (N2 .LT. 1  .OR.  N2 .GT. NN) GO TO 12
//          LPN = LPTR(LP)
//          N3 = ABS(LIST(LPN))
//          IF (N2 .LT. N1  .OR.  N3 .LT. N1) GO TO 4
//          NT = NT + 1
//C
//C   Compute the coordinates of the circumcenter of
//C     (N1,N2,N3).
//C
            CIRCUM (X[N1-1],Y[N1-1],X[N2-1],Y[N2-1],X[N3-1],Y[N3-1], RATIO, CX,CY,CR,SA,AR);
            IF (SA = 0.0) THEN
            begin
              NULL := NULL + 1;
            END
            else
            begin
              ARMAX := MAX(ARMAX,AR);
  //          CALL CIRCUM (X(N1),Y(N1),X(N2),Y(N2),X(N3),Y(N3),
  //     .                 RATIO, CX,CY,CR,SA,AR)
  //          IF (SA .EQ. 0.) THEN
  //            NULL = NULL + 1
  //            GO TO 4
  //          ENDIF
  //          ARMAX = MAX(ARMAX,AR)
  //C
  //C   Test for nodes within the circumcircle.
  //C
              RS := CR*CR*(1.0-RTOL);
              for K := 1 to NN do
              begin
                IF (K = N1)  OR  (K = N2)  OR (K = N3) then
                begin
                  continue;
                end;
                IF (Sqr(CX-X[K-1]) + Sqr(CY-Y[K-1]) < RS) then
                begin
    //C   Node K is interior to the circumcircle of (N1,N2,N3).
                  NFAIL := NFAIL + 1;
                  break;
                end;
              end;
    //          RS = CR*CR*(1.-RTOL)
    //          DO 2 K = 1,NN
    //            IF (K .EQ. N1  .OR.  K .EQ. N2  .OR.
    //     .          K .EQ. N3) GO TO 2
    //            IF ((CX-X(K))**2 + (CY-Y(K))**2 .LT. RS) GO TO 3
    //    2       CONTINUE
    //          GO TO 4
    //C
    //C   Node K is interior to the circumcircle of (N1,N2,N3).
    //C
    //    3     NFAIL = NFAIL + 1
    //C
    //C   Bottom of loop on neighbors.
    //C
    //    4     IF (LP .NE. LPL) GO TO 1

              end;
            end;
          end;
        until LP = LPL;
        IF (NNB < 2)  OR  ((NNB = 2)  AND (LIST[LPL-1] > 0)) then
        begin
//C Node N1 has fewer than three neighbors.
//C
          IER := 1;
          N1_Temp := N1;
{$IFDEF UseTripackMessages}
          IF (RITE) then Trmtst110(IFORTRAN, N1_Temp, NNB);
{$ENDIF}
          Exit
        end;
//        IF (NNB .LT. 2  .OR.  (NNB .EQ. 2  .AND.
//     .      LIST(LPL) .GT. 0)) GO TO 11
//    5   CONTINUE
      end;
//C
//C Test parameters for consistency and check for NFAIL = 0.
//C
      NA := LPMAX div 2;
      IF (NB < 3)  OR  (NT <> 2*NN-NB-2)  OR (NA <> 3*NN-NB-3) then
      begin
//C Inconsistent triangulation parameters encountered.
//C
        IER := 4;
{$IFDEF UseTripackMessages}
        IF (RITE) then Trmtst140(IFORTRAN,  N, LNEW, NB, NT, NA);
{$ENDIF}
        Exit
      end;
      IF (NFAIL <> 0) then
      begin
//C Circumcircle test failure.
//C
        IER := 5;
{$IFDEF UseTripackMessages}
        IF (RITE) then Trmtst150(IFORTRAN,  NFAIL);
{$ENDIF}
        Exit;
      end;
//      NA = LPMAX/2
//      IF (NB .LT. 3  .OR.  NT .NE. 2*NN-NB-2  .OR.
//     .    NA .NE. 3*NN-NB-3) GO TO 14
//      IF (NFAIL .NE. 0) GO TO 15
//C
//C No errors were encountered.
//C
      IER := 0;
      IF (NULL = 0) then
      begin
        Exit;
      end;
      IER := -1;
{$IFDEF UseTripackMessages}
      IF (RITE) then Trmtst100(IFORTRAN, NULL);
{$ENDIF}
//      IER = 0
//      IF (NULL .EQ. 0) RETURN
//      IER = -1
//!      IF (RITE) WRITE (LUN,100) NULL
//      IF (RITE) call Trmtst100(IFORTRAN, NULL)
//!  100 FORMAT (//5X,'*** TRMTST -- ',I5,' NULL TRIANGLES ',
//!     .        'ARE PRESENT'/19X,'(NULL TRIANGLES ',
//!     .        'ON THE BOUNDARY ARE UNAVOIDABLE) ***'//)
//      RETURN
//C
//C Node N1 has fewer than three neighbors.
//C
//   11 IER = 1
//!      IF (RITE) WRITE (LUN,110) N1, NNB
//      IF (RITE) CALL Trmtst110(IFORTRAN, N1, NNB)
//!  110 FORMAT (//5X,'*** TRMTST -- NODE ',I5,
//!     .        ' HAS ONLY ',I5,' NEIGHBORS ***'/)
//      RETURN
//C
//C N2 = LIST(LP) is outside its valid range.
//C
//   12 IER = 2
// !     IF (RITE) WRITE (LUN,120) N2, LP, N1
//      IF (RITE) CALL Trmtst120(IFORTRAN,  N2, LP, N1)
//!  120 FORMAT (//5X,'*** TRMTST -- LIST(LP) =',I5,
//!     .        ', FOR LP =',I5,','/19X,
//!     .'IS NOT A VALID NEIGHBOR OF ',I5,' ***'/)
//      RETURN
//C
//C LIST pointer LP is outside its valid range.
//C
//   13 IER = 3
//!      IF (RITE) WRITE (LUN,130) LP, LNEW, N1
//      IF (RITE) CALL Trmtst130(IFORTRAN,  LP, LNEW, N1)
//!  130 FORMAT (//5X,'*** TRMTST -- LP =',I5,' IS NOT IN THE',
//!     .        ' RANGE 1 TO LNEW-1 FOR LNEW = ',I5/
//!     .        19X,'LP POINTS TO A NEIGHBOR OF ',I5,
//!     .        ' ***'/)
//      RETURN
//C
//C Inconsistent triangulation parameters encountered.
//C
//   14 IER = 4
//!      IF (RITE) WRITE (LUN,140) N, LNEW, NB, NT, NA
//      IF (RITE) CALL Trmtst140(IFORTRAN,  N, LNEW, NB, NT, NA)
//!  140 FORMAT (//5X,'*** TRMTST -- INCONSISTENT PARAMETERS',
//!     .        ' ***'/19X,'N = ',I5,' NODES',12X,'LNEW =',I5/
//!     .        19X,'NB = ',I5,' BOUNDARY NODES'/
//!     .        19X,'NT = ',I5,' TRIANGLES'/
//!     .        19X,'NA = ',I5,' ARCS'/)
//      RETURN
//C
//C Circumcircle test failure.
//C
//   15 IER = 5
//!      IF (RITE) WRITE (LUN,150) NFAIL
//      IF (RITE) CALL Trmtst140(IFORTRAN,  NFAIL)
//!  150 FORMAT (//5X,'*** TRMTST -- ',I5,' CIRCUMCIRCLES ',
//!     .        'CONTAIN NODES IN THEIR INTERIORS ***'/)
//      RETURN
//      END
end;

end.
