unit SfrFortranInterface;

interface

//uses TripackTypes;

procedure SfrDemo();
  stdcall; external 'tripack.dll';

// The following commented-out procedure declarations were used for debugging
// purposes during translation of the SRFPACK routines from Fortran to
// Object Pascal.  As each subroutine was translated, The Object Pascal
// version were tested by comaring their results to the results when the
// Fortran routines were called.

//procedure ARCINT_ext (var B,X1,X2,Y1,Y2,H1,H2,HX1,HX2,HY1,
//  HY2,SIGMA : single; var DFLAG: longbool; var HP,HXP,HYP: single; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure CNTOUR_exp (var NX,NY: longint; var X,Y: TNiSingleArray;
//  var Z: TNi_NjSingleArray; var CVAL: single; var LC,NCMAX: longint;
//  var IWK: longint; var XC, YC: TNmaxSingleArray; var ILC: TLwkIntArray;
//  // IWK is the first element in an array.
//  var NC,IER: longint);
//  stdcall; external 'tripack.dll';

//procedure COORDS_ext (var XP,YP,X1,X2,X3,Y1,Y2,Y3, B1,B2, B3: single; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure CRPLOT_ext (var LUN: longint; var PLTSIZ: single; var NX,NY: longint;
//  var PX,PY: TNiSingleArray; var PZ: TNi_NjSingleArray; var NCON: longint;
//  var IWK: TLwkIntArray; var XC,YC: TNmaxSingleArray; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure FVAL_ext (var XP,YP,X1,X2,X3,Y1,Y2,Y3,F1,F2,F3,
//  FX1,FX2,FX3,FY1,FY2,FY3,SIG1,SIG2,
//  SIG3, FP: single; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure GETSIG_ext (var N: longint; var X,Y,H: TNmaxSingleArray;
//  var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray; var HXHY: TNtnxSingleArray;
//  var TOL: single; var SIGMA: TN6SingleArray; var DSMAX: single; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure GRADG_ext (var NCC: longint; var LCC: TNcmaxIntarray; var N: longint;
//  var X,Y,Z: TNmaxSingleArray; var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray;
//  var IFLGS: longint; var SIGMA: TN6SingleArray; var NIT: longint;
//  var DGMAX: single; var GRAD: TNtnxSingleArray; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure GIVENS_ext (var A,B, C,S: single);
//  stdcall; external 'tripack.dll';

//procedure GRADL_ext (var K,NCC: longint; var LCC: TNcmaxIntarray; var N: longint;
//  var X,Y,Z: TNmaxSingleArray; var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray;
//  var DX,DY: single; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure GRCOEF_ext (var SIGMA,DCUB, D,SD: single);
//  stdcall; external 'tripack.dll';

//procedure INTRC0_ext (var PX,PY: single; var NCC: longint;
//  var LCC: TNcmaxIntarray; var N: longint;
//  var X,Y,Z: TNmaxSingleArray; var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray;
//  var IST: longint; var PZ: single; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure INTRC1_ext (var PX,PY: single; var NCC: longint;
//  var LCC: TNcmaxIntarray; var N: longint; var X,Y,Z: TNmaxSingleArray;
//  var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray;var IFLGS: longint;
//  var SIGMA: TN6SingleArray; var GRAD: TNtnxSingleArray; var DFLAG: longbool;
//  var IST :longint; var PZ,PZX,PZY: single; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure ROTATE_ext (var N: longint; var C,S, X,Y: single );
//// X and Y should the first elements in an array.
//  stdcall; external 'tripack.dll';

//procedure SETRO1_ext (var XK,YK,ZK,XI,YI,ZI,S1,S2,W: single; var ROW: single);
//// Row should be the first element of an array.
//  stdcall; external 'tripack.dll';

//procedure SGPRNT_ext (var N,LUNIT: longint; var LIST,LPTR: TN6IntArray;
//  var LEND: TNmaxIntArray; var SIGMA: TN6SingleArray);
//  stdcall; external 'tripack.dll';

//procedure SNHCSH_ext (var X, SINHM,COSHM,COSHMM: single);
//  stdcall; external 'tripack.dll';

//procedure UNIF_ext (var NCC: longint; var LCC: TNcmaxIntarray; var N: longint;
//  var X,Y,Z: TNmaxSingleArray; var GRAD: TNtnxSingleArray;
//  var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray; var IFLGS: longint;
//  var SIGMA: TN6SingleArray; var NROW,NX,NY: longint; PX,PY: TNiSingleArray;
//  var SFLAG: longbool; var SVAL: single; var ZZ: TNi_NjSingleArray; var IER: longint);
//  stdcall; external 'tripack.dll';

//FUNCTION TRVOL_ext (var X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3: single): single;
//  stdcall; external 'tripack.dll';

//FUNCTION VOLUME_ext (var NCC: longint; var LCC: TNcmaxIntarray; var N: longint;
//  var X,Y,Z: TNmaxSingleArray; var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray): single;
//  stdcall; external 'tripack.dll';

//procedure TVAL_ext (var X,Y,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,ZX1,
//  ZX2,ZX3,ZY1,ZY2,ZY3: single; var DFLAG: longbool; var F,FX,FY: single;
//  var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure ZGRADG_ext (var NCC: longint; var LCC: TNcmaxIntarray; var N: longint;
//  var X,Y: TNmaxSingleArray; var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray;
//  var IFLGS: longint; var SIGMA: TN6SingleArray; var NIT: longint;
//  var DZMAX : single; var Z: TNmaxSingleArray;
//  var GRAD: TNtnxSingleArray; var IER: longint);
//  stdcall; external 'tripack.dll';

//procedure ZINIT_ext (var NCC: longint; var LCC: TNcmaxIntarray; var N: longint;
//  var X,Y: TNmaxSingleArray; var LIST,LPTR: TN6IntArray; var LEND: TNmaxIntArray;
//  var Z: TNmaxSingleArray; var IER: longint);
//  stdcall; external 'tripack.dll';

implementation

end.
