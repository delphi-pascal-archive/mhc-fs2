unit OpenML;

{******************************************************************************}
{                                                                              }
{        SOREL (C)CopyRight       1999-2000 Russia, S.-Petersburg.             }
{                                                                              }
{    OpenML    -        модуль вычислительных алгоритмов.(класс TMathLib)      }
{                                                                              }
{                                                                              }
{                     -OpenML-   ver. 3.4.3  (доделать!)                       }
{******************************************************************************}

interface

{$D+}


uses
  Windows,  SysUtils, Classes,  Dialogs, Math;

type
TAddress = Cardinal;
type TFloatType = extended;
const NumberFact: Cardinal = 1754;
const _Pi: extended = 3.1415926535897932;
const ml_Integral_Currency: Cardinal  = 100;
const ml_Derivative_Currency: Cardinal  = 101;
type
PFloatType = ^TFloatType;
type
TIntegType = Integer;
type
PIntegType = ^TIntegType;
{type
PFunc = Foreval_HD.PFunc; }
{type
PStack = Foreval_HD.PStack; }
{type
TFuncList = Foreval_HD.TFuncList; }
type
TFunc = function(x: TFloatType): TFloatType;
type
TArray1 = array of TFloatType;
type
TArrayI = array of Integer;
type
TArrayX = array of extended;
type
TArrayA = array of TAddress;
type
TArray2 = array of array of TFloatType;
type
TArrayS1 = array of String;
type
TString1 = String[1];
{type
TArrayA = array of TAddress; }
type
PArray1 = array of PFloatType;
type
TInteg2 = record
           I1: Integer;
           I2: Integer;
          end;
type
TArray1_2 = array of TInteg2;
type
TSetMode = record
             DiffL: Integer;
             DiffH: TFloatType;
             IntegL: Integer;
             IntegH: TFloatType;
             FZero: TFloatType;
             E_Root: TFloatType;
             N_RootSafe: Integer;
             F_RootType: Integer;
             F_RootPower: Integer;
             E_SystRoot: TFloatType;
             N_SystRootSafe: Integer;
             F_SystRootType: Integer;
             F_SystRootPower: Integer;
            end;



type
TFourie = record
             A: TArray1;
             B: TArray1;
            end;

type
TSplain = record
             A0: TArray1;
             A1: TArray1;
             A2: TArray1;
             A3: TArray1;
             PS: TArray1;
             SL: Integer;
            end;

type
PFourie = ^TFourie;
type
PSplain = ^TSplain;

type
  TMathLib = class

  private
   F_CalcError: TIntegType;
   H_Error: String;
   Fact: TArray1;
   CurrentMode,InitMode: TSetMode;
   IntegH,FZero: TFloatType;
   IntegL: Integer;
   procedure CreateFactMass;
   procedure InitIntegral;
   function  ReadCalcError: TIntegType;
   //function  ReadError: TIntegType;
   function  Integral(func: TFunc; a,b: TFloatType; N:TIntegType):TFloatType;
   function  Integral1PL(Func: TAddress;  PV: PFloatType; M: Cardinal; a,b: TFloatType): TFloatType;

   //function  Diff1(x: TFloatType): TFloatType; overload;
   function  Diff1(Func: TAddress;  x: TFloatType): TFloatType; overload;
   //function  Diff2(x: TFloatType): TFloatType; overload;
   function  Diff2(Func: TAddress;  x: TFloatType): TFloatType; overload;
   function  Diff3(Func: TAddress;  x: TFloatType): TFloatType;
   function  Diff4(Func: TAddress;  x: TFloatType): TFloatType;
   function  Diff5(Func: TAddress;  x: TFloatType): TFloatType;
   function  Diff1_I(Func: TFunc; x: TFloatType): TFloatType;
   function  Diff2_I(Func: TFunc; x: TFloatType): TFloatType;

   procedure Root_IR1(x: TFloatType; var Rt: TFloatType; var Error: Integer);
   procedure Root_IR2(x: TFloatType; var Rt: TFloatType; var Error: Integer);
   procedure Root_IM1(x: TFloatType; var Rt: TFloatType; var Error: Integer);
   procedure Root_IM2(x: TFloatType; var Rt: TFloatType; var Error: Integer);
   function  RoundX(x: TFloatType): TFloatType;

   //procedure TranslationError; override;
   function  Diff_N(N: TIntegType; Func: TAddress;  x: TFloatType): TFloatType;
   procedure InitDiff;
   procedure SetDiffC(k: integer);                    //help for diff coef
   function  DiffN(N: Integer; X: Double): extended;  //help for diff coef
   procedure CalcDiffCoeff;
   function  A1_N: extended;
   function  A2_N: extended;
   function  A3_N: extended;
   function  A4_N: extended;
   function  A5_N: extended;
   function  A6_N: extended;
   function  A7_N: extended;
   function  A8_N: extended;
   function  A9_N: extended;
   function  A10_N: extended;
   function  A11_N: extended;
   function  A12_N: extended;
   function  A13_N: extended;
   function  A14_N: extended;
   function  A15_N: extended;
   function  A16_N: extended;
   function  A17_N: extended;
   function  A18_N: extended;
   function  A19_N: extended;
   function  A20_N: extended;
   function  Dist(Xk,Xl: TArray1): TFloatType;
   procedure SE_SetVar(PX: PArray1; X: TArray1);
   function  Norm(FL: TArrayA): TFloatType;
   procedure NLSE_N(FL: TArrayA; PX: PArray1; X0: TArray1; E: TFloatType; var Root: TArray1; var Er: Integer);
   function  DGF(FL: TArrayA;  FV: PFloatType; var Error: Integer): TFloatType;
   function  GF(FL: TArrayA; var Error: Integer): TFloatType;
   function  SE_gradF(FL: TArrayA; PX: PArray1; var Error: Integer): TArray1;
   function  SE_SQDF(FL: TArrayA; PX: PArray1; var Error: Integer): TFloatType;
   function  NLSE_G(FL: TArrayA; PX: PArray1; X0: TArray1;   E: TFloatType; safe: Integer; var Error: Integer): TArray1;
   function  NLSE_M1(FL: TArrayA; PX: PArray1; X0: TArray1;  E: TFloatType; safe: Integer; var Error: Integer): TArray1;
   function  NLSE_M2(FL: TArrayA; PX: PArray1; X0: TArray1;  E: TFloatType; safe: Integer; var Error: Integer): TArray1;
   function  NLSE_R1(P: Integer; FL: TArrayA; PX: PArray1; X0: TArray1;  E: TFloatType; safe: Integer; var Error: Integer): TArray1;
   function  NLSE_R2(P: Integer; FL: TArrayA; PX: PArray1; X0: TArray1;  E: TFloatType; safe: Integer; var Error: Integer): TArray1;
   function  RootM1(Func: TAddress; x: TFloatType; PV: PFloatType; var Error: Integer): TFloatType;
   function  RootM2(FL: TAddress; PV: PFloatType; x: TFloatType; E: TFloatType; safe: Integer; var Error: Integer): TFloatType;
   function  RootR1(Func: TAddress; x: TFloatType; PV: PFloatType; var Error: Integer): TFloatType;
   function  RootR2(P: Integer; FL: TAddress; PV: PFloatType; x: TFloatType;   E: TFloatType;  safe: Integer; var Error: Integer): TFloatType;
   procedure InitRoot;
   procedure InitSystRoot;
   procedure SaveInitMode;
   procedure LSE_Gausse(A: TArray2; B: TArray1; var B1: TArray1{; var D: TFloatType});
   procedure LSE_Jordan(A: TArray2; B: TArray1; var B1: TArray1{; var D: TFloatType});
   procedure MatrixDeterminant_Gausse(A: TArray2;  var D: TFloatType);
   procedure MatrixDeterminant_Jordan(A: TArray2;  var D: TFloatType);
   procedure LSEDet_Gausse(A: TArray2; B: TArray1; var B1: TArray1; var D: TFloatType);
   procedure LSEDet_Jordan(A: TArray2; B: TArray1; var B1: TArray1; var D: TFloatType);
   procedure MatrixPolynom_Faddeev(A: TArray2; var P: TArray1);
   procedure MatrixPolynom_Leverie(A: TArray2; var P: TArray1);



  


  public

    constructor Create{(AOwner:TComponent); override};
    property  CalculationError: TIntegType read  ReadCalcError default 0;
    //property  ERROR: TIntegType read ReadError default 0;
    function  Bessel(M: TIntegType; X:TFloatType):TFloatType ;
    function  Gamma(X: TFloatType): TFloatType;
    function  Beta(p,q: TFloatType):TFloatType;
    function  IntEllipt1(f,x: TFloatType): TFloatType;
    function  IntEllipt2(f,x: TFloatType): TFloatType;
    function  Chebyshev_P(n: TIntegType; x: TFloatType): TFloatType;
    function  Laguerre_P(n:TIntegType; x:TFloatType):TFloatType ;
    function  Legendre_P(n:TIntegType; x:TFloatType):TFloatType ;
    function  Hermite_P(n:TIntegType; x:TFloatType):TFloatType ;
    function  IntLaplas(x:TFloatType):TFloatType;
    function  Erf(x:TFloatType):TFloatType;
    procedure LSE(A: TArray2; B: TArray1; var B1: TArray1{; var D: TFloatType});
    procedure MatrixDeterminant(A: TArray2;  var D: TFloatType);
    procedure MatrixInvert(A: TArray2; var B: TArray2{; var D: TFloatType});
    procedure MatrixMul(A: TArray2; B: TArray2; var C: TArray2);
    function  Derivative1(Func: TAddress;  PV: PFloatType; x: TFloatType): TFloatType; overload;
    //function  Derivative1(PV: PFloatType; x: TFloatType): TFloatType;  overload;
    function  Derivative2(Func: TAddress;  PV: PFloatType; x: TFloatType): TFloatType; overload;
    //function  Derivative2(PV: PFloatType; x: TFloatType): TFloatType;  overload;
    function  Derivative3(Func: TAddress;  PV: PFloatType; x: TFloatType): TFloatType;
    function  Derivative4(Func: TAddress;  PV: PFloatType; x: TFloatType): TFloatType;
    function  Derivative5(Func: TAddress;  PV: PFloatType; x: TFloatType): TFloatType;
    function  Root(Func: TAddress; PV: PFloatType; x0: TFloatType; {убрать} var Error: Integer{убрать}): TFloatType;
    procedure PolyRoot(PL: TArray1; var R: TArray1);


    function  Integral1(Func: TAddress;  PV: PFloatType; a,b: TFloatType): TFloatType;     overload;
    function  Integral2F(Func: TAddress;  PV1,PV2: PFloatType; a,b: TFloatType; Func1,Func2: TAddress): TFloatType;
    function  Integral2(Func: TAddress;  PV1: PFloatType; PV2: PFloatType; a,b,c,d: TFloatType): TFloatType;
    function  Integral3(Func: TAddress;  PV1,PV2,PV3: PFloatType; a,b,c,d,e,f: TFloatType): TFloatType;  overload;
    function  Integral3F(Func: TAddress;  PV1,PV2,PV3: PFloatType; a,b,c,d: TFloatType; Func1,Func2: TAddress): TFloatType;
    function  Integral3FF(Func: TAddress;  PV1,PV2,PV3: PFloatType; a,b: TFloatType; Func1,Func2,Func3,Func4: TAddress): TFloatType;

    function  IntegralN(N: Integer; A,B: TArray1; VL: PArray1; Func: Taddress): TFloatType;
    function  IntegralNF(N: Integer; FuncN: TArrayA; VL: PArray1; Func: TAddress): TFloatType;
    function  IntegralMK(N: Integer; A,B: TArray1; VL: PArray1; Func: TAddress): TFloatType;
    function  IntegralMKF(N: Integer; FuncN: TArrayA; VL: PArray1; Func: TAddress): TFloatType;

    function  DERK6(Func:TAddress; PX,PY:PFloatType; X0,Y0,X: TFloatType): TFloatType;
    procedure DERK6A(Func:TAddress; PX,PY:PFloatType; X0,Y0,X: TFloatType; var A: TArray1);
    procedure DERK6Sys(F:TArrayA; PV: PArray1;  Y0: TArray1; X0,X: TFloatType; var RV: TArray1);
    function  DERK6N(F:TAddress; PV: PArray1;  Y0: TArray1; X0,X: TFloatType): TFloatType;

    //function  Sum1(PV: PIntegType; a,b: TIntegType): TFloatType; overload;
    function  Sum1(Func: TAddress;  PV: PIntegType; a,b: TIntegType): TFloatType; overload;
    //function  Sum2(PV1,PV2: PIntegType; a,b: TIntegType; F1,F2: String): TFloatType; overload;
    function  Sum2(Func: TAddress;  PV1,PV2: PIntegType; a,b: TIntegType; F1,F2: String): TFloatType; overload;
    function  Sum2(Func: TAddress;  PV1,PV2: PIntegType; a,b,c,d: TIntegType): TFloatType; overload;
    //function  Sum3(PV1,PV2,PV3: PIntegType; a,b: TIntegType; F1,F2,F3,F4: String): TFloatType; overload;
    function  Sum3(Func: TAddress;  PV1,PV2,PV3: PIntegType; a,b: TIntegType; F1,F2,F3,F4: String): TFloatType; overload;
    function  Integral1(F,X: TArray1; a,b: TFloatType; n: TIntegType = 1; zl: TFloatType = 1E-10): TFloatType; overload;

    procedure Fourie(Func: TAddress;  PV: PFloatType; a,b: TFloatType;  NK: TIntegType; var FS,FC: TArray1); overload;
    procedure Fourie(F: TArrayA; X: TArray1; PV: PFloatType;  NK: TIntegType; var FS,FC: TArray1); overload;
    procedure CreateSplain3(MX: TArray1; MF: TArray1; var SP: TSplain);
    function  Splain3(x: TFloatType; SP: TSplain): TFloatType;
    procedure LSM(X,Y: TArray1; M: Cardinal; var PL: TArray1);
    procedure LSMI(Func: TAddress; PV: PFloatType; a,b: TFloatType; M: Cardinal; var PL: TArray1);


    function  Derivative1(F,X: TArray1; t: TFloatType): TFloatType;   overload;
    function  Derivative2(F,X: TArray1; t: TFloatType): TFloatType;   overload;
    procedure MatrixInvertDet(A: TArray2; var B: TArray2; var D: TFloatType);
    procedure LSEDet(A: TArray2; B: TArray1; var B1: TArray1; var D: TFloatType);
    function  Sum3(Func: TAddress;  PV1,PV2,PV3: PIntegType; a,b: TIntegType; Func1: TAddress;  Func2: TAddress;   Func3: TAddress;  Func4: TAddress): TFloatType; overload;
    function  Derivative_N(N: TIntegType; Func: TAddress;   PV: PFloatType; x: TFloatType): TFloatType;
    function  Factorial(N: Integer): TFloatType;
    procedure SetDiffMode(N: Integer; h: TFloatType = 0.01);
    function  Cnk(n,k: Integer): TFloatType;
    function  NLSE(FL: TArrayA; PV: PArray1; X0: TArray1; var Error: Integer; E: TFloatType = 0; safe: Integer = 0; TF: Integer = 0; TP: Integer = 0): TArray1;
    function  ScanSysRoot(FL: TArrayA; PV: PArray1; X0,X1: TArray1; h: TFloatType; T: Integer; var Error: Integer; E: TFloatType = 0; safe: Integer = 0; TF: Integer = 0; TP: Integer = 0): TArray2;
    function  ScanRoot(FL: TAddress; PV: PFloatType; X0,X1,h: TFloatType; T: Integer; var Error: Integer; E: TFloatType = 0; safe: Integer = 0; TF: Integer = 0; TP: Integer = 0): TArray1;
    procedure SetRootMode(E: TFloatType; Safe: Integer; Mode: Integer; Power: Integer);
    procedure SetSystRootMode(E: TFloatType; Safe: Integer; Mode: Integer; Power: Integer);
    function  SaveSetModes: TSetMode;
    procedure PresetRecall;
    procedure ExchangeStr(A: TArray2; i,j: Cardinal);
    procedure ExchangeClm(A: TArray2; i,j: Cardinal);
    procedure MatrixMulConstAdd(A,B: TArray2; x: TFloatType; var C: TArray2);
    procedure MatrixMulConst(A: TArray2; x: TFloatType);
    procedure MatrixAdd(A,B: TArray2; var C: TArray2);
    procedure GetMatrixE(x: TFloatType; N: Cardinal; var E: TArray2);
    procedure MatrixSub(A,B: TArray2; var C: TArray2);
    procedure MatrixMinor(A: TArray2; i,j: Cardinal; var B: TArray2);
    procedure MatrixStrMulConst(A: TArray2; I: Cardinal; x: TFloatType);
    procedure MatrixAddStrMulConst(A: TArray2; I: Cardinal; x: TFloatType);
    function  MatrixTrace(A: TArray2): TFloatType;
    procedure MatrixTranspose(A: TArray2);
    procedure GetMatrixLR(A: TArray2; var L,R: TArray2);
    procedure MatrixPolynomInv(A: TArray2; var P: TArray1; var C: TArray2);
    procedure MatrixVectorSL(A: TArray2; L: TFloatType; var V: TArray1);
    procedure GetMatrixG(A: TArray2; L: TFloatType; K: Cardinal;  var G: TArray2);
    procedure MatrixPolynom(A: TArray2; var P: TArray1);

    function  CalcPolynom(PL: TArray1; x: TFloatType): TFloatType;
    procedure SetMode(Mode: Integer;v: TFloatType);



            
  end;


  var
  MathLib: TMathLib;
  Px:    TFloatType;
  Pj,
  Pm,                  //параметры специальных функций
  SL,                  //число сплайнов
  G_PN0,G_PN:    TIntegType;  //степень полинома (поиск корней):начального/текущего
  A3,A2,A1,A0,        //коэфф. сплайнов
  PS,                 //аргументы сплайнов
  Pk0,Pk:    TArray1;     //коэфф. полинома (поиск корней):начального/текущего
  FV1,FV2,FV3:   PFloatType;
  IV1,IV2,IV3:           PIntegType;
  an,bn: array [1..15] of extended; //интегральные коэффиценты
  D1,D2,D3,D4,D5: array of extended;  //массивы коэф-ов для 5-ти производных
  DA,DD: TArrayX; //help mass for diff
  DiffMode,DiffL: Integer;
  DiffH: TFloatType;
  IFunc: TAddress;
  G_TS,G_TC: TArray1_2; //массивы номеров переставлений строк/столбцов (исп-ся в Matrix преобразованиях)
  F_Clm,F_Str: Integer; //соответствующие флаги
  F_DetType: Integer;     //0 - Gausse; 1 - Jordan
  F_LSEType: Integer;     //1 - Gausse; 0 - Jordan
  F_LSEDetType: Integer;  //1 - Gausse; 0 - Jordan
  F_MatrixPolynom: Integer; //0 - Leverie; 1 - Faddeev;
  H_Integral: TFloatType;   //точность интегрирования
  N_Integral: Integer;   //число узлов интегрирования
  F_Integral_MultyH: Integer; //для кратных инт-ов: 1-для каждого инт-ла
                              //своё разбиение - h из массива M_IntegalH;
  M_IntegralH: TArray1;
  N_IntegralMK: Cardinal;  //число точек для интеграла - MK
  F_Approximation: Cardinal; //1 - начальные приближения задаются; 0 - внутренние(по умолчанию)
  N_RootSafe: Cardinal;    //число циклов для прерывания зацикленности при поиске корней
  E_Root: TFloatType;      //точность поиска корней
  F_RootType: Cardinal;    //тип формулы поиска корней
  F_RootPower: Cardinal;   //степень рекурентности -//-
  A_Root: TFloatType;      //начальное приближение (зависит от состояния F_Approximation)
                           //для систем нелинейных ур-ий:
  E_SystRoot: TFloatType;
  F_SystRootType: Cardinal;
  F_SystRootPower: Cardinal;
  N_SystRootSafe: Cardinal;
  A_SystRoot: TArray1;
  A_LSE: TArray1;         //для LSE

  E_Round: Cardinal;   //степень округления; исп-ся в Polyroot для уточнения целых корней







//procedure Register;
function  CalcFunc(Addr: TAddress): TFloatType;
procedure XCHSC(i,j: TIntegType; M: TArray2; B: TArray1; var E: Integer);
procedure XCHSC1(i,j: TIntegType; A: TArray2; B: TArray2; var E: Integer);
procedure XCHSC2(i,j: TIntegType; M: TArray2;  var E: Integer);

implementation
//uses Unit1;
var
MI: Cardinal;


{
procedure Register;
begin
RegisterComponents('Samples',[TMathLib]);
end;
}


function CalcFunc(Addr: TAddress): TFloatType;
asm
  call Addr
end;


procedure XCHSC(i,j: TIntegType; M: TArray2; B: TArray1; var E: Integer);
label endp;
var
x: TFloatType;
l,k,N: TIntegType;
begin
E:=0;
N:=High(M);
for k:=i to N do
begin
 if M[k,j] <> 0 then
 begin
  for l:=0 to N do
  begin
   x:=M[i,l]; M[i,l]:=M[k,l]; M[k,l]:=x;
  end;
   x:=B[i]; B[i]:=B[k]; B[k]:=x;
  goto endp;
 end;
end;



for k:=j to N do
begin
 if M[i,k] <> 0 then
 begin
  for l:=0 to N do
  begin
   x:=M[l,j]; M[l,j]:=M[l,k]; M[l,k]:=x;
   SetLength(G_TC,Length(G_TC)+1);
   G_TC[High(G_TC)].I1:=j;
   G_TC[High(G_TC)].I2:=k;
   F_Clm:=1;
  end;
  goto endp;
 end;
end;


E:=1;

endp:
end;



procedure XCHSC1(i,j: TIntegType; A: TArray2; B: TArray2; var E: Integer);
label 1,endp;
var
x: TFloatType;
l,k,N: TIntegType;
begin
E:=0;
N:=High(A);

for k:=i to N do
begin
 if A[k,j] <> 0 then
 begin
  for l:=0 to N do
  begin
   x:=A[i,l]; A[i,l]:=A[k,l]; A[k,l]:=x;
   x:=B[i,l]; B[i,l]:=B[k,l]; B[k,l]:=x;
  end;
  goto 1;
 end;
end;
E:=1;

1:
if E = 0 then goto endp;

for k:=j to N do
begin
 if A[i,k] <> 0 then
 begin
  for l:=0 to N do
  begin
   x:=A[l,j]; A[l,j]:=A[l,k]; A[l,k]:=x;
  end;
  E:=0;
  goto endp;
 end;
end;


E:=1;


endp:
end;



procedure XCHSC2(i,j: TIntegType; M: TArray2;  var E: Integer);
label endp;
var
x: TFloatType;
l,k,N: TIntegType;
begin
E:=0;
N:=High(M);
for k:=i to N do
begin
 if M[k,j] <> 0 then
 begin
  for l:=0 to N do
  begin
   x:=M[i,l]; M[i,l]:=M[k,l]; M[k,l]:=x;
  end;
  goto endp;
 end;
end;



for k:=j to N do
begin
 if M[i,k] <> 0 then
 begin
  for l:=0 to N do
  begin
   x:=M[l,j]; M[l,j]:=M[l,k]; M[l,k]:=x;
  end;
  goto endp;
 end;
end;


E:=1;

endp:
end;



procedure TMathLib.CreateFactMass;
var
i,j: TIntegType;
S: TFloatType;
begin                   //создание массива факториалов


SetLength(Fact,NumberFact+1);

S:=1;
for j:=1 to NumberFact do
begin
 S:=S*j;
 Fact[j]:=S;
end;
 Fact[0]:=1;
end;



constructor TMathLib.Create{(AOwner:TComponent)};
var
R: TFloatType;
begin
//порядок не менять
  F_CalcError:=0;
  InitRoot;
  InitSystRoot;
  inherited Create{(AOwner)};
  //Fact:=_Factorial;
  CreateFactMass;
  InitDiff;
  InitIntegral;
  SaveInitMode;


  MI:=0;
end;



procedure TMathLib.InitIntegral;
begin
 H_Integral:=0.1;
 N_Integral:=15;
 F_Integral_MultyH:=0;
 N_IntegralMK:=1000;
 an[1]:=0.03075324199611726835; an[2]:=0.07036604748810812471;
 an[3]:=0.10715922046717193501; an[4]:=0.13957067792615431445;
 an[5]:=0.16626920581699393355; an[6]:=0.18616100001556221103;
 an[7]:=0.19843148532711157646; an[8]:=0.20257824192556127288;
 an[9]:=an[7];  an[10]:=an[6];  an[11]:=an[5]; an[12]:=an[4];
 an[13]:=an[3]; an[14]:=an[2];  an[15]:=an[1];
 bn[1]:=0.98799251802048542849; bn[2]:=0.93727339240070590431;
 bn[3]:=0.84820658341042721620; bn[4]:=0.72441773136017004742;
 bn[5]:=0.57097217260853884754; bn[6]:=0.39415134707756336990;
 bn[7]:=0.20119409399743452230; bn[8]:=0;
 bn[9]:=-bn[7]; bn[10]:=-bn[6]; bn[11]:=-bn[5]; bn[12]:=-bn[4];
 bn[13]:=-bn[3];bn[14]:=-bn[2]; bn[15]:=-bn[1];
end;

procedure TMathLib.InitDiff;
begin
DiffL:=14;
DiffH:=0.01;
SetLength(D1,DiffL+1);
SetLength(D2,DiffL+1);
SetLength(D3,DiffL+1);
SetLength(D4,DiffL+1);
SetLength(D5,DiffL+1);
CalcDiffCoeff;
end;


(*procedure TMathLib.TranslationError;
var
S: String;
begin
if ShowErrorMessage then
begin
if F_CalcError <> 0 then
begin
 if F_CalcError  = 10 then S:='WRONG ARGUMENT';
 if F_CalcError  = 11 then S:='CALCULATION ERROR';
 if F_CalcError  = 12 then S:='DISTINCTION DIMENSION';
 if F_CalcError  = 13 then S:='OUT OF RANGE';
 MessageBeep(0);
 MessageDlg(H_Error+#13#10+S,mtError,[mbOk],0);
 F_CalcError:=0;
end
else inherited {TranslationError;}
end;

end;
*)

function TMathLib.ReadCalcError: TIntegType;
begin
ReadCalcError:=F_CalcError;
F_CalcError:=0;
end;




{function TMathLib.ReadError: TIntegType;
begin
 //0 < ReadError < 10 - SyntaxError
 //10 < ReadError  - CalcError
 if ErrorCode <> 0 then begin ReadError:=ErrorCode; ErrorCode:=0; end
 else
 if F_CalcError <> 0 then begin ReadError:=F_CalcError; F_CalcError:=0; end
 else
 ReadError:=0;
end;
}


function _Bessel(t: TFloatType): TFloatType;
begin
_Bessel:=cos(Pm*t-Px*sin(t))/_Pi;
end;


function _IntEllipt1(t: TFloatType): TFloatType;
begin
_IntEllipt1:=1/(sqrt(1-sqr(Px*sin(t))));
end;


function _IntEllipt2(t: TFloatType): TFloatType;
begin
_IntEllipt2:=sqrt(1-sqr(Px*sin(t)));
end;


function _IntLaplas(t: TFloatType): TFloatType;
begin
_IntLaplas:=exp(-sqr(t)/2)/sqrt(2*_Pi);
end;


function _Erf(t: TFloatType): TFloatType;
begin
_Erf:=2*exp(-sqr(t))/sqrt(_Pi);
end;


function Polynom(x: TFloatType): TFloatType;
var
i: LongInt;
r: TFloatType;
begin
r:=0;
for i:=0 to G_PN do
begin
r:=r+Pk[i]*intpower(x,G_PN-i);
end;
Polynom:=r;
end;



function Polynom0(x: TFloatType): TFloatType;
var
i: LongInt;
r: TFloatType;
begin
r:=0;
for i:=0 to G_PN0 do
begin
r:=r+Pk0[i]*intpower(x,G_PN0-i);
end;
Polynom0:=r;
end;



function TMathLib.CalcPolynom(PL: TArray1; x: TFloatType): TFloatType;
var
i,Sz: LongInt;
r: TFloatType;
begin
r:=0;
Sz:=High(PL);
for i:=0 to Sz do
begin
r:=r+PL[i]*intpower(x,Sz-i);
end;
CalcPolynom:=r;
end;


function Diff1_P(x: TFloatType): TFloatType;
var
i: LongInt;
r: TFloatType;
begin
r:=0;
for i:=0 to G_PN-1 do
begin
r:=r+Pk[i]*intpower(x,G_PN-i-1)*(G_PN-i);
end;
Diff1_P:=r;
end;



function Diff2_P(x: TFloatType): TFloatType;
var
i: LongInt;
r: TFloatType;
begin
r:=0;
for i:=0 to G_PN-2 do
begin
r:=r+Pk[i]*intpower(x,G_PN-i-2)*(G_PN-i)*(G_PN-i-1);
end;
Diff2_P:=r;
end;



function TMathLib.Integral(func: TFunc; a,b: TFloatType; N:TIntegType):TFloatType;
var
j,i: TIntegType;
x1,x2,t,Int,h:TFloatType;
begin
h:=abs(b-a)/N; Int:=0;

for j:=0 to N-1 do
begin
x1:=a+j*h; x2:=a+(j+1)*h;
for i:=1 to 7 do
begin
t:=(x2-x1)*0.5*bn[i]+(x1+x2)*0.5;
Int:=Int+an[i]*func(t)*(x2-x1)*0.5;
end;
end;

Integral:=Int;
end;



function TMathLib.Bessel(M: TIntegType; X: TFloatType): TFloatType;
var
Int: TFloatType;
N: TIntegType;
begin
N:=1000;  Pm:=M; Px:=X;  Bessel:=0;

try
 Int:=Integral(_Bessel,0,_Pi,N);
 if abs(Int) < 1e-15 then Int:=0;
 Bessel:=Int;
except
 F_CalcError:=11;  H_Error:='BESSEL:'; {TranslationError;}
end;
end;




function TMathLib.Gamma(X: TFloatType): TFloatType;
label endp;
var
Tr,n,i: TIntegType;
S,x1,x2,y,c: TFloatType;
begin
Gamma:=0;
if (Trunc(x)=x) and (x<0) then begin F_CalcError:=10; H_Error:='GAMMA:'; {TranslationError;} goto endp; end;
if x = 0 then begin Gamma:=1; goto endp; end;

n:=Trunc(NumberFact-abs(x));
Tr:=Trunc(x);
if (Tr = x)and (x > 0) then begin S:=Fact[Tr-1]; end
else
begin
 x1:=x;
 if x1 < 0 then
 begin
  c:=-_Pi/(x1*sin(Pi*x1)); x1:=abs(x1); y:=1;  x2:=x1+n;
  S:=exp(-x2)*Power(x2,x2-0.5)*sqrt(2*_Pi)*(1+1/(12*x2)+1/(288*sqr(x2))-139/(51840*sqr(x2)*x2)-571/(2488320*sqr(sqr(x2))));
  for i:=0 to n-1 do
  begin
   y:=y*(x1+i);
  end;
  S:=c*y/S;
 end
else

 begin
  y:=1;  x2:=x1+n;
  S:=exp(-x2)*Power(x2,x2-0.5)*sqrt(2*_Pi)*(1+1/(12*x2)+1/(288*sqr(x2))-139/(51840*sqr(x2)*x2)-571/(2488320*sqr(sqr(x2))));
  for i:=0 to n-1 do
  begin
   y:=y*(x1+i);
  end;
  S:=S/y;
 end;
end;

Gamma:=S;

endp:
end;



function TMathLib.Beta(p,q: TFloatType):TFloatType;
label endp;
begin
if (Trunc(p)=p) and (p<0) then begin F_CalcError:=10; H_Error:='BETA:'; {TranslationError;} goto endp; end;
if (Trunc(q)=q) and (q<0) then begin F_CalcError:=10; H_Error:='BETA:'; {TranslationError;} goto endp; end;
try
Beta:=Gamma(p)*Gamma(q)/Gamma(p+q);
except
F_CalcError:=11; H_Error:='BETA:'; {TranslationError;}
end;

endp:
end;




function TMathLib.IntEllipt1(f,x: TFloatType): TFloatType;
var
Int: TFloatType;
n: TIntegType;
begin
try
n:=100; Px:=x;
if f > 2*pi then n:=Trunc(200*f);
Int:=Integral(_IntEllipt1,0,f,n);
IntEllipt1:=Int;
except
F_CalcError:=11; H_Error:='INT_ELLIPT1:'; {TranslationError;}
end;

end;



function TMathLib.IntEllipt2(f,x: TFloatType): TFloatType;
var
Int: TFloatType;
n: TIntegType;
begin
try
n:=100; Px:=x;
if f > 2*pi then n:=Trunc(200*f);
Int:=Integral(_IntEllipt2,0,f,n);
{if abs(Int) < 1e-18 then Int:=0;}
IntEllipt2:=Int;
except
F_CalcError:=11; H_Error:='INT_ELLIPT2:'; {TranslationError;}
end;

end;




function TMathLib.Chebyshev_P(n: TIntegType; x: TFloatType): TFloatType;
begin
if (n<0)  then begin F_CalcError:=10; H_Error:='CHEBYSHEV:'; {TranslationError;}  end
else
begin
try
Chebyshev_P:=cos(n*arccos(x));
except
F_CalcError:=11; H_Error:='CHEBYSHEV:'; {TranslationError;}
end;
end;

end;


function TMathLib.Laguerre_P(n:TIntegType; x:TFloatType):TFloatType ;
begin
if n < 0 then begin F_CalcError:=10; H_Error:='LAGUERRE:'; {ShowError;} end else
try
begin
if n = 0 then Laguerre_P:=1
else
if n = 1 then Laguerre_P:=1-x
else
Laguerre_P:=(2*n-1-x)*Laguerre_P(n-1,x)-sqr((n-1))*Laguerre_P(n-2,x);
end;
except
F_CalcError:=11; H_Error:='LAGUERRE:'; {ShowError;}
end;

end;



function TMathLib.Legendre_P(n:TIntegType; x:TFloatType):TFloatType ;
begin
if n < 0 then begin F_CalcError:=10;  H_Error:='LEGENDRE:'; {ShowError;} end else
try
begin
if n = 0 then Legendre_P:=1
else
if n = 1 then Legendre_P:=x
else
Legendre_P:=(2*n-1)/(n)*x*Legendre_P(n-1,x)-(n-1)/(n)*Legendre_P(n-2,x);
end;
except
 F_CalcError:=11;  H_Error:='LEGENDRE:'; {ShowError;}
end;

end;


function TMathLib.Hermite_P(n:TIntegType; x:TFloatType):TFloatType ;
begin
if n < 0 then begin F_CalcError:=10; H_Error:='HERMITE:'; {TranslationError;} end else
try
begin
if n = 0 then Hermite_P:=1
else
if n = 1 then Hermite_P:=2*x
else
Hermite_P:=2*x*Hermite_P(n-1,x)-2*(n-1)*Hermite_P(n-2,x);
end;
except
F_CalcError:=11; H_Error:='HERMITE:'; {TranslationError;}
end;

end;



function TMathLib.IntLaplas(x:TFloatType):TFloatType;
var
n: TIntegType;
begin
n:=200;
if x > 100 then n:= Trunc(x*2);
IntLaplas:=Integral(_IntLaplas,0,x,n);
end;


function TMathLib.Erf(x:TFloatType):TFloatType;
var
n: TIntegType;
begin
n:=200;
if x > 100 then n:= Trunc(x*2);
Erf:=Integral(_Erf,0,x,n);
end;




procedure TMathLib.LSE_Gausse(A: TArray2; B: TArray1; var B1: TArray1{; var D: TFloatType});
label endp;
var
i,j,k,N: Cardinal;
S,T,D1,x: TFloatType;
E,Z: Integer;
adrA,adrB,adrB1: TAddress;
P: Pointer;
begin
 N:=Length(A);
 SetLength(B1,N);
 dec(N);
 E:=0;
 adrA:=TAddress(A);  adrB:=TAddress(B); adrB1:=TAddress(B1);
 F_Clm:=0;
 {D1:=1;
 Z:=1;}


//DOUBLE:
 asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi


    xor   ecx,ecx
    mov   edx,adrB
@@K:
    mov   eax,adrA
    mov   ebx,[eax+4*ecx]

    fldz
    fld   qword ptr [ebx+8*ecx]
    fcompp
    fstsw ax
    sahf
    jnz   @@5
    push  edx
    lea   eax,E
    push  eax
    mov   eax,ecx
    mov   edx,ecx
    mov   K,ecx
    mov   ecx,adrA
    call  XCHSC
    //if E = 1 then ERROR        //добавить !!!
    mov   ecx,K
    mov   edx,adrB
    mov   eax,adrA
    {neg   Z}
@@5:
    fld1
    fdiv  qword ptr [ebx+8*ecx]
    fst   st(1)
    {fld   qword ptr [D1]
    fmul  qword ptr [ebx+8*ecx]
    fstp  qword ptr [D1]}
    mov   esi,ecx
@@I:
    mov   eax,adrA
    mov   eax,[eax+4*esi]
    mov   edi,ecx
@@J:
    cmp   esi,ecx
    jnz   @@1
    fmul  qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
    fst   st(1)
    jmp   @@2
@@1:
    cmp   edi,ecx
    jle   @@2
    fld   qword ptr [eax+8*ecx]
    fmul  qword ptr [ebx+8*edi]
    fsubr qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
@@2:
    inc   edi
    cmp   edi,N
    jle   @@J

    cmp   esi,ecx
    jg    @@3
    fmul  qword ptr [edx+8*esi]
    fstp  qword ptr [edx+8*esi]
    fst   st(1)
    jmp   @@4
@@3:

    fld   qword ptr [eax+8*ecx]
    fmul  qword ptr [edx+8*ecx]
    fsubr qword ptr [edx+8*esi]
    fstp  qword ptr [edx+8*esi]

@@4:
    inc   esi
    cmp   esi,N
    jle   @@I

    inc   ecx
    cmp   ecx,N
    jle   @@K

 end;


 asm
   mov  eax,adrB1
   mov  ebx,adrB
   mov  ecx,N
   mov  esi,[ebx+8*ecx]
   mov  [eax+8*ecx],esi
   mov  esi,[ebx+8*ecx+$04]
   mov  [eax+8*ecx+$04],esi
 end;



 asm

     mov   eax,adrA
     mov   ebx,adrB
     mov   ecx,adrB1
     mov   esi,N
     dec   esi
@@K:
     mov   edx,[eax+4*esi]
     fldz
     mov   edi,N
@@I:
     fld   qword ptr [edx+8*edi]
     fmul  qword ptr [ecx+8*edi]
     fadd
     dec   edi
     cmp   edi,esi
     jnl   @@I

     fsubr qword ptr [ebx+8*esi]
     fstp  qword ptr [ecx+8*esi]
     dec   esi
     jnl   @@K


     pop   edi
     pop   esi
     pop   edx
     pop   ecx
     pop   ebx
     pop   eax
 end;





//EXTENDED:
(*
 for k:=0 to N do
 begin
  if A[k,k] = 0 then
  begin
   XCHSC(k,k,A,B,E);
   if E = 1 then goto endp;
   {Z:=-Z;}
   end;
  T:=1/A[k,k];
  {D1:=D1*A[k,k];}
  for i:=k to N do
  begin
   for j:=k to N do
   begin
    if i=k then A[i,j]:=A[i,j]*T else
    if j>k then A[i,j]:=A[i,j]-A[i,k]*A[k,j];
   end;

   if i=k then B[i]:=B[i]*T else
   if i>k then B[i]:=B[i]-A[i,k]*B[k];
  end;
 end;

 B1[N]:=B[N];

 for k:=N-1 downto 0 do
 begin
  S:=0;
  for i:=N downto k do
  begin
   S:=S+A[k,i]*B1[i];
  end;
   B1[k]:=B[k]-S;
 end;
*)


{D:=D1*Z;}

if F_Clm = 1 then
begin
 for i:=0 to High(G_TC) do
 begin
  x:=B1[G_TC[i].I1];
  B1[G_TC[i].I1]:=B1[G_TC[i].I2];
  B1[G_TC[i].I2]:=x;
 end;
end;

endp:
end;



procedure TMathLib.LSE_Jordan(A: TArray2; B: TArray1; var B1: TArray1{; var D: TFloatType});
label endp;
var
N,i,j,k,N1,k1: TIntegType;
T,T1,T2,D1,x: TFloatType;
E,Z: Integer;
adrA,adrB: TAddress;
begin
E:=0;
N:=High(A);
adrA:=TAddress(A);
adrB:=TAddress(B);
F_Clm:=0;
{Z:=1;
D1:=1;}



//DOUBLE:
asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    xor   ecx,ecx
    mov   edx,adrB
@@K:
    mov   eax,adrA
    mov   ebx,[eax+4*ecx]

    fldz
    fld   qword ptr [ebx+8*ecx]
    fcompp
    fstsw ax
    sahf
    jnz   @@2
    push  edx
    lea   eax,E
    push  eax
    mov   eax,ecx
    mov   edx,ecx
    mov   K,ecx
    mov   ecx,adrA
    call  XCHSC
    //if E = 1 then ERROR        //добавить !!!
    mov   ecx,K
    mov   edx,adrB
    mov   eax,adrA
    {neg   Z}

@@2:
    {fld   qword ptr [D1]
    fmul  qword ptr [ebx+8*ecx]
    fstp  qword ptr [D1]}
    fld1
    fdiv  qword ptr [ebx+8*ecx]
    fst   st(1)
    mov   edi,ecx
@@J:
    fmul  qword ptr [ebx+8*edi]
    fstp  qword ptr [ebx+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J

    fmul  qword ptr [edx+8*ecx]
    fstp  qword ptr [edx+8*ecx]
    //ffree st(0)


    xor   esi,esi
@@I:
    cmp   esi,ecx
    jz    @@1
    mov   eax,adrA
    mov   eax,[eax+4*esi]
    fld   qword ptr [eax+8*ecx]
    fst   st(1)
    mov   edi,ecx
@@J1:
    fmul  qword ptr [ebx+8*edi]
    fsubr qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J1

    fmul  qword ptr [edx+8*ecx]
    fsubr qword ptr [edx+8*esi]
    fstp  qword ptr [edx+8*esi]
    //ffree st(0)
@@1:

    inc   esi
    cmp   esi,N
    jle   @@I

    inc   ecx
    cmp   ecx,N
    jle   @@K

    pop   edi
    pop   esi
    pop   edx
    pop   ecx
    pop   ebx
    pop   eax
end;




//EXTENDED:
(*
for k:=0 to N do
begin

 if A[k,k] = 0 then
 begin
  XCHSC(k,k,A,B,E);
  if E = 1 then goto endp;
  {Z:=-Z;}
 end;

 T:=1/A[k,k];

 {D1:=D1*A[k,k];}

 for j:=k to N do
  begin
    A[k,j]:=A[k,j]*T;
  end;

  B[k]:=B[k]*T;



 for i:=0 to N do
 begin

  if i <> k then
  begin

   T1:=A[i,k];

   for j:=k to N do
   begin
    A[i,j]:=A[i,j]-A[k,j]*T1;
   end;

    B[i]:=B[i]-B[k]*T1;

  end;
 end;
end;
*)

{D:=D1*Z;}

if F_Clm = 1 then
begin
 for i:=0 to High(G_TC) do
 begin
  x:=B[G_TC[i].I1];
  B[G_TC[i].I1]:=B[G_TC[i].I2];
  B[G_TC[i].I2]:=x;
 end;
end;

B1:=B;
endp:
end;



procedure TMathLib.LSEDet_Gausse(A: TArray2; B: TArray1; var B1: TArray1; var D: TFloatType);
label endp;
var
i,j,k,N: Cardinal;
S,T,D1,x: TFloatType;
E,Z: Integer;
adrA,adrB,adrB1: TAddress;
P: Pointer;
begin
 N:=Length(A);
 SetLength(B1,N);
 dec(N);
 E:=0;
 adrA:=TAddress(A);  adrB:=TAddress(B); adrB1:=TAddress(B1);
 F_Clm:=0;
 D1:=1;
 Z:=1;


//DOUBLE:
 asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi


    xor   ecx,ecx
    mov   edx,adrB
@@K:
    mov   eax,adrA
    mov   ebx,[eax+4*ecx]

    fldz
    fld   qword ptr [ebx+8*ecx]
    fcompp
    fstsw ax
    sahf
    jnz   @@5
    push  edx
    lea   eax,E
    push  eax
    mov   eax,ecx
    mov   edx,ecx
    mov   K,ecx
    mov   ecx,adrA
    call  XCHSC
    //if E = 1 then ERROR        //добавить !!!
    mov   ecx,K
    mov   edx,adrB
    mov   eax,adrA
    neg   Z
@@5:
    fld1
    fdiv  qword ptr [ebx+8*ecx]
    fst   st(1)
    fld   qword ptr [D1]
    fmul  qword ptr [ebx+8*ecx]
    fstp  qword ptr [D1]
    mov   esi,ecx
@@I:
    mov   eax,adrA
    mov   eax,[eax+4*esi]
    mov   edi,ecx
@@J:
    cmp   esi,ecx
    jnz   @@1
    fmul  qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
    fst   st(1)
    jmp   @@2
@@1:
    cmp   edi,ecx
    jle   @@2
    fld   qword ptr [eax+8*ecx]
    fmul  qword ptr [ebx+8*edi]
    fsubr qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
@@2:
    inc   edi
    cmp   edi,N
    jle   @@J

    cmp   esi,ecx
    jg    @@3
    fmul  qword ptr [edx+8*esi]
    fstp  qword ptr [edx+8*esi]
    fst   st(1)
    jmp   @@4
@@3:

    fld   qword ptr [eax+8*ecx]
    fmul  qword ptr [edx+8*ecx]
    fsubr qword ptr [edx+8*esi]
    fstp  qword ptr [edx+8*esi]

@@4:
    inc   esi
    cmp   esi,N
    jle   @@I

    inc   ecx
    cmp   ecx,N
    jle   @@K

 end;


 asm
   mov  eax,adrB1
   mov  ebx,adrB
   mov  ecx,N
   mov  esi,[ebx+8*ecx]
   mov  [eax+8*ecx],esi
   mov  esi,[ebx+8*ecx+$04]
   mov  [eax+8*ecx+$04],esi
 end;



 asm

     mov   eax,adrA
     mov   ebx,adrB
     mov   ecx,adrB1
     mov   esi,N
     dec   esi
@@K:
     mov   edx,[eax+4*esi]
     fldz
     mov   edi,N
@@I:
     fld   qword ptr [edx+8*edi]
     fmul  qword ptr [ecx+8*edi]
     fadd
     dec   edi
     cmp   edi,esi
     jnl   @@I

     fsubr qword ptr [ebx+8*esi]
     fstp  qword ptr [ecx+8*esi]
     dec   esi
     jnl   @@K


     pop   edi
     pop   esi
     pop   edx
     pop   ecx
     pop   ebx
     pop   eax
 end;





//EXTENDED:
(*
 for k:=0 to N do
 begin
  if A[k,k] = 0 then
  begin
   XCHSC(k,k,A,B,E);
   if E = 1 then goto endp;
   Z:=-Z;
   end;
  T:=1/A[k,k];
  D1:=D1*A[k,k];
  for i:=k to N do
  begin
   for j:=k to N do
   begin
    if i=k then A[i,j]:=A[i,j]*T else
    if j>k then A[i,j]:=A[i,j]-A[i,k]*A[k,j];
   end;

   if i=k then B[i]:=B[i]*T else
   if i>k then B[i]:=B[i]-A[i,k]*B[k];
  end;
 end;

 B1[N]:=B[N];

 for k:=N-1 downto 0 do
 begin
  S:=0;
  for i:=N downto k do
  begin
   S:=S+A[k,i]*B1[i];
  end;
   B1[k]:=B[k]-S;
 end;
*)


D:=D1*Z;

if F_Clm = 1 then
begin
 for i:=0 to High(G_TC) do
 begin
  x:=B1[G_TC[i].I1];
  B1[G_TC[i].I1]:=B1[G_TC[i].I2];
  B1[G_TC[i].I2]:=x;
 end;
end;

endp:
end;




procedure TMathLib.LSEDet_Jordan(A: TArray2; B: TArray1; var B1: TArray1; var D: TFloatType);
label endp;
var
N,i,j,k,N1,k1: TIntegType;
T,T1,T2,D1,x: TFloatType;
E,Z: Integer;
adrA,adrB: TAddress;
begin
E:=0;
N:=High(A);
adrA:=TAddress(A);
adrB:=TAddress(B);
F_Clm:=0;
Z:=1;
D1:=1;



//DOUBLE:
asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    xor   ecx,ecx
    mov   edx,adrB
@@K:
    mov   eax,adrA
    mov   ebx,[eax+4*ecx]

    fldz
    fld   qword ptr [ebx+8*ecx]
    fcompp
    fstsw ax
    sahf
    jnz   @@2
    push  edx
    lea   eax,E
    push  eax
    mov   eax,ecx
    mov   edx,ecx
    mov   K,ecx
    mov   ecx,adrA
    call  XCHSC
    //if E = 1 then ERROR        //добавить !!!
    mov   ecx,K
    mov   edx,adrB
    mov   eax,adrA
    neg   Z

@@2:
    fld   qword ptr [D1]
    fmul  qword ptr [ebx+8*ecx]
    fstp  qword ptr [D1]
    fld1
    fdiv  qword ptr [ebx+8*ecx]
    fst   st(1)
    mov   edi,ecx
@@J:
    fmul  qword ptr [ebx+8*edi]
    fstp  qword ptr [ebx+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J

    fmul  qword ptr [edx+8*ecx]
    fstp  qword ptr [edx+8*ecx]
    //ffree st(0)


    xor   esi,esi
@@I:
    cmp   esi,ecx
    jz    @@1
    mov   eax,adrA
    mov   eax,[eax+4*esi]
    fld   qword ptr [eax+8*ecx]
    fst   st(1)
    mov   edi,ecx
@@J1:
    fmul  qword ptr [ebx+8*edi]
    fsubr qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J1

    fmul  qword ptr [edx+8*ecx]
    fsubr qword ptr [edx+8*esi]
    fstp  qword ptr [edx+8*esi]
    //ffree st(0)
@@1:

    inc   esi
    cmp   esi,N
    jle   @@I

    inc   ecx
    cmp   ecx,N
    jle   @@K

    pop   edi
    pop   esi
    pop   edx
    pop   ecx
    pop   ebx
    pop   eax
end;




//EXTENDED:
(*
for k:=0 to N do
begin

 if A[k,k] = 0 then
 begin
  XCHSC(k,k,A,B,E);
  if E = 1 then goto endp;
  Z:=-Z;
 end;

 T:=1/A[k,k];

 D1:=D1*A[k,k];

 for j:=k to N do
  begin
    A[k,j]:=A[k,j]*T;
  end;

  B[k]:=B[k]*T;



 for i:=0 to N do
 begin

  if i <> k then
  begin

   T1:=A[i,k];

   for j:=k to N do
   begin
    A[i,j]:=A[i,j]-A[k,j]*T1;
   end;

    B[i]:=B[i]-B[k]*T1;

  end;
 end;
end;
*)

D:=D1*Z;

if F_Clm = 1 then
begin
 for i:=0 to High(G_TC) do
 begin
  x:=B[G_TC[i].I1];
  B[G_TC[i].I1]:=B[G_TC[i].I2];
  B[G_TC[i].I2]:=x;
 end;
end;

B1:=B;
endp:
end;




procedure TMathLib.LSE(A: TArray2;  B: TArray1; var B1: TArray1);
begin
 if F_LSEType = 0 then LSE_Jordan(A,B,B1) else
 if F_LSEType = 1 then LSE_Gausse(A,B,B1);
end;



procedure TMathLib.LSEDet(A: TArray2;  B: TArray1; var B1: TArray1; var D: TFloatType);
begin
 if F_LSEDetType = 0 then LSEDet_Jordan(A,B,B1,D) else
 if F_LSEDetType = 1 then LSEDet_Gausse(A,B,B1,D);
end;





procedure TMathLib.MatrixDeterminant_Gausse(A: TArray2;  var D: TFloatType);
label endp;
var
i,j,k,N: Cardinal;
S,T,D1: TFloatType;
E,Z: Integer;
adrA,adrB,adrB1: TAddress;
P: Pointer;
begin
 N:=Length(A);
 dec(N);
 E:=0;
 adrA:=TAddress(A);
 

 D1:=1;
 Z:=1;


//DOUBLE:
 asm
    push  eax
    push  ebx
    push  ecx
    push  esi
    push  edi


    xor   ecx,ecx

@@K:
    mov   eax,adrA
    mov   ebx,[eax+4*ecx]

    fldz
    fld   qword ptr [ebx+8*ecx]
    fcompp
    fstsw ax
    sahf
    jnz   @@5
    lea   eax,E
    push  eax
    mov   eax,ecx
    mov   edx,ecx
    mov   K,ecx
    mov   ecx,adrA
    call  XCHSC2
    cmp   E,1
    jz    @@End
    //if E = 1 then ERROR        //добавить !!!
    mov   ecx,K
    mov   eax,adrA
    neg   Z
@@5:
    fld1
    fdiv  qword ptr [ebx+8*ecx]
    fst   st(1)
    fld   qword ptr [D1]
    fmul  qword ptr [ebx+8*ecx]
    fstp  qword ptr [D1]
    mov   esi,ecx
@@I:
    mov   eax,adrA
    mov   eax,[eax+4*esi]
    mov   edi,ecx
@@J:
    cmp   esi,ecx
    jnz   @@1
    fmul  qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
    fst   st(1)
    jmp   @@2
@@1:
    cmp   edi,ecx
    jle   @@2
    fld   qword ptr [eax+8*ecx]
    fmul  qword ptr [ebx+8*edi]
    fsubr qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
@@2:
    inc   edi
    cmp   edi,N
    jle   @@J

    inc   esi
    cmp   esi,N
    jle   @@I

    inc   ecx
    cmp   ecx,N
    jle   @@K

@@End:
    pop   edi
    pop   esi
    pop   ecx
    pop   ebx
    pop   eax
 end;





//EXTENDED:
{
 for k:=0 to N do
 begin
  if A[k,k] = 0 then
  begin
   XCHSC2(k,k,A,E);
   if E = 1 then goto endp;
   Z:=-Z;
   end;
  T:=1/A[k,k];
  D1:=D1*A[k,k];
  for i:=k to N do
  begin
   for j:=k to N do
   begin
    if i=k then A[i,j]:=A[i,j]*T else
    if j>k then A[i,j]:=A[i,j]-A[i,k]*A[k,j];
   end;
  end;
 end;
}

if E = 1 then D1:=0;
D:=D1*Z;


endp:
end;






procedure TMathLib.MatrixDeterminant_Jordan(A: TArray2;  var D: TFloatType);
label endp;
var
N,i,j,k,N1,k1: TIntegType;
T,T1,T2,D1: TFloatType;
E,Z: Integer;
adrA: TAddress;
begin
E:=0;
N:=High(A);
adrA:=TAddress(A);
Z:=1;
D1:=1;



//DOUBLE:
asm
    push  eax
    push  ebx
    push  ecx

    push  esi
    push  edi

    xor   ecx,ecx

@@K:
    mov   eax,adrA
    mov   ebx,[eax+4*ecx]

    fldz
    fld   qword ptr [ebx+8*ecx]
    fcompp
    fstsw ax
    sahf
    jnz   @@2
    lea   eax,E
    push  eax
    mov   eax,ecx
    mov   edx,ecx
    mov   K,ecx
    mov   ecx,adrA
    call  XCHSC2
    cmp   E,1
    jz    @@End
    //if E = 1 then ERROR        //добавить !!!
    mov   ecx,K
    mov   eax,adrA
    neg   Z

@@2:
    fld   qword ptr [D1]
    fmul  qword ptr [ebx+8*ecx]
    fstp  qword ptr [D1]
    fld1
    fdiv  qword ptr [ebx+8*ecx]
    fst   st(1)
    mov   edi,ecx
@@J:
    fmul  qword ptr [ebx+8*edi]
    fstp  qword ptr [ebx+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J


    //ffree st(0)


    xor   esi,esi
@@I:
    cmp   esi,ecx
    jz    @@1
    mov   eax,adrA
    mov   eax,[eax+4*esi]
    fld   qword ptr [eax+8*ecx]
    fst   st(1)
    mov   edi,ecx
@@J1:
    fmul  qword ptr [ebx+8*edi]
    fsubr qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J1


    //ffree st(0)
@@1:

    inc   esi
    cmp   esi,N
    jle   @@I

    inc   ecx
    cmp   ecx,N
    jle   @@K

@@End:
    pop   edi
    pop   esi
    pop   ecx
    pop   ebx
    pop   eax
end;







//EXTENDED:
{
for k:=0 to N do
begin

 if A[k,k] = 0 then
 begin
  XCHSC2(k,k,A,E);
  if E = 1 then goto endp;
  Z:=-Z;
 end;

 T:=1/A[k,k];

 D1:=D1*A[k,k];

 for j:=k to N do
  begin
    A[k,j]:=A[k,j]*T;
  end;


 for i:=0 to N do
 begin

  if i <> k then
  begin

   T1:=A[i,k];

   for j:=k to N do
   begin
    A[i,j]:=A[i,j]-A[k,j]*T1;
   end;


  end;
 end;
end;
}

if E = 1 then D1:=0;
D:=D1*Z;

endp:
end;














procedure TMathLib.MatrixInvert(A: TArray2; var B: TArray2 {; var D: TFloatType});
label 1,endp;
var
N,i,j,k,M1: TIntegType;
T,T1,T2: TFloatType;
E,Z: Integer;
adrA,adrB: TAddress;
begin
E:=0;

N:=High(A);
{Z:=1;
D:=1;}

SetLength(B,N+1,N+1);

for i:=0 to N do
begin
for j:=0 to N do
begin
 if (i = j) then B[i,j]:=1 else
 B[i,j]:=0;
end;
end;


//DOUBLE:
adrA:=TAddress(A);
adrB:=TAddress(B);

asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    xor   ecx,ecx
@@K:
    mov   eax,adrA
    mov   eax,[eax+4*ecx]

    fldz
    fld   qword ptr [eax+8*ecx]
    fcompp
    fstsw ax
    sahf
    jnz   @@2
    mov   edx,adrB
    push  edx
    lea   eax,E
    push  eax
    mov   eax,ecx
    mov   edx,ecx
    mov   K,ecx
    mov   ecx,adrA
    call  XCHSC1
    //if E = 1 then ERROR        //добавить !!!
    mov   ecx,K
    {neg   Z}

@@2:
    mov   eax,adrA
    mov   eax,[eax+4*ecx]
    {fld   qword ptr [D]
    fmul  qword ptr [eax+8*ecx]
    fstp  qword ptr [D]}
    fld1
    fdiv  qword ptr [eax+8*ecx]
    fst   st(1)

    mov   eax,adrA
    mov   eax,[eax+4*ecx]
    mov   edi,ecx
@@J1:
    fmul  qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J1

    mov   eax,adrB
    mov   eax,[eax+4*ecx]
    xor   edi,edi
@@J2:
    fmul  qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J2

    //ffree st(0)

    xor   esi,esi
@@I:
    mov   eax,adrA
    mov   eax,[eax+4*esi]
    mov   ebx,adrB
    mov   ebx,[ebx+4*esi]

    cmp   esi,ecx
    jz    @@1
    fld   qword ptr [eax+8*ecx]
    fst   st(1)

    mov   edx,adrA
    mov   edx,[edx+4*ecx]
    mov   edi,ecx
@@J3:
    fmul  qword ptr [edx+8*edi]
    fsubr qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J3

    mov   edx,adrB
    mov   edx,[edx+4*ecx]
    xor   edi,edi
@@J4:
    fmul  qword ptr [edx+8*edi]
    fsubr qword ptr [ebx+8*edi]
    fstp  qword ptr [ebx+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J4

    //ffree st(0)

@@1:
    inc   esi
    cmp   esi,N
    jle   @@I

    inc   ecx
    cmp   ecx,N
    jle   @@K


    pop   edi
    pop   esi
    pop   edx
    pop   ecx
    pop   ebx
    pop   eax
end;



//EXTENDED:
(*
for k:=0 to N do
begin


 if A[k,k] = 0 then
 begin
  XCHSC1(k,k,A,B,E);
  if E = 1 then goto endp;
  {Z:=-Z;}
 end;

 T:=1/A[k,k];

 {D:=D*A[k,k];}

 for j:=k to N do
  begin
    A[k,j]:=A[k,j]*T;
  end;

 for j:=0 to N do
 begin
   B[k,j]:=B[k,j]*T;
 end;





 for i:=0 to N do
 begin

  if i <> k then
  begin

   T1:=A[i,k];

   for j:=k to N do
   begin
    A[i,j]:=A[i,j]-A[k,j]*T1;
   end;

   for j:=0 to N do
   begin
    B[i,j]:=B[i,j]-B[k,j]*T1;
   end;


  end;
 end;
end;
*)

{D:=D*Z;}

endp:
end;





procedure TMathLib.MatrixInvertDet(A: TArray2; var B: TArray2 ; var D: TFloatType);
label 1,endp;
var
N,i,j,k,M1: TIntegType;
T,T1,T2,D1: TFloatType;
E,Z: Integer;
adrA,adrB: TAddress;
begin
E:=0;

N:=High(A);
Z:=1;
D1:=1;

SetLength(B,N+1,N+1);

for i:=0 to N do
begin
for j:=0 to N do
begin
 if (i = j) then B[i,j]:=1 else
 B[i,j]:=0;
end;
end;


//DOUBLE:
adrA:=TAddress(A);
adrB:=TAddress(B);

asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    xor   ecx,ecx
@@K:
    mov   eax,adrA
    mov   eax,[eax+4*ecx]

    fldz
    fld   qword ptr [eax+8*ecx]
    fcompp
    fstsw ax
    sahf
    jnz   @@2
    mov   edx,adrB
    push  edx
    lea   eax,E
    push  eax
    mov   eax,ecx
    mov   edx,ecx
    mov   K,ecx
    mov   ecx,adrA
    call  XCHSC1
    //if E = 1 then ERROR        //добавить !!!
    mov   ecx,K
    neg   Z

@@2:
    mov   eax,adrA
    mov   eax,[eax+4*ecx]
    fld   qword ptr [D1]
    fmul  qword ptr [eax+8*ecx]
    fstp  qword ptr [D1]
    fld1
    fdiv  qword ptr [eax+8*ecx]
    fst   st(1)

    mov   eax,adrA
    mov   eax,[eax+4*ecx]
    mov   edi,ecx
@@J1:
    fmul  qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J1

    mov   eax,adrB
    mov   eax,[eax+4*ecx]
    xor   edi,edi
@@J2:
    fmul  qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J2

    //ffree st(0)

    xor   esi,esi
@@I:
    mov   eax,adrA
    mov   eax,[eax+4*esi]
    mov   ebx,adrB
    mov   ebx,[ebx+4*esi]

    cmp   esi,ecx
    jz    @@1
    fld   qword ptr [eax+8*ecx]
    fst   st(1)

    mov   edx,adrA
    mov   edx,[edx+4*ecx]
    mov   edi,ecx
@@J3:
    fmul  qword ptr [edx+8*edi]
    fsubr qword ptr [eax+8*edi]
    fstp  qword ptr [eax+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J3

    mov   edx,adrB
    mov   edx,[edx+4*ecx]
    xor   edi,edi
@@J4:
    fmul  qword ptr [edx+8*edi]
    fsubr qword ptr [ebx+8*edi]
    fstp  qword ptr [ebx+8*edi]
    fst   st(1)
    inc   edi
    cmp   edi,N
    jle   @@J4

    //ffree st(0)

@@1:
    inc   esi
    cmp   esi,N
    jle   @@I

    inc   ecx
    cmp   ecx,N
    jle   @@K


    pop   edi
    pop   esi
    pop   edx
    pop   ecx
    pop   ebx
    pop   eax
end;



//EXTENDED:
(*
for k:=0 to N do
begin


 if A[k,k] = 0 then
 begin
  XCHSC1(k,k,A,B,E);
  if E = 1 then goto endp;
  Z:=-Z;
 end;

 T:=1/A[k,k];

 D1:=D1*A[k,k];

 for j:=k to N do
  begin
    A[k,j]:=A[k,j]*T;
  end;

 for j:=0 to N do
 begin
   B[k,j]:=B[k,j]*T;
 end;





 for i:=0 to N do
 begin

  if i <> k then
  begin

   T1:=A[i,k];

   for j:=k to N do
   begin
    A[i,j]:=A[i,j]-A[k,j]*T1;
   end;

   for j:=0 to N do
   begin
    B[i,j]:=B[i,j]-B[k,j]*T1;
   end;


  end;
 end;
end;
*)

D:=D1*Z;

endp:
end;




procedure TMathLib.MatrixPolynom_Faddeev(A: TArray2; var P: TArray1);
var
N,i,j: Cardinal;
B,G,E: TArray2;
begin
N:=High(A);
SetLength(B,N+1,N+1);
SetLength(P,N+2);

for i:=0 to N do
for j:=0 to N do
B[i,j]:=A[i,j];

P[0]:=1;
P[1]:=-MatrixTrace(B);

for i:=2 to N+1 do
begin
  GetMatrixE(P[i-1],N+1,E);
  MatrixAdd(E,B,G);
  MatrixMul(G,A,B);
  P[i]:=-MatrixTrace(B)/(i);
end;

end;



procedure TMathLib.MatrixPolynom_Leverie(A: TArray2; var P: TArray1);
var
N,i,j: Cardinal;
B: TArray2;
S: TArray1;
T: TFloatType;
begin
N:=High(A);
SetLength(B,N+1,N+1);
SetLength(S,N+2);
SetLength(P,N+2);

for i:=0 to N do
for j:=0 to N do
B[i,j]:=A[i,j];

for i:=1 to N+1 do
begin
 S[i]:=MatrixTrace(A);
 MatrixMul(B,A,A);
end;

P[0]:=1;
P[1]:=-S[1];


for i:=2 to N+1 do
begin
  T:=0;
  for j:=1 to i-1 do
  begin
   T:=T+S[j]*P[i-j];
  end;
  P[i]:=-(T+S[i])/i;
end;

end;




procedure TMathLib.MatrixPolynom(A: TArray2; var P: TArray1);
begin
if F_MatrixPolynom = 0 then MatrixPolynom_Leverie(A,P) else
if F_MatrixPolynom = 1 then MatrixPolynom_Faddeev(A,P);
end;



procedure TMathLib.MatrixPolynomInv(A: TArray2; var P: TArray1; var C: TArray2);
var
N,i,j: Cardinal;
B,G,E: TArray2;
x: TFloatType;
begin
N:=High(A);
SetLength(B,N+1,N+1);
SetLength(P,N+2);

for i:=0 to N do
for j:=0 to N do
B[i,j]:=A[i,j];

P[0]:=1;
P[1]:=-MatrixTrace(B);

for i:=2 to N+1 do
begin
  GetMatrixE(P[i-1],N+1,E);
  MatrixAdd(E,B,G);
  MatrixMul(G,A,B);
  P[i]:=-MatrixTrace(B)/(i);
end;

x:=-1/P[N+1];
MatrixMulConst(G,x);
C:=G;
end;



procedure TMathLib.MatrixVectorSL(A: TArray2; L: TFloatType; var V: TArray1);
var
N,i,j: Cardinal;
B,E,C: TArray2;
D: TFloatType;
begin
//нахождение собств. вектора соотв. простому собств. числу - L
N:=High(A);
SetLength(V,N+1);

for i:=0 to N do
begin
  GetMatrixE(L,N+1,E);
  MatrixSub(E,A,B);
  MatrixMinor(B,0,i,C);
  MatrixDeterminant(C,D);
  V[i]:=IntPower(-1,i)*D;
end;

end;





procedure TMathLib.GetMatrixG(A: TArray2; L: TFloatType; K: Cardinal;  var G: TArray2);
var
N,i,j,m: Cardinal;                    //Gk(L)  (k - производная)
B,G1,E,GL: TArray2;                   //G(L) -приоединённая матрица к |A-L*E|
P: TArray1;
L1: TFloatType;
begin

N:=High(A);
SetLength(B,N+1,N+1);
SetLength(P,N+2);

L1:=IntPower(L,N-K);
if K <> 0 then L1:=L1*Factorial(N)/Factorial(N-K);
GetMatrixE(L1,N+1,G);
GetMatrixE(1,N+1,G1);

for i:=0 to N do
for j:=0 to N do
B[i,j]:=A[i,j];

P[0]:=1;
P[1]:=-MatrixTrace(B);

for i:=2 to N+1 do
begin
  GetMatrixE(P[i-1],N+1,E);
  MatrixAdd(E,B,G1);
  if (N+1-K) >= i then
  begin
   L1:=IntPower(L,N-i+1-K);
   if K <> 0 then L1:=L1*Factorial(N-i+1)/Factorial(N-i+1-K);
   MatrixMulConstAdd(G,G1,L1,G);
  end;
  MatrixMul(G1,A,B);
  P[i]:=-MatrixTrace(B)/(i);
end;

end;



procedure TMathLib.MatrixDeterminant(A: TArray2;  var D: TFloatType);
begin
 if F_DetType = 1 then MatrixDeterminant_Jordan(A,D) else
 if F_DetType = 0 then MatrixDeterminant_Gausse(A,D);
end;




procedure TMathLib.ExchangeClm(A: TArray2; i,j: Cardinal);
var
l,S: Cardinal;
x: TFloatType;
adrA: TAddress;
begin
 S:=High(A);

//DOUBLE:
 adrA:=TAddress(A);
 asm
  push  eax
  push  ebx
  push  ecx
  push  edx
  push  edi
  push  esi

  mov   eax,adrA
  mov   edi,i
  mov   esi,j
  mov   ecx,S
@@L:
  mov   eax,adrA
  mov   ebx,[eax+4*ecx]

  mov   eax,[ebx+8*esi]
  mov   edx,[ebx+8*edi]
  mov   [ebx+8*edi],eax
  mov   [ebx+8*esi],edx

  mov   eax,[ebx+8*esi+$04]
  mov   edx,[ebx+8*edi+$04]
  mov   [ebx+8*edi+$04],eax
  mov   [ebx+8*esi+04],edx
  loop  @@L


  mov   eax,adrA
  mov   ebx,[eax]
  mov   eax,[ebx+8*esi]
  mov   edx,[ebx+8*edi]
  mov   [ebx+8*edi],eax
  mov   [ebx+8*esi],edx

  mov   eax,[ebx+8*esi+$04]
  mov   edx,[ebx+8*edi+$04]
  mov   [ebx+8*edi+$04],eax
  mov   [ebx+8*esi+04],edx

  pop   esi
  pop   edi
  pop   edx
  pop   ecx
  pop   ebx
  pop   eax

 end;

//EXTENDED:
{
 for l:=0 to S do
 begin
  x:=A[l,i]; A[l,i]:=A[l,j]; A[l,j]:=x;
 end;
}
end;



procedure TMathLib.ExchangeStr(A: TArray2; i,j: Cardinal);
var
l,C: Cardinal;
x: TFloatType;
adrA: TAddress;
begin
 C:=High(A[0]);

//DOUBLE:
 adrA:=TAddress(A);
 asm
  push  eax
  push  ebx
  push  ecx
  push  edi
  push  esi

  mov   eax,adrA
  mov   ebx,adrA
  mov   ecx,i
  mov   eax,[eax+4*ecx]
  mov   ecx,j
  mov   ebx,[ebx+4*ecx]
  mov   ecx,C
@@L:
  mov   esi,[eax+8*ecx]
  mov   edi,[ebx+8*ecx]
  mov   [eax+8*ecx],edi
  mov   [ebx+8*ecx],esi

  mov   esi,[eax+8*ecx+$04]
  mov   edi,[ebx+8*ecx+$04]
  mov   [eax+8*ecx+$04],edi
  mov   [ebx+8*ecx+$04],esi
  loop  @@L




  mov   esi,[eax+8*ecx]
  mov   edi,[ebx+8*ecx]
  mov   [eax+8*ecx],edi
  mov   [ebx+8*ecx],esi

  mov   esi,[eax+8*ecx+$04]
  mov   edi,[ebx+8*ecx+$04]
  mov   [eax+8*ecx+$04],edi
  mov   [ebx+8*ecx+04],esi


  pop   esi
  pop   edi
  pop   ecx
  pop   ebx
  pop   eax

 end;

//EXTENDED:
 {
 for l:=0 to C do
 begin
  x:=A[i,l]; A[i,l]:=A[j,l]; A[j,l]:=x;
 end;
 }
end;





procedure TMathLib.MatrixMulConst(A: TArray2; x: TFloatType);
var
S,C,i,j: Cardinal;
adrA: TAddress;
begin
 S:=High(A); C:=High(A[0]);

//DOUBLE:
adrA:=TAddress(A);
asm
   push  eax
   push  ebx
   push  ecx
   push  edx
   push  esi
   push  edi

   mov   eax,adrA
   mov   ebx,C
   mov   ecx,S
   xor   esi,esi
   fld   qword ptr [x]
   fst   st(1)
@@I:
   mov   edx,[eax+4*esi]
   xor   edi,edi
@@J:
   fmul  qword ptr [edx+8*edi]
   fstp  qword ptr [edx+8*edi]
   fst   st(1)
   inc   edi
   cmp   edi,ebx
   jle   @@J
   inc   esi
   cmp   esi,ecx
   jle   @@I

   ffree st(0)
   pop   edi
   pop   esi
   pop   edx
   pop   ecx
   pop   ebx
   pop   eax
end;

//EXTENDED:
{
 for i:=0 to S do
 for j:=0 to C do
 A[i,j]:=A[i,j]*x;
}
end;




procedure TMathLib.MatrixMulConstAdd(A,B: TArray2; x: TFloatType; var C: TArray2);
var
S1,C1,i,j: Cardinal;
adrA,adrB,adrC: TAddress;                //C = A+B*x
begin
 S1:=High(A); C1:=High(A[0]);
 SetLength(C,S1+1,C1+1);
//DOUBLE:

adrA:=TAddress(A);  adrB:=TAddress(B); adrC:=TAddress(C);
asm
   push  eax
   push  ebx
   push  ecx
   push  esi
   push  edi


   xor   esi,esi
   fld   qword ptr [x]
   fst   st(1)
@@I:
   mov   eax,adrA
   mov   eax,[eax+4*esi]
   mov   ebx,adrB
   mov   ebx,[ebx+4*esi]
   mov   ecx,adrC
   mov   ecx,[ecx+4*esi]

   xor   edi,edi
@@J:
   fmul  qword ptr [ebx+8*edi]
   fadd  qword ptr [eax+8*edi]
   fstp  qword ptr [ecx+8*edi]
   fst   st(1)
   inc   edi
   cmp   edi,C1
   jle   @@J
   inc   esi
   cmp   esi,S1
   jle   @@I

   ffree st(0)
   pop   edi
   pop   esi
   pop   ecx
   pop   ebx
   pop   eax
end;

//EXTENDED:
{
 for i:=0 to S1 do
 for j:=0 to C1 do
 C[i,j]:=A[i,j]+B[i,j]*x;
}
end;




function TMathLib.MatrixTrace(A: TArray2): TFloatType;
var
N,i: Cardinal;
t: TFloatType;
adrA: TAddress;
begin
N:=High(A);
t:=0;
adrA:=TAddress(A);

//DOUBLE:

asm
   push   eax
   push   ebx
   push   ecx
   push   edx
   mov    ebx,adrA
   mov    ecx,N
   inc    ecx
   xor    edx,edx
   fldz
@@I:
   mov    eax,[ebx+4*edx]
   fadd   qword ptr [eax+8*edx]
   inc    edx
   loop   @@I
   fstp   t
   pop    edx
   pop    ecx
   pop    ebx
   pop    eax
end;

//EXTENDED:
{
for i:=0 to N do
t:=t+A[i,i];
}

MatrixTrace:=t;
end;







procedure TMathLib.MatrixTranspose(A: TArray2);
var
S,C,i,j: Cardinal;
adrA: TAddress;
x: TFloatType;
begin
S:=High(A);

//DOUBLE:
adrA:=TAddress(A);
asm
  push  eax
  push  ebx
  push  ecx
  push  edx
  push  esi
  push  edi

  xor   esi,esi
@@I:
  mov   eax,adrA
  mov   eax,[eax+4*esi]
  mov   edi,esi
@@J:
  mov   ebx,adrA
  mov   ebx,[ebx+4*edi]

  mov   ecx,[eax+8*edi]
  mov   edx,[ebx+8*esi]
  mov   [eax+8*edi],ecx
  mov   [ebx+8*esi],edx

  mov   ecx,[eax+8*edi+$04]
  mov   edx,[ebx+8*esi+$04]
  mov   [eax+8*edi+$04],edx
  mov   [ebx+8*esi+$04],ecx

  inc   edi
  cmp   edi,S
  jle   @@J

  inc   esi
  cmp   esi,S
  jle   @@I

  pop   edi
  pop   esi
  pop   edi
  pop   ecx
  pop   ebx
  pop   eax
end;

//EXTENDED:
{
for i:=0 to S do
begin
 for j:=i to S do
 begin
 x:=A[i,j]; A[i,j]:=A[j,i]; A[j,i]:=x;
 end;
end;
}
end;



procedure TMathLib.GetMatrixLR(A: TArray2; var L,R: TArray2);
label endp;
var
i,j,k,N: Cardinal;
S,T: TFloatType;
E,Z: Integer;
begin                         //A -> R !!!  (в рез-те преобразования)
 N:=Length(A);
 SetLength(L,N,N);
 dec(N);
 E:=0;



 for i:=0 to N do
 for j:=0 to N do
 begin
 L[i,j]:=0;
 end;



 for k:=0 to N do
 begin
  if A[k,k] = 0 then
  begin
   XCHSC2(k,k,A,E);
   if E = 1 then goto endp;
   end;
  L[k,k]:=A[k,k];
  T:=1/A[k,k];
  for i:=k to N do
  begin
   for j:=k to N do
   begin
    if i=k then A[i,j]:=A[i,j]*T else
    if j>k then A[i,j]:=A[i,j]-A[i,k]*A[k,j];
   end;
  end;
 end;


 for i:=0 to N do
 for j:=0 to N do
 begin
 if i > j then
 begin
  L[i,j]:=A[i,j];
  A[i,j]:=0;
 end;
 end;

R:=A;
endp:
end;






procedure TMathLib.GetMatrixE(x: TFloatType; N: Cardinal; var E: TArray2);
var
i,j,N1: Cardinal;
begin
SetLength(E,N,N);
N1:=N-1;
for i:=0 to N1 do
for j:=0 to N1 do
begin
 if i = j then E[i,j]:=x
 else E[i,j]:=0;
end;

end;






procedure TMathLib.MatrixStrMulConst(A: TArray2; I: Cardinal; x: TFloatType);
var
C,k: Cardinal;
begin                      //ExternalLib: A[I,j]:=A[I,j]*x; для всех j
C:=High(A[0]);             //написать на asm !!!

for k:=0 to C do
 A[I,k]:=A[I,k]*x;
end;



procedure TMathLib.MatrixAddStrMulConst(A: TArray2; I: Cardinal; x: TFloatType);
var
C,S,k,j: Cardinal;
adrA: TAddress;
begin                      //ExternalLib: A[k,j]:=A[k,j]+A[I,j]*x; для всех j; k <> I;
S:=High(A); C:=High(A[0]);


adrA:=TAddress(A);
asm
   push  eax
   push  ebx
   push  ecx
   push  edx
   push  esi
   push  edi

   xor   esi,esi
   mov   eax,adrA
   mov   edx,I
   mov   ecx,[eax+4*edx]
   fld   qword ptr [x]
   fst   st(1)
@@K:
   cmp   esi,edx
   jz    @@1

   xor   edi,edi
   mov   ebx,[eax+4*esi]
@@J:
   fmul  qword ptr [ecx+8*edi]
   fadd  qword ptr [ebx+8*edi]
   fstp  qword ptr [ebx+8*edi]
   fst   st(1)

   inc   edi
   cmp   edi,C
   jle   @@J
@@1:
   inc   esi
   cmp   esi,S
   jle   @@K

   ffree st(0)


  pop   edi
  pop   esi
  pop   edx
  pop   ecx
  pop   ebx
  pop   eax

end;


{
for k:=0 to S do
if k <> I then
for j:=0 to C do
begin
 A[k,j]:=A[k,j]+A[I,j]*x;
end;
}

end;




procedure TMathLib.MatrixAdd(A,B: TArray2; var C: TArray2);
var
i,j,N: Cardinal;
adrA,adrB,adrC: TAddress;
begin
N:=High(A);
SetLength(C,N+1,N+1);

//DOUBLE:
adrA:=TAddress(A);
adrB:=TAddress(B);
adrC:=TAddress(C);
asm
   push  eax
   push  ebx
   push  ecx
   push  esi
   push  edi
   xor   esi,esi
@@I:
   mov   eax,adrA
   mov   eax,[eax+4*esi]
   mov   ebx,adrB
   mov   ebx,[ebx+4*esi]
   mov   ecx,adrC
   mov   ecx,[ecx+4*esi]
   xor   edi,edi
@@J:
   fld   qword ptr [eax+8*edi]
   fadd  qword ptr [ebx+8*edi]
   fstp  qword ptr [ecx+8*edi]
   inc   edi
   cmp   edi,N
   jle   @@J

   inc   esi
   cmp   esi,N
   jle   @@I

   pop   edi
   pop   esi
   pop   ecx
   pop   ebx
   pop   eax
end;

//EXTENDED:
{
for i:=0 to N do
for j:=0 to N do
C[i,j]:=A[i,j]+B[i,j];
}
end;



procedure TMathLib.MatrixSub(A,B: TArray2; var C: TArray2);
var
i,j,N: Cardinal;
adrA,adrB,adrC: TAddress;
begin
N:=High(A);
SetLength(C,N+1,N+1);

//DOUBLE:
adrA:=TAddress(A);
adrB:=TAddress(B);
adrC:=TAddress(C);

asm
   push  eax
   push  ebx
   push  ecx
   push  esi
   push  edi
   xor   esi,esi
@@I:
   mov   eax,adrA
   mov   eax,[eax+4*esi]
   mov   ebx,adrB
   mov   ebx,[ebx+4*esi]
   mov   ecx,adrC
   mov   ecx,[ecx+4*esi]
   xor   edi,edi
@@J:
   fld   qword ptr [eax+8*edi]
   fsub  qword ptr [ebx+8*edi]
   fstp  qword ptr [ecx+8*edi]
   inc   edi
   cmp   edi,N
   jle   @@J

   inc   esi
   cmp   esi,N
   jle   @@I

   pop   edi
   pop   esi
   pop   ecx
   pop   ebx
   pop   eax
end;

//EXTENDED:

{
for i:=0 to N do
for j:=0 to N do
C[i,j]:=A[i,j]-B[i,j];
}

end;



procedure TMathLib.MatrixMinor(A: TArray2; i,j: Cardinal; var B: TArray2);
var
i1,j1,i2,j2,S,C: Cardinal;
begin
S:=High(A); C:=High(A[0]);
SetLength(B,S,C);

i2:=0; j2:=0;
for i1:=0 to S do
begin
 if i1 <> i then
 begin
   j2:=0;
   for j1:=0 to C do
   begin
    if j1 <> j then begin B[i2,j2]:=A[i1,j1]; inc(j2); end;
   end;
   inc(i2);
 end;
end;


end;


procedure TMathLib.MatrixMul(A: TArray2; B: TArray2; var C: TArray2);
label endp;
var
K,M1,M2,M,N,i,j,l,Size2: LongInt;
S: TFloatType;
//C: TArray1;
adrA,adrB,adrC: TAddress;
begin
N:=High(A); M1:=High(A[0]); M2:=High(B); K:=High(B[0]);
if M1 <> M2 then begin F_CalcError:=12;   H_Error:='MATRIX_MUL:';  {TranslationError;} goto endp; end;
M:=M1;
SetLength(C,N+1,K+1);
{
 for i:=0 to N do
 begin
  for j:=0 to K do
  begin
   S:=0;
   for l:=0 to M do
   begin
    S:=S+A[i,l]*B[l,j];
   end;
   C[i,j]:=S;
  end;
 end;
}

adrA:=TAddress(A);
adrB:=TAddress(B);
adrC:=TAddress(C);
N:=N+1;
K:=K+1;
M:=M+1;

//DOUBLE:
asm
    mov   ebx,adrB
    xor   esi,esi
@@I:
    mov   eax,adrA
    mov   eax,[eax+4*esi]
    xor   edi,edi
@@J:
    fldz
    xor   ecx,ecx
@@L:
    mov   edx,[ebx+4*ecx]
    fld   qword ptr [edx+8*edi]
    fmul  qword ptr [eax+8*ecx]
    fadd
    inc   ecx
    cmp   ecx,M
    jne   @@L
    mov   ecx,adrC
    mov   ecx,[ecx+4*esi]
    fstp  qword ptr [ecx+8*edi]
    inc   edi
    cmp   edi,K
    jne   @@J
    inc   esi
    cmp   esi,N
    jne   @@I
end;

//EXTENDED:
{
asm
    xor   esi,esi
@@I:
    mov   eax,adrA
    mov   eax,[eax+4*esi]
    xor   edi,edi
@@J:
    fldz
    xor   ecx,ecx
@@L:
    mov   ebx,adrB
    mov   edx,[ebx+4*ecx]
    lea   ebx,[edi+4*edi]
    fld   tbyte ptr [edx+2*ebx]

    lea   ebx,[ecx+4*ecx]
    fld   tbyte ptr [eax+2*ebx]

    fmul
    fadd
    inc   ecx
    cmp   ecx,M
    jne   @@L

    mov   ecx,adrC
    mov   ecx,[ecx+4*esi]
    lea   ebx,[edi+4*edi]
    fstp  tbyte ptr [ecx+2*ebx]

    inc   edi
    cmp   edi,K
    jne   @@J
    inc   esi
    cmp   esi,N
    jne   @@I
end;
}

endp:
end;






{
procedure TMathLib.CreateSplain3(MX: TArray1; MF: TArray1);
var                                //MX, MF - отсчёт от 1
T,Max: real;
i,j,k,l,N: LongInt;
B,B1,h,M,F: TArray1;
A: TArray2;
begin
N:=High(MX)-1;  SL:=N;
SetLength(F,N+1); SetLength(A,N,N);
SetLength(PS,N+1);  SetLength(B,N);    SetLength(h,N+1);  SetLength(M,N+1);
SetLength(A3,N+1); SetLength(A2,N+1); SetLength(A1,N+1); SetLength(A0,N+1);


k:=N+1;
for i:=1 to N+1 do
begin
 Max:=MX[1]; l:=1;
 for j:=1 to k do
  begin
   if MX[j] > Max then
   begin
    Max:=MX[j]; l:=j;
   end;
 end;

 if l <> k then
 begin
  T:=MX[l];
  MX[l]:=MX[k];
  MX[k]:=T;
  T:=MF[l];
  MF[l]:=MF[k];
  MF[k]:=T;
 end;
 dec(k);
end;


for i:=0 to N do
begin
F[i]:=MF[i+1];
PS[i]:=MX[i+1];
end;


for i:=1 to N do
begin
h[i]:=PS[i]-PS[i-1];
end;

for i:=1 to N-1 do
begin
B[i]:=(F[i+1]-F[i])/h[i+1]-(F[i]-F[i-1])/h[i];
end;


for i:=1 to N-1 do
begin
for j:=1 to N-1 do
begin
 if j = i-1 then A[i,j]:=h[i]/6
 else
 if j = i then A[i,j]:=(h[i]+h[i+1])/3
 else
 if j = i+1 then A[i,j]:=h[i+1]/6
 else A[i,j]:=0;
end;
end;


LSE(A,B,B1);

M[0]:=0; M[N]:=0;
for i:=1 to N-1 do
begin
 M[i]:=B1[i];
end;

for i:=1 to N do
begin
 A3[i]:=(M[i]-M[i-1])/(6*h[i]);
 A2[i]:=(M[i-1]*PS[i]-M[i]*PS[i-1])/(2*h[i]);
 A1[i]:=(3*M[i]*sqr(PS[i-1])-3*M[i-1]*sqr(PS[i])-6*F[i-1]+M[i-1]*sqr(h[i])+
        6*F[i]-M[i]*sqr(h[i]))/(6*h[i]);
 A0[i]:=(M[i-1]*intpower(PS[i],3)-M[i]*intpower(PS[i-1],3)+6*PS[i]*F[i-1]-
        M[i-1]*sqr(h[i])*PS[i]-6*PS[i-1]*F[i]+M[i]*sqr(h[i])*PS[i-1])/(6*h[i]);
end;


end;



function TMathLib.Splain3(x: TFloatType): TFloatType;
label 1,endp;
var
i,j: LongInt;
begin

for i:=0 to SL-1 do
begin
 if (x >= PS[i]) and (x <= PS[i+1]) then begin j:=i+1; goto 1; end
 else
 if (x <= PS[SL-i]) and (x >= PS[SL-i-1]) then begin j:=SL-i; goto 1; end;
end;


1:
try
Splain3:=A3[j]*x*sqr(x)+A2[j]*sqr(x)+A1[j]*x+A0[j];
except
F_CalcError:=13;
H_Error:='SPLAIN3:';
//TranslationError;
end;


endp:
end;
}






procedure TMathLib.CreateSplain3(MX: TArray1; MF: TArray1; var SP: TSplain);
var                               
T,Max: real;
i,j,k,l,N: LongInt;
B,B1,h,M,F: TArray1;
A: TArray2;
MX1,MF1: TArray1;
begin
N:=High(MX);

SL:=N;
SetLength(F,N+1); SetLength(A,N,N);
SetLength(PS,N+1);  SetLength(B,N);    SetLength(h,N+1);  SetLength(M,N+1);
SetLength(A3,N+1); SetLength(A2,N+1); SetLength(A1,N+1); SetLength(A0,N+1);

SetLength(MX1,N+2);
SetLength(MF1,N+2);
for i:=1 to N+1 do
begin
 MX1[i]:=MX[i-1];
 MF1[i]:=MF[i-1];
end;


k:=N+1;
for i:=1 to N+1 do
begin
 Max:=MX1[1]; l:=1;
 for j:=1 to k do
  begin
   if MX1[j] > Max then
   begin
    Max:=MX1[j]; l:=j;
   end;
 end;

 if l <> k then
 begin
  T:=MX1[l];
  MX1[l]:=MX1[k];
  MX1[k]:=T;
  T:=MF1[l];
  MF1[l]:=MF1[k];
  MF1[k]:=T;
 end;
 dec(k);
end;


for i:=0 to N do
begin
F[i]:=MF1[i+1];
PS[i]:=MX1[i+1];
end;


for i:=1 to N do
begin
h[i]:=PS[i]-PS[i-1];
end;

for i:=1 to N-1 do
begin
B[i]:=(F[i+1]-F[i])/h[i+1]-(F[i]-F[i-1])/h[i];
end;


for i:=1 to N-1 do
begin
for j:=1 to N-1 do
begin
 if j = i-1 then A[i,j]:=h[i]/6
 else
 if j = i then A[i,j]:=(h[i]+h[i+1])/3
 else
 if j = i+1 then A[i,j]:=h[i+1]/6
 else A[i,j]:=0;
end;
end;


////////////////////
for i:=1 to N-1 do
begin
 for j:=1 to N-1 do
 begin
  A[i-1,j-1]:=A[i,j];
 end;
 B[i-1]:=B[i];
end;

SetLength(A,N-1,N-1);
SetLength(B,N-1);
///////////////////


LSE(A,B,B1);

//////////////////
for i:=1 to N-1 do
begin
 B1[i-1]:=B1[i];
end;
SetLength(B1,N-1);
//////////////////

M[0]:=0; M[N]:=0;
for i:=1 to N-1 do
begin
 M[i]:=B1[i];
end;



M[0]:=0; M[N]:=0;
for i:=1 to N-1 do
begin
 M[i]:=B1[i];
end;

for i:=1 to N do
begin
 A3[i]:=(M[i]-M[i-1])/(6*h[i]);
 A2[i]:=(M[i-1]*PS[i]-M[i]*PS[i-1])/(2*h[i]);
 A1[i]:=(3*M[i]*sqr(PS[i-1])-3*M[i-1]*sqr(PS[i])-6*F[i-1]+M[i-1]*sqr(h[i])+
        6*F[i]-M[i]*sqr(h[i]))/(6*h[i]);
 A0[i]:=(M[i-1]*intpower(PS[i],3)-M[i]*intpower(PS[i-1],3)+6*PS[i]*F[i-1]-
        M[i-1]*sqr(h[i])*PS[i]-6*PS[i-1]*F[i]+M[i]*sqr(h[i])*PS[i-1])/(6*h[i]);
end;


SetLength(SP.A0,N);
SetLength(SP.A1,N);
SetLength(SP.A2,N);
SetLength(SP.A3,N);
SetLength(SP.PS,N+1);
SP.SL:=SL-1;
for i:=0 to N-1 do
begin
 SP.A0[i]:=A0[i+1];
 SP.A1[i]:=A1[i+1];
 SP.A2[i]:=A2[i+1];
 SP.A3[i]:=A3[i+1];
 SP.PS[i]:=PS[i];
end;
 SP.PS[N]:=PS[N];

A0:=nil;  A1:=nil; A2:=nil; A3:=nil; PS:=nil;
B:=nil; B1:=nil; H:=nil; M:=nil; F:=nil;
A:=nil; MX1:=nil; MF1:=nil;
end;


function TMathLib.Splain3(x: TFloatType; SP: TSplain): TFloatType;
label 1,endp;
var
i,j: LongInt;
begin

for i:=0 to SP.SL do
begin
 if (x >= SP.PS[i]) and (x <= SP.PS[i+1]) then begin j:=i; goto 1; end
 else
 if (x <= SP.PS[SL-i]) and (x >= SP.PS[SL-i-1]) then begin j:=SL-i-1; goto 1; end;
end;


1:
try
Splain3:=SP.A3[j]*x*sqr(x)+SP.A2[j]*sqr(x)+SP.A1[j]*x+SP.A0[j];
except
F_CalcError:=13;
H_Error:='SPLAIN3:';
{TranslationError;}
end;


endp:
end;




function  TMathLib.NLSE_G(FL: TArrayA; PX: PArray1; X0: TArray1;   E: TFloatType; safe: Integer; var Error: Integer): TArray1;
label endp;
var
Lk,Mk,Fk,SQDF,Fk1,Fk2: TFloatType;
grad,Xk,Xk1,Xk2,Xl: TArray1;
N,i,j,k,SE_N: Integer;
begin
if (High(FL) <> High(PX)) and  (High(FL) <> High(X0)) then
begin
//Error;
end;
SE_N:=High(FL);
Error:=0;
Xk:=X0;
N:=Length(X0);
SetLength(Xk1,SE_N+1);
SetLength(Xk2,SE_N+1);
SetLength(Xl,SE_N+1);

SE_SetVar(PX,Xk);
Fk:=GF(FL,Error);
if Error = 1 then goto endp;
//111111111111111111111111
try
Xl:=Copy(Xk,0,Length(Xk));
 Fk:=GF(FL,Error);
 SQDF:=SE_SQDF(FL,PX,Error);
 Lk:=Fk/SQDF;
 grad:=SE_gradF(FL,PX,Error);

 for i:=1 to SE_N do
 begin
  Xk1[i]:=Xk[i]-Lk*grad[i];
  Xk2[i]:=Xk[i]-Lk*grad[i]*0.5;
 end;

 SE_SetVar(PX,Xk1);
 Fk1:=GF(FL,Error);
 SE_SetVar(PX,Xk2);
 Fk2:=GF(FL,Error);


 Mk:=(3*Fk-4*Fk2+Fk1)/(Fk-2*Fk2+Fk1)*Lk*0.25;
 Xl:=Copy(Xk,0,Length(Xk));
 for i:=1 to SE_N do
 begin
  Xk[i]:=Xk[i]-Mk*grad[i];
 end;

 SE_SetVar(PX,Xk);
except
Error:=1;
end;
if Error = 1 then goto endp;
//111111111111111111111111111111


k:=0;
while {sqrt(Fk) > E} dist(Xk,Xl) > E do
begin
inc(k);
try
 Fk:=GF(FL,Error);
 SQDF:=SE_SQDF(FL,PX,Error);
 Lk:=Fk/SQDF;
 grad:=SE_gradF(FL,PX,Error);


 for i:=1 to SE_N do
 begin
  Xk1[i]:=Xk[i]-Lk*grad[i];
  Xk2[i]:=Xk[i]-Lk*grad[i]*0.5;
 end;

 SE_SetVar(PX,Xk1);
 Fk1:=GF(FL,Error);
 SE_SetVar(PX,Xk2);
 Fk2:=GF(FL,Error);


 Mk:=(3*Fk-4*Fk2+Fk1)/(Fk-2*Fk2+Fk1)*Lk*0.25;
 Xl:=Copy(Xk,0,Length(Xk));
 for i:=1 to SE_N do
 begin
  Xk[i]:=Xk[i]-Mk*grad[i];
 end;

except
Error:=1;
end;

 SE_SetVar(PX,Xk);
 if Error = 1 then goto endp;
if k > safe then begin Error:=1; goto endp;  end;
end;

endp:
//отсечение случайных корней
SE_SetVar(PX,Xk);
try
 if abs(CalcFunc(FL[1])) > 1E-12 then Error:=1;  //условно
except
Error:=1;
end;

NLSE_G:=Xk;
end;



function TMathLib.GF(FL: TArrayA; var Error: Integer): TFloatType;
var
F,QF: TFloatType;
i: Integer;
begin
Error:=0;
F:=0;
try
for i:=1 to High(FL) do
begin
 QF:=CalcFunc(FL[i]);
 F:=F+sqr(QF);
end;
except
 Error:=1;
end;
GF:=F;
end;



function TMathLib.SE_SQDF(FL: TArrayA; PX: PArray1; var Error: Integer): TFloatType;
var
F,QF: TFloatType;
i: Integer;
FV: PFloatType;
begin
Error:=0;
F:=0;
try
for i:=1 to High(PX) do
begin
 QF:=DGF(FL,PX[i],Error);
 F:=F+sqr(QF);
end;
except
 Error:=1;
end;
SE_SQDF:=F;
end;





function TMathLib.SE_gradF(FL: TArrayA; PX: PArray1; var Error: Integer): TArray1;
var
F: TFloatType;
i: Integer;
FV: PFloatType;
DF: TArray1;
begin
Error:=0;
SetLength(DF,Length(FL));
try
for i:=1 to High(PX) do
begin
 F:=DGF(FL,PX[i],Error);
 DF[i]:=-F;
end;
except
Error:=1;
end;
SE_gradF:=DF;
end;







function TMathLib.DGF(FL: TArrayA;  FV: PFloatType; var Error: Integer): TFloatType;
var
h,dy5,H1,xh,Yh: TFloatType;
i: Integer;
begin
h:=DiffH;
H1:=FV^;
xh:=FV^-DiffL*h*0.5;
Yh:=0;

try
for i:=0 to  DiffL do
begin
 FV^:=xh;
 Yh:=Yh+D1[i]*GF(FL,Error);
 xh:=xh+h;
end;
except
Error:=1;
end;

FV^:=H1;
DGF:=Yh/h;
end;



{
function TMathLib.DGF(FL: TArrayA;  FV: PFloatType): TFloatType;
var
x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,
h,dy5,H1: TFloatType;

begin
h:=0.01; H1:=FV^;
x0:=H1-5*h; x1:=x0+h; x2:=x1+h;  x3:=x2+h; x4:=x3+h; x5:=x4+h; x6:=x5+h;
x7:=x6+h; x8:=x7+h; x9:=x8+h; x10:=x9+h;
FV^:=x0;  y0:=GF(FL);
FV^:=x1;  y1:=GF(FL);
FV^:=x2;  y2:=GF(FL);
FV^:=x3;  y3:=GF(FL);
FV^:=x4;  y4:=GF(FL);
FV^:=x5;  y5:=GF(FL);
FV^:=x6;  y6:=GF(FL);
FV^:=x7;  y7:=GF(FL);
FV^:=x8;  y8:=GF(FL);
FV^:=x9;  y9:=GF(FL);
FV^:=x10; y10:=GF(FL);


dy5:=1/(4536*h)*(-3.6*y0+45*y1-270*y2+1080*y3-3780*y4+3780*y6-1080*y7+270*y8-45*y9+3.6*y10);
FV^:=H1;
DGF:=dy5;
end;
}





{
function TMathLib.Diff1(x: TFloatType): TFloatType;
var
h,dy5,H1,xh,Yh: TFloatType;
i: Integer;
R: TFloatType;
Adr: LongInt;
begin
h:=DiffH;
H1:=FV1^;
xh:=x-DiffL*h*0.5;
Yh:=0;

for i:=0 to  DiffL do
begin
 FV1^:=xh;
 Yh:=Yh+D1[i]*Calculation;
 xh:=xh+h;
end;


FV1^:=H1;
Diff1:=Yh/h;
end;
}



function TMathLib.Diff1(Func: TAddress;  x: TFloatType): TFloatType;
var
h,dy5,H1,xh,Yh: TFloatType;
i: Integer;
begin
h:=DiffH;
H1:=FV1^;
xh:=x-DiffL*h*0.5;
Yh:=0;
//Form1.L4.Caption:=FloatToStr(x);
for i:=0 to  DiffL do
begin
 FV1^:=xh;
 Yh:=Yh+D1[i]*CalcFunc(Func);
 xh:=xh+h;
end;


FV1^:=H1;
Diff1:=Yh/h;
end;




function TMathLib.Diff2(Func: TAddress;  x: TFloatType): TFloatType;
var
h,dy5,H1,xh,Yh: TFloatType;
i: Integer;
begin
h:=DiffH;
H1:=FV1^;
xh:=x-DiffL*h*0.5;
Yh:=0;

for i:=0 to  DiffL do
begin
 FV1^:=xh;
 Yh:=Yh+D2[i]*CalcFunc(Func);
 xh:=xh+h;
end;


FV1^:=H1;
Diff2:=Yh/sqr(h);
end;



function TMathLib.Diff3(Func: TAddress;  x: TFloatType): TFloatType;
var
h,dy5,H1,xh,Yh: TFloatType;
i: Integer;
begin
h:=DiffH;
H1:=FV1^;
xh:=x-DiffL*h*0.5;
Yh:=0;

for i:=0 to  DiffL do
begin
 FV1^:=xh;
 Yh:=Yh+D3[i]*CalcFunc(Func);
 xh:=xh+h;
end;


FV1^:=H1;
Diff3:=Yh/(h*sqr(h));
end;



function TMathLib.Diff4(Func: TAddress;  x: TFloatType): TFloatType;
var
h,dy5,H1,xh,Yh: TFloatType;
i: Integer;
begin
h:=DiffH;
H1:=FV1^;
xh:=x-DiffL*h*0.5;
Yh:=0;

for i:=0 to  DiffL do
begin
 FV1^:=xh;
 Yh:=Yh+D4[i]*CalcFunc(Func);
 xh:=xh+h;
end;


FV1^:=H1;
Diff4:=Yh/(sqr(sqr(h)));
end;



function TMathLib.Diff5(Func: TAddress;  x: TFloatType): TFloatType;
var
h,dy5,H1,xh,Yh: TFloatType;
i: Integer;
begin
h:=DiffH;
H1:=FV1^;
xh:=x-DiffL*h*0.5;
Yh:=0;

for i:=0 to  DiffL do
begin
 FV1^:=xh;
 Yh:=Yh+D5[i]*CalcFunc(Func);
 xh:=xh+h;
end;


FV1^:=H1;
Diff5:=Yh/(h*sqr(sqr(h)));
end;






function TMathLib.Derivative_N(N: TIntegType; Func: TAddress;   PV: PFloatType; x: TFloatType): TFloatType;
begin
FV1:=PV;
Derivative_N:=Diff_N(N,Func,x);
end;



function TMathLib.Diff_N(N: TIntegType; Func: TAddress;   x: TFloatType): TFloatType;
var
x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,
h,dy5,H1: TFloatType;
begin
if N = 1 then Diff_N:=Diff1(Func,x)
else
if N = 2 then Diff_N:=Diff2(Func,x)
else
begin
 h:=0.01;
 x0:=x-5*h; x1:=x0+h; x2:=x1+h;  x3:=x2+h; x4:=x3+h; x5:=x4+h; x6:=x5+h;
 x7:=x6+h; x8:=x7+h; x9:=x8+h; x10:=x9+h;

 y0:=Diff_N(N-1,Func,x0);
 y1:=Diff_N(N-1,Func,x1);
 y2:=Diff_N(N-1,Func,x2);
 y3:=Diff_N(N-1,Func,x3);
 y4:=Diff_N(N-1,Func,x4);
 y5:=Diff_N(N-1,Func,x5);
 y6:=Diff_N(N-1,Func,x6);
 y7:=Diff_N(N-1,Func,x7);
 y8:=Diff_N(N-1,Func,x8);
 y9:=Diff_N(N-1,Func,x9);
 y10:=Diff_N(N-1,Func,x10);

 dy5:=1/(4536*h)*(-3.6*y0+45*y1-270*y2+1080*y3-3780*y4+3780*y6-1080*y7+270*y8-45*y9+3.6*y10);
 FV1^:=H1;
 Diff_N:=dy5;

end;


end;


{
function TMathLib.Diff2(x: TFloatType): TFloatType;
var
h,dy5,H1,xh,Yh: TFloatType;
i: Integer;
R: TFloatType;
Adr: LongInt;
begin
h:=DiffH;
H1:=FV1^;
xh:=x-DiffL*h*0.5;
Yh:=0;

for i:=0 to  DiffL do
begin
 FV1^:=xh;
 Yh:=Yh+D2[i]*Calculation;
 xh:=xh+h;
end;


FV1^:=H1;
Diff2:=Yh/sqr(h);
end;
}






{
function TMathLib.Derivative1(PV: PFloatType; x: TFloatType): TFloatType;
begin
FV1:=PV;
Derivative1:=Diff1(x);
end;
}

function TMathLib.Derivative1(Func: TAddress;   PV: PFloatType; x: TFloatType): TFloatType;
begin
FV1:=PV;
IFunc:=Func;
Derivative1:=Diff1(Func,x);
end;



function TMathLib.Derivative1(F,X: TArray1; t: TFloatType): TFloatType;
label 1;
var
SL1: TIntegType;
B3,B2,B1,B0,PS1: TArray1;
h,xh,Yh: TFloatType;
i,j: Integer;
begin
B3:=A3; B2:=A2; B1:=A1; B0:=A0; SL1:=SL; PS1:=PS;
//CreateSplain3(X,F);


//поиск производной обычным способом
{h:=DiffH;
xh:=t-DiffL*h*0.5;
Yh:=0;

for i:=0 to  DiffL do
begin
 Yh:=Yh+D1[i]*Splain3(xh);
 xh:=xh+h;
end;
}
//поиск производной через дифф-е Splain'a:
for i:=0 to SL-1 do
begin
 if (t >= PS[i]) and (t <= PS[i+1]) then begin j:=i+1; goto 1; end
 else
 if (t <= PS[SL-i]) and (t >= PS[SL-i-1]) then begin j:=SL-i; goto 1; end;
end;
1:
Derivative1:=3*A3[j]*sqr(t)+2*A2[j]*t+A1[j];


//Derivative1:=Yh/h;
A3:=B3; A2:=B2; A1:=B1; A0:=B0; SL:=SL1; PS:=PS1;
end;


{
function TMathLib.Derivative2(PV: PFloatType; x: TFloatType): TFloatType;
begin
FV1:=PV;
Derivative2:=Diff2(x);
end;
}

function TMathLib.Derivative2(Func: TAddress;   PV: PFloatType; x: TFloatType): TFloatType;
begin
FV1:=PV;
Derivative2:=Diff2(Func,x);
end;


function TMathLib.Derivative3(Func: TAddress;   PV: PFloatType; x: TFloatType): TFloatType;
begin
FV1:=PV;
Derivative3:=Diff3(Func,x);
end;


function TMathLib.Derivative4(Func: TAddress;   PV: PFloatType; x: TFloatType): TFloatType;
begin
FV1:=PV;
Derivative4:=Diff4(Func,x);
end;


function TMathLib.Derivative5(Func: TAddress;   PV: PFloatType; x: TFloatType): TFloatType;
begin
FV1:=PV;
Derivative5:=Diff5(Func,x);
end;



function TMathLib.Derivative2(F,X: TArray1; t: TFloatType): TFloatType;
var
SL1: TIntegType;
B3,B2,B1,B0,PS1: TArray1;
h,xh,Yh: TFloatType;
i: Integer;
begin
B3:=A3; B2:=A2; B1:=A1; B0:=A0; SL1:=SL; PS1:=PS;
//CreateSplain3(X,F);

h:=DiffH;
xh:=t-DiffL*h*0.5;
Yh:=0;

for i:=0 to  DiffL do
begin
 FV1^:=xh;
 //Yh:=Yh+D2[i]*Splain3(xh);
 xh:=xh+h;
end;


Derivative2:=Yh/sqr(h);
A3:=B3; A2:=B2; A1:=B1; A0:=B0; SL:=SL1; PS:=PS1;
end;





function TMathLib.Root(Func: TAddress; PV: PFloatType; x0: TFloatType; {убрать}var Error: Integer{убрать}): TFloatType;
var
R,x: TFloatType;
begin

//if F_Approximation = 1 then x:=A_Root else x:=0;

F_RootType:=3;
F_RootPower:=2;
E_Root:=1E-12;

x:=x0;
 if F_RootType = 1 then R:=RootR1(Func,x,PV,Error)
 else
 {if TF = 2 then R:=RootR2(TP,FL,PV,X0,E,safe,Error)
 else}
 if F_RootType = 3 then R:=RootM1(Func,x,PV,Error);
 {else
 if TF = 4 then R:=RootM2(FL,PV,X0,E,safe,Error);
 }
 if Error = 1 then begin{ERROR}end;

Root:=R;
end;





function TMathLib.Diff1_I(Func: TFunc; x: TFloatType): TFloatType;
var
x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,
h,dy5: real;
begin
h:=0.01;
x0:=x-5*h; x1:=x0+h; x2:=x1+h;  x3:=x2+h; x4:=x3+h; x5:=x4+h; x6:=x5+h;
x7:=x6+h; x8:=x7+h; x9:=x8+h; x10:=x9+h;
y0:=Func(x0); y1:=Func(x1); y2:=Func(x2); y3:=Func(x3); y4:=Func(x4);
{y5:=Func(x5);} y6:=Func(x6); y7:=Func(x7); y8:=Func(x8); y9:=Func(x9);
y10:=Func(x10);

dy5:=1/(4536*h)*(-3.6*y0+45*y1-270*y2+1080*y3-3780*y4+3780*y6-1080*y7+270*y8-45*y9+3.6*y10);

Diff1_I:=dy5;
end;




function TMathLib.Diff2_I(Func: TFunc; x: TFloatType): TFloatType;
var
x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,
h,dy5: real;
begin
h:=0.01;
x0:=x-5*h; x1:=x0+h; x2:=x1+h;  x3:=x2+h; x4:=x3+h; x5:=x4+h; x6:=x5+h;
x7:=x6+h; x8:=x7+h; x9:=x8+h; x10:=x9+h;
y0:=Func(x0); y1:=Func(x1); y2:=Func(x2); y3:=Func(x3); y4:=Func(x4);
y5:=Func(x5); y6:=Func(x6); y7:=Func(x7); y8:=Func(x8); y9:=Func(x9);
y10:=Func(x10);

dy5:=1/(28350*sqr(h))*(9*y0-140.625*y1+1125*y2-6750*y3+47250*y4-82986.75*y5+
47250*y6-6750*y7+1125*y8-140.625*y9+9*y10);

Diff2_I:=dy5;
end;





procedure TMathLib.Root_IR1(x: TFloatType; var Rt: TFloatType; var Error: Integer);
label 1,2,endp;
var
si,i,P,safe,FR: Integer;
D,D1,D1I,T,H,PH,E,PL0: TFloatType;
begin

Error:=0;
P:=F_RootPower;
safe:=N_RootSafe;
E:=E_Root;


   1:
   D:=Polynom(x);
   PL0:=Polynom0(x);
   D1:=Diff1_P(x);
   if (D1 = 0) and (abs(PL0{D}) < E) then goto endp;  //это корень


   if (D1 = 0)  and (abs(PL0{D}) >= E) then
   begin
    X:=X+1; goto 1;
    {локальный минимум, но не корень: взять другое значение X}
   end;

 try
    D1I:=1/D1;
    X:=X-D*D1I;
    for i:=3 to P do
    begin
     D:=Polynom(X);
     X:=X-D*D1I;
    end;
 except
 Error:=1;
 end;


if Error = 1 then goto endp;


si:=0;
PL0:=Polynom0(x);
while  abs(PL0{D}) > E  do
begin
inc(si);

  2:
   PL0:=Polynom0(x);
   D:=Polynom(x);
   D1:=Diff1_P(x);
   if (D1 = 0) and (abs(PL0{D}) < E) then goto endp;  //это корень


   if (D1 = 0)  and (abs(PL0{D}) > E) then
   begin
    X:=X+1; goto 2;
    {локальный минимум, но не корень: взять другое значение X}
   end;


  try
    D:=Polynom(x);
    D1I:=1/D1;
    X:=X-D*D1I;
    for i:=3 to P do
    begin
     D:=Polynom(X);
     X:=X-D*D1I;
    end;
 except
 Error:=1;
 end;



if Error = 1 then goto endp;

 if si > safe then
 begin Error:=1; goto endp; end;

inc(si);
end;


endp:
Rt:=X;
end;





procedure TMathLib.Root_IM1(x: TFloatType; var Rt: TFloatType; var Error: Integer);
label 1,2,endp;
var
si,i,P,safe,FR: Integer;
D,D1,G,U,DC,T,H,PH,E,Z,B,PL0: TFloatType;
begin
FR:=0;

Error:=0;
P:=5;
safe:=N_RootSafe;
E:=E_Root;
T:=X;


1:
   G:=Diff1_P(x);
   D:=Polynom(x);
   PL0:=Polynom0(x);
   if abs(D) < E then goto endp;
   if (G = 0) and (abs(PL0{D}) < E) then goto endp;
   if (G = 0) and (abs(PL0{D}) >= E) then
   begin
    X:=X/2; goto 1;
   end;

   try
    B:=-1/G;
    G:=(Polynom(x+B*D)-D)/(D*B);
   except
    Error:=1;
   end;

   if (G = 0) and (abs(PL0{D}) < E) then goto endp;
   if (G = 0) and (abs(PL0{D}) >= E) then
   begin
    X:=X/2; goto 1;
   end;
   X:=X-D/G;

if Error = 1 then goto endp;

PL0:=Polynom0(x);

si:=0;
while  abs(PL0{D}) > E   do
begin
inc(si);


2:
   D:=Polynom(x);
   PL0:=Polynom0(x);

   if {abs(D) < E} D = 0   then goto endp;
   if (G = 0) and (abs(PL0{D}) < E)   then goto endp;
   if (G = 0) and (abs(PL0{D}) >= E) then
   begin
    X:=X/2; goto 2;
   end;

   try
    B:=-1/G;
    G:=(Polynom(x+B*D)-D)/(D*B);
   except
    Error:=1;
   end;

   if (G = 0) and (abs(PL0{D}) < E)   then goto endp;
   if (G = 0) and (abs(PL0{D}) >= E) then
   begin
    X:=X/2;
    G:=(Polynom(x+B*D)-D)/(D*B);
    goto 2;
   end;
   X:=X-D/G;



 if Error = 1 then goto endp;
 if si > safe then
 begin Error:=1; goto endp; end;
end;


endp:
Rt:=X;
end;





procedure TMathLib.Root_IR2(x: TFloatType; var Rt: TFloatType; var Error: Integer);
label 1,2,endp;
var
si,i,P,safe,FR: Integer;
D,D1,D2,DF,U,DC,T,H,PH,E,PL0: TFloatType;
begin

Error:=0;
P:=F_RootPower;
safe:=N_RootSafe;
E:=E_Root;
T:=X;


   1:
   PL0:=Polynom0(x);
   D:=Polynom(x);
   D1:=Diff1_P(x);
   if (D1 = 0) and (abs(PL0{D}) < E) then goto endp;  //это корень


   if (D1 = 0)  and (abs(PL0{D}) >= E) then
   begin
    X:=X+1; goto 1;
    {локальный минимум, но не корень: взять другое значение X}
   end;

  D2:=Diff2_P(x);

  try
    U:=D/D1;
    DC:=1/(D1-D2*U);
    X:=X-D*DC;
    for i:=3 to P do
    begin
     D:=Polynom(X);
     X:=X-D*DC;
    end;
 except
 Error:=1;
 end;


if Error = 1 then goto endp;

PL0:=Polynom0(x);
si:=0;
while  abs(PL0{D}) > E  do
begin
inc(si);

  2:
   PL0:=Polynom0(x);
   D:=Polynom(x);
   D1:=Diff1_P(x);
   if (D1 = 0) and (abs(PL0{D}) < E) then goto endp;  //это корень

   D2:=Diff2_P(x);

   if (D1 = 0)  and (abs(PL0{D}) > E) then
   begin
    X:=X+1; goto 2;
    {локальный минимум, но не корень: взять другое значение X}
   end;


  try
    D:=Polynom(x);
    U:=D/D1;
    DC:=1/(D1-D2*U);
    X:=X-D*DC;
    for i:=3 to P do
    begin
     D:=Polynom(X);
     X:=X-D*DC;
    end;
 except
 Error:=1;
 end;



if Error = 1 then goto endp;

 if si > safe then
 begin Error:=1; goto endp; end;

inc(si);
end;


endp:
Rt:=X;
end;





procedure TMathLib.Root_IM2(x: TFloatType; var Rt: TFloatType; var Error: Integer);
label 1,2,endp;
var
si,i,P,safe,FR: Integer;
D,D1,D2,DF,U,DC,T,H,PH,E,Z,L,PL0: TFloatType;
begin
FR:=0;

Error:=0;
P:=5;
safe:=N_RootSafe;
E:=E_Root;
T:=X;


1:
   L:=Diff1_P(x);
   PL0:=Polynom0(x);
   D:=Polynom(x);
   if (L = 0) and (abs(PL0{D}) < E) then goto endp;
   if (L = 0) and (abs(PL0{D}) >= E) then
   begin
    X:=X+1; goto 1;
   end;
   T:=x;

   Z:=x-D/L;
   x:=Z;
   DF:=Polynom(x);
   if (Z-T) = 0 then
   begin
    goto endp;
    x:=Z;
   end;
try
   L:=(DF-D)/(Z-T);
   x:=T-D/L;
except
 Error:=1;
end;


if Error = 1 then goto endp;
PL0:=Polynom0(x);
si:=0;
while  abs(PL0{D}) > E   do
begin
inc(si);
FR:=0;


   2:
   PL0:=Polynom0(x);
   L:=Diff1_P(x);
   D:=Polynom(x);
   if (L = 0) and (abs(PL0{D}) < E) then goto endp;
   if (L = 0) and (abs(PL0{D}) >= E) then
   begin
    X:=X+1; goto 2;
   end;
   T:=x;


   Z:=x-D/L;
   x:=Z;
   DF:=Polynom(x);
   if (Z-T) = 0 then
   begin
    x:=Z;
    goto endp;
   end;
try
   L:=(DF-D)/(Z-T);
   x:=T-D/L;
except
 Error:=1;
end;


 if Error = 1 then goto endp;
 if si > safe then
 begin Error:=1; goto endp; end;
end;


endp:
Rt:=X;
end;














procedure TMathLib.PolyRoot(PL: TArray1; var R: TArray1);
label 1,2,3,4,endp;
var
N,i,j,RB: LongInt;
x,T,E,x0,D,D1,Z: TFloatType;
Er: Integer;

begin
N:=Length(PL);



G_PN0:=N-1; SetLength(Pk0,N);
G_PN:=N-1;  SetLength(Pk,N);
for i:=0 to G_PN do
begin
Pk[i]:=PL[i];
Pk0[i]:=PL[i];
end;



if F_Approximation = 1 then x0:=A_Root else x0:=0;{x0:=-Pk[1]/Pk[0]};

if F_RootType = 1 then
Root_IR1(x0,x,Er);
if F_RootType = 2 then
Root_IR2(x0,x,Er);
if F_RootType = 3 then
Root_IM1(x0,x,Er);
if F_RootType = 4 then
Root_IM2(x0,x,Er);


if Er = 1 then goto endp;
x:=RoundX(x);  //округление рез-та если целое (по состоянию флага E_Round)

SetLength(R,1); R[0]:=x; dec(N); dec(G_PN);

while G_PN > 0 do
begin

 for i:=0 to G_PN do
 begin
  Pk[i]:=Pk[i]+Pk[i-1]*x;
 end;

 x0:=x;
 if F_RootType = 1 then
 Root_IR1(x0,x,Er);
 if F_RootType = 2 then
 Root_IR2(x0,x,Er);
 if F_RootType = 3 then
 Root_IM1(x0,x,Er);
 if F_RootType = 4 then
 Root_IM2(x0,x,Er);
 if Er = 1 then goto endp;
 x:=RoundX(x);

 SetLength(R,Length(R)+1); R[High(R)]:=x; dec(N); dec(G_PN);

end;


endp:
end;



{
function TMathLib.Integral1(PV: PFloatType; a,b: TFloatType; n: TIntegType = 1; zl: TFloatType = 1E-14): TFloatType;
var
j,i:TIntegType;
x1,x2,H1,Int1,h:TFloatType;
begin

FV1:=PV;
H1:=FV1^;

if n <=0 then n:=1;

if n > 0 then
begin
h:=(b-a)/n; Int1:=0;

for j:=0 to n-1 do
begin
x1:=a+j*h; x2:=a+(j+1)*h;
for i:=1 to 15 do
begin
FV1^:=(x2-x1)*0.5*bn[i]+(x1+x2)*0.5;
Int1:=Int1+an[i]*Calculation*(x2-x1)*0.5;
end;
end;

end;

if abs(Int1) <= zl then Int1:=0;
Integral1:=Int1;
FV1^:=H1;

end;
}





function TMathLib.Integral1(Func: TAddress;  PV: PFloatType; a,b: TFloatType): TFloatType;
var
j,i,n:TIntegType;
x1,x2,Int,H1,hax,sx,ax,h:TFloatType;
begin
H1:=PV^;

h:=H_Integral;
n:=Trunc(abs(b-a)/h);
if n = 0 then n:=1;
h:=(b-a)/n;


Int:=0;
for j:=0 to n-1 do
begin
x1:=a+j*h; x2:=a+(j+1)*h;
ax:=(x1+x2)*0.5; sx:=(x2-x1)*0.5;
for i:=1 to N_Integral do
begin
PV^:=sx*bn[i]+ax;
Int:=Int+an[i]*CalcFunc(Func)*sx;
end;
end;


Integral1:=Int;
PV^:=H1;
end;




{
function TMathLib.Integral2(PV1: PFloatType; PV2: PFloatType; a,b: TFloatType; F1,F2: String; n: TIntegType = 1; zl: TFloatType = 1E-14): TFloatType;
var
R,j1,i1,j2,i2,n1,n2:TIntegType;
Int1,Int2,T,HV1,HV2,h1,h2,xa,xb,xc,xd,c,d:TFloatType;
Func1,Func2: TAddress;
begin
FV1:=PV1;
FV2:=PV2;
HV1:=FV1^;
HV2:=FV2^;

if n <=0 then n:=1;

try
c:=StrToFloat(F1);
d:=StrToFloat(F2);
Int2:=0;
n1:=n; n2:=n;
h1:=(b-a)/n1; h2:=(d-c)/n2;

for j1:=0 to n1-1 do
begin
xa:=a+j1*h1; xb:=a+(j1+1)*h1;
for i1:=1 to 15 do
begin
FV1^:=(xb-xa)*0.5*bn[i1]+(xa+xb)*0.5;

 Int1:=0;
 for j2:=0 to n2-1 do
 begin
 xc:=c+j2*h2; xd:=c+(j2+1)*h2;
 for i2:=1 to 15 do
 begin
 FV2^:=(xd-xc)*0.5*bn[i2]+(xc+xd)*0.5;
 Int1:=Int1+an[i2]*Calculation*(xd-xc)*0.5;
 end;
 end;

Int2:=Int2+an[i1]*Int1*(xb-xa)*0.5;
end;
end;

except
SetExtExpression(F1,Func1);
SetExtExpression(F2,Func2);

n1:=n;
n2:=n;
h1:=(b-a)/n1;
Int2:=0;

for j1:=0 to n1-1 do
begin
xa:=a+j1*h1; xb:=a+(j1+1)*h1;
for i1:=1 to 15 do
begin
FV1^:=(xb-xa)*0.5*bn[i1]+(xa+xb)*0.5;


 Int1:=0;
 d:=CalcFunc(Func2);
 c:=CalcFunc(Func1);
 T:=(d-c);
 h2:=T/n2;
 for j2:=0 to n2-1 do
 begin
 xc:=c+j2*h2; xd:=c+(j2+1)*h2;
 for i2:=1 to 15 do
 begin
 FV2^:=(xd-xc)*0.5*bn[i2]+(xc+xd)*0.5;
 Int1:=Int1+an[i2]*Calculation*(xd-xc)*0.5;
 end;
 end;


Int2:=Int2+an[i1]*Int1*(xb-xa)*0.5;
end;
end;
FreeExtFunc(Func1);
FreeExtFunc(Func2);
end;


if abs(Int2) <= zl then Int2:=0;
Integral2:=Int2;
FV1^:=HV1;
FV2^:=HV2;
end;
}



(*
function TMathLib.Integral2(Func: TAddress;  PV1: PFloatType; PV2: PFloatType; a,b: TFloatType; F1,F2: String; n: TIntegType = 1; zl: TFloatType = 1E-14): TFloatType;
var
R,j1,i1,j2,i2,n1,n2:TIntegType;
Int1,Int2,T,HV1,HV2,h1,h2,xa,xb,xc,xd,c,d,h:TFloatType;
Func1,Func2: TAddress;
begin
h:=F_IntegralH;
FV1:=PV1;
FV2:=PV2;
HV1:=FV1^;
HV2:=FV2^;

if n <=0 then n:=1;

try
c:=StrToFloat(F1);
d:=StrToFloat(F2);
Int2:=0;
n1:=n; n2:=n;
h1:=(b-a)/n1; h2:=(d-c)/n2;

for j1:=0 to n1-1 do
begin
xa:=a+j1*h1; xb:=a+(j1+1)*h1;
for i1:=1 to 15 do
begin
FV1^:=(xb-xa)*0.5*bn[i1]+(xa+xb)*0.5;

 Int1:=0;
 for j2:=0 to n2-1 do
 begin
 xc:=c+j2*h2; xd:=c+(j2+1)*h2;
 for i2:=1 to 15 do
 begin
 FV2^:=(xd-xc)*0.5*bn[i2]+(xc+xd)*0.5;
 Int1:=Int1+an[i2]*CalcFunc(Func)*(xd-xc)*0.5;
 end;
 end;

Int2:=Int2+an[i1]*Int1*(xb-xa)*0.5;
end;
end;

except
SetExtExpression(F1,Func1);
SetExtExpression(F2,Func2);

n1:=Trunc(abs(b-a)/h);
n2:=Trunc(1/h);
h1:=(b-a)/n1;
Int2:=0;

for j1:=0 to n1-1 do
begin
xa:=a+j1*h1; xb:=a+(j1+1)*h1;
for i1:=1 to 15 do
begin
FV1^:=(xb-xa)*0.5*bn[i1]+(xa+xb)*0.5;


 Int1:=0;
 d:=CalcFunc(Func2);
 c:=CalcFunc(Func1);
 T:=(d-c);
 h2:=T/n2;
 for j2:=0 to n2-1 do
 begin
 xc:=c+j2*h2; xd:=c+(j2+1)*h2;
 for i2:=1 to 15 do
 begin
 FV2^:=(xd-xc)*0.5*bn[i2]+(xc+xd)*0.5;
 Int1:=Int1+an[i2]*CalcFunc(Func)*(xd-xc)*0.5;
 end;
 end;


Int2:=Int2+an[i1]*Int1*(xb-xa)*0.5;
end;
end;
FreeExtFunc(Func1);
FreeExtFunc(Func2);
end;


if abs(Int2) <= zl then Int2:=0;
Integral2:=Int2;
FV1^:=HV1;
FV2^:=HV2;
end;
*)



function TMathLib.Integral2F(Func: TAddress;  PV1,PV2: PFloatType; a,b: TFloatType; Func1,Func2: TAddress): TFloatType;
var
R,j1,i1,j2,i2,n1,n2:TIntegType;
Int1,Int2,T,HV1,HV2,h1,h2,ax1,ax2,sx1,sx2,c,d,h,x1,x2:TFloatType;
TH: Integer;
begin
TH:=0;
h:=H_Integral;
HV1:=PV1^;
HV2:=PV2^;

n1:=Trunc(abs(b-a)/h);
if n1 = 0 then n1:=1;
h1:=(b-a)/n1;


Int2:=0;

for j1:=0 to n1-1 do
begin
x1:=a+j1*h1; x2:=a+(j1+1)*h1;
ax1:=(x1+x2)*0.5; sx1:=(x2-x1)*0.5;
for i1:=1 to N_Integral do
begin
PV1^:=sx1*bn[i1]+ax1;


 Int1:=0;
 d:=CalcFunc(Func2);
 c:=CalcFunc(Func1);
 n2:=Trunc(abs(d-c)/h);
 if n2 = 0 then n2:=1;
 h2:=(d-c)/n2;

 for j2:=0 to n2-1 do
 begin
 x1:=c+j2*h2; x2:=c+(j2+1)*h2;
 ax2:=(x1+x2)*0.5; sx2:=(x2-x1)*0.5;
 for i2:=1 to N_Integral do
 begin
 PV2^:=sx2*bn[i2]+ax2;
 Int1:=Int1+an[i2]*CalcFunc(Func)*sx2;
 end;
 end;


Int2:=Int2+an[i1]*Int1*sx1;
end;
end;


Integral2F:=Int2;
PV1^:=HV1;
PV2^:=HV2;
end;





function TMathLib.Integral2(Func: TAddress;  PV1,PV2: PFloatType; a,b,c,d: TFloatType): TFloatType;
var
R,j1,i1,j2,i2,n1,n2:TIntegType;
Int1,Int2,T,HV1,HV2,h1,h2,xa,xb,xc,xd,h:TFloatType;
ax1,ax2,sx1,sx2: TFloatType;
begin
h:=H_Integral;
HV1:=PV1^;
HV2:=PV2^;

h:=H_Integral;

n1:=Trunc(abs(b-a)/h);
if n1 = 0 then n1:=1;
h1:=(b-a)/n1;

n2:=Trunc(abs(d-c)/h);
if n2 = 0 then n2:=1;
h2:=(d-c)/n2;

//Form1.L2.Caption:=FloatToStr(n2);
Int2:=0;


for j1:=0 to n1-1 do
begin
xa:=a+j1*h1; xb:=a+(j1+1)*h1;
sx1:=(xb-xa)*0.5; ax1:=(xa+xb)*0.5;
for i1:=1 to N_Integral do
begin
PV1^:=sx1*bn[i1]+ax1;

 Int1:=0;
 for j2:=0 to n2-1 do
 begin
 xc:=c+j2*h2; xd:=c+(j2+1)*h2;
 sx2:=(xd-xc)*0.5; ax2:=(xc+xd)*0.5;
 for i2:=1 to N_Integral do
 begin
 PV2^:=sx2*bn[i2]+ax2;
 Int1:=Int1+an[i2]*CalcFunc(Func)*sx2;
 end;
 end;

Int2:=Int2+an[i1]*Int1*sx1;


end;
end;


Integral2:=Int2;
//Integral2:=FV2^;
PV1^:=HV1;
PV2^:=HV2;
end;





{
function TMathLib.Integral3(PV1: PFloatType; PV2: PFloatType; PV3: PFloatType; a,b: TFloatType; F1,F2,F3,F4: String; n: TIntegType = 1; zl: TFloatType = 1E-14): TFloatType;
var
R,j1,i1,j2,i2,j3,i3,n1,n2,n3:TIntegType;
Int1,Int2,Int3,T2,T3,HV1,HV2,HV3,h1,h2,h3,xa,xb,xc,xd,xe,xf,c,d,e,f:TFloatType;
Func1,Func2,Func3,Func4: TAddress;
begin
FV1:=PV1;
FV2:=PV2;
FV3:=PV3;
HV1:=FV1^;
HV2:=FV2^;
HV3:=FV3^;

if n <=0 then n:=1;

try
c:=StrToFloat(F1);
d:=StrToFloat(F2);
e:=StrToFloat(F3);
f:=StrToFloat(F4);

Int1:=0;
n3:=n; n2:=n;  n1:=n;
h3:=(b-a)/n3; h2:=(d-c)/n2;  h1:=(e-f)/n1;

for j3:=0 to n3-1 do
begin
xa:=a+j3*h3; xb:=a+(j3+1)*h3;
for i3:=1 to 15 do
begin
FV1^:=(xb-xa)*0.5*bn[i3]+(xa+xb)*0.5;

 Int2:=0;
 for j2:=0 to n2-1 do
 begin
 xc:=c+j2*h2; xd:=c+(j2+1)*h2;
 for i2:=1 to 15 do
 begin
 FV2^:=(xd-xc)*0.5*bn[i2]+(xc+xd)*0.5;

  Int1:=0;
  for j1:=0 to n1-1 do
  begin
  xe:=e+j1*h1; xf:=e+(j1+1)*h1;
  for i1:=1 to 15 do
  begin
  FV3^:=(xe-xf)*0.5*bn[i1]+(xe+xf)*0.5;
  Int1:=Int1+an[i1]*Calculation*(xe-xf)*0.5;
  end;
  end;

 Int2:=Int2+an[i2]*Int1*(xd-xc)*0.5;
 end;
 end;

Int3:=Int3+an[i3]*Int2*(xb-xa)*0.5;
end;
end;

except
SetExtExpression(F1,Func1);
SetExtExpression(F2,Func2);
SetExtExpression(F3,Func3);
SetExtExpression(F4,Func4);
Int1:=0;
n3:=n; h3:=(b-a)/n3;
n2:=n; n1:=n;

for j3:=0 to n3-1 do
begin
xa:=a+j3*h3; xb:=a+(j3+1)*h3;
for i3:=1 to 15 do
begin
FV1^:=(xb-xa)*0.5*bn[i3]+(xa+xb)*0.5;

 Int2:=0;
 d:=CalcFunc(Func2);
 c:=CalcFunc(Func1);
 h2:=(d-c)/n2;
 for j2:=0 to n2-1 do
 begin
 xc:=c+j2*h2; xd:=c+(j2+1)*h2;
 for i2:=1 to 15 do
 begin
 FV2^:=(xd-xc)*0.5*bn[i2]+(xc+xd)*0.5;

  Int1:=0;
  f:=CalcFunc(Func4);
  e:=CalcFunc(Func3);
  h1:=(f-e)/n2;
  for j1:=0 to n1-1 do
  begin
  xe:=e+j1*h1; xf:=e+(j1+1)*h1;
  for i1:=1 to 15 do
  begin
  FV3^:=(xf-xe)*0.5*bn[i1]+(xe+xf)*0.5;
  Int1:=Int1+an[i1]*Calculation*(xf-xe)*0.5;
  end;
  end;

 Int2:=Int2+an[i2]*Int1*(xd-xc)*0.5;
 end;
 end;

Int3:=Int3+an[i3]*Int2*(xb-xa)*0.5;
end;
end;

FreeExtFunc(Func1);
FreeExtFunc(Func2);
FreeExtFunc(Func3);
FreeExtFunc(Func4);
end;


if abs(Int3) <= zl then Int3:=0;
Integral3:=Int3;
FV1^:=HV1;
FV2^:=HV2;
FV3^:=HV3;
end;
}







function TMathLib.Integral3FF(Func: TAddress;  PV1,PV2,PV3: PFloatType; a,b: TFloatType; Func1,Func2,Func3,Func4: TAddress): TFloatType;
var
R,j1,i1,j2,i2,j3,i3,n1,n2,n3:TIntegType;
Int1,Int2,Int3,HV1,HV2,HV3,h1,h2,h3,x1,x2,c,d,e,f,h:TFloatType;
ax1,ax2,ax3,sx1,sx2,sx3: TFloatType;
begin
h:=H_Integral;
HV1:=PV1^;
HV2:=PV2^;
HV3:=PV3^;

Int3:=0;
n3:=Trunc(abs(b-a)/h);
if n3 = 0 then n3:=1;
h3:=(b-a)/n3;

for j3:=0 to n3-1 do
begin
x1:=a+j3*h3; x2:=a+(j3+1)*h3;
ax3:=(x1+x2)*0.5; sx3:=(x2-x1)*0.5;
for i3:=1 to N_Integral do
begin
PV1^:=sx3*bn[i3]+ax3;

 Int2:=0;
 d:=CalcFunc(Func2);
 c:=CalcFunc(Func1);
 n2:=Trunc(abs(d-c)/h);
 if n2 = 0 then n2:=1;
 h2:=(d-c)/n2;
 for j2:=0 to n2-1 do
 begin
 x1:=c+j2*h2; x2:=c+(j2+1)*h2;
 ax2:=(x1+x2)*0.5; sx2:=(x2-x1)*0.5;
 for i2:=1 to N_Integral do
 begin
 PV2^:=sx2*bn[i2]+ax2;

  Int1:=0;
  f:=CalcFunc(Func4);
  e:=CalcFunc(Func3);
  n1:=Trunc(abs(f-e)/h);
  if n1 = 0 then n1:=1;
  h1:=(f-e)/n1;
  for j1:=0 to n1-1 do
  begin
  x1:=e+j1*h1; x2:=e+(j1+1)*h1;
  ax1:=(x1+x2)*0.5; sx1:=(x2-x1)*0.5;
  for i1:=1 to N_Integral do
  begin
  PV3^:=sx1*bn[i1]+ax1;
  Int1:=Int1+an[i1]*CalcFunc(Func)*sx1;
  end;
  end;

 Int2:=Int2+an[i2]*Int1*sx2;
 end;
 end;

Int3:=Int3+an[i3]*Int2*sx3;
end;
end;


Integral3FF:=Int3;
PV1^:=HV1;
PV2^:=HV2;
PV3^:=HV3;
end;






function TMathLib.Integral3F(Func: TAddress;  PV1,PV2,PV3: PFloatType; a,b,c,d: TFloatType; Func1,Func2: TAddress): TFloatType;
var
R,j1,i1,j2,i2,j3,i3,n1,n2,n3:TIntegType;
Int1,Int2,Int3,HV1,HV2,HV3,h1,h2,h3,x1,x2,e,f,h:TFloatType;
ax1,ax2,ax3,sx1,sx2,sx3: TFloatType;
begin
h:=H_Integral;
HV1:=PV1^;
HV2:=PV2^;
HV3:=PV3^;

Int3:=0;
n3:=Trunc(abs(b-a)/h);
if n3 = 0 then n3:=1;
h3:=(b-a)/n3;

for j3:=0 to n3-1 do
begin
x1:=a+j3*h3; x2:=a+(j3+1)*h3;
ax3:=(x1+x2)*0.5; sx3:=(x2-x1)*0.5;
for i3:=1 to N_Integral do
begin
PV1^:=sx3*bn[i3]+ax3;

 Int2:=0;
 n2:=Trunc(abs(d-c)/h);
 if n2 = 0 then n2:=1;
 h2:=(d-c)/n2;
 for j2:=0 to n2-1 do
 begin
 x1:=c+j2*h2; x2:=c+(j2+1)*h2;
 ax2:=(x1+x2)*0.5; sx2:=(x2-x1)*0.5;
 for i2:=1 to N_Integral do
 begin
 PV2^:=sx2*bn[i2]+ax2;

  Int1:=0;
  f:=CalcFunc(Func2);
  e:=CalcFunc(Func1);
  n1:=Trunc(abs(f-e)/h);
  if n1 = 0 then n1:=1;
  h1:=(f-e)/n1;
  for j1:=0 to n1-1 do
  begin
  x1:=e+j1*h1; x2:=e+(j1+1)*h1;
  ax1:=(x1+x2)*0.5; sx1:=(x2-x1)*0.5;
  for i1:=1 to N_Integral do
  begin
  PV3^:=sx1*bn[i1]+ax1;
  Int1:=Int1+an[i1]*CalcFunc(Func)*sx1;
  end;
  end;

 Int2:=Int2+an[i2]*Int1*sx2;
 end;
 end;

Int3:=Int3+an[i3]*Int2*sx3;
end;
end;


Integral3F:=Int3;
PV1^:=HV1;
PV2^:=HV2;
PV3^:=HV3;
end;








function TMathLib.Integral3(Func: TAddress;  PV1,PV2,PV3: PFloatType; a,b,c,d,e,f: TFloatType): TFloatType;
var
R,j1,i1,j2,i2,j3,i3,n1,n2,n3,n4:TIntegType;
Int1,Int2,Int3,T,HV1,HV2,HV3,h1,h2,h3,xa,xb,xc,xd,xe,xf,h:TFloatType;
ax1,ax2,ax3,sx1,sx2,sx3: TFloatType;
//Func1,Func2: TAddress;
TH,TH1: Integer; //test
begin
TH:=0; TH1:=0;
h:=H_Integral;
FV1:=PV1;
FV2:=PV2;
FV3:=PV3;
HV1:=FV1^;
HV2:=FV2^;
HV3:=FV3^;

h:=H_Integral;

n1:=Trunc(abs(b-a)/h);
if n1 = 0 then n1:=1;
h1:=(b-a)/n1;

n2:=Trunc(abs(d-c)/h);
if n2 = 0 then n2:=1;
h2:=(d-c)/n2;

n3:=Trunc(abs(f-e)/h);
if n3 = 0 then n3:=1;
h3:=(f-e)/n3;

//Form1.L2.Caption:=FloatToStr(n2);
Int3:=0;


for j1:=0 to n1-1 do
begin
xa:=a+j1*h1; xb:=a+(j1+1)*h1;
ax1:=(xb+xa)*0.5; sx1:=(xb-xa)*0.5;
for i1:=1 to 15 do
begin
FV1^:=sx1*bn[i1]+ax1;

 Int2:=0;
 for j2:=0 to n2-1 do
 begin
 xc:=c+j2*h2; xd:=c+(j2+1)*h2;
 ax2:=(xd+xc)*0.5; sx2:=(xd-xc)*0.5;
 for i2:=1 to 15 do
 begin
 FV2^:=sx2*bn[i2]+ax2;

  Int1:=0;
  for j3:=0 to n3-1 do
  begin
  xe:=e+j3*h3; xf:=e+(j3+1)*h3;
  ax3:=(xf+xe)*0.5; sx3:=(xf-xe)*0.5;
  for i3:=1 to 15 do
  begin
  FV3^:=sx3*bn[i3]+ax3;
  Int1:=Int1+an[i3]*CalcFunc(Func)*sx3;
  //inc(TH);
  end;
  end;

 Int2:=Int2+an[i2]*Int1*sx2;
 //inc(TH);
 end;
 end;

Int3:=Int3+an[i1]*Int2*sx1;

//inc(TH1);
//Unit1.Form1.M2.Lines[TH1-1]:=FloatToStr(Int2)+';'+FloatToStr({FV1^}Int1);
//Unit1.Form1.M2.Lines[TH1]:=FloatToStr(FV1^);


end;
end;


Integral3:=Int3;
//Integral2:=FV2^;
FV1^:=HV1;
FV2^:=HV2;
FV3^:=HV3;
end;






{
function TMathLib.Sum1(PV: PIntegType; a,b: TIntegType): TFloatType;
var
HV1,i: TIntegType;
Sum: TFloatType;
begin

HV1:=PV^;
IV1:=PV;

Sum:=0;
for i:=a to b do
begin
IV1^:=i;
Sum:=Sum+Calculation;
end;

Sum1:=Sum;
PV^:=HV1;

end;
}



function TMathLib.Sum1(Func: TAddress;  PV: PIntegType; a,b: TIntegType): TFloatType;
var
HV1,i: TIntegType;
Sum: TFloatType;
begin
HV1:=PV^;
IV1:=PV;

Sum:=0;
for i:=a to b do
begin
IV1^:=i;
Sum:=Sum+CalcFunc(Func);
end;

Sum1:=Sum;
PV^:=HV1;
end;





function TMathLib.Sum2(Func: TAddress;  PV1,PV2: PIntegType; a,b,c,d: TIntegType): TFloatType;
var
HV1,HV2,i1,i2: TIntegType;
S1,S2: TFloatType;
begin
HV1:=PV1^;
HV2:=PV2^;
IV1:=PV1;
IV2:=PV2;

S1:=0;

for i1:=a to b do
begin
IV1^:=i1;

 S2:=0;
 for i2:=c to d do
 begin
 IV2^:=i2;
 S2:=S2+CalcFunc(Func);
 end;

S1:=S1+S2;
end;


Sum2:=S1;
PV1^:=HV1;
PV2^:=HV2;
end;




{
function TMathLib.Sum2(PV1,PV2: PIntegType; a,b: TIntegType; F1,F2: String): TFloatType;
var
HV1,HV2,i1,i2,c,d: TIntegType;
S1,S2: TFloatType;
Func1,Func2: TAddress;
begin
HV1:=PV1^;
HV2:=PV2^;
IV1:=PV1;
IV2:=PV2;

try
S2:=0;
c:=StrToInt(F1);
d:=StrToInt(F2);

for i1:=a to b do
begin
IV1^:=i1;

 S2:=0;
 for i2:=c to d do
 begin
 IV2^:=i2;
 S2:=S2+Calculation;
 end;

S1:=S1+S2;
end;

except

SetExtExpression(F1,Func1);
SetExtExpression(F2,Func2);
for i1:=a to b do
begin
IV1^:=i1;

 S2:=0;
 c:=Trunc(CalcFunc(Func1));
 d:=Trunc(CalcFunc(Func2));
 for i2:=c to d do
 begin
 IV2^:=i2;
 S2:=S2+Calculation;
 end;

S1:=S1+S2;
end;
FreeExtFunc(Func1);
FreeExtFunc(Func2);
end;


Sum2:=S1;
PV1^:=HV1;
PV2^:=HV2;
end;
}




function TMathLib.Sum2(Func: TAddress;  PV1,PV2: PIntegType; a,b: TIntegType; F1,F2: String): TFloatType;
var
HV1,HV2,i1,i2,c,d: TIntegType;
S1,S2: TFloatType;
Func1,Func2: TAddress;
begin
HV1:=PV1^;
HV2:=PV2^;
IV1:=PV1;
IV2:=PV2;

try
S1:=0;
c:=StrToInt(F1);
d:=StrToInt(F2);

for i1:=a to b do
begin
IV1^:=i1;

 S2:=0;
 for i2:=c to d do
 begin
 IV2^:=i2;
 S2:=S2+CalcFunc(Func);
 end;

S1:=S1+S2;
end;

except
S1:=0;
//SetExtExpression(F1,Func1);
//SetExtExpression(F2,Func2);
for i1:=a to b do
begin
IV1^:=i1;

 S2:=0;
 c:=Trunc(CalcFunc(Func1));
 d:=Trunc(CalcFunc(Func2));
 for i2:=c to d do
 begin
 IV2^:=i2;
 S2:=S2+CalcFunc(Func);
 end;

S1:=S1+S2;
end;
//FreeExtFunc(Func1);
//FreeExtFunc(Func2);
end;


Sum2:=S1;
PV1^:=HV1;
PV2^:=HV2;
end;





{
function TMathLib.Sum3(PV1,PV2,PV3: PIntegType; a,b: TIntegType; F1,F2,F3,F4: String): TFloatType;
var
HV1,HV2,HV3,i1,i2,i3,c,d,e,f: TIntegType;
S1,S2,S3: TFloatType;
Func1,Func2,Func3,Func4: TAddress;
begin
HV1:=PV1^;
HV2:=PV2^;
HV3:=PV3^;
IV1:=PV1;
IV2:=PV2;
IV3:=PV3;

try
S1:=0;
c:=StrToInt(F1);
d:=StrToInt(F2);
e:=StrToInt(F3);
f:=StrToInt(F4);

for i1:=a to b do
begin
IV1^:=i1;

 S2:=0;
 for i2:=c to d do
 begin
 IV2^:=i2;

  S3:=0;
  for i3:=e to f do
  begin
  IV3^:=i3;
  S3:=S3+Calculation;
  end;

 S2:=S2+S3;
 end;

S1:=S1+S2;
end;

except

SetExtExpression(F1,Func1);
SetExtExpression(F2,Func2);
SetExtExpression(F3,Func3);
SetExtExpression(F4,Func4);

for i1:=a to b do
begin
IV1^:=i1;

 S2:=0;
 c:=Trunc(CalcFunc(Func1));
 d:=Trunc(CalcFunc(Func2));

 for i2:=c to d do
 begin
 IV2^:=i2;

  S3:=0;
  e:=Trunc(CalcFunc(Func3));
  f:=Trunc(CalcFunc(Func4));
  for i3:=e to f do
  begin
  IV3^:=i3;
  S2:=S2+Calculation;
  end;

 S2:=S2+S3;
 end;

S1:=S1+S2;
end;
FreeExtFunc(Func1);
FreeExtFunc(Func2);
FreeExtFunc(Func3);
FreeExtFunc(Func4);
end;


Sum3:=S1;
PV1^:=HV1;
PV2^:=HV2;
PV3^:=HV3;
end;
}



function TMathLib.Sum3(Func: TAddress;  PV1,PV2,PV3: PIntegType; a,b: TIntegType; F1,F2,F3,F4: String): TFloatType;
var
HV1,HV2,HV3,i1,i2,i3,c,d,e,f: TIntegType;
S1,S2,S3: TFloatType;
Func1,Func2,Func3,Func4: TAddress;
begin
HV1:=PV1^;
HV2:=PV2^;
HV3:=PV3^;
IV1:=PV1;
IV2:=PV2;
IV3:=PV3;

try
S1:=0;
c:=StrToInt(F1);
d:=StrToInt(F2);
e:=StrToInt(F3);
f:=StrToInt(F4);

for i1:=a to b do
begin
IV1^:=i1;

 S2:=0;
 for i2:=c to d do
 begin
 IV2^:=i2;

  S3:=0;
  for i3:=e to f do
  begin
  IV3^:=i3;
  S3:=S3+CalcFunc(Func);
  end;

 S2:=S2+S3;
 end;

S1:=S1+S2;
end;

except

//SetExtExpression(F1,Func1);
//SetExtExpression(F2,Func2);
//SetExtExpression(F3,Func3);
//SetExtExpression(F4,Func4);

for i1:=a to b do
begin
IV1^:=i1;

 S2:=0;
 c:=Trunc(CalcFunc(Func1));
 d:=Trunc(CalcFunc(Func2));

 for i2:=c to d do
 begin
 IV2^:=i2;

  S3:=0;
  e:=Trunc(CalcFunc(Func3));
  f:=Trunc(CalcFunc(Func4));
  for i3:=e to f do
  begin
  IV3^:=i3;
  S2:=S2+CalcFunc(Func);
  end;

 S2:=S2+S3;
 end;

S1:=S1+S2;
end;
//FreeExtFunc(Func1);
//FreeExtFunc(Func2);
//FreeExtFunc(Func3);
//FreeExtFunc(Func4);
end;


Sum3:=S1;
PV1^:=HV1;
PV2^:=HV2;
PV3^:=HV3;
end;





function TMathLib.Sum3(Func: TAddress;  PV1,PV2,PV3: PIntegType; a,b: TIntegType; Func1: TAddress;  Func2: TAddress;   Func3: TAddress;  Func4: TAddress): TFloatType;
var
HV1,HV2,HV3,i1,i2,i3,c,d,e,f: TIntegType;
S1,S2,S3: TFloatType;
//Func1,Func2,Func3,Func4: TAddress;
begin
HV1:=PV1^;
HV2:=PV2^;
HV3:=PV3^;
IV1:=PV1;
IV2:=PV2;
IV3:=PV3;

{
try
S1:=0;
c:=StrToInt(F1);
d:=StrToInt(F2);
e:=StrToInt(F3);
f:=StrToInt(F4);

for i1:=a to b do
begin
IV1^:=i1;

 S2:=0;
 for i2:=c to d do
 begin
 IV2^:=i2;

  S3:=0;
  for i3:=e to f do
  begin
  IV3^:=i3;
  S3:=S3+CalcFunc(Func);
  end;

 S2:=S2+S3;
 end;

S1:=S1+S2;
end;

except
SetExtExpression(F1,Func1);
SetExtExpression(F2,Func2);
SetExtExpression(F3,Func3);
SetExtExpression(F4,Func4);
}
for i1:=a to b do
begin
IV1^:=i1;

 S2:=0;
 c:=Trunc(CalcFunc(Func1));
 d:=Trunc(CalcFunc(Func2));

 for i2:=c to d do
 begin
 IV2^:=i2;

  S3:=0;
  e:=Trunc(CalcFunc(Func3));
  f:=Trunc(CalcFunc(Func4));
  for i3:=e to f do
  begin
  IV3^:=i3;
  S2:=S2+CalcFunc(Func);
  end;

 S2:=S2+S3;
 end;

S1:=S1+S2;
end;
{
FreeExtFunc(Func1);
FreeExtFunc(Func2);
FreeExtFunc(Func3);
FreeExtFunc(Func4);
end;
}

Sum3:=S1;
PV1^:=HV1;
PV2^:=HV2;
PV3^:=HV3;
end;





function TMathLib.Integral1(F,X: TArray1; a,b: TFloatType; n: TIntegType = 1; zl: TFloatType = 1E-10): TFloatType;
var
x1,x2,v,Int,h: TFloatType;
j,i,SL1: TIntegType;
B3,B2,B1,B0,PS1: TArray1;
SP1: TSplain;
begin
B3:=A3; B2:=A2; B1:=A1; B0:=A0; SL1:=SL; PS1:=PS;

CreateSplain3(X,F,SP1);

if n <=0 then n:=1;

if n > 0 then
begin
h:=(b-a)/n; Int:=0;
for j:=0 to n-1 do
begin
x1:=a+j*h; x2:=a+(j+1)*h;
for i:=1 to 15 do
begin
v:=(x2-x1)*0.5*bn[i]+(x1+x2)*0.5;
Int:=Int+an[i]*Splain3(v,SP1)*(x2-x1)*0.5;;
end;
end;
end;

if abs(Int) <= zl then Int:=0;
Integral1:=Int;

A3:=B3; A2:=B2; A1:=B1; A0:=B0; SL:=SL1; PS:=PS1;
end;





function TMathLib.IntegralN(N: Integer; A,B: TArray1; VL: PArray1; Func: TAddress): TFloatType;
label endp;
var
HX,SVL: TArray1;
NX: TArrayI;
L,L1,L2,L3,nn,i: Integer;
h: TFloatType;

//рекурентная ф-я вычисления кратного инт-ла
function IntRec(K: Integer): TFloatType;
var
x1,x2,RI,Int,ax,sx: TFloatType;
i,j: Integer;
begin
if K = 0 then
begin
 Int:=0;
 for j:=0 to NX[K]-1 do
 begin
 x1:=A[K]+j*HX[K]; x2:=A[K]+(j+1)*HX[K];
 sx:=(x2-x1)*0.5; ax:=(x1+x2)*0.5;
 for i:=1 to N_Integral do
 begin
 VL[K]^:=sx*bn[i]+ax;
 Int:=Int+an[i]*CalcFunc(Func)*sx;
 end;
 end;
end
else
begin
 Int:=0;
 for j:=0 to NX[K]-1 do
 begin
 x1:=A[K]+j*HX[K]; x2:=A[K]+(j+1)*HX[K];
 sx:=(x2-x1)*0.5; ax:=(x1+x2)*0.5;
 for i:=1 to N_Integral do
 begin
 VL[K]^:=sx*bn[i]+ax;
 RI:=IntRec(K-1);
 Int:=Int+an[i]*RI*sx;
 end;
 end;
end;


IntRec:=Int;
end;


begin
L:=Length(VL); L1:=Length(A); L2:=Length(B); L3:=Length(M_IntegralH);
if (L1 <> L2) or (L <> L1) or (N <> L) then begin {ERROR} goto endp; end;
if (F_Integral_MultyH = 1) and (L3 <> N) then begin {ERROR} goto endp; end;
SetLength(HX,L);  //приращения
SetLength(SVL,L); //сохр. перем.
SetLength(NX,L);  //разбиения

N:=N-1;
//сохранение переменных:
for i:=0 to N do
begin
 SVL[i]:=VL[i]^;
end;

//расчёт приращений:
for i:=0 to N do
begin
 if F_Integral_MultyH = 1 then h:=M_IntegralH[i] else h:=H_Integral;
 nn:=Trunc(abs(B[i]-A[i])/h);
 if nn = 0 then nn:=1;
 NX[i]:=nn;
 HX[i]:=(B[i]-A[i])/nn;
end;


IntegralN:=IntRec(N);


//восстановление переменных:
for i:=0 to N do
begin
 VL[i]^:=SVL[i];
end;

endp:
end;





function TMathLib.IntegralNF(N: Integer; FuncN: TArrayA; VL: PArray1; Func: TAddress): TFloatType;
label endp;
var
HX,SVL: TArray1;
L,L1,L2,L3,nn,i: Integer;
h: TFloatType;

//рекурентная ф-я вычисления кратного инт-ла
function IntRec(K: Integer): TFloatType;
var
x1,x2,x01,x02,RI,Int,ax,sx,h: TFloatType;
i,j,n: Integer;
begin
if K = 0 then
begin
 Int:=0;
 x01:=CalcFunc(FuncN[0]);
 x02:=CalcFunc(FuncN[1]);
 n:=Trunc(abs(x02-x01)/HX[0]);
 if n = 0 then n:=1;
 h:=(x02-x01)/n;

 for j:=0 to n-1 do
 begin
 x1:=x01+j*h; x2:=x01+(j+1)*h;
 sx:=(x2-x1)*0.5; ax:=(x1+x2)*0.5;
 for i:=1 to N_Integral do
 begin
 VL[K]^:=sx*bn[i]+ax;
 Int:=Int+an[i]*CalcFunc(Func)*sx;
 end;
 end;
end
else
begin
 Int:=0;
 x01:=CalcFunc(FuncN[2*K]);
 x02:=CalcFunc(FuncN[2*K+1]);
 n:=Trunc(abs(x02-x01)/HX[K]);
 if n = 0 then n:=1;
 h:=(x02-x01)/n;

 for j:=0 to n-1 do
 begin
 x1:=x01+j*h; x2:=x01+(j+1)*h;
 sx:=(x2-x1)*0.5; ax:=(x1+x2)*0.5;
 for i:=1 to N_Integral do
 begin
 VL[K]^:=sx*bn[i]+ax;
 RI:=IntRec(K-1);
 Int:=Int+an[i]*RI*sx;
 end;
 end;
end;


IntRec:=Int;
end;


begin
L:=Length(VL); L1:=Length(FuncN);  L3:=Length(M_IntegralH);
if (L <> N) or (2*N <> L1) then begin {ERROR} goto endp; end;
if (F_Integral_MultyH = 1) and (L3 <> N) then begin {ERROR} goto endp; end;
SetLength(HX,L);  //приращения
SetLength(SVL,L); //сохр. перем.


N:=N-1;
//сохранение переменных:
for i:=0 to N do
begin
 SVL[i]:=VL[i]^;
end;

//расчёт приращений:
for i:=0 to N do
begin
 if F_Integral_MultyH = 1 then h:=M_IntegralH[i] else h:=H_Integral;
 HX[i]:=h;
end;


IntegralNF:=IntRec(N);


//восстановление переменных:
for i:=0 to N do
begin
 VL[i]^:=SVL[i];
end;

endp:
end;






function TMathLib.IntegralMK(N: Integer; A,B: TArray1; VL: PArray1; Func: TAddress): TFloatType;
label endp;
var
SVL: TArray1;
L,L1,L2,L3,nn,i,j: Integer;
Int: TFloatType;
begin
L:=Length(VL); L1:=Length(A); L2:=Length(B); 
if (L1 <> L2) or (L <> L1) or (N <> L) then begin {ERROR} goto endp; end;

SetLength(SVL,L); //сохр. перем.


//сохранение переменных:
for i:=0 to N-1 do
begin
 SVL[i]:=VL[i]^;
end;

Int:=0;
Randomize;
for i:=1 to N_IntegralMK do
begin
 for j:=0 to N-1 do
 begin
  VL[j]^:=(B[j]-A[j])*Random+A[j];
 end;
 Int:=Int+CalcFunc(Func);
end;


//восстановление переменных:
for i:=0 to N-1 do
begin
 VL[i]^:=SVL[i];
end;

//нормировка: если мера объёма <> 1
for i:=0 to N-1 do
begin
Int:=Int*(B[i]-A[i]);
end;

IntegralMK:=Int/N_IntegralMK;
endp:
end;





function TMathLib.IntegralMKF(N: Integer; FuncN: TArrayA; VL: PArray1; Func: TAddress): TFloatType;
label endp;
var
SVL: TArray1;
L,L1,L2,L3,nn,i,j: Integer;
Int,x1,x2,LX: TFloatType;
begin
L:=Length(VL); L1:=Length(FuncN);
if (L <> N) or (2*N <> L1) then begin {ERROR} goto endp; end;

SetLength(SVL,L); //сохр. перем.


//сохранение переменных:
for i:=0 to N-1 do
begin
 SVL[i]:=VL[i]^;
end;



Int:=0;
Randomize;
for i:=1 to N_IntegralMK do
begin
 LX:=1;
 for j:=N-1 downto 0 do
 begin
  x1:=CalcFunc(FuncN[2*j]); x2:=CalcFunc(FuncN[2*j+1]);
  VL[j]^:=(x2-x1)*Random+x1;
  LX:=LX*(x2-x1); //нормировка по объёму
 end;
 Int:=Int+LX*CalcFunc(Func);
end;




//восстановление переменных:
for i:=0 to N-1 do
begin
 VL[i]^:=SVL[i];
end;



IntegralMKF:=Int/N_IntegralMK;
endp:
end;



function TMathLib.DERK6(Func:TAddress; PX,PY:PFloatType; X0,Y0,X: TFloatType): TFloatType;
var
Yk,Xk,K1,K2,K3,K4,K5,K6,SX,SY,h: TFloatType;
T1,T2,i,n: LongInt;
begin
//RK 6-го порядка в точке X (для ур-я 1-ой степени)
h:=0.01;

SX:=PX^; SY:=PY^;

n:=Trunc(abs(X-X0)/h);
h:=(X-X0)/n;

Xk:=X0; Yk:=Y0;


for i:=1 to n do
begin
PX^:=Xk; PY^:=Yk;
K1:=CalcFunc(Func)*h;
PX^:=Xk+h*0.5; PY^:=Yk+K1*0.5;
K2:=CalcFunc(Func)*h;
PY^:=Yk+(K1+K2)*0.25;
K3:=CalcFunc(Func)*h;
PX^:=Xk+h; PY^:=Yk-K2+K3*2;
K4:=CalcFunc(Func)*h;
PX^:=Xk+h*0.66666666666666666666; PY^:=Yk+(7*K1+10*K2+K4)*0.037037037037037037037037;
K5:=CalcFunc(Func)*h;
PX^:=Xk+h*0.2; PY^:=Yk+(28*K1-125*K2+546*K3+54*K4-378*K5)*0.016;
K6:=CalcFunc(Func)*h;

Yk:=Yk+(K1+4*K3+K4)*0.166666666666666666666;
Xk:=Xk+h;
end;

PX^:=SX;
PY^:=SY;


DERK6:=Yk;
end;





procedure TMathLib.DERK6A(Func:TAddress; PX,PY:PFloatType; X0,Y0,X: TFloatType; var A: TArray1);
var
Yk,Xk,K1,K2,K3,K4,K5,K6,SX,SY,h: TFloatType;
T1,T2,i,n: LongInt;
begin
//RK 6-го порядка на отрезке [X0,X]
h:=0.1;

SX:=PX^; SY:=PY^;

n:=Trunc(abs(X-X0)/h);
h:=(X-X0)/n;
SetLength(A,n);

Xk:=X0; Yk:=Y0;


for i:=1 to n do
begin
PX^:=Xk; PY^:=Yk;
K1:=CalcFunc(Func)*h;
PX^:=Xk+h*0.5; PY^:=Yk+K1*0.5;
K2:=CalcFunc(Func)*h;
PY^:=Yk+(K1+K2)*0.25;
K3:=CalcFunc(Func)*h;
PX^:=Xk+h; PY^:=Yk-K2+K3*2;
K4:=CalcFunc(Func)*h;
PX^:=Xk+h*0.66666666666666666666; PY^:=Yk+(7*K1+10*K2+K4)*0.037037037037037037037037;
K5:=CalcFunc(Func)*h;
PX^:=Xk+h*0.2; PY^:=Yk+(28*K1-125*K2+546*K3+54*K4-378*K5)*0.016;
K6:=CalcFunc(Func)*h;

Yk:=Yk+(K1+4*K3+K4)*0.166666666666666666666;
Xk:=Xk+h;
A[i-1]:=Yk;
end;

PX^:=SX;
PY^:=SY;

end;








procedure TMathLib.DERK6Sys(F:TArrayA; PV: PArray1;  Y0: TArray1; X0,X: TFloatType; var RV: TArray1);
var
Xk,h: TFloatType;
T1,T2,i,n,j: LongInt;
SV,Yk,K1,K2,K3,K4,K5,K6: TArray1;
begin
//RK 6-го порядка для системы (1-го порядка)
h:=0.00001;



n:=Trunc(abs(X-X0)/h);
h:=(X-X0)/n;

SetLength(SV,Length(PV));
SetLength(Yk,Length(Y0));
for i:=0 to High(PV) do
begin
 SV[i]:=PV[i]^;
end;

Xk:=X0;
for i:=0 to High(Y0) do
begin
 Yk[i]:=Y0[i];
end;

SetLength(K1,Length(F));
SetLength(K2,Length(F));
SetLength(K3,Length(F));
SetLength(K4,Length(F));
SetLength(K5,Length(F));
SetLength(K6,Length(F));


for i:=1 to n do
begin
//1
 PV[0]^:=Xk;
 for j:=1 to High(PV) do
 begin
  PV[j]^:=Yk[j-1];
 end;

 for j:=0 to High(F) do
 begin
  K1[j]:=CalcFunc(F[j])*h;
 end;
//2
 PV[0]^:=Xk+h*0.5;
 for j:=1 to High(PV) do
 begin
  PV[j]^:=Yk[j-1]+K1[j-1]*0.5;
 end;

 for j:=0 to High(F) do
 begin
  K2[j]:=CalcFunc(F[j])*h;
 end;
//3
 for j:=1 to High(PV) do
 begin
  PV[j]^:=Yk[j-1]+(K1[j-1]+K2[j-1])*0.25;
 end;

 for j:=0 to High(F) do
 begin
  K3[j]:=CalcFunc(F[j])*h;
 end;
//4
 PV[0]^:=Xk+h;
 for j:=1 to High(PV) do
 begin
  PV[j]^:=Yk[j-1]-K2[j-1]+K3[j-1]*2;
 end;

 for j:=0 to High(F) do
 begin
  K4[j]:=CalcFunc(F[j])*h;
 end;
//5
 PV[0]^:=Xk+h*0.66666666666666666666;
 for j:=1 to High(PV) do
 begin
  PV[j]^:=Yk[j-1]+(7*K1[j-1]+10*K2[j-1]+K4[j-1])*0.037037037037037037037037;
 end;

 for j:=0 to High(F) do
 begin
  K5[j]:=CalcFunc(F[j])*h;
 end;
//6
 PV[0]^:=Xk+h*0.2;
 for j:=1 to High(PV) do
 begin
  PV[j]^:=Yk[j-1]+(28*K1[j-1]-125*K2[j-1]+546*K3[j-1]+54*K4[j-1]-378*K5[j-1])*0.016;
 end;

 for j:=0 to High(F) do
 begin
  K6[j]:=CalcFunc(F[j])*h;
 end;

 Xk:=Xk+h;
 for j:=1 to High(PV) do
 begin
  Yk[j-1]:=Yk[j-1]+(K1[j-1]+4*K3[j-1]+K4[j-1])*0.166666666666666666666;
 end;


end;


SetLength(RV,Length(Yk));
for i:=0 to High(Yk) do
begin
 RV[i]:=Yk[i];
end;

for i:=0 to High(PV) do
begin
 PV[i]^:=SV[i];
end;


end;







function TMathLib.DERK6N(F:TAddress; PV: PArray1;  Y0: TArray1; X0,X: TFloatType): TFloatType;
var
Xk,h: TFloatType;
T1,T2,i,n,j,nh: LongInt;
SV,Yk,K1,K2,K3,K4,K5,K6: TArray1;
begin
//RK 6-го порядка в точке X (для ур-я N-ой степени путём сведения к системе 1-го порядка)
h:=0.00001;




n:=Trunc(abs(X-X0)/h);
h:=(X-X0)/n;

SetLength(SV,Length(PV));
SetLength(Yk,Length(Y0));
for i:=0 to High(PV) do
begin
 SV[i]:=PV[i]^;
end;

Xk:=X0;
for i:=0 to High(Y0) do
begin
 Yk[i]:=Y0[i];
end;

nh:=High(Y0);
SetLength(K1,nh+1);
SetLength(K2,nh+1);
SetLength(K3,nh+1);
SetLength(K4,nh+11);
SetLength(K5,nh+1);
SetLength(K6,nh+1);


for i:=1 to n do
begin
//1
 PV[0]^:=Xk;
 for j:=1 to High(PV) do
 begin
  PV[j]^:=Yk[j-1];
 end;

 for j:=0 to nh-1 do
 begin
  K1[j]:=PV[j+2]^*h;
 end;

 K1[nh]:=CalcFunc(F)*h;


//2
 PV[0]^:=Xk+h*0.5;
 for j:=1 to High(PV) do
 begin
  PV[j]^:=Yk[j-1]+K1[j-1]*0.5;
 end;

 for j:=0 to nh-1 do
 begin
  K2[j]:=PV[j+2]^*h;
 end;

 K2[nh]:=CalcFunc(F)*h;

//3
 for j:=1 to High(PV) do
 begin
  PV[j]^:=Yk[j-1]+(K1[j-1]+K2[j-1])*0.25;
 end;

 for j:=0 to nh-1 do
 begin
  K3[j]:=PV[j+2]^*h;
 end;

 K3[nh]:=CalcFunc(F)*h;

//4
 PV[0]^:=Xk+h;
 for j:=1 to High(PV) do
 begin
  PV[j]^:=Yk[j-1]-K2[j-1]+K3[j-1]*2;
 end;

 for j:=0 to nh-1 do
 begin
  K4[j]:=PV[j+2]^*h;
 end;

 K4[nh]:=CalcFunc(F)*h;

//5
 PV[0]^:=Xk+h*0.66666666666666666666;
 for j:=1 to High(PV) do
 begin
  PV[j]^:=Yk[j-1]+(7*K1[j-1]+10*K2[j-1]+K4[j-1])*0.037037037037037037037037;
 end;

 for j:=0 to nh-1 do
 begin
  K5[j]:=PV[j+2]^*h;
 end;

 K5[nh]:=CalcFunc(F)*h;

//6
 PV[0]^:=Xk+h*0.2;
 for j:=1 to High(PV) do
 begin
  PV[j]^:=Yk[j-1]+(28*K1[j-1]-125*K2[j-1]+546*K3[j-1]+54*K4[j-1]-378*K5[j-1])*0.016;
 end;

 for j:=0 to nh-1 do
 begin
  K6[j]:=PV[j+2]^*h;
 end;

 K6[nh]:=CalcFunc(F)*h;


 Xk:=Xk+h;
 for j:=1 to High(PV) do
 begin
  Yk[j-1]:=Yk[j-1]+(K1[j-1]+4*K3[j-1]+K4[j-1])*0.166666666666666666666;
 end;


end;


for i:=0 to High(PV) do
begin
 PV[i]^:=SV[i];
end;

DERK6N:=Yk[0];
end;







{
procedure TMathLib.Fourie(PV: PFloatType; a,b: TFloatType;  NK: TIntegType; var FS,FC: TArray1; n: TIntegType = 1; zl: TFloatType = 1E-14);
var
CS,CC,l,x1,x2,Al,H1,h: TFloatType;  //отсчёт коэффицентов с '0'
j,i,k: TIntegType;
begin
FV1:=PV;
H1:=FV1^;
l:=abs(b-a);
if n <=0 then n:=1;
h:=(b-a)/n;
SetLength(FS,NK+2); SetLength(FC,NK+2);

for k:=0 to NK do
begin
Al:=k*2*_pi/l;   CS:=0; CC:=0;

 for j:=0 to n-1 do
 begin
 x1:=a+j*h; x2:=a+(j+1)*h;
 for i:=1 to 15 do
 begin
 FV1^:=(x2-x1)*0.5*bn[i]+(x1+x2)*0.5;
 CS:=CS+an[i]*Calculation*sin(Al*FV1^)*(x2-x1)*0.5;
 CC:=CC+an[i]*Calculation*cos(Al*FV1^)*(x2-x1)*0.5;
 end;
 end;

if abs(2*CS/l) < zl then CS:=0;
if abs(2*CC/l) < zl then CC:=0;
FS[k]:=2*CS/l;
FC[k]:=2*CC/l;
end;

FV1^:=H1;
end;
}





procedure TMathLib.Fourie(Func: TAddress;  PV: PFloatType; a,b: TFloatType;  NK: TIntegType; var FS,FC: TArray1);
var
CS,CC,l,x1,x2,Al,H1,h,sx,ax: TFloatType;
j,i,k,n: TIntegType;
begin
h:=H_Integral;
H1:=PV^;
h:=H_Integral;
n:=Trunc(abs(b-a)/h);
if n = 0 then n:=1;
h:=(b-a)/n;

l:=abs(b-a);
SetLength(FS,NK+2); SetLength(FC,NK+2);

for k:=0 to NK do
begin
Al:=k*2*_pi/l;   CS:=0; CC:=0;

 for j:=0 to n-1 do
 begin
 x1:=a+j*h; x2:=a+(j+1)*h;
 ax:=(x1+x2)*0.5; sx:=(x2-x1)*0.5;
 for i:=1 to N_Integral do
 begin
 PV^:=sx*bn[i]+ax;
 CS:=CS+an[i]*CalcFunc(Func)*sin(Al*PV^)*sx;
 CC:=CC+an[i]*CalcFunc(Func)*cos(Al*PV^)*sx;
 end;
 end;


FS[k]:=2*CS/l;
FC[k]:=2*CC/l;
end;

PV^:=H1;
end;








{
procedure TMathLib.Fourie(F: TArrayS1; X: TArray1; PV: PFloatType;  NK: TIntegType; var FS,FC: TArray1; n: TIntegType = 1; zl: TFloatType = 1E-14);
var
CS,CC,l,x1,x2,Int,Al,H1,h: TFloatType;  //отсчёт коэффицентов с '0'
j,i,k,m,nf: TIntegType;                 //F,X - отсчёт с '1'
FA: array of TAddress;
begin
FV1:=PV;
H1:=FV1^;
if n <=0 then n:=1;
SetLength(FS,NK+2); SetLength(FC,NK+2);
nf:=High(F); SetLength(FA,nf+1);
l:=abs(X[nf+1]-X[1]);

for i:=1 to nf do
begin
SetExtExpression(F[i],FA[i]);
end;

for k:=0 to NK do
begin
 CS:=0; CC:=0;

 for m:=1 to nf do
 begin

  h:=(X[m+1]-X[m])/n;
  Al:=k*2*_pi/l;
  for j:=0 to n-1 do
  begin
  x1:=X[m]+j*h; x2:=X[m]+(j+1)*h;
  for i:=1 to 15 do
  begin
  FV1^:=(x2-x1)*0.5*bn[i]+(x1+x2)*0.5;
  CS:=CS+an[i]*CalcFunc(FA[m])*sin(Al*FV1^)*(x2-x1)*0.5;
  CC:=CC+an[i]*CalcFunc(FA[m])*cos(Al*FV1^)*(x2-x1)*0.5;
  end;
  end;

 end;

if abs(2*CS/l) < zl then CS:=0;
if abs(2*CC/l) < zl then CC:=0;
FS[k]:=2*CS/l;
FC[k]:=2*CC/l;
end;

FV1^:=H1;
for i:=1 to nf do
begin
FreeExtFunc(FA[i]);
end;
end;
}


procedure TMathLib.Fourie(F: TArrayA; X: TArray1; PV: PFloatType;  NK: TIntegType; var FS,FC: TArray1);
label endp;
var
CS,CC,l,x1,x2,Int,Al,H1,ax,sx,h: TFloatType;
j,i,k,m,nf,nx,n: TIntegType;
begin
H1:=PV^;
h:=H_Integral;


nf:=High(F); nx:=High(X);
if nx <> nf+1 then begin {ERROR} goto endp; end;


SetLength(FS,NK+2); SetLength(FC,NK+2);
l:=abs(X[nx]-X[0]);



for k:=0 to NK do
begin
 CS:=0; CC:=0;

 for m:=0 to nf do
 begin


  n:=Trunc(abs(X[m+1]-X[m])/h);
  if n = 0 then n:=1;
  h:=(X[m+1]-X[m])/n;
  Al:=k*2*_pi/l;
  for j:=0 to n-1 do
  begin
  x1:=X[m]+j*h; x2:=X[m]+(j+1)*h;
  ax:=(x1+x2)*0.5; sx:=(x2-x1)*0.5;
  for i:=1 to N_Integral do
  begin
  PV^:=sx*bn[i]+ax;
  CS:=CS+an[i]*CalcFunc(F[m])*sin(Al*PV^)*sx;
  CC:=CC+an[i]*CalcFunc(F[m])*cos(Al*PV^)*sx;
  end;
  end;

 end;


FS[k]:=2*CS/l;
FC[k]:=2*CC/l;
end;

PV^:=H1;

endp:
end;




procedure TMathLib.LSM(X,Y: TArray1; M: Cardinal; var PL: TArray1);
label endp;
var                     //M-1 - степень полинома
i,j,k,L1,L2,N: Cardinal;
S: TArray2;
R: TArray1;
t,TS,TR: TFloatType;
begin
L1:=Length(X); L2:=Length(Y);
if (L1 <> L2) or (M > L1) then begin {ERROR} goto endp; end;
N:=L1;


if M = N then
begin
  SetLength(S,N,N);
  SetLength(R,N);
  for i:=0 to N-1 do
  begin
   for j:=0 to N-1 do
   begin
    S[i,j]:=IntPower(X[i],j);
   end;
   R[i]:=Y[i];
  end;

  LSE(S,R,PL);
end
else
begin
  SetLength(S,M,M);
  SetLength(R,M);
  for j:=0 to M-1 do
  begin

   for k:=0 to M-1 do
   begin
    TS:=0;
    for i:=0 to N-1 do
    begin
     TS:=TS+IntPower(X[i],j)*IntPower(X[i],k);
    end;
    S[j,k]:=TS;
   end;

   TR:=0;
   for i:=0 to N-1 do
   begin
    TR:=TR+Y[i]*IntPower(X[i],j);
   end;
    R[j]:=TR;
  end;

  LSE(S,R,PL);
end;



for i:=0 to High(PL) div 2 do
begin
 t:=PL[i];
 PL[i]:=PL[M-1-i];
 PL[M-1-i]:=t;
end;

endp:
end;



procedure TMathLib.LSMI(Func: TAddress; PV: PFloatType; a,b: TFloatType; M: Cardinal; var PL: TArray1);
var                          //M-1 - степень полинома
S: TArray2;                  //интегральный LSM
R: TArray1;
i,j: Cardinal;
t: TFloatType;
begin
SetLength(S,M,M);
SetLength(R,M);

for i:=0 to M-1 do
begin
 for j:=0 to M-1 do
 begin
  S[i,j]:=(IntPower(b,j+i+1)-intPower(a,j+i+1))/(i+j+1);
 end;

 R[i]:=Integral1PL(Func,PV,i,a,b);
end;

LSE(S,R,PL);

for i:=0 to High(PL) div 2 do
begin
 t:=PL[i];
 PL[i]:=PL[M-1-i];
 PL[M-1-i]:=t;
end;

end;



function TMathLib.Integral1PL(Func: TAddress;  PV: PFloatType; M: Cardinal; a,b: TFloatType): TFloatType;
var                           //используется в LSMI
j,i,n:TIntegType;
x1,x2,Int,H1,hax,sx,ax,h:TFloatType;
begin
H1:=PV^;

h:=H_Integral;
n:=Trunc(abs(b-a)/h);
if n = 0 then n:=1;
h:=(b-a)/n;


Int:=0;
for j:=0 to n-1 do
begin
x1:=a+j*h; x2:=a+(j+1)*h;
ax:=(x1+x2)*0.5; sx:=(x2-x1)*0.5;
for i:=1 to N_Integral do
begin
PV^:=sx*bn[i]+ax;
Int:=Int+an[i]*IntPower(PV^,M)*CalcFunc(Func)*sx;
end;
end;


Integral1PL:=Int;
PV^:=H1;
end;



function  TMathLib.Factorial(N: Integer): TFloatType;
begin
Factorial:=Fact[N];
end;




procedure TMathLib.SetDiffMode(N: Integer; h: TFloatType = 0.01);
begin
DiffH:=h; 
if N <> DiffL then
if (N >= 2) and (N <= 20) then
begin
 SetLength(D1,DiffL+1);
 SetLength(D2,DiffL+1);
 SetLength(D3,DiffL+1);
 SetLength(D4,DiffL+1);
 SetLength(D5,DiffL+1);
 CalcDiffCoeff;
end;
end;



function TMathLib.DiffN(N: Integer; X: Double): extended;
var
i: Integer;
F: extended;
DH: TArrayX;
begin
SetLength(DH,DiffL+1);
DH:=Copy(DD,0,DiffL+1);

for i:=1 to DiffL do
begin
if DiffL-N-i+1 < 0
then DH[i]:=0
else
DH[i]:=Factorial(DiffL-i+1)/Factorial(DiffL-N-i+1)*DH[i];
end;

F:=0;

for i:=1 to DiffL do
begin
F:=F+intpower(x,DiffL-i+1-N)*DH[i];
end;

DiffN:=F;
end;



procedure TMathLib.CalcDiffCoeff;
var
i: Integer;
x: TFloatType;
C: TFloatType;
begin
x:=DiffL/2;

for i:=0 to DiffL do
begin
SetDiffC(i);
C:=Cnk(DiffL,i)/Factorial(DiffL)*IntPower(-1,DiffL+i);
D1[i]:=DiffN(1,x)*C;
D2[i]:=DiffN(2,x)*C;
D3[i]:=DiffN(3,x)*C;
D4[i]:=DiffN(4,x)*C;
D5[i]:=DiffN(5,x)*C;
end;

end;



procedure TMathLib.SetDiffC(k: integer);
var
i,n: Integer;
begin
SetLength(DA,DiffL+1);
SetLength(DD,DiffL+1);
n:=0;
for i:=0 to DiffL do
begin
if i <> k then begin inc(n); DA[n]:=-i; end;
end;



if DiffL >= 1  then DD[1]:=1;
if DiffL >= 2  then DD[2]:=A1_N;
if DiffL >= 3  then DD[3]:=A2_N;
if DiffL >= 4  then DD[4]:=A3_N;
if DiffL >= 5  then DD[5]:=A4_N;
if DiffL >= 6  then DD[6]:=A5_N;
if DiffL >= 7  then DD[7]:=A6_N;
if DiffL >= 8  then DD[8]:=A7_N;
if DiffL >= 9  then DD[9]:=A8_N;
if DiffL >= 10 then DD[10]:=A9_N;
if DiffL >= 11 then DD[11]:=A10_N;
if DiffL >= 12 then DD[12]:=A11_N;
if DiffL >= 13 then DD[13]:=A12_N;
if DiffL >= 14 then DD[14]:=A13_N;
if DiffL >= 15 then DD[15]:=A14_N;
if DiffL >= 16 then DD[16]:=A15_N;
if DiffL >= 17 then DD[17]:=A16_N;
if DiffL >= 18 then DD[18]:=A17_N;
if DiffL >= 19 then DD[19]:=A18_N;
if DiffL >= 20 then DD[20]:=A19_N;
end;




function TMathLib.A2_N: extended;
var
i1,i2: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-1 do
for i2:=i1+1 to DiffL do
F:=F+DA[i1]*DA[i2];

A2_N:=F;
end;





function TMathLib.A3_N: extended;
var
i1,i2,i3: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-2 do
for i2:=i1+1 to DiffL-1 do
for i3:=i2+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3];

A3_N:=F;
end;



function TMathLib.A4_N: extended;
var
i1,i2,i3,i4: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-3 do
for i2:=i1+1 to DiffL-2 do
for i3:=i2+1 to DiffL-1 do
for i4:=i3+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4];

A4_N:=F;
end;



function TMathLib.A5_N: extended;
var
i1,i2,i3,i4,i5: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-4 do
for i2:=i1+1 to DiffL-3 do
for i3:=i2+1 to DiffL-2 do
for i4:=i3+1 to DiffL-1 do
for i5:=i4+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5];

A5_N:=F;
end;




function TMathLib.A6_N: extended;
var
i1,i2,i3,i4,i5,i6: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-5 do
for i2:=i1+1 to DiffL-4 do
for i3:=i2+1 to DiffL-3 do
for i4:=i3+1 to DiffL-2 do
for i5:=i4+1 to DiffL-1 do
for i6:=i5+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6];

A6_N:=F;
end;





function TMathLib.A7_N: extended;
var
i1,i2,i3,i4,i5,i6,i7: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-6 do
for i2:=i1+1 to DiffL-5 do
for i3:=i2+1 to DiffL-4 do
for i4:=i3+1 to DiffL-3 do
for i5:=i4+1 to DiffL-2 do
for i6:=i5+1 to DiffL-1 do
for i7:=i6+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7];

A7_N:=F;
end;





function TMathLib.A8_N: extended;
var
i1,i2,i3,i4,i5,i6,i7,i8: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-7 do
for i2:=i1+1 to DiffL-6 do
for i3:=i2+1 to DiffL-5 do
for i4:=i3+1 to DiffL-4 do
for i5:=i4+1 to DiffL-3 do
for i6:=i5+1 to DiffL-2 do
for i7:=i6+1 to DiffL-1 do
for i8:=i7+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7]*DA[i8];

A8_N:=F;
end;





function TMathLib.A9_N: extended;
var
i1,i2,i3,i4,i5,i6,i7,i8,i9: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-8 do
for i2:=i1+1 to DiffL-7 do
for i3:=i2+1 to DiffL-6 do
for i4:=i3+1 to DiffL-5 do
for i5:=i4+1 to DiffL-4 do
for i6:=i5+1 to DiffL-3 do
for i7:=i6+1 to DiffL-2 do
for i8:=i7+1 to DiffL-1 do
for i9:=i8+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7]*DA[i8]*DA[i9];

A9_N:=F;
end;



function TMathLib.A10_N: extended;
var
i1,i2,i3,i4,i5,i6,i7,i8,i9,i10: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-9 do
for i2:=i1+1 to DiffL-8 do
for i3:=i2+1 to DiffL-7 do
for i4:=i3+1 to DiffL-6 do
for i5:=i4+1 to DiffL-5 do
for i6:=i5+1 to DiffL-4 do
for i7:=i6+1 to DiffL-3 do
for i8:=i7+1 to DiffL-2 do
for i9:=i8+1 to DiffL-1 do
for i10:=i9+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7]*DA[i8]*DA[i9]*DA[i10];

A10_N:=F;
end;




function TMathLib.A11_N: extended;
var
i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-10 do
for i2:=i1+1 to DiffL-9 do
for i3:=i2+1 to DiffL-8 do
for i4:=i3+1 to DiffL-7 do
for i5:=i4+1 to DiffL-6 do
for i6:=i5+1 to DiffL-5 do
for i7:=i6+1 to DiffL-4 do
for i8:=i7+1 to DiffL-3 do
for i9:=i8+1 to DiffL-2 do
for i10:=i9+1 to DiffL-1 do
for i11:=i10+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7]*DA[i8]*DA[i9]*DA[i10]*DA[i11];

A11_N:=F;
end;





function TMathLib.A12_N: extended;
var
i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-11 do
for i2:=i1+1 to DiffL-10 do
for i3:=i2+1 to DiffL-9 do
for i4:=i3+1 to DiffL-8 do
for i5:=i4+1 to DiffL-7 do
for i6:=i5+1 to DiffL-6 do
for i7:=i6+1 to DiffL-5 do
for i8:=i7+1 to DiffL-4 do
for i9:=i8+1 to DiffL-3 do
for i10:=i9+1 to DiffL-2 do
for i11:=i10+1 to DiffL-1 do
for i12:=i11+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7]*DA[i8]*DA[i9]*DA[i10]*DA[i11]*DA[i12];

A12_N:=F;
end;




function TMathLib.A13_N: extended;
var
i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-12 do
for i2:=i1+1 to DiffL-11 do
for i3:=i2+1 to DiffL-10 do
for i4:=i3+1 to DiffL-9 do
for i5:=i4+1 to DiffL-8 do
for i6:=i5+1 to DiffL-7 do
for i7:=i6+1 to DiffL-6 do
for i8:=i7+1 to DiffL-5 do
for i9:=i8+1 to DiffL-4 do
for i10:=i9+1 to DiffL-3 do
for i11:=i10+1 to DiffL-2 do
for i12:=i11+1 to DiffL-1 do
for i13:=i12+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7]*DA[i8]*DA[i9]*DA[i10]*DA[i11]*DA[i12]*DA[i13];

A13_N:=F;
end;




function TMathLib.A14_N: extended;
var
i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-13 do
for i2:=i1+1 to DiffL-12 do
for i3:=i2+1 to DiffL-11 do
for i4:=i3+1 to DiffL-10 do
for i5:=i4+1 to DiffL-9 do
for i6:=i5+1 to DiffL-8 do
for i7:=i6+1 to DiffL-7 do
for i8:=i7+1 to DiffL-6 do
for i9:=i8+1 to DiffL-5 do
for i10:=i9+1 to DiffL-4 do
for i11:=i10+1 to DiffL-3 do
for i12:=i11+1 to DiffL-2 do
for i13:=i12+1 to DiffL-1 do
for i14:=i13+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7]*DA[i8]*DA[i9]*DA[i10]*DA[i11]*DA[i12]*DA[i13]*DA[i14];

A14_N:=F;
end;





function TMathLib.A15_N: extended;
var
i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-14 do
for i2:=i1+1 to DiffL-13 do
for i3:=i2+1 to DiffL-12 do
for i4:=i3+1 to DiffL-11 do
for i5:=i4+1 to DiffL-10 do
for i6:=i5+1 to DiffL-9 do
for i7:=i6+1 to DiffL-8 do
for i8:=i7+1 to DiffL-7 do
for i9:=i8+1 to DiffL-6 do
for i10:=i9+1 to DiffL-5 do
for i11:=i10+1 to DiffL-4 do
for i12:=i11+1 to DiffL-3 do
for i13:=i12+1 to DiffL-2 do
for i14:=i13+1 to DiffL-1 do
for i15:=i14+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7]*DA[i8]*DA[i9]*DA[i10]*DA[i11]*DA[i12]*DA[i13]*DA[i14]
*DA[i15];

A15_N:=F;
end;





function TMathLib.A16_N: extended;
var
i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-15 do
for i2:=i1+1 to DiffL-14 do
for i3:=i2+1 to DiffL-13 do
for i4:=i3+1 to DiffL-12 do
for i5:=i4+1 to DiffL-11 do
for i6:=i5+1 to DiffL-10 do
for i7:=i6+1 to DiffL-9 do
for i8:=i7+1 to DiffL-8 do
for i9:=i8+1 to DiffL-7 do
for i10:=i9+1 to DiffL-6 do
for i11:=i10+1 to DiffL-5 do
for i12:=i11+1 to DiffL-4 do
for i13:=i12+1 to DiffL-3 do
for i14:=i13+1 to DiffL-2 do
for i15:=i14+1 to DiffL-1 do
for i16:=i15+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7]*DA[i8]*DA[i9]*DA[i10]*DA[i11]*DA[i12]*DA[i13]*DA[i14]
*DA[i15]*DA[i16];

A16_N:=F;
end;





function TMathLib.A17_N: extended;
var
i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-16 do
for i2:=i1+1 to DiffL-15 do
for i3:=i2+1 to DiffL-14 do
for i4:=i3+1 to DiffL-13 do
for i5:=i4+1 to DiffL-12 do
for i6:=i5+1 to DiffL-11 do
for i7:=i6+1 to DiffL-10 do
for i8:=i7+1 to DiffL-9 do
for i9:=i8+1 to DiffL-8 do
for i10:=i9+1 to DiffL-7 do
for i11:=i10+1 to DiffL-6 do
for i12:=i11+1 to DiffL-5 do
for i13:=i12+1 to DiffL-4 do
for i14:=i13+1 to DiffL-3 do
for i15:=i14+1 to DiffL-2 do
for i16:=i15+1 to DiffL-1 do
for i17:=i16+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7]*DA[i8]*DA[i9]*DA[i10]*DA[i11]*DA[i12]*DA[i13]*DA[i14]
*DA[i15]*DA[i16]*DA[i17];

A17_N:=F;
end;




function TMathLib.A18_N: extended;
var
i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17,i18: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL-17 do
for i2:=i1+1 to DiffL-16 do
for i3:=i2+1 to DiffL-15 do
for i4:=i3+1 to DiffL-14 do
for i5:=i4+1 to DiffL-13 do
for i6:=i5+1 to DiffL-12 do
for i7:=i6+1 to DiffL-11 do
for i8:=i7+1 to DiffL-10 do
for i9:=i8+1 to DiffL-9 do
for i10:=i9+1 to DiffL-8 do
for i11:=i10+1 to DiffL-7 do
for i12:=i11+1 to DiffL-6 do
for i13:=i12+1 to DiffL-5 do
for i14:=i13+1 to DiffL-4 do
for i15:=i14+1 to DiffL-3 do
for i16:=i15+1 to DiffL-2 do
for i17:=i16+1 to DiffL-1 do
for i18:=i17+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7]*DA[i8]*DA[i9]*DA[i10]*DA[i11]*DA[i12]*DA[i13]*DA[i14]
*DA[i15]*DA[i16]*DA[i17]*DA[i18];

A18_N:=F;
end;





function TMathLib.A19_N: extended;
label endp;
var
i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17,i18,i19: Integer;
F: extended;
begin
F:=0;

for i1:=1 to DiffL-18 do
begin
//if i1 = 3 then goto endp;
for i2:=i1+1 to DiffL-17 do
for i3:=i2+1 to DiffL-16 do
for i4:=i3+1 to DiffL-15 do
for i5:=i4+1 to DiffL-14 do
for i6:=i5+1 to DiffL-13 do
for i7:=i6+1 to DiffL-12 do
for i8:=i7+1 to DiffL-11 do
for i9:=i8+1 to DiffL-10 do
for i10:=i9+1 to DiffL-9 do
for i11:=i10+1 to DiffL-8 do
for i12:=i11+1 to DiffL-7 do
for i13:=i12+1 to DiffL-6 do
for i14:=i13+1 to DiffL-5 do
for i15:=i14+1 to DiffL-4 do
for i16:=i15+1 to DiffL-3 do
for i17:=i16+1 to DiffL-2 do
for i18:=i17+1 to DiffL-1 do
for i19:=i18+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7]*DA[i8]*DA[i9]*DA[i10]*DA[i11]*DA[i12]*DA[i13]*DA[i14]
*DA[i15]*DA[i16]*DA[i17]*DA[i18]*DA[i19];
end;

endp:
A19_N:=F;
end;





function TMathLib.A20_N: extended;
var
i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17,i18,i19,i20: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL do
for i2:=i1+1 to DiffL do
for i3:=i2+1 to DiffL do
for i4:=i3+1 to DiffL do
for i5:=i4+1 to DiffL do
for i6:=i5+1 to DiffL do
for i7:=i6+1 to DiffL do
for i8:=i7+1 to DiffL do
for i9:=i8+1 to DiffL do
for i10:=i9+1 to DiffL do
for i11:=i10+1 to DiffL do
for i12:=i11+1 to DiffL do
for i13:=i12+1 to DiffL do
for i14:=i13+1 to DiffL do
for i15:=i14+1 to DiffL do
for i16:=i15+1 to DiffL do
for i17:=i16+1 to DiffL do
for i18:=i17+1 to DiffL do
for i19:=i18+1 to DiffL do
for i20:=i19+1 to DiffL do
F:=F+DA[i1]*DA[i2]*DA[i3]*DA[i4]*DA[i5]*DA[i6]*DA[i7]*DA[i8]*DA[i9]*DA[i10]*DA[i11]*DA[i12]*DA[i13]*DA[i14]
*DA[i15]*DA[i16]*DA[i17]*DA[i18]*DA[i19]*DA[i20];

A20_N:=F;
end;





function TMathLib.A1_N: extended;
var
i1: Integer;
F: extended;
begin
F:=0;
for i1:=1 to DiffL do
F:=F+DA[i1];

A1_N:=F;
end;




function TMathLib.Cnk(n,k: Integer): TFloatType;
begin
Cnk:=Factorial(n)/(Factorial(k)*Factorial(n-k));
end;





procedure TMathLib.NLSE_N(FL: TArrayA; PX: PArray1; X0: TArray1;  E: TFloatType; var Root: TArray1; var Er: Integer);
label endp;
var
i,j,k: Integer;
A: TArray2;
B,dXk: TArray1;
Xk,Xl: TArray1;
SE_N: Integer;
begin

if (High(FL) <> High(PX)) and  (High(FL) <> High(X0)) then
begin
//Error;
end;

Er:=0;
k:=0;
SE_N:=High(FL);

SetLength(A,SE_N+1,SE_N+1);
SetLength(B,SE_N+1);
Xk:=X0;
SE_SetVar(PX,Xk);

//1111111111111111
for i:=1 to SE_N do
 begin
  for j:=1 to SE_N do
  begin
   FV1:=PX[j];
   A[i,j]:=Diff1(FL[i],PX[j]^);
  end;
  B[i]:=-CalcFunc(FL[i]);


 end;

 LSE(A,B,dXk);


 Xl:=Copy(Xk,0,Length(Xk));

 for i:=1 to SE_N do
 begin
  Xk[i]:=Xk[i]+dXk[i];
 end;

 SE_SetVar(PX,Xk);
//111111111111111111

while  {Norm(FL) > E}  Dist(Xk,Xl) > E do
begin
 inc(k);
 for i:=1 to SE_N do
 begin
  for j:=1 to SE_N do
  begin
   FV1:=PX[j];
   A[i,j]:=Diff1(FL[i],PX[j]^);
  end;
  B[i]:=-CalcFunc(FL[i]);
 end;

 LSE(A,B,dXk);

 Xl:=Copy(Xk,0,Length(Xk));

 for i:=1 to SE_N do
 begin
  Xk[i]:=Xk[i]+dXk[i];
 end;

 SE_SetVar(PX,Xk);

 if k > 1000 then begin Er:=1; goto endp;  end;
end;

endp:
Root:=Xk;
Er:=k;
end;




function TMathLib.Norm(FL: TArrayA): TFloatType;
var
F,QF: TFloatType;
i: Integer;
begin
F:=0;
for i:=1 to High(FL) do
begin
 QF:=CalcFunc(FL[i]);
 F:=F+sqr(QF);
end;

Norm:=sqrt(F);
end;



function TMathLib.Dist(Xk,Xl: TArray1): TFloatType;
var
F,QF: TFloatType;
i: Integer;
begin
F:=0;
for i:=1 to High(Xk) do
begin
 F:=F+sqr(Xk[i]-Xl[i]);
end;

Dist:=sqrt(F);
end;



procedure TMathLib.SE_SetVar(PX: PArray1; X: TArray1);
var
i: Integer;
begin
for i:=1 to High(PX) do
begin
 PX[i]^:=X[i];
end;
end;



function TMathLib.NLSE(FL: TArrayA; PV: PArray1; X0: TArray1; var Error: Integer; E: TFloatType = 0; safe: Integer = 0; TF: Integer = 0; TP: Integer = 0): TArray1;
var
R: TArray1;
Xn: TArray1;
begin
if (High(FL) <> High(PV)) or (High(FL) <> High(X0))  then
begin
//Error;
end;
if E = 0 then E:=E_SystRoot;
if safe = 0 then safe:=N_SystRootSafe;
if TF = 0 then TF:=F_SystRootType;
if TP = 0 then TP:=F_SystRootPower;
Xn:=Copy(X0,0,Length(X0));

 if TF = 1 then R:=NLSE_R1(TP,FL,PV,Xn,E,safe,Error)
 else
 if TF = 2 then R:=NLSE_R2(TP,FL,PV,Xn,E,safe,Error)
 else
 if TF = 3 then R:=NLSE_M1(FL,PV,Xn,E,safe,Error)
 else
 if TF = 4 then R:=NLSE_M2(FL,PV,Xn,E,safe,Error)
 else
 if TF = 5 then R:=NLSE_G(FL,PV,Xn,E,safe,Error);

NLSE:=R;
end;




function TMathLib.RootR1(Func: TAddress; x: TFloatType; PV: PFloatType; var Error: Integer): TFloatType;
label 1,2,endp;
var
i: Integer;
safe,si,P: Cardinal;
D,D1,D1I,D2,DF,U,T,H,PH,E: TFloatType;
begin
//Rec1
Error:=0;
H:=PV^;
PV^:=X;
safe:=N_RootSafe;
E:=E_Root;
P:=F_RootPower;


   1:
 try
  D:=CalcFunc(Func);
 except
  Error:=1;
 end;
  if Error = 1 then goto endp;
  if (abs(D) < E) then goto endp;

 try
   D1:=Derivative1(Func,PV,PV^);
 except
  Error:=1;
 end;
 if Error = 1 then goto endp;

   if (D1 = 0) and (abs(D) < E) then goto endp;  //это корень

   if (D1 = 0)  and (abs(D) >= E) then
   begin
    X:=X+1; goto 1;
    {локальный минимум, но не корень: взять другое значение X}
   end;

 try
   D1I:=1/D1;
   PV^:=PV^-D*D1I;
   for i:=3 to P do
   begin
    D:=CalcFunc(Func);
    PV^:=PV^-D*D1I;
   end;
  except
   Error:=1;
  end;

if Error = 1 then goto endp;

si:=0;
while abs(D) > E  do
begin
 2:
 try
   D:=CalcFunc(Func);
 except
  Error:=1;
 end;
 if Error = 1 then goto endp;
 if abs(D) < E then goto endp;

 try
   D1:=Derivative1(Func,PV,PV^);
 except
  Error:=1;
 end;


 if Error = 1 then goto endp;
  
   if (D1 = 0) and (abs(D) < E) then goto endp;  //это корень

   if (D1 = 0)  and (abs(D) >= E) then
   begin
    X:=X+1; goto 2;
    {локальный минимум, но не корень: взять другое значение X}
   end;

 try
   D:=CalcFunc(Func);
 except
  Error:=1;
 end;
 if Error = 1 then goto endp;
 if abs(D) < E then goto endp;


 D1I:=1/D1;
 PV^:=PV^-D*D1I;
   for i:=3 to P do
   begin
    try
     D:=CalcFunc(Func);
    except
     Error:=1;
    end;
     if Error = 1 then goto endp;
     if abs(D) < E then goto endp;
     PV^:=PV^-D*D1I;
   end;



 inc(si);
 if si > safe then begin Error:=1; goto endp; end;
end;


endp:
RootR1:=PV^;
PV^:=H;
end;






function TMathLib.RootR2(P: Integer; FL: TAddress; PV: PFloatType; x: TFloatType;   E: TFloatType;  safe: Integer; var Error: Integer): TFloatType;
label endp;
var
i: Integer;
D,D1,D2,DF,U,DC,T,H,PH: TFloatType;
begin
//Rec2


Error:=0;
T:=0;
H:=PV^;
PV^:=X; T:=X;
 try
   D1:=Derivative1(FL,PV,PV^);
   D2:=Derivative2(FL,PV,PV^);
   D:=CalcFunc(FL);
   U:=D/D1;
   DC:=1/(D1-D2*U);
   PV^:=PV^-D*DC;
   for i:=3 to P do
   begin
    D:=CalcFunc(FL);
    PV^:=PV^-D*DC;
   end;
 except
 Error:=1;
 end;
if Error = 1 then goto endp;

i:=0;
while abs(T-PV^) > E {abs(D) > E}  do
begin
inc(i);
 T:=PV^;
 try
   D1:=Derivative1(FL,PV,PV^);
   D2:=Derivative2(FL,PV,PV^);
   D:=CalcFunc(FL);
   U:=D/D1;
   DC:=1/(D1-D2*U);
   PV^:=PV^-D*DC;
   for i:=3 to P do
   begin
    D:=CalcFunc(FL);
    PV^:=PV^-D*DC;
   end;
 except
 Error:=1;
 end;
 if Error = 1 then goto endp;

 if i > safe then begin Error:=1; goto endp; end;
end;


endp:
RootR2:=PV^;
PV^:=H;
end;








function TMathLib.RootM1(Func: TAddress; x: TFloatType; PV: PFloatType; var Error: Integer): TFloatType;
label 1,2,endp;
var
i,FZ: Integer;
safe,si: Cardinal;
D,D1,D1I,D2,DF,U,T,H,PH,G,E,B: TFloatType;
begin

Error:=0;
H:=PV^;
PV^:=X;
safe:=N_RootSafe;
E:=E_Root;
FZ:=0;
T:=0;
H:=PV^;

1:
   try
    G:=Derivative1(Func,PV,PV^);
    D:=CalcFunc(Func);
   except
    Error:=1;
   end;
   if Error = 1 then goto endp;

   if abs(D) < E then goto endp;
   if (G = 0) and (abs(D) < E) then goto endp;
   if (G = 0) and (abs(D) >= E) then
   begin
    PV^:=PV^/2; goto 1;
   end;

   try
    B:=-1/G;
    T:=PV^;
    PV^:=PV^+B*D;
    G:=(CalcFunc(Func)-D)/(D*B);
   except
    Error:=1;
   end;
   PV^:=T;
   if (G = 0) and (abs(D) < E) then goto endp;
   if (G = 0) and (abs(D) >= E) then
   begin
    X:=X/2; goto 1;
   end;
   PV^:=PV^-D/G;

   if Error = 1 then goto endp;


si:=0;
while abs(D) > E  do
begin
2:
   try
    G:=Derivative1(Func,PV,PV^);
    D:=CalcFunc(Func);
   except
    Error:=1;
   end;
   if Error = 1 then goto endp;
    
   if abs(D) < E then goto endp;
   if (G = 0) and (abs(D) < E) then goto endp;
   if (G = 0) and (abs(D) >= E) then
   begin
    PV^:=PV^/2; goto 2;
   end;

   try
    B:=-1/G;
    T:=PV^;
    PV^:=PV^+B*D;
    G:=(CalcFunc(Func)-D)/(D*B);
   except
    Error:=1;
   end;
   PV^:=T;
   if (G = 0) and (abs(D) < E) then goto endp;
   if (G = 0) and (abs(D) >= E) then
   begin
    X:=X/2; goto 2;
   end;
   PV^:=PV^-D/G;

   if Error = 1 then goto endp;


 inc(si);
 if si > safe then begin Error:=1; goto endp; end;
end;


endp:
RootM1:=PV^;
PV^:=H;
end;






function TMathLib.RootM2(FL: TAddress; PV: PFloatType; x: TFloatType; E: TFloatType; safe: Integer; var Error: Integer): TFloatType;
label 1,endp;
var
i,FZ: Integer;
D,D1,D1I,D2,DF,U,T,H,PH,L,Z: TFloatType;
begin
Error:=0;
FZ:=0;
T:=0;
H:=PV^;
PV^:=X; T:=X;
try
{D:=Foreval.CalcFunc(FL);
L:=D/X;}
//L:=1;
L:=Derivative1(FL,PV,PV^);

   D:=CalcFunc(FL);
   T:=PV^;
   Z:=PV^-D/L;
   PV^:=Z;
   DF:=CalcFunc(FL);
   L:=(DF-D)/(Z-T);
   PV^:=T-D/L;
 except
 Error:=1;
 end;
if Error = 1 then goto endp;


i:=0;
while abs(T-PV^) > E {abs(D) > E}  do
begin
 T:=PV^;
 try
   D:=CalcFunc(FL);
   T:=PV^;
   Z:=PV^-D/L;
   PV^:=Z;
   DF:=CalcFunc(FL);
   L:=(DF-D)/(Z-T);
   PV^:=T-D/L;
 except
 Error:=1;
 end;
 if Error = 1 then goto endp;
 inc(i);
 if i > safe then begin Error:=1; goto endp; end;
end;



endp:
RootM2:=PV^;
PV^:=H;
end;






function TMathLib.NLSE_R1(P: Integer; FL: TArrayA; PX: PArray1; X0: TArray1;  E: TFloatType; safe: Integer; var Error: Integer): TArray1;
label endp;
var
i,j,k,N: Integer;
A: TArray2;
B,dXk: TArray1;
Xk,Xl,HX: TArray1;
SE_N: Integer;
U,D,D1I,D2,DF1,L,DF2,DC,L1,L2,L3,L4,L5,L6,L7: TFloatType;
begin
//P - степень алгоритма
if (High(FL) <> High(PX)) or  (High(FL) <> High(X0)) then
begin
//Error;
end;
Error:=0;
SE_N:=High(FL);

SetLength(HX,SE_N+1);
//сохранение внешних переменных:
for i:=1 to SE_N do
begin
HX[i]:=PX[i]^;
end;


SetLength(A,SE_N+1,SE_N+1);
SetLength(B,SE_N+1);
Xk:=X0;
SE_SetVar(PX,Xk);

//1111111111111111
  Xl:=Copy(Xk,0,Length(Xk));
  for j:=1 to SE_N do
  begin
    FV1:=PX[j];
   try
    D1I:=1/Derivative1(FL[j],FV1,FV1^);
    D:=CalcFunc(FL[j]);
    FV1^:=FV1^-D*D1I;
    for i:=3 to P do
    begin
     D:=CalcFunc(FL[j]);
     FV1^:=FV1^-D*D1I;
    end;
   except
    Error:=1;
   end;
    if Error = 1 then goto endp;
    Xk[j]:=FV1^;
  end;
  SE_SetVar(PX,Xk);
//111111111111111111
k:=0;
while  {Norm(FL) > E}  Dist(Xk,Xl) > E do
begin
 inc(k);
  Xl:=Copy(Xk,0,Length(Xk));
  for j:=1 to SE_N do
  begin
    FV1:=PX[j];
   try
    D1I:=1/Derivative1(FL[j],FV1,FV1^);
    D:=CalcFunc(FL[j]);
    FV1^:=FV1^-D*D1I;
    for i:=3 to P do
    begin
     D:=CalcFunc(FL[j]);
     FV1^:=FV1^-D*D1I;
    end;
   except
    Error:=1;
   end;
    if Error = 1 then goto endp;
    Xk[j]:=FV1^;
  end;
  SE_SetVar(PX,Xk);
 if k > safe then
 begin
  Error:=1;  goto endp;
 end;
end;

endp:
//сохранение внешних переменных:
for i:=1 to SE_N do
begin
PX[i]^:=HX[i];
end;

NLSE_R1:=Xk;
end;






function TMathLib.NLSE_R2(P: Integer; FL: TArrayA; PX: PArray1; X0: TArray1;  E: TFloatType; safe: Integer; var Error: Integer): TArray1;
label endp;
var
i,j,k,N: Integer;
A: TArray2;
B,dXk,HX: TArray1;
Xk,Xl: TArray1;
SE_N: Integer;
U,D,D1,D2,DF,DF1,L,DF2,DC,L1,L2,L3,L4,L5,L6,L7: TFloatType;
begin
//(2*(P-1)степень алгоритма
if (High(FL) <> High(PX)) or  (High(FL) <> High(X0)) then
begin
//Error;
end;
Error:=0;
SE_N:=High(FL);
SetLength(HX,SE_N+1);
//сохранение внешних переменных:
for i:=1 to SE_N do
begin
HX[i]:=PX[i]^;
end;
SetLength(A,SE_N+1,SE_N+1);
SetLength(B,SE_N+1);
Xk:=X0;
SE_SetVar(PX,Xk);

//1111111111111111
  Xl:=Copy(Xk,0,Length(Xk));
  for j:=1 to SE_N do
  begin
    FV1:=PX[j];
   try
    D1:=Derivative1(FL[j],FV1,FV1^);
    D2:=Derivative2(FL[j],FV1,FV1^);
    D:=CalcFunc(FL[j]);
    DF:=1/(D1-D2*D/D1);
    FV1^:=FV1^-D*DF;
    for i:=3 to P do
    begin
     D:=CalcFunc(FL[j]);
     FV1^:=FV1^-D*DF;
    end;
   except
    Error:=1;
   end;
   Xk[j]:=FV1^;
  end;
  SE_SetVar(PX,Xk);
//111111111111111111
k:=0;
while  {Norm(FL) > E}  Dist(Xk,Xl) > E do
begin
 inc(k);
  Xl:=Copy(Xk,0,Length(Xk));
  for j:=1 to SE_N do
  begin
    FV1:=PX[j];
   try
    D1:=Derivative1(FL[j],FV1,FV1^);
    D2:=Derivative2(FL[j],FV1,FV1^);
    D:=CalcFunc(FL[j]);
    DF:=1/(D1-D2*D/D1);
    FV1^:=FV1^-D*DF;
    for i:=3 to P do
    begin
     D:=CalcFunc(FL[j]);
     FV1^:=FV1^-D*DF;
    end;
   except
    Error:=1;
   end;
   Xk[j]:=FV1^;
  end;
  SE_SetVar(PX,Xk);
 if k > safe then
 begin
  Error:=1; goto endp;
 end;
end;

endp:
//сохранение внешних переменных:
for i:=1 to SE_N do
begin
PX[i]^:=HX[i];
end;
NLSE_R2:=Xk;
end;






function TMathLib.NLSE_M1(FL: TArrayA; PX: PArray1; X0: TArray1;  E: TFloatType; safe: Integer; var Error: Integer): TArray1;
label 1, endp;
var
i,j,k,N,FZ: Integer;
A: TArray2;
B,dXk,HX: TArray1;
Xk,Xl,G: TArray1;
SE_N: Integer;
U,D,D1I,D2,DF1,L,DF,DC,T: TFloatType;
begin
//P - степень алгоритма
if (High(FL) <> High(PX)) or  (High(FL) <> High(X0)) then
begin
//Error;
end;
SetLength(HX,SE_N+1);
//сохранение внешних переменных:
for i:=1 to SE_N do
begin
HX[i]:=PX[i]^;
end;
Error:=0;
FZ:=0;
SE_N:=High(FL);

SetLength(A,SE_N+1,SE_N+1);
SetLength(B,SE_N+1);
Xk:=X0;
SE_SetVar(PX,Xk);
SetLength(G,SE_N+1);

for i:=1 to SE_N do
begin
 FV1:=PX[i];
 G[i]:=Derivative1(FL[i],FV1,FV1^);
end;

//1111111111111111
  Xl:=Copy(Xk,0,Length(Xk));
  for j:=1 to SE_N do
  begin
    FV1:=PX[j];
   try
    //G:=Foreval.Derivative1(FL[j],FV1,FV1^);
    D:=CalcFunc(FL[j]);
    T:=FV1^;
    FV1^:=FV1^-D/G[j];
    DF:=CalcFunc(FL[j]);
    G[j]:=-(DF-D)*G[j]/D;
    FV1^:=T;
    if G[j] = 0 then FZ:=1;
    FV1^:=FV1^-D/G[j];
   except
    Error:=1;
   end;
    if FZ = 1 then goto 1;
    if Error = 1 then goto endp;
    Xk[j]:=FV1^;
  end;
  SE_SetVar(PX,Xk);
//111111111111111111
k:=0;
while  {Norm(FL) > E}  Dist(Xk,Xl) > E do
begin
 inc(k);
  Xl:=Copy(Xk,0,Length(Xk));
  for j:=1 to SE_N do
  begin
    FV1:=PX[j];
   try
    D:=CalcFunc(FL[j]);
    T:=FV1^;
    FV1^:=FV1^-D/G[j];
    DF:=CalcFunc(FL[j]);
    G[j]:=-(DF-D)*G[j]/D;
    FV1^:=T;
    if G[j] = 0 then FZ:=1;
    FV1^:=FV1^-D/G[j];
   except
    Error:=1;
   end;

    if FZ = 1 then goto 1;
    if Error = 1 then goto endp;
    Xk[j]:=FV1^;
  end;
  SE_SetVar(PX,Xk);

 if k > safe then
 begin
  Error:=1;  goto endp;
 end;
end;

1:
endp:
//сохранение внешних переменных:
for i:=1 to SE_N do
begin
PX[i]^:=HX[i];
end;
NLSE_M1:=Xk;
end;




function TMathLib.NLSE_M2(FL: TArrayA; PX: PArray1; X0: TArray1;  E: TFloatType; safe: Integer; var Error: Integer): TArray1;
label 1, endp;
var
i,j,k,N,FZ: Integer;
A: TArray2;
B,dXk,HX: TArray1;
Xk,Xl,L: TArray1;
SE_N: Integer;
U,D,D1I,D2,DF1,DF,DC,T,Z: TFloatType;
begin
if (High(FL) <> High(PX)) or  (High(FL) <> High(X0)) then
begin
//Error;
end;
SetLength(HX,SE_N+1);
//сохранение внешних переменных:
for i:=1 to SE_N do
begin
HX[i]:=PX[i]^;
end;
Error:=0;
FZ:=0;
SE_N:=High(FL);

SetLength(A,SE_N+1,SE_N+1);
SetLength(B,SE_N+1);
Xk:=X0;
SE_SetVar(PX,Xk);
SetLength(L,SE_N+1);

for i:=1 to SE_N do
begin
 FV1:=PX[i];
 L[i]:=Derivative1(FL[i],FV1,FV1^);
end;

//1111111111111111
  Xl:=Copy(Xk,0,Length(Xk));
  for j:=1 to SE_N do
  begin
    FV1:=PX[j];
   try
    D:=CalcFunc(FL[j]);
    T:=FV1^;
    Z:=FV1^-D/L[j];
    FV1^:=Z;
    DF:=CalcFunc(FL[j]);
    L[j]:=(DF-D)/(Z-T);
    FV1^:=T-D/L[j];
   except
    Error:=1;
   end;
    if Error = 1 then goto endp;
    Xk[j]:=FV1^;
  end;
  SE_SetVar(PX,Xk);
//111111111111111111
k:=0;
while  {Norm(FL) > E}  Dist(Xk,Xl) > E do
begin
 inc(k);
  Xl:=Copy(Xk,0,Length(Xk));
  for j:=1 to SE_N do
  begin
    FV1:=PX[j];
   try
    D:=CalcFunc(FL[j]);
    T:=FV1^;
    Z:=FV1^-D/L[j];
    FV1^:=Z;
    DF:=CalcFunc(FL[j]);
    L[j]:=(DF-D)/(Z-T);
    FV1^:=T-D/L[j];
   except
    Error:=1;
   end;

    if Error = 1 then goto endp;
    Xk[j]:=FV1^;
  end;
  SE_SetVar(PX,Xk);

 if k > safe then
 begin
  Error:=1;  goto endp;
 end;
end;

1:
endp:
//сохранение внешних переменных:
for i:=1 to SE_N do
begin
PX[i]^:=HX[i];
end;
NLSE_M2:=Xk;
end;








function TMathLib.ScanRoot(FL: TAddress; PV: PFloatType; X0,X1,h: TFloatType; T: Integer; var Error: Integer; E: TFloatType = 0; safe: Integer = 0; TF: Integer = 0; TP: Integer = 0): TArray1;
label 1, endp;
var
XR: TArray1;
i,k,Er,N,j,Fst: Integer;
Xn,R: TFloatType;
begin
if E = 0 then E:=E_Root;
if safe = 0 then safe:=N_RootSafe;
if TF = 0 then TF:=F_RootType;
if TP = 0 then TP:=F_RootPower;
SetLength(XR,1);
Error:=0;
Fst:=0;
if X0 < X1 then Xn:=X0 else Xn:=X1;
N:=TRunc(abs(X1-X0)/h);
h:=abs(X1-X0)/N;

for i:=1 to N do
begin
 {if TF = 1 then R:=RootR1(TP,FL,PV,Xn,E,safe,Er)
 else
 if TF = 2 then R:=RootR2(TP,FL,PV,Xn,E,safe,Er)
 else
 if TF = 3 then R:=RootM1(FL,PV,Xn,E,safe,Er)
 else
 if TF = 4 then R:=RootM2(FL,PV,Xn,E,safe,Er);
 }
 //если найден корень -R (при нач. прибл. Xn) без ошибки (Er = 0)
 if Er  = 0 then
 begin
  //если  R нах-ся в пределах отрезка [X0,X1]
  if (R < X0) or (R > X1) then goto 1;
  //только один корень
  if T = 0 then begin  SetLength(XR,Length(XR)+1); XR[1]:=R; goto endp;  end;
  //если это первый корень
  if Fst = 0 then
  begin
   Fst:=1;
   SetLength(XR,2);
   XR[1]:=R;
   goto 1;
  end
  else
  begin
    //остальные корни:
    //проверка: если R содержится в массиве найденных корней XR, с точностью ??
    for j:=1 to High(XR) do
    begin
     if abs(R-XR[j]) < 1E-10 then goto 1   //устанавливается условно
    end;
    //запись R -> XR;
    SetLength(XR,Length(XR)+1);
    XR[High(XR)]:=R;
    goto 1;
   end;
 end;

1:
 Xn:=Xn+h;
end;

if High(XR) = 0 then Error:=1;

endp:
ScanRoot:=XR;
end;







function TMathLib.ScanSysRoot(FL: TArrayA; PV: PArray1; X0,X1: TArray1; h: TFloatType; T: Integer; var Error: Integer; E: TFloatType = 0; safe: Integer = 0; TF: Integer = 0; TP: Integer = 0): TArray2;
label 1, 2, 3, endp;
var
XR: TArray2;
i,k,Er,N,j,Fst,Len,CS,i1,EL,N1,N2,Str,Col: Integer;
Xn,R,Hn,Xn0,XnC,ZIP: TArray1;
H1,H2: TFloatType;
PRL: Boolean;
SS: String;
begin
if (High(FL) <> High(PV)) or (High(FL) <> High(X0)) or (High(FL) <> High(X1)) then
begin
//Error;
end;
h:=abs(h);
PRL:=True;
if E = 0 then E:=E_SystRoot;
if safe = 0 then safe:=N_SystRootSafe;
if TF = 0 then TF:=F_SystRootType;
if TP = 0 then TP:=F_SystRootPower;
SetLength(XR,1,1);
Error:=0;
Fst:=0;
Len:=High(X0);
Col:=Len+1;
Str:=1;
SetLength(Hn,Len+1);
SetLength(Xn,Len+1);
SetLength(XnC,Len+1);
SetLength(Xn0,Len+1); //всегда начальные значения
SetLength(ZIP,Len+1);
for i:=1 to Len do
begin
 ZIP[i]:=0;
end;
//настройка нач. условий:
//Xn[i] - массив текущих коорд.
for i:=1 to Len do
begin
 if X0[i] < X1[i] then
 begin
  Xn[i]:=X0[i];
 end
 else
 //нач. и кон. коорд. - наоборот => перестановка
 if X0[i] > X1[i] then
 begin
  Xn[i]:=X1[i];
  X1[i]:=X0[i];
  X0[i]:=Xn[i];
 end
 else
 begin
  //Error;
 end;

end;



2:


 XnC:=Copy(Xn,0,Length(Xn));
 if TF = 1 then R:=NLSE_R1(TP,FL,PV,XnC,E,safe,Er)
 else
 if TF = 2 then R:=NLSE_R2(TP,FL,PV,XnC,E,safe,Er)
 else
 if TF = 3 then R:=NLSE_M1(FL,PV,XnC,E,safe,Er)
 else
 if TF = 4 then R:=NLSE_M2(FL,PV,XnC,E,safe,Er)
 else
 if TF = 5 then R:=NLSE_G(FL,PV,XnC,E,safe,Er);



 //если найден вектор -R (при нач. прибл. Xn) без ошибки (Er = 0)
 if Er  = 0 then
 begin
  //если  R нах-ся в пределах n-куба [X0,X1]
  for j:=1 to Len do
  begin
    if (R[j] < X0[j]) or (R[j] > X1[j]) then goto 1;
  end;

  //только один корень
  if T = 0 then
  begin
   SetLength(XR,Length(XR)+1,Len);
   for j:=1 to Len do
   begin
    XR[1,j]:=R[j];
   end;
   goto endp;
  end;
  //если это первый корень
  if Fst = 0 then
  begin
   Fst:=1;
   inc(Str);
   SetLength(XR,Str,Col);
   for j:=1 to Len do
   begin
    XR[1,j]:=R[j];
   end;
   goto 1;
  end
  else
  begin
    //остальные корни:
    //проверка: если R содержится в массиве найденных корней XR, с точностью ???
    for j:=1 to Str-1 do
    begin
     CS:=0;
     for k:=1 to Len do
     begin
      if abs(R[k]-XR[j,k]) < 1E-10 then inc(CS);  //устнавливается в ручную
     end;
     if CS = Len then goto 1;
    end;

    //запись R -> XR;
    inc(Str);
    SetLength(XR,Str,Col);
    for j:=1 to Len do
    begin
     XR[Str-1,j]:=R[j];
    end;

    goto 1;
   end;
 end;


1:
//пересчет коорд.
if PRL = True then
begin
 for k:=1 to Len do
 begin
  if k = 1 then
  begin
   Xn[1]:=Xn[1]+h;
   if Xn[1] > X1[1] then begin ZIP[2]:=1; Xn[1]:=X0[1]; end;
  end
  else
  if (k < Len) and (ZIP[k] = 1) then
  begin
   Xn[k]:=Xn[k]+h; ZIP[k]:=0;
   if Xn[k] > X1[k] then begin Xn[k]:=X0[k]; ZIP[k+1]:=1; end;
  end
  else
  if (K = Len) and (ZIP[k] = 1) then
  begin
    ZIP[Len]:=0; Xn[Len]:=Xn[Len]+h;
    if Xn[Len] > X1[Len]
    then PRL:=False; //последний проход
  end;
 end;

goto 2;
end;


if High(XR) = 0 then Error:=1;

endp:
ScanSysRoot:=XR;
end;







procedure TMathLib.SetRootMode(E: TFloatType; Safe: Integer; Mode: Integer; Power: Integer);
begin
E_Root:=E;
N_RootSafe:=Safe;
if (Mode >= 1) and (Mode <= 4) then
F_RootType:=Mode;
if Power >=2 then
F_RootPower:=Power;
end;



procedure TMathLib.SetSystRootMode(E: TFloatType; Safe: Integer; Mode: Integer; Power: Integer);
begin
E_SystRoot:=E;
N_SystRootSafe:=Safe;
if (Mode >= 1) and (Mode <= 5) then
F_SystRootType:=Mode;
if Power >=2 then
F_SystRootPower:=Power;
end;


procedure TMathLib.InitRoot;
begin
E_Root:=1E-5; //1E-5;
F_RootType:=3;
F_RootPower:=2;
N_RootSafe:=1000;
E_Round:=5;
F_Approximation:=0;
end;


procedure TMathLib.InitSystRoot;
begin
E_SystRoot:=1E-15;
F_SystRootType:=5;
F_SystRootPower:=2;
end;


function TMathLib.SaveSetModes: TSetMode;
var
SM: TSetMode;
begin
SM.DiffL:=DiffL;
SM.DiffH:=DiffH;
SM.IntegL:=IntegL;
SM.IntegH:=IntegH;
SM.FZero:=FZero;
SM.E_Root:=E_Root;
SM.N_RootSafe:=N_RootSafe;
SM.F_RootType:=F_RootType;
SM.F_RootPower:=F_RootPower;
SM.E_SystRoot:=E_SystRoot;
SM.N_SystRootSafe:=N_SystRootSafe;
SM.F_SystRootType:=F_SystRootType;
SM.F_SystRootPower:=F_SystRootPower;

SaveSetModes:=SM;
end;


procedure TMathLib.PresetRecall;
begin
{
DiffL:=InitMode.DiffL;
DiffH:=InitMode.DiffH;
IntegL:=InitMode.IntegL;
IntegH:=InitMode.IntegH;
FZero:=InitMode.FZero;
E_Root:=InitMode.E_Root;
N_RootSafe:=InitMode.N_RootSafe;
F_RootType:=InitMode.F_RootType;
F_RootPower:=InitMode.F_RootPower;
SystRootE:=InitMode.SystRootE;
N_SystRootSafe:=InitMode.N_SystRootSafe;
F_SystRootType:=InitMode.F_SystRootType;
F_SystRootPower:=InitMode.F_SystRootPower;
}
end;


procedure TMathLib.SaveInitMode;
begin
{
InitMode.DiffL:=DiffL;
InitMode.DiffH:=DiffH;
InitMode.IntegL:=IntegL;
InitMode.IntegH:=IntegH;
InitMode.FZero:=FZero;
InitMode.E_Root:=E_Root;
InitMode.N_RootSafe:=N_RootSafe;
InitMode.F_RootType:=F_RootType;
InitMode.F_RootPower:=F_RootPower;
InitMode.SystRoot:=E_SystRoot;
InitMode.N_SystRootSafe:=N_SystRootSafe;
InitMode.F_SystRootType:=F_SystRootType;
InitMode.F_SystRootPower:=F_SystRootPower;
}
end;


procedure TMathLib.SetMode(Mode: Integer;v: TFloatType);
begin
 if Mode = ml_Integral_Currency then
 begin
  if abs(v) <> 0 then H_Integral:=abs(v);
 end
 else
 if Mode = ml_Derivative_Currency then
 begin
  if abs(v) <> 0 then DiffH:=abs(v);
 end;
end;


function TMathLib.RoundX(x: TFloatType): TFloatType;
label 1,endp;
var
D,D1,ZI: TFloatType;
RB: TFloatType;
i: Integer;
begin
ZI:=Trunc(x);
RB:=x;
D:=abs(x-ZI);
D1:=D;
for i:=1 to E_Round do
begin
 D1:=D1*10;
 if Trunc(D1) <> 9 then  goto 1;
 D1:=D1-Trunc(D1);
end;
if RB < 0 then RB:=ZI-1 else RB:=ZI+1;
goto endp;
1:
D1:=D;
for i:=1 to E_Round do
begin
 D1:=D1*10;
 if Trunc(D1) <> 0 then  goto endp;
 D1:=D1-Trunc(D1);
end;
RB:=ZI;

endp:
RoundX:=RB;
end;





end.



//Проверить AaBmC1&AaBmC&AmBaC

procedure TForm1.AaBmC(A,B,C,D: TArray2);
var
i,j,k,N,L,M,adrA,adrB,adrC,adrD: Cardinal;
S: double;
begin              //D[n,m]=(A[n,l]+B[n,l])*C[l,m]
N:=High(A); L:=High(A[0]); M:=High(C[0]);

{
for i:=0 to N do
begin
 for j:=0 to M do
 begin
  S:=0;
  for k:=0 to L do
  begin
   S:=S+(A[i,k]+B[i,k])*C[k,j];
  end;
  D[i,j]:=S;
 end;
end;
}

adrA:=TAddress(A);
adrB:=TAddress(B);
adrC:=TAddress(C);
adrD:=TAddress(D);
inc(N);
inc(L);
inc(M);

asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    xor   esi,esi              //esi->I
    push  adrC
@@I:
    mov   eax,adrA
    mov   ebx,adrB
    mov   eax,[eax+4*esi]      //adr i-ой строки A -> eax
    mov   ebx,[ebx+4*esi]      //adr i-ой строки B -> ebx
    xor   edi,edi              //edi->J
@@J:
    fldz
    xor   ecx,ecx
@@K:                           //ecx->K
    mov   edx,adrC
    mov   edx,[edx+4*ecx]
    fld   qword ptr [eax+8*ecx]
    fadd  qword ptr [ebx+8*ecx]
    fmul  qword ptr [edx+8*edi]
    fadd
    inc   ecx
    cmp   ecx,L
    jne   @@K

    mov   edx,adrD
    mov   edx,[edx+4*esi]
    fstp  qword ptr [edx+8*edi]
    inc   edi
    cmp   edi,M
    jne   @@J

    inc   esi
    cmp   esi,N
    jne   @@I

    pop  adrC
    pop  edi
    pop  esi
    pop  edx
    pop  ecx
    pop  ebx
    pop  eax
end;

procedure TForm1.AaBmC1(A,B,C,D: TArray2);
var
i,j,k,N,adrA,adrB,adrC,adrD: Cardinal;
S: double;
begin
N:=High(A);
{
for i:=0 to N do
begin
 for k:=0 to N do
 begin
  S:=(A[i,k]+B[i,k]);
  for j:=0 to N do
  begin
   D[i,j]:=D[i,j]+S*C[k,j];
  end;
 end;
end;
}
adrA:=TAddress(A);
adrB:=TAddress(B);
adrC:=TAddress(C);
adrD:=TAddress(D);
N:=N+1;
//K:=K+1;
//M:=M+1;

asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    xor   esi,esi              {esi->I}
    push  adrC
@@I:
    mov   eax,adrA
    mov   ebx,adrB
    mov   eax,[eax+4*esi]      {adr i-ой строки A -> eax}
    mov   ebx,[ebx+4*esi]      {adr i-ой строки B -> ebx}
    xor   edi,edi              {edi->K}
@@K:
    ffree ST(0)
    fld   qword ptr [eax+8*edi]
    fadd  qword ptr [ebx+8*edi]
    xor   ecx,ecx
@@J:                           {ecx->J}
    mov   edx,adrC
    mov   edx,[edx+4*edi]
    fmul  qword ptr [edx+8*ecx]
    mov   edx,adrD
    mov   edx,[edx+4*esi]
    fadd  qword ptr [edx+8*ecx]
    fst   qword ptr [edx+8*ecx]

    inc   ecx
    cmp   ecx,N
    jne   @@J

    inc   edi
    cmp   edi,N
    jne   @@K

    inc   esi
    cmp   esi,N
    jne   @@I

    pop  adrC
    pop  edi
    pop  esi
    pop  edx
    pop  ecx
    pop  ebx
    pop  eax
end;


end;






procedure TForm1.AmBaC(A,B,C,D: TArray2);
var
adrA,adrB,adrC,adrD,N,K,M,i,j,l: TAddress;
S: double;
begin                                  //D[n,m]:=A[n,k]*B[k,m]+C[n,m]
N:=High(A); K:=High(A[0]); M:=High(B[0]);

{
 for i:=0 to N do
 begin
  for j:=0 to K do
  begin
   S:=0;
   for l:=0 to M do
   begin
    S:=S+A[i,l]*B[l,j];
   end;
   D[i,j]:=S+C[i,j];
  end;
 end;
}


adrA:=TAddress(A);
adrB:=TAddress(B);
adrC:=TAddress(C);
adrD:=TAddress(D);
N:=N+1;
K:=K+1;
M:=M+1;

//DOUBLE:
asm
    mov   ebx,adrB
    xor   esi,esi
@@I:
    mov   eax,adrA
    mov   eax,[eax+4*esi]
    xor   edi,edi
@@J:
    fldz
    xor   ecx,ecx
@@L:
    mov   edx,[ebx+4*ecx]
    fld   qword ptr [edx+8*edi]
    fmul  qword ptr [eax+8*ecx]
    fadd
    inc   ecx
    cmp   ecx,M
    jne   @@L

    mov   ecx,adrC
    mov   ecx,[ecx+4*esi]
    fadd  qword ptr [ecx+8*edi]
    mov   ecx,adrD
    mov   ecx,[ecx+4*esi]
    fstp  qword ptr [ecx+8*edi]

    inc   edi
    cmp   edi,K
    jne   @@J

    inc   esi
    cmp   esi,N
    jne   @@I
end;

end;


//EXTERNAL LIB:

{*********************************************************************}

procedure SetMatrix(AdrS: TAddress; S,C: Cardinal; var DM: TArray2);
var
adrD: TAddress;
A1: TArray1;
i,j,k: Cardinal;

begin                                           //External Lib
//статич. масс. [S,C] -> динамич. масс. [S,C]
SetLength(DM,S,C);
adrD:=TAddress(DM);

//DOUBLE:

asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    xor   edx,edx
    mov   ebx,adrS
@@1:
    mov   eax,adrD
    mov   eax,[eax+4*edx]
    mov   esi,ebx
    mov   edi,eax
    mov   ecx,C
    shl   ecx,1
    cld
    rep   movsd

    mov   ecx,C
    shl   ecx,3
    add   ebx,ecx

    inc   edx
    cmp   edx,S
    jne   @@1

    pop   edi
    pop   esi
    pop   edx
    pop   ecx
    pop   ebx
    pop   eax
end;


//EXTENDED:
{
asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    xor   edx,edx
    mov   ebx,adrS
@@1:
    mov   eax,adrD
    mov   eax,[eax+4*edx]
    mov   esi,ebx
    mov   edi,eax
    mov   ecx,C
    lea   ecx,ecx+4*ecx
    cld
    rep   movsw

    mov   ecx,C
    lea   ecx,ecx+4*ecx
    lea   ebx,ebx+2*ecx

    inc   edx
    cmp   edx,S
    jne   @@1

    pop   edi
    pop   esi
    pop   edx
    pop   ecx
    pop   ebx
    pop   eax
end;
}


end;





procedure GetMatrix(DM: TArray2; var S,C: Cardinal; var SM: TArray1);
var
adrD,adrS: TAddress;
C1,S1: Cardinal;
begin                                         //External Lib
//динамич. масс. [S,C] -> статич. масс. [S,C]
S1:=Length(DM);
C1:=Length(DM[0]);
SetLength(SM,S1*C1);
adrD:=TAddress(DM);
adrS:=TAddress(SM);
S:=S1;
C:=C1;

//DOUBLE:

asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    xor   edx,edx
    mov   ebx,adrS
@@1:
    mov   eax,adrD
    mov   eax,[eax+4*edx]
    mov   esi,eax
    mov   edi,ebx
    mov   ecx,C1
    shl   ecx,1   //ecx*2  //D/E
    cld
    rep   movsd            //D/E
    mov   ecx,C1  //ecx*8  //D/E
    shl   ecx,3
    add   ebx,ecx
    inc   edx
    cmp   edx,S1
    jne   @@1

    pop   edi
    pop   esi
    pop   edx
    pop   ecx
    pop   ebx
    pop   eax
end;


//EXTENDED:
{
asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    xor   edx,edx
    mov   ebx,adrS
@@1:
    mov   eax,adrD
    mov   eax,[eax+4*edx]
    mov   esi,eax
    mov   edi,ebx
    mov   ecx,C1
    lea   ecx,ecx+4*ecx
    cld
    rep   movsw

    mov   ecx,C1
    lea   ecx,ecx+4*ecx
    lea   ebx,ebx+2*ecx
    inc   edx
    cmp   edx,S1
    jne   @@1

    pop   edi
    pop   esi
    pop   edx
    pop   ecx
    pop   ebx
    pop   eax
end;
}

end;




procedure  MatrixCopy(A,B: TArray2);
var
S,C,N: Cardinal;
adrA,adrB: TAddress;
begin                                 //External Lib
S:=High(A); C:=High(A[0]);
adrA:=TAddress(A); adrB:=TAddress(B);
//DOUBLE:
N:=(C+1)*2;
asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    mov   eax,adrA
    mov   ebx,adrB
    xor   edx,edx
@@I:
    mov   esi,[eax+4*edx]
    mov   edi,[ebx+4*edx]
    mov   ecx,N
    cld
    rep   movsd
    inc   edx
    cmp   edx,S
    jle   @@I

    pop   edi
    pop   esi
    pop   edx
    pop   ecx
    pop   ebx
    pop   eax
end;


//EXTENDED:
{
N:=(C+1)*5;
asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    mov   eax,adrA
    mov   ebx,adrB
    xor   edx,edx
@@I:
    mov   esi,[eax+4*edx]
    mov   edi,[ebx+4*edx]
    mov   ecx,N
    cld
    rep   movsw
    inc   edx
    cmp   edx,S
    jle   @@I

    pop   edi
    pop   esi
    pop   edx
    pop   ecx
    pop   ebx
    pop   eax
end;
}

end;






procedure  MatrixNewCopy(A: TArray2; var B: TArray2);
var
S,C,N: Cardinal;
adrA,adrB: TAddress;
begin                                 //External Lib
S:=High(A); C:=High(A[0]);
SetLength(B,S+1,C+1);
adrA:=TAddress(A); adrB:=TAddress(B);
//DOUBLE:
N:=(C+1)*2;
asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    mov   eax,adrA
    mov   ebx,adrB
    xor   edx,edx
@@I:
    mov   esi,[eax+4*edx]
    mov   edi,[ebx+4*edx]
    mov   ecx,N
    cld
    rep   movsd
    inc   edx
    cmp   edx,S
    jle   @@I

    pop   edi
    pop   esi
    pop   edx
    pop   ecx
    pop   ebx
    pop   eax
end;


//EXTENDED:
{
N:=(C+1)*5;
asm
    push  eax
    push  ebx
    push  ecx
    push  edx
    push  esi
    push  edi

    mov   eax,adrA
    mov   ebx,adrB
    xor   edx,edx
@@I:
    mov   esi,[eax+4*edx]
    mov   edi,[ebx+4*edx]
    mov   ecx,N
    cld
    rep   movsw
    inc   edx
    cmp   edx,S
    jle   @@I

    pop   edi
    pop   esi
    pop   edx
    pop   ecx
    pop   ebx
    pop   eax
end;
}

end;



