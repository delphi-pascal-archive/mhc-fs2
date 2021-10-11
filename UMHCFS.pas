unit UMHCFS;

{   SOREL(C)       CopyRight   1999-2000 Russia, S.-Petersburg.                }
{                                                                              }
{                  calculator         MHC-FS     ver 1.0.1                     }
{                                                                              }
{                            e-mail: delphimanx@rambler.ru                     }


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, ComCtrls, ExtCtrls, DBCtrls, Buttons, Math,
  ThFortFunc, ThFlash, Lib;

type
  TString1 = String[1];

type
  TString5 = String[5];




type
  TForm1 = class(TForm)



    SBsum: TSpeedButton;
    Panel3: TPanel;
    Panel2: TPanel;
    Stdrg: TStaticText;
    StF: TStaticText;
    Stsig: TStaticText;
    SBFH: TSpeedButton;
    SBsig: TSpeedButton;
    SBdrg: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    SBsub: TSpeedButton;
    SBmul: TSpeedButton;
    SBdvd: TSpeedButton;
    SBpoint: TSpeedButton;
    SB9: TSpeedButton;
    SB3: TSpeedButton;
    SB6: TSpeedButton;
    SB8: TSpeedButton;
    SB7: TSpeedButton;
    SBneg: TSpeedButton;
    SB2: TSpeedButton;
    SB5: TSpeedButton;
    SB4: TSpeedButton;
    SBC: TSpeedButton;
    SB1: TSpeedButton;
    SBeq: TSpeedButton;
    SB0: TSpeedButton;
    SBCmn: TSpeedButton;
    SB1x: TSpeedButton;
    SBln: TSpeedButton;
    SBsin: TSpeedButton;
    SBcos: TSpeedButton;
    SBtg: TSpeedButton;
    SBctg: TSpeedButton;
    SBA: TSpeedButton;
    SBB: TSpeedButton;
    StA: TStaticText;
    StB: TStaticText;
    StC: TStaticText;
    SBeq2: TSpeedButton;
    SBsqr: TSpeedButton;
    SBxy: TSpeedButton;
    SBlogxy: TSpeedButton;
    SBxS: TSpeedButton;
    SBxM: TSpeedButton;
    SBSxS: TSpeedButton;
    SBMxM: TSpeedButton;
    SBSx: TSpeedButton;
    SBMx: TSpeedButton;
    SBSC: TSpeedButton;
    SBxplusM: TSpeedButton;
    StS: TStaticText;
    StM: TStaticText;
    StV: TStaticText;
    Stbr: TStaticText;
    DRG: TLabel;
    LStat: TLabel;
    Lth: TLabel;
    Lcth: TLabel;
    Lash: TLabel;
    Lach: TLabel;
    Lath: TLabel;
    Lacth: TLabel;
    Lsum: TLabel;
    Lx1: TLabel;
    Lsum2: TLabel;
    Lx2: TLabel;
    Lsx2: TLabel;
    Lsn: TLabel;
    Ln: TLabel;
    Lsn1: TLabel;
    Ln1: TLabel;
    LMHC1S: TLabel;
    Label3: TLabel;
    Lx_: TLabel;
    StHrs: TStaticText;
    StTime: TStaticText;
    StMns: TStaticText;
    T1: TTimer;
    SBesc: TSpeedButton;
    SBCE: TSpeedButton;
    SBarrow: TSpeedButton;
    SBb1: TSpeedButton;
    SBb2: TSpeedButton;
    SBdms: TSpeedButton;
    T2: TTimer;
    LF: TLabel;
    StFx: TStaticText;
    Stx: TStaticText;
    SBFunc: TSpeedButton;
    STFunc: TStaticText;
    L_FE: TLabel;
    ImArr1: TImage;
    ImArr2: TImage;
    ImDel: TImage;
    ImX: TImage;
    ImFS: TImage;
    ImSF: TImage;
    ImSC: TImage;
    ImA: TImage;
    Imb: TImage;
    ImC: TImage;
    ImFSF: TImage;
    LS: TLabel;
    SBD: TSpeedButton;
    SBis2: TSpeedButton;
    SBis1: TSpeedButton;
    SBE: TSpeedButton;
    SBddx: TSpeedButton;
    Imd: TImage;
    Ime: TImage;
    ImFX: TImage;
    StD: TStaticText;
    StE: TStaticText;
    Imn: TImage;
    Imk: TImage;
    Imy: TImage;
    Imbr2: TImage;
    Imbr1: TImage;
    ImMH: TImage;
    Image1: TImage;


    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SB0Click(Sender: TObject);
    procedure SB1Click(Sender: TObject);
    procedure SB2Click(Sender: TObject);
    procedure SB3Click(Sender: TObject);
    procedure SB4Click(Sender: TObject);
    procedure SB5Click(Sender: TObject);
    procedure SB6Click(Sender: TObject);
    procedure SB7Click(Sender: TObject);
    procedure SB8Click(Sender: TObject);
    procedure SB9Click(Sender: TObject);
    procedure SBsumClick(Sender: TObject);
    procedure SBsubClick(Sender: TObject);
    procedure SBmulClick(Sender: TObject);
    procedure SBdvdClick(Sender: TObject);
    procedure SBeqClick(Sender: TObject);
    //procedure SBdigClick(Sender: TObject);
   



    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBdmsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBCmnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBsqrMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBxyMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBctgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBtgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBcosMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBsinMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBeq2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SB1xMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBlnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBlogxyMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);


    procedure ERROR;
    procedure SBdmsClick(Sender: TObject);
    procedure WMVar(V: TFloat);
    procedure WEVar(E: TFloat);
    procedure OutVar(OV: TFloat);
    procedure ShowVar;
    procedure WSMVar(S: String);
    procedure WSEVar;
    procedure NewNum;
    procedure Sum;
    procedure Dvd;
    procedure Mul;
    procedure Sub;
    function  arcsh(X: TFloat): TFloat;
    procedure XtoY;
    procedure XRootY;
    function  arcch(x: TFloat): TFloat;
    function  Qroot(x: TFloat): TFloat;
    function  Quad(x: TFloat): TFloat;
    procedure Cnm;
    procedure logxy;
    function  Factor(x: TFloat): TFloat;
    function  arccth(x: TFloat): TFloat;
    function  DToR(x: TFloat): TFloat;
    function  GToR(X:TFloat): TFloat;
    function  RToD(x: TFloat): TFloat;
    function  RToG(x: TFloat): TFloat;
    function  ch(x: TFloat): TFloat;
    function  sh(x: TFloat): TFloat;
    function  sinus(x: TFloat): TFloat;
    function  cosinus(x: TFloat): TFloat;
    function  ArcCosinus(X: TFloat): TFloat;
    function  Tang(x:TFloat):TFloat;
    function  CTang(x:TFloat):TFloat;
    function  ArcTang(x:TFloat):TFloat;
    function  ArcSinus(x:TFloat):TFloat;
    function  Aver:TFloat;
    function  ArcCTang(x:TFloat):TFloat;
    function  th(x: TFloat):TFloat;
    function  cth(x: TFloat):TFloat;
    function  Qsum:TFloat;
    function  SigN:TFloat;
    function  SigN1:TFloat;
    function  SumS:TFloat;
    function  Lne(x: TFloat):TFloat;
    function  QR12: TFloat;
    function  arcth(x:TFloat): TFloat;
    procedure Eq2;
    function  DMS(x: TFloat): TFloat;
    procedure SBnegClick(Sender: TObject);
    procedure SBpointClick(Sender: TObject);
    procedure SBescClick(Sender: TObject);
    procedure SBCEClick(Sender: TObject);
    procedure SBarrowClick(Sender: TObject);
    procedure WriteVar;
    procedure FormCreate(Sender: TObject);
    procedure SetFH;
    procedure SBFHClick(Sender: TObject);
    procedure SBAClick(Sender: TObject);
    procedure SBBClick(Sender: TObject);
    procedure SBCClick(Sender: TObject);
    procedure SBxMClick(Sender: TObject);
    procedure SBMxClick(Sender: TObject);
    procedure SBMxMClick(Sender: TObject);
    procedure SBxplusMClick(Sender: TObject);
    procedure SBSCClick(Sender: TObject);
    procedure SBxSClick(Sender: TObject);
    procedure SBSxClick(Sender: TObject);
    procedure SBb1Click(Sender: TObject);
    procedure SBb2Click(Sender: TObject);
    procedure SBdrgClick(Sender: TObject);
    procedure SBsigClick(Sender: TObject);
    procedure SBxyClick(Sender: TObject);
    procedure SBsqrClick(Sender: TObject);
    procedure SBCmnClick(Sender: TObject);
    procedure SBcosClick(Sender: TObject);
    procedure SBtgClick(Sender: TObject);
    procedure SBctgClick(Sender: TObject);
    procedure SBsinClick(Sender: TObject);
    procedure SBlnClick(Sender: TObject);
    procedure SB1xClick(Sender: TObject);
    procedure SBlogxyClick(Sender: TObject);
    procedure SBeq2Click(Sender: TObject);
    procedure T1Timer(Sender: TObject);
    procedure SBSxSClick(Sender: TObject);
    procedure T2Timer(Sender: TObject);
    procedure SBFuncClick(Sender: TObject);
    procedure AddFunction(S: String);
    procedure ViewerL;
    procedure ViewerR;
    procedure DelFuncSymbol;
    procedure SBeqMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBddxClick(Sender: TObject);
    function Diff1(x: TFloat): TFloat;
    //procedure CreateFunc;
    procedure SBDClick(Sender: TObject);
    procedure SBEClick(Sender: TObject);
    procedure SBAMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBCMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBDMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBEMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBddxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBis1Click(Sender: TObject);
    procedure SBis1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBis2Click(Sender: TObject);
    procedure SBis2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);


   procedure  SetFunction;
 

  private

      //MN - мантисса текущего числа
      //EN - пор€док текущего числа
      //ES - пор€док текущего числа (String)
      //SN - знак текущего числа (0,1)
      //SE - знак пор€дка текущего числа  (0,1)
      //cRS - текущ. эл-т стека
      //FE - флаг эксп. набора
      //FF - флаг плав. числа (с '.')
      //FNN - флаг нового числа
      //FSP - флаг нажати€ ('.') = TRUE
      //FSE - флаг нажати€ ('=') = TRUE
      //FER - флаг ошибки (TRUE)
      //FRS - флаг просмотра стека (TRUE)
      //FEO - разрешение операции (исп-с€ дл€ ')' и дл€ 'A','B','C',..,'M')
      //FTR  - true если идет выполнение подзадачи (спец. ф-ии)
      MN, EN, CInt, CFl, CTN, SN, SE, R_M,
      Con_E, Con_Pi, R_X1, R_X2, Sdms,
      MAX_NUM, MIN_NUM : TFloat;
      ES, SS: String;
      COP: String[5];
      FNN, FSP, FF, FE, FSE, FER, FRS, FEO, FSS : Boolean;
      LeN, LeS, LeRS, cRS, Br, DIG, cFP, cFS, LeFS, DRS: Integer;
      FH, Sig, RX12, smd, Stime: String[1];
      Hrs, Mns, FFF: String[2];
      GRD: String[4];
      R_S: array[1..99] of TFloat;
      F_S: array[1..99] of String;
      BN: array[1..99] of TFloat;
      BC: array[1..99] of String[5];
     


  public
    R_X, R_Y, V_X, V_Y, R_A, R_B, R_C, R_D, R_E,
    F_A, F_B, F_C, F_D, F_E,P_A,P_B,P_C,P_D,P_E: TFloat;
    V_N,V_K: Integer;
    FTF: Boolean;
    SFunc, reg: String;
    C_Func: Pointer;

    procedure BeginFort(FCOP: String; reg: string; X: TFloat = 0);
    procedure Delay(D: LongInt);
    procedure BeginFlash;
    function  Calc(Func: Pointer): TFloat;
  end;



var
  Form1: TForm1;
  Fort: TThFortFunc;
  Flash: TThFlash;
  //MathProcA: TMathLib;


implementation



{$R *.DFM}
function TForm1.Calc(Func: Pointer): TFloat;
  asm
   Call Func
  end;

procedure TForm1.SetFunction;
begin
 if C_Func <> nil then flPerform(fl_FREE,Cardinal(C_Func));
 flCompile(SFunc,0,C_Func);
end;





procedure TForm1.NewNum;
begin
CInt:=0; CFl:=0; MN:=0; CTN:=0; SN:=0; FNN:=False; FSP:=False;
FF:=False; Len:=0; FE:=False; EN:=0; ES:=Copy('00',1,2);SE:=1;
end;

procedure TForm1.WMVar(V: TFloat);
var
maxLenVar: Integer;
begin
maxLenVar:=12;
WSMVar(FloatToStr(V));

if FNN = True then NewNum;

if (FSP = True) and (FF = False) then
begin
CInt:=0; FSP:=False; FF:=True;
CFl:=1; CTN:=abs(MN);
if CTN < 1 then Len:=Len+1;
end;

if LeN < MaxLenVar then
begin
LeN:=LeN+1;
CTN:=V*Power(10,-CInt-CFl)+CTN;
MN:=CTN*Power(10,CInt);

If FF = True then
begin
CFl:=CFl+1;
end
else
begin
CInt:=CInt+1;
end;

MN:=MN*Power(-1,SN);
If reg = 'x' then
begin
R_X:=MN; R_X:=R_X; {StV.Caption:=FloatToStr(R_X);}
end
else
begin
R_Y:=MN; R_Y:=R_Y; {StV.Caption:=FloatToStr(R_Y);}
end;
end;

{if SN = 1 then
begin
Delete(SS,1,1);
Insert('Ц',SS,1);
end;
StV.Caption:=SS;
if SN = 1 then
begin
Delete(SS,1,1);
Insert(' ',SS,1);
end;}
ShowVar;
end;

procedure TForm1.WEVar(E: TFloat);
begin
Delete(ES,1,1);
Insert(FloatToStr(E),ES,2);
WSEVar;
EN:=SE*StrToFloat(ES);
try
if reg = 'x' then
begin
R_X:=MN*Power(10,EN);{StV.Caption:=FloatToStr(R_X);}
end
else
begin
R_Y:=MN*Power(10,EN);{StV.Caption:=FloatToStr(R_Y);}
end;
except
ERROR;
end;
end;

procedure TForm1.WSEVar;
begin
Delete(SS,Length(SS)-2{3,4},3{4,5}); //EXP
If SE = 1 then Insert('+',SS,Length(SS)+1)
else  Insert('Ц',SS,Length(SS)+1);
Insert(ES,SS,Length(SS)+1);
StV.Caption:=SS;
end;

procedure TForm1.OutVar(OV: TFloat);
label 1;
var
St, SV, M, E, MA, ex: String;
LE, i,  P, Lp: Integer;                           
Neg,SE: Boolean;                                  
begin
if OV > MAX_NUM then ERROR;
if abs(OV) <  MIN_NUM then OV:=0;
if FER =True then goto 1;
LE:=0;  P:=0; St:=FloatToStr(OV);  Neg:= False; E:=Copy('',1,1);
SE:=False; Lp:=0;

if St[1] = '-' then
 begin
  Neg:=True;
  Delete(St,1,1);
 end;

For i:=1 to Length(St) do
begin
if St[i] = 'E' then LE:=i;
end;

if LE > 0 then
 begin
  M:=Copy(St,1,LE-1);
  E:=Copy(St,LE,Length(St)-LE+1);
  SE:=True;
 end
else
 begin
  for i:=1 to Length(St) do
   begin
    if St[i] = ',' then P:=i;
   end;
  if P <> 0 then MA:=Copy(St,1,P-1)
  else MA:=Copy(St,1,Length(St));

  if Length(MA) > DIG then
   begin
    E:=IntToStr(Length(MA)-1);
    Insert('E',E,1);
    M:=Copy(MA,1,Length(MA));
    Insert(',',M,2);
    SE:=True;
   end
  else
  M:=Copy(St,1,Length(St));
 end;



for i:=1 to Length(M) do
begin
if M[i] = ',' then Lp:=1;
end;

M:=Copy(M,1,DIG+Lp);

If Neg = True then
begin
Insert('Ц',M,1);
end;

P:=0;
For i:=1 to Length(M) do
begin
if M[i] = ',' then P:=i;
end;

If P <> 0 then
begin
Delete(M,P,1); Insert('.',M,P);
end
else  Insert('.',M,Length(M)+1);

If SE = True Then
begin
Delete(E,1,1); Insert('E',E,1);
If E[2] = '-' then
begin
Delete(E,2,1); Insert('Ц',E,2);
end
else Insert('+',E,2);
ex:=Copy(E,3,Length(E)-2);
Delete(E,3,Length(E)-2);
{If Length(ex) = 1 then Insert('000',ex,1);
If Length(ex) = 2 then Insert('00',ex,1);
If Length(ex) = 3 then Insert('0',ex,1);}
If Length(ex) = 1 then Insert('0',ex,1);
Insert(ex,E,3);
end;
SV:=Copy(M,1,Length(M)); Insert(E,SV,Length(SV)+1);

StV.Caption:=SV;

1: end;


procedure TForm1.WSMVar(S: String);
var
MaxLenVar: Integer;
begin
MaxLenVar:=12;
If FNN = True then
begin
SS:=Copy(' 0.',1,3); Delete(SS,2,1); LeS:=2;
end;
If (FSP = True) and (FF = False) then
begin
LeS:=LeS+1;
end;
if Len < MaxLenVar then
begin
Insert(S,SS,LeS);
LeS:=LeS+1;
end;
{if SN = 1 then
begin
Delete(SS,1,1);
Insert('Ц',SS,1);
end;
StV.Caption:=SS;
if SN = 1 then
begin
Delete(SS,1,1);
Insert(' ',SS,1);
end;}
end;


procedure TForm1.ShowVar;
begin
if FER = False then
begin
if FE = False then
begin

  if SN = 1 then
  begin
   Delete(SS,1,1);
   Insert('Ц',SS,1);
  end;
  StV.Caption:=SS;
  if SN = 1 then
  begin
   Delete(SS,1,1);
   Insert(' ',SS,1);
  end
end

else
begin
  If SE = 1 then
  begin
   Delete(SS,Length(SS)-2{3,4},1);   //EXP
   Insert('+',SS,Length(SS)-1{2,3});
  end
  else
  begin
   Delete(SS,Length(SS)-2{3,4},1);
   Insert('Ц',SS,Length(SS)-1{2,3});          //EXP
  end;
 StV.Caption:=SS;
end;
end;
end;




procedure TForm1.Sum;
begin
R_X:=R_X+R_Y;
end;



procedure TForm1.Sub;
begin
R_X:=R_X-R_Y;
end;

procedure TForm1.Mul;
begin
try
R_X:=R_X*R_Y;
except
ERROR;
end;
end;

procedure TForm1.Dvd;
begin
try
R_X:=R_X/R_Y;
except
ERROR;
end;
end;

function TForm1.arcsh(X: TFloat):TFloat;
begin
try
arcsh:=Logn(exp(1),x+sqrt(sqr(x)+1));
except
ERROR;
end;
end;

procedure TForm1.SBeqClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin

if (FH = 'F') and (SFunc <> '') then
 begin
  SetFunction;
 try
 except
 FER:=True;
 ERROR;
 end;
 FH:=' ';
 StF.Caption:=FH;
 end;

if FH = 'F' then goto 1;

if COP = '+' then Sum;
if COP = '-' then Sub;
if COP = '/' then Dvd;
if COP = '*' then Mul;
if COP = 'xy'    then XtoY;
if COP = 'xRy'   then XRootY;
if COP = 'Cnm'   then Cnm;
if COP = 'logxy' then logxy;
{if COP = 'NOP' then R_X:=FE;}
1:
COP:='NOP';
R_Y:=0;
Reg:='x';
NewNum;
FNN:=True;
MN:=R_X;
Form1.OutVar(R_X);
{if MN < 0 then SN:=1;} {если включать, то убрать условие FNN=False из /-/}
{StV.Caption:=FloatToStr(R_X);}
FEO:=False;
end;
end;


procedure TForm1.WriteVar;
begin
{if Reg = 'x' then  R_X:=StrtoFloat(SS)
 else
 R_Y:=StrtoFloat(SS) }
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if (key = 49) or (key = 97) then
begin
SB1.Down:=False;
SB1.GroupIndex:=0;
end;
if (key = 50) or (key = 98) then
begin
SB2.Down:=False;
SB2.GroupIndex:=0;
end;
if (key = 51) or (key = 99) then
begin
SB3.GroupIndex:=0;
SB3.Down:=False;
end;
if (key = 52) or (key = 100) then
begin
SB4.GroupIndex:=0;
SB4.Down:=False;
end;
if (key = 53) or (key = 101) then
begin
SB5.GroupIndex:=0;
SB5.Down:=False;
end;
if (key = 54) or (key = 102) then
begin
SB6.GroupIndex:=0;
SB6.Down:=False;
end;
if (key = 55) or (key = 103) then
begin
SB7.GroupIndex:=0;
SB7.Down:=False;
end;
if (key = 56) or (key = 104) then
begin
SB8.GroupIndex:=0;
SB8.Down:=False;
end;
if (key = 57) or (key = 105) then
begin
SB9.GroupIndex:=0;
SB9.Down:=False;
end;
if (key = 48) or (key = 96) then
begin
SB0.GroupIndex:=0;
SB0.Down:=False;
end;
if (key = 13) or (key = 187) then
begin
SBeq.GroupIndex:=0;
SBeq.Down:=False;
end;
if key = 27 then
begin
SBesc.GroupIndex:=0;
SBesc.Down:=False;
end;
if (key = 46)  then
begin
SBce.GroupIndex:=0;
SBce.Down:=False;
end;
if (key = 37) or (key = 39) then
begin
 if FFF = 'EF' then
 begin
  if key = 37 then
  begin
   SBxM.GroupIndex:=0;
   SBxM.Down:=False;
  end
  else
  begin
   SBMx.GroupIndex:=0;
   SBMx.Down:=False;
  end
 end
else
 begin
  SBarrow.GroupIndex:=0;
  SBarrow.Down:=False;
 end;
end;
if (key = 106) {or ((key = 56) and (ssShift in Shift))} then
begin
SBmul.GroupIndex:=0;
SBmul.Down:=False;
end;
if (key = 111) or (key = 191) then
begin
SBdvd.GroupIndex:=0;
SBdvd.Down:=False;
end;
if (key = 109) or (key = 189) then
begin
SBsub.GroupIndex:=0;
SBsub.Down:=False;
end;
if (key = 107)  then
begin
SBsum.GroupIndex:=0;
SBsum.Down:=False;
end;
if (key = 110) or (key = 190) or (key = 188) then
begin
SBpoint.GroupIndex:=0;
SBpoint.Down:=False;
end;
if (key = 69)  then
begin
SBdms.GroupIndex:=0;
SBdms.Down:=False;
end;
if (key = 78)  then
begin
SBneg.GroupIndex:=0;
SBneg.Down:=False;
end;
if (key = 8) and (FFF = 'EF') then
begin
SBMxM.GroupIndex:=0;
SBMxM.Down:=False;
end;
if (key = 88) and (FFF = 'EF') then
begin
SBxplusM.GroupIndex:=0;
SBxplusM.Down:=False;
end;
if (key = 65)  then
begin
SBA.GroupIndex:=0;
SBA.Down:=False;
end;
if (key = 66)  then
begin
SBB.GroupIndex:=0;
SBB.Down:=False;
end;
if (key = 67)  then
begin
SBC.GroupIndex:=0;
SBC.Down:=False;
end;
{if   (key = 57) and (ssShift in Shift) then
begin
SBb1.GroupIndex:=0;
SBb1.Down:=False;
end;
if   (key = 48) and (ssShift in Shift) then
begin
SBb2.GroupIndex:=0;
SBb2.Down:=False;
end;}
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if (key = 49) or (key = 97) then
begin
SB1.GroupIndex:=1;
SB1.Down:=True;
SB1.Click;
end;
if (key = 50) or (key = 98) then
begin
SB2.GroupIndex:=1;
SB2.Down:=True;
SB2.Click;
end;
if (key = 51) or (key = 99) then
begin
SB3.GroupIndex:=1;
SB3.Down:=True;
SB3.Click;
end;
if (key = 52) or (key = 100) then
begin
SB4.GroupIndex:=1;
SB4.Down:=True;
SB4.Click;
end;
if (key = 53) or (key = 101) then
begin
SB5.GroupIndex:=1;
SB5.Down:=True;
SB5.Click;
end;
if (key = 54) or (key = 102) then
begin
SB6.GroupIndex:=1;
SB6.Down:=True;
SB6.Click;
end;
if (key = 55) or (key = 103) then
begin
SB7.GroupIndex:=1;
SB7.Down:=True;
SB7.Click;
end;
if (key = 56) or (key = 104) then
begin
SB8.GroupIndex:=1;
SB8.Down:=True;
SB8.Click;
end;
if (key = 57) or (key = 105) then
begin
SB9.GroupIndex:=1;
SB9.Down:=True;
SB9.Click;
end;
if (key = 48) or (key = 96) then
begin
SB0.GroupIndex:=1;
SB0.Down:=True;
SB0.Click;
end;
if (key = 13) {or (key = 187)} then
begin
SBeq.GroupIndex:=1;
SBeq.Down:=True;
SBeq.Click;
end;
if key = 27  then
begin
SBesc.GroupIndex:=1;
SBesc.Down:=True;
SBesc.Click;
end;
if (key = 46)  then
begin
SBce.GroupIndex:=1;
SBce.Down:=True;
SBce.Click;
end;
if (key = 37) or (key = 39) then
begin
if FFF = 'EF' then
 begin
  if key = 37 then
   begin
    SBxM.GroupIndex:=1;
    SBxM.Down:=True;
    SBxM.Click;
   end
   else
   begin
    SBMx.GroupIndex:=1;
    SBMx.Down:=True;
    SBMx.Click;
   end
 end
else
 begin
  SBarrow.GroupIndex:=1;
  SBarrow.Down:=True;
  SBarrow.Click;
 end;
end;
if (key = 106) {or ((key = 56) and (ssShift in Shift))} then
begin
SBmul.GroupIndex:=1;
SBmul.Down:=True;
SBmul.Click;
end;
if (key = 111) or (key = 191) then
begin
SBdvd.GroupIndex:=1;
SBdvd.Down:=True;
SBdvd.Click;
end;
if (key = 109) or (key = 189) then
begin
SBsub.GroupIndex:=1;
SBsub.Down:=True;
SBsub.Click;
end;
if (key = 107)  then
begin
SBsum.GroupIndex:=1;
SBsum.Down:=True;
SBsum.Click;
end;
if (key = 110) or (key = 190) or (key = 188) then
begin
SBpoint.GroupIndex:=1;
SBpoint.Down:=True;
SBpoint.Click;
end;
if (key = 69)  then
begin
SBdms.GroupIndex:=1;
SBdms.Down:=True;
SBFH.Click;
SBdms.Click;
end;
if (key = 78)  then
begin
SBneg.GroupIndex:=1;
SBneg.Down:=True;
SBneg.Click;
end;
if (key = 8) and (FFF = 'EF') then
begin
SBMxM.GroupIndex:=1;
SBMxM.Down:=True;
SBMxM.Click;
end;
if (key = 88) and (FFF = 'EF') then
begin
SBxplusM.GroupIndex:=1;
SBxplusM.Down:=True;
SBxplusM.Click;
end;
if (key = 65)  then
begin
SBA.GroupIndex:=1;
SBA.Down:=True;
SBA.Click;
end;
if (key = 66)  then
begin
SBB.GroupIndex:=1;
SBB.Down:=True;
SBB.Click;
end;
if (key = 67)  then
begin
SBC.GroupIndex:=1;
SBC.Down:=True;
SBC.Click;
end;
{if   (key = 57) and (ssShift in Shift) then
begin
SBb1.GroupIndex:=1;
SBb1.Down:=True;
SBb1.Click;
end;
if   (key = 48) and (ssShift in Shift) then
begin
SBb2.GroupIndex:=1;
SBb2.Down:=True;
SBb2.Click;
end; }
end;

procedure TForm1.SB0Click(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('0'); goto 1; end;
If FNN = True Then
begin
if reg = 'x' then R_X:=0
else R_Y:=0;
StV.Caption:=' 0.';
end;
if (MN <> 0) or (FSP = True) or (FF = True) then
if FE = False then WMVar(0)
else Form1.WEVar(0);
end;
1:
end;


procedure TForm1.SB1Click(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('1'); goto 1; end;

if FE = False then WMVar(1)
else Form1.WEVar(1);
end;

1:
end;

procedure TForm1.SB2Click(Sender: TObject);
label 1;
begin
if (FER = False) then
begin
if FFF = 'EF' then begin AddFunction('2'); goto 1; end;
if FE = False then WMVar(2)
else Form1.WEVar(2);
end;
1:
end;

procedure TForm1.SB3Click(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('3'); goto 1; end;
if FE = False then WMVar(3)
else Form1.WEVar(3);
end;
1:
end;

procedure TForm1.SB4Click(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('4'); goto 1; end;
if FE = False then WMVar(4)
else Form1.WEVar(4);
end;
1:
end;

procedure TForm1.SB5Click(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('5'); goto 1; end;
if FE = False then WMVar(5)
else Form1.WEVar(5);
end;
1:
end;

procedure TForm1.SB6Click(Sender: TObject);
label 1;
begin
if (FER = False) then
begin
if FFF = 'EF' then begin AddFunction('6'); goto 1; end;
if FE = False then WMVar(6)
else Form1.WEVar(6);
end;
1:
end;

procedure TForm1.SB7Click(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('7'); goto 1; end;
if FE = False then WMVar(7)
else Form1.WEVar(7);
end;
1:
end;

procedure TForm1.SB8Click(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('8'); goto 1; end;
if FE = False then WMVar(8)
else Form1.WEVar(8);
end;
1:
end;

procedure TForm1.SB9Click(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('9'); goto 1; end;
if FE = False then WMVar(9)
else Form1.WEVar(9);
end;
1:
end;

procedure TForm1.SBsumClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('+'); goto 1; end;

If ((COP <> 'NOP') and (FNN = False)) or (FEO = True) then
begin
 Sbeq.Click;
 COP:='NOP';
end;
R_Y:=R_X;
NewNum;
FNN:=True;
Reg:='y';
COP:='+';
end;
1:
end;


procedure TForm1.SBsubClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('-'); goto 1; end;

If (COP <> 'NOP') and (FNN = False) or (FEO = True) then
begin
 Sbeq.Click;
 COP:='NOP';
end;
R_Y:=R_X;
NewNum;
FNN:=True;
Reg:='y';
COP:='-';
end;
1:
end;


procedure TForm1.SBmulClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('*'); goto 1; end;

If (COP <> 'NOP') and (FNN = False) or (FEO = True) then
begin
 Sbeq.Click;
 COP:='NOP';
end;
R_Y:=R_X;
NewNum;
FNN:=True;
Reg:='y';
COP:='*';
end;
1:
end;

procedure TForm1.SBdvdClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('/'); goto 1; end;

If (COP <> 'NOP')  and (FNN = False) or (FEO = True) then
begin
 Sbeq.Click;
 COP:='NOP';
end;
R_Y:=R_X;
NewNum;
FNN:=True;
Reg:='y';
COP:='/';
end;
1:
end;



procedure TForm1.SBnegClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('-'); goto 1; end;

If FNN = False then
begin

if FE = False then
begin
if SN = 0 then SN:=1 else SN:=0;
MN:=(-1)*MN;
If reg = 'x' then
begin
R_X:=MN; {StV.Caption:=FloatToStr(R_X);}
end
else
begin
R_Y:=MN; {StV.Caption:=FloatToStr(R_Y);}
end;
ShowVar;
end

else
begin
if SE = 1 then SE:=-1 else SE:=1;
EN:=(-1)*EN;
If reg = 'x' then
begin
R_X:=MN*Power(10,EN); {StV.Caption:=FloatToStr(R_X);}
end
else
begin
R_Y:=MN*Power(10,EN); {StV.Caption:=FloatToStr(R_Y);}
end;
ShowVar;
end;
end;
end;
1:
end;



procedure TForm1.SBpointClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('.'); goto 1; end;

if FNN = True then
begin
NewNum;
SS:=Copy(' 0.',1,3);
StV.Caption:=SS;
{Delete(SS,2,1);} LeS:=3;
if reg = 'x' then R_X:=0
else R_Y:=0;
end;
FSP:=True;
end;
1:
end;

procedure TForm1.SBescClick(Sender: TObject);
label 1;
begin
NewNum;
FH:=' ';
StF.Caption:=FH;
COP:='NOP';
R_X:=0; R_Y:=0;
R_X1:=0;
R_X2:=0;
FNN:=True;
Reg:='x';
RX12:='1';
smd:='d';
SS:=Copy(' 0.',1,3);
StV.Caption:=SS;
Br:=0;
FEO:=False;
StBr.Caption:='   ';

If FTF = True then
begin
Fort.Suspend;
Fort.Terminate;
Stv.Font.Color:=clAqua;
FTF:=False;
StV.Caption:=SS;
end;

if FFF = 'EF' then
begin
FSS:=False;
cFS:=LeFS;
if LeFS <> 0 then begin StS.Caption:=IntToStr(cFS); cFP:=0; end;
if FER = False then SFunc:='';
LF.Caption:=SFunc;
goto 1;
end;

FRS:=False;
cRS:=LeRS;
if LeRS <> 0 then StS.Caption:=IntToStr(cRS);

1:
FER:=False;
end;



procedure TForm1.SBCEClick(Sender: TObject);
label 1;
var
i: Integer;
begin
if (FER = False)  then
begin
if (FFF = 'EF') and (FSS = True) then
begin
 for i:=LeFS-cFS to  LeFS-1 do
 begin
 F_S[i]:=F_S[i+1];
 end;
 LeFS:=LeFS-1;
 cFS:=LeFS;
 if cFS <> 0 then StS.Caption:=IntToStr(cFS)
 else StS.Caption:='  ';
 FSS:=False;
 LF.Caption:=' ';
 goto 1;
end;

If Reg='x' Then
begin
R_X:=0;
end
else
begin
R_Y:=0;
end;
NewNum;
FNN:=True;
SS:=Copy(' 0.',1,3);
StV.Caption:=SS;

if FRS = True then
begin
for i:=LeRS-cRS to  LeRS-1 do
begin
R_S[i]:=R_S[i+1];
end;
LeRS:=LeRS-1;
cRS:=LeRS;
if cRS <> 0 then StS.Caption:=IntToStr(cRS)
else StS.Caption:='   ';
FRS:=False;
end;
end;
1:
end;


procedure TForm1.SBarrowClick(Sender: TObject);
var
xch: TFloat;
begin
if (FER = False) and (FTF = False) then
begin
if reg = 'x' then
begin
xch:=R_Y; reg:= 'y';
end
else
begin
xch:=R_X; reg:='x';
end;


if reg = 'x' then
begin
reg:= 'y'; R_X:=R_Y; R_Y:=xch;
end
else
begin
reg:= 'x'; R_Y:=R_X; R_X:=xch;
end;

NewNum;
FNN:=True;
{StV.Caption:=FloatToStr(xch);}
OutVar(xch);
end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//Form1.Color:=TColor(RGB(230,230,230));
//Panel2.Color:=TColor(RGB(170,170,0));
//STV.Color:=TColor(RGB(170,170,0));

L_FE.Font.Color:=RGB(175,175,255);
Con_E:=2.718281828459045;
Con_Pi:=3.141592653589793238462643;
NewNum;
SetLength(ES,4);
LeFS:=0;
DRS:=99;
COP:='NOP';
R_X:=0; R_Y:=0;
R_X1:=0;
R_X2:=0;
Reg:='x';
RX12:='1';
smd:='d';
FH:=' ';
FSE:=False;
FNN:=True;
FFF:='  ';
SS:=Copy(' 0.',1,3);
StFx.Caption:='    ';
Stx.Caption:=' ';
Stime:=':';
LeRS:=0;
cRS:=LeRS;
cFS:=LeFS;
Br:=0;
FEO:=False;
GRD:='deg';
Sig:=' ';
FRS:=False;
FSS:=False;
DIG:=14;  //кол-во выводимых цифр
MAX_NUM:=9.999999999999E99;
MIN_NUM:=9.999999999999E-100;
SFunc:='';
cFP:=0;

FTF:=False;

flSetVar('A',@R_A,fl_Extended);
flSetVar('B',@R_B,fl_Extended);
flSetVar('C',@R_C,fl_Extended);
flSetVar('D',@R_D,fl_Extended);
flSetVar('E',@R_E,fl_Extended);
flSetVar('x',@R_X,fl_Extended);
flSetVar('y',@R_Y,fl_Extended);
flSetVar('n',@V_N,fl_Integer);
flSetVar('k',@V_K,fl_Integer);
C_Func:=nil;
end;


{procedure TForm1.InitMathLib;
begin
 MathProcA:=TMathLib.Create;
end; }



procedure TForm1.SetFH;
begin
If FH = ' ' then
begin
StF.Font.Color:=clRed; FH:='F';
end
else
if FH = 'F' then
begin
StF.Font.Color:=clYellow; FH:='H';
end
else
begin
FH:=' ';
end;
StF.Caption:=FH;
end;
{procedure TForm1.SBBackClick(Sender: TObject);
var
WN, InN, BrN, c: TFloat;
begin
c:=1;
CFl:=CFl-1;
WN:=R_X;

If WN < 0 then c:=-1;
WN:=abs(WN);
InN:=Trunc(WN);
BrN:=WN-Trunc(WN);
if WN <> 0 Then
begin
if (BrN = 0)  then
begin
  InN:=InN/10;
  InN:=Trunc(InN);
end
 else
begin
  BrN:=BrN*Power(10,CFl-1);
  BrN:=Trunc(BrN);
  BrN:=BrN*Power(10,1-CFl);
  CFl:=CFl-1;
end;
WN:=InN+BrN;
WN:=WN*c;
R_X:=WN;
StV.Caption:=FloatToStr(R_X);
end;
end; }


procedure TForm1.SBFHClick(Sender: TObject);
begin
if (FER = False) and (FTF = False) then
begin
SetFH;
end;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{If Button = mbRight then SetFH;}
end;

procedure TForm1.SBdmsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
If Button = mbRight then SetFH;
end;

procedure TForm1.SBCmnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
If Button = mbRight then SetFH;
end;

procedure TForm1.SBsqrMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
If Button = mbRight then SetFH;
end;

procedure TForm1.SBxyMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
If Button = mbRight then SetFH;
end;

procedure TForm1.SBctgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
If Button = mbRight then SetFH;
end;

procedure TForm1.SBtgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
If Button = mbRight then SetFH;
end;

procedure TForm1.SBcosMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
If Button = mbRight then SetFH;
end;

procedure TForm1.SBsinMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
If Button = mbRight then SetFH;
end;

procedure TForm1.SBeq2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
If Button = mbRight then SetFH;
end;

procedure TForm1.SB1xMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
If Button = mbRight then SetFH;
end;

procedure TForm1.SBlnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
If Button = mbRight then SetFH;
end;

procedure TForm1.SBlogxyMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
If Button = mbRight then SetFH;
end;

procedure TForm1.SBdmsClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FH = ' ' then
begin
if FFF = 'EF' then goto 1;

if reg = 'x' then
begin
R_X:=DMS(R_X);
OutVar(R_X);
end
else
begin
R_Y:=DMS(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
NewNum;
FNN:=True;
FEO:=True;
end

else

if FH = 'F' then
begin
if FFF = 'EF' then goto 1;

if (FNN = False) and (FE = False) then
begin
FE:=True;
Insert('E+00',SS,Length(SS)+1);
if SN = 1 then
begin
Delete(SS,1,1);
Insert('Ц',SS,1);
end;
StV.Caption:=SS;
end;
FH:=' '; StF.Caption:=FH;
end


else
begin
if FFF = 'EF' then begin AddFunction('arcth()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=th(R_X);
OutVar(R_X);
end
else
begin
R_Y:=th(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
NewNum;
FNN:=True;
FEO:=True;
end;
end;

1:
end;






procedure TForm1.SBAClick(Sender: TObject);
label 1;
begin
 If (FER = False)  then
 begin
 if FFF = 'EF' then begin AddFunction('A'); goto 1; end;

  if FH = ' ' then
  begin

   if (COP <> 'NOP') and (FNN = True) Then
   begin
   If reg = 'x' then R_X:=R_A
   else R_Y:=R_A;
   OutVar(R_A);
   NewNum;
   FNN:=True;
   FEO:=True;
   end

   else
    begin
    If reg = 'x' then R_A:=R_X
    else R_A:=R_Y;
    If R_A = 0 then StA.Caption:=' '
    else StA.Caption:='A';
    NewNum;
    FNN:=True;
    end;
   goto 1;
  end;

  if FH = 'F' then
  begin
   If reg = 'x' then F_A:=R_X
    else F_A:=R_Y;
   FH:=' ';
   StF.Caption:=FH;
   NewNum;
   FNN:=True;
   BeginFlash;
  end;

 end;
1:
end;





procedure TForm1.SBBClick(Sender: TObject);
label 1;
begin
 If (FER = False)  then
 begin
 if FFF = 'EF' then begin AddFunction('B'); goto 1; end;

  if FH = ' ' then
  begin

   if (COP <> 'NOP') and (FNN = True) Then
   begin
   If reg = 'x' then R_X:=R_B
   else R_Y:=R_B;
   OutVar(R_B);
   NewNum;
   FNN:=True;
   FEO:=True;
   end

   else
    begin
    If reg = 'x' then R_B:=R_X
    else R_B:=R_Y;
    If R_B = 0 then StB.Caption:=' '
    else StB.Caption:='B';
    NewNum;
    FNN:=True;
    end;
   goto 1;
  end;

  if FH = 'F' then
  begin
  If reg = 'x' then F_B:=R_X
    else F_B:=R_Y;
  FH:=' ';
  StF.Caption:=FH;
  NewNum;
  FNN:=True;
  BeginFlash;
  end;

 end;
1:
end;



procedure TForm1.SBCClick(Sender: TObject);
label 1;
begin
 If (FER = False)  then
 begin
 if FFF = 'EF' then begin AddFunction('C'); goto 1; end;

  if FH = ' ' then
  begin

   if (COP <> 'NOP') and (FNN = True) Then
   begin
   If reg = 'x' then R_X:=R_C
   else R_Y:=R_C;
   OutVar(R_C);
   NewNum;
   FNN:=True;
   FEO:=True;
   end

   else
    begin
    If reg = 'x' then R_C:=R_X
    else R_C:=R_Y;
    If R_C = 0 then StC.Caption:=' '
    else StC.Caption:='C';
    NewNum;
    FNN:=True;
    end;
   goto 1;
  end;

  if FH = 'F' then
  begin
  If reg = 'x' then F_C:=R_X
    else F_C:=R_Y;
  FH:=' ';
  StF.Caption:=FH;
  NewNum;
  FNN:=True;
  BeginFlash;
  end;

 end;
1:
end;





procedure TForm1.SBxMClick(Sender: TObject);
label 1;
begin
IF (FER = False)  then
begin
if FFF = 'EF' then begin ViewerR; goto 1; end;

If reg = 'x' then R_M:=R_X
else R_M:=R_Y;
If R_M = 0 then StM.Caption:=' '
else StM.Caption:='M';
NewNum;
FNN:=True;
end;
1:
end;

procedure TForm1.SBMxClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin ViewerL; goto 1; end;

If reg = 'x' then R_X:=R_M
else R_Y:=R_M;
OutVar(R_M);
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;

procedure TForm1.SBMxMClick(Sender: TObject);
label 1;
var
xch: TFloat;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin DelFuncSymbol; goto 1; end;

xch:=R_M;
If reg = 'x' then R_M:=R_X
else R_M:=R_Y;
If R_M = 0 then StM.Caption:=' '
else StM.Caption:='M';
NewNum;
FNN:=True;
If reg = 'x' then R_X:=xch
else R_Y:=xch;
OutVar(xch);
end;
1:
end;


procedure TForm1.SBxplusMClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('x'); goto 1; end;

If reg = 'x' then R_M:=R_M+R_X
else  R_M:=R_M+R_Y;
If R_M = 0 then StM.Caption:=' '
else StM.Caption:='M';
FNN:=True; ;
end;
1:
end;


procedure TForm1.SBSCClick(Sender: TObject);
label 1;
begin
if (FER = False) then
begin
if FFF = 'EF' then
begin
LeFS:=0;
StS.Caption:='  ';
goto 1;
end;

LeRS:=0;
StS.Caption:='  ';
end;
1:
end;


procedure TForm1.SBxSClick(Sender: TObject);
label 1;
var
i: Integer;
begin
if (FER = False) Then
begin
 if (FFF = 'EF') and (SFunc <> '') then
 begin
  i:=LeFS;
  while i >= 1 do
  begin
  if i = DRS then i:=DRS-1;
  F_S[i+1]:=F_S[i];
  i:=i-1;
  end;
  if LeFS < DRS then LeFS:=LeFS+1;
  F_S[1]:=SFunc;
  StS.Caption:=IntToStr(LeFS);
  cFS:=LeFS;
  goto 1;
 end;

i:=LeRS;
while i >= 1 do
begin
if i = DRS then i:=DRS-1;
R_S[i+1]:=R_S[i];
i:=i-1;
end;
if LeRS < DRS then LeRS:=LeRS+1;
If reg = 'x' then R_S[1]:=R_X
else R_S[1]:=R_Y;
StS.Caption:=IntToStr(LeRS);
FNN:=True;
cRS:=LeRS;
end;
1:
end;



procedure TForm1.SBSxClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then
 begin
  if LeFS <> 0 then
  begin
  if cFS = 0 Then cFS:=LeFS;
  SFunc:=Copy(F_S[LeFS-cFS+1],1,Length(F_S[LeFS-cFS+1]));
  LF.Caption:=SFunc;
  StS.Caption:=IntToStr(cFS);
  FSS:=True;
  cFS:=cFS-1;
  end;
  goto 1;
 end;

if LeRS <> 0 then
begin
if cRS = 0 Then cRS:=LeRS;
If reg = 'x' then R_X:=R_S[LeRS-cRS+1]
else R_Y:=R_S[LeRS-cRS+1];
OutVar(R_S[LeRS-cRS+1]);
StS.Caption:=IntToStr(cRS);
if COP <> 'NOP' then FNN:=False
else FNN:=True;
FRS:=True;
cRS:=cRS-1;
end;
end;
1:
end;


procedure TForm1.SBb1Click(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction('('); goto 1; end;
if Br < 99 then
begin
Br:=Br+1;
BN[Br]:=R_X;
BC[Br]:=Copy(COP,1,5);
FNN:=True;
COP:='NOP';
reg:='x';
StV.Caption:=' 0.';
StBr.Caption:='('+IntToStr(Br);
R_X:=0;
end;
end;
1:
end;


procedure TForm1.SBb2Click(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then begin AddFunction(')'); goto 1; end;
if Br > 0 then
begin
Sbeq.Click;
FNN:=True;
FEO:=True;
R_Y:=R_X;
R_X:=BN[Br];
COP:=BC[Br];
Br:=Br-1;
if Br <>0 then StBr.Caption:='('+IntToStr(Br)
else StBr.Caption:='   ';
if COP = 'NOP' then OutVar(R_X);
end;
end;
1:
end;


procedure TForm1.SBdrgClick(Sender: TObject);
begin
if (FER = False) and (FTF = False) then
begin
If GRD = 'deg' then GRD:='rad'
else
If GRD = 'rad' then GRD:='grad'
else GRD:='deg';
StDRG.Caption:=GRD;
end;
end;


procedure TForm1.SBsigClick(Sender: TObject);
begin
if (FER = False) {and (FFF = '  ')} then
begin
if Sig = 's' then Sig:= ' '
else
Sig:= 's';
Stsig.Caption:=Sig;
end;
end;

procedure TForm1.SBxyClick(Sender: TObject);
label 1;
begin
if (FER = False) then
begin
if FH = ' ' then
begin
if FFF = 'EF' then begin AddFunction('^'); FH:=' '; StF.Caption:=FH; goto 1; end;

If ((COP <> 'NOP') and (FNN = False)) or (FEO = True) then
begin
 Sbeq.Click;
 COP:='NOP';
end;
R_Y:=R_X;
NewNum;
FNN:=True;
Reg:='y';
COP:='xy';
end

else
if FH = 'F' then
begin
if FFF = 'EF' then goto 1;

If ((COP <> 'NOP') and (FNN = False)) or (FEO = True) then
begin
 Sbeq.Click;
 COP:='NOP';
end;
R_Y:=R_X;
NewNum;
FNN:=True;
Reg:='y';
COP:='xRy';
FH:=' ';
StF.Caption:=FH;
end

else
begin
if FFF = 'EF' then begin AddFunction('arcsh()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=sh(R_X);
OutVar(R_X);
end
else
begin
R_Y:=sh(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;



procedure TForm1.ERROR;
begin
NewNum;
R_X:=0;
R_Y:=0;
COP:='NOP';
FER:=True;
StV.Caption:='ERROR';
end;

procedure TForm1.XtoY;
begin
try
R_X:=Power(R_X,R_Y);
except
ERROR;
end;
end;

procedure TForm1.XRootY;
begin
try
R_X:=Power(R_X,1/R_Y);
except
ERROR;
end;
end;

procedure TForm1.SBsqrClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FH = ' ' then
begin
if FFF = 'EF' then goto 1;

if reg = 'x' then
begin
R_X:=Qroot(R_X);
OutVar(R_X);
end
else
begin
R_Y:=Qroot(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

else
if FH = 'F' then
begin
if FFF = 'EF' then goto 1;

if reg = 'x' then
begin
R_X:=Quad(R_X);
OutVar(R_X);
end
else
begin
R_Y:=Quad(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

else
begin
if FFF = 'EF' then begin AddFunction('arcch()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=ch(R_X);
OutVar(R_X);
end
else
begin
R_Y:=ch(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;

function TForm1.Qroot(x: TFloat):TFloat;
begin
try
Qroot:=sqrt(x);
except
ERROR;
end;
end;

function TForm1.Quad(x: TFloat):TFloat;
begin
try
Quad:=sqr(x);
except
ERROR;
end;
end;


function TForm1.arcch(x: TFloat):TFloat;
begin
try
arcch:=LogN(exp(1),x+sqrt(sqr(x)-1));
except
ERROR;
end;
end;



procedure TForm1.SBCmnClick(Sender: TObject);
label 1;
begin

if (FER = False)  then
begin
if  (FTF = False) and  (FFF = 'EF') then begin AddFunction('!'); goto 1; end;


if FH = ' ' then
begin
if FFF = 'EF' then goto 1;

If ((COP <> 'NOP') and (FNN = False)) or (FEO = True) then
begin
 Sbeq.Click;
 COP:='NOP';
end;
R_Y:=R_X;
NewNum;
FNN:=True;
Reg:='y';
COP:='Cnm';
end

else
if FH = 'F' then
begin
if FFF = 'EF' then begin AddFunction('()!'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=factor(R_X);
OutVar(R_X);
end
else
begin
R_Y:=factor(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

else
begin
if reg = 'x' then
begin
if FFF = 'EF' then begin AddFunction('arccth()'); FH:=' '; StF.Caption:=FH; goto 1; end;

R_X:=cth(R_X);
OutVar(R_X);
end
else
begin
R_Y:=cth(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;




procedure TForm1.Cnm;
label 1;
var
fn, fm, fnm: TFloat;
i: Integer;
begin
If (R_Y > R_X) or (Trunc(R_X) <> R_X) or (Trunc(R_Y) <> R_Y) or (R_X < 0) or (R_Y < 0) then
begin
ERROR;
goto 1;
end;
if R_X = 0 then R_X:=1;
if R_Y = 0 then R_Y:=1;
fn:=1; fm:=1; fnm:=1;
try
for i:=1 to Trunc(R_X) do
begin
fn:=fn*i;
end;

for i:=1 to Trunc(R_Y) do
begin
fm:=fm*i;
end;

for i:=1 to Trunc(R_X-R_Y) do
begin
fnm:=fnm*i;
end;
R_X:=fn/(fm*fnm);
except
ERROR;
end;
1:
end;

function TForm1.Factor(X: TFloat): TFloat;
var
i: Integer;
fac: TFloat;
begin
try
If (Trunc(X) = X) and (x>=0)  then
begin
if X =0 then X:=1;
Fac:=1;
for i:=1 to Trunc(X) do
begin
Fac:=Fac*i;
end;
Factor:=fac;
end
else
ERROR;
except
ERROR;
end;
end;

function TForm1.arccth(x:TFloat): TFloat;
begin
try
arccth:=0.5*logn(exp(1),(1+x)/(x-1));
except
ERROR;
end;
end;

function TForm1.DToR(x: TFloat):TFloat;
begin
DToR:=x*Pi/180;
end;

function TForm1.GToR(x: TFloat): TFloat;
begin
GToR:=x*Pi/200;
end;


function TForm1.RToD(x: TFloat): TFloat;
begin
RToD:=x*180/Pi;
end;

function TForm1.RToG(x: TFloat): TFloat;
begin
RToG:=x*200/Pi;
end;


procedure TForm1.SBcosClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FH = ' ' then
begin
if FFF = 'EF' then begin AddFunction('cos()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=cosinus(R_X);
OutVar(R_X);
end
else
begin
R_Y:=cosinus(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

else
if FH = 'F' then
begin
if FFF = 'EF' then begin AddFunction('arccos()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=ArcCosinus(R_X);
OutVar(R_X);
end
else
begin
R_Y:=ArcCosinus(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

else
begin
if FFF = 'EF' then begin AddFunction('ch()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=arcch(R_X);
OutVar(R_X);
end
else
begin
R_Y:=arcch(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;


function TForm1.Cosinus(x:TFloat):TFloat;
begin
if GRD = 'deg' then
begin
Cosinus:=cos(DToR(x));
if abs(cos(DToR(x))) < 1e-18 then Cosinus:=0;
end
else
begin
if GRD = 'grad' then
begin
Cosinus:=cos(GToR(x));
if abs(cos(GToR(x))) < 1e-18 then Cosinus:=0;
end
else
begin
Cosinus:=cos(x);
if abs(cos(x)) < 1e-18 then Cosinus:=0;
end;
end;
end;

function TForm1.ch(x: TFloat):TFloat;
begin
ch:=(exp(x)+exp(-x))/2;
end;


function TForm1.ArcCosinus(x:TFloat):TFloat;
begin
if GRD = 'deg' then
begin
try
ArcCosinus:=RToD(Arccos(x));
if abs(Arccos(x)) < 1e-19 then ArcCosinus:=0;
except
ERROR;
end;
end
else
begin
if GRD = 'grad' then
begin
try
ArcCosinus:=RToG(Arccos(x));
if abs(Arccos(x)) < 1e-19 then ArcCosinus:=0;
except
ERROR;
end;
end
else
begin
try
ArcCosinus:=arccos(x);
if abs(arccos(x)) < 1e-19 then ArcCosinus:=0;
except
ERROR;
end;
end;
end;
end;


procedure TForm1.SBtgClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin

if FH = ' ' then
begin
if FFF = 'EF' then begin AddFunction('tg()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=Tang(R_X);
OutVar(R_X);
end
else
begin
R_Y:=Tang(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

else
if FH = 'F' then
begin
if FFF = 'EF' then begin AddFunction('arctg()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=ArcTang(R_X);
OutVar(R_X);
end
else
begin
R_Y:=ArcTang(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

else
begin
if FFF = 'EF' then begin AddFunction('th()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=arcth(R_X);
OutVar(R_X);
end
else
begin
R_Y:=arcth(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;


function TForm1.Tang(x:TFloat):TFloat;
begin
if GRD = 'deg' then
begin
try
Tang:=Tan(DToR(x));
if abs(Tan(DToR(x))) > 1e18 then ERROR;
except
ERROR;
end;
end
else
begin
if GRD = 'grad' then
begin
try
Tang:=Tan(GToR(x));
if abs(Tan(GToR(x))) > 1e18 then ERROR;
except
ERROR;
end;
end
else
begin
try
Tang:=Tan(x);
if abs(Tan(x)) > 1e18 then ERROR;
except
ERROR;
end;
end;
end;
end;


function TForm1.ArcTang(x:TFloat):TFloat;
begin
if GRD = 'deg' then
begin
try
ArcTang:=RToD(ArcTan(x));
except
ERROR;
end;
end
else
begin
if GRD = 'grad' then
begin
try
ArcTang:=RToG(ArcTan(x));
except
ERROR;
end;
end
else
begin
try
ArcTang:=arctan(x);
except
ERROR;
end;
end;
end;
end;

function TForm1.th(x: TFloat):TFloat;
begin
th:=(exp(x)-exp(-x))/(exp(x)+exp(-x))
end;

procedure TForm1.SBctgClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FH = ' ' then
begin
if FFF = 'EF' then begin AddFunction('ctg()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=CTang(R_X);
OutVar(R_X);
end
else
begin
R_Y:=CTang(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

else
if FH = 'F' then
begin
if FFF = 'EF' then begin AddFunction('arcctg()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=ArcCTang(R_X);
OutVar(R_X);
end
else
begin
R_Y:=ArcCTang(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

else
begin
if FFF = 'EF' then begin AddFunction('cth()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=arccth(R_X);
OutVar(R_X);
end
else
begin
R_Y:=arccth(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;


function TForm1.CTang(x:TFloat):TFloat;
begin
if GRD = 'deg' then
begin
try
CTang:=coTan(DToR(x));
if abs(coTan(DToR(x))) > 1e18 then ERROR;
except
ERROR;
end;
end
else
begin
if GRD = 'grad' then
begin
try
CTang:=coTan(GToR(x));
if abs(coTan(GToR(x))) > 1e18 then ERROR;
except
ERROR;
end;
end
else
begin
try
CTang:=coTan(x);
if abs(coTan(x)) > 1e18 then ERROR;
except
ERROR;
end;
end;
end;
end;


function TForm1.ArcCTang(x:TFloat):TFloat;
begin
if GRD = 'deg' then
begin
try
ArcCTang:=RToD(Pi/2-ArcTan(x));
except
ERROR;
end;
end
else
begin
if GRD = 'grad' then
begin
try
ArcCTang:=RToG(Pi/2-ArcTan(x));
except
ERROR;
end;
end
else
begin
try
ArcCTang:=Pi/2-arctan(x);
except
ERROR;
end;
end;
end;
end;


function TForm1.cth(x: TFloat):TFloat;
begin
try
cth:=(exp(x)+exp(-x))/(exp(x)-exp(-x));
except
ERROR;
end;
end;

procedure TForm1.SBsinClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if sig = 's' then
begin
if FFF = 'EF' then goto 1;

if reg = 'x' then
 begin
 R_X:=aver;
 OutVar(R_X);
 end
else
 begin
 R_Y:=aver;
 OutVar(R_Y);
 end;
end
else

begin
if FH = ' ' then
begin
if FFF = 'EF' then begin AddFunction('sin()'); FH:=' '; StF.Caption:=FH; goto 1; end;
if reg = 'x' then
begin
R_X:=sinus(R_X);
OutVar(R_X);
end
else
begin
R_Y:=sinus(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

else
if FH = 'F' then
begin
if FFF = 'EF' then begin AddFunction('arcsin()'); FH:=' '; StF.Caption:=FH; goto 1; end;
if reg = 'x' then
begin
R_X:=Arcsinus(R_X);
OutVar(R_X);
end
else
begin
R_Y:=Arcsinus(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

else
begin
if reg = 'x' then
begin
if FFF = 'EF' then begin AddFunction('sh()'); FH:=' '; StF.Caption:=FH; goto 1; end;
R_X:=arcsh(R_X);
OutVar(R_X);
end
else
begin
R_Y:=arcsh(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end;
end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;



function TForm1.Sinus(x:TFloat):TFloat;
begin
if GRD = 'deg' then
begin
Sinus:=sin(DToR(x));
if abs(sin(DToR(x))) < 1e-18 then Sinus:=0;
end
else
begin
if GRD = 'grad' then
begin
Sinus:=sin(GToR(x));
if abs(sin(GToR(x))) < 1e-18 then Sinus:=0;
end
else
begin
Sinus:=sin(x);
if abs(sin(x)) < 1e-18 then Sinus:=0;
end;
end;
end;


function TForm1.ArcSinus(x:TFloat):TFloat;
begin
if GRD = 'deg' then
begin
try
ArcSinus:=RToD(Arcsin(x));
if abs(Arcsin(x)) < 1e-19 then ArcSinus:=0;
except
ERROR;
end;
end
else
begin
if GRD = 'grad' then
begin
try
ArcSinus:=RToG(Arcsin(x));
if abs(Arcsin(x)) < 1e-19 then ArcSinus:=0;
except
ERROR;
end;
end
else
begin
try
ArcSinus:=arcsin(x);
if abs(arcsin(x)) < 1e-19 then ArcSinus:=0;
except
ERROR;
end;
end;
end;
end;



function TForm1.sh(x: TFloat):TFloat;
begin
sh:=(exp(x)-exp(-x))/2;
end;


function TForm1.Aver:TFloat;
var
i: Integer;
Ssum: TFloat;
begin
try
Ssum:=0;
for i:=1 to LeRS do
begin
Ssum:=Ssum+R_S[i];
end;
Aver:=Ssum/LeRS;
except
ERROR;
end;
end;

procedure TForm1.SBlnClick(Sender: TObject);
label 1;
begin
if (FER = False) then
begin
if sig = 's' then
begin
if FFF = 'EF' then goto 1;

if reg = 'x' then
 begin
 R_X:=Qsum;
 OutVar(R_X);
 end
else
 begin
 R_Y:=Qsum;
 OutVar(R_Y);
 end;
end
else

begin
if FH = ' ' then
begin
if FFF = 'EF' then begin AddFunction('ln()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=Lne(R_X);
OutVar(R_X);
end
else
begin
R_Y:=Lne(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

else
if FH = 'F' then
begin
if FFF = 'EF' then begin AddFunction('exp()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=exp(R_X);
OutVar(R_X);
end
else
begin
R_Y:=exp(R_Y);
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;

function TForm1.Qsum:TFloat;
var
i: Integer;
Ssum: TFloat;
begin
Ssum:=0;
for i:=1 to LeRS do
begin
Ssum:=Ssum+sqr(R_S[i]);
end;
Qsum:=Ssum;
end;


function TForm1.Lne(x:TFloat):TFloat;
begin
try
Lne:=LogN(exp(1),x);
except
ERROR;
end;
end;


procedure TForm1.SB1xClick(Sender: TObject);
label 1;
begin
if (FER = False) then
begin
if sig = 's' then
begin
if FFF = 'EF' then goto 1;

if reg = 'x' then
 begin
 R_X:=SigN;
 OutVar(R_X);
 end
else
 begin
 R_Y:=SigN;
 OutVar(R_Y);
 end;
end
else

begin
if FH = ' ' then
begin
if FFF = 'EF' then goto 1;

if reg = 'x' then
begin
try
R_X:=1/R_X;
OutVar(R_X);
except
ERROR;
end;
end
else
begin
try
R_Y:=1/R_Y;
OutVar(R_Y);
except
ERROR;
end;
end;
FH:=' ';
StF.Caption:=FH;
end

else
if FH = 'F' then
begin
if FFF = 'EF' then begin AddFunction('pi'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
R_X:=Con_Pi;
OutVar(R_X);
end
else
begin
R_Y:=Con_Pi;
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end

end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;


function TForm1.SigN:TFloat;
var
i: Integer;
Ssum: TFloat;
begin
try
Ssum:=0;
for i:=1 to LeRS do
begin
Ssum:=SSum+sqr(R_S[i]-Aver);
end;
SigN:=sqrt(Ssum/LeRS);
except
ERROR;
end;
end;

procedure TForm1.SBlogxyClick(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if sig = 's' then
begin
if FFF = 'EF' then goto 1;

if reg = 'x' then
 begin
 R_X:=SumS;
 OutVar(R_X);
 end
else
 begin
 R_Y:=SumS;
 OutVar(R_Y);
 end;
end
else

if  FH = ' ' then
begin
if FFF = 'EF' then begin AddFunction('log(,)'); FH:=' '; StF.Caption:=FH; goto 1; end;


If ((COP <> 'NOP') and (FNN = False)) or (FEO = True) then
begin
 Sbeq.Click;
 COP:='NOP';
end;
R_Y:=R_X;
NewNum;
FNN:=True;
Reg:='y';
COP:='logxy';
FH:= ' ';
StF.Caption:=FH;
end

else
if FH = 'F' then
begin
if FFF = 'EF' then begin AddFunction('lg()'); FH:=' '; StF.Caption:=FH; goto 1; end;

if reg = 'x' then
begin
try
R_X:=Log10(R_X);
OutVar(R_X);
except
ERROR;
end;
end
else
begin
try
R_Y:=Log10(R_Y);
OutVar(R_Y);
except
ERROR;
end;
end;
FH:=' ';
StF.Caption:=FH;
end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;


function TForm1.SigN1:TFloat;
var
i: Integer;
Ssum: TFloat;
begin
try
Ssum:=0;
for i:=1 to LeRS do
begin
Ssum:=SSum+sqr(R_S[i]-Aver);
end;
SigN1:=sqrt(Ssum/(LeRS-1));
except
ERROR;
end;
end;


procedure TForm1.logxy;
begin
try
R_X:=LogN(R_X,R_Y);
except
ERROR;
end;
end;

function TForm1.SumS:TFloat;
var
i: Integer;
Ssum: TFloat;
begin
Ssum:=0;
for i:=1 to LeRS do
begin
Ssum:=SSum+R_S[i];
end;
SumS:=Ssum;
end;


procedure TForm1.Eq2;
var
D: TFloat;
begin
try
If R_A = 0 then R_A:=1;
D:=sqrt(sqr(R_B)-4*R_A*R_C);
R_X1:=(-R_B+D)/(2*R_A);
R_X2:=(-R_B-D)/(2*R_A);
StV.Caption:=' 0.';
except
ERROR;
end;
end;

procedure TForm1.SBeq2Click(Sender: TObject);
label 1;
begin
if (FER = False)  then
begin
if FFF = 'EF' then goto 1;

if sig = 's' then
begin
if reg = 'x' then
 begin
 R_X:=SigN1;
 OutVar(R_X);
 end
else
 begin
 R_Y:=SigN1;
 OutVar(R_Y);
 end;
end
else

begin
if FH = ' ' then
begin
eq2;
FH:=' ';
StF.Caption:=FH;
end

else
if FH = 'F' then
begin
if reg = 'x' then
begin
R_X:=QR12;
OutVar(R_X);
end
else
begin
R_Y:=QR12;
OutVar(R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end;
NewNum;
FNN:=True;
FEO:=True;
end;
end;
1:
end;

function TForm1.QR12: TFloat;
begin
If RX12 = '1' then
begin
RX12:='2'; QR12:=R_X1;
end

else
begin
RX12:='1'; QR12:=R_X2;
end;
end;

function TForm1.arcth(x:TFloat): TFloat;
begin
try
arcth:=0.5*logn(exp(1),(1+x)/(1-x));
except
ERROR;
end;
end;

function TForm1.DMS(x: TFloat): TFloat;
begin
if smd = 'd' then
begin
smd:='m'; DMS:=x;  Sdms:=x;
end
else
if smd = 'm' then
begin
smd:='s'; DMS:=Sdms+x/60;   Sdms:=Sdms+x/60;;
end
else
begin
smd:='d';DMS:=Sdms+x/3600;
end;



end;

procedure TForm1.T1Timer(Sender: TObject);
var
QH:String[1];
begin
StTime.Caption:=':';
QH:=Copy(TimeToStr(Time),2,1);
If QH = ':' then
begin
Hrs:=Copy(TimeToStr(Time),1,1);
Insert('0',Hrs,1);
Mns:=Copy(TimeToStr(Time),3,2);
end
else
begin
Hrs:=Copy(TimeToStr(Time),1,2);
Mns:=Copy(TimeToStr(Time),4,2);
end;
StHrs.Caption:=Hrs;
StMns.Caption:=Mns;
T2.Enabled:=True;
end;

procedure TForm1.SBSxSClick(Sender: TObject);
label 1;
var
xch: TFloat;
Sxch: String;
begin
if (FER = False)  then
begin
if FFF = 'EF' then
 begin
  if FSS = True then
  begin
   Sxch:=F_S[LeFS-cFS];
   F_S[LeFS-cFS]:=SFunc;
   SFunc:=Copy(Sxch,1,Length(Sxch));
   LF.Caption:=SFunc;
   FSS:=False;
   end;
  goto 1;
 end;

if FRS = True then
begin
xch:=R_S[LeRS-cRS];
If reg = 'x' then R_S[LeRS-cRS]:=R_X
else R_S[LeRS-cRS]:=R_Y;
If reg = 'x' then R_X:=xch
else R_Y:=xch;
OutVar(xch);
FNN:=True;
FRS:=False;
end;
end;
1:
end;



procedure TForm1.T2Timer(Sender: TObject);
begin
StTime.Caption:=' ';
T2.Enabled:=False;
end;


procedure TForm1.SBFuncClick(Sender: TObject);
var
EC: Cardinal;
begin
if FFF = '  ' then
begin
 FFF:='EF';
 STFunc.Font.Color:=RGB(175,175,255);
 STFunc.Caption:=FFF;
 StFx.Caption:='f(x)';
 LF.Caption:=SFunc;
 if LeFS <> 0 then
 StS.Caption:=IntToStr(LeFS) else StS.Caption:='  ';
end
{else
if FFF = 'EF' then
begin
 FFF:='CF';
 STFunc.Font.Color:=clAqua;
 STFunc.Caption:=FFF;
 Stx.Caption:='x';
 FortL.Expression:=SFunc;
 LF.Caption:=Copy(SFunc,1,Length(SFunc));
 if LeRS <> 0 then
 StS.Caption:=IntToStr(LeRS) else StS.Caption:='  ';
 if FortL.SyntaxError <> 0 then
 begin
 FER:=True; StV.Caption:='TRANSLATION ERROR';
 FFF:='  ';
 SBFunc.Click;
 end
end}
else
begin
 LF.Caption:=Copy(SFunc,1,Length(SFunc));
 FFF:='  ';
 STFunc.Caption:=FFF;
 cFP:=Length(SFunc);
 if LeRS <> 0 then
 StS.Caption:=IntToStr(LeRS) else StS.Caption:='  ';
 //CreateFunc;
 SetFunction;
 if SFunc = '' then StFx.Caption:='    ';
 flGetErrorCode(EC);
 if EC <> 0 then
 begin
 FER:=True; StV.Caption:='TRANSLATION ERROR';
 FFF:='  ';
 SBFunc.Click;
 end;

 //STFunc.Font.Color:=clBlack;

 //StFx.Caption:='    ';
 //Stx.Caption:=' ';
 //LF.Caption:='';

end;



end;




procedure TForm1.AddFunction(S: String);
begin
Insert(S,SFunc,cFP+1);
cFP:=cFP+Length(S);
LF.Caption:=Copy(SFunc,1,cFP);
end;




procedure TForm1.ViewerL;
begin
if cFP > 1 then
begin
 dec(cFP);
 LF.Caption:=Copy(SFunc,1,cFP);
end;
end;




procedure TForm1.ViewerR;
begin
if cFP < Length(SFunc) then
begin
 inc(cFP);
 LF.Caption:=Copy(SFunc,1,cFP);
end;
end;



procedure TForm1.DelFuncSymbol;
begin
if cFP > 0 then
begin
Delete(SFunc,cFP,1);
dec(cFP);
LF.Caption:=Copy(SFunc,1,cFP);
end;
end;




procedure TForm1.SBeqMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 If Button = mbRight then SetFH;
end;



procedure TForm1.BeginFort(FCOP: String; reg: String; X: TFloat = 0);
begin
Fort:=TThFortFunc.Create(FCOP,reg,X);
Fort.FreeOnTerminate:= True;
Fort.Priority:= tpLower;
Fort.Resume;
FTF:=True;
//Stv.Font.Color:=clBlack;
StV.Caption:='CALCULATION'
end;


procedure TForm1.BeginFlash;
begin
Flash:=TThFlash.Create;
Flash.FreeOnTerminate:= True;
Flash.Priority:= tpLower;
Flash.Resume;
end;


(*procedure TForm1.CreateFunc;
begin
{FortL.A:=R_A;  FortL.B:=R_B;  FortL.C:=R_C;
FortL.D:=R_D;  FortL.E:=R_E;
FortL.X:=R_X;
FortL.Expression:=SFunc; }
end;
*)



(*
procedure TForm1.SBddxClick(Sender: TObject);
label 1;
begin
if (FER = False) and (FTF = False) then
begin
if FFF = 'EF' then begin AddFunction('y'); goto 1; end;

if FH = ' ' then
begin

if reg = 'x' then
begin
{SetFunction;
R_X:=Diff1(R_X);
//OutVar(R_X);}
V_X:=R_X;
SetFunction;
MathProcA.Derivative1(TAddress(Form1.C_Func),@Form1.R_X,Form1.V_X); //_Root;
BeginFort('DERIVATIVE1',reg);
FH:=' ';
StF.Caption:=FH;

end
else
begin
{SetFunction;
R_Y:=Diff1(R_Y);
OutVar(R_Y);}
V_X:=R_X;
SetFunction;
BeginFort('DERIVATIVE1',reg);
FH:=' ';
StF.Caption:=FH;
end;
FH:=' ';
StF.Caption:=FH;
end

else
if FH = 'F' then
begin

if reg = 'x' then
begin
//CreateFunc;
SetFunction;
BeginFort('ROOT',reg,R_X);
end
else
begin
//CreateFunc;
SetFunction;
BeginFort('ROOT',reg,R_Y);
end;
FH:=' ';
StF.Caption:=FH;
end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;
*)



procedure TForm1.SBddxClick(Sender: TObject);
label 1;
begin
if (FER = False) and (FTF = False) then
begin
if FFF = 'EF' then begin AddFunction('y'); goto 1; end;

if FH = ' ' then
begin
//MathProcA.SetMode(ml_Derivative_Currency,F_E);
//R_X:=MathProcA.Derivative1(TAddress(C_Func),@R_X,R_X);
SetFunction;
BeginFort('DERIVATIVE1',reg);
OutVar(R_X);
FH:=' ';
StF.Caption:=FH;
end


else
if FH = 'F' then
begin
SetFunction;
BeginFort('ROOT1',reg);
FH:=' ';
StF.Caption:=FH;
OutVar(R_X);
end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;




function TForm1.Diff1(x: TFloat): TFloat;
var
x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,
h,dy5: TFloat;
begin
h:=0.01;
x0:=x-5*h; x1:=x0+h; x2:=x1+h;  x3:=x2+h; x4:=x3+h; x5:=x4+h; x6:=x5+h;
x7:=x6+h; x8:=x7+h; x9:=x8+h; x10:=x9+h;
R_X:=x0;  y0:=Calc(C_Func);
R_X:=x1;  y1:=Calc(C_Func);
R_X:=x2;  y2:=Calc(C_Func);
R_X:=x3;  y3:=Calc(C_Func);
R_X:=x4;  y4:=Calc(C_Func);
R_X:=x5;  y5:=Calc(C_Func);
R_X:=x6;  y6:=Calc(C_Func);
R_X:=x7;  y7:=Calc(C_Func);
R_X:=x8;  y8:=Calc(C_Func);
R_X:=x9;  y9:=Calc(C_Func);
R_X:=x10; y10:=Calc(C_Func);


dy5:=1/(4536*h)*(-3.6*y0+45*y1-270*y2+1080*y3-3780*y4+3780*y6-1080*y7+270*y8-45*y9+3.6*y10);
Diff1:=dy5;
end;




procedure TForm1.SBDClick(Sender: TObject);
label 1;
begin
 If (FER = False)  then
 begin
 if FFF = 'EF' then begin AddFunction('D'); goto 1; end;

  if FH = ' ' then
  begin

   if (COP <> 'NOP') and (FNN = True) Then
   begin
   If reg = 'x' then R_X:=R_D
   else R_Y:=R_D;
   OutVar(R_D);
   NewNum;
   FNN:=True;
   FEO:=True;
   end

   else
    begin
    If reg = 'x' then R_D:=R_X
    else R_D:=R_Y;
    If R_D = 0 then StD.Caption:=' '
    else StD.Caption:='D';
    NewNum;
    FNN:=True;
    end;
   goto 1;
  end;

  if FH = 'F' then
  begin
  If reg = 'x' then F_D:=R_X
    else F_D:=R_Y;
  FH:=' ';
  StF.Caption:=FH;
  NewNum;
  FNN:=True;
  BeginFlash;
  end;

 end;
1:
end;



procedure TForm1.SBEClick(Sender: TObject);
label 1;
begin
 If (FER = False)  then
 begin
 if FFF = 'EF' then begin AddFunction('E'); goto 1; end;

  if FH = ' ' then
  begin

   if (COP <> 'NOP') and (FNN = True) Then
   begin
   If reg = 'x' then R_X:=R_E
   else R_Y:=R_E;
   OutVar(R_E);
   NewNum;
   FNN:=True;
   FEO:=True;
   end

   else
    begin
    If reg = 'x' then R_E:=R_X
    else R_E:=R_Y;
    If R_E = 0 then StE.Caption:=' '
    else StE.Caption:='E';
    NewNum;
    FNN:=True;
    end;
   goto 1;
  end;

  if FH = 'F' then
  begin
  If reg = 'x' then F_E:=R_X
    else F_E:=R_Y;
  FH:=' ';
  StF.Caption:=FH;
  NewNum;
  FNN:=True;
  BeginFlash;
  end;

 end;
1:
end;



procedure TForm1.SBAMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 If Button = mbRight then SetFH;
end;

procedure TForm1.SBBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 If Button = mbRight then SetFH;
end;

procedure TForm1.SBCMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 If Button = mbRight then SetFH;
end;

procedure TForm1.SBDMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 If Button = mbRight then SetFH;
end;

procedure TForm1.SBEMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 If Button = mbRight then SetFH;
end;

procedure TForm1.SBddxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 If Button = mbRight then SetFH;
end;



procedure TForm1.SBis1Click(Sender: TObject);
label 1;
begin
if (FER = False) and (FTF = False) then
begin
if FFF = 'EF' then begin AddFunction('n'); goto 1; end;

if FH = ' ' then
begin
SetFunction;
BeginFort('INTEGRAL1',reg);
FH:=' ';
StF.Caption:=FH;
OutVar(R_X);
end


else
if FH = 'F' then
begin
SetFunction;
BeginFort('SUM1',reg);
FH:=' ';
StF.Caption:=FH;
OutVar(R_X);
end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;








procedure TForm1.SBis1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 If Button = mbRight then SetFH;
end;



procedure TForm1.Delay(D: LongInt);
var
T1,T2: LongInt;
begin
T1:=D+GetTickCount;
T2:=GetTickCount;
while T2 < T1 do
begin
T2:=GetTickCount;
end;
end;









procedure TForm1.SBis2Click(Sender: TObject);
label 1;
begin
if (FER = False) and (FTF = False) then
begin
if FFF = 'EF' then begin AddFunction('k'); goto 1; end;

if FH = ' ' then
begin
SetFunction;
BeginFort('INTEGRAL2',reg);
FH:=' ';
StF.Caption:=FH;
OutVar(R_X);
end


else
if FH = 'F' then
begin
//CreateFunc;
SetFunction;
BeginFort('SUM2',reg);
FH:=' ';
StF.Caption:=FH;
OutVar(R_X);
end;
NewNum;
FNN:=True;
FEO:=True;
end;
1:
end;











procedure TForm1.SBis2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 If Button = mbRight then SetFH;
end;

end.
