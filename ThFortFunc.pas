unit ThFortFunc;

interface

uses
  Classes, Math,  Graphics, OpenML;

type TFloat = extended;
type TIntegType = Integer;

TFunction = function(x: TFloat): TFloat;

type
  TThFortFunc = class(TThread)
  private
    Result: TFloat;
  protected
    procedure Execute; override;

  public
    constructor Create(FCOP:String; reg: String; X: TFloat);
    //function  _Root: TFloat;
    //function  _Sum1: TFloat;
    //function  _Sum2: TFloat;
  end;

var
 F_COP, I_reg: String;
 //Func: TFunction;
 I_X: TFloat;
 Error: Boolean;
 an,bn: array [1..15] of TFloat;
 N_Integral: Integer;
 MathProc: TMathLib;




implementation
uses UMHCFS;





constructor TThFortFunc.Create(FCOP:String; reg: String; X: TFloat);
begin
inherited Create(False);
MathProc:=TMathLib.Create;
Result:=0;
F_COP:=FCOP;
//Func:=FortFunc1;
I_X:=X;
I_reg:=Copy(reg,1,1);
Error:=False;
//Init;
end;



procedure TThFortFunc.Execute;
var
E: Integer;
begin
 E:=0;  I_X:=Form1.R_X;
 MathProc.SetMode(ml_Integral_Currency,Form1.F_E);
 MathProc.SetMode(ml_Derivative_Currency,Form1.F_E);
 try
 if F_COP = 'DERIVATIVE1' then Result:=MathProc.Derivative1(TAddress(Form1.C_Func),@Form1.R_X,Form1.R_X);
 if F_COP = 'ROOT1' then Result:=MathProc.Root(TAddress(Form1.C_Func),@Form1.R_X,Form1.R_X,E);// _Root;
 if F_COP = 'INTEGRAL1' then Result:=MathProc.Integral1(TAddress(Form1.C_Func),@Form1.R_X,Form1.F_A,Form1.F_B);  //_Integral1;
 if F_COP = 'SUM1' then Result:=MathProc.Sum1(TAddress(Form1.C_Func),@Form1.V_N,Trunc(Form1.F_A),Trunc(Form1.F_B));//_Sum1;
 if F_COP = 'INTEGRAL2' then Result:=MathProc.Integral2(TAddress(Form1.C_Func),@Form1.R_X,@Form1.R_Y,Form1.F_A,Form1.F_B,Form1.F_C,Form1.F_D); //_Integral2;
 if F_COP = 'SUM2' then Result:=MathProc.Sum2(TAddress(Form1.C_Func),@Form1.V_N,@Form1.V_K,Trunc(Form1.F_A),Trunc(Form1.F_B),Trunc(Form1.F_C),Trunc(Form1.F_D)); //_Integral2;

 except
 Error:=True;
 end;

 if E = 1 then Error:=True;
 if Error = True then
 begin
  Form1.STV.Font.Color:=clAqua; Form1.ERROR;
 end
 else
 begin
  Form1.STV.Font.Color:=clAqua;
  Form1.R_X:=Result;
  Form1.OutVar(Result);
  if I_reg = 'x' then
  begin
   Form1.R_X:=Result; Form1.reg:= 'x';
  end
  else  Form1.R_Y:=Result;
 end;

 Form1.FTF:=False;
end;




end.
