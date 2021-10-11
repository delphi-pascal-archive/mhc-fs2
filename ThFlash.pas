unit ThFlash;

interface

uses
  Classes, Graphics;

type
  TThFlash = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public  
    constructor Create;
  end;

implementation
uses
UMHCFS;


{ ThFlash }

procedure TThFlash.Execute;
begin
 UMHCFS.Form1.StV.Font.Color:=clBlack;
 UMHCFS.Form1.Delay(50);
 UMHCFS.Form1.StV.Font.Color:=clAqua;
end;


constructor TThFlash.Create;
begin
inherited Create(False);
end;


end.
