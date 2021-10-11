program MhcFS;

uses
  Forms,
  UMHCFS in 'UMHCFS.pas' {Form1},
  ThFortFunc in 'ThFortFunc.pas',
  ThFlash in 'ThFlash.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
