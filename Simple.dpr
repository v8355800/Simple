program Simple;

uses
  Forms,
  uMain in 'uMain.pas' {Form1},
  uIntegral in 'uIntegral.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
