program SerialThreads;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  Serial in 'Serial.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
