program TestWatch;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  WatchFileSystem in 'WatchFileSystem.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
