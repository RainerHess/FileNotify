program FileNotifyTest;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  crc32 in 'crc32.pas';

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  ReportMemoryLeaksOnShutdown := TRUE;

  Application.Initialize;
  Application.Title := 'DirMonitorTest';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
