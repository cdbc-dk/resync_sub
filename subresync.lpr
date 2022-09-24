program subresync;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  umain;


{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='Subtitle Resync';
  Application.Initialize;
  Application.CreateForm(TfrmMain,frmMain);
  Application.Run;
end.

