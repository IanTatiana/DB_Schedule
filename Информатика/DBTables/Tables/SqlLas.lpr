program SqlLas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, main, UTables, UFilter, UTableForm, UTrackBarPaint,
  UEditForm, USchedule, UMyDBTools, UEditCardForm, UExport, UConflictsForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TConflictForm, ConflictsForm);
  Application.Run;
end.

