program ampaside;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} //{$IFDEF UseCThreads}
  cthreads, {$ENDIF} //{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  lazcontrols,
  //--------------
  uAboutForm,
  uAMPASCore,
  uCodeEditor,
  uCodeEditorDM,
  uFileManagerFrame,
  uIDEConfig,
  uMainForm,
  uNewProjForm,
  uProjectOptionsFrame,
  uProjectBuilding,
  uProjectConfig,
  uProjectManager,
  uEditorOptionsFrame,
  uIDEOptionsForm,
  uGeneralOptionsFrame,
  uEditorConfig,
  uIDEDirectivesForm,
  uFileManager;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

