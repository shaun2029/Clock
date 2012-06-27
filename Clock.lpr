//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

program Clock;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, ClockMain, MetOffice, alarm, ClockSettings,
  Reminders, DatePicker, ReminderList, FindThread, music, sync, udpclient,
  udpserver, PlaylistCreator, udpcommandserver;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClockMain, frmClockMain);
  Application.CreateForm(TfrmClockSettings, frmClockSettings);
  Application.CreateForm(TfrmReminders, frmReminders);
  Application.CreateForm(TfrmDatePicker, frmDatePicker);
  Application.CreateForm(TfrmReminderList, frmReminderList);
  Application.CreateForm(TfrmPlaylist, frmPlaylist);
  Application.Run;
end.

