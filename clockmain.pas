//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit ClockMain;

{$mode Delphi}

//{$DEFINE PICSHOW}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, MetOffice, Alarm, ClockSettings, Reminders, ReminderList, LCLProc,
  Music, Sync, Process, MusicPlayer, PlaylistCreator, UDPCommandServer;

const
  VERSION = '1.0.9';

type

  TMusicState = (msOff, msMusicPlaying, msMusicPaused, msSleepPlaying, msSleepPaused);

  { TfrmClockMain }

  TfrmClockMain = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    labLocation: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    labSong: TLabel;
    lblTime: TLabel;
    mmoHTML: TStaticText;
    tmrMinute: TTimer;
    tmrWeather: TTimer;
    tmrTime: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure lblTimeClick(Sender: TObject);
    procedure tmrTimeTimer(Sender: TObject);
    procedure tmrWeatherTimer(Sender: TObject);
    procedure tmrMinuteTimer(Sender: TObject);
  private
    { private declarations }
    FMPGPlayer: TMusicPlayer;
    FMetOffice: TMetOffice;
    FAlarm, FReminderAlarm: TAlarm;
    FCurrentReminders: TReminders;
    FTimer: TAlarm;
    FSyncServer: TSyncServer;
    FSyncClient: TSyncClient;

    Images: array [0..4] of TImage;
    Labels: array [0..9] of TLabel;
    DayLabels: array[0..4] of TLabel;
    WindLabels: array[0..4] of TLabel;
    Locations: array[0..3] of string;
    FCurrentLocation: integer;

    FMusicPlayer, FSleepPlayer: TPlayer;
 	  FMusicState: TMusicState;

    FCOMServer: TCOMServer;
    FWeatherReport: string;

    procedure CloseApp;
    procedure ConfigureWifi;
    function DayOfWeekStr(Date: TDateTime): string;
    procedure PauseMusic;
    procedure Shutdown(Reboot: boolean);
    procedure UpdateSettings;
    procedure UpdateWeather;

    procedure BeforeAlarm;
    procedure AfterAlarm;
  public
    { public declarations }
    HTTPBuffer: string;
  end;

var
  frmClockMain: TfrmClockMain;

implementation

{$R *.lfm}

{ TfrmClockMain }

procedure TfrmClockMain.PauseMusic;
begin
  // Disable music around alarm times and timer times of it is playing
  case FMusicState of
    msMusicPlaying:
      begin
        FMusicPlayer.Stop;
	      FMusicState := msMusicPaused;
	    end;
    msSleepPlaying:
      begin
        FSleepPlayer.Stop;
        FMusicState := msSleepPaused;
	    end;
  end;
end;

procedure TfrmClockMain.BeforeAlarm;
begin
  PauseMusic;

  // Play music after alarm
  if frmClockSettings.cbxPlayMusic.Checked or frmClockSettings.cbxSilentAlarm.Checked then
    FMusicState := msMusicPaused;
end;

procedure TfrmClockMain.AfterAlarm;
begin
  // Possibly start music after alarm
  case FMusicState of
    msMusicPaused:
	    begin
	      FMusicPlayer.Play;
	      FMusicState := msMusicPlaying;
	    end;
	  msSleepPaused:
	    begin
	      FSleepPlayer.Play;
		    FMusicState := msSleepPlaying;
	    end;
  end;
end;

function TfrmClockMain.DayOfWeekStr(Date: TDateTime): string;
var
  DOW: Integer;
begin
  DOW := DayOfWeek(Date);

  case DOW of
    1: Result := 'Sunday';
    2: Result := 'Monday';
    3: Result := 'Tuesday';
    4: Result := 'Wednesday';
    5: Result := 'Thursday';
    6: Result := 'Friday';
    else Result := 'Saturday';
  end;
end;

procedure TfrmClockMain.tmrTimeTimer(Sender: TObject);
var
  H, M, S, MS: word;
  Day, Month, Year: word;
  Current: TDateTime;
  DayStr: string;
  TimeCaption: string;
  ReminderList: TStringList;
  i, j: Integer;
  Command: TRemoteCommand;
  Key: Char;
begin
  Current := Now;

  DecodeTime(Current, H, M, S, MS);
  DecodeDate(Current, Year, Month, Day);
  DayStr := Copy(DayOfWeekStr(Current), 1, 3);

  TimeCaption := Format('%s %.2d/%.2d %.2d:%.2d:%.2d ', [DayStr, Day, Month, H, M, S]);

  if TimeCaption <> lblTime.Caption then
    lblTime.Caption := TimeCaption;

  tmrTime.Tag := tmrTime.Tag + 1;

  if tmrTime.Tag >= 1 then
  begin
    if not frmClockSettings.Visible then
      UpdateSettings;

    tmrWeather.Enabled := True;

    // Disable weather update around alarm times
    if (Current > FAlarm.AlarmTime - EncodeTime(0, 5, 0, 0))
      and (Current < FAlarm.AlarmTime + EncodeTime(0, 5, 0, 0)) then
    begin
      tmrWeather.Enabled := False;
    end;

    // Disable weather when reminder alarm about to be activated
    if (Length(FCurrentReminders) > 0) and (Current > FReminderAlarm.AlarmTime - EncodeTime(0, 2, 0, 0))
      and (Current < FReminderAlarm.AlarmTime + EncodeTime(0, 0, 10, 0)) then
    begin
      tmrWeather.Enabled := False;
    end;

    // If reminder alarm active display reminder and disable weather update
    if (FReminderAlarm.State = asActive) then
    begin
      if (mmoHTML.Font.Color <> clYellow) then
      begin
        mmoHTML.Font.Color := clYellow;
        ReminderList := TStringList.Create;
        frmReminders.SortReminders(FCurrentReminders);
        frmReminders.PopulateList(FCurrentReminders, ReminderList);
        mmoHTML.Caption := ReminderList.Text;
        ReminderList.Free;
      end;

      tmrWeather.Enabled := False;
    end;

    // Do not update weather if reminder is displayed
    if (mmoHTML.Font.Color = clYellow) then
    begin
      tmrWeather.Enabled := False;
    end;

    // Turn off timer
    if Current > FTimer.AlarmTime + EncodeTime(0, 5, 0, 0) then
    begin
      for i := 0 to High(FTimer.Days) do
      begin
        if FTimer.Days[i] <> False then
          FTimer.Days[i] := False;
      end;
    end;

    FAlarm.Tick;
    FReminderAlarm.Tick;
    FTimer.Tick;	

    if Assigned(FMusicPlayer) and Assigned(FSleepPlayer) then
    begin
      i := FSleepPlayer.Tick;
      j := FMusicPlayer.Tick;

      if i >= 0  then
        labSong.Caption := 'Updating sleep music list ... ' + IntToStr(i)
      else if j >= 0 then
        labSong.Caption := 'Updating music list ... ' + IntToStr(j)
      else if FMusicPlayer.State = psPlaying then
        labSong.Caption := FMusicPlayer.SongArtist + ' - ' + FMusicPlayer.SongTitle
      else if FSleepPlayer.State = psPlaying then
          labSong.Caption := FSleepPlayer.SongArtist + ' - ' + FSleepPlayer.SongTitle
      else labSong.Caption :=  'Shaun''s Clock Version: ' + VERSION;
    end;

    tmrTime.Tag := 0;
  end;

  if Assigned(FCOMServer) then
  begin
    FCOMServer.Playing := LabSong.Caption;
    FCOMServer.WeatherReport := FWeatherReport;
    Command := FCOMServer.GetCommand;

    case Command of
      rcomNext:
        begin
          if FMusicState in [msSleepPlaying, msSleepPaused] then
            Key := 's'
          else
            Key := 'm';

          FormKeyPress(Self, Key);
        end;
      rcomMusic:
        begin
          if not (FMusicState in [msMusicPlaying, msMusicPaused]) then
          begin
            Key := 'm';
            FormKeyPress(Self, Key);
          end;
        end;
      rcomSleep:
        begin
          if not (FMusicState in [msSleepPlaying, msSleepPaused]) then
          begin
            Key := 's';
            FormKeyPress(Self, Key);
          end;
        end;
      rcomPause:
        begin
          Key := 'p';

          FormKeyPress(Self, Key);
        end;
      rcomVolumeUp:
        begin
          Key := '.';

          FormKeyPress(Self, Key);
        end;
      rcomVolumeDown:
        begin
          Key := ',';

          FormKeyPress(Self, Key);
        end;
    end;
  end;
end;

procedure TfrmClockMain.tmrWeatherTimer(Sender: TObject);
var
  Forecast: TWeatherReport;
  i: Integer;
begin
  tmrWeather.Enabled := False;

  if frmClockSettings.Visible = False then
    UpdateWeather;

  tmrWeather.Enabled := True;
end;

procedure TfrmClockMain.tmrMinuteTimer(Sender: TObject);
var
  Rems: string;
  CurrentList: TStringList;
begin
  tmrMinute.Enabled := False;

  frmReminders.RefreshReminders;

  if tmrMinute.Tag >= 5 then
  begin
    tmrMinute.Tag := 0;

    if Assigned(FSyncServer) then
      FSyncServer.RemindersFile(GetAppConfigFile(False))
    else if Assigned(FSyncClient) then
    begin
      if FSyncClient.GetReminders(Rems) then
      begin
        CurrentList := TStringList.Create;

        try
          if FileExists(GetAppConfigFile(False)) then
            CurrentList.LoadFromFile(GetAppConfigFile(False));

          if CurrentList.Text <> Rems then
          begin
            CurrentList.Text := Rems;
            CurrentList.SaveToFile(GetAppConfigFile(False));
            frmReminders.ReadReminders;
          end;
        except
        end;

        CurrentList.Free;

        frmReminders.ReadReminders;
      end;
    end;
  end
  else tmrMinute.Tag := tmrMinute.Tag + 1;

  tmrMinute.Enabled := True;
end;

procedure TfrmClockMain.UpdateWeather;
var
  Forecast: TWeatherReport;
  i, j: Integer;
begin
  tmrWeather.Enabled := False;

  mmoHTML.Font.Color := clWhite;

  FWeatherReport := '';

  for i := 0 to 4 do
  begin
    try
      if FMetOffice.GetForecast('http://www.metoffice.gov.uk/mobile/',
        '5dayforecastdetail?forecastid=' + Trim(Locations[FCurrentLocation]),
        i, Forecast, Images) then
      begin
        if i = 0 then
        begin
          labLocation.Caption := Forecast.Title;
          mmoHTML.Caption := Forecast.Report;
        end;

        DayLabels[i].Caption := Forecast.Day;
        Labels[i*2].Caption := IntToStr(Forecast.TempDay) + '°C';
        Labels[(i*2) + 1].Caption := IntToStr(Forecast.TempNight) + '°C';
        WindLabels[i].Caption := IntToStr(Forecast.WindSpeedDay) + 'mph';

        FWeatherReport := FWeatherReport + Format('%s %d°C (%d°C) %dmph' + LineEnding,
          [Forecast.Day, Forecast.TempDay, Forecast.TempNight, Forecast.WindSpeedDay]);
      end
      else
      begin
        mmoHTML.Caption := 'Failed to update weather' + LineEnding + 'Press W to configure Wifi.';
        tmrWeather.Enabled := True;
        Exit;
      end;
    except
      on E: exception do
      begin
        mmoHTML.Caption := 'Exception updating weather' + LineEnding + 'Press W to configure Wifi.';
        DebugLn('Unhandled Exception: Failure during weather update.');
        DebugLn('Exception: ' + E.Message);
        tmrWeather.Enabled := True;
        Exit;
      end;
    end;

    Application.ProcessMessages;
  end;

  FWeatherReport := mmoHTML.Caption + LineEnding + FWeatherReport;
  tmrWeather.Enabled := True;
end;


procedure TfrmClockMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FMetOffice := TMetOffice.Create;

  labSong.Caption := '';

  Labels[0] := Label1;
  Labels[1] := Label2;
  Labels[2] := Label3;
  Labels[3] := Label4;
  Labels[4] := Label5;
  Labels[5] := Label6;
  Labels[6] := Label7;
  Labels[7] := Label8;
  Labels[8] := Label9;
  Labels[9] := Label10;

  DayLabels[0] := Label11;
  DayLabels[1] := Label12;
  DayLabels[2] := Label13;
  DayLabels[3] := Label14;
  DayLabels[4] := Label15;

  WindLabels[0] := Label16;
  WindLabels[1] := Label17;
  WindLabels[2] := Label18;
  WindLabels[3] := Label19;
  WindLabels[4] := Label20;

  Images[0] := Image1;
  Images[1] := Image2;
  Images[2] := Image3;
  Images[3] := Image4;
  Images[4] := Image5;

  for i := 0 to High(Labels) do
    Labels[i].Caption := '';

  for i := 0 to High(DayLabels) do
    DayLabels[i].Caption := '';

  for i := 0 to High(WindLabels) do
    WindLabels[i].Caption := '';

  FMPGPlayer := TMusicPlayer.Create;
  FMPGPlayer.Equalizer := ChangeFileExt(GetAppConfigFile(False), '_eq.cfg');


  FAlarm := TAlarm.Create(FMPGPlayer);
  FAlarm.Path := ExtractFilePath(Application.ExeName);

  FReminderAlarm := TAlarm.Create(FMPGPlayer);
  FReminderAlarm.Path := ExtractFilePath(Application.ExeName);

  FTimer := TAlarm.Create(FMPGPlayer);
  FTimer.Path := ExtractFilePath(Application.ExeName);

  FAlarm.OnBeforeAlarm := BeforeAlarm;
  FReminderAlarm.OnBeforeAlarm := BeforeAlarm;
  FTimer.OnBeforeAlarm := BeforeAlarm;

  FAlarm.OnAfterAlarm := AfterAlarm;
  FReminderAlarm.OnAfterAlarm := AfterAlarm;
  FTimer.OnAfterAlarm := AfterAlarm;

  FCurrentLocation := 0;

{$IFDEF PICSHOW}
  Self.BorderStyle := bsSingle;
{$ENDIF}

  FMusicPlayer := nil;
  FSleepPlayer := nil;
  FSyncServer := nil;
  FSyncClient := nil;
  
  FMusicState := msOff;

  FCOMServer := TCOMServer.Create(44558);

  FWeatherReport := '';
end;

procedure TfrmClockMain.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  FAlarm.ResetAlarm;
  FTimer.ResetAlarm;
  FReminderAlarm.ResetAlarm;
end;

procedure TfrmClockMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FMusicPlayer) then
    FMusicPlayer.Free;

  if Assigned(FSleepPlayer) then
    FSleepPlayer.Free;

  FMPGPlayer.Free;

  if Assigned(FSyncClient) then
    FreeAndNil(FSyncClient);

  if Assigned(FSyncServer) then
    FreeAndNil(FSyncServer);

  FMetOffice.Free;
  FAlarm.Free;
  FReminderAlarm.Free;
  FTimer.Free;
  FCOMServer.Free;
end;

procedure TfrmClockMain.FormHide(Sender: TObject);
begin
  FAlarm.ResetAlarm;
  FTimer.ResetAlarm;
  FReminderAlarm.ResetAlarm;
end;

procedure TfrmClockMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 112) or (key = 72) then
  begin
    Key := 0;

    ShowMessage('Keyboard Shortcuts' +  LineEnding +
      'Power - Powerdown' + LineEnding +
      'Power + Shift (...) - Reboot' + LineEnding + LineEnding +
      'W - Configure Wifi' + LineEnding + LineEnding +
      'Enter - Settings' + LineEnding +
      'Space - Stop alarm' + LineEnding +
      'M - Play music / skip song' + LineEnding +
      'L - List and select music' + LineEnding +
      'S - Play sleep music / skip song' + LineEnding +
      'P - Stop music' + LineEnding +
      'U - Update music' + LineEnding + LineEnding +
      'N - Next weather location' + LineEnding +
      'R - Reminders'  + LineEnding + LineEnding +
      'Smiley :-) - Exit');
  end
  {$IFNDEF PICSHOW}
  else if (Key = 43) and (ssShift in Shift) then
  begin
    Key := 0;
    Shutdown(True);
  end
  else if Key = 43 then
  begin
    Key := 0;
    Shutdown(False);
  end
  else if Key = 87 then
  begin
    Key := 0;
    ConfigureWifi;
  end
  else if (Key = 27) and (ssShift in Shift) then
  begin
    Key := 0;
    CloseApp;
  end;
  {$ELSE};
  {$ENDIF}
end;

procedure TfrmClockMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  tmrMinute.Enabled := False;

  FAlarm.ResetAlarm;
  FReminderAlarm.ResetAlarm;
  FTimer.ResetAlarm;

  if Key = #27 then {$IFDEF PICSHOW} Self.Hide {$ENDIF}
  else if Key = #13 then
  begin
    frmClockSettings.ShowModal;
  end
  else if (Key = 'l') or (Key='L') then
  begin
    frmPlaylist.LoadSongs(ChangeFileExt(GetAppConfigFile(False), '_music.cfg'),
      frmClockSettings.edtMusicPath.Text);

    if frmPlaylist.ShowModal = mrOk then
    begin
      FMusicPlayer.PlaySelection(frmPlaylist.MusicPath);
    end;
  end
  else if (Key = 'r') or (Key = 'R') then frmReminderList.Show
  else if (Key = 'n') or (Key = 'N') then
  begin
    Inc(FCurrentLocation);
    if FCurrentLocation > High(Locations) then FCurrentLocation := 0;
    UpdateWeather;
  end
  else if (Key = 'm') or (Key = 'M') then
  begin
    if Assigned(FMusicPlayer) and Assigned(FSleepPlayer) then
    begin
	    if not (FMusicState in [msMusicPaused, msSleepPaused]) then
	    begin
        FSleepPlayer.Stop;
		    FMusicPlayer.Next;
		    FMusicState := msMusicPlaying;
	    end
	    else
	    begin
		    FMusicState := msMusicPaused;
	    end;
    end;
  end
  else if (Key = 'p') or (Key = 'P') then
  begin
    if Assigned(FMusicPlayer) and Assigned(FSleepPlayer) then
    begin
      FSleepPlayer.Pause;
      FMusicPlayer.Pause;
	    FMusicState := msOff;
    end;
  end
  else if (Key = 's') or (Key = 'S') then
  begin
    if Assigned(FMusicPlayer) and Assigned(FSleepPlayer) then
    begin
	    if not (FMusicState in [msMusicPaused, msSleepPaused]) then
	    begin
        FMusicPlayer.Stop;
		    FSleepPlayer.Next;
		    FMusicState := msSleepPlaying;
	    end
	    else
	    begin
		    FMusicState := msSleepPaused;
	    end;
    end;
  end
  else if (Key = 'u') or (Key = 'U') then
  begin
    if Assigned(FMusicPlayer) and Assigned(FSleepPlayer) then
    begin
      FMusicPlayer.RescanSearchPath;
		  FSleepPlayer.RescanSearchPath;
    end;
  end
  else if (Key = '.') then
  begin
    if Assigned(FMPGPlayer) then
    begin
      FMPGPlayer.VolumeUp;
    end;
  end
  else if (Key = ',') then
  begin
    if Assigned(FMPGPlayer) then
    begin
      FMPGPlayer.VolumeDown;
    end;
  end
  else if mmoHtml.Font.Color = clYellow then
  begin
    UpdateWeather;
  end;

  tmrMinute.Enabled := True;
end;

procedure TfrmClockMain.FormShow(Sender: TObject);
begin
  if not FileExists('/usr/bin/mpg123') then
    ShowMessage('Alarm Not Working' + LineEnding
    + 'The package mpg123 was not found on this system.' + LineEnding
    + 'Please install mpg123 to enable the alarm by running the command:' + LineEnding
    + 'sudo apt-get mpg123');

  if not FileExists(ExtractFilePath(Application.ExeName) + 'alarm.mp3')
    and not FileExists('/usr/share/clock/alarm.mp3') then
    ShowMessage('Alarm Not Working' + LineEnding
    + 'The mp3 file "alarm.mp3" can not be found.' + LineEnding
    + 'Please copy the file "alarm.mp3" to the location:' + LineEnding
    + '/usr/share/clock/alarm.mp3');

  UpdateSettings;
  UpdateWeather;
  frmReminderList.FReminders := frmReminders;
  tmrMinute.Enabled := True;
end;

procedure TfrmClockMain.lblTimeClick(Sender: TObject);
begin
  if (FAlarm.State = asActive)
    or (FTimer.State = asActive)
    or (FReminderAlarm.State = asActive) then
  begin
    FAlarm.ResetAlarm;
    FTimer.ResetAlarm;
    FReminderAlarm.ResetAlarm;
  end
  else frmClockSettings.ShowModal;
end;

procedure TfrmClockMain.UpdateSettings;
var
  i: Integer;
  Minutes: LongInt;
  Hours: Integer;
begin
  Locations[0] := frmClockSettings.edtLocation.Text;
  Locations[1] := frmClockSettings.edtLocation1.Text;
  Locations[2] := frmClockSettings.edtLocation2.Text;
  Locations[3] := frmClockSettings.edtLocation3.Text;

  FAlarm.Days[1] := frmClockSettings.cbxSun.Checked;
  FAlarm.Days[2] := frmClockSettings.cbxMon.Checked;
  FAlarm.Days[3] := frmClockSettings.cbxTue.Checked;
  FAlarm.Days[4] := frmClockSettings.cbxWed.Checked;
  FAlarm.Days[5] := frmClockSettings.cbxThu.Checked;
  FAlarm.Days[6] := frmClockSettings.cbxFri.Checked;
  FAlarm.Days[7] := frmClockSettings.cbxSat.Checked;

  FAlarm.AlarmTime := Date + EncodeTime(frmClockSettings.edtHour.Value,
    frmClockSettings.edtMinute.Value, 0, 0);

  FAlarm.Silent := frmClockSettings.cbxSilentAlarm.Checked;

  if frmClockSettings.cbxEnableReminders.Checked then
    FCurrentReminders := frmReminders.GetCurrentReminders
  else SetLength(FCurrentReminders, 0);

  // Reminders
  for i := 1 to 7 do
    FReminderAlarm.Days[i] := Length(FCurrentReminders) > 0;

  FReminderAlarm.AlarmTime := Date + EncodeTime(frmClockSettings.edtRemHour.Value,
    frmClockSettings.edtRemMinute.Value, 0, 0);

  if frmClockSettings.TimerActive then
  begin
    for i := 0 to High(FTimer.Days) do
      FTimer.Days[i] := False;

    Minutes := StrToInt(frmClockSettings.stxtTimer.Caption);

    if Minutes > 0 then
    begin
      Hours := Minutes div 60;
      Minutes := Minutes mod 60;

      FTimer.AlarmTime := Now + EncodeTime(Hours,
        Minutes, 0, 0);

      FTimer.Days[DayOfWeek(FTimer.AlarmTime)] := True;
    end;

    frmClockSettings.TimerActive := False;
  end;

  if not Assigned(FMusicPlayer) then
  begin
    FMusicPlayer := TPlayer.Create(FMPGPlayer,
      ChangeFileExt(GetAppConfigFile(False), '_music.cfg'), frmClockSettings.edtMusicPath.Text);
  end
  else if FMusicPlayer.SearchPath <> frmClockSettings.edtMusicPath.Text then
  begin
    FreeAndNil(FMusicPlayer);
    FMusicPlayer := TPlayer.Create(FMPGPlayer,
      ChangeFileExt(GetAppConfigFile(False), '_music.cfg'), frmClockSettings.edtMusicPath.Text);
  end;

  if not Assigned(FSleepPlayer) then
  begin
    FSleepPlayer := TPlayer.Create(FMPGPlayer,
      ChangeFileExt(GetAppConfigFile(False), '_sleep.cfg'), frmClockSettings.edtSleepPath.Text);
  end
  else if FSleepPlayer.SearchPath <> frmClockSettings.edtSleepPath.Text then
  begin
    FreeAndNil(FSleepPlayer);
    FSleepPlayer := TPlayer.Create(FMPGPlayer,
      ChangeFileExt(GetAppConfigFile(False), '_sleep.cfg'), frmClockSettings.edtSleepPath.Text);
  end;

  if frmClockSettings.cbxGetReminders.Checked then
  begin
    frmReminderList.CanEdit := False;

    if Assigned(FSyncServer) then
      FreeAndNil(FSyncServer);

    if not Assigned(FSyncClient) then
      FSyncClient := TSyncClient.Create;
  end
  else
  begin
    frmReminderList.CanEdit := True;

    if Assigned(FSyncClient) then
      FreeAndNil(FSyncClient);

    if not Assigned(FSyncServer) then
    begin
      FSyncServer := TSyncServer.Create;
      FSyncServer.RemindersFile(GetAppConfigFile(False));
    end;
  end;
end;

procedure TfrmClockMain.Shutdown(Reboot: boolean);
var
  Process: TProcess;
  Timeout: TDateTime;
  i: integer;
begin
  tmrWeather.Enabled := False;
  tmrTime.Enabled := False;
  tmrMinute.Enabled := False;

  Self.KeyPreview := False;

  if Reboot then lblTime.Caption := 'Rebooting ...'
  else lblTime.Caption := 'Powering Down ...';

  Application.ProcessMessages;

  if Assigned(FMusicPlayer) and Assigned(FSleepPlayer) then
  begin
    FSleepPlayer.Stop;
    FMusicPlayer.Stop;
  end;

  Process := TProcess.Create(nil);

  try
    if Reboot then Process.CommandLine := 'sudo /sbin/reboot'
    else Process.CommandLine := 'sudo /sbin/shutdown -h now';

    Process.Options := Process.Options + [poWaitOnExit];
    Process.Execute;
   except
    on E: Exception do
    begin
      DebugLn(Self.ClassName + #9#9 + E.Message);
    end;
  end;

  Process.Free;
end;

procedure TfrmClockMain.ConfigureWifi;
var
  Process: TProcess;
begin
  tmrWeather.Enabled := False;
  tmrTime.Enabled := False;
  tmrMinute.Enabled := False;
  Self.KeyPreview := False;

  PauseMusic;

//  Self.WindowState := wsMinimized;

  lblTime.Caption := 'Configure Wifi ...';
  mmoHTML.Caption := '';
  Application.ProcessMessages;

  Process := TProcess.Create(nil);

  try
    Process.CommandLine := 'sudo /usr/local/sbin/ewoc-z2sid-term';
    Process.Options := Process.Options + [poWaitOnExit];
    Process.Execute;

    lblTime.Caption := 'Updating weather ...';

    Application.ProcessMessages;

    UpdateWeather;

    tmrWeather.Enabled := True;
    tmrTime.Enabled := True;
    tmrMinute.Enabled := True;

    AfterAlarm;

    Self.KeyPreview := True;
  except
    on E: Exception do
    begin
      DebugLn(Self.ClassName + #9#9 + E.Message);
    end;
  end;

  Process.Free;
end;

procedure TfrmClockMain.CloseApp;
var
  Process: TProcess;
begin
  tmrWeather.Enabled := False;
  tmrTime.Enabled := False;
  tmrMinute.Enabled := False;
  Self.KeyPreview := False;

  PauseMusic;

  Process := TProcess.Create(nil);

  try
    if FileExists('/usr/bin/lxpanel') then
    begin
      Process.CommandLine := '/usr/bin/lxpanel';
      Process.Execute;
    end;

    Self.Close;
  except
    on E: Exception do
    begin
      DebugLn(Self.ClassName + #9#9 + E.Message);
    end;
  end;

  Process.Free;
end;

end.

