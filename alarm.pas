//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit alarm;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Process, LCLProc, MusicPlayer;

type

  TAlarmCallback = procedure of object;

  TAlarmState = (asOff, asSet, asActive);

  { TAlarm }

  TAlarm = class
  private
    FOnAfterAlarm: TAlarmCallback;
    FOnBeforeAlarm: TAlarmCallback;

    FMusicPlayer: TMusicPlayer;
    FAlarmState: TAlarmState;
    FAlarmTime: TDatetime;
    FPath: string;
    FSilent: boolean;
    LastSouded: TDateTime;

    procedure SoundAlarm;
  public
    Days: array [1..7] of boolean;

    procedure Tick;
    procedure ResetAlarm;
    procedure Stop;

    constructor Create(MusicPlayer: TMusicPlayer);
    destructor Destroy; override;
  published
    property AlarmTime: TDatetime read FAlarmTime write FAlarmTime;
    property Path: string write FPath;
    property State: TAlarmState read FAlarmState;

    property OnBeforeAlarm: TAlarmCallback write FOnBeforeAlarm;
    property OnAfterAlarm: TAlarmCallback write FOnAfterAlarm;
    property Silent: boolean write FSilent;
  end;

implementation

{ TAlarm }

procedure TAlarm.SoundAlarm;
var
  AlarmFile: string;
begin
DebugLn('Alarm: Sound alarm');

  if FileExists(FPath + 'alarm.mp3') then
    AlarmFile := FPath + 'alarm.mp3'
  else
    AlarmFile := '/usr/share/clock/alarm.mp3';

  FMusicPlayer.Play(AlarmFile);
end;

procedure TAlarm.Tick;
var
  CurrTime: TDateTime;
begin
  CurrTime := Now;
  FAlarmTime := Date + Frac(FAlarmTime);

  // Is the alarm set to go off for the current day?
  if Days[DayOfWeek(CurrTime)] then
  begin
    if (CurrTime <= FAlarmTime) then
    begin
      case FAlarmState of
        asOff: FAlarmState := asSet;
      end;
    end;
  end
  else
  begin
    case FAlarmState of
      asSet: FAlarmState := asOff;
    end;
  end;

  if (FAlarmState <> asOff) and (CurrTime > FAlarmTime)
    and (CurrTime < FAlarmTime + EncodeTime(0, 3, 0, 0)) then
  begin
    if FSilent then
    begin
      if Assigned(FOnBeforeAlarm) then
        FOnBeforeAlarm;

      FAlarmState := asOff;

      if Assigned(FOnAfterAlarm) then
        FOnAfterAlarm;
    end
    else if not ((CurrTime > FAlarmTime + EncodeTime(0, 1, 0, 0))
      and (CurrTime < FAlarmTime + EncodeTime(0, 2, 0, 0))) then
    begin
      // Minute of silence between 1x2 minutes of ringing
      case FAlarmState of
        asSet:
          begin
            if Assigned(FOnBeforeAlarm) then
              FOnBeforeAlarm;

            SoundAlarm;
            FAlarmState := asActive;
          end;
        asActive:
          begin
            if FMusicPlayer.State = mpsStopped then
              SoundAlarm;
          end;
      end;
    end;
  end
  else
  begin
    case FAlarmState of
      asActive:
        begin
          FAlarmState := asOff;

          if Assigned(FOnAfterAlarm) then
            FOnAfterAlarm;
        end;
    end;
  end;
end;

procedure TAlarm.ResetAlarm;
begin
  if FAlarmState = asActive then
  begin
    Stop;
    FAlarmState := asOff;

    if Assigned(FOnAfterAlarm) then
      FOnAfterAlarm;
  end;
end;

procedure TAlarm.Stop;
begin
  FMusicPlayer.Stop;
end;

constructor TAlarm.Create(MusicPlayer: TMusicPlayer);
var
  i: Integer;
begin
  inherited Create;

  FMusicPlayer := MusicPlayer;

  FOnBeforeAlarm := nil;
  FOnAfterAlarm := nil;

  for i := 1 to 7 do Days[i] := False;
  FAlarmTime := 0;
  FAlarmState := asOff;
  FPath := '';

  FSilent := False;
end;

destructor TAlarm.Destroy;
begin
  Stop;

  inherited Destroy;
end;

end.

