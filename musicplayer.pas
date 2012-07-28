//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit MusicPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, ID3Engine, LCLProc;

type

  TMusicPlayerState = (mpsStopped, mpsPlaying);

  { TMusicPlayer }

  TMusicPlayer  = class
  private
    FEqualizer: string;
    FPlayProcess: TProcess;
    FPlayTimeout: TDateTime;
    FSongArtist: string;
    FSongTitle: string;
    FState: TMusicPlayerState;
    FId3: TID3Engine;
    FStartTime: TDateTime;

    procedure EqualizerDefault(Filename: string);
    function GetState: TMusicPlayerState;
    procedure PlaySong(Song: string);
    procedure StartPlayProcess(out Process: TProcess; Song: String);
    procedure StopSong;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Play(Filename: string);
    procedure VolumeUp;
    procedure VolumeDown;
    procedure Stop;
  published
    property SongArtist: string read FSongArtist;
    property SongTitle: string read FSongTitle;
    property State: TMusicPlayerState read GetState;
    property Equalizer: string write FEqualizer;
  end;

const
  {  Older versions of MPG123 (0.1.12) continue playing for the length
   of the buffer after verbose output has stopped. The verbose output
   is used to work out when playback has stopped.

   Newer versions of MPG123 (0.2.13) stop playing when the verbose
   output stops. To work around this problem, the buffer for non ARM
   systems is short.}

  // Zipit requires a big buffer for network play.
  {$ifdef CPUARM}
  BUFFER_TIME = 20; // in seconds
  {$else}
  BUFFER_TIME = 1;
  {$endif}

  // Buffer size in KB. Based on 44100 samples per sec * 2 bytes
  BUFFER_SIZE = (44100 * 2 * 2 * BUFFER_TIME) div 1024;

implementation

procedure TMusicPlayer.PlaySong(Song: string);
begin
  // Ensure that song is not playing
  StopSong;

  try
    FId3.Active := False;
    FId3.FileName := Song;
    FId3.Active := True;
    FSongArtist := FId3.Artist;
    FSongTitle := FId3.Title;
    FId3.Active := False;
  except
    on E: Exception do
    begin
      FSongTitle := '';

      DebugLn(Self.ClassName + #9#9 + 'Failed to get ID3 Tags for "'
        + ExtractFilename(Song) + '"');
      DebugLn(Self.ClassName + #9#9 + E.Message);
    end;
  end;

  try
    if FileExists(Song) then
    begin
      StartPlayProcess(FPlayProcess, Song);

      FState := mpsPlaying;

      if Trim(FSongTitle) = '' then FSongTitle := ExtractFilename(Song);
    end;
  except
    on E: Exception do
    begin
      DebugLn(Self.ClassName + #9#9 + E.Message);
    end;
  end;
end;

procedure TMusicPlayer.StartPlayProcess(out Process: TProcess; Song: String);
begin
  Process := TProcess.Create(nil);
  Process.Options := Process.Options + [poUsePipes];

  // Use mpg321 if possible
  if FileExists('/usr/bin/mpg321') then
  begin
    Process.CommandLine := Format('mpg321 "%s"', [Song]);
  end
  else
  begin
    Process.CommandLine := Format('mpg123 --rva-mix --buffer %d --preload 1.0 "%s"', [BUFFER_SIZE, Song]);
  end;

  Process.Execute;
  // Slow dow the process in case of missing files
  FStartTime := Now + EncodeTime(0, 0, 10, 0);
end;


function TMusicPlayer.GetState: TMusicPlayerState;
begin
  if (FState = mpsPlaying) and (Now > FStartTime) then
  begin
    if not Assigned(FPlayProcess) or not FPlayProcess.Running then
    begin
      if Assigned(FPlayProcess) then
        FreeAndNil(FPlayProcess);

      FState := mpsStopped;
    end;
  end;

  Result := FState;
end;

procedure TMusicPlayer.StopSong;
var
  Command: string;
begin
  if Assigned(FPlayProcess) then
  begin
    if FPlayProcess.Running then
      FPlayProcess.Terminate(0);

    FreeAndNil(FPlayProcess);
  end;

  FState := mpsStopped;
end;

constructor TMusicPlayer.Create;
begin
  FPlayProcess := nil;
  FId3 := TID3Engine.Create(nil);
  FId3.ReadingOnly := True;
  FEqualizer := '';
end;

destructor TMusicPlayer.Destroy;
begin
  StopSong;

  try
    FId3.Free;
  except
  end;

  inherited Destroy;
end;

procedure TMusicPlayer.Play(Filename: string);
begin
  if (FEqualizer <> '') and not FileExists(FEqualizer) then
    EqualizerDefault(FEqualizer);

  PlaySong(Filename);
end;

procedure TMusicPlayer.VolumeUp;
var
  Process: TProcess;
begin
  try
    Process := TProcess.Create(nil);
    Process.Options := Process.Options + [poWaitOnExit];

    Process.CommandLine := 'amixer set PCM 3+';
    Process.Execute;
  except
    on E: Exception do
    begin
      DebugLn(Self.ClassName + #9#9 + E.Message);
    end;
  end;
end;

procedure TMusicPlayer.VolumeDown;
var
  Process: TProcess;
begin
  try
    Process := TProcess.Create(nil);
    Process.Options := Process.Options + [poWaitOnExit];

    Process.CommandLine := 'amixer set PCM 3-';
    Process.Execute;
  except
    on E: Exception do
    begin
      DebugLn(Self.ClassName + #9#9 + E.Message);
    end;
  end;
end;

procedure TMusicPlayer.Stop;
begin
  StopSong;
end;

procedure TMusicPlayer.EqualizerDefault(Filename: string);
var
  myFile : TextFile;
begin
  try
    // Try to open the file for writing to
    AssignFile(myFile, Filename);
    ReWrite(myFile);

    WriteLn(myFile, '# mpg123 equalizer file');
    WriteLn(myFile, '# 32 Band 2 Channel');
    WriteLn(myFile, '# Levels 0 -> 1 e.g. 0.5 0.5');
    WriteLn(myFile, '#');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '1 1');
    WriteLn(myFile, '');

    // Close the file
    CloseFile(myFile);
  except
  end;
 end;

end.

