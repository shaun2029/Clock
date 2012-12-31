//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit music;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FindThread, MusicPlayer, LCLProc, IniFiles;

type

  TPlayerState = (psStopped, psPlaying);

  { TPlayer }

  TPlayer = class
  private
    FSongList, FPathList: TStringList;
    FPlaySongList, FPlayPathList: TStringList;

    FFindFiles: TFindFilesThread;
    FFindPlayFiles: TFindFilesThread;

    FMusicPlayer: TMusicPlayer;
    FConfigFile, FSearchPath: string;
    FSongIndex: integer;
    FState: TPlayerState;

    function GetFileName(Index: integer; SongList, PathList: TStringList): string;
    function GetSongArtist: string;
    function GetSongTitle: string;
    procedure LoadSettings;
    function PlayPlaylistSong: boolean;
    procedure PlaySong(Next: boolean);
    procedure RandomiseList(var List: TStringList);
    procedure SaveSettings;
    procedure StopSong;
  public
    procedure Play;
    procedure Pause;  // Actualy stops the music

    {  Stop just sets stopped state without calling FMusicPlayer.Stop
       This is because mpg123 output buffering is an issue. }
    procedure Stop;
    procedure Next;

    procedure RescanSearchPath;

    function Tick: integer;

    procedure PlaySelection(SearchPath: string);

    constructor Create(MusicPlayer: TMusicPlayer; ConfigFile, SearchPath: string);
    destructor Destroy; override;
  published
    property SongArtist: string read GetSongArtist;
    property SongTitle: string read GetSongTitle;
    property State: TPlayerState read FState;
    property SearchPath: string read FSearchPath;
  end;

implementation

{ TPlayer }

constructor TPlayer.Create(MusicPlayer: TMusicPlayer; ConfigFile, SearchPath: string);
begin
  inherited Create;

  FMusicPlayer := MusicPlayer;

  FConfigFile := ConfigFile;
  FSongList := TStringList.Create;
  FPathList := TStringList.Create;
  FPlaySongList := TStringList.Create;
  FPlayPathList := TStringList.Create;
  FSongIndex := 0;
  FState := psStopped;

  FSearchPath := SearchPath;
  LoadSettings;
end;

destructor TPlayer.Destroy;
begin
  if Assigned(FFindFiles) then
  begin
    FFindFiles.Terminate;
    FFindFiles.WaitFor;
    FreeAndNil(FFindFiles);
  end
  else SaveSettings;

  if Assigned(FFindPlayFiles) then
  begin
    FFindPlayFiles.Terminate;
    FFindPlayFiles.WaitFor;
    FreeAndNil(FFindPlayFiles);
  end;

  FSongList.Free;
  FPathList.Free;
  FPlaySongList.Free;
  FPlayPathList.Free;

  inherited Destroy;
end;

procedure TPlayer.StopSong;
begin
  FMusicPlayer.Stop;
end;

procedure TPlayer.Stop;
begin
  if FState = psPlaying then
  begin
//DebugLn('Music: Stop');

    FState := psStopped;
    StopSong;
  end;

  Tick;
end;

procedure TPlayer.Pause;
begin
  if FState = psPlaying then
  begin
//DebugLn('Music: Pause');

    FState := psStopped;
    StopSong;

    Tick;
  end
  else Play;
end;

procedure TPlayer.Play;
begin
//DebugLn('Music: Play');

  FState := psPlaying;
  PlaySong(False);
  
  Tick;
end;

procedure TPlayer.Next;
begin
//DebugLn('Music: Next');

  FState := psPlaying;
  PlaySong(True);

  Tick;
end;

// Returns true if scanning for music files

function TPlayer.Tick: integer;
begin
  Result := -1;

  if Assigned(FFindFiles) then
  begin
    if FFindFiles.Complete then
    begin
      FFindFiles.Terminate;
      FFindFiles.WaitFor;
      FreeAndNil(FFindFiles);
      RandomiseList(FSongList);
    end
    else Result := FFindFiles.Count;
  end
  else if Assigned(FFindPlayFiles) then
  begin
    if FFindPlayFiles.Complete then
    begin
      FFindPlayFiles.Terminate;
      FFindPlayFiles.WaitFor;
      FreeAndNil(FFindPlayFiles);
    end
    else Result := FFindPlayFiles.Count;
  end
  else if (FState = psPlaying) then
  begin
    if FMusicPlayer.State = mpsStopped then
    begin
      PlaySong(True);
    end;
  end;
end;

procedure TPlayer.PlaySong(Next: boolean);
var
  Filename: string;
begin
  if PlayPlaylistSong then Exit;

  if Next then Inc(FSongIndex , 1);
  
  if FSongIndex < 0 then FSongIndex := FSongList.Count -1;
  if FSongIndex >= FSongList.Count then FSongIndex := 0;

  try
    if (FSongIndex < FSongList.Count) then
    begin
      Filename := GetFilename(FSongIndex, FSongList, FPathList);

      if FileExists(Filename) then FMusicPlayer.Play(Filename)
      else DebugLn('Music: Failed to find "' + Filename + '"');
    end;
  except
    on E: Exception do
    begin
      DebugLn(Self.ClassName + #9#9 + E.Message);
    end;
  end;

  SaveSettings;
end;

function TPlayer.PlayPlaylistSong: boolean;
var
  Filename: string;
begin
  Result := False;

  if FPlaySongList.Count > 0 then
  begin
    Result := True;

    try
      Filename := GetFilename(0, FPlaySongList, FPlayPathList);
      FPlaySongList.Delete(0);

      if FileExists(Filename) then FMusicPlayer.Play(Filename)
      else DebugLn('Music: Failed to find "' + Filename + '"');
    except
      on E: Exception do
      begin
        DebugLn(Self.ClassName + #9#9 + E.Message);
      end;
    end;
  end;
end;

procedure TPlayer.SaveSettings;
var
  IniFile: TIniFile;
begin
  if FConfigFile = '' then Exit;

  IniFile := TIniFile.Create(FConfigFile);

  try
    IniFile.WriteString('Settings', 'MusicPath', FSearchPath);
    IniFile.WriteInteger('Settings', 'Position', FSongIndex);

    if (FSongList.Count > 0) then
    begin
      FSongList.SaveToFile(ChangeFileExt(FConfigFile, '.pl'));
      FPathList.SaveToFile(ChangeFileExt(FConfigFile, '.plp'));
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TPlayer.LoadSettings;
var
  IniFile: TIniFile;
begin
  if FConfigFile = '' then Exit;

  IniFile := TIniFile.Create(FConfigFile);

  try
    if FSearchPath = '' then
      FSearchPath := IniFile.ReadString('Settings', 'MusicPath', '~/Music');

    FSongIndex := IniFile.ReadInteger('Settings', 'Position', 0);

    if FileExists(ChangeFileExt(FConfigFile, '.pl'))
      and FileExists(ChangeFileExt(FConfigFile, '.plp')) then
    begin
      FSongList.LoadFromFile(ChangeFileExt(FConfigFile, '.pl'));
      FPathList.LoadFromFile(ChangeFileExt(FConfigFile, '.plp'));
    end;
  finally
    IniFile.Free;
  end;
end;

function TPlayer.GetSongArtist: string;
begin
  Result := FMusicPlayer.SongArtist;
end;

function TPlayer.GetSongTitle: string;
begin
  Result := FMusicPlayer.SongTitle;
end;

procedure TPlayer.RandomiseList(var List: TStringList);
var
  i, r: integer;
  Str: string;
begin
  RandSeed := 1;

  for i := 0 to  List.Count -1 do
  begin
    r := Random(List.Count);
    Str := List.Strings[r];
    List.Strings[r] := List.Strings[i];
    List.Strings[i] := Str;
  end;
end;

procedure TPlayer.RescanSearchPath;
begin
  if Assigned(FFindFiles) then
  begin
    FFindFiles.Terminate;
    FFindFiles.WaitFor;
    FreeAndNil(FFindFiles);
    RandomiseList(FSongList);
  end
  else
  begin
    FFindFiles := TFindFilesThread.Create(FSongList, FPathList, FSearchPath, '.mp3');
    FFindFiles.Resume;
  end;
end;

function TPlayer.GetFileName(Index: integer; SongList, PathList: TStringList) : string;
var
  PathIndex: integer;
  StartPos: integer;
begin
  Result := SongList.Strings[Index];

  StartPos := Pos(':', Result);

  if StartPos >= 0 then
  begin
    PathIndex := StrToIntDef(Copy(Result, 1, StartPos - 1), -1);

    if PathIndex >= 0 then
    begin
      Result := PathList.Strings[PathIndex]
        + Copy(Result, StartPos + 1, Length(Result));

      //DebugLn('GetFileName: Song ' + Result);
    end;
  end
  else Result := '';
end;

procedure TPlayer.PlaySelection(SearchPath: string);
begin
  if Assigned(FFindPlayFiles) then
  begin
    FFindPlayFiles.Terminate;
    FFindPlayFiles.WaitFor;
    FreeAndNil(FFindPlayFiles);
  end;

  FFindPlayFiles := TFindFilesThread.Create(FPlaySongList, FPlayPathList, SearchPath, '.mp3');
  FFindPlayFiles.Resume;
end;

end.

