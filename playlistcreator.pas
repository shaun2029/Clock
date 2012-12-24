//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit PlaylistCreator;

{$mode Delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniFiles;

type

  { TfrmPlaylist }

  TfrmPlaylist = class(TForm)
    lstDisplay: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }

    FLevel, FMinLevel: integer;
    FPath: TStringList;
    FPathList: TStringList;
    FStartPath: string;

    procedure Display(Next: boolean);

    function GetLevel(Level: integer; const Path: string; out Data: string): boolean;
    function GetMusicPath: string;
  public
    { public declarations }
    procedure LoadSongs(const ConfigFile, StartPath: string);
  published
    property MusicPath: string read GetMusicPath;
  end;

var
  frmPlaylist: TfrmPlaylist;

implementation

{$R *.lfm}

{ TfrmPlaylist }

procedure TfrmPlaylist.FormCreate(Sender: TObject);
begin
  FPathList := TStringList.Create;
  FPath := TStringList.Create;
end;

procedure TfrmPlaylist.FormDestroy(Sender: TObject);
begin
  FPathList.Free;
  FPath.Free;
end;

procedure TfrmPlaylist.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then Self.ModalResult := mrCancel
  else if Key = 13 then
  begin
    Self.ModalResult := mrOk;
  end
  else if Key = 39 then
  begin
    Display(True);
  end
  else if Key = 37 then
  begin
    Display(False);
  end;
end;

{ Claculate what needs to be displayed for file navigation.
  If Next is true display the next branch else display the previous. }
procedure TfrmPlaylist.Display(Next: boolean);
var
  i, Level, Selected: Integer;
  Found: boolean;
  Data, SelectedStr: string;
  CurrPath: string;
  SelectList: TStringList;
begin
  Found := False;
  SelectList := TStringList.Create;
  Selected := lstDisplay.ItemIndex;

  if (Selected < 0) then Selected := 0;

  if (Selected < lstDisplay.Items.Count) then
    SelectedStr := lstDisplay.Items.Strings[Selected]
  else
    Selectedstr := '';

  if Next then Level := FLevel + 1
  else
  begin
    if Flevel <= FMinLevel then Flevel := FMinLevel
    else Level := FLevel - 1;
  end;

  CurrPath := FStartPath;

  if Next then
  begin
    for i := 0 to FPath.Count -1 do
    begin
      CurrPath := CurrPath + FPath.Strings[i] + '/';
    end;

    CurrPath := CurrPath + Selectedstr;
  end
  else
  begin
    for i := 0 to FPath.Count -2 do
    begin
      CurrPath := CurrPath + FPath.Strings[i] + '/';
    end;
  end;

  for i := 0 to FPathList.Count -1 do
  begin
    if (Pos(CurrPath, FPathList.Strings[i]) = 1)
      and GetLevel(Level, FPathList.Strings[i], Data) then
    begin
      if not Found then
      begin
        Found := True;
      end;

      if SelectList.IndexOf(Data) < 0 then
        SelectList.Add(Data);
    end;
  end;

  if Found then
  begin
    lstDisplay.Items.Text := SelectList.Text;

    if Next then
    begin
      lstDisplay.ItemIndex := 0;

      if SelectedStr <> '' then
        FPath.Append(SelectedStr);
    end
    else
    begin
      lstDisplay.ItemIndex := 0;

      if FPath.Count > 0 then
      begin
        Selected := lstDisplay.Items.IndexOf(FPath.Strings[FPath.Count - 1]);

        if Selected >= 0 then
        begin
         lstDisplay.ItemIndex := Selected;
        end
      end;

      if FPath.Count > 0 then
        FPath.Delete(FPath.Count - 1)
    end;

    FLevel := Level;
  end;

  lstDisplay.Sorted := True;
  SelectList.Free;
end;

procedure TfrmPlaylist.LoadSongs(const ConfigFile, StartPath: string);
var
  IniFile: TIniFile;
  i: Integer;
begin
  IniFile := TIniFile.Create(ConfigFile);

  try
    if FileExists(ChangeFileExt(ConfigFile, '.plp')) then
    begin
      FPathList.LoadFromFile(ChangeFileExt(ConfigFile, '.plp'));
      FPathList.Sort;
    end;
  finally
    IniFile.Free;
  end;

  FStartPath := StartPath;
  if FStartPath[Length(FStartPath)] <> '/' then
    FStartPath := FStartPath + '/';

  FMinLevel := 0;

  for i := 2 to Length(FStartPath) do
  begin
    if FStartPath[i] = '/' then
      Inc(FMinLevel);
  end;

  FPath.Clear;
  lstDisplay.Clear;

  FLevel := FMinLevel - 1;
  Display(True);
end;

function TfrmPlaylist.GetLevel(Level: integer; const Path: string; out Data: string): boolean;
var
  SPos, EPos, Lev: integer;
  Found: boolean;
  i: Integer;
begin
  Result := False;
  SPos := 0;
  EPos := 0;
  Data := '';

  Found := False;
  Lev := 0;

  for i := 1 to Length(Path) do
  begin
    if Path[i] = '/' then
    begin
      if Lev = Level then
      begin
        Found := True;
        SPos := i + 1;
        Break;
      end
      else Inc(Lev);
    end;
  end;

  if not Found then Exit;

  Found := False;
  Lev := 0;

  for i := SPos to Length(Path) do
  begin
    if (Path[i] = '/') then
    begin
      Found := True;
      EPos := i;
      Break;
    end;
  end;

  if SPos > 1 then
  begin
    if Epos > SPos then
    begin
      Data := Copy(Path, SPos, EPos - SPos);
      Result := True;
    end;
  end;
end;

function TfrmPlaylist.GetMusicPath: string;
var
  i: integer;
  Selected: LongInt;
begin
  Result := FStartPath;

  for i := 0 to FPath.Count -1 do
  begin
    Result := Result + FPath.Strings[i] + '/';
  end;

  Selected := lstDisplay.ItemIndex;

  if Selected >= 0 then
  begin
    if (Selected < lstDisplay.Items.Count) then
    begin
      Result := Result + lstDisplay.Items.Strings[Selected] + '/';
    end;
  end;
end;


end.

