//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit FindThread;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LCLProc, SyncObjs;

type

  { TFindFilesThread }

  TFindFilesThread = class(TThread)
  private
    FSearchPath: string;
    FExtension: string;
    FComplete: boolean;
    FCritical: TCriticalSection;
    FFileList, FPathList: TStringList;

    procedure FindFiles(FilesList, PathList: TStringList; StartDir, Extension: string);
    function GetComplete: boolean;
    function GetCount: integer;
  public
    constructor Create(FileList, PathList: TStringList; SearchPath, Extension: string);
    destructor Destroy; override;

    procedure Execute; override;
  published
    property Complete: boolean read GetComplete;
    property SearchPath: string read FSearchPath;
    property Count: integer read GetCount;
  end;

implementation

{ TFindFilesThread }

function TFindFilesThread.GetComplete: boolean;
begin
  Result := FComplete;
end;

function TFindFilesThread.GetCount: integer;
begin
  FCritical.Enter;
  Result := FFileList.Count;
  FCritical.Leave;
end;

constructor TFindFilesThread.Create(FileList, PathList: TStringList;
  SearchPath, Extension: string);
begin
  inherited Create(True);
  FSearchPath := SearchPath;
  FComplete := False;
  FExtension := Extension;

  FFileList := FileList;
  FPathList := PathList;

  FFileList.Clear;
  FPathList.Clear;

  FCritical := TCriticalSection.Create;
end;

destructor TFindFilesThread.Destroy;
begin
  FCritical.Free;

  inherited Destroy;
end;

procedure TFindFilesThread.Execute;
var
  total: integer;
begin
  FindFiles(FFileList, FPathList, FSearchPath, FExtension);
  FFileList.Sort;
  FComplete := True;
end;

// Recursive procedure to build a list of files
procedure TFindFilesThread.FindFiles(FilesList, PathList: TStringList;
  StartDir, Extension: string);
var
  SR: TSearchRec;
  DirList: TStringList;
  IsFound: Boolean;
  i: integer;
  PathIndex: integer;
  PathIndexStr: string;
begin
  if Terminated then Exit;

  if StartDir[length(StartDir)] <> '/' then
    StartDir := StartDir + '/';

  // Build a list of the files in the directory StartDir


  PathIndex := PathList.IndexOf(StartDir);
  if PathIndex < 0 then
  begin
    PathIndex := PathList.Count;
    PathList.Add(StartDir);
  end;

  PathIndexStr := IntToStr(PathIndex);

  IsFound :=
    FindFirst(StartDir+'*' + Extension, faAnyFile-faDirectory, SR) = 0;
  while IsFound do
  begin
    FCritical.Enter;
    FilesList.Add(PathIndexStr + ':' + SR.Name);
    FCritical.Leave;

    IsFound := FindNext(SR) = 0;
  end;
  FindClose(SR);

  // Build a list of subdirectories
  DirList := TStringList.Create;
  IsFound := FindFirst(StartDir+'*', faAnyFile and faDirectory, SR) = 0;
  while IsFound and not Terminated do begin
    if ((SR.Attr and faDirectory) <> 0) and
         (SR.Name[1] <> '.') then
      DirList.Add(StartDir + SR.Name);

    IsFound := FindNext(SR) = 0;

    Sleep(10);
  end;
  FindClose(SR);

  // Scan the list of subdirectories
  for i := 0 to DirList.Count - 1 do
    FindFiles(FilesList, PathList, DirList[i], Extension);

  DirList.Free;
end;


end.

