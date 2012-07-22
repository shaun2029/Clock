//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit sync;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UDPClient, UDPServer;

type

  { TSyncServer }

  TSyncServer = class
  private
    FServer: TUDPServer;
  public
    procedure RemindersFile(Filename: string);

    constructor Create;
    destructor Destroy; override;
  published
  end;

  { TSyncClient }

  TSyncClient = class
  private
    FClient: TUDPClient;
  public
    function GetReminders(out Reminders: string): boolean;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSyncClient }

function TSyncClient.GetReminders(out Reminders: string): boolean;
begin
  Result := FClient.RequestReminders(Reminders);
end;

constructor TSyncClient.Create;
begin
  inherited Create;

  FClient := TUDPClient.Create;
end;

destructor TSyncClient.Destroy;
begin
  FClient.Free;

  inherited Destroy;
end;

{ TSyncServer }

procedure TSyncServer.RemindersFile(Filename: string);
var
  FileData: TStringList;
begin
  FileData := TStringList.Create;

  try
    if FileExists(Filename) then
      FileData.LoadFromFile(Filename);

    FServer.Send(FileData.Text);
  except
  end;

  FileData.Free;
end;

constructor TSyncServer.Create;
begin
  inherited Create;

  FServer := TUDPServer.Create;
end;

destructor TSyncServer.Destroy;
begin
  inherited Destroy;
end;

end.

