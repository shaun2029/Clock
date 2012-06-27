//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit udpserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc,

  // synapse
  blcksock;

type

  { TUDPServerThread }

  TUDPServerThread = class(TThread)
  protected
    FData: TStringList;

    procedure Execute; override;
  public
    constructor Create(Data: string);
    destructor Destroy; override;
  end;

  TUDPServer = class
  private
    FUDPServerThread: TUDPServerThread;

    procedure Stop;
    procedure Start(Data: string);

    function GetRunning: Boolean;
  public
    procedure Send(Data: string);
    property Running: Boolean read GetRunning;
  end;

implementation

{ TUDPServer }

function TUDPServer.GetRunning: Boolean;
begin
  Result := FUDPServerThread <> nil;
end;

procedure TUDPServer.Send(Data: string);
begin
  Stop;
  Start(Data);
end;

procedure TUDPServer.Start(Data: string);
begin
  FUDPServerThread := TUDPServerThread.Create(Data);
end;

procedure TUDPServer.Stop;
begin
  if FUDPServerThread <> nil then
  begin
    FUDPServerThread.Terminate;
    FUDPServerThread.WaitFor;
    FreeAndNil(FUDPServerThread);
  end;
end;

{ TUDPServerThread }

procedure TUDPServerThread.Execute;
var
  Socket: TUDPBlockSocket;
  Buffer: string;
  Size: Integer;
  i: Integer;
  DataBuff: string;
  Pos: integer;
  Total: integer;
  PakNo: integer;
begin
  Socket := TUDPBlockSocket.Create;
  try
    Socket.Bind('0.0.0.0', '44559');
    try
      if Socket.LastError <> 0 then
      begin
        raise Exception.CreateFmt('Bind failed with error code %d', [Socket.LastError]);
        Exit;
      end;

      while not Terminated do
      begin
        // wait one second for new packet
        Buffer := Socket.RecvPacket(1000);

        if Socket.LastError = 0 then
        begin
          if Buffer = 'REQUEST REMINDERS' then
          begin
            Total := FData.Count;

			      // Send packet with reminder total
            Socket.SendString('REMINDERS:' + IntToStr(Total));

            DataBuff := FData.Text;
            Pos := 1;
            PakNo := 1;
            
			      // Send the reminders in packets of <= 512 bytes
            while Pos < Length(DataBuff) do
            begin
              Buffer := 'REMPAK:' + IntToStr(PakNo) + ';' + Copy(DataBuff, Pos, 500);

              Socket.SendString(Buffer);
              Inc(Pos, 500);
              Inc(PakNo);
            end;
          end;
        end;

        // minimal sleep
        if Buffer = '' then
          Sleep(10);
      end;

    finally
      Socket.CloseSocket;
    end;
  finally
    Socket.Free;
  end;
end;

constructor TUDPServerThread.Create(Data: string);
begin
  inherited Create(False);

  FData := TStringList.Create;
  FData.Text := Data;
end;

destructor TUDPServerThread.Destroy;
begin
  FData.Free;

  inherited Destroy;
end;

end.


