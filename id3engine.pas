unit ID3Engine;
{-----------------------------------------------------------------------------

  H    H                          GGGG RRRR    AAA  FFFFF
  H    H       BB             DD GG    RR_RR  AA¯AA FF
  HHHHHH YY  Y BBBBB RRR II DDDD GG GG RRRRRR AAAAA FFFF
  HH  HH YY  Y BBL B R   II D DD  GGGG RR  RR AA AA FF
  HH  HH YYYYY BBBBB RR  II DDDD
            YY
            YY

                               HybridGRAF is our TradeMark
----------------------------------------------------------
  HybridGRAF Interactive (c) 2002, All rights reserved.
----------------------------------------------------------

    Unit Name: ID3Engine
  Description: ID3 Tagging Engine
       Author: William Anthony
     Creation: 5/8/2002 12:16:25 PM
      Version: 2.0
       E-mail: hybridgraf@hotmail.com
         Site: http://go.to/hybridgraf
 Legal issues: Copyright (C) 2002 by HybridGRAF Interactive

Usage: This software is provided 'as-is', without any express or
implied warranty.  In no event will the author be held liable
for any  damages arising from the use of this software.

Permission is granted to anyone to use this software for any
purpose, including commercial applications, and to alter it
and redistribute it freely, subject to the following
restrictions:

1. The origin of this software must not be misrepresented,
you must not claim that you wrote the original software.
If you use this software in a product, an acknowledgment
in the product documentation would be appreciated but is
not required.

2. Altered source versions must be plainly marked as such, and
must not be misrepresented as being the original software.

3. This notice may not be removed or altered from any source
distribution.

4. If you decide to use this software in any of your applications.
Send me an EMail address and tell me about it.

----------------------------------------------------------------------------------}
{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  zstream,
  Dialogs;
  // Windows;

const
  cID3BufferMax     = 4096;

type
  TCRCData = 0..1099511627776;
  TID3Engine = class;
  TID3FrameName = array[0..3] of Char;
  TID3Frames = class;
  TID3Frames_Item = class;
  TID3ImageSizeRestrictions = (irNone, ir256x256max, ir64x64max, ir64x64fixed);
  TID3TagSizeRestrictions = (sr128Frames_1MB, sr64Frames_128KB, sr32Frames_40KB, sr32Frames_4KB);
  TID3TextSizeRestrictions = (trNone, tr1024max, tr128max, tr30max);
  TISODate = array[0..7] of Char;
  TLanguageID = array[0..2] of Char;

  TID3TagRestrictions = class(TPersistent)
  private
    FImageEncodingRestrictions: boolean;
    FImageSize: TID3ImageSizeRestrictions;
    FTagSize: TID3TagSizeRestrictions;
    FTextEncodingRestrictions: boolean;
    FTextSize: TID3TextSizeRestrictions;
  public
    procedure Clear; virtual;
    procedure LoadFromByte(Value: Byte);
    procedure SaveToByte(var Value: Byte);
  published
    property ImageEncodingRestrictions: boolean read FImageEncodingRestrictions write FImageEncodingRestrictions;
    property ImageSize: TID3ImageSizeRestrictions read FImageSize write FImageSize;
    property TagSize: TID3TagSizeRestrictions read FTagSize write FTagSize;
    property TextEncodingRestrictions: boolean read FTextEncodingRestrictions write FTextEncodingRestrictions;
    property TextSize: TID3TextSizeRestrictions read FTextSize write FTextSize;
  end;

  TID3ExtendedHeader = class(TPersistent)
  private
    FCRCDataPresent,
      FTagIsAnUpdate,
      FTagRestricted: Boolean;
    FCRCData: TCrcdata;
    FTagRestrictions: TID3TagRestrictions;
    procedure SetCRCData(const Value: TCRCData);
    procedure SetTagRestrictions(const Value: TID3TagRestrictions);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
  published
    property CRCData: TCRCData read FCRCData write SetCRCData;
    property CRCDataPresent: boolean read FCRCDataPresent write FCRCDataPresent;
    property TagIsAnUpdate: boolean read FTagIsAnUpdate write FTagIsAnUpdate;
    property TagRestricted: boolean read FTagRestricted write FTagRestricted;
    property TagRestrictions: TID3TagRestrictions read FTagRestrictions write SetTagRestrictions;
  end;

  TID3Header = class(TPersistent)
  private
    FExperimental,
      FExtendedHeaderPresent,
      FFooterPresent,
      FUnsynchronisation: Boolean;
    FVersionNumber,
      FVersionRevision: Byte;
    FOwner: TId3engine;
    FExtendedHeader: TID3Extendedheader;
    procedure SetExtendedHeader(const Value: TID3ExtendedHeader);
  protected
    FTagSize: integer;
  public
    constructor Create(AOwner: TID3Engine); virtual;
    destructor Destroy; override;
    function LoadFromStream(const Stream: TStream): boolean; virtual;
    function SaveToStream(const Stream: TStream; const WriteFooter: boolean = False): Cardinal; virtual;
    procedure Clear; virtual;
    property Owner: TID3Engine read FOwner;
  published
    property ExperimentalMode: Boolean read FExperimental write FExperimental;
    property ExtendedHeader: TID3ExtendedHeader read FExtendedHeader write SetExtendedHeader;
    property ExtendedHeaderPresent: boolean read FExtendedHeaderPresent write FExtendedHeaderPresent;
    property FooterPresent: Boolean read FFooterPresent write FFooterPresent;
    property Unsynchronisation: boolean read FUnsynchronisation write FUnsynchronisation;
    property VersionNumber: Byte read FVersionNumber write FVersionNumber;
    property VersionRevision: Byte read FVersionRevision write FVersionRevision;
  end;

  TID3EngineFrameNotify = procedure(Frame: TID3Frames_Item; Stream: TStream) of object;

  { TID3Engine }

  TID3Engine = class(TComponent)
  private
    FReadingOnly: boolean;
    FWriteID3V1: Boolean;
    FTagPosition,
      FTotalTagSize: Cardinal;
    FPadding: Integer;
    FFilename: TFilename;
    FFileStream: TFilestream;
    FOnFrameDecrypt,
      FOnFrameEncrypt: TID3EngineFrameNotify;
    FFrames: TID3Frames;
    FHeader: TID3Header;
    FOnEraseTag,
      FOnFileClose,
      FOnFileOpen,
      FOnReadTag,
      FOnWriteTag: TNotifyevent;
    FFullID3TagSeek: boolean;
    function GetFieldAlbum: string;
    function GetFieldArtist: string;
    function GetFieldEncodedBy: string;
    function GetFieldGenre: string;
    function GetFieldSubTitle: string;
    function GetFieldText(FrameName: TID3FrameName): string;
    function GetFieldTitle: string;
    function GetFieldTrack: string;
    function GetFieldYear: string;
    function IsActive: boolean;
    procedure SetActive(const Value: boolean);
    procedure SetFieldAlbum(const Value: string);
    procedure SetFieldArtist(const Value: string);
    procedure SetFieldEncodedBy(const Value: string);
    procedure SetFieldGenre(const Value: string);
    procedure SetFieldSubTitle(const Value: string);
    procedure SetFieldText(FrameName: TID3FrameName; const Value: string);
    procedure SetFieldTitle(const Value: string);
    procedure SetFieldTrack(const Value: string);
    procedure SetFieldYear(const Value: string);
    procedure SetFilename(const Value: TFileName);
    procedure SetFrames(const Value: TID3Frames);
    procedure SetHeader(const Value: TID3Header);
    procedure SetPadding(const Value: integer);
  protected
    function SeekID3Tag: boolean;
    procedure Clear;
    procedure ReadID3Tag;
    procedure ReadID3V1Tag;
    procedure RemoveID3Tag;
    procedure WriteID3Tag;
    procedure WriteID3V1Tag;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddComment(Language: TLanguageID; Description: string; Comment: string): integer;
    function AddPicture(MIME: string; Description: string; PType: Byte; ImageData: TStream): integer;
    function AddURL(Description, URL: string): integer;
    function CountFrames(FrameName: TID3FrameName): integer;
    function GetComment(var Language: TLanguageID; var Description, Comment: string; index: integer): boolean;
    function GetEncryptionMethod(AMethodID: Byte; var Owner: string; BinData: TStream): boolean;
    function GetGroupingID(AGroupID: Byte; var Owner: string; BinData: TStream): boolean;
    function GetPicture(var MIME, Description: string; var PType: Byte; ImageData: TStream; Index: integer): boolean;
    function GetURL(var Description, URL: string; Index: integer): boolean;
    procedure Commit; virtual;
    procedure Erase; virtual;
    procedure GetCommercialInfo(var Price: string; var ValidUntil: TISODate; var ContactURL: string; var ReceivedAs: Byte; var SellerName, Description, MIME: string; SellerLogoData: TStream);
    procedure GetOwnership(var PricePaid: string; var Date: TISODate; var Seller: string);
    procedure RegisterEncryptionMethod(Owner: string; MethodID: Byte; BinData: TStream);
    procedure RegisterGroupingID(Owner: string; MethodID: Byte; BinData: TStream);
    procedure Rollback; virtual;
    procedure SetCommercialInfo(Price: string; ValidUntil: TISODate; ContactURL: string; ReceivedAs: Byte; SellerName, Description, MIME: string; SellerLogoData: TStream);
    procedure SetOwnership(PricePaid: string; Date: TISODate; Seller: string);
    property TagPosition: Cardinal read FTagPosition;
  published
    property ReadingOnly: boolean read FReadingOnly write FReadingOnly;
    property Active: boolean read IsActive write SetActive stored False;
    property Album: string read GetFieldAlbum write SetFieldAlbum stored False;
    property Artist: string read GetFieldArtist write SetFieldArtist stored False;
    property EncodedBy: string read GetFieldEncodedBy write SetFieldEncodedBy stored False;
    property FileName: TFileName read FFilename write SetFilename;
    property Frames: TID3Frames read FFrames write SetFrames stored False;
    property FullID3TagSeek: boolean read FFullID3TagSeek write FFullID3TagSeek;
    property Genre: string read GetFieldGenre write SetFieldGenre stored False;
    property Header: TID3Header read FHeader write SetHeader stored False;
    property OnEraseTag: TNotifyEvent read FOnEraseTag write FOnEraseTag;
    property OnFileClose: TNotifyEvent read FOnFileClose write FOnFileClose;
    property OnFileOpen: TNotifyEvent read FOnFileOpen write FOnFileOpen;
    property OnFrameDecrypt: TID3EngineFrameNotify read FOnFrameDecrypt write FOnFrameDecrypt;
    property OnFrameEncrypt: TID3EngineFrameNotify read FOnFrameEncrypt write FOnFrameEncrypt;
    property OnReadTag: TNotifyEvent read FOnReadTag write FOnReadTag;
    property OnWriteTag: TNotifyEvent read FOnWriteTag write FOnWriteTag;
    property Padding: integer read FPadding write SetPadding stored False;
    property SubTitle: string read GetFieldSubTitle write SetFieldSubTitle stored False;
    property Title: string read GetFieldTitle write SetFieldTitle stored False;
    property Track: string read GetFieldTrack write SetFieldTrack stored False;
    property WriteID3V1: boolean read FWriteID3V1 write FWriteID3V1;
    property Year: string read GetFieldYear write SetFieldYear stored False;
  end;

  TID3Frames_Item = class(TCollectionItem)
  private
    FCompress,
      FDLI,
      FEncrypted,
      FFileAlterDiscardable,
      FGrouping,
      FReadOnly,
      FTagAlterDiscardable,
      FUnSynchronisation: Boolean;
    FEncryptionID,
      FGroupID: Byte;
    FName: TID3FrameName;
    FParent: TID3Frames;
    FData: TStream;
    function GetName: string;
    procedure SetData(const Value: TStream);
    procedure SetEncryptionID(const Value: Byte);
    procedure SetGroupID(const Value: Byte);
    procedure SetName(const Value: string);
    procedure SetCompress(const Value: boolean);
    function GetDataSize: Integer;
    procedure SetDataSize(const Value: Integer);
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    property Data: TStream read FData write SetData;
  published
    property Compress: boolean read FCompress write SetCompress;
    property Encrypted: boolean read FEncrypted write FEncrypted;
    property EncryptionID: Byte read FEncryptionID write SetEncryptionID;
    property FileAlterDiscardable: boolean read FFileAlterDiscardable write FFileAlterDiscardable;
    property GroupID: Byte read FGroupID write SetGroupID;
    property Grouping: boolean read FGrouping write FGrouping;
    property Name: string read GetName write SetName;
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property TagAlterDiscardable: boolean read FTagAlterDiscardable write FTagAlterDiscardable;
    property UnSynchronisation: boolean read FUnSynchronisation write FUnSynchronisation;
    property DataLengthIndicator: boolean read FDLI write FDLI;
    property DataSize: Integer read GetDataSize write SetDataSize;
    property AsText: string read GetText write SetText stored False;
  end;

  TID3Frames = class(TCollection)
  private
    FOwner: TID3Engine;
    function GetItem(Index: Integer): TID3Frames_Item;
    procedure SetItem(Index: Integer; Value: TID3Frames_Item);
    function GetFrame(FrameName: TID3FrameName): TID3Frames_Item;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TID3Engine);
    function Add: TID3Frames_Item;

    property MainOwner: TID3Engine read FOwner;
    property Items[Index: Integer]: TID3Frames_Item read GetItem write SetItem; default;
    property Frame[FrameName: TID3FrameName]: TID3Frames_Item read GetFrame;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

procedure Register;

var
  ID3V4FrameIDS, ID3Genres: TStrings;

implementation

uses ZLib;

var
  cBits             : array[0..7] of Byte = (1, 2, 4, 8, 16, 32, 64, 128);

type
  TBytesArray = array[0..High(WORD) - 1] of Byte;
  _TAGID3V1 = record
    Header: array[0..2] of Char;
    Album, Artist, Title: array[0..29] of Char;
    Year: array[0..3] of Char;
    Comments: array[0..28] of Char;
    Track, Genre: Byte;
  end;

procedure Register;
begin
  RegisterComponents('HybridGRAF', [TID3Engine]);
end;

{ S - Procedures }

function ValidateFrameName(FrameName: TID3FrameName): boolean;
var
  i                 : Byte;
begin
  Result := True;
  for i := 0 to 3 do
    if not (Ord(FrameName[i]) in [48..57, 65..90]) then
      begin
        Result := False;
        Break;
      end;
end;

function GetTempFile: string;
var
  TDir              : string;
  TD                : PChar;
begin
  TDir := GetTempDir(True);;

  if (TDir[Length(TDir)] <> '\') then TDir := TDir + '\';

  Randomize;
  repeat
    Result := TDir + 'ID3TMP' + IntToHex(Random(2147483647), 2) + '.tmp';
  until not FileExists(Result);
end;

procedure SyncSafe(var Buffer; const BufferSize: Byte; const Decode: Boolean = False); stdcall;
var
  FBuffer           : array of byte;
  BsI, BI           : Byte;
  sI, I             : Byte;
begin
  SetLength(FBuffer, BufferSize);
  FillChar(PByte(FBuffer)^, BufferSize, 0);

  BsI := 0;
  sI := 0;
  for i := 0 to BufferSize - 1 do
    begin
      BI := 0;

      while (BI < 8) do
        begin
          if Decode then
            begin
              if ((TByteArray(Buffer)[si] and cBits[BsI]) = cBits[BsI]) then
                FBuffer[I] := (FBuffer[I] or cBits[BI]);
            end
          else
            if ((TByteArray(Buffer)[i] and cBits[BI]) = cBits[BI]) then
            FBuffer[sI] := (FBuffer[sI] or cBits[BsI]);

          inc(BI);
          inc(BsI);

          if (BI = 7) and (sI < (BufferSize - 1)) then
            begin
              Inc(sI);
              BsI := 0;
            end;
        end;
      TByteArray(Buffer)[i] := FBuffer[i];
    end;
  TByteArray(Buffer)[BufferSize - 1] := (TByteArray(Buffer)[BufferSize - 1] or 128) xor 128;
  SetLength(FBuffer, 0);
end;

procedure StreamToString(var Encoding: Byte; var Str: string; Stream: TStream);
begin
  Encoding := 0;
  Str := '';

  if (Stream.Size > 0) then
    begin
      Stream.Position := 0;
      Stream.Read(Encoding, 1);

      SetLength(Str, Stream.Size - 1);
      Stream.Read(PChar(Str)^, Stream.Size - 1);
      Str := PChar(Str);
      Trim(Str);

      Stream.Position := 0;
    end;
end;

procedure StringToStream(const Encoding: Byte; const Str: string; Stream: TStream);
begin
  Stream.Position := 0;
  Stream.Size := 0;
  Stream.Write(Encoding, 1);
  Stream.Write(PChar(Str)^, Length(Str));
  Stream.Position := 0;
end;

procedure InvertBuffer(var Buffer; Size: Byte);
var
  TA                : array of byte;
  i                 : integer;
begin
  SetLength(TA, Size);
  Dec(Size);

  for i := Size downto 0 do
    TA[Size - i] := TByteArray(Buffer)[i];

  for i := 0 to Size do
    TByteArray(Buffer)[i] := TA[i];

  SetLength(TA, 0);
end;

procedure CopyStream(const Source, Destination: TStream; Amount: Cardinal);
var
  FReadCount        : Cardinal;
  FBufferReadCount  : Cardinal;
  FBuffer           : PByte;
begin
  if (Amount = 0) then
    begin
      Source.Position := 0;
      Amount := Source.Size;
    end;

  if (Source.Position + Amount) > Source.Size then Amount := Source.Size - Source.Position;

  FReadCount := 0;
  FBuffer := GetMemory(cID3BufferMax);
  try
    while (FReadCount < Amount) do
      begin
        if (FReadCount + cID3BufferMax) > Amount then
          FBufferReadCount := Amount - FReadCount
        else
          FBufferReadCount := cID3BufferMax;

        FBufferReadCount := Source.Read(FBuffer^, FBufferReadCount);
        Destination.Write(FBuffer^, FBufferReadCount);

        FReadCount := FReadCount + FBufferReadCount;
      end;
  finally
    FreeMemory(FBuffer);
  end;
end;

procedure CompressStream(Stream: TStream; CompressionLevel: TCompressionLevel);
var
  TCS               : TCompressionStream;
  TFN               : string;
  TFS               : TFileStream;
begin
  TFN := GetTempFile;
  TFS := TFileStream.Create(TFN, fmCreate);
  try
    CopyStream(Stream, TFS, 0);

    Stream.Position := 0;
    Stream.Size := 0;
    TCS := TCompressionStream.Create(CompressionLevel, Stream);
    try
      CopyStream(TFS, TCS, 0);
    finally
      TCS.Free;
    end;
  finally
    TFS.Free;
    DeleteFile(PChar(TFN));
  end;
end;

procedure DecompressStream(Stream: TStream);
var
  TCS               : TDecompressionStream;
  TFN               : string;
  TFS               : TFileStream;
  RC                : Word;
  TB                : array[0..cID3BufferMax - 1] of Byte;
begin
  Stream.Position := 0;

  TFN := GetTempFile;
  TFS := TFileStream.Create(TFN, fmCreate);
  try
    CopyStream(Stream, TFS, 0);

    Stream.Position := 0;
    Stream.Size := 0;
    TFS.Position := 0;
    TCS := TDecompressionStream.Create(TFS);
    try
      repeat
        RC := TCS.Read(TB, cID3BufferMax);
        Stream.Write(TB, RC);
      until (RC < cID3BufferMax);
    finally
      TCS.Free;
    end;
  finally
    TFS.Free;
    DeleteFile(PChar(TFN));
  end;
end;

{ TID3Frames_Item }

procedure TID3Frames_Item.Assign(Source: TPersistent);
begin
  if Source is TID3Frames_Item then
    begin
    end
  else
    inherited;
end;

constructor TID3Frames_Item.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParent := TID3Frames(Collection);
  FData := TMemoryStream.Create;
end;

destructor TID3Frames_Item.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

function TID3Frames_Item.GetDataSize: Integer;
begin
  Result := FData.Size;
end;

function TID3Frames_Item.GetDisplayName: string;
begin
  if (FName = '') then
    Result := inherited GetDisplayName
  else
    begin
      Result := FName;
      if (ID3V4FrameIDS.IndexOfName(Result) >= 0) then Result := ID3V4FrameIDS.Values[Result];
    end;
end;

function TID3Frames_Item.GetName: string;
begin
  Result := StrPas(FName);
end;

function TID3Frames_Item.GetText: string;
var
  EB                : Byte;
begin
  StreamToString(EB, Result, FData);
  FData.Position := 0;
end;

procedure TID3Frames_Item.SetCompress(const Value: boolean);
begin
  FCompress := Value;
  FDLI := True;
end;

procedure TID3Frames_Item.SetData(const Value: TStream);
begin
  FData.Size := Value.Size;
  FData.Position := 0;
  CopyStream(Value, FData, 0);
end;

procedure TID3Frames_Item.SetDataSize(const Value: Integer);
begin

end;

procedure TID3Frames_Item.SetEncryptionID(const Value: Byte);
begin
  FEncryptionID := Value;
  FEncrypted := True;
end;

procedure TID3Frames_Item.SetGroupID(const Value: Byte);
begin
  FGroupID := Value;
  FGrouping := True;
end;

procedure TID3Frames_Item.SetName(const Value: string);
begin
  if (Length(Value) >= 4) then StrPCopy(FName, Value);
end;

procedure TID3Frames_Item.SetText(const Value: string);
begin
  StringToStream(0, Value, FData);
  FData.Position := 0;
end;

{ TID3Frames }

function TID3Frames.Add: TID3Frames_Item;
begin
  Result := TID3Frames_Item(inherited Add);
end;

constructor TID3Frames.Create(AOwner: TID3Engine);
begin
  inherited Create(TID3Frames_Item);
  FOwner := AOwner;
end;

function TID3Frames.GetFrame(FrameName: TID3FrameName): TID3Frames_Item;
var
  i, c              : integer;
begin
  Result := nil;
  c := Count - 1;
  for i := 0 to c do
    if GetItem(i).FName = FrameName then
      begin
        Result := GetItem(i);
        Break;
      end;
end;

function TID3Frames.GetItem(Index: Integer):
  TID3Frames_Item;
begin
  Result := TID3Frames_Item(inherited GetItem(Index));
end;

function TID3Frames.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TID3Frames.LoadFromStream(Stream: TStream);
var
  LP                : Cardinal;
  TS                : Integer;
  FName             : TID3FrameName;
  FFrame            : TID3Frames_Item;
  FFrameSize, FDLI  : integer;
  FFrameFlags       : array[0..1] of byte;
begin
  Clear;

  TS := FOwner.FHeader.FTagSize;
  repeat
    LP := Stream.Position;
    Stream.Read(FName, 4);

    if ValidateFrameName(FName) then
      begin
        FFrame := Add;
        FFrame.FName := FName;

        Stream.Read(FFrameSize, 4);
        InvertBuffer(FFrameSize, 4);
        SyncSafe(FFrameSize, 4, True);

        Stream.Read(FFrameFlags, 2);
        FFrame.FTagAlterDiscardable := (FFrameFlags[0] and 64) = 64;
        FFrame.FFileAlterDiscardable := (FFrameFlags[0] and 32) = 32;
        FFrame.FReadOnly := (FFrameFlags[0] and 16) = 16;

        FFrame.FGrouping := (FFrameFlags[1] and 64) = 64;
        FFrame.FCompress := (FFrameFlags[1] and 8) = 8;
        FFrame.FEncrypted := (FFrameFlags[1] and 4) = 4;
        FFrame.FUnSynchronisation := (FFrameFlags[1] and 2) = 2;
        FFrame.FDLI := (FFrameFlags[1] and 1) = 1;

        if FFrame.FGrouping then Stream.Read(FFrame.FGroupID, 1);
        if FFrame.FEncrypted then Stream.Read(FFrame.FEncryptionID, 1);
        if FFrame.FDLI then Stream.Read(FDLI, 4);

        CopyStream(Stream, FFrame.FData, FFrameSize);

        if FFrame.FEncrypted then
          if Assigned(FOwner.FOnFrameDecrypt) then FOwner.FOnFrameDecrypt(FFrame, FFrame.FData);

        if FFrame.FCompress then DecompressStream(FFrame.FData);

        Dec(TS, (Stream.Position - LP));
      end
    else
      begin
        Stream.Position := TS;
        Break;
      end;
  until (TS <= 0);
  FOwner.SetPadding(TS);
end;

procedure TID3Frames.SaveToStream(Stream: TStream);
var
  LP                : Cardinal;
  FTData            : TMemoryStream;
  FFrame            : TID3Frames_Item;
  FFrameSize, FDLI  : integer;
  FFrameFlags       : array[0..1] of byte;
  i, c              : integer;
begin
  LP := Stream.Position;
  c := (Count - 1);

  FTData := TMemoryStream.Create;
  try
    for i := 0 to c do
      begin
        FFrame := GetItem(i);

        FTData.Position := 0;
        FTData.SetSize(0);
        CopyStream(FFrame.FData, FTData, 0);

        if FFrame.FCompress then CompressStream(FTData, clMax);

        if FFrame.FEncrypted then
          if Assigned(FOwner.FOnFrameEncrypt) then FOwner.FOnFrameEncrypt(FFrame, FTData);

        FFrameSize := FTData.Size;

        { Actual Write }
        Stream.Write(FFrame.FName, 4);

        SyncSafe(FFrameSize, 4);
        InvertBuffer(FFrameSize, 4);
        Stream.Write(FFrameSize, 4);

        // Encode Flags
        FFrameFlags[0] := 0;
        if FFrame.FTagAlterDiscardable then FFrameFlags[0] := (FFrameFlags[0] or 64);
        if FFrame.FFileAlterDiscardable then FFrameFlags[0] := (FFrameFlags[0] or 32);
        if FFrame.FReadOnly then FFrameFlags[0] := (FFrameFlags[0] or 16);

        FFrameFlags[1] := 0;
        if FFrame.FGrouping then FFrameFlags[1] := (FFrameFlags[1] or 64);
        if FFrame.FCompress then FFrameFlags[1] := (FFrameFlags[1] or 8);
        if FFrame.FEncrypted then FFrameFlags[1] := (FFrameFlags[1] or 4);
        if FFrame.FUnSynchronisation then FFrameFlags[1] := (FFrameFlags[1] or 2);
        if FFrame.FDLI then FFrameFlags[1] := (FFrameFlags[1] or 1);

        Stream.Write(FFrameFlags, 2);

        if FFrame.FGrouping then Stream.Write(FFrame.FGroupID, 1);
        if FFrame.FEncrypted then Stream.Write(FFrame.FEncryptionID, 1);
        if FFrame.FDLI then
          begin
            FDLI := FFrame.FData.Size;

            SyncSafe(FDLI, 4);
            InvertBuffer(FDLI, 4);
            Stream.Write(FDLI, 4);
          end;

        FTData.Position := 0;
        CopyStream(FTData, Stream, 0);
      end;
  finally
    FTData.Position := 0;
    FTData.SetSize(0);
    FTData.Free;
  end;
  FOwner.FHeader.FTagSize := (Stream.Position - LP);
end;

procedure TID3Frames.SetItem(Index: Integer; Value: TID3Frames_Item);
begin
  inherited SetItem(Index, Value);
end;

{ TID3Engine }

function TID3Engine.AddComment(Language: TLanguageID; Description, Comment: string): integer;
var
  TB                : Byte;
begin
  Result := -1;
  if not IsActive then Exit;

  Result := CountFrames('COMM');
  with FFrames.Add do
    begin
      FName := 'COMM';
      TB := 0;
      FData.Write(TB, 1);
      FData.Write(Language, 3);
      FData.Write(PChar(Description)^, Length(Description));
      FData.Write(TB, 1);
      FData.Write(PChar(Comment)^, Length(Comment));
      FData.Write(TB, 1);
    end;
end;

function TID3Engine.AddPicture;
var
  TB                : Byte;
begin
  Result := -1;
  if not IsActive then Exit;

  Result := CountFrames('APIC');
  with FFrames.Add do
    begin
      FName := 'APIC';
      TB := 0;
      FData.Write(TB, 1);
      FData.Write(PChar(MIME)^, Length(MIME));
      FData.Write(TB, 1);
      FData.Write(PType, 1);
      FData.Write(PChar(Description)^, Length(Description));
      FData.Write(TB, 1);
      FData.CopyFrom(ImageData, 0);
    end;
end;

function TID3Engine.AddURL(Description, URL: string): integer;
var
  tb                : Byte;
begin
  Result := -1;
  if not IsActive then Exit;

  Result := CountFrames('WXXX');
  with FFrames.Add do
    begin
      FName := 'WXXX';
      tb := 0;
      FData.Write(tb, 1);
      FData.Write(PChar(Description)^, Length(Description));
      FData.Write(tb, 2);
      FData.Write(PChar(URL)^, Length(URL));
    end;
end;

procedure TID3Engine.Clear;
begin
  FHeader.Clear;
  FFrames.Clear;
  FPadding := 0;
  FTotalTagSize := 0;
  FTagPosition := 0;
end;

function TID3Engine.CountFrames(FrameName: TID3FrameName): integer;
var
  i, c              : integer;
begin
  Result := 0;
  c := FFrames.Count - 1;
  for i := 0 to c do
    if FFrames.GetItem(i).FName = FrameName then
      inc(Result);
end;

constructor TID3Engine.Create(AOwner: TComponent);
begin
  inherited;

  ID3Genres := TStringList.Create;
  ID3Genres.Add('(0) Blues');
  ID3Genres.Add('(1) Classic Rock');
  ID3Genres.Add('(2) Country');
  ID3Genres.Add('(3) Dance');
  ID3Genres.Add('(4) Disco');
  ID3Genres.Add('(5) Funk');
  ID3Genres.Add('(6) Grunge');
  ID3Genres.Add('(7) Hip - Hop');
  ID3Genres.Add('(8) Jazz');
  ID3Genres.Add('(9) Metal');
  ID3Genres.Add('(10) New Age');
  ID3Genres.Add('(11) Oldies');
  ID3Genres.Add('(12) Other');
  ID3Genres.Add('(13) Pop');
  ID3Genres.Add('(14) R&B');
  ID3Genres.Add('(15) Rap');
  ID3Genres.Add('(16) Reggae');
  ID3Genres.Add('(17) Rock');
  ID3Genres.Add('(18) Techno');
  ID3Genres.Add('(19) Industrial');
  ID3Genres.Add('(20) Alternative');
  ID3Genres.Add('(21) Ska');
  ID3Genres.Add('(22) Death Metal');
  ID3Genres.Add('(23) Pranks');
  ID3Genres.Add('(24) Soundtrack');
  ID3Genres.Add('(25) Euro - Techno');
  ID3Genres.Add('(26) Ambient');
  ID3Genres.Add('(27) Trip - Hop');
  ID3Genres.Add('(28) Vocal');
  ID3Genres.Add('(29) Jazz + Funk');
  ID3Genres.Add('(30) Fusion');
  ID3Genres.Add('(31) Trance');
  ID3Genres.Add('(32) Classical');
  ID3Genres.Add('(33) Instrumental');
  ID3Genres.Add('(34) Acid');
  ID3Genres.Add('(35) House');
  ID3Genres.Add('(36) Game');
  ID3Genres.Add('(37) Sound Clip');
  ID3Genres.Add('(38) Gospel');
  ID3Genres.Add('(39) Noise');
  ID3Genres.Add('(40) AlternRock');
  ID3Genres.Add('(41) Bass');
  ID3Genres.Add('(42) Soul');
  ID3Genres.Add('(43) Punk');
  ID3Genres.Add('(44) Space');
  ID3Genres.Add('(45) Meditative');
  ID3Genres.Add('(46) Instrumental Pop');
  ID3Genres.Add('(47) Instrumental Rock');
  ID3Genres.Add('(48) Ethnic');
  ID3Genres.Add('(49) Gothic');
  ID3Genres.Add('(50) Darkwave');
  ID3Genres.Add('(51) Techno - Industrial');
  ID3Genres.Add('(52) Electronic');
  ID3Genres.Add('(53) Pop - Folk');
  ID3Genres.Add('(54) Eurodance');
  ID3Genres.Add('(55) Dream');
  ID3Genres.Add('(56) Southern Rock');
  ID3Genres.Add('(57) Comedy');
  ID3Genres.Add('(58) Cult');
  ID3Genres.Add('(59) Gangsta');
  ID3Genres.Add('(60) Top 40');
  ID3Genres.Add('(61) Christian Rap');
  ID3Genres.Add('(62) Pop / Funk');
  ID3Genres.Add('(63) Jungle');
  ID3Genres.Add('(64) Native American');
  ID3Genres.Add('(65) Cabaret');
  ID3Genres.Add('(66) New Wave');
  ID3Genres.Add('(67) Psychadelic');
  ID3Genres.Add('(68) Rave');
  ID3Genres.Add('(69) Showtunes');
  ID3Genres.Add('(70) Trailer');
  ID3Genres.Add('(71) Lo - Fi');
  ID3Genres.Add('(72) Tribal');
  ID3Genres.Add('(73) Acid Punk');
  ID3Genres.Add('(74) Acid Jazz');
  ID3Genres.Add('(75) Polka');
  ID3Genres.Add('(76) Retro');
  ID3Genres.Add('(77) Musical');
  ID3Genres.Add('(78) Rock & Roll');
  ID3Genres.Add('(79) Hard Rock');

  { FRAMES }
  ID3V4FrameIDS := TStringList.Create;
  ID3V4FrameIDS.Add('COMM=Comments');
  ID3V4FrameIDS.Add('EQU2=Equalisation (2)');
  ID3V4FrameIDS.Add('ETCO=Event timing codes');
  ID3V4FrameIDS.Add('GEOB=General encapsulated object');
  ID3V4FrameIDS.Add('LINK=Linked information');
  ID3V4FrameIDS.Add('MCDI=Music CD identifier');
  ID3V4FrameIDS.Add('MLLT=MPEG location lookup table');
  ID3V4FrameIDS.Add('PCNT=Play counter');
  ID3V4FrameIDS.Add('POPM=Popularimeter');
  ID3V4FrameIDS.Add('PRIV=Private frame');
  ID3V4FrameIDS.Add('RBUF=Recommended buffer size');
  ID3V4FrameIDS.Add('RVA2=Relative volume adjustment (2)');
  ID3V4FrameIDS.Add('RVRB=Reverb');
  ID3V4FrameIDS.Add('SIGN=Signature frame');
  ID3V4FrameIDS.Add('SYLT=Synchronised lyric/text');
  ID3V4FrameIDS.Add('SYTC=Synchronised tempo codes');
  ID3V4FrameIDS.Add('TALB=Album');
  ID3V4FrameIDS.Add('TBPM=BPM (beats per minute)');
  ID3V4FrameIDS.Add('TCOM=Composer');
  ID3V4FrameIDS.Add('TCON=Content type');
  ID3V4FrameIDS.Add('TCOP=Copyright message');
  ID3V4FrameIDS.Add('TDEN=Encoding time');
  ID3V4FrameIDS.Add('TDLY=Playlist delay');
  ID3V4FrameIDS.Add('TDOR=Original release time');
  ID3V4FrameIDS.Add('TDRC=Recording time');
  ID3V4FrameIDS.Add('TDRL=Release time');
  ID3V4FrameIDS.Add('TDTG=Tagging time');
  ID3V4FrameIDS.Add('TENC=Encoded by');
  ID3V4FrameIDS.Add('TEXT=Lyricist/Text writer');
  ID3V4FrameIDS.Add('TFLT=File type');
  ID3V4FrameIDS.Add('TIPL=Involved people list');
  ID3V4FrameIDS.Add('TIT1=Content group description');
  ID3V4FrameIDS.Add('TIT2=Title');
  ID3V4FrameIDS.Add('TIT3=Subtitle');
  ID3V4FrameIDS.Add('TKEY=Initial key');
  ID3V4FrameIDS.Add('TLAN=Language(s)');
  ID3V4FrameIDS.Add('TLEN=Length');
  ID3V4FrameIDS.Add('TMCL=Musician credits list');
  ID3V4FrameIDS.Add('TMED=Media type');
  ID3V4FrameIDS.Add('TMOO=Mood');
  ID3V4FrameIDS.Add('TOAL=Original album');
  ID3V4FrameIDS.Add('TOFN=Original filename');
  ID3V4FrameIDS.Add('TOLY=Original lyricist(s)/text writer(s)');
  ID3V4FrameIDS.Add('TOPE=Original artist(s)/performer(s)');
  ID3V4FrameIDS.Add('TOWN=File owner/licensee');
  ID3V4FrameIDS.Add('TPE1=Lead performer(s)/Soloist(s)');
  ID3V4FrameIDS.Add('TPE2=Band');
  ID3V4FrameIDS.Add('TPE3=Conductor/performer refinement');
  ID3V4FrameIDS.Add('TPE4=Interpreted, remixed, or otherwise modified by');
  ID3V4FrameIDS.Add('TPOS=Part of a set');
  ID3V4FrameIDS.Add('TPRO=Produced notice');
  ID3V4FrameIDS.Add('TPUB=Publisher');
  ID3V4FrameIDS.Add('TRCK=Track number/Position in set');
  ID3V4FrameIDS.Add('TRSN=Internet radio station name');
  ID3V4FrameIDS.Add('TRSO=Internet radio station owner');
  ID3V4FrameIDS.Add('TSOA=Album sort order');
  ID3V4FrameIDS.Add('TSOP=Performer sort order');
  ID3V4FrameIDS.Add('TSOT=Title sort order');
  ID3V4FrameIDS.Add('TSRC=ISRC (international standard recording code)');
  ID3V4FrameIDS.Add('TSSE=Software/Hardware and settings used for encoding');
  ID3V4FrameIDS.Add('TSST=Set subtitle');
  ID3V4FrameIDS.Add('TXXX=User defined text information frame');
  ID3V4FrameIDS.Add('TYER=Year');
  ID3V4FrameIDS.Add('UFID=Unique file identifier');
  ID3V4FrameIDS.Add('USER=Terms of use');
  ID3V4FrameIDS.Add('USLT=Unsynchronised lyric/text transcription');
  ID3V4FrameIDS.Add('WCOM=Commercial information');
  ID3V4FrameIDS.Add('WCOP=Copyright/Legal information');
  ID3V4FrameIDS.Add('WOAF=Official audio file webpage');
  ID3V4FrameIDS.Add('WOAR=Official artist/performer webpage');
  ID3V4FrameIDS.Add('WOAS=Official audio source webpage');
  ID3V4FrameIDS.Add('WORS=Official Internet radio station homepage');
  ID3V4FrameIDS.Add('WPAY=Payment');
  ID3V4FrameIDS.Add('WPUB=Publishers official webpage');
  ID3V4FrameIDS.Add('WXXX=User defined URL link frame');

  FReadingOnly := False;

  FHeader := TID3Header.Create(Self);
  FFrames := TID3Frames.Create(Self);
end;

destructor TID3Engine.Destroy;
begin
  FFrames.Free;
  FHeader.Free;

  ID3V4FrameIDS.Free;
  ID3Genres.Free;

  inherited;
end;


procedure TID3Engine.Erase;
begin
  if not IsActive then Exit;

  RemoveID3Tag;
  if Assigned(FOnEraseTag) then FOnEraseTag(Self);
end;

function TID3Engine.GetComment(var Language: TLanguageID; var Description, Comment: string; index: integer): boolean;
var
  i, c, fc          : integer;
  TB                : Byte;
begin
  Result := False;
  fc := -1;

  Comment := '';
  Description := '';
  Language := '';

  if not IsActive then Exit;

  c := FFrames.Count - 1;
  for i := 0 to c do
    if FFrames.Items[i].FName = 'COMM' then
      begin
        Inc(fc);
        if (fc = Index) then
          with FFrames.Items[i] do
            begin
              FData.Position := 0;
              FData.Read(TB, 1);
              FData.Read(Language, 3);
              FData.Read(TB, 1);
              repeat
                Description := Description + Char(TB);
                FData.Read(TB, 1);
              until (TB = 0);
              FData.Read(TB, 1);
              if TB <> 0 then FData.Position := FData.Position - 1;
              SetLength(Comment, FData.Size - FData.Position);
              FData.Read(PChar(Comment)^, FData.Size - FData.Position);
              Comment := PChar(Comment);
            end;
      end;
end;

function TID3Engine.GetFieldAlbum: string;
begin
  Result := GetFieldText('TALB');
end;

function TID3Engine.GetFieldArtist: string;
begin
  Result := GetFieldText('TPE1');
end;

function TID3Engine.GetFieldEncodedBy: string;
begin
  Result := GetFieldText('TENC');
end;

function TID3Engine.GetFieldGenre: string;
begin
  Result := GetFieldText('TIT1');
end;

function TID3Engine.GetFieldTitle: string;
begin
  Result := GetFieldText('TIT2');
end;

function TID3Engine.GetFieldTrack: string;
begin
  Result := GetFieldText('TRCK');
end;

function TID3Engine.GetFieldYear: string;
begin
  Result := GetFieldText('TYER');
end;

procedure TID3Engine.GetOwnership;
var
  EB                : Byte;
  FF                : TID3Frames_Item;
begin
  PricePaid := '';
  Date := '';
  Seller := '';

  FF := FFrames.Frame['OWNE'];
  if (FF <> nil) then
    begin
      FF.FData.Position := 0;
      FF.FData.Read(EB, 1);
      FF.FData.Read(EB, 1);
      repeat
        PricePaid := PricePaid + Char(EB);
        FF.FData.Read(EB, 1);
      until (EB = 0);
      PricePaid := PChar(PricePaid);

      FF.FData.Read(Date, 8);

      SetLength(Seller, FF.FData.Size - FF.FData.Position);
      FF.FData.Read(PChar(Seller)^, FF.FData.Size - FF.FData.Position);
      Seller := PChar(Seller);
    end;
end;

function TID3Engine.GetPicture;
var
  i, c, fc          : integer;
  TB                : Byte;
begin
  Result := False;
  fc := -1;

  if not IsActive then Exit;

  c := FFrames.Count - 1;
  for i := 0 to c do
    if FFrames.Items[i].FName = 'APIC' then
      begin
        Inc(fc);
        Mime := '';
        if (fc = Index) then
          with FFrames.Items[i] do
            begin
              FData.Position := 0;
              FData.Read(TB, 1);
              FData.Read(TB, 1);
              repeat
                Mime := Mime + Char(TB);
                FData.Read(TB, 1);
              until (TB = 0);
              Mime := LowerCase(PChar(Mime));
              FData.Read(PType, 1);

              FData.Read(TB, 1);
              repeat
                Description := Description + Char(TB);
                FData.Read(TB, 1);
              until (TB = 0);

              ImageData.Position := 0;
              ImageData.Size := 0;
              ImageData.CopyFrom(FData, FData.Size - FData.Position);
              Result := True;
            end;
      end;
end;

function TID3Engine.GetURL(var Description, URL: string; Index: integer): boolean;
var
  rb                : byte;
  i, c, fc          : integer;
begin
  Result := False;
  fc := -1;

  if not IsActive then Exit;

  Description := '';
  URL := '';

  c := FFrames.Count - 1;
  for i := 0 to c do
    if (FFrames.Items[i].FName = 'WXXX') then
      begin
        inc(fc);
        if (fc = index) then
          with FFrames.Items[i] do
            begin
              fc := 0;
              Result := True;
              FData.Position := 0;
              FData.Read(rb, 1);
              FData.Read(rb, 1);
              repeat
                Description := Description + Char(rb);
                FData.Read(rb, 1);
              until (rb = 0);
              Description := PChar(Description);
              FData.Read(rb, 1);
              if (rb <> 0) then FData.Position := FData.Position - 1;
              SetLength(URL, FData.Size - FData.Position);
              FData.Read(PChar(URL)^, FData.Size - FData.Position);
              URL := PChar(URL);
            end;
      end;

end;

function TID3Engine.IsActive: boolean;
begin
  Result := Assigned(FFileStream);
end;

procedure TID3Engine.Commit;
begin
  if not IsActive then Exit;

  WriteID3Tag;
  if Assigned(FOnWriteTag) then FOnWriteTag(Self);
end;

procedure TID3Engine.ReadID3Tag;
begin
  if IsActive then
    begin
      Clear;

      if not SeekID3Tag then FTagPosition := 0;
      FFileStream.Position := FTagPosition;
      if FHeader.LoadFromStream(FFileStream) then
        begin
          FFrames.LoadFromStream(FFileStream);
          if FHeader.FFooterPresent then FPadding := 0;
        end
      else
        ReadID3V1Tag;

      FTotalTagSize := (FFileStream.Position - FTagPosition) + FPadding;
      if Assigned(FOnReadTag) then FOnReadTag(Self);
    end;
end;

procedure TID3Engine.ReadID3V1Tag;
var
  ID3V1TAG          : _TAGID3V1;
begin
  if IsActive then
    begin
      FFileStream.Position := (FFileStream.Size - 128);
      FFileStream.Read(ID3V1TAG, 128);

      if (ID3V1TAG.Header = 'TAG') then
        begin
          Album := Trim(ID3V1TAG.Album);
          Artist := Trim(ID3V1TAG.Artist);
          Title := Trim(ID3V1TAG.Title);
          Year := Trim(ID3V1TAG.Year);
          AddComment('ENG', 'MAIN', Trim(ID3V1TAG.Comments));

          try
            Track := IntToStr(ID3V1TAG.Track);
          except
            Track := '';
          end;

          if (ID3V1TAG.Genre < 80) then Genre := ID3Genres.Strings[ID3V1TAG.Genre];

          FHeader.FExtendedHeaderPresent := True;
          FHeader.FExtendedHeader.FTagIsAnUpdate := True;
          FPadding := 250;
        end;
    end;
end;

procedure TID3Engine.RemoveID3Tag;
var
  TFN               : string;
  TT                : TFileStream;
begin
  if not IsActive then Exit;

  if (FHeader.FTagSize > 0) then
    begin
      TFN := GetTempFile;
      TT := TFileStream.Create(TFN, fmCreate);
      try
        if (FTagPosition > 0) then
          begin
            FFileStream.Position := 0;
            CopyStream(FFileStream, TT, FTagPosition);
          end;

        FFileStream.Position := FTagPosition + FTotalTagSize;
        CopyStream(FFileStream, TT, FFileStream.Size - FFileStream.Position);

        FFileStream.Position := 0;
        FFileStream.Size := TT.Size;
        FFileStream.Position := 0;

        CopyStream(TT, FFileStream, 0);
      finally
        TT.Free;
        DeleteFile(PChar(TFN));
      end;
    end;
end;

procedure TID3Engine.Rollback;
begin
  if not IsActive then Exit;

  ReadID3Tag;
end;

procedure TID3Engine.SetActive(const Value: boolean);
begin
  if IsActive and not Value then
    begin
{
      if csDesigning in ComponentState then
        if MessageBox(0, 'Commit Changes?', 'Debugger', MB_YESNO) = idYES then Commit;
}
      if (FFileStream <> nil) then FreeAndNil(FFileStream);
      Clear;

      if Assigned(FOnFileClose) then FOnFileClose(Self);
    end
  else
    if not IsActive and Value then
    if FileExists(FFilename) then
      begin
        if FReadingOnly then
          FFileStream := TFileStream.Create(FFilename, fmOpenRead or fmShareDenyNone)
        else
          FFileStream := TFileStream.Create(FFilename, fmOpenReadWrite or fmShareDenyNone);

        ReadID3Tag;
        if IsActive then
          if Assigned(FOnFileOpen) then FOnFileOpen(Self);
      end;
end;

procedure TID3Engine.SetFieldAlbum(const Value: string);
begin
  SetFieldText('TALB', Value);
end;

procedure TID3Engine.SetFieldArtist(const Value: string);
begin
  SetFieldText('TPE1', Value);
end;

procedure TID3Engine.SetFieldEncodedBy(const Value: string);
begin
  SetFieldText('TENC', Value);
end;

procedure TID3Engine.SetFieldGenre(const Value: string);
begin
  SetFieldText('TIT1', Value);
end;

procedure TID3Engine.SetFieldTitle(const Value: string);
begin
  SetFieldText('TIT2', Value);
end;

procedure TID3Engine.SetFieldTrack(const Value: string);
begin
  SetFieldText('TRCK', Value);
end;

procedure TID3Engine.SetFieldYear(const Value: string);
begin
  SetFieldText('TYER', Value);
end;

procedure TID3Engine.SetFilename(const Value: TFileName);
begin
  if not IsActive then FFilename := Value;
end;

procedure TID3Engine.SetFrames(const Value: TID3Frames);
begin
  FFrames.Assign(Value);
end;

procedure TID3Engine.SetHeader(const Value: TID3Header);
begin
  FHeader.Assign(Value);
end;

procedure TID3Engine.SetOwnership;
var
  EB                : Byte;
  FF                : TID3Frames_Item;
begin
  FF := FFrames.Frame['OWNE'];
  if (FF = nil) then
    begin
      FF := FFrames.Add;
      FF.FName := 'OWNE';
    end;
  FF.FData.Position := 0;
  FF.FData.Size := 0;

  EB := 0;
  FF.FData.Write(EB, 1);
  FF.FData.Write(PChar(PricePaid)^, Length(PricePaid));
  FF.FData.Write(EB, 1);

  FF.FData.Write(Date, 8);
  FF.FData.Write(PChar(Seller)^, Length(Seller));
  FF.FData.Write(EB, 1);
end;

procedure TID3Engine.SetPadding(const Value: integer);
begin
  if FHeader.FFooterPresent or (Value < 0) then
    FPadding := 0
  else
    FPadding := Value;
end;

procedure TID3Engine.WriteID3Tag;
var
  TTS, TS           : Integer;
  TSPos             : Word;
  TFN               : string;
  TT                : TFileStream;
begin
  if IsActive then
    begin
      FFileStream.Position := FTagPosition;

      TS := FHeader.FTagSize;

      TFN := GetTempFile;
      TT := TFileStream.Create(TFN, fmCreate);
      try
        TSPos := FHeader.SaveToStream(TT);
        FFrames.SaveToStream(TT);

        if FHeader.FFooterPresent then
          FHeader.SaveToStream(TT, True)
        else
          TT.Size := TT.Size + FPadding;

        FHeader.FTagSize := TT.Size;
        Dec(FHeader.FTagSize, 10);
        if FHeader.FFooterPresent then Dec(FHeader.FTagSize, 10);

        TT.Position := TSPos;
        TTS := FHeader.FTagSize;
        SyncSafe(TTS, 4);
        InvertBuffer(TTS, 4);
        TT.Write(TTS, 4);

        TT.Position := 0;

        { Write to File }
        if (TS > 0) and (TS < FHeader.FTagSize) then
          begin
            RemoveID3Tag;

            TT.Position := TT.Size;

            FFileStream.Position := 0;
            CopyStream(FFileStream, TT, 0);

            FFileStream.Position := 0;
            FTagPosition := 0;
          end
        else
          begin
            FFileStream.Position := FTagPosition;
            TT.Position := 0;
          end;

        CopyStream(TT, FFileStream, 0);
        FTotalTagSize := TT.Size;
      finally
        TT.Free;
        DeleteFile(PChar(TFN));
      end;

      if FWriteID3V1 then WriteID3V1Tag;
    end;
end;

procedure TID3Engine.WriteID3V1Tag;
var
  ID3V1TAG          : _TAGID3V1;
  ts                : string;
  r1, r2            : byte;
begin
  if IsActive then
    begin
      FFileStream.Position := (FFileStream.Size - 128);
      FillChar(ID3V1TAG, SizeOf(_TAGID3V1), 0);

      FFileStream.Read(ID3V1TAG, 128);

      if (ID3V1TAG.Header = 'TAG') then
        FFileStream.Position := (FFileStream.Size - 128)
      else
        FFileStream.Position := FFileStream.Size;

      FillChar(ID3V1TAG, SizeOf(_TAGID3V1), 0);

      StrPCopy(ID3V1TAG.Album, Copy(Album, 1, 30));
      StrPCopy(ID3V1TAG.Artist, Copy(Artist, 1, 30));
      StrPCopy(ID3V1TAG.Title, Copy(Title, 1, 30));
      StrPCopy(ID3V1TAG.Year, Copy(Year, 1, 4));

      try
        ID3V1TAG.Track := StrToInt(Track);
      except
        ID3V1TAG.Track := 0;
      end;

      ts := Genre;
      r1 := pos('(', ts);
      r2 := pos(')', ts);
      if (r1 > 0) and (r2 > 0) then
        ts := Copy(ts, r1 + 1, r2 - (r1 + 1))
      else
        ts := '0';

      try
        ID3V1TAG.Genre := StrToInt(ts);
      except
        ID3V1TAG.Genre := 0;
      end;

      ID3V1TAG.Header := 'TAG';
      FFileStream.Write(ID3V1TAG, 128);
    end;
end;

function TID3Engine.GetFieldSubTitle: string;
begin
  Result := GetFieldText('TIT3');
end;

procedure TID3Engine.SetFieldSubTitle(const Value: string);
begin
  SetFieldText('TIT3', Value);
end;

procedure TID3Engine.SetFieldText(FrameName: TID3FrameName; const Value: string);
var
  FF                : TID3Frames_Item;
begin
  if not IsActive then Exit;

  FF := FFrames.Frame[FrameName];
  if (FF = nil) then
    begin
      FF := FFrames.Add;
      FF.FName := FrameName
    end;

  StringToStream(0, Value, FF.FData);
end;

function TID3Engine.GetFieldText(FrameName: TID3FrameName): string;
var
  E                 : Byte;
  FF                : TID3Frames_Item;
begin
  if IsActive then
    begin
      FF := FFrames.Frame[FrameName];
      if (FF <> nil) then StreamToString(E, Result, FF.FData);
    end
  else
    Result := '';
end;

function TID3Engine.SeekID3Tag: boolean;
var
  FBuffer           : array[0..cID3BufferMax - 1] of byte;
  i, RB             : Word;
  CI, IP            : Cardinal;
  I_INDEX           : array of cardinal;
  HN                : array[0..2] of char;
begin
  Result := False;
  FFileStream.Position := 0;
  FFileStream.Read(HN, 3);

  if (HN = 'ID3') then
    begin
      FFileStream.Read(HN, 1);
      if (Ord(HN[0]) <= 4) then
        begin
          Result := True;
          Exit;
        end;
    end;

  FFileStream.Position := 0;

  if FFullID3TagSeek then
    begin
      CI := 0;
      SetLength(I_INDEX, 0);

      repeat
        RB := FFileStream.Read(FBuffer, cID3BufferMax);
        for i := 0 to RB - 1 do
          begin
            SetLength(I_INDEX, CI + 1);
            I_INDEX[CI] := FFileStream.Position;
            Inc(CI);
          end;
        IP := FFileStream.Position;

        for i := 0 to CI - 1 do
          begin
            FFileStream.Position := I_INDEX[i];
            FFileStream.Read(HN, 3);
            if (HN = 'ID3') then
              begin
                FFileStream.Read(HN, 1);
                if (Ord(HN[0]) <= 4) then
                  begin
                    FTagPosition := I_INDEX[i];
                    FFileStream.Position := FTagPosition;
                    Result := True;
                    Break;
                    Break;
                  end;
              end;
          end;
        FFileStream.Position := IP;

        CI := 0;
        SetLength(I_INDEX, 0);
      until (RB < cID3BufferMax);
      SetLength(I_INDEX, 0);
    end;
end;

procedure TID3Engine.RegisterGroupingID(Owner: string; MethodID: Byte; BinData: TStream);
var
  TB                : Byte;
begin
  with FFrames.Add do
    begin
      FName := 'GRID';
      TB := 0;
      FData.Write(PChar(Owner)^, Length(Owner));
      FData.Write(TB, 1);
      FData.Write(MethodID, 1);
      FData.CopyFrom(BinData, 0);
      FData.Position := 0;
    end;
end;

function TID3Engine.GetGroupingID(AGroupID: Byte; var Owner: string; BinData: TStream): boolean;
var
  i, c              : integer;
  FOwner            : string;
  TB                : Byte;
begin
  Result := False;

  Owner := '';
  BinData.Position := 0;
  BinData.Size := 0;

  if not IsActive then Exit;

  c := FFrames.Count - 1;
  for i := 0 to c do
    if FFrames.Items[i].FName = 'GRID' then
      with FFrames.Items[i] do
        begin
          FOwner := '';
          FData.Position := 0;
          FData.Read(TB, 1);
          FData.Read(TB, 1);
          repeat
            FOwner := FOwner + Char(TB);
            FData.Read(TB, 1);
          until (TB = 0);
          FData.Read(TB, 1);
          if (TB = AGroupID) then
            begin
              Owner := PChar(FOwner);
              BinData.CopyFrom(FData, FData.Size - FData.Position);
              Result := True;
              Break;
            end;
          FData.Position := 0;
        end;
end;


procedure TID3Engine.RegisterEncryptionMethod(Owner: string; MethodID: Byte; BinData: TStream);
var
  TB                : Byte;
begin
  CountFrames('ENCR');

  with FFrames.Add do
    begin
      FName := 'ENCR';
      TB := 0;
      FData.Write(PChar(Owner)^, Length(Owner));
      FData.Write(TB, 1);
      FData.Write(MethodID, 1);
      FData.CopyFrom(BinData, 0);
      FData.Position := 0;
    end;
end;

function TID3Engine.GetEncryptionMethod(AMethodID: Byte; var Owner: string; BinData: TStream): boolean;
var
  i, c              : integer;
  FOwner            : string;
  TB                : Byte;
begin
  Result := False;

  Owner := '';
  BinData.Position := 0;
  BinData.Size := 0;

  if not IsActive then Exit;

  c := FFrames.Count - 1;
  for i := 0 to c do
    if FFrames.Items[i].FName = 'ENCR' then
      with FFrames.Items[i] do
        begin
          FOwner := '';
          FData.Position := 0;
          FData.Read(TB, 1);
          FData.Read(TB, 1);
          repeat
            FOwner := FOwner + Char(TB);
            FData.Read(TB, 1);
          until (TB = 0);
          FData.Read(TB, 1);
          if (TB = AMethodID) then
            begin
              Owner := PChar(FOwner);
              BinData.CopyFrom(FData, FData.Size - FData.Position);
              Result := True;
              Break;
            end;
          FData.Position := 0;
        end;
end;

procedure TID3Engine.SetCommercialInfo(Price: string; ValidUntil: TISODate;
  ContactURL: string; ReceivedAs: Byte; SellerName, Description,
  MIME: string; SellerLogoData: TStream);
var
  EB                : Byte;
  FF                : TID3Frames_Item;
begin
  MIME := LowerCase(MIME);

  FF := FFrames.Frame['COMR'];
  if (FF = nil) then
    begin
      FF := FFrames.Add;
      FF.FName := 'COMR';
    end;

  FF.FData.Position := 0;
  FF.FData.Size := 0;

  EB := 0;
  FF.FData.Write(EB, 1);
  FF.FData.Write(PChar(Price)^, Length(Price));
  FF.FData.Write(EB, 1);
  FF.FData.Write(ValidUntil, 8);
  FF.FData.Write(PChar(ContactURL)^, Length(ContactURL));
  FF.FData.Write(EB, 1);
  FF.FData.Write(ReceivedAs, 1);
  FF.FData.Write(PChar(SellerName)^, Length(SellerName));
  FF.FData.Write(EB, 1);
  FF.FData.Write(PChar(Description)^, Length(Description));
  FF.FData.Write(EB, 1);
  FF.FData.Write(PChar(MIME)^, Length(MIME));
  FF.FData.Write(EB, 1);
  FF.FData.CopyFrom(SellerLogoData, 0);
end;

procedure TID3Engine.GetCommercialInfo(var Price: string;
  var ValidUntil: TISODate; var ContactURL: string; var ReceivedAs: Byte;
  var SellerName, Description, MIME: string; SellerLogoData: TStream);
var
  EB                : Byte;
  FF                : TID3Frames_Item;
begin
  Price := '';
  ValidUntil := '00000000';
  ContactURL := '';
  ReceivedAs := 0;
  SellerName := '';
  Description := '';
  MIME := '';

  FF := FFrames.Frame['COMR'];
  if (FF <> nil) then
    begin
      FF.FData.Position := 0;

      EB := 0;
      FF.FData.Read(EB, 1);

      FF.FData.Read(EB, 1);
      repeat
        Price := Price + Char(EB);
        FF.FData.Read(EB, 1);
      until (EB = 0);

      FF.FData.Read(ValidUntil, 8);

      FF.FData.Read(EB, 1);
      repeat
        ContactURL := ContactURL + Char(EB);
        FF.FData.Read(EB, 1);
      until (EB = 0);

      FF.FData.Read(ReceivedAs, 1);

      FF.FData.Read(EB, 1);
      repeat
        SellerName := SellerName + Char(EB);
        FF.FData.Read(EB, 1);
      until (EB = 0);

      FF.FData.Read(EB, 1);
      repeat
        Description := Description + Char(EB);
        FF.FData.Read(EB, 1);
      until (EB = 0);

      FF.FData.Read(EB, 1);
      repeat
        MIME := MIME + Char(EB);
        FF.FData.Read(EB, 1);
      until (EB = 0);
      MIME := LowerCase(PChar(MIME));

      SellerLogoData.Position := 0;
      SellerLogoData.Size := 0;
      SellerLogoData.CopyFrom(FF.FData, FF.FData.Size - FF.FData.Position);
    end;
end;


{ TID3Header }

procedure TID3Header.Clear;
begin
  FExperimental := False;
  FExtendedHeader.Clear;
  FExtendedHeaderPresent := False;
  FFooterPresent := False;
  FVersionNumber := 4;
  FVersionRevision := 0;
  FTagSize := 0;
end;

constructor TID3Header.Create(AOwner: TID3Engine);
begin
  inherited Create;
  FOwner := AOwner;
  FExtendedHeader := TID3ExtendedHeader.Create;
end;

destructor TID3Header.Destroy;
begin
  FExtendedHeader.Free;
  inherited;
end;

function TID3Header.LoadFromStream(const Stream: TStream): boolean;
var
  FHeader           : array[0..5] of Byte;
  FExtendedSize     : integer;
begin
  Clear;

  Result := False;
  FTagSize := -1;

  Stream.Read(FHeader, 6);
  if (FHeader[0] = 73) and (FHeader[1] = 68) and (FHeader[2] = 51) and (FHeader[3] <= 4) then
    begin
      Result := True;
      FVersionNumber := FHeader[3];
      FVersionRevision := FHeader[4];

      FUnsynchronisation := (FHeader[5] and 128) = 128;
      FExtendedHeaderPresent := (FHeader[5] and 64) = 64;
      FExperimental := (FHeader[5] and 32) = 32;
      FFooterPresent := (FHeader[5] and 16) = 16;

      Stream.Read(FTagSize, 4);
      InvertBuffer(FTagSize, 4);
      SyncSafe(FTagSize, 4, True);

      if FExtendedHeaderPresent then
        begin
          Stream.Read(FExtendedSize, 4);
          InvertBuffer(FExtendedSize, 4);
          SyncSafe(FExtendedSize, 4, True);

          Stream.Read(FHeader[0], 1);
          Stream.Read(FHeader[0], 1);
          FExtendedHeader.FTagIsAnUpdate := (FHeader[0] and 64) = 64;
          FExtendedHeader.FCRCDataPresent := (FHeader[0] and 32) = 32;
          FExtendedHeader.FTagRestricted := (FHeader[0] and 16) = 16;

          if FExtendedHeader.FCRCDataPresent then
            begin
              Stream.Read(FExtendedHeader.FCRCData, 5);
              InvertBuffer(FExtendedHeader.FCRCData, 5);
              SyncSafe(FExtendedHeader.FCRCData, 5, True);
            end;

          if FExtendedHeader.FTagRestricted then
            begin
              Stream.Read(FHeader[0], 1);
              FExtendedHeader.FTagRestrictions.LoadFromByte(FHeader[0]);
            end;
        end;
    end;
end;

function TID3Header.SaveToStream(const Stream: TStream; const WriteFooter: boolean = False): Cardinal;
var
  FHeaderFlags      : Byte;
  FCRC              : TCRCData;
  FExtendedSize     : integer;
begin
  if WriteFooter then
    Stream.Write('3DI', 3)
  else
    Stream.Write('ID3', 3);

  Stream.Write(FVersionNumber, 1);
  Stream.Write(FVersionRevision, 1);

  FHeaderFlags := 0;
  if FUnsynchronisation then FHeaderFlags := (FHeaderFlags or 128);
  if FExtendedHeaderPresent then FHeaderFlags := (FHeaderFlags or 64);
  if FExperimental then FHeaderFlags := (FHeaderFlags or 32);
  if FFooterPresent then FHeaderFlags := (FHeaderFlags or 16);

  Stream.Write(FHeaderFlags, 1);

  Result := Stream.Position;

  if not WriteFooter then FTagSize := 0;
  Stream.Write(FTagSize, 4);

  if FExtendedHeaderPresent and not WriteFooter then
    begin
      FExtendedSize := 6;
      if FExtendedHeader.FCRCDataPresent then inc(FExtendedSize, 5);
      if FExtendedHeader.FTagRestricted then inc(FExtendedSize);

      SyncSafe(FExtendedSize, 4);
      InvertBuffer(FExtendedSize, 4);
      Stream.Write(FExtendedSize, 4);

      FHeaderFlags := 1;
      Stream.Write(FHeaderFlags, 1);

      FHeaderFlags := 0;
      if FExtendedHeader.FTagIsAnUpdate then FHeaderFlags := (FHeaderFlags or 64);
      if FExtendedHeader.FCRCDataPresent then FHeaderFlags := (FHeaderFlags or 32);
      if FExtendedHeader.FTagRestricted then FHeaderFlags := (FHeaderFlags or 16);
      Stream.Write(FHeaderFlags, 1);

      if FExtendedHeader.FCRCDataPresent then
        begin
          FCRC := FExtendedHeader.FCRCData;
          SyncSafe(FCRC, 5);
          InvertBuffer(FCRC, 5);
          Stream.Write(FCRC, 5);
        end;

      if FExtendedHeader.FTagRestricted then
        begin
          FExtendedHeader.FTagRestrictions.SaveToByte(FHeaderFlags);
          Stream.Write(FHeaderFlags, 1);
        end;
    end;
end;

procedure TID3Header.SetExtendedHeader(const Value: TID3ExtendedHeader);
begin
  FExtendedHeader.Assign(Value);
end;

{ TID3ExtendedHeader }

procedure TID3ExtendedHeader.Clear;
begin
  FCRCData := 0;
  FCRCDataPresent := False;
  FTagIsAnUpdate := False;
  FTagRestricted := False;
  FTagRestrictions.Clear;
end;

constructor TID3ExtendedHeader.Create;
begin
  inherited Create;
  FTagRestrictions := TID3TagRestrictions.Create;
end;

destructor TID3ExtendedHeader.Destroy;
begin
  FTagRestrictions.Free;
  inherited;
end;

procedure TID3ExtendedHeader.SetCRCData(const Value: TCRCData);
begin
  FCRCData := Value;
  FCRCDataPresent := True;
end;

{ TID3TagResctrictions }

procedure TID3TagRestrictions.Clear;
begin
  FImageEncodingRestrictions := False;
  FImageSize := irNone;
  FTagSize := sr128Frames_1MB;
  FTextEncodingRestrictions := False;
  FTextSize := trNone;
end;

procedure TID3TagRestrictions.LoadFromByte(Value: Byte);
var
  t1, t2            : boolean;
begin
  Clear;

  t1 := (Value and 2) = 2;
  t2 := (Value and 1) = 1;

  if not t1 and t2 then
    FImageSize := ir256x256max
  else
    if t1 and not t2 then
    FImageSize := ir64x64max
  else
    if t1 and t2 then
    FImageSize := ir64x64fixed;

  FImageEncodingRestrictions := (Value and 4) = 4;

  t1 := (Value and 16) = 16;
  t2 := (Value and 8) = 8;

  if not t1 and t2 then
    FTextSize := tr1024max
  else
    if t1 and not t2 then
    FTextSize := tr128max
  else
    if t1 and t2 then
    FTextSize := tr30max;

  FTextEncodingRestrictions := (Value and 32) = 32;

  t1 := (Value and 128) = 128;
  t2 := (Value and 64) = 64;

  if not t1 and t2 then
    FTagSize := sr64Frames_128KB
  else
    if t1 and not t2 then
    FTagSize := sr64Frames_128KB
  else
    if t1 and t2 then
    FTagSize := sr32Frames_4KB;
end;

procedure TID3TagRestrictions.SaveToByte(var Value: Byte);
begin
  Value := 0;

  case FImageSize of
    ir256x256max: Value := Value or 1;
    ir64x64max: Value := Value or 2;
    ir64x64fixed: Value := Value or 3;
  end;

  if FImageEncodingRestrictions then Value := Value or 4;

  case FTextSize of
    tr1024max: Value := Value or 8;
    tr128max: Value := Value or 16;
    tr30max: Value := Value or 24;
  end;

  if FTextEncodingRestrictions then Value := Value or 32;

  case FTagSize of
    sr64Frames_128KB: Value := Value or 64;
    sr32Frames_40KB: Value := Value or 128;
    sr32Frames_4KB: Value := Value or 192;
  end;
end;

procedure TID3ExtendedHeader.SetTagRestrictions(const Value: TID3TagRestrictions);
begin
  FTagRestrictions.Assign(Value);
end;

end.

