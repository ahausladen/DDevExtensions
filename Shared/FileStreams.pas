{******************************************************************************}
{*                                                                            *}
{* CompileInterceptor IDE Plugin                                              *}
{*                                                                            *}
{* (C) 2006-2009 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

unit FileStreams;

{$I ..\jedi\jedi.inc}

interface

uses
  Windows, SysUtils, Classes, Contnrs;

const
  MaxWriteAvgBufferCount = 5;
  MaxReadAvgBufferCount = 3;
  DISK_SECTOR_SIZE = 512;
  MaxWritePoolBufferSize = DISK_SECTOR_SIZE * 256;
  MaxReadPoolBufferSize = DISK_SECTOR_SIZE * 24;

type
  {$IFNDEF UNICODE}
  RawByteString = AnsiString;
  UnicodeString = WideString;
  {$ENDIF ~UNICODE}

  TBufferPool = class(TObject)
  private
    FWriteBuffers: TList;
    FWriteBufferState: TList;
    FReadBuffers: TList;
    FReadBufferState: TList;
  public
    constructor Create;
    destructor Destroy; override;

    function AllocWrite: PByteArray;
    procedure ReleaseWrite(P: PByteArray);
    function AllocRead: PByteArray;
    procedure ReleaseRead(P: PByteArray);
  end;

  TBOMType = (bomAnsi, bomUtf8, bomUcs2BE, bomUcs2LE, bomUcs4BE, bomUcs4LE);
  TBOMArray = array[0..3] of Byte;

  POrgStreamData = ^TOrgStreamData;
  TOrgStreamData = record
    Open: function(const Filename: PAnsiChar): THandle; pascal;
    Seek: function(hFile: THandle; Offset, Origin: Integer): Integer; pascal;
    Read: function(hFile: THandle; Buffer: PByte; Size: Cardinal): Integer; pascal;
    Write: function(hFile: THandle; Buffer: PByte; Size: Cardinal): Integer; pascal;
    FileStatus: function(hFile: THandle; out FileDate: Integer; out FileSize: Integer): Integer; pascal;
  end;

  TOrgStream = class(TStream)
  private
    FHandle: THandle;
    FOrgStreamData: POrgStreamData;
  protected
    procedure SetSize(NewSize: Longint); override;
    {$IFDEF COMPILER6_UP}
    procedure SetSize(const NewSize: Int64); override;
    {$ENDIF COMPILER6_UP}
  public
    constructor Create(AHandle: THandle; AOrgStreamData: POrgStreamData = nil);

    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    {$IFDEF COMPILER6_UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    {$ENDIF COMPILER6_UP}

    property Handle: THandle read FHandle;
  end;

  TBufferedWriteStream = class(TStream)
  private
    FBufSize: Cardinal;
    FBuffer: PByteArray;
    FBufStart: Cardinal;
    FCalcBufStart: Boolean;
    FStream: TStream;
    FOwnsStream: Boolean;
  protected
    procedure SetSize(NewSize: Longint); override;
    {$IFDEF COMPILER6_UP}
    procedure SetSize(const NewSize: Int64); override;
    {$ENDIF COMPILER6_UP}
  public
    constructor Create(AHandle: THandle; AOrgStreamData: POrgStreamData = nil); overload;
    constructor Create(AStream: TStream; AOwnsStream: Boolean = True); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    {$IFDEF COMPILER6_UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    {$ENDIF COMPILER6_UP}
    procedure Flush;

    property Stream: TStream read FStream write FStream;
    property OwnsStream: Boolean read FOwnsStream write FOwnsStream;
  end;

  TInjectStream = class(TOrgStream)
  private
    FInjectData: RawByteString;
    FVirtualPosition: Int64;
    FLoading: Boolean;
    FUtfConversionSize: Integer;
    {$IFDEF COMPILER10_UP}
    FBOMLen: Integer;
    FBOM: TBOMArray;
    FBOMType: TBOMType;
    {$ENDIF COMPILER10_UP}
  protected
    procedure SetSize(NewSize: Longint); override;
    {$IFDEF COMPILER6_UP}
    procedure SetSize(const NewSize: Int64); override;
    {$ENDIF COMPILER6_UP}
  public
    constructor Create(AHandle: THandle; const AInjectData: RawByteString; AOrgStreamData: POrgStreamData = nil);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    {$IFDEF COMPILER6_UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    {$ENDIF COMPILER6_UP}

    property UtfConversionSize: Integer read FUtfConversionSize;
    property InjectData: RawByteString read FInjectData;
  end;

  TFileCache = class;

  TFileCacheReaderStream = class(TStream)
  private
    FFileCache: TFileCache;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
    {$IFDEF COMPILER6_UP}
    procedure SetSize(const NewSize: Int64); override;
    {$ENDIF COMPILER6_UP}
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    constructor Create(AFileCache: TFileCache);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    {$IFDEF COMPILER6_UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    {$ENDIF COMPILER6_UP}

    property FileCache: TFileCache read FFileCache;
  end;

  TFileCache = class(TObject)
  private
    FBuffer: PByteArray;
    FSize: Integer;
    FReaders: TObjectList;
    function GetReader(Index: Integer): TFileCacheReaderStream;
    function GetReaderCount: Integer;
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    constructor Create(hFile: THandle; AOrgStreamData: POrgStreamData = nil); overload;
    constructor Create(hFile: THandle; ASize: Integer; AOrgStreamData: POrgStreamData = nil); overload;
    constructor Create(AStream: TStream); overload;
    destructor Destroy; override;

    function NewReader: TFileCacheReaderStream;

    property Size: Integer read FSize;
    property Buffer: PByteArray read FBuffer;
    property ReaderCount: Integer read GetReaderCount;
    property Readers[Index: Integer]: TFileCacheReaderStream read GetReader; default;
  end;

  TBufferStream = class(TStream)
  private
    FBuffer: PByteArray;
    FSize: Integer;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(ABuffer: PByteArray; ASize: Integer);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TRawByteStringStream = class(TBufferStream)
  private
    FData: RawByteString;
  public
    constructor Create(const AData: RawByteString);
  end;

  TBufferedReadStream = class(TFileStream)
  private
    FBufSize, FBufPos: Integer;
    FBuffer: PByteArray;
    FPosition: Int64;
    FSize: Int64;
    function GetEof: Boolean;
  protected
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(const Filename: string);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure Flush;

    property FilePos: Int64 read FPosition;
    property Eof: Boolean read GetEof;
  end;

var
  BufferPool: TBufferPool;

function ReadBOM(Stream: TStream; out BOMLen: Integer; var BOM: TBOMArray): TBOMType;
function ReadAllFileUcs2(Stream: TStream; BOMType: TBOMType; ExtraData: string): string;
function ReadAllFileUcs4(Stream: TStream; BOMType: TBOMType): UCS4String;

implementation

resourcestring
  RsNotSupported = 'Operation "%s" not supported';

{ TBufferPool }

constructor TBufferPool.Create;
begin
  inherited Create;
  FWriteBuffers := TList.Create;
  FWriteBufferState := TList.Create;
  FReadBuffers := TList.Create;
  FReadBufferState := TList.Create;
end;

destructor TBufferPool.Destroy;
var
  I: Integer;
begin
  for I := 0 to FWriteBuffers.Count - 1 do
    if FWriteBuffers[I] <> nil then
      VirtualFree(FWriteBuffers[I], MaxWritePoolBufferSize, MEM_FREE);
  FWriteBuffers.Free;
  FWriteBufferState.Free;
  for I := 0 to FReadBuffers.Count - 1 do
    if FReadBuffers[I] <> nil then
      VirtualFree(FReadBuffers[I], MaxReadPoolBufferSize, MEM_FREE);
  FReadBuffers.Free;
  FReadBufferState.Free;
  inherited Destroy;
end;

function TBufferPool.AllocWrite: PByteArray;
var
  I: Integer;
begin
  for I := 0 to FWriteBufferState.Count - 1 do
  begin
    if FWriteBufferState[I] = nil then // unlocked
    begin
      FWriteBufferState[I] := Pointer(1); // locked
      Result := FWriteBuffers[I];
      Exit;
    end;
  end;
  Result := VirtualAlloc(nil, MaxWritePoolBufferSize, MEM_COMMIT, PAGE_READWRITE);
  FWriteBufferState.Add(Pointer(1));
  FWriteBuffers.Add(Result);
end;

procedure TBufferPool.ReleaseWrite(P: PByteArray);
var
  I: Integer;
begin
  for I := 0 to FWriteBufferState.Count - 1 do
  begin
    if FWriteBuffers[I] = P then
    begin
      FWriteBufferState[I] := nil;
      Break;
    end;
  end;
  if FWriteBuffers.Count > MaxWriteAvgBufferCount then
  begin
    for I := FWriteBufferState.Count - 1 downto 0 do
    begin
      if FWriteBufferState[I] = nil then
      begin
        VirtualFree(FWriteBuffers[I], MaxWritePoolBufferSize, MEM_FREE);
        FWriteBuffers.Delete(I);
        FWriteBufferState.Delete(I);
        if FWriteBuffers.Count = MaxWriteAvgBufferCount then
          Break;
      end;
    end;
  end;
end;

function TBufferPool.AllocRead: PByteArray;
var
  I: Integer;
begin
  for I := 0 to FReadBufferState.Count - 1 do
  begin
    if FReadBufferState[I] = nil then // unlocked
    begin
      FReadBufferState[I] := Pointer(1); // locked
      Result := FReadBuffers[I];
      Exit;
    end;
  end;
  Result := VirtualAlloc(nil, MaxReadPoolBufferSize, MEM_COMMIT, PAGE_READWRITE);
  FReadBufferState.Add(Pointer(1));
  FReadBuffers.Add(Result);
end;

procedure TBufferPool.ReleaseRead(P: PByteArray);
var
  I: Integer;
begin
  for I := 0 to FReadBufferState.Count - 1 do
  begin
    if FReadBuffers[I] = P then
    begin
      FReadBufferState[I] := nil;
      Break;
    end;
  end;
  if FReadBuffers.Count > MaxReadAvgBufferCount then
  begin
    for I := FReadBufferState.Count - 1 downto 0 do
    begin
      if FReadBufferState[I] = nil then
      begin
        VirtualFree(FReadBuffers[I], MaxReadPoolBufferSize, MEM_FREE);
        FReadBuffers.Delete(I);
        FReadBufferState.Delete(I);
        if FReadBuffers.Count = MaxReadAvgBufferCount then
          Break;
      end;
    end;
  end;
end;

{ TBufferedWriteStream }

constructor TBufferedWriteStream.Create(AHandle: THandle; AOrgStreamData: POrgStreamData);
begin
  Create(TOrgStream.Create(AHandle, AOrgStreamData));
end;

constructor TBufferedWriteStream.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := AOwnsStream;

  if BufferPool = nil then
    GetMem(FBuffer, MaxWritePoolBufferSize)
  else
    FBuffer := BufferPool.AllocWrite;
end;

destructor TBufferedWriteStream.Destroy;
begin
  Flush;
  if BufferPool = nil then
    FreeMem(FBuffer)
  else
    BufferPool.ReleaseWrite(FBuffer);
  if FOwnsStream then
    FStream.Free;
  inherited Destroy;
end;

procedure TBufferedWriteStream.Flush;
var
  Count: Cardinal;
begin
  Count := FBufSize - FBufStart;
  if Count > 0 then
    Stream.Write(FBuffer[FBufStart], Count);
  FBufSize := 0;
  FBufStart := 0;
  FCalcBufStart := False;
end;

function TBufferedWriteStream.Write(const Buffer; Count: Longint): Longint;
var
  P: PByte;
  Diff: Longint;
begin
  if FCalcBufStart then
  begin
    FCalcBufStart := False;
    FBufStart := Stream.Position and (DISK_SECTOR_SIZE - 1);
    FBufSize := FBufStart;
  end;

  P := @Buffer;
  Result := Count;
  if FBufSize + Cardinal(Count) > MaxWritePoolBufferSize then
  begin
    // Fill remaining Buffer bytes
    Diff := MaxWritePoolBufferSize - FBufSize;
    Move(P^, FBuffer[FBufSize], Diff);
    Dec(Count, Diff);
    Inc(P, Diff);
    FBufSize := MaxWritePoolBufferSize;
    Flush;
  end;

  if Count > MaxWritePoolBufferSize then
  begin
    // Buffer is too small, write as many blocks as possible
    Flush;

    Diff := Count mod MaxWritePoolBufferSize;
    Dec(Count, Diff);
    Stream.Write(P^, Count);
    Inc(P, Count);
    Count := Diff;
  end;

  if Count > 0 then
  begin
    // append data to Buffer
    Move(P^, FBuffer[FBufSize], Count);
    Inc(FBufSize, Count);
  end;
end;

function TBufferedWriteStream.Read(var Buffer; Count: Longint): Longint;
begin
  Flush;
  Result := Stream.Read(Buffer, Count);
  FCalcBufStart := True;
end;

{$IFDEF COMPILER6_UP}
function TBufferedWriteStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TBufferedWriteStream.Seek(Offset: Longint; Origin: Word): Longint;
{$ENDIF COMPILER6_UP}
var
  StreamPos: Int64;
begin
  StreamPos := Stream.Position;
  if (Origin = {$IFDEF COMPILER6_UP}soCurrent{$ELSE}soFromCurrent{$ENDIF}) and (Offset = 0) then
    Result := StreamPos + FBufSize
  else
  if (Origin = {$IFDEF COMPILER6_UP}soBeginning{$ELSE}soFromBeginning{$ENDIF}) and (Offset = StreamPos + FBufSize) then
    Result := StreamPos + FBufSize
  else
  begin
    Flush;
    Result := Stream.Seek(Offset, Origin);

    { Align to next block }
    FBufStart := Result and (DISK_SECTOR_SIZE - 1);
    FBufSize := FBufStart;
    FCalcBufStart := False;
  end;
end;

procedure TBufferedWriteStream.SetSize(NewSize: Longint);
begin
  Flush;
  FCalcBufStart := True;
  Stream.Size := NewSize;
end;

{$IFDEF COMPILER6_UP}
procedure TBufferedWriteStream.SetSize(const NewSize: Int64);
begin
  Flush;
  FCalcBufStart := True;
  Stream.Size := NewSize;
end;
{$ENDIF COMPILER6_UP}

{ TOrgStream }

constructor TOrgStream.Create(AHandle: THandle; AOrgStreamData: POrgStreamData);
begin
  inherited Create;
  FHandle := AHandle;
  FOrgStreamData := AOrgStreamData;
end;

procedure TOrgStream.SetSize(NewSize: Longint);
begin
  {$IFDEF COMPILER6_UP}
  SetSize(Int64(NewSize));
  {$ELSE}
  Seek(NewSize, soFromBeginning);
  if FOrgStreamData = nil then
    Win32Check(SetEndOfFile(FHandle));
  {$ENDIF COMPILER6_UP}
end;

{$IFDEF COMPILER6_UP}
procedure TOrgStream.SetSize(const NewSize: Int64);
begin
  Seek(NewSize, soBeginning);
  if FOrgStreamData = nil then
    {$WARNINGS OFF}
    Win32Check(SetEndOfFile(FHandle));
    {$WARNINGS ON}
end;
{$ENDIF COMPILER6_UP}

function TOrgStream.Read(var Buffer; Count: Integer): Integer;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    if FOrgStreamData = nil then
    begin
      Result := FileRead(FHandle, Buffer, Count);
      if Result = -1 then
        Result := 0;
    end
    else
      Result := FOrgStreamData.Read(FHandle, PByte(@Buffer), Count);
  end
  else
    Result := 0;
end;

function TOrgStream.Write(const Buffer; Count: Integer): Integer;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    if FOrgStreamData = nil then
    begin
      Result := FileWrite(FHandle, Buffer, Count);
      if Result = -1 then
        Result := 0;
    end
    else
      Result := FOrgStreamData.Write(FHandle, PByte(@Buffer), Count);
  end
  else
    Result := 0;
end;

{$IFDEF COMPILER6_UP}
function TOrgStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TOrgStream.Seek(Offset: Longint; Origin: Word): Longint;
{$ENDIF COMPILER6_UP}
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    if FOrgStreamData = nil then
      Result := FileSeek(FHandle, Offset, Ord(Origin))
    else
      Result := FOrgStreamData.Seek(FHandle, Integer(Offset), Integer(Origin));
  end
  else
    Result := 0;
end;

function ReadBOM(Stream: TStream; out BOMLen: Integer; var BOM: TBOMArray): TBOMType;
begin
  // BOM detection
  BOM[2] := 0;
  BOM[3] := 0;
  Result := bomAnsi;
  BOMLen := Stream.Read(BOM, 2);
  if BOMLen = 2 then
  begin
    if (BOM[0] = $EF) and (BOM[1] = $BB) then
    begin
      Inc(BOMLen, Stream.Read(BOM[2], 1));
      if BOM[2] = $BF then
        Result := bomUtf8;
    end
    else if (BOM[0] = $FF) and (BOM[1] = $FE) then
    begin
      Inc(BOMLen, Stream.Read(BOM[2], 2));
      if (BOM[2] = 0) and (BOM[3] = 0) then
        Result := bomUcs4LE
      else
      begin
        Dec(BOMLen, 2);
        Stream.Seek(-2, soCurrent);
        Result := bomUcs2LE;
      end;
    end
    else if (BOM[0] = $FE) and (BOM[1] = $FF) then
    begin
      Result := bomUcs2BE;
    end
    else if (BOM[0] = 0) and (BOM[1] = 0) then
    begin
      Inc(BOMLen, Stream.Read(BOM[2], 2));
      if (BOM[2] = $FE) and (BOM[3] = $FF) then
        Result := bomUcs4BE;
    end;
  end;
end;

{$IFDEF COMPILER10_UP}

function ReadAllFileUcs2(Stream: TStream; BOMType: TBOMType; ExtraData: UnicodeString): UnicodeString;
const
  ReadCount = 64 * 1024;
var
  Len, Size: Integer;
  P: PWideChar;
begin
  Len := Length(ExtraData);
  SetLength(Result, ReadCount + Len);
  if Len > 0 then
    Move(ExtraData[1], Result[1], Len * SizeOf(WideChar));
  repeat
    Size := Stream.Read(PWideChar(PWideChar(Result) + Len)^, ReadCount * 2) div 2;
    Inc(Len, Size);
    SetLength(Result, Len);
  until Size <> ReadCount;

  if BOMType = bomUcs2BE then
  begin
    // swap bytes
    P := PWideChar(Result);
    while Len > 0 do
    begin
      P^ := WideChar((Word(P^) {and $00FF}) shl 8 or
                     (Word(P^) {and $FF00}) shr 8);
      Inc(P);
      Dec(Len);
    end;
  end;
end;

function ReadAllFileUcs4(Stream: TStream; BOMType: TBOMType): UCS4String;
const
  ReadCount = 64 * 1024;
var
  Len, Size: Integer;
  P: PUCS4Char;
begin
  SetLength(Result, ReadCount);
  Len := 0;
  repeat
    Size := Stream.Read(PAnsiChar(PAnsiChar(Result) + (Len * 4))^, ReadCount * 4) div 4;
    Inc(Len, Size);
    SetLength(Result, Len);
  until Size <> ReadCount;

  if BOMType = bomUcs4BE then
  begin
    // swap bytes
    P := PUCS4Chars(Result);
    while Len > 0 do
    begin
      P^ := UCS4Char((LongWord(P^) {and $000000FF}) shl 24 or
                     (LongWord(P^) and $0000FF00) shl 8 or
                     (LongWord(P^) and $00FF0000) shr 8 or
                     (LongWord(P^) {and $FF000000}) shr 24);
      Inc(P);
      Dec(Len);
    end;
  end;
end;

{$ENDIF COMPILER10_UP}

{ TInjectStream }

constructor TInjectStream.Create(AHandle: THandle; const AInjectData: RawByteString; AOrgStreamData: POrgStreamData);
{$IFDEF COMPILER10_UP}
var
  I: Integer;
  ExtraData: UnicodeString;
{$ENDIF COMPILER10_UP}
begin
  inherited Create(AHandle, AOrgStreamData);
  FInjectData := AInjectData;
  FLoading := True;

  {$IFDEF COMPILER10_UP}
  // BOM detection
  ReadBOM(Self, FBOMLen, FBOM);

  // data conversion
  case FBOMType of
    bomAnsi:
      begin
        {$IFDEF UNICODE}
        SetCodePage(FInjectData, CP_ACP);
        {$ENDIF UNICODE}
        for I := 0 to FBOMLen - 1 do
          FInjectData := FInjectData + AnsiChar(FBOM[I]);
      end;
    bomUtf8:
      begin
        {$IFDEF UNICODE}
        SetCodePage(FInjectData, CP_UTF8);
        {$ELSE}
        FInjectData := AnsiToUtf8(FInjectData);
        {$ENDIF UNICODE}
        FInjectData := AnsiChar($EF) + AnsiChar($BB) + AnsiChar($BF) +
                       FInjectData;
      end;

    { The Compiler cannot handle these file types. So we convert them to UTF8 }
    bomUcs2BE, bomUcs2LE:
      begin
        {$IFDEF UNICODE}
        SetCodePage(FInjectData, CP_UTF8);
        {$ELSE}
        FInjectData := AnsiToUtf8(FInjectData);
        {$ENDIF UNICODE}
        FInjectData := AnsiChar($EF) + AnsiChar($BB) + AnsiChar($BF) +
                       FInjectData +
                       UTF8Encode(ReadAllFileUcs2(Self, FBOMType, ExtraData));
        FUtfConversionSize := Length(FInjectData);
      end;
    bomUcs4BE, bomUcs4LE:
      begin
        {$IFDEF UNICODE}
        SetCodePage(FInjectData, CP_UTF8);
        {$ELSE}
        FInjectData := AnsiToUtf8(FInjectData);
        {$ENDIF UNICODE}
        FInjectData := AnsiChar($EF) + AnsiChar($BB) + AnsiChar($BF) +
                       FInjectData +
                       UTF8Encode(
                         {$IFDEF UNICODE}
                         UCS4StringToUnicodeString
                         {$ELSE}
                         UCS4StringToWideString
                         {$ENDIF UNICODE}
                           (ReadAllFileUcs4(Self, FBOMType))
                       );
        FUtfConversionSize := Length(FInjectData);
      end;
  end;
  {$ENDIF COMPLER10_UP}
  FLoading := False;
end;

function TInjectStream.Read(var Buffer; Count: Longint): Longint;
var
  P: PByte;
  Len: Integer;
begin
  if FLoading then
  begin
    Result := inherited Read(Buffer, Count);
    Exit;
  end;

  P := @Buffer;
  Result := 0;
  Len := Length(InjectData);

  if FVirtualPosition < Len then
  begin
    if FVirtualPosition + Count < Len then
    begin
      Move(InjectData[FVirtualPosition + 1], P^, Count);
      Result := Count;
    end
    else
    begin
      Move(InjectData[FVirtualPosition + 1], P^, Len - FVirtualPosition);
      Result := Len - FVirtualPosition;
    end;
    Inc(P, Result);
    Dec(Count, Result);
  end;

  if Count > 0 then
    Inc(Result, inherited Read(P^, Count));

  Inc(FVirtualPosition, Result);
end;

{$IFDEF COMPILER6_UP}
function TInjectStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TInjectStream.Seek(Offset: Longint; Origin: Word): Longint;
const
  soEnd = soFromEnd;
  soBeginning = soFromBeginning;
  soCurrent = soFromCurrent;
{$ENDIF COMPILER6_UP}
var
  RelPosition: Int64;
  Len: Integer;
begin
  if FLoading then
  begin
    Result := inherited Seek(Offset, Origin);
    Exit;
  end;

  Len := Length(InjectData);

  if Origin = soEnd then
  begin
    RelPosition := inherited Seek(Offset, soBeginning);
    FVirtualPosition := RelPosition + Len;
    Origin := soCurrent;
  end;

  RelPosition := Offset;
  case Origin of
    soBeginning:
      begin
        if RelPosition < 0 then
          RelPosition := 0
        else if RelPosition < Len then
          inherited Seek(0, soBeginning)
        else
        begin
          RelPosition := inherited Seek(Offset - Len, soBeginning);
          Inc(RelPosition, Len);
        end;
        FVirtualPosition := RelPosition;
      end;

    soCurrent:
      begin
        if FVirtualPosition + RelPosition < 0 then
          FVirtualPosition := 0
        else if FVirtualPosition + RelPosition < Len then
        begin
          inherited Seek(0, soBeginning);
          Inc(FVirtualPosition, RelPosition);
        end
        else
        begin
          RelPosition := inherited Seek(Offset, soCurrent);
          Inc(RelPosition, Len);
          FVirtualPosition := RelPosition;
        end;
      end;
  end;

  Result := FVirtualPosition;
end;

function TInjectStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.CreateFmt(RsNotSupported, ['Write']);
end;

procedure TInjectStream.SetSize(NewSize: Longint);
begin
  raise Exception.CreateFmt(RsNotSupported, ['SetSize']);
end;

{$IFDEF COMPILER6_UP}
procedure TInjectStream.SetSize(const NewSize: Int64);
begin
  raise Exception.CreateFmt(RsNotSupported, ['SetSize']);
end;
{$ENDIF COMPILER6_UP}

{ TFileCacheReaderStream }

class function TFileCacheReaderStream.NewInstance: TObject;
begin
  if Self = TFileCacheReaderStream then
  begin
    GetMem(Pointer(Result), InstanceSize);
    PInteger(Result)^ := Integer(Self);
  end
  else
    Result := inherited NewInstance;
end;

procedure TFileCacheReaderStream.FreeInstance;
begin
  if ClassType = TFileCacheReaderStream then
    FreeMem(Pointer(Self))
  else
    inherited FreeInstance;
end;

constructor TFileCacheReaderStream.Create(AFileCache: TFileCache);
begin
  FFileCache := AFileCache;
  FFileCache.FReaders.Add(Self);
  FPosition := 0;
end;

destructor TFileCacheReaderStream.Destroy;
begin
  FFileCache.FReaders.Extract(Self);
  inherited Destroy;
end;

function TFileCacheReaderStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Result := FFileCache.Size - FPosition;
    if Result > 0 then
    begin
      if Result > Count then
        Result := Count;
      Move(FFileCache.FBuffer[FPosition], Buffer, Result);
      Inc(FPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TFileCacheReaderStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.CreateFmt(RsNotSupported, ['Write']);
end;

procedure TFileCacheReaderStream.SetSize(NewSize: Longint);
begin
  raise Exception.CreateFmt(RsNotSupported, ['SetSize']);
end;

{$IFDEF COMPILER6_UP}
procedure TFileCacheReaderStream.SetSize(const NewSize: Int64);
begin
  raise Exception.CreateFmt(RsNotSupported, ['SetSize']);
end;
{$ENDIF COMPILER6_UP}

{$IFDEF COMPILER6_UP}
function TFileCacheReaderStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TFileCacheReaderStream.Seek(Offset: Longint; Origin: Word): Longint;
const
  soEnd = soFromEnd;
  soBeginning = soFromBeginning;
  soCurrent = soFromCurrent;
{$ENDIF COMPILER6_UP}
begin
  case Origin of
    soBeginning:
      FPosition := Offset;
    soCurrent:
      Inc(FPosition, Offset);
    soEnd:
      FPosition := FFileCache.Size + Offset;
  end;
  if FPosition < 0 then
    FPosition := 0;
  if FPosition > FFileCache.Size then
    FPosition := FFileCache.Size;
  Result := FPosition;
end;

{ TFileCache }

class function TFileCache.NewInstance: TObject;
begin
  if Self = TFileCache then
  begin
    GetMem(Pointer(Result), InstanceSize);
    PInteger(Result)^ := Integer(Self);
  end
  else
    Result := inherited NewInstance;
end;

procedure TFileCache.FreeInstance;
begin
  if ClassType = TFileCache then
    FreeMem(Pointer(Self))
  else
    inherited FreeInstance;
end;

constructor TFileCache.Create(hFile: THandle; AOrgStreamData: POrgStreamData = nil);
begin
  Create(hFile, -1, AOrgStreamData);
end;

constructor TFileCache.Create(hFile: THandle; ASize: Integer; AOrgStreamData: POrgStreamData);
const
{$IFDEF CONSOLE}
  BufReadSize = 64 * 1024;
{$ELSE}
  // BCB 5, 6 abort with buffers larger than 16Kb if the file is opened in the editor
  BufReadSize = 15872; // The compiler uses this number of bytes for read operations.
{$ENDIF CONSOLE}
var
  n: Integer;
  Stream: TStream;
begin
  inherited Create;
  FSize := ASize;
  FReaders := TObjectList.Create;
  FReaders.Capacity := 10;

  if ASize = -1 then
  begin
    FBuffer := nil;
    {$IFDEF COMPILER10_UP}
    Stream := TInjectStream.Create(hFile, '', AOrgStreamData); // UCS2 and UCS4 support
    {$ELSE}
    Stream := TOrgStream.Create(hFile, AOrgStreamData); // UCS2 and UCS4 support
    {$ENDIF COMPILER10_UP}
    try
      ASize := 0;
      repeat
        ReallocMem(FBuffer, ASize + BufReadSize);
        n := Stream.Read(FBuffer[ASize], BufReadSize);
        Inc(ASize, n);
        if ASize > 100 * 1024 * 1024 then // do not count on the IDE's FileRead implementation
        begin
          ASize := 0;
          Break;
        end;
      until n <> BufReadSize;
      ReallocMem(FBuffer, ASize);
    finally
      Stream.Free;
    end;
    FSize := ASize;
  end
  else
  begin
    Stream := TInjectStream.Create(hFile, '', AOrgStreamData); // UCS2 and UCS4 support
    try
      if TInjectStream(Stream).UtfConversionSize > 0 then
        FSize := TInjectStream(Stream).UtfConversionSize;
      if FSize > 0 then
      begin
        GetMem(FBuffer, FSize);
        Stream.Read(PByte(FBuffer)^, FSize);
      end;
    finally
      Stream.Free;
    end;
  end;
end;

constructor TFileCache.Create(AStream: TStream);
begin
  inherited Create;
  FReaders := TObjectList.Create;

  if AStream <> nil then
  begin
    FSize := AStream.Size;
    if FSize > 0 then
    begin
      GetMem(FBuffer, FSize);
      AStream.Read(FBuffer^, FSize);
    end;
  end;
end;

destructor TFileCache.Destroy;
begin
  if FBuffer <> nil then
    FreeMem(FBuffer);
  FReaders.Free;
  inherited Destroy;
end;

function TFileCache.GetReaderCount: Integer;
begin
  Result := FReaders.Count;
end;

function TFileCache.GetReader(Index: Integer): TFileCacheReaderStream;
begin
  Result := TFileCacheReaderStream(FReaders[Index]);
end;

function TFileCache.NewReader: TFileCacheReaderStream;
begin
  Result := TFileCacheReaderStream.Create(Self);
end;

{ TBufferStream }

constructor TBufferStream.Create(ABuffer: PByteArray; ASize: Integer);
begin
  inherited Create;
  FBuffer := ABuffer;
  FSize := ASize;
  FPosition := 0;
end;

function TBufferStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Result := FSize - FPosition;
    if Result > 0 then
    begin
      if Result > Count then
        Result := Count;
      Move(FBuffer[FPosition], Buffer, Result);
      Inc(FPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TBufferStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.CreateFmt(RsNotSupported, ['Write']);
end;

procedure TBufferStream.SetSize(NewSize: Longint);
begin
  raise Exception.CreateFmt(RsNotSupported, ['SetSize']);
end;

procedure TBufferStream.SetSize(const NewSize: Int64);
begin
  raise Exception.CreateFmt(RsNotSupported, ['SetSize']);
end;

function TBufferStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      FPosition := Offset;
    soCurrent:
      Inc(FPosition, Offset);
    soEnd:
      FPosition := FSize + Offset;
  end;
  if FPosition < 0 then
    FPosition := 0;
  if FPosition > FSize then
    FPosition := FSize;
  Result := FPosition;
end;

{ TBufferedReadStream }

type
  PInt64 = ^TInt64;
  TInt64 = packed record
    Lo, Hi: Cardinal;
  end;

constructor TBufferedReadStream.Create(const Filename: string);
begin
  inherited Create(Filename, fmOpenRead or fmShareDenyWrite);
  FBuffer := BufferPool.AllocRead;
  FSize := -1;
  FBufPos := MaxReadPoolBufferSize;
  FBufSize := MaxReadPoolBufferSize;
end;

destructor TBufferedReadStream.Destroy;
begin
  BufferPool.ReleaseRead(FBuffer);
  inherited Destroy;
end;

procedure TBufferedReadStream.Flush;
begin
  raise Exception.CreateFmt(RsNotSupported, ['Flush']);
end;

function TBufferedReadStream.GetEof: Boolean;
begin
  if FSize = -1 then
    PInt64(@FSize).Lo := GetFileSize(FHandle, @TInt64(FSize).Hi);
  Result := FPosition >= FSize;
end;

function TBufferedReadStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.CreateFmt(RsNotSupported, ['Write']);
end;

function TBufferedReadStream.Read(var Buffer; Count: Longint): Longint;
type
  P3Bytes = ^T3Bytes;
  T3Bytes = packed record
    b: array[0..2] of Byte;
  end;
var
  FreeSize: Integer;
begin
  FreeSize := FBufSize - FBufPos;
  if FreeSize >= Count then
  begin
    Result := Count;
    case Count of
      0: Exit;
      1: PByteArray(@Buffer)[0] := FBuffer[FBufPos];
      2: PWordArray(@Buffer)[0] := PWordArray(@FBuffer[FBufPos])[0];
      4: PCardinal(@Buffer)^ := PCardinal(@FBuffer[FBufPos])^;
      8: PInt64(@Buffer)^ := PInt64(@FBuffer[FBufPos])^;
      3: P3Bytes(@Buffer)^ := P3Bytes(@FBuffer[FBufPos])^;
    else
      Move(FBuffer[FBufPos], Buffer, Count);
    end;
    Inc(FBufPos, Count);
    Inc(FPosition, Count);
    Exit;
  end;

  if FreeSize > 0 then
  begin
    Move(FBuffer[FBufPos], Buffer, FreeSize);
    Result := FreeSize;
    Inc(FPosition, FreeSize);
  end
  else
    Result := 0;

  if FBufSize < MaxReadPoolBufferSize then
    Exit; // Eof

  { Fill Buffer }
  FBufPos := 0;
  FBufSize := inherited Read(FBuffer[0], MaxReadPoolBufferSize);

  { Read remaining bytes }
  Inc(Result, Read(PByteArray(@Buffer)[Result], Count - Result));
end;

function TBufferedReadStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Origin = soCurrent then
  begin
    if (FBufPos + Offset >= 0) and (FBufPos + Offset < FBufSize) then
    begin
      Inc(FPosition, Offset);
      Inc(FBufPos, Offset);
      Result := FPosition;
    end
    else
    begin
      FBufPos := MaxReadPoolBufferSize;
      FBufSize := MaxReadPoolBufferSize; // discard buffer}
      Result := inherited Seek(FPosition + Offset, soBeginning);
      FPosition := Result;
    end;
  end
  else
  begin
    FBufPos := MaxReadPoolBufferSize;
    FBufSize := MaxReadPoolBufferSize; // discard buffer}
    Result := inherited Seek(Offset, Origin);
    FPosition := Result;
  end;
end;

procedure TBufferedReadStream.SetSize(NewSize: Longint);
begin
  raise Exception.CreateFmt(RsNotSupported, ['SetSize']);
end;

procedure TBufferedReadStream.SetSize(const NewSize: Int64);
begin
  raise Exception.CreateFmt(RsNotSupported, ['SetSize']);
end;

{ TRawByteStringStream }

constructor TRawByteStringStream.Create(const AData: RawByteString);
begin
  FData := AData;
  inherited Create(PByteArray(FData), Length(FData));
end;

end.
