{******************************************************************************}
{*                                                                            *}
{* CompileInterceptor IDE Plugin                                              *}
{*                                                                            *}
{* (C) 2006-2011 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit FileStreams;

{$I CompileInterceptor.inc}

interface

uses
  Windows, SysUtils, Classes, Contnrs;

const
  MaxWriteAvgBufferCount = 5;
  MaxWritePoolBufferSize = 4096 * 128;

type
  {$IFNDEF UNICODE}
  RawByteString = AnsiString;
  UnicodeString = WideString;
  {$ENDIF UNICODE}

  TBufferPool = class(TObject)
  private
    FWriteBuffers: TList;
    FWriteBufferState: TList;
  public
    constructor Create;
    destructor Destroy; override;

    function AllocWrite: PByteArray;
    procedure ReleaseWrite(P: PByteArray);
  end;

  TBOMType = (bomAnsi, bomUtf8, bomUcs2BE, bomUcs2LE, bomUcs4BE, bomUcs4LE);

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
    constructor Create(AHandle: THandle; AOrgStreamData: POrgStreamData);

    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    {$IFDEF COMPILER6_UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    {$ENDIF COMPILER6_UP}

    property Handle: THandle read FHandle;
  end;

  TBufferedWriteStream = class(TOrgStream)
  private
    FBufSize: Integer;
    FBuffer: PByteArray;
  protected
    procedure SetSize(NewSize: Longint); override;
    {$IFDEF COMPILER6_UP}
    procedure SetSize(const NewSize: Int64); override;
    {$ENDIF COMPILER6_UP}
  public
    constructor Create(AHandle: THandle; AOrgStreamData: POrgStreamData);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    {$IFDEF COMPILER6_UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    {$ENDIF COMPILER6_UP}
    procedure Flush;
  end;

  TInjectStream = class(TOrgStream)
  private
    FInjectData: RawByteString;
    FVirtualPosition: Int64;
    FLoading: Boolean;
    {$IFDEF COMPILER10_UP}
    FBOMLen: Integer;
    FBOM: array[0..3] of Byte;
    FBOMType: TBOMType;
    {$ENDIF COMPILER10_UP}
  protected
    procedure SetSize(NewSize: Longint); override;
    {$IFDEF COMPILER6_UP}
    procedure SetSize(const NewSize: Int64); override;
    {$ENDIF COMPILER6_UP}
  public
    constructor Create(AHandle: THandle; const AInjectData: RawByteString; AOrgStreamData: POrgStreamData);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    {$IFDEF COMPILER6_UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    {$ENDIF COMPILER6_UP}

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
    constructor Create(hFile: THandle; {ASize: Integer; }AOrgStreamData: POrgStreamData);
    destructor Destroy; override;

    function NewReader: TFileCacheReaderStream;

    property Size: Integer read FSize;
    property Buffer: PByteArray read FBuffer;
    property ReaderCount: Integer read GetReaderCount;
    property Readers[Index: Integer]: TFileCacheReaderStream read GetReader; default;
  end;

var
  BufferPool: TBufferPool;

implementation

resourcestring
  RsNotSupported = 'Operation "%s" not supported';

{ TBufferPool }

constructor TBufferPool.Create;
begin
  inherited Create;
  FWriteBuffers := TList.Create;
  FWriteBufferState := TList.Create;
end;

destructor TBufferPool.Destroy;
var
  i: Integer;
begin
  for i := 0 to FWriteBuffers.Count - 1 do
    if FWriteBuffers[i] <> nil then
      VirtualFree(FWriteBuffers[i], MaxWritePoolBufferSize, MEM_FREE);
  FWriteBuffers.Free;
  FWriteBufferState.Free;
  inherited Destroy;
end;

function TBufferPool.AllocWrite: PByteArray;
var
  i: Integer;
begin
  for i := 0 to FWriteBufferState.Count - 1 do
  begin
    if FWriteBufferState[i] = nil then // unlocked
    begin
      FWriteBufferState[i] := Pointer(1); // locked
      Result := FWriteBuffers[i];
      Exit;
    end;
  end;
  Result := VirtualAlloc(nil, MaxWritePoolBufferSize, MEM_COMMIT, PAGE_READWRITE);
  FWriteBufferState.Add(Pointer(1));
  FWriteBuffers.Add(Result);
end;

procedure TBufferPool.ReleaseWrite(P: PByteArray);
var
  i: Integer;
begin
  for i := 0 to FWriteBufferState.Count - 1 do
  begin
    if FWriteBuffers[i] = P then
      FWriteBufferState[i] := nil;
  end;
  if FWriteBuffers.Count > MaxWriteAvgBufferCount then
  begin
    for i := FWriteBufferState.Count - 1 downto 0 do
    begin
      if FWriteBufferState[i] = nil then
      begin
        VirtualFree(FWriteBuffers[i], MaxWritePoolBufferSize, MEM_FREE);
        FWriteBuffers.Delete(i);
        FWriteBufferState.Delete(i);
        if FWriteBuffers.Count = MaxWriteAvgBufferCount then
          Break;
      end;
    end;
  end;
end;

{ TBufferedWriteStream }

constructor TBufferedWriteStream.Create(AHandle: THandle; AOrgStreamData: POrgStreamData);
begin
  inherited Create(AHandle, AOrgStreamData);
  FBufSize := 0;
  FBuffer := BufferPool.AllocWrite;
end;

destructor TBufferedWriteStream.Destroy;
begin
  Flush;
  BufferPool.ReleaseWrite(FBuffer);
  inherited Destroy;
end;

procedure TBufferedWriteStream.Flush;
begin
  if FBufSize > 0 then
  begin
    inherited Write(FBuffer^, FBufSize); // write remaining bytes
    FBufSize := 0;
  end;
end;

function TBufferedWriteStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  if FBufSize + Count > MaxWritePoolBufferSize then
    Flush;
  if Count > MaxWritePoolBufferSize then
  begin
    // write directly
    Flush;
    inherited Write(Buffer, Count);
  end
  else
  begin
    Move(Buffer, FBuffer[FBufSize], Count);
    Inc(FBufSize, Count);
  end;
end;

function TBufferedWriteStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise Exception.CreateFmt(RsNotSupported, ['Read']);
end;

{$IFDEF COMPILER6_UP}
function TBufferedWriteStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TBufferedWriteStream.Seek(Offset: Longint; Origin: Word): Longint;
{$ENDIF COMPILER6_UP}
begin
  raise Exception.CreateFmt(RsNotSupported, ['Seek']);
end;

procedure TBufferedWriteStream.SetSize(NewSize: Longint);
begin
  raise Exception.CreateFmt(RsNotSupported, ['SetSize']);
end;

{$IFDEF COMPILER6_UP}
procedure TBufferedWriteStream.SetSize(const NewSize: Int64);
begin
  raise Exception.CreateFmt(RsNotSupported, ['SetSize']);
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
  {$ENDIF COMPILER6_UP}
end;

{$IFDEF COMPILER6_UP}
procedure TOrgStream.SetSize(const NewSize: Int64);
begin
  Seek(NewSize, soBeginning);
end;
{$ENDIF COMPILER6_UP}

function TOrgStream.Read(var Buffer; Count: Integer): Integer;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    Result := FOrgStreamData.Read(FHandle, PByte(@Buffer), Count)
  else
    Result := 0;
end;

function TOrgStream.Write(const Buffer; Count: Integer): Integer;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    Result := FOrgStreamData.Write(FHandle, PByte(@Buffer), Count)
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
    Result := FOrgStreamData.Seek(FHandle, Integer(Offset), Integer(Origin))
  else
    Result := 0;
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

function ReadAllFileUcs4(Stream: TStream; BOMType: TBOMType): Ucs4String;
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
  FBOM[2] := 0;
  FBOM[3] := 0;
  FBOMType := bomAnsi;
  FBOMLen := inherited Read(FBOM, 2);
  if FBOMLen = 2 then
  begin
    if (FBOM[0] = $EF) and (FBOM[1] = $BB) then
    begin
      Inc(FBOMLen, inherited Read(FBOM[2], 1));
      if FBOM[2] = $BF then
        FBOMType := bomUtf8;
    end
    else if (FBOM[0] = $FF) and (FBOM[1] = $FE) then
    begin
      Inc(FBOMLen, inherited Read(FBOM[2], 2));
      if (FBOM[2] = 0) and (FBOM[3] = 0) then
        FBOMType := bomUcs4LE
      else
      begin
        Dec(FBOMLen, 2);
        ExtraData := PWideChar(@FBOM[2])^;
        FBOMType := bomUcs2LE;
      end;
    end
    else if (FBOM[0] = $FE) and (FBOM[1] = $FF) then
    begin
      FBOMType := bomUcs2BE;
    end
    else if (FBOM[0] = 0) and (FBOM[1] = 0) then
    begin
      Inc(FBOMLen, inherited Read(FBOM[2], 2));
      if (FBOM[2] = $FE) and (FBOM[3] = $FF) then
        FBOMType := bomUcs4BE;
    end;
  end;

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
  GetMem(Pointer(Result), InstanceSize);
  PInteger(Result)^ := Integer(Self);
end;

procedure TFileCacheReaderStream.FreeInstance;
begin
  FreeMem(Pointer(Self));
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
  GetMem(Pointer(Result), InstanceSize);
  PInteger(Result)^ := Integer(Self);
end;

constructor TFileCache.Create(hFile: THandle; {Size: Integer; }AOrgStreamData: POrgStreamData);
const
  // BCB 5, 6 abort with buffers larger than 16Kb if the file is opened in the editor
  BufReadSize = 15872; // The compiler uses this number of bytes for read operations.
var
  n: Integer;
  Stream: TStream;
  ASize: Integer;
begin
  inherited Create;
//  FSize := ASize;
  FReaders := TObjectList.Create;

//  if ASize = -1 then
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
{  end
  else
  begin
    if FSize > 0 then
    begin
      GetMem(FBuffer, FSize);
      AOrgStreamData.Read(hFile, PByte(FBuffer), FSize);
    end;}
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

end.
