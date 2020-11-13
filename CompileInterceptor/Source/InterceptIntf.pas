{******************************************************************************}
{*                                                                            *}
{* CompileInterceptor IDE Plugin                                              *}
{*                                                                            *}
{* (C) 2006-2011 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

unit InterceptIntf;

{$IFDEF UNICODE}
  {$STRINGCHECKS OFF}
{$ENDIF UNICODE}

interface

type
  TCompileInterceptOptions = type Cardinal;

const
  CIO_ALTERFILES       = $0001;  // The interceptor supports the AlterFile() method
  CIO_VIRTUALFILES     = $0002;  // The interceptor supports the VirtrualFile() method
  CIO_INSPECTFILENAMES = $0004;  // The interceptor supports the InspectFilename() method
  CIO_ALTERMESSAGES    = $0008;  // The interceptor supports the AlterMessage() method
  CIO_COMPILEPROJECTS  = $0010;  // The interceptor supports the CompileProject() method
  // Version 2.0
  CIO_VIRTUALOUTFILES  = $0020;  // The interceptor supports the CreateVirtualFile() method
  CIO_FILENAMEDATES    = $0040;  // The interceptor supports the FileNameDate() method

type
  TMsgKind = (mkHint, mkWarning, mkError, mkFatal, mkInfo);

  PUtf8Char = PAnsiChar;

  TInspectFileMode = (ifmOpen, ifmCreate);

  IUnicodeString = interface
    ['{3B33C7A5-63F4-4700-A6D5-4072D707536C}']
    function GetValue: PWideChar;
    procedure SetValue(P: PWideChar);
    procedure SetString(P: PWideChar; Len: Integer);
    function GetLength: Integer;

    property Value: PWideChar read GetValue write SetValue;
  end;

  IWideString = IUnicodeString;

  IVirtualStream = interface
    ['{6BBD7B93-9402-4534-ADD3-A3D287FD70E9}']
    function Seek(Offset: Integer; Origin: Integer): Integer; stdcall;
    function Read(var Buffer; Size: Integer): Integer; stdcall;
    procedure FileStatus(out FileDate: Integer; out FileSize: Integer); stdcall;
  end;

  IVirtualOutStream = interface(IVirtualStream)
    ['{9715EB14-952D-42B6-B0BF-4B949337CD8A}']
    function Write(const Buffer; Size: Integer): Integer; stdcall;
  end;

  ICompileInterceptor = interface
    ['{186D90CD-598B-4162-8E03-0BF8298A0826}']
      { GetOptions() returns the interceptor's options. }
    function GetOptions: TCompileInterceptOptions; stdcall;

      { GetVirtualFile() is called when the compiler wants to open a file. If
        the returned value is not NIL the compiler will use the virtual stream
        and AlterFile is not called.
        Requires CIO_VIRTUALFILE
        If the interface supports ICompileInterceptor20 this method isn't called. }
    function GetVirtualFile(Filename: PWideChar): IVirtualStream; stdcall; // deprecated 'implement OpenVirtualFile'

      { AlterFile() is called when the file is no virtual file and CIO_ALTERFILE
        is set. FileDate is obsolete and always 0. }
    function AlterFile(Filename: PWideChar; Content: PByte; FileDate, FileSize: Integer): IVirtualStream; stdcall;

      { InspectFilename() is called when a file is opened or created and
        CIO_INSPECTFILENAMES is set. }
    procedure InspectFilename(Filename: PWideChar; FileMode: TInspectFileMode); stdcall;

       { AlterMessage() is called when the compiler wants to display a message.
         The method must return True if it has changed one of the parameters. }
    function AlterMessage(IsCompilerMessage: Boolean; var MsgKind: TMsgKind; var Code: Integer;
      const Filename: IUnicodeString; var Line, Column: Integer; const Msg: IUnicodeString): Boolean; stdcall;

      { CompileProject is called before a project is compiled. }
    procedure CompileProject(ProjectFilename, UnitPaths, SourcePaths, DcuOutputDir: PWideChar;
      IsCodeInsight: Boolean; var Cancel: Boolean); stdcall;
  end;

  ICompileInterceptor20 = interface(ICompileInterceptor)
    ['{02B49BD8-69F4-4105-8592-E37408788840}']

      { CreateVirtualFile() is called when a file is created and CIO_VIRTUALOUTFILES is set.
        If the method returns False the file will be created with the default file creation
        function. If Stream is not nil and the method returns True, instead of creating a file
        the compiler writes the output into the stream. }
    function CreateVirtualFile(Filename: PWideChar; out Stream: IVirtualOutStream): Boolean; stdcall;

      { OpenVirtualFile() is called when the compiler wants to open a file. If
        the returned value is True, the compiler will use the virtual stream and AlterFile isn't
        called. If Stream is nil and the method returns True, no file will be opened and the compiler
        will fail with "file not found". If the method returns False the next handler is called and
        if that one also returns False, AlterFile is called (if CIO_ALTERFILE is set).
        Requires CIO_VIRTUALFILE. }
    function OpenVirtualFile(Filename: PWideChar; out Stream: IVirtualStream): Boolean; stdcall;

      { FileNameDate() is called when the compiler wants to retrieve the file age of a file.
        The method returns True if it has handled the call. Requires CIO_FILENAMEDATES. }
    function FileNameDate(Filename: PWideChar; out AFileDate: Integer): Boolean; stdcall;
  end;

  ICompileInterceptorServices = interface
    ['{CA696A1B-77EF-4EEB-9F22-9EE6E53B2B76}']
      { RegisterInterceptor() registers a compile interceptor. }
    function RegisterInterceptor(Interceptor: ICompileInterceptor): Integer; stdcall;
      { UnregisterInterceptor() removes the compile interceptor that is assigned
        to the specified ID. }
    procedure UnregisterInterceptor(Id: Integer); stdcall;

      { Returns the file content. The editor content is returned if the file is
        opened in the editor. }
    function GetFileContent(Filename: PWideChar): IVirtualStream; stdcall;
  end;

  TUnicodeStringAdapter = class(TInterfacedObject, IUnicodeString)
  private
    FValue: string;
  protected
    { IUnicodeString }
    function GetLength: Integer;
    function GetValue: PWideChar;
    procedure SetString(P: PWideChar; Len: Integer);
    procedure SetValue(P: PWideChar);
  public
    constructor Create(const AValue: string);
  end;

  TWideStringAdapter = TUnicodeStringAdapter;

  TGetCompileInterceptorServices = function: ICompileInterceptorServices; stdcall;
    { external 'CompileInterceptorW.dll' name 'GetCompileInterceptorServices'; }

//const
//  CompileInterceptorEntryPoint = 'CompileInterceptorEntry';
//
//type
//  TDoneProc = procedure; stdcall;
//  TCompileInterceptorEntryPoint = procedure(const CompileInterceptorServices: ICompileInterceptorServices; var DoneProc: TDoneProc); stdcall;

implementation

{ TUnicodeStringAdapter }

constructor TUnicodeStringAdapter.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

function TUnicodeStringAdapter.GetLength: Integer;
begin
  Result := Length(FValue);
end;

function TUnicodeStringAdapter.GetValue: PWideChar;
begin
  Result := PWideChar(FValue);
end;

procedure TUnicodeStringAdapter.SetString(P: PWideChar; Len: Integer);
begin
  System.SetString(FValue, P, Len);
end;

procedure TUnicodeStringAdapter.SetValue(P: PWideChar);
begin
  FValue := P;
end;

end.
