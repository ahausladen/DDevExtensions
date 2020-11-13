{******************************************************************************}
{*                                                                            *}
{* CompileInterceptor IDE Plugin                                              *}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

// JCL_DEBUG_EXPERT_INSERTJDBG OFF

library CompileInterceptorW;

{$I CompileInterceptor.inc}

{$IF CompilerVersion >= 21.0}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}

uses
  ShareMem,
  Windows,
  SysUtils,
  Classes,
  Registry,
  CompilerHooks in 'CompilerHooks.pas',
  FileStreams in 'FileStreams.pas',
  InterceptIntf in 'InterceptIntf.pas',
  InterceptImpl in 'InterceptImpl.pas',
  IdeDllNames in 'IdeDllNames.pas',
  ToolsAPIIntf in 'ToolsAPIIntf.pas';

{$R *.res}
{
const
  WizardEntryPoint = 'INITWIZARD0001';

type
  TWizardRegisterProc = function(const Wizard: IInterface): Boolean;
  TWizardTerminateProc = procedure;

var
  Libs: array of record
    h: HMODULE;
    DoneProc: TDoneProc;
  end;

procedure DoneWizard;
var
  i: Integer;
begin
  try
    for i := 0 to High(Libs) do
      if Assigned(Libs[i].DoneProc) then
        Libs[i].DoneProc;
  finally
    FiniCompileInterceptor; // release interfaces first
  end;

  for i := 0 to High(Libs) do
    if Libs[i].h <> 0 then
      FreeLibrary(Libs[i].h);
end;

function InitWizard(const BorlandIDEServices: IInterface;
  RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc): Boolean; stdcall;
var
  Reg: TRegistry;
  List: TStrings;
  i: Integer;
  EntryPoint: TCompileInterceptorEntryPoint;
begin
  Terminate := DoneWizard;
  Result := True;

  InitCompileInterceptor;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly('\Software\DelphiTools\CompileInterceptor\Plugins') then
    begin
      List := TStringList.Create;
      try
        Reg.GetValueNames(List);
        SetLength(Libs, List.Count);
        for i := 0 to List.Count - 1 do
          if (Reg.ReadString(List[i]) <> '') and FileExists(List[i]) then
          begin
            Libs[i].h := LoadLibrary(PChar(List[i]));
            if Libs[i].h <> 0 then
            begin
              EntryPoint := GetProcAddress(Libs[i].h, CompileInterceptorEntryPoint);
              if Assigned(EntryPoint) then
                EntryPoint(CompileInterceptorServices, Libs[i].DoneProc);
            end;
          end;
      finally
        List.Free;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

exports
  InitWizard name WizardEntryPoint;}

begin
end.
