unit TaskbarIntf;

interface

uses
  Windows, CommCtrl, ComObj, ActiveX;

const
  {$EXTERNALSYM CLSID_TaskbarList}
  CLSID_TaskbarList: TGUID = '{56FDF344-FD6D-11d0-958A-006097C9A090}';

const
  {$EXTERNALSYM IID_ITaskbarList}
  IID_ITaskbarList: TGUID = '{56FDF342-FD6D-11d0-958A-006097C9A090}';

type
  {$EXTERNALSYM ITaskbarList}
  ITaskbarList = interface(IUnknown)
  ['{56FDF342-FD6D-11d0-958A-006097C9A090}']
    function HrInit: HResult; stdcall;
    function AddTab(hwnd: HWND): HResult; stdcall;
    function DeleteTab(hwnd: HWND): HResult; stdcall;
    function ActivateTab(hwnd: HWND): HResult; stdcall;
    function SetActiveAlt(hwnd: HWND): HResult; stdcall;
  end;

const
  {$EXTERNALSYM IID_ITaskbarList2}
  IID_ITaskbarList2: TGUID = '{602D4995-B13A-429b-A66E-1935E44F4317}';

type
  {$EXTERNALSYM ITaskbarList2}
  ITaskbarList2 = interface(ITaskbarList)
  ['{602D4995-B13A-429b-A66E-1935E44F4317}']
    function MarkFullscreenWindow(hwnd: HWND;
      fFullscreen: BOOL): HResult; stdcall;
  end;

const
  {$EXTERNALSYM THBF_ENABLED}
  THBF_ENABLED        = 0;            //The button is active and available to the user.
  {$EXTERNALSYM THBF_DISABLED}
  THBF_DISABLED       = $1;           //The button is disabled. It is present, but has a visual state that
                                      //indicates that it will not respond to user action.
  {$EXTERNALSYM THBF_DISMISSONCLICK}
  THBF_DISMISSONCLICK = $2;           //When the button is clicked, the taskbar button's flyout closes immediately.
  {$EXTERNALSYM THBF_NOBACKGROUND}
  THBF_NOBACKGROUND   = $4;           //Do not draw a button border, use only the image.
  {$EXTERNALSYM THBF_HIDDEN}
  THBF_HIDDEN         = $8;           //The button is not shown to the user.
  {$EXTERNALSYM THBF_NONINTERACTIVE}
  THBF_NONINTERACTIVE = $10;          //The button is enabled but not interactive; no pressed button state is
                                      //drawn. This value is intended for instances where the button is used in
                                      //a notification.

  {$EXTERNALSYM THB_BITMAP}
  THB_BITMAP  = $1;           //The iBitmap member contains valid information.
  {$EXTERNALSYM THB_ICON}
  THB_ICON    = $2;           //The hIcon member contains valid information.
  {$EXTERNALSYM THB_TOOLTIP}
  THB_TOOLTIP = $4;           //The szTip member contains valid information.
  {$EXTERNALSYM THB_FLAGS}
  THB_FLAGS   = $8;           //The dwFlags member contains valid information.

type
  PThumbButton = ^TThumbButton;
  {$EXTERNALSYM THUMBBUTTON}
  THUMBBUTTON = record
    dwMask: DWORD;                   //A combination of THUMBBUTTONMASK values that specify which members
                                      //of this structure contain valid data; other members are ignored,
                                      //with the exception of iId, which is always required.
    iID: UINT;
    iBitmap: UINT;
    hIcon: HICON;
    szTip: array [0..259] of WCHAR;
    dwFlags: DWORD;
  end;
  TThumbButton = THUMBBUTTON;
  {$EXTERNALSYM LPTHUMBBUTTON}
  LPTHUMBBUTTON = PThumbButton;

const
  {$EXTERNALSYM THBN_CLICKED}
  THBN_CLICKED = $1800;       //When a button in a thumbnail toolbar is clicked, the window associated with
                              //that thumbnail is sent a WM_COMMAND message with the HIWORD of its wParam
                              //parameter set to THBN_CLICKED and the LOWORD to the button ID.

  {$EXTERNALSYM TBPF_NOPROGRESS}
  TBPF_NOPROGRESS = 0;              //Stops displaying progress and returns the button to its normal state.
                                    //Call this method with this flag to dismiss the progress bar when the
                                    //operation is complete or cancelled.
  {$EXTERNALSYM TBPF_INDETERMINATE}
  TBPF_INDETERMINATE  = $1;         //The progress indicator does not grow in size, but cycles repeatedly
                                    //along the length of the taskbar button. This indicates activity without
                                    //specifying what proportion of the progress is complete. Progress is
                                    //taking place, but there is no prediction as to how long the operation
                                    //will take.
  {$EXTERNALSYM TBPF_NORMAL}
  TBPF_NORMAL = $2;                 //The progress indicator grows in size from left to right in proportion
                                    //to the estimated amount of the operation completed. This is a determinate
                                    //progress indicator; a prediction is being made as to the duration of the
                                    //operation.
  {$EXTERNALSYM TBPF_ERROR}
  TBPF_ERROR  = $4;                 //The progress indicator turns red to show that an error has occurred in one
                                    //of the windows that is broadcasting progress. This is a determinate state.
                                    //If the progress indicator is in the indeterminate state, it switches to a
                                    //red determinate display of a generic percentage not indicative of actual
                                    //progress.
  {$EXTERNALSYM TBPF_PAUSED}
  TBPF_PAUSED = $8;                 //The progress indicator turns yellow to show that progress is currently
                                    //stopped in one of the windows but can be resumed by the user.
                                    //No error condition exists and nothing is preventing the progress
                                    //from continuing. This is a determinate state. If the progress indicator
                                    //is in the indeterminate state, it switches to a yellow determinate display
                                    //of a generic percentage not indicative of actual progress.

const
  {$EXTERNALSYM IID_ITaskbarList3}
  IID_ITaskbarList3: TGUID = '{ea1afb91-9e28-4b86-90e9-9e9f8a5eefaf}';

type
  {$EXTERNALSYM ITaskbarList3}
  ITaskbarList3 = interface(ITaskBarList2)
  ['{ea1afb91-9e28-4b86-90e9-9e9f8a5eefaf}']
    function SetProgressValue(hwnd: HWND;
                              ullCompleted,
                              ullTotal: ULONGLONG): HRESULT ; stdcall;
    function SetProgressState(hwnd: HWND; tbpFlags: DWORD): HRESULT; stdcall;
    function RegisterTab(hwndTab, hwndMDI: HWND): HRESULT; stdcall;
    function UnregisterTab(hwndTab: HWND): HRESULT; stdcall;
    function SetTabOrder(hwndTab, hwndInsertBefore: HWND): HRESULT; stdcall;
    function SetTabActive(hwndTab, hwndMDI: HWND;
                          dwReserved: DWORD): HRESULT; stdcall;
    function ThumbBarAddButtons(hwnd: HWND; cButtons: UINT;
                                pButton: LPTHUMBBUTTON): HRESULT; stdcall;
    function ThumbBarUpdateButtons(hwnd: HWND; cButtons: UINT;
                                   pButton: LPTHUMBBUTTON): HRESULT; stdcall;
    function ThumbBarSetImageList(hwnd: HWND;
                                  himl: HIMAGELIST): HRESULT; stdcall;
    function SetOverlayIcon(hwnd: HWND; hIcon: HICON;
                            pszDescription: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailTooltip(hwnd: HWND;
                                 pszTip: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailClip(hwnd: HWND; prcClip: PRECT): HRESULT; stdcall;

  end;

const
  {$EXTERNALSYM STPF_NONE}
  STPF_NONE                       = $0;         //No specific property values are specified.
                                                //The default behavior is used: the tab window provides a
                                                //thumbnail and peek image, either live or static as appropriate.
  {$EXTERNALSYM STPF_USEAPPTHUMBNAILALWAYS}
  STPF_USEAPPTHUMBNAILALWAYS      = $1;         //Always use the thumbnail provided by the main application frame
                                                //window rather than a thumbnail provided by the individual tab
                                                //window. Do not combine this value with
                                                //STPF_USEAPPTHUMBNAILWHENACTIVE; doing so will result in an error.
  {$EXTERNALSYM STPF_USEAPPTHUMBNAILWHENACTIVE}
  STPF_USEAPPTHUMBNAILWHENACTIVE  = $2;         //When the application tab is active and a live representation of
                                                //its window is available, use the main application frame window
                                                //thumbnail. At other times, use the tab window thumbnail.
                                                //Do not combine this value with STPF_USEAPPTHUMBNAILALWAYS;
                                                //doing so will result in an error.
  {$EXTERNALSYM STPF_USEAPPPEEKALWAYS}
  STPF_USEAPPPEEKALWAYS           = $4;         //Always use the peek image provided by the main application
                                                //frame window rather than a peek image provided by the
                                                //individual tab window. Do not combine this value with
                                                //STPF_USEAPPPEEKWHENACTIVE; doing so will result in an error.

  {$EXTERNALSYM STPF_USEAPPPEEKWHENACTIVE}
  STPF_USEAPPPEEKWHENACTIVE       = $8;         //When the application tab is active and a live representation
                                                //of its window is available, show the main application frame
                                                //in the peek feature. At other times, use the tab window.
                                                //Do not combine this value with STPF_USEAPPPEEKALWAYS;
                                                //doing so will result in an error.

  {$EXTERNALSYM IID_ITaskbarList4}
  IID_ITaskbarList4: TGUID = '{c43dc798-95d1-4bea-9030-bb99e2983a1a}';

type
  {$EXTERNALSYM ITaskbarList4}
  ITaskbarList4 = interface(ITaskbarList3)
  ['{c43dc798-95d1-4bea-9030-bb99e2983a1a}']
    function SetTabProperties(hwndTab: HWND; stpFlags: DWORD): HRESULT; stdcall;
  end;

function CreateTaskbarList: ITaskbarList;

implementation

var
  CoInitialized: Boolean = False;

function CreateTaskbarList: ITaskbarList;
begin
  if not CoInitialized then
  begin
    case CoInitializeEx(nil, COINIT_APARTMENTTHREADED) of
      S_OK, S_FALSE:
        CoInitialized := True;
    end;
  end;

  Result := ITaskbarList(CreateComObject(CLSID_TaskbarList));
  if Result <> nil then
    Result.HrInit;
end;


initialization

finalization
  if CoInitialized then
    CoUninitialize;

end.
