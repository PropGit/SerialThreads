unit Serial;

{Chip: The TPropellerSerial class represents your PNut serial class or routines in your serial unit.  They are executed by the GUI thread.
       The TDebugThread class is the separate thread that only reads from the already-open serial port and places received data in the circular buffer (defined by the TPropellerSerial class)}

interface

uses
  Windows, SysUtils, Classes, Forms, Dialogs, Math;

const
  P2BaudRate  = 2000000;
  RxBuffSize  = 256;  {NOTE: Must always result in an even number or SetupComm in Win API will fail.}

type
  {Custom Exceptions}
//  EDupHandle = class(Exception);
  ETerminate = class(Exception);   {GUI Requested Debug Thread termination}
  EReadFailed = class(Exception);  {ReadFile (on serial port) failed}
  EWaitFailed = class(Exception);  {Wait (on serial port) failed}
  EGIOFailed = class(Exception);   {Get pending I/O (on serial port) failed}

  {IDs representing the above "Failed" exceptions}
  TFailedCode = (ecReadFailed, ecWaitFailed, ecGIOFailed);

const
  {Define indexes and IDs for events used by TDebugThread}
  IOEvent = 0;                                  {Index of I/O event object in TDebugThread's FEvents array}
  TerminateRequest = 1;                         {Index of Terminate Request event object in TDebugThread's FEvents array}
  DataRcvd = WAIT_OBJECT_0 + IOEvent;           {ID of I/O Event; returned by WaitForMultipleObjects()}
  TermReqd = WAIT_OBJECT_0 + TerminateRequest;  {ID of Terminate Request event; returned by WaitForMultipleObjects()}

type
  {Define the Propeller Debug thread (runs independent of the GUI thread)}
  TDebugThread = class(TThread)
  private
//    FCallerThread  : Cardinal;
    FCommOverlap   : Overlapped;
    FEvents        : TWOHandleArray; {Array of event objects; holds I/O event and GUI Alert event objects}
    FFailCode      : TFailedCode;
    FErrorCode     : Cardinal;
    procedure Error(FCode: TFailedCode; ErrorCode: Cardinal);
    procedure DisplayError;
  protected
    procedure Execute; override;
  public
    constructor Create(TermEvent: THandle); reintroduce;  //(CallingThread: Cardinal); reintroduce;
  end;

  {Define the Propeller Serial object}
  TPropellerSerial = class(TObject)
    function OpenComm: Boolean;
    procedure CloseComm;
  private
//    FGUIProcHandle : THandle;                  {Handle to GUI Thread (main process)}
    FCommDCB         : TDCB;                     {Serial port data structure}
    FDebugThread     : TDebugThread;             {Debug thread object}
    FDebugTerminate  : THandle;                  {Event to signal Debug thread termination}
//    procedure WaitForDebugThread;
  public
    constructor Create; reintroduce;
    function StartDebug: Boolean;                {Start Debug Thread}
    procedure StopDebug;                         {Stop Debug Thread}
  end;

  {Global Routines}
  procedure TerminateDebug(Value: Cardinal); stdcall;

var
  RxBuff        : array[0..RxBuffSize-1] of byte;
  RxHead        : Cardinal;
  RxTail        : Cardinal;
  ComPort       : String;
  CommHandle    : THandle;


implementation

{##############################################################################}
{##############################################################################}
{############################# Global Routines ################################}
{##############################################################################}
{##############################################################################}

procedure TerminateDebug(Value: Cardinal);
{Dummy regular procedure; serves only as a target for a wait-state-interrupting APC call to Debug Thread.}
begin
end;

{##############################################################################}
{##############################################################################}
{########################### TDebugThread Routines ############################}
{##############################################################################}
{##############################################################################}

{The TDebugThread is used by the TPropellerSerial object when it is ready to debug on the already-open port.
 The TPropellerSerial object executes in the context of the main GUI thread and the TDebugThread runs unencumbered
 by Windows messages; it has no GUI components to deal with, so it performs fast retrieval of data from the serial port.}

procedure TDebugThread.Error(FCode: TFailedCode; ErrorCode: Cardinal);
{Fatal error occured; prep to self terminate thread and alert GUI (user).}
begin
  {Store codes since Synchronize() disallows parameters}
  FFailCode := FCode;
  FErrorCode := ErrorCode;
  {Flag to terminate then call on GUI to DisplayError}
  Terminate;
  Synchronize(DisplayError);
end;

{------------------------------------------------------------------------------}

procedure TDebugThread.DisplayError;
{Display error.  This method is executed in the context of the GUI thread.}
begin
  MessageBeep(MB_ICONERROR);
  case FFailCode of
    ecReadFailed : MessageDlg('Serial port read error.  Code: ' + inttostr(FErrorCode) , mtError, [mbOK], 0);
    ecWaitFailed : MessageDlg('Serial port wait error.  Code: ' + inttostr(FErrorCode), mtError, [mbOK], 0);
    ecGIOFailed  : MessageDlg('Serial port "get data" error.  Code: ' + inttostr(FErrorCode), mtError, [mbOK], 0);
  end;
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooo Protected Routines ooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

procedure TDebugThread.Execute;
{Asynchronously receive serial data into RxBuff; sleep when nothing available.}
var
  RcvCount : Cardinal;    {Requested data count and actual received data count}

begin
  while not Terminated do                                                                                                    {Until we're flagged to terminate...}
    try                                                                                                                      {  we'll retrieve serial data as fast as possible.}
      if not ReadFile(CommHandle, RxBuff[RxHead], RxBuffSize-RxHead, RcvCount, @FCommOverlap) then                           {Start asynchronous Read; true = complete}
        begin {Async Read pending (or error)}
        if GetLastError <> ERROR_IO_PENDING then raise EReadFailed.Create('');                                                 {Read error? exit}
        case WaitForMultipleObjects(2, @FEvents, False, INFINITE) of                                                           {Else, wait for pending I/O or termination request...}
          DataRcvd : if not GetOverlappedResult(CommHandle, FCommOverlap, RcvCount, False) then raise EGIOFailed.Create('');     {Read done? Get count of received bytes; error if necessary}
          TermReqd : raise ETerminate.Create('');                                                                                {Terminate requested? Abort}
          else       raise EWaitFailed.Create('');                                                                               {Error? treat as wait failure}
        end; {case}
        end; {Async Read}                                                                                                    {Done!}
      RxHead := (RxHead + RcvCount) mod RxBuffSize;                                                                            {Adjust head}
    except {Handle exceptions}
      on EReadFailed do Error(ecReadFailed, GetLastError);                                                                   {Prompt user for... read fail}
      on EWaitFailed do Error(ecWaitFailed, GetLastError);                                                                   {... wait fail}
      on EGIOFailed do Error(ecGIOFailed, GetLastError);                                                                     {... overlapped I/O fail}
      on ETerminate do Terminate;                                                                                            {or terminate}
    end; {try..except / while..do}

  {Stop debugging; terminate after canceling any pending I/O on port}
  if CommHandle <> INVALID_HANDLE_VALUE then CancelIO(CommHandle);
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooooo Public Routines oooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

constructor TDebugThread.Create(TermEvent: THandle);
{TermEvent = GUI thread's termination request event object.
 This method is executed in the context of the GUI thread.}
begin
  FreeOnTerminate := True;
//  {Store caller and last-used information}
//  FCallerThread := CallingThread;
  {Create I/O and GUI Alert events (auto-reset and initially nonsignaled)}
  FEvents[IOEvent] := CreateEvent(nil, False, False, nil);
  FEvents[TerminateRequest] := TermEvent;
  {Configure overlapped structure for I/O events}
  FCommOverlap.Offset := 0;
  FCommOverlap.OffsetHigh := 0;
  FCommOverlap.hEvent := FEvents[IOEvent];
  {Start thread}
  inherited Create(False);
end;

{##############################################################################}
{##############################################################################}
{######################### TPropellerSerial Routines ##########################}
{##############################################################################}
{##############################################################################}

{TPropellerSerial manages the serial port (Open/Close) and Starts/Stops a separate TDebugThread that waits for and fetches data from the serial port.}

function TPropellerSerial.OpenComm: Boolean;
{Open comm port}
const
  CommTimeouts: TCOMMTIMEOUTS =
    (ReadIntervalTimeout         : MAXDWORD;
     ReadTotalTimeoutMultiplier  : MAXDWORD;
     ReadTotalTimeoutConstant    : MAXDWORD-1;
     WriteTotalTimeoutMultiplier : 0;
     WriteTotalTimeoutConstant   : 0);
begin
  result := False;
  CommHandle := CreateFile(PChar('\\.\' + ComPort), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
  if CommHandle = INVALID_HANDLE_VALUE then exit;                                {Failed to open port}
  if not SetupComm(CommHandle, 0, 4096) then begin CloseComm; exit; end;         {Failed to set port}
  FCommDCB.DCBlength := sizeof(TDCB);
  if GetCommState(CommHandle, FCommDCB) then
    begin {Got serial port configuration data; adjust for our use}
    FCommDCB.BaudRate := P2BaudRate;
    FCommDCB.Parity   := NOPARITY;
    FCommDCB.ByteSize := 8;
    FCommDCB.StopBits := ONESTOPBIT;
    FCommDCB.Flags    := 0;
    end
  else                                                                           {Failed to get serial port configuration data}
    begin
    CloseComm;
    Exit;
    end;
  SetCommState(CommHandle, FCommDCB);
  SetCommTimeouts(CommHandle, CommTimeouts);
  RxHead := 0;                                                                   {Head points to first empty buffer byte}
  RxTail := 0;                                                                   {Tail points to the next filled buffer byte (unless buffer empty; RxTail = RxHead)}
  result := True;
end;

{------------------------------------------------------------------------------}

procedure TPropellerSerial.CloseComm;
{Close comm port}
begin
  if CommHandle <> INVALID_HANDLE_VALUE then
    begin
    StopDebug;
    CloseHandle(CommHandle);
    CommHandle := INVALID_HANDLE_VALUE;
    end;
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooo Private Routines oooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

//{------------------------------------------------------------------------------}
//
//procedure TPropellerSerial.WaitForDebugThread;
//{Wait for Debug Thread.
// This is accomplished by sleeping in an alertable state until the next message is received.  Once woken, the DebugThread-requested regular procedure
// is executed (it issued a QueueUserAPC()) and Windows messages are processed for this application.}
//begin
//  while Debugging do {Wait for communication thread to finish, updating the screen as necessary along the way.}
//    begin
//    sleepex(0, True);
//    application.processmessages;
//    end;
//end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooo Public Routines oooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

constructor TPropellerSerial.Create;
{Create Propeller Serial object}
begin
//  {Initialize CommHandle to invalid}
//  CommHandle := INVALID_HANDLE_VALUE;
  {Initialize Debug Thread and Alert Event object to signal it}
  FDebugThread := nil;
  FDebugTerminate := CreateEvent(nil, False, False, nil);
//  {Duplicate our "GUI" thread's pseudo-handle to make it usable by any of our threads}
//  if not DuplicateHandle(GetCurrentProcess, GetCurrentThread, GetCurrentProcess, @FGUIProcHandle, 0, False, DUPLICATE_SAME_ACCESS) then
//    begin
//    FGUIProcHandle := INVALID_HANDLE_VALUE; {Failed to create process handle}
//    raise EDupHandle.Create('Unable to create GUI handle; serial communication disabled.');
//    end;
  inherited Create;
end;

{------------------------------------------------------------------------------}

function TPropellerSerial.StartDebug: Boolean;
{Create separate "debug" thread to receive serial data and place it in buffer}
begin
  result := True;  {Assume success}
  try
//    if FGUIProcHandle = INVALID_HANDLE_VALUE then abort;                            {Abort if GUI handle unavailable}
    if FDebugThread = nil then FDebugThread := TDebugThread.Create(FDebugTerminate);  {Create debug thread with handle to termination request event object}
  except
    result := False;
  end;
end;

{------------------------------------------------------------------------------}

procedure TPropellerSerial.StopDebug;
{Terminate separate "debug" thread}
begin
  {Signal Debug Thread to terminate}
  if FDebugThread <> nil then SetEvent(FDebugTerminate);
  FDebugThread := nil;
end;

//{------------------------------------------------------------------------------}
//
//Initialization
//  Debugging := False;
  
end.


