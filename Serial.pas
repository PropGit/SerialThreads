unit Serial;

{Chip: The TPropellerSerial class represents your PNut serial class or routines in your serial unit.  They are executed by the GUI thread.
       The TDebugThread class is the separate thread that only reads from the already-open serial port and places received data in the circular buffer (defined by the TPropellerSerial class)}

interface

uses
  Windows, SysUtils, Classes, Forms, Dialogs;

const
  P2BaudRate  = 2000000;
  RxBuffSize  = 4096;  {NOTE: Must always result in an even number or SetupComm in Win API will fail.}

type
  {Custom Exceptions}
//  EDupHandle = class(Exception);
  EGUISignaled = class(Exception);
  EReadFailed = class(Exception);  {ReadFile (on serial port) failed}
  EWaitFailed = class(Exception);  {Wait (on serial port) failed}
  EGIOFailed = class(Exception);   {Get pending I/O (on serial port) failed}

  {IDs representing the above "Failed" exceptions}
  TFailedCode = (ecReadFailed, ecWaitFailed, ecGIOFailed);

const
  {Define indexes and IDs for events used by TDebugThread}
  IOEvent = 0;                            {Index of I/O event object in TDebugThread's FEvents array}
  GUIAlert = 1;                           {Index of GUI Alert event object in TDebugThread's FEvents array}
  GUIAlerted = WAIT_OBJECT_0 + GUIAlert;  {ID of GUI Alert event (returned by WaitForMultipleObjects)}

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
    constructor Create(Alert: THandle); reintroduce;  //(CallingThread: Cardinal); reintroduce;
  end;

  {Define the Propeller Serial object}
  TPropellerSerial = class(TObject)
    function OpenComm: Boolean;
    procedure CloseComm;
  private
//    FGUIProcHandle : THandle;                  {Handle to GUI Thread (main process)}
    FCommDCB     : TDCB;
    FDebugAlert  : THandle;
    FDebugThread : TDebugThread;                 {Debug thread object}
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
//  Debugging     : Boolean;  {Indicates debug thread is running}


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
 The TPropellerSerial object executes in the context of the main GUI thread and the TDebugThread runs unencumbered by any GUI messages;
 it has no GUI components to deal with, so it performs fast retrieval of data from the serial port.}

procedure TDebugThread.Error(FCode: TFailedCode; ErrorCode: Cardinal);
{Prep to self terminate thread and alert GUI (user) that a serial port error occurred.}
begin
  FFailCode := FCode;
  FErrorCode := ErrorCode;
  Terminate;                      {Signal to self terminate}
  Synchronize(DisplayError);      {Display Error on GUI}
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
{Receive serial data.}
var
  GetData : Cardinal;    {Asynchronous Wait result (and also dummy variable for ReadFile)}

begin
  while not Terminated do
    begin
    {Receive serial data asynchronously}
    try
      if not ReadFile(CommHandle, RxBuff, RxBuffSize, GetData, @FCommOverlap) then                            {Start asynchronous Read; true = complete}
        begin {Read operation incomplete (or error)}
        if GetLastError <> ERROR_IO_PENDING then raise EReadFailed.Create('');                                  {ReadFile error?}
        GetData := WaitForMultipleObjects(2, @FEvents, False, INFINITE);                                        {Else I/O pending; wait for completion or alert (ie: GUI thread contacted us)}
        if GetData = GUIAlerted then raise EGUISignaled.Create('');                                             {Abort if alerted; handled silently}
        if GetData = WAIT_FAILED then raise EWaitFailed.Create('');                                             {Error if wait failed}
        end;                                                                                                  {Ready!}
      if not GetOverlappedResult(CommHandle, FCommOverlap, RxTail, False) then raise EGIOFailed.Create('');   {Get count of received bytes; error if necessary}
    except {Handle exceptions}
      on EReadFailed do Error(ecReadFailed, GetLastError);
      on EWaitFailed do Error(ecWaitFailed, GetLastError);
      on EGIOFailed do Error(ecGIOFailed, GetLastError);
      on EGUISignaled do;
    end; {try..except}
    end; {while..do}
  {Terminating thread now; cancel any pending I/O on port}
  if CommHandle <> INVALID_HANDLE_VALUE then CancelIO(CommHandle);
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooooo Public Routines oooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

constructor TDebugThread.Create(Alert: THandle);
{Alert = GUI thread's alert event object.}
begin
  {NOTE: This method is executed in the context of the calling thread (GUI thread), making it safe to access global objects  that are not thread-aware.}
  FreeOnTerminate := True;
//  {Store caller and last-used information}
//  FCallerThread := CallingThread;
  {Create I/O and GUI Alert events (auto-reset and initially nonsignaled)}
  FEvents[IOEvent] := CreateEvent(nil, False, False, nil);
  FEvents[GUIAlert] := Alert;
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
  RxHead := 0;
  RxTail := 0;
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
  FDebugAlert := CreateEvent(nil, False, False, nil);
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
    if FDebugThread = nil then FDebugThread := TDebugThread.Create(FDebugAlert);      {Create debug thread}
  except
    result := False;
  end;
end;

{------------------------------------------------------------------------------}

procedure TPropellerSerial.StopDebug;
{Terminate separate "debug" thread}
begin
  try
    {Terminate thread by flagging it than waking it up from its I/O wait state by queing an asynchronous procedure call to it}
    if FDebugThread <> nil then
      begin
      FDebugThread.Terminate;           {Signal Debug Thread to terminate}
      SetEvent(FDebugAlert);            {Wake the Debug Thread so it cooperatively terminates}
//      QueueUserAPC(@TerminateDebug, FDebugThread.Handle, 0);
      end;
    FDebugThread := nil;
  except {Handle aborts by simply exiting}
  end;
end;

//{------------------------------------------------------------------------------}
//
//Initialization
//  Debugging := False;
  
end.


