unit Serial;

{Chip: The TPropellerSerial class represents your PNut serial class or routines in your serial unit.  They are executed by the GUI thread.
       The TDebugThread class is the separate thread that only reads from the already-open serial port and places received data in the circular buffer (defined by the TPropellerSerial class)}

interface

uses
  Windows, SysUtils, Classes, Forms;

const
  P2BaudRate  = 2000000;
  RxBuffSize  = 4096;  {NOTE: Must always result in an even number or SetupComm in Win API will fail.}

type
  {Custom Exceptions}
  EDupHandle = class(Exception);

  {Define the Propeller Debug thread (runs independent of the GUI thread)}
  TDebugThread = class(TThread)
  private
    FCallerThread  : Cardinal;
    FCommOverlap   : Overlapped;
    FCommIOEvent   : THandle;
    procedure Receive;
    procedure Finish(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(CallingThread: Cardinal); reintroduce;
  end;

  {Define the Propeller Serial object}
  TPropellerSerial = class(TObject)
    FCommDCB       : TDCB;
    function OpenComm: Boolean;
    procedure CloseComm;
  private
    FGUIProcHandle : THandle;                    {Handle to GUI Thread (main process)}
    FDebugThread   : TDebugThread;               {Debug thread object}
//    procedure WaitForCommunicationThread;
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
{####################### TCommunicationThread Routines ########################}
{##############################################################################}
{##############################################################################}

{The TDebugThread is created by the TPropellerSerial object when it is ready to debug on the already open port.  The TPropellerSerial object executes in the context of the main GUI thread.
 The TDebugThread runs unencumbered by any GUI messages; it has no GUI components to deal with, so it performs fast retrieval of data from the serial port.

'

 The TPropellerSerial object waits for those
queued APC calls and executes them, as well as the normal GUI Windows messages, until the TCommunicationThread is done.  This way, the GUI thread responds to Windows messages and also updates
the ProgressForm's state in a timely manner, while all communication can freely execute with little or no interruption (only those that are associated with normal O.S. task switching).}

{------------------------------------------------------------------------------}

procedure TDebugThread.Receive;
{Receive serial data.}
var
  WaitResult : Cardinal;
  X          : Cardinal; {Dummy variable for ReadFile}

    {----------------}

    procedure ReadError;
    begin
      abort;     //!!! Need to define an error response
    end;

    {----------------}

begin
  try
    if not ReadFile(CommHandle, RxBuff, RxBuffSize, X, @FCommOverlap) then                     {Read Rx (overlapped); true = complete}
      begin {Read operation incomplete (or error)}
      if GetLastError <> ERROR_IO_PENDING then ReadError;                                      {If not IO Pending, Error: unable to read from the port}
      WaitResult := WaitForSingleObjectEx(FCommIOEvent, INFINITE, True);                       {Wait for I/O completion or alert state (ie: GUI thread contacted us)}
      if WaitResult = WAIT_FAILED then ReadError;
      end;
    if not GetOverlappedResult(CommHandle, FCommOverlap, RxTail, False) then ReadError;        {Get count of received bytes; error if necessary}
  except {Handle exceptions silently}
  end;
end;

{------------------------------------------------------------------------------}

procedure TDebugThread.Finish(Sender: TObject);
{Clean up in preparation to terminate thread.}
begin
  CancelIO(CommHandle);                      {Cancel any pending I/O on port}
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooo Protected Routines ooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

procedure TDebugThread.Execute;
{Communication Thread's main method.  Scan all available serial ports for a Propeller chip and retrieve its version number, and optionally download application code.
 If FBinImage = nil, the thread terminates after scanning and possibly finding and retrieving a Propeller chip version number.
 If FBinImage <> nil, the thread terminates after scanning and possibly retrieving version number and downloading an application to the found Propeller chip.}
begin
  {Exit if I/O event handle invalid}
//  if FCommIOEvent = 0 then Error(0);
  while not Terminated do
   begin
   Receive;
   end;
  {Update GUI - Done}
//  QueueUserAPC(@UpdateSerialStatus, FCallerThread, 0);

end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooooo Public Routines oooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

constructor TDebugThread.Create(CallingThread: Cardinal);
var
  Idx : Integer;
begin
  {NOTE: This method is executed in the context of the calling thread (GUI thread), making it safe to access global objects  that are not thread-aware.}
  FreeOnTerminate := True;
  OnTerminate := Finish;
  {Store caller and last-used information}
  FCallerThread := CallingThread;
  {Create I/O Event (auto-reset and initially nonsignaled) and configure into overlapped structure for I/O events (offset 0)}
  FCommIOEvent := createevent(nil, False, False, nil);
  FCommOverlap.Offset := 0;
  FCommOverlap.OffsetHigh := 0;
  FCommOverlap.hEvent := FCommIOEvent;
  {Start thread}
  inherited Create(False);
end;

{##############################################################################}
{##############################################################################}
{######################### TPropellerSerial Routines ##########################}
{##############################################################################}
{##############################################################################}

{... cause a TDebugThread to be created and executed.  The GUI thread (still executing the .???????????? method) then processes normal Windows messages (GUI related) and also waits for
asynchronous procedure calls that are queued by the TDebugThread to convey that debug data has arrived.  Those APC calls are also executed in the context of the GUI thread.
This way, the GUI thread responds to Windows messages and also ??????????????, while all communication can freely execute with little or no interruption (only those that are associated
with normal O.S. task switching).}

{------------------------------------------------------------------------------}

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

function TPropellerSerial.StartDebug: Boolean;
{Create separate "debug" thread to receive serial data and place it in buffer}
begin
  result := True;  {Assume success}
  try
    if FGUIProcHandle = INVALID_HANDLE_VALUE then abort;                            {Abort if GUI handle unavailable}
    if FDebugThread = nil then FDebugThread := TDebugThread.Create(FGUIProcHandle); {Otherwise, create debug thread}
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
      FDebugThread.Terminate;
      QueueUserAPC(@TerminateDebug, FDebugThread.Handle, 0);
      end;
    FDebugThread := nil;
  except {Handle aborts by simply exiting}
  end;
end;

{------------------------------------------------------------------------------}

//procedure TPropellerSerial.WaitForCommunicationThread;
//{Wait for communication thread to finish.  This is accomplished by sleeping in an alertable state for 1/2 the ProgressForm's increment delay period, or until the next message is received
//from the communication thread.  Once woken, the UpdateSerialStatus regular procedure is executed (if communication thread queued a message via asynchronous procedure call) and Windows messages
//are processed for this application.  This method finishes and exits when CommInProgress is false; the communication thread indicated it was done.}
//begin
//  while CommInProgress do {Wait for communication thread to finish, updating the screen as necessary along the way.}
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
  {Initialize CommHandle to invalid}
  CommHandle := INVALID_HANDLE_VALUE;
  FDebugThread := nil;
  {Duplicate our "GUI" thread's pseudo-handle to make it usable by any of our threads}
  if not DuplicateHandle(GetCurrentProcess, GetCurrentThread, GetCurrentProcess, @FGUIProcHandle, 0, False, DUPLICATE_SAME_ACCESS) then
    begin
    FGUIProcHandle := INVALID_HANDLE_VALUE; {Failed to create process handle}
    raise EDupHandle.Create('Unable to create GUI handle; serial communication disabled.');
    end;
  inherited Create;
end;

{------------------------------------------------------------------------------}

end.


