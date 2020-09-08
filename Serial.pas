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

  {Define the Propeller Debug thread}
  TDebugThread = class(TThread)
  private
    FCallerThread  : Cardinal;
    function  Receive(Timeout: Int64; Connected: Boolean = True; Template: Boolean = False): Byte;
    procedure Finish(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(CallingThread: Cardinal); reintroduce;
  end;

  {Define the Propeller Serial object}
  TPropellerSerial = class(TObject)
    FComPort       : String;
    FCommOpen      : Boolean;
    FCommHandle    : THandle;
    FCommDCB       : TDCB;
    FCommOverlap   : Overlapped;
    FCommIOEvent   : THandle;
    FRxBuffStart   : Cardinal;
    FRxBuffEnd     : Cardinal;

    procedure OpenComm;
    procedure CloseComm;
  private
    FGUIProcHandle : THandle;                    {Handle to GUI Thread (main process)}
    FDebugThread   : TDebugThread;               {Debug thread}
//    procedure Communicate();
    procedure WaitForCommunicationThread;
  public
    constructor Create; reintroduce;
  end;

  {Global Routines}
  procedure UpdateSerialStatus(Value: Cardinal); stdcall;

var
  CommInProgress : Boolean;                      {True = communication in process; False = communication done}

  FRxBuff        : array[0..RxBuffSize-1] of byte;


implementation

{##############################################################################}
{##############################################################################}
{############################# Global Routines ################################}
{##############################################################################}
{##############################################################################}

procedure UpdateSerialStatus(Value: Cardinal);
{Update serial status (identify/download progress).
 NOTE: This method is called by the TCommunicationThread via APC (Asynchronous Procedure Call) when it has a status update.}
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

function TDebugThread.Receive(Timeout: Int64; Connected: Boolean = True; Template: Boolean = False): Byte;
{Receive from Propeller.
 Propeller 1: Response is an encoded bit inside of a byte.  Optionally, we transmit a timing template if necessary.
 Propeller 2: Response is a byte.

 Timeout:   Maximum wait period for receiving anything.
 Connected: True: established communication already.
            False: haven't established communication yet.
 Template:  True: [Propeller 1 only] if no response, transmit timing template.
            False: if no response, fail.}
var
  StartTime  : Int64;
  Read       : Boolean; {True = data already read before ReadFile returned; False = data read in progress}
  WaitResult : Cardinal;
  X          : Cardinal; {Dummy variable for ReadFile}

    {----------------}

    procedure ReadError;
    {Notify of read error}
    begin
//      Error(0);
    end;

    {----------------}

begin
  FCommOverlap.Offset := 0;
  FCommOverlap.OffsetHigh := 0;
  StartTime := GetTickCount;
  repeat                                                                                        {Loop...}
    if FRxBuffStart = FRxBuffEnd then                                                             {Buffer empty, check Rx}
      begin
      FRxBuffStart := 0;                                                                            {Reset start}
      QueueUserAPC(@UpdateSerialStatus, FCallerThread, 0);                                          {Update GUI - Progressing (receiving bit)}
      Read := ReadFile(FCommHandle, FRxBuff, RxBuffSize, X, @FCommOverlap);                         {Read Rx}
      if not Read then                                                                              {Data not entirely read yet?}
        begin
        if GetLastError <> ERROR_IO_PENDING then ReadError;                                           {Error, unable to read}
        WaitResult := waitforsingleobject(FCommIOEvent, 1000);                                        {Wait for completion, or 1 second, whichever comes first}
//        if WaitResult = WAIT_FAILED then Error(0);
        if WaitResult = WAIT_TIMEOUT then ReadError;                                                  {Error, timed-out on read of PC hardware}
        end;
      if not GetOverlappedResult(FCommHandle, FCommOverlap, FRxBuffEnd, True) then ReadError;       {Get count of received bytes; error if necessary}
      end;
    if FRxBuffStart <> FRxBuffEnd then                                                            {Buffer has data, parse it}
      begin
      Result := FRxBuff[FRxBuffStart];                                                              {P2 gets raw byte, P1 byte is translated to properly-formed data to 0 or 1; improper data will be > 1}
      Inc(FRxBuffStart);
      if not Connected then Exit;                                                                   {P2? Exit returning Result; P1 & Result properly-formed (or ill-formed but not yet connected)? exit, returning Result}
      end;
  until GetTickCount - StartTime > Timeout;                                                       {Loop back until time-out}
end;

{------------------------------------------------------------------------------}

procedure TDebugThread.Finish(Sender: TObject);
{Clean up in preparation to terminate communication thread.}
begin
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
  OpenComm;



  {Update GUI - Done}
  QueueUserAPC(@UpdateSerialStatus, FCallerThread, 0);

  CloseComm;
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooooo Public Routines oooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

constructor TDebugThread.Create(CallingThread: Cardinal);
{Create the communication thread and copy the binary image (if available) to download.  If the binary image is not available, only scanning and version check of the PropModel Propeller chip
 will be performed (in the .Execute method). CallingThread must be the handle of the calling thread (the GUI process).  LastPortUsed must be the ID of the last serial port successfully
 used to communication to a Propeller chip (0 if none).  This method (TCommunicationThread.Create) is the only method within this object that is executed in the context of the GUI thread.
 For this reason, this method copies the list of available serial ports and the number of excluded ports from the COM object, and also copies some serial-related preference items for use
 later by the TCommunicationThread (so no multi-thread shared-resource management is necessary).
 NOTE: The regular procedure UpdateSerialStatus (with stdcall declaration) is called via APCs from this Communication Thread to provide status updates.}
var
  Idx : Integer;
begin
  {NOTE: This method is executed in the context of the calling thread (GUI thread), making it safe to access global objects (like CPrefs and the COM object) that are not thread-aware.}
  FreeOnTerminate := True;
  OnTerminate := Finish;
  {Store caller and last-used information}
  FCallerThread := CallingThread;
  {Create I/O Event and set overlapped structure}
  FCommIOEvent := createevent(nil, True, False, nil);
  FCommOverlap.hEvent := FCommIOEvent;
  {Copy binary image (if available) for downloading}
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

procedure TDebugThread.OpenComm;
{Open comm port}
const
  CommTimeouts: TCOMMTIMEOUTS =
    (ReadIntervalTimeout         : MAXDWORD;
     ReadTotalTimeoutMultiplier  : 0;
     ReadTotalTimeoutConstant    : 0;
     WriteTotalTimeoutMultiplier : 0;
     WriteTotalTimeoutConstant   : 0);
begin
  FCommOpen := False;
  FCommHandle := CreateFile(PChar('\\.\' + FComPort), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
//  if FCommHandle = INVALID_HANDLE_VALUE then Error();  {Failed to open port}
//  if not SetupComm(FCommHandle, 0, TxBuffSize) then Error();
  FCommDCB.DCBlength := sizeof(TDCB);
  if GetCommState(FCommHandle, FCommDCB) then
    begin {Got serial port configuration data; adjust for our use}
    FCommDCB.BaudRate := P2BaudRate;
    FCommDCB.Parity   := NOPARITY;
    FCommDCB.ByteSize := 8;
    FCommDCB.StopBits := ONESTOPBIT;
    FCommDCB.Flags    := 0;
    end;
//  else    {Unable to get serial port configuration data}
//    Error(0);
  SetCommState(FCommHandle, FCommDCB);
  SetCommTimeouts(FCommHandle, CommTimeouts);
  FRxBuffStart := 0;
  FRxBuffEnd := 0;
  FCommOpen := True;
end;

{------------------------------------------------------------------------------}

procedure TDebugThread.CloseComm;
{Close comm port}
begin
  if FCommOpen then
    begin
    CancelIO(FCommHandle);                      {Cancel any pending I/O on port}
    CloseHandle(FCommHandle);
    FCommOpen := False;
    end;
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooo Private Routines oooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

procedure TPropellerSerial.Communicate();
{Communicate with the PropModel Propeller chip.  If BinaryImage = nil, a Version Check is performed, otherwise, a Download is performed.
This method launches a Communication Thread to scan available serial ports and search for a Propeller chip, then either checks the version or downloads
an application (BinaryImage) to that Propeller chip.}

begin
  try
    FVerResult := 0;                                                                                                                    {Initialize version result}
    if FGUIProcHandle = INVALID_HANDLE_VALUE then abort;                                                                                {Abort if GUI handle unavailable}
  except {Handle aborts by simply exiting}
  end;
end;

{------------------------------------------------------------------------------}

procedure TPropellerSerial.WaitForCommunicationThread;
{Wait for communication thread to finish.  This is accomplished by sleeping in an alertable state for 1/2 the ProgressForm's increment delay period, or until the next message is received
from the communication thread.  Once woken, the UpdateSerialStatus regular procedure is executed (if communication thread queued a message via asynchronous procedure call) and Windows messages
are processed for this application.  This method finishes and exits when CommInProgress is false; the communication thread indicated it was done.}
begin
  while CommInProgress do {Wait for communication thread to finish, updating the screen as necessary along the way.}
    begin
    sleepex(0, True);
    application.processmessages;
    end;
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooo Public Routines oooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

constructor TPropellerSerial.Create;
{Create Propeller Serial object}
begin
  {Duplicate our "GUI" thread's pseudo-handle to make it usable by any of our threads}
  if not DuplicateHandle(GetCurrentProcess, GetCurrentThread, GetCurrentProcess, @FGUIProcHandle, 0, False, DUPLICATE_SAME_ACCESS) then
    FGUIProcHandle := INVALID_HANDLE_VALUE; {Failed to create process handle}
  inherited Create;
  if FGUIProcHandle = INVALID_HANDLE_VALUE then raise EDupHandle.Create('Unable to create GUI handle; serial communication disabled.');
end;

{------------------------------------------------------------------------------}

end.


