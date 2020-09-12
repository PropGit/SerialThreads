unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Serial, StdCtrls, Math;

type
  TForm1 = class(TForm)
    PortEdit: TEdit;
    Label1: TLabel;
    PortButton: TButton;
    RxMemo: TMemo;
    procedure PortButtonClick(Sender: TObject);
    procedure ParseAllRx;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1     : TForm1;
  Ser       : TPropellerSerial;
  Debugging : Boolean;  {Indicates debug thread is running}

implementation

{$R *.dfm}

procedure TForm1.PortButtonClick(Sender: TObject);
{Open/Close COM port and start/stop debug thread}
begin
  if not Debugging then
    begin
    ComPort := PortEdit.Text;
    if Ser.OpenComm and Ser.StartDebug then
      begin
      Debugging := True;
      PortButton.Caption := 'Close Port';
      RxMemo.Clear;
      ParseAllRx;
      end
    else
      Ser.CloseComm
    end
  else
    begin
    Debugging := False;
    Ser.StopDebug;
    Ser.CloseComm;
    PortButton.Caption := 'Open Port';
    end
end;

{------------------------------------------------------------------------------}

procedure TForm1.ParseAllRx;
var
  Len   : Cardinal;
  PStr  : PChar;
  Lines : TStrings;
  LRCh  : Char;      {Last received character}
const
  cEOL  = [char(13), char(10)];

begin
  Lines := TStringList.Create;
  LRCh := char(10);
  PStr := nil;
  try
    PStr := StrAlloc(RxBuffSize+1);
    while Debugging do
      begin
      {Calc length of received data}
      Len := ifthen(RxHead >= RxTail, RxHead, RxBuffSize) - RxTail;
      if Len > 0 then
        begin {Data available}
        {Move received data from buffer}
        CopyMemory(PStr, @RxBuff[RxTail], Len);
        PStr[Len] := char(0);
        RxTail := (RxTail + Len) mod RxBuffSize;
        {Parse data}
        ExtractStrings([], [], PStr, Lines);
        if (Lines.Count > 0) and not (LRCh in cEOL) and not (PStr[0] in cEOL) then
          begin {Partial line received prior; patch previous and next together}
          RxMemo.Lines[RxMemo.Lines.Count-1] := RxMemo.Lines[RxMemo.Lines.Count-1] + Lines[0];
          Lines.Delete(0);
          end;
        RxMemo.Lines.AddStrings(Lines);
        Lines.Clear;
        {Save last character for partial-line checking}
        LRCh := PStr[Len-1];
        end; {data available}
      Application.ProcessMessages;
      Sleep(10);
      end; {while debugging}
  finally
    StrDispose(PStr);
    Lines.Destroy;
  end;
end;

{------------------------------------------------------------------------------}

Initialization
  Ser := TPropellerSerial.Create;
  Debugging := False;

Finalization
  Ser.Destroy;

end.
