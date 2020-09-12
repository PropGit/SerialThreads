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
  Len      : Integer;
  PStr     : PChar;
  Lines    : TStrings;
  Patch    : Boolean;

{  PtStr    : String;
  PtLen    : Integer;}

begin
  Lines := TStringList.Create;
  Patch := False;
  try
    while Debugging do
      begin
      {Calc length of received data}
      Len := ifthen(RxHead >= RxTail, RxHead, RxBuffSize) - RxTail;
      if Len > 0 then
        begin {Data available}
        {Move received data from buffer}
        PStr := StrAlloc(Len+1);
        CopyMemory(PStr, @RxBuff[RxTail], Len);
        PStr[Len] := char(0);
        RxTail := (RxTail + Len) mod RxBuffSize;
        {Parse data}
        ExtractStrings([], [], PStr, Lines);
        if not Patch then
          RxMemo.Lines.AddStrings(Lines)
        else
          begin
          RxMemo.Lines[RxMemo.Lines.Count-1] := RxMemo.Lines[RxMemo.Lines.Count-1] + Lines[0];
          Lines.Delete(0);
          RxMemo.Lines.AddStrings(Lines);
          end;
        if PStr[Len-1] <> char(10) then Patch := True;
        StrDispose(PStr);
        Lines.Clear;

{
          PtStr := RxMemo.Lines.Strings[RxMemo.Lines.Count-1];
          RxMemo.Lines.Delete(RxMemo.Lines.Count-1);
          PtLen := length(PtStr);
          PStr := StrAlloc(PtLen+Len+1);
          CopyMemory(PStr, @PtStr[1], PtLen);
          CopyMemory(@PStr[PtLen], @RxBuff[RxTail], Len);
          PStr[PtLen+Len] := char(0);
          end;
        RxMemo.Lines.Append(StrPas(PStr));}
        end; {data available}
      Application.ProcessMessages;
      Sleep(10);
      end; {while debugging}
  finally
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
