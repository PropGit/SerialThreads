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
var
  Len      : Integer;
  PStr     : PChar;
  Patch    : Boolean;
  PtStr    : String;
  PtLen    : Integer;

begin
  if not Debugging then
    begin
    ComPort := PortEdit.Text;
    if Ser.OpenComm and Ser.StartDebug then
      begin
      Debugging := True;
      PortButton.Caption := 'Close Port';
      RxMemo.Clear;
      Patch := False;
      while Debugging do
        begin
        Len := ifthen(RxHead >= RxTail, RxHead, RxBuffSize) - RxTail;
        if Len > 0 then
          begin
          if not Patch then
            begin
            PStr := StrAlloc(Len+1);
            CopyMemory(PStr, @RxBuff[RxTail], Len);
            PStr[Len] := char(0);
            end
          else
            begin
            PtStr := RxMemo.Lines.Strings[RxMemo.Lines.Count-1];
            RxMemo.Lines.Delete(RxMemo.Lines.Count-1);
            PtLen := length(PtStr);
            PStr := StrAlloc(PtLen+Len+1);
            CopyMemory(PStr, @PtStr[1], PtLen);
            CopyMemory(@PStr[PtLen], @RxBuff[RxTail], Len);
            PStr[PtLen+Len] := char(0);
            end;
          RxMemo.Lines.Append(StrPas(PStr));
          RxTail := (RxTail + Len) mod RxBuffSize;
          if PStr[Len-1] <> char(10) then
            Patch := True
          else
            if RxMemo.Lines[RxMemo.Lines.Count-1] = '' then RxMemo.Lines.Delete(RxMemo.Lines.Count-1);
          StrDispose(PStr);
          end;
        Application.ProcessMessages;
        Sleep(10);
        end
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

Initialization
  Ser := TPropellerSerial.Create;
  Debugging := False;

Finalization
  Ser.Destroy;

end.
