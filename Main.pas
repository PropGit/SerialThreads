unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Serial, StdCtrls, Math;

type
  TForm1 = class(TForm)
    PortEdit: TEdit;
    PortLabel: TLabel;
    PortButton: TButton;
    RxMemo: TMemo;
    BaudLabel: TLabel;
    BaudEdit: TEdit;
    BuffSizeLabel: TLabel;
    BuffSizeEdit: TEdit;
    { Event declarations }
    procedure PortButtonClick(Sender: TObject);
    procedure BaudEditExit(Sender: TObject);
    procedure BuffSizeEditExit(Sender: TObject);
  private
    { Non-Event declarations }
    procedure ParseAllRx;
    procedure SetControlState(Enabled: Boolean);
    function StrToInt(Str: String): Int64;
  public
    { Public declarations }
  end;

var
  Form1     : TForm1;
  Ser       : TPropellerSerial;
  Debugging : Boolean;  {Indicates debug thread is running}

implementation

{$R *.dfm}

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooo Event Routines oooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

procedure TForm1.PortButtonClick(Sender: TObject);
{Open/Close COM port and start/stop debug thread}
begin
  if not Debugging then
    begin
    ComPort := PortEdit.Text;
    if Ser.OpenComm and Ser.StartDebug then
      begin
      Debugging := True;
      SetControlState(False);
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
    SetControlState(True);
    end
end;

{------------------------------------------------------------------------------}

procedure TForm1.BaudEditExit(Sender: TObject);
begin
  BaudRate := StrToInt(BaudEdit.Text);
end;

{------------------------------------------------------------------------------}

procedure TForm1.BuffSizeEditExit(Sender: TObject);
begin
  MakeRxBuffer(StrToInt(BuffSizeEdit.Text));
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooo Non-Event Routines oooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

procedure TForm1.ParseAllRx;
var
  Len   : Cardinal;
  PStr  : PChar;
  Lines : TStrings;
  Patch : Boolean;      {True = must patch previous and current line together}
const
  EOL  = [char(13), char(10)];  {End of line characters}

begin
  Lines := TStringList.Create;
  Patch := False;
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
        {Parse data; extract non-blank lines}
        ExtractStrings([], [], PStr, Lines);
        {Handle patching of previous partial line}
        if (Lines.Count > 0) and Patch and not (PStr[0] in EOL) then
          begin {Partial line received prior and now; patch together}
          RxMemo.Lines[RxMemo.Lines.Count-1] := RxMemo.Lines[RxMemo.Lines.Count-1] + Lines[0];
          Lines.Delete(0);
          end;
        {Add lines to memo}
        RxMemo.Lines.AddStrings(Lines);
        {Partial line? Needs future patching}
        Patch := (Lines.Count > 0) and not (PStr[Len-1] in EOL);
        Lines.Clear;
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

procedure TForm1.SetControlState(Enabled: Boolean);
begin
  if Enabled then
    begin
    BuffSizeLabel.Font.Color := clBlack;
    BuffSizeEdit.ReadOnly := False;
    BuffSizeEdit.Color := clWhite;
    BaudLabel.Font.Color := clBlack;
    BaudEdit.ReadOnly := False;
    BaudEdit.Color := clWhite;
    PortLabel.Font.Color := clBlack;
    PortEdit.ReadOnly := False;
    PortEdit.Color := clWhite;
    PortButton.Caption := 'Open Port';
    end
  else
    begin
    BuffSizeLabel.Font.Color := clGray;
    BuffSizeEdit.ReadOnly := True;
    BuffSizeEdit.Color := clGray;
    BaudLabel.Font.Color := clGray;
    BaudEdit.ReadOnly := True;
    BaudEdit.Color := clGray;
    PortLabel.Font.Color := clGray;
    PortEdit.ReadOnly := True;
    PortEdit.Color := clGray;
    PortButton.Caption := 'Close Port';
    end;
end;

{------------------------------------------------------------------------------}

function TForm1.StrToInt(Str: String): Int64;
{Convert String to 64-bit Integer. If integer value is preceeded by non-digit data, searches until it finds the first valid
digit, then converts until the next invalid digit or end of string.}
var
  Idx   : Integer;
begin
  while (length(Str) > 0) and not (Str[1] in ['0'..'9', '-']) do delete(Str, 1, 1);
  Val(Str, Result, Idx);
end;

{------------------------------------------------------------------------------}

Initialization
  Ser := TPropellerSerial.Create;
  Debugging := False;

Finalization
  Ser.Destroy;

end.
