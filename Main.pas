unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Serial, StdCtrls;

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
  Form1: TForm1;
  Ser:   TPropellerSerial;

implementation

{$R *.dfm}



procedure TForm1.PortButtonClick(Sender: TObject);
begin
  if PortButton.Caption = 'Open Port' then
    begin
    ComPort := PortEdit.Text;
    Ser.OpenComm;
    if Ser.StartDebug then
      PortButton.Caption := 'Close Port'
    else
      Ser.CloseComm
    end
  else
    begin
    Ser.StopDebug;
    Ser.CloseComm;
    PortButton.Caption := 'Close Port';
    end
end;

Initialization
  Ser := TPropellerSerial.Create;


Finalization
  Ser.Destroy;

end.
