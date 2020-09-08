unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Serial;

type
  TForm1 = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  DBG:   TPropellerSerial;

implementation

{$R *.dfm}

Initialization

DBG := TPropellerSerial.Create;


Finalization

DBG.Destroy;

end.
