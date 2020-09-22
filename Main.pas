unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Serial, StdCtrls, Math, StrUtils;

type
  TPatType = (ST, AL, NM, WS, EL); {STart, ALpha, NuMeric, White Space, End of Line}

  {Pattern entry (used by PatList)}
  PPattern = ^TPattern;
  TPattern = record
    PType   : TPatType;       {The type of pattern}
    Size    : Cardinal;       {The length of the pattern}
    Content : String;         {The specific content (for exact pattern matching only)}
  end;
  
  TForm1 = class(TForm)
    PortEdit: TEdit;
    PortLabel: TLabel;
    PortButton: TButton;
    RxMemo: TMemo;
    BaudLabel: TLabel;
    BaudEdit: TEdit;
    BuffSizeLabel: TLabel;
    BuffSizeEdit: TEdit;
    TemplateCheckBox: TCheckBox;
    SkipLabel: TLabel;
    SkipEdit: TEdit;
    { Event declarations }
    procedure FormCreate(Sender: TObject);
    procedure BuffSizeEditExit(Sender: TObject);
    procedure BaudEditExit(Sender: TObject);
    procedure TemplateCheckBoxClick(Sender: TObject);
    procedure PortButtonClick(Sender: TObject);
  private
    { Non-Event declarations }
    procedure ParseAllRx;
    procedure SetControlState;
    procedure SetSkipState;
    function  StrToInt(Str: String): Int64;
    procedure ClearPatternList;
    function  AddPattern(PType: TPatType; Size: Cardinal; Content: String): PPattern;
  public
    { Public declarations }
  end;

var
  Form1     : TForm1;
  Ser       : TPropellerSerial;
  Debugging : Boolean;  {Indicates debug thread is running}
  PatMatch  : Boolean;  {Indicates pattern match (template) mode}
  PatList   : TList;

implementation

{$R *.dfm}

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooo Event Routines oooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

procedure TForm1.FormCreate(Sender: TObject);
begin
  BuffSizeEdit.Text := IntToStr(DefaultRxBufferSize);
  BaudEdit.Text := IntToStr(DefaultBaudRate);
  PortEdit.Text := DefaultPort;
  PatMatch := TemplateCheckbox.Checked;
end;

{------------------------------------------------------------------------------}

procedure TForm1.BuffSizeEditExit(Sender: TObject);
begin
  RxBuffSize := StrToInt(BuffSizeEdit.Text);
  MakeRxBuffer;
end;

{------------------------------------------------------------------------------}

procedure TForm1.BaudEditExit(Sender: TObject);
begin
  BaudRate := StrToInt(BaudEdit.Text);
end;

{------------------------------------------------------------------------------}

procedure TForm1.TemplateCheckBoxClick(Sender: TObject);
begin
  PatMatch := TemplateCheckbox.Checked;
  SetSkipState;
end;

{------------------------------------------------------------------------------}

procedure TForm1.PortButtonClick(Sender: TObject);
{Open/Close COM port and start/stop debug thread}
begin
  if not Debugging then
    begin
    ComPort := PortEdit.Text;
    if Ser.OpenComm and Ser.StartDebug then
      begin
      Debugging := True;
      SetControlState;
      RxMemo.Clear;
      ParseAllRx;
      end
    else
      Ser.CloseComm
    end
  else
    begin
    Debugging := False;
    SetControlState;
    Ser.StopDebug;
    Ser.CloseComm;
    end
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooo Non-Event Routines oooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

procedure TForm1.ParseAllRx;
const
  EOL   = [char(10), char(13)];       {End of line characters}
  {Character to Pattern Type (CtoPT).
  (WS) White Space [0..9, 11, 12, 14..32] (includes controls), (EL) End of Line [10, 13], (NM) Numeric [48..57], and (AL) Alpha [33..47, 58..255] (includes punctuation)}
  CtoPT : array[char] of TPatType =
          {0   1   2   3   4   5   6   7   8   TB  LF  11  12  CR  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31}
          (WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, EL, WS, WS, EL, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS, WS,
          {32  !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /   0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?}
           WS, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, NM, NM, NM, NM, NM, NM, NM, NM, NM, NM, AL, AL, AL, AL, AL, AL,
          {@   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _}
           AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL,
          {`   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   p   q   r   s   t   u   v   w   x   y   z   123 |   125 ~   ?}
           AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL,
          {128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159}
           AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL,
          {160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191}
           AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL,
          {À   Á   Â   Ã   Ä   Å   Æ   Ç   È   É   Ê   Ë   Ì   Í   Î   Ï   Ð   Ñ   Ò   Ó   Ô   Õ   Ö   ×   Ø   Ù   Ú   Û   Ü   Ý   Þ   ß}
           AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL,
          {à   á   â   ã   ä   å   æ   ç   è   é   ê   ë   ì   í   î   ï   ð   ñ   ò   ó   ô   õ   ö   ÷   ø   ù   ú   û   ü   ý   þ   8}
           AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL
          );
var
  Len      : Cardinal;
  PStr     : PChar;
  Lines    : TStrings;
  Patch    : Boolean;      {True = must patch previous and current line together}
  PatSet   : Boolean;      {True = setting pattern by example template}
  Idx      : Cardinal;     {Data index (for pattern matching)}
//  Sz       : Cardinal;     {Data type size (for pattern matching)}

    {----------------}

    procedure EmitLines;
    {Extract and emit lines of data from PStr into RxMemo}
    begin
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
    end;

    {----------------}

    procedure ParsePattern;
    {Parse pattern from incoming data}
    var
      NewState : TPatType;
      Pat      : TPattern;
    begin
      Pat := TPattern(PatList.Items[PatList.Count-1]^);
      Idx := 0;
      while Idx < Len do
        begin {For all current data in buffer...}
        {Get new state}
        NewState := CtoPT[PStr[Idx]];
        if NewState = Pat.PType then
          begin {Same as previous state?  Increment content length and store content if Alpha}
          inc(Pat.Size);
          if NewState = AL then Pat.Content := Pat.Content + PStr[Idx];
          end
        else    {Else, add new state to the list}
          Pat := TPattern(AddPattern(NewState, 1, ifthen(NewState <> AL, '', PStr[Idx]))^);
        inc(Idx);
        end; {for all current data}
    end;

    {----------------}

    procedure MatchPattern;
    {Match template pattern to incoming data}
    begin
    end;

    {----------------}

begin
  Lines := TStringList.Create;
  Patch := False;
  PStr := nil;
  ClearPatternList;
  AddPattern(ST, 0, '');
  PatSet := PatMatch;
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
        if not PatMatch then
          EmitLines
        else
          if PatSet then
            begin
            ParsePattern;
            EmitLines;
            end
          else
            MatchPattern;
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

procedure TForm1.SetControlState;
{Set enabled/disabled state of configuration controls depending on debugging status}
begin
  if Debugging then
    begin
    BuffSizeLabel.Font.Color := clGray;
    BuffSizeEdit.ReadOnly := True;
    BuffSizeEdit.Color := clBtnFace;
    BaudLabel.Font.Color := clGray;
    BaudEdit.ReadOnly := True;
    BaudEdit.Color := clBtnFace;
    PortLabel.Font.Color := clGray;
    PortEdit.ReadOnly := True;
    PortEdit.Color := clBtnFace;
    TemplateCheckBox.Enabled := False;
    TemplateCheckBox.Font.Color := clGray;
    SetSkipState;
    PortButton.Caption := 'Close Port';
    end
  else
    begin
    BuffSizeLabel.Font.Color := clWindowText;
    BuffSizeEdit.ReadOnly := False;
    BuffSizeEdit.Color := clWhite;
    BaudLabel.Font.Color := clWindowText;
    BaudEdit.ReadOnly := False;
    BaudEdit.Color := clWhite;
    PortLabel.Font.Color := clWindowText;
    PortEdit.ReadOnly := False;
    PortEdit.Color := clWhite;
    TemplateCheckBox.Enabled := True;
    TemplateCheckBox.Font.Color := clWindowText;
    SetSkipState;
    PortButton.Caption := 'Open Port';
    end;
end;

{------------------------------------------------------------------------------}

procedure TForm1.SetSkipState;
{Enable/Display Skip control based on template checkbox and debugging state}
begin
  if not Debugging and PatMatch then
    begin
    SkipLabel.Font.Color := clWindowText;
    SkipEdit.ReadOnly := False;
    SkipEdit.Color := clWhite;
    end
  else
    begin
    SkipLabel.Font.Color := clGray;
    SkipEdit.ReadOnly := True;
    SkipEdit.Color := clBtnFace;
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

procedure TForm1.ClearPatternList;
{Clear entire pattern list}
begin
  while PatList.Count > 0 do
    begin
    Dispose(PatList.Items[0]);
    PatList.Delete(0);
    end;
end;

{------------------------------------------------------------------------------}

function TForm1.AddPattern(PType: TPatType; Size: Cardinal; Content: String): PPattern;
{Add (append) a new pattern to pattern list and return that pattern.}
begin
  new(Result);
  Result.PType := PType;
  Result.Size := Size;
  Result.Content := Content;
  PatList.Add(Result);
end;

{------------------------------------------------------------------------------}

Initialization
  Ser := TPropellerSerial.Create;
  Debugging := False;
  PatList := TList.Create;

Finalization
  Ser.Destroy;

end.
