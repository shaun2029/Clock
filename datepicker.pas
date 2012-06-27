unit DatePicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Calendar;

type

  { TfrmDatePicker }

  TfrmDatePicker = class(TForm)
    Calendar: TCalendar;
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmDatePicker: TfrmDatePicker;

implementation

{$R *.lfm}

{ TfrmDatePicker }

procedure TfrmDatePicker.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    Self.ModalResult := mrCancel;
  end
  else if Key = #13 then
  begin
    Self.ModalResult := mrOk;
  end;
end;

end.

