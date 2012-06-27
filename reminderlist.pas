unit ReminderList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Reminders;

type

  { TfrmReminderList }

  TfrmReminderList = class(TForm)
    lbxReminders: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    FCanEdit: boolean;
    procedure SetCanEdit(const AValue: boolean);
    { private declarations }
  public
    { public declarations }
    FReminders: TfrmReminders;

    property CanEdit: boolean read FCanEdit write SetCanEdit;
  end;

var
  frmReminderList: TfrmReminderList;

implementation

{$R *.lfm}

{ TfrmReminderList }

procedure TfrmReminderList.FormKeyPress(Sender: TObject; var Key: char);
var
  Index: integer;
begin
  if Key = #27 then
  begin
    Self.Close
  end
  else if FCanEdit then
  begin
    if (Key = 'd') or (Key = 'D') then
    begin
      Index := lbxReminders.ItemIndex;

      FReminders.DeleteReminder(lbxReminders.ItemIndex);
      lbxReminders.Clear;
      FReminders.PopulateList(lbxReminders.Items);

      if Index < lbxReminders.Count - 1 then
        lbxReminders.ItemIndex := Index
      else if lbxReminders.Count > 0 then
        lbxReminders.ItemIndex := lbxReminders.Count - 1;

      Key := #0;
    end
    else if (Key = #13) or (Key = 'e') or (Key = 'E') then
    begin
      Index := lbxReminders.ItemIndex;

      FReminders.DisplayReminder(Index);

      FReminders.Editing := True;

      if FReminders.ShowModal = mrOk then
      begin
        FReminders.UpdateWithCurrentReminder(Index);

        lbxReminders.Clear;
        FReminders.PopulateList(lbxReminders.Items);

        if Index < lbxReminders.Count - 1 then
          lbxReminders.ItemIndex := Index
        else if lbxReminders.Count > 0 then
          lbxReminders.ItemIndex := lbxReminders.Count - 1;
      end;

      FReminders.Editing := False;

      Key := #0;
    end
    else if (Key = 'a') or (Key = 'A') then
    begin
      frmReminders.ShowModal;

      lbxReminders.Clear;
      FReminders.SortReminders;
      FReminders.PopulateList(lbxReminders.Items);

      Key := #0;
    end;
  end
  else if (Key = 'd') or (Key = 'D') or (Key = 'e') or (Key = 'E')
   or (Key = 'e') or (Key = 'A') or (Key = #13) then
  begin
    ShowMessage('You can not edit this list when reminders are fetched from ' +
     'the server.' + LineEnding + 'Change the setting "Fetch reminders from server" by using the Settings dialog.');
  end;
end;

procedure TfrmReminderList.FormCreate(Sender: TObject);
begin
  FCanEdit := False;
end;

procedure TfrmReminderList.FormShow(Sender: TObject);
begin
  lbxReminders.Clear;
  FReminders.SortReminders;
  FReminders.PopulateList(lbxReminders.Items);
  if lbxReminders.Count > 0 then lbxReminders.ItemIndex := 0;
end;

procedure TfrmReminderList.SetCanEdit(const AValue: boolean);
begin
  if FCanEdit=AValue then Exit;
  FCanEdit := AValue;
end;

end.

