unit lfm_subentryedit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons, LConvEncoding;

type
  PSubItemEdit = ^TSubItemEdit;
  TSubItemEdit = record
    Id: string;
    StartAsString: string;
    DoneAsString: string;
    DurationAsString: string;
    Text: string;
  end;

  { TfrmSubItemEdit }
  TfrmSubItemEdit = class(TForm)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    edtText: TEdit;
    edtDone: TEdit;
    edtStart: TEdit;
    gbxStart: TGroupBox;
    gbxDone: TGroupBox;
    gbxText: TGroupBox;
    imgButtons: TImageList;
    stbInfo: TStatusBar;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    fItem: PSubItemEdit;
    fMaxTextLengtht: integer;
  public
    class function EditSubItem(anItem: PSubItemEdit): boolean;
  end;

var
  frmSubItemEdit: TfrmSubItemEdit;

implementation
{$R *.lfm}

{ TfrmSubItemEdit }
procedure TfrmSubItemEdit.btnCancelClick(Sender: TObject);
begin
  { for now: do nothing }
end;

procedure TfrmSubItemEdit.btnOkClick(Sender: TObject);
begin
  fItem^.StartAsString:= edtStart.Text;
  fItem^.DoneAsString:= edtDone.Text;
  fItem^.Text:= edtText.Text;
end;

class function TfrmSubItemEdit.EditSubItem(anItem: PSubItemEdit): boolean;
begin
  with TfrmSubItemEdit.Create(nil) do try
    fItem:= anItem;                         { ahhh... the beauty of pointers }
    Caption:= Caption + fItem^.Id;
    edtStart.Text:= fItem^.StartAsString;
    edtDone.Text:= fItem^.DoneAsString;
    edtText.Text:= fItem^.Text;
    fMaxTextLengtht:= Length(edtText.Text);
    Result:= (Showmodal = mrOK);
  finally Free; end;
end;

initialization

end.

