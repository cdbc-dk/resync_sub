unit lfm_codepageedit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Buttons, StdCtrls, libSRIntf;

type

  { TfrmCodepage }

  TfrmCodepage = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    cbxCodepages: TComboBox;
    gbxCodepage: TGroupBox;
    lblActive: TLabel;
    pnlButtons: TPanel;
    stbInfo: TStatusBar;
    procedure btnOkClick(Sender: TObject);
  private
    fIntf: ISubtitleResync;
    procedure InitComboBox(aStr: string);
  public

  end;

{ Lets the user choose another codepage for reading files }
function EditCodepage(anInterface: ISubtitleResync): boolean;

var
  frmCodepage: TfrmCodepage;

implementation
uses bc_advstring;

function EditCodepage(anInterface: ISubtitleResync): boolean;
begin
  with TfrmCodepage.Create(nil) do try
    fIntf:= anInterface;
    InitComboBox(fIntf.Get_CodepageOptions);
    Result:= (ShowModal = mrOK);
  finally
    Free;
  end;
end;

{$R *.lfm}

{ TfrmCodepage }

procedure TfrmCodepage.btnOkClick(Sender: TObject);
begin
  fIntf.Codepage:= cbxCodepages.ItemIndex;
end;

procedure TfrmCodepage.InitComboBox(aStr: string);
begin
  { populate with options }
  cbxCodepages.Items.Add(copy(bcGetFieldToken(1,bcGetFieldToken(1,aStr,'|'),' '),3,4));
  cbxCodepages.Items.Add(copy(bcGetFieldToken(1,bcGetFieldToken(2,aStr,'|'),' '),3,10));
  cbxCodepages.Items.Add(copy(bcGetFieldToken(1,bcGetFieldToken(3,aStr,'|'),' '),3,11));
  cbxCodepages.Items.Add(copy(bcGetFieldToken(1,bcGetFieldToken(4,aStr,'|'),' '),3,4));
  { set the active codepage }
  cbxCodepages.ItemIndex:= fIntf.Codepage;
end;

initialization


end.

