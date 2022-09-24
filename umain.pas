unit umain; 
{$define uselib}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, StdCtrls, ExtCtrls, Buttons, Grids,
  {$ifdef uselib}
    libSRIntf,
  {$else}
    usubentries,
  {$endif} ActnList;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    actEdit: TAction;
    actClose: TAction;
    actExit: TAction;
    actHelp: TAction;
    actAbout: TAction;
    actGotoHalf: TAction;
    actGoto2Fifth: TAction;
    actGoto3Fifth: TAction;
    actGoto4Fifth: TAction;
    actGoto1Fifth: TAction;
    actSave: TAction;
    actResync: TAction;
    actMerge: TAction;
    actOpen: TAction;
    Actions: TActionList;
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnBrowse: TButton;
    chbAutoLoad: TCheckBox;
    edtEntry: TEdit;
    edtExit: TEdit;
    edtFactor: TEdit;
    edtFilename: TEdit;
    GroupBox1: TGroupBox;
    gbxFactor: TGroupBox;
    gbxEntry: TGroupBox;
    gbxExit: TGroupBox;
    ImageList1: TImageList;
    lblFactor: TLabel;
    lblExit: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnu_Help_Help: TMenuItem;
    mnu_File_Resync: TMenuItem;
    mnu_File_Merge: TMenuItem;
    mnu_File_Edit: TMenuItem;
    mnu_File_Open: TMenuItem;
    mnu_File_Save: TMenuItem;
    MenuItem5: TMenuItem;
    mnu_File_Exit: TMenuItem;
    MenuItem7: TMenuItem;
    mnu_Help_About: TMenuItem;
    mnu_File_Close: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    btnExit: TSpeedButton;
    btnEdit: TSpeedButton;
    btnOpen: TSpeedButton;
    btnMerge: TSpeedButton;
    btnResync: TSpeedButton;
    btnSave: TSpeedButton;
    btnClose: TSpeedButton;
    btnHelp: TSpeedButton;
    btnAbout: TSpeedButton;
    btnLibOptions: TSpeedButton;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    Timer1: TTimer;
    procedure actAboutExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actGoto1FifthExecute(Sender: TObject);
    procedure actGoto2FifthExecute(Sender: TObject);
    procedure actGoto3FifthExecute(Sender: TObject);
    procedure actGoto4FifthExecute(Sender: TObject);
    procedure actGotoHalfExecute(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure actMergeExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actResyncExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnLibOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fGridRowSelected: integer;
    RunWithParams: boolean;
    {$ifdef uselib}
    fSubResync: ISubtitleResync; { dynamically linking }
    procedure Iterate_Data;
    {$else}
    fSE: TSubEntries;          { straight pascal object }
    {$endif}
  public
    {$ifdef uselib}
    procedure SubResyncEnumerate(aSubItem: PSubItem;var aCancel: boolean);
    {$endif}
    procedure CheckModified;
    procedure CheckCodepage;
    procedure Enable_Disable_Actions(Data_Loaded: boolean);
    procedure ClearGrid;
    procedure ShowGrid;
  end; 

var
  frmMain: TfrmMain;

implementation
{$R *.lfm}
uses bc_advstring, lfm_subentryedit, lfm_codepageedit; {LCLType, LazUTF8, LConvEncoding}
{ TfrmMain }
procedure TfrmMain.RadioButton1Change(Sender: TObject); {*}
begin
  if RadioButton1.Checked then begin
    StatusBar1.Panels[0].Text:= ' Push forward...';
    RadioButton2.Checked:= false;
  end;
end;

procedure TfrmMain.RadioButton2Change(Sender: TObject); {*}
begin
  if RadioButton2.Checked then begin
    StatusBar1.Panels[0].Text:= ' Pull backward...';
    RadioButton1.Checked:= false;
  end;
end;

procedure TfrmMain.StringGrid1DblClick(Sender: TObject); {*}
begin
  if actEdit.Enabled then actEditExecute(Sender);
end;

procedure TfrmMain.Timer1Timer(Sender: TObject); {*}
begin
  StringGrid1.SetFocus;
  Timer1.Enabled:= false;
end;
{$ifdef uselib}
procedure TfrmMain.Iterate_Data;
begin
  ClearGrid;
  StringGrid1.BeginUpdate;
  fSubResync.Enumerate;
  StringGrid1.EndUpdate(true);  //bm
end;
{$endif}

{$ifdef uselib}
procedure TfrmMain.SubResyncEnumerate(aSubItem: PSubItem;var aCancel: boolean);
begin
  StringGrid1.RowCount:= StringGrid1.RowCount+1;
  StringGrid1.Cells[0,StringGrid1.RowCount-1]:= bcStrPas(aSubItem^.Id);
  StringGrid1.Cells[1,StringGrid1.RowCount-1]:= bcStrPas(aSubItem^.StartAsString);
  StringGrid1.Cells[2,StringGrid1.RowCount-1]:= bcStrPas(aSubItem^.DoneAsString);
  StringGrid1.Cells[3,StringGrid1.RowCount-1]:= bcStrPas(aSubItem^.DurationAsString);
  StringGrid1.Cells[4,StringGrid1.RowCount-1]:= bcStrPas(aSubItem^.Text);
  aCancel:= false;
end;
{$endif}
procedure TfrmMain.CheckModified; {*}
begin
{$ifdef uselib}
  if fSubResync.Modified then StatusBar1.Panels[1].Text:= 'Modified'
  else StatusBar1.Panels[1].Text:= '';
{$else}
  if fSE.Modified then StatusBar1.Panels[1].Text:= 'Modified'
  else StatusBar1.Panels[1].Text:= '';
{$endif}
end;

procedure TfrmMain.CheckCodepage;
begin
  case fSubResync.Codepage of
    0: StatusBar1.Panels[2].Text:= 'UTF8';
    1: StatusBar1.Panels[2].Text:= 'ISO_8859_1';
    2: StatusBar1.Panels[2].Text:= 'ISO_8859_15';
    3: StatusBar1.Panels[2].Text:= '1252';
  end;
end;

procedure TfrmMain.Enable_Disable_Actions(Data_Loaded: boolean); {*}
begin
  case Data_Loaded of
    false: begin
             actEdit.Enabled:= false;
             actOpen.Enabled:= true;
             actMerge.Enabled:= true;
             actResync.Enabled:= false;
             actSave.Enabled:= false;
             actClose.Enabled:= false;
           end;
    true: begin
            actEdit.Enabled:= true;
            actOpen.Enabled:= false;
            actMerge.Enabled:= false;
            actResync.Enabled:= true;
            actSave.Enabled:= true;
            actClose.Enabled:= true;
          end;
  end;
end;

procedure TfrmMain.ClearGrid; {*}
var x,y: integer;
begin
  StringGrid1.BeginUpdate;
  for X:= 0 to StringGrid1.ColCount-1 do
    for Y:= 1 to StringGrid1.RowCount-1 do begin
      StringGrid1.Cells[X,Y]:= '';
      StringGrid1.Objects[X,Y]:= nil;
    end;
  StringGrid1.Clear;
  StringGrid1.RowCount:= 1;
  StringGrid1.Cells[0,0]:= 'Id';
  StringGrid1.Cells[1,0]:= 'Start';
  StringGrid1.Cells[2,0]:= 'Done';
  StringGrid1.Cells[3,0]:= 'Duration';
  StringGrid1.Cells[4,0]:= 'Text';
  StringGrid1.EndUpdate;
end;

procedure TfrmMain.ShowGrid; {*}
{$ifndef uselib}
var
  I,tc: integer;
  S: string;
{$endif}
begin
{$ifdef uselib}
  Iterate_Data;
{$else}
  ClearGrid;
  for I := 0 to  fSE.Count-1 do begin
    StringGrid1.RowCount:= I+2;
    StringGrid1.Cells[0,I+1]:= inttostr(fSE[I].Id);
    StringGrid1.Cells[1,I+1]:= fSE[I].StartAsString;
    StringGrid1.Cells[2,I+1]:= fSE[I].DoneAsString;
    StringGrid1.Cells[3,I+1]:= fSE[I].DurationAsString;
    S:= '';
    if fSE[I].Text.Count > 0 then
      for tc:= 0 to fSE[I].Text.Count-1 do S:= S + fSE[I].Text[tc] + '|';
    StringGrid1.Cells[4,I+1]:= copy(S,1,length(S)-1);
  end;
{$endif}
  if not RunWithParams then StringGrid1.SetFocus;
  if fGridRowSelected > 0 then StringGrid1.Row:= fGridRowSelected
  else StringGrid1.Row:= 1;
end;

procedure TfrmMain.btnBrowseClick(Sender: TObject); {*}
begin
  if OpenDialog1.Execute then edtFilename.Text:= OpenDialog1.FileName;
  if chbAutoLoad.Checked then begin
    actCloseExecute(Self);
    if actOpen.Enabled then actOpenExecute(Sender);
  end;
end;

procedure TfrmMain.btnLibOptionsClick(Sender: TObject);
begin
  lfm_codepageedit.EditCodepage(fSubResync);
  CheckCodepage;
end;

procedure TfrmMain.actEditExecute(Sender: TObject);
var
  Sie: PSubItemEdit;
  SI: TSubItem;
begin
  Sie:= GetMem(Sizeof(TSubItemEdit));
  try
    FillChar(Sie^,Sizeof(TSubItemEdit),0);
    fGridRowSelected:= StringGrid1.Row;
    if fGridRowSelected <= 0 then fGridRowSelected:= 1;
    Sie^.Id:= bcCloneString(StringGrid1.Cells[0,fGridRowSelected]);
    Sie^.StartAsString:= bcCloneString(StringGrid1.Cells[1,fGridRowSelected]);
    Sie^.DoneAsString:= bcCloneString(StringGrid1.Cells[2,fGridRowSelected]);
    Sie^.DurationAsString:= bcCloneString(StringGrid1.Cells[3,fGridRowSelected]);
    Sie^.Text:= bcCloneString(StringGrid1.Cells[4,fGridRowSelected]);
    if TfrmSubItemEdit.EditSubItem(Sie) then begin
    {$ifdef uselib}
      SI.Id:= pchar(Sie^.Id);
      SI.StartAsString:= pchar(Sie^.StartAsString);
      SI.DoneAsString:= pchar(Sie^.DoneAsString);
      SI.DurationAsString:= pchar(Sie^.DurationAsString);
      SI.Text:= pchar(Sie^.Text);
      if fSubResync.Edit_Entry(@SI) then begin
        ShowGrid;
      end;
    {$endif}
    end;
  finally Freemem(Sie,Sizeof(TSubItemEdit)); end;
  CheckModified;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  actCloseExecute(Sender); // ask for save, free and close
  Close;
end;

procedure TfrmMain.actGoto1FifthExecute(Sender: TObject);
begin
  StringGrid1.Row:= ((StringGrid1.RowCount div 5) * 1) + 19;
  StringGrid1.Row:= ((StringGrid1.RowCount div 5) * 1);
end;

procedure TfrmMain.actGoto2FifthExecute(Sender: TObject);
begin
  StringGrid1.Row:= ((StringGrid1.RowCount div 5) * 2) + 19;
  StringGrid1.Row:= ((StringGrid1.RowCount div 5) * 2);
end;

procedure TfrmMain.actGoto3FifthExecute(Sender: TObject);
begin
  StringGrid1.Row:= ((StringGrid1.RowCount div 5) * 3) + 19;
  StringGrid1.Row:= ((StringGrid1.RowCount div 5) * 3);
end;

procedure TfrmMain.actGoto4FifthExecute(Sender: TObject);
begin
  StringGrid1.Row:= ((StringGrid1.RowCount div 5) * 4) + 19;
  StringGrid1.Row:= ((StringGrid1.RowCount div 5) * 4);
end;

procedure TfrmMain.actGotoHalfExecute(Sender: TObject);
begin
  StringGrid1.Row:= (StringGrid1.RowCount div 2) + 19;
  StringGrid1.Row:= (StringGrid1.RowCount div 2);
end;

procedure TfrmMain.actHelpExecute(Sender: TObject);
begin  ShowMessage(bcShortenFilename(edtFilename.Text)); // ææ
  Showmessage('Help for Subtitle Resync'+#13+
              'Coming soon...'+#13+
              'Please be patient - regards Benny');
  { TODO 1 -obc -cImplement : Create help system }
end;

procedure TfrmMain.actCloseExecute(Sender: TObject);
begin
{$ifdef uselib}
  if fSubResync.Modified then begin
    if messagedlg('Subtitle Resync - Save...',              //caption
                  'Subtitles have changed, Save now?',      //message
                  mtConfirmation,                           //dialogtype
                  mbYesNo,                                  //buttons
                  -1) = mrYes then actSaveExecute(Sender);  //save
    fSubResync.Clear;
  end;
{$else}
  if assigned(fSe) then begin
    if fSe.Modified then
      if messagedlg('Subtitle Resync - Save...',             //caption
                    'Subtitles have changed, Save now?',     //message
                    mtConfirmation,                          //dialogtype
                    mbYesNo,                                 //buttons
                    -1) = mrYes then actSaveExecute(Sender); //save
    FreeAndNil(fSE);
  end;
{$endif}
  ClearGrid;
  Enable_Disable_Actions(false);
  CheckModified;
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  Showmessage('About Subtitle Resync...'+#13+
              'Vers.: 9.15.11.2009'+#13+
{$ifdef uselib}fSubResync.Version+#13+{$endif}
              'copyright (c) 2008 - 2020 cdbc.dk');
end;

procedure TfrmMain.actMergeExecute(Sender: TObject);
var chbSave: boolean;
begin
{$ifdef uselib}
  actCloseExecute(Sender);
  chbSave:= chbAutoLoad.Checked;
  chbAutoLoad.Checked:= false;
  btnBrowseClick(Sender);
  chbAutoLoad.Checked:= chbSave;
  if OpenDialog1.Execute then begin
    if not fSubResync.Merge(edtFilename.Text,OpenDialog1.FileName,SRT) then
      raise Exception.Create('ERROR: Cannot merge subtitle files '+edtFilename.Text+' & '+OpenDialog1.FileName);
  end;
{$else}
  actCloseExecute(Sender);
  chbSave:= chbAutoLoad.Checked;
  chbAutoLoad.Checked:= false;
  btnBrowseClick(Sender);
  chbAutoLoad.Checked:= chbSave;
  if OpenDialog1.Execute then fSE.Merge(edtFilename.Text,OpenDialog1.FileName,sfSRT);
{$endif}
  Enable_Disable_Actions(true);
  ShowGrid;
  CheckModified;
  StringGrid1.Row:= 1; { we've loaded two new files }
  StatusBar1.Panels[0].Text:= ' Subtitles merged, Subtitle count: '+inttostr(StringGrid1.RowCount-1);
end;

procedure TfrmMain.actOpenExecute(Sender: TObject); {*}
var
  S: string;
  Cp: word;
begin
  S:= '';
  S:= edtFilename.Text;
{$ifdef uselib}
  if not fSubResync.LoadFromFile(S,SRT) then raise Exception.Create('ERROR: Cannot open subtitle file '+S);
{$else}
//  fSE:= TSubEntries.Create;
  if not fSE.LoadFromFile(S,sfSRT) then raise Exception.Create('ERROR: Cannot open subtitle file '+S);
{$endif}
  Enable_Disable_Actions(true);
  ShowGrid;
  CheckModified;
  { check against codepage, to assertain if we've converted text }
  Cp:= fSubResync.Codepage;
  if Cp <> CPUTF8 then begin
    StatusBar1.Panels[1].Text:= 'Modified'; { just to trick the user to save :-) }
    ShowMessage('While loading the file, text has been converted to UTF8'+#10+
                'Please save data once to write the new encoding to file.');
  end;
  StringGrid1.Row:= 1; { we've loaded a new file }
  StatusBar1.Panels[0].Text:= ' Subtitles loaded, Subtitle count: '+inttostr(StringGrid1.RowCount-1);
  case Cp of
    0: StatusBar1.Panels[2].Text:= 'UTF8';
    1: StatusBar1.Panels[2].Text:= 'ISO_8859_1';
    2: StatusBar1.Panels[2].Text:= 'ISO_8859_15';
    3: StatusBar1.Panels[2].Text:= '1252';
  end;
end;

procedure TfrmMain.actResyncExecute(Sender: TObject); {*}
var Ms,IdEntry,IdExit: integer;
begin
  Ms:= StrToInt(edtFactor.Text); // miliseconds
  IdEntry:= StrToInt(edtEntry.Text); // id entrypoint ~ default is 0
  IdExit:= StrToInt(edtExit.Text); // id exitpoint ~ default is -1
  if IdEntry > 0 then dec(IdEntry); // subentries are 0-based, grid is 1-based
{$ifdef uselib}
  if RadioButton1.Checked then begin // forwards  -1 => whole file
    if not fSubResync.Resync(Ms,IdEntry,IdExit) then
      raise Exception.Create('ERROR: Cannot resync subtitle file '+edtFilename.Text);
  end else if RadioButton2.Checked then begin // backwards -1 => whole file
    if not fSubResync.Resync(-Ms,IdEntry,IdExit) then
      raise Exception.Create('ERROR: Cannot resync subtitle file '+edtFilename.Text);
  end;
  ShowGrid;
  StatusBar1.Panels[0].Text:= ' Subtitles resync''ed, Subtitle count: '+inttostr(StringGrid1.RowCount-1);
{$else}
  if RadioButton1.Checked then fSE.Resync(Ms,IdEntry,IdExit) // forwards  -1 => whole file
  else if RadioButton2.Checked then fSE.Resync(-Ms,IdEntry,IdExit); // backwards -1 => whole file
  ShowGrid;
  StatusBar1.Panels[0].Text:= ' Subtitles resync''ed, Subtitle count: '+inttostr(fSE.Count);  ///
{$endif}
  CheckModified;
end;

procedure TfrmMain.actSaveExecute(Sender: TObject); {*}
var S,S2: string;
begin
  S:= ChangeFileExt(edtFilename.Text,'.old');
  RenameFile(edtFilename.Text,S);
{$ifdef uselib}
  fSubResync.SaveToFile(edtFilename.Text,SRT);
{$else}
  fSE.SaveToFile(edtFilename.Text,sfSRT);
  fSe.Modified:= false;
{$endif}
  if Length(ExtractFileName(S)) > 35 then begin
    S2:= bcShortenFilename(ExtractFileName(bcSwapChar('.','_',copy(S,1,length(S)-4))+'.old'));
    S:= bcShortenFilename(ExtractFileName(bcSwapChar('.','_',copy(edtFilename.Text,1,length(edtFilename.Text)-4))+'.srt'));
  end else begin
    S2:= ExtractFileName(bcSwapChar('.','_',copy(S,1,length(S)-4))+'.old');
    S:= ExtractFileName(bcSwapChar('.','_',copy(edtFilename.Text,1,length(edtFilename.Text)-4))+'.srt');
  end;
  StatusBar1.Panels[0].Text:= ' Subtitles saved to: '+S+', original file renamed to: '+S2;
  CheckModified;
  CheckCodepage;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
{$ifdef uselib}
  fSubResync:= libSRIntf.CreateSubtitleResync('/home/bc/src/lib64/');
  if assigned(fSubResync) then begin
    StatusBar1.Panels[0].Text:= 'Library loaded: '+fSubResync.Version;
    CheckCodepage;
    fSubResync.OnEnumerate:= @SubResyncEnumerate;
  end else StatusBar1.Panels[0].Text:= 'ERROR: Cannot load Library /home/sh/src/lib64/libsubresync.so!';
{$else}
  fSE:= TSubEntries.Create;
  StatusBar1.Panels[0].Text:= 'Engine loaded: '+usubentries.Version;
{$endif}
  StatusBar1.Panels[0].Width:= StatusBar1.Width - 62;
end;

procedure TfrmMain.FormDestroy(Sender: TObject); {*}
begin
{$ifdef uselib}
  fSubResync:= nil;
{$else}
  FreeAndNil(fSE);
{$endif}
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); {*}
begin
  case Key of
    $71,$0D: if actEdit.Enabled then actEditExecute(Sender); { F2 = VK_F2 ~ 113, Enter = VK_RETURN ~ 13 }
    $1B: actExitExecute(Sender); { Esc = VK_ESCAPE ~ 27 }
    $51: if ssCtrl in Shift then showmessage('Ctrl + Q'); { VK_Q = $51 ~ line 475 in lcltype }
  end;
//  {$ifdef LCLqt} StatusBar1.Panels[0].Text:= 'Running QT'; {$endif} //bm
end;          // {$ifdef LCLqt}  {$endif}

procedure TfrmMain.FormShow(Sender: TObject); {*}
begin
  {$ifdef LCLqt}
    btnBrowse.Font.Color:= clLime;//clWhite;
    edtFactor.Font.Color:= clBlue;
    edtEntry.Font.Color:= clBlue;
    edtExit.Font.Color:= clBlue;
    lblExit.Font.Color:= clYellow;
    lblFactor.Font.Color:= clYellow;
    StatusBar1.Font.Color:= clSkyBlue;
    StringGrid1.TitleFont.Color:= clWhite;
  {$endif}
  if Paramcount >= 1 then begin
    edtFilename.Text:= ParamStr(1);
    RunWithParams:= true; //
    Timer1.Enabled:= true; //ææ
    actOpenExecute(Sender);
    Enable_Disable_Actions(true);
  end else Enable_Disable_Actions(false);
end;
(*
tips:
      UTF8Encode

?!?!?:
TPointerHelper = type helper for Pointer
  constructor createBuffer(AValue: nativeuint);
  constructor createLongInt(AValue: longint);
  constructor createShortString(AValue: shortstring);
  procedure InitPrev(AValue: Pointer); inline;
  function GetPrev: Pointer; inline;
  procedure GetLongint(var AValue: longint);
  procedure GetShortString(var AValue: shortstring);
  procedure TestWrite;
  procedure Free; inline;
end;

--------

constructor TPointerHelper.createBuffer(AValue: nativeuint);
begin
  GetMem(Self, SizeOf(Pointer) * 2 + Avalue);
end;

constructor TPointerHelper.createLongInt(AValue: longint);
begin
  GetMem(Self, SizeOf(Pointer) * 2 + SizeOf(longint));
  Move(AValue, TPointerArray(Self)[2], SizeOf(longint));
end;

constructor TPointerHelper.createShortString(AValue: shortstring);
begin
  GetMem(Self, SizeOf(Pointer) * 2 + SizeOf(shortstring));
  Move(AValue, TPointerArray(Self)[2], SizeOf(shortstring));
end;

procedure TPointerHelper.InitPrev(AValue: Pointer);
begin
  TPointerArray(Self)[0] := AValue;
end;

---------
typecasts...
var
  a: byte; b: word;
  c: AnsiChar; d: WideChar;
...
  c := AnsiChar(a);  Byte(c) := a;
  d := WideChar(b); word(d) := b;

-----------


*)

end.

