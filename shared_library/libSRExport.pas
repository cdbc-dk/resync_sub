(*  18.10.2009 bc changed typecast in 'Load', 'Merge' and 'Save' to ansistring
                  parameters are pchar...
    28.10.2009 bc added support for editing entries by 'Id'.
    20.02.2011 bc added support for utf-8 bom (byte-order-marker) in
                  'OpenFromStream'.
    20.02.2011 bc added ability to recover from broken timelines,
                  rudimentary for now.
    09.09.2022 bc code review

*)
unit libSRExport;
interface

{$define fpc}

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}
uses
  SysUtils,
  Classes,
  usubentries;

const
  SRT = 0;    { ~ TSubFormat }
  SUB = 1;
  None = 2;

type
  { callback data structure }
  PSubItem = ^TSubItem;
  TSubItem = record
    Id: pchar;
    StartAsString: pchar;
    DoneAsString: pchar;
    DurationAsString: pchar;
    Text: pchar;
  end;
  { function prototype }
  TCallBack_SR = function(SubItem: PSubItem;OwnerData,aHandle:pointer): boolean; cdecl; { if false then break loop }

{ exported functions }
function sr_Init(var aHandle: pointer): boolean; cdecl;
function sr_GetCodepage(aHandle: pointer): word; cdecl;
function sr_GetCodepageOptions(aHandle: pointer): pchar; cdecl;
function sr_SetCodepage(aHandle: pointer;aCodepage: word): boolean; cdecl;
function sr_Version: pchar; cdecl;
function sr_LoadFromFile(aHandle: pointer;aFilename: pchar;aFormat: integer): boolean; cdecl;
function sr_MergeFiles(aHandle: pointer;aFilename1,aFilename2: pchar;aFormat: integer): boolean; cdecl;
function sr_Resync(aHandle: pointer;aFactor,IdEntrypoint,IdExitpoint: integer): boolean; cdecl;
function sr_Edit_Entry(aHandle:pointer; anEntry: PSubItem): boolean; cdecl;
function sr_Enumerate(aHandle,OwnerData:pointer;CallBack: TCallBack_SR): boolean; cdecl;
function sr_SaveToFile(aHandle: pointer;aFilename: pchar;aFormat: integer): boolean; cdecl;
function sr_Clear(aHandle: pointer): boolean; cdecl;
function sr_Modified(aHandle: pointer): boolean; cdecl; { property }
function sr_Count(aHandle: pointer): integer; cdecl; { property }
function sr_Exit(var aHandle: pointer): boolean; cdecl;

implementation

function sr_Init(var aHandle: pointer): boolean; cdecl;
begin
  try
    usubentries.TSubEntries(aHandle):= usubentries.TSubEntries.Create;
    Result:= true; 
  except
    Result:= false;
    aHandle:= nil;
  end;
end;

function sr_GetCodepage(aHandle: pointer): word; cdecl;
begin
  Result:= usubentries.TSubEntries(aHandle).Codepage; { (0)..3 }
end;

function sr_GetCodepageOptions(aHandle: pointer): pchar; cdecl;
begin
  Result:= pchar(usubentries.TSubEntries(aHandle).GetCodepageOptions);
end;

function sr_SetCodepage(aHandle: pointer; aCodepage: word): boolean; cdecl;
begin
  usubentries.TSubEntries(aHandle).Codepage:= aCodepage;
  Result:= true;
end;

function sr_Version: pchar; cdecl;
begin
  Result:= pchar(usubentries.Version);
end;

function sr_LoadFromFile(aHandle: pointer;aFilename: pchar;aFormat: integer): boolean; cdecl;
begin
  try
    Result:= usubentries.TSubEntries(aHandle).LoadFromFile(string(aFilename),TSubFormat(aFormat));
  except
    Result:= false;
  end;
end;

function sr_MergeFiles(aHandle: pointer;aFilename1,aFilename2: pchar;aFormat: integer): boolean; cdecl;
begin
  try
    usubentries.TSubEntries(aHandle).Merge(string(aFilename1),string(aFilename2),TSubFormat(aFormat));
    Result:= true;
  except
    Result:= false;
  end;
end;

function sr_Resync(aHandle: pointer;aFactor,IdEntrypoint,IdExitpoint: integer): boolean; cdecl;
begin
  try
    usubentries.TSubEntries(aHandle).Resync(aFactor,IdEntrypoint,IdExitpoint);
    Result:= true;
  except
    Result:= false;
  end;
end;

function sr_Edit_Entry(aHandle:pointer; anEntry: PSubItem): boolean; cdecl;
var
  lId: integer;
  Entry: TSubEntry;
begin
  if assigned(anEntry) then begin
    lId:= strtoint(string(anEntry^.Id));
    Entry:= usubentries.TSubEntries(aHandle).GetEntryFromId(lId); { get a reference to it }
    if assigned(Entry) then begin
      Entry.StartAsString:= string(anEntry^.StartAsString);
      Entry.DoneAsString:= '                 ' + string(anEntry^.DoneAsString); { timeline: 00:00:11,647 --> 00:01:20,647 }
      Entry.SetTextAsString(string(anEntry^.Text));
      usubentries.TSubEntries(aHandle).Modified:= true;
      Result:= true;
    end else Result:= false;
  end else Result:= false;
end;

function sr_Enumerate(aHandle,OwnerData: pointer;CallBack: TCallBack_SR): boolean; cdecl;
var
  I,tc: integer;
  S: string;
  SI: PSubItem;
begin
  Result:= true; {$ifdef fpc} SI:= nil; {$endif}
  getmem(SI,sizeof(TSubItem));
  fillchar(SI^,sizeof(TSubItem),0);
  try
    for I := 0 to  usubentries.TSubEntries(aHandle).Count-1 do begin
      SI^.Id:= pchar(inttostr(usubentries.TSubEntries(aHandle)[I].Id));
      SI^.StartAsString:= pchar(usubentries.TSubEntries(aHandle)[I].StartAsString);
      SI^.DoneAsString:= pchar(usubentries.TSubEntries(aHandle)[I].DoneAsString);
      SI^.DurationAsString:= pchar(usubentries.TSubEntries(aHandle)[I].DurationAsString);
      S:= '';
      if usubentries.TSubEntries(aHandle)[I].Text.Count > 0 then
        for tc:= 0 to usubentries.TSubEntries(aHandle)[I].Text.Count-1 do S:= S + usubentries.TSubEntries(aHandle)[I].Text[tc] + '|';
      S:= copy(S,1,length(S)-1); { skip the last pipe | }
      SI^.Text:= pchar(S);
      if assigned(CallBack) then if not CallBack(SI,OwnerData,aHandle) then begin
        Result:= false;
        break;
      end;
      fillchar(SI^,sizeof(TSubItem),0);
    end;
  finally
    freemem(SI);
  end;
end;

function sr_SaveToFile(aHandle: pointer;aFilename: pchar;aFormat: integer): boolean; cdecl;
begin
  try
    usubentries.TSubEntries(aHandle).SaveToFile(string(aFilename),TSubFormat(aFormat));
    usubentries.TSubEntries(aHandle).Modified:= false;
    Result:= true;
  except
    Result:= false;
  end;
end;

function sr_Clear(aHandle: pointer): boolean; cdecl;
begin
  try
    usubentries.TSubEntries(aHandle).ClearEntries;
    Result:= true;
  except
    Result:= false;
  end;
end;

function sr_Modified(aHandle: pointer): boolean; cdecl; { property }
begin
  Result:= usubentries.TSubEntries(aHandle).Modified;
end;

function sr_Count(aHandle: pointer): integer; cdecl; { property }
begin
  Result:= usubentries.TSubEntries(aHandle).Count;
end;

function sr_Exit(var aHandle: pointer): boolean; cdecl;
begin
  try
    usubentries.TSubEntries(aHandle).Free;
    aHandle:= nil;
    Result:= true;
  except
    Result:= false;
  end;
end;

end.
