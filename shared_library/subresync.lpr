library subresync;
{$mode objfpc}{$H+}
uses
  Classes,libSRExport; { exposes the "flat'ened" object }

exports
  sr_Init,
  sr_GetCodepage,
  sr_GetCodepageOptions,
  sr_SetCodepage,
  sr_Version,
  sr_LoadFromFile,
  sr_MergeFiles,
  sr_Resync,
  sr_Edit_Entry,
  sr_Enumerate,
  sr_SaveToFile,
  sr_Clear,
  sr_Modified,
  sr_Count,
  sr_Exit;

begin
end.

