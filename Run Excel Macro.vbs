Dim Excel
Dim ExcelDoc


'Get the Directory of this script

Set objShell = CreateObject("Wscript.Shell")
strPath = Wscript.ScriptFullName
Set objFSO = CreateObject("Scripting.FileSystemObject")
Set objFile = objFSO.GetFile(strPath)

'Get the filepath of this script
strFolder = objFSO.GetParentFolderName(objFile) 

Set Excel = CreateObject("Excel.Application")

'Make Word visible
Excel.Visible = FALSE

'Open the Document
Set ExcelDoc = Excel.Workbooks.Open(strFolder & "\vba_for_postprocessing.xlsm")

'Run the macro
Excel.Run "WorksheetLoop"

'Release the object variables
Set ExcelDoc = Nothing
Set Excel = Nothing

'Open the Process Output Folder when complete (commented out as not needed)
'strPath = "explorer.exe /e," & strFolder & "\website\"
'objShell.Run strPath