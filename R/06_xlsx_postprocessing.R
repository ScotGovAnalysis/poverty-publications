# VBA post processing ----------------------------------------------------------

# Marks up headings, notes, links and sources properly
# Produces an ods file

# This script calls 'Run Excel Macro.vbs' which opens VBA within the
# 'VBA - Convert XLSX to ODS.xlsm' file, which creates .ods files from all .xlsx
# files in the /output folder.

system_command <- paste("WScript", '"./Run Excel Macro.vbs"', sep = " ")
system(command = system_command, wait = TRUE)

