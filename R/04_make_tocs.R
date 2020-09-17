
# Create content sheet

# to do: change hyperlink text to table titles
# to do: turn into function

filename <- "output/Poverty three-year average.xlsx"

options("openxlsx.borderStyle" = "thin")
options("openxlsx.borderColour" = "black")

titleStyle <- createStyle(fontName="Segoe UI Semibold", fontSize = 14)
tocStyle <- createStyle(fontName="Segoe UI", fontSize = 12, fontColour = "blue", textDecoration = "underline")

wb <- loadWorkbook(filename)
sheets <- names(wb)

sheets <- sheets[!sheets == "Contents"]

sheetlinks <- sapply(seq_along(sheets), function(i) makeHyperlinkString(sheets[[i]], text = sheets[[i]]))

if ("Contents" %in% getSheetNames(filename)){removeWorksheet(wb, "Contents")}

addWorksheet(wb, "Contents", gridLines = FALSE)

# add title
writeData(wb, "Contents", "Tables", startRow = 2, startCol = 2)
addStyle(wb, "Contents", rows = 2, cols = 2, style = titleStyle)

# add list of sheets
writeFormula(wb, "Contents", startRow = 3, startCol = 2, x = sheetlinks)
addStyle(wb, "Contents", rows = 3:(length(sheets) + 3), cols = 2, style = tocStyle)

# Move contents sheet to first position
order <- worksheetOrder(wb)
worksheetOrder(wb) <- c(order[length(order)], order[1:(length(order)-1)])
  
saveWorkbook(wb, filename, overwrite = TRUE)




