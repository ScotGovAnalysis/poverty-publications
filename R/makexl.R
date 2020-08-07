library(tidyverse)
library(openxlsx)

options("openxlsx.borderStyle" = "thin")
options("openxlsx.borderColour" = "black")

titleStyle <- createStyle(fontName="Segoe UI Semibold", fontSize = 14)
subtitleStyle <- createStyle(fontName="Segoe UI", fontSize = 12)
headerStyle <- createStyle(fontName="Segoe UI Semibold", fontSize = 10, halign = "right", border = "bottom")
uberheaderStyle <- createStyle(fontName="Segoe UI Semibold", fontSize = 10, halign = "center", 
                               border = "TopBottomLeftRight", borderColour = "#D3D3D3", borderStyle = "thin")

bodyStyle <- createStyle(halign = "right")
endrowStyle <- createStyle(border = "bottom", halign = "right")
sourceStyle <- createStyle(fontName="Segoe UI", fontSize = 10)
footnoteHeaderStyle <- createStyle(fontName="Segoe UI Semibold", fontSize = 11, textDecoration = "BOLD")
footnoteStyle <- createStyle(fontName="Segoe UI", fontSize = 11)

df <- relpovbhc

filename <- "Poverty single year.xlsx"
sheetname <- "Relative BHC"
title <- "Relative poverty before housing costs"
subtitle <- "Number and proportion of people with household incomes below 60% of the UK median, Scotland"
headers <- c("Year", "People", "Children", "Working-age adults", "Pensioners", 
             "People", "Children", "Working-age adults", "Pensioners")
uberheaders <- c(" " = 1, "Number" = 4, "Proportion" = 4)

source <- "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset"
footnotes <- c("footnote1", "another caveat", "dfin")


endcol <- length(df) + 1
endrow <- dim(df)[1] + 6
headers <- ifelse(is.na(headers), NULL, as.data.frame(t(headers)))

uh <- data.frame("headers" = names(uberheaders), "cells" = uberheaders) %>%
  mutate(cumcells = cumsum(cells))

uhlabel1 <- as.character(rep(uh$headers[1], uh$cells[1]))
uhlabel2 <- as.character(rep(uh$headers[2], uh$cells[2]))
uhlabel3 <- as.character(rep(uh$headers[3], uh$cells[3]))

uhlabels <- c(uhlabel1, uhlabel2, uhlabel3)
uhlabels <- as.data.frame(t(uhlabels))


wb <- createWorkbook()
addWorksheet(wb, sheetname, gridLines = FALSE)

# Title row
writeData(wb, 1, title, startRow = 2, startCol = 2)
addStyle(wb, 1, rows = 2, cols = 2, style = titleStyle)

# Subtitle row
writeData(wb, 1, subtitle, startRow = 3, startCol = 2)
addStyle(wb, 1, rows = 3, cols = 2, style = subtitleStyle)

# Uber header (above headers)
mergeCells(wb, 1, cols = 2:(1 + uh$cumcells[1]), rows = 5)
mergeCells(wb, 1, cols = (2 + uh$cumcells[1]):(1 + uh$cumcells[2]), rows = 5)
mergeCells(wb, 1, cols = (2 + uh$cumcells[2]):(1 + uh$cumcells[3]), rows = 5)

writeData(wb, 1, uhlabels, startRow = 5, startCol = 2, colNames = FALSE)
addStyle(wb, 1, rows = 5, cols = 3:endcol, style = uberheaderStyle)

# Data / body (with header)
writeData(wb, 1, df, startRow = 6, startCol = 2)
writeData(wb, 1, headers, startRow = 6, startCol = 2, colNames = FALSE)
addStyle(wb, 1, rows = 6, cols = 2:endcol, style = headerStyle)
addStyle(wb, 1, rows = 7:endrow, cols = 2:endcol, style = bodyStyle, gridExpand = TRUE)
addStyle(wb, 1, rows = endrow, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)

# Data source
writeData(wb, 1, source, startRow = endrow + 1, startCol = 2)
addStyle(wb, 1, rows = endrow + 1, cols = 2, style = sourceStyle)

# Footnotes
writeData(wb, 1, "Notes", startRow = endrow + 3, startCol = 2)
addStyle(wb, 1, rows = endrow + 3, cols = 2, style = footnoteHeaderStyle)
writeData(wb, 1, footnotes, startRow = endrow + 4, startCol = 2)
addStyle(wb, 1, rows = (endrow + 4):(endrow + 14), cols = 2, style = footnoteStyle)

setColWidths(wb, 1, cols = 3:endcol, widths = "auto")
saveWorkbook(wb, filename, overwrite = TRUE)
openXL(wb)