

library(tidyverse)
library(openxlsx)
library(haven)
library(stringr)
library(Hmisc)
library(scales)


# replace categories with other categories 
decode <- function(x, search, replace, default = NULL) {
  # build a nested ifelse function by recursion
  
  decode.fun <- function(search, replace, default = NULL)
    
    if (length(search) == 0L) {function(x) if (is.null(default)) x else rep(default, length(x))} 
  else {function(x) ifelse(x == search[1L], 
                           replace[1L],
                           decode.fun(tail(search,  -1L),
                                      tail(replace, -1L),
                                      default)(x))}
  
  return(decode.fun(search, replace, default)(x))
}

# get flags for poverty outcomes and some hhld level charateristics 
gethhworkstatus <- function(df){
  
  # get household level work status
  workinghh <- df %>%
    mutate(working = ifelse(ecobu %in% labels[["economic"]]$labels[1:5], 1, 0)) %>%
    group_by(sernum) %>%
    summarise(workinghh = max(working))
  
  df %>%
    left_join(workinghh, by = "sernum")
}

gethhdisabledstatus <- function(df){
  
  # get household level disability status
  disabledhh <- df %>%
    group_by(sernum) %>%
    summarise(disch_hh = max(discorkid),
              disad_hh = max(discorabflg)) %>%
    mutate(disch_hh = ifelse(disch_hh > 0, 1, 0),
           disad_hh = ifelse(disad_hh >0, 1, 0),
           dispp_hh = ifelse(disch_hh + disad_hh > 0, 1, 0 ),
           disch_hh = factor(disch_hh, levels = labels[["disch"]]$codes, labels = labels[["disch"]]$labels),
           disad_hh = factor(disad_hh, levels = labels[["disad"]]$codes, labels = labels[["disad"]]$labels),
           dispp_hh = factor(dispp_hh, levels = labels[["dispp"]]$codes, labels = labels[["dispp"]]$labels),
           disch_hh = forcats::fct_explicit_na(disch_hh),
           disad_hh = forcats::fct_explicit_na(disad_hh),
           dispp_hh = forcats::fct_explicit_na(dispp_hh))
  
  df %>%
    left_join(disabledhh, by = "sernum")
}

# get flags for poverty outcomes
getpovertyflags <- function(df){
  
  df %>%
    mutate(abspovahc = ifelse(s_oe_ahc*infl_ahc < abspovahc_threshold, 1, 0),
           abspovbhc = ifelse(s_oe_bhc*infl_bhc < abspovbhc_threshold, 1, 0),
           workpovahc = ifelse(low60ahc == 1 & workinghh == 1, 1, 0),
           workpovbhc = ifelse(low60bhc == 1 & workinghh == 1, 1, 0),
           cmdahc = ifelse(low70ahc == 1 & mdch == 1, 1, 0),
           cmdahc_new = ifelse(low70ahc == 1 & mdchnew == 1, 1, 0),
           cmdbhc = ifelse(low70bhc == 1 & mdch == 1, 1, 0),
           cmdbhc_new = ifelse(low70bhc == 1 & mdchnew == 1, 1, 0)) 
}

# get urban/rural flags from FRS househol dataset
geturbanrural <- function(df){
  
  # get the correct FRS househol dataset (current year is stored in "comment" attribute)
  urindshh <- househol[[comment(df)]]
  
  # remove some attributes to avoid warnings
  attr(urindshh$sernum, "format.sas") <- NULL
  attr(urindshh$sernum, "label") <- NULL
  attr(df$sernum, "format.sas") <- NULL
  attr(df$sernum, "label") <- NULL
  
  # join with hbai dataset
  df %>%
    left_join(urindshh, by = "sernum") %>%
    mutate(urinds = factor(urinds, 
                           levels = labels[["urbrur"]]$codes, 
                           labels = labels[["urbrur"]]$labels))
}

# get poverty flags and adult weight from tidy hbai dataset
addpovflagsnadultwgt <- function(df){
  
  # get the correct tidy hbai dataset
  # current year is stored in "comment" attribute
  pov_hbai <- tidyhbai[[comment(df)]]
  
  # remove some attributes to avoid warnings
  attr(pov_hbai$sernum, "format.sas") <- NULL
  attr(pov_hbai$sernum, "label") <- NULL
  attr(pov_hbai$benunit, "format.sas") <- NULL
  attr(pov_hbai$benunit, "label") <- NULL
  attr(df$sernum, "format.sas") <- NULL
  attr(df$sernum, "label") <- NULL
  attr(df$benunit, "format.sas") <- NULL
  attr(df$benunit, "label") <- NULL
  
  pov_hbai <- pov_hbai %>%
    select(sernum, benunit, gs_newad, adultb, low50ahc, low60ahc, gvtregn)
  
  # join with adult dataset
  df %>%
    left_join(pov_hbai, by = c("sernum", "benunit")) %>%
    mutate(adultwgt = gs_newad/adultb)
}

# get poverty numbers and rates for children, adults, pensioners, and people 
getpov <- function(df, povvar){
  
  df$povvar <- df[[povvar]]
  
  df %>%
    filter(gvtregn == "Scotland") %>%
    mutate(chn = sum(gs_newch),
           wan = sum(gs_newwa),
           pnn = sum(gs_newpn),
           ppn = sum(gs_newpp)) %>%
    group_by(povvar) %>%
    summarise(chnum = sum(gs_newch),
              wanum = sum(gs_newwa),
              pnnum = sum(gs_newpn),
              ppnum = sum(gs_newpp),
              chn = max(chn),
              wan = max(wan),
              pnn = max(pnn),
              ppn = max(ppn)) %>%
    mutate(chrate = chnum/chn,
           warate = wanum/wan,
           pnrate = pnnum/pnn,
           pprate = ppnum/ppn) %>%
    select(ppnum, chnum, wanum, pnnum, 
           pprate, chrate, warate, pnrate, povvar) %>%
    filter(povvar == 1) %>%
    ungroup() %>%
    select(-povvar)
  
}

# get poverty numbers and rates by grouping variable 
getpovby <- function(df, povvar, groupingvar){
  
  df$povvar <- df[[povvar]]
  df$groupingvar <- df[[groupingvar]]
  
  grouped <- df %>%
    mutate(groupingvar = factor(groupingvar),
           groupingvar = fct_explicit_na(groupingvar)) %>%
    filter(gvtregn == "Scotland") %>%
    group_by(groupingvar) %>%
    mutate(chn = sum(gs_newch),
           wan = sum(gs_newwa),
           pnn = sum(gs_newpn),
           ppn = sum(gs_newpp),
           adn = sum(gs_newad),
           groupsample = n(),
           groupsample_ch = sum(gs_newch > 0, na.rm=TRUE),
           groupsample_wa = sum(gs_newwa > 0, na.rm=TRUE),
           groupsample_pn = sum(gs_newpn > 0, na.rm=TRUE),
           groupsample_ad = sum(gs_newad > 0, na.rm=TRUE)) %>%
    group_by(povvar, groupingvar) %>%
    summarise(chnum = sum(gs_newch),
              wanum = sum(gs_newwa),
              pnnum = sum(gs_newpn),
              ppnum = sum(gs_newpp),
              adnum = sum(gs_newad),
              chn = max(chn),
              wan = max(wan),
              pnn = max(pnn),
              ppn = max(ppn),
              adn = max(adn),
              groupsample = max(groupsample),
              groupsample_ch = max(groupsample_ch),
              groupsample_wa = max(groupsample_wa),
              groupsample_pn = max(groupsample_pn),
              groupsample_ad = max(groupsample_ad),
              povsample = n(),
              povsample_ch = sum(gs_newch > 0, na.rm=TRUE),
              povsample_wa = sum(gs_newwa > 0, na.rm=TRUE),
              povsample_pn = sum(gs_newpn > 0, na.rm=TRUE),
              povsample_ad = sum(gs_newad > 0, na.rm=TRUE)) %>%
    filter(povvar == 1) %>%
    mutate(chrate = chnum/chn,
           warate = wanum/wan,
           pnrate = pnnum/pnn,
           pprate = ppnum/ppn,
           adrate = adnum/adn,
           ppcomp = ppnum/sum(ppnum),
           chcomp = chnum/sum(chnum),
           wacomp = wanum/sum(wanum),
           pncomp = pnnum/sum(pnnum),
           adcomp = adnum/sum(adnum)) %>%
    ungroup() %>%
    select(groupingvar, 
           ppnum, chnum, wanum, pnnum, adnum,
           pprate, chrate, warate, pnrate, adrate,
           ppcomp, chcomp, wacomp, pncomp, adcomp,
           groupsample, groupsample_ch, groupsample_wa, groupsample_pn, groupsample_ad,
           povsample, povsample_ch, povsample_wa, povsample_pn, povsample_ad)
  
  total <- df %>%
    mutate(groupingvar = factor(groupingvar),
           groupingvar = fct_explicit_na(groupingvar)) %>%
    filter(gvtregn == "Scotland") %>%
    mutate(chn = sum(gs_newch),
           wan = sum(gs_newwa),
           pnn = sum(gs_newpn),
           ppn = sum(gs_newpp),
           adn = sum(gs_newad),
           groupsample = n(),
           groupsample_ch = sum(gs_newch > 0, na.rm=TRUE),
           groupsample_wa = sum(gs_newwa > 0, na.rm=TRUE),
           groupsample_pn = sum(gs_newpn > 0, na.rm=TRUE),
           groupsample_ad = sum(gs_newad > 0, na.rm=TRUE)) %>%
    group_by(povvar) %>%
    summarise(chnum = sum(gs_newch),
              wanum = sum(gs_newwa),
              pnnum = sum(gs_newpn),
              ppnum = sum(gs_newpp),
              adnum = sum(gs_newad),
              chn = max(chn),
              wan = max(wan),
              pnn = max(pnn),
              ppn = max(ppn),
              adn = max(adn),
              groupsample = max(groupsample),
              groupsample_ch = max(groupsample_ch),
              groupsample_wa = max(groupsample_wa),
              groupsample_pn = max(groupsample_pn),
              groupsample_ad = max(groupsample_ad),
              povsample = n(),
              povsample_ch = sum(gs_newch > 0, na.rm=TRUE),
              povsample_wa = sum(gs_newwa > 0, na.rm=TRUE),
              povsample_pn = sum(gs_newpn > 0, na.rm=TRUE),
              povsample_ad = sum(gs_newad > 0, na.rm=TRUE)) %>%
    filter(povvar == 1) %>%
    mutate(chrate = chnum/chn,
           warate = wanum/wan,
           pnrate = pnnum/pnn,
           pprate = ppnum/ppn,
           adrate = adnum/adn,
           ppcomp = ppnum/sum(ppnum),
           chcomp = chnum/sum(chnum),
           wacomp = wanum/sum(wanum),
           pncomp = pnnum/sum(pnnum),
           adcomp = adnum/sum(adnum),
           groupingvar = "All") %>%
    ungroup() %>%
    select(groupingvar, 
           ppnum, chnum, wanum, pnnum, adnum,
           pprate, chrate, warate, pnrate, adrate,
           ppcomp, chcomp, wacomp, pncomp, adcomp,
           groupsample, groupsample_ch, groupsample_wa, groupsample_pn, groupsample_ad,
           povsample, povsample_ch, povsample_wa, povsample_pn, povsample_ad)
  
  rbind(total, grouped)
  
}

# get poverty numbers and rates by grouping variable - tidy adult dataset 
getpovby_adult <- function(df, povvar, groupingvar){
  
  df$povvar <- df[[povvar]]
  df$groupingvar <- df[[groupingvar]]
  
  grouped <- df %>%
    mutate(groupingvar = factor(groupingvar),
           groupingvar = fct_explicit_na(groupingvar)) %>%
    filter(gvtregn == "Scotland") %>%
    group_by(groupingvar) %>%
    mutate(adn = sum(gs_newad),
           groupsample_ad = sum(gs_newad > 0, na.rm=TRUE)) %>%
    group_by(povvar, groupingvar) %>%
    summarise(adnum = sum(gs_newad),
              adn = max(adn),
              groupsample_ad = max(groupsample_ad),
              povsample_ad = sum(gs_newad > 0, na.rm=TRUE)) %>%
    filter(povvar == 1) %>%
    mutate(adrate = adnum/adn,
           adcomp = adnum/sum(adnum)) %>%
    ungroup() %>%
    select(groupingvar, 
           adnum, adrate, adcomp,
           groupsample_ad, povsample_ad)
  
  total <- df %>%
    mutate(groupingvar = factor(groupingvar),
           groupingvar = fct_explicit_na(groupingvar)) %>%
    filter(gvtregn == "Scotland") %>%
    mutate(adn = sum(gs_newad),
           groupsample_ad = sum(gs_newad > 0, na.rm=TRUE)) %>%
    group_by(povvar) %>%
    summarise(adnum = sum(gs_newad),
              adn = max(adn),
              groupsample_ad = max(groupsample_ad),
              povsample_ad = sum(gs_newad > 0, na.rm=TRUE)) %>%
    filter(povvar == 1) %>%
    mutate(adrate = adnum/adn,
           adcomp = adnum/sum(adnum),
           groupingvar = "All") %>%
    ungroup() %>%
    select(groupingvar, 
           adnum, adrate, adcomp,
           groupsample_ad, povsample_ad)
  
  rbind(total, grouped)
  
}

# Add year variable to table
addyearvar <- function(df){
  
  df %>%
    rownames_to_column(var = "years") %>%
    mutate(years = str_sub(years, 1L, 4L),
           years = factor(years, 
                          levels = unique(labels[["years"]]$years), 
                          ordered = TRUE)) %>%
    select(years, everything())
}

# Format population numbers and rates
fmtpop <- function(x){
  
  require(scales)
  
  ifelse(!is.na(x), comma(x, 10000), NA)
}

fmtpct <- function(x){
  
  require(scales)
  
  ifelse(!is.na(x), percent(x, 1), NA)
}

formatpov <- function(df){
  
  df %>%
    mutate(years = factor(years, 
                          levels = labels[["years"]]$years, 
                          labels = labels[["years"]]$formatted)) %>%
    mutate_at(vars(ends_with("num")), fmtpop) %>%
    mutate_at(vars(ends_with("rate")), fmtpct) %>%
    select(1:9)
}

get3yraverage <- function(x){x = (x + lag(x, 1L) + lag(x, 2L))/3}

get3yrtotal <- function(x){x = x + lag(x, 1L) + lag(x, 2L)}

formatpov3yraverage <- function(df){
  
  df %>%
    mutate_at(vars(c(ends_with("rate")), ends_with("num")), get3yraverage) %>%
    tail(-2L) %>%
    mutate(years = factor(years, 
                          levels = labels[["years"]]$years, 
                          labels = labels[["years"]]$formatted)) %>%
    mutate_at(vars(ends_with("num")), fmtpop) %>%
    mutate_at(vars(ends_with("rate")), fmtpct) %>%
    select(1:9)
}

formatpovby3yraverage <- function(df){
  
df %>%
  group_by(groupingvar) %>%
  arrange(groupingvar, years) %>%
  mutate_at(vars(c(ends_with("rate")), ends_with("num"), ends_with("comp")), get3yraverage) %>%
  mutate_at(vars(contains("sample")), get3yrtotal) %>%
  mutate_at(vars(ends_with("num")), fmtpop) %>%
  mutate_at(vars(c(ends_with("rate"), ends_with("comp"))), fmtpct) %>%
  filter(groupingvar != "(Missing)") %>%
  ungroup() %>%
  mutate(groupingvar = factor(groupingvar),
         groupingvar = fct_relevel(groupingvar, "All")) %>%
  filter(!is.na(adnum))
}

ppsamplesizecheck <- function(df){
  df %>%
    mutate(ppnum = ifelse(povsample < 100, "..", ppnum),
           pprate = ifelse(groupsample < 100, "..", pprate))
}

chsamplesizecheck <- function(df){
  df %>%
    mutate(chnum = ifelse(povsample_ch < 100, "..", chnum),
           chrate = ifelse(groupsample_ch < 100, "..", chrate))
}

wasamplesizecheck <- function(df){
  df %>%
    mutate(wanum = ifelse(povsample_wa < 100, "..", wanum),
           warate = ifelse(groupsample_wa < 100, "..", warate))
}

pnsamplesizecheck <- function(df){
  df %>%
    mutate(pnnum = ifelse(povsample_pn < 100, "..", pnnum),
           pnrate = ifelse(groupsample_pn < 100, "..", pnrate))
}

adsamplesizecheck <- function(df){
  df %>%
    mutate(adnum = ifelse(povsample_ad < 100, "..", adnum),
           adrate = ifelse(groupsample_ad < 100, "..", adrate))
}

splitntranspose <- function(df, measure){
  
  df$measure <- df[["measure"]]
  
  df %>%
    select(years, groupingvar, measure) %>%
    spread(years, measure)
  
}


# Add header above header with merged cells
addUberheader <- function(wb = wb, sheet = 1, uberheaders = uberheaders, row = 5){
  
  # Do we have an uberheader?
  if (is.vector(uberheaders)){
  
  # Merged cells for uberheader
  
  uh <- data.frame("headers" = names(uberheaders), "cells" = uberheaders) %>%
    mutate(cumcells = cumsum(cells),
           startcol = 2, 
           startcol = ifelse(row_number()==1, 2, lag(startcol + cumcells)),
           endcol = startcol + cells - 1,
           colstring = str_c(startcol, ":", endcol),
           label = strrep(str_c(headers, ","), cells))
  
  uhlist <- vector("list", dim(uh)[1])
  
  for (i in 1:length(uhlist)){
    uhlist[[i]] <- c(uh$startcol[i], uh$endcol[i])
  }
  
  uhlabels <- paste(uh$label, sep = ",", collapse = "")
  uhlabels <- str_split(uhlabels, ",")
  uhlabels <- as.data.frame(t(as.data.frame(uhlabels))) %>%
    select(-last_col())
  
  lapply(uhlist, function(x) mergeCells(wb, sheet, cols = x[1]:x[2], rows = row))
  
  writeData(wb, sheet, uhlabels, startRow = row, startCol = 2, colNames = FALSE)
  }
  
}

# Create spreadsheet for headline poverty spreadsheets
createSpreadsheet <- function(data){
  
  df <- data[["df"]]
  filename <- paste0("output/", data[["filename"]])
  sheetname <- data[["sheetname"]]
  title <- data[["title"]]
  subtitle <- data[["subtitle"]]
  headers <- data[["headers"]]
  uberheaders <- data[["uberheaders"]]
  source <- data[["source"]]
  footnotes <- data[["footnotes"]]
  
  # Styles for Excel outputs
  
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
  
  # Calculate body dimensions
  endcol <- length(df) + 1
  endrow <- dim(df)[1] + 6
  
  # Transform headers into a data frame so they can be written as data
  headers <- ifelse(is.na(headers), NULL, as.data.frame(t(headers)))

  # If workbook already exists, open it and add sheet, otherwise create new workbook
  if (file.exists(filename)) {
    wb <- loadWorkbook(filename)
    if (sheetname %in% getSheetNames(filename)){removeWorksheet(wb, sheetname)}} 
  else {wb <- createWorkbook()}

  addWorksheet(wb, sheetname, gridLines = FALSE)

  # Title row
  writeData(wb, sheetname, title, startRow = 2, startCol = 2)
  addStyle(wb, sheetname, rows = 2, cols = 2, style = titleStyle)
  
  # Subtitle row
  writeData(wb, sheetname, subtitle, startRow = 3, startCol = 2)
  addStyle(wb, sheetname, rows = 3, cols = 2, style = subtitleStyle)
  
  # Uber header (above headers)
  addUberheader(wb, sheetname, uberheaders)
  if (is.vector(uberheaders)){
    addStyle(wb, sheetname, rows = 5, cols = 3:endcol, style = uberheaderStyle)}
  
  # Data / body (with header)
  writeData(wb, sheetname, df, startRow = 6, startCol = 2)
  writeData(wb, sheetname, headers, startRow = 6, startCol = 2, colNames = FALSE)
  addStyle(wb, sheetname, rows = 6, cols = 2:endcol, style = headerStyle)
  addStyle(wb, sheetname, rows = 7:endrow, cols = 2:endcol, style = bodyStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)
  
  # Data source
  writeData(wb, sheetname, source, startRow = endrow + 1, startCol = 2)
  addStyle(wb, sheetname, rows = endrow + 1, cols = 2, style = sourceStyle)
  
  # Footnotes
  if (is.vector(footnotes)){
  writeData(wb, sheetname, "Notes", startRow = endrow + 3, startCol = 2)
  addStyle(wb, sheetname, rows = endrow + 3, cols = 2, style = footnoteHeaderStyle)
  writeData(wb, sheetname, footnotes, startRow = endrow + 4, startCol = 2)
  addStyle(wb, sheetname, rows = (endrow + 4):(endrow + 14), cols = 2, style = footnoteStyle)
  }
  
  setColWidths(wb, sheetname, cols = 3:endcol, widths = "auto")
  saveWorkbook(wb, filename, overwrite = TRUE)
}

# Create spreadsheet for poverty characteristics spreadsheets
createWideSpreadsheet <- function(data){
  
  df1 <- data[["df1"]]
  df2 <- data[["df2"]]
  df3 <- data[["df3"]]
  df4 <- data[["df4"]]
  df5 <- data[["df5"]]
  df6 <- data[["df6"]]
  
  filename <- paste0("output/", data[["filename"]])
  sheetname <- data[["sheetname"]]
  title_a <- data[["title_a"]]
  title_b <- data[["title_b"]]
  title_c <- data[["title_c"]]
  subtitle_a <- data[["subtitle_a"]]
  subtitle_b <- data[["subtitle_b"]]
  subtitle_c <- data[["subtitle_c"]]
  subsubtitle_rel <- data[["subsubtitle_rel"]]
  subsubtitle_sev <- data[["subsubtitle_sev"]]
  headers <- data[["headers"]]
  source <- data[["source"]]
  footnotes <- data[["footnotes"]]
  
  # Styles for Excel outputs
  
  options("openxlsx.borderStyle" = "thin")
  options("openxlsx.borderColour" = "black")
  
  titleStyle <- createStyle(fontName="Segoe UI Semibold", fontSize = 14)
  subtitleStyle <- createStyle(fontName="Segoe UI", fontSize = 12)
  headerStyle <- createStyle(fontName="Segoe UI Semibold", fontSize = 10, halign = "right", border = "bottom")
  bodyStyle <- createStyle(halign = "right")
  endrowStyle <- createStyle(border = "bottom", halign = "right")
  sourceStyle <- createStyle(fontName="Segoe UI", fontSize = 10)
  footnoteHeaderStyle <- createStyle(fontName="Segoe UI Semibold", fontSize = 11, textDecoration = "BOLD")
  footnoteStyle <- createStyle(fontName="Segoe UI", fontSize = 11)
  
  # Calculate body dimensions
  endcol <- length(df1) + 1
  endrow1 <- dim(df1)[1] + 6
  endrow2 <- endrow1 + dim(df2)[1] + 4
  endrow3 <- endrow2 + dim(df3)[1] + 8
  endrow4 <- endrow3 + dim(df4)[1] + 4
  endrow5 <- endrow4 + dim(df5)[1] + 8
  endrow6 <- endrow5 + dim(df6)[1] + 4
  
  # Transform headers into a data frame so they can be written as data
  headers <- ifelse(is.na(headers), NULL, as.data.frame(t(headers)))
  
  # If workbook already exists, open it and add sheet, otherwise create new workbook
  if (file.exists(filename)) {
    wb <- loadWorkbook(filename)
    if (sheetname %in% getSheetNames(filename)){removeWorksheet(wb, sheetname)}} 
  else {wb <- createWorkbook()}
  
  addWorksheet(wb, sheetname, gridLines = FALSE)
  
  # "A" tables - title row
  writeData(wb, sheetname, title_a, startRow = 2, startCol = 2)
  addStyle(wb, sheetname, rows = 2, cols = 2, style = titleStyle)
  
  # "A" tables = subtitle row
  writeData(wb, sheetname, subtitle_a, startRow = 3, startCol = 2)
  addStyle(wb, sheetname, rows = 3, cols = 2, style = subtitleStyle)
  
  # Add rel pov table title
  writeData(wb, sheetname, subsubtitle_rel, startRow = 5, startCol = 2)
  addStyle(wb, sheetname, rows = 5, cols = 2, style = subtitleStyle)
  
  # Header
  writeData(wb, sheetname, headers, startRow = 6, startCol = 2, colNames = FALSE)
  addStyle(wb, sheetname, rows = 6, cols = 2:endcol, style = headerStyle)
  
  # Data / body 1
  writeData(wb, sheetname, df1, startRow = 7, startCol = 2, colNames = FALSE)
  addStyle(wb, sheetname, rows = 7, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = 8:endrow1, cols = 2:endcol, style = bodyStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow1, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)
  
  # Data source
  writeData(wb, sheetname, source, startRow = endrow1 + 1, startCol = 2)
  addStyle(wb, sheetname, rows = endrow1 + 1, cols = 2, style = sourceStyle)
  
  # Add sev pov table title
  writeData(wb, sheetname, subsubtitle_sev, startRow = endrow1 + 3, startCol = 2)
  addStyle(wb, sheetname, rows = endrow1 + 3, cols = 2, style = subtitleStyle)
  
  # Header
  writeData(wb, sheetname, headers, startRow = endrow1 + 4, startCol = 2, colNames = FALSE)
  addStyle(wb, sheetname, rows = endrow1 + 4, cols = 2:endcol, style = headerStyle)
  
  # Data / body 2
  writeData(wb, sheetname, df2, startRow = endrow1 + 5, startCol = 2, colNames = FALSE)
  addStyle(wb, sheetname, rows = endrow1 + 5, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow1 + 6:endrow2, cols = 2:endcol, style = bodyStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow2, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)
  
  # Data source
  writeData(wb, sheetname, source, startRow = endrow2 + 1, startCol = 2)
  addStyle(wb, sheetname, rows = endrow2 + 1, cols = 2, style = sourceStyle)
  
  # "B" tables - title row
  writeData(wb, sheetname, title_b, startRow = endrow2 + 4, startCol = 2)
  addStyle(wb, sheetname, rows = endrow2 + 4, cols = 2, style = titleStyle)
  
  # "B" tables - subtitle row
  writeData(wb, sheetname, subtitle_b, startRow = endrow2 + 5, startCol = 2)
  addStyle(wb, sheetname, rows = endrow2 + 5, cols = 2, style = subtitleStyle)
  
  # Add rel pov table title
  writeData(wb, sheetname, subsubtitle_rel, startRow = endrow2 + 7, startCol = 2)
  addStyle(wb, sheetname, rows = endrow2 + 7, cols = 2, style = subtitleStyle)
  
  # Header
  writeData(wb, sheetname, headers, startRow = endrow2 + 8, startCol = 2, colNames = FALSE)
  addStyle(wb, sheetname, rows = endrow2 + 8, cols = 2:endcol, style = headerStyle)
  
  # Data / body 3
  writeData(wb, sheetname, df3, startRow = endrow2 + 9, startCol = 2, colNames = FALSE)
  addStyle(wb, sheetname, rows = endrow2 + 9, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow2 + 10:endrow3, cols = 2:endcol, style = bodyStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow3, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)
  
  # Data source
  writeData(wb, sheetname, source, startRow = endrow3 + 1, startCol = 2)
  addStyle(wb, sheetname, rows = endrow3 + 1, cols = 2, style = sourceStyle)
  
  # Add sev pov table title
  writeData(wb, sheetname, subsubtitle_sev, startRow = endrow3 + 3, startCol = 2)
  addStyle(wb, sheetname, rows = endrow3 + 3, cols = 2, style = subtitleStyle)
  
  # Header
  writeData(wb, sheetname, headers, startRow = endrow3 + 4, startCol = 2, colNames = FALSE)
  addStyle(wb, sheetname, rows = endrow3 + 4, cols = 2:endcol, style = headerStyle)
  
  # Data / body 4
  writeData(wb, sheetname, df4, startRow = endrow3 + 5, startCol = 2, colNames = FALSE)
  addStyle(wb, sheetname, rows = endrow3 + 5, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow3 + 6:endrow4, cols = 2:endcol, style = bodyStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow4, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)
  
  # Data source
  writeData(wb, sheetname, source, startRow = endrow4 + 1, startCol = 2)
  addStyle(wb, sheetname, rows = endrow4 + 1, cols = 2, style = sourceStyle)
  
  # "C" tables - title row
  writeData(wb, sheetname, title_c, startRow = endrow4 + 4, startCol = 2)
  addStyle(wb, sheetname, rows = endrow4 + 4, cols = 2, style = titleStyle)
  
  # "C" tables - subtitle row
  writeData(wb, sheetname, subtitle_c, startRow = endrow4 + 5, startCol = 2)
  addStyle(wb, sheetname, rows = endrow4 + 5, cols = 2, style = subtitleStyle)
  
  # Add rel pov table title
  writeData(wb, sheetname, subsubtitle_rel, startRow = endrow4 + 7, startCol = 2)
  addStyle(wb, sheetname, rows = endrow4 + 7, cols = 2, style = subtitleStyle)
  
  # Header
  writeData(wb, sheetname, headers, startRow = endrow4 + 8, startCol = 2, colNames = FALSE)
  addStyle(wb, sheetname, rows = endrow4 + 8, cols = 2:endcol, style = headerStyle)
  
  # Data / body 5
  writeData(wb, sheetname, df5, startRow = endrow4 + 9, startCol = 2, colNames = FALSE)
  addStyle(wb, sheetname, rows = endrow4 + 9, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow4 + 10:endrow5, cols = 2:endcol, style = bodyStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow5, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)
  
  # Data source
  writeData(wb, sheetname, source, startRow = endrow5 + 1, startCol = 2)
  addStyle(wb, sheetname, rows = endrow5 + 1, cols = 2, style = sourceStyle)
  
  # Add sev pov table title
  writeData(wb, sheetname, subsubtitle_sev, startRow = endrow5 + 3, startCol = 2)
  addStyle(wb, sheetname, rows = endrow5 + 3, cols = 2, style = subtitleStyle)
  
  # Header
  writeData(wb, sheetname, headers, startRow = endrow5 + 4, startCol = 2, colNames = FALSE)
  addStyle(wb, sheetname, rows = endrow5 + 4, cols = 2:endcol, style = headerStyle)
  
  # Data / body 6
  writeData(wb, sheetname, df6, startRow = endrow5 + 5, startCol = 2, colNames = FALSE)
  addStyle(wb, sheetname, rows = endrow5 + 5, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow5 + 6:endrow6, cols = 2:endcol, style = bodyStyle, gridExpand = TRUE)
  addStyle(wb, sheetname, rows = endrow6, cols = 2:endcol, style = endrowStyle, gridExpand = TRUE)
  
  # Data source
  writeData(wb, sheetname, source, startRow = endrow6 + 1, startCol = 2)
  addStyle(wb, sheetname, rows = endrow6 + 1, cols = 2, style = sourceStyle)
  
  
  # Footnotes
  if (is.vector(footnotes)){
    writeData(wb, sheetname, "Notes", startRow = endrow6 + 3, startCol = 2)
    addStyle(wb, sheetname, rows = endrow6 + 3, cols = 2, style = footnoteHeaderStyle)
    writeData(wb, sheetname, footnotes, startRow = endrow6 + 4, startCol = 2)
    addStyle(wb, sheetname, rows = (endrow6 + 4):(endrow6 + 14), cols = 2, style = footnoteStyle)
  }
  
  setColWidths(wb, sheetname, cols = 2, widths = 40)
  setColWidths(wb, sheetname, cols = 3:endcol, widths = "auto")
  saveWorkbook(wb, filename, overwrite = TRUE)
}