


library(tidyverse)
library(xltabr)

source("R/00_functions.R")
source("R/00_strings.R")

hbai <- readRDS("data/tidyhbai.rds")

# Tables - Poverty and deprivation numbers and rates 
# for children, working-age, pensioners, and all people

sevpovbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, low50bhc))
sevpovahc <- do.call(rbind.data.frame, lapply(hbai, getpov, low50ahc))

cmdahc <- do.call(rbind.data.frame, lapply(hbai, getpov, cmdahc))
cmdbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, cmdbhc))
cmdahc_new <- do.call(rbind.data.frame, lapply(hbai, getpov, cmdahc_new))
cmdbhc_new <- do.call(rbind.data.frame, lapply(hbai, getpov, cmdbhc_new))
cmdahc <- do.call(rbind.data.frame, lapply(hbai, getpov, cmdahc))

pndep <- do.call(rbind.data.frame, lapply(hbai, getpov, mdpn)) 

relpovbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, low60bhc)) %>% addyearvar() %>% formatpov()
relpovahc <- do.call(rbind.data.frame, lapply(hbai, getpov, low60ahc)) %>% addyearvar() %>% formatpov()
abspovbhc <- do.call(rbind.data.frame, lapply(hbai, getpov, abspovbhc)) 
abspovahc <- do.call(rbind.data.frame, lapply(hbai, getpov, abspovahc)) 


wb <- auto_df_to_wb(df = relpovbhc, auto_number_format = FALSE, vertical_border = FALSE) 

openxlsx::openXL(wb)




