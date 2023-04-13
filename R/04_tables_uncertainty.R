
# load -------------------------------------------------------------------------
library(readxl)
source("R/00_functions.R")
uncertainty <- list()

# open xlsm file, enable editing, and save before importing

# Import data from XLSX --------------------------------------------------------

relahc <- read_xlsx("data/Poverty 3yr Scotland no2021.xlsm", sheet = "relahc_main") %>%
  mutate(type = "rel", key = "ahc") %>%
  select(-"0")

relbhc <- read_xlsx("data/Poverty 3yr Scotland no2021.xlsm", sheet = "relbhc_main") %>%
  mutate(type = "rel", key = "bhc") %>%
  select(-"0")

absahc <- read_xlsx("data/Poverty 3yr Scotland no2021.xlsm", sheet = "absahc_main") %>%
  mutate(type = "abs", key = "ahc") %>%
  select(-"0")

absbhc <- read_xlsx("data/Poverty 3yr Scotland no2021.xlsm", sheet = "absbhc_main") %>%
  mutate(type = "abs", key = "bhc") %>%
  select(-"0")

cmdbhc <- read_xlsx("data/LowincMatDepCh 3yr Scotland no2021.xlsm", sheet = "main") %>%
  mutate(type = "cmd", key = "bhc", thresh = NA) %>% head(1L)

penmd <- read_xlsx("data/MatDepPn 3yr Scotland no2021.xlsm", sheet = "main") %>%
  mutate(type = "pmd", key = NA, thresh = NA, group = "Pensioners")


# Tidy data --------------------------------------------------------------------

data <- rbind(relahc, relbhc,
              absahc, absbhc,
              cmdbhc, penmd) %>%
  filter(thresh == 60 | is.na(thresh)) %>%

  mutate_at(vars(contains("num")), ~fmtpop(round2(., -4))) %>%
  mutate_at(vars(contains("rate")), ~round2(., 2)) %>%
  mutate(Group = word(group, 1),
         Group = ifelse(Group == "Working-age", "Working-age adults", Group),
         Group = ifelse(Group == "Lowinc", "Children", Group),
         Group = factor(Group, levels = c("People", "Children", "Working-age adults", "Pensioners")),
         Proportion = paste0(central_rate, "% (", lower_rate, " - ", upper_rate, "%)"),
         Number = paste0(central_num, " (", lower_num, " - ", upper_num, ")")) %>%
  filter(Group != "Adult",
         Group != "Benefit") %>%
  arrange(type, key, Group)

# Create tables ----------------------------------------------------------------

uncertainty$unformatted <- data %>%
  select(Group, key, type, central_rate, lower_rate, upper_rate,
         central_num, lower_num, upper_num)

uncertainty$relahc <- data %>% filter(type == "rel", key == "ahc") %>%
  select(Group, Proportion, Number)

uncertainty$relbhc <- data %>% filter(type == "rel", key == "bhc") %>%
  select(Group, Proportion, Number)

uncertainty$absahc <- data %>% filter(type == "abs", key == "ahc") %>%
  select(Group, Proportion, Number)

uncertainty$absbhc <- data %>% filter(type == "abs", key == "bhc") %>%
  select(Group, Proportion, Number)

uncertainty$cmd <- data %>% filter(type == "cmd") %>%
  select(Group, Proportion, Number) %>%
  # suppress numbers
  mutate(Number = "Not available")

uncertainty$pmd <- data %>% filter(type == "pmd") %>%
  select(Group, Proportion, Number) %>%
  # suppress numbers
  mutate(Number = "Not available")

saveRDS(uncertainty, "data/uncertainty.rds")
rm(list = ls())

cat("Uncertainty tables (repackaged from DWP) created", fill = TRUE)
