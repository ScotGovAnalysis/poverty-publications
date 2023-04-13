# load ----------------------------------------------------------------------

library(tidyverse)
source("R/00_strings.R")

househol <- readRDS("data/househol_clean.rds")

years <- levels(labels$years$formatted)

# Sampling method: stratified clustered sample

# get PSUs ---------------------------------------------------------------------

psu <- househol %>%
  mutate(yearn = factor(year, levels = labels$years$years,
                        labels = labels$years$numbered),
         yearn = as.numeric(yearn)) %>%

  # psu and strata info available from 9798
  filter(yearn >= 4) %>%

  select(sernum, yearn, sstrtreg) %>%
  mutate(region = case_when(yearn < 7 ~ sstrtreg,
                            yearn >= 7 ~ floor(sernum/100000000)),

         # Northern Ireland regions
         region = ifelse(region == 30 & yearn == 14, 50, region),
         month = case_when(region == 50 ~ floor(sernum/10) - 100 * floor(sernum / 1000)),

         psu = case_when(yearn < 7 ~ floor(sernum/10000),
                         yearn >= 7 ~ floor(sernum/100000))) %>%
  select(yearn, sernum, region, month, psu) %>%
  arrange(yearn, region, month, psu)

# get strata (psu-pairs) -------------------------------------------------------

# GB
psu_pair_gb <- psu %>%
  filter(region != 50) %>%
  select(yearn, region, psu) %>%
  group_by(yearn, region, psu) %>%
  slice(1) %>%
  group_by(yearn, region) %>%
  mutate(x = row_number(),
         psu_pair_number = region * 1000 + (x + x %% 2) / 2,
         psu_pair_number = ifelse(x == max(x) & x %% 2 == 1,
                                  psu_pair_number - 1,
                                  psu_pair_number)) %>%
  select(yearn, region, psu, psu_pair_number)

# NI
psu_pair_ni <- psu %>%
  filter(region == 50) %>%
  select(yearn, month, psu) %>%
  group_by(yearn, month, psu) %>%
  slice(1) %>%
  group_by(yearn, month) %>%
  mutate(x = row_number(),
         psu_pair_number = 5000000 + month * 1000 + (x + x %% 2) / 2,
         psu_pair_number = ifelse(x == max(x) & x %% 2 == 1,
                                  psu_pair_number - 1,
                                  psu_pair_number)) %>%
  select(yearn, month, psu, psu_pair_number)

# combine ----------------------------------------------------------------------

psu_gb <- psu %>%
  filter(region != 50) %>%
  left_join(psu_pair_gb, by = c("yearn", "region", "psu"))

psu_ni <- psu %>%
  filter(region == 50) %>%
  left_join(psu_pair_ni, by = c("yearn", "month", "psu"))

sample_design_identifiers <- rbind(psu_gb, psu_ni) %>%
  select(yearn, sernum, psu, psu_pair_number)

# checks -----------------------------------------------------------------------

# check for duplicate hhlds
sample_design_identifiers %>%
  group_by(yearn, sernum) %>%
  count() %>%
  filter(n > 1)

# check for lonely psus (less than 2 psus per cluster)
sample_design_identifiers %>%
  group_by(yearn, psu_pair_number) %>%
  count() %>%
  filter(n < 2)

## remove hhlds not included in hbai
# left_join(hbai %>% select(sernum, benunit, gvtregn, yearn) %>% filter(benunit == 1, yearn == 26), by = "sernum") %>%
# filter(!is.na(gvtregn))

# save -------------------------------------------------------------------------
saveRDS(sample_design_identifiers, "data/sample_design_identifiers.rds")
rm(list = ls())

cat("Sampling identifiers (PSUs and strata) created", fill = TRUE)
