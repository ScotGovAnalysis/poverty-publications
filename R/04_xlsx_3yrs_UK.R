
# Create spreadsheet for 3-year averaged data

source("R/00_functions.R", encoding = "UTF-8")
source("R/00_strings.R")

filename <- "output/UK comparisons.xlsx"

hbai <- readRDS("data/tidyhbai.rds")
persistent <- readRDS("data/persistentpoverty.rds") %>%
  mutate_if(is.numeric, ~round2(., 2)) %>%
  mutate(group = factor(group, levels = c("pp", "ch", "wa", "pn"))) %>%
  rename(Period = period)

# 1 Relative AHC ---------------------------------------------------------------
pp <- getpovby(hbai, pov = "low60ahc", by = "gvtregn") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

ch <- getpovby(hbai, pov = "low60ahc", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

wa <- getpovby(hbai, pov = "low60ahc", by = "gvtregn", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

pn <- getpovby(hbai, pov = "low60ahc", by = "gvtregn", weight = "gs_newpn") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

data <- list(file = filename,
             sheet = "1 Rel AHC",
             sheettitle = "1. Relative poverty (after housing costs)",
             dfs = list(pp, ch, wa, pn),
             formats = c("pct", "pct", "pct", "pct"),
             totalrow = TRUE,
             titles = c("People in relative poverty",
                        "Children in relative poverty",
                        "Working-age adults in relative poverty",
                        "Pensioners in relative poverty"),
             subtitles = c("Proportion of people in each group who are in relative poverty (below 60% of UK median income after housing costs)",
                           "Proportion of children in each group who are in relative poverty (below 60% of UK median income after housing costs)",
                           "Proportion of working-age adults in each group who are in relative poverty (below 60% of UK median income after housing costs)",
                           "Proportion of pensioners in each group who are in relative poverty (below 60% of UK median income after housing costs)"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset"
)

# Create new worksheet
createWideSpreadsheet(data)

# 2 Absolute AHC ---------------------------------------------------------------
pp <- getpovby(hbai, pov = "low60ahcabs", by = "gvtregn") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

ch <- getpovby(hbai, pov = "low60ahcabs", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

wa <- getpovby(hbai, pov = "low60ahcabs", by = "gvtregn", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

pn <- getpovby(hbai, pov = "low60ahcabs", by = "gvtregn", weight = "gs_newpn") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

data <- list(file = filename,
             sheet = "2 Abs AHC",
             sheettitle = "2. Absolute poverty (after housing costs)",
             dfs = list(pp, ch, wa, pn),
             formats = c("pct", "pct", "pct", "pct"),
             totalrow = TRUE,
             titles = c("People in absolute poverty",
                        "Children in absolute poverty",
                        "Working-age adults in absolute poverty",
                        "Pensioners in absolute poverty"),
             subtitles = c("Proportion of people in each group who are in absolute poverty (below 60% of UK median income after housing costs)",
                           "Proportion of children in each group who are in absolute poverty (below 60% of UK median income after housing costs)",
                           "Proportion of working-age adults in each group who are in absolute poverty (below 60% of UK median income after housing costs)",
                           "Proportion of pensioners in each group who are in absolute poverty (below 60% of UK median income after housing costs)"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset"
)

# Create new worksheet
createWideSpreadsheet(data)

# 3 In-work AHC ------------------------------------------------------------------

df <- hbai %>%
  filter(low60ahc == 1) %>%
  mutate(workinghh = factor(workinghh, levels = labels$workinghh$labels,
                              labels = labels$workinghh$codes))

ch <- getpovby(df, pov = "workinghh", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 4) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

wa <- getpovby(df, pov = "workinghh", by = "gvtregn", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 4) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

data <- list(file = filename,
             sheet = "3 In-work AHC",
             sheettitle = "3. In-work poverty (after housing costs)",
             dfs = list(ch, wa),
             formats = c("pct", "pct"),
             totalrow = TRUE,
             titles = c("Children in relative poverty who live in working households",
                        "Working-age adults in relative poverty who live in working households"),
             subtitles = c("Proportion of children in relative poverty (below 60% of UK median income after housing costs) who live in a household where someone is in paid work",
                           "Proportion of working-age adults in in relative poverty (below 60% of UK median income after housing costs) who live in a household where someone is in paid work"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset"
)

# Create new worksheet
createWideSpreadsheet(data)

# 4 Relative BHC -----------------------------------------------------------------
pp <- getpovby(hbai, pov = "low60bhc", by = "gvtregn") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

ch <- getpovby(hbai, pov = "low60bhc", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

wa <- getpovby(hbai, pov = "low60bhc", by = "gvtregn", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

pn <- getpovby(hbai, pov = "low60bhc", by = "gvtregn", weight = "gs_newpn") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

data <- list(file = filename,
             sheet = "4 Rel BHC",
             sheettitle = "4. Relative poverty (before housing costs)",
             dfs = list(pp, ch, wa, pn),
             formats = c("pct", "pct", "pct", "pct"),
             totalrow = TRUE,
             titles = c("People in relative poverty",
                        "Children in relative poverty",
                        "Working-age adults in relative poverty",
                        "Pensioners in relative poverty"),
             subtitles = c("Proportion of people in each group who are in relative poverty (below 60% of UK median income before housing costs)",
                           "Proportion of children in each group who are in relative poverty (below 60% of UK median income before housing costs)",
                           "Proportion of working-age adults in each group who are in relative poverty (below 60% of UK median income before housing costs)",
                           "Proportion of pensioners in each group who are in relative poverty (below 60% of UK median income before housing costs)"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset"
)

# Create new worksheet
createWideSpreadsheet(data)

# 5 Absolute BHC ---------------------------------------------------------------

pp <- getpovby(hbai, pov = "low60bhcabs", by = "gvtregn") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

ch <- getpovby(hbai, pov = "low60bhcabs", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

wa <- getpovby(hbai, pov = "low60bhcabs", by = "gvtregn", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

pn <- getpovby(hbai, pov = "low60bhcabs", by = "gvtregn", weight = "gs_newpn") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 3) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

data <- list(file = filename,
             sheet = "5 Abs BHC",
             sheettitle = "5. Absolute poverty (before housing costs)",
             dfs = list(pp, ch, wa, pn),
             formats = c("pct", "pct", "pct", "pct"),
             totalrow = TRUE,
             titles = c("People in absolute poverty",
                        "Children in absolute poverty",
                        "Working-age adults in absolute poverty",
                        "Pensioners in absolute poverty"),
             subtitles = c("Proportion of people in each group who are in absolute poverty (below 60% of UK median income before housing costs)",
                           "Proportion of children in each group who are in absolute poverty (below 60% of UK median income before housing costs)",
                           "Proportion of working-age adults in each group who are in absolute poverty (below 60% of UK median income before housing costs)",
                           "Proportion of pensioners in each group who are in absolute poverty (below 60% of UK median income before housing costs)"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset"
)

# Create new worksheet
createWideSpreadsheet(data)

# 6 In-work BHC ----------------------------------------------------------------

df <- hbai %>%
  filter(low60bhc == 1) %>%
  mutate(workinghh = factor(workinghh, levels = labels$workinghh$labels,
                            labels = labels$workinghh$codes))

ch <- getpovby(df, pov = "workinghh", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 4) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

wa <- getpovby(df, pov = "workinghh", by = "gvtregn", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  filter(yearn >= 4) %>%
  mutate(Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  roundall() %>%
  select(year, Region, rate) %>%
  spread(year, rate)

data <- list(file = filename,
             sheet = "6 In-work BHC",
             sheettitle = "6. In-work poverty (before housing costs)",
             dfs = list(ch, wa),
             formats = c("pct", "pct"),
             totalrow = TRUE,
             titles = c("Children in relative poverty who live in working households",
                        "Working-age adults in relative poverty who live in working households"),
             subtitles = c("Proportion of children in relative poverty (below 60% of UK median income before housing costs) who live in a household where someone is in paid work",
                           "Proportion of working-age adults in in relative poverty (below 60% of UK median income before housing costs) who live in a household where someone is in paid work"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset"
)

# Create new worksheet
createWideSpreadsheet(data)

# 7 CMD ------------------------------------------------------------------------

cmd_ahc <- getpovby(filter(hbai, gs_newch > 0), pov = "cmdahc",
                    weight = "gs_newch", by = "gvtregn") %>%
  filter(yearn >= 18) %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(Measure = "After housing costs",
         Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  select(year, Region, rate) %>%
  spread(year, rate)

cmd_bhc <- getpovby(filter(hbai, gs_newch > 0), pov = "cmdbhc",
                    weight = "gs_newch", by = "gvtregn") %>%
  filter(yearn >= 18) %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  samplesizecheck() %>%
  roundall() %>%
  mutate(Measure = "Before housing costs",
         Region = fct_relevel(groupingvar, "Scotland", after = 2L),
         Region = fct_relevel(Region, "Wales", after = 3L),
         year = factor(yearn, levels = labels$years$numbered,
                       labels = labels$years$periods)) %>%
  select(year, Region, rate) %>%
  spread(year, rate)

data <- list(file = filename,
             sheet = "7 CMD",
             sheettitle = "7. Child material deprivation",
             dfs = list(cmd_ahc, cmd_bhc),
             formats = c("pct", "pct"),
             totalrow = TRUE,
             titles = c("Children in combined low income and material deprivation (after housing costs)",
                        "Children in combined low income and material deprivation (before housing costs)"),
             subtitles = c("Proportion of children in each group who are in combined low income (below 70% of UK median income after housing costs) and material deprivation",
                           "Proportion of children in each group who are in combined low income (below 70% of UK median income before housing costs) and material deprivation"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset"
)

# Create new worksheet
createWideSpreadsheet(data)

# 8 Latest AHC -----------------------------------------------------------------

# Rel AHC
pp <- getpovby(hbai, pov = "low60ahc", by = "gvtregn") %>%
  group_by(groupingvar) %>%
  get3yrtable()

ch <- getpovby(hbai, pov = "low60ahc", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable()

wa <- getpovby(hbai, pov = "low60ahc", by = "gvtregn", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable()

pn <- getpovby(hbai, pov = "low60ahc", by = "gvtregn", weight = "gs_newpn") %>%
  group_by(groupingvar) %>%
  get3yrtable()

relahc <- rbind(pp, ch, wa, pn)  %>%
  filter(yearn == max(yearn)) %>%
  mutate(Region = ifelse(groupingvar == "All", "UK", groupingvar),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland")),
         group = case_when(weight == "gs_newpp" ~ "People",
                           weight == "gs_newch" ~ "Children",
                           weight == "gs_newwa" ~ "Working-age adults",
                           weight == "gs_newpn" ~ "Pensioners"),
         group = factor(group, levels = c("People", "Children",
                                           "Working-age adults", "Pensioners"))) %>%
  roundall() %>%
  select(Region, rate, group) %>%
  spread(group, rate)

# In-work AHC
df <- hbai %>%
  filter(low60ahc == 1,
         yearn >= max(yearn) - 2) %>%
  mutate(workinghh = factor(workinghh, levels = labels$workinghh$labels,
                            labels = labels$workinghh$codes))

ch <- getpovby(df, pov = "workinghh", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable()

wa <- getpovby(df, pov = "workinghh", by = "gvtregn", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable()

workahc <- rbind(ch, wa) %>%
  mutate(Region = ifelse(groupingvar == "All", "UK", groupingvar),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland")),
         group = case_when(weight == "gs_newpp" ~ "People",
                           weight == "gs_newch" ~ "Children",
                           weight == "gs_newwa" ~ "Working-age adults",
                           weight == "gs_newpn" ~ "Pensioners"),
         group = factor(group, levels = c("People", "Children",
                                          "Working-age adults", "Pensioners"))) %>%
  roundall() %>%
  select(Region, rate, group) %>%
  spread(group, rate) %>%
  mutate(People = NA,
         Pensioners = NA) %>%
  select(Region, People, Children, "Working-age adults", Pensioners)

# Abs AHC
pp <- getpovby(hbai, pov = "low60ahcabs", by = "gvtregn") %>%
  group_by(groupingvar) %>%
  get3yrtable()

ch <- getpovby(hbai, pov = "low60ahcabs", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable()

wa <- getpovby(hbai, pov = "low60ahcabs", by = "gvtregn", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable()

pn <- getpovby(hbai, pov = "low60ahcabs", by = "gvtregn", weight = "gs_newpn") %>%
  group_by(groupingvar) %>%
  get3yrtable()

absahc <- rbind(pp, ch, wa, pn)  %>%
  filter(yearn == max(yearn)) %>%
  mutate(Region = ifelse(groupingvar == "All", "UK", groupingvar),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland")),
         group = case_when(weight == "gs_newpp" ~ "People",
                           weight == "gs_newch" ~ "Children",
                           weight == "gs_newwa" ~ "Working-age adults",
                           weight == "gs_newpn" ~ "Pensioners"),
         group = factor(group, levels = c("People", "Children",
                                          "Working-age adults", "Pensioners"))) %>%
  roundall() %>%
  select(Region, rate, group) %>%
  spread(group, rate)

# Pers AHC

persahc <- persistent %>%
  filter(housingcosts == "AHC",
         Period == max(Period)) %>%
  mutate(group = case_when(group == "pp" ~ "People",
                           group == "ch" ~ "Children",
                           group == "wa" ~ "Working-age adults",
                           group == "pn" ~ "Pensioners"),
         group = factor(group, levels = c("People", "Children",
                                          "Working-age adults", "Pensioners")),
         Region = ifelse(nation == "Total", "UK", nation),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland"))) %>%
  select(Region, value, group) %>%
  spread(group, value)

# Child AHC

chrel <- getpovby(hbai, pov = "low60ahc", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  roundall() %>%
  filter(yearn == max(yearn)) %>%
  mutate(group = "Relative",
         Region = ifelse(groupingvar == "All", "UK", groupingvar),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                   "Wales", "Northern Ireland"))) %>%
  select(Region, group, rate)

chabs <- getpovby(hbai, pov = "low60ahcabs", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  roundall() %>%
  filter(yearn == max(yearn)) %>%
  mutate(group = "Absolute",
         Region = ifelse(groupingvar == "All", "UK", groupingvar),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland"))) %>%
  select(Region, group, rate)

chmd <- getpovby(hbai, pov = "cmdahc", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  roundall() %>%
  filter(yearn == max(yearn)) %>%
  mutate(group = "Low income & material deprivation",
         Region = ifelse(groupingvar == "All", "UK", groupingvar),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland"))) %>%
  select(Region, group, rate)

chpers <- persistent %>%
  filter(housingcosts == "AHC",
         group == "ch",
         Period == max(Period)) %>%
  mutate(Region = ifelse(nation == "Total", "UK", nation),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland")),
         rate = value) %>%
  select(Region, rate) %>%
  mutate(group = "Persistent")

chall <- rbind(chrel, chabs, chmd, chpers) %>%
  mutate(group = factor(group, levels = c("Relative", "Absolute",
                                          "Low income & material deprivation",
                                          "Persistent"))) %>%
  spread(group, rate)

data <- list(file = filename,
             sheet = "8 Latest AHC",
             sheettitle = "8. Latest estimates after housing costs",
             dfs = list(chall, relahc, workahc, absahc, persahc),
             formats = c("pct", "pct","pct","pct","pct"),
             totalrow = TRUE,
             titles = c("Child poverty target measures",
                        "Relative poverty",
                        "Share of those in poverty who live in working households",
                        "Absolute poverty",
                        "Persistent poverty"),
             subtitles = c("Three-year averages, 2017-20, except for persistent poverty, 2015-2019",
                           "Three-year averages, 2017-20",
                           "Three-year averages, 2017-20",
                           "Three-year averages, 2017-20",
                           "2015-19"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset"
)

# Create new worksheet
createWideSpreadsheet(data)

wb <- loadWorkbook(data$file)
addStyle(wb, sheet = data$sheet, style = createStyle(wrapText = TRUE),
         rows = 1:100, cols = 5, stack = TRUE)
addStyle(wb, sheet = data$sheet, style = createStyle(fgFill = "#dce6f1"),
         rows = c(10, 21, 32, 43, 54), cols = 2:6, stack = TRUE, gridExpand = TRUE)
saveWorkbook(wb, filename, overwrite = TRUE)

mark_missing(data, ncols = 1, nrows = 2, xlscol = 5, xlsrow = 8)
mark_missing(data, ncols = 1, nrows = 2, xlscol = 5, xlsrow = 11)
mark_missing(data, ncols = 1, nrows = 5, xlscol = 3, xlsrow = 30)
mark_missing(data, ncols = 1, nrows = 5, xlscol = 6, xlsrow = 30)

# 9 Latest BHC -----------------------------------------------------------------

# Rel bhc
pp <- getpovby(hbai, pov = "low60bhc", by = "gvtregn") %>%
  group_by(groupingvar) %>%
  get3yrtable()

ch <- getpovby(hbai, pov = "low60bhc", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable()

wa <- getpovby(hbai, pov = "low60bhc", by = "gvtregn", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable()

pn <- getpovby(hbai, pov = "low60bhc", by = "gvtregn", weight = "gs_newpn") %>%
  group_by(groupingvar) %>%
  get3yrtable()

relbhc <- rbind(pp, ch, wa, pn)  %>%
  filter(yearn == max(yearn)) %>%
  mutate(Region = ifelse(groupingvar == "All", "UK", groupingvar),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland")),
         group = case_when(weight == "gs_newpp" ~ "People",
                           weight == "gs_newch" ~ "Children",
                           weight == "gs_newwa" ~ "Working-age adults",
                           weight == "gs_newpn" ~ "Pensioners"),
         group = factor(group, levels = c("People", "Children",
                                          "Working-age adults", "Pensioners"))) %>%
  roundall() %>%
  select(Region, rate, group) %>%
  spread(group, rate)

# In-work bhc
df <- hbai %>%
  filter(low60bhc == 1,
         yearn >= max(yearn) - 2) %>%
  mutate(workinghh = factor(workinghh, levels = labels$workinghh$labels,
                            labels = labels$workinghh$codes))

ch <- getpovby(df, pov = "workinghh", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable()

wa <- getpovby(df, pov = "workinghh", by = "gvtregn", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable()

workbhc <- rbind(ch, wa) %>%
  mutate(Region = ifelse(groupingvar == "All", "UK", groupingvar),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland")),
         group = case_when(weight == "gs_newpp" ~ "People",
                           weight == "gs_newch" ~ "Children",
                           weight == "gs_newwa" ~ "Working-age adults",
                           weight == "gs_newpn" ~ "Pensioners"),
         group = factor(group, levels = c("People", "Children",
                                          "Working-age adults", "Pensioners"))) %>%
  roundall() %>%
  select(Region, rate, group) %>%
  spread(group, rate) %>%
  mutate(People = NA,
         Pensioners = NA) %>%
  select(Region, People, Children, "Working-age adults", Pensioners)

# Abs bhc
pp <- getpovby(hbai, pov = "low60bhcabs", by = "gvtregn") %>%
  group_by(groupingvar) %>%
  get3yrtable()

ch <- getpovby(hbai, pov = "low60bhcabs", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable()

wa <- getpovby(hbai, pov = "low60bhcabs", by = "gvtregn", weight = "gs_newwa") %>%
  group_by(groupingvar) %>%
  get3yrtable()

pn <- getpovby(hbai, pov = "low60bhcabs", by = "gvtregn", weight = "gs_newpn") %>%
  group_by(groupingvar) %>%
  get3yrtable()

absbhc <- rbind(pp, ch, wa, pn)  %>%
  filter(yearn == max(yearn)) %>%
  mutate(Region = ifelse(groupingvar == "All", "UK", groupingvar),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland")),
         group = case_when(weight == "gs_newpp" ~ "People",
                           weight == "gs_newch" ~ "Children",
                           weight == "gs_newwa" ~ "Working-age adults",
                           weight == "gs_newpn" ~ "Pensioners"),
         group = factor(group, levels = c("People", "Children",
                                          "Working-age adults", "Pensioners"))) %>%
  roundall() %>%
  select(Region, rate, group) %>%
  spread(group, rate)

# Pers bhc

persbhc <- persistent %>%
  filter(housingcosts == "BHC",
         Period == max(Period)) %>%
  mutate(group = case_when(group == "pp" ~ "People",
                           group == "ch" ~ "Children",
                           group == "wa" ~ "Working-age adults",
                           group == "pn" ~ "Pensioners"),
         group = factor(group, levels = c("People", "Children",
                                          "Working-age adults", "Pensioners")),
         Region = ifelse(nation == "Total", "UK", nation),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland"))) %>%
  select(Region, value, group) %>%
  spread(group, value)

# Child bhc

chrel <- getpovby(hbai, pov = "low60bhc", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  roundall() %>%
  filter(yearn == max(yearn)) %>%
  mutate(group = "Relative",
         Region = ifelse(groupingvar == "All", "UK", groupingvar),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland"))) %>%
  select(Region, group, rate)

chabs <- getpovby(hbai, pov = "low60bhcabs", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  roundall() %>%
  filter(yearn == max(yearn)) %>%
  mutate(group = "Absolute",
         Region = ifelse(groupingvar == "All", "UK", groupingvar),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland"))) %>%
  select(Region, group, rate)

chmd <- getpovby(hbai, pov = "cmdbhc", by = "gvtregn", weight = "gs_newch") %>%
  group_by(groupingvar) %>%
  get3yrtable() %>%
  roundall() %>%
  filter(yearn == max(yearn)) %>%
  mutate(group = "Low income & material deprivation",
         Region = ifelse(groupingvar == "All", "UK", groupingvar),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland"))) %>%
  select(Region, group, rate)

chpers <- persistent %>%
  filter(housingcosts == "BHC",
         group == "ch",
         Period == max(Period)) %>%
  mutate(Region = ifelse(nation == "Total", "UK", nation),
         Region = factor(Region, levels = c("UK", "England", "Scotland",
                                            "Wales", "Northern Ireland")),
         rate = value) %>%
  select(Region, rate) %>%
  mutate(group = "Persistent")

chall <- rbind(chrel, chabs, chmd, chpers) %>%
  mutate(group = factor(group, levels = c("Relative", "Absolute",
                                          "Low income & material deprivation",
                                          "Persistent"))) %>%
  spread(group, rate)

data <- list(file = filename,
             sheet = "9 Latest BHC",
             sheettitle = "9. Latest estimates before housing costs",
             dfs = list(chall, relbhc, workbhc, absbhc, persbhc),
             formats = c("pct", "pct","pct","pct","pct"),
             totalrow = TRUE,
             titles = c("Child poverty target measures",
                        "Relative poverty",
                        "Share of those on poverty who live in working households",
                        "Absolute poverty",
                        "Persistent poverty"),
             subtitles = c("Three-year averages, 2017-20, except for persistent poverty, 2015-2019",
                           "Three-year averages, 2017-20",
                           "Three-year averages, 2017-20",
                           "Three-year averages, 2017-20",
                           "2015-19"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset"
)

# Create new worksheet
createWideSpreadsheet(data)

wb <- loadWorkbook(data$file)
addStyle(wb, sheet = data$sheet, style = createStyle(wrapText = TRUE),
         rows = 1:100, cols = 5, stack = TRUE)
addStyle(wb, sheet = data$sheet, style = createStyle(fgFill = "#dce6f1"),
         rows = c(10, 21, 32, 43, 54), cols = 2:6, stack = TRUE, gridExpand = TRUE)
saveWorkbook(wb, filename, overwrite = TRUE)

mark_missing(data, ncols = 1, nrows = 5, xlscol = 3, xlsrow = 30)
mark_missing(data, ncols = 1, nrows = 5, xlscol = 6, xlsrow = 30)

# 10 Food security -------------------------------------------------------------

pp <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)") %>%
  group_by(gvtregn) %>%
  mutate(population = sum(gs_newpp)) %>%
  group_by(gvtregn, foodsec) %>%
  summarise(number = sum(gs_newpp),
            composition = roundpct(number / max(population)),
            group = "All people") %>%
  select(gvtregn, group, foodsec, composition)

pp_all <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)") %>%
  mutate(population = sum(gs_newpp)) %>%
  group_by(foodsec) %>%
  summarise(number = sum(gs_newpp),
            composition = roundpct(number / max(population)),
            group = "All people",
            gvtregn = "UK") %>%
  select(gvtregn, group, foodsec, composition)

pp_rel <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)",
         low60ahc == 1) %>%
  group_by(gvtregn) %>%
  mutate(population = sum(gs_newpp)) %>%
  group_by(gvtregn, foodsec) %>%
  summarise(number = sum(gs_newpp),
            composition = roundpct(number / max(population)),
            group = "People in relative poverty") %>%
  select(gvtregn, group, foodsec, composition)

pp_rel_all <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)",
         low60ahc == 1) %>%
  mutate(population = sum(gs_newpp)) %>%
  group_by(foodsec) %>%
  summarise(number = sum(gs_newpp),
            composition = roundpct(number / max(population)),
            group = "People in relative poverty",
            gvtregn = "UK") %>%
  select(gvtregn, group, foodsec, composition)

pp_abs <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)",
         low60ahcabs == 1) %>%
  group_by(gvtregn) %>%
  mutate(population = sum(gs_newpp)) %>%
  group_by(gvtregn, foodsec) %>%
  summarise(number = sum(gs_newpp),
            composition = roundpct(number / max(population)),
            group = "People in absolute poverty") %>%
  select(gvtregn, group, foodsec, composition)

pp_abs_all <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)",
         low60ahcabs == 1) %>%
  mutate(population = sum(gs_newpp)) %>%
  group_by(foodsec) %>%
  summarise(number = sum(gs_newpp),
            composition = roundpct(number / max(population)),
            group = "People in absolute poverty",
            gvtregn = "UK") %>%
  select(gvtregn, group, foodsec, composition)

ch <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)") %>%
  group_by(gvtregn) %>%
  mutate(population = sum(gs_newch)) %>%
  group_by(gvtregn, foodsec) %>%
  summarise(number = sum(gs_newch),
            composition = roundpct(number / max(population)),
            group = "All children") %>%
  select(gvtregn, group, foodsec, composition)

ch_all <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)") %>%
  mutate(population = sum(gs_newch)) %>%
  group_by(foodsec) %>%
  summarise(number = sum(gs_newch),
            composition = roundpct(number / max(population)),
            group = "All children",
            gvtregn = "UK") %>%
  select(gvtregn, group, foodsec, composition)

ch_rel <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)",
         low60ahc == 1) %>%
  group_by(gvtregn) %>%
  mutate(population = sum(gs_newch)) %>%
  group_by(gvtregn, foodsec) %>%
  summarise(number = sum(gs_newch),
            composition = roundpct(number / max(population)),
            group = "Children in relative poverty") %>%
  select(gvtregn, group, foodsec, composition)

ch_rel_all <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)",
         low60ahc == 1) %>%
  mutate(population = sum(gs_newch)) %>%
  group_by(foodsec) %>%
  summarise(number = sum(gs_newch),
            composition = roundpct(number / max(population)),
            group = "Children in relative poverty",
            gvtregn = "UK") %>%
  select(gvtregn, group, foodsec, composition)

ch_abs <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)",
         low60ahcabs == 1) %>%
  group_by(gvtregn) %>%
  mutate(population = sum(gs_newch)) %>%
  group_by(gvtregn, foodsec) %>%
  summarise(number = sum(gs_newch),
            composition = roundpct(number / max(population)),
            group = "Children in absolute poverty") %>%
  select(gvtregn, group, foodsec, composition)

ch_abs_all <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)",
         low60ahcabs == 1) %>%
  mutate(population = sum(gs_newch)) %>%
  group_by(foodsec) %>%
  summarise(number = sum(gs_newch),
            composition = roundpct(number / max(population)),
            group = "Children in absolute poverty",
            gvtregn = "UK") %>%
  select(gvtregn, group, foodsec, composition)

hh <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)",
         benunit == 1) %>%
  group_by(gvtregn) %>%
  mutate(population = sum(gs_newbu)) %>%
  group_by(gvtregn, foodsec) %>%
  summarise(number = sum(gs_newbu),
            composition = roundpct(number / max(population)),
            group = "All households") %>%
  select(gvtregn, group, foodsec, composition)

hh_all <- hbai %>%
  filter(yearn >= 26,
         foodsec != "(Missing)",
         benunit == 1) %>%
  mutate(population = sum(gs_newbu)) %>%
  group_by(foodsec) %>%
  summarise(number = sum(gs_newbu),
            composition = roundpct(number / max(population)),
            group = "All households",
            gvtregn = "UK") %>%
  select(gvtregn, group, foodsec, composition)

data <- bind_rows(pp, pp_all, pp_rel, pp_rel_all, pp_abs, pp_abs_all,
                  ch, ch_all, ch_rel, ch_rel_all, ch_abs, ch_abs_all,
                  hh, hh_all) %>%
  ungroup() %>%
  mutate(gvtregn = factor(gvtregn,
                          levels = c("UK", "England", "Scotland", "Wales",
                                     "Northern Ireland"),
                          ordered = TRUE)) %>%
  rename(Region = gvtregn)

table1 <- filter(data, group == "All people") %>%
  select(-group) %>%
  spread(foodsec, composition)

table2 <- filter(data, group == "People in relative poverty") %>%
  select(-group) %>%
  spread(foodsec, composition)

table3 <- filter(data, group == "People in absolute poverty") %>%
  select(-group) %>%
  spread(foodsec, composition)

table4 <- filter(data, group == "All children") %>%
  select(-group) %>%
  spread(foodsec, composition)

table5 <- filter(data, group == "Children in relative poverty") %>%
  select(-group) %>%
  spread(foodsec, composition)

table6 <- filter(data, group == "Children in absolute poverty") %>%
  select(-group) %>%
  spread(foodsec, composition)

table7 <- filter(data, group == "All households") %>%
  select(-group) %>%
  spread(foodsec, composition)

data <- list(file = filename,
             sheet = "10 Food security",
             sheettitle = "10. Household food security",
             dfs = list(table1, table2, table3, table4, table5, table6, table7),
             formats = c("pct", "pct","pct","pct","pct","pct", "pct"),
             totalrow = TRUE,
             titles = c("Household food security for all people",
                        "Household food security for people in relative poverty",
                        "Household food security for people in absolute poverty",
                        "Household food security for all children",
                        "Household food security for children in relative poverty",
                        "Household food security for children in absolute poverty",
                        "Household food security - all households"),
             subtitles = c("Composition of people by level of household food security, 2019/20",
                           "Composition of people in relative poverty after housing costs by level of household food security, 2019/20",
                           "Composition of people in absolute poverty after housing costs by level of household food security, 2019/20",
                           "Composition of children by level of household food security, 2019/20",
                           "Composition of children in relative poverty after housing costs by level of household food security, 2019/20",
                           "Composition of children in absolute poverty after housing costs by level of household food security, 2019/20",
                           "Composition of households by level of household food security, 2019/20"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset"
)

# Create new worksheet
createWideSpreadsheet(data)

wb <- loadWorkbook(data$file)
addStyle(wb, sheet = data$sheet, style = createStyle(fgFill = "#dce6f1"),
         rows = c(10, 21, 32, 43, 54, 65, 74), cols = 2:6, stack = TRUE, gridExpand = TRUE)
saveWorkbook(wb, filename, overwrite = TRUE)

# TOC --------------------------------------------------------------------------

headings <- list(location = c(0, 4, 8, 10),
                 titles = c("Headline poverty measures - after housing costs",
                            "Headline poverty measures - before housing costs",
                            "Material deprivation",
                            "Latest estimates"))

createContentSheet(filename, headings)

rm(list = ls())
