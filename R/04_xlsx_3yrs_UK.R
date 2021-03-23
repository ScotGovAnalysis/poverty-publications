
# Create spreadsheet for 3-year averaged data

source("R/00_functions.R", encoding = "UTF-8")
source("R/00_strings.R")

filename <- "output/UK comparisons.xlsx"

hbai <- readRDS("data/tidyhbai.rds")

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

# 8 Food security --------------------------------------------------------------

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
            gvtregn = "All") %>%
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
            gvtregn = "All") %>%
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
            gvtregn = "All") %>%
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
            gvtregn = "All") %>%
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
            gvtregn = "All") %>%
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
            gvtregn = "All") %>%
  select(gvtregn, group, foodsec, composition)

data <- bind_rows(pp, pp_all, pp_rel, pp_rel_all, pp_abs, pp_abs_all,
                  ch, ch_all, ch_rel, ch_rel_all, ch_abs, ch_abs_all) %>%
  ungroup() %>%
  mutate(gvtregn = factor(gvtregn,
                          levels = c("All", "England", "Scotland", "Wales",
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

data <- list(file = filename,
             sheet = "8 Food security",
             sheettitle = "8. Household food security",
             dfs = list(table1, table2, table3, table4, table5, table6),
             formats = c("pct", "pct","pct","pct","pct","pct"),
             totalrow = TRUE,
             titles = c("Household food security for all people",
                        "Household food security for people in relative poverty",
                        "Household food security for people in absolute poverty",
                        "Household food security for all children",
                        "Household food security for children in relative poverty",
                        "Household food security for children in absolute poverty"),
             subtitles = c("Composition of people by level of household food security, 2019/20",
                           "Composition of people in relative poverty after housing costs by level of household food security, 2019/20",
                           "Composition of people in absolute poverty after housing costs by level of household food security, 2019/20",
                           "Composition of children by level of household food security, 2019/20",
                           "Composition of children in relative poverty after housing costs by level of household food security, 2019/20",
                           "Composition of children in absolute poverty after housing costs by level of household food security, 2019/20"),
             source = "Source: Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset"
)

# Create new worksheet
createWideSpreadsheet(data)

# TOC --------------------------------------------------------------------------

headings <- list(location = c(0, 4, 8, 10),
                 titles = c("Headline poverty measures - after housing costs",
                            "Headline poverty measures - before housing costs",
                            "Material deprivation",
                            "Household food security"))

createContentSheet(filename, headings)

rm(list = ls())
