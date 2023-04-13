# note that highcharts objects contain the whole dataframe - i.e. remove any
# variables that shouldn't be in the public domain before creating the chart

# load packages and data -------------------------------------------------------
library(tidyverse)
library(highcharter)

source("R/00_functions.R", encoding = "UTF-8")
source("R/00_strings.R")
source("R/00_colours.R")

alldata <- readRDS("data/tables.rds")
alldata_1yr <- readRDS("data/tables_1yr.rds")
CIs <- readRDS("data/indicative_CIs.rds")
persistent_priority <- readRDS("data/persistentpriority.rds") %>%
  filter(Period == max(Period))

# lists for saving charts and tables in
charts <- list()
chart_tables <- list()

# get vectors with formatted years and (3-yr / 5-yr) periods
years <- levels(labels$years$formatted)
periods <- levels(labels$years$periods)
period5yr <- levels(labels$years$period5yr)

# set 1,000s comma separator
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

# 0 key trend charts -----------------------------------------------------------

# * ch -------------------------------------------------------------------------

df <- alldata$relAHC$rates %>%
  filter(Group == "Children") %>%
  pivot_longer(cols = periods, names_to = "x", values_to = "y") %>%
  mutate(label = fmtpct(y),
         x = factor(x, ordered = TRUE))

ch_pov <- small_plot(df)

ggplot2::ggsave("website/img/chart0a.png", width = 8.3, height = 5.5, units = "cm", dpi = 500)

# * wa -------------------------------------------------------------------------

df <- alldata$relAHC$rates %>%
  filter(Group == "Working-age adults") %>%
  pivot_longer(cols = periods, names_to = "x", values_to = "y") %>%
  mutate(label = fmtpct(y),
         x = factor(x, ordered = TRUE))

wa_pov <- small_plot(df)

ggplot2::ggsave("website/img/chart0b.png", width = 8.3, height = 5.5, units = "cm", dpi = 500)

# * pn -------------------------------------------------------------------------

df <- alldata$relAHC$rates %>%
  filter(Group == "Pensioners") %>%
  pivot_longer(cols = periods, names_to = "x", values_to = "y") %>%
  mutate(label = fmtpct(y),
         x = factor(x, ordered = TRUE))

pn_pov <- small_plot(df)

ggplot2::ggsave("website/img/chart0c.png", width = 8.3, height = 5.5, units = "cm", dpi = 500)

# 1 all people -----------------------------------------------------------------

# * - pp rel -------------------------------------------------------------------

data <- rbind(alldata$relAHC$rates %>%
                filter(Group == "All people") %>%
                mutate(key = "After housing costs") %>%
                select(-Group),
              alldata$relBHC$rates %>%
                filter(Group == "All people") %>%
                mutate(key = "Before housing costs") %>%
                select(-Group)) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%

  # add CI data
  left_join(CIs$pp_rel, by = c("key", "x" = "period")) %>%

  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$pp_rel <- hc_line(data) %>%
  add_recessionbar() %>%
  add_CIs(data = data, colors = SGmix6_cat[1:2])

chart_tables$pp_rel <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pp_samples <- alldata$relAHC$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group == "All people")

# * - pp abs -------------------------------------------------------------------
data <- rbind(alldata$absAHC$rates %>%
                filter(Group == "All people") %>%
                mutate(key = "After housing costs") %>%
                select(-Group),
              alldata$absBHC$rates %>%
                filter(Group == "All people") %>%
                mutate(key = "Before housing costs") %>%
                select(-Group)) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%

  # add CI data
  left_join(CIs$pp_abs, by = c("key", "x" = "period")) %>%

  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$pp_abs <- hc_line(data) %>%
  add_recessionbar() %>%
  add_CIs(data = data, colors = SGmix6_cat[1:2])

chart_tables$pp_abs <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

# * - pp foodsec ---------------------------------------------------------------

data <- rbind(alldata$foodsec$comps %>%
                filter(Group == "People") %>%
                mutate(Group = "All people"),
              alldata$foodsec_pov_pp$rel_comp %>%
                filter(Group == "People") %>%
                mutate(Group = "In relative poverty"),
              alldata$foodsec_pov_pp$sev_comp %>%
                filter(Group == "People") %>%
                mutate(Group = "In severe poverty")) %>%
  pivot_longer(cols = c("High", "Marginal", "Low", "Very low"),
               names_to = "key", values_to = "y") %>%
  rename(x = Group) %>%
  mutate(key = factor(key, levels = c("High", "Marginal", "Low", "Very low")))

charts$pp_foodsec <- hc_stackedbar(data)


chart_tables$pp_foodsec <- rbind(alldata$foodsec$comps %>%
                                   filter(Group == "People") %>%
                                   mutate(Group = "All people"),
                                 alldata$foodsec_pov_pp$rel_comp %>%
                                   filter(Group == "People") %>%
                                   mutate(Group = "In relative poverty"),
                                 alldata$foodsec_pov_pp$sev_comp %>%
                                   filter(Group == "People") %>%
                                   mutate(Group = "In severe poverty"))  %>%
  mutate_if(is.numeric, fmtpct)

chart_tables$pp_foodsec_samples <- rbind(alldata$foodsec$sample %>%
                                           filter(Group == "People") %>%
                                           mutate(Group = "All people"),
                                         alldata$foodsec_pov_pp$rel_sample %>%
                                           filter(Group == "People") %>%
                                           mutate(Group = "In relative poverty"),
                                         alldata$foodsec_pov_pp$sev_sample %>%
                                           filter(Group == "People") %>%
                                           mutate(Group = "In severe poverty")) %>%
  mutate_if(is.numeric, comma, 1)

# 2 children -------------------------------------------------------------------

# * - ch rel -------------------------------------------------------------------
data <- rbind(alldata$relAHC$rates %>%
                filter(Group == "Children") %>%
                mutate(key = "After housing costs") %>%
                select(-Group),
              alldata$relBHC$rates %>%
                filter(Group == "Children") %>%
                mutate(key = "Before housing costs") %>%
                select(-Group)) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%

  # add CI data
  left_join(CIs$ch_rel, by = c("key", "x" = "period")) %>%
  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$ch_rel <- data %>%
  hc_line() %>%
  add_CIs(data = data, colors = SGmix6_cat[1:2]) %>%
  add_recessionbar()

chart_tables$ch_rel <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$ch_samples <- alldata$relAHC$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group == "Children")

# * - ch work ------------------------------------------------------------------

data <- alldata$relAHC$rates %>%
  filter(Group == "Children") %>%
  rbind(alldata$workinghh_ch$rel_comps %>%
          filter(Group != "All") %>%
          mutate("1994-97" = NA,
                 "1995-98" = NA)) %>%
  mutate(key = "After housing costs") %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  pivot_wider(names_from = Group, values_from = y) %>%
  rename(y = "Children",
         none = "No one in paid work",
         some = "Someone in paid work") %>%
  mutate(mid = y * some,
         x = factor(x, ordered = TRUE),
         key = "In relative poverty")

charts$ch_work <- hc_line(data) %>%
  highcharter::hc_plotOptions(spline = list(zIndex = 100))  %>%
  highcharter::hc_add_dependency(., name = "modules/pattern-fill.js") %>%
  highcharter::hc_add_series(filter(data, x >= "1996-99"),
                name = "In workless households",
                "arearange",
                hcaes(x = "x",
                      low = "mid",
                      high = "y"),
                marker = list(enabled = FALSE),
                fillOpacity = 0.3,
                color = list(pattern = list(
                  path = list(d = 'M 0 0 L 0 10 M 2 0 L 2 10 M 4 0 L 4 10',
                              strokeWidth = 3),
                  width = 10,
                  height = 10,
                  opacity = 0.3,
                  color = SGorange)),
                # turn tooltip off
                enableMouseTracking = FALSE) %>%
  highcharter::hc_add_series(data,
                name = "In working households",
                "areaspline",
                hcaes(x = "x",
                      y = "mid"),
                marker = list(enabled = FALSE),
                color = list(pattern = list(
                  path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11',
                              strokeWidth = 3),
                  width = 10,
                  height = 10,
                  opacity = 0.3,
                  color = SGblue)),
                lineColor = SGmidgrey,
                # turn tooltip off
                enableMouseTracking = FALSE) %>%

  highcharter::hc_yAxis(min = 0) %>%
  highcharter::hc_tooltip(formatter = JS('function () {

                    return "<strong>" + this.point.name + "</strong><br>" +
                    this.series.name + ": " +
                    Highcharts.numberFormat(this.y*100, 0) + "%<br>" +
                    " - in workless households: "  +
                    Highcharts.numberFormat(this.point.none*100, 0) +
                    "%<br> - in working households: " +
                    Highcharts.numberFormat(this.point.some*100, 0) + "%"

                }')) %>%
  highcharter::hc_size(width = 680, height = 400)

charts$ch_work2 <- hc_line(data %>% mutate(y = some, key = "In working households") %>% select(key, x, y)) %>%
  highcharter::hc_add_dependency(., name = "modules/pattern-fill.js") %>%
  highcharter::hc_add_series(data,
                             "areaspline",
                             hcaes(x = x,
                                   y = some),
                             marker = list(enabled = FALSE),
                             color = list(pattern = list(
                               path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11',
                                           strokeWidth = 3),
                               width = 10,
                               height = 10,
                               opacity = 0.3,
                               color = SGblue)),
                             showInLegend = FALSE,
                             enableMouseTracking = FALSE) %>%

  highcharter::hc_yAxis(min = 0) %>%
  highcharter::hc_size(width = 680, height = 400)

chart_tables$ch_work <- data %>%
  filter(x >= "1996-99") %>%
  select(x, none, some) %>%
  pivot_longer(cols = c("none", "some"), names_to = "Group", values_to = "y") %>%
  mutate(Group = case_when(Group == "none" ~ "In workless households",
                           Group == "some" ~ "In working households"),
         y = fmtpct(y)) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$ch_work_samples <- alldata$relAHC$sample %>%
  select(names(chart_tables$ch_work)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group == "Children")

# * - ch abs ----------------------------------------------------------------------
data <- rbind(alldata$absAHC$rates %>%
                filter(Group == "Children") %>%
                mutate(key = "After housing costs") %>%
                select(-Group),
              alldata$absBHC$rates %>%
                filter(Group == "Children") %>%
                mutate(key = "Before housing costs") %>%
                select(-Group)) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x")  %>%

  # add CI data
  left_join(CIs$ch_abs, by = c("key", "x" = "period")) %>%

  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$ch_abs <- data %>%
  hc_line() %>%
  add_recessionbar() %>%
  add_CIs(data = data, colors = SGmix6_cat[1:2])

chart_tables$ch_abs <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)


# * - ch dep ----------------------------------------------------------------------
data <- alldata$cmd$rates %>%
  pivot_longer(cols = periods[periods >= "2004-07"],
               values_to = "y", names_to = "x") %>%
  filter(y != 99992,
         x != "2009-12") %>%
  mutate(key = case_when(Measure %in% c("New measure, after housing costs",
                                        "Old measure, after housing costs") ~
                           "After housing costs",
                         TRUE ~ "Before housing costs")) %>%

  # add CI data
  left_join(CIs$ch_dep, by = c("Measure", "key", "x" = "period")) %>%
  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened) %>%
  arrange(key, x)

charts$ch_dep <- data %>%
  complete_ts() %>%
  hc_line(covid = TRUE) %>%
  add_cmdbar() %>%
  add_CIs(data, colors = c(SGmix6_cat[1:2])) %>%

  # mark covid estimates
  hc_plotOptions(
    spline = list(
      zoneAxis = "x",
      zones = list(
        list(value = 23.1,
             dashStyle = "Solid"),
        list(value = 26,
             dashStyle = "Dot",
             color = SGdarkgrey)
      )))

chart_tables$ch_dep <- data %>%
  select(x, y, Measure) %>%
  mutate(y = fmtpct(y)) %>%
  pivot_wider(names_from = x, values_from = y) %>%
  mutate_all( ~ replace(., is.na(.), "--"))

chart_tables$ch_dep_samples <- alldata$cmd$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  mutate_all( ~ replace(., . == "99,992", "--")) %>%
  mutate_all( ~ replace(., . == "99,991", "--"))

# * - ch foodsec ---------------------------------------------------------------

data <- rbind(alldata$foodsec$comps %>%
                filter(Group == "Children") %>%
                mutate(Group = "All children"),
              alldata$foodsec_pov_pp$rel_comp %>%
                filter(Group == "Children") %>%
                mutate(Group = "In relative poverty"),
              alldata$foodsec_pov_pp$sev_comp %>%
                filter(Group == "Children") %>%
                mutate(Group = "In severe poverty")) %>%
  pivot_longer(cols = c("High", "Marginal", "Low", "Very low"),
               names_to = "key", values_to = "y") %>%
  rename(x = Group) %>%
  mutate(key = factor(key, levels = c("High", "Marginal", "Low", "Very low")))

charts$ch_foodsec <- hc_stackedbar(data)

chart_tables$ch_foodsec <- rbind(alldata$foodsec$comps %>%
                                   filter(Group == "Children") %>%
                                   mutate(Group = "All children"),
                                 alldata$foodsec_pov_pp$rel_comp %>%
                                   filter(Group == "Children") %>%
                                   mutate(Group = "In relative poverty"),
                                 alldata$foodsec_pov_pp$sev_comp %>%
                                   filter(Group == "Children") %>%
                                   mutate(Group = "In severe poverty")) %>%
  mutate_if(is.numeric, fmtpct)

chart_tables$ch_foodsec_samples <- rbind(alldata$foodsec$sample %>%
                                           filter(Group == "Children") %>%
                                           mutate(Group = "All children"),
                                         alldata$foodsec_pov_pp$rel_sample %>%
                                           filter(Group == "Children") %>%
                                           mutate(Group = "In relative poverty"),
                                         alldata$foodsec_pov_pp$sev_sample %>%
                                           filter(Group == "Children") %>%
                                           mutate(Group = "In severe poverty")) %>%
  mutate_if(is.numeric, comma, 1)

# * - ch priority groups x 4 --------------------------------------------------

# rel

data <- alldata$priority$rel %>%
  rename(x = Group,
         y = Rate)

charts$ch_pri_rel <- hc_bar(data)

chart_tables$ch_pri_rel <- data %>%
  rename(Group = x,
         Rate = y) %>%
  mutate_if(is.numeric, fmtpct) %>%
  mutate(Rate = ifelse(grepl("%", Rate), Rate, "--"))

chart_tables$ch_pri_samples <- alldata$priority$sample %>%
  mutate_if(is.numeric, comma, 1)

# abs

data <- alldata$priority$abs %>%
  rename(x = Group,
         y = Rate)

charts$ch_pri_abs <- hc_bar(data)

chart_tables$ch_pri_abs <- data %>%
  rename(Group = x,
         Rate = y) %>%
  mutate_if(is.numeric, fmtpct) %>%
  mutate(Rate = ifelse(grepl("%", Rate), Rate, "--"))

# cmd

data <- alldata$priority$cmd %>%
  rename(x = Group,
         y = Rate)

charts$ch_pri_dep <- hc_bar(data)

chart_tables$ch_pri_dep <- data %>%
  rename(Group = x,
         Rate = y) %>%
  mutate_if(is.numeric, fmtpct) %>%
  mutate(Rate = ifelse(grepl("%", Rate), Rate, "--"))

# per

data1 <- persistent_priority %>%
  filter(Group %in% c("Mixed/ multiple ethnic groups", "Asian/ Asian British",
                      "Black/ African/ Caribbean/ Black British",
                      "Other ethnic group")) %>%
  summarise(Group = "Minority ethnic family",
            Rate = NA,
            Sample = sum(Sample))

data2 <- persistent_priority %>%
  filter(!Group %in% c("Mixed/ multiple ethnic groups", "Asian/ Asian British",
                      "Black/ African/ Caribbean/ Black British",
                      "Other ethnic group")) %>%
  select(-Period)

data <- rbind(data1, data2) %>%
  mutate(Group = factor(Group,
                        levels = c("Total",
                                   "Three or more children",
                                   "At least one disabled adult",
                                   "0 - 4",
                                   "Minority ethnic family",
                                   "Lone parent family",
                                   "Yes, mother under 25"),
                        labels = c("All children",
                                   "3 or more children in the family",
                                   "Disabled adult(s) in family",
                                   "Youngest child in the family is under 5",
                                   "Minority ethnic family",
                                   "Single parent family",
                                   "Mother under 25"),
                        ordered = TRUE),
         Rate = round2(Rate, 2)) %>%
  arrange(Group)

charts$ch_pri_per <- hc_bar(data %>% rename(x = Group, y = Rate))

chart_tables$ch_pri_per <- data %>%
  select(Group, Rate) %>%
  mutate_if(is.numeric, fmtpct) %>%
  mutate(Rate = ifelse(grepl("%", Rate), Rate, "--"))

chart_tables$ch_pri_per_sample <- data %>%
  select(Group, Sample)


# 3 working-age ----------------------------------------------------------------

# * - wa rel -------------------------------------------------------------------
data <- rbind(alldata$relAHC$rates %>%
                filter(Group == "Working-age adults") %>%
                mutate(key = "After housing costs") %>%
                select(-Group),
              alldata$relBHC$rates %>%
                filter(Group == "Working-age adults") %>%
                mutate(key = "Before housing costs") %>%
                select(-Group)) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x")  %>%

  # add CI data
  left_join(CIs$wa_rel, by = c("key", "x" = "period")) %>%

  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$wa_rel <- hc_line(data) %>%
  add_recessionbar() %>%
  add_CIs(data = data, colors = SGmix6_cat[1:2])

chart_tables$wa_rel <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$wa_samples <- alldata$relAHC$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group == "Working-age adults")

# * - wa work ------------------------------------------------------------------

data <- alldata$relAHC$rates %>%
  filter(Group == "Working-age adults") %>%
  rbind(alldata$workinghh_wa$rel_comps %>%
          filter(Group != "All") %>%
          mutate("1994-97" = NA,
                 "1995-98" = NA)) %>%
  mutate(key = "After housing costs") %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  pivot_wider(names_from = Group, values_from = y) %>%
  rename(y = "Working-age adults",
         none = "No one in paid work",
         some = "Someone in paid work") %>%
  mutate(mid = y * some,
         x = factor(x, ordered = TRUE),
         key = "In relative poverty")


charts$wa_work <- hc_line(data) %>%
  highcharter::hc_plotOptions(spline = list(zIndex = 4)) %>%
  highcharter::hc_add_dependency(., name = "modules/pattern-fill.js") %>%
  highcharter::hc_add_series(filter(data, x >= "1996-99"),
                             name = "In workless households",
                             "arearange",
                             hcaes(x = "x",
                                   low = "mid",
                                   high = "y"),
                             marker = list(enabled = FALSE),
                             fillOpacity = 0.3,
                             color = list(pattern = list(
                               path = list(d = 'M 0 0 L 0 10 M 2 0 L 2 10 M 4 0 L 4 10',
                                           strokeWidth = 3),
                               width = 10,
                               height = 10,
                               opacity = 0.3,
                               color = SGorange)),
                             # turn tooltip off
                             enableMouseTracking = FALSE) %>%
  highcharter::hc_add_series(data,
                             name = "In working households",
                             "areaspline",
                             hcaes(x = "x",
                                   y = "mid"),
                             marker = list(enabled = FALSE),
                             fillOpacity = 0.3,
                             color = list(pattern = list(
                               path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11',
                                           strokeWidth = 3),
                               width = 10,
                               height = 10,
                               opacity = 0.3,
                               color = SGblue)),
                             lineColor = SGmidgrey,
                             # turn tooltip off
                             enableMouseTracking = FALSE) %>%

  highcharter::hc_yAxis(min = 0) %>%
  highcharter::hc_tooltip(formatter = JS('function () {

                    return "<strong>" + this.point.name + "</strong><br>" +
                    this.series.name + ": " +
                    Highcharts.numberFormat(this.y*100, 0) +
                    "%<br> - in workless households: "  +
                    Highcharts.numberFormat(this.point.none*100, 0)  +
                    "%<br> - in working households: " +
                    Highcharts.numberFormat(this.point.some*100, 0) + "%"

                }')) %>%
  highcharter::hc_size(width = 680, height = 400)

charts$wa_work2 <- hc_line(data %>% mutate(y = some, key = "In working households") %>% select(key, x, y)) %>%
  highcharter::hc_add_dependency(., name = "modules/pattern-fill.js") %>%
  highcharter::hc_add_series(data,
                             "areaspline",
                             hcaes(x = x,
                                   y = some),
                             marker = list(enabled = FALSE),
                             color = list(pattern = list(
                               path = list(d = 'M 0 0 L 10 10 M 9 -1 L 11 1 M -1 9 L 1 11',
                                           strokeWidth = 3),
                               width = 10,
                               height = 10,
                               opacity = 0.3,
                               color = SGblue)),
                             showInLegend = FALSE,
                             enableMouseTracking = FALSE) %>%

  highcharter::hc_yAxis(min = 0) %>%
  highcharter::hc_size(width = 680, height = 400)


chart_tables$wa_work <- data %>%
  filter(x >= "1996-99") %>%
  select(x, none, some) %>%
  pivot_longer(cols = c("none", "some"), names_to = "Group", values_to = "y") %>%
  mutate(Group = case_when(Group == "none" ~ "In workless households",
                           Group == "some" ~ "In working households"),
         y = fmtpct(y)) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$wa_work_samples <- alldata$relAHC$sample %>%
  select(names(chart_tables$wa_work)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group == "Working-age adults")

# * - wa abs -------------------------------------------------------------------

data <- rbind(alldata$absAHC$rates %>%
                filter(Group == "Working-age adults") %>%
                mutate(key = "After housing costs") %>%
                select(-Group),
              alldata$absBHC$rates %>%
                filter(Group == "Working-age adults") %>%
                mutate(key = "Before housing costs") %>%
                select(-Group)) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%

  # add CI data
  left_join(CIs$wa_abs, by = c("key", "x" = "period")) %>%

  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$wa_abs <- hc_line(data) %>%
  add_recessionbar() %>%
  add_CIs(data = data, colors = SGmix6_cat[1:2])

chart_tables$wa_abs <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

# * - wa foodsec ---------------------------------------------------------------

data <- rbind(alldata$foodsec$comps %>%
                filter(Group == "Working-age adults") %>%
                mutate(Group = "All working-age adults"),
              alldata$foodsec_pov_pp$rel_comp %>%
                filter(Group == "Working-age adults") %>%
                mutate(Group = "In relative poverty"),
              alldata$foodsec_pov_pp$sev_comp %>%
                filter(Group == "Working-age adults") %>%
                mutate(Group = "In severe poverty")) %>%
  pivot_longer(cols = c("High", "Marginal", "Low", "Very low"),
               names_to = "key", values_to = "y") %>%
  rename(x = Group) %>%
  mutate(key = factor(key, levels = c("High", "Marginal", "Low", "Very low")))

charts$wa_foodsec <- hc_stackedbar(data)

chart_tables$wa_foodsec <- rbind(alldata$foodsec$comps %>%
                                   filter(Group == "Working-age adults") %>%
                                   mutate(Group = "All working-age adults"),
                                 alldata$foodsec_pov_pp$rel_comp %>%
                                   filter(Group == "Working-age adults") %>%
                                   mutate(Group = "In relative poverty"),
                                 alldata$foodsec_pov_pp$sev_comp %>%
                                   filter(Group == "Working-age adults") %>%
                                   mutate(Group = "In severe poverty")) %>%
  mutate_if(is.numeric, fmtpct)

chart_tables$wa_foodsec_samples <- rbind(alldata$foodsec$sample %>%
                                           filter(Group == "Working-age adults") %>%
                                           mutate(Group = "All working-age adults"),
                                         alldata$foodsec_pov_pp$rel_sample %>%
                                           filter(Group == "Working-age adults") %>%
                                           mutate(Group = "In relative poverty"),
                                         alldata$foodsec_pov_pp$sev_sample %>%
                                           filter(Group == "Working-age adults") %>%
                                           mutate(Group = "In severe poverty")) %>%
  mutate_if(is.numeric, comma, 1)

# 4 pensioners -----------------------------------------------------------------

# * - pn rel -------------------------------------------------------------------

data <- rbind(alldata$relAHC$rates %>%
                filter(Group == "Pensioners") %>%
                mutate(key = "After housing costs") %>%
                select(-Group),
              alldata$relBHC$rates %>%
                filter(Group == "Pensioners") %>%
                mutate(key = "Before housing costs") %>%
                select(-Group)) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x")  %>%

  # add CI data
  left_join(CIs$pn_rel, by = c("key", "x" = "period")) %>%

  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$pn_rel <- hc_line(data) %>%
  add_recessionbar() %>%
  add_CIs(data = data, colors = SGmix6_cat[1:2])

chart_tables$pn_rel <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pn_samples <- alldata$relAHC$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group == "Pensioners")

# * - pn abs -------------------------------------------------------------------
data <- rbind(alldata$absAHC$rates %>%
                filter(Group == "Pensioners") %>%
                mutate(key = "After housing costs") %>%
                select(-Group),
              alldata$absBHC$rates %>%
                filter(Group == "Pensioners") %>%
                mutate(key = "Before housing costs") %>%
                select(-Group)) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x")  %>%

  # add CI data
  left_join(CIs$pn_abs, by = c("key", "x" = "period")) %>%

  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$pn_abs <- hc_line(data) %>%
  add_recessionbar() %>%
  add_CIs(data = data, colors = SGmix6_cat[1:2])

chart_tables$pn_abs <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

# * - pn dep -------------------------------------------------------------------

data <- alldata$pmd$rates %>%
  pivot_longer(cols = periods[periods >= "2009-12"],
               values_to = "y", names_to = "x") %>%
  mutate(key = "Pensioners") %>%

  # add CI data
  left_join(CIs$pn_dep, by = c("key", "x" = "period")) %>%

  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened,
         key =  "Pensioners (65+)")

charts$pn_dep <- data %>%
  complete_ts() %>%
  hc_line(covid = TRUE) %>%
  add_CIs(data = data, colors = SGmix6_cat[1]) %>%
  hc_xAxis(plotBands = list(list(from = 23.8,
                                 to = 25.2,
                                 color = SGlightgrey,
                                 borderColor = SGmidgrey,
                                 borderWidth = 2,
                                 label = list(
                                   text = "Responses affected<br>by the pandemic",
                                   rotation = 0,
                                   x = -10,
                                   y = 16,
                                   align = "left",
                                   textAlign = "right")))) %>%
  # mark covid estimates
  hc_plotOptions(
    spline = list(
      zoneAxis = "x",
      zones = list(
        list(value = 23.1,
             dashStyle = "Solid"),
        list(value = 26,
             dashStyle = "Dot",
             color = SGdarkgrey)
      )))

chart_tables$pn_dep <- data %>%
  select(x, y) %>%
  mutate(y = fmtpct(y),
         Group = "Pensioners aged 65 and over") %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pn_dep_samples <- alldata$pmd$sample %>%
  mutate_if(is.numeric, comma, 1)

# * - pn foodsec ---------------------------------------------------------------

data <- rbind(alldata$foodsec$comps %>%
                filter(Group == "Pensioners") %>%
                mutate(Group = "All pensioners"),
              alldata$foodsec_pov_pp$rel_comp %>%
                filter(Group == "Pensioners") %>%
                mutate(Group = "In relative poverty"),
              alldata$foodsec_pov_pp$sev_comp %>%
                filter(Group == "Pensioners") %>%
                mutate(Group = "In severe poverty")) %>%
  pivot_longer(cols = c("High", "Marginal", "Low", "Very low"),
               names_to = "key", values_to = "y") %>%
  rename(x = Group) %>%
  mutate(key = factor(key, levels = c("High", "Marginal", "Low", "Very low")),
         y = ifelse(y == 99993, NA, y))

charts$pn_foodsec <- hc_stackedbar(data)

chart_tables$pn_foodsec <- rbind(alldata$foodsec$comps %>%
                                   filter(Group == "Pensioners") %>%
                                   mutate(Group = "All pensioners"),
                                 alldata$foodsec_pov_pp$rel_comp %>%
                                   filter(Group == "Pensioners") %>%
                                   mutate(Group = "In relative poverty"),
                                 alldata$foodsec_pov_pp$sev_comp %>%
                                   filter(Group == "Pensioners") %>%
                                   mutate(Group = "In severe poverty")) %>%
  mutate_if(is.numeric, fmtpct) %>%
  mutate_all( ~ replace(., is.na(.), "--")) %>%
  mutate_all( ~ replace(., . == "9999300%", "--"))

chart_tables$pn_foodsec_samples <- rbind(alldata$foodsec$sample %>%
                                           filter(Group == "Pensioners") %>%
                                           mutate(Group = "All pensioners"),
                                         alldata$foodsec_pov_pp$rel_sample %>%
                                           filter(Group == "Pensioners") %>%
                                           mutate(Group = "In relative poverty"),
                                         alldata$foodsec_pov_pp$sev_sample %>%
                                           filter(Group == "Pensioners") %>%
                                           mutate(Group = "In severe poverty")) %>%
  mutate_if(is.numeric, comma, 1)

# 5 equality -------------------------------------------------------------------

# * - wa age ----------------------------------------------------------------------
# new: replace adult age breakdown with head age breakdown and more categories
# too many missing values in adult age variable, but none in head age variable
# split into two charts: working age and older age

data <- alldata$ageband_pp$rel_rates %>%
  filter(Group != "All") %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  mutate(key = Group,
         key = fct_drop(key),
         key = as.character(key)) %>%

  # add CI data
  left_join(CIs$age, by = c("key", "x" = "period")) %>%

  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

# wa age

data1 <- data %>%
  filter(key %in% c("16-24", "25-34", "35-44", "45-54", "55-64"))

charts$wa_age <- data1 %>%
  hc_line() %>%
  add_CIs(data = data1, colors = SGmix6_cat[1:5])

chart_tables$wa_age <- data1 %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$wa_age_samples <- alldata$ageband_pp$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group %in% c("16-24", "25-34", "35-44", "45-54", "55-64"))

# * - pn age -------------------------------------------------------------------

data2 <- data %>%
  filter(key %in% c("65-74", "75-84", "85+"))

charts$pn_age <- data2 %>%
  hc_line() %>%
  add_CIs(data = data2, colors = SGmix6_cat[1:3])

chart_tables$pn_age <- data2 %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pn_age_samples <- alldata$ageband_pp$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group %in% c("65-74", "75-84", "85+"))

# * - wa gender ----------------------------------------------------------------
data <- alldata$singlehh_ad$rel_rates %>%
  filter(Group %in% c("Female working-age adult with dependent children",
                    "Female working-age adult, no dependent children",
                    "Male working-age adult with dependent children",
                    "Male working-age adult, no dependent children")) %>%

  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  rename(key = Group) %>%

  # add CI data
  left_join(CIs$gender %>% mutate(key = as.character(key)), by = c("key", "x" = "period")) %>%
  mutate(key = case_when(key == "Female working-age adult with dependent children"
                         ~ "Single mother",
                         key == "Female working-age adult, no dependent children"
                         ~ "Single woman, no children",
                         key == "Male working-age adult with dependent children"
                         ~ "Single father",
                         key == "Male working-age adult, no dependent children"
                         ~ "Single man, no children"),
         x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$wa_gender <- data %>%
  filter(key != "Single father") %>%
  hc_line() %>%
  add_CIs(data = data %>% filter(key != "Single father"),
          colors = SGmix6_cat[1:3])

chart_tables$wa_gender <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y) %>%
  arrange(desc(.[[2]])) %>%
  mutate_all( ~ replace(., is.na(.), "--"))

chart_tables$wa_gender_samples <- alldata$singlehh_ad$sample  %>%
  filter(Group %in% c("Female working-age adult with dependent children",
                      "Female working-age adult, no dependent children",
                      "Male working-age adult with dependent children",
                      "Male working-age adult, no dependent children")) %>%
  mutate(Group = case_when(Group == "Female working-age adult with dependent children"
                           ~ "Single mother",
                           Group == "Female working-age adult, no dependent children"
                           ~ "Single woman, no children",
                           Group == "Male working-age adult with dependent children"
                           ~ "Single father",
                           Group == "Male working-age adult, no dependent children"
                           ~ "Single man, no children"),
         Group = factor(Group, levels = chart_tables$wa_gender$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

# * - pn gender ----------------------------------------------------------------

data <- alldata$singlehh_ad$rel_rates %>%
  filter(Group %in% c("Female pensioner", "Male pensioner")) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  mutate(key = Group) %>%

  # add CI data
  left_join(CIs$gender %>% mutate(key = as.character(key)), by = c("key", "x" = "period")) %>%

  mutate(key = case_when(key == "Female pensioner" ~ "Single female pensioner",
                         key == "Male pensioner" ~ "Single male pensioner"),
         x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$pn_gender <- data %>%
  hc_line() %>%
  add_CIs(data = data, colors = SGmix6_cat[1:2])

chart_tables$pn_gender <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pn_gender_samples <- alldata$singlehh_ad$sample  %>%
  filter(Group %in% c("Female pensioner", "Male pensioner")) %>%
  mutate(Group = case_when(Group == "Female pensioner" ~ "Single female pensioner",
                           Group == "Male pensioner" ~ "Single male pensioner"),
         Group = factor(Group, levels = chart_tables$pn_gender$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

# * - ad sexid -----------------------------------------------------------------
data <- alldata$sexid_ad$rel_rates %>%
  filter(Group != "All") %>%
  mutate(Group = as.character(Group),
         Group = case_when(Group == "Other" ~ "LGB+",
                           TRUE ~ Group)) %>%
  pivot_longer(cols = periods[18:(length(periods))], values_to = "y", names_to = "x") %>%
  rename(key = Group) %>%

  # add CI data
  left_join(CIs$sexid %>% mutate(key = as.character(key)), by = c("key", "x" = "period")) %>%

  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$ad_sexid <- data %>%
  filter(key != "(Missing)") %>%
  complete_ts() %>%
  hc_line() %>%
  add_CIs(data = data %>% filter(key != "(Missing)"),
          colors = SGmix6_cat[1:2])

chart_tables$ad_sexid <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y) %>%
  arrange(desc(.[[2]]))

chart_tables$ad_sexid_age <- alldata$sexid_ad$age %>%
  filter(Group != "All") %>%
  mutate(Group = as.character(Group),
         Group = case_when(Group == "Other" ~ "LGB+",
                           TRUE ~ Group),
         Group = factor(Group,
                        levels = c("LGB+",
                                   "Heterosexual / straight",
                                   "(Missing)"))) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

chart_tables$ad_sexid_samples <- alldata$sexid_ad$sample %>%
  filter(Group != "All") %>%
  mutate(Group = as.character(Group),
         Group = case_when(Group == "Other" ~ "LGB+",
                           TRUE ~ Group),
         Group = factor(Group,
                        levels = c("LGB+",
                                   "Heterosexual / straight",
                                   "(Missing)"))) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

# * - ad marital ------------------------------------------------------------------
data <- alldata$marital_ad$rel_rates %>%
  filter(Group != "All") %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  mutate(key = Group) %>%

  # add CI data
  left_join(CIs$marital %>% mutate(key = as.character(key)), by = c("key", "x" = "period")) %>%
  mutate(key = case_when(key == "Divorced / Civil Partnership dissolved / separated"
                           ~ "Divorced",
                         key == "Married / Civil Partnership" ~ "Married",
                           TRUE ~ key),
         x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$ad_marital <- data %>%
  hc_line() %>%
  add_CIs(data = data, colors = SGmix6_cat[1:5])

chart_tables$ad_marital <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y) %>%
  arrange(desc(.[[2]]))

chart_tables$ad_marital_samples <- alldata$marital_ad$sample %>%
  filter(Group != "All") %>%
  mutate(Group = case_when(Group == "Divorced / Civil Partnership dissolved / separated"
                           ~ "Divorced",
                           Group == "Married / Civil Partnership" ~ "Married",
                           TRUE ~ as.character(Group)),
         Group = factor(Group, levels = chart_tables$ad_marital$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

# * - pp ethnic ----------------------------------------------------------------

data <- alldata$ethgrphh_pp$rel_rates %>%
  filter(Group != "All") %>%
  pivot_longer(cols = period5yr[8:length(period5yr)], values_to = "y", names_to = "x") %>%
  mutate(key = Group) %>%

  # add CI data
  left_join(CIs$ethnic, by = c("key", "x" = "period")) %>%
  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$pp_ethnic <- data %>%
  complete_ts(x = period5yr) %>%
  hc_line() %>%
  add_CIs(data = data, colors = SGmix6_cat[1:4])

chart_tables$pp_ethnic <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y) %>%
  arrange(desc(.[[2]]))

chart_tables$pp_ethnic_age <- alldata$ethgrphh_pp$age %>%
  filter(Group != "All") %>%
  mutate(Group = factor(Group, levels = chart_tables$pp_ethnic$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

chart_tables$pp_ethnic_samples <- alldata$ethgrphh_pp$sample %>%
  filter(Group != "All") %>%
  mutate(Group = factor(Group, levels = chart_tables$pp_ethnic$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

# * - ad religion -----------------------------------------------------------------
data <- alldata$religsc_ad$rel_rates  %>%
  filter(Group != "All") %>%
  pivot_longer(cols = period5yr[18:length(period5yr)], values_to = "y", names_to = "x") %>%
  mutate(key = Group) %>%

  # add CI data
  left_join(CIs$religion, by = c("key", "x" = "period")) %>%
  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$ad_religion <-  data %>%
  complete_ts(x = period5yr) %>%
  hc_line() %>%
  add_CIs(data = data, colors = SGmix6_cat)

chart_tables$ad_religion <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y) %>%
  arrange(desc(.[[2]]))

chart_tables$ad_religion_age <- alldata$religsc_ad$age %>%
  filter(Group != "All") %>%
  mutate(Group = factor(Group, levels = chart_tables$ad_religion$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

chart_tables$ad_religion_samples <- alldata$religsc_ad$sample %>%
  filter(Group != "All") %>%
  mutate(Group = factor(Group, levels = chart_tables$ad_religion$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

# * - pp dis1 -------------------------------------------------------------

data <- alldata$disab_pp$rel_rates %>%
  filter(Group %in% c("In household with disabled person(s)",
                      "In household with no disabled person(s)")) %>%
  mutate(Group = case_when(Group == "In household with disabled person(s)"
                           ~ "Someone disabled",
                           Group == "In household with no disabled person(s)"
                           ~ "No-one disabled")) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  filter(!is.na(y)) %>%
  mutate(key = Group) %>%

  # add CI data
  left_join(CIs$dis1, by = c("key", "x" = "period")) %>%

  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$pp_dis1 <- data %>%
  complete_ts() %>%
  hc_line() %>%
  add_disbar() %>%
  add_CIs(data = data, colors = SGmix6_cat[1:2])

chart_tables$pp_dis1 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pp_dis1_samples <- alldata$disab_pp$sample %>%
  filter(Group %in% c("In household with disabled person(s)",
                      "In household with no disabled person(s)")) %>%
  mutate_if(is.numeric, comma, 1) %>%
  mutate(Group = case_when(Group == "In household with disabled person(s)"
                           ~ "Someone disabled",
                           Group == "In household with no disabled person(s)"
                           ~ "No-one disabled"))

# * - pp dis2 -------------------------------------------------------------
data <- alldata$disab_nobens_pp$rel_rates %>%
  filter(Group %in% c("In household with disabled person(s)",
                      "In household with no disabled person(s)"),
         Group != "All") %>%
  mutate(Group = case_when(Group == "In household with disabled person(s)"
                           ~ "Someone disabled",
                           Group == "In household with no disabled person(s)"
                           ~ "No-one disabled")) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  filter(!is.na(y)) %>%
  mutate(key = Group) %>%

  # add CI data
  left_join(CIs$dis2, by = c("key", "x" = "period")) %>%

  mutate(x = factor(x, ordered = TRUE),
         low = y - halfCI_widened,
         low = ifelse(low < 0, 0, low),
         high = y + halfCI_widened) %>%
  select(-halfCI_widened)

charts$pp_dis2 <- data %>%
  complete_ts() %>%
  hc_line() %>%
  add_disbar() %>%
  add_CIs(data = data, colors = SGmix6_cat[1:2])

chart_tables$pp_dis2 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pp_dis2_samples <- alldata$disab_nobens_pp$sample %>%
  filter(Group %in% c("In household with disabled person(s)",
                      "In household with no disabled person(s)")) %>%
  mutate_if(is.numeric, comma, 1) %>%
  mutate(Group = case_when(Group == "In household with disabled person(s)"
                           ~ "Someone disabled",
                           Group == "In household with no disabled person(s)"
                           ~ "No-one disabled"))

# 6 income ---------------------------------------------------------------------

# * - palma --------------------------------------------------------------------
data <- alldata$palmagini$palma %>%
  pivot_longer(cols = periods, names_to = "x", values_to = "y") %>%
  mutate(key = Measure)

charts$palma <- data %>%
  hc_line(minYRange = 1.3) %>%
  add_recessionbar()

chart_tables$palma <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

# * - gini ---------------------------------------------------------------------
data <- alldata$palmagini$gini %>%
  pivot_longer(cols = periods, names_to = "x", values_to = "y") %>%
  mutate(key = Measure)

charts$gini <- data %>%
  hc_line(minYRange = 0.18) %>%
  add_recessionbar()

chart_tables$gini <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)


# * - medians ------------------------------------------------------------------
data <- rbind(filter(alldata$medians$ahc, Group == "All people") %>%
                mutate(key = "After housing costs"),
              filter(alldata$medians$bhc, Group == "All people") %>%
                mutate(key = "Before housing costs")) %>%
  select(-Group) %>%
  pivot_longer(cols = periods, names_to = "x", values_to = "y")

# no CIs as method inappropriate for medians

charts$medians <- hc_line(data, minYRange = 500) %>%
  highcharter::hc_yAxis(min = 200,
                        labels = list(formatter = NULL,
                        format = "£{value:,f}")) %>%
  add_recessionbar() %>%
  hc_yAxis(accessibility = list(description = "Amount"))

chart_tables$medians <- data %>%
  select(x, y, key) %>%
  mutate(y = paste0("£", y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

# * - deciles ------------------------------------------------------------------
data <- alldata_1yr$decilepoints$bhc %>%
  pivot_longer(cols = years, names_to = "key", values_to = "y") %>%
  filter(key %in% tail(years, 4)) %>%
  mutate(x = factor(Decile, levels = c("1", "2", "3", "4", "5", "6", "7", "8",
                                       "9"),
                    labels = c("Lowest", "2nd", "3rd", "4th", "5th", "6th",
                               "7th", "8th", "Highest"), ordered = TRUE))

charts$deciles <- data %>%
  mutate(y = ifelse(y == 99992, NA, y)) %>%
  hc_groupedcol()

chart_tables$deciles <- data %>%
  select(x, y, key) %>%
  mutate(y = ifelse(y == 99992, "--", paste0("£", y))) %>%
  rename(Year = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$deciles_samples <- alldata_1yr$decilepoints$sample %>%
  select(chart_tables$deciles$Year) %>%
  pivot_longer(cols = chart_tables$deciles$Year, names_to = "Year",
               values_to = "Sample") %>%
  mutate(Sample = ifelse(Year == "2020/21", 0, comma(Sample, 1)))


# * - decile point change ------------------------------------------------------

data <- alldata_1yr$decilepoints$bhc %>%
  select(Decile, years[length(years) - 3], years[length(years)])

data <- data.frame(Decile = data$Decile,
                   Change = data[, 3]/data[, 2] - 1) %>%
  rename(x = Decile,
         y = 2) %>%
  mutate(x = factor(x, levels = seq(1, 9, 1),
                    labels = c("Lowest", "2nd", "3rd", "4th", "5th", "6th",
                               "7th", "8th", "Highest")))

charts$dec_change <- hc_col(data) %>%
  highcharter::hc_yAxis(minRange = 0.2)

chart_tables$dec_change <- data %>%
  rename(Decile = x,
         Change = y) %>%
  mutate_if(is.numeric, fmtpct)

chart_tables$dec_change_samples <- alldata_1yr$decilepoints$sample %>%
  select(Group, years[length(years) - 3], max(years)) %>%
  mutate_if(is.numeric, comma, 1)


# * - distribution -------------------------------------------------------------

data <- alldata$distribution$dist %>%
  filter(income > 0,
         income < 1600) %>%
  uncount(gs_newpp)

decpts <- alldata$distribution$distdecs %>%
  select(x, xpos, value) %>%
  rename(decile = x,
         x = xpos)

thresholds <- alldata$distribution$distthresh

charts$distribution <- highchart() %>%
  hc_add_theme(my_theme) %>%
  highcharter::hc_add_series(density(data$income, adjust = 1.5), type = "area") %>%
  hc_colors(colors = SGblue) %>%
  highcharter::hc_chart(backgroundColor = SGwhite) %>%
  highcharter::hc_yAxis(visible = FALSE)  %>%
  highcharter::hc_xAxis(title = list(text = ""),
           tickLength = 0,
           labels = list(
             style = list(fontSize = 'medium',
                          color = SGdarkgrey,
                          textOutline = 0),
             format = "£{value:,f}"
             ),
           plotLines = list(
             list(color = SGmidgrey,
                  width = 2,
                  value = thresholds$povthresh,
                  label = list(style = list(fontSize = "medium",
                                            color = SGdarkgrey),
                               align = "right",
                               x = -6,
                               rotation = 0,
                               text = paste0("Poverty threshold:<br>£",
                                             round2(thresholds$povthresh, 0)))),
             list(color = SGmidgrey,
                  width = 2,
                  value = thresholds$UKmedian,
                  label = list(style = list(fontSize = "medium",
                                            color = SGdarkgrey),
                               rotation = 0,
                               text = paste0("UK median income:<br>£",
                                             round2(thresholds$UKmedian, 0))))
           ))  %>%
  highcharter::hc_legend(enabled = FALSE) %>%
  highcharter::hc_tooltip(backgroundColor = SGwhite,
             useHTML = TRUE,
             style = list(fontSize = 'medium',
                          color = SGtext),
             formatter = JS('function () {
                              return "£" + Highcharts.numberFormat(this.point.x, 0);
                              }')  ) %>%
  hc_exporting_options() %>%
  highcharter::hc_plotOptions(
    area = list(
      zoneAxis = "x",
      # color band locations from decpts
      zones = lapply(seq(1, 10, 1), function(x) {
        x = list(value = decpts$value[x],
                 fillColor = ifelse(as.numeric(x) %% 2 == 0, SGblue, SGlightblue))
      }))) %>%
  highcharter::hc_annotations(
    list(
      labelOptions = list(shape = NULL,
                          backgroundColor = NULL,
                          borderColor = NULL,
                          y = 0,
                          style = list(fontSize = "medium",
                                       fontWeight = "bold")),
      # decile label positions from decpts
      labels = lapply(seq(1, 10, 1), function(x) {
        x = list(point = list(xAxis = 0, yAxis = 0, y = 0,
                              x = decpts$x[x]),
                 text = decpts$decile[x])
      }))) %>%
  highcharter::hc_size(width = 680, height = 400)


# * - sources -------------------------------------------------------------------

data <- alldata$sources$sources %>%
  filter(Decile != "All") %>%
  pivot_longer(cols = c("Earnings", "Investments", "Occupational pensions",
                        "Other sources", "Social Security payments"),
               names_to = "key", values_to = "y") %>%
  rename(x = Decile) %>%
  mutate(key = ifelse(key == "Social Security payments", "Social security payments", key),
         x = factor(x, levels = c("All", as.character(seq(1, 10, 1))),
                    labels = c("All", "Lowest", "2nd", "3rd", "4th", "5th",
                               "6th", "7th", "8th", "9th", "Highest")))

charts$sources <- hc_stackedcol(data)

chart_tables$sources <- data %>%
  pivot_wider(names_from = x, values_from = y) %>%
  mutate_if(is.numeric, fmtpct) %>%
  rename(Source = key)

chart_tables$sources_samples <- alldata$decileshares$sample %>%
  select(Decile, max(periods)) %>%
  ungroup() %>%
  mutate(Decile = factor(Decile, levels = c("All", as.character(seq(1, 10, 1))),
                         labels = c("All", "Lowest", "2nd", "3rd", "4th", "5th",
                                    "6th", "7th", "8th", "9th", "Highest"))) %>%
  pivot_wider(names_from = Decile, values_from = max(periods)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  mutate(Group = "All") %>%
  select(Group, everything())


# save all ---------------------------------------------------------------------
saveRDS(charts, "data/povertycharts.rds")
saveRDS(chart_tables, "data/povertyhtmltables.rds")
rm(list = ls())

cat("Poverty charts and html tables created", fill = TRUE)

