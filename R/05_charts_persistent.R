# load packages and data -------------------------------------------------------
library(tidyverse)
library(highcharter)
library(networkD3)

source("R/00_functions.R")
source("R/00_colours.R")

persistentcharts <- list()

source <- "Source: Understanding Society, 2010-2011 to 2019-2020"

periods <- c("2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018",
             "2015-2019", "2016-2020", "2017-2021")

alldata <- readRDS("data/persistentpoverty.rds") %>%
  filter(group %in% c("ch", "wa", "pn", "pp"),
         nation == "Scotland") %>%
  mutate(y = ifelse(housingcosts == "sample", value, round2(value, 2)),
         x = factor(period, ordered = TRUE),
         group = factor(group,
                        levels = c("pp", "ch", "wa", "pn"),
                        labels = c("All individuals", "Children",
                                   "Working-age adults", "Pensioners"))) %>%
  select(-nation)

# persistent poverty -----------------------------------------------------------

chartdata <- alldata %>%
  filter(housingcosts != "sample") %>%
  mutate(housingcosts = factor(housingcosts, levels = c("AHC", "BHC"),
                               labels = c("After housing costs",
                                          "Before housing costs"))) %>%
  group_by(group, housingcosts) %>%
  mutate(key = housingcosts)


persistentcharts$chart1 <- chartdata %>%
  filter(group == "All individuals") %>%
  hc_line(persistent = TRUE) %>%
  hc_xAxis(tickPositions = c(0, 2, 4, 7))

persistentcharts$chart2 <- chartdata %>%
  filter(group == "Children") %>%
  hc_line(persistent = TRUE) %>%
  hc_xAxis(tickPositions = c(0, 2, 4, 7))

persistentcharts$chart3 <- chartdata %>%
  filter(group == "Working-age adults") %>%
  hc_line(persistent = TRUE) %>%
  hc_xAxis(tickPositions = c(0, 2, 4, 7))

persistentcharts$chart4 <- chartdata %>%
  filter(group == "Pensioners") %>%
  hc_line(persistent = TRUE) %>%
  hc_xAxis(tickPositions = c(0, 2, 4, 7))


persistentcharts <- lapply(persistentcharts, function(x) {
  x %>% highcharter::hc_xAxis(tickInterval = 2)
})

# exit entry chart -------------------------------------------------------------

sankey_data <- readRDS("data/persistentpoverty.rds") %>%
  filter(group %in% c("exit", "entry"),
         nation == "Scotland",
         period == max(period),
         housingcosts == "AHC")

entry <- filter(sankey_data, group == "entry")$value
exit <- filter(sankey_data, group == "exit")$value
povrate <- 0.21

persistentcharts$sankey <- highchart() %>%
  hc_add_series(type = "sankey",
                data = list(
                  list(description = "Remaining in poverty",
                       from = "In poverty",
                       to = "in",
                       weight = 100 * round2(povrate - povrate*exit, 2),
                       custom = list(extraInformation = 'Remaining in poverty',
                                     value = 100 - round2(exit, 2)*100)),
                  list(description = "Exiting poverty",
                       from = "In poverty",
                       to = "out",
                       weight = 100 * round2(povrate*exit, 2),
                       custom = list(extraInformation = 'Exiting poverty',
                                     value = round2(exit, 2)*100)),
                  list(description = "Entering poverty",
                       from = "Not in poverty",
                       to = "in",
                       weight = 100 * round2((1-povrate)*entry, 2),
                       custom = list(extraInformation = 'Entering poverty',
                                     value = round2(entry, 2)*100)),
                  list(description = "Remaining out of poverty",
                       from = "Not in poverty",
                       to = "out",
                       weight = 100 * round2((1-povrate) - (1-povrate)*entry, 2),
                       custom = list(extraInformation = 'Remaining out of poverty',
                                     value = 100 - round2(entry, 2) * 100))
                ),
                nodes = list(list(id = "Not in poverty",
                                  color = SGblue),
                             list(id = "In poverty",
                                  color = SGorange),
                             list(id = "in",
                                  name = "In poverty",
                                  color = SGorange),
                             list(id = "out",
                                  name = "Not in poverty",
                                  color = SGblue)),
                tooltip = list(
                  nodeFormat = '{point.name}<br/>',
                  headerFormat = '',
                  pointFormat = '<b>{point.custom.extraInformation}:
                  {point.custom.value}%</b><br/>')
  ) %>%
  hc_add_theme(my_theme) %>%
  hc_size(width = 680, height = 300)


# save all ---------------------------------------------------------------------
saveRDS(persistentcharts, "data/persistentcharts.rds")
rm(list = ls())

cat("Persistent poverty charts created", fill = TRUE)
