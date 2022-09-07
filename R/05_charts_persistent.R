# load packages and data -------------------------------------------------------
library(tidyverse)
library(scales)
library(ggiraph)
library(ggrepel)
library(networkD3)

source("R/00_functions.R")
source("R/00_colours.R")

persistentcharts <- list()

source <- "Source: Understanding Society, 2010-2011 to 2019-2020"

periods <- c("2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018",
             "2015-2019", "2016-2020")

alldata <- readRDS("data/persistentpoverty.rds") %>%
  filter(group %in% c("ch", "wa", "pn", "pp"),
         nation == "Scotland") %>%
  mutate(y = ifelse(housingcosts == "sample", value, round(value, 2)),
         x = factor(period, ordered = TRUE),
         group = factor(group,
                         levels = c("pp", "ch", "wa", "pn"),
                         labels = c("All individuals", "Children",
                                    "Working-age adults", "Pensioners"))) %>%
  select(-nation)

mytheme <- theme_grey() +
  theme(text = element_text(colour = SGgreys[1], size = 14),
        line = element_line(colour = SGgreys[1],
                            linetype = 1,
                            lineend = 2,
                            size = 0.5),

        plot.title = element_text(hjust = 0, colour = SGgreys[1], size = 12),
        plot.caption = element_text(hjust = 1),
        plot.title.position = "plot",

        legend.position = "top",
        legend.title = element_blank(),

        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),

        axis.line.x = element_line(),
        axis.ticks.length = unit(2, "pt"),
        axis.ticks.y = element_blank(),

        axis.title = element_blank(),
        axis.text.y = element_blank())

theme_set(mytheme)

# persistent poverty -----------------------------------------------------------

chartdata <- alldata %>%
  filter(housingcosts != "sample") %>%
  mutate(housingcosts = factor(housingcosts, levels = c("AHC", "BHC"),
                               labels = c("After housing costs",
                                          "Before housing costs"))) %>%
  group_by(group, housingcosts) %>%
  mutate(key = housingcosts,
         labels = case_when(period == min(period) ~ paste0(key, ": ", fmtpct(y)),
                            period == max(period) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", period, ")"),
         data_id = paste(key, period))


persistentcharts$chart1 <- persistentchart(filter(chartdata, group == "All individuals"))
persistentcharts$chart2 <- persistentchart(filter(chartdata, group == "Children"))
persistentcharts$chart3 <- persistentchart(filter(chartdata, group == "Working-age adults"))
persistentcharts$chart4 <- persistentchart(filter(chartdata, group == "Pensioners"))

# exit entry chart -------------------------------------------------------------

my_color <- paste0("d3.scaleOrdinal() .domain(['Not in poverty', 'In poverty']) .range(['",
                   SGblues[6], "', '", SGoranges[2], "'])")

sankey_data <- readRDS("data/persistentpoverty.rds") %>%
  filter(group %in% c("exit", "entry"),
         nation == "Scotland",
         period == max(period),
         housingcosts == "AHC")

entry <- filter(sankey_data, group == "entry")$value
exit <- filter(sankey_data, group == "exit")$value

nodes <- data.frame(name = c("Not in poverty", "In poverty", NA, NA),
                    group = c(SGblues[6], SGoranges[2], SGblues[6], SGoranges[2]))

links <- data.frame(source = c(name1 = 0, name2 = 0, name3 = 1, name4 = 1),
                    target = c(2, 3, 2, 3),
                    value = c(name5 = 0.80 - 0.80*entry, name6 = 0.80*entry,
                              name7 = 0.20*exit, name8 = 0.20 - 0.20*exit),
                    group = c(SGblues[6], SGblues[6], SGoranges[2], SGoranges[2]))

sn <- sankeyNetwork(Links = links,
                    Source = "source",
                    Target = "target",
                    Value = "value",
                    LinkGroup = "group",

                    Nodes = nodes,
                    NodeID = "name",
                    NodeGroup = "group",

                    fontFamily = "Arial",
                    fontSize = 16,
                    nodeWidth = 40,
                    nodePadding = 20,
                    colourScale = my_color,
                    height = 300,
                    width = 650)

# add node tooltips
sn$x$nodes$tooltips <- c("Not in poverty", "Not in poverty", "In poverty", "In poverty")

# add links tooltips
sn$x$links$tooltips <- c(paste0("Not in poverty -> Not in poverty: ", fmtpct(1 - entry)),
                         paste0("Not in poverty -> In poverty: ", fmtpct(entry)),
                         paste0("In poverty -> Not in poverty: ", fmtpct(exit)),
                         paste0("In poverty -> In poverty: ", fmtpct(1 - exit)))


persistentcharts$sankey <- htmlwidgets::onRender(
  sn,
  '
            function(el, x) {

              d3.selectAll(".node, .link").select("title foreignObject body pre")
                .text(function(d) { return d.tooltips; });
              d3.selectAll(".node text").attr("font-weight", "bold");
              d3.selectAll(".node text").style("fill", "#333333");
            }
            '
)

# save all ---------------------------------------------------------------------
saveRDS(persistentcharts, "data/persistentcharts.rds")
rm(list = ls())

