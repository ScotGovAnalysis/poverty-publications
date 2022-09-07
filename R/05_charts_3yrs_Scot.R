# load packages and data -------------------------------------------------------
library(tidyverse)
library(labelled)
library(ggrepel)
library(ggiraph)

source("R/00_functions.R")
source("R/00_strings.R")
source("R/00_colours.R")

alldata <- readRDS("data/tables.rds")
alldata_1yr <- readRDS("data/tables_1yr.rds")

charts <- list()
chart_tables <- list()

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

years <- levels(labels$years$formatted)
periods <- levels(labels$years$periods)
period5yr <- levels(labels$years$period5yr)

# 1 poverty --------------------------------------------------------------------

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
  mutate(x = factor(x, ordered = TRUE),
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))


charts$pov_1 <- data %>%
  linechart() +
  scale_y_continuous(limits = c(0.05, 0.35)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3)))

chart_tables$pov_1 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pov_1_samples <- alldata$relAHC$sample %>%
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
  mutate(x = factor(x, ordered = TRUE),
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$pov_2 <- data %>%
  linechart() +
  scale_y_continuous(limits = c(0.13, 0.6)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3)))

chart_tables$pov_2 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pov_2_samples <- alldata$absAHC$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group == "All people")

# * - wa rel -------------------------------------------------------------------
data <- rbind(alldata$relAHC$rates %>%
                filter(Group == "Working-age adults") %>%
                mutate(key = "After housing costs") %>%
                select(-Group),
              alldata$relBHC$rates %>%
                filter(Group == "Working-age adults") %>%
                mutate(key = "Before housing costs") %>%
                select(-Group)) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  mutate(x = factor(x, ordered = TRUE),
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$pov_3 <- data %>%
  linechart() +
  scale_y_continuous(limits = c(0.05, 0.35)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3)))

chart_tables$pov_3 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pov_3_samples <- alldata$relAHC$sample %>%
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
  mutate(mid_line = y * some,
         x = factor(x, ordered = TRUE),
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = case_when(x <= "1995-98" ~ paste0(x, "\nWork status not available"),
                             TRUE ~ paste0(x, "\nIn workless households: ",
                                           fmtpct(none),
                                           "\nIn working households: ",
                                           fmtpct(some))),
         data_id = paste(x, key))

charts$pov_4 <- data %>%
  linechart() +
  scale_y_continuous(limits = c(0.0, 0.3)) +
  addscales() +
  addsource() +
  addlabels()  +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3))) +
  geom_ribbon(data = filter(data, x >= "1997-00" & x < max(x)),
              mapping = aes(ymin = mid_line, ymax = y),
              show.legend = FALSE,
              fill = SGblue,
              alpha = 0.2) +
  geom_area(data = filter(data, x >= "1997-00" & x < max(x)),
            aes(y = mid_line),
            show.legend = FALSE,
            fill = SGoranges[1],
            alpha = 0.2,
            outline.type = "both",
            colour = SGoranges[1]) +
  geom_point_interactive(data = filter(data, x >= "1997-00" & x < max(x)),
                         aes(x = x, y = mid_line,
                             tooltip = tooltip,
                             data_id = data_id),
                         show.legend = FALSE,
                         size = 6,
                         colour = "white",
                         alpha = 0.01) +
  annotate("text", x = 4.5, y = 0.15, label = "In workless households",
           colour = SGblue, hjust = 0) +
  annotate("text", x = 4.5, y = 0.06, label = "In working households",
           colour = SGoranges[1], hjust = 0)

chart_tables$pov_4 <- data %>%
  filter(x >= "1996-99") %>%
  select(x, none, some) %>%
  pivot_longer(cols = c("none", "some"), names_to = "Group", values_to = "y") %>%
  mutate(Group = case_when(Group == "none" ~ "In workless households",
                           Group == "some" ~ "In working households"),
         y = fmtpct(y)) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pov_4_samples <- alldata$relAHC$sample %>%
  select(names(chart_tables$pov_4)) %>%
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
  mutate(x = factor(x, ordered = TRUE),
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$pov_5 <- data %>%
  linechart() +
  scale_y_continuous(limits = c(0.13, 0.6)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3)))

chart_tables$pov_5 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pov_5_samples <- alldata$absAHC$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group == "Working-age adults")

# * - pn rel -------------------------------------------------------------------
data <- rbind(alldata$relAHC$rates %>%
                filter(Group == "Pensioners") %>%
                mutate(key = "After housing costs") %>%
                select(-Group),
              alldata$relBHC$rates %>%
                filter(Group == "Pensioners") %>%
                mutate(key = "Before housing costs") %>%
                select(-Group)) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  mutate(x = factor(x, ordered = TRUE),
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$pov_6 <- data %>%
  linechart() +
  scale_y_continuous(limits = c(0.05, 0.35)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3)))

chart_tables$pov_6 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pov_6_samples <- alldata$relAHC$sample %>%
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
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  mutate(x = factor(x, ordered = TRUE),
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$pov_7 <- data %>%
  linechart() +
  scale_y_continuous(limits = c(0.1, 0.58)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3)))

chart_tables$pov_7 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pov_7_samples <- alldata$absAHC$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group == "Pensioners")

# * - pn dep -------------------------------------------------------------------
data <- alldata$pmd$rates %>%
  pivot_longer(cols = periods[periods >= "2009-12"],
               values_to = "y", names_to = "x") %>%
  mutate(key = "none",
         x = factor(x, ordered = TRUE),
         label = case_when(x == min(x) |x == max(x) ~ fmtpct(y)),
         tooltip = paste0(fmtpct(y), " (", x, ")"),
         data_id = paste(x))

charts$pov_8 <- data %>%
  linechart(recession = FALSE) +
  scale_y_continuous(limits = c(0, 0.3)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3)))

chart_tables$pov_8 <- data %>%
  select(x, y) %>%
  mutate(y = fmtpct(y),
         Group = "Pensioners aged 65 and over") %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$pov_8_samples <- alldata$pmd$sample %>%
  mutate_if(is.numeric, comma, 1)

# 2 child poverty --------------------------------------------------------------

# * - rel ----------------------------------------------------------------------
data <- rbind(alldata$relAHC$rates %>%
                filter(Group == "Children") %>%
                mutate(key = "After housing costs") %>%
                select(-Group),
              alldata$relBHC$rates %>%
                filter(Group == "Children") %>%
                mutate(key = "Before housing costs") %>%
                select(-Group)) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  mutate(x = factor(x, ordered = TRUE),
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$child_1 <- data %>%
  linechart() +
  scale_y_continuous(limits = c(0.1, 0.4)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3)))

chart_tables$child_1 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$child_1_samples <- alldata$relAHC$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group == "Children")

# * - work ---------------------------------------------------------------------
data <- alldata$relAHC$rates %>%
  filter(Group == "Children") %>%
  rbind(alldata$workinghh_wa$rel_comps %>%
          filter(Group != "All") %>%
          mutate("1994-97" = NA,
                 "1995-98" = NA)) %>%
  mutate(key = "After housing costs") %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  pivot_wider(names_from = Group, values_from = y) %>%
  rename(y = "Children",
         none = "No one in paid work",
         some = "Someone in paid work") %>%
  mutate(mid_line = y * some,
         x = factor(x, ordered = TRUE),
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = case_when(x <= "1995-98" ~ paste0(x, "\nWork status not available"),
                             TRUE ~ paste0(x, "\nIn workless households: ",
                                           fmtpct(none),
                                           "\nIn working households: ",
                                           fmtpct(some))),
         data_id = paste(x, key))

charts$child_2 <- data %>%
  linechart() +
  scale_y_continuous(limits = c(0.0, 0.44)) +
  addscales() +
  addsource() +
  addlabels()  +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3))) +
  geom_ribbon(data = filter(data, x >= "1997-00" & x < max(x)),
              mapping = aes(ymin = mid_line, ymax = y),
              show.legend = FALSE,
              fill = SGblue,
              alpha = 0.2) +
  geom_area(data = filter(data, x >= "1997-00" & x < max(x)),
            aes(y = mid_line),
            show.legend = FALSE,
            fill = SGoranges[1],
            alpha = 0.2,
            outline.type = "both",
            colour = SGoranges[1]) +
  geom_point_interactive(data = filter(data, x >= "1997-00" & x < max(x)),
                         aes(x = x, y = mid_line,
                             tooltip = tooltip,
                             data_id = data_id),
                         show.legend = FALSE,
                         size = 6,
                         colour = "white",
                         alpha = 0.01) +
  annotate("text", x = 4.5, y = 0.2, label = "In workless households",
           colour = SGblue, hjust = 0) +
  annotate("text", x = 4.5, y = 0.06, label = "In working households",
           colour = SGoranges[1], hjust = 0)

chart_tables$child_2 <- data %>%
  filter(x >= "1996-99") %>%
  select(x, none, some) %>%
  pivot_longer(cols = c("none", "some"), names_to = "Group", values_to = "y") %>%
  mutate(Group = case_when(Group == "none" ~ "In workless households",
                           Group == "some" ~ "In working households"),
         y = fmtpct(y)) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$child_2_samples <- alldata$relAHC$sample %>%
  select(names(chart_tables$child_2)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group == "Children")

# * - abs ----------------------------------------------------------------------
data <- rbind(alldata$absAHC$rates %>%
                filter(Group == "Children") %>%
                mutate(key = "After housing costs") %>%
                select(-Group),
              alldata$absBHC$rates %>%
                filter(Group == "Children") %>%
                mutate(key = "Before housing costs") %>%
                select(-Group)) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  mutate(x = factor(x, ordered = TRUE),
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$child_3 <- data %>%
  linechart() +
  scale_y_continuous(limits = c(0.06, 0.53)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3)))

chart_tables$child_3 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$child_3_samples <- alldata$absAHC$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group == "Children")

# * - dep ----------------------------------------------------------------------
data <- alldata$cmd$rates %>%
  pivot_longer(cols = periods[periods >= "2004-07"],
               values_to = "y", names_to = "x") %>%
  filter(!is.na(y) | x == "2009-12") %>%
  mutate(key = case_when(Measure %in% c("New measure, after housing costs",
                                        "Old measure, after housing costs") ~
                           "After housing costs",
                         TRUE ~ "Before housing costs"),
         x = factor(x, ordered = TRUE),
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key)) %>%
  arrange(x)

charts$child_4 <- data %>%
  linechart(recession = FALSE) +
  scale_y_continuous(limits = c(0.05, 0.35)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(3, 3))) +

  geom_vline(aes(xintercept = 16),
             colour = SGgreys[3],
             alpha = 0.9) +
  annotate("text", label = "Methodology change\n2010/11", colour = SGgreys[2],
           size = 3, x = 16.2, y = Inf, hjust = 0, vjust = 2)

chart_tables$child_4 <- data %>%
  select(x, y, Measure) %>%
  mutate(y = fmtpct(y)) %>%
  pivot_wider(names_from = x, values_from = y) %>%
  mutate_all( ~ replace(., is.na(.), "--"))

chart_tables$child_4_samples <- alldata$cmd$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  mutate_all( ~ replace(., is.na(.), "--"))

# 3 equality -------------------------------------------------------------------

# * - age ----------------------------------------------------------------------
data <- alldata$ageband_ad$rel_rates %>%
  filter(Group != "All") %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  mutate(key = Group,
         x = factor(x, ordered = TRUE),
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ paste0(fmtpct(y), " (", key, ")")),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$equality_1 <- data %>%
  filter(key != "All") %>%
  linechart(recession = FALSE) +
  scale_y_continuous(limits = c(0.1, 0.35)) +
  addscales(palette = SGmix9[4:9]) +
  addsource() +
  addlabels(filter(data, key != "All")) +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(8, 8)))

chart_tables$equality_1 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$equality_1_samples <- alldata$ageband_ad$sample %>%
  mutate_if(is.numeric, comma, 1) %>%
  filter(Group != "All")

# * - gender wa ----------------------------------------------------------------
data <- alldata$singlehh_ad$rel_rates %>%
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
                           ~ "Single man, no children")) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  mutate(key = Group,
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$equality_2 <- data %>%
  linechart(recession = FALSE) +
  scale_y_continuous(limits = c(0.21, 0.68)) +
  addscales(c(SGmix3, SGblue)) +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(15, 3)))

chart_tables$equality_2 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y) %>%
  arrange(desc(.[[2]])) %>%
  mutate_all( ~ replace(., is.na(.), "--"))

chart_tables$equality_2_samples <- alldata$singlehh_ad$sample  %>%
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
         Group = factor(Group, levels = chart_tables$equality_2$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

# * - gender pn ----------------------------------------------------------------
data <- alldata$singlehh_ad$rel_rates %>%
  filter(Group %in% c("Female pensioner", "Male pensioner")) %>%
  mutate(Group = case_when(Group == "Female pensioner" ~ "Single female pensioner",
                           Group == "Male pensioner" ~ "Single male pensioner")) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  mutate(key = Group,
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$equality_3 <- data %>%
  linechart(recession = FALSE) +
  scale_y_continuous(limits = c(0.07, 0.54)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(14, 3)))

chart_tables$equality_3 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$equality_3_samples <- alldata$singlehh_ad$sample  %>%
  filter(Group %in% c("Female pensioner", "Male pensioner")) %>%
  mutate(Group = case_when(Group == "Female pensioner" ~ "Single female pensioner",
                           Group == "Male pensioner" ~ "Single male pensioner"),
         Group = factor(Group, levels = chart_tables$equality_3$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

# * - marital ------------------------------------------------------------------
data <- alldata$marital_ad$rel_rates %>%
  filter(Group != "All") %>%
  mutate(Group = case_when(Group == "Divorced / Civil Partnership dissolved / separated"
                           ~ "Divorced",
                           Group == "Married / Civil Partnership" ~ "Married",
                           TRUE ~ Group)) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  mutate(key = Group,
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ paste0(fmtpct(y), " (", key, ")")),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$equality_4 <- data %>%
  linechart(recession = FALSE) +
  scale_y_continuous(limits = c(0.03, 0.5)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(8, 8)))

chart_tables$equality_4 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y) %>%
  arrange(desc(.[[2]]))

chart_tables$equality_4_samples <- alldata$marital_ad$sample %>%
  filter(Group != "All") %>%
  mutate(Group = case_when(Group == "Divorced / Civil Partnership dissolved / separated"
                           ~ "Divorced",
                           Group == "Married / Civil Partnership" ~ "Married",
                           TRUE ~ Group),
         Group = factor(Group, levels = chart_tables$equality_4$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

# * - ethnic -------------------------------------------------------------------
data <- alldata$ethgrphh_pp$rel_rates %>%
  filter(Group != "All") %>%
  pivot_longer(cols = period5yr[8:length(period5yr)], values_to = "y", names_to = "x") %>%
  mutate(key = Group,
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$equality_5 <- data %>%
  linechart(recession = FALSE) +
  scale_y_continuous(limits = c(0.03, 0.5)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$period5yr),
                   breaks = c("1994-99", "", "", "", "", "",
                              "2000-05", "", "", "", "", "",
                              "2006-11", "", "", "", "", "",
                              "2012-17", "", "", "", "2016-21"),
                   expand = expansion(add = c(9, 3)))

chart_tables$equality_5 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y) %>%
  arrange(desc(.[[2]]))

chart_tables$equality_5_age <- alldata$ethgrphh_pp$age %>%
  filter(Group != "All") %>%
  mutate(Group = factor(Group, levels = chart_tables$equality_5$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

chart_tables$equality_5_samples <- alldata$ethgrphh_pp$sample %>%
  filter(Group != "All") %>%
  mutate(Group = factor(Group, levels = chart_tables$equality_5$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

# * - religion -----------------------------------------------------------------
data <- alldata$religsc_ad$rel_rates  %>%
  filter(Group != "All") %>%
  pivot_longer(cols = period5yr[18:length(period5yr)], values_to = "y", names_to = "x") %>%
  mutate(key = Group,
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$equality_6 <- data %>%
  linechart(recession = FALSE) +
  scale_y_continuous(limits = c(0.08, 0.6)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$period5yr),
                   breaks = c("1994-99", "", "", "", "", "",
                              "2000-05", "", "", "", "", "",
                              "2006-11", "", "", "", "", "",
                              "2012-17", "", "", "", "2016-21"),
                   expand = expansion(add = c(1, 3)))

chart_tables$equality_6 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y) %>%
  arrange(desc(.[[2]]))

chart_tables$equality_6_age <- alldata$religsc_ad$age %>%
  filter(Group != "All") %>%
  mutate(Group = factor(Group, levels = chart_tables$equality_6$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

chart_tables$equality_6_samples <- alldata$religsc_ad$sample %>%
  filter(Group != "All") %>%
  mutate(Group = factor(Group, levels = chart_tables$equality_6$Group, ordered = TRUE)) %>%
  mutate_if(is.numeric, comma, 1) %>%
  arrange(Group)

# * - disability 1 -------------------------------------------------------------
data <- alldata$disab_pp$rel_rates %>%
  filter(Group %in% c("In household with disabled person(s)",
                      "In household with no disabled person(s)")) %>%
  mutate(Group = case_when(Group == "In household with disabled person(s)"
                           ~ "Someone disabled",
                           Group == "In household with no disabled person(s)"
                           ~ "No-one disabled")) %>%
  pivot_longer(cols = periods, values_to = "y", names_to = "x") %>%
  filter(!is.na(y)) %>%
  mutate(key = Group,
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$equality_7 <- data %>%
  linechart(recession = FALSE) +
  scale_y_continuous(limits = c(0.1, 0.4)) +
  addscales() +
  addsource() +
  addlabels() +
  disabilitybreaks() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(10, 3)))

chart_tables$equality_7 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$equality_7_samples <- alldata$disab_pp$sample %>%
  filter(Group %in% c("In household with disabled person(s)",
                      "In household with no disabled person(s)")) %>%
  mutate_if(is.numeric, comma, 1) %>%
  mutate(Group = case_when(Group == "In household with disabled person(s)"
                           ~ "Someone disabled",
                           Group == "In household with no disabled person(s)"
                           ~ "No-one disabled"))

# * - disability 2 -------------------------------------------------------------
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
  mutate(key = Group,
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$equality_8 <- data %>%
  linechart(recession = FALSE) +
  scale_y_continuous(limits = c(0.1, 0.4)) +
  addscales() +
  addsource() +
  addlabels() +
  disabilitybreaks() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(10, 3)))

chart_tables$equality_8 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Group = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$equality_8_samples <- alldata$disab_nobens_pp$sample %>%
  filter(Group %in% c("In household with disabled person(s)",
                      "In household with no disabled person(s)")) %>%
  mutate_if(is.numeric, comma, 1) %>%
  mutate(Group = case_when(Group == "In household with disabled person(s)"
                           ~ "Someone disabled",
                           Group == "In household with no disabled person(s)"
                           ~ "No-one disabled"))

# * - TO DO country of origin --------------------------------------------------

# 4 income ---------------------------------------------------------------------
# * - palma --------------------------------------------------------------------
data <- alldata$palmagini$palma %>%
  pivot_longer(cols = periods, names_to = "x", values_to = "y") %>%
  mutate(key = Measure,
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$inc_1 <- data %>%
  linechart() +
  scale_y_continuous(limits = c(0.8, 1.8)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3)))

chart_tables$inc_1 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$inc_1_samples <- alldata$palmagini$sample %>%
  mutate_if(is.numeric, comma, 1)

# * - gini ---------------------------------------------------------------------
data <- alldata$palmagini$gini %>%
  pivot_longer(cols = periods, names_to = "x", values_to = "y") %>%
  mutate(key = Measure,
         label = case_when(x == min(x) ~ paste0(key, ": ", fmtpct(y)),
                           x == max(x) ~ fmtpct(y)),
         tooltip = paste0(key, ": ", fmtpct(y), " (", x, ")"),
         data_id = paste(x, key))

charts$inc_2 <- data %>%
  linechart() +
  scale_y_continuous(limits = c(0.27, 0.4)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3)))

chart_tables$inc_2 <- data %>%
  select(x, y, key) %>%
  mutate(y = fmtpct(y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$inc_2_samples <- alldata$palmagini$sample %>%
  mutate_if(is.numeric, comma, 1)

# * - medians ------------------------------------------------------------------
data <- rbind(filter(alldata$medians$ahc, Group == "All people") %>%
                mutate(key = "After housing costs"),
              filter(alldata$medians$bhc, Group == "All people") %>%
                mutate(key = "Before housing costs")) %>%
  select(-Group) %>%
  pivot_longer(cols = periods, names_to = "x", values_to = "y") %>%
  mutate(label = case_when(x == min(x) ~ paste0(key, ": £", y),
                           x == max(x) ~ paste0("£", y)),
         tooltip = paste0(key, ": £", y, " (", x, ")"),
         data_id = paste(x, key))

charts$inc_3 <- data %>%
  linechart() +
  scale_y_continuous(limits = c(250, 600)) +
  addscales() +
  addsource() +
  addlabels() +
  scale_x_discrete(drop = FALSE,
                   limits = levels(labels$years$periods),
                   breaks = c("1994-97", "", "", "", "", "",
                              "2000-03", "", "", "", "", "",
                              "2006-09", "", "", "", "", "",
                              "2012-15", "", "", "", "", "",
                              "2018-21"),
                   expand = expansion(add = c(12, 3)))

chart_tables$inc_3 <- data %>%
  select(x, y, key) %>%
  mutate(y = paste0("£", y)) %>%
  rename(Measure = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$inc_3_samples <- alldata$medians$sample %>%
  filter(Group == "All people") %>%
  mutate_if(is.numeric, comma, 1)

# * - deciles ------------------------------------------------------------------
data <- alldata_1yr$decilepoints$bhc %>%
  pivot_longer(cols = years, names_to = "key", values_to = "y") %>%
  filter(key %in% tail(years, 5)) %>%
  mutate(x = Decile,
         tooltip = paste0("£", y, " (Decile ", x, ", ", key, ")"),
         data_id = paste(x, key),
         id = ifelse(key == "2020/21", 2, 1),
         id = factor(id, ordered = TRUE))

charts$inc_4 <- data %>%
  ggplot(aes(x = x, y = y, fill = key, group = key)) +

  geom_bar_interactive(aes(tooltip = tooltip,
                           data_id = tooltip),
                       position = 'dodge',
                       colour = "white",
                       stat = "identity") +

  scale_fill_manual(values = rev(c(SGoranges[4], SGblues[3:6]))) +

  scale_x_discrete(labels = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th",
                              "8th", "9th"),
                   expand = c(0.1, 0.1)) +

  scale_y_continuous(labels = comma_format(prefix = "£")) +

  theme(axis.line.y = element_line(),
        axis.text.y = element_text(hjust = 1,
                                   margin = margin(0, 3, 0, 0, "pt")),
        axis.ticks.length = unit(2, "pt"),
        axis.ticks.y = element_line(),
        axis.title = element_blank()) +

  addsource()

chart_tables$inc_4 <- data %>%
  select(x, y, key) %>%
  mutate(y = paste0("£", y)) %>%
  rename(Year = key) %>%
  pivot_wider(names_from = x, values_from = y)

chart_tables$inc_4_samples <- alldata_1yr$decilepoints$sample %>%
  select(chart_tables$inc_4$Year) %>%
  pivot_longer(cols = chart_tables$inc_4$Year, names_to = "Year",
               values_to = "Sample") %>%
  mutate_if(is.numeric, comma, 1)

# save all ---------------------------------------------------------------------
saveRDS(charts, "data/povertycharts.rds")
saveRDS(chart_tables, "data/povertyhtmltables.rds")
rm(list = ls())
