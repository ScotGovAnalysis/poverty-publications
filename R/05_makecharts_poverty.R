
# Create charts for poverty publication - both, interactive and non-interactive versions

library(tidyverse)
library(labelled)
library(plotly)

source("R/00_functions.R")
source("R/00_colours.R")
source("R/05_prepcharts_poverty.R")

# Theme ----

mytheme <- theme_grey() + 
  theme(text = element_text(colour = SGgreys[1], size = 14), 
        
        line = element_line(colour = SGgreys[1], 
                            linetype = 1, 
                            lineend = 2, 
                            size = 0.5), 
        
        plot.title = element_text(hjust = 0, colour = SGgreys[1]), 
        plot.subtitle = element_text(hjust = 0, colour = SGgreys[1]), 
        plot.caption = element_text(hjust = 1), 
        
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

# Key trends ----

# chart0a

# chart0b

# chart0c

# Poverty ----

data <- mutate(relpov, value = pprate)

chart01 <- plotdata(data) +
  addxlabels() +
  scale_y_continuous(limits = c(0, 0.45)) +
  addnames(AHC_ypos = 0.28, BHC_ypos = 0.16) +
  addscales() +
  addsource() +
  addlabels()

data <- mutate(abspov, value = pprate)

chart02 <- plotdata(data) +
  addxlabels() +
  scale_y_continuous(limits = c(0, 0.45)) +
  addnames(AHC_ypos = 0.41, BHC_ypos = 0.18) +
  addscales() +
  addsource() +
  addlabels()

data <- mutate(relpov, value = warate)

chart03 <- plotdata(data) +
  addxlabels() +
  scale_y_continuous(limits = c(0, 0.45)) +
  addnames(AHC_ypos = 0.24, BHC_ypos = 0.13) +
  addscales() +
  addsource() +
  addlabels()

data <- mutate(abspov, value = warate)

chart04 <- plotdata(data) +
  addxlabels() +
  scale_y_continuous(limits = c(0, 0.45)) +
  addnames(AHC_ypos = 0.33, BHC_ypos = 0.15) +
  addscales() +
  addsource() +
  addlabels()

data <- mutate(workpov, value = wacomp) %>%
  filter(groupingvar == "Someone in paid work")

chart05 <- plotdata(data, up = 0.3) +
  addxlabels() +
  scale_y_continuous(limits = c(0.3, 0.75)) +
  addnames(AHC_ypos = 0.56, BHC_ypos = 0.39) +
  addscales() +
  addsource() +
  addlabels()

data <- mutate(relpov, value = pnrate)

chart06 <- plotdata(data) +
  addxlabels() +
  scale_y_continuous(limits = c(0, 0.45)) +
  addnames(AHC_ypos = 0.34, BHC_ypos = 0.17) +
  addscales() +
  addsource() +
  addlabels()

data <- mutate(abspov, value = pnrate)

chart07 <- plotdata(data, up = 0.11) +
  addxlabels() +
  scale_y_continuous(limits = c(0.11, 0.56)) +
  addnames(AHC_ypos = 0.18, BHC_ypos = 0.55) +
  addscales() +
  addsource() +
  addlabels()

data <- mutate(pndep, value = pnrate, key = "AHC")

chart08 <- plotdata(data) +
  addxlabels() +
  scale_y_continuous(limits = c(0, 0.45)) +
  addscales() +
  addsource() +
  addlabels(uprAHC = -0.015, uprBHC = 0.03,
            uplAHC = -0.015, uplBHC = 0.03)

# Equality ----

# Child poverty ----

# Income ----

# Child poverty update ----

remove(abspov, cmd, data, deciles, disability, disability2, distribution, 
       ethnic, gender, gini, labels, marital, medians, mytheme, palma, periods, 
       pndep, religion, relpov, SGblue, SGblue2, SGblues, SGgreys, SGmix, 
       SGoranges, sources, UKdeciles, workpov, yearsno)