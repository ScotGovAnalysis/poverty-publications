
# Define colour palette with SG colours

SGblue <- "#0065bd"
SGblue2 <- "#002d54"

SGblues <- c("#002d54", "#00437e", "#0065bd", "#4c93d0", "#7fb2de", "#d9effc")
SGgreys <- c("#333333", "#727272", "#b3b3b3", "#ebebeb", "#f8f8f8")
SGoranges <- c("#aa5327", "#f47738", "#f79f73", "#fac8af")

SGmix <- c(SGblues[3], SGblues[1], SGgreys[2], SGoranges[1],
           SGoranges[2], SGblues[5], SGgreys[3])

SGmix2 <- c("Very low" = SGoranges[1], "Low" = SGoranges[2],
            "Marginal" = SGgreys[4], "High" = SGblues[5])

# scales::show_col(SGblues)
# scales::show_col(SGgreys)
# scales::show_col(SGoranges)
# scales::show_col(SGmix)


