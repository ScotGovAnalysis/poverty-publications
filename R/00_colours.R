# https://design-system.service.gov.uk/styles/colour/

# colours -------

SGblue <- "#1d70b8"
SGdarkblue <- "#003078"
SGorange <- "#F36720" # used a darker shade as contrast ratio with white was too
# low (<3)
SGlightorange <- "#f47738" # this is the actual SG orange
SGpaleorange <- "#fce3d7"
SGbrown <- "#b58840"
SGred <- "#d4351c"
SGlightgreen <- "#85994b"
SGturquoise <- "#28a197"
SGgreen <- "#00703c"
SGlightblue <- "#5694ca"

SGtext <- "#0b0c0c"
SGwhite <- "#ffffff"
SGdarkgrey <- "#505a5f"
SGmidgrey <- "#b1b4b6" # fails contrast check
SGlightgrey <- "#f3f2f1" # fails

# Categorical palette 6 categories
SGmix6_cat <- c(SGblue, SGdarkblue, SGorange, SGbrown, SGlightgreen, SGturquoise)

# scales::show_col(SGmix6_cat2)

# Continuous palette 4 categories
SGmix4_cont <- c("#00437e", SGblue, SGlightblue, "#7fb2de")
SGmix9_cont <- c(SGdarkgrey, "#00437e", SGblue, SGlightblue, "#7fb2de", SGturquoise,
                 SGlightgreen, SGbrown, SGorange)


# scales::show_col(SGmix4_cont)
# scales::show_col(SGmix9_cont)
# scales::show_col(SGmix6_cat)

# highcharter theme ----

my_theme <- highcharter::hc_theme(

  # * colors ----
  colors = SGmix6_cat,

  # * chart ----
  chart = list(backgroundColor = SGwhite,
               style = list(fontFamily = "Roboto",
                            fontSize = 'medium',
                            color = SGtext)),

  # * titles ----

  title = list(
    useHTML = TRUE,
    align = "left",
    style = list(fontSize = 'large',
                 color = SGtext,
                 fontWeight = 'bold')),

  subtitle = list(
    useHTML = TRUE,
    align = "left",
    style = list(fontSize = 'medium',
                 color = SGtext,
                 # enable wrapped text for subtitles
                 whiteSpace = 'inherit')),

  credits = list(
    style = list(fontSize = 'small',
                 color = SGdarkgrey)),

  # * y axis ----

  yAxis = list(
    title = list(
      style = list(fontSize = 'medium',
                   color = SGdarkgrey)),
    labels = list(
        style = list(fontSize = 'medium',
                     color = SGdarkgrey)),
    useHTML = TRUE,
    lineWidth = 1,
    tickAmount = 5,
    tickmarkPlacement = "on",
    tickLength = 5),

  # * x axis ----

  xAxis = list(
    title = list(
      style = list(fontSize = 'medium',
                   color = SGdarkgrey)),
    labels = list(
      useHTML = TRUE,
      style = list(fontSize = 'medium',
                   color = SGdarkgrey)),
    tickmarkPlacement = "on",
    tickLength = 0),

  # * tooltip ----
  tooltip = list(
    useHTML = TRUE,
    backgroundColor = SGwhite,
    style = list(color = SGtext)),

  # * legend ----

  legend = list(
    itemStyle = list(
      color = SGtext,
      fontSize = "medium"
    )),

  plotOptions = list(
    series = list(

      # * dataLabels ----
      dataLabels = list(
        style = list(fontSize = "medium",
                     color = SGtext)),

      marker = list(fillColor = SGwhite,
                    lineWidth = 2,
                    lineColor = NA)))
)




