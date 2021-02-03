# Functions for rendering the website

infobox <- function(md) {
  div(class = "infobox-text",
      create_html(md))
}

message <- function(md) {
  tags <- htmltools::tags
  div(class = "message-text",
      tags$strong(md))
}

devnote <- function(md) {
  tags <- htmltools::tags
  div(class = "devnote-text",
      tags$strong(md))
}

create_html <- function(md) {
  htmltools::HTML(
    markdown::markdownToHTML(
      text = md, fragment.only = TRUE
    )
  )
}

abbr <- function(short, long) {
  tags <- htmltools::tags
  tags$abbr(title = long,
            short)
  }


interactive <- function(chart,
                        title,
                        subtitle,
                        description = NULL,
                        height = 3.5) {

  title <- str_wrap(title, 68)
  subtitle <- str_wrap(subtitle, 78)
  chart <- chart + labs(title = title, subtitle = subtitle)
  chart <- girafe(ggobj = chart, width = 1, height_svg = height, width_svg = 7)
  chart <- girafe_options(x = chart,
                 opts_hover(css = "fill: #fdd522; stroke: gray; fill-opacity: 1; stroke-opacity: 1"))

  id <- word(title, 1L, sep = ":")
  id <- word(id, 2L)
  id <- paste0("figure-", id)

  tags <- htmltools::tags

  tags$figure(
    id = id,
    role = "group",
    "aria-label" = title,
    chart,
    tags$figcaption(description)
  )
}

make4panels <- function(title_tl, subtitle_tl, text_tl, chart_tl, desc_tl,
                        title_tr, subtitle_tr, text_tr, chart_tr, desc_tr,
                        title_bl, subtitle_bl, text_bl, chart_bl, desc_bl,
                        title_br, subtitle_br, text_br, chart_br, desc_br) {

  tags <- htmltools::tags

  chart_tl <- girafe(ggobj = chart_tl)
  chart_tr <- girafe(ggobj = chart_tr)
  chart_bl <- girafe(ggobj = chart_bl)
  chart_br <- girafe(ggobj = chart_br)

  div(class = "row fluid-row",

      div(class = "col-md-6",

          div(class = "panel panel-default",
              div(class = "panel-heading",
                  h2(class = "panel-title", title_tl),
                  p(subtitle_tl)),
              tags$figure(role = "group",
                          class = "panel-body",
                          "aria-label" = desc_tl,
                          tags$figcaption(text_tl),
                          chart_tl)),

          div(class = "panel panel-default",
              div(class = "panel-heading",
                  h2(class = "panel-title", title_bl),
                  p(subtitle_bl)),
              tags$figure(role = "group",
                          class = "panel-body",
                          "aria-label" = desc_bl,
                          tags$figcaption(text_bl),
                          chart_bl))),

      div(class = "col-md-6",

          div(class = "panel panel-default",
              div(class = "panel-heading",
                  h2(class = "panel-title", title_tr),
                  p(subtitle_tr)),
              tags$figure(role = "group",
                          class = "panel-body",
                          "aria-label" = desc_tr,
                          tags$figcaption(text_tr),
                          chart_tr)),

          div(class = "panel panel-default",
              div(class = "panel-heading",
                  h2(class = "panel-title", title_br),
                  p(subtitle_br)),
              tags$figure(role = "group",
                          class = "panel-body",
                          "aria-label" = desc_br,
                          tags$figcaption(text_br),
                          chart_br))) )
}
