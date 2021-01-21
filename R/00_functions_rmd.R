# Functions for rendering the website

infobox <- function(md) {
  div(class = "ds_inset-text",
      div(class = "ds_inset-text__text",
          create_html(md)))
}

message <- function(md) {
  div(class = "message-text",
      div(class = "message-text__text",
          create_html(md)))
}

devnote <- function(md) {
  div(class = "devnote-text",
      div(class = "devnote-text__text",
          create_html(md)))
}

create_html <- function(md) {
  htmltools::HTML(
    markdown::markdownToHTML(
      text = md, fragment.only = TRUE
    )
  )
}

interactive <- function(chart, caption, height = 3) {

  chart <- girafe(ggobj = chart, width = 1, height_svg = height, width_svg = 7)
  chart <- girafe_options(x = chart,
                 opts_hover(css = "fill: #fdd522; stroke: gray; fill-opacity: 1; stroke-opacity: 1"))

  id <- word(caption, 1L, sep = ":")
  id <- word(id, 2L)
  id <- paste0("figure-", id)

  tags <- htmltools::tags

  tags$figure(
    id = id,
    alt = caption,
    tags$figcaption(caption),
    chart
  )
}

add_tablecaption <- function(md) {
  id <- word(md, 1L, sep = ":")
  id <- word(id, 2L)
  id <- paste0("tablecap-", id)
  p(class = "caption", id = id, md)
}

make4panels <- function(title_tl, subtitle_tl, text_tl, chart_tl, desc_tl,
                        title_tr, subtitle_tr, text_tr, chart_tr, desc_tr,
                        title_bl, subtitle_bl, text_bl, chart_bl, desc_bl,
                        title_br, subtitle_br, text_br, chart_br, desc_br) {

  tags <- htmltools::tags

  div(class = "row fluid-row",

      div(class = "col-md-6",

          tags$figure(class = "panel panel-default",
                      div(class = "panel-heading",
                          h1(class = "panel-title", title_tl),
                          p(subtitle_tl)),
                      div(class = "panel-body",
                          tags$figcaption(text_tl),
                          chart_tl)),

          tags$figure(class = "panel panel-default",
                      div(class = "panel-heading",
                          h1(class = "panel-title", title_bl),
                          p(subtitle_bl)),
                      div(class = "panel-body",
                          tags$figcaption(text_bl),
                          chart_bl))),

      div(class = "col-md-6",

          tags$figure(class = "panel panel-default",
                      div(class = "panel-heading",
                          h1(class = "panel-title", title_tr),
                          p(subtitle_tr)),
                      div(class = "panel-body",
                          tags$figcaption(text_tr),
                          chart_tr)),

          tags$figure(class = "panel panel-default",
                      div(class = "panel-heading",
                          h1(class = "panel-title", title_br),
                          p(subtitle_br)),
                      div(class = "panel-body",
                          tags$figcaption(text_br),
                          chart_br))) )
}
