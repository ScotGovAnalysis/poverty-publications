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

interactive <- function(chart, height = 3) {
  chart <- girafe(ggobj = chart, width = 1, height_svg = height, width_svg = 7)
  girafe_options(x = chart,
                 opts_hover(css = "fill: #fdd522; stroke: gray; fill-opacity: 1; stroke-opacity: 1"))
}

add_caption <- function(md) {
  id <- word(md, 2L, sep = " ")
  id <- paste0("figcap-", id)
  p(class = "caption", id = paste0("#", id), md)
}

add_tablecaption <- function(md) {
  id <- word(md, 2L, sep = " ")
  id <- paste0("tablecap-", id)
  p(class = "caption", id = paste0("#", id), md)
}
