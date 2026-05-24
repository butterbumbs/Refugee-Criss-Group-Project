library(tidyverse)
library(leaflet)
library(sf)
library(scales)

make_map <- function(map_data) {
  pal <- colorNumeric(
    palette  = c("yellow", "green", "red"),
    domain   = map_data$gar_pct,
    na.color = "#dddddd"
  )
  leaflet(map_data) %>%
    addProviderTiles("CartoDB") %>%
    addPolygons(
      layerId     = ~name,
      fillColor   = ~pal(gar_pct),
      fillOpacity = 0.85,
      color       = "white",
      weight      = 1,
      highlight   = highlightOptions(
        weight      = 3,
        color       = "#7a003c",
        fillOpacity = 0.95,
        bringToFront = TRUE
      ),
      label        = ~paste0(name, " — GARs: ", GAR,
                             " (", round(gar_pct, 3), "% of pop)"),
      labelOptions = labelOptions(
        style     = list("font-size" = "13px", "padding" = "4px 8px"),
        direction = "auto"
      )
    ) %>%
    addLegend(
      position  = "topright",
      pal       = pal,
      values    = ~gar_pct,
      labFormat = labelFormat(suffix = "%"),
      title     = "GAR Arrival %"
    )
}

make_city_narrative <- function(city, cons_data) {
  row   <- cons_data %>% filter(combined_name == city) %>% slice(1)
  s2015 <- cons_data %>% filter(combined_name == city, year == 2015) %>% pull(mean_cons)
  s2016 <- cons_data %>% filter(combined_name == city, year == 2016) %>% pull(mean_cons)
  
  if (any(row$did_status %in% c("Treated", "Control"))) {
    change <- s2016 - s2015
    dir    <- if (change < 0) "fell" else "rose"
    change_phrase <- paste0(dir, " by ", percent(abs(change), accuracy = 0.1),
                            " between 2015 and 2016")
    city_p <- tags$p(paste0(
      city, " is a ", tolower(row$did_status), " municipality in this study. ",
      "In 2016, Syrian Government-Assisted Refugee arrivals made up ",
      round(row$gar_pct, 3), "% of its population (",
      formatC(row$GAR, format = "d", big.mark = ","), " individuals). In ",
      tolower(row$did_status), " municipalities, the average Conservative support ", change_phrase, "."
    ))
  } else {
    city_p <- tags$p(paste0(
      city, " is not in this study. ",
      "However, in 2016, Syrian Government-Assisted Refugee arrivals made up ",
      round(row$gar_pct, 3), "% of its population (",
      formatC(row$GAR, format = "d", big.mark = ","), " individuals)."
    ))
  }
  city_p
}


takeaway <- function(...) {
  div(class = "takeaway-box",
      div(class = "takeaway-label", "Key Takeaway"), ...)
}

anno_card <- function(title, intro_text, ...) {
  bullets <- list(...)
  div(class = "anno-card",
      div(class = "anno-title", title),
      tags$p(style = "margin-bottom:0.5rem;", intro_text),
      if (length(bullets) > 0) tags$ul(lapply(bullets, tags$li))
  )
}

plot_row <- function(img_src, anno_title, intro_text, ...) {
  div(class = "plot-row",
      layout_columns(
        col_widths = c(7, 5), fill = FALSE,
        card(tags$img(src = img_src, style = "width:100%; border-radius:4px; display:block;")),
        anno_card(anno_title, intro_text, ...)
      ))
}

next_btn <- function(id, label, page_info = NULL) {
  div(class = "next-btn-wrap",
      if (!is.null(page_info)) span(class = "page-counter", page_info),
      tags$button(
        class   = "next-page-btn", id = id,
        onclick = paste0("Shiny.setInputValue('", id, "', Math.random())"),
        label
      ))
}

author_card <- function(name, bio, contact) {
  div(class = "author-card",
      div(class = "author-name", name),
      div(class = "section-text", style = "font-size:0.88rem;", bio),
      div(class = "author-contact",
          div(class = "author-contact-label", "Contact"),
          contact)
  )
}