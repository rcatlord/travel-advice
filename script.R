library(tidyverse) ; library(sf) ; library(rvest) ; library(leaflet) ; library(leaflet.extras) ; library(htmlwidgets)

sf <- st_read("data/world.geojson") |> 
  filter(!is.na(url))

# one line travel advice
read_status <- ~{
  message(.)
  read_html(.) |>
    html_nodes("strong") |>
    html_text2() 
}

# date updated
read_updated <- ~{
  message(.)
  read_html(.) |>
    html_nodes(".gem-c-metadata__definition") |>
    html_text2()
}

status <- sf %>%
  st_drop_geometry() |>
  mutate(status = map(pull(sf, url), read_status)) |>
  unnest(status) %>% 
  select(name, status)

updated <- sf %>%
  st_drop_geometry() |>
  mutate(updated = map(pull(sf, url), read_updated)) |>
  unnest(updated) |>
  filter(row_number() %% 3 == 2) |> 
  select(name, updated)

world <- left_join(sf, status, by = "name") |>
  left_join(updated, by = "name") |>
  mutate(status = case_when(
    str_detect(status, "essential") ~ "Advise against all but essential travel",
    is.na(status) ~ "Check travel advice before travelling",
    TRUE ~ "Advise against all travel"),
    status = as_factor(status)
  )

factpal <- colorFactor(palette = c("#E94F35","#FCB700","#C5D54E"), 
                       levels = c("Advise against all travel",
                                  "Advise against all but essential travel",
                                  "Check travel advice before travelling"))

map <- leaflet(world,
                options = leafletOptions(
                  maxZoom = 5,
                  crs = leafletCRS(
                    crsClass = "L.Proj.CRS", code = "ESRI:53009",
                    proj4def = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs",
                    resolutions = c(65536, 32768, 16384, 8192, 4096, 2048)))) |>
  setView(0,0,1) |> 
  addPolygons(weight = 1, color = "#FFFFFF", opacity = 1, 
              fillColor = ~factpal(world$status), fillOpacity = 0.8,
              popup = paste0(
                "<strong>", world$name, "</strong>",
                "<br>",
                "<a style='text-decoration: none;', href='", world$url,"' target='_blank'>FCDO travel advice</a>",
                "<br>",
                "<em>Updated: ", world$updated, "</em>"
              ),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
              highlightOptions = highlightOptions(color = "#000000", weight = 2, bringToFront = TRUE)) |>
  addLegend(position = "bottomleft", colors = c("#E94F35","#FCB700","#C5D54E"),
            labels = c("Advise against all travel",
                       "Advise against all but essential travel",
                       "Check travel advice before travelling"), opacity = 0.8) |>
  addControl(paste0("<h2>Latest FCDO travel advice</h2>"), position = "topright", className = "map-title") |> 
  addGraticule(style = list(color = "#999", weight = 0.5, opacity = 1, fill = NA)) |> 
  addGraticule(sphere = TRUE, style = list(color = "#777", weight = 1, opacity = 0.25, fill = NA)) |> 
  addEasyButton(easyButton(
    icon = "ion-arrow-shrink",
    title = "Reset view",
    onClick = JS("function(btn, map){ map.setView([0,0],1); }")
    )) |> 
  setMapWidgetStyle(list(background = "white"))

saveWidget(map, "index.html", title = "Latest FCDO travel advice")
