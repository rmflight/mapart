source("packages.R")
loadd(lexington_bbx)
loadd(lexington_map)
names(lexington_map)
loadd(lexington_counties_water)
loadd(sarah_routes)

lexington_bbx = list(min_lon = -84.7533,
                     max_lon = -84.355,
                     min_lat = 37.9358,
                     max_lat = 38.1775)

(lexington_bbx$max_lat - lexington_bbx$min_lat) / (lexington_bbx$max_lon - lexington_bbx$min_lon)

color_roads <- rgb(0.42,0.449,0.488)
final_map = ggplot() +
  geom_sf(data = lexington_counties_water,
          inherit.aes = FALSE,
          lwd= 0.0, fill = rgb(0.203,0.234,0.277)) +
  geom_sf(data = lexington_map$streets$osm_lines,
          inherit.aes = FALSE,
          color=color_roads,
          size = .4,
          alpha = .65) +
  geom_sf(data = lexington_map$highways$osm_lines,
          inherit.aes = FALSE,
          color=color_roads,
          size = .6,
          alpha = .65) +
  geom_sf(data = st_geometry(sarah_routes),
          inherit.aes = FALSE, col = "orange", alpha = 0.2) +
  coord_sf(xlim = c(lexington_bbx$min_lon, lexington_bbx$max_lon),
           ylim = c(lexington_bbx$min_lat, lexington_bbx$max_lat),
           expand = FALSE) +
  theme(legend.position = "none") + theme_void() +
  theme(panel.background=
          element_rect(fill = "white"))

ggsave(final_map,
       filename = "sarah_lexington.png",
       scale = 1,
       width = 20,
       height = 16,
       units = "in",
       bg = rgb(0.203,0.234,0.277),
       dpi = 500)


