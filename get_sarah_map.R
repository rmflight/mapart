get_sarah_map = function(){
  min_lon = -84.7533
  max_lon = -84.2143
  min_lat = 37.9358
  max_lat = 38.1775
  bbx = rbind(x=c(min_lon,max_lon),y=c(min_lat,max_lat))
  colnames(bbx) <- c("min","max")

  highways = bbx %>%
    opq()%>%
    add_osm_feature(key = "highway",
                    value=c("motorway", "trunk",
                            "primary","secondary",
                            "tertiary","motorway_link",
                            "trunk_link","primary_link",
                            "secondary_link",
                            "tertiary_link",
                            "residential", "living_street",
                            "service","unclassified",
                            "pedestrian", "footway",
                            "track","path")) %>%
    osmdata_sf()
  highways$bbx = bbx
  highways
}

plot_map = function(map){
  theme_set(theme_void() + theme(legend.position = "none"))
  ggplot() + geom_sf(data = map$osm_lines, aes(color = highway), size = 0.4, alpha = 0.65) +
    coord_sf(xlim = map$bbx["x", ],
             ylim = map$bbx["y", ],
             expand = FALSE)
}