get_map = function(bbx_list){
  bbx = rbind(x=c(bbx_list$min_lon, bbx_list$max_lon),y=c(bbx_list$min_lat, bbx_list$max_lat))
  colnames(bbx) = c("min","max")

  highways = bbx %>%
    opq() %>%
    add_osm_feature(key = "highway",
                    value=c("motorway", "trunk",
                            "primary","secondary",
                            "tertiary","motorway_link",
                            "trunk_link","primary_link",
                            "secondary_link",
                            "tertiary_link")) %>%
    osmdata_sf()
  streets = bbx %>%
    opq() %>%
    add_osm_feature(key = "highway",
                    value = c("residential", "living_street",
                              "service","unclassified",
                              "pedestrian", "footway",
                              "track","path")) %>%
    osmdata_sf()

  list(highways = highways,
       streets = streets)
}

plot_map = function(map, bbx_list){
  bbx = rbind(x=c(bbx_list$min_lon, bbx_list$max_lon),y=c(bbx_list$min_lat, bbx_list$max_lat))
  theme_set(theme_void() + theme(legend.position = "none"))
  ggplot() + geom_sf(data = map$osm_lines, aes(color = highway), size = 0.4, alpha = 0.65) +
    coord_sf(xlim = bbx["x", ],
             ylim = bbx["y", ],
             expand = FALSE)
}

get_counties = function(bbx_list, state = "KY"){

  counties_state = counties(state=state, cb=T, class="sf")
  counties_state = st_crop(counties_state,
                         xmin = bbx_list$min_lon, xmax = bbx_list$max_lon,
                         ymin = bbx_list$min_lat, ymax = bbx_list$max_lat)
  counties_state
}

get_us_water = function(bbx_list, counties_list, state){
  get_water = function(county_GEOID, state = state){
    area_water(state, county_GEOID, class = "sf")
  }
  water = do.call(rbind,
                   lapply(counties_list$COUNTYFP, get_water, state))
  water = st_crop(water,
                   xmin = bbx_list$min_lon, xmax = bbx_list$max_lon,
                   ymin = bbx_list$min_lat, ymax = bbx_list$max_lat)
  water
}

combine_counties_uswater = function(counties_state, counties_water){
  st_difference(counties_state, st_union(counties_water))
}

get_takeout_locations = function(takeout_dir){

  file2 = file.path(takeout_dir, "Takeout", "Location History", "Semantic Location History")
  files = list.files(file2, pattern = "*.json", recursive = TRUE, full.names = TRUE)
  get_locations = function(file, .progress = NULL){
    knitrProgressBar::update_progress(.progress)
    data = jsonlite::fromJSON(file)
    tl_obj = data$timelineObjects$placeVisit
    loc = cbind(tl_obj$location, tl_obj$duration)
    tt = as.numeric(loc$startTimestampMs)/1000
    loc$time=as.POSIXct(tt,origin = "1970-01-01")
    #conver longitude & latitude from E7 to GPS
    loc$lat = loc$latitudeE7 / 1e7
    loc$lon = loc$longitudeE7 / 1e7
    loc = data.frame(loc)
    loc = loc[, c("placeId", "time", "lat", "lon")]
    loc = dplyr::filter(loc, !is.na(lon))
    loc
  }
  #prog = knitrProgressBar::progress_estimated(length(files))
  locs_df = purrr::map_df(files, get_locations)
  locs_df
}

get_sarah_routes = function(locs_df){
  old_home = list(lat = 37.9898308, lon = -84.5054868)
  new_home = list(lat = 37.982469, lon = -84.506552)
  locs_df$day = lubridate::floor_date(locs_df$time, unit = "day")
  locs_df = tibble::as_tibble(locs_df)
  locs_df = dplyr::mutate(locs_df, home = dplyr::case_when(
    day <= as.POSIXct("2018-03-14") ~ list(old_home),
    TRUE ~ list(new_home)
  ))

  split_day = split(locs_df, locs_df$day)

  day_routes = purrr::map(split_day, daily_routes)
  day_routes = do.call(rbind, day_routes)
}

daily_routes = function(day_locations){
  home_loc = day_locations$home[[1]]
  use_locs = day_locations[, c("lat", "lon")]
  use_locs2 = rbind(data.frame(lat = home_loc$lat, lon = home_loc$lon),
                    use_locs,
                    data.frame(lat = home_loc$lat, lon = home_loc$lon))
  route = NULL
  for(irow in 2:nrow(use_locs2)){
    p1 = c(use_locs2$lon[irow - 1], use_locs2$lat[irow - 1])
    p2 = c(use_locs2$lon[irow], use_locs2$lat[irow])
    oo = osrmRoute(src = p1, dst = p2, returnclass = "sf",
                    overview = "full")
    route <- rbind(route, oo)
  }
  route
}