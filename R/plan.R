the_plan <-
  drake_plan(
    lexington_bbx = list(min_lon = -84.7533,
                         max_lon = -84.2143,
                         min_lat = 37.9358,
                         max_lat = 38.1775),

    lexington_map = get_map(lexington_bbx),
    lexington_counties = get_counties(lexington_bbx, "KY"),
    lexington_water = get_us_water(lexington_bbx,
                                   lexington_counties,
                                   "KY"),
    lexington_counties_water = combine_counties_uswater(lexington_counties, lexington_water),
    sarah_locations = get_takeout_locations("saraheflight/saraheflight_takeout"),
    sarah_routes = get_sarah_routes(sarah_locations)
)
