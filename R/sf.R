segments_to_sf = function(parsed) {
    dataf = pluck(parsed, 'segments', 'segments') |> 
        map(pluck, 'locations', 'locations') |> 
        modify_depth(1, map, as_tibble) |> 
        modify_depth(1, bind_rows) |> 
        bind_rows(.id = 'segment') |> 
        mutate(elevation = set_metres(elevation), 
               segment = as.integer(segment))
    
    attributes(dataf) = c(attributes(dataf), 
                          parsed$metadata$entries)
    attr(dataf, 'n_entries') = NULL
    attr(dataf, 'version') = parsed$version
    attr(dataf, 'file') = parsed$file
    
    data_sf = st_as_sf(dataf, 
                       coords = c('long', 'lat'), 
                       crs = 4326)
    
    ## TODO: trk presumably gives elevation in WGS84, used by GPS
    ## but map displays typically convert this to EGM96, which is 
    ## more accurate. 
    ## this approach doesn't change the elevations
    # head(datasf$elevation)
    # datasf = datasf |> 
    #     st_transform(crs = 5773)
    
    return(data_sf)
}

## TODO: Export as gpx
# datasf |> 
#     group_by(track_fid = segment) |> 
#     mutate(track_seg_id = row_number()) |> 
#     select(track_fid, track_seg_id, 
#            ele = elevation, time) |> 
#     st_write('test.gpx', layer = 'track_points')

## TODO: Export as Rds

#' Attach metres to a variable, then convert to feet
set_metres = function(metres) {
    units::set_units(metres, 'm')
}
to_ft = function(metres) {
    units::set_units(metres, 'ft')
}
to_mi = function(km) {
    units::set_units(km, 'mi')
}

#' Total elevation gain, with smoothing to reduce noise
total_climb = function(elevations, timestamps,
                       .period = 'second', .every = 60) {
    smoothed = slider::slide_period_dbl(elevations, 
                                        timestamps, 
                                        .period = .period, 
                                        .every = .every, 
                                        .f = mean)
    delta = diff(smoothed)
    climbing = keep(delta, ~.x > 0)
    return(sum(climbing))
}

#' Take the output of segments_to_sf() and collapse to 1 row per segment + date
to_linestring = function(datasf, smoothing = 60) {
    datasf |> 
        mutate(date = lubridate::date(time)) |> 
        group_by(segment, date) |> 
        summarize(elevation = list(elevation), 
                  time = list(time), 
                  do_union = FALSE) |> 
        st_cast('LINESTRING') |> 
        mutate(file = attr(datasf, 'file'), 
               creator = attr(datasf, 'creator'),
               start_time = map_vec(time, min), 
               end_time = map_vec(time, max),
               duration = difftime(end_time, start_time, 
                                   units = 'hours'), 
               start_elevation = map_vec(elevation, first),
               max_elevation = map_vec(elevation, max),
               elevation_gain = map2_vec(elevation, time, 
                                         total_climb, 
                                         .every = smoothing),
               length = st_length(geometry),
               ## Shenandoah Trail Difficulty
               std = sqrt(2 * as.numeric(to_mi(length)) * 
                              as.numeric(to_ft(elevation_gain)))
        ) |> 
        relocate(file, .before = everything()) |> 
        relocate(time, elevation, geometry, .after = everything())
}

#' Total elevation gain, with smoothing to reduce noise
total_climb = function(elevations, timestamps,
                       .period = 'second', .every = 60) {
    smoothed = slider::slide_period_vec(elevations, 
                                        timestamps, 
                                        .period = .period, 
                                        .every = .every, 
                                        .f = mean)
    delta = diff(smoothed)
    climbing = keep(delta, ~ as.numeric(.x) > 0)
    return(sum(climbing))
}

#' Plot the elevation over time
## todo: smoothing
make_profile = function(ele, time, 
                        period = 'second', 
                        every = 60) {
    dataf_raw = data.frame(
        i = time, 
        ele
    )
    dataf_smoothed = slider::slide_period_dfr(dataf_raw, 
                                              dataf_raw$i, 
                                              .period = period, 
                                              .every = every, 
                                              ~ data.frame(
                                                  time = max(.x$i),
                                                  ele = mean(.x$ele)
                                              )) |> 
        mutate(ele = to_ft(ele))
    # return(dataf_smoothed)
    ggplot(dataf_smoothed, aes(time, ele)) +
        geom_line() +
        labs(y = 'elevation')
}




