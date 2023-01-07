#' Tidy a parsed trk file
#' 
#' Converts the segments from a parsed trk file into a sf (simple features) dataframe.  One row = one location.  Column `segment` gives the segment index within the trk file.  File-level metadata, including the file basename, are attached as attributes. 
#' @param parsed Parsed contents of a trk file, eg, as returned by `parse_trk()`
#' @returns See details
#' @export 
segments_to_sf = function(parsed) {
    dataf = parsed |> 
        purrr::pluck('segments', 'segments') |> 
        purrr::map(pluck, 'locations', 'locations') |> 
        purrr::modify_depth(1, map, as_tibble) |> 
        purrr::modify_depth(1, bind_rows) |> 
        dplyr::bind_rows(.id = 'segment') |> 
        dplyr::mutate(elevation = set_metres(elevation), 
               segment = as.integer(segment))
    
    attributes(dataf) = c(attributes(dataf), 
                          parsed$metadata$entries)
    attr(dataf, 'n_entries') = NULL
    attr(dataf, 'version') = parsed$version
    attr(dataf, 'file') = parsed$file
    
    data_sf = sf::st_as_sf(dataf, 
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

#' Attach metres to a variable, and convert to feet or miles
#' @import units
#' @param metres Numeric vector with values in metres
#' @param km Numeric vector with values in kilometers
#' @name units
NULL

#' @rdname units
#' @export
set_metres = function(metres) {
    units::set_units(metres, 'm')
}
#' @rdname units
#' @export
to_ft = function(metres) {
    units::set_units(metres, 'ft')
}
#' @rdname units
#' @export
to_mi = function(km) {
    units::set_units(km, 'mi')
}

#' Total elevation gain
#' 
#' Computes total elevation gained on a hike, with smoothing to reduce noise. 
#' @param elevations Vector of elevations
#' @param timestamps Vector of POSIX timestamps
#' @param .period Unit of time used for smooth
#' @param .every How many time units to smooth together
#' @returns Length-one numeric
#' @export
total_climb = function(elevations, timestamps,
                       .period = 'second', .every = 60) {
    smoothed = slider::slide_period_dbl(as.numeric(elevations), 
                                        timestamps, 
                                        .period = .period, 
                                        .every = .every, 
                                        .f = mean)
    delta = diff(smoothed)
    climbing = purrr::keep(delta, ~.x > 0)
    return(sum(climbing))
}

#' Aggregate tracks by date and segment
#' 
#' Take the output of segments_to_sf() and aggregate to 1 row per segment + date
#' 
#' This generally assumes that the column names are unchanged from `segments_to_sf()`.  Geometry column is aggregated as type `LINESTRING`.  Columns are added for useful trk file metadata, including base filename and APQ version, along with segment-level statistics including total time and distance, elevation gain, and [Shenadoah Trail Difficulty](https://www.nps.gov/shen/planyourvisit/how-to-determine-hiking-difficulty.htm). 
#' @param datasf sf dataframe to be aggregated
#' @param smoothing Amount of smoothing to be done in calculating total climb, in seconds
#' @returns sf dataframe with one row = one date-segment combination
#' @export
to_linestring = function(datasf, smoothing = 60) {
    datasf |> 
        dplyr::mutate(date = lubridate::date(time)) |> 
        dplyr::group_by(segment, date) |> 
        dplyr::summarize(elevation = list(elevation), 
                  time = list(time), 
                  do_union = FALSE) |> 
        sf::st_cast('LINESTRING') |> 
        dplyr::mutate(file = attr(datasf, 'file'), 
               creator = attr(datasf, 'creator'),
               start_time = purrr::map_vec(time, min), 
               end_time = purrr::map_vec(time, max),
               duration = difftime(end_time, start_time, 
                                   units = 'hours'), 
               start_elevation = purrr::map_vec(elevation, first),
               max_elevation = purrr::map_vec(elevation, max),
               elevation_gain = purrr::map2_vec(elevation, time, 
                                         total_climb, 
                                         .every = smoothing),
               length = sf::st_length(geometry),
               ## Shenandoah Trail Difficulty
               std = sqrt(2 * as.numeric(to_mi(length)) * 
                              as.numeric(to_ft(elevation_gain)))
        ) |> 
        dplyr::relocate(file, .before = everything()) |> 
        dplyr::relocate(time, elevation, geometry, .after = everything()) |> 
        sf::st_as_sf()
}


#' Profile plot
#' 
#' One-liner to create a profile plot (elevation over time) with smoothing
#' 
#' Elevations are converted to feet
#' @inheritParams total_climb
#' @returns `ggplot2` plot
#' @export
ggprofile = function(ele, time, 
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
        dplyr::mutate(ele = to_ft(ele))
    # return(dataf_smoothed)
    ggplot(dataf_smoothed, aes(time, ele)) +
        geom_line() +
        labs(y = 'elevation')
}




