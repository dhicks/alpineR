#' Parse a trk file
#' 
#' Parse an AlpineQuest trk file
#' 
#' Currently only trk versions 3 (APQ 2.0 through 2.2.7c) and 4 (2.2.8c and later) are supported.  Messages will indicate numbers of metadata entries, waypoints, locations, etc., for debugging purposes. 
#' @param path Path to the trk file to be parsed
#' @returns A list of the parsed file contents; exact structure depends on file version and contents.  This represents the file stream pretty directly; use `segments_to_sf()` to get a tidy representation. 
#' @export 
parse_trk = function(path) {
    raw = readr::read_file_raw(path) |> 
        reset_pos()
    
    ## Check the first 4 bytes for the file version
    start = get_raw(raw, 4)
    if (identical(start, 
                  as.raw(c(0x00, 0x00, 0x00, 0x03)))) {
        ## APQ 2.0 through 2.2.7c
        set_ver(raw, 3L)
    } else if (identical(start, 
                         as.raw(c(0x50, 0x50, 0x0e, 0x01)))) {
        ## APQ 2.2.8c through present
        set_ver(raw, 4L)
    } else {
        stop(glue::glue("Unidentified or not supported file version {paste(start, collapse = ' ')}"))
    }
    message(glue::glue('trk file version {version(raw)}'))
    
    if (identical(version(raw), 3L)) {
        header_size = get_int(raw)
        n_locations = get_int(raw)
        n_segments = get_int(raw)
        n_waypoints = get_int(raw)
        ## coords and time of 1. location
        long = get_coord(raw)
        lat = get_coord(raw)
        time = get_time(raw)
        ## total track length (m)
        len = get_double(raw)
        ## total track length due to elevation changes (m)
        len_ele = get_double(raw)
        ## total track elevation gain (m)
        gain = get_double(raw)
        ## total track time (s)
        duration = get_long(raw)
        
        metadata = get_metadata(raw)
        waypoints = get_waypoints(raw)
        segments = get_segments(raw)
        return(lst(file = basename(path), 
                   version = version(raw),
                   n_locations, n_locations, 
                   n_segments, n_waypoints, 
                   long, lat, time, 
                   len, len_ele, gain, duration, 
                   metadata, 
                   waypoints, 
                   segments))
    }
    
    if (identical(version(raw), 4L)) {
        ## File header size (bytes before {Waypoints})
        header_size = get_int(raw)
        
        technical_metadata = get_metadata(raw)
        user_metadata = get_metadata4(raw)

        waypoints = get_waypoints(raw)
        segments = get_segments(raw)
        
        lst(file = basename(path), 
            version = ver(raw),
            technical_metadata, 
            user_metadata, 
            waypoints, 
            segments)
    }
}
