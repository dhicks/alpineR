parse_trk = function(path) {
    raw = read_file_raw(path) |> 
        reset_pos()
    start = get_bytes(raw, 4)
    
    if (identical(start, 
                  as.raw(c(0x00, 0x00, 0x00, 0x03)))) {
        ## APQ 2.0 through 2.2.7c
        set_ver(raw, 3L)
    } else if (identical(start, 
                         as.raw(c(0x50, 0x50, 0x0e, 0x01)))) {
        ## APQ 2.2.8c through present
        set_ver(raw, 4L)
    }
    message(glue('trk file version {version(raw)}'))
    
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
        ## double: total track length due to elevation changes (m)
        len_ele = get_double(raw)
        ## double: total track elevation gain (m)
        gain = get_double(raw)
        ## long: total track time (s)
        duration = get_long(raw)
        
        ## Metadata
        metadata = get_metadata(raw)
        ## Waypoints
        waypoints = get_waypoints(raw)
        ## Segments
        segments = get_segments(raw)
        return(lst(n_locations, n_locations, 
                   n_segments, n_waypoints, 
                   long, lat, time, 
                   len, len_ele, gain, duration, 
                   metadata, 
                   waypoints, 
                   segments))
    }
    
    if (identical(version(raw), 4L)) {
        ## File header ----
        ## header size (bytes before {Waypoints})
        header_size = get_int(raw)
        header_size
        
        ## Metadata ----
        ## Technical metadata
        # debugonce(get_metadata_entry)
        technical_metadata = get_metadata(raw)
        
        ## User metadata
        # debugonce(get_string)
        # set_pos(raw, 77)
        user_metadata = get_metadata4(raw)
        user_metadata
        
        
        ## Waypoints ----
        # debugonce(get_location)
        # set_pos(raw, 247-4)
        waypoints = get_waypoints(raw)
        
        ## Track segments ----
        # debug(get_location_value)
        # set_pos(raw, 261)
        segments = get_segments(raw)
        
        lst(technical_metadata, 
            user_metadata, 
            waypoints, 
            segments)
    }
    }
