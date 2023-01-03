parse_trk = function(path) {
    raw = read_file_raw(path) |> 
        reset_pos()
    
    ## File header ----
    ## Start-of-file constant: 50 50 0E 01
    get_bytes(raw, 4)
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
    user_metadata = get_metadata(raw)
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
