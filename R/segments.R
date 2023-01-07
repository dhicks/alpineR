#' Segment structures
#' 
#' Parse Segment structures
#' 
#' trk files can contain one or more segments.  Each segment can contain metadata and a series of locations.  Data will be read from the current read position of `source`, ie, `pos(source)`. 
#' 
#' @param source Raw track file contents to be parsed
#' @return Nested list
#' @name segments
NULL

#' @rdname segments
get_segments = function(source) {
    n_segments = get_int(source)
    message(glue::glue('{n_segments} segments starting at {pos(source)-4}'))
    if (n_segments < 1) {
        return(list())
    }
    segments = purrr::map(1:n_segments, 
                   ~ get_segment(source), 
                   .progress = 'segments')
    return(lst(n_segments, segments))
}

#' @rdname segments
get_segment = function(source) {
    metadata = get_metadata(source)
    locations = get_locations(source)
    return(lst(metadata, locations))
}

