get_segments = function(source) {
    n_segments = get_int(source)
    message(glue('{n_segments} segments starting at {pos(source)-4}'))
    if (n_segments < 1) {
        return(list())
    }
    segments = map(1:n_segments, 
                   ~ get_segment(source), 
                   .progress = 'segments')
    return(lst(n_segments, segments))
}

get_segment = function(source) {
    metadata = get_metadata(source)
    locations = get_locations(source)
    return(lst(metadata, locations))
}

