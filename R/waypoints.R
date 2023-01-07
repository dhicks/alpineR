#' Waypoint structures
#' 
#' Parse Waypoint structures
#' 
#' A waypoint can contain metadata and a single location. Data will be read from the current read position of `source`, ie, `pos(source)`. 
#' 
#' @param source Raw track file contents to be parsed
#' @return Nested list
#' @name waypoints
NULL

#' @rdname waypoints
get_waypoints = function(source) {
    n_waypoints = get_int(source)
    message(glue::glue('{n_waypoints} waypoints starting at {pos(source)-4}'))
    if (n_waypoints < 1) {
        return(list())
    }
    waypoints = purrr::map(1:n_waypoints, 
                    ~ get_waypoint(source), 
                    .progress = 'waypoints')
    return(lst(n_waypoints, waypoints))
}

#' @rdname waypoints
get_waypoint = function(source) {
    user_metadata = get_metadata(source)
    location = get_location(source)
}


