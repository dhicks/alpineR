get_waypoints = function(source) {
    n_waypoints = get_int(source)
    message(glue('{n_waypoints} waypoints starting at {pos(source)-4}'))
    if (n_waypoints < 1) {
        return(list())
    }
    waypoints = map(1:n_waypoints, 
                    ~ get_waypoint(source), 
                    .progress = 'waypoints')
    return(lst(n_waypoints, waypoints))
}

get_waypoint = function(source) {
    user_metadata = get_metadata(source)
    location = get_location(source)
}


