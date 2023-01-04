get_waypoints = function(source) {
    n_waypoints = get_int(source)
    message(glue('{n_waypoints} waypoints starting at {pos(source)-4}'))
    if (n_waypoints < 1) {
        return(list())
    }
    waypoints = map(1:n_waypoints, 
                    ~ get_waypoint(source))
    return(lst(n_waypoints, waypoints))
}

get_waypoint = function(source) {
    user_metadata = get_metadata(source)
    location = get_location(source)
}

get_location = function(source) {
    if (identical(version(source), 3L)) {
        size = get_int(source) + 4
        long = get_coord(source)
        lat = get_coord(source)
        elevation = get_int(source) * 1e-3
        time = get_time(source)
        if (size > 4*4+8) {
            accuracy = get_int(source)
        } else {
            accuracy = NA_integer_
        }
        if (size > 4*4+8+4) {
            pressure = get_int(source) * 1e-3
        } else {
            pressure = NA_integer_
        }
        return(lst(long, lat, elevation, 
                   time, accuracy, pressure))
    }
    else if (identical(version(source), 4L)) {
        size = get_int(source)
        long = get_coord(source)
        lat = get_coord(source)
        
        total_len = 4*3
        location_values = list()
        while (total_len < size) {
            new_value = get_location_value(source)
            new_value_lst = list(new_value$value) |> 
                set_names(new_value$type)
            location_values = c(location_values, new_value_lst)
            total_len = total_len + new_value$len
        }
        return(c(lst(size, long, lat), location_values))
    }
}
get_location_value = function(source) {
    type_raw = get_byte(source)
    type = case_when(
        type_raw == as.raw(c('0x61')) ~ 'accuracy',
        type_raw == as.raw(c('0x62')) ~ 'battery level', 
        type_raw == as.raw(c('0x65')) ~ 'elevation', 
        type_raw == as.raw(c('0x6e')) ~ 'cell network info', 
        type_raw == as.raw(c('0x70')) ~ 'pressure', 
        type_raw == as.raw(c('0x73')) ~ 'satellites', 
        type_raw == as.raw(c('0x74')) ~ 'time', 
        type_raw == as.raw(c('0x76')) ~ 'vert. accuracy')
    assert_that(!is.na(type), 
                msg = glue('Bad type {as.character(type_raw)} in location value at {pos(source) - 1}'))
    if (identical(type, 'accuracy')) {
        value = get_int(source) * 1e-2
        size = 4
    } else if (identical(type, 'battery level')) {
        value = get_byte(source)
        size = 1
    } else if (identical(type, 'elevation')) {
        value = get_int(source) * 1e-3
        size = 4
    } else if (identical(type, 'cell network info')) {
        value = get_bytes(source, 2)
        size = 2
    } else if (identical(type, 'pressure')) {
        value = get_int(source) * 1e-3
        size = 4
    } else if (identical(type, 'satellites')) {
        value = get_bytes(source, 8)
        size = 8
    } else if (identical(type, 'time')) {
        value = get_time(source)
        size = 8
    } else if (identical(type, 'vert. accuracy')) {
        value = get_int(source) * 1e-2
        size = 4
    }
    return(lst(len = size + 1, 
               type, 
               value))
}
