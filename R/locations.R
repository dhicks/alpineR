#' Locations
#' 
#' Parse locations
#' 
#' trk file version 4 enabled the data content of locations to be much more flexible, treating them as a container for one or more location values. Data will be read from the current read position of `source`, ie, `pos(source)`. 
#' 
#' @param source Raw track file contents to be parsed
#' @return Nested list.  An individual location is a flat list of {key: value} pairs. 
#' @name locations
NULL

#' @rdname locations
get_locations = function(source) {
    n_locations = get_int(source)
    message(glue::glue('{n_locations} locations starting at {pos(source)-4}'))
    if (n_locations < 1) {
        return(lst(n_locations))
    }
    locations = purrr::map(1:n_locations, 
                    ~ get_location(source), 
                    .progress = 'locations')
    return(lst(n_locations, locations))
}

#' @rdname locations
get_location = function(source) {
    assertthat::assert_that(version(source) %in% c(3L, 4L))
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
                magrittr::set_names(new_value$type)
            location_values = c(location_values, new_value_lst)
            total_len = total_len + new_value$len
        }
        return(c(lst(size, long, lat), location_values))
    }
}

#' @rdname locations
get_location_value = function(source) {
    type_raw = get_byte(source)
    type = dplyr::case_when(
        type_raw == as.raw(c('0x61')) ~ 'accuracy',
        type_raw == as.raw(c('0x62')) ~ 'battery level', 
        type_raw == as.raw(c('0x65')) ~ 'elevation', 
        type_raw == as.raw(c('0x6e')) ~ 'cell network info', 
        type_raw == as.raw(c('0x70')) ~ 'pressure', 
        type_raw == as.raw(c('0x73')) ~ 'satellites', 
        type_raw == as.raw(c('0x74')) ~ 'time', 
        type_raw == as.raw(c('0x76')) ~ 'vert. accuracy')
    assertthat::assert_that(!is.na(type), 
                msg = glue::glue('Bad type {as.character(type_raw)} in location value at {pos(source) - 1}'))
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
        value = list(get_raw(source, 2))
        size = 2
    } else if (identical(type, 'pressure')) {
        value = get_int(source) * 1e-3
        size = 4
    } else if (identical(type, 'satellites')) {
        value = list(get_raw(source, 8))
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
