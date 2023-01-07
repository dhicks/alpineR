#' File specification and read position attributes
#' 
#' Track file specification and read position
#' 
#' File specification version and read position are handled in an OOP style as object attributes.  These functions retrieve and set the values of these attributes.  Notably, they use `data.table::setattr()` to modify attributes from within functions by reference.  
#' 
#' @param object Object with attributes to be retrieved or set. Usually raw file contents, eg, output from `read_file_raw()`. 
#' @param version Value to be set to the `"version"` attribute
#' @param position Value to be set for the `"position"` attribute
#' @param increment Value to be added to the `"position"` attribute
#' @return Varies by function; either attribute value or `object` (invisibly)
#' @name file_attributes
NULL

#' @rdname file_attributes
version = purrr::attr_getter('version')
#' @rdname file_attributes
set_ver = function(object, version) {
    data.table::setattr(object, 'version', version)
}

#' @rdname file_attributes
pos = purrr::attr_getter('position')
#' @rdname file_attributes
set_pos = function(object, position) {
    data.table::setattr(object, 'position', position)
}
#' @rdname file_attributes
reset_pos = function(object) {
    set_pos(object, 0)
}
#' @rdname file_attributes
move_pos = function(object, increment) {
    set_pos(object, pos(object) + increment)
}


#' Functions to read primitives
#' 
#' Read primitive datatypes from trk files
#' 
#' By default, data is read off from the current read location of `source` (`pos(source)`) and the read location is updated.  `get_time()` parses data to POSIXct. 
#' 
#' @param source Data source (eg, raw trk file contents) 
#' @param offset Offset in bytes to start reading data
#' @param move_pos Should the read location be updated? 
#' @param len Number of bytes to be read
#' @param size Length of string to be read
#' @return Varies by function
#' @name get_primitives
NULL

#' @rdname get_primitives
get_byte = function(source, 
                    offset = pos(source), 
                    move_pos = TRUE) {
    value = source[offset + 1]
    if (move_pos) {
        move_pos(source, 1)
    }
    return(value)
}

# get_bytes = function(source, 
#                      len,
#                      offset = pos(source)) {
#     purrr::map_vec(1:len, 
#                    ~ get_byte(source, offset))
# }

#' @rdname get_primitives
get_int = function(source, 
                   offset = pos(source), 
                   move_pos = TRUE) {
    value = source[(offset+1):(offset+4)] |> 
        readBin('integer', size = 4, 
                signed = TRUE, endian = 'big')
    if (move_pos) {
        move_pos(source, 4)
    }
    return(value)
}

#' @rdname get_primitives
get_bool = function(source, 
                    offset = pos(source), 
                    move_pos = TRUE) {
    value = source[(offset+1):(offset+2)] |> 
        readBin('logical', size = 1, 
                signed = TRUE, endian = 'big')
    if (move_pos) {
        move_pos(source, 1)
    }
    return(value)
}

#' @rdname get_primitives
get_long = function(source, 
                    offset = pos(source), 
                    move_pos = TRUE) {
    ## <https://stackoverflow.com/a/74974331/3187973>
    bits = sapply(source[(offset+1):(offset+8)],
                  function(y)
                      rev(as.integer(rawToBits(y))))
    value = sum(bits[-1] * 2^(62:0)) - bits[1] * 2^63
    if (move_pos) {
        move_pos(source, 8)
    }
    return(value)
}

#' @rdname get_primitives
get_coord = function(source, 
                     offset = pos(source), 
                     move_pos = TRUE) {
    get_int(source, offset, move_pos) |> 
        magrittr::multiply_by(1e-7)
}

#' @rdname get_primitives
get_time = function(source, 
                    offset = pos(source), 
                    move_pos = TRUE) {
    get_long(source, offset, move_pos) |> 
        magrittr::multiply_by(1e-3) |> 
        as.POSIXct(origin = '1970-01-01')
}

#' @rdname get_primitives
get_double = function(source, 
                      offset = pos(source), 
                      move_pos = TRUE) {
    value = source[(offset+1):(offset+8)] |> 
        readBin('double', size = 8, 
                signed = TRUE, endian = 'big')
    if (move_pos) {
        move_pos(source, 8)
    }
    return(value)
}

#' @rdname get_primitives
get_raw = function(source, 
                   size,
                   offset = pos(source), 
                   move_pos = TRUE) {
    value = source[(offset+1):(offset+size)] |> 
        readBin('raw', n = size, size = 1,
                signed = TRUE, endian = 'big')
    if (move_pos) {
        move_pos(source, size)
    }
    return(value)
}

#' @rdname get_primitives
get_string = function(source, 
                      size = NULL,
                      offset = pos(source),
                      move_pos = TRUE) {
    if (is.null(size)) {
        ## If size is not known in advance, string should start w/ an int for its length
        size = get_int(source, offset, move_pos)
        offset = offset + 4
    }
    value = source[(offset+1):(offset+size)] |> 
        readBin('character', size = size, 
                signed = TRUE, endian = 'big')
    if (move_pos) {
        move_pos(source, size)
    }
    return(value)
}
