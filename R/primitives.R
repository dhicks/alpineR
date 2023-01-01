## Track position in the file ----
pos = attr_getter('position')
set_pos = function(object, position) {
    data.table::setattr(object, 'position', position)
}
reset_pos = function(object) {
    set_pos(object, 0)
}
move_pos = function(object, increment) {
    set_pos(object, pos(object) + increment)
}

## Functions to handle primitives ----
get_byte = function(source, 
                    offset = pos(source), 
                    move_pos = TRUE) {
    value = source[offset + 1]
    if (move_pos) {
        move_pos(source, 1)
    }
    return(value)
}

get_bytes = function(source, 
                     len,
                     offset = pos(source), 
                     move_pos = TRUE) {
    map_raw(1:len, ~get_byte(source))
}

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
get_coord = function(source, 
                     offset = pos(source), 
                     move_pos = TRUE) {
    get_int(source, offset, move_pos) |> 
        multiply_by(1e-7)
}
get_time = function(source, 
                    offset = pos(source), 
                    move_pos = TRUE) {
    get_long(source, offset, move_pos) |> 
        multiply_by(1e-3) |> 
        as.POSIXct(origin = '1970-01-01')
}

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

get_raw = function(source, 
                   size,
                   offset = pos(source), 
                   move_pos = TRUE) {
    value = source[(offset+1):(offset+size)] |> 
        readBin('raw', size = size, 
                signed = TRUE, endian = 'big')
    if (move_pos) {
        move_pos(source, size)
    }
    return(value)
}

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
