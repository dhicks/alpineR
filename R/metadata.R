get_metadata = function(source) {
    entries = get_metadata_content(source)
    if (entries$n_entries < 0) {
        return(lst(entries))
    }
    if (identical(version(source), 3L)) {
        n_extensions = get_int(source)
        if (identical(n_extensions, -1L)) {
            return(lst(entries))
        }
    }
    extensions = get_metadata_extensions(source)
    return(c(entries, extensions))
}

get_metadata_content = function(source = raw) {
    n_entries = get_int(source)
    message(glue('{n_entries} metadata entries starting at {pos(source)-4}'))
    if (n_entries < 0) {
        return(lst(n_entries))
    }
    if (n_entries > 0) {
        entries = list()
        while (length(entries) < n_entries) {
            new_entry = get_metadata_entry(source)
            new_entry_lst = list(new_entry$content) |> 
                set_names(new_entry$name)
            entries = c(entries, new_entry_lst)
        }
    } else {
        entries = NULL
    }
    if (n_entries >= 0 && version(source) >= 4) {
        version = get_int(source)
    } else {
        version = NA_integer_
    }
    return(c(lst(n_entries), 
               entries, 
               lst(version)))
}

get_metadata_entry = function(source) {
    name = get_string(source)
    entry_type = get_int(source)
    stopifnot(!is.na(entry_type))
    ## Size of entry is based on type
    ## NB can't use case_when() bc that evaluates all 
    ## RHS expressions, and get_int() changes pos(source)
    if (identical(entry_type, -1L)) {
        ## Boolean
        size = 1
    } else if (identical(entry_type, -2L)) {
        ## long
        size = 8
    } else if (identical(entry_type, -3L)) {
        ## double
        size = 8
    } else if (identical(entry_type, -4L)) {
        ## raw
        size = get_int(source)
    } else if (entry_type >= 0) {
        ## string
        size = entry_type
    }
    ## Content
    if (identical(entry_type, -1L)) {
        ## Boolean
        content = get_bool(source)
    } else if (identical(entry_type, -2L)) {
        ## long
        content = get_long(source)
    } else if (identical(entry_type, -3L)) {
        ## double
        content = get_double(source)
    } else if (identical(entry_type, -4L)) {
        ## raw
        content = get_raw(source, size)
    } else if (entry_type >= 0) {
        ## string
        content = get_string(source, size)
    }
    
    return(list(name = name, 
                type = entry_type, 
                content = content))
}

get_metadata_extensions = function(source) {
    n_extensions = get_int(source)
    message(glue('{n_extensions} metadata extensions starting at {pos(source)-4}'))
    if (n_extensions < 1) {
        return(lst(n_extensions))
    }
    extensions = map(1:n_extensions, 
                     ~ get_metadata_extension(source), 
                     progress = 'metadata extensions')
    return(lst(n_extensions, extensions))
}

get_metadata_extension = function(source = raw) {
    name = get_string(source)
    extension = get_metadata_content()
}