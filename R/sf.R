library(sf)

segments_to_sf = function(parsed) {
    dataf = pluck(parsed, 'segments', 'segments') |> 
        map(pluck, 'locations', 'locations') |> 
        modify_depth(1, map, as_tibble) |> 
        modify_depth(1, bind_rows) |> 
        bind_rows(.id = 'segment')
    
    data_sf = st_as_sf(dataf, 
                       coords = c('long', 'lat'), 
                       crs = 4326)
    
    ## TODO: trk presumably gives elevation in WGS84, used by GPS
    ## but map displays typically convert this to EGM96, which is more accurate
    ## this bit doesn't change the elevations
    # head(datasf$elevation)
    # datasf = datasf |> 
    #     st_transform(crs = 5773)
    
    return(data_sf)
}

## TODO: Export as gpx
# datasf |> 
#     group_by(track_fid = segment) |> 
#     mutate(track_seg_id = row_number()) |> 
#     select(track_fid, track_seg_id, 
#            ele = elevation, time) |> 
#     st_write('test.gpx', layer = 'track_points')


