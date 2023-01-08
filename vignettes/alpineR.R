library(tidyverse)
library(here)
library(alpineR)

library(leaflet)


## Read in raw file ----
## TODO: July 2015: APQ 1.4.22 (file version 2)
## though I only have a handful of these from 2015
# file = here('test_data', 'track_20150725_223847.trk')
# parsed = parse_trk(file)
# dataf = segments_to_sf(parsed)

## May 2016: APQ 2.0.0
# file = here('test_data', 'track_20160505_115608.trk')
# parsed = parse_trk(file)
# dataf = segments_to_sf(parsed)

## Feb 2021: APQ 2.2.7c
# debugonce(parse_trk)
# dataf = parse_trk(here('test_data', 'track_20210205_112437.trk')) |> 
#     segments_to_sf()

## April 2021: APQ 2.2.8c
# file = here('test_data', 'track_20210417_094737.trk')
# parsed = parse_trk(file)
# dataf = segments_to_sf(parsed)

## December 2022: APQ 2.3.3b
dataf = parse_trk(here('test_data', 'track_20221223_103047.trk')) |> 
    segments_to_sf()

## Plot with leaflet to check coordinates
dataf |> 
    to_linestring() |> 
    leaflet() |> 
    addPolylines() |> 
    addProviderTiles('OpenTopoMap', group = 'OpenTopoMap')

## Write out
## As GPX
gps_out(dataf, here('test_data', 'test_out.gpx'))

## Only works with 1 row = 1 location format
try({
    dataf |> 
        to_linestring() |> 
        gps_out(here('test_data', 'test_out.gpx'))
})

## As Rds
write_rds(dataf, here('test_data', 'test_rds.Rds'))
dataf |> 
    to_linestring() |> 
    write_rds(here('test_data', 'test_rds.Rds'))

