library(tidyverse)
library(here)
# library(glue)
# library(assertthat)
# library(magrittr)
library(alpineR)

# library(sf)
# library(units)
library(leaflet)

# list.files(path = 'R', pattern = '.R') %>%
#     here('R', .) |> 
#     walk(source)

## Read in raw file ----
## APQ 2.2.7c
# debugonce(parse_trk)
# dataf = parse_trk(here('test_data', 'track_20210205_112437.trk'))
    # segments_to_sf()

## APQ 2.2.8c
file = here('test_data', 'track_20210205_112437.trk')
parsed = parse_trk(file)
dataf = segments_to_sf(parsed)

## APQ 2.0.0
# file = here('test_data', 'track_20160505_115608.trk')
# parsed = parse_trk(file)
# dataf = segments_to_sf(parsed)

## TODO: APQ 1.4.22 (file version 2)
## though I only have a handful of these from 2015
# file = here('test_data', 'track_20150725_223847.trk')
# parsed = parse_trk(file)
# dataf = segments_to_sf(parsed)

## Plot with leaflet to check coordinates
dataf |> 
    to_linestring() |> 
    leaflet() |> 
    addPolylines() |> 
    addProviderTiles('OpenTopoMap', group = 'OpenTopoMap')

