library(tidyverse)
library(here)
library(glue)
library(assertthat)
library(magrittr)

library(sf)
library(leaflet)

list.files(path = 'R', pattern = '.R') %>%
    here('R', .) |> 
    walk(source)

## Read in raw file ----
file = here('test_data', 'track_20210417_094737.trk')
parsed = parse_trk(file)
dataf = segments_to_sf(parsed)

## Plot with leaflet to check coordinates
dataf |> 
    summarize(ele = list(elevation),
              time = list(time),
              do_union = FALSE) |>
    st_cast('LINESTRING') |>
    leaflet() |> 
    addPolylines() |> 
    addProviderTiles('OpenTopoMap', group = 'OpenTopoMap')
