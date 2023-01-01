library(tidyverse)
library(here)
library(glue)
library(assertthat)
library(magrittr)

list.files(path = 'R', pattern = '.R') %>%
    here('R', .) |> 
    walk(source)

## Read in raw file ----
file = here('test_data', 'track_20210417_094737.trk')
raw = read_file_raw(file) |> 
    reset_pos()

## File header ----
## Start-of-file constant: 50 50 0E 01
get_bytes(raw, 4)
## header size (bytes before {Waypoints})
header_size = get_int(raw)
header_size

## Metadata ----
## Technical metadata
# debugonce(get_metadata_entry)
technical_metadata = get_metadata(raw)

## User metadata
# debugonce(get_string)
# set_pos(raw, 77)
user_metadata = get_metadata(raw)
user_metadata


## Waypoints ----
# debugonce(get_location)
# set_pos(raw, 251)
waypoints = get_waypoints(raw)

## Track segments ----
# debug(get_location_value)
# set_pos(raw, 261)
segments = get_segments(raw)
