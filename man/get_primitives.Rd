% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/primitives.R
\name{get_primitives}
\alias{get_primitives}
\alias{get_byte}
\alias{get_int}
\alias{get_bool}
\alias{get_long}
\alias{get_coord}
\alias{get_time}
\alias{get_double}
\alias{get_raw}
\alias{get_string}
\title{Functions to read primitives}
\usage{
get_byte(source, offset = pos(source), move_pos = TRUE)

get_int(source, offset = pos(source), move_pos = TRUE)

get_bool(source, offset = pos(source), move_pos = TRUE)

get_long(source, offset = pos(source), move_pos = TRUE)

get_coord(source, offset = pos(source), move_pos = TRUE)

get_time(source, offset = pos(source), move_pos = TRUE)

get_double(source, offset = pos(source), move_pos = TRUE)

get_raw(source, size, offset = pos(source), move_pos = TRUE)

get_string(source, size = NULL, offset = pos(source), move_pos = TRUE)
}
\arguments{
\item{source}{Data source (eg, raw trk file contents)}

\item{offset}{Offset in bytes to start reading data}

\item{move_pos}{Should the read location be updated?}

\item{size}{Length of string to be read}

\item{len}{Number of bytes to be read}
}
\value{
Varies by function
}
\description{
Read primitive datatypes from trk files
}
\details{
By default, data is read off from the current read location of \code{source} (\code{pos(source)}) and the read location is updated.  \code{get_time()} parses data to POSIXct.
}
