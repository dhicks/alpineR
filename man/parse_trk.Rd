% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_trk.R
\name{parse_trk}
\alias{parse_trk}
\title{Parse a trk file}
\usage{
parse_trk(path)
}
\arguments{
\item{path}{Path to the trk file to be parsed}
}
\value{
A list of the parsed file contents; exact structure depends on file version and contents.  This represents the file stream pretty directly; use \code{segments_to_sf()} to get a tidy representation.
}
\description{
Parse an AlpineQuest trk file
}
\details{
Currently only trk versions 3 (APQ 2.0 through 2.2.7c) and 4 (2.2.8c and later) are supported.  Messages will indicate numbers of metadata entries, waypoints, locations, etc., for debugging purposes.
}
