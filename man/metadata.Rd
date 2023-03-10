% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata.R
\name{metadata}
\alias{metadata}
\alias{get_metadata}
\alias{get_metadata_content}
\alias{get_metadata_entry}
\alias{get_metadata_extensions}
\alias{get_metadata_extension}
\title{Metadata structures}
\usage{
get_metadata(source)

get_metadata_content(source)

get_metadata_entry(source)

get_metadata_extensions(source)

get_metadata_extension(source)
}
\arguments{
\item{source}{Raw track file contents to be parsed}
}
\value{
Nested list; a single metadata entry is a flat list of {key: value} pairs.
}
\description{
Parse metadata structures
}
\details{
Top-level metadata structures are divided into entries and extensions.  The exact layout differs slightly depending on the file version.  Data will be read from the current read position of \code{source}, ie, \code{pos(source)}.
}
