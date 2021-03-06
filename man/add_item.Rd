% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irt_scale.R
\name{add_item}
\alias{add_item}
\title{Add or replace an item to a scale}
\usage{
add_item(scale, item, replace = FALSE)
}
\arguments{
\item{scale}{An irt_scale object}

\item{item}{An irt_item to add to scale}

\item{replace}{Set to overwrite an already existing item}
}
\value{
A new scale with the item added
}
\description{
\code{add_item} returns a new scale with item added
Will not add items with less than 2 levels and warn instead.
Will not add items with the same number as item already in scale unless replace is set to TRUE
}
\keyword{internal}
