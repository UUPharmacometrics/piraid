% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irt_model.R
\name{irt_item_assignment_code}
\alias{irt_item_assignment_code}
\title{Generate NONMEM code for assignment of item model parameters}
\usage{
irt_item_assignment_code(model, item, next_theta, first)
}
\arguments{
\item{model}{An irt_model object}

\item{item}{An irt_item object}

\item{next_theta}{The number of the next THETA to be created}

\item{first}{Is this the first item?}
}
\value{
A code generator object
}
\description{
Generate NONMEM code for assignment of item model parameters
}
