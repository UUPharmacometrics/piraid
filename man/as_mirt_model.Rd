% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/piraid_mirt_interop.R
\name{as_mirt_model}
\alias{as_mirt_model}
\title{Convert piraid model to mirt model}
\usage{
as_mirt_model(model, use_data = FALSE)
}
\arguments{
\item{model}{A piraid irt_model}

\item{use_data}{Whether to use the associated dataset or create pseudo data}
}
\value{
An mirt SingleGroupClass
}
\description{
This function converts a piraid irt_model to an mirt SingleGroupClass to allow the subsequent use of mirt functions
}
