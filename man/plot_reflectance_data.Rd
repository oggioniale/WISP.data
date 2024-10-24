% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{plot_reflectance_data}
\alias{plot_reflectance_data}
\title{Create a plot of reflectance data}
\usage{
plot_reflectance_data(data)
}
\arguments{
\item{data}{A \code{tibble}. From wisp_get_reflectance_data() function.}
}
\description{
This function return a plotly of each spectral signature measured by a WISP
station.

\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\examples{
# example code
WISP.data::plot_reflectance_data(data = reflec_data)

}
\author{
Alessandro Oggioni, phD \email{oggioni.a@irea.cnr.it}

Nicola Ghirardi, phD \email{nicola.ghirardi@cnr.it}
}
