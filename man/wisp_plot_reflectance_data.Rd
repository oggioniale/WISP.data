% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{wisp_plot_reflectance_data}
\alias{wisp_plot_reflectance_data}
\title{Create a plot of reflectance data}
\usage{
wisp_plot_reflectance_data(data)
}
\arguments{
\item{data}{A \code{tibble} obtained by any of the functions provided by this
package: wisp_get_reflectance_data(), or after QC and SR removal operations.}
}
\value{
description A plotly object with the spectral signatures of the reflectance
data.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
This function return a plotly of each spectral signature measured by a
WISPstation.
}
\examples{
# example code
\dontrun{
## Not run:
WISP.data::wisp_plot_reflectance_data(data = reflect_data_sr)
}
## End (Not run)

}
\author{
Alessandro Oggioni, phD \email{oggioni.a@irea.cnr.it}

Nicola Ghirardi, phD \email{nicola.ghirardi@cnr.it}
}
