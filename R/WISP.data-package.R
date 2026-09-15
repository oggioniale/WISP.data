#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

#' Package startup
#'
#' Registers the custom "NTU" (Nephelometric Turbidity Unit) with the
#' 'units' package once, when WISP.data is loaded, instead of doing so
#' inside `wisp_calc_Novoa_TUR()` at every call. NTU is not part of the
#' standard UDUNITS database, so it must be declared before it can be used
#' with `units::set_units()`. This is a one-time, session-scoped
#' registration in the 'units' package's internal unit table; it does not
#' install any software and does not write anything to disk.
#'
#' @param libname A `character`. The library path where the package is
#' installed (passed automatically by R).
#' @param pkgname A `character`. The name of the package being loaded
#' (passed automatically by R).
#' @return No return value, called for side effects (registers the "NTU"
#' unit with the 'units' package).
#' @noRd
.onLoad <- function(libname, pkgname) {
  has_ntu <- tryCatch({
    units::as_units("NTU")
    TRUE
  }, error = function(e) FALSE)
  
  if (!has_ntu) {
    units::install_unit(
      symbol = "NTU",
      name = "Nephelometric Turbidity Unit"
    )
  }
}