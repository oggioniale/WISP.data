## Resubmission

This is a resubmission. In response to the editor's comments, I have:

* Removed the redundant "Tools for" from the beginning of the `Title` field.

* Added `\value` to all `.Rd` files for exported functions, describing the
  structure/class of the returned object and what it means (including
  `wisp_runApp()`, which now documents
  `\value{No return value, called for side effects to launch the
  interactive Shiny application.}`).

* Reviewed every use of `\dontrun{}`:
  - The two remaining `\dontrun{}` examples (`wisp_get_reflectance_data()`
    and `wisp_get_reflectance_multi_data()`) require a real, private
    WISPstation account (`userid`/`pwd`) and genuinely cannot be executed
    by CRAN or by users without personal credentials; `\dontrun{}` is kept
    for these, per the CRAN policy exception for examples that cannot be
    run at all.
  - The example for `wisp_runApp()`, which launches a blocking interactive
    Shiny application, is now wrapped in `if (interactive()) {}` instead of
    `\dontrun{}`.
  - No other function used `\dontrun{}`; all other executable examples
    that take longer than a few seconds are wrapped in `\donttest{}`.

* Added `testthat` unit tests for the internal (non-exported) helper
  functions used by the Shiny app (`wisp_runApp()`): date-range
  validation, time-range string building, download filename building, and
  dataset selection for the comparison plot. The Shiny UI/server logic
  itself cannot be checked automatically by `R CMD check`, so this covers
  the testable logic extracted from it.

* Removed the default output path from `wisp_qc_reflectance_data()` and
  `wisp_sr_reflectance_data()`. When `out_dir` is not supplied and
  `save_csv = TRUE`, files are now written to `tempdir()`; nothing is
  written to the user's home filespace or working directory by default.
  Examples, tests, and vignettes only ever write to `tempdir()`.

* Removed the `units::install_unit()` call from inside
  `wisp_calc_Novoa_TUR()` (an internal, non-exported function). The custom
  "NTU" unit is now registered once in `.onLoad()` instead of on every
  function call, and no software/package is installed anywhere in the
  package's functions, examples, or vignettes.

## Test environments
* local Windows 11 x64, R 4.6.0
* win-builder (devel)

## R CMD check results
There were 0 ERRORS, 0 WARNINGS, 0 NOTE.

## Downstream dependencies
There are no downstream dependencies.