# wisp_runApp function ----
message("\n---- Test wisp_runApp() ----")

test_that("Expect error if WIPS website is down", {
  nls <- curl::nslookup("wispcloud.waterinsight.nl")
  expect_type(nls, "character")
})

# skip_if_offline(host = "https://wispcloud.waterinsight.nl/")

test_that("", {

})


# wisp_get_reflectance_data function ----
message("\n---- Test wisp_get_reflectance_data() ----")

test_that("Expect error if WIPS website is down", {
  nls <- curl::nslookup("wispcloud.waterinsight.nl")
  expect_type(nls, "character")
})

# skip_if_offline(host = "https://wispcloud.waterinsight.nl/")

test_that("", {
  
})


# qc_reflectance_data function ----
message("\n---- Test qc_reflectance_data() ----")

# skip_if_offline(host = "https://wispcloud.waterinsight.nl/")

test_that("", {
  
})


# plot_reflectance_data function ----
message("\n---- Test plot_reflectance_data() ----")

# skip_if_offline(host = "https://wispcloud.waterinsight.nl/")

test_that("", {
  
})

