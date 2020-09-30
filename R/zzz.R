# nocov start
.onLoad <- function(...) {
  s3_register("fabletools::features", "SharedTsibbleData")
  s3_register("fabletools::model", "SharedTsibbleData")
  s3_register("fabletools::components", "SharedTsibbleData")
  s3_register("fabletools::forecast", "SharedTsibbleData")

  s3_register("dplyr::mutate", "SharedTsibbleData")
  s3_register("dplyr::filter", "SharedTsibbleData")
}
# nocov end
