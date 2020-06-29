# nocov start
.onLoad <- function(...) {
  s3_register("fabletools::features", "SharedTsibbleData")
  s3_register("fabletools::model", "SharedTsibbleData")
  s3_register("fabletools::components", "SharedTsibbleData")
  s3_register("fabletools::forecast", "SharedTsibbleData")
}
# nocov end
