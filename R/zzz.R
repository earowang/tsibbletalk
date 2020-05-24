# nocov start
.onLoad <- function(...) {
  s3_register("fabletools::features", "SharedTsibbleData")
}
# nocov end
