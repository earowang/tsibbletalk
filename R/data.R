#' Monthly Australian domestic overnight trips
#'
#' A dataset containing the monthly overnight trips from 1998 Jan to 2019 Dec
#' across Australia.
#'
#' @format A tsibble with 80,696 rows and 5 variables:
#' * **Month**: Year month (index)
#' * **State**: States and territories of Australia
#' * **Region**: The tourism regions are formed through the aggregation of
#' Statistical Local Areas (SLAs) which are defined by the various State and
#' Territory tourism authorities according to their research and marketing
#' needs
#' * **Purpose**: Stopover purpose of visit:
#'   - "Holiday"
#'   - "Visiting friends and relatives"
#'   - "Business"
#'   - "Other reason"
#' * **Trips**: Overnight trips in thousands
#' @references [Tourism Research Australia](https://www.tra.gov.au)
#' @docType data
#' @name tourism_monthly
#' @usage tourism_monthly
#' @examples
#' data(tourism_monthly)
"tourism_monthly"
