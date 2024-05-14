#' @title readOzoneShock
#' @description read Ozone shock
#' Data from the EAT-Lancet deepdive on Ozone shock effects on crop yields.
#' @return List of magpie objects with results on countrylevel, weight, unit and description.
#' @author Jake Tommey
#' @examples
#' \dontrun{
#' readSource("OzoneShock")
#' }
#' @importFrom readxl read_xls


correctOzoneYieldShock <- function(x) {
  x <- toolConditionalReplace(x, conditions = "is.na()", replaceby = 0)
  isocountries <- unique(toolGetMapping("mapCoords2Country.rds", where = "mrcommons")$iso)
  missingImport <- c("QAT", "HTI", "IRL", "CHN", "IND",
    "ZAF", "MDG", "CRI", "AUS", "HTI",
    "MYS", "ZAF", "IDN", "VEN", "CHN"
  )
  names(missingImport) <- c("BHR", "DOM", "GBR", "HKG", "LKA",
    "LSO", "MUS", "NIC", "NZL", "PRI",
    "SGP", "SWZ", "TLS", "TTO", "TWN"
  )
  x <- toolCountryFill(x, fill = 0.0, countrylist = isocountries, verbosity = 0, map = missingImport)
  return(x)
}