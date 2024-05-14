#' @title readOzoneShock
#' @description read Ozone shock
#' Data from the EAT-Lancet deepdive on Ozone shock effects on crop yields.
#' Sources:
#'  # TODO
#' @return List of magpie objects with results on countrylevel, weight, unit and description.
#' @author Jake Tommey
#' @examples
#' \dontrun{
#' readSource("OzoneShock")
#' }
#' @importFrom readxl read_xlsx

readOzoneYieldShock <- function() {
  # read-in file for ozone shocks
  x <- readxl::read_excel("Ag_CCShocks.xlsx", sheet = "Ozone")
  # convert to magclass object
  x <- as.magpie(x[, 1:4], spatial = 1)
  getYears(x) <- 2050
  return(x)
}