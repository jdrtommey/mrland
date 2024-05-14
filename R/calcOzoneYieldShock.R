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

calcOzoneYieldShock <- function(
  weighting = "totalcrop",
  marginal_land = "magpie" #nolint
) {
  shocks <- readSource("OzoneYieldShock", convert = "onlycorrect")
  shocks <- add_columns(shocks, addnm = "y2020", dim = 2, fill = 0)
  # time interpolate the shock
  shocks <- magclass::time_interpolate(
    shocks,
    interpolated_year = seq(1965, 2150, 5),
    integrate_interpolated_years = TRUE,
    extrapolation_type = "constant"
  )

  mapping <- mrcommons::toolGetMappingCoord2Country()
  mapping$coordiso <- paste(mapping$coords, mapping$iso, sep = ".")
  shocks <- toolAggregate(shocks, mapping, from = "iso", to = "coordiso")
  getSets(shocks) <- c("x.y.iso", "year", "rcp.crop")

  # create a MAgPIE object with the correct shape.
  cropNames <- toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral", where = "mappingfolder")$MAgPIE
  yieldShock <- new.magpie(
    cells_and_regions = getCells(shocks),
    years = getYears(shocks),
    names = c(paste0("rcp2p6.", cropNames), paste0("rcp7p0.", cropNames))
  )

  yieldShock[, , "tece"] <- shocks[, , "Wheat"]
  yieldShock[, , "soybean"] <- shocks[, , "soybean"]
  yieldShock[, , "rice_pro"] <- shocks[, , "Rice"]
  yieldShock[, , "sugr_beet"] <- shocks[, , "sugarbeet"]
  yieldShock[, , "maiz"] <- shocks[, , "Maize"]
  otherCrops <- yieldShock[, , c("tece", "soybean", "rice_pro", "sugr_beet", "maiz"), invert = TRUE]
  yieldShock[, , getItems(otherCrops, dim = 3)] <- (dimSums(shocks, dim = 3) / length(getItems(shocks, dim = 3)))
  yieldShock[, , "pasture"] <- 0

  getSets(yieldShock) <- c("x", "y", "iso", "year", "rcp", "crop")

  cropAreaWeights <- calcOutput(
    "YieldsWeight",
    yields = yieldShock,
    weighting = weighting,
    marginal_land = marginal_land
  )

  yieldShock <- yieldShock[, , intersect(getItems(yieldShock, dim = 3), getItems(cropAreaWeights, dim=3))]
  return(
    list(
      x = yieldShock,
      weight = cropAreaWeights,
      unit = "t per ha",
      description = "percentage yield shock due to ozone",
      isocountries = FALSE
    )
  )
}
