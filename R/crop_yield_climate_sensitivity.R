#' Crop yield anomaly response to climate
#'
#' @param mean_monthly_min_temp : A numeric vector representing the average minimum temperatures 
#' for each month (e.g., 12 values for a year).
#' @param total_monthly_precip : Precipitation (mm), series
#' @param crop : type of crop analysed. Available crop types: almond, avocado, oranges
#'
#' @return Y: Yield anomaly (tons per acre)
#' @export
#'
#' @examples
#'
crop_yield_climate_sensitivity <- function(mean_monthly_min_temp, total_monthly_precip, crop = 'almond') {
  if (crop == 'almond') {
    crop_yield <- (-0.015) * mean_monthly_min_temp - 0.0046 * (mean_monthly_min_temp)^2 - 
      0.07 * total_monthly_precip + 0.0043 * (total_monthly_precip)^2 + 0.28
  }
  
  # Calculate the min, max, and mean crop yields
  min_crop_yield <- min(crop_yield)
  max_crop_yield <- max(crop_yield)
  mean_crop_yield <- mean(crop_yield)
  
  # return(list(
  #   min_crop_yield = round(min_crop_yield, 2),
  #   max_crop_yield = round(max_crop_yield, 2),
  #   mean_crop_yield = round(mean_crop_yield, 2)
  # ))
  # 
  return(list(
    min_crop_yield = trunc(min_crop_yield * 100) / 100,
    max_crop_yield = trunc(max_crop_yield * 100) / 100,
    mean_crop_yield = trunc(mean_crop_yield * 100) / 100
  ))

}

