#' Crop yield anomaly response to climate
#'
#' @param mean_monthly_min_temp : A numeric vector representing the average minimum temperatures 
#' for each month (e.g., 12 values for a year).
#' @param total_monthly_precip : Precipitation (mm), series
#' @param year : years in which observations were recorded, series
#'
#' @return Y: Yield anomaly per each observation year (tons per acre)
#' @export
#'
#' @examples
#' almond_yield_response(year_series, t_min_feb_series, p_jan_series)
#' 
#' 
almond_yield_response <- function(year, mean_monthly_min_temp, total_monthly_precip)
  {
    almond_yield <- pmax(0,
                         (-0.015) * mean_monthly_min_temp - 
                           0.0046 * (mean_monthly_min_temp)^2 -
                           0.07 * total_monthly_precip + 
                           0.0043 * (total_monthly_precip)^2 + 
                           0.28)

    almond_yield_df <- data.frame(
      year = year,
      almond_yield = almond_yield
    )
  return(almond_yield_df)
}
  