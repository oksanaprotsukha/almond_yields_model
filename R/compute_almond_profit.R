#' computes profit based on almond yield
#' @param  price ($/lb)
#' @param  almond_yield (tons/acre)
#' @param  year (observation_year)
#' @param discount rate (default 0.05) 5% based on Sgroi et al., 2015; Torres et al., 2016. https://www.sciencedirect.com/science/article/pii/S0308521X20300366
#' @return data frame with estimate of profit
compute_crop_profit <- function(price_per_lb, crop_yield, year, fixed, opex, discount = 0.05) {
  # make sure values are reasonable
  if (length(crop_yield) < 1) {
    return(NA)
  }
  
  # crop yield cannot be negative
  if (min(crop_yield) < 0) {
    return(NA)
  }
  
  # handle scalar or vector price input
  if (length(price_per_lb) == 1) {
    price_per_lb <- rep(price_per_lb, length(crop_yield))
  } else if (length(price_per_lb) != length(crop_yield)) {
    stop("Length of price_per_lb must be either 1 or match length of crop_yield")
  }
  
  # generate a unique identifier or scenario number
  scen <- seq(from = 1, to = length(crop_yield))
  yearprofit <- data.frame(scen = scen, crop_yield = crop_yield, year = year, price = price_per_lb)
  yearprofit$revenue <- yearprofit$crop_yield * yearprofit$price * 2000 # convert to price per tone
  yearprofit$cost <- fixed + opex
  
  
  # note how discount is passed through to this function
  # remember to normalize the year to start year e.g the first year
  yearprofit <- yearprofit %>%
    mutate(adj_revenue = compute_NPV(value = revenue, time = year - year[1], discount = discount),
           adj_cost = compute_NPV(value = cost, time = year - year[1], discount = discount),
           profit = adj_revenue - adj_cost
           )
  
  return(yearprofit)
}
