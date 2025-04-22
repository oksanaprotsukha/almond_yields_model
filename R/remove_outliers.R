#' Remove outliers from the dataframe for clean plotting
#'
#' @param df 
#' @param group_var 
#' @param target_var 
#'
#' @return
#' @export
#'
#' @examples
remove_outliers <- function(df, group_var, target_var) {
  df %>%
    group_by({{ group_var }}) %>%
    filter(between({{ target_var }},
                   quantile({{ target_var }}, 0.25) - 1.5 * IQR({{ target_var }}),
                   quantile({{ target_var }}, 0.75) + 1.5 * IQR({{ target_var }}))) %>%
    ungroup()
}