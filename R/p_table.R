#' Generate table of percentiles
#'
#' @param x Drawings
#' @param step number of percentiles needed
#' @param reference reference cost for contingency
#' @keywords percentiles
#' @export
#' @examples
#' sims <- gen_draws(df)
#' p_table(mc_sum(sims))

p_table = function(x,step = 10,reference = NULL,unit = 'million') {

  pseq = seq.int(0,100,by=step)/100
  p_labels = sapply(pseq, function(x)
    paste0("P", round(100 * x, 0)))
  p_values = quantile(x, pseq)
  if (!is.null(reference)) {
    percent = paste0(round(100 * (p_values - reference) / reference, 2), " %")
    contingency = round(p_values - reference, 2)
  } else {
    percent = NA
    contingency = NA
  }

  if (unit == "million") {
    format_lab = function(x) formatMil(x,2)
    format_dev = function(x) formatMil(x,2)} else
      if (unit=='date') {
        format_lab = function(x) format.Date(as.Date(x,origin="1970-01-01"),format ="%d-%b-%y" )
        format_dev = function(x) paste(round(x,1),"days")
      } else {
        format_lab = round
        format_dev = function(x) x
      }


  data.frame(
    Percentiles = p_labels,
    Value = format_lab(p_values),
    Deviation = percent,
    Contingency = format_dev(contingency),
    stringsAsFactors = F
  )

}

