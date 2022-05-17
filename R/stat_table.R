#' Generate table of statistics
#'
#' @param x Drawings
#' @param reference reference cost for contingency
#' @keywords statistics
#' @export
#' @examples
#' sims <- gen_draws(df)
#' stat_table(mc_sum(sims))

stat_table = function(x,reference = NULL, unit = 'million', show_odd = F){

  s_labels = c("Mean","Median","Std Dev")
  s_values = c(mean(x),median(x),sd(x))
  if (!is.null(reference)) {
    percent = c(paste0(round(100 * (s_values[1:2] - reference) / reference,2)," %"),
                NA)
  } else percent = NA

  if (unit == "million") {
    format_lab = function(x) formatMil(x,2)
    format_dev = function(x) formatMil(x,2)} else
      if (unit=='date') {
        format_lab = function(x) format.Date(as.Date(x,origin="1970-01-01"),format ="%d-%b-%y" )
        format_dev = function(x) paste(round(x,1),"days")
      } else {
        format_lab = round
      }

  if (!is.null(reference)){
    odd = round(100*length(which(x<reference+1))/length(x),1)
    if (odd < 1) odd = " < 1 %" else
      odd = paste0(odd, " %")
    odd
  }

  if (show_odd){
    s_labels = c(s_labels,"Deterministic Odd")
    s_values = c(format_lab(mean(x)),format_lab(median(x)),format_dev(sd(x)),odd)
   percent = c(percent,NA)
    }  else  {
    s_values = c(format_lab(mean(x)),format_lab(median(x)),format_dev(sd(x)))
    }
  data.frame(Statistic = s_labels,
             Value = s_values,
             Deviation = percent,
             stringsAsFactors = F)
}
