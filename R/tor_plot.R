#' Generate tornado chart
#'
#' @param df Tornado dataframe
#' @param abrv Abbreviate names or not
#' @param fill Color (RGB) of the inbound fill
#' @keywords tornado, chart
#' @export
#' @examples
#' sims <- gen_draws(df)
#' tor_df = tor_table(sims,mc_sum(sims),top=10)
#' tor_plot(tor_df)

tor_plot = function(df,abrv = F,fill = '#E00034'){

  p = ggplot(df, aes(var, c2v)) +
    geom_bar(stat = "identity",fill = fill) +
    geom_label(aes(label = paste(c2v, "%")),
               position = position_nudge(y = -3),
               size = 3)+
    theme_bw() +
    xlab("Drivers")+
    ylab("Contribution to Variance (%)")+
    coord_flip()

  if (!!abrv) {
    abbreviate = function(x) base::abbreviate(x,minlength = abrv)
    p = p + scale_x_discrete(labels = abbreviate)
  }

  p

}
