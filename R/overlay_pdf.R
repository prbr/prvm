#' Generate histogram for one mc variable
#'
#' @param mc_vars List of Drawings
#' @param varnames Names of the variables
#' @param fill Colors (RGB) of the inbound fill
#' @keywords histogram
#' @export
#' @examples
#' sims <- gen_draws(df)
#' mc_plot(mc_sum(sims))

overlay_pdf = function(mc_vars,
                       varnames = NULL,
                       fill = '#E00034',
                       unit = "million",
                       alpha0 = .8, hide = F) {
  if (is.null(varnames))
    varnames = paste("Var", 1:length(mc_vars))

  #get percentiles of a vector for a specified conf interval
  bounds = function(d, x = .9) {
    low = (1 - x) / 2
    high = 1 - low
    c(l = quantile(d, low),
      h = quantile(d, high))
  }

  # get density values for a specified var w/ spec name
  get_df = function(v, name = "var") {
    data.frame(
      x = density(v)$x,
      y = density(v)$y,
      var = name,
      stringsAsFactors = F
    )
  }

  #create the dataframe of percentiles
  list_df = lapply(1:length(mc_vars),
                   function(x)
                     get_df(mc_vars[[x]], varnames[x]))
  df_plot = do.call(rbind, list_df)

  #plot facetted plot of several variables

  # plot0 = ggplot(df_plot,aes(x,y)) +
  #   geom_density(fill = "#253746",colour = NA,
  #                alpha = .8,stat='identity') +
  #   xlab('Cost')

  plot0 = ggplot(df_plot, aes(x, y))
  alphas = rev(seq(alpha0,1,by = (1-alpha0)/(length(mc_vars)-1)))

  if (length(hide)>1){
    alphas = hide
  } else if (hide) {
    alphas = c(1,rep(0,length(mc_vars)-1))
  }
  for (n in 1:length(mc_vars)) {
    plot0 = plot0 +
      geom_density(
        data = subset(df_plot, var == varnames[n]),
        aes(x, y,fill=var),
       # fill = "#253746",
        colour = NA,
        alpha = alphas[n],
        stat = 'identity'
      )
    # +
    #   geom_ribbon(
    #     data = subset(
    #       df_plot,
    #       x > bounds(mc_vars[[n]])[1] &
    #         x < bounds(mc_vars[[n]])[2] &
    #         var == varnames[n]
    #     ),
    #     aes(ymax = y, fill = var),
    #     ymin = 0,
    #     colour = NA,
    #     alpha = 1
    #   )

  }

  def_colors = c("#E00034", "#0046AD", gen_colors()[4],
                 gen_colors('secondary')[5],gen_colors())

  plot0 = plot0 + #facet_grid(var ~ .) +
    scale_fill_manual(values = def_colors[1:length(mc_vars)]) +
    theme_bw() + xlab("Cost") + scale_x_continuous(labels = formatMil)
  plot0

}
