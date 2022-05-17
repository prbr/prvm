#' Generate histogram for one mc variable
#'
#' @param var Drawings
#' @param name Name of the variable
#' @param fill Color (RGB) of the inbound fill
#' @keywords histogram
#' @export
#' @examples
#' sims <- gen_draws(df)
#' mc_plot(mc_sum(sims))

mc_plot = function(mc_var,name = "Variable",fill = '#E00034',unit = "million", reference = NULL){

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

  # generate df to draw line and text on boundaries
  plot_bound = function(vec, side = 1) {
    df = data.frame(x = bounds(vec)[side],
                    y = 0,
                    yend = max(get_df(vec)$y))
  }


  df = get_df(mc_var, name)

  inbound_df = subset(df, x > bounds(mc_var)[1] &
                        x < bounds(mc_var)[2] &
                        var == name)

  df_low = data.frame(plot_bound(mc_var, 1),
                      var = name,
                      stringsAsFactors = F)


  df_high = data.frame(plot_bound(mc_var, 2),
                       var = name,
                       stringsAsFactors = F)

  p = ggplot(df, aes(x, y)) +
    geom_density(
      fill = "#253746",
      colour = NA,
      alpha = .8,
      stat = "identity"
    ) +
    xlab(name) + ylab('Density') +
    theme_bw()

  if (nrow(inbound_df)) {
    p = p + geom_ribbon(
      data = inbound_df,
      aes(ymax = y),
      fill = fill,
      ymin = 0,
      colour = NA,
      alpha = .8
    ) +
      geom_segment(aes(
        x = x,
        xend = x,
        y = y,
        yend = yend
      ),
      data = df_low,
      size = .1) +
      geom_segment(aes(
        x = x,
        xend = x,
        y = y,
        yend = yend
      ),
      data = df_high,
      size = .1)

    if (unit == "million") {
      format_lab = formatMil } else
        if (unit=='date') {
          format_lab = function(x) format.Date(as.Date(x,origin="1970-01-01"),format ="%d-%b-%y" )
        } else {
          format_lab = round
        }

    #if (unit == "million") {
      p = p +
        geom_label(aes(
          x = x,
          y = .75 * (y + yend),
          label = paste("P5: ", format_lab(x))
        ),
        data = df_low,
        size = 3) +
        geom_label(aes(
          x = x,
          y = .75 * (y + yend),
          label = paste("P95: ", format_lab(x))
        ),
        data = df_high,
        size = 3)
      if (!is.null(reference))
        p = p +
        geom_segment(
          x = reference,
          xend = reference,
          y = df_low$y,
          yend = df_low$yend,
        size = .1,
        color = "#E00034") +
        geom_label(
          x = reference,
          y = .5 * (df_low$y + df_low$yend),
          label = paste("Reference: ", format_lab(reference)),
          color = "#E00034",
        size = 3)

        p = p + scale_x_continuous(labels = format_lab)
    # } else {
    #   p = p +
    #     geom_label(aes(
    #       x = x,
    #       y = .75 * (y + yend),
    #       label = paste("P5: ", round(x))
    #     ),
    #     data = df_low,
    #     size = 5) +
    #     geom_label(aes(
    #       x = x,
    #       y = .75 * (y + yend),
    #       label = paste("P95: ", round(x))
    #     ),
    #     data = df_high,
    #     size = 5)
    # }
  }
  p

  #     geom_segment(aes(x = ref_cost,xend =1.9*10^9,y = y,yend=yend,frame=var),data=df_high,
  #                  size =.1,color='#253746'
  #     )+
  #     geom_text(aes(x = .95*ref_cost,y = .75*(y + yend)),data=df_high, size = 5,
  #               label = "Revenue",color='#253746'
  #     )

}
