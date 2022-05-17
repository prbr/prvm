#' Generate histogram for one mc variable
#'
#' @param var Drawings
#' @param name Name of the variable
#' @param fill Color (RGB) of the inbound fill
#' @keywords histogram
#' @export
#' @examples
#' sims <- gen_draws(df)
#' mc_hist(mc_sum(sims))

mc_hist <- function(mc_var,name = "Variable",fill = '#E00034',unit = "date"){

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
      x = hist(v)$mids,
      y = hist(v)$counts,
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
    geom_bar(
      fill = "#253746",
      colour = NA,
      alpha = .8,
      stat = "identity"
    ) +
    xlab(name) + ylab('Density') +
    theme_bw()

  if (nrow(inbound_df)) {
    p = p + geom_bar(
      data = inbound_df,
      fill = fill,
      stat = "identity",
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
      size = 3) + scale_x_continuous(labels = format_lab)

  }
  p



}
