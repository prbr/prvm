#' Generate Draws with betapert for the input dataframe given
#'
#' @param data The input dataframe
#' @param Low name of the column for low, same for Medium, High, etc.
#' @param draws number of draws defaults to 50000
#' @param bound percentile of the low values in the assumption distribution, e.g. 10 means the low is the P10 and High is P90, default is 0
#' @keywords multiple, draws, dataframe
#' @export
#' @examples
#' gen_draws(df,Medium = "Nominal")


gen_draws <- function(data,
                      Low = "Low",
                      Medium = "Medium",
                      High = "High",
                      Prob = NULL,
                      draws = 50000,
                      bound = 0,
                      refine = F,
                      positive = F) {
  new_range = function(min, mode, max, alpha) {
    nmin = mode - (min - mode) / alpha
    nmax = mode - (max - mode) / alpha
    data.frame(Low = nmin, mode, High = nmax)
  }


  alpha = qpert(bound / 100)

  if (refine) {
    sens_alpha = sapply(0:10, function(x)
      alpha * (1 - (x - 5) / 10))
    sens_dfs = lapply(sens_alpha, function(x) {
      new_df = new_range(data[, Low],
                         data[, Medium],
                         data[, High], x)
    })

    lapply(1:nrow(data), function(var) {
      dists = sapply(sens_dfs, function(x, var) {
        d0 = mc2d::rpert(5000,
                         min = x[var, 1],
                         mode = x[var, 2],
                         max = x[var, 3])
        qs = quantile(d0, c(.1, .9))
        (qs[[1]] - data[var, Low]) ^ 2 + (qs[[2]] - data[var, High]) ^
          2
      })


      data[var, ] <<- sens_dfs[[which(dists == min(dists))]][var, ]
    })

  } else {
    new_df = new_range(data[, Low],
                       data[, Medium],
                       data[, High], alpha)
    data[, Low] = new_df[, 1]
    data[, Medium] = new_df[, 2]
    data[, High] = new_df[, 3]
  }


  get_inputs = function(data,row){

    if ("distribution" %in% colnames(data)){
      distribution = data[row,"distribution"]
    } else distribution = "betapert"

    if (is.null(Prob)) prob = 1 else prob = as.numeric(data[row, Prob])

    list(
      min = as.numeric(data[row, Low]),
      mode = as.numeric(data[row, Medium]),
      max = as.numeric(data[row, High]),
      distr = distribution,
      prob = prob
    )

  }

  draw <- function(draws,inputs) {
    if (inputs["distr"] == "betapert"){
      mc2d::rpert(draws,
                  min = as.numeric(inputs["min"]),
                  mode = as.numeric(inputs["mode"]),
                  max = as.numeric(inputs["max"])
      )*rbinom(draws, 1, as.numeric(inputs["prob"]))
    } else if (inputs["distr"] == "uniform"){
      runif(draws, min = as.numeric(inputs["min"]),
            max = as.numeric(inputs["max"]))*
        rbinom(draws, 1, as.numeric(inputs["prob"]))
    }
  }

  draw_inputs = lapply(1:nrow(data),
                       function(x) get_inputs(data,x))

  sims <- lapply(draw_inputs, function(x)
    draw(draws, x)
  )

  if (positive)
    sims = lapply(sims, function(x)
      pmax(0, x))

  sims
}
