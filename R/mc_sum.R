#' Sum list of variables
#'
#' @param list List of drawings
#' @keywords sum, drawings
#' @export
#' @examples
#' sims <- gen_draws(df)
#' mc_sum(sims)

mc_sum <- function(list){
  Reduce("+",list)
}
