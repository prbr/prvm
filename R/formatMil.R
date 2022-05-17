#' Format $ numbers to Millions
#'
#' @param x number to format
#' @param digits number of digits after comma
#' @keywords percentiles
#' @export
#' @examples
#' formatMil(352458967)


formatMil = function (x,digits =1){
  paste0(prettyNum(round(x/10^6,digits),big.mark=",")," MM")
}
