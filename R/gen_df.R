#' Generate combined dataframe ready to plot
#'
#' @param var Either a vector or a list of vector
#' @keywords combine data frames
#' @export
#' @examples
#' a = rpert(10000,.9,1,1.1)*1000
#' b = rpert(10000,.8,1,1.2)*1000
#' df = gen_plotdf(list(a,b))
#' ggplot(df) + geom_density(aes(val,group=var,color=var))

gen_plotdf = function(var) {
  s = substitute(var)
  names = deparse(s)
  if (grepl("list(",names,fixed=T)) names <- sub("\\(.", "",s)[-1]
  if (length(names) == 1) {
    data.frame(val = var,
               var = names)
  } else{
    l_df= lapply(1:length(names), function(x)
      data.frame(val = var[[x]],
                 var = names[[x]]))
    do.call(rbind,l_df)
  }

}
