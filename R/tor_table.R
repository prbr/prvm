#' Generate table of tornado chart
#'
#' @param x Drawings
#' @param output vector of output variable
#' @param inputs list of vector of input variables
#' @param type type of sensitivity analysis (e.g. c2v: contribution to variance, mean impact)
#' @param top if integer x is supplied on show top x variables
#' @keywords sensitivity, tornado
#' @export
#' @examples
#' sims <- gen_draws(df)
#' tor_table(sims,mc_sum(sims))

tor_table = function(inputs, output, type = "c2v",top = F){

  contr2var = function(x,alt=F){
    v = x^2
    cv = sapply(v, function(i){
      (i -min(v,na.rm=T))/(max(v,na.rm=T)-min(v,na.rm=T))
    })
    100*cv / sum(cv,na.rm=T)

  }

  cor_df = data.frame(as.data.frame(inputs),output)
  cor_vector = cor(cor_df)[,ncol(cor_df)]
  c2v = contr2var(cor_vector[-length(cor_vector)])
  c2v_df = data.frame(var = names(c2v),
                      c2v = round(c2v,2),
                      stringsAsFactors = F)
  c2v_df = c2v_df[order(c2v,decreasing = T),]
  c2v_df$var = factor(c2v_df$var, levels = rev(c2v_df$var))
  row.names(c2v_df) = NULL

  if (!!top) c2v_df = c2v_df[1:top,]
  c2v_df
}
