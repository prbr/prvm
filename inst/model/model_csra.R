
#Cost Risk Analysis for MDP2 FPU SoW
#setwd("M:/departments/R&IM/Project Risk and Value/Projects/070953T001 - 2655-16 BP MAD DOG 2 FPU/10. Probabilistic Risk Analysis/Cost/model")

library(prvm)
library(thgenesis)
library(dplyr)
library(ggplot2)
library(mc2d)
library(plotly)

#Cost Uncertainty--------

#loading of cost ranges
df = read.csv("input3.csv",stringsAsFactor= F)

#separation of line items with Qty*Rate input vs. Direct Cost range
var_cost = is.na(df$Qty)|df$Qty==0
var_qrate = !var_cost

# sample for variable w/ rates and qty's
bound = 20
qty_draws = gen_draws(df[var_qrate,],
                      Low="Qty.L",
                      Medium = "Qty.ML",
                      High = "Qty.H",bound=bound)

rate_draws = gen_draws(df[var_qrate,],
                      Low="Rate.L",
                      Medium = "Rate.ML",
                      High = "Rate.H",bound=bound)

qrate_draws = lapply(1:length(qty_draws),function(x){
  cost = qty_draws[[x]]*rate_draws[[x]]
})

names(qrate_draws) <- paste(df[var_qrate,"Item"], 
                            df[var_qrate,"Group"],
                            sep = " - ")

# sample for var with cost ranges only
cost_draws = gen_draws(df[var_cost,],
                       Low="Cost.L",
                       Medium = "Cost.ML",
                       High = "Cost.H",
                       bound= bound)

names(cost_draws) <- paste(df[!var_qrate,"Item"], 
                            df[!var_qrate,"Group"],
                            sep = " - ")

# combine all uncertainty variables

unc_draws = c(qrate_draws,cost_draws)
total_unc = mc_sum(unc_draws)
unc_plot = mc_plot(total_unc,fill=gen_colors()[4])
unc_plot
#ggsave("unc_plot.png",unc_plot,type="cairo-png")

#External Risks--------

#load external risk file
df_ext = read.csv("ExRisks_2016-05-25.csv",stringsAsFactor= F) %>%
  filter(Include)

ext_draws = gen_draws(df_ext,Medium = "Med",
          Prob = "Prob")

ext_draws = lapply(ext_draws,function(x) x*10^6)
names(ext_draws) = paste("Risk",df_ext$RiskIndex)

total_ext = mc_sum(ext_draws)
ext_plot = mc_plot(total_ext,fill=gen_colors("secondary")[2])
ext_plot
#ggsave("ext_plot.png",ext_plot,type="cairo-png")

#Combine uncertainty & External Risks--------
total_ref = sum(df$USD)

all_draws = c(unc_draws,ext_draws)

total_draws = mc_sum(all_draws)
mc_plot(total_draws)

#gen_report(total_draws,total_ref)

#Breakdown by region--------

kl_ref = sum(df$USD[!grepl("Houston",df$Group)])
tpna_ref = sum(df$USD[grepl("Houston",df$Group)])
tpna_ratio = tpna_ref/(kl_ref+tpna_ref)

Houston_cost = mc_sum(unc_draws[grepl("Houston",names(unc_draws))]) +
  tpna_ratio*total_ext
tpna_plot = mc_plot(Houston_cost,"TPNA Cost")
#gen_report(Houston_cost,tpna_ref)

KL_cost = mc_sum(unc_draws[grepl("KL",names(unc_draws))]) +
  (1-tpna_ratio)*total_ext

kl_plot = mc_plot(KL_cost,"KL Cost")
#gen_report(KL_cost,kl_ref)

compare_pdf(list(KL_cost,Houston_cost))

#Tornado Chart overall--------

tor_df = tor_table(all_draws,mc_sum(all_draws),top=10)

t_plot = tor_plot(tor_df,abrv = 50)

ggplotly(t_plot)

#Tornado Chart Uncertainty-------

unc_tdf = tor_table(unc_draws,total_unc,top=10)

unc_tplot = tor_plot(unc_tdf,abrv = 50)

#Tornado Chart Ext Risks----------

cor_df = data.frame(ext_draws,total_ext)
unc_tdf = tor_table(unc_draws,total_unc,top=10)

unc_tplot = tor_plot(unc_tdf,abrv = 50)


#Tornado chart (avg $ contribution to contingency)
cont_df = data.frame(var = names(ext_draws),
                     mean = sapply(ext_draws, mean)/10^6)

#Alt case (+15% to Procurement cost)----------

 ####df = read.csv("archive/input_alt.csv",stringsAsFactor= F)

# Sensitivity by cost package -----------

#group line items by category / cost packages

group_category = "Category"

pkg_df = df %>% group_by_(group_category) %>%
  summarize(sum(USD))


list_cat =  lapply(levels(as.factor(df[,group_category])),
                   function(x)
                     df[,group_category] == x)

df$qrate = sapply(1:nrow(df), function(x){
  if (x %in% which(var_qrate)) which(which(var_qrate)==x) else 0})


df$cost = sapply(1:nrow(df), function(x){
  if (x %in% which(var_cost)) which(which(var_cost)==x) else 0})

pkg_draws = lapply(levels(as.factor(df$Category)), function(x){
  id_qrate = filter(df,eval(df[,group_category] ==x & df$qrate >0))$qrate
  id_cost = filter(df,eval(df[,group_category] == x & df$cost >0))$cost
  if (is.null(id_qrate))
    mc_sum(cost_draws[id_cost])
  if (is.null(id_cost))
    mc_sum(qrate_draws[id_qrate])
  if (!is.null(id_qrate)&!is.null(id_cost))  
    mc_sum(c(qrate_draws[id_qrate],cost_draws[id_cost]))
})

names(pkg_draws) = levels(as.factor(df[,group_category]))
                          
#tornado chart for pkgs

cor_df = data.frame(pkg_draws,total_draws)
pkg_tdf = tor_table(pkg_draws,total_draws,top=10)
unc_tplot = tor_plot(pkg_tdf,abrv = 50)


pkg_percentiles = lapply(1:length(pkg_draws),function(x){
  p_table(pkg_draws[[x]],
          reference = unlist(pkg_df[pkg_df[,1]==names(pkg_draws)[x],2]))[c(5,7,8),3]
}
  )

names(pkg_percentiles) = levels(as.factor(df$Category))
View(pkg_percentiles)


#overlay chart

assumption_list = all_draws
assumption_df = data.frame(
  Name = names(assumption_list),
  Min = sapply(assumption_list, function(x)
    quantile(x, 0)),
  P10 = sapply(assumption_list, function(x)
    quantile(x, 0.1)),
  P20 = sapply(assumption_list, function(x)
    quantile(x, .2)),
  P80 = sapply(assumption_list, function(x)
    quantile(x, 0.8)),
  P90 = sapply(assumption_list, function(x)
    quantile(x, 0.9)),
  Max = sapply(assumption_list, function(x)
    quantile(x, 1))
)
row.names(assumption_df)<- NULL     
