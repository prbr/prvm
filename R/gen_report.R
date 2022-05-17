#' Generate Report
#'
#' @param var Mc variable
#' @param reference reference value for contingency
#' @keywords statistics
#' @export
#' @examples
#' var = rpert(10000,.8,1,1.2)*5000000
#' gen_report(var)

gen_report = function(var,reference,folder = getwd()){

template <- system.file("report/report_template.Rmd",package = "prvm")
save(var,file="var.Rdata")
path = paste0(folder,"/var.Rdata")
output_file = paste0(folder,"/result_report.docx")
rmarkdown::render(template,
                  output_file = output_file,
                  params=list(path=path,
                              ref_Total=reference))

}
