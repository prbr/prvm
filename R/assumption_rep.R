#' Generate Report
#'
#' @param var Mc variable
#' @param reference reference value for contingency
#' @keywords statistics
#' @export
#' @examples
#' var = rpert(10000,.8,1,1.2)*5000000
#' gen_report(var)

assumption_rep = function(sims,reference,folder = getwd()){

  template <- system.file("report/assumption_report.Rmd",package = "prvm")
  path = paste0(folder,"/sims.Rdata")
  save(sims,file=path)
  output_file = paste0(folder,"/assumption_report.docx")
  rmarkdown::render(template,
                    output_file = output_file,
                    params=list(path=path))

}
