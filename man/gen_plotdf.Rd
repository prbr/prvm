% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_df.R
\name{gen_plotdf}
\alias{gen_plotdf}
\title{Generate combined dataframe ready to plot}
\usage{
gen_plotdf(var)
}
\arguments{
\item{var}{Either a vector or a list of vector}
}
\description{
Generate combined dataframe ready to plot
}
\examples{
a = rpert(10000,.9,1,1.1)*1000
b = rpert(10000,.8,1,1.2)*1000
df = gen_plotdf(list(a,b))
ggplot(df) + geom_density(aes(val,group=var,color=var))
}
\keyword{combine}
\keyword{data}
\keyword{frames}

