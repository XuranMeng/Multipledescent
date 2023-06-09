% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/syseq.R
\name{syseq}
\alias{syseq}
\title{syseq}
\usage{
syseq(m, Values)
}
\arguments{
\item{m, }{a vector with length equal to the number of activation functions+1.}

\item{Values}{The output from getvalue.}
}
\value{
a vector
}
\description{
The implicit functions. Note that we eliminate the imaginary part and give the equation which the fixed points are real numbers larger than 0.
}
\details{
It gives the implicit functions. syseq(m)=m gives the statinary fixed points.
}
