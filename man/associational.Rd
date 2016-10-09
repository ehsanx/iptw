\name{associational}
\alias{associational}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Measures of Association
%%  ~~function to do ... ~~
}
\description{Calculates measures of associations
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
associational(form.table.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form.table.object}{An object created from form.table function
%%     ~~Describe \code{form.table.object} here~~
}
}
\details{Calculates measures of associations, such as, risk difference, risk ratio, risk ratio
%%  ~~ If necessary, more details than the description above ~~
}
\author{
Ehsan Karim ehsan@alumni.ubc.ca
%%  ~~who you are~~
}
\seealso{
\code{\link{measures.calc}},\code{\link{standardization}},\code{\link{iptw}}
}
\examples{
form.table.object <- form.table(Y1A1L1=150, Y1A0L1=45, Y1A1L0=20, Y1A0L0=5, Y0A1L1=300, Y0A0L1=10, Y0A1L0=40, Y0A0L0=55)
form.table.object
associational(form.table.object)
  }
