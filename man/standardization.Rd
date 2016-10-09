\name{standardization}
\alias{standardization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Standardized measures
%%  ~~function to do ... ~~
}
\description{Calculates Standardized measures
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
standardization(form.table.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form.table.object}{An object created from form.table function
%%     ~~Describe \code{form.table.object} here~~
}
}
\details{Standardized measures from total population, direct and indirect measures can be calculated.
%%  ~~ If necessary, more details than the description above ~~
}

\author{
Ehsan Karim ehsan@alumni.ubc.ca
}

\examples{
form.table.object <- form.table(Y1A1L1=150, Y1A0L1=45, Y1A1L0=20, Y1A0L0=5, Y0A1L1=300, Y0A0L1=10, Y0A1L0=40, Y0A0L0=55)
form.table.object
standardization(form.table.object)
}
