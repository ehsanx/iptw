\name{measures.calc}
\alias{measures.calc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Risk measures
%%  ~~function to do ... ~~
}
\description{Calculates risk measures
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
measures.calc(r1, r0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r1}{Risk of the treated subjects
%%     ~~Describe \code{r1} here~~
}
  \item{r0}{Risk of the untreated subjects
%%     ~~Describe \code{r0} here~~
}
}
\details{Calculates risk difference, risk ratio and odds ratio
%%  ~~ If necessary, more details than the description above ~~
}


\author{
Ehsan Karim ehsan@alumni.ubc.ca
}


\examples{
r1 <- .9
r0 <- .5
measures.calc(r1,r0)
}
