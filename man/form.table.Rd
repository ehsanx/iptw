\name{form.table}
\alias{form.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Forming Basic Tables
%%  ~~function to do ... ~~
}
\description{
Forms the crude, stratified tables suitable for using in other functions of this package.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
form.table(Y1A1L1, Y1A0L1, Y1A1L0, Y1A0L0, Y0A1L1, Y0A0L1, Y0A1L0, Y0A0L0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y1A1L1}{Frequency of subjects who are treated (A=1), exposed to confounder (L=1) and developed the outcome (Y=1).
%%     ~~Describe \code{Y1A1L1} here~~
}
  \item{Y1A0L1}{Frequency of subjects who are not treated (A=0), but exposed to confounder (L=1) and developed the outcome (Y=1).
%%     ~~Describe \code{Y1A0L1} here~~
}
  \item{Y1A1L0}{Frequency of subjects who are treated (A=1), unexposed to confounder (L=0) and developed the outcome (Y=1).
%%     ~~Describe \code{Y1A1L0} here~~
}
  \item{Y1A0L0}{Frequency of subjects who are not treated (A=0), unexposed to confounder (L=0) and developed the outcome (Y=1).
%%     ~~Describe \code{Y1A0L0} here~~
}
  \item{Y0A1L1}{Frequency of subjects who are treated (A=1), exposed to confounder (L=1) but did not developed the outcome (Y=0).
%%     ~~Describe \code{Y0A1L1} here~~
}
  \item{Y0A0L1}{Frequency of subjects who are not treated (A=0), but exposed to confounder (L=1) and did not developed the outcome (Y=0).
%%     ~~Describe \code{Y0A0L1} here~~
}
  \item{Y0A1L0}{Frequency of subjects who are treated (A=1), unexposed to confounder (L=0) and did not developed the outcome (Y=0).
%%     ~~Describe \code{Y0A1L0} here~~
}
  \item{Y0A0L0}{Frequency of subjects who are untreated (A=0), unexposed to confounder (L=0) and did not developed the outcome (Y=0).
%%     ~~Describe \code{Y0A0L0} here~~
}
}
\details{Frequency of all possible subject combination is required to insert in this function. This function eventually generates tables suitable for using in all other functions in this package.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{data}{List frequencies}
\item{crude.data}{Combines all the frequencies in a 2x2 table of treatment vs. outcome}
\item{stratified.table}{Crude table is stratified by condounder exposure}
\item{condounder.exposed.data}{2x2 table only containing subjects exposed to confounder}
\item{condounder.unexposed.data}{2x2 table only containing subjects unexposed to confounder}
%% ...
}

\author{
Ehsan Karim ehsan@alumni.ubc.ca
%%  ~~who you are~~
}


\examples{
form.table.object <- form.table(Y1A1L1=150, Y1A0L1=45, Y1A1L0=20, Y1A0L0=5, Y0A1L1=300, Y0A0L1=10, Y0A1L0=40, Y0A0L0=55)
form.table.object
}
