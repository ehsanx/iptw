\name{iptw}
\alias{iptw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Standardized measures
%%  ~~function to do ... ~~
}
\description{Standardized measures are calculated by this function using inverse probability of treatment weights (IPTW).
}
\usage{
iptw(form.table.object, type = "w")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form.table.object}{An object created from form.table function
%%     ~~Describe \code{form.table.object} here~~
}
  \item{type}{Type of weighting scheme to be used
%%     ~~Describe \code{type} here~~
}
}
\details{Supported weighting types are "0" (no weighting), "w" (unstabilized weights), "sw" (stabilized weights), "wn" (normalized unstabilied weights), "swn" (normalized stabilied weights), "swn2" (experimental). 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{IPTW.measures}{IPTW measures are listed}
\item{pseudo.data}{pseudo frequency data are taulated}
\item{crude.data}{unstratified or Crude pseudo frequency data}
\item{stratified.table}{stratified or Crude pseudo frequency data}
%% ...
}
\author{
Ehsan Karim ehsan@alumni.ubc.ca
}
\seealso{
\code{\link{measures.calc}},\code{\link{standardization}},\code{\link{iptw}}
}
\examples{
form.table.object <- form.table(Y1A1L1=150, Y1A0L1=45, Y1A1L0=20, Y1A0L0=5, Y0A1L1=300, Y0A0L1=10, Y0A1L0=40, Y0A0L0=55)
form.table.object
iptw(form.table.object, type = "sw")
}
