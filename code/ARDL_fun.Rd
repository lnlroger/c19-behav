\name{Choosing and estimating ARDL models flexibly}
\alias{ARDL.flex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
ARDL.flex estimates ARDL(p,q) models flexibly, and automates model selection based on a variety of statistical criteria and in line with good econometric practice.
In its current form, it estimates models of different lag lengths specified with max.p and max.q and selects based on the Bayesian Information Criterion (BIC) among those models that pass the Breush-Godfrey test for serial autocorrelation in the error term; it is designed to carry out this operation for any number of units, e.g. countries or cities (specified using by.var).

}
\usage{
ARDL.flex(data.input = df.now, 
depvar = "diff.Movement",
indepvar = "diff.StringencyIndex", 
impulsevar = indepvar[1], 
max.p = 12, max.q = 12, 
write.output = FALSE, 
path.output = "ARDL-output.Rds", 
by.var = "Country", 
vars.to.deseason = "diff.Movement", 
seasonality = NULL, t.min = 30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.input}{
The data.frame to be used for estimation.
Note: The function doesn't do the differencing of the data, this needs to be done beforehand.
* See test-ARDL_fun.R for functional code chunk to prepare data in context of Covid-19 project.
}
  \item{depvar}{
Dependent variable.
}
  \item{indepvar}{
Independent variable or variables (include as c() in the case of more than one independent variable).
}
  \item{impulsevar}{
The independent variable for which long-run responses etc. shall be computed. By default corresponds to the first element of indepvar, but can be altered.
}
  \item{max.p, max.q}{
Maximum number of lags to be considered for the independent and dependent variable respectively (at least 1).}
  \item{write.output}{
Logical indication whether or not to save key results in separate dataset.
}
  \item{path.output}{
If write.output == TRUE, where to save it.
}
  \item{by.var}{
Which variable determines the observational unit in data.input? E.g., Country or City.
}
  \item{vars.to.deseason}{
Select which variables should be deseasoned.
}
  \item{seasonality}{
Which variable determines the seasonality? (e.g., weekdays) 
Needs to be a factor.
}
  \item{t.min}{
Minimum number of observations to be required for an observational unit to be retained.
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns a list with the following objects:

\item{summary }{A data.frame summarising for each observational unit the model chosen, the underlying criteria, and key parameters (e.g. long run coefficient).}
\item{bic }{A list of matrices collecting the BIC for all considered models for each country.}
\item{bg.test }{A list of matrices collecting the p-value of the Breush-Godfrey test for all considered models for each country}
\item{longrun }{A list of matrices collecting the long run coefficients obtained from all considered models for each country.}
\item{model }{The selected model for each country (ardlDlm object).}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Lionel Roger, Orestis Kopsacheilis
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Prepare dataset (take differences, drop unnecessary variables and missing observations)
df.now <- read_rds("../df_covid_long.rds") %>%
  filter(sub_region_1 == "") %>%
  group_by(Country) %>%
  mutate(diff.Movement = c(NA, diff(Movement)),
         diff.StringencyIndex = c(NA, diff(StringencyIndex))) %>%
  dplyr::select(Country, Date, diff.Movement, diff.StringencyIndex) %>%
  drop_na()

# Run function
test.ARDL <- ARDL.flex(max.p = 2, max.q = 2)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ARDL }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~Covid-19 }% __ONLY ONE__ keyword per line
