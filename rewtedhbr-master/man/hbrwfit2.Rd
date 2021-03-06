\name{hbrwfit2}
\alias{hbrwfit2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function which computes the weights for the iteratively reweighted LS estimate of the HBR fit.
%%  ~~function to do ... ~~
}
\description{
Function which computes the weights for the iteratively reweighted LS estimate of the HBR fit.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hbrwfit2(xmat, y, numstp = 10, epcchk = 1e-04, B = 101, scores = wscores)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xmat}{
%%     ~~Describe \code{xmat} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{numstp}{
%%     ~~Describe \code{numstp} here~~
}
  \item{epcchk}{
%%     ~~Describe \code{epcchk} here~~
}
  \item{B}{
%%     ~~Describe \code{B} here~~
}
  \item{scores}{
%%     ~~Describe \code{scores} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (xmat, y, numstp = 10, epcchk = 1e-04, B = 101, scores = wscores) 
{
    x <- centerx(xmat)
    pchk <- length(x[1, ])
    plim <- (pchk + 1) * (numstp + 1)
    matbeta <- matrix(rep(0, plim), ncol = (numstp + 1))
    matwts <- c()
    ehatk <- c()
    wts2 <- c()
    chks <- 1
    k <- 1
    while (chks > 0) {
        if (k == 1) {
            firstfit <- kstephbr(y, x, ehatk, 1, wts2, B, scores)
            robdis2 <- firstfit$robdis2
            ehatlts <- firstfit$ehatlts
            betak <- firstfit$fitkp1$coef
            wts2 <- firstfit$wts2
            matwts <- cbind(matwts, firstfit$pwts)
            matbeta[, k] <- betak
            ehatk <- firstfit$fitkp1$resid
            k <- k + 1
        }
        else {
            kp1fit <- kstephbr(y, x, ehatk, k, wts2, B, scores)
            betakp1 <- kp1fit$fitkp1$coef
            matwts <- cbind(matwts, kp1fit$pwts)
            matbeta[, k] <- betakp1
            delta <- betakp1 - betak
            chkdel <- sum(delta^2)/(epcchk + sum(betak^2))
            if (chkdel < epcchk) {
                chks <- -1
            }
            else {
                k <- k + 1
                ehatk <- kp1fit$fitkp1$resid
                betak <- betakp1
            }
        }
        if (k > numstp) {
            chks <- -1
        }
    }
    findim <- dim(matwts)[2]
    finwts <- cbind(wts2, matwts[, findim])
    if (k == 1) {
        coef <- matbeta[, 1]
    }
    else {
        coef <- matbeta[, k]
    }
    fitted.values <- kp1fit$fitkp1$fitted.values
    residuals <- kp1fit$fitkp1$residuals
    list(coef = coef, fitted.values = fitted.values, residuals = residuals, 
        kp1fit = kp1fit, matbeta = matbeta, finwts = finwts, 
        robdis2 = robdis2, ehatlts = ehatlts)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
