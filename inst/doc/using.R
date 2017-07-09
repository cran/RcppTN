## ----eval=FALSE----------------------------------------------------------
#  library("devtools")
#  install_github(repo = "RcppTN",
#                 username = "olmjo",
#                 subdir = "pkg",
#                 ref = "development"
#                 )

## ------------------------------------------------------------------------
library("RcppTN")
set.seed(1)
rtn()
set.seed(1)
rtn()

## ------------------------------------------------------------------------
set.seed(1)
rtn()
set.seed(1)
rtn(.mean = 0, .sd = 1, .low = -Inf, .high = Inf)
set.seed(1)
rtn()
set.seed(1)
rnorm(1)

## ------------------------------------------------------------------------
set.seed(11)
rtn()
rtn()
set.seed(1)
rtn()
rtn()
set.seed(11)
rtn()
rtn()

## ----eval = FALSE--------------------------------------------------------
#  ## Not Run -- will cause error
#  rtn(.mean = c(0, 1), .sd = 1)

## ----tidy=FALSE----------------------------------------------------------
rtn(0, -1, 0, 1)
rtn(0, 1, 0, -1)
rtn(c(0,0), c(1,1), c(0,0), c(-Inf,Inf))

## ----eval = FALSE--------------------------------------------------------
#  ## Not Run -- no warning given
#  rtn(0, -1, 0, 1, .checks = FALSE)

## ----tidy=FALSE----------------------------------------------------------
set.seed(1)
output <- rtn(.mean = rep(0, 1000),
              .sd = rep(1, 1000),
              .low = rep(1, 1000),
              .high = rep(2, 1000)
              )
length(output)
mean(output)

## ----tidy=FALSE, out.height = ".7\\textwidth", out.width= ".7\\textwidth"----
bigoutput <- rep(NA, 1000)
for (i in 1:length(bigoutput)) {
    bigoutput[i] <- mean(rtn(.mean = rep(0, 1000),
                             .sd = rep(1, 1000),
                             .low = rep(1, 1000),
                             .high = rep(2, 1000)
                             )
                         )
}
summary(bigoutput)

## ------------------------------------------------------------------------
outputA <- rtn(.mean = rep(0, 5000),
               .sd = rep(1, 5000),
               .low = rep(-1, 5000),
               .high = rep(Inf, 5000)
               )
outputB <- rtn(.mean = rep(0, 5000),
               .sd = rep(1, 5000),
               .low = rep(0, 5000),
               .high = rep(1, 5000)
               )
outputC <- rtn(.mean = rep(0, 5000),
               .sd = rep(1, 5000),
               .low = rep(-Inf, 5000),
               .high = rep(Inf, 5000)
               )
outputD <- rtn(.mean = rep(0, 5000),
               .sd = rep(1, 5000),
               .low = rep(5, 5000),
               .high = rep(Inf, 5000)
               )

dfOutput <- rbind(data.frame(value = outputA, dist = "A"),
                  data.frame(value = outputB, dist = "B"),
                  data.frame(value = outputC, dist = "C"),
                  data.frame(value = outputD, dist = "D")
                 )

## ----"diffdists", echo = FALSE, message = FALSE--------------------------
library("ggplot2")

ggplot(dfOutput) +
    geom_histogram(aes(x=value,
                       y=..density..,
                       fill = dist
                       ),
                   alpha = 1/3,
                   position = "identity"
                   ) +
    scale_fill_discrete("Distribution") +
    theme(legend.position = "bottom")

## ----"randombounds", tidy=FALSE, out.height = ".65\\textwidth", out.width= ".65\\textwidth", message=FALSE----
lows <- rtn(rep(0, 1000),
            rep(3, 1000),
            rep(-10, 1000),
            rep(3, 1000)
            )
highs <- rtn(rep(0, 1000),
             rep(3, 1000),
             rep(3, 1000),
             rep(4, 1000)
             )
all(lows < highs)

outputD <- rtn(.mean = rep(0, 1000),
               .sd = rep(3, 1000),
               .low = lows,
               .high = highs
               )

ggplot() +
    geom_histogram(aes(x = outputD))


## ------------------------------------------------------------------------
etn(.mean = 0,
    .sd = 1,
    .low = 0,
    .high = 10
    )

etn(0, 1, 3.5, 3.7)

## ------------------------------------------------------------------------
vtn(.mean = 0,
    .sd = 1,
    .low = 0,
    .high = 10
    )

vtn(0, 1, 3.5, 3.7)

## ------------------------------------------------------------------------
dtn(.x = 4,
    .mean = 0,
    .sd = 1,
    .low = 0,
    .high = 10
    )

dtn(3.6, 0, 1, 3.5, 3.7)

## ------------------------------------------------------------------------
enttn(.mean = rep(0, 2),
      .sd = c(.01, 100),
      .low = rep(-1, 2),
      .high = rep(1, 2)
      )

## ----tidy = FALSE--------------------------------------------------------
library("Rcpp")
sourceCpp(code = "
#include <Rcpp.h>

#include <RcppTN.h>
// [[Rcpp::depends(RcppTN)]]

using namespace Rcpp ;

// [[Rcpp::export]]
List rcpp_hello_world() {
  double a = RcppTN::rtn1(0.0, 1.0, 3.5, 3.7) ;
  double b = RcppTN::etn1(0.0, 1.0, 3.5, 3.7) ;
  double c = RcppTN::vtn1(0.0, 1.0, 3.5, 3.7) ;
  double d = RcppTN::dtn1(3.6, 0.0, 1.0, 3.5, 3.7) ;
  double e = RcppTN::enttn1(0.0, 1.0, 3.5, 3.7) ;
  NumericVector y = NumericVector::create(a, b, c, d, e) ;
  List z = List::create(y) ;
  return(z) ;
}
"
          )

rcpp_hello_world()

## ----eval=FALSE----------------------------------------------------------
#  library("Rcpp")
#  Rcpp.package.skeleton(path="~/Desktop")

