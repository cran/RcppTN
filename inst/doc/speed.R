## ----message = FALSE-----------------------------------------------------
library("RcppTN")
library("truncnorm")
library("msm")
library("microbenchmark")

set.seed(1)
rtn() # RcppTN
set.seed(1)
rtnorm(n=1) # msm
set.seed(1)
rtruncnorm(n=1) # truncnorm

## ------------------------------------------------------------------------
set.seed(1)
rtn(.mean = 0, .sd = 1, .low = 4, .high = 4.1)
set.seed(1)
rtnorm(n=1, mean = 0, sd = 1, lower = 4, upper = 4.1)
set.seed(1)
rtruncnorm(n=1, mean = 0, sd = 1, a = 4, b = 4.1)

## ------------------------------------------------------------------------
set.seed(1)
rtn(.mean = 0, .sd = 1, .low = 5, .high = Inf)
set.seed(1)
rtnorm(n=1, mean = 0, sd = 1, lower = 5, upper = Inf)
set.seed(1)
rtruncnorm(n=1, mean = 0, sd = 1, a = 5, b = Inf)

## ----tidy=FALSE----------------------------------------------------------
sizes <- c(1e1, 1e3, 1e5)
lows <- c(-1, 5, -Inf, 4, 4, -Inf, 50)
highs <- c(1, Inf, 10, 7, 4.1, Inf, 100)

## ----tidy=FALSE----------------------------------------------------------
s <- sizes[2]

microbenchmark(
    "rtn" = rtn(.mean = rep(0, s),
    .low = rep(lows[1], s),
    .high = rep(highs[1], s),
    .checks = FALSE
    ),
    "rtruncnorm" = rtruncnorm(n = s,
    a = rep(lows[1], s),
    b = rep(highs[1], s)
    ),
    "rtnorm" = rtnorm(n = s,
    lower = rep(lows[1], s),
    upper = rep(highs[1], s)
    ),
    times = 100
    )

## ----tidy=FALSE----------------------------------------------------------
microbenchmark(
    "rtn" = rtn(.mean = rep(0, s),
    .low = rep(lows[5], s),
    .high = rep(highs[5], s),
    .checks = FALSE
    ),
    "rtruncnorm" = rtruncnorm(n = s,
    a = rep(lows[5], s),
    b = rep(highs[5], s)
    ),
    "rtnorm" = rtnorm(n = s,
    lower = rep(lows[5], s),
    upper = rep(highs[5], s)
    ),
    times = 100
    )

## ----"bigbench", echo = FALSE--------------------------------------------
cnt <- 1
for (case in 1:length(lows)) {
    for (s in sizes) {
        out <- {microbenchmark(rtn = rtn(.mean = rep(0, s),
                               .low = rep(lows[case], s),
                               .high = rep(highs[case], s),
                               .checks = FALSE
                               ),
                               rtruncnorm = rtruncnorm(n = s,
                               a = rep(lows[case], s),
                               b = rep(highs[case], s)
                               ),
                               times = 100L
                               )
            }
        out$case <- case
        out$size <- s
        if (cnt == 1) {
            dfOut <- out
        } else {
            dfOut <- rbind(dfOut,
                           out
                           )
        }
        cnt <- cnt + 1
    }
}

## ----"bigbenchfig", echo = FALSE, message = FALSE, out.width = "\\linewidth"----
library("ggplot2")
theme_set(theme_bw())


dfOut$case2 <- paste("(", lows[dfOut$case], ",", highs[dfOut$case], ")", sep = " ")
dfOut$size2 <- format(dfOut$size, scientific = 2)

ggplot(as.data.frame(dfOut)) +
    geom_boxplot(aes(x = case2,
                     y = time / 10000,
                     color = expr
                     ),
                 position = position_dodge(width = .75),
                 alpha = .5,
                 size = .5,
                 outlier.size = 1
                 ) +
    scale_y_log10(breaks = c(2, 4, 8, 10, 15, 20, 30, 1000, 2500, 10000)) +
    scale_color_discrete("R Function",
                         labels = c("rtn()", "rtruncnorm()")
                         ) +
    labs(x = "Interval",
         y = "Microseconds / 10"
         ) +
    theme(legend.position = "bottom") +
    ## scale_x_discrete("Sample Size",
    ##                  limits = c("10", "1000", "1e+05"),
    ##                  labels = c("1e1", "1e3", "1e5")
    ##                  ) +
    facet_grid(size2 ~ ., scales = "free")

