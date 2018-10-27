exam.norm <-
    function(X,
             univariate = T,
             univariate.test = F,
             bivariate = F,
             multivariate = T) {
        if (univariate == T) {
            if (ncol(X) == 1) {
                par(mfrow = c(1, 1))
            }
            if (ncol(X) > 1 & ncol(X) < 4) {
                par(mfrow = c(1, ncol(X)))
            }
            if (ncol(X) > 3) {
                par(mfrow = c(ceiling(ncol(X) / 3), 3))
            }
            for (i in 1:(ncol(X))) {
                qqPlot(
                    X[i],
                    ylab = colnames(X)[i],
                    xlab = paste("normal",
                                 "quantiles"),
                    main = "QQNORM"
                )
            }
        }
        if (sum(univariate, bivariate, multivariate) > 1) {
            windows()
        }
        if (bivariate == T) {
            panel.hist <- function(x, ...) {
                usr <- par("usr")
                on.exit(par(usr))
                par(usr = c(usr[1:2], 0, 1.5))
                h <- hist(x, plot = F)
                breaks <- h$breaks
                nB <- length(breaks)
                y <- h$counts
                y <- y / max(y)
                rect(breaks[-nB], 0, breaks[-1], y, col = "bisque4", ...)
            }
            pairs(X, diag.panel = panel.hist)
        }
        if (sum(univariate, bivariate, multivariate) == 3) {
            windows()
        }
        if (multivariate == T) {
            mu <- colMeans(X)
            sig <- cov(X)
            stopifnot(mahalanobis(X, 0, diag(ncol(X))) == rowSums(X * X))
            par(mfrow = c(2, 1))
            D2 <- mahalanobis(X, mu, sig)
            n = dim(X)[1]
            p = dim(X)[2]
            plot(
                density(D2, bw = .5),
                main = paste(
                    "Mahalanobis Distances (N=",
                    as.character(n),
                    "
                    & p=",
                    as.character(p),
                    ")"
                )
            )
            qqplot(qchisq(ppoints(n), df = p),
                   D2,
                   main = "Q-Q Plot of Mahalanobis D Squared vs.
                   Quantiles of Chi Squared")
            abline(0, 1, col = 'gray')
        }
        if (univariate == F &
            multivariate == F & bivariate == F) {
            print("User must specify at least
                  one method of examining for normality")
        }
        if (univariate.test == T) {
            results <- vector("list", ncol(X))
            for (i in 1:(ncol(X))) {
                results[[i]] <- shapiro.test(X[, i])
            }
            names(results) <- colnames(X)
            print(results)
        }
        }
