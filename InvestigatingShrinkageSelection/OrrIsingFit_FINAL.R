#THIS FUNCTION HAS THE SHRINKAGE PARAMETER IN GLMNET TO ZERO (lambda=0)
#OTHERWISE IT IS IDENTICAL TO Isingfit in the ISING FIT PACKAGE
#CREATED 11-20-16
#NOTES-
#1. Source this file by> source("OrrIsingFit.r")
#2. Call just as you would Ising Fit but need to add the parameters, e.g.,
# Fit <- Orr.IsingFit(x,"binomial",TRUE,0.25,TRUE,TRUE,NA,0-?) where x is identical to what was
#used in IsingFit.
#the paramter glm.lam is for the lambda in glmnet call

Orr.IsingFit <- function (x, family, AND, gamma, plot, progressbar, lowerbound.lambda,glm.lam) 
{
    t0 <- Sys.time()
    xx <- x
    if (family != "binomial")
        stop("This procedure is currently only supported for binary (family='binomial') data")
    checklognet <- function(y) {
        res <- c()
        y = as.factor(y)
        ntab = table(y)
        minclass = min(ntab)
        if (minclass <= 1) 
            res = 0
        else res = 1
        return(res)
    }
    NodesToAnalyze <- apply(x, 2, checklognet) != 0
    names(NodesToAnalyze) <- colnames(x)
    if (!any(NodesToAnalyze)) 
        stop("No variance in dataset")
    if (any(!NodesToAnalyze)) {
        warning(paste("Nodes with too little variance (not allowed):", 
            paste(colnames(x)[!NodesToAnalyze], collapse = ", ")))
    }
    x <- as.matrix(x)
    allthemeans <- colMeans(x)
    x <- x[, NodesToAnalyze, drop = FALSE]
    nvar <- ncol(x)
    p <- nvar - 1
    intercepts <- betas <- lambdas <- list(vector, nvar)
    nlambdas <- rep(0, nvar)
    for (i in 1:nvar) {
        a <- glmnet(x[, -i], x[, i], family = family,lambda=glm.lam)
        intercepts[[i]] <- a$a0
        betas[[i]] <- a$beta
        lambdas[[i]] <- a$lambda
        nlambdas[i] <- length(lambdas[[i]])
    }
    if (progressbar == TRUE) 
        pb <- txtProgressBar(max = nvar, style = 3)
    P <- logl <- sumlogl <- J <- matrix(0, max(nlambdas), nvar)
    for (i in 1:nvar) {
        J[1:ncol(betas[[i]]), i] <- colSums(betas[[i]] != 0)
    }
    logl_M <- P_M <- array(0, dim = c(nrow(x), max(nlambdas), 
        nvar))
    N <- nrow(x)
    for (i in 1:nvar) {
        betas.ii <- as.matrix(betas[[i]])
        int.ii <- intercepts[[i]]
        y <- matrix(0, nrow = N, ncol = ncol(betas.ii))
        xi <- x[, -i]
        NB <- nrow(betas.ii)
        for (bb in 1:NB) {
            y <- y + betas.ii[rep(bb, N), ] * xi[, bb]
        }
        y <- matrix(int.ii, nrow = N, ncol = ncol(y), byrow = TRUE) + 
            y
        n_NA <- max(nlambdas) - ncol(y)
        if (n_NA > 0) {
            for (vv in 1:n_NA) {
                y <- cbind(y, NA)
            }
        }
        P_M[, , i] <- exp(y * x[, i])/(1 + exp(y))
        logl_M[, , i] <- log(P_M[, , i])
        if (progressbar == TRUE) 
            setTxtProgressBar(pb, i)
    }
    logl_Msum <- colSums(logl_M, 1, na.rm = FALSE)
    if (progressbar == TRUE) 
        close(pb)
    sumlogl <- logl_Msum
    sumlogl[sumlogl == 0] = NA
    penalty <- J * log(nrow(x)) + 2 * gamma * J * log(p)
    EBIC <- -2 * sumlogl + penalty
    lambda.mat <- matrix(NA, nrow(EBIC), ncol(EBIC))
    for (i in 1:nvar) {
        lambda.mat[, i] <- c(lambdas[[i]], rep(NA, nrow(EBIC) - 
            length(lambdas[[i]])))
    }
    if (!is.na(lowerbound.lambda)) {
        EBIC <- EBIC/(lambda.mat >= lowerbound.lambda) * 1
    }
    lambda.opt <- apply(EBIC, 2, which.min)
    lambda.val <- rep(NA, nvar)
    thresholds <- 0
    for (i in 1:length(lambda.opt)) {
        lambda.val[i] <- lambda.mat[lambda.opt[i], i]
        thresholds[i] <- intercepts[[i]][lambda.opt[i]]
    }
    weights.opt <- matrix(, nvar, nvar)
    for (i in 1:nvar) {
        weights.opt[i, -i] <- betas[[i]][, lambda.opt[i]]
    }
    asymm.weights <- weights.opt
    diag(asymm.weights) = 0
    if (AND == TRUE) {
        adj <- weights.opt
        adj <- (adj != 0) * 1
        EN.weights <- adj * t(adj)
        EN.weights <- EN.weights * weights.opt
        meanweights.opt <- (EN.weights + t(EN.weights))/2
        diag(meanweights.opt) <- 0
    }
    else {
        meanweights.opt <- (weights.opt + t(weights.opt))/2
        diag(meanweights.opt) <- 0
    }
    graphNew <- matrix(0, length(NodesToAnalyze), length(NodesToAnalyze))
    graphNew[NodesToAnalyze, NodesToAnalyze] <- meanweights.opt
    colnames(graphNew) <- rownames(graphNew) <- colnames(xx)
    threshNew <- ifelse(allthemeans > 0.5, -Inf, Inf)
    threshNew[NodesToAnalyze] <- thresholds
    if (plot == TRUE) 
        notplot = FALSE
    else notplot = TRUE
    q <- qgraph(graphNew, layout = "spring", labels = names(NodesToAnalyze), 
        DoNotPlot = notplot)
    Res <- list(weiadj = graphNew, thresholds = threshNew, q = q, 
        gamma = gamma, AND = AND, time = Sys.time() - t0, asymm.weights = asymm.weights, 
        lambda.values = lambda.val)
    class(Res) <- "IsingFit"
    return(Res)
}
#EOF
