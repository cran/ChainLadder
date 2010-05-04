test.alpha1 <- function(){
    alpha <- 1

    x <- RAA[1:9,1]
    y <- RAA[1:9,2]
    weights <- RAA

    weights[!is.na(weights)] <- 1
    w <- weights[length(x),1]

    F <- y/x
    delta <- 2-alpha

    f <- sum(w*x^alpha*F)/sum(w*x^alpha)
    f.lm <- summary(lm(y~x + 0 ,weights=w/x^delta))$coef[1]
    f.chainladder <- summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef[1]

    checkEquals(f, f.lm)
    checkEquals(f, f.chainladder)
}

test.alpha2 <- function(){
    alpha <- 2

    x <- RAA[1:9,1]
    y <- RAA[1:9,2]
    weights <- RAA

    weights[!is.na(weights)] <- 1
    w <- weights[length(x),1]

    F <- y/x
    delta <- 2-alpha

    f <- sum(w*x^alpha*F)/sum(w*x^alpha)
    f.lm <- summary(lm(y~x + 0 ,weights=w/x^delta))$coef[1]
    f.chainladder <- summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef[1]

    checkEquals(f, f.lm)
    checkEquals(f, f.chainladder)
}

test.alpha0 <- function(){
    alpha <- 0

    x <- RAA[1:9,1]
    y <- RAA[1:9,2]
    weights <- RAA

    weights[!is.na(weights)] <- 1
    w <- weights[length(x),1]

    F <- y/x
    delta <- 2-alpha

    f <- sum(w*x^alpha*F)/sum(w*x^alpha)
    f.lm <- summary(lm(y~x + 0 ,weights=w/x^delta))$coef[1]
    f.chainladder <- summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef[1]

    checkEquals(f, f.lm)
    checkEquals(f, f.chainladder)
}
