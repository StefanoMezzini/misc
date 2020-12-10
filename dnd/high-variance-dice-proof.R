# regular d20
x <- 1:20
p <- rep(1 / 20, 20)
Ed20 <- x %*% p
Vd20 <- (x - rep(Ed20, 20)) %*% (x - rep(Ed20, 20)) / (20 - 1)

# high variance d20
x.highv <- c(1, 2, 3, 4, 5, 6, 15, 16, 17, 18, 19, 20)
p.highv <- c(3, 2, 2, 1, 1, 1, 1,  1,  1,  2,  2,  3) / 20
Ed20.highv <- x.highv %*% p.highv
Vd20.highv <- (x.highv - rep(Ed20.highv, length(x.highv))) %*%
  (x.highv - rep(Ed20.highv, length(x.highv))) / (length(x.highv) - 1)

# same expectations (means)
Ed20 == Ed20.highv

# different variances
Vd20
Vd20.highv
