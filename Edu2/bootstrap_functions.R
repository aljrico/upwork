bootstrap_test_old <- function(x, y, n = 1e5) {
  l <- (length(x) + length(y)) / 100

  if ((length(x) + length(y)) < 1e3) l <- 10

  x_means <- c()
  y_means <- c()

  for (i in 1:n) {
    x_means[[i]] <- sample(x, size = l, replace = TRUE) %>% mean(na.rm = TRUE)
    y_means[[i]] <- sample(y, size = l, replace = TRUE) %>% mean(na.rm = TRUE)
  }

  t.test(x = x_means, y = y_means) %>%
    .$p.value %>%
    return()
}


bootstrap_test <- function(x, y, n = 1e3) {
	x <- x[!is.na(x)]
	y <- y[!is.na(y)]

  middle <- c(x, y) %>% mean(na.rm = TRUE)
  xt <- x - mean(x, na.rm = TRUE) + middle
  yt <- y - mean(y, na.rm = TRUE) + middle

  non_b_statistic <- t.test(x, y) %>% .$statistic

  boot_t <- c()
  for (i in 1:n) {
    sample_x <- sample(xt, replace = TRUE)
    sample_y <- sample(yt, replace = TRUE)
    boot_t[[i]] <- t.test(sample_x, sample_y) %>% .$statistic
  }
  result <- mean(abs(boot_t) > abs(non_b_statistic))
  result %>% return()
}
