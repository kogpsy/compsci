
# Generate a sample dataset
create_dataframe <- function(na_value = -99) {
    set.seed(1014)
    df <- data.frame(replicate(6, sample(c(1:10,
                                           na_value),
                                         6, rep = TRUE)))
    names(df) <- letters[1:6]
    df
}


## ------------------------------------------------------------------------
df <- create_dataframe()

## ------------------------------------------------------------------------
df$a[df$a == -99] <- NA
df$b[df$b == -99] <- NA
df$c[df$c == -98] <- NA
df$d[df$d == -99] <- NA
df$e[df$e == -99] <- NA
df$f[df$g == -99] <- NA

## Can anyone spot an error?

## ------------------------------------------------------------------------
df <- create_dataframe()
df

## ------------------------------------------------------------------------
fix_missing <- function(x) {
  x[x == -99] <- NA
  x
}

## ------------------------------------------------------------------------
df$d

## ------------------------------------------------------------------------
df$a <- fix_missing(df$a)
df$b <- fix_missing(df$b)
df$c <- fix_missing(df$c)
df$d <- fix_missing(df$d)
df$e <- fix_missing(df$e)
df$f <- fix_missing(df$e)

## ------------------------------------------------------------------------
df <- create_dataframe()
df

## ------------------------------------------------------------------------
df[] <- lapply(df, fix_missing)
df

## ------------------------------------------------------------------------
df <- create_dataframe(na_value = -98)
df

## ------------------------------------------------------------------------
fix_missing <- function(x, na_value = -99) {
  x[x == na_value] <- NA
  x
}

## ------------------------------------------------------------------------
df[] <- lapply(df, fix_missing, na_value = -98)
df

## ------------------------------------------------------------------------
power_fun_1 <- function(x, k = 2) {
    x^k
}

## ------------------------------------------------------------------------
power_fun_1(3)

## ------------------------------------------------------------------------
power_fun_2 <- function(x, k = 2) {
    list(result = x^k,
         call = paste(as.character(x), "to the power of",
                      as.character(k)))
}

## ------------------------------------------------------------------------
power_fun_2(2, 3)

## ------------------------------------------------------------------------
power_fun_3 <- function(x, k = 2) {
    out <- list(result = x^k,
                x = x,
                k = k,
         call = paste(as.character(x), "to the power of",
                      as.character(k)))
    class(out) <- "some_class"
    out
}

## ------------------------------------------------------------------------
plot.some_class <- function(z) {
    curve(x^z$k, -2*z$x, 2*z$x, lwd = 3,
          col = "violet", ylab = z$call,
          xlab = "x")
    points(z$x, z$result, pch = 19, col = "blue", cex = 3)
}

## ----fig-margin, fig.margin = TRUE, fig.cap = "Power function plot", fig.width=3.5, fig.height=3.5, cache=TRUE----
out <- power_fun_3(2, k = 3)
plot(out)

## ------------------------------------------------------------------------
sessionInfo()

