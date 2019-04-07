getRidOfNAs <- function(x) {
  original.order.of.columns <- names(x)                 # save this order for rearranging columns later
  y <- apply(x[, c(5, 16:23)], 2, replaceNAsWithZeros)  # Other.releases, Covered.by, ...
  x[, c(5, 16:23)] <- NULL                              # remove them from the original dataframe
  x <- cbind(x, y)                                      # cbind the modified columns
  x <- x[original.order.of.columns]                     # rearrange the columns in the original order
  y <- apply(x[, 24:30], 2, getRidOfNAsTop50)           # take care of all Top.50 variables
  x[, c(24:30)] <- NULL                                 # remove them from the original dataframe
  x <- cbind(x, y)                                      # cbind the modified columns
  x
}

replaceNAsWithZeros <- function(col) {
  col[is.na(col)] <- 0
  col
}

getRidOfNAsTop50 <- function(col) {
  # na <- which(col[is.na(col)])
  # col[na] <- 0
  col <- replaceNAsWithZeros(col)
  not.zero <- which(col != 0)
  # col[not.zero] <- 51 - col
  col[not.zero] <- 51 - col[not.zero]
  col
}

factorize <- function(x) {
  x$Single.certification[x$Single.certification == ""] <- "No"
  x$Single.certification[x$Single.certification == "BPI Silver; RIAA Gold"] <- "RIAA Gold, BPI Silver"
  x$Single.certification <- as.factor(x$Single.certification)

  x$Cover <- as.factor(x$Cover)
  levels(x$Cover) <- c("No", "Yes")
  
  y <- apply(x[, 24:30], 2, factorizeTop50)             # take care of all Top.50 variables
  x[, c(24:30)] <- NULL                                 # remove them from the original dataframe
  x <- cbind(x, y)                                      # cbind the modified columns

  x
}

factorizeTop50 <- function(col) {
  col[col > 0] <- 1
  col <- as.factor(col)
  levels(col) <- c("No", "Yes")
  col
}