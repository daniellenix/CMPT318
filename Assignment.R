
#PARSE DATA
print("reading data...")
data <- read.table("data.txt", header = FALSE, sep = ",", skip=10080*16 + 1, nrows=10080)

#FUNCTIONS
geo_mean <- function(values, na.rm=TRUE){
  exp(sum(log(values[values > 0]), na.rm=na.rm) / length(values))
}

mode <- function(x, na.rm = TRUE){
  res <- unique(x, na.rm=na.rm)
  res[which.max(tabulate(match(x, res)))]
}

#CALULATIONS
A_ar_mean <- mean(data[, 3], na.rm=TRUE)
A_gm_mean <- geo_mean(data[, 3], na.rm=TRUE)
A_median <- median(data[, 3], na.rm=TRUE)
A_mode <- mode(data[, 3], na.rm=TRUE)
A_weekday_min <- min(data[7*60:19*6, 3], na.rm=TRUE)
A_weekday_max <- max(data[7*60:19*6, 3], na.rm=TRUE)
A_weekend_min <- min(data[19*6 + 1:24*60, 3], na.rm=TRUE)
A_weekend_max <- max(data[19*6 + 1:24*60, 3], na.rm=TRUE)

print(A_weekday_min)
print(A_weekday_max)

B_ar_mean <- mean(data[, 4], na.rm=TRUE)
B_gm_mean <- geo_mean(data[, 4], na.rm=TRUE)
B_median <- median(data[, 4], na.rm=TRUE)
B_mode <- mode(data[, 4], na.rm=TRUE)
B_weekday_min <- min(data[7*60:19*6, 4], na.rm=TRUE)
B_weekday_max <- max(data[7*60:19*6, 4], na.rm=TRUE)
B_weekend_min <- min(data[19*6 + 1:24*60, 4], na.rm=TRUE)
B_weekend_max <- max(data[19*6 + 1:24*60, 4], na.rm=TRUE)


C_ar_mean <- mean(data[, 5], na.rm=TRUE)
C_gm_mean <- geo_mean(data[, 5], na.rm=TRUE)
C_median <- median(data[, 5], na.rm=TRUE)
C_mode <- mode(data[, 5], na.rm=TRUE)
C_weekday_min <- min(data[7*60:19*6, 5], na.rm=TRUE)
C_weekday_max <- max(data[7*60:19*6, 5], na.rm=TRUE)
C_weekend_min <- min(data[19*6 + 1:24*60, 5], na.rm=TRUE)
C_weekend_max <- max(data[19*6 + 1:24*60, 5], na.rm=TRUE)

