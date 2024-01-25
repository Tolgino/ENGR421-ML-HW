#setwd("/Users/tolga/Desktop")
data <- read.csv(file = "hw04_data_set.csv", header = TRUE)

x <- data$x
y <- data$y

x_train <- matrix(head(x,100))
x_test <- matrix(tail(x,33))

y_train <- matrix(head(y,100))
y_test <- matrix(tail(y,33))

bin_width <- 3.0
origin <- 0.0

rmse = function(p, t){
  sqrt(mean((p - t)^2))
}

left_borders <- seq(from = origin, to = max(x), by = bin_width)
right_borders <- seq(from = origin + bin_width, to = max(x) + bin_width, by = bin_width)
p_hat <- sapply(1:length(left_borders), function(a) {sum(y_train[left_borders[a] < x_train & x_train <= right_borders[a]]) / sum(left_borders[a] < x_train & x_train <= right_borders[a])}) 

plot(x_train, y_train, type = "p", pch = 20, col = "blue",
     xlim = c(origin, 60), ylim = c(min(y), max(y)),
     xlab = "x", ylab = "y", las = 1, main = sprintf("h = %g", bin_width))
points(x_test, y_test, type = "p", pch = 20, col = "red")
legend("topleft", c("training", "test"), col = c("blue", "red"), pch = 20)

for(b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(p_hat[b], p_hat[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(p_hat[b], p_hat[b + 1]), lwd = 2, col = "black") 
  }
}

sprintf("Regressogram => RMSE is %g when h is %g", rmse(y_test, p_hat[round(x_test / bin_width, 0)]), bin_width)

######################################################################################################

data_interval <- seq(from =  min(x), to = max(x), by = 0.01)
p_hat2 <- sapply(data_interval, function(a) {sum(y_train[(a - 0.5 * bin_width) < x_train & x_train <= (a + 0.5 * bin_width)])/ sum((a - 0.5 * bin_width) < x_train & x_train <= (a + 0.5 * bin_width))}) 

plot(x_train, y_train, type = "p", pch = 20, col = "blue",
     xlim = c(origin, 60), ylim = c(min(y), max(y)),
     xlab = "x", ylab = "y", las = 1, main = sprintf("h = %g", bin_width))
points(x_test, y_test, type = "p", pch = 20, col = "red")
legend("topleft", c("training", "test"), col = c("blue", "red"), pch = 20)

lines(data_interval, p_hat2, type = "l", lwd = 2, col = "black")

sprintf("Running Mean Smoother => RMSE is %g when h is %g", rmse(y_test, p_hat2[x_test / round((max(x) - min(x)) /length(data_interval), 2)]), bin_width)

######################################################################################################

bin_width2 <- 1.0
p_hat3 <- sapply(data_interval, function(a) {sum(y_train * 1 / sqrt(2 * pi) * exp(-0.5 * (a - x_train)^2 / bin_width2^2))/ sum(1 / sqrt(2 * pi) * exp(-0.5 * (a - x_train)^2 / bin_width2^2))}) 

plot(x_train, y_train, type = "p", pch = 20, col = "blue",
     xlim = c(origin, 60), ylim = c(min(y), max(y)),
     xlab = "x", ylab = "y", las = 1,  main = sprintf("h = %g", bin_width2))
points(x_test, y_test, type = "p", pch = 20, col = "red")
legend("topleft", c("training", "test"), col = c("blue", "red"), pch = 20)

lines(data_interval, p_hat3, type = "l", lwd = 2, col = "black")

sprintf("Kernel Smoother => RMSE is %g when h is %g", rmse(y_test, p_hat3[x_test / round((max(x) - min(x)) /length(data_interval), 2)]), bin_width2)