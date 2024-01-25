#setwd("/Users/tolga/Desktop")
data <- read.csv(file = "hw04_data_set.csv", header = TRUE)

x <- data$x
y <- data$y

x_train <- matrix(head(x,100))
x_test <- matrix(tail(x,33))

y_train <- matrix(head(y,100))
y_test <- matrix(tail(y,33))

N_train <- length(y_train)
N_test <- length(y_test)

plot(x_train, y_train, type = "p", pch = 20, col = "blue",
    xlim = c(min(x), 60), ylim = c(min(y), max(y)),
    xlab = "x", ylab = "y", main = "P = 15")
points(x_test, y_test, type = "p", pch = 20, col = "red")
legend("topleft", c("training", "test"), col = c("blue", "red"), pch = 20)

left_borders <- seq(from = 0, to = max(x), by = 0.01) 
right_borders <- seq(from = 0.01, to = max(x) + 0.01, by = 0.01)
P <- 15

node_splits <- c()
node_frequencies <- c()

node_indices <- list(1:N_train)
is_terminal <- c(FALSE)
need_split <- c(TRUE)

while (1) {
  split_nodes <- which(need_split)
  if (length(split_nodes) == 0) {
    break
  }
  
  for (split_node in split_nodes) {
    data_indices <- node_indices[[split_node]]
    need_split[split_node] <- FALSE
    node_mean <- mean(y_train[data_indices])
    
    if (length(x_train[data_indices]) <= P) {
      is_terminal[split_node] <- TRUE
      node_frequencies[split_node] <- node_mean
    } 
    else {
      is_terminal[split_node] <- FALSE
      unique_values <- sort(unique(x_train[data_indices]))
      split_positions <- (unique_values[- 1] + unique_values[- length(unique_values)]) / 2
      split_scores <- rep(0, length(split_positions))
      
      for (s in 1:length(split_positions)) {
        left_indices <- data_indices[which(x_train[data_indices] < split_positions[s])]
        right_indices <- data_indices[which(x_train[data_indices] > split_positions[s])]
        score <- 0
        
        if (length(left_indices) > 0) {
          meanLeft <- mean(y_train[left_indices])
          score <- score + sum((y_train[left_indices] - meanLeft) * (y_train[left_indices] - meanLeft))
        }
        
        if (length(right_indices) > 0) {
          meanRight <- mean(y_train[right_indices])
          score <- score + sum((y_train[right_indices] - meanRight) * (y_train[right_indices] - meanRight))
        }
        
        split_scores[s] <- score / (length(left_indices) + length(right_indices))
      }
      
      if (length(unique_values) == 1) {
        is_terminal[split_node] <- TRUE
        node_frequencies[split_node] <- node_mean
        next 
      }
      
      best_split <- split_positions[which.min(split_scores)]
      node_splits[split_node] <- best_split
      
      left_indices <- data_indices[which(x_train[data_indices] < best_split)]
      node_indices[[2 * split_node]] <- left_indices
      is_terminal[2 * split_node] <- FALSE
      need_split[2 * split_node] <- TRUE
      
      right_indices <- data_indices[which(x_train[data_indices] >= best_split)]
      node_indices[[2 * split_node + 1]] <- right_indices
      is_terminal[2 * split_node + 1] <- FALSE
      need_split[2 * split_node + 1] <- TRUE
    }
  }
}

left_border_predicted <- rep(0, length(left_borders))
for(i in 1:length(left_borders)) {
  index <- 1
  while(1) {
    if(is_terminal[index] == TRUE) {
      left_border_predicted[i] <- node_frequencies[index]
      break
    } else {
      if(left_borders[i] <= node_splits[index]) {
        index <- index * 2
      } else {
        index <- index * 2 + 1
      }
    }
  }
}

right_border_predicted <- rep(0, length(right_borders))
for(i in 1:length(right_borders)) {
  index <- 1
  while(1) {
    if(is_terminal[index] == TRUE) {
      right_border_predicted[i] <- node_frequencies[index]
      break
    } else {
      if(right_borders[i] <= node_splits[index]) {
        index <- index * 2
      } else {
        index <- index * 2 + 1
      }
    }
  }
}

for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(left_border_predicted[b], left_border_predicted[b]), col = "black", lwd=2, pch=20)
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(left_border_predicted[b], right_border_predicted[b]), col = "black", lwd=2, pch=20)
  }
}

rmse = function(p, t){
  sqrt(mean((p - t)^2))
}

y_predicted <- rep(0, N_test)
for (i in 1:N_test) {
  index <- 1
  while(1) {
    if(is_terminal[index] == TRUE) {
      y_predicted[i] <- node_frequencies[index]
      break
    } else {
      if(x_test[i] <= node_splits[index]) {
        index <- index * 2
      } else {
        index <- index * 2 + 1
      }
    }
  }
}
sprintf("RMSE is %.4f when P is %s", rmse(y_test, y_predicted), P)

RMSE <- rep(0, 10)
for (p in seq(from = 5, to = 50, by = 5)) {
  P <- p
  node_splits <- c()
  node_frequencies <- c()
  
  node_indices <- list(1:N_train)
  is_terminal <- c(FALSE)
  need_split <- c(TRUE)
  
  while (1) {
    split_nodes <- which(need_split)
    if (length(split_nodes) == 0) {
      break
    }
    
    for (split_node in split_nodes) {
      data_indices <- node_indices[[split_node]]
      need_split[split_node] <- FALSE
      node_mean <- mean(y_train[data_indices])
      
      if (length(x_train[data_indices]) <= P) {
        is_terminal[split_node] <- TRUE
        node_frequencies[split_node] <- node_mean
      } 
      else {
        is_terminal[split_node] <- FALSE
        
        unique_values <- sort(unique(x_train[data_indices]))
        split_positions <- (unique_values[- 1] + unique_values[- length(unique_values)]) / 2
        split_scores <- rep(0, length(split_positions))
        
        for (s in 1:length(split_positions)) {
          left_indices <- data_indices[which(x_train[data_indices] <= split_positions[s])]
          right_indices <- data_indices[which(x_train[data_indices] > split_positions[s])]
          total_error <- 0
          
          if (length(left_indices) > 0) {
            mean <- mean(y_train[left_indices])
            total_error <- total_error + sum((y_train[left_indices] - mean) ^ 2)
          }
          
          if (length(right_indices) > 0) {
            mean <- mean(y_train[right_indices])
            total_error <- total_error + sum((y_train[right_indices] - mean) ^ 2)
          }
          
          split_scores[s] <- total_error / (length(left_indices) + length(right_indices))
        }
        
        if (length(unique_values) == 1) {
          is_terminal[split_node] <- TRUE
          node_frequencies[split_node] <- node_mean
          next 
        }
        
        best_split <- split_positions[which.min(split_scores)]
        node_splits[split_node] <- best_split
        
        left_indices <- data_indices[which(x_train[data_indices] < best_split)]
        node_indices[[2 * split_node]] <- left_indices
        is_terminal[2 * split_node] <- FALSE
        need_split[2 * split_node] <- TRUE
        
        right_indices <- data_indices[which(x_train[data_indices] >= best_split)]
        node_indices[[2 * split_node + 1]] <- right_indices
        is_terminal[2 * split_node + 1] <- FALSE
        need_split[2 * split_node + 1] <- TRUE
      }
    }
  }
  for (i in 1:N_test) {
    index <- 1
    while(1) {
      if(is_terminal[index] == TRUE) {
        y_predicted[i] <- node_frequencies[index]
        break
      } else {
        if(x_test[i] <= node_splits[index]) {
          index <- index * 2
        } else {
          index <- index * 2 + 1
        }
      }
    }
  }
  RMSE[p / 5] <- rmse(y_test, y_predicted)
}

plot(seq(from = 5, to = 50, by = 5), RMSE, type = "b", las = 1,
     ylim = c(min(RMSE), max(RMSE)),
     xlab = "Pre-pruning size (P)", ylab = "RMSE")
lines(seq(from = 5, to = 50, by = 5), RMSE, type="b", col="black", lwd=1, pch=20)