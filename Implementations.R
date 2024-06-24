linear_regression <- function(Y, X) {
  # Checking if Y and X are valid
  if (!is.matrix(Y) || !is.matrix(X)) {
    stop("Y and X must be matrices.")
  }
  
  # Check if Y and X are valid
  if (!all(sapply(Y, is.numeric)) || !all(sapply(X, is.numeric))) {
    stop("Y and X must contain numeric values only.")
  }
  
  # Check dimensions of Y and X
  if (nrow(Y) != nrow(X)) {
    stop("Number of rows in Y must be equal to the number of rows in X.")
  }
  
  # Add intercept to the design matrix
  X <- cbind(1, X)
  
  # Estimate regression coefficients
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  
  # Estimates of Y
  Y_hat <- X %*% beta
  
  # Residuals
  residuals <- Y - Y_hat
  
  # TSS
  TSS <- sum((Y - mean(Y))^2)
  
  # RMSS
  RMSS <- sum((Y_hat - mean(Y))^2)
  
  # RSS
  RSS <- sum(residuals^2)
  
  # R-square
  R_square <- RMSS / TSS
  
  result <- list(
    "Estimates of Y" = Y_hat,
    "Residuals" = residuals,
    "TSS" = TSS,
    "RMSS" = RMSS,
    "RSS" = RSS,
    "R_square" = R_square,
    "Coefficients" = beta
  )
  
  return(result)
}

model_selection <- function(Y, X) {
  models <- list()
  
  print("Entering model_selection function")
  print(paste("Type of Y:", class(Y)))
  print(paste("Type of X:", class(X)))
  
  # Convert Y to matrix if it's not already
  if (!is.matrix(Y)) {
    Y <- as.matrix(Y)
  }
  
  # Convert X to matrix if it's not already
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  
  # Generate all possible combinations of X variables
  for (i in 1:ncol(X)) {
    print(paste("Number of Variables:", i))
    combinations <- combn(ncol(X), i)
    for (j in 1:ncol(combinations)) {
      print(paste("Combination:", j))
      print(combinations[, j])
      model <- list(
        "Number of Variables" = i,
        "Variable (X) Name" = paste(colnames(X)[combinations[, j]], collapse = " "),
        "TSS" = NA,
        "RMSS" = NA,
        "RSS" = NA,
        "R_Square" = NA
      )
      
      # Ensure the subset of X is a matrix
      X_subset <- X[, combinations[, j], drop = FALSE]
      
      # Perform linear regression for each combination
      result <- linear_regression(Y, X_subset)
      
      model$TSS <- result$TSS
      model$RMSS <- result$RMSS
      model$RSS <- result$RSS
      model$R_Square <- result$R_square
      
      models[[length(models) + 1]] <- model
    }
  }
  
  # Convert list of models to data frame
  models_df <- do.call(rbind, lapply(models, as.data.frame))
  
  # Ensure R_Square is numeric
  models_df$R_Square <- as.numeric(models_df$R_Square)
  
  # Sort data frame based on R-square in descending order
  models_df <- models_df[order(-models_df$R_Square), ]
  
  return(models_df)
}

# Function to check and convert data to numeric
convert_to_numeric <- function(data) {
  for (i in 1:ncol(data)) {
    data[, i] <- as.numeric(data[, i])
  }
  return(data)
}

# Read data from file
data <- read.table("data.txt", header = TRUE)

# Convert data to numeric
data <- convert_to_numeric(data)

# Split data into Y and X 
Y <- as.matrix(data[, 2])  
X <- as.matrix(data[, -c(1, 2)])  

# Task 1: Simple and multiple linear regression analysis
result <- linear_regression(Y, X)
print(result)

# Task 2: Model selection based on R-square
model_selection_result <- model_selection(as.matrix(Y), as.matrix(X))
print(model_selection_result)

