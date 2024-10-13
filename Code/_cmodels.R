### ------------------------------------
### Functions to generate different candidate models (-> F-Signals)
### ------------------------------------
### Decision Trees
cm_dt <- function(x_train,
                  y_train,
                  x_pred,
                  vec_depth,
                  vec_active,
                  windows) {

  # Set active variables if not provided
  if (is.null(vec_active)) {
    vec_active <- seq_len(ncol(x_train))
  }

  # Check for NA-Values
  if (any(is.na(x_train)) || any(is.na(y_train)) || any(is.na(x_pred))) {
    stop("DT: NA problem.")
  }

  # Ensure y_train has a column name "response"
  colnames(y_train) <- "response"

  # Result-Object: Forecasts
  col_names <- apply(expand.grid(vec_depth, windows), 1,
                     function(x) paste0("TREE_W", x[2], "_D", x[1]))
  pred <- matrix(NA, ncol = length(windows) * length(vec_depth), nrow = 1,
                 dimnames = list(NULL, col_names))

  # Number of Observations
  n_obs <- nrow(x_train)

  # Counter for Models
  i <- 1

  # Convert Test-Data
  test_df <- as.data.frame(x_pred[, vec_active, drop = FALSE])

  # Loop over windows and depths to create forecasts
  for (win in windows) {

    # Expanding Window
    win <- ifelse(win == 0, n_obs, win)

    # Convert Train-Data
    train_df <- as.data.frame(cbind(y_train[(n_obs - win + 1):n_obs, , drop = FALSE],
                                    x_train[(n_obs - win + 1):n_obs, vec_active, drop = FALSE]))

    # Loop over Depths
    for (d in vec_depth) {

      # Fit Model
      model <- rpart::rpart(response ~ .,
                            data = train_df,
                            method = "anova",
                            maxdepth = d,
                            minsplit = 15)

      # Predict
      pred[i] <- predict(model, test_df)

      # Update Counter
      i <- i + 1
    }
  }

  # Return
  return(pred)
}

### ------------------------------------
### Elastic-Net
cm_eln <- function(x_train,
                   y_train,
                   x_pred,
                   vec_alpha,
                   vec_active,
                   windows,
                   ran_st) {

  # Set active variables if not provided
  if (is.null(vec_active)) {
    vec_active <- seq_len(ncol(x_train))
  }

  # Check for NA-Values
  if (any(is.na(x_train)) || any(is.na(y_train)) || any(is.na(x_pred))) {
    rlang::abort("ELN: NA problem.")
  }

  # Result-Object: Forecasts
  col_names <- apply(expand.grid(vec_alpha, windows), 1,
                     function(x) paste0("ELN_W", x[2], "_A", x[1]))
  pred <- matrix(NA, ncol = length(windows) * length(vec_alpha), nrow = 1,
                 dimnames = list(NULL, col_names))

  # Number of Observations
  n_obs <- nrow(x_train)

  # Counter for Models
  i <- 1

  # Loop over windows and alphas to create forecasts
  for (win in windows) {

    # Expanding Window
    win <- ifelse(win == 0, n_obs, win)

    # Train Data
    x_train_sub <- x_train[(n_obs - win + 1):n_obs, vec_active, drop = FALSE]
    y_train_sub <- y_train[(n_obs - win + 1):n_obs, , drop = FALSE]

    # Reproducibility: CV-Foldid
    set.seed(ran_st)
    foldid <- sample(rep(seq_len(5), length.out = nrow(x_train_sub)))

    # Loop over different alphas
    for (a in vec_alpha) {

      # Fit Model
      glm_model <- glmnet::cv.glmnet(x = x_train_sub,
                                     y = y_train_sub,
                                     nfold = 5,
                                     alpha = a,
                                     intercept = TRUE,
                                     standardize = TRUE,
                                     foldid = foldid)

      # Predict
      pred[i] <- predict(glm_model,
                         newx = x_pred[, vec_active, drop = FALSE],
                         s = "lambda.min")

      # Update Counter
      i <- i + 1
    }
  }

  # Return
  return(pred)
}

### ----------------------------------------
### GBM Volatility-Tree (Application: Finance)
cm_gbm <- function(x_train,
                   y_train,
                   x_pred,
                   folds,
                   ntrees,
                   learning_rate,
                   n_cores,
                   ran_st) {

  # Check for NA-Values
  if (any(is.na(x_train)) || any(is.na(y_train)) || any(is.na(x_pred))) {
    rlang::abort("GBM: NA problem.")
  }

  # Convert Data
  dtrain <- lightgbm::lgb.Dataset(x_train, label = y_train)

  # Set up GBM Parameters
  param_lgb <- list(boosting = "gbdt",
                    objective = "regression",
                    metric = "mse",
                    num_threads = n_cores,
                    learning_rate = learning_rate,
                    force_row_wise = TRUE,
                    early_stopping = -1,
                    verbose = -1)

  # Cross-Validation
  set.seed(ran_st)
  model_lgb.cv <- lightgbm::lgb.cv(params = param_lgb,
                                   data = dtrain,
                                   nfold = folds,
                                   nrounds = ntrees,
                                   showsd = FALSE)

  # Train Final Model
  model_lgb <- lightgbm::lgb.train(params = param_lgb,
                                   data = dtrain,
                                   nrounds = model_lgb.cv$best_iter)

  # Predict
  pred <- predict(model_lgb, x_pred)

  # Return
  return(pred)
}

### ------------------------------------
### Combination of DP- and TBL-Model (Application: Finance)
cm_dp_tbl <- function(x_train,
                      y_train,
                      x_pred) {

  # Check for NA-Values
  if (any(is.na(x_train)) || any(is.na(y_train)) || any(is.na(x_pred))) {
    rlang::abort("DP-TBL: NA problem.")
  }
  if (ncol(x_train) != 2) {
    rlang::abort("DP-TBL: Number of predictors exceeds 2.")
  }

  # Add Intercept and Subset
  x_train_dp  <- cbind(int = 1, x_train[, c("dp"),  drop = FALSE])
  x_train_tbl <- cbind(int = 1, x_train[, c("tbl"), drop = FALSE])

  # Fit Models
  model_dp  <- stats::.lm.fit(x_train_dp, y_train)
  model_tbl <- stats::.lm.fit(x_train_tbl, y_train)

  # Predict
  pred_dp  <- model_dp$coefficients  %*% c(1, x_pred[, "dp"])
  pred_tbl <- model_tbl$coefficients %*% c(1, x_pred[, "tbl"])

  # Combine Predictions
  pred <- (pred_dp + pred_tbl) / 2

  # Return
  return(pred)
}

### ------------------------------------
### Principal Component Regression (Application: Finance)
cm_pcr <- function(x_train,
                   y_train,
                   x_pred,
                   n_comp,
                   val) {

  # Check for NA-Values
  if (any(is.na(x_train)) || any(is.na(y_train)) || any(is.na(x_pred))) {
    rlang::abort("PCR: NA problem.")
  }

  # Check Dimensions
  if (n_comp > ncol(x_train)) {
    rlang::abort("PCR: Number of components exceeds number of predictors.")
  }

  # Perform PCA
  pca_model <- stats::prcomp(x_train, scale. = TRUE, center = TRUE)

  # Validate Number of Components with Adjusted R2
  if (val) {

    # Initialize
    max_adj_r2 <- -Inf
    best_idx <- 1

    # Loop over Components
    for (i in seq(n_comp)) {

      # Fit Regression
      model <- stats::.lm.fit(x = cbind(1, pca_model$x[, 1:i, drop = FALSE]),
                              y = y_train)

      # Calculate adjusted R-squared
      y_hat <- model$coefficients[1] +
        pca_model$x[, 1:i, drop = FALSE] %*%
          model$coefficients[-1]
      r2 <- 1 - sum((y_train - y_hat)^2) / sum((y_train - mean(y_train))^2)
      adj_r2 <- 1 - (1 - r2) * (nrow(y_train) - 1) / (nrow(y_train) - i - 1)

      # Update best Model
      if (adj_r2 > max_adj_r2) {
        max_adj_r2 <- adj_r2
        best_idx <- i
      }
    }
  } else {
    # If no validation take fix n_comp
    best_idx <- n_comp
  }

  # Re-Fit Regression
  model <- .lm.fit(x = cbind(1, pca_model$x[, 1:best_idx, drop = FALSE]),
                   y = y_train)

  # Project Test Data
  x_pred_pca <- predict(pca_model, newdata = x_pred)[, 1:best_idx]

  # Predict
  pred <- model$coefficients[1] + x_pred_pca %*% model$coefficients[-1]

  # Return
  return(pred)
}

#######################
# # Principal Component Regression - Finance
# pcr_model <- function(x_train,
#                       y_train,
#                       x_pred,
#                       n_comp,
#                       folds,
#                       ran_st) {
# 
#   # Check for NA-Values
#   if (any(is.na(x_train)) || any(is.na(y_train)) || any(is.na(x_pred))) {
#     stop("PCR: NA problem.")
#   }
#   if (n_comp > ncol(x_train)) {
#     stop("PCR: Number of components exceeds number of predictors.")
#   }
# 
#   # Response-Name
#   colnames(y_train) <- "response"
# 
#   # Train Data
#   df_train <- as.data.frame(cbind(y_train, x_train))
# 
#   # Test Data
#   df_test <- as.data.frame(x_pred)
# 
#   # Set Seed
#   set.seed(ran_st)
# 
#   # Fit Model
#   model_pcr <- pls::pcr(response ~ .,
#                         ncomp  = n_comp,
#                         data   = df_train,
#                         scale  = TRUE,
#                         center = TRUE,
#                         validation = c("CV"),
#                         segments = folds,
#                         model = FALSE)
# 
#   # Get CV-Number of Components
#   cv_ncomp <- which.min(pls::RMSEP(model_pcr, estimate = "adjCV")$val[-1])
# 
#   # Predict
#   pred <- predict(model_pcr, df_test, ncomp = cv_ncomp)
# 
#   # Return
#   return(pred)
# }