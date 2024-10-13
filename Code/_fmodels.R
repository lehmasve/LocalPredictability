### ----------------------------------------
### Functions for Forecasting Models
### ----------------------------------------
# (Prevailing) Historical Mean (PHM)
phm <- function(y, window_size, mean_type = "rolling") {

  # Check NA
  if (any(is.na(y))) {
    rlang::abort("PHM: NA problem.")
  }

  # Calculate Rolling or Expanding Mean
  if (mean_type == "rolling") {
    preds <- dplyr::lag(roll::roll_mean(y, width = window_size, min_obs = 1), n = 1)
  } else if (mean_type == "expanding") {
    preds <- dplyr::lag(roll::roll_mean(y, width = nrow(y), min_obs = 1), n = 1)
  } else {
    stop("Invalid mean_type. Use 'rolling' or 'expanding'.")
  }

  # Rolling Variance
  vari <- dplyr::lag(roll::roll_var(y, width = window_size, min_obs = 1), n = 1)

  # Return
  return(list(pred = preds, var = vari))
}

### ----------------------------------------
# AR-Model (OLS)
ar_model <- function(x_train,
                     y_train,
                     x_pred,
                     mlags) {

  # Check NA
  if (any(is.na(x_train)) || any(is.na(y_train)) || any(is.na(x_pred))) {
    rlang::abort("AR: NA problem.")
  }

  # Split Data
  x_train_ar <- cbind(int = 1, x_train[, c(1:mlags), drop = FALSE])
  x_pred_ar  <- cbind(int = 1,  x_pred[, c(1:mlags), drop = FALSE])

  # Fit AR-Model
  model_ar <- stats::.lm.fit(x_train_ar, y_train)

  # Predict
  pred_ar <- x_pred_ar %*% model_ar$coefficients

  # Return
  return(pred_ar)
}

### ----------------------------------------
# Relaxed Lasso
relasso_model <- function(x_train,
                          y_train,
                          x_pred,
                          folds,
                          ran_st) {

  # Check NA
  if (any(is.na(x_train)) || any(is.na(y_train)) || any(is.na(x_pred))) {
    rlang::abort("Relax: NA problem.")
  }

  # Reproducibility: CV-Fold-ID
  set.seed(ran_st)
  foldid <- sample(rep(seq_len(folds), length.out = nrow(x_train)))

  # Cross-Validation for Lambda and Gamma
  rela_model <- glmnet::cv.glmnet(x = x_train,
                                  y = y_train,
                                  nfold = folds,
                                  alpha = 1,
                                  intercept = TRUE,
                                  standardize = TRUE,
                                  relax = TRUE,
                                  foldid = foldid)

  # Predict
  pred_rlass <- predict(rela_model,
                        newx = x_pred,
                        s = "lambda.min",
                        gamma = "gamma.min")

  # Return
  return(pred_rlass)
}

### ----------------------------------------
# XGBoost
xgb_model <- function(x_train,
                      y_train,
                      x_pred,
                      folds,
                      ntrees,
                      learning_rate,
                      target_number,
                      n_cores,
                      ran_st) {

  # Check NA
  if (any(is.na(x_train)) || any(is.na(y_train)) || any(is.na(x_pred))) {
    rlang::abort("XGB: NA problem.")
  }

  # Check Targeting
  if (!is.null(target_number)) {
    if (target_number > ncol(x_train)) {
      rlang::abort("XGB: Target number exceeds number of predictors.")
    }
  }

  # Targeting
  if (!is.null(target_number)) {

    # Fit Lasso-Model
    model_lasso <- glmnet::glmnet(x = x_train,
                                  y = y_train,
                                  alpha = 1,
                                  intercept = TRUE,
                                  standardize = TRUE,
                                  nlambda = 750)

    # Get Target Lambda
    target_lam_idx <- max(which(model_lasso$df <= target_number))
    target_lam <- model_lasso$lambda[target_lam_idx]

    # Get Target Predictors
    target_preds <- which(as.vector(coef(model_lasso, s = target_lam))[-1] != 0)
    if (length(target_preds) == 0 || is.null(target_preds)) {
      rlang::abort("XGB: No valid target predictors found.")
    }
  } else {
    target_preds <- seq_len(ncol(x_train))
  }

  # Convert Data
  dtrain <- xgboost::xgb.DMatrix(x_train[, target_preds, drop = FALSE],
                                 label = y_train)

  # Set up XGBoost Parameters
  param_xgb <- list(booster = "gbtree",
                    objective = "reg:squarederror",
                    eta = learning_rate,
                    nthread = n_cores)

  # Cross-Validation
  set.seed(ran_st)
  model_xgb.cv <- xgboost::xgb.cv(params = param_xgb,
                                  data = dtrain,
                                  nfold = folds,
                                  nrounds = ntrees,
                                  early_stopping_rounds = floor(0.2 * ntrees),
                                  verbose = 0,
                                  showsd = FALSE)

  # Train Final Model
  model_xgb <- xgboost::xgb.train(params = param_xgb,
                                  data = dtrain,
                                  nrounds = model_xgb.cv$best_iter,
                                  verbose = 0)

  # Predict
  pred_xgb <- predict(model_xgb, x_pred[, target_preds, drop = FALSE])

  # Return
  return(pred_xgb)
}

### ----------------------------------------
# Multiple Additive Regression Trees with Dropouts (LightGBM)
dart_model <- function(x_train,
                       y_train,
                       x_pred,
                       folds,
                       ntrees,
                       learning_rate,
                       drop_rate,
                       target_number,
                       n_cores,
                       ran_st) {

  # Check NA
  if (any(is.na(x_train)) || any(is.na(y_train)) || any(is.na(x_pred))) {
    rlang::abort("DART: NA problem.")
  }

  # Check Targeting
  if (!is.null(target_number)) {
    if (target_number > ncol(x_train)) {
      rlang::abort("DART: Target number exceeds number of predictors.")
    }
  }

  # Targeting
  if (!is.null(target_number)) {

    # Fit Lasso-Model
    model_lasso <- glmnet::glmnet(x = x_train,
                                  y = y_train,
                                  alpha = 1,
                                  intercept = TRUE,
                                  standardize = TRUE,
                                  nlambda = 750)

    # Get Target Lambda
    target_lam_idx <- max(which(model_lasso$df <= target_number))
    target_lam <- model_lasso$lambda[target_lam_idx]

    # Get Target Predictors
    target_preds <- which(as.vector(coef(model_lasso, s = target_lam))[-1] != 0)
    if (length(target_preds) == 0 || is.null(target_preds)) {
      rlang::abort("DART: No valid target predictors found.")
    }
  } else {
    target_preds <- seq_len(ncol(x_train))
  }

  # Convert Data
  dtrain <- lightgbm::lgb.Dataset(x_train[, target_preds, drop = FALSE],
                                  label = y_train)

  # Set up GBM Parameters
  param_lgb <- list(boosting = "dart",
                    objective = "regression",
                    metric = "mse",
                    data_sample_strategy = "bagging",
                    subsample_freq = 0,
                    subsample = 1.0,
                    feature_frac = 1.0,
                    enable_bundle = TRUE,
                    num_threads = n_cores,
                    learning_rate = learning_rate,
                    drop_rate = drop_rate,
                    early_stopping = 0,
                    num_leaves = 31,
                    max_depth = -1,
                    min_samples_leaf = 20,
                    force_row_wise = FALSE,
                    fore_col_wise = TRUE)

  # Cross-Validation
  set.seed(ran_st)
  model_lgb.cv <- lightgbm::lgb.cv(params = param_lgb,
                                   data = dtrain,
                                   nfold = folds,
                                   nrounds = ntrees,
                                   showsd = FALSE,
                                   verbose = -1)

  # Train Final Model
  model_lgb <- lightgbm::lgb.train(params = param_lgb,
                                   data = dtrain,
                                   nrounds = model_lgb.cv$best_iter,
                                   verbose = -1)

  # Predict
  pred <- predict(model_lgb, x_pred[, target_preds, drop = FALSE])

  # Return
  return(pred)
}

### ----------------------------------------
# Targeted Random Forest
trf_model <- function(x_train,
                      y_train,
                      x_pred,
                      target_number,
                      ntrees,
                      max_depth,
                      n_cores,
                      ran_st) {

  # Check
  if (!is.null(target_number)) {
    #if (target_number > nrow(x_train)) {
    #  rlang::abort("TRF: Target number exceeds number of observations.")
    #}
    if (target_number > ncol(x_train)) {
      rlang::abort("TRF: Target number exceeds number of predictors.")
    }
  }
  if (any(is.na(x_train)) || any(is.na(y_train)) || any(is.na(x_pred))) {
    rlang::abort("TRF: NA problem.")
  }

  # Targeting
  if (!is.null(target_number)) {

    # Fit Lasso-Model
    model_lasso <- glmnet::glmnet(x = x_train,
                                  y = y_train,
                                  alpha = 1,
                                  intercept = TRUE,
                                  standardize = TRUE,
                                  nlambda = 750)

    # Get Target Lambda
    target_lam_idx <- max(which(model_lasso$df <= target_number))
    target_lam <- model_lasso$lambda[target_lam_idx]

    # Get Target Predictors
    target_preds <- which(as.vector(coef(model_lasso, s = target_lam))[-1] != 0)
    if (length(target_preds) == 0 || is.null(target_preds)) {
      rlang::abort("TRF: No valid target predictors found.")
    }
  } else {
    target_preds <- seq_len(ncol(x_train))
  }

  # Train Model
  model_trf <- ranger::ranger(y = y_train,
                              x = x_train[, target_preds, drop = FALSE],
                              num.trees = ntrees,
                              mtry = floor(sqrt(length(target_preds))),
                              min.node.size = 5,
                              min.bucket = 1,
                              max.depth = max_depth,
                              sample.fraction = 1,
                              oob.error = FALSE,
                              node.stats = FALSE,
                              num.threads = n_cores,
                              seed = ran_st)

  # Predict
  pred_trf <- predict(model_trf, x_pred[, target_preds, drop = FALSE])$predictions #nolint

  # Return
  return(pred_trf)
}

### ----------------------------------------
# (Targeted) Complete Subset
csr_model <- function(x_train,
                      y_train,
                      x_pred,
                      target_number,
                      subset_length,
                      upper_bound,
                      sampling) {

  # Check
  if (!is.null(target_number)) {
    if (target_number < subset_length) {
      rlang::abort("CSR: Target number below subset length.")
    }
    if (target_number > nrow(x_train)) {
      rlang::abort("CSR: Target number exceeds number of observations.")
    }
    if (target_number > ncol(x_train)) {
      rlang::abort("CSR: Target number exceeds number of predictors.")
    }
    if (!sampling && target_number > 20) {
      rlang::abort("CSR: Sampling should be used for more than 20 predictors.")
    }
  }
  if (any(is.na(x_train)) || any(is.na(y_train)) || any(is.na(x_pred))) {
    rlang::abort("TRT: NA problem.")
  }
  if (sampling && is.null(upper_bound)) {
    rlang::abort("CSR: No upper bound given for sampling.")
  }
  if (!sampling && is.null(target_number) && ncol(x_train) > 20) {
    rlang::abort("CSR: Sampling should be used for more than 20 predictors.")
  }

  # Targeting
  if (!is.null(target_number)) {

    # Fit Lasso-Model
    model_lasso <- glmnet::glmnet(x = x_train,
                                  y = y_train,
                                  alpha = 1,
                                  intercept = TRUE,
                                  standardize = TRUE,
                                  nlambda = 750)

    # Get Target Lambda
    target_lam_idx <- max(which(model_lasso$df <= target_number))
    target_lam <- model_lasso$lambda[target_lam_idx]

    # Get Target Predictors
    target_preds <- which(as.vector(coef(model_lasso, s = target_lam))[-1] != 0)
    if (length(target_preds) == 0 || is.null(target_preds)) {
      rlang::abort("CSR: No valid target predictors found.")
    }
  } else {
    target_preds <- seq_len(ncol(x_train))
  }

  # If no Sampling
  if (!sampling) {

    # Get all Combinations of Length subset_length
    comb <- combn(target_preds, subset_length)

    # Set up Vector
    pred_csr <- rep(NA, ncol(comb))

    # Create Subset Forecasts
    for (u in seq_len(ncol(comb))) {

      # Select Predictors
      x_train_ss <- cbind(int = 1, x_train[, comb[, u], drop = FALSE])
      x_pred_ss  <- cbind(int = 1,  x_pred[, comb[, u], drop = FALSE])

      # Fit Model
      model_ss <- stats::.lm.fit(x_train_ss, y_train)

      # Predict
      pred_csr[u] <- x_pred_ss %*% model_ss$coefficients
    }

    # If Sampling
  } else if (sampling) {

    # Set up Vector
    pred_csr <- rep(NA, upper_bound)

    # Create Subset Forecasts
    for (u in seq_len(upper_bound)) {

      # Sample Predictors
      set.seed(u)
      idx <- sample(target_preds, subset_length)

      # Select Predictors
      x_train_ss <- cbind(int = 1, x_train[, idx, drop = FALSE])
      x_pred_ss  <- cbind(int = 1,  x_pred[, idx, drop = FALSE])

      # Fit Model
      model_ss <- stats::.lm.fit(x_train_ss, y_train)

      # Predict
      pred_csr[u] <- model_ss$coefficients %*% x_pred_ss[,]
    }
  } else {
    rlang::abort("CSR: Sampling problem.")
  }

  # Compute Aggregate Forecast
  pred <- mean(pred_csr)

  # Return
  return(pred)
}

### ----------------------------------------
### STSC Variants
# STSC-Sx - Using LASSO so select X forecasts ###
stscsx <- function(y,
                   X,
                   Ext_F,
                   lambda_grid,
                   kappa_grid,
                   init,
                   bias,
                   n_cores,
                   window_size,
                   target_number) {

  # TVC-Forecasts
  results_tvc <- hdflex::tvc(y,
                             X,
                             Ext_F,
                             init,
                             lambda_grid,
                             kappa_grid,
                             bias)

  # Assign Result
  forecast_tvc <- results_tvc$Forecasts$Point_Forecasts
  variance_tvc <- results_tvc$Forecasts$Variance_Forecasts

  # Remove
  rm(list = c("results_tvc"))

  # Get first Non-Na-Row
  first_complete <- which(complete.cases(forecast_tvc))[1]
  non_na_idx <- seq(first_complete, nrow(forecast_tvc))
  adj <- first_complete - 1

  # Subset Data
  y_sub <- y[non_na_idx, , drop = FALSE]
  forecast_tvc_sub <- forecast_tvc[non_na_idx, , drop = FALSE]
  variance_tvc_sub <- variance_tvc[non_na_idx, , drop = FALSE]

  # Remove
  rm(list = c("forecast_tvc", "variance_tvc"))

  # Set Time-Sequence
  win <- window_size - 1
  t_start <- win
  t_end <- nrow(y_sub) - 1
  t_seq <- seq(t_start, t_end)

  # Result-Object: Prediction-Variance-Matrix
  res  <- matrix(NA, nrow = nrow(y), ncol = 2,
                 dimnames = list(rownames(y), c("STSCSx", "STSCSx_VAR")))

  # Open Parallel Backend
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)

  # Parallel Loop over time
  res[t_seq + adj + 1, ] <- foreach::foreach(t = t_seq,
                                             .combine = "rbind",
                                             .packages = c("glmnet")) %dopar% {

    # Set Training-Data
    x_train <- forecast_tvc_sub[(t - win + 1):t, , drop = FALSE]
    y_train <- y_sub[(t - win + 1):t, , drop = FALSE]

    # Step 1: Candidate Model Selection (Lasso)
    model_lasso <- glmnet::glmnet(x = x_train,
                                  y = y_train,
                                  alpha = 1,
                                  intercept = TRUE,
                                  standardize = FALSE,
                                  nlambda = 750)

    # Get Target Lambda
    target_lam_idx <- max(which(model_lasso$df <= target_number))
    target_lam <- model_lasso$lambda[target_lam_idx]

    # Get Target Predictors
    target_preds <- which(as.vector(coef(model_lasso, s = target_lam))[-1] != 0)
    n_target <- length(target_preds)
    if (n_target == 0 || is.null(target_preds)) {
      rlang::abort("STSC-Sx: No valid target predictors found.")
    }

    # Step 2: Equal Weight Combination (Logarithmic Combination)
    weights <- rep(1.0 / n_target, n_target)
    variance_agg <- 1.0 / sum(weights / variance_tvc_sub[t + 1, target_preds])
    forecast_agg <- sum(weights * forecast_tvc_sub[t+1, target_preds] / variance_tvc_sub[t+1, target_preds]) * variance_agg #nolint

    # Return
    return(cbind(forecast_agg, variance_agg))
  }

  # Close Parallel Backend
  parallel::stopCluster(cl)

  # Return
  return(res)
}

### ----------------------------------------
# STSC-SFLEX - Using LASSO to dynamically select forecasts ###
stscsflex <- function(y,
                      X,
                      Ext_F,
                      lambda_grid,
                      kappa_grid,
                      init,
                      bias,
                      n_cores,
                      window_size,
                      folds) {

  # TVC-Forecasts
  results_tvc <- hdflex::tvc(y,
                             X,
                             Ext_F,
                             init,
                             lambda_grid,
                             kappa_grid,
                             bias)

  # Assign Result
  forecast_tvc <- results_tvc$Forecasts$Point_Forecasts
  variance_tvc <- results_tvc$Forecasts$Variance_Forecasts

  # Remove
  rm(list = c("results_tvc"))

  # Get first Non-Na-Row
  first_complete <- which(complete.cases(forecast_tvc))[1]
  non_na_idx <- seq(first_complete, nrow(forecast_tvc))
  adj <- first_complete - 1

  # Subset Data
  y_sub <- y[non_na_idx, , drop = FALSE]
  forecast_tvc_sub <- forecast_tvc[non_na_idx, , drop = FALSE]
  variance_tvc_sub <- variance_tvc[non_na_idx, , drop = FALSE]

  # Remove
  rm(list = c("forecast_tvc", "variance_tvc"))

  # PHM-Model
  results_phm <- hdflex::tvc(y,
                             matrix(rep(0, length(y))),
                             NULL,
                             init,
                             lambda_grid,
                             kappa_grid,
                             bias)

  # Assign Result
  forecast_phm_sub <- results_phm$Forecasts$Point_Forecasts[non_na_idx, , drop = FALSE]
  variance_phm_sub <- results_phm$Forecasts$Variance_Forecasts[non_na_idx, , drop = FALSE]

  # Remove
  rm(list = c("results_phm"))

  # Set Time-Sequence
  win <- window_size - 1
  t_start <- win
  t_end <- nrow(y_sub) - 1
  t_seq <- seq(t_start, t_end)

  # Result-Object: Prediction-Variance-Matrix
  res <- matrix(NA, nrow = nrow(y), ncol = 2,
                dimnames = list(rownames(y), c("STSCSFLEX", "STSCSFLEX_VAR")))

  # Open Parallel Backend
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)

  # Parallel Loop over time
  res[t_seq + adj + 1, ] <- foreach::foreach(t = t_seq,
                                             .combine = "rbind",
                                             .packages = c("glmnet")) %dopar% {

    # Set Training-Data
    x_train <- forecast_tvc_sub[(t - win + 1):t, , drop = FALSE]
    y_train <- y_sub[(t - win + 1):t, , drop = FALSE]

    # Reproducibility: CV-Foldid
    set.seed(t)
    foldid <- sample(rep(seq_len(folds), length.out = nrow(x_train)))

    # Step1: Candidate Model Selection (Lasso)
    glm_model <- glmnet::cv.glmnet(x = x_train,
                                   y = y_train,
                                   nfold = folds,
                                   alpha = 1,
                                   intercept = TRUE,
                                   standardize = FALSE,
                                   foldid = foldid)

    # Step 2: Equal Weight Combination (Logarithmic Combination)
    target_cf <- which(as.vector(coef(glm_model, s = "lambda.min"))[-1] != 0)
    n_target <- length(target_cf)

    if (n_target == 0) {
      weights <- rep(1.0 / ncol(forecast_phm_sub), ncol(forecast_phm_sub))
      variance_agg <- 1.0 / sum(weights / variance_phm_sub[t + 1, ])
      forecast_agg <- sum(weights * forecast_phm_sub[t + 1, ] / variance_phm_sub[t + 1, ]) * variance_agg

    } else if (n_target > 0) {
      weights <- rep(1.0 / n_target, n_target)
      variance_agg <- 1.0 / sum(weights / variance_tvc_sub[t + 1, target_cf])
      forecast_agg <- sum(weights * forecast_tvc_sub[t+1, target_cf] / variance_tvc_sub[t+1, target_cf]) * variance_agg #nolint

    } else {
      rlang::abort("STSC-SFLEX: No valid candidate models found.")
    }

    # Return
    return(cbind(forecast_agg, variance_agg))
  }

  # Close Parallel Backend
  parallel::stopCluster(cl)

  # Return
  return(res)
}

### ----------------------------------------
# Prinipal Component Dynamic Model Averaging
pcdma <- function(y,
                  X,
                  Ext_F,
                  window_size,
                  n_comp,
                  alpha,
                  lambda,
                  kappa,
                  n_cores,
                  exl_pca = NULL) {

  ### Check
  if (nrow(y) != nrow(X)) {
    rlang::abort("PC-DMA: Number of observations in y and X do not match.")
  }
  if (window_size > nrow(y)) {
    rlang::abort("PC-DMA: Window size exceeds number of observations.")
  }
  if (n_comp > window_size) {
    rlang::abort("PC-DMA: Number of Components exceeds Window Size.")
  }

  # Response-Name
  colnames(y) <- "response"

  # Combine Signals and Remove NA-Values
  S_sub <- na.omit(cbind(if (exists("X")) X,
                         if (exists("Ext_F")) Ext_F))

  # Remove
  rm(list = c("X", "Ext_F"))

  ### Check
  if (ncol(S_sub) < 15) {
    rlang::abort("PC-DMA: Simple DMA would be better suited.")
  }
  if (n_comp > ncol(S_sub)) {
    rlang::abort("PC-DMA: Number of Components exceeds number of predictors.")
  }

  # Separate predictors for PCA and those to be added directly to DMA
  if (!is.null(exl_pca)) {
    S_pca <- S_sub[, -exl_pca, drop = FALSE]
    S_exl <- S_sub[, exl_pca, drop = FALSE]
  } else {
    S_pca <- S_sub
    S_exl <- NULL
  }

  # Get first non-NA-row
  first_complete <- nrow(y) - nrow(S_sub) + 1
  non_na_idx <- seq(first_complete, nrow(y))

  # Remove
  rm(list = c("S_sub"))

  # Subset Response
  y_sub <- y[non_na_idx, , drop = FALSE]

  # Result-Object: Prinipal Components
  res_pca_sub <- matrix(NA, ncol = n_comp, nrow = nrow(y_sub),
                        dimnames = list(rownames(y_sub),
                                        paste0("PC", seq_len(n_comp))))

  # Initial PCA-Projection
  row_idx <- seq(window_size - 1)
  pca_model <- stats::prcomp(S_pca[row_idx, , drop = FALSE],
                             center = TRUE,
                             scale. = TRUE)
  res_pca_sub[row_idx, ] <- pca_model$x[, seq_len(n_comp), drop = FALSE]

  # Remove
  rm(list = c("pca_model", "row_idx"))

  # Time Sequence
  t_start <- window_size
  t_end <- nrow(y_sub)
  t_seq <- seq(t_start, t_end)

  # Open Parallel Backend
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)

  # Project Signals for every t
  res_pca_sub[t_seq, ] <- foreach::foreach(t = t_seq,
                                           .combine = "rbind",
                                           .packages = c("stats")) %dopar% {

    # Get Training-Data
    s_train <- S_pca[(t - window_size + 1):t, , drop = FALSE]

    # Fit PCA
    pca_model <- stats::prcomp(s_train, center = TRUE, scale. = TRUE)

    # Return
    return(pca_model$x[window_size, seq_len(n_comp), drop = FALSE])
  }

  # Close Parallel Backend
  parallel::stopCluster(cl)

  # Combine PCA results with excluded predictors
  if (!is.null(S_exl)) {
    combined_data <- cbind(y_sub, S_exl, res_pca_sub)
  } else {
    combined_data <- cbind(y_sub, res_pca_sub)
  }

  # Remove
  rm(list = c("res_pca_sub", "S_exl"))

  # Set up DMA-Result Matrix
  res_pcdma <- matrix(NA, ncol = 4, nrow = nrow(y),
                      dimnames = list(rownames(y),
                                      c("PC_DMA_MU",
                                        "PC_DMA_PLL",
                                        "PC_DMA_VAR",
                                        "PC_DMA_CRPS")))

  # Fit DMA Model
  model <- eDMA::DMA(formula = response ~ .,
                     data = combined_data,
                     dAlpha = alpha,
                     vDelta = lambda,
                     vKeep = NULL,
                     bZellnerPrior = FALSE,
                     dG = 100,
                     bParallelize = FALSE,
                     iCores = 1,
                     dBeta = kappa)

  # Continuous-Ranked-Probability-Score
  crps_score <- compute_crps(model, y_sub, lambda)

  # Assign estimated Quantities
  res_pcdma[non_na_idx[-1], 1] <- model@Est$vyhat[-1]
  res_pcdma[non_na_idx[-1], 2] <- model@Est$vLpdfhat[-1]
  res_pcdma[non_na_idx[-1], 3] <- model@Est$vtotal[-1]
  res_pcdma[non_na_idx[-1], 4] <- crps_score

  # Remove
  rm(list = c("model", "crps_score", "y_sub"))

  # Cut Init-Period
  res_pcdma[non_na_idx[1:window_size], ] <- NA

  # Return
  return(res_pcdma)
}

### ----------------------------------------
# Dynamic Model Averaging
dma <- function(y,
                X,
                Ext_F,
                alpha,
                lambda,
                kappa) {

  ### Check
  if (nrow(y) != nrow(X)) {
    rlang::abort("DMA: Number of observations in y and X do not match.")
  }

  # Response-Name
  colnames(y) <- "response"

  # Combine Signals and Remove NA-Values
  S_sub <- na.omit(cbind(if (exists("X")) X,
                         if (exists("Ext_F")) Ext_F))

  # Check
  if (ncol(S_sub) > 20) {
    rlang::abort("DMA: Use PC-DMA instead.")
  }

  # Get first non-NA-row
  first_complete <- nrow(y) - nrow(S_sub) + 1
  non_na_idx <- seq(first_complete, nrow(y))

  # Subset Response
  y_sub <- y[non_na_idx, , drop = FALSE]

  # Set up Result Matrix
  res_dma <- matrix(NA, ncol = 4, nrow = nrow(y),
                    dimnames = list(rownames(y),
                                    c("PC_DMA_MU",
                                      "PC_DMA_PLL",
                                      "PC_DMA_VAR",
                                      "PC_DMA_CRPS")))

  # Fit DMA Model
  model <- eDMA::DMA(formula = response ~ .,
                     data = cbind(y_sub, S_sub),
                     dAlpha = alpha,
                     vDelta = lambda,
                     vKeep = NULL,
                     bZellnerPrior = FALSE,
                     dG = 100,
                     bParallelize = FALSE,
                     iCores = 1,
                     dBeta = kappa)

  # Continuous-Ranked-Probability-Score
  crps_score <- compute_crps(model, y_sub, lambda)

  # Assign estimated Quantities
  res_dma[non_na_idx[-1], 1] <- model@Est$vyhat[-1]
  res_dma[non_na_idx[-1], 2] <- model@Est$vLpdfhat[-1]
  res_dma[non_na_idx[-1], 3] <- model@Est$vtotal[-1]
  res_dma[non_na_idx[-1], 4] <- crps_score

  # Remove
  rm(list = c("model", "crps_score", "y_sub"))

  # Return
  return(res_dma)
}

######## ----------------------------------------
### Continuous Ranked Probability Score (CRPS) Function
# Function to compute the CRPS
crps <- function(obs, mu, sig, df, dis) {

  "
  For reference see: 
  http://journals.ametsoc.org/doi/pdf/10.1175/MWR2904.1
  https://CRAN.R-project.org/package=scoringRules
  "

  # Check Distribution
  if (dis == "t") {
    if (is.null(df)) {
      rlang::abort("Invalid Degrees of Freedom")
    }
  }

  # Convert
  obs <- as.numeric(obs)
  mu  <- as.numeric(mu)
  sig <- as.numeric(sig)

  # Check Dimensions
  if (length(obs) != length(mu) || length(obs) != length(sig)) {
    rlang::abort("Invalid Dimensions")
  }

  # Normal or Students t-Distribution
  if (dis == "normal") {

    # Standardized obs
    z  <- (obs - mu) / sig
    # PDF evaluated at the normalized predition error
    pdf <- dnorm(z, 0.0, 1.0)
    # CDF evaluated at the normalized predition error
    cdf <- pnorm(z, 0.0, 1.0)
    # Inverse of pi
    pi_inv <- 1.0 / sqrt(pi)
    # Compute Continuous Ranked Probability Score
    crps <- sig * (z * (2.0 * cdf - 1.0) + 2.0 * pdf - pi_inv)

  } else if (dis == "t") {

    ### For formula see: 10.32614/CRAN.package.scoringRules

    # Scale Conversion
    s <- sig * sqrt((df - 2.0) / df)
    # Standardized obs
    z  <- (obs - mu) / s
    # PDF evaluated at the normalized predition error
    pdf <- dt(z, df)
    # CDF evaluated at the normalized predition error
    cdf <- pt(z, df)
    # Beta-Function
    bfrac <- beta(0.5, df - 0.5) / beta(0.5, 0.5 * df) ** 2.0
    # Compute Continuous Ranked Probability Score
    crps <- s * (
      z * (2.0 * cdf - 1.0) +
        2.0 / (df - 1.0) *
          (pdf * (df + z ** 2.0) - sqrt(df) * bfrac)
    )
  } else {
    stop("Invalid Distribution")
  }

  # Return
  return(list(crps = crps, average_score = mean(crps)))
}

### ----------------------------------------
### Function for eDMA::DMA to calculate CRPS
compute_crps <- function(model, y_sub, lambda) {

  # Continuous-Ranked-Probability-Score
  Reduce("+", lapply(seq_along(lambda), function(i) {

    # Set Degress of Freedom
    dlm_n <- model@Est$vdfree[-1]

    # Set DLM-Variance
    dlm_Q <- model@Est$dlm_Q[[i]][-1, ]

    # Set DLM-Forecast
    dlm_f <- model@Est$dlm_f[[i]][-1, ]

    # Set DLM-Weight
    dlm_w <- model@Est$dlm_w[[i]][-1, ]

    # Set Forgetting-Factor-Weight
    d_w   <- model@Est$mpmt[-1, i]

    # Compute DLM-CRPS
    dlm_crps <- do.call("cbind",
                        lapply(seq_len(ncol(dlm_f)),
                               function(i) {crps(y_sub[-1],
                                                 dlm_f[, i],
                                                 sqrt(dlm_Q[, i]),
                                                 dlm_n,
                                                 "t")$crps}))

    # Compute DMA-CRPS
    dma_crps <- rowSums(dlm_crps * dlm_w)

    ## Compute DMA-D-CRPS
    dmad_crps <- dma_crps * d_w

    # Return
    return(dmad_crps)
  }))
}
