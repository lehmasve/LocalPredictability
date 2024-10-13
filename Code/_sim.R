### ------------------------------------
### Functions for Simulation Study
### ------------------------------------
### DGP Function: Time-varying parameter process
f_1 <- function(x,
                int,
                beta,
                n_init,
                norm_c) {

  # Total observations
  n_obs <- nrow(x)

  # Generate time-varying-parameter
  theta <- rep(NA, n_obs)
  t <- seq_len(n_obs)
  theta[] <- int + beta * (t - n_init) / norm_c

  # Compute function value
  f_x <- theta * x

  # Return
  return(f_x)
}

### ------------------------------------
### DGP Function: Non-linear additive Friedman (1991) process
f_2 <- function(X) {

  # Modeling data with additive, nonlinear dependence
  f_x <- 0.1 * exp(4.0 * X[, 1]) +
         4.0 / (1.0 + exp(-10.0 * (X[, 2] - 0.5))) +
         0.5 ** X[, 3] +
         X[, 4] ** 0.5 +
         X[, 5] ** 2.0

  # Return
  return(f_x)
}

### ------------------------------------
### DGP Function: Non-linear interactive Friedman (1991) process
f_3 <- function(X) {

  # Modeling data with interactions
  f_x <- 5.0 * sin(pi * X[, 1] * X[, 2]) +
         10.0 * (X[, 3] - 0.5) ** 2.0 +
         X[, 4] ** 0.5 * X[, 5] ** 2.0

  # Return
  return(f_x)
}

### ------------------------------------
### DGP Function: Linear-additive (dense) process
f_4 <- function(X, beta) {

  # Modeling (dense) linear-additive predictability structure
  f_x <- beta * rowSums(X)

  # Return
  return(f_x)
}

### ------------------------------------
### DGP Function: Pure noise process
f_0 <- function(n_obs, n_init) {

  # Modeling zero
  f_x <- rep(0.0, (n_obs + n_init))

  # Return
  return(f_x)
}

### ------------------------------------
### Wrapper for DGP functions
wrapper_f <- function(x1,
                      X2,
                      X3,
                      X4,
                      dgp,
                      n_obs,
                      n_init) {

  # Dimension checks
  if (ncol(x1) != 1) {
    rlang::abort("Invalid x1")
  }
  if (ncol(X2) != 5) {
    rlang::abort("Invalid X2")
  }
  if (ncol(X3) != 5) {
    rlang::abort("Invalid X3")
  }
  if (ncol(X4) != 50) {
    rlang::abort("Invalid X4")
  }
  if (any(c(nrow(x1), nrow(X2), nrow(X3), nrow(X4)) != (n_obs + n_init))) {
    rlang::abort("Unequal Number of Observations")
  }

  # Initialize Output Vector
  output <- rep(0.0, (n_obs + n_init))
  ctr <- 0

  # Loop over DGP
  for (sub_process in dgp) {

    # Assign start and end value for time period
    start <- sub_process$start
    end <- sub_process$end

    # Assign active DGP-Function in this period
    dgp_func <- sub_process$func

    # Assign (potentially relevant) F1-Paramter for this period
    tvp_params <- sub_process$tvp
    int_tvp <- tvp_params[1]
    beta_tvp <- tvp_params[2]
    normc_tvp <- tvp_params[3]

    # Assign (potentially relevant) F4 parameter for this period
    beta_dense <- sub_process$dense

    # F1-Check
    if ("F1" %in% dgp_func && (is.null(int_tvp) || is.null(beta_tvp) || is.null(normc_tvp))) {
      rlang::abort("Parameter for F1 not defined")
    }

    # F4-Check
    if ("F4" %in% dgp_func && is.null(beta_dense)) {
      rlang::abort("Parameter for F4 not defined")
    }

    # Update counter
    ctr <- ctr + length(seq(start, end))

    # Loop over active functions in current time period
    for (func in dgp_func) {

      # Calculate function value
      f_x <- switch(func,
        "F0" = f_0(n_obs, n_init),
        "F1" = f_1(x1, int_tvp, beta_tvp, n_init, normc_tvp),
        "F2" = f_2(X2),
        "F3" = f_3(X3),
        "F4" = f_4(X4, beta_dense),
        rlang::abort("Invalid DGP-function specified.")
      )

      # Assign to Output
      output[start:end] <- output[start:end] + f_x[start:end]
    }
  }

  # Check
  if (ctr != (n_obs + n_init)) {
    rlang::abort("Invalid output")
  }

  # Return
  return(output)
}

### ------------------------------------
# Function to simulate data
sim_data <- function(n_obs,
                     n_init,
                     n_signals,
                     dgp,
                     rho,
                     snr,
                     ran_st) {

  ### Set Seed
  set.seed(ran_st)

  ### Checks
  # Check Number of Signals
  if (n_signals < (1 + 5 + 5 + 50)) {
    rlang::abort("Number of Signals: Too few")
  }
  # Check Sub-Periods
  bps <- unlist(lapply(dgp, function(sub_process) c(sub_process$start, sub_process$end)))
  if (length(unique(bps)) != length(bps)) {
    rlang::abort("Invalid Break Points: Duplicate points found")
  } else if (any(bps < 1)) {
    rlang::abort("Invalid Break Points: Points less than 1 found")
  } else if (any(bps > (n_obs + n_init))) {
    rlang::abort("Invalid Break Points: Points greater than the allowed range found")
  } else if (any(diff(bps) < 1)) {
    rlang::abort("Invalid Break Points: Points are not in ascending order")
  }

  ### Get DGP-Functions and active signals
  # Active DGP-Functions
  active_func <- unique(unlist(sapply(dgp, function(item) item$func)))

  # Active signals
  idx_active <- NULL
  if ("F1" %in% active_func) {
    idx_active <- 1
  }
  if ("F2" %in% active_func) {
    idx_active <- c(idx_active, seq_len(5) + 1)
  }
  if ("F3" %in% active_func) {
    idx_active <- c(idx_active, seq_len(5) + 1 + 5)
  }
  if ("F4" %in% active_func) {
    idx_active <- c(idx_active, seq_len(50) + 1 + 5 + 5)
  }
  if (any(is.na(idx_active)) || any(is.na(active_func))) {
    rlang::abort("No active variables")
  }

  # Select 10 (random) noise-signals
  idx_non_active <- setdiff(seq_len(n_signals), idx_active)
  vec_non_active <- sample(idx_non_active, 10)

  ### Generate Signals
  # F1: Standard-Normal Distribution
  x1 <- replicate(1, rnorm(n_obs + n_init))

  # F2: Uniform Distribution
  X2 <- replicate(5, runif(n_obs + n_init))

  # F3: Uniform Distribution
  X3 <- replicate(5, runif(n_obs + n_init))

  # F4: Multivariate-Normal Distribution
  R <- matrix(rho, ncol = 50, nrow = 50)
  diag(R) <- 1.0
  X4 <- MASS::mvrnorm(n = (n_obs + n_init),
                      mu = rep(0.0, nrow(R)),
                      Sigma = R)

  # Remaining (Noise) Signals: Standard-Normal Distribution
  n_remain <- n_signals - 1 - 5 - 5 - 50
  X_noise <- MASS::mvrnorm(n = (n_obs + n_init),
                           mu = rep(0.0, n_remain),
                           Sigma = diag(n_remain))

  ### Compute f(x)
  f_x <- wrapper_f(x1,
                   X2,
                   X3,
                   X4,
                   dgp,
                   n_obs,
                   n_init)

  ### Signal Matrix
  # Combine Signals
  X <- cbind(x1, X2, X3, X4, X_noise)
  if (any(dim(X) != c((n_obs + n_init), n_signals))) {
    rlang::abort("Invalid Dimensions")
  }

  ### Compute Reponse
  # Error-Term: Standard-Normal Distribution
  eps <- rnorm((n_obs + n_init), 0.0, 1.0)

  # Error-Term-Adjustment according to SNR
  f_x_eval <- f_x[(n_init + 1):(n_obs + n_init)]
  eps_eval <- eps[(n_init + 1):(n_obs + n_init)]
  if (var(f_x_eval) == 0) {
    adj <- 1
  } else {
    adj <- sqrt(var(f_x_eval) / (snr * var(eps_eval)))
  }

  # Generate Response
  y <- as.matrix(f_x + rep(adj, (n_obs + n_init)) * eps, ncol = 1)

  ### Add Names
  colnames(X) <- paste0("X", seq_len(n_signals))
  colnames(y) <- "response"
  rownames(X) <- seq_len((n_obs + n_init))
  rownames(y) <- seq_len((n_obs + n_init))

  ### Return
  return(list(covariates = X,
              response = y,
              active = idx_active,
              non_active = vec_non_active))
}

### ------------------------------------
### Computation Time
### ------------------------------------
# Function to Simulate Data
sim_data_ct <- function(numb_obs, numb_preds) {

  # Signal-to-Noise Ratio
  snr <- 0.5

  # Number of relevant Signals
  n_relevant <- 10

  # Generate Covariates
  X <- matrix(rnorm(numb_obs * numb_preds), nrow = numb_obs, ncol = numb_preds)

  # Generate Beta-Coefficients
  beta <- runif(n_relevant, -1.0, 1.0)

  # Compute f(x)
  f_x <- X[, seq(n_relevant)] %*% beta

  # Generate Error-Term
  eps <- rnorm(numb_obs)

  # Calculate SNR-Adjustment
  adj <- sqrt(var(f_x) / (snr * var(eps)))

  # Calculate Response
  y <- as.matrix(f_x + rep(adj, numb_obs) * eps, ncol = 1)

  # Add Names
  colnames(X) <- paste0("X", seq_len(numb_preds))
  colnames(y) <- "response"

  # Return
  return(list(y = y, X = X))
}

### ------------------------------------
# Model: Relaxed-Lasso
benchmark_rela <- function(y, X, win) {

  # Parameter
  rela_folds <- 5

  # Time Sequence
  t_start <- win
  t_end <- nrow(y) - 1
  t_seq <- seq(t_start, t_end)

  # Create Forecasts
  for (t in t_seq) {

    # Create Train-Data
    x_train <- X[(t - win + 1):t, , drop = FALSE]
    y_train <- y[(t - win + 1):t, , drop = FALSE]

    # Create Test-Data
    x_test <- X[t + 1, , drop = FALSE]

    # Fit and Predict
    pred_relax <- relasso_model(x_train,
                                y_train,
                                x_test,
                                rela_folds,
                                123)
  }
}

### ------------------------------------
# Model: DART
benchmark_dart <- function(y, X, win) {

  # Parameter
  dart_folds  <- 5
  dart_ntrees <- 500
  dart_lr     <- 0.1
  dart_dr     <- 0.1
  dart_target <- NULL
  dart_cores  <- 1

  # Time Sequence
  t_start <- win
  t_end <- nrow(y) - 1
  t_seq <- seq(t_start, t_end)

  # Create x Forecasts
  for (t in t_seq) {

    # Create Train-Data
    x_train <- X[(t - win + 1):t, , drop = FALSE]
    y_train <- y[(t - win + 1):t, , drop = FALSE]

    # Create Test-Data
    x_test <- X[t + 1, , drop = FALSE]

    # Fit and Predict
    pred_dart <- dart_model(x_train,
                            y_train,
                            x_test,
                            dart_folds,
                            dart_ntrees,
                            dart_lr,
                            dart_dr,
                            dart_target,
                            dart_cores,
                            123)
  }
}

### ------------------------------------
# Model: XGBoost
benchmark_xgb <- function(y, X, win) {

  # Parameter
  xgb_folds  <- 5
  xgb_ntrees <- 500
  xgb_lr     <- 0.1
  xgb_target <- NULL
  xgb_cores  <- 1

  # Time Sequence
  t_start <- win
  t_end <- nrow(y) - 1
  t_seq <- seq(t_start, t_end)

  # Create x Forecasts
  for (t in t_seq) {

    # Create Train-Data
    x_train <- X[(t - win + 1):t, , drop = FALSE]
    y_train <- y[(t - win + 1):t, , drop = FALSE]

    # Create Test-Data
    x_test <- X[t + 1, , drop = FALSE]

    # Predict
    pred_xgb <- xgb_model(x_train,
                          y_train,
                          x_test,
                          xgb_folds,
                          xgb_ntrees,
                          xgb_lr,
                          xgb_target,
                          xgb_cores,
                          123)
  }
}

### ------------------------------------
# Model: Targeted Random Forests
benchmark_trf <- function(y, X, win) {

  # Parameter
  trf_n_target  <- 50
  trf_ntrees    <- 500
  trf_max_depth <- 3
  trf_cores     <- 1

  # Time Sequence
  t_start <- win
  t_end <- nrow(y) - 1
  t_seq <- seq(t_start, t_end)

  # Create x Forecasts
  for (t in t_seq) {

    # Create Train-Data
    x_train <- X[(t - win + 1):t, , drop = FALSE]
    y_train <- y[(t - win + 1):t, , drop = FALSE]

    # Create Test-Data
    x_test <- X[t + 1, , drop = FALSE]

    # Fit and Predict
    pred_trf <- trf_model(x_train,
                          y_train,
                          x_test,
                          trf_n_target,
                          trf_ntrees,
                          trf_max_depth,
                          trf_cores,
                          123)
  }
}

### ------------------------------------
# Model: (Targeted) Complete Subset
benchmark_tcsr <- function(y, X, win) {

  # Parameter
  csr_n_target <- 20
  csr_n_subset <- 10
  csr_ubound   <- 10000
  csr_sampling <- TRUE

  # Time
  t_start <- win
  t_end <- nrow(y) - 1
  t_seq <- seq(t_start, t_end)

  # Create x Forecasts
  for (t in t_seq) {

    # Create Train-Data
    x_train <- X[(t - win + 1):t, , drop = FALSE]
    y_train <- y[(t - win + 1):t, , drop = FALSE]

    # Create Test-Data
    x_test <- X[t + 1, , drop = FALSE]

    # Fit and Predict
    pred_csr <- csr_model(x_train,
                          y_train,
                          x_test,
                          csr_n_target,
                          csr_n_subset,
                          csr_ubound,
                          csr_sampling)
  }
}

### ------------------------------------
# Model: STSC
benchmark_stsc <- function(y, X) {

  # Set TV-C-Parameter
  init <- 5 * 12
  lambda_grid <- c(0.9667, 0.9833, 1.0000)
  kappa_grid <- c(0.93, 0.95, 0.97)
  bias <- TRUE

  # Set DSC-Parameter
  gamma_grid <- c(0.40, 0.50, 0.60, 0.70, 0.80, 0.90,
                  0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1.00)
  n_tvc <- ncol(X) * length(lambda_grid)
  psi_grid <- c(1:100, sapply(1:4, function(i) floor(i * n_tvc / 4)))
  delta <- 0.9833
  burn_in <- init / 2
  burn_in_dsc <- 1
  metric <- 5
  equal_weight <- TRUE
  incl <- NULL
  parallel <- FALSE
  n_threads <- 1

  # Apply STSC-Function
  stsc_results <- hdflex::stsc(y,
                               X,
                               NULL,
                               init,
                               lambda_grid,
                               kappa_grid,
                               bias,
                               gamma_grid,
                               psi_grid,
                               delta,
                               burn_in,
                               burn_in_dsc,
                               metric,
                               equal_weight,
                               incl,
                               parallel,
                               n_threads,
                               NULL)
}

### ------------------------------------
# STSC-Sx
benchmark_stscsx <- function(y, X, win) {

  # Set TV-C-Parameter
  init <- 5 * 12
  lambda_grid <- c(0.9667, 0.9833, 1.0000)
  kappa_grid <- c(0.93, 0.95, 0.97)
  bias <- TRUE
  n_cores <- 1

  # Set Lasso-Target-Parameter
  target_number <- 10

  # Apply STSC-S-X Function
  stscs10_results <- stscsx(y,
                            X,
                            NULL,
                            lambda_grid,
                            kappa_grid,
                            init,
                            bias,
                            n_cores,
                            win,
                            target_number)
}
### ------------------------------------
# STSC-S-Flex
benchmark_stscsflex <- function(y, X, win) {

  # Set TV-C-Parameter
  init <- 5 * 12
  lambda_grid <- c(0.9667, 0.9833, 1.0000)
  kappa_grid <- c(0.93, 0.95, 0.97)
  bias <- TRUE
  n_cores <- 1

  # Set Lasso-CV-Parameter
  folds <- 5

  # Apply ST-S-FLEX-Function
  stscsflex_results <- stscsflex(y,
                                 X,
                                 NULL,
                                 lambda_grid,
                                 kappa_grid,
                                 init,
                                 bias,
                                 n_cores,
                                 win,
                                 folds)
}

### ------------------------------------
# (Principal Component) Dynamic Model Averaging
benchmark_pcdma <- function(y, X, win, n_comp) {

  # Set Parameter
  pcdma_comp   <- n_comp
  pcdma_alpha  <- 0.99
  pcdma_lambda <- c(0.9667, 0.9833, 1.0000)
  pcdma_kappa  <- 0.97

  # Response-Name
  colnames(y) <- "response"

  # Time Sequence
  t_start <- win
  t_end <- nrow(y)
  t_seq <- seq(t_start, t_end)

  # Result-Object: Prinipal Components
  res_pca <- matrix(NA, ncol = pcdma_comp, nrow = nrow(y),
                    dimnames = list(rownames(y),
                                    paste0("PC", seq_len(pcdma_comp))))

  # Initial PCA-Projection
  row_idx <- seq(win - 1)
  pca_model <- stats::prcomp(X[row_idx, , drop = FALSE],
                             center = TRUE,
                             scale. = TRUE)
  res_pca[row_idx, ] <- pca_model$x[, seq_len(pcdma_comp), drop = FALSE]

  # Project Signals for every t
  res_pca[t_seq, ] <- foreach::foreach(t = t_seq,
                                       .combine = "rbind",
                                       .packages = c("stats")) %do% {

    # Get Training-Data
    x_train <- X[(t - win + 1):t, , drop = FALSE]

    # Fit PCA
    pca_model <- stats::prcomp(x_train, center = TRUE, scale. = TRUE)

    # Return
    return(pca_model$x[win, seq_len(pcdma_comp), drop = FALSE])
  }

  # Fit Model
  model <- eDMA::DMA(formula = response ~ .,
                     data = cbind(y, res_pca),
                     dAlpha = pcdma_alpha,
                     vDelta = pcdma_lambda,
                     vKeep = NULL,
                     bZellnerPrior = FALSE,
                     dG = 100,
                     bParallelize = FALSE,
                     iCores = 1,
                     dBeta = pcdma_kappa)
}
### ------------------------------------