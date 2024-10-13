### ------------------------------------
### Several helper Functions
### ------------------------------------
# Function to Convert Jupyter Notebooks to R-Files
convert_ipynb_to_r <- function(input, output = xfun::with_ext(input, "R"), keep_rmd = FALSE, ...) {

  # Check if necessary packages are installed
  if (!require("rmarkdown")) return("Missing necessary package: 'rmarkdown'")
  if (!require("knitr")) return("Missing necessary package: 'knitr'")

  # Check if file extension is matches Jupyter notebook.
  if (tolower(xfun::file_ext(input)) != "ipynb") {
    return("Error: Invalid file format")
  }

  #  Conversion process: .ipynb to .Rmd
  rmarkdown::convert_ipynb(input)

  #  Conversion process: .Rmd to .R
  knitr::purl(xfun::with_ext(input, "Rmd"), output = output)

  # Keep or remove intermediary .Rmd
  if (keep_rmd == FALSE) {
    file.remove(xfun::with_ext(input, "Rmd"))
  }
}

### ------------------------------------
# Function to generate 1:n lags for a specific variable
get_lags <- function(df, var, lags) {
  map_lag <- lags %>% purrr::map(~purrr::partial(lag, n = .x))
  return(df %>% dplyr::mutate(dplyr::across(.cols = {{var}}, .fns = map_lag, .names = "{.col}_lag{lags}")))
}

### ------------------------------------
# Output Combination Function for foreach (Application: Finance)
comb <- function(...) {
  mapply("rbind", ..., SIMPLIFY = FALSE)
}

### ------------------------------------
# Transformation function based on Matlab code from McCracken (Application: Inflation)
transform_tcode  <- function(data) {

  tcode <- data[1]
  data  <- data[2:length(data)]

  # 1. No transformation
  if (tcode == 1) {
    output <- data
    # 2. First differences
  } else if (tcode == 2) {
    output <- data - dplyr::lag(data, 1)
    # 3. Second differences
  } else if (tcode == 3) {
    output <- (data - dplyr::lag(data, 1)) - (dplyr::lag(data, 1) - dplyr::lag(data, 2))
    # 4. Natural Log
  } else if (tcode == 4) {
    output <- log(data)
    # 5. First differences of natural log
  } else if (tcode == 5) {
    output <- log(data) - dplyr::lag(log(data), 1)
    # 6. Second differences of natural log
  } else if (tcode == 6) {
    output <- (log(data) - dplyr::lag(log(data), 1)) - (dplyr::lag(log(data), 1) - dplyr::lag(log(data), 2))
    # 7. First differences of percent change
  } else {
    output <- (data / dplyr::lag(data, 1) - 1)  -  (dplyr::lag(data, 1) / dplyr::lag(data, 2) - 1)
  }
  return(c(tcode, output))
}

### ------------------------------------
# Transformation Response Variable (Application: Inflation)
transform_ytcode <- function(data) {
  # Differences of natural log
  output <- log(data) - dplyr::lag(log(data), 1)
  return(output)
}

### ------------------------------------
# Adjust for outliers using fraction of IQR (Application: Inflation)
adjout_oos <- function(obs, init, thrs, iwin) {

  # Check NA-values
  check_na(obs)

  # Remove NA
  na_ctr <- sum(is.na(obs))
  y <- na.omit(obs)

  # Define Variables
  init_seq <- seq_len(init)
  roll_seq <- seq(init + 1, length(y))

  # Initialize vectors
  mod_vec <- rep(NA, length(y))
  iqr_vec <- rep(NA, length(y))
  med_vec <- rep(NA, length(y))

  # Training Sample
  iqr_vec[init_seq] <- IQR(y[init_seq])
  med_vec[init_seq] <- median(y[init_seq])

  # Calculate Expanding Median and IQR for outlier detection
  iqr_vec[roll_seq] <- sapply(roll_seq, function(i) IQR(y[1:i]))
  med_vec[roll_seq] <- sapply(roll_seq, function(i) median(y[1:i]))

  # Calculate Rolling Median for Imputation
  for (i in seq_len(length(y))) {
    j1 <- max(1, (i - iwin + 1))
    j2 <- i
    mod_vec[i] <- median(y[j1:j2])
  }

  # Absolute Deviation from Median
  y_dev <- abs(y - med_vec)

  # Above / Below Threshold
  iabove <- y_dev >  (thrs * iqr_vec)
  ibelow <- y_dev <= (thrs * iqr_vec)

  # Create Output
  output <- (ibelow * y) + (iabove * mod_vec)

  # Return
  return(c(rep(NA, na_ctr), output))
}

### ------------------------------------
# Check whether a vector contains non-consecutive na-values from the start
check_na <- function(vec) {

  # Find position of NA values
  na_positions <- which(is.na(vec))

  # Proceed if NA values in vector
  if (length(na_positions) > 0) {

    # Check if NA values are not consecutive from the start
    if (any(diff(na_positions) != 1) || min(na_positions) != 1) {
      warning("Vector contains non-consecutive NA-values.")
    }
  }
}
