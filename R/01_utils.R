# ------------------------------------------------------------
# 01_utils.R
# Helper functions
# ------------------------------------------------------------

source("R/00_config.R")

is_commerce_zip_path <- function(path) {
  base <- stringi::stri_trans_nfc(basename(path))
  grepl("상권|추정매출|행정동", base)
}

extract_raw_archives <- function() {
  zip_files <- list.files(DATA_RAW, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
  if (length(zip_files) == 0) {
    message("No zip files found in data/raw. If files are already extracted, this is fine.")
    return(invisible(NULL))
  }
  for (z in zip_files) {
    if (is_commerce_zip_path(z)) next
    message("Unzipping: ", basename(z))
    try(utils::unzip(z, exdir = DATA_EXTRACTED), silent = TRUE)
  }
  invisible(NULL)
}

all_input_files <- function(pattern) {
  files <- unique(c(
    list.files(DATA_RAW, pattern = pattern, full.names = TRUE, recursive = TRUE),
    list.files(DATA_EXTRACTED, pattern = pattern, full.names = TRUE, recursive = TRUE)
  ))
  files[!grepl("(^|/)__MACOSX(/|$)|(^|/)\\._", files)]
}

has_utf8_bom <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  b <- readBin(con, what = "raw", n = 3)
  length(b) == 3 && identical(as.integer(b), c(239L, 187L, 191L))
}

detect_csv_encoding <- function(path) {
  if (has_utf8_bom(path)) "UTF-8" else "CP949"
}

read_csv_korean <- function(path, encoding = NULL, col_types = readr::cols(.default = "c"), n_max = Inf) {
  if (is.null(encoding)) encoding <- detect_csv_encoding(path)
  readr::read_csv(
    file = path,
    locale = readr::locale(encoding = encoding),
    col_types = col_types,
    show_col_types = FALSE,
    progress = FALSE,
    n_max = n_max
  )
}

# subway line name changed historically from 9호선2단계 to 9호선2~3단계.
normalize_line_name <- function(x) {
  dplyr::recode(as.character(x), "9호선2단계" = "9호선2~3단계", .default = as.character(x))
}

parse_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

pct_from_log <- function(beta) {
  (exp(beta) - 1) * 100
}

fixest_term_row <- function(model, term) {
  ct <- fixest::coeftable(model)
  ci <- confint(model)
  if (!term %in% rownames(ct)) {
    return(tibble::tibble(
      term = term, estimate = NA_real_, se = NA_real_, p_value = NA_real_,
      ci_low = NA_real_, ci_high = NA_real_
    ))
  }
  tibble::tibble(
    term = term,
    estimate = unname(ct[term, "Estimate"]),
    se = unname(ct[term, "Std. Error"]),
    p_value = unname(ct[term, "Pr(>|t|)"]),
    ci_low = unname(ci[term, 1]),
    ci_high = unname(ci[term, 2])
  )
}

softmax <- function(theta) {
  e <- exp(theta - max(theta))
  e / sum(e)
}

fit_scm_weights <- function(y_pre, x_pre) {
  k <- ncol(x_pre)
  obj <- function(theta) {
    w <- softmax(theta)
    mean((y_pre - as.numeric(x_pre %*% w))^2, na.rm = TRUE)
  }
  opt <- optim(rep(0, k), obj, method = "BFGS", control = list(maxit = 5000))
  softmax(opt$par)
}

save_csv <- function(x, path) {
  readr::write_excel_csv(x, path)
}
