# ------------------------------------------------------------
# 04_prepare_commerce.R
# Load commerce ZIP files and build dong-quarter panels
# ------------------------------------------------------------

source("R/01_utils.R")

extract_raw_archives()

read_commerce_zip <- function(zip_path) {
  tmp <- tempfile("commerce_zip_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  is_commerce_zip <- is_commerce_zip_path(zip_path)
  csvs <- character()
  if (!is_commerce_zip) {
    try(utils::unzip(zip_path, exdir = tmp), silent = TRUE)
    csvs <- list.files(tmp, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
    csvs <- csvs[!grepl("(^|/)__MACOSX(/|$)|(^|/)\\._", csvs)]
  }

  for (csv in csvs) {
    probe <- try(read_csv_korean(csv, encoding = "CP949", n_max = 1), silent = TRUE)
    if (inherits(probe, "try-error")) next
    if (all(c("기준_년분기_코드", "행정동_코드", "당월_매출_금액") %in% names(probe))) {
      message("Reading commerce file from zip: ", basename(zip_path))
      return(read_csv_korean(csv, encoding = "CP949") %>% mutate(source_zip = basename(zip_path)))
    }
  }

  # Some Seoul commerce ZIPs store member names in CP949. On UTF-8 macOS
  # filesystems, R's unzip can fail before creating the CSV path. Stream the
  # single CSV payload to an ASCII temp file and inspect the columns instead.
  if (length(csvs) == 0 && nzchar(Sys.which("unzip"))) {
    csv_dump <- tempfile("commerce_payload_", fileext = ".csv")
    on.exit(unlink(csv_dump), add = TRUE)
    status <- try(
      system2("unzip", args = c("-p", shQuote(zip_path)), stdout = csv_dump, stderr = FALSE),
      silent = TRUE
    )
    if (!inherits(status, "try-error") && identical(status, 0L) && file.exists(csv_dump) && file.size(csv_dump) > 0) {
      probe <- try(read_csv_korean(csv_dump, encoding = "CP949", n_max = 1), silent = TRUE)
      if (!inherits(probe, "try-error") &&
          all(c("기준_년분기_코드", "행정동_코드", "당월_매출_금액") %in% names(probe))) {
        message("Reading commerce file from zip stream: ", basename(zip_path))
        return(read_csv_korean(csv_dump, encoding = "CP949") %>% mutate(source_zip = basename(zip_path)))
      }
    }
  }

  NULL
}

message("Preparing commerce panels...")
zip_candidates <- all_input_files("\\.zip$")
zip_candidates <- zip_candidates[vapply(zip_candidates, is_commerce_zip_path, logical(1))]
commerce_list <- purrr::map(zip_candidates, read_commerce_zip)
commerce_raw <- dplyr::bind_rows(commerce_list)
if (nrow(commerce_raw) == 0) stop("No commerce 추정매출-행정동 files found.")

rename_map <- c(
  quarter_code = "기준_년분기_코드",
  dong_code = "행정동_코드",
  dong_name = "행정동_코드_명",
  service_code = "서비스_업종_코드",
  service_name = "서비스_업종_코드_명",
  sales = "당월_매출_금액",
  transactions = "당월_매출_건수",
  weekday_sales = "주중_매출_금액",
  weekend_sales = "주말_매출_금액",
  sales_00_06 = "시간대_00~06_매출_금액",
  sales_06_11 = "시간대_06~11_매출_금액",
  sales_11_14 = "시간대_11~14_매출_금액",
  sales_14_17 = "시간대_14~17_매출_금액",
  sales_17_21 = "시간대_17~21_매출_금액",
  sales_21_24 = "시간대_21~24_매출_금액",
  male_sales = "남성_매출_금액",
  female_sales = "여성_매출_금액",
  age10_sales = "연령대_10_매출_금액",
  age20_sales = "연령대_20_매출_금액",
  age30_sales = "연령대_30_매출_금액",
  age40_sales = "연령대_40_매출_금액",
  age50_sales = "연령대_50_매출_금액",
  age60plus_sales = "연령대_60_이상_매출_금액",
  cnt_00_06 = "시간대_건수~06_매출_건수",
  cnt_06_11 = "시간대_건수~11_매출_건수",
  cnt_11_14 = "시간대_건수~14_매출_건수",
  cnt_14_17 = "시간대_건수~17_매출_건수",
  cnt_17_21 = "시간대_건수~21_매출_건수",
  cnt_21_24 = "시간대_건수~24_매출_건수",
  male_transactions = "남성_매출_건수",
  female_transactions = "여성_매출_건수",
  age10_transactions = "연령대_10_매출_건수",
  age20_transactions = "연령대_20_매출_건수",
  age30_transactions = "연령대_30_매출_건수",
  age40_transactions = "연령대_40_매출_건수",
  age50_transactions = "연령대_50_매출_건수",
  age60plus_transactions = "연령대_60_이상_매출_건수"
)

commerce_clean <- commerce_raw %>%
  rename(any_of(rename_map)) %>%
  select(any_of(c(
    "quarter_code", "dong_code", "dong_name", "service_code", "service_name",
    "sales", "transactions", "weekday_sales", "weekend_sales",
    "sales_00_06", "sales_06_11", "sales_11_14", "sales_14_17", "sales_17_21", "sales_21_24",
    "male_sales", "female_sales", "age10_sales", "age20_sales", "age30_sales", "age40_sales", "age50_sales", "age60plus_sales",
    "cnt_00_06", "cnt_06_11", "cnt_11_14", "cnt_14_17", "cnt_17_21", "cnt_21_24",
    "male_transactions", "female_transactions", "age10_transactions", "age20_transactions", "age30_transactions", "age40_transactions", "age50_transactions", "age60plus_transactions",
    "source_zip"
  ))) %>%
  mutate(across(c(quarter_code, dong_code, sales, transactions, weekday_sales, weekend_sales,
                  sales_00_06, sales_06_11, sales_11_14, sales_14_17, sales_17_21, sales_21_24,
                  male_sales, female_sales, age10_sales, age20_sales, age30_sales, age40_sales, age50_sales, age60plus_sales,
                  cnt_00_06, cnt_06_11, cnt_11_14, cnt_14_17, cnt_17_21, cnt_21_24,
                  male_transactions, female_transactions, age10_transactions, age20_transactions, age30_transactions, age40_transactions, age50_transactions, age60plus_transactions), parse_num)) %>%
  mutate(
    quarter_code = as.integer(quarter_code),
    dong_code = as.integer(dong_code),
    year = quarter_code %/% 10,
    quarter = quarter_code %% 10,
    qstr = paste0(year, "Q", quarter)
  )

q_codes <- sort(unique(commerce_clean$quarter_code))
q_index_map <- setNames(seq_along(q_codes) - 1L, q_codes)
commerce_clean <- commerce_clean %>%
  mutate(
    q_index = as.integer(q_index_map[as.character(quarter_code)]),
    rel_q = q_index - as.integer(q_index_map[as.character(TRANSITION_Q)]),
    post = as.integer(quarter_code >= POST_Q),
    transition = as.integer(quarter_code == TRANSITION_Q)
  )

save_csv(commerce_clean %>% distinct(dong_code, dong_name) %>% arrange(dong_code), file.path(OUT_PROCESSED, "commerce_dong_codebook.csv"))
save_csv(commerce_clean %>% distinct(service_code, service_name) %>% arrange(service_code), file.path(OUT_PROCESSED, "commerce_service_codebook.csv"))
save_csv(commerce_clean, file.path(OUT_PROCESSED, "commerce_service_dong_quarter_panel_2019_2024.csv"))

agg_cols <- c("sales", "transactions", "weekday_sales", "weekend_sales",
              "sales_00_06", "sales_06_11", "sales_11_14", "sales_14_17", "sales_17_21", "sales_21_24",
              "male_sales", "female_sales", "age10_sales", "age20_sales", "age30_sales", "age40_sales", "age50_sales", "age60plus_sales",
              "cnt_00_06", "cnt_06_11", "cnt_11_14", "cnt_17_21", "cnt_14_17", "cnt_21_24",
              "male_transactions", "female_transactions", "age10_transactions", "age20_transactions", "age30_transactions", "age40_transactions", "age50_transactions", "age60plus_transactions")

add_commerce_outcomes <- function(df) {
  df %>%
    mutate(
      log_sales = log(sales + 1),
      log_transactions = log(transactions + 1),
      avg_ticket = sales / if_else(transactions == 0, NA_real_, transactions),
      log_avg_ticket = log(coalesce(avg_ticket, 0) + 1),
      weekend_share = weekend_sales / if_else(weekday_sales + weekend_sales == 0, NA_real_, weekday_sales + weekend_sales),
      after_work_sales = sales_17_21 + sales_21_24,
      after_work_share = after_work_sales / if_else(sales == 0, NA_real_, sales),
      night_sales = sales_21_24 + sales_00_06,
      night_share = night_sales / if_else(sales == 0, NA_real_, sales),
      night_transactions = cnt_21_24 + cnt_00_06,
      night_tx_share = night_transactions / if_else(transactions == 0, NA_real_, transactions),
      age20_30_sales = age20_sales + age30_sales,
      age40_50_sales = age40_sales + age50_sales,
      age50_60_sales = age50_sales + age60plus_sales,
      age20_30_share = age20_30_sales / if_else(sales == 0, NA_real_, sales),
      age40_50_share = age40_50_sales / if_else(sales == 0, NA_real_, sales),
      age50_60_share = age50_60_sales / if_else(sales == 0, NA_real_, sales),
      male_sales_share = male_sales / if_else(male_sales + female_sales == 0, NA_real_, male_sales + female_sales),
      female_sales_share = female_sales / if_else(male_sales + female_sales == 0, NA_real_, male_sales + female_sales),
      district_code = dong_code %/% 1000
    )
}

commerce_panel <- commerce_clean %>%
  group_by(dong_code, dong_name, quarter_code, year, quarter, qstr, q_index, rel_q, post, transition) %>%
  summarise(across(all_of(agg_cols), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  add_commerce_outcomes()

save_csv(commerce_panel, file.path(OUT_PROCESSED, "commerce_dong_quarter_panel_2019_2024.csv"))

message("Commerce preparation complete.")
