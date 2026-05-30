# ------------------------------------------------------------
# 02_prepare_subway.R
# Load subway daily and time-band data and build analysis panels
# ------------------------------------------------------------

source("R/01_utils.R")

extract_raw_archives()

read_subway_daily_file <- function(path) {
  # Some CARD_SUBWAY files add a trailing empty field after the registered date.
  # The analysis only uses the stable first five columns.
  df <- suppressWarnings(read_csv_korean(path))
  needed <- c("사용일자", "노선명", "역명", "승차총승객수", "하차총승객수", "등록일자")
  missing <- setdiff(needed[1:5], names(df))
  if (length(missing) > 0) stop("Subway file missing columns: ", path)
  df %>%
    dplyr::select(any_of(needed)) %>%
    mutate(source_file = basename(path))
}

message("Preparing subway daily panel...")
subway_files <- all_input_files("CARD_SUBWAY_MONTH.*\\.csv$")
if (length(subway_files) == 0) stop("No CARD_SUBWAY_MONTH*.csv files found.")

subway_raw <- purrr::map_dfr(subway_files, read_subway_daily_file) %>%
  mutate(
    date = as.Date(사용일자, format = "%Y%m%d"),
    line_norm = normalize_line_name(노선명),
    station = as.character(역명),
    station_line = paste0(line_norm, "_", station),
    board = parse_num(승차총승객수),
    alight = parse_num(하차총승객수),
    riders = board + alight,
    month = as.Date(format(date, "%Y-%m-01")),
    month_str = format(month, "%Y-%m")
  ) %>%
  filter(!is.na(date), !is.na(board), !is.na(alight))

save_csv(
  tibble(station_line = sort(unique(subway_raw$station_line))),
  file.path(OUT_PROCESSED, "subway_station_line_codebook.csv")
)

missing_units <- setdiff(ALL_STATIONS, unique(subway_raw$station_line))
if (length(missing_units) > 0) {
  warning("Missing station_line units: ", paste(missing_units, collapse = ", "))
}

subway_daily_panel <- subway_raw %>%
  filter(station_line %in% ALL_STATIONS) %>%
  mutate(
    treated = as.integer(station_line %in% TREATED_STATIONS),
    post_day = as.integer(date >= OPEN_DATE),
    transition_month = as.integer(month == TRANSITION_MONTH),
    log_riders = log1p(riders)
  )

save_csv(subway_daily_panel, file.path(OUT_PROCESSED, "subway_daily_panel_2018_2024.csv"))

subway_monthly_panel <- subway_daily_panel %>%
  group_by(station_line, month, month_str, treated) %>%
  summarise(
    avg_daily_riders = mean(riders, na.rm = TRUE),
    avg_daily_board = mean(board, na.rm = TRUE),
    avg_daily_alight = mean(alight, na.rm = TRUE),
    days = n_distinct(date),
    .groups = "drop"
  ) %>%
  mutate(
    post = as.integer(month >= POST_MONTH),
    transition = as.integer(month == TRANSITION_MONTH),
    did = treated * post,
    log_avg_daily_riders = log(avg_daily_riders),
    rel_month = 12 * (year(month) - year(POST_MONTH)) + month(month) - month(POST_MONTH),
    group = if_else(treated == 1, "Treated", "Control")
  )

save_csv(subway_monthly_panel, file.path(OUT_PROCESSED, "subway_monthly_panel_2018_2024.csv"))

# Time-band data. Identify the file by columns rather than by filename.
message("Preparing subway time-band panel if available...")
csv_candidates <- setdiff(all_input_files("\\.csv$"), subway_files)
timeband_path <- NULL
for (p in csv_candidates) {
  head_df <- try(read_csv_korean(p, encoding = "CP949", n_max = 1), silent = TRUE)
  if (!inherits(head_df, "try-error") && all(c("사용월", "호선명", "지하철역") %in% names(head_df))) {
    time_cols <- names(head_df)[stringr::str_detect(names(head_df), "시-.*(승차인원|하차인원)")]
    if (length(time_cols) > 0) {
      timeband_path <- p
      break
    }
  }
}

if (!is.null(timeband_path)) {
  tb <- read_csv_korean(timeband_path, encoding = "CP949") %>%
    mutate(
      사용월 = as.integer(사용월),
      line_norm = normalize_line_name(호선명),
      station_line = paste0(line_norm, "_", 지하철역),
      month = as.Date(paste0(사용월, "01"), format = "%Y%m%d"),
      month_str = format(month, "%Y-%m"),
      treated = as.integer(station_line %in% TREATED_STATIONS),
      post = as.integer(month >= POST_MONTH),
      transition = as.integer(month == TRANSITION_MONTH),
      did = treated * post
    ) %>%
    filter(사용월 >= 201801, 사용월 <= 202412, station_line %in% ALL_STATIONS)

  get_hour_cols <- function(hours) {
    prefixes <- sprintf("%02d시-", hours)
    names(tb)[purrr::map_lgl(names(tb), function(nm) {
      any(startsWith(nm, prefixes)) && stringr::str_detect(nm, "승차인원|하차인원")
    })]
  }

  all_time_cols <- names(tb)[stringr::str_detect(names(tb), "시-.*(승차인원|하차인원)")]
  specs <- list(
    total = all_time_cols,
    morning_peak = get_hour_cols(c(7, 8)),
    evening_peak = get_hour_cols(c(17, 18, 19)),
    late_night = get_hour_cols(c(21, 22, 23, 0, 1, 2, 3)),
    daytime = get_hour_cols(c(11, 12, 13, 14, 15, 16))
  )

  for (nm in names(specs)) {
    cols <- specs[[nm]]
    tb[[nm]] <- rowSums(as.data.frame(lapply(tb[cols], parse_num)), na.rm = TRUE)
    tb[[paste0("log_", nm)]] <- log(tb[[nm]] + 1)
  }

  save_csv(tb, file.path(OUT_PROCESSED, "subway_timeband_monthly_panel_2018_2024.csv"))
} else {
  message("No time-band subway file found. Skipping time-band panel.")
}

message("Subway preparation complete.")
