# ------------------------------------------------------------
# 09_analyze_reallocation_extensions.R
# Extension hypotheses: bus substitution, living population, and dining/nightlife
# ------------------------------------------------------------

source("R/01_utils.R")

message("Analyzing reallocation extensions...")

EXT_DIR <- file.path(OUT_DIR, "reallocation_extensions")
EXT_TABLES <- file.path(EXT_DIR, "tables")
EXT_FIGURES <- file.path(EXT_DIR, "figures")
for (d in c(EXT_DIR, EXT_TABLES, EXT_FIGURES)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

SECONDARY_TREATED <- c(
  `11680521` = "논현1동",
  `11650531` = "서초4동"
)

ANCHOR_TREATED <- c(
  `11680510` = "신사동",
  `11680640` = "역삼1동"
)

SECONDARY_CODES <- as.integer(names(SECONDARY_TREATED))
ANCHOR_CODES <- as.integer(names(ANCHOR_TREATED))
CONTROL_CODES <- as.integer(names(CONTROL_MAIN))
TARGET_DONG_CODES <- c(ANCHOR_CODES, SECONDARY_CODES, CONTROL_CODES)

safe_divide <- function(num, den) {
  num / if_else(den == 0, NA_real_, den)
}

effect_row <- function(model, term, outcome, extra = list()) {
  r <- fixest_term_row(model, term)
  tibble(
    outcome = outcome,
    term = term,
    coef = r$estimate,
    se = r$se,
    p_value = r$p_value,
    ci_low = r$ci_low,
    ci_high = r$ci_high,
    pct_effect_if_log = if_else(startsWith(outcome, "log"), pct_from_log(r$estimate), NA_real_),
    ci_low_pct_if_log = if_else(startsWith(outcome, "log"), pct_from_log(r$ci_low), NA_real_),
    ci_high_pct_if_log = if_else(startsWith(outcome, "log"), pct_from_log(r$ci_high), NA_real_),
    effect_pp_if_share = if_else(str_detect(outcome, "share"), 100 * r$estimate, NA_real_),
    ci_low_pp_if_share = if_else(str_detect(outcome, "share"), 100 * r$ci_low, NA_real_),
    ci_high_pp_if_share = if_else(str_detect(outcome, "share"), 100 * r$ci_high, NA_real_)
  ) %>% bind_cols(as_tibble(extra))
}

fit_one_term <- function(dat, outcomes, term = "did", fe, cluster, extra = list()) {
  purrr::map_dfr(outcomes, function(y) {
    mod <- fixest::feols(
      as.formula(paste0(y, " ~ ", term, " | ", fe)),
      cluster = as.formula(paste0("~ ", cluster)),
      data = dat
    )
    effect_row(mod, term, y, c(extra, list(n_obs = nobs(mod), n_units = n_distinct(dat[[cluster]]))))
  })
}

fmt_pct <- function(x) if_else(is.na(x), "NA", sprintf("%.2f%%", x))
fmt_pp <- function(x) if_else(is.na(x), "NA", sprintf("%.2f p.p.", x))
fmt_p <- function(x) if_else(is.na(x), "NA", if_else(x < 0.001, "<0.001", sprintf("%.3f", x)))

# ------------------------------------------------------------
# 1) Bus substitution around the Shinbundang extension corridor.
# ------------------------------------------------------------

BUS_PANEL_PATH <- file.path(OUT_PROCESSED, "bus_corridor_monthly_panel_2019_2024.csv")

read_bus_file <- function(path) {
  message("  bus: ", basename(path))
  cmd <- sprintf("iconv -f CP949 -t UTF-8 %s", shQuote(path))
  dt <- data.table::fread(cmd = cmd, showProgress = FALSE)

  names(dt) <- stringi::stri_trans_nfc(names(dt))
  names(dt) <- str_replace_all(names(dt), '"', "")
  names(dt) <- str_replace_all(names(dt), "\ufeff", "")
  names(dt) <- str_replace_all(names(dt), "^\\?+", "")
  names(dt) <- if_else(str_detect(names(dt), "사용년월"), "사용년월", names(dt))
  board_cols <- names(dt)[str_detect(names(dt), "시승차총승객수$")]
  alight_cols <- names(dt)[str_detect(names(dt), "시하차총승객수$")]
  hour_board_cols <- setNames(board_cols, as.integer(str_extract(board_cols, "^\\d+")))
  hour_alight_cols <- setNames(alight_cols, as.integer(str_extract(alight_cols, "^\\d+")))

  treated_pattern <- paste(c("강남역", "신논현역", "논현역", "신사역"), collapse = "|")
  control_pattern <- paste(c(
    "역삼역", "선릉역", "삼성역", "교대역", "서초역", "압구정역",
    "잠원역", "학동역", "강남구청역", "청담역", "반포역", "고속터미널"
  ), collapse = "|")

  dt <- dt %>%
    mutate(
      month = as.Date(paste0(as.character(.data$사용년월), "01"), format = "%Y%m%d"),
      route_no = as.character(.data$노선번호),
      stop_id = as.character(.data$표준버스정류장ID),
      stop_ars = as.character(.data$버스정류장ARS번호),
      stop_name = as.character(.data$역명),
      group = case_when(
        str_detect(stop_name, treated_pattern) ~ "treated_corridor",
        str_detect(stop_name, control_pattern) ~ "control_corridor",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(group))

  if (nrow(dt) == 0) return(tibble())

  to_num <- function(x) parse_num(x)
  num_cols <- function(cols) as.data.frame(lapply(as.data.frame(dt[, cols, with = FALSE]), to_num))
  dt$total_board <- rowSums(num_cols(board_cols), na.rm = TRUE)
  dt$total_alight <- rowSums(num_cols(alight_cols), na.rm = TRUE)
  dt$morning_alight <- rowSums(num_cols(hour_alight_cols[as.character(7:9)]), na.rm = TRUE)
  dt$evening_board <- rowSums(num_cols(hour_board_cols[as.character(17:19)]), na.rm = TRUE)
  late_hours <- as.character(c(21:23, 0:5))
  dt$late_night_total <- rowSums(num_cols(hour_board_cols[late_hours]), na.rm = TRUE) +
    rowSums(num_cols(hour_alight_cols[late_hours]), na.rm = TRUE)

  dt %>%
    transmute(
      month,
      route_no,
      stop_id,
      stop_ars,
      stop_name,
      group,
      unit = paste(route_no, stop_id, sep = "_"),
      total_board,
      total_alight,
      total_riders = total_board + total_alight,
      morning_alight,
      evening_board,
      late_night_total
    ) %>%
    group_by(month, route_no, stop_id, stop_ars, stop_name, group, unit) %>%
    summarise(
      across(c(total_board, total_alight, total_riders, morning_alight, evening_board, late_night_total), sum, na.rm = TRUE),
      .groups = "drop"
    )
}

build_bus_panel <- function() {
  bus_dir <- file.path(DATA_RAW, "bus")
  files <- c(
    file.path(bus_dir, "bus_2019.csv"),
    file.path(bus_dir, "bus_2020.csv"),
    file.path(bus_dir, "bus_2021.csv"),
    list.files(file.path(bus_dir, "bus_2022_unzipped"), pattern = "\\.csv$", full.names = TRUE),
    list.files(bus_dir, pattern = "^bus_2023\\d{2}\\.csv$", full.names = TRUE),
    list.files(bus_dir, pattern = "^bus_2024\\d{2}\\.csv$", full.names = TRUE)
  )
  files <- files[file.exists(files) & file.size(files) > 1000]
  if (length(files) == 0) stop("No bus source files found under data/raw/bus.")

  panel <- purrr::map_dfr(files, read_bus_file) %>%
    mutate(
      month_str = format(month, "%Y-%m"),
      post = as.integer(month >= POST_MONTH),
      transition = as.integer(month == TRANSITION_MONTH),
      treated = as.integer(group == "treated_corridor"),
      did = treated * post,
      log_total_riders = log(total_riders + 1),
      log_total_board = log(total_board + 1),
      log_total_alight = log(total_alight + 1),
      log_morning_alight = log(morning_alight + 1),
      log_evening_board = log(evening_board + 1),
      log_late_night_total = log(late_night_total + 1)
    ) %>%
    filter(transition == 0) %>%
    group_by(unit) %>%
    filter(any(post == 0), any(post == 1), n_distinct(month) >= 12) %>%
    ungroup()

  save_csv(panel, BUS_PANEL_PATH)
  panel
}

bus_panel <- if (file.exists(BUS_PANEL_PATH)) {
  readr::read_csv(BUS_PANEL_PATH, show_col_types = FALSE) %>% mutate(month = as.Date(month))
} else {
  build_bus_panel()
}

bus_outcomes <- c(
  "log_total_riders", "log_total_board", "log_total_alight",
  "log_morning_alight", "log_evening_board", "log_late_night_total"
)

bus_did_results <- fit_one_term(
  bus_panel,
  bus_outcomes,
  term = "did",
  fe = "unit + month_str",
  cluster = "unit",
  extra = list(hypothesis = "bus_substitution", treated_group = "Shinbundang corridor bus stops")
)

save_csv(bus_did_results, file.path(EXT_TABLES, "bus_corridor_did.csv"))

bus_trend <- bus_panel %>%
  group_by(month, group) %>%
  summarise(mean_log_total_riders = mean(log_total_riders, na.rm = TRUE), .groups = "drop")

ggplot(bus_trend, aes(month, mean_log_total_riders, linetype = group)) +
  geom_vline(xintercept = POST_MONTH, linetype = "dashed") +
  geom_line(linewidth = 0.5) +
  labs(title = "Bus ridership trend: treated corridor vs controls", x = "Month", y = "Mean log riders", linetype = NULL) +
  theme_minimal()
ggsave(file.path(EXT_FIGURES, "bus_corridor_ridership_trend.png"), width = 9, height = 4.8, dpi = 200)

bus_plot <- bus_did_results %>%
  mutate(
    label = recode(
      outcome,
      log_total_riders = "Total board+alight",
      log_total_board = "Total boarding",
      log_total_alight = "Total alighting",
      log_morning_alight = "AM alighting 07-10",
      log_evening_board = "PM boarding 17-20",
      log_late_night_total = "Late-night total"
    )
  )

ggplot(bus_plot, aes(pct_effect_if_log, reorder(label, pct_effect_if_log))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = ci_low_pct_if_log, xmax = ci_high_pct_if_log), orientation = "y", width = 0.16) +
  labs(title = "Bus DID around Shinbundang extension corridor", x = "Percent effect", y = NULL) +
  theme_minimal()
ggsave(file.path(EXT_FIGURES, "bus_corridor_did_effects.png"), width = 8.5, height = 4.8, dpi = 200)

# ------------------------------------------------------------
# 2) Living population composition in secondary corridor.
# ------------------------------------------------------------

LIVING_PANEL_PATH <- file.path(OUT_PROCESSED, "living_population_dong_monthly_panel_2021_2024.csv")

read_living_member <- function(zip_path, member) {
  message("  living: ", basename(zip_path), " / ", member)
  cmd <- sprintf("unzip -p %s %s", shQuote(zip_path), shQuote(member))
  cols <- 1:32
  dt <- data.table::fread(cmd = cmd, header = FALSE, skip = 1, fill = TRUE, select = cols, showProgress = FALSE)
  setnames(dt, paste0("V", cols))

  dt <- dt[V3 %in% TARGET_DONG_CODES]
  if (nrow(dt) == 0) return(tibble())

  male_cols <- paste0("V", 5:18)
  female_cols <- paste0("V", 19:32)
  age20_30_cols <- paste0("V", c(8:11, 22:25))
  age40_50_cols <- paste0("V", c(12:15, 26:29))
  age60plus_cols <- paste0("V", c(16:18, 30:32))

  dt[, `:=`(
    date = as.Date(as.character(V1), "%Y%m%d"),
    hour = as.integer(V2),
    dong_code = as.integer(V3),
    total_population = as.numeric(V4)
  )]

  dt[, age20_30_population := rowSums(.SD, na.rm = TRUE), .SDcols = age20_30_cols]
  dt[, age40_50_population := rowSums(.SD, na.rm = TRUE), .SDcols = age40_50_cols]
  dt[, age60plus_population := rowSums(.SD, na.rm = TRUE), .SDcols = age60plus_cols]
  dt[, male_population := rowSums(.SD, na.rm = TRUE), .SDcols = male_cols]
  dt[, month := as.Date(format(date, "%Y-%m-01"))]
  dt[, after_work_population := fifelse(hour %in% 17:20, total_population, 0)]
  dt[, late_night_population := fifelse(hour %in% c(21:23, 0:5), total_population, 0)]

  dt[, .(
    total_population = mean(total_population, na.rm = TRUE),
    population_sum = sum(total_population, na.rm = TRUE),
    age20_30_population = sum(age20_30_population, na.rm = TRUE),
    age40_50_population = sum(age40_50_population, na.rm = TRUE),
    age60plus_population = sum(age60plus_population, na.rm = TRUE),
    male_population = sum(male_population, na.rm = TRUE),
    after_work_population = sum(after_work_population, na.rm = TRUE),
    late_night_population = sum(late_night_population, na.rm = TRUE)
  ), by = .(dong_code, month)] %>%
    as_tibble()
}

build_living_panel <- function() {
  living_dir <- file.path(DATA_RAW, "living_population")
  zip_files <- list.files(living_dir, pattern = "^LOCAL_PEOPLE_DONG_202(1|2|3|4).*\\.zip$", full.names = TRUE)
  zip_files <- zip_files[file.size(zip_files) > 1000]
  if (length(zip_files) == 0) stop("No living population zip files found under data/raw/living_population.")

  panel <- purrr::map_dfr(zip_files, function(z) {
    members <- utils::unzip(z, list = TRUE)$Name
    purrr::map_dfr(members, ~read_living_member(z, .x))
  }) %>%
    group_by(dong_code, month) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      dong_name = case_when(
        dong_code %in% SECONDARY_CODES ~ unname(SECONDARY_TREATED[as.character(dong_code)]),
        dong_code %in% ANCHOR_CODES ~ unname(ANCHOR_TREATED[as.character(dong_code)]),
        dong_code %in% CONTROL_CODES ~ unname(CONTROL_MAIN[as.character(dong_code)]),
        TRUE ~ NA_character_
      ),
      group = case_when(
        dong_code %in% SECONDARY_CODES ~ "secondary",
        dong_code %in% ANCHOR_CODES ~ "anchor",
        dong_code %in% CONTROL_CODES ~ "control",
        TRUE ~ NA_character_
      ),
      month_str = format(month, "%Y-%m"),
      post = as.integer(month >= POST_MONTH),
      transition = as.integer(month == TRANSITION_MONTH),
      secondary = as.integer(group == "secondary"),
      anchor = as.integer(group == "anchor"),
      did = secondary * post,
      anchor_post = anchor * post,
      secondary_post = secondary * post,
      log_total_population = log(total_population + 1),
      age20_30_share = safe_divide(age20_30_population, population_sum),
      age40_50_share = safe_divide(age40_50_population, population_sum),
      age60plus_share = safe_divide(age60plus_population, population_sum),
      male_share = safe_divide(male_population, population_sum),
      after_work_share = safe_divide(after_work_population, population_sum),
      late_night_share = safe_divide(late_night_population, population_sum),
      unit = paste0(dong_code, "_", dong_name)
    ) %>%
    filter(!is.na(group), transition == 0)

  save_csv(panel, LIVING_PANEL_PATH)
  panel
}

living_panel <- if (file.exists(LIVING_PANEL_PATH)) {
  readr::read_csv(LIVING_PANEL_PATH, show_col_types = FALSE) %>% mutate(month = as.Date(month))
} else {
  build_living_panel()
}

living_secondary_panel <- living_panel %>%
  filter(group %in% c("secondary", "control"))

living_outcomes <- c(
  "log_total_population", "age20_30_share", "age40_50_share",
  "age60plus_share", "male_share", "after_work_share", "late_night_share"
)

living_secondary_results <- fit_one_term(
  living_secondary_panel,
  living_outcomes,
  term = "did",
  fe = "unit + month_str",
  cluster = "unit",
  extra = list(hypothesis = "secondary_living_population", treated_group = "secondary")
)

save_csv(living_secondary_results, file.path(EXT_TABLES, "living_secondary_did.csv"))

living_split_results <- purrr::map_dfr(living_outcomes, function(y) {
  mod <- fixest::feols(
    as.formula(paste0(y, " ~ anchor_post + secondary_post | unit + month_str")),
    cluster = ~ unit,
    data = living_panel
  )
  bind_rows(
    effect_row(mod, "anchor_post", y, list(hypothesis = "living_anchor_secondary_split", group = "anchor", n_obs = nobs(mod), n_units = n_distinct(living_panel$unit))),
    effect_row(mod, "secondary_post", y, list(hypothesis = "living_anchor_secondary_split", group = "secondary", n_obs = nobs(mod), n_units = n_distinct(living_panel$unit)))
  )
})

save_csv(living_split_results, file.path(EXT_TABLES, "living_anchor_secondary_split_did.csv"))

living_trend <- living_panel %>%
  group_by(month, group) %>%
  summarise(age20_30_share = mean(age20_30_share, na.rm = TRUE), .groups = "drop")

ggplot(living_trend, aes(month, age20_30_share * 100, linetype = group)) +
  geom_vline(xintercept = POST_MONTH, linetype = "dashed") +
  geom_line(linewidth = 0.5) +
  labs(title = "Living population age 20-30 share", x = "Month", y = "Share (%)", linetype = NULL) +
  theme_minimal()
ggsave(file.path(EXT_FIGURES, "living_age20_30_share_trend.png"), width = 9, height = 4.8, dpi = 200)

living_plot <- living_secondary_results %>%
  filter(outcome != "log_total_population") %>%
  mutate(
    label = recode(
      outcome,
      age20_30_share = "Age 20-30 share",
      age40_50_share = "Age 40-50 share",
      age60plus_share = "Age 60+ share",
      male_share = "Male share",
      after_work_share = "After-work share",
      late_night_share = "Late-night share"
    )
  )

ggplot(living_plot, aes(effect_pp_if_share, reorder(label, effect_pp_if_share))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = ci_low_pp_if_share, xmax = ci_high_pp_if_share), orientation = "y", width = 0.16) +
  labs(title = "Secondary corridor DID: living population composition", x = "Percentage-point effect", y = NULL) +
  theme_minimal()
ggsave(file.path(EXT_FIGURES, "living_secondary_composition_effects.png"), width = 8.5, height = 4.8, dpi = 200)

# ------------------------------------------------------------
# 3) Dining-type consumption vs nightlife in secondary corridor.
# ------------------------------------------------------------

service_panel_path <- file.path(OUT_PROCESSED, "commerce_service_dong_quarter_panel_2019_2024.csv")
if (!file.exists(service_panel_path)) source("R/04_prepare_commerce.R")

service_panel <- readr::read_csv(service_panel_path, show_col_types = FALSE)

service_group_panel <- service_panel %>%
  mutate(
    service_group = case_when(
      str_detect(service_name, "한식|중식|일식|양식|분식|패스트푸드|치킨|음식점") &
        !str_detect(service_name, "커피|제과|호프|주점|노래방") ~ "dining",
      str_detect(service_name, "호프|주점|노래방") ~ "nightlife",
      str_detect(service_name, "편의점|슈퍼마켓|반찬가게") ~ "convenience",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(service_group), dong_code %in% c(SECONDARY_CODES, CONTROL_CODES), transition == 0) %>%
  group_by(service_group, dong_code, dong_name, quarter_code, qstr, post) %>%
  summarise(
    sales = sum(sales, na.rm = TRUE),
    transactions = sum(transactions, na.rm = TRUE),
    sales_17_21 = sum(sales_17_21, na.rm = TRUE),
    sales_21_24 = sum(sales_21_24, na.rm = TRUE),
    sales_00_06 = sum(sales_00_06, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    unit = paste0(dong_code, "_", dong_name),
    secondary = as.integer(dong_code %in% SECONDARY_CODES),
    did = secondary * post,
    log_sales = log(sales + 1),
    log_transactions = log(transactions + 1),
    log_after_work_sales = log(sales_17_21 + sales_21_24 + 1),
    log_late_night_sales = log(sales_21_24 + sales_00_06 + 1),
    late_night_share = safe_divide(sales_21_24 + sales_00_06, sales)
  )

dining_outcomes <- c("log_sales", "log_transactions", "log_after_work_sales", "log_late_night_sales", "late_night_share")

commerce_time_service_results <- service_group_panel %>%
  group_by(service_group) %>%
  group_modify(~fit_one_term(
    .x,
    dining_outcomes,
    term = "did",
    fe = "unit + qstr",
    cluster = "unit",
    extra = list(hypothesis = "secondary_dining_vs_nightlife", treated_group = "secondary")
  )) %>%
  ungroup()

save_csv(commerce_time_service_results, file.path(EXT_TABLES, "commerce_secondary_dining_nightlife_did.csv"))

commerce_plot <- commerce_time_service_results %>%
  filter(outcome %in% c("log_after_work_sales", "log_late_night_sales")) %>%
  mutate(label = paste(service_group, recode(outcome, log_after_work_sales = "after-work", log_late_night_sales = "late-night")))

ggplot(commerce_plot, aes(pct_effect_if_log, reorder(label, pct_effect_if_log))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = ci_low_pct_if_log, xmax = ci_high_pct_if_log), orientation = "y", width = 0.16) +
  labs(title = "Secondary corridor DID: consumption time by service group", x = "Percent effect", y = NULL) +
  theme_minimal()
ggsave(file.path(EXT_FIGURES, "commerce_dining_nightlife_time_effects.png"), width = 8.5, height = 4.8, dpi = 200)

# ------------------------------------------------------------
# 4) Tue-Thu commuting reallocation in subway daily data.
# ------------------------------------------------------------

subway_daily_path <- file.path(OUT_PROCESSED, "subway_daily_panel_2018_2024.csv")
if (!file.exists(subway_daily_path)) source("R/02_prepare_subway.R")

subway_daily <- readr::read_csv(subway_daily_path, show_col_types = FALSE) %>%
  mutate(
    date = as.Date(date),
    date_str = format(date, "%Y-%m-%d"),
    dow = lubridate::wday(date, week_start = 1),
    day_group = case_when(
      dow %in% 2:4 ~ "Tue-Thu",
      dow %in% c(1, 5) ~ "Mon/Fri",
      TRUE ~ "Weekend"
    ),
    did = treated * post_day
  ) %>%
  filter(transition_month == 0)

subway_daygroup_results <- subway_daily %>%
  group_by(day_group) %>%
  group_modify(~ {
    mod <- fixest::feols(
      log_riders ~ did | station_line + date_str,
      cluster = ~ station_line,
      data = .x
    )
    effect_row(mod, "did", "log_riders", list(hypothesis = "subway_midweek_commute", n_obs = nobs(mod), n_units = n_distinct(.x$station_line)))
  }) %>%
  ungroup()

save_csv(subway_daygroup_results, file.path(EXT_TABLES, "subway_daygroup_did.csv"))

ggplot(subway_daygroup_results, aes(pct_effect_if_log, reorder(day_group, pct_effect_if_log))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = ci_low_pct_if_log, xmax = ci_high_pct_if_log), orientation = "y", width = 0.16) +
  labs(title = "Subway DID by day group", x = "Percent effect", y = NULL) +
  theme_minimal()
ggsave(file.path(EXT_FIGURES, "subway_daygroup_effects.png"), width = 7.5, height = 4.2, dpi = 200)

# ------------------------------------------------------------
# Combined key estimates and short report.
# ------------------------------------------------------------

extension_key_estimates <- bind_rows(
  bus_did_results %>%
    transmute(hypothesis, group = treated_group, outcome, estimate = pct_effect_if_log, effect_scale = "percent", p_value, source_table = "bus_corridor_did"),
  living_secondary_results %>%
    transmute(hypothesis, group = treated_group, outcome, estimate = coalesce(pct_effect_if_log, effect_pp_if_share), effect_scale = if_else(startsWith(outcome, "log"), "percent", "percentage points"), p_value, source_table = "living_secondary_did"),
  commerce_time_service_results %>%
    transmute(hypothesis, group = service_group, outcome, estimate = coalesce(pct_effect_if_log, effect_pp_if_share), effect_scale = if_else(startsWith(outcome, "log"), "percent", "percentage points"), p_value, source_table = "commerce_secondary_dining_nightlife_did"),
  subway_daygroup_results %>%
    transmute(hypothesis, group = day_group, outcome, estimate = pct_effect_if_log, effect_scale = "percent", p_value, source_table = "subway_daygroup_did")
)

save_csv(extension_key_estimates, file.path(EXT_TABLES, "extension_key_estimates.csv"))

row_one <- function(df, outcome_value, group_col = NULL, group_value = NULL) {
  out <- df %>% filter(outcome == outcome_value)
  if (!is.null(group_col)) out <- out %>% filter(.data[[group_col]] == group_value)
  out %>% slice(1)
}

bus_total <- row_one(bus_did_results, "log_total_riders")
bus_am <- row_one(bus_did_results, "log_morning_alight")
living_age <- row_one(living_secondary_results, "age20_30_share")
living_after <- row_one(living_secondary_results, "after_work_share")
dining_after <- row_one(commerce_time_service_results, "log_after_work_sales", "service_group", "dining")
night_late <- row_one(commerce_time_service_results, "log_late_night_sales", "service_group", "nightlife")
midweek <- row_one(subway_daygroup_results, "log_riders", "day_group", "Tue-Thu")

report_lines <- c(
  "# 재배치 가설 추가 분석 결과",
  "",
  "## 핵심 결과",
  "",
  paste0("- 버스 corridor 총 승하차 DID: ", fmt_pct(bus_total$pct_effect_if_log), ", p = ", fmt_p(bus_total$p_value), "."),
  paste0("- 버스 corridor 출근시간 하차 DID: ", fmt_pct(bus_am$pct_effect_if_log), ", p = ", fmt_p(bus_am$p_value), "."),
  paste0("- Secondary corridor 생활인구 20-30대 비중 DID: ", fmt_pp(living_age$effect_pp_if_share), ", p = ", fmt_p(living_age$p_value), "."),
  paste0("- Secondary corridor 생활인구 after-work 시간대 비중 DID: ", fmt_pp(living_after$effect_pp_if_share), ", p = ", fmt_p(living_after$p_value), "."),
  paste0("- Secondary corridor 식사형 업종 after-work 매출 DID: ", fmt_pct(dining_after$pct_effect_if_log), ", p = ", fmt_p(dining_after$p_value), "."),
  paste0("- Secondary corridor nightlife 업종 late-night 매출 DID: ", fmt_pct(night_late$pct_effect_if_log), ", p = ", fmt_p(night_late$p_value), "."),
  paste0("- 지하철 Tue-Thu DID: ", fmt_pct(midweek$pct_effect_if_log), ", p = ", fmt_p(midweek$p_value), "."),
  "",
  "## 해석",
  "",
  "추가 분석은 신분당선 개통 효과가 총량 증가보다 교통·소비 흐름의 재배치로 나타났는지 확인하기 위한 것이다.",
  "버스 분석은 강남·신논현·논현·신사역 인근 버스 정류장의 노선-정류장-월 승하차량을 주변 통제 정류장과 비교했다.",
  "생활인구 분석은 2021-2024년 행정동-시간대 생활인구 자료를 사용해 secondary corridor의 연령·시간대 구성이 통제동 대비 어떻게 변했는지 보았다.",
  "상권 시간대 분석은 secondary corridor의 소비를 식사형 업종, nightlife 업종, 편의형 소매로 나누어 after-work와 late-night 매출을 비교했다.",
  "",
  "거리 gradient와 창폐업 가설은 상권 또는 점포 단위 위치 자료와 창폐업 자료가 필요하다. 현재 행정동 단위 상권 패널만으로는 역으로부터의 거리별 효과를 방어적으로 추정하기 어렵다."
)

writeLines(report_lines, file.path(EXT_DIR, "REALLOCATION_EXTENSIONS_REPORT.md"), useBytes = TRUE)

message("Reallocation extension analysis complete.")
