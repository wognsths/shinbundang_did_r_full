# ------------------------------------------------------------
# 00_config.R
# Project-level configuration for the Shinbundang DID analysis
# ------------------------------------------------------------

ensure_utf8_locale <- function() {
  current <- Sys.getlocale("LC_CTYPE")
  if (grepl("UTF-8|UTF8", current, ignore.case = TRUE)) return(invisible(current))

  for (loc in c("en_US.UTF-8", "ko_KR.UTF-8", "C.UTF-8")) {
    updated <- suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
    if (!is.na(updated) && grepl("UTF-8|UTF8", updated, ignore.case = TRUE)) {
      return(invisible(updated))
    }
  }

  invisible(current)
}

ensure_utf8_locale()

required_packages <- c(
  "data.table", "dplyr", "tidyr", "stringr", "stringi", "readr", "lubridate",
  "fixest", "ggplot2", "purrr", "broom", "jsonlite"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    "\nInstall them with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))"
  )
}

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(fixest)
library(ggplot2)
library(purrr)
library(broom)
library(jsonlite)

PROJECT_ROOT <- normalizePath(getwd(), mustWork = TRUE)
PROJECT_RENVIRON <- file.path(PROJECT_ROOT, ".Renviron")
if (file.exists(PROJECT_RENVIRON)) readRenviron(PROJECT_RENVIRON)

SEOUL_OPEN_DATA_API_KEY <- Sys.getenv("SEOUL_OPEN_DATA_API_KEY", unset = "")
GYEONGGI_FLOW_API_KEY <- Sys.getenv("GYEONGGI_FLOW_API_KEY", unset = "")
GYEONGGI_PURPOSE_API_KEY <- Sys.getenv("GYEONGGI_PURPOSE_API_KEY", unset = "")

DATA_RAW <- file.path(PROJECT_ROOT, "data", "raw")
DATA_EXTRACTED <- file.path(PROJECT_ROOT, "data", "extracted")
OUT_DIR <- file.path(PROJECT_ROOT, "outputs")
OUT_PROCESSED <- file.path(OUT_DIR, "processed")
OUT_TABLES <- file.path(OUT_DIR, "tables")
OUT_FIGURES <- file.path(OUT_DIR, "figures")

for (d in c(DATA_RAW, DATA_EXTRACTED, OUT_DIR, OUT_PROCESSED, OUT_TABLES, OUT_FIGURES)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Treatment timing: 신분당선 강남~신사 연장 개통일
OPEN_DATE <- as.Date("2022-05-28")
POST_MONTH <- as.Date("2022-06-01")
TRANSITION_MONTH <- as.Date("2022-05-01")
POST_Q <- 20223       # 2022Q3부터 post
TRANSITION_Q <- 20222 # 개통일이 포함된 2022Q2는 transition quarter

# Station-line units. 신규 신분당선 역 자체가 아니라 기존 환승/인접 역의 기존 노선 승하차량을 본다.
TREATED_STATIONS <- c("2호선_강남", "9호선_신논현", "7호선_논현", "3호선_신사")

CONTROL_STATIONS <- c(
  "2호선_역삼", "2호선_선릉", "2호선_삼성(무역센터)", "2호선_교대(법원.검찰청)", "2호선_서초",
  "3호선_압구정", "3호선_잠원", "3호선_매봉", "3호선_양재(서초구청)", "3호선_교대(법원.검찰청)",
  "7호선_학동", "7호선_강남구청", "7호선_청담", "7호선_반포",
  "9호선_사평", "9호선_고속터미널", "9호선_신반포", "9호선_구반포",
  "9호선2~3단계_언주", "9호선2~3단계_선정릉", "9호선2~3단계_봉은사"
)
ALL_STATIONS <- c(TREATED_STATIONS, CONTROL_STATIONS)

# Commerce units: 행정동 단위. core는 개통역 영향권으로 가장 방어하기 쉬운 동만 사용.
TREATED_CORE <- c(
  `11680510` = "신사동",
  `11680521` = "논현1동",
  `11680640` = "역삼1동",
  `11650531` = "서초4동"
)

TREATED_EXTENDED <- c(
  TREATED_CORE,
  `11650540` = "잠원동",
  `11650560` = "반포1동",
  `11650520` = "서초2동"
)

CONTROL_MAIN <- c(
  `11680531` = "논현2동",
  `11680650` = "역삼2동",
  `11680545` = "압구정동",
  `11680565` = "청담동",
  `11680580` = "삼성1동",
  `11680590` = "삼성2동",
  `11680600` = "대치1동",
  `11680610` = "대치2동",
  `11680630` = "대치4동",
  `11650510` = "서초1동",
  `11650530` = "서초3동",
  `11650570` = "반포2동",
  `11650580` = "반포3동",
  `11650581` = "반포4동"
)

CONTAMINATED_DONGS <- as.integer(names(TREATED_EXTENDED))

setFixest_notes(FALSE)
