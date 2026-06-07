# 신분당선 강남–신사 연장 개통 효과 분석: R 코드베이스

이 프로젝트는 **신분당선 강남–신사 연장 개통(2022-05-28)**을 교통 인프라 shock으로 두고, 주변 기존 지하철역 이용량과 행정동 상권 지표의 변화를 분석합니다.

## GitHub 공개 방식

이 저장소에는 코드, 최종 보고서, 작은 결과표와 그림만 포함합니다. 원자료와 대용량 중간 산출물은 용량과 재배포 이슈 때문에 Git에 올리지 않습니다.

- 제외: `data/raw/`, `data/extracted/`, `outputs/processed/`
- 포함: `R/`, `run_*.R`, `outputs/FINAL_REPORT_DRAFT.pdf`, `outputs/FINAL_REPORT_DRAFT.Rmd`, `outputs/FINAL_REPORT_DRAFT.md`, 최종 보고서에 쓰인 작은 결과표와 그림
- API 키: `.Renviron`에 저장하고 GitHub에는 올리지 않습니다. `.Renviron.example`만 참고용으로 포함합니다.

원자료 배치와 재현 방법은 `data/README.md`를 참고하세요.

핵심 아이디어는 다음입니다.

- **메인 분석:** 기존 지하철 환승/인접역의 기존 노선 승하차량 변화
- **보조 분석:** 개통역 주변 행정동의 상권 매출, 거래건수, 야간 매출 비중 변화
- **방법:** Difference-in-Differences, event study, robustness checks, simple synthetic control

현재 `outputs/`에는 이미 계산된 결과 파일과 그림이 들어 있습니다. 원자료를 `data/raw/`에 넣고 `Rscript run_all.R`를 실행하면 같은 구조로 재생성됩니다.

---

## 1. 연구 질문

### 교통 분석

> 신분당선 강남–신사 연장 개통 이후, 기존 환승/인접 지하철역의 기존 노선 승하차량은 주변 통제역 대비 어떻게 변했는가?

여기서 중요한 점은 **신규 신분당선 역 자체의 이용량**을 보는 것이 아니라, 다음 기존 역-노선 단위의 승하차량을 본다는 것입니다.

- `2호선_강남`
- `9호선_신논현`
- `7호선_논현`
- `3호선_신사`

즉 해석은 “전체 대중교통 수요 증가/감소”가 아니라, **기존 노선 이용·환승 패턴의 재배치**에 가깝습니다.

### 상권 분석

> 신분당선 강남–신사 연장 개통 이후, 주변 행정동 상권의 총매출, 거래건수, 야간 매출 비중은 통제 행정동 대비 어떻게 변했는가?

상권 분석은 행정동-분기 단위이므로 역세권 효과가 희석될 수 있습니다. 따라서 보고서에서는 지하철 분석을 main evidence, 상권 분석을 secondary/exploratory evidence로 두는 것을 권장합니다.

---

## 2. 분석 설계

### 2.1 지하철 DID

단위는 역-노선 \(s\), 시간은 월 \(t\)입니다.

\[
Y_{s,t}
=
\alpha_s
+
\lambda_t
+
\beta (Treated_s \times Post_t)
+
\varepsilon_{s,t}.
\]

- \(Y_{s,t}\): 월평균 일일 승하차량의 로그
- \(\alpha_s\): 역-노선 fixed effect
- \(\lambda_t\): 월 fixed effect
- \(Treated_s\): 강남, 신논현, 논현, 신사 기존 노선 역이면 1
- \(Post_t\): 2022년 6월 이후면 1
- 2022년 5월은 개통일이 포함된 transition month로 제외

### 2.2 상권 DID

단위는 행정동 \(d\), 시간은 분기 \(q\)입니다.

\[
Y_{d,q}
=
\alpha_d
+
\lambda_q
+
\beta (Treated_d \times Post_q)
+
\varepsilon_{d,q}.
\]

- \(Y_{d,q}\): 총매출 로그, 거래건수 로그, 평균 결제단가 로그, 주말 비중, 야간 매출 비중 등
- \(\alpha_d\): 행정동 fixed effect
- \(\lambda_q\): 분기 fixed effect
- \(Treated_d\): 신사동, 논현1동, 역삼1동, 서초4동이면 1
- \(Post_q\): 2022Q3 이후면 1
- 2022Q2는 transition quarter로 제외

---

## 3. 프로젝트 구조

```text
shinbundang_did_r/
├── README.md
├── run_all.R
├── R/
│   ├── 00_config.R
│   ├── 01_utils.R
│   ├── 02_prepare_subway.R
│   ├── 03_analyze_subway.R
│   ├── 04_prepare_commerce.R
│   ├── 05_analyze_commerce.R
│   └── 06_make_summary.R
├── data/
│   └── raw/
│       └── PLACE_RAW_FILES_HERE.md
└── outputs/
    ├── processed/
    ├── tables/
    ├── figures/
    ├── balance/
    ├── identification/
    └── REPORT_SUMMARY.md
```

---

## 4. 실행 방법

### 4.1 패키지 설치

R에서 다음 패키지를 설치합니다.

```r
install.packages(c(
  "data.table", "dplyr", "tidyr", "stringr", "readr", "lubridate",
  "fixest", "ggplot2", "purrr", "broom", "jsonlite"
))
```

### 4.2 원자료 배치

`data/raw/` 폴더에 다음 자료를 넣습니다.

1. 지하철 일별 승하차 자료
   - `CARD_SUBWAY_MONTH_2018.csv`
   - `CARD_SUBWAY_MONTH_2019.csv`
   - `CARD_SUBWAY_MONTH_2020.csv`
   - `CARD_SUBWAY_MONTH_2021.csv`
   - `CARD_SUBWAY_MONTH_2022.csv`
   - `CARD_SUBWAY_MONTH_202301.csv` ~ `CARD_SUBWAY_MONTH_202312.csv`
   - `CARD_SUBWAY_MONTH_202401.csv` ~ `CARD_SUBWAY_MONTH_202412.csv`

2. 지하철 시간대별 승하차 자료
   - `서울시 지하철 호선별 역별 시간대별 승하차 인원 정보.csv`

3. 상권 추정매출-행정동 자료
   - 2019년, 2020년, 2021년, 2022년, 2023년, 2024년 ZIP

압축파일을 그대로 넣어도 됩니다. `run_all.R`이 `data/raw/`의 ZIP을 `data/extracted/`로 풀고 재귀적으로 검색합니다.

### 4.3 전체 실행

프로젝트 루트에서 실행합니다.

```bash
Rscript run_all.R
```

각 단계만 따로 실행할 수도 있습니다.

```bash
Rscript R/02_prepare_subway.R
Rscript R/03_analyze_subway.R
Rscript R/04_prepare_commerce.R
Rscript R/05_analyze_commerce.R
Rscript R/06_make_summary.R
```

Anchor 상권과 secondary corridor 상권을 분리하는 보조 분석만 따로 실행하려면 다음을 실행합니다.

```bash
Rscript run_anchor_secondary.R
```

교통 흐름·소비 구성 재배치 가설을 보는 추가 분석만 따로 실행하려면 다음을 실행합니다.

```bash
Rscript run_commute_composition.R
```

---

## 5. 주요 결과 요약

### 5.1 지하철 메인 DID

`outputs/tables/subway_did_main.csv`

| outcome | 추정치 |
|---|---:|
| 기존 환승/인접역 월평균 일일 승하차량 | 약 -11.0% |
| p-value | 약 0.052 |

해석:

> 개통 이후 기존 환승/인접역의 기존 노선 승하차량은 통제역 대비 약 11% 낮아진 것으로 추정됩니다. 다만 p-value가 0.05 근처이므로 강하게 단정하기보다는 “경계적으로 유의한 감소” 정도가 안전합니다.

이 결과는 **전체 지하철 수요 감소**가 아니라, 신분당선 개통에 따른 **기존 노선 승하차·환승 패턴 재배치**로 해석하는 편이 더 자연스럽습니다.

### 5.2 시간대별 지하철 DID

`outputs/tables/subway_timeband_did.csv`

| outcome | 추정 효과 | p-value |
|---|---:|---:|
| total | -11.0% | 0.052 |
| morning_peak | -12.2% | 0.010 |
| evening_peak | -9.9% | 0.071 |
| late_night | -4.0% | 0.578 |
| daytime | -12.6% | 0.059 |

해석:

> 총량 감소는 특히 출근 시간대에서 뚜렷합니다. 야간 시간대 효과는 유의하지 않습니다.

### 5.3 지하철 synthetic control

`outputs/tables/subway_scm_summary.csv`

| 항목 | 값 |
|---|---:|
| pre-RMSPE | 약 0.0178 |
| post average gap | 약 -12.1% |

해석:

> synthetic control에서도 DID와 비슷하게 post 기간에 약 -12% 수준의 gap이 관찰됩니다. 지하철 쪽 결과는 DID와 SCM 방향이 비교적 일관됩니다.

### 5.4 상권 DID

`outputs/tables/commerce_did_main_2019_2024.csv`

| outcome | 추정 효과 | p-value |
|---|---:|---:|
| 총매출 로그 | -9.6% | 0.121 |
| 거래건수 로그 | -10.6% | 0.122 |
| 평균 결제단가 로그 | +1.2% | 0.854 |
| 주말 매출 비중 | -0.008 | 0.369 |
| 야간 매출 비중 | +0.014 | 0.043 |
| 야간 거래 비중 | +0.008 | 0.422 |

해석:

> 상권 총매출과 거래건수는 유의하게 증가하지 않습니다. 야간 매출 비중은 약 1.4%p 증가한 것으로 보이지만, pretrend 문제가 크므로 causal effect로 강하게 주장하기 어렵습니다.

### 5.5 상권 pretrend 주의

`outputs/tables/commerce_pretrend_tests_2019_2024.csv`

상권 event-study pretrend test는 대부분 좋지 않습니다. 즉 상권 분석은 다음처럼 표현하는 것이 안전합니다.

> 지하철 개통이 행정동 전체 상권 규모를 확실히 키웠다는 근거는 약하다. 다만 야간 매출 비중과 소비자 연령 구성에서는 변화가 관찰되어, 상권 총량보다는 소비 구성 변화 가능성을 탐색적으로 제시할 수 있다.

### 5.6 Anchor vs secondary corridor 보조 분석

`outputs/anchor_secondary/`

기존 강한 상권인 신사동·역삼1동을 anchor treated로, 논현1동·서초4동을 secondary corridor treated로 분리한 보조 분석입니다. 핵심 결과는 다음처럼 해석하는 것이 안전합니다.

> secondary corridor의 총매출 증가 효과는 명확하지 않으며, 거래건수는 감소하는 방향입니다. 다만 20-30대 소비 비중은 증가하는 방향으로 관찰됩니다. 따라서 상권 총량 확대보다는 소비자 구성 변화 가능성을 보조적으로 제시하는 것이 적절합니다.

### 5.7 재배치 가설 추가 분석

`outputs/commute_composition/`

상권 총량 효과보다 교통 흐름과 소비 구성의 재배치 여부를 보는 추가 분석입니다. 핵심은 다음입니다.

- 기존 노선 감소는 출근 하차, 퇴근 승차 흐름에서 상대적으로 뚜렷합니다.
- 주중 효과가 주말보다 안정적으로 추정됩니다.
- secondary corridor의 총매출 증가 효과는 명확하지 않지만, 20-30대 소비 비중은 증가 방향으로 관찰됩니다.
- 연령·성별 분석은 여러 outcome을 함께 보는 이질성 분석이므로 exploratory evidence로 제시하는 것이 안전합니다.

---

## 6. 보고서에서 추천하는 주장 강도

### 강하게 말해도 되는 쪽

- 지하철 기존 노선 승하차량은 개통 이후 통제역 대비 감소하는 방향으로 나타난다.
- 시간대별로는 출근시간대 변화가 상대적으로 더 뚜렷하다.
- DID와 간단한 synthetic control이 비슷한 방향을 보인다.

### 약하게 말해야 하는 쪽

- 상권 총매출 효과는 명확하지 않다.
- 야간 매출 비중 변화는 관찰되지만 pretrend가 좋지 않으므로 exploratory result로 보는 것이 안전하다.
- 행정동 단위 자료는 역세권 효과를 희석시킬 수 있다.

---

## 7. 주요 출력 파일

### Report

- `outputs/FINAL_REPORT_DRAFT.md`
- `outputs/FINAL_REPORT_DRAFT.pdf`
- `outputs/FINAL_REPORT_DRAFT.Rmd`
- `outputs/REPORT_SUMMARY.md`

### Presentation

- `outputs/presentation.html`
- `outputs/presentation.pdf`
- `outputs/presentation_script.md`

### Tables

- `outputs/tables/subway_did_main.csv`
- `outputs/tables/subway_timeband_did.csv`
- `outputs/tables/subway_event_study_coefficients.csv`
- `outputs/tables/subway_pretrend_tests.csv`
- `outputs/tables/subway_did_robustness.csv`
- `outputs/tables/subway_scm_summary.csv`
- `outputs/tables/commerce_did_main_2019_2024.csv`
- `outputs/tables/commerce_pretrend_tests_2019_2024.csv`
- `outputs/tables/commerce_did_robustness_2019_2024.csv`
- `outputs/tables/commerce_scm_summary.csv`
- `outputs/tables/combined_key_estimates_2019_2024.csv`
- `outputs/balance/subway_pretreatment_balance.csv`
- `outputs/balance/commerce_pretreatment_balance.csv`
- `outputs/identification/tables/pretrend_diagnostic_summary.csv`
- `outputs/identification/tables/subway_control_set_sensitivity.csv`
- `outputs/identification/tables/subway_permutation_pvalue.csv`
- `outputs/identification/tables/commerce_dong_heterogeneity.csv`
- `outputs/anchor_secondary/tables/commerce_secondary_only_did.csv`
- `outputs/anchor_secondary/tables/commerce_anchor_vs_secondary_split_did.csv`
- `outputs/anchor_secondary/tables/commerce_single_dong_did.csv`
- `outputs/commute_composition/tables/subway_commute_direction_did.csv`
- `outputs/commute_composition/tables/subway_weekday_weekend_did.csv`
- `outputs/commute_composition/tables/commerce_secondary_consumption_composition_did.csv`
- `outputs/commute_composition/tables/commerce_secondary_age_gender_did.csv`
- `outputs/commute_composition/tables/commerce_anchor_secondary_composition_split_did.csv`
- `outputs/control_robustness/tables/control_robustness_summary_for_report.csv`
- `outputs/gyeonggi_activity/tables/gyeonggi_bundang_day_dong_did.csv`
- `outputs/supplementary_flows/tables/bus_corridor_did.csv`
- `outputs/supplementary_flows/tables/living_secondary_did.csv`

### Figures

- `outputs/figures/subway_trend_treated_control_2018_2024.png`
- `outputs/figures/subway_event_study_2018_2024.png`
- `outputs/figures/subway_scm_2018_2024.png`
- `outputs/figures/commerce_trend_sales_2019_2024.png`
- `outputs/figures/commerce_event_study_log_sales_2019_2024.png`
- `outputs/figures/commerce_event_study_night_share_2019_2024.png`
- `outputs/anchor_secondary/figures/commerce_three_group_sales_trend.png`
- `outputs/commute_composition/figures/subway_direction_timeband_effects.png`
- `outputs/commute_composition/figures/commerce_secondary_age_gender_effects.png`
- `outputs/control_robustness/figures/dag_identification.png`
- `outputs/control_robustness/figures/did_control_robustness.png`
- `outputs/identification/figures/subway_permutation_histogram.png`
- `outputs/supplementary_flows/figures/bus_corridor_did_effects.png`
- `outputs/supplementary_flows/figures/living_secondary_composition_effects.png`

---

## 8. 보고서 문장 예시

> 본 연구는 2022년 5월 28일 신분당선 강남–신사 연장 개통을 교통 인프라 shock으로 보고, 주변 기존 지하철역 및 행정동 상권에 미친 영향을 Difference-in-Differences와 synthetic control을 통해 분석한다. 지하철 분석에서는 개통역 자체가 아니라 기존 노선의 환승/인접 역을 처치역으로 정의하여, 신분당선 개통이 기존 노선 이용 패턴을 어떻게 재배치했는지 살펴본다. 분석 결과, 기존 환승/인접역의 기존 노선 승하차량은 통제역 대비 약 11% 감소하는 방향으로 나타났으며, 특히 출근시간대 감소가 상대적으로 뚜렷했다. 반면 행정동 단위 상권 총매출과 거래건수에 대해서는 유의한 증가 효과가 확인되지 않았다. 야간 매출 비중은 증가하는 방향으로 나타났으나, 상권 event-study의 pretrend가 좋지 않아 이 결과는 탐색적 증거로 해석한다.

---

## 9. 한계

1. 통제역/통제동 선택은 완전히 무작위가 아니므로 parallel trends 가정이 핵심입니다.
2. 2020–2021년 코로나 충격이 pre-period에 포함되어 있어 event-study pretrend 해석이 까다롭습니다.
3. 상권 자료는 행정동-분기 단위이므로 역 주변의 좁은 공간 효과를 직접 포착하기 어렵습니다.
4. 연령·성별 구성 결과는 여러 outcome을 함께 보는 탐색적 결과이므로 main evidence로 과도하게 해석하지 않아야 합니다.
5. 신분당선 신규 노선 자체의 승하차량이 전체 교통수요 변화와 함께 분석되지 않으면, 지하철 결과는 “전체 수요 변화”보다 “기존 노선 이용 패턴 변화”로 해석해야 합니다.
