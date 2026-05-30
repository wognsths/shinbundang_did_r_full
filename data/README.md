# Data Files

이 저장소는 GitHub 업로드용으로 원자료와 대용량 중간 산출물을 포함하지 않습니다.

## 제외된 폴더

- `data/raw/`: 서울시 열린데이터광장, 서울 생활인구, 버스, 경기도데이터드림 등에서 받은 원자료
- `data/extracted/`: 압축 해제된 원자료
- `outputs/processed/`: 분석 과정에서 생성되는 대용량 패널 데이터

위 폴더들은 `.gitignore`에 의해 GitHub에 올라가지 않습니다.

## 로컬에서 재현하는 방법

1. `.Renviron.example`을 참고해 프로젝트 루트에 `.Renviron` 파일을 만들고 API 키를 입력합니다.
2. 원자료를 `data/raw/` 아래에 배치합니다.
3. 프로젝트 루트에서 다음을 실행합니다.

```bash
Rscript run_all.R
```

## 공개 저장소에 포함되는 파일

- R 분석 코드
- 실행 스크립트
- 최종 보고서 PDF/Rmd/Markdown
- 작은 결과표와 그림

원자료를 재배포해야 하는 경우에는 GitHub 저장소에 직접 넣기보다 GitHub Release, OSF, Zenodo 등 외부 데이터 저장소에 올리고 이 README에 링크를 추가하는 방식을 권장합니다.
