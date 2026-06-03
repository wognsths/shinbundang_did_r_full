"""
발표 대본 TTS 생성 스크립트
사용법: python generate_tts.py --key sk-YOUR_API_KEY
"""

import argparse
import re
import os
from pathlib import Path

try:
    from openai import OpenAI
except ImportError:
    print("openai 패키지가 없습니다. 설치:")
    print("  pip install openai")
    raise SystemExit(1)

SCRIPT_PATH = Path(__file__).parent / "presentation_script.md"
OUT_DIR = Path(__file__).parent / "tts_audio"

VOICE = "ash"
MODEL = "gpt-4o-mini-tts"
INSTRUCTIONS = (
    "한국어 학술 발표 톤으로 읽어주세요. "
    "차분하고 명확하게, 숫자와 통계는 또렷하게 발음해주세요. "
    "속도는 약간 느리게, 슬라이드 전환 느낌으로 각 단락 사이에 짧은 호흡을 넣어주세요."
)


def parse_slides(text: str) -> list[dict]:
    pattern = r"## \[슬라이드 (\d+)\s*/\s*~?(\d+)초\]\s*(.+)\n\n(.+?)(?=\n---|\Z)"
    matches = re.findall(pattern, text, re.DOTALL)
    slides = []
    for num, secs, title, body in matches:
        slides.append({
            "num": int(num),
            "title": title.strip(),
            "secs": int(secs),
            "text": body.strip(),
        })
    return slides


def main():
    parser = argparse.ArgumentParser(description="발표 대본 TTS 생성")
    parser.add_argument("--key", required=True, help="OpenAI API 키")
    parser.add_argument("--voice", default=VOICE, help=f"음성 (기본: {VOICE})")
    parser.add_argument("--model", default=MODEL, help=f"모델 (기본: {MODEL})")
    parser.add_argument("--script", default=str(SCRIPT_PATH), help="대본 경로")
    parser.add_argument("--slides", default=None, help="특정 슬라이드만 생성 (예: 1,3,5)")
    args = parser.parse_args()

    client = OpenAI(api_key=args.key)
    OUT_DIR.mkdir(exist_ok=True)

    script_text = Path(args.script).read_text(encoding="utf-8")
    slides = parse_slides(script_text)
    print(f"대본에서 {len(slides)}개 슬라이드 파싱 완료")

    target_nums = None
    if args.slides:
        target_nums = set(int(x) for x in args.slides.split(","))

    for slide in slides:
        if target_nums and slide["num"] not in target_nums:
            continue

        filename = f"slide_{slide['num']:02d}_{slide['title'][:20]}.mp3"
        filename = re.sub(r'[<>:"/\\|?*]', '_', filename)
        out_path = OUT_DIR / filename

        print(f"  슬라이드 {slide['num']:2d} ({slide['secs']}초) — {slide['title'][:30]}...", end=" ", flush=True)

        response = client.audio.speech.create(
            model=args.model,
            voice=args.voice,
            input=slide["text"],
            instructions=INSTRUCTIONS,
            response_format="mp3",
        )
        response.stream_to_file(str(out_path))
        print(f"-> {out_path.name}")

    print(f"\n완료. {OUT_DIR}/ 에 MP3 파일 생성됨.")
    print("음성 변경: --voice alloy|ash|ballad|coral|echo|fable|nova|onyx|sage|shimmer")


if __name__ == "__main__":
    main()
