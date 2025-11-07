from pathlib import Path
from src.components.parser import Parser
from src.components.lexer import LexicalError
from src.utils.read_file import file_to_buffer

SAMPLES_DIR = Path(__file__).resolve().parent.parent / "samples"

def run_parser_on_file(file_path: str):
    print("=" * 80)
    try:
        rel_path = file_path.relative_to(Path.cwd())
    except ValueError:
        rel_path = file_path
    print(f"Testing file: {rel_path}")
    print("-" * 80)

    buffer: str = file_to_buffer(path=file_path)

    parser = Parser()

    try:
      parser.parse(buffer)
    except LexicalError:
      pass
    
def main():
    samples = sorted(SAMPLES_DIR.glob("**/*.ras"))
    if not samples:
        print(f"No .ras files found in {SAMPLES_DIR}")
        return

    print("\nRunning parser tests on sample programs...\n")

    for file_path in samples:
        run_parser_on_file(file_path)

    print("All tests completed.\n")


if __name__ == "__main__":
    main()
