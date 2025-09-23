from src.components.lexer import Lexer
from src.utils.read_file import file_to_buffer

def main():
  code_path="./program.rascal"
  buffer: str = file_to_buffer(path=code_path)

  lexer = Lexer().tokenize(buffer)

if __name__ == "__main__":
  main()
