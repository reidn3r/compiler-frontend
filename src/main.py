import sys
from src.components.parser import Parser
from src.utils.read_file import file_to_buffer
from src.components.ast import print_ast

def main():
  if len(sys.argv) < 2:
    print("Usage: python -m src.main <input_path>")
    sys.exit(1)

  code_path = sys.argv[1]
  buffer: str = file_to_buffer(path=code_path)
  parser = Parser()
  ast = parser.parse(buffer)
  
  if ast:
    print_ast(ast)

if __name__ == "__main__":
  main()
