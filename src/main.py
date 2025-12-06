import sys
from src.components.parser import Parser
from src.utils.read_file import file_to_buffer
from src.components.ast import print_ast
from src.components.mepa_service import MepaService

def main():
  if len(sys.argv) < 2:
    print("Usage: python -m src.main <input_path>")
    sys.exit(1)

  code_path = sys.argv[1]
  buffer: str = file_to_buffer(path=code_path)
  parser = Parser()
  (ast, symbolsTable) = parser.parse(buffer)

  if ast and symbolsTable:
    print_ast(ast)
    mepaService = MepaService()
    mepaService.run(inputPath=code_path, root=ast, symbolsTable=symbolsTable)

if __name__ == "__main__":
  main()
