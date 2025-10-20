from src.components.parser import Parser
from src.utils.read_file import file_to_buffer

def main():
  code_path="./program.rascal"
  buffer: str = file_to_buffer(path=code_path)

  parser = Parser()

  print(parser.parse(buffer))

if __name__ == "__main__":
  main()
