def file_to_buffer(path: str) -> str:
  buffer = ""
  with open(path, 'r') as f:
    buffer = f.read()
  return buffer
