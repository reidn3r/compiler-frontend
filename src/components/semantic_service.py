import src.components.ast as ast
import src.components.symbol as symbol

class SemanticService:
  def __init__(self):
    self.symbolsTable: dict[tuple[str, str], symbol.Symbol] = {}
    self.scope = symbol.GLOBAL_SCOPE

  def set_scope(self, scope):
    self.scope = scope

  def error_node(self, line):
    return ast.ErrorNode(line)
  
  def print_error(self, msg, line):
    print(f"Erro semântico: {msg} (Linha {line})")

  def build_key(self, name):
    return (name, self.scope)

  def declare(self, name, type, category, line=-1):
    key = self.build_key(name)
    if key in self.symbolsTable:
      self.print_error(f"Identificador '{name}' já declarado neste escopo.", line)
      return False
    self.symbolsTable[key] = {
      "type": type,
      "category": category,
      "scope": self.scope
    }
    return True

  def lookup(self, name, line=-1):
    key = self.build_key(name)
    if key not in self.symbolsTable:
      self.print_error(f"Identificador '{name}' não declarado.", line)
      return None
    return self.symbolsTable[key]

  def assert_type(self, node, expected, line):
    if node.type != expected:
      self.print_error("Tipo incompatível.", line)
      return self.error_node(line)
    return node

  def assert_boolean(self, node, line):
    if node.type != symbol.Type.BOOLEAN:
      self.print_error("Expressão deve ser do tipo booleano.", line)
      return self.error_node(line)
    return node

  def assert_integer(self, node, line):
    if node.type != symbol.Type.INTEGER:
      self.print_error("Expressão deve ser do tipo inteiro.", line)
      return self.error_node(line)
    return node

  def assert_binary(self, left, right, expected, line):
    if left.type != expected or right.type != expected:
      if expected == symbol.Type.INTEGER:
        self.print_error("Operandos aritméticos devem ser inteiros.", line)
      else:
        self.print_error("Operandos lógicos devem ser booleanos.", line)
      return self.error_node(line)
    return expected

  def assert_param_count(self, args, expected, line):
    if len(args) != expected:
      self.print_error(f"Número de parâmetros deve ser igual a {expected}.", line)
      return False
    return True

  def assert_param_types(self, args, expected_types, line):
    for i, arg in enumerate(args):
      if arg.type != expected_types[i]:
        self.print_error(f"Tipo do parâmetro {i+1} não corresponde.", line)
        return False
    return True
