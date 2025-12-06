from typing import Dict, Tuple
import src.components.ast as ast
import src.components.symbol as symbol

SymbolTable = Dict[Tuple[str, str], symbol.Symbol]

class SemanticService:
  def __init__(self):
    self.symbolsTable: SymbolTable = {}
    self.scope = symbol.GLOBAL_SCOPE
    self.addrCounter = 0

  def set_scope(self, scope):
    self.scope = scope

  def error_node(self, line):
    return ast.ErrorNode(line)
  
  def print_error(self, msg, line):
    print(f"Erro semântico: {msg} (Linha {line})")

  def build_key(self, identifier):
    return (identifier, self.scope)

  def declare(self, identifier: str, category: symbol.Category, type=None, param_types=None, param_count=None, line=-1):
    key = self.build_key(identifier)
    if key in self.symbolsTable:
      self.print_error(f"Identificador '{identifier}' já declarado neste escopo.", line)
      return False
    entry = {
      "category": category,
      "scope": self.scope,
      "address": self.addrCounter
    }
    if type is not None:
      entry["type"] = type
    if param_types is not None:
      entry["param_types"] = param_types
    if param_count is not None:
      entry["param_count"] = param_count
    self.symbolsTable[key] = entry
    self.addrCounter += 1
    return True

  def lookup(self, name, line=-1):
    key = self.build_key(name)
    if key not in self.symbolsTable:
      line >= 0 and self.print_error(f"Identificador '{name}' não declarado.", line)
      return None
    return self.symbolsTable[key]
  
  def assert_no_procedure(self, node, line):
    if node is None or node.type is None:
      self.print_error("Expressões não podem conter chamadas de procedimento.", line)
      return False
    return True

  def assert_type(self, node, expected, line):
    if node.type != expected:
      self.print_error("Tipo incompatível.", line)
      return self.error_node(line)
    return node
  
  def assert_assignable(self, var_type, expr_type, line):
    if expr_type != var_type:
      self.print_error("A expressão à direita deve possuir o mesmo tipo da variável à esquerda", line)
      return False
    return True

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

  def assert_unary(self, operand, expected, line):
    if operand is None or operand.type != expected:
      if expected == symbol.Type.INTEGER:
        self.print_error("Operandos de expressão aritmética devem ser inteiros.", line)
      else:
        self.print_error("Operandos de expressão lógica devem ser booleanos.", line)
      return None
    return expected

  def assert_binary(self, left, right, expected, line):
    if left.type != expected or right.type != expected:
      if expected == symbol.Type.INTEGER:
        self.print_error("Operandos aritméticos devem ser inteiros.", line)
      else:
        self.print_error("Operandos lógicos devem ser booleanos.", line)
      return self.error_node(line)
    return expected

  def assert_write_args(self, args, line):
    for expr in args:
      if expr is None or expr.type is None:
        self.print_error("Argumentos de write devem ser expressões válidas e bem tipadas.", line)
        return self.error_node(line)
    return args

  def assert_param_count(self, entry: symbol.Symbol, expected, line):
    if entry.get("param_count") != expected:
      self.print_error(f"Número de parâmetros deve ser igual a {expected}.", line)
      return False
    return True

  def assert_param_types(self, entry: symbol.Symbol, args, line):
    expected = entry.get("param_types", [])
    for i, arg in enumerate(args):
      if i >= len(expected) or arg.type != expected[i]:
        self.print_error(f"Tipo do parâmetro {i+1} não corresponde.", line)
        return False
    return True
