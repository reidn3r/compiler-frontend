import src.components.symbol as symbol

class ASTNode:
  def __init__(self, line):
    self.line = line
    self.type = None

class Program(ASTNode):
  def __init__(self, id, block, line):
    super().__init__(line)
    self.id = id
    self.block = block

class Block(ASTNode):
  def __init__(self, var_section, subr_section, compound_stmt, line):
    super().__init__(line)
    self.var_section = var_section
    self.subr_section = subr_section
    self.compound_stmt = compound_stmt

class VarSection(ASTNode):
  def __init__(self, declarations, line):
    super().__init__(line)
    self.declarations = declarations

class VarDeclaration(ASTNode):
  def __init__(self, identifiers, var_type, line):
    super().__init__(line)
    self.identifiers = identifiers
    self.var_type = var_type

class CommandSeq(ASTNode):
  def __init__(self, statements, line):
    super().__init__(line)
    self.statements = statements

class Assignment(ASTNode):
  def __init__(self, id, expr, line):
    super().__init__(line)
    self.id = id
    self.expr = expr

class UnaryOp(ASTNode):
  def __init__(self, op, operand, line):
    super().__init__(line)
    self.op = op
    self.operand = operand

class BinaryOp(ASTNode):
  def __init__(self, op, left, right, line):
    super().__init__(line)
    self.op = op
    self.left = left
    self.right = right

class ReadStatement(ASTNode):
  def __init__(self, identifiers, line):
    super().__init__(line)
    self.identifiers = identifiers

class WriteStatement(ASTNode):
  def __init__(self, args, line):
    super().__init__(line)
    self.args = args

class IfStatement(ASTNode):
  def __init__(self, condition, then_stmt, else_stmt, line):
    super().__init__(line)
    self.condition = condition
    self.then_stmt = then_stmt
    self.else_stmt = else_stmt

class WhileStatement(ASTNode):
  def __init__(self, condition, body, line):
    super().__init__(line)
    self.condition = condition
    self.body = body

class SubroutineSection(ASTNode):
  def __init__(self, declarations, line):
    super().__init__(line)
    self.declarations = declarations

class ProcedureDeclaration(ASTNode):
  def __init__(self, id, parameters, body, line):
    super().__init__(line)
    self.id = id
    self.parameters = parameters
    self.body = body

class FunctionDeclaration(ASTNode):
  def __init__(self, id, parameters, return_type, body, line):
    super().__init__(line)
    self.id = id
    self.parameters = parameters
    self.return_type = return_type
    self.body = body

class ParameterDeclaration(ASTNode):
  def __init__(self, identifiers, param_type, line):
    super().__init__(line)
    self.identifiers = identifiers
    self.param_type = param_type

class SubroutineBlock(ASTNode):
  def __init__(self, declarations, body, line):
    super().__init__(line)
    self.declarations = declarations
    self.body = body

class ProcedureCall(ASTNode):
  def __init__(self, id, args, line):
    super().__init__(line)
    self.id = id
    self.args = args or []

class FunctionCall(ASTNode):
  def __init__(self, id, args, line, return_type=None):
    super().__init__(line)
    self.id = id
    self.args = args or []
    self.type = return_type

class Variable(ASTNode):
  def __init__(self, id, line, var_type=None):
    super().__init__(line)
    self.id = id
    self.type = var_type

class BooleanLiteral(ASTNode):
  def __init__(self, value, line):
    super().__init__(line)
    self.value = value
    self.type = symbol.Type.BOOLEAN

class NumberLiteral(ASTNode):
  def __init__(self, value, line):
    super().__init__(line)
    self.value = value
    self.type = symbol.Type.INTEGER

class ErrorNode(ASTNode):
  def __init__(self, line):
    super().__init__(line)

def print_ast(node, indent=0):
  prefix = "  " * indent
  print(f"{prefix}{node.__class__.__name__}")

  for attr, value in node.__dict__.items():
    if attr.startswith('_'):
      continue
    print(f"{prefix}  {attr}:", end=" ")
    if isinstance(value, ASTNode):
      print()
      print_ast(value, indent + 2)
    elif isinstance(value, list):
      print()
      for item in value:
        if isinstance(item, ASTNode):
          print_ast(item, indent + 2)
        else:
          print(f"{prefix}    {item}")
    else:
      print(value)
