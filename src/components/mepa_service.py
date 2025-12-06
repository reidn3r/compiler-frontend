import src.components.ast as ast
from src.components.semantic_service import SymbolTable

class MepaService:
  def __init__(self):
    self.opMap = {
      '+': 'SOMA', 
      '-': 'SUBT', 
      '*': 'MULT', 
      'div': 'DIVI',
      'and': 'CONJ', 
      'or': 'DISJ', 
      'not': 'NEGA',
      '==': 'CMIG', 
      '!=': 'CMDG',
      '<': 'CMME', 
      '>': 'CMMA', 
      '<=': 'CMEG', 
      '>=': 'CMAG'
    }
    
  def run(self, inputPath: str, root: ast.ASTNode, symbolsTable: SymbolTable):
    outputPath = inputPath.split('.')[0] + ".mepa"
    with open(outputPath, "w") as mepaFile:
      self.generate_code(root, symbolsTable, mepaFile)

  def generate_code(self, root: ast.ASTNode, symbolsTable: SymbolTable, mepaFile: str):
    def write(instruction):
      mepaFile.write(instruction + "\n")

    def visit(node):
      match node:
        case ast.Program(block=block):
          write("INPP")
          write(f"AMEM {len(symbolsTable)}")
          visit(block)
          write("PARA")
          write("FIM")
        case ast.Block(var_section=var_section, subr_section=subr_section, compound_stmt=compound_stmt):
          visit(var_section)
          visit(subr_section)
          visit(compound_stmt)
        case ast.VarSection(declarations=declarations):
          return
        case ast.VarDeclaration(identifiers=identifiers, var_type=var_type):
          return
        case ast.CommandSeq(statements=statements):
          return
        case ast.Assignment(expr=expr):
          return
        case ast.UnaryOp(op=op, operand=operand):
          return
        case ast.BinaryOp(op=op, left=left, right=right):
          return
        case ast.ReadStatement(identifiers=identifiers):
          return
        case ast.WriteStatement(args=args):
          return
        case ast.IfStatement(condition=condition, then_stmt=then_stmt, else_stmt=else_stmt):
          return
        case ast.WhileStatement(condition=condition, body=body):
          return
        case ast.SubroutineSection(declarations=declarations):
          return
        case ast.ProcedureDeclaration(parameters=parameters, body=body):
          return
        case ast.FunctionDeclaration(parameters=parameters, return_type=return_type, body=body):
          return
        case ast.ParameterDeclaration(identifiers=identifiers, param_type=param_type):
          return
        case ast.SubroutineBlock(declarations=declarations, body=body):
          return
        case ast.ProcedureCall(args=args):
          return
        case ast.FunctionCall(args=args, return_type=return_type):
          return
        case ast.Variable(var_type=var_type):
          return
        case ast.BooleanLiteral(value=value):
          return
        case ast.NumberLiteral(value=value):
          return
        case _:
          print("Nó inválido na AST")
    
    visit(root)
