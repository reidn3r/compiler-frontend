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
          for decl in declarations:
            visit(decl)
        case ast.VarDeclaration():
          pass
        case ast.CommandSeq(statements=statements):
          for stmt in statements:
            visit(stmt)
        case ast.Assignment(expr=expr):
          visit(expr)
        case ast.UnaryOp(op=op, operand=operand):
          pass
        case ast.BinaryOp(op=op, left=left, right=right):
          pass
        case ast.ReadStatement(identifiers=identifiers):
          pass
        case ast.WriteStatement(args=args):
          pass
        case ast.IfStatement(condition=condition, then_stmt=then_stmt, else_stmt=else_stmt):
          pass
        case ast.WhileStatement(condition=condition, body=body):
          pass
        case ast.SubroutineSection(declarations=declarations):
          pass
        case ast.ProcedureDeclaration(parameters=parameters, body=body):
          pass
        case ast.FunctionDeclaration(parameters=parameters, return_type=return_type, body=body):
          pass
        case ast.ParameterDeclaration(identifiers=identifiers, param_type=param_type):
          pass
        case ast.SubroutineBlock(declarations=declarations, body=body):
          pass
        case ast.ProcedureCall(args=args):
          pass
        case ast.FunctionCall(args=args, return_type=return_type):
          pass
        case ast.Variable(id=id, var_type=var_type):
          pass
        case ast.BooleanLiteral(value=value):
          pass
        case ast.NumberLiteral(value=value):
          pass
        case _:
          pass
    
    visit(root)
