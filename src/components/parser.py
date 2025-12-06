import ply.yacc as yacc
from src.components.lexer import Lexer
import src.components.ast as ast
import src.components.symbol as symbol
from src.components.semantic_service import SemanticService

class Parser:
  def __init__(self):
    self.lexer = Lexer()
    self.tokens = self.lexer.tokens
    self.parser = yacc.yacc(module=self)
    self.semanticService = SemanticService()

  def p_program(self, p):
    '''program : PROGRAM identifier SEMICOLON block DOT'''
    name = p[2]

    self.semanticService.symbolsTable[name] = {
      "category": symbol.Category.PROGRAM,
      "scope": self.semanticService.scope
    }

    p[0] = ast.Program(name, p[4], p.lineno(1))

  def p_block(self, p):
    '''block : var_declaration_section subroutine_declaration_section compound_statement
             | var_declaration_section compound_statement
             | subroutine_declaration_section compound_statement
             | compound_statement'''
    if len(p) == 4:
      p[0] = ast.Block(p[1], p[2], p[3], p.lineno(1))
    elif len(p) == 3:
      if isinstance(p[1], ast.VarSection):
        p[0] = ast.Block(p[1], None, p[2], p.lineno(1))
      else:
        p[0] = ast.Block(None, p[1], p[2], p.lineno(1))
    else:
      p[0] = ast.Block(None, None, p[1], p.lineno(1))

  def p_var_declaration_section(self, p):
    '''var_declaration_section : VAR var_declaration_list'''
    p[0] = ast.VarSection(p[2], p.lineno(1))

  def p_var_declaration_list(self, p):
    '''var_declaration_list : var_declaration SEMICOLON
                            | var_declaration_list var_declaration SEMICOLON'''
    if len(p) == 3:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

  def p_var_declaration(self, p):
    '''var_declaration : identifier_list COLON type'''
    ids = p[1]
    t = p[3]

    for ident in ids:
      key = self.semanticService.build_key(ident)
      if key in self.semanticService.symbolsTable:
        self.semanticService.print_error(f"Identificador '{ident}' já declarado neste escopo.", p.lineno(1))
      else:
        self.semanticService.symbolsTable[key] = {
          "type": symbol.Type(t),
          "category": symbol.Category.VARIABLE,
          "scope": self.semanticService.scope
        }

    p[0] = ast.VarDeclaration(ids, t, p.lineno(1))

  def p_identifier_list(self, p):
    '''identifier_list : identifier
                       | identifier_list COMMA identifier'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

  def p_type(self, p):
    '''type : BOOLEAN
            | INTEGER'''
    p[0] = p[1]

  def p_subroutine_declaration_section(self, p):
    '''subroutine_declaration_section : subroutine_declaration_list'''
    p[0] = ast.SubroutineSection(p[1], p.lineno(1))

  def p_subroutine_declaration_list(self, p):
    '''subroutine_declaration_list : subroutine_declaration SEMICOLON
                                   | subroutine_declaration_list subroutine_declaration SEMICOLON'''
    if len(p) == 3:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

  def p_subroutine_declaration(self, p):
    '''subroutine_declaration : procedure_declaration
                              | function_declaration'''
    p[0] = p[1]

  def p_procedure_declaration(self, p):
    '''procedure_declaration : PROCEDURE identifier local_scope formal_parameters SEMICOLON subroutine_block
                            | PROCEDURE identifier local_scope SEMICOLON subroutine_block'''
    hasParams = len(p) == 7
    self.semanticService.scope = symbol.GLOBAL_SCOPE

    name = p[2]
    key = self.semanticService.build_key(name)

    if key in self.semanticService.symbolsTable:
      self.semanticService.print_error(f"Identificador '{name}' já declarado neste escopo.", p.lineno(2))
      p[0] = ast.ErrorNode(p.lineno(2))
      return

    self.semanticService.symbolsTable[key] = {
      "category": symbol.Category.PROCEDURE,
      "scope": self.semanticService.scope,
      "param_types": []
    }

    if hasParams:
      params = p[4]
      count = 0
      for par in params:               # par is a ParameterDeclaration
        for _ in par.identifiers:      # each identifier in the parameter list
          self.semanticService.symbolsTable[key]["param_types"].append(symbol.Type(par.param_type))
          count += 1
      self.semanticService.symbolsTable[key]["param_count"] = count
    else:
      self.semanticService.symbolsTable[key]["param_count"] = 0

    if hasParams:
      p[0] = ast.ProcedureDeclaration(name, p[4], p[6], p.lineno(1))
    else:
      p[0] = ast.ProcedureDeclaration(name, None, p[5], p.lineno(1))

  def p_function_declaration(self, p):
    '''function_declaration : FUNCTION identifier local_scope formal_parameters COLON type prepare_return SEMICOLON subroutine_block
                            | FUNCTION identifier local_scope COLON type prepare_return SEMICOLON subroutine_block'''
    hasParams = len(p) == 10
    self.semanticService.scope = symbol.GLOBAL_SCOPE

    name = p[2]
    ret_type = p[6] if hasParams else p[5]
    key = self.semanticService.build_key(name)

    if key in self.semanticService.symbolsTable:
      self.semanticService.print_error(f"Identificador '{name}' já declarado neste escopo.", p.lineno(2))
      p[0] = ast.ErrorNode(p.lineno(2))
      return

    self.semanticService.symbolsTable[key] = {
      "type": symbol.Type(ret_type),
      "category": symbol.Category.FUNCTION,
      "scope": self.semanticService.scope,
      "param_types": []
    }

    if hasParams and p[4] is not None:
      params = p[4]
      count = 0
      for par in params:
        for _ in par.identifiers:
          self.semanticService.symbolsTable[key]["param_types"].append(symbol.Type(par.param_type))
          count += 1
      self.semanticService.symbolsTable[key]["param_count"] = count
    else:
      self.semanticService.symbolsTable[key]["param_count"] = 0

    if hasParams:
      p[0] = ast.FunctionDeclaration(name, p[4], ret_type, p[9], p.lineno(1))
    else:
      p[0] = ast.FunctionDeclaration(name, None, ret_type, p[8], p.lineno(1))

  def p_local_scope(self, p):
    "local_scope :"
    identifier = p[-1]
    self.semanticService.scope = identifier
    if p[-2] != 'function':
      return
    # Add the function id inside the subroutine scope
    key = self.semanticService.build_key(identifier)
    self.semanticService.symbolsTable[key] = {
      "category": symbol.Category.FUNCTION,
      "scope": self.semanticService.scope,
    }

  def p_prepare_return(self, p):
    "prepare_return :"
    # Add type to the local function symbol
    functionId = p[-5] if p[-4] is None else p[-4]
    type = p[-1]
    key = self.semanticService.build_key(functionId)
    if key in self.semanticService.symbolsTable:
      self.semanticService.symbolsTable[key]["type"] = symbol.Type(type)

  def p_subroutine_block(self, p):
    '''subroutine_block : var_declaration_section compound_statement
                        | compound_statement'''
    if len(p) == 3:
      p[0] = ast.SubroutineBlock(p[1], p[2], p.lineno(1))
    else:
      p[0] = ast.SubroutineBlock(None, p[1], p.lineno(1))

  def p_formal_parameters(self, p):
    '''formal_parameters : LPAREN parameter_declaration_list RPAREN'''
    p[0] = p[2]

  def p_parameter_declaration_list(self, p):
    '''parameter_declaration_list : parameter_declaration
                                  | parameter_declaration_list SEMICOLON parameter_declaration'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

  def p_parameter_declaration(self, p):
    '''parameter_declaration : identifier_list COLON type'''
    ids = p[1]
    t = p[3]

    for ident in ids:
      key = self.semanticService.build_key(ident)
      if key in self.semanticService.symbolsTable:
        self.semanticService.print_error(f"Identificador '{ident}' já declarado neste escopo.", p.lineno(1))
      else:
        self.semanticService.symbolsTable[key] = {
          "type": symbol.Type(t),
          "category": symbol.Category.PARAMETER,
          "scope": self.semanticService.scope
        }

    p[0] = ast.ParameterDeclaration(ids, t, p.lineno(1))

  def p_compound_statement(self, p):
    '''compound_statement : BEGIN statement_list END'''
    p[0] = ast.CommandSeq(p[2], p.lineno(1))

  def p_statement_list(self, p):
    '''statement_list : statement
                      | statement_list SEMICOLON statement'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

  def p_statement(self, p):
    '''statement : assignment
                 | procedure_call
                 | conditional
                 | repetition
                 | read_statement
                 | write_statement
                 | compound_statement'''
    p[0] = p[1]

  def p_assignment(self, p):
    '''assignment : identifier ASSIGN expression'''
    ident = p[1]
    key = self.semanticService.build_key(ident)

    if key not in self.semanticService.symbolsTable:
      self.semanticService.print_error(f"Identificador '{ident}' não declarado.", p.lineno(1))
      p[0] = ast.ErrorNode(p.lineno(1))
      return

    var_type = self.semanticService.symbolsTable[key]["type"]
    expr = p[3]

    if expr is not None and expr.type != var_type:
      self.semanticService.print_error("A expressão à direita deve possuir o mesmo tipo da variável à esquerda", p.lineno(2))
      p[0] = ast.ErrorNode(p.lineno(1))
      return

    p[0] = ast.Assignment(ident, expr, p.lineno(1))

  def p_procedure_call(self, p):
    '''procedure_call : identifier LPAREN expression_list RPAREN
                      | identifier LPAREN RPAREN'''
    hasArgs = len(p) == 5
    ident = p[1]
    key = self.semanticService.build_key(ident)

    if key not in self.semanticService.symbolsTable:
      self.semanticService.print_error(f"Identificador '{ident}' não declarado.", p.lineno(1))
      p[0] = ast.ErrorNode(p.lineno(1))
      return

    if hasArgs:
      args = p[3]
      expectedCount = self.semanticService.symbolsTable[key]["param_count"]
      expectedTypes = self.semanticService.symbolsTable[key]["param_types"]

      if len(args) != expectedCount:
        self.semanticService.print_error(f"Número de parâmetros de {ident} deve ser igual a {expectedCount}", p.lineno(2))
        p[0] = ast.ErrorNode(p.lineno(1))
        return

      for i, expr in enumerate(args):
        if expr.type != expectedTypes[i]:
          self.semanticService.print_error(f"Tipo do parâmetro {i+1} não corresponde", p.lineno(2))
          p[0] = ast.ErrorNode(p.lineno(1))
          return

      p[0] = ast.ProcedureCall(ident, args, p.lineno(1))
    else:
      p[0] = ast.ProcedureCall(ident, [], p.lineno(1))

  def p_conditional(self, p):
    '''conditional : IF expression THEN statement ELSE statement
                  | IF expression THEN statement'''
    cond = p[2]
    if cond.type != symbol.Type.BOOLEAN:
      self.semanticService.print_error("Expressão em condicional deve ser do tipo booleano", p.lineno(1))
      p[0] = ast.ErrorNode(p.lineno(1))
      return

    if len(p) == 7:
      p[0] = ast.IfStatement(cond, p[4], p[6], p.lineno(1))
    else:
      p[0] = ast.IfStatement(cond, p[4], None, p.lineno(1))

  def p_repetition(self, p):
    '''repetition : WHILE expression DO statement'''
    cond = p[2]
    if cond.type != symbol.Type.BOOLEAN:
      self.semanticService.print_error("Expressão em repetição deve ser do tipo booleano", p.lineno(1))
      p[0] = ast.ErrorNode(p.lineno(1))
      return
    p[0] = ast.WhileStatement(cond, p[4], p.lineno(1))

  def p_read_statement(self, p):
    '''read_statement : READ LPAREN identifier_list RPAREN'''
    id_list = p[3]
    for ident in id_list:
      key = self.semanticService.build_key(ident)
      if key not in self.semanticService.symbolsTable:
        self.semanticService.print_error("Argumentos de read devem ser variáveis declaradas e visíveis no escopo atual.", p.lineno(2))
        p[0] = ast.ErrorNode(p.lineno(2))
        return
    p[0] = ast.ReadStatement(id_list, p.lineno(1))

  def p_write_statement(self, p):
    '''write_statement : WRITE LPAREN expression_list RPAREN'''
    args = p[3]
    for expr in args:
      if expr.type is None:
        self.semanticService.print_error("Argumentos de write devem ser expressões válidas e bem tipadas.", p.lineno(2))
        p[0] = ast.ErrorNode(p.lineno(2))
        return
    p[0] = ast.WriteStatement(args, p.lineno(1))

  def p_expression_list(self, p):
    '''expression_list : expression
                       | expression_list COMMA expression'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

  def p_expression(self, p):
    '''expression : simple_expression relational_op simple_expression
                  | simple_expression'''
    if len(p) == 4:
      left = p[1]
      op = p[2]
      right = p[3]

      node = ast.BinaryOp(op, left, right, p.lineno(2))
      node.type = symbol.Type.BOOLEAN
      p[0] = node
    else:
      p[0] = p[1]

  def p_relational_op(self, p):
    '''relational_op : EQ
                     | NEQ
                     | LT
                     | LEQ
                     | GT
                     | GEQ'''
    p[0] = p[1]

  def p_simple_expression(self, p):
    '''simple_expression : term
                        | simple_expression PLUS term
                        | simple_expression MINUS term
                        | simple_expression OR term'''
    if len(p) == 2:
      p[0] = p[1]
    else:
      left = p[1]
      op = p[2]
      right = p[3]

      if op in ('+', '-'):
        if left.type == symbol.Type.INTEGER and right.type == symbol.Type.INTEGER:
          node = ast.BinaryOp(op, left, right, p.lineno(2))
          node.type = symbol.Type.INTEGER
          p[0] = node
        else:
          self.semanticService.print_error("Operandos de expressão aritmética devem ser inteiros", p.lineno(2))
          p[0] = ast.ErrorNode(p.lineno(1))

      else:
        if left.type == symbol.Type.BOOLEAN and right.type == symbol.Type.BOOLEAN:
          node = ast.BinaryOp(op, left, right, p.lineno(2))
          node.type = symbol.Type.BOOLEAN
          p[0] = node
        else:
          self.semanticService.print_error("Operandos de expressão lógica devem ser booleanos", p.lineno(2))
          p[0] = ast.ErrorNode(p.lineno(1))

  def p_term(self, p):
    '''term : factor
            | term MULT factor
            | term DIV factor
            | term AND factor'''
    if len(p) == 2:
      p[0] = p[1]
    else:
      left = p[1]
      op = p[2]
      right = p[3]

      if op in ('*', 'div'):
        if left.type == symbol.Type.INTEGER and right.type == symbol.Type.INTEGER:
          node = ast.BinaryOp(op, left, right, p.lineno(2))
          node.type = symbol.Type.INTEGER
          p[0] = node
        else:
          self.semanticService.print_error("Operandos de expressão aritmética devem ser inteiros", p.lineno(2))
          p[0] = ast.ErrorNode(p.lineno(1))

      else:
        if left.type == symbol.Type.BOOLEAN and right.type == symbol.Type.BOOLEAN:
          node = ast.BinaryOp(op, left, right, p.lineno(2))
          node.type = symbol.Type.BOOLEAN
          p[0] = node
        else:
          self.semanticService.print_error("Operandos de expressão lógica devem ser booleanos", p.lineno(2))
          p[0] = ast.ErrorNode(p.lineno(1))

  def p_factor(self, p):
    '''factor : variable
              | number
              | boolean
              | function_call
              | LPAREN expression RPAREN
              | NOT factor
              | MINUS factor'''
    if len(p) == 2:
      p[0] = p[1]
    elif len(p) == 4:
      p[0] = p[2]
    else:
      op = p[1]
      operand = p[2]

      if op == 'not':
        if operand.type == symbol.Type.BOOLEAN:
          node = ast.UnaryOp(op, operand, p.lineno(1))
          node.type = symbol.Type.BOOLEAN
          p[0] = node
        else:
          self.semanticService.print_error("Operandos de expressão lógica devem ser booleanos", p.lineno(1))
          p[0] = ast.ErrorNode(p.lineno(1))

      else:
        if operand.type == symbol.Type.INTEGER:
          node = ast.UnaryOp(op, operand, p.lineno(1))
          node.type = symbol.Type.INTEGER
          p[0] = node
        else:
          self.semanticService.print_error("Operandos de expressão aritmética devem ser inteiros", p.lineno(1))
          p[0] = ast.ErrorNode(p.lineno(1))

  def p_variable(self, p):
    '''variable : identifier'''
    identifier = p[1]
    key = self.semanticService.build_key(identifier)
    if key in self.semanticService.symbolsTable:
      var_type = self.semanticService.symbolsTable[key]["type"]
      p[0] = ast.Variable(identifier, p.lineno(1), var_type)
    else:
      self.semanticService.print_error(f"Variável '{identifier}' não declarada", p.lineno(1))
      p[0] = ast.ErrorNode(p.lineno(1))

  def p_boolean(self, p):
    '''boolean : FALSE
               | TRUE'''
    p[0] = ast.BooleanLiteral(value=p[1], line=p.lineno(1))

  def p_function_call(self, p):
    '''function_call : identifier LPAREN expression_list RPAREN
                     | identifier LPAREN RPAREN'''
    hasArgs = len(p) == 5
    identifier = p[1]
    key = self.semanticService.build_key(identifier)

    if key not in self.semanticService.symbolsTable:
      self.semanticService.print_error(f"Função '{identifier}' não foi declarada", p.lineno(1))
      p[0] = ast.ErrorNode(p.lineno(1))
      return

    returnType = self.semanticService.symbolsTable[key].get("type")
    if returnType is None:
      self.semanticService.print_error(f"Identificador '{identifier}' não possui tipo associado", p.lineno(1))
      p[0] = ast.ErrorNode(p.lineno(1))
      return

    if hasArgs:
      args = p[3]
      expectedCount = self.semanticService.symbolsTable[key]["param_count"]
      expectedTypes = self.semanticService.symbolsTable[key]["param_types"]

      if len(args) != expectedCount:
        self.semanticService.print_error(f"Número de parâmetros de {identifier} deve ser igual a {expectedCount}", p.lineno(2))
        p[0] = ast.ErrorNode(p.lineno(1))
        return

      for i, arg in enumerate(args):
        if arg.type != expectedTypes[i]:
          self.semanticService.print_error(f"Tipo do parâmetro {i+1} não corresponde", p.lineno(2))
          p[0] = ast.ErrorNode(p.lineno(1))
          return

      p[0] = ast.FunctionCall(identifier, args, p.lineno(1), returnType)
    else:
      p[0] = ast.FunctionCall(identifier, [], p.lineno(1), returnType)

  def p_identifier(self, p):
    '''identifier : ID'''
    p[0] = p[1]

  def p_number(self, p):
    '''number : NUMBER'''
    p[0] = ast.NumberLiteral(value=p[1], line=p.lineno(1))
  
  def _emit_op_expected(self, op, expected, lineno):
    print(f"Operador '{op}' deve ser seguido de um <{expected}>. Linha {lineno}.")

  def p_simple_expression_plus_error(self, p):
    '''simple_expression : simple_expression PLUS error term'''
    self._emit_op_expected('+', 'termo', p.lineno(3))

  def p_simple_expression_minus_error(self, p):
    '''simple_expression : simple_expression MINUS error term'''
    self._emit_op_expected('-', 'termo', p.lineno(3))

  def p_simple_expression_or_error(self, p):
    '''simple_expression : simple_expression OR error term'''
    self._emit_op_expected('or', 'termo', p.lineno(3))

  def p_term_mult_error(self, p):
    '''term : term MULT error factor'''
    self._emit_op_expected('*', 'fator', p.lineno(3))
     
  def p_term_div_error(self, p):
    '''term : term DIV error factor'''
    self._emit_op_expected('/', 'fator', p.lineno(3))

  def p_term_and_error(self, p):
    '''term : term AND error factor'''
    self._emit_op_expected('and', 'fator', p.lineno(3))

  def p_block_double_var_error(self, p):
    '''var_declaration_section : VAR error SEMICOLON'''
    print(f"Palavra-chave 'var' inesperada. "
          f"A gramática só permite uma <var_declaration_section>. "
          f"Linha {p.lineno(2)}.")
    
    p[0] = p[1]

  def p_nested_subroutine(self, p):
    '''subroutine_block : var_declaration_section FUNCTION
                        | var_declaration_section PROCEDURE'''
    print(f"Erro sintático: token inesperado '{p[2]}' (tipo {p.slice[2].type}) na linha {p.lineno(2)}")
    print(f"Subrotinas aninhadas não são permitidas. Linha {p.lineno(2)}")

  def p_trailing_semicolon(self, p):
    '''compound_statement : BEGIN statement_list SEMICOLON END'''
    print(f"Erro sintático: token inesperado '{p[4]}' (tipo {p.slice[4].type}) na linha {p.lineno(4)}")
    print(f"Token 'end' inesperado. Não deveria haver o ';' no último comando. Linha {p.lineno(4)}")
  
  def p_no_parameters_subroutine(self, p):
    '''formal_parameters : LPAREN RPAREN'''
    print(f"Erro sintático: token inesperado '{p[2]}' (tipo {p.slice[2].type}) na linha {p.lineno(2)}")
    print(f"Não deveria haver o '()' em subrotinas sem parâmetros. Linha {p.lineno(2)}")

  def p_error(self, p):
    if p:
      print(f"Erro sintático: token inesperado '{p.value}' (tipo {p.type}) na linha {p.lineno}")
    else:
      print("Erro sintático: fim inesperado do arquivo (EOF)")
      print(f"Parser esperava o token '.' para finalizar o programa. Linha {self.lexer.lexer.lineno}")

  def parse(self, source: str) -> ast.ASTNode:
    return self.parser.parse(source, lexer=self.lexer.lexer)
