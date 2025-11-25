import ply.yacc as yacc
from src.components.lexer import Lexer
import src.components.ast as ast

class Parser:
  def __init__(self):
    self.lexer = Lexer()
    self.tokens = self.lexer.tokens
    self.parser = yacc.yacc(module=self)

  def p_program(self, p):
    '''program : PROGRAM identifier SEMICOLON block DOT'''
    p[0] = (ast.PROGRAM, p[2], p[4])

  def p_block(self, p):
    '''block : var_declaration_section subroutine_declaration_section compound_statement
             | var_declaration_section compound_statement
             | subroutine_declaration_section compound_statement
             | compound_statement'''
    if len(p) == 4:
        p[0] = (ast.BLOCK, p[1], p[2], p[3])
    elif len(p) == 3:
        p[0] = (ast.BLOCK, p[1], p[2])
    else:
        p[0] = (ast.BLOCK, p[1])

  def p_var_declaration_section(self, p):
    '''var_declaration_section : VAR var_declaration_list'''
    p[0] = (ast.VAR_SECTION, p[2])

  def p_var_declaration_list(self, p):
    '''var_declaration_list : var_declaration SEMICOLON
                            | var_declaration_list var_declaration SEMICOLON'''
    if len(p) == 3:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

  def p_var_declaration(self, p):
    '''var_declaration : identifier_list COLON type'''
    p[0] = (ast.VAR_DECL, p[1], p[3])

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
    p[0] = (ast.SUBROUTINE_SECTION, p[1])

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
    '''procedure_declaration : PROCEDURE identifier formal_parameters SEMICOLON subroutine_block
                             | PROCEDURE identifier SEMICOLON subroutine_block'''
    if len(p) == 6:
        p[0] = (ast.PROCEDURE, p[2], p[3], p[5])
    else:
        p[0] = (ast.PROCEDURE, p[2], None, p[4])

  def p_function_declaration(self, p):
    '''function_declaration : FUNCTION identifier formal_parameters COLON type SEMICOLON subroutine_block
                            | FUNCTION identifier COLON type SEMICOLON subroutine_block'''
    if len(p) == 8:
        p[0] = (ast.FUNCTION, p[2], p[3], p[5], p[7])
    else:
        p[0] = (ast.FUNCTION, p[2], None, p[4], p[6])

  def p_subroutine_block(self, p):
    '''subroutine_block : var_declaration_section compound_statement
                        | compound_statement'''
    if len(p) == 3:
        p[0] = (ast.SUBROUTINE_BLOCK, p[1], p[2])
    else:
        p[0] = (ast.SUBROUTINE_BLOCK, p[1])

  def p_formal_parameters(self, p):
    '''formal_parameters : LPAREN parameter_declaration_list RPAREN'''
    p[0] = (ast.PARAMETERS, p[2])

  def p_parameter_declaration_list(self, p):
    '''parameter_declaration_list : parameter_declaration
                                  | parameter_declaration_list SEMICOLON parameter_declaration'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

  def p_parameter_declaration(self, p):
    '''parameter_declaration : identifier_list COLON type'''
    p[0] = (ast.PARAMETER, p[1], p[3])

  def p_compound_statement(self, p):
    '''compound_statement : BEGIN statement_list END'''
    p[0] = (ast.COMMAND_SEQ, p[2])

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
    p[0] = (ast.ASSIGN, p[1], p[3])

  def p_procedure_call(self, p):
    '''procedure_call : identifier LPAREN expression_list RPAREN
                      | identifier LPAREN RPAREN'''
    if len(p) == 5:
        p[0] = (ast.PROC_CALL, p[1], p[3])
    else:
        p[0] = (ast.PROC_CALL, p[1], None)

  def p_conditional(self, p):
    '''conditional : IF expression THEN statement ELSE statement
                   | IF expression THEN statement'''
    if len(p) == 7:
        p[0] = (ast.IF, p[2], p[4], p[6])
    else:
        p[0] = (ast.IF, p[2], p[4], None)

  def p_repetition(self, p):
    '''repetition : WHILE expression DO statement'''
    p[0] = (ast.WHILE, p[2], p[4])

  def p_read_statement(self, p):
    '''read_statement : READ LPAREN identifier_list RPAREN'''
    p[0] = (ast.READ, p[3])

  def p_write_statement(self, p):
    '''write_statement : WRITE LPAREN expression_list RPAREN'''
    p[0] = (ast.WRITE, p[3])

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
        p[0] = (ast.BINARY_OP, p[1], p[2], p[3])
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
        p[0] = (ast.BINARY_OP, p[1], p[2], p[3])

  def p_term(self, p):
    '''term : factor
            | term MULT factor
            | term DIV factor
            | term AND factor'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = (ast.BINARY_OP, p[1], p[2], p[3])

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
    elif len(p) == 3:
        p[0] = (ast.UNARY_OP, p[1], p[2])
    else:
        p[0] = p[2]

  def p_variable(self, p):
    '''variable : identifier'''
    p[0] = (ast.VAR, p[1])

  def p_boolean(self, p):
    '''boolean : FALSE
               | TRUE'''
    p[0] = (ast.BOOL, p[1])

  def p_function_call(self, p):
    '''function_call : identifier LPAREN expression_list RPAREN
                     | identifier LPAREN RPAREN'''
    if len(p) == 5:
      p[0] = (ast.FUNC_CALL, p[1], p[3])
    else:
      p[0] = (ast.FUNC_CALL, p[1], None)

  def p_identifier(self, p):
    '''identifier : ID'''
    p[0] = p[1]

  def p_number(self, p):
    '''number : NUMBER'''
    p[0] = (ast.NUM, p[1])
  
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
    '''var_declaration_section : VAR error'''
    print(f"Palavra-chave 'var' inesperada. "
          f"A gramática só permite uma <var_declaration_section>. "
          f"Linha {p.lineno(2)}.")
    
    p[0] = p[1]

  def p_nested_subroutine(self, p):
    '''subroutine_block : var_declaration_section FUNCTION
                        | var_declaration_section PROCEDURE'''
    print(f"Erro sintático: token inesperado '{p[2]}' (tipo {p.slice[2].type}) na linha {p.lineno(2)}")
    print(f"Subrotinas aninhadas não são permitidas. Linha {p.lineno(2)}")
    raise SyntaxError

  def p_trailing_semicolon(self, p):
    '''compound_statement : BEGIN statement_list SEMICOLON END'''
    print(f"Erro sintático: token inesperado '{p[4]}' (tipo {p.slice[4].type}) na linha {p.lineno(4)}")
    print(f"Token 'end' inesperado. Não deveria haver o ';' no último comando. Linha {p.lineno(4)}")
    raise SyntaxError
  
  def p_no_parameters_subroutine(self, p):
    '''formal_parameters : LPAREN RPAREN'''
    print(f"Erro sintático: token inesperado '{p[2]}' (tipo {p.slice[2].type}) na linha {p.lineno(2)}")
    print(f"Não deveria haver o '()' em subrotinas sem parâmetros. Linha {p.lineno(2)}")
    raise SyntaxError

  def p_error(self, p):
    if p:
      print(f"Erro sintático: token inesperado '{p.value}' (tipo {p.type}) na linha {p.lineno}")
    else:
      print("Erro sintático: fim inesperado do arquivo (EOF)")
      print(f"Parser esperava o token '.' para finalizar o programa. Linha {self.lexer.lexer.lineno}")

  def parse(self, source: str):
    return self.parser.parse(source, lexer=self.lexer.lexer)
