import ply.yacc as yacc
from src.components.lexer import Lexer

class Parser:
  def __init__(self):
    self.lexer = Lexer()
    self.tokens = self.lexer.tokens
    self.parser = yacc.yacc(module=self)

  def p_program(self, p):
    '''program : PROGRAM identifier SEMICOLON block DOT'''
    p[0] = ('program', p[2], p[4])

  def p_block(self, p):
    '''block : var_declaration_section subroutine_declaration_section compound_statement
             | var_declaration_section compound_statement
             | subroutine_declaration_section compound_statement
             | compound_statement'''
    if len(p) == 4:
        p[0] = ('block', p[1], p[2], p[3])
    elif len(p) == 3:
        p[0] = ('block', p[1], p[2])
    else:
        p[0] = ('block', p[1])

  def p_var_declaration_section(self, p):
    '''var_declaration_section : VAR var_declaration_list'''
    p[0] = ('var_section', p[2])

  def p_var_declaration_list(self, p):
    '''var_declaration_list : var_declaration SEMICOLON
                            | var_declaration_list var_declaration SEMICOLON'''
    if len(p) == 3:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

  def p_var_declaration(self, p):
    '''var_declaration : identifier_list COLON type'''
    p[0] = ('var_decl', p[1], p[3])

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
    p[0] = ('subroutine_section', p[1])

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
        p[0] = ('procedure', p[2], p[3], p[5])
    else:
        p[0] = ('procedure', p[2], None, p[4])

  def p_function_declaration(self, p):
    '''function_declaration : FUNCTION identifier formal_parameters COLON type SEMICOLON subroutine_block
                              | FUNCTION identifier COLON type SEMICOLON subroutine_block'''
    if len(p) == 8:
        p[0] = ('function', p[2], p[3], p[5], p[7])
    else:
        p[0] = ('function', p[2], None, p[4], p[6])

  def p_subroutine_block(self, p):
    '''subroutine_block : var_declaration_section compound_statement
                        | compound_statement'''
    if len(p) == 3:
        p[0] = ('subroutine_block', p[1], p[2])
    else:
        p[0] = ('subroutine_block', p[1])

  def p_formal_parameters(self, p):
    '''formal_parameters : LPAREN parameter_declaration_list RPAREN'''
    p[0] = ('parameters', p[2])

  def p_parameter_declaration_list(self, p):
    '''parameter_declaration_list : parameter_declaration
                                  | parameter_declaration_list SEMICOLON parameter_declaration'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

  def p_parameter_declaration(self, p):
    '''parameter_declaration : identifier_list COLON type'''
    p[0] = ('parameter', p[1], p[3])

  def p_compound_statement(self, p):
    '''compound_statement : BEGIN statement_list END'''
    p[0] = ('begin_end', p[2])

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
    p[0] = ('assign', p[1], p[3])

  def p_procedure_call(self, p):
    '''procedure_call : identifier LPAREN expression_list RPAREN
                      | identifier'''
    if len(p) == 5:
        p[0] = ('proc_call', p[1], p[3])
    else:
        p[0] = ('proc_call', p[1], None)

  def p_conditional(self, p):
    '''conditional : IF expression THEN statement ELSE statement
                   | IF expression THEN statement'''
    if len(p) == 7:
        p[0] = ('if', p[2], p[4], p[6])
    else:
        p[0] = ('if', p[2], p[4], None)

  def p_repetition(self, p):
    '''repetition : WHILE expression DO statement'''
    p[0] = ('while', p[2], p[4])

  def p_read_statement(self, p):
    '''read_statement : READ LPAREN identifier_list RPAREN'''
    p[0] = ('read', p[3])

  def p_write_statement(self, p):
    '''write_statement : WRITE LPAREN expression_list RPAREN'''
    p[0] = ('write', p[3])

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
        p[0] = ('binop', p[2], p[1], p[3])
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
        p[0] = ('binop', p[2], p[1], p[3])

  def p_term(self, p):
    '''term : factor
            | term MULT factor
            | term DIV factor
            | term AND factor'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('binop', p[2], p[1], p[3])

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
        p[0] = ('unop', p[1], p[2])
    else:
        p[0] = p[2]

  def p_variable(self, p):
    '''variable : identifier'''
    p[0] = ('var', p[1])

  def p_boolean(self, p):
    '''boolean : FALSE
               | TRUE'''
    p[0] = ('bool', p[1])

  def p_function_call(self, p):
    '''function_call : identifier LPAREN expression_list RPAREN'''
    p[0] = ('func_call', p[1], p[3])

  def p_identifier(self, p):
    '''identifier : ID'''
    p[0] = p[1]

  def p_number(self, p):
    '''number : NUMBER'''
    p[0] = ('num', p[1])

  def p_error(self, p):
    if p:
        print(f"Syntax error at token {p.type} ('{p.value}') at line {p.lineno}")
    else:
        print("Syntax error at EOF")

  def parse(self, source: str):
      return self.parser.parse(source, lexer=self.lexer.lexer)
