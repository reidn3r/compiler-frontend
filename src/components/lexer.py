from src.abstract.abstract_lexer import AbstractLexer
import ply.lex as lex

class LexicalError(Exception):
    pass

class Lexer(AbstractLexer):
  def __init__(self):
    super().__init__()
    self.lexer = lex.lex(module=self)

  # REGRAS PARA SÍMBOLOS
  t_LPAREN = r'\('
  t_RPAREN = r'\)'
  t_DOT = r'\.'
  t_COMMA = r','
  t_SEMICOLON = r';'
  t_PLUS = r'\+'
  t_MINUS = r'-'
  t_MULT = r'\*'
  t_ASSIGN = r':='
  t_EQ = r'='
  t_NEQ = r'<>'
  t_GT = r'>'
  t_LT = r'<'
  t_GEQ = r'>='
  t_LEQ = r'<='
  t_COLON = r':'
  t_FALSE = r'false'
  t_TRUE = r'true'
  t_BOOLEAN = r'boolean'
  t_INTEGER = r'integer'
  
  def t_ID(self, t):
    r'[a-zA-Z][a-zA-Z_0-9]*'
    t.type = self.reserved.get(t.value, 'ID')  
    return t

  def t_NUMBER(self, t):
    r'\d+'
    t.value = int(t.value) 
    return t
  
  t_ignore = ' \t'

  def t_newline(self, t):
    r'\n+'
    t.lexer.lineno += len(t.value)

  def t_error(self, t):
    print(f"Caractere inválido '{t.value[0]}' na linha {t.lineno}")
    # t.lexer.skip(1)
    raise LexicalError(f"Caractere inválido '{t.value[0]}' na linha {t.lineno}")
  
  def tokenize(self, buffer: str):
    self.lexer.input(buffer)
    tokens = []
    while True:
      tok = self.lexer.token()
      if not tok:
        break
      tokens.append(tok)
    return tokens
