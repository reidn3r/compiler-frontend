class AbstractLexer:
  reserved = {
    "program": "PROGRAM",
    "var": "VAR", 
    "procedure": "PROCEDURE",
    "function": "FUNCTION",
    "begin": "BEGIN",
    "end": "END",
    "false": "FALSE",
    "true": "TRUE",
    "if": "IF",
    "then": "THEN",
    "else": "ELSE",
    "while": "WHILE",
    "do": "DO",
    "read": "READ",
    "write": "WRITE",
    "and": "AND",
    "or": "OR",
    "not": "NOT",
    "div": "DIV"
  }

  tokens = list(reserved.values()) + [
    'ID','INTEGER', 'LPAREN', 'RPAREN', 'DOT',
    'COMMA', 'SEMICOLON', 'PLUS', 'MINUS', 'MULT',
    'ASSIGN', 'NEQ', 'GT', 'LT', 'GEQ', 'LEQ', 'COLON'
  ]

  def __init__(self):
    self.symbols = [
      "(", ")", ".", ",", ";", 
      "+", "-", "*", "=", "<>",
      ">", "<", ">=", "<=", ":=", ":"
    ]
