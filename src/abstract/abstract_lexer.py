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
    "div": "DIV",
    "false": "FALSE",
    "true": "TRUE",
    "boolean": "BOOLEAN",
    "integer": "INTEGER",
  }

  tokens = list(reserved.values()) + [
    'ID','NUMBER', 'LPAREN', 'RPAREN', 'DOT',
    'COMMA', 'SEMICOLON', 'PLUS', 'MINUS', 'MULT',
    'ASSIGN', 'EQ', 'NEQ', 'GT', 'LT', 'GEQ', 'LEQ', 'COLON'
  ]

  def __init__(self):
    self.symbols = [
      "(", ")", ".", ",", ";", 
      "+", "-", "*", "=", "<>",
      ">", "<", ">=", "<=", ":=", ":"
    ]
