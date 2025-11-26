from enum import Enum, auto
from typing import TypedDict

class Type(Enum):
  INTEGER = auto()
  BOOLEAN = auto()

class Category(Enum):
  PROGRAM = auto()
  VARIABLE = auto()
  FUNCTION = auto()
  PROCEDURE = auto()
  PARAMETER = auto()

class Scope(Enum):
  GLOBAL = auto()
  SUBROUTINE = auto()

class Symbol(TypedDict, total=False):
  type: Type
  category: Category
  scope: Scope

# Syntax Directed Translation
# Type (e.g. integer, bool)
# Category (e.g. program, variable, function, procedure, parameter)
# Scope (e.g. global, subroutine)

# Programa:
# Sequência de comandos mandatória
# Escopo Global
# Em Subrotinas, há o Escopo Local
# Inserir identificador do programa na tabela de símbolos, com a categoria "programa"

# Declaração:
# Variáveis, funções, procedimentos e parâmetros
# Verificar se existe no escopo atual: se sim erro senão adiciona

# Comandos:
# 
# 
# 
# 
# 
