from enum import Enum, auto
from typing import TypedDict

class Type(Enum):
  INTEGER = 'integer'
  BOOLEAN = 'boolean'

class Category(Enum):
  PROGRAM = auto()
  VARIABLE = auto()
  FUNCTION = auto()
  PROCEDURE = auto()
  PARAMETER = auto()

GLOBAL_SCOPE = "global"

class Symbol(TypedDict, total=False):
  id: str
  type: Type
  category: Category
  scope: str
