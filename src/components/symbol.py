from enum import Enum, auto
from typing import List, TypedDict, Union

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

class VariableSymbol(TypedDict):
  id: str
  type: Type
  category: Category
  scope: str
  address: int

class SubroutineSymbol(TypedDict):
  id: str
  type: Type | None
  category: Category
  scope: str
  address: int
  param_types: List[Type]
  param_count: int

Symbol = Union[VariableSymbol, SubroutineSymbol]
