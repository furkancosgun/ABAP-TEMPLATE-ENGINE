CLASS zcl_token_types DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:ty_token_type TYPE string.
    TYPES:ty_token_types TYPE TABLE OF ty_token_type WITH EMPTY KEY.
    "Tokens
    CONSTANTS:
      c_for      TYPE ty_token_type VALUE 'for',
      c_endfor   TYPE ty_token_type VALUE 'endfor',
      c_if       TYPE ty_token_type VALUE 'if',
      c_endif    TYPE ty_token_type VALUE 'endif',
      c_ifn      TYPE ty_token_type VALUE 'ifn',
      c_endifn   TYPE ty_token_type VALUE 'endifn',
      c_partial  TYPE ty_token_type VALUE '@',
      c_variable TYPE ty_token_type VALUE 'variable',
      c_text     TYPE ty_token_type VALUE 'text',
      c_do       TYPE ty_token_type VALUE 'do',
      c_enddo    TYPE ty_token_type VALUE 'enddo'.
    "Flags
    CONSTANTS:
      c_open  TYPE ty_token_type VALUE '<%',
      c_close TYPE ty_token_type VALUE '%>'.
ENDCLASS.



CLASS zcl_token_types IMPLEMENTATION.
ENDCLASS.
