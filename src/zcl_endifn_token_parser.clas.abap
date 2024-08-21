CLASS zcl_endifn_token_parser DEFINITION
INHERITING FROM zcl_token_parser
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS parse REDEFINITION.
ENDCLASS.



CLASS zcl_endifn_token_parser IMPLEMENTATION.
  METHOD parse.
    io_tree->add_node( node = VALUE #( ( zcl_token_types=>c_endifn ) ) ).
  ENDMETHOD.
ENDCLASS.
