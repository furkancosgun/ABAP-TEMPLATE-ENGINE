CLASS zcl_partial_token_parser DEFINITION
INHERITING FROM zcl_token_parser
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS parse REDEFINITION.
    CONSTANTS: c_regex TYPE string VALUE '@(?:\s*)?(\w+)'.
ENDCLASS.



CLASS zcl_partial_token_parser IMPLEMENTATION.
  METHOD parse.
    FIND ALL OCCURRENCES OF REGEX c_regex IN token IGNORING CASE SUBMATCHES DATA(partial_name).
    IF sy-subrc EQ 0.
      io_tree->add_node( node = VALUE #( ( zcl_token_types=>c_partial ) ( partial_name ) ) ).
    ELSE.
      me->append_text( io_tree = io_tree template = template start_pos = start_pos end_pos = end_pos ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
