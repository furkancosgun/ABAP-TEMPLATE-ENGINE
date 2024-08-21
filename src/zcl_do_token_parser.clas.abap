CLASS zcl_do_token_parser DEFINITION
INHERITING FROM zcl_token_parser
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: c_regex TYPE string VALUE 'do\s+(.*)'.
    METHODS parse REDEFINITION.
ENDCLASS.



CLASS zcl_do_token_parser IMPLEMENTATION.
  METHOD parse.
    FIND ALL OCCURRENCES OF REGEX c_regex IN token IGNORING CASE SUBMATCHES DATA(count).
    IF sy-subrc EQ 0.
      io_tree->add_node( node = VALUE #( ( zcl_token_types=>c_do ) ( count ) ) ).
    ELSE.
      me->append_text( io_tree = io_tree template = template start_pos = start_pos end_pos = end_pos ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
