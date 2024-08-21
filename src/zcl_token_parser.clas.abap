CLASS zcl_token_parser DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .


  PUBLIC SECTION.
    TYPES: ty_string_table TYPE TABLE OF string WITH EMPTY KEY.
    TYPES: ty_tree_type TYPE ty_string_table.
    TYPES: ty_tree_table TYPE TABLE OF ty_tree_type WITH EMPTY KEY.
    METHODS:
      parse ABSTRACT
        IMPORTING
          token     TYPE string
          start_pos TYPE i
          end_pos   TYPE i
          io_tree   TYPE REF TO zcl_node_tree
          template  TYPE string.
    METHODS:
      append_text
        IMPORTING
          io_tree   TYPE REF TO zcl_node_tree
          template  TYPE string
          start_pos TYPE i
          end_pos   TYPE i.
ENDCLASS.



CLASS zcl_token_parser IMPLEMENTATION.
  METHOD append_text.
    DATA(lv_text) = substring(
      val = template
      off = start_pos
      len = end_pos - start_pos + strlen( zcl_token_types=>c_close )
    ).
    io_tree->add_node( node = VALUE #( ( zcl_token_types=>c_text ) ( lv_text ) ) ).
  ENDMETHOD.
ENDCLASS.
