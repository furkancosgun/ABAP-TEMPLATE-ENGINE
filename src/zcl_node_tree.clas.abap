CLASS zcl_node_tree DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      ty_string_tab TYPE TABLE OF string WITH EMPTY KEY,
      ty_node_type  TYPE ty_string_tab,
      ty_tree_table TYPE TABLE OF ty_node_type WITH EMPTY KEY.

    METHODS:
      add_node
        IMPORTING
          node TYPE ty_node_type OPTIONAL.

    METHODS:
      get_tree
        RETURNING
          VALUE(rt_tree) TYPE ty_tree_table.

    METHODS:
      get_node
        IMPORTING
          index          TYPE i
        RETURNING
          VALUE(rt_node) TYPE ty_node_type.

    DATA:mt_tree TYPE ty_tree_table.
ENDCLASS.



CLASS zcl_node_tree IMPLEMENTATION.
  METHOD:add_node.
    APPEND INITIAL LINE TO mt_tree ASSIGNING FIELD-SYMBOL(<fs_node>).
    APPEND LINES OF node TO <fs_node>.
  ENDMETHOD.
  METHOD:get_tree.
    rt_tree = mt_tree.
  ENDMETHOD.
  METHOD:get_node.
    READ TABLE mt_tree INDEX index INTO rt_node.
  ENDMETHOD.
ENDCLASS.
