CLASS zcl_template_engine DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.

    CLASS-METHODS:
      create
        IMPORTING
          template           TYPE string
        RETURNING
          VALUE(ro_instance) TYPE REF TO zcl_template_engine.

    METHODS:
      render
        IMPORTING
          VALUE(context)    TYPE any OPTIONAL
        RETURNING
          VALUE(r_template) TYPE string.

  PRIVATE SECTION.
    METHODS:
      parse_template
        RETURNING
          VALUE(ro_tree) TYPE REF TO zcl_node_tree.
    METHODS:
      render_template
        IMPORTING
          io_tree           TYPE REF TO zcl_node_tree
        CHANGING
          context           TYPE any
        RETURNING
          VALUE(r_template) TYPE string.
    METHODS:
      identify_token
        IMPORTING
          token          TYPE string
        RETURNING
          VALUE(rv_type) TYPE string.
    METHODS:
      starts_with
        IMPORTING
          value             TYPE string
          with              TYPE zcl_token_types=>ty_token_type
        RETURNING
          VALUE(rv_boolean) TYPE boolean.
    METHODS:
      extract_body
        IMPORTING
          io_tree     TYPE REF TO zcl_node_tree
          start_index TYPE i
          start_token TYPE zcl_token_types=>ty_token_type
          end_token   TYPE zcl_token_types=>ty_token_type
        EXPORTING
          eo_tree     TYPE REF TO zcl_node_tree
          end_index   TYPE i.
    METHODS:
      render_variable
        IMPORTING
          value     TYPE any
          fieldname TYPE string
        CHANGING
          output    TYPE zcl_token_parser=>ty_tree_type.

    TYPES:BEGIN OF mty_type_cache,
            fieldname TYPE string,
            typedescr TYPE REF TO cl_abap_typedescr,
            funcname  TYPE funcname,
          END OF mty_type_cache.

    TYPES: BEGIN OF mty_token_parsers,
             token_type TYPE string,
             parser     TYPE REF TO zcl_token_parser,
           END OF mty_token_parsers.

    CONSTANTS:mc_max_partial_size TYPE i VALUE 10.

    CLASS-DATA:token_parsers TYPE HASHED TABLE OF mty_token_parsers WITH UNIQUE KEY token_type.
    CLASS-DATA:current_partial_size TYPE i.
    DATA:mt_type_cache TYPE HASHED TABLE OF mty_type_cache WITH UNIQUE KEY fieldname.
    DATA:template      TYPE string.
ENDCLASS.

CLASS zcl_template_engine IMPLEMENTATION.
  METHOD:class_constructor.
    token_parsers = VALUE #(
      ( token_type = zcl_token_types=>c_for        parser = NEW zcl_for_token_parser( ) )
      ( token_type = zcl_token_types=>c_endfor     parser = NEW zcl_endfor_token_parser( ) )
      ( token_type = zcl_token_types=>c_if         parser = NEW zcl_if_token_parser( ) )
      ( token_type = zcl_token_types=>c_endif      parser = NEW zcl_endif_token_parser( ) )
      ( token_type = zcl_token_types=>c_ifn        parser = NEW zcl_ifn_token_parser( ) )
      ( token_type = zcl_token_types=>c_endifn     parser = NEW zcl_endifn_token_parser( ) )
      ( token_type = zcl_token_types=>c_partial    parser = NEW zcl_partial_token_parser( ) )
      ( token_type = zcl_token_types=>c_do         parser = NEW zcl_do_token_parser( ) )
      ( token_type = zcl_token_types=>c_enddo      parser = NEW zcl_enddo_token_parser( ) )
    ).
  ENDMETHOD.
  METHOD:create.
    ro_instance = NEW #( ).
    ro_instance->template = template.
  ENDMETHOD.
  METHOD render.
    r_template = me->render_template( EXPORTING io_tree = me->parse_template( ) CHANGING context = context ).
  ENDMETHOD.
  METHOD parse_template.
    DATA:
      pos       TYPE i,
      start_pos TYPE i,
      end_pos   TYPE i.
    DATA:
      token      TYPE string,
      token_type TYPE string.

    ro_tree = NEW #( ).

    WHILE pos <= strlen( me->template ).
      FIND FIRST OCCURRENCE OF zcl_token_types=>c_open IN substring( val = me->template off = pos ) MATCH OFFSET start_pos.
      IF sy-subrc NE 0.
        ro_tree->add_node( node = VALUE #( ( zcl_token_types=>c_text ) ( substring( val = me->template off = pos ) ) ) ).
        EXIT.
      ENDIF.

      ADD pos TO start_pos.

      FIND FIRST OCCURRENCE OF zcl_token_types=>c_close IN substring( val = me->template off = start_pos ) MATCH OFFSET end_pos.
      IF sy-subrc NE 0.
        ro_tree->add_node( node = VALUE #( ( zcl_token_types=>c_text ) ( substring( val = me->template off = pos ) ) ) ).
        EXIT.
      ENDIF.

      IF start_pos > pos.
        ro_tree->add_node( node = VALUE #( ( zcl_token_types=>c_text ) ( substring( val = me->template off = pos len = start_pos - pos ) ) ) ).
      ENDIF.

      token = substring( val = me->template off = start_pos + strlen( zcl_token_types=>c_open ) len = end_pos - strlen( zcl_token_types=>c_close ) ).

      CONDENSE token.

      token_type = me->identify_token( token = token ).

      READ TABLE token_parsers WITH KEY token_type = token_type REFERENCE INTO DATA(lr_parser).
      IF sy-subrc EQ 0.
        lr_parser->parser->parse( token = token start_pos = start_pos end_pos = end_pos io_tree = ro_tree template = template ).
      ELSE.
        ro_tree->add_node( node = VALUE #( ( zcl_token_types=>c_variable ) ( token ) ) ).
      ENDIF.

      pos = start_pos + end_pos + strlen( zcl_token_types=>c_close ).
    ENDWHILE.
  ENDMETHOD.

  METHOD identify_token.
    DATA(lv_token) = to_lower( token ).
    CASE abap_true.
      WHEN starts_with( value = lv_token with = zcl_token_types=>c_for ).
        rv_type = zcl_token_types=>c_for.
      WHEN starts_with( value = lv_token with = zcl_token_types=>c_endfor ).
        rv_type = zcl_token_types=>c_endfor.
      WHEN starts_with( value = lv_token with = zcl_token_types=>c_ifn ).
        rv_type = zcl_token_types=>c_ifn.
      WHEN starts_with( value = lv_token with = zcl_token_types=>c_endifn ).
        rv_type = zcl_token_types=>c_endifn.
      WHEN starts_with( value = lv_token with = zcl_token_types=>c_if ).
        rv_type = zcl_token_types=>c_if.
      WHEN starts_with( value = lv_token with = zcl_token_types=>c_endif ).
        rv_type = zcl_token_types=>c_endif.
      WHEN starts_with( value = lv_token with = zcl_token_types=>c_partial ).
        rv_type = zcl_token_types=>c_partial.
      WHEN starts_with( value = lv_token with = zcl_token_types=>c_do ).
        rv_type = zcl_token_types=>c_do.
      WHEN starts_with( value = lv_token with = zcl_token_types=>c_enddo ).
        rv_type = zcl_token_types=>c_enddo.
      WHEN OTHERS.
        rv_type = zcl_token_types=>c_variable.
    ENDCASE.
  ENDMETHOD.

  METHOD starts_with.
    rv_boolean = boolc( value CP |{ with }*| ).
  ENDMETHOD.

  METHOD render_template.
    DATA:
      output         TYPE zcl_node_tree=>ty_node_type,
      extracted_body TYPE REF TO zcl_node_tree,
      index          TYPE i VALUE 1.

    FIELD-SYMBOLS:<fs_value> TYPE any,
                  <fs_table> TYPE STANDARD TABLE.

    WHILE index <= lines( io_tree->mt_tree ).
      DATA(node) = io_tree->get_node( index = index ).
      DATA(node_type) = VALUE #( node[ 1 ] OPTIONAL ).
      DATA(node_value) = VALUE #( node[ 2 ] OPTIONAL ).
      DATA(node_value_second) = VALUE #( node[ 3 ] OPTIONAL ).

      IF node_type NE zcl_token_types=>c_text.
        TRANSLATE node_value TO UPPER CASE.
        TRANSLATE node_value_second TO UPPER CASE.
      ENDIF.

      CASE node_type.
        WHEN zcl_token_types=>c_text.
          APPEND node_value TO output.
        WHEN zcl_token_types=>c_variable.
          ASSIGN COMPONENT node_value OF STRUCTURE context TO <fs_value>.
          IF sy-subrc NE 0.
            ASSIGN COMPONENT node_value OF STRUCTURE sy TO <fs_value>.
          ENDIF.
          IF <fs_value> IS ASSIGNED.
            me->render_variable(
              EXPORTING
                value     = <fs_value>
                fieldname = node_value
              CHANGING
                output    = output
            ).
          ENDIF.
        WHEN zcl_token_types=>c_for.
          me->extract_body(
            EXPORTING
              io_tree = io_tree
              start_index = index + 1
              start_token = zcl_token_types=>c_for
              end_token   = zcl_token_types=>c_endfor
            IMPORTING
              eo_tree     = extracted_body
              end_index   = index
          ).
          ASSIGN COMPONENT node_value_second OF STRUCTURE context TO <fs_table>.
          IF sy-subrc EQ 0.
            LOOP AT <fs_table> ASSIGNING <fs_value>.
              ASSIGN COMPONENT node_value OF STRUCTURE context TO FIELD-SYMBOL(<fs_field>).
              IF sy-subrc EQ 0.
                <fs_field> = <fs_value>.
                APPEND me->render_template( EXPORTING io_tree = extracted_body CHANGING context = context ) TO output.
              ENDIF.
            ENDLOOP.
          ENDIF.
        WHEN zcl_token_types=>c_if.
          me->extract_body(
            EXPORTING
              io_tree      = io_tree
              start_index  = index + 1
              start_token  = zcl_token_types=>c_if
              end_token    = zcl_token_types=>c_endif
            IMPORTING
              eo_tree     = extracted_body
              end_index   = index
          ).
          ASSIGN COMPONENT node_value OF STRUCTURE context TO <fs_value>.
          IF sy-subrc EQ 0.
            IF <fs_value> EQ abap_true.
              APPEND me->render_template( EXPORTING io_tree = extracted_body CHANGING context = context ) TO output.
            ENDIF.
          ENDIF.
        WHEN zcl_token_types=>c_ifn.
          me->extract_body(
            EXPORTING
              io_tree      = io_tree
              start_index  = index + 1
              start_token  = zcl_token_types=>c_ifn
              end_token    = zcl_token_types=>c_endifn
            IMPORTING
              eo_tree     = extracted_body
              end_index   = index
          ).
          ASSIGN COMPONENT node_value OF STRUCTURE context TO <fs_value>.
          IF sy-subrc EQ 0.
            IF <fs_value> EQ abap_false.
              APPEND me->render_template( EXPORTING io_tree = extracted_body CHANGING context = context ) TO output.
            ENDIF.
          ENDIF.
        WHEN zcl_token_types=>c_partial.
          IF current_partial_size NE mc_max_partial_size.
            ADD 1 TO current_partial_size.
            ASSIGN COMPONENT node_value OF STRUCTURE context TO <fs_value>.
            IF sy-subrc EQ 0 AND <fs_value> IS INSTANCE OF zcl_template_engine.
              APPEND CAST zcl_template_engine( <fs_value> )->render( context = context ) TO output.
            ENDIF.
          ENDIF.
        WHEN zcl_token_types=>c_do.
          me->extract_body(
            EXPORTING
              io_tree      = io_tree
              start_index  = index + 1
              start_token  = zcl_token_types=>c_do
              end_token    = zcl_token_types=>c_enddo
            IMPORTING
              eo_tree     = extracted_body
              end_index   = index
          ).
          ASSIGN COMPONENT node_value OF STRUCTURE context TO <fs_value>.
          IF sy-subrc NE 0.
            IF node_value CO '0123456789'.
              ASSIGN node_value TO <fs_value>.
            ENDIF.
          ENDIF.
          IF <fs_value> IS ASSIGNED.
            DO <fs_value> TIMES.
              APPEND me->render_template( EXPORTING io_tree = extracted_body CHANGING context = context ) TO output.
            ENDDO.
          ENDIF.
      ENDCASE.
      ADD 1 TO index.
    ENDWHILE.
    LOOP AT output INTO DATA(out).
      IF sy-tabix EQ 1.
        r_template = out.
      ELSE.
        r_template = |{ r_template }{ out }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD extract_body.
    DATA(level) = 1.

    end_index = start_index.

    eo_tree = NEW #( ).

    WHILE end_index <= lines( io_tree->mt_tree ).
      DATA(node) = io_tree->get_node( index = end_index ).
      DATA(node_type) = VALUE #( node[ 1 ] OPTIONAL ).

      CASE node_type.
        WHEN start_token.
          ADD 1 TO level.
        WHEN end_token.
          SUBTRACT 1 FROM level.
      ENDCASE.

      IF level EQ 0.
        EXIT.
      ENDIF.

      eo_tree->add_node( node = node ).

      ADD 1 TO end_index.
    ENDWHILE.
  ENDMETHOD.
  METHOD:render_variable.
    DATA:
      lv_value     TYPE c LENGTH 1024,
      lv_funcname  TYPE funcname,
      lo_typedescr TYPE REF TO cl_abap_typedescr.

    READ TABLE mt_type_cache WITH KEY fieldname = fieldname REFERENCE INTO DATA(lr_type_cache).
    IF sy-subrc = 0.
      lo_typedescr = lr_type_cache->typedescr.
      lv_funcname  = lr_type_cache->funcname.
    ELSE.
      lo_typedescr = cl_abap_typedescr=>describe_by_data( p_data = value ).
      lv_funcname  = CAST cl_abap_elemdescr( lo_typedescr )->edit_mask.

      IF lv_funcname IS NOT INITIAL.
        REPLACE '==' IN lv_funcname WITH space.
        CONDENSE lv_funcname NO-GAPS.
        lv_funcname = |CONVERSION_EXIT_{ lv_funcname }_OUTPUT|.
      ENDIF.

      INSERT VALUE #( fieldname = fieldname
                       typedescr = lo_typedescr
                       funcname  = lv_funcname ) INTO TABLE mt_type_cache.
    ENDIF.


    CHECK lo_typedescr IS BOUND.

    APPEND INITIAL LINE TO output ASSIGNING FIELD-SYMBOL(<fs_value>).

    lv_value = value.

    TRY.
        CASE lo_typedescr->type_kind.
          WHEN cl_abap_typedescr=>typekind_date.
            lv_value = |{ CONV d( value ) DATE = ENVIRONMENT }|.
          WHEN cl_abap_typedescr=>typekind_time.
            lv_value = |{ CONV t( value ) TIME = ENVIRONMENT }|.
          WHEN cl_abap_typedescr=>typekind_packed.
            WRITE value TO lv_value DECIMALS lo_typedescr->decimals.
          WHEN OTHERS.
            IF lv_funcname IS NOT INITIAL.
              CALL FUNCTION lv_funcname
                EXPORTING
                  input  = value
                IMPORTING
                  output = lv_value
                EXCEPTIONS
                  OTHERS = 1.
            ENDIF.
        ENDCASE.
      CATCH cx_root INTO DATA(e).
    ENDTRY.
    <fs_value> = lv_value.
  ENDMETHOD.
ENDCLASS.
