*&---------------------------------------------------------------------*
*& Report ZTEMPLATE_ENGINE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztemplate_engine_demo.

*--------------------------------------------------------------*
* 1. Conditional Rendering Example
*--------------------------------------------------------------*
FORM example_1.
* Define a template with conditional rendering
  DATA(template) = |<%if is_deleted %><p> Is Deleted = True </p><%endif%>| &&
                   |<%ifn is_deleted %><p> Is Deleted = False </p><%endifn%>|.

* Create an instance of the template engine with the defined template
  DATA(lo_template) = zcl_template_engine=>create( template = template ).

* Define the context structure
  TYPES: BEGIN OF lty_context,
           is_deleted TYPE boolean,
         END OF lty_context.

* Initialize context with data
  DATA: ls_context TYPE lty_context VALUE 'X'.

* Render the template with the context
  cl_demo_output=>display( lo_template->render( ls_context ) ).
ENDFORM.

*--------------------------------------------------------------*
* 2. Loop and Conditional Rendering Example
*--------------------------------------------------------------*
FORM example_2.
* Define a template with a loop and conditional rendering
  DATA(template) = |<% for item in items %>| &&
                   |<% if item-show %>| &&
                   |<p>Item: <%item-name%></p>{ cl_abap_char_utilities=>newline }| &&
                   |<% endif %>| &&
                   |<% endfor %>|.

* Define the structure for loop items
  TYPES: BEGIN OF lty_context_line,
           name TYPE string,
           show TYPE boolean,
         END OF lty_context_line.

* Define the main context structure
  TYPES: BEGIN OF lty_context,
           items TYPE TABLE OF lty_context_line WITH EMPTY KEY,
           item  TYPE lty_context_line, "item required
         END OF lty_context.

* Initialize context with data
  DATA: ls_context TYPE lty_context.
  ls_context-items = VALUE #( ( name = 'Item 1' show = abap_true )
                              ( name = 'Item 2' show = abap_false )
                              ( name = 'Item 3' show = abap_true ) ).

* Create an instance of the template engine with the defined template
  DATA(lo_template) = zcl_template_engine=>create( template = template ).

* Render the template with the context
  cl_demo_output=>display( lo_template->render( ls_context ) ).
ENDFORM.
*--------------------------------------------------------------*
* 3. Repeating Content Example
*--------------------------------------------------------------*
FORM example_3.
* Define a template to repeat content
  DATA(template) = |<% do 5 %><p>Hello World</p>{ cl_abap_char_utilities=>newline }<% enddo %>|.

* Create an instance of the template engine with the defined template
  DATA(lo_template) = zcl_template_engine=>create( template = template ).

* Render the template
  cl_demo_output=>display( lo_template->render( ) ).
ENDFORM.
*--------------------------------------------------------------*
* 4. Repeating Content with Context Example
*--------------------------------------------------------------*
FORM example_4.
* Define the context structure
  TYPES: BEGIN OF lty_context,
           count TYPE i,
         END OF lty_context.

* Initialize context with data
  DATA: ls_context TYPE lty_context.
  ls_context-count = 5.

* Define a template to repeat content based on context
  DATA(template) = |<% do count %><p>Hello World</p>{ cl_abap_char_utilities=>newline }<% enddo %>|.

* Create an instance of the template engine with the defined template
  DATA(lo_template) = zcl_template_engine=>create( template = template ).

* Render the template with the context
  cl_demo_output=>display( lo_template->render( ls_context ) ).
ENDFORM.

*--------------------------------------------------------------*
* 5. Partial Templates Example
*--------------------------------------------------------------*
FORM example_5.
* Define partial templates with more descriptive content
  DATA(lo_partial1) = zcl_template_engine=>create(
      |Greetings from Partial 1! Here is the name provided in the context: <%name%>|
  ).
  DATA(lo_partial2) = zcl_template_engine=>create(
      |Hello from Partial 2! The name in the context is: <%name%>|
  ).
  DATA(lo_partial3) = zcl_template_engine=>create(
      |Welcome from Partial 3! Name accessed from context: <%name%>|
  ).

* Define a main template that uses the partial templates
  DATA(lo_template) = zcl_template_engine=>create(
      |<p>Message from Partial 1: <%@partial1%></p>{ cl_abap_char_utilities=>newline }| &&
      |<p>Message from Partial 2: <%@partial2%></p>{ cl_abap_char_utilities=>newline }| &&
      |<p>Message from Partial 3: <%@partial3%></p>{ cl_abap_char_utilities=>newline }| &&
      |<p>This is the main layout. The name used here is: <%name%></p>|
  ).

* Define the context structure including partial templates
  TYPES: BEGIN OF lty_context,
           partial1 TYPE REF TO zcl_template_engine,
           partial2 TYPE REF TO zcl_template_engine,
           partial3 TYPE REF TO zcl_template_engine,
           name     TYPE string,
         END OF lty_context.

* Initialize context with data
  DATA: ls_context TYPE lty_context.
  ls_context-partial1 = lo_partial1.
  ls_context-partial2 = lo_partial2.
  ls_context-partial3 = lo_partial3.
  ls_context-name     = 'TEMPLATE_ENGINE'.

* Render the main template with the context
  cl_demo_output=>display( lo_template->render( ls_context ) ).
ENDFORM.

START-OF-SELECTION.
*  PERFORM example_1.
*  PERFORM example_2.
*  PERFORM example_3.
*  PERFORM example_4.
*  PERFORM example_5.
