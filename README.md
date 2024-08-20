# ABAP Template Engine

This project provides a flexible and powerful template engine for ABAP environments. It supports dynamic template rendering with conditional logic, loops, and partial templates. The following examples illustrate how to use various features of the template engine.

## License
This project is licensed under the [MIT License](LICENSE). See the LICENSE file for details.

## Contents
- [Conditional Rendering Example](#conditional-rendering-example)
- [Loop and Conditional Rendering Example](#loop-and-conditional-rendering-example)
- [Repeating Content Example](#repeating-content-example)
- [Repeating Content with Context Example](#repeating-content-with-context-example)
- [Partial Templates Example](#partial-templates-example)

### Conditional Rendering Example
This example demonstrates how to use conditional logic within a template to show different outputs based on a boolean flag.

```abap
* Define a template with conditional rendering
  DATA(template) = |<%if is_deleted %> Is Deleted = True <%endif%> | &&
                   |<%ifn is_deleted %> Is Deleted = False <%endifn%>|.

* Create an instance of the template engine with the defined template
  DATA(lo_template) = zcl_template_engine=>create( template = template ).

* Define the context structure
  TYPES: BEGIN OF lty_context,
           is_deleted TYPE boolean,
         END OF lty_context.

* Initialize context with data
  DATA: ls_context TYPE lty_context VALUE 'X'.

* Render the template with the context
  WRITE lo_template->render( ls_context ).
```

### Loop and Conditional Rendering Example
This example shows how to render a list of items, applying conditional display based on item properties.

```abap
* Define a template with a loop and conditional rendering
  DATA(template) = |<% for item in items %>| &&
                   |<% if item-show %>| &&
                   |<p>Item: <%item-name%></p>| &&
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
  ls_context-items = VALUE #( ( name = 'Item 1' show = abap_true  )
                              ( name = 'Item 2' show = abap_false )
                              ( name = 'Item 3' show = abap_true  ) ).

* Create an instance of the template engine with the defined template
  DATA(lo_template) = zcl_template_engine=>create( template = template ).

* Render the template with the context
  WRITE lo_template->render( ls_context ).
```

### Repeating Content Example
This example demonstrates how to repeat a section of the template a specified number of times.

```abap
* Define a template to repeat content
  DATA(template) = |<% do 30 %> <p>Hello World</p> <% enddo %>|.

* Create an instance of the template engine with the defined template
  DATA(lo_template) = zcl_template_engine=>create( template = template ).

* Render the template
  WRITE lo_template->render( ).
```

### Repeating Content with Context Example
This example shows how to use a context variable to control the number of repetitions in the template.

```abap
* Define the context structure
  TYPES: BEGIN OF lty_context,
           count TYPE i,
         END OF lty_context.

* Initialize context with data
  DATA: ls_context TYPE lty_context.
  ls_context-count = 3.

* Define a template to repeat content based on context
  DATA(template) = |<% do count %> <p>Hello World</p> <% enddo %>|.

* Create an instance of the template engine with the defined template
  DATA(lo_template) = zcl_template_engine=>create( template = template ).

* Render the template with the context
  WRITE lo_template->render( ls_context ).
```

### Partial Templates Example
This example demonstrates how to use partial templates to modularize and reuse template snippets.

```abap

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
      |Message from Partial 1: <%@partial1%>| &&
      |Message from Partial 2: <%@partial2%>| &&
      |Message from Partial 3: <%@partial3%>| &&
      |This is the main layout. The name used here is: <%name%>|
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
  WRITE lo_template->render( ls_context ).
```

Feel free to adjust any details according to your specific needs or preferences.
