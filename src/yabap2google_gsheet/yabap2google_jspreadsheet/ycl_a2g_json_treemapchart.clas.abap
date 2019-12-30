CLASS ycl_a2g_json_treemapchart DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_treemapchart,
             labels       TYPE ycl_a2g_json_chartdata=>ty_s_json_chartdata,
             parentlabels TYPE ycl_a2g_json_chartdata=>ty_s_json_chartdata,
             sizedata     TYPE ycl_a2g_json_chartdata=>ty_s_json_chartdata,
             colordata    TYPE ycl_a2g_json_chartdata=>ty_s_json_chartdata,
             textformat   TYPE ycl_a2g_json_textformat=>ty_s_json_textformat,
             levels       TYPE i,
             hintedlevels TYPE i,
             minvalue     TYPE i,
             maxvalue     TYPE i,
             headercolor  TYPE ycl_a2g_json_color=>ty_s_json_color,
             colorscale   TYPE ycl_a2g_json_treemapchartcols=>ty_s_json_treemapchartcols,
             hidetooltips TYPE string,
           END OF ty_s_json_treemapchart.
    TYPES ty_t_json_treemapchart TYPE STANDARD TABLE OF ty_s_json_treemapchart WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_treemapchart  TYPE ty_s_json_treemapchart.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_treemapchart IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_treemapchart  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
