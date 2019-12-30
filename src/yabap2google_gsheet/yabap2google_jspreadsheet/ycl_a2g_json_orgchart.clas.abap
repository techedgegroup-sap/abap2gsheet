CLASS ycl_a2g_json_orgchart DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_orgchart,
             nodesize          TYPE string,
             nodecolor         TYPE ycl_a2g_json_color=>ty_s_json_color,
             selectednodecolor TYPE ycl_a2g_json_color=>ty_s_json_color,
             labels            TYPE ycl_a2g_json_chartdata=>ty_s_json_chartdata,
             parentlabels      TYPE ycl_a2g_json_chartdata=>ty_s_json_chartdata,
             tooltips          TYPE ycl_a2g_json_chartdata=>ty_s_json_chartdata,
           END OF ty_s_json_orgchart.
    TYPES ty_t_json_orgchart TYPE STANDARD TABLE OF ty_s_json_orgchart WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_orgchart  TYPE ty_s_json_orgchart.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_orgchart IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_orgchart  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
