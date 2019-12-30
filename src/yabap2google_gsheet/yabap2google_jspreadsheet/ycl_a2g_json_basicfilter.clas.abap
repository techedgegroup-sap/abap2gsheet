CLASS ycl_a2g_json_basicfilter DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_basicfilter,
             range     TYPE ycl_a2g_json_gridrange=>ty_s_json_gridrange,
             sortspecs TYPE ycl_a2g_json_sheet_prop=>ty_t_json_sheet_prop,
             criteria  TYPE ycl_a2g_json_filtercriteria=>ty_s_json_filtercriteria,
           END OF ty_s_json_basicfilter.
    TYPES ty_t_json_basicfilter TYPE STANDARD TABLE OF ty_s_json_basicfilter WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_basicfilter  TYPE ty_s_json_basicfilter.
  PRIVATE SECTION.
ENDCLASS.


CLASS ycl_a2g_json_basicfilter IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_basicfilter  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
