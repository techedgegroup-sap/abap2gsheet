CLASS ycl_a2g_json_filterview DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_filterview,
             filterviewid TYPE string,
             title        TYPE string,
             range        TYPE ycl_a2g_json_gridrange=>ty_t_json_gridrange,
             namedrangeid TYPE string,
             sortspecs    TYPE ycl_a2g_json_sortspec=>ty_t_json_sortspec,
             criteria     TYPE ycl_a2g_json_filtercriteria=>ty_s_json_filtercriteria,
           END OF ty_s_json_filterview.
    TYPES ty_t_json_filterview TYPE STANDARD TABLE OF ty_s_json_filterview WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_filterview  TYPE ty_s_json_filterview.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_filterview IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_filterview  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
