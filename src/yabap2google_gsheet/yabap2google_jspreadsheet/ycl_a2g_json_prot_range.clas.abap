CLASS ycl_a2g_json_prot_range DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_prot_range,
             protectedrangeid      TYPE i,
             range                 TYPE ycl_a2g_json_gridrange=>ty_s_json_gridrange,
             namedrangeid          TYPE string,
             description           TYPE string,
             warningonly           TYPE string,
             requestingusercanedit TYPE string,
             unprotectedranges     TYPE ycl_a2g_json_gridrange=>ty_t_json_gridrange,
             editors               TYPE ycl_a2g_json_editors=>ty_s_json_editors,
           END OF ty_s_json_prot_range.
    TYPES ty_t_json_prot_range TYPE STANDARD TABLE OF ty_s_json_prot_range WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_prot_range  TYPE ty_s_json_prot_range.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_prot_range IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_prot_range  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
