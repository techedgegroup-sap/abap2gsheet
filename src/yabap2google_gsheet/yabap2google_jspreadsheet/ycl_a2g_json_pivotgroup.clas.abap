CLASS ycl_a2g_json_pivotgroup DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    TYPES: BEGIN OF ty_s_json_pivotgroup,
             sourcecolumnoffset TYPE i,
             showtotals         TYPE string,
             valuemetadata      TYPE ycl_a2g_json_pivotgrpvalmeta=>ty_t_json_pivotgrpvalmeta,
             sortorder          TYPE string,
             valuebucket        TYPE ycl_a2g_json_pivotgrpsvalbuck=>ty_s_json_pivotgrpsvalbuck,
             repeatheadings     TYPE string,
             label              TYPE string,
             grouprule          TYPE ycl_a2g_json_pivotgrouprule=>ty_s_json_pivotgrouprule,
           END OF ty_s_json_pivotgroup.
    TYPES ty_t_json_pivotgroup TYPE STANDARD TABLE OF ty_s_json_pivotgroup WITH NON-UNIQUE DEFAULT KEY.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_pivotgroup  TYPE ty_s_json_pivotgroup.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_pivotgroup IMPLEMENTATION.
  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_pivotgroup  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
