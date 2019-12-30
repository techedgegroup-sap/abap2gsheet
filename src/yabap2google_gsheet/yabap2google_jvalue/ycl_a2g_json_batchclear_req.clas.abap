CLASS ycl_a2g_json_batchclear_req DEFINITION
  PUBLIC
    INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.

    TYPES: BEGIN OF ty_s_json_batchclear,
             ranges TYPE stringtab,
           END OF ty_s_json_batchclear.

    CONSTANTS: gc_fnam_ranges           TYPE string VALUE 'RANGES'.

  PROTECTED SECTION.

    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.


    DATA: gs_batchclear_req TYPE ty_s_json_batchclear.

    METHODS: set_ranges
      IMPORTING !i_value TYPE REF TO data.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_batchclear_req IMPLEMENTATION.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_ranges.    me->set_ranges( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_ranges.     return = REF #( me->gs_batchclear_req-ranges ).
    ENDCASE.
  ENDMETHOD.

  METHOD set_ranges.

    FIELD-SYMBOLS <fs_value> TYPE stringtab.
    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      me->gs_batchclear_req-ranges = <fs_value> .
    ENDIF.
  ENDMETHOD.

  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_batchclear_req ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

ENDCLASS.
