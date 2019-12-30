CLASS ycl_a2g_json_copyto_req DEFINITION
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

    TYPES: BEGIN OF ty_s_json_batchget,
             destinationSpreadsheetId TYPE string,
           END OF ty_s_json_batchget.

    CONSTANTS: gc_fnam_DEST_SPREADSHEETID   TYPE string VALUE 'DEST_SPREADSHEETID'.
  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_copyto TYPE ty_s_json_batchget.

    METHODS set_DEST_SPREADSHEETID  IMPORTING !i_value TYPE REF TO data.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_copyto_req IMPLEMENTATION.
  METHOD set_DEST_SPREADSHEETID.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_copyto-destinationspreadsheetid <> <fs_value>.
      me->gs_copyto-destinationspreadsheetid = <fs_value>.
    ENDIF.
  ENDMETHOD.



  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_DEST_SPREADSHEETID      . me->set_DEST_SPREADSHEETID( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_DEST_SPREADSHEETID       . return = REF #( me->gs_copyto-destinationspreadsheetid ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.

  ENDMETHOD.

  METHOD rebuild_data.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_copyto ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.


ENDCLASS.
