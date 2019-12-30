CLASS ycl_a2g_json_batchclear_res DEFINITION
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
             spreadsheetid TYPE string,
             clearedranges TYPE ya2google_t_jstring,
           END OF ty_s_json_batchclear.

    CONSTANTS: gc_fnam_spreadsheetid   TYPE string VALUE 'SPREADSHEETID'.
    CONSTANTS: gc_fnam_clearedranges   TYPE string VALUE 'CLEAREDRANGE'.

  PROTECTED SECTION.

    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_batchclear_res TYPE ty_s_json_batchclear.

    METHODS: set_clearedranges
      IMPORTING !i_value TYPE REF TO data.

    METHODS: set_spreadsheetid
      IMPORTING !i_value TYPE REF TO data.


  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_batchclear_res IMPLEMENTATION.

  METHOD set_spreadsheetid.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchclear_res-spreadsheetid <> <fs_value>.
      me->gs_batchclear_res-spreadsheetid = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_clearedranges.

    FIELD-SYMBOLS <fs_value> TYPE stringtab.
    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      append <fs_value> to me->gs_batchclear_res-clearedranges.
    ENDIF.
  ENDMETHOD.


  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_spreadsheetid.    me->set_clearedranges( i_value ).
      WHEN gc_fnam_clearedranges.    me->set_clearedranges( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_spreadsheetid.   return = REF #( me->gs_batchclear_res-spreadsheetid ).
      WHEN gc_fnam_clearedranges.   return = REF #( me->gs_batchclear_res-clearedranges ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.
  ENDMETHOD.

  METHOD rebuild_data.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_batchclear_res ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.
ENDCLASS.
