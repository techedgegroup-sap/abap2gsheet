CLASS ycl_a2g_json_itercalcsettings DEFINITION
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

    TYPES: BEGIN OF ty_s_json_itercalcsettings ,
             maxiterations        TYPE i,
             convergencethreshold TYPE i,
           END OF ty_s_json_itercalcsettings.

    CONSTANTS: gc_fnam_maxiterations  TYPE string VALUE 'MAXITER'.
    CONSTANTS: gc_fnam_converge  TYPE string VALUE 'CONVERGE'.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_itercalcsettings  TYPE ty_s_json_itercalcsettings.

    METHODS set_maxiterations      IMPORTING !i_value TYPE REF TO data.
    METHODS set_converge        IMPORTING !i_value TYPE REF TO data.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_itercalcsettings IMPLEMENTATION.



  METHOD set_converge.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_itercalcsettings-convergencethreshold <> <fs_value>.
      me->gs_itercalcsettings-convergencethreshold = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_maxiterations.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_itercalcsettings-maxiterations <> <fs_value>.
      me->gs_itercalcsettings-maxiterations = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_maxiterations . me->set_maxiterations( i_value ).
      WHEN gc_fnam_converge  . me->set_converge( i_value  ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_maxiterations      . return = REF #( me->gs_itercalcsettings-maxiterations ).
      WHEN gc_fnam_converge     . return = REF #( me->gs_itercalcsettings-convergencethreshold ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.

  ENDMETHOD.

  METHOD rebuild_data.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_itercalcsettings ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.



ENDCLASS.
