CLASS ycl_a2g_json_extendedvalue DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_json_extendedvalue,
             number_value  TYPE dmbtr,
             string_value  TYPE string,
             boolvalue    TYPE string,
             formulavalue TYPE string,
             errorvalue   TYPE YCL_A2G_JSON_ERRORVALUE=>ty_s_json_errorvalue,
           END OF ty_s_json_extendedvalue.
    TYPES ty_t_json_ExtendedValue TYPE STANDARD TABLE OF ty_s_json_ExtendedValue WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: gc_fnam_numbervalue     TYPE string VALUE 'NUMBERVALUE'.
    CONSTANTS: gc_fnam_stringvalue     TYPE string VALUE 'string_value'.
    CONSTANTS: gc_fnam_boolvalue       TYPE string VALUE 'BOOLVALUE'.
    CONSTANTS: gc_fnam_formulavalue    TYPE string VALUE 'FORMULAVALUE'.
    CONSTANTS: gc_fnam_errorvalue      TYPE string VALUE 'ERRORVALUE'.


    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.
    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.


  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_ExtendedValue  TYPE ty_s_json_ExtendedValue.

    METHODS set_numbervalue       IMPORTING !i_value TYPE REF TO data.
    METHODS set_string_value       IMPORTING !i_value TYPE REF TO data.
    METHODS set_boolvalue         IMPORTING !i_value TYPE REF TO data.
    METHODS set_formulavalue      IMPORTING !i_value TYPE REF TO data.

    METHODS new_errorvalue        RETURNING VALUE(return) TYPE REF TO yif_a2g_json.


  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_extendedvalue IMPLEMENTATION.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_numbervalue   . me->set_numbervalue( i_value ).
      WHEN gc_fnam_stringvalue   . me->set_string_value( i_value ).
      WHEN gc_fnam_boolvalue     . me->set_boolvalue( i_value ).
      WHEN gc_fnam_formulavalue  . me->set_formulavalue( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_numbervalue    . return = REF #( me->gs_ExtendedValue-number_value  ).
      WHEN gc_fnam_stringvalue    . return = REF #( me->gs_ExtendedValue-string_value  ).
      WHEN gc_fnam_boolvalue      . return = REF #( me->gs_ExtendedValue-boolvalue    ).
      WHEN gc_fnam_formulavalue   . return = REF #( me->gs_ExtendedValue-formulavalue ).
    ENDCASE.
  ENDMETHOD.


  METHOD set_formulavalue.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_ExtendedValue-formulavalue <> <fs_value>.
      me->gs_ExtendedValue-formulavalue = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_boolvalue.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_ExtendedValue-boolvalue <> <fs_value>.
      me->gs_ExtendedValue-boolvalue = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_string_value.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_ExtendedValue-string_value <> <fs_value>.
      me->gs_ExtendedValue-string_value = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_numbervalue.

    FIELD-SYMBOLS <fs_value> TYPE dmbtr.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_ExtendedValue-number_value <> <fs_value>.
      me->gs_ExtendedValue-number_value = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD push_data.
    DATA(lif_a2g_json_prop) = me->new_errorvalue(  ).
    lif_a2g_json_prop->yif_a2g_context~write_data( REF #( me->gs_ExtendedValue-errorvalue ) ).
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_errorvalue.
        me->gs_ExtendedValue-errorvalue  =  <fs_value_range>.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_ExtendedValue  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_errorvalue  . return = me->new_errorvalue( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_errorvalue. return ?= me->go_json_array->getinstance( gc_fnam_errorvalue ).
    ENDCASE.
  ENDMETHOD.

    METHOD new_errorvalue.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_ERRORVALUE' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_errorvalue
                                     im_object = lo_object ).
  ENDMETHOD.

ENDCLASS.
