CLASS ycl_a2g_json_spreadsheet_prop DEFINITION
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
    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.
    METHODS: yif_a2g_json~set_default        REDEFINITION.
    TYPES: BEGIN OF ty_s_json_spreadsheet_prop,
             title                        TYPE string,
             locale                       TYPE string,
             autorecalc                   TYPE string,
             timezone                     TYPE string,
             defaultformat                TYPE ycl_a2g_json_cellformat=>ty_s_json_cellformat,
             iterativecalculationsettings TYPE ycl_a2g_json_itercalcsettings=>ty_s_json_itercalcsettings,
           END OF ty_s_json_spreadsheet_prop.

    CONSTANTS: gc_fnam_title  TYPE string VALUE 'TITLE'.
    CONSTANTS: gc_fnam_locale  TYPE string VALUE 'LOCALE'.
    CONSTANTS: gc_fnam_autorecalc TYPE string VALUE 'AUTORECALC'.
    CONSTANTS: gc_fnam_timezone TYPE string VALUE 'TIMEZONE'.

    CONSTANTS: gc_fnam_defaultformat  TYPE string VALUE 'CELLFORMAT'.
    CONSTANTS: gc_fnam_itercalcsettings      TYPE string VALUE 'ITERCALC'.

  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_spreadsheet_prop  TYPE ty_s_json_spreadsheet_prop.

    METHODS set_title      IMPORTING !i_value TYPE REF TO data.
    METHODS set_locale        IMPORTING !i_value TYPE REF TO data.
    METHODS set_autorecalc        IMPORTING !i_value TYPE REF TO data.
    METHODS set_timezone        IMPORTING !i_value TYPE REF TO data.

    METHODS new_defaultformat      RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_itercalcsettings   RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_spreadsheet_prop IMPLEMENTATION.

  METHOD yif_a2g_json~set_default.

    DATA: lv_string TYPE string.

    DATA(lif_format) = me->yif_a2g_json~new_element( gc_fnam_defaultformat ).
    lif_format->set_default( ).

    lv_string = 'it_IT'.
    me->set_locale( REF #( lv_string ) ).
    lv_string = 'ON_CHANGE'.
    me->set_autorecalc( REF #( lv_string ) ) .
    lv_string = 'Etc/GMT'.
    me->set_timezone( REF #( lv_string ) ).

  ENDMETHOD.


  METHOD set_autorecalc.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_spreadsheet_prop-autorecalc <> <fs_value>.
      me->gs_spreadsheet_prop-autorecalc = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_timezone.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_spreadsheet_prop-timezone <> <fs_value>.
      me->gs_spreadsheet_prop-timezone = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD set_locale.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_spreadsheet_prop-locale <> <fs_value>.
      me->gs_spreadsheet_prop-locale = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_title.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_spreadsheet_prop-title <> <fs_value>.
      me->gs_spreadsheet_prop-title = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_title . me->set_title( i_value ).
      WHEN gc_fnam_locale  . me->set_locale( i_value  ).
      WHEN gc_fnam_autorecalc  . me->set_autorecalc( i_value  ).
      WHEN gc_fnam_timezone  . me->set_timezone( i_value  ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_title      . return = REF #( me->gs_spreadsheet_prop-title ).
      WHEN gc_fnam_locale     . return = REF #( me->gs_spreadsheet_prop-locale ).
      WHEN gc_fnam_autorecalc . return = REF #( me->gs_spreadsheet_prop-autorecalc ).
      WHEN gc_fnam_timezone   . return = REF #( me->gs_spreadsheet_prop-timezone ).
    ENDCASE.
  ENDMETHOD.

  METHOD push_data.

    DATA(lif_a2g_json_format) = me->new_defaultformat(  ).
    lif_a2g_json_format->yif_a2g_context~write_data( REF #( me->gs_spreadsheet_prop-defaultformat ) ).

    DATA(lif_a2g_json_sett) = me->new_itercalcsettings(  ).
    lif_a2g_json_sett->yif_a2g_context~write_data( REF #( me->gs_spreadsheet_prop-iterativecalculationsettings ) ).

  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name CS me->gc_fnam_defaultformat.
        me->gs_spreadsheet_prop-defaultformat  =  <fs_value_range>.
      ELSEIF lv_name CS me->gc_fnam_itercalcsettings.
        me->gs_spreadsheet_prop-iterativecalculationsettings  =  <fs_value_range>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_spreadsheet_prop  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_defaultformat  . return = me->new_defaultformat( ).
      WHEN gc_fnam_itercalcsettings        . return = me->new_itercalcsettings( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_defaultformat. return ?= me->go_json_array->getinstance( gc_fnam_defaultformat ).
      WHEN gc_fnam_itercalcsettings. return ?= me->go_json_array->getinstance( gc_fnam_itercalcsettings ).
    ENDCASE.
  ENDMETHOD.

  METHOD new_defaultformat.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_CELLFORMAT' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_defaultformat
                                     im_object = lo_object ).
  ENDMETHOD.

  METHOD new_itercalcsettings.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_ITERCALCSETTINGS' ).

    lo_object ?= return.
    me->go_json_array->setinstance(  im_name = me->gc_fnam_itercalcsettings
                                     im_object = lo_object ).
  ENDMETHOD.



ENDCLASS.
