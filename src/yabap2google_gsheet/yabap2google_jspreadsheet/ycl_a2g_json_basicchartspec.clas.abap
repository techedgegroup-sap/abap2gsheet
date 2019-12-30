CLASS ycl_a2g_json_basicchartspec DEFINITION
  PUBLIC
 INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF TY_S_JSON_BASICCHARTSPEC,
             CHART_TYPE        TYPE STRING,
             LEGENDPOSITION   TYPE STRING,
             AXIS             TYPE YCL_A2G_JSON_BASICCHARTAXIS=>TY_T_JSON_BASICCHARTAXIS,
             DOMAINS          TYPE YCL_A2G_JSON_BASICCHARTDOMAIN=>TY_T_JSON_BASICCHARTDOMAIN,
             SERIES           TYPE YCL_A2G_JSON_BASICCHARTSERIES=>TY_T_JSON_BASICCHARTSERIES,
             HEADERCOUNT      TYPE I,
             THREEDIMENSIONAL TYPE STRING,
             INTERPOLATENULLS TYPE STRING,
             STACKEDTYPE      TYPE STRING,
             LINESMOOTHING    TYPE STRING,
             COMPAREMODE      TYPE STRING,
           END OF TY_S_JSON_BASICCHARTSPEC.
    TYPES ty_t_json_basicchartspec TYPE STANDARD TABLE OF ty_s_json_basicchartspec WITH NON-UNIQUE DEFAULT KEY.

        CONSTANTS: gc_fnam_CHARTTYPE          TYPE string VALUE 'CHARTTYPE'.
        CONSTANTS: gc_fnam_LEGENDPOSITION     TYPE string VALUE 'LEGENDPOSITION'.
        CONSTANTS: gc_fnam_HEADERCOUNT        TYPE string VALUE 'HEADERCOUNT'.
        CONSTANTS: gc_fnam_THREEDIMENSIONAL   TYPE string VALUE 'THREEDIMENSIONAL'.
        CONSTANTS: gc_fnam_INTERPOLATENULLS   TYPE string VALUE 'INTERPOLATENULLS'.
        CONSTANTS: gc_fnam_STACKEDTYPE        TYPE string VALUE 'STACKEDTYPE'.
        CONSTANTS: gc_fnam_LINESMOOTHING      TYPE string VALUE 'LINESMOOTHING'.
        CONSTANTS: gc_fnam_COMPAREMODE        TYPE string VALUE 'COMPAREMODE'.

        CONSTANTS: gc_fnam_AXIS               TYPE string VALUE 'AXIS'.
        CONSTANTS: gc_fnam_DOMAINS            TYPE string VALUE 'DOMAINS'.
        CONSTANTS: gc_fnam_SERIES             TYPE string VALUE 'SERIES'.

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

    DATA: gs_basicchartspec  TYPE ty_s_json_basicchartspec.

    METHODS set_CHARTTYPE          IMPORTING !i_value TYPE REF TO data.
    METHODS set_LEGENDPOSITION     IMPORTING !i_value TYPE REF TO data.
    METHODS set_HEADERCOUNT        IMPORTING !i_value TYPE REF TO data.
    METHODS set_THREEDIMENSIONAL   IMPORTING !i_value TYPE REF TO data.
    METHODS set_INTERPOLATENULLS   IMPORTING !i_value TYPE REF TO data.
    METHODS set_STACKEDTYPE        IMPORTING !i_value TYPE REF TO data.
    METHODS set_LINESMOOTHING      IMPORTING !i_value TYPE REF TO data.
    METHODS set_COMPAREMODE        IMPORTING !i_value TYPE REF TO data.

    METHODS new_AXIS      RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_DOMAINS   RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
    METHODS new_SERIES    RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_json_basicchartspec IMPLEMENTATION.

  METHOD set_COMPAREMODE.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_basicchartspec-COMPAREMODE <> <fs_value>.
      me->gs_basicchartspec-COMPAREMODE = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_LINESMOOTHING.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_basicchartspec-LINESMOOTHING <> <fs_value>.
      me->gs_basicchartspec-LINESMOOTHING = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_STACKEDTYPE.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_basicchartspec-STACKEDTYPE <> <fs_value>.
      me->gs_basicchartspec-STACKEDTYPE = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_INTERPOLATENULLS.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_basicchartspec-INTERPOLATENULLS <> <fs_value>.
      me->gs_basicchartspec-INTERPOLATENULLS = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_THREEDIMENSIONAL.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_basicchartspec-THREEDIMENSIONAL <> <fs_value>.
      me->gs_basicchartspec-THREEDIMENSIONAL = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_LEGENDPOSITION.

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_basicchartspec-LEGENDPOSITION <> <fs_value>.
      me->gs_basicchartspec-LEGENDPOSITION = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_CHARTTYPE .

    FIELD-SYMBOLS <fs_value> TYPE string.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_basicchartspec-CHART_TYPE  <> <fs_value>.
      me->gs_basicchartspec-CHART_TYPE  = <fs_value>.
    ENDIF.
  ENDMETHOD.

  METHOD set_HEADERCOUNT.

    FIELD-SYMBOLS <fs_value> TYPE i.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_basicchartspec-HEADERCOUNT <> <fs_value>.
      me->gs_basicchartspec-HEADERCOUNT = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_CHARTTYPE         . me->set_CHARTTYPE( i_value ).
      WHEN gc_fnam_LEGENDPOSITION    . me->set_LEGENDPOSITION( i_value ).
      WHEN gc_fnam_HEADERCOUNT       . me->set_HEADERCOUNT( i_value ).
      WHEN gc_fnam_THREEDIMENSIONAL  . me->set_THREEDIMENSIONAL( i_value ).
      WHEN gc_fnam_INTERPOLATENULLS  . me->set_INTERPOLATENULLS( i_value ).
      WHEN gc_fnam_STACKEDTYPE       . me->set_STACKEDTYPE( i_value ).
      WHEN gc_fnam_LINESMOOTHING     . me->set_LINESMOOTHING( i_value ).
      WHEN gc_fnam_COMPAREMODE       . me->set_COMPAREMODE( i_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_attribute.
    CASE  i_name.
      WHEN gc_fnam_CHARTTYPE          . return = REF #( me->gs_basicchartspec-CHART_TYPE        ).
      WHEN gc_fnam_LEGENDPOSITION     . return = REF #( me->gs_basicchartspec-LEGENDPOSITION   ).
      WHEN gc_fnam_HEADERCOUNT        . return = REF #( me->gs_basicchartspec-HEADERCOUNT      ).
      WHEN gc_fnam_THREEDIMENSIONAL   . return = REF #( me->gs_basicchartspec-THREEDIMENSIONAL ).
      WHEN gc_fnam_INTERPOLATENULLS   . return = REF #( me->gs_basicchartspec-INTERPOLATENULLS ).
      WHEN gc_fnam_STACKEDTYPE        . return = REF #( me->gs_basicchartspec-STACKEDTYPE      ).
      WHEN gc_fnam_LINESMOOTHING      . return = REF #( me->gs_basicchartspec-LINESMOOTHING    ).
      WHEN gc_fnam_COMPAREMODE        . return = REF #( me->gs_basicchartspec-COMPAREMODE      ).
    ENDCASE.
  ENDMETHOD.


  METHOD push_data.
    LOOP AT me->gs_basicchartspec-AXIS INTO DATA(ls_AXIS).
      DATA(lif_a2g_json_sheet) = me->new_AXIS(  ).
      lif_a2g_json_sheet->yif_a2g_context~write_data( REF #( ls_AXIS ) ).
    ENDLOOP.

    LOOP AT me->gs_basicchartspec-DOMAINS INTO DATA(ls_DOMAINS).
      DATA(lif_a2g_json_namedrange) = me->new_DOMAINS(  ).
      lif_a2g_json_namedrange->yif_a2g_context~write_data( REF #( ls_DOMAINS ) ).
    ENDLOOP.

    LOOP AT me->gs_basicchartspec-SERIES INTO DATA(ls_metadata).
      DATA(lif_a2g_json_metadata) = me->new_SERIES(  ).
      lif_a2g_json_metadata->yif_a2g_context~write_data( REF #( ls_metadata ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.


clear: me->gs_basicchartspec-SERIES , me->gs_basicchartspec-DOMAINS, me->gs_basicchartspec-AXIS.

    DATA(lt_names) =  me->go_json_array->getallnamesofinstances(  ).
    LOOP AT lt_names INTO DATA(lv_name).

      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      IF lv_name     CS me->gc_fnam_AXIS     .     APPEND <fs_value_range> TO me->gs_basicchartspec-AXIS        .
      ELSEIF lv_name CS me->gc_fnam_DOMAINS  .     APPEND <fs_value_range> TO me->gs_basicchartspec-DOMAINS     .
      ELSEIF lv_name CS me->gc_fnam_SERIES   .     APPEND <fs_value_range> TO me->gs_basicchartspec-SERIES      .
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_basicchartspec  ).
  ENDMETHOD.

  METHOD generate_rules.
  ENDMETHOD.

  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_AXIS  . return = me->new_AXIS( ).
      WHEN gc_fnam_DOMAINS  . return = me->new_DOMAINS( ).
      WHEN gc_fnam_SERIES   . return = me->new_SERIES( ).
    ENDCASE.
  ENDMETHOD.

  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_AXIS.
        DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_AXIS ).
        TRY.
            DATA(lv_name) = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      WHEN gc_fnam_DOMAINS .
        lt_names =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_DOMAINS ).
        TRY.
            lv_name = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      WHEN gc_fnam_SERIES .
        lt_names =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_SERIES ).
        TRY.
            lv_name = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

    ENDCASE.
  ENDMETHOD.

  METHOD new_AXIS.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_BASICCHARTAXIS' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_AXIS ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_AXIS && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.


  METHOD new_DOMAINS.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_BASICCHARTDOMAIN' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_DOMAINS ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_DOMAINS && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.


  METHOD new_SERIES.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_BASICCHARTSERIES' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_SERIES ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_SERIES && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.


ENDCLASS.
