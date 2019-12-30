CLASS ycl_a2g_json_batchupdate_req DEFINITION
  PUBLIC
  INHERITING FROM ycl_a2g_jsonbase
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING if_msg_manager TYPE REF TO yif_a2g_msg_manager.

    METHODS: yif_a2g_json~new_element        REDEFINITION.
    METHODS: yif_a2g_json~get_element        REDEFINITION.
    METHODS: yif_a2g_json~set_attribute      REDEFINITION.
    METHODS: yif_a2g_json~get_attribute      REDEFINITION.

    TYPES: BEGIN OF ty_s_json_batchupdate_req,
             valueinputoption             TYPE string,
             data                         TYPE ycl_a2g_json_valuerange=>ty_t_json_value_range,
             includevaluesinresponse      TYPE string,
             responsevaluerenderoption    TYPE string,
             responsedatetimerenderoption TYPE string,
           END OF ty_s_json_batchupdate_req.

    CONSTANTS: gc_fnam_valueinputoption     TYPE string VALUE 'VALUEINPUTOPTION'.
    CONSTANTS: gc_fnam_data                 TYPE string VALUE 'DATA'.
    CONSTANTS: gc_fnam_includevaluesinres   TYPE string VALUE 'INCLUDEVALUESRES'.
    CONSTANTS: gc_fnam_resvaluerenderoption TYPE string VALUE 'RESVALUERENDEROPTION'.
    CONSTANTS: gc_fnam_resdatetimerenderopt TYPE string VALUE 'RESDATETIMERENDEROPT'.
  PROTECTED SECTION.
    METHODS generate_rules REDEFINITION.
    METHODS rebuild_data   REDEFINITION.
    METHODS push_data      REDEFINITION.

    DATA: gs_batchupdate_req TYPE ty_s_json_batchupdate_req.

    METHODS SET_valueinputoption           IMPORTING !i_value TYPE REF TO data.
    METHODS SET_includevaluesinres     IMPORTING !i_value TYPE REF TO data.
    METHODS SET_resvaluerenderoption       IMPORTING !i_value TYPE REF TO data.
    METHODS SET_resdatetimerenderopt   IMPORTING !i_value TYPE REF TO data.

    METHODS new_datafilters RETURNING VALUE(return) TYPE REF TO yif_a2g_json.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_A2G_JSON_BATCHUPDATE_REQ IMPLEMENTATION.


  METHOD constructor.
    super->constructor( if_msg_manager ).
    me->gv_data = REF #( me->gs_batchupdate_req ).
  ENDMETHOD.


  METHOD generate_rules.
  ENDMETHOD.


  METHOD new_datafilters.
    DATA: lo_object TYPE REF TO object.
    return = me->go_json_factory->build_json_instance( 'YCL_A2G_JSON_VALUERANGE' ).

    lo_object ?= return.
    DATA: lv_line TYPE numc4.
    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_data ).
    lv_line = lines( lt_names ).
    me->go_json_array->setinstance(  im_name = me->gc_fnam_data && lv_line
                                     im_object = lo_object ).
  ENDMETHOD.


  METHOD push_data.

    LOOP AT me->gs_batchupdate_req-DATA INTO DATA(ls_datafilter).
      DATA(lif_a2g_json_grid) = me->new_datafilters(  ).
      lif_a2g_json_grid->yif_a2g_context~write_data( REF #( ls_datafilter ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD rebuild_data.
    DATA: lif_a2g_json TYPE REF TO yif_a2g_json.
    FIELD-SYMBOLS <fs_value_range> TYPE any.


    DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_data ).
    LOOP AT lt_names INTO DATA(lv_name).
      lif_a2g_json ?= me->go_json_array->getinstance( lv_name ).
      DATA(dref_data) = lif_a2g_json->yif_a2g_context~read_data( ).
      ASSIGN dref_data->* TO <fs_value_range>.
      APPEND <fs_value_range> TO me->gs_batchupdate_req-datA.
    ENDLOOP.

  ENDMETHOD.


  METHOD SET_includevaluesinres.

    FIELD-SYMBOLS <fs_value> TYPE STRING.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchupdate_req-includevaluesinresponse <> <fs_value>.
      me->gs_batchupdate_req-includevaluesinresponse = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD SET_resdatetimerenderopt .

    FIELD-SYMBOLS <fs_value> TYPE STRING.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchupdate_req-responsedatetimerenderoption <> <fs_value>.
      me->gs_batchupdate_req-responsedatetimerenderoption = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD SET_resvaluerenderoption.

    FIELD-SYMBOLS <fs_value> TYPE STRING.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchupdate_req-responsevaluerenderoption <> <fs_value>.
      me->gs_batchupdate_req-responsevaluerenderoption = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD SET_valueinputoption.

    FIELD-SYMBOLS <fs_value> TYPE STRING.

    ASSIGN i_value->* TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CHECK me->gs_batchupdate_req-valueinputoption <> <fs_value>.
      me->gs_batchupdate_req-valueinputoption = <fs_value>.
    ENDIF.
  ENDMETHOD.


  METHOD yif_a2g_json~get_attribute.

    CASE  i_name.
      WHEN gc_fnam_valueinputoption     . return = REF #( me->gs_batchupdate_req-valueinputoption ).
      WHEN gc_fnam_includevaluesinres   . return = REF #( me->gs_batchupdate_req-includevaluesinresponse ).
      WHEN gc_fnam_resvaluerenderoption . return = REF #( me->gs_batchupdate_req-responsevaluerenderoption ).
      WHEN gc_fnam_resdatetimerenderopt . return = REF #( me->gs_batchupdate_req-responsedatetimerenderoption ).
    ENDCASE.

  ENDMETHOD.


  METHOD yif_a2g_json~get_element.
    CASE i_name .
      WHEN gc_fnam_data.
        DATA(lt_names) =  me->go_json_array->getpartnamesofinstances( me->gc_fnam_data ).
        TRY.
            DATA(lv_name) = lt_names[ i_enum ].
            return ?= me->go_json_array->getinstance( lv_name ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
    ENDCASE.
  ENDMETHOD.


  METHOD yif_a2g_json~new_element.
    CASE i_name .
      WHEN gc_fnam_data  . return = me->new_datafilters( ).
    ENDCASE.
  ENDMETHOD.


  METHOD yif_a2g_json~set_attribute.
    CASE  i_name.
      WHEN gc_fnam_valueinputoption     . me->SET_valueinputoption( i_value ).
      WHEN gc_fnam_includevaluesinres   . me->SET_includevaluesinres( i_value ).
      WHEN gc_fnam_resdatetimerenderopt . me->SET_resvaluerenderoption( i_value ).
      WHEN gc_fnam_resdatetimerenderopt . me->SET_resdatetimerenderopt( i_value ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
