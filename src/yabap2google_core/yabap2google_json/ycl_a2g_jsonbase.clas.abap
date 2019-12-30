"! <h1>ycl_a2g_jsonbase</h1>
"! <p class="shorttext synchronized" lang="en">Cmp. Appl - Abap 2 Google - Json Model Base Abstrace</p>
CLASS ycl_a2g_jsonbase DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_a2g_json_context .
    INTERFACES yif_a2g_context .
    INTERFACES yif_a2g_json .
    INTERFACES yif_a2g_serialize .

    "! True
    CONSTANTS gc_true TYPE oax VALUE 'X' ##NO_TEXT.
    "! False
    CONSTANTS gc_false TYPE oax VALUE ' ' ##NO_TEXT.

    "! Build the class
    "! @parameter if_msg_manager | message managere where soter alla message triggered by the applicaition
    METHODS constructor
      IMPORTING
        !if_msg_manager TYPE REF TO yif_a2g_msg_manager .
  PROTECTED SECTION.


    DATA:
      "! Rule factory to create the instance of check class
      go_rule_factory TYPE REF TO ycl_a2g_rule_factory,
      "! JSON FACTORY FO CHILD GENERATION
      go_json_factory TYPE REF TO ycl_a2g_json_factory,
      "! Array of json instance for build json structure
      go_json_array   TYPE REF TO ycl_a2g_array,
      "! Message manager instance
      go_msg_manager  TYPE REF TO yif_a2g_msg_manager,
      "! Abap structure for data
      gv_data         TYPE REF TO data,
      "! Json Data string
      gv_json         TYPE string,
      gv_struct_nane  TYPE string.

    "! This Method update the single values into the gv_data structure
    "! @parameter i_fieldname | name of the field of the structure
    "! @parameter i_value     | value to set
    METHODS  update_field
      IMPORTING !i_fieldname TYPE        string
                !i_value     TYPE REF TO data.

    "! genereta the rule of the class
    METHODS generate_rules  ABSTRACT.
    "! rebuild the data into the gv_data attribute
    METHODS rebuild_data    ABSTRACT.
    "! Push down the data to the subobject
    METHODS push_data    ABSTRACT.


    "! This Method execute the check of the single fields
    "! @parameter i_fieldname   | name of the filed to check
    "! @parameter i_recheck     | check again the value if already checked
    "! @raising ycx_a2g_objbase | exception class
    METHODS check_field
      IMPORTING !i_fieldname TYPE        string
                !i_recheck   TYPE        oax    OPTIONAL
      RAISING   ycx_a2g_objbase.


    "! This Method execute the check of the single fields
    "! @parameter i_classname   | name of the rule class to instatiate
    "! @parameter i_fieldname   | name of the field to link the class instance
    "! @parameter i_mandatary   | enable the mandatory
    METHODS generate_rule
      IMPORTING !i_classname TYPE   string
                !i_fieldname TYPE   string
                !i_mandatary TYPE   oax    OPTIONAL.

    "! Convert the abap structure of the class into Json model in string format
    METHODS abap_2_json .
    "! Convert a Json model in string format into the abap main structure of the class
    METHODS json_2_abap .


  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_jsonbase IMPLEMENTATION.


  METHOD  abap_2_json.
    FIELD-SYMBOLS <fs_struct> TYPE any.
    ASSIGN me->gv_data->* TO <fs_struct>.
    me->gv_json = /ui2/cl_json=>serialize( data         = <fs_struct>
                                           compress     = abap_true
                                           pretty_name  = /ui2/cl_json=>pretty_mode-camel_case ).

  ENDMETHOD.


  METHOD check_field.
    "&  Declaration Part
    DATA: lif_rule    TYPE REF TO yif_a2g_rule,
          lif_context TYPE REF TO yif_a2g_context,
          ox_rule     TYPE REF TO ycx_a2g_rule.

    "&  Source Part
    lif_context ?= me.

    lif_rule ?= me->go_rule_factory->get_rule_by_field( i_fieldname ).
    TRY .
        lif_rule->execute( i_context = lif_context
                           i_recheck = i_recheck  ).

      CATCH ycx_a2g_rule INTO ox_rule.
        RAISE EXCEPTION TYPE ycx_a2g_objbase
          EXPORTING
            textid = ox_rule->if_t100_message~t100key.
      CATCH cx_root.
* the rule does not exist. go to the next check
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    "&  Source Part
    IF if_msg_manager IS BOUND.
      me->go_msg_manager ?= if_msg_manager.
    ELSE.
      me->go_msg_manager = ycl_a2g_msg_manager=>create_init_msg_manager(  ).
    ENDIF.
    me->go_rule_factory = NEW #( me->go_msg_manager ).
    me->go_json_factory = NEW #( me->go_msg_manager ).
    me->go_json_array   = NEW #( ).
  ENDMETHOD.


  METHOD generate_rule.
    "&  Declaration Part
    DATA: lif_rule TYPE REF TO yif_a2g_rule.

    "&  Source Part
    lif_rule ?= me->go_rule_factory->get_rule_by_field( i_fieldname ).
    IF NOT lif_rule IS  BOUND.
      lif_rule ?= me->go_rule_factory->get_rule( i_classname ).
      IF i_mandatary = me->gc_true.
        lif_rule->set_mandatary( ).
      ENDIF.
      lif_rule->assign_dependencies( io_rule_factory = me->go_rule_factory
                                     i_fieldname     = i_fieldname ).
    ENDIF.
  ENDMETHOD.


  METHOD  json_2_abap.
    FIELD-SYMBOLS <fs_struct> TYPE any.
    ASSIGN me->gv_data->* TO <fs_struct>.
    /ui2/cl_json=>deserialize( EXPORTING json        = me->gv_json
                                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                               CHANGING  data        = <fs_struct> ).

  ENDMETHOD.


  METHOD update_field.
    "&  Declaration Part
    FIELD-SYMBOLS <fs_structure> TYPE any.
    FIELD-SYMBOLS <fs_field> TYPE any.
    FIELD-SYMBOLS <fs_field_source> TYPE any.

    "&  Source Part
    ASSIGN me->gv_data->* TO <fs_structure>.
    ASSIGN COMPONENT i_fieldname OF STRUCTURE <fs_structure> TO <fs_field>.
    IF sy-subrc = 0.
      ASSIGN i_value->* TO <fs_field_source>.
      <fs_field> = <fs_field_source>.
    ENDIF.
  ENDMETHOD.


  METHOD yif_a2g_context~get_protocol.
    return = me->go_msg_manager.
  ENDMETHOD.


  METHOD yif_a2g_context~read_data.
    "&  Source Part
    me->rebuild_data( ).
    return = me->gv_data.
  ENDMETHOD.


  METHOD yif_a2g_context~write_data.
    "&  Source Part
    FIELD-SYMBOLS <fs_struct> TYPE any.
    FIELD-SYMBOLS <fs_struct_in> TYPE any.
    ASSIGN me->gv_data->* TO <fs_struct>.
    ASSIGN input->* TO <fs_struct_in>.
    <fs_struct> = <fs_struct_in>.
    me->abap_2_json( ).
    me->push_data( ).
  ENDMETHOD.                    "YIF_cONTEXT~write_data


  METHOD yif_a2g_json_context~read_json_data.
    "&  Source Part
    me->rebuild_data( ).
    me->abap_2_json( ).
    return = me->gv_json.
  ENDMETHOD.


  METHOD yif_a2g_json_context~write_json_data.
    "&  Source Part
    me->gv_json = input.
    me->json_2_abap( ).
    me->push_data( ).
  ENDMETHOD.                    "YIF_cONTEXT~write_data


  METHOD  yif_a2g_json~get_abap.
    me->json_2_abap( ).
    return = me->gv_data.
  ENDMETHOD.


  METHOD  yif_a2g_json~get_attribute.
* Do Nothing no attribute ad this level
  ENDMETHOD.


  METHOD  yif_a2g_json~get_element.
* Do Nothing no element ad this level
  ENDMETHOD.


  METHOD  yif_a2g_json~new_element.
* Do Nothing no element ad this level
  ENDMETHOD.


  METHOD  yif_a2g_json~set_attribute.
* Do Nothing no attribute ad this level
  ENDMETHOD.

  METHOD  yif_a2g_json~set_default.
* Do Nothing no attribute ad this level
  ENDMETHOD.

ENDCLASS.
