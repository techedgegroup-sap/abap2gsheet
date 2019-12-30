"!<h1>YIF_A2G_OBJBASE</h1>
"! <p class="shorttext synchronized" lang="en">Cmp. Appl - Abap 2 Google - Object base</p>
"! <p>This class is the abstaction of base used to all component element. Not all operations are used
"! in all classes but each class reimplement anc use only the necessary operation</p>
CLASS ycl_a2g_objbase DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_a2g_command .
    INTERFACES yif_a2g_context .
    INTERFACES yif_a2g_serialize .
*    INTERFACES yif_a2g_trtyp .

    "! True
    CONSTANTS gc_true TYPE oax VALUE 'X' ##NO_TEXT.
    "! False
    CONSTANTS gc_flase TYPE oax VALUE '' ##NO_TEXT.


    "! This Method is the constructor
    "! @parameter if_msg_manager  | istance of message manager
    METHODS constructor
      IMPORTING !if_msg_manager TYPE REF TO yif_a2g_msg_manager .

    "! This Method rebuild the data from the referfence variable
    METHODS rebuild_data ABSTRACT .

  PROTECTED SECTION.

    "! Simple rule manager factory
    DATA go_rule_factory TYPE REF TO ycl_a2g_rule_factory .
    "! Message manager
    DATA go_msg_manager  TYPE REF TO yif_a2g_msg_manager .
    "! Reference of data structure to manage
    DATA gv_data         TYPE REF TO data .
    "! Reference of datax structure to manage for changes
    DATA gv_datax        TYPE REF TO data .

    "! This Method update the single fields and the relative flag changed
    "! @parameter i_fieldname  | Field to update
    "! @parameter i_value      | value to assign
    METHODS update_field
      IMPORTING !i_fieldname TYPE string
                !i_value     TYPE REF TO data .

    "! This Method generate the relevant rule for each fields
    METHODS generate_rules ABSTRACT .

    "! This Method execute the checks of single fields
    "! @parameter i_fieldname     | Fieldname to check
    "! @parameter i_recheck       | execute a new check
    "! @raising   ycx_a2g_objbase | general exception if the checs goes wrong
    METHODS check_field
      IMPORTING !i_fieldname TYPE string
                !i_recheck   TYPE oax       OPTIONAL
      RAISING   ycx_a2g_objbase .


    "! This Method execute the checks of the objet for a specific scope
    "! @raising   ycx_a2g_objbase | general exception if the checs goes wrong
    METHODS check_scope ABSTRACT
      RAISING ycx_a2g_objbase .

    "! This Method execute the full checks of the object
    "! @raising   ycx_a2g_objbase | general exception if the checs goes wrong
    METHODS check_scope_full
      RAISING ycx_a2g_objbase .

    "! This Method generate & register the simple rule checker
    "! @parameter i_classname | Simple rule class
    "! @parameter i_fieldname | field name for the selected class
    "! @parameter i_mandatary | Mandatary fields
    METHODS generate_rule
      IMPORTING !i_classname TYPE string
                !i_fieldname TYPE string
                !i_mandatary TYPE oax       OPTIONAL .
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_objbase IMPLEMENTATION.


  METHOD check_field.
    "&  Declaration Part
    DATA: lif_rule    TYPE REF TO yif_a2g_rule,
          lif_context TYPE REF TO yif_a2g_context.

    "&  Source Part
    lif_context ?= me.

    lif_rule ?= me->go_rule_factory->get_rule_by_field( i_fieldname ).
    TRY .
        lif_rule->execute( i_context = lif_context
                           i_recheck = i_recheck  ).

      CATCH ycx_a2g_rule INTO DATA(ox_rule).
        RAISE EXCEPTION TYPE ycx_a2g_objbase
          EXPORTING
            textid = ox_rule->if_t100_message~t100key.
      CATCH cx_root.
* the rule does not exist skip the checks
    ENDTRY.

  ENDMETHOD.                    "check_field


  METHOD check_scope_full.
    "&  Declaration Part
    DATA: lif_rule    TYPE REF TO yif_a2g_rule,
          lif_context TYPE REF TO yif_a2g_context.

    "&  Source Part
    lif_context ?= me.
    LOOP AT me->go_rule_factory->get_rules_by_scope( 'F' ) INTO lif_rule.
      TRY .
          lif_rule->execute( lif_context ).
        CATCH ycx_a2g_rule .
        CATCH cx_root.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.                    "CHECK_SCOPE_FULL


  METHOD constructor.
    "&  Declaration Part
    "&  Source Part
    me->go_rule_factory = NEW #( if_msg_manager = if_msg_manager ).
    me->go_msg_manager ?= if_msg_manager.

    IF NOT me->go_msg_manager IS BOUND.
      me->go_msg_manager = ycl_a2g_msg_manager=>create_init_msg_manager( ).
    ENDIF.

  ENDMETHOD.                    "constructor


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

  ENDMETHOD.                    "generate_rule


  METHOD update_field.
    "&  Declaration Part
    FIELD-SYMBOLS: <fs_structure>    TYPE any,
                   <fs_field>        TYPE any,
                   <fs_field_source> TYPE any.

    "&  Source Part
    ASSIGN me->gv_data->* TO <fs_structure>.
    ASSIGN COMPONENT i_fieldname OF STRUCTURE <fs_structure> TO <fs_field>.
    IF sy-subrc = 0.
      ASSIGN i_value->* TO <fs_field_source>.
      <fs_field> = <fs_field_source>.
    ENDIF.
      UNASSIGN: <fs_structure>, <fs_field>.
      IF NOT me->gv_datax IS INITIAL.
        ASSIGN me->gv_datax->* TO <fs_structure>.
        ASSIGN COMPONENT i_fieldname OF STRUCTURE <fs_structure> TO <fs_field>.
        <fs_field> = me->gc_true.
      ENDIF.

  ENDMETHOD.                    "update_field


  METHOD yif_a2g_context~get_protocol.
    "&  Declaration Part
    "&  Source Part
    return ?= me->go_msg_manager.
  ENDMETHOD.                    "YIF_A2G_CONTEXT~GET_PROTOCOL


  METHOD yif_a2g_context~read_data.
    "&  Declaration Part
    "&  Source Part
    return = me->gv_data.
  ENDMETHOD.                    "YIF_A2G_CONTEXT~READ_DATA


  METHOD yif_a2g_context~write_data.
    "&  Declaration Part
    FIELD-SYMBOLS: <fs_store> TYPE any,
                   <fs_in>    TYPE any.

    "&  Source Part
    ASSIGN me->gv_data->* TO <fs_store>.
    ASSIGN input->* TO <fs_in>.
    <fs_store> = <fs_in>.

    me->rebuild_data( ).
      TRY.
          me->check_scope_full( ).
        CATCH ycx_a2g_objbase .
      ENDTRY.
  ENDMETHOD.                    "YIF_A2G_CONTEXT~write_data


  METHOD yif_a2g_command~execute.
    "&  Declaration Part
    "&  Source Part
    IF im_fcode = 'CHECK'.   me->check_scope( ). ENDIF.
  ENDMETHOD.                    "yif_a2g_command~execute


  METHOD yif_a2g_command~set_fcode.
    "&  Declaration Part
    "&  Source Part
    me->yif_a2g_command~gv_fcode = im_fcode.
  ENDMETHOD.                    "yif_a2g_command~SET_FCODE

ENDCLASS.
