"!<h1>YIF_A2G_SIMPLE_RULE</h1>
"! <p class="shorttext synchronized" lang="en">Cmp. Appl - Abap 2 Google - Simple Rule</p>
"!<p>This class in an abstract class for all simple checks class needs.</p>
CLASS ycl_a2g_simple_rule DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_a2g_rule .

    "! This Method create the instance ( User the Factory class to instantiate all class
    "! @parameter if_msg_manager  | Cmp. Appl. - Abap 2 Google - Message Manager interface
    METHODS constructor
      IMPORTING
        !if_msg_manager TYPE REF TO yif_a2g_msg_manager OPTIONAL .

    "! True
    CONSTANTS gc_true  TYPE oax VALUE 'X' ##NO_TEXT.
    "! False
    CONSTANTS gc_false TYPE oax VALUE ' ' ##NO_TEXT.


  PROTECTED SECTION.

    "! Flag data changed
    DATA is_changed       TYPE        oax VALUE ' '         ##NO_TEXT.
    "! Array for dependant instance for top down checks
    DATA go_dependant     TYPE REF TO ycl_a2g_array .
    "! Message manager
    DATA gif_msg_manager  TYPE REF TO yif_a2g_msg_manager .
    "! Filed old value
    DATA old_value        TYPE REF TO data .
    "! FActory for rule management
    DATA go_rule_factory  TYPE REF TO ycl_a2g_rule_factory .
    "! name of the field to check
    DATA gv_fieldname     TYPE        string .
    "! Mandatary fields
    DATA gv_mandatary     TYPE        oax .

    "! Constats to individuate the Observer
    CONSTANTS gc_observ TYPE string VALUE 'Observ'          ##NO_TEXT.
    "! Constats to individuate the precondition
    CONSTANTS gc_precon TYPE string VALUE 'Precond'         ##NO_TEXT.

    "! This Method Checks the validity of the checks
    "! @parameter if_context   | Context od data checks
    "! @raising   ycx_a2g_rule | Rule Exception
    METHODS is_valid ABSTRACT
      IMPORTING !if_context TYPE REF TO yif_a2g_context
      RAISING   ycx_a2g_rule .


    "! This Method Checks if the input is changed
    "! @parameter if_context   | Context of data checks
    "! @parameter return       | identify if there are some changes
    METHODS has_input_changed ABSTRACT
      IMPORTING !if_context   TYPE REF TO yif_a2g_context
      RETURNING VALUE(return) TYPE oax .

    "! This Method  get the dependencies of the checks
    METHODS get_dependencies .

    "! This Method Add the precondition to the class
    "! @parameter if_rule   | Instance of the checks to add as precondition
    METHODS add_precondition
      IMPORTING !if_rule TYPE REF TO yif_a2g_rule .

    "! This Method checks il the field is mandatary
    "! @parameter if_context   | Context of data checks
    "! @raising   ycx_a2g_rule | Rule Exception field is manadatary
    METHODS is_mandatary
      IMPORTING !if_context TYPE REF TO yif_a2g_context
      RAISING   ycx_a2g_rule .

  PRIVATE SECTION.

    "! Internal progressive ids
    DATA gv_num TYPE numc4 .

    "! This Method give back the next free number
    "! @parameter return     | Next IDs
    METHODS get_next_number
      RETURNING VALUE(return) TYPE numc4 .

ENDCLASS.



CLASS ycl_a2g_simple_rule IMPLEMENTATION.

  METHOD add_precondition.
"&  Declaration Part
    DATA: lv_name   TYPE        string.
    DATA: lo_object TYPE REF TO object.

"&  Source Part
    DATA(lv_seqno) = me->get_next_number( ).
    lv_name = me->gc_precon && lv_seqno.
    lo_object ?= if_rule.
    me->go_dependant->setinstance( im_name   = lv_name
                                   im_object = lo_object ).

  ENDMETHOD.                    "add_precondition


  METHOD constructor.
"&  Declaration Part
"&  Source Part
    me->gif_msg_manager ?= if_msg_manager.
    me->go_dependant = NEW #( ).
  ENDMETHOD.                    "constructor


  METHOD get_dependencies.
  ENDMETHOD.                    "GET_DEPENDENCIES


  METHOD get_next_number.
"&  Declaration Part
"&  Source Part
    ADD 1 TO me->gv_num.
    return = me->gv_num.
  ENDMETHOD.                    "GET_NEXT_NUMBER


  METHOD is_mandatary.
"&  Declaration Part
    FIELD-SYMBOLS <fs_ref_check> TYPE any.
    DATA: ls_t100key TYPE scx_t100key.

"&  Source Part
    IF me->gv_mandatary = me->gc_true.
      ASSIGN old_value->* TO <fs_ref_check>.
      CHECK sy-subrc = 0.
      IF <fs_ref_check> IS INITIAL.

        ls_t100key-msgid = 'YA2GRULE'.
        ls_t100key-msgno = '000'.
        ls_t100key-attr1 = me->gv_fieldname.
        me->gif_msg_manager->register( im_t100key = ls_t100key
                                       im_msgty   = me->gif_msg_manager->gc_error ).

        RAISE EXCEPTION TYPE ycx_a2g_rule
          EXPORTING
            textid = ls_t100key.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "is_mandatary


  METHOD yif_a2g_rule~assign_dependencies.
"&  Declaration Part
    DATA: lif_rule TYPE REF TO yif_a2g_rule.

"&  Source Part
    me->go_rule_factory ?= io_rule_factory.
    me->gv_fieldname = i_fieldname.
    lif_rule ?= me.
    me->go_rule_factory->register_rule_for_field( if_rule     = lif_rule
                                                  i_fieldname = me->gv_fieldname ).

    me->go_rule_factory->set_scope( i_scope = me->go_rule_factory->gc_scope_full
                                    if_rule = lif_rule ).

  ENDMETHOD.                    "yif_a2g_rule~assign_dependencies


  METHOD yif_a2g_rule~attach.
"&  Declaration Part
    DATA: lv_name   TYPE        string,
          lo_object TYPE REF TO object.

"&  Source Part
    IF if_rule IS BOUND.
      DATA(lv_seqno) = me->get_next_number( ).
      lv_name = me->gc_observ && lv_seqno.
      lo_object ?= if_rule.
      me->go_dependant->setinstance( im_name   = lv_name
                                     im_object = lo_object ).
    ENDIF.
  ENDMETHOD.                    "yif_a2g_rule~attach


  METHOD yif_a2g_rule~execute.
"&  Declaration Part
    DATA: lv_changed TYPE        oax.
    DATA: lif_rule   TYPE REF TO yif_a2g_rule.
    DATA: lv_exist   TYPE        xfeld.

"&  Source Part
    IF i_recheck =  me->gc_true.
      lv_changed = i_recheck.
    ELSE.
      lv_changed = me->has_input_changed( i_context ).
    ENDIF.
    IF lv_changed = me->gc_true.

*** Check precoditions
      TRY.
          LOOP AT me->go_dependant->getpartnamesofinstances( im_substr = me->gc_precon ) INTO DATA(lv_name) .
            CAST yif_a2g_rule( me->go_dependant->getinstance( lv_name ) )->execute( i_context ).
          ENDLOOP.

          me->is_valid( i_context ).
          me->is_mandatary( i_context ).

* Waterfall checks.
          LOOP AT me->go_dependant->getpartnamesofinstances( im_substr = me->gc_observ ) INTO lv_name.
            TRY.
                CAST yif_a2g_rule( me->go_dependant->getinstance( lv_name ) )->execute( i_context = i_context
                                                                                        i_recheck = 'X' ).
              CATCH ycx_a2g_rule INTO DATA(ox_rule).
*** Missing .... Register error after waterfall checks. after raise ycx_a2g_rule excption again
            ENDTRY.
          ENDLOOP.

        CATCH ycx_a2g_rule INTO ox_rule.
          RAISE EXCEPTION TYPE ycx_a2g_rule
            EXPORTING
              textid = ox_rule->if_t100_message~t100key.
      ENDTRY.
    ENDIF.

  ENDMETHOD.                    "yif_a2g_rule~execute


  METHOD yif_a2g_rule~set_mandatary.
"&  Declaration Part
"&  Source Part
    me->gv_mandatary = 'X'.
  ENDMETHOD.                    "yif_a2g_rule~SET_MANDATARY
ENDCLASS.
