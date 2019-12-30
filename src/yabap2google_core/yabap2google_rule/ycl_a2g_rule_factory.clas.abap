"!<h1>YIF_A2G_RULE_FACTORY</h1>
"! <p class="shorttext synchronized" lang="en">Cmp. Appl - Abap 2 Google - Simple Rule Factory</p>
"!<p>This class is the builder class of all checks class.</p>
CLASS ycl_a2g_rule_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Constant scope suffix
    CONSTANTS gc_scope TYPE string VALUE 'Scope'     ##NO_TEXT.
    "! Constant field suffix
    CONSTANTS gc_field TYPE string VALUE 'Fieldname' ##NO_TEXT.

    "! constant scope create
    CONSTANTS gc_scope_create TYPE char1 VALUE 'H' ##NO_TEXT.
    "! constant scope change
    CONSTANTS gc_scope_change TYPE char1 VALUE 'V' ##NO_TEXT.
    "! constant scope full
    CONSTANTS gc_scope_full   TYPE char1 VALUE 'F' ##NO_TEXT.
    "! constant scope Delete
    CONSTANTS gc_scope_delete TYPE char1 VALUE 'L' ##NO_TEXT.

    "! This Method is the costructori of the class
    "! @parameter if_msg_manager   | message manager
    METHODS constructor
      IMPORTING !if_msg_manager TYPE REF TO yif_a2g_msg_manager .

    "! This Method return an instance of the check to execute or manage
    "! @parameter i_fieldname | name of the field
    "! @parameter Return      | return the instance of the checks
    METHODS get_rule_by_field
      IMPORTING !i_fieldname  TYPE        string
      RETURNING VALUE(return) TYPE REF TO yif_a2g_rule .

    "! This Method register the rule for the field
    "! @parameter i_fieldname | name of the filed
    "! @parameter if_rule     | instance of the rule to assign
    METHODS register_rule_for_field
      IMPORTING !i_fieldname TYPE string
                !if_rule     TYPE REF TO yif_a2g_rule .

    "! This Method get an instance of the single rule
    "! @parameter im_classname | name of the class inherinting from Ycl_a2g_simple_rule
    "! @parameter Return       | instance of the rule generated
    METHODS get_rule
      IMPORTING !im_classname TYPE string
      RETURNING VALUE(return) TYPE REF TO yif_a2g_rule .

    "! This Method returns the list of the rule of the scope
    "! @parameter i_scope    | Scope transaction type
    "! @parameter return     | list of rule by scope
    METHODS get_rules_by_scope
      IMPORTING !i_scope      TYPE trtyp
      RETURNING VALUE(return) TYPE ya2google_t_rules .

    "! This Method assign a scope tu a rule for the scope checks
    "! @parameter i_scope    | Scope transaction type
    "! @parameter if_rule    | Rule instances
    METHODS set_scope
      IMPORTING !i_scope TYPE        trtyp
                !if_rule TYPE REF TO yif_a2g_rule .

    "! name value structure type
    TYPES: BEGIN OF ty_s_value ,
             name  TYPE string,
             value TYPE string,
           END OF ty_s_value.

  PROTECTED SECTION.

    "! list of rules
    DATA go_rules        TYPE REF TO ycl_a2g_array .
    "! message manager
    DATA gif_msg_manager TYPE REF TO yif_a2g_msg_manager .
    "! name value
    DATA gs_value TYPE                   ty_s_value.
    "! name value list
    DATA gt_value TYPE STANDARD TABLE OF ty_s_value INITIAL SIZE 0.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_a2g_rule_factory IMPLEMENTATION.

  METHOD constructor.
    "&  Declaration Part
    "&  Source Part
    me->gif_msg_manager ?= if_msg_manager.
    me->go_rules = NEW #( ).
  ENDMETHOD.                    "constructor


  METHOD get_rule.
    "&  Declaration Part
    DATA: lv_classname TYPE string.

    "&  Source Part
    lv_classname = im_classname.
    TRANSLATE lv_classname TO UPPER CASE.

    CREATE OBJECT return TYPE (lv_classname)
      EXPORTING
        if_msg_manager = me->gif_msg_manager.

  ENDMETHOD.                    "get_rule


  METHOD get_rules_by_scope.
    "&  Declaration Part
    DATA: lv_chk_rule TYPE string.

    "&  Source Part
    lv_chk_rule =  me->gc_scope && '-' && i_scope.
    LOOP AT me->go_rules->getallnamesofinstances( ) INTO DATA(lv_rule).
      CHECK lv_rule CS lv_chk_rule.
      APPEND CAST yif_a2g_rule( me->go_rules->getinstance( lv_rule ) ) TO return.
    ENDLOOP.

  ENDMETHOD.                    "get_rules_by_scope


  METHOD get_rule_by_field.
    "&  Declaration Part
    DATA: lv_rule TYPE string.

    "&  Source Part
    lv_rule = me->gc_field && '-' && i_fieldname .
    return ?= me->go_rules->getinstance( lv_rule ).

  ENDMETHOD.                    "get_rule_by_field


  METHOD register_rule_for_field.
    "&  Declaration Part
    DATA: lv_rule TYPE string.
    DATA: lo_object TYPE REF TO object.

    "&  Source Part
    lv_rule = me->gc_field && '-' && i_fieldname .
    lo_object ?= me->go_rules->getinstance( lv_rule ).

    IF lo_object IS BOUND.
      me->go_rules->deleteinstance( lv_rule ).
      FREE lo_object.
    ENDIF.

    lo_object ?= if_rule.
    me->go_rules->setinstance( im_name   = lv_rule
                               im_object = lo_object ).

  ENDMETHOD.                    "register_rule_for_field


  METHOD set_scope.
    "&  Declaration Part
    DATA: lv_rule    TYPE string,
          lt_rules   TYPE ya2google_t_strings,
          lv_value   TYPE string,
          lv_num     TYPE numc2,
          lv_num_str TYPE string,
          lo_object  TYPE REF TO object.

    "&  Source Part
    lv_value =  me->gc_scope && '-' && i_scope .

    TRY.
        lv_num_str = me->gt_value[ name  = lv_value ]-value.
        lv_num = lv_num_str.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    ADD 1 TO lv_num.
    lv_rule = me->gc_scope && '-' && i_scope && '-' && lv_num.

    lo_object ?= me->go_rules->getinstance( lv_rule ).
    CLEAR lv_num_str.
    lv_num_str = lv_num.

    READ TABLE me->gt_value ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY name  = lv_value.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO me->gt_value ASSIGNING <fs>.
      <fs>-name = lv_value.
    ENDIF.

    <fs>-value = lv_num_str.

    IF lo_object IS BOUND.
      me->go_rules->deleteinstance( lv_rule ).
      FREE lo_object.
    ENDIF.

    lo_object ?= if_rule.
    me->go_rules->setinstance( im_name   = lv_rule
                               im_object = lo_object ).
  ENDMETHOD.                    "set_scope
ENDCLASS.
