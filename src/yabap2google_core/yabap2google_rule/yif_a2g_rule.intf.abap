"!<h1>YIF_A2G_RULE</h1>
"! <p class="shorttext synchronized" lang="en">Cmp. Appl - Abap 2 Google - Rule Interface</p>
"!<p>This class is used to manage the various message of an application.</p>
INTERFACE yif_a2g_rule
  PUBLIC .

  "! This Method assign a dependencies between the field and the class of checks
  "! @parameter io_rule_factory | Cmp. Appl - Abap 2 Google - Simple Rule Factory
  "! @parameter i_fieldname     | Fild name where link the check
  METHODS assign_dependencies
    IMPORTING !io_rule_factory TYPE REF TO ycl_a2g_rule_factory
              !i_fieldname     TYPE        string .

  "! This Method assign a secondary check to the class
  "! @parameter if_rule   | Cmp. Appl - Abap 2 Google - Rule Interface
  METHODS attach
    IMPORTING !if_rule TYPE REF TO yif_a2g_rule .


  "! This Method Execute the checks registers
  "! @parameter i_context     | Cmp. Appl - Abap 2 Google - Context element
  "! @parameter i_recheck     | do the check again
  METHODS execute
    IMPORTING !i_context TYPE REF TO yif_a2g_context
              !i_recheck TYPE        oax            OPTIONAL
    RAISING   ycx_a2g_rule .

  "! This Method set the fieldname as mandatary
  METHODS set_mandatary .

ENDINTERFACE.
