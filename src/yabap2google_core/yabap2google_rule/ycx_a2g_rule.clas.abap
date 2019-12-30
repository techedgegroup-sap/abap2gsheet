 "!<h1>YCX_A2G_RULE</h1>
 "! Cmp. Appl. - Abap 2 Google - rule Exception
 "!<p>This class is used to manage the exception triggereb into the rule manager.</p>
CLASS ycx_a2g_rule DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    "! This Method is the costructor of class
    "! @parameter textid       | Key Name of Object
    "! @parameter previous     | istance of object to store
    METHODS constructor
      IMPORTING !textid   LIKE if_t100_message=>t100key OPTIONAL
                !previous LIKE previous                 OPTIONAL .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycx_a2g_rule IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
"&  Declaration Part
"&  Source Part
    super->constructor( previous = previous ).

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
