"!<h1>YIF_A2G_MSG_MANAGER</h1>
"! <p class="shorttext synchronized" lang="en">Cmp. Appl - Abap 2 Google - Message Metadata ABS</p>
"!<p>This class is used as a model to manage the various message of an application.</p>
CLASS ycl_a2g_meta_msg DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED .

  PUBLIC SECTION.

    "! Message Class
    DATA gv_msgid TYPE symsgid READ-ONLY .
    "! Message Number
    DATA gv_msgno TYPE symsgno READ-ONLY .
    "! Message Variable 1
    DATA gv_attr1 TYPE symsgv READ-ONLY .
    "! Message Variable 2
    DATA gv_attr2 TYPE symsgv READ-ONLY .
    "! Message Variable 3
    DATA gv_attr3 TYPE symsgv READ-ONLY .
    "! Message Variable 4
    DATA gv_attr4 TYPE symsgv READ-ONLY .
    "! Message type
    DATA gv_msgty TYPE symsgty .

    "! Setter for GV_MSGID
    "! @parameter I_MSGID       | Mesage class
    METHODS set_msgid
      IMPORTING !i_msgid TYPE symsgid .

    "! Setter for GV_MSGno
    "! @parameter I_MSGno       | Mesage number
    METHODS set_msgno
      IMPORTING !i_msgno TYPE symsgno .

    "! Setter for GV_attr1
    "! @parameter I_attr1       | Message Variable 1
    METHODS set_attr1
      IMPORTING !i_attr1 TYPE symsgv .

    "! Setter for GV_attr2
    "! @parameter I_attr2       | Message Variable 2
    METHODS set_attr2
      IMPORTING !i_attr2 TYPE symsgv .

    "! Setter for GV_attr3
    "! @parameter I_attr3       | Message Variable 3
    METHODS set_attr3
      IMPORTING !i_attr3 TYPE symsgv .

    "! Setter for GV_attr4
    "! @parameter I_attr4       | Message Variable 4
    METHODS set_attr4
      IMPORTING !i_attr4 TYPE symsgv .

    "! Setter for GV_msgty
    "! @parameter I_msgty       | Message type
    METHODS set_msgty
      IMPORTING !i_msgty TYPE symsgty .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_a2g_meta_msg IMPLEMENTATION.

  METHOD set_attr1.
    me->gv_attr1 = i_attr1.
  ENDMETHOD.                                                "SET_ATTR1

  METHOD set_attr2.
    me->gv_attr2 = i_attr2.
  ENDMETHOD.                                                "SET_ATTR2

  METHOD set_attr3.
    me->gv_attr3 = i_attr3.
  ENDMETHOD.                                                "SET_ATTR3

  METHOD set_attr4.
    me->gv_attr4 = i_attr4.
  ENDMETHOD.                                                "SET_ATTR4

  METHOD set_msgid.
    me->gv_msgid = i_msgid.
  ENDMETHOD.                    "SET_MSGID

  METHOD set_msgno.
    me->gv_msgno = i_msgno.
  ENDMETHOD.                    "SET_MSGNO

  METHOD set_msgty.
    me->gv_msgty = i_msgty.
  ENDMETHOD.                    "SET_MSGTY
ENDCLASS.
