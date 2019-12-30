"!<h1>YIT_A2G_JSON</h1>
"! <p class="shorttext synchronized" lang="en">Cmp. Appl. - Abap 2 Google - Json Base Interface</p>
INTERFACE yif_a2g_json
  PUBLIC .

  INTERFACES yif_a2g_json_context.
  INTERFACES yif_a2g_serialize.


  "! Get the abap structure
  "! @parameter return    |   abap data structure
  METHODS  get_abap
    RETURNING VALUE(return) TYPE REF TO data.

  "! Set the attribute of the json model
  "! @parameter i_name    |   Name of the attribute to change
  "! @parameter i_value   |   Value ammitted to the atribute
  METHODS  set_attribute
    IMPORTING !i_name  TYPE        string
              !i_value TYPE REF TO data .

  "! Get the attribute of the json model
  "! @parameter i_name    |   Name of the attribute to change
  "! @parameter return    |   Value ammitted to the atribute
  METHODS  get_attribute
    IMPORTING !i_name       TYPE        string
    RETURNING VALUE(return) TYPE REF TO data.

  "! Set the element of the json model
  "! @parameter i_name    |   Name of the attribute to change
  "! @parameter return    |   is an instance of this interface for a specific element
  METHODS  new_element
    IMPORTING !i_name       TYPE        string
    RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  "! Get the element of the json model
  "! @parameter i_name    |   Name of the attribute to change
  "! @parameter i_enum    |   in case of array the number of the item
  "! @parameter return    |   is an instance of this interface for a specific element
  METHODS  get_element
    IMPORTING !i_name       TYPE        string
              !i_enum       TYPE        string       OPTIONAL
    RETURNING VALUE(return) TYPE REF TO yif_a2g_json.

  "! Set the default value of subitem
  METHODS set_default.


ENDINTERFACE.
