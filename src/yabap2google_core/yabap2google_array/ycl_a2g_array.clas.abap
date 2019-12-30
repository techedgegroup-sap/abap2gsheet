"!<h1>YCL_A2G_ARRAY</h1>
"!<p>This class is used to manage an object list using an Hashed table .</p>
class YCL_A2G_ARRAY definition
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! This Method save an object into the class
    "! @parameter im_name       | Key Name of Object
    "! @parameter im_object     | istance of object to store
    METHODS setinstance
      IMPORTING !im_name   TYPE        string
                !im_object TYPE REF TO object .

    "! This Method retrive an object into the class
    "! @parameter im_name       | Key Name of Object
    "! @parameter return        | istance of object requested
    METHODS getinstance
      IMPORTING !im_name      TYPE        string
      RETURNING VALUE(return) TYPE REF TO object .

    "! This Method delete a specific nstance from the class
    "! @parameter im_name       | Key Name of Object to delete
    METHODS deleteinstance
      IMPORTING !im_name TYPE string .

    "! This Method return a list of all object stored
    "! @parameter return       | List of instance
    METHODS getallinstaces
      RETURNING VALUE(return) TYPE ya2google_t_objects .

    "! This Method return a list of all name of object stored
    "! @parameter return       | List of the key of the instance
    METHODS getallnamesofinstances
      RETURNING VALUE(return) TYPE ya2google_t_strings .

    "! This Method reset all objects stored
    METHODS deleteinstances .

    "! This Method return a list of all object stored
    "! @parameter return       | List of instance
    METHODS getpartnamesofinstances
      IMPORTING !im_substr    TYPE string
      RETURNING VALUE(return) TYPE ya2google_t_strings .
  PROTECTED SECTION.
  PRIVATE SECTION.

    "! Hashed listo of objects
    DATA gt_nameobject TYPE ya2google_t_objarray .
ENDCLASS.



CLASS YCL_A2G_ARRAY IMPLEMENTATION.

  METHOD deleteinstance.
"&  Declaration Part
"&  Source Part
    DELETE me->gt_nameobject WHERE name = im_name.

  ENDMETHOD.                    "DELETEOBJECT


  METHOD deleteinstances.
"&  Declaration Part
"&  Source Part
    CLEAR me->gt_nameobject.

  ENDMETHOD.                    "DELETEOBJECTS


  METHOD getallnamesofinstances.
"&  Declaration Part
"&  Source Part
    LOOP AT me->gt_nameobject INTO DATA(ls_line).
      APPEND ls_line-name TO return.
    ENDLOOP.

  ENDMETHOD.                    "GETALLNAMESOFOBJECTS


  METHOD getallinstaces.
"&  Declaration Part
"&  Source Part
    LOOP AT me->gt_nameobject INTO DATA(ls_line).
      APPEND ls_line-object TO return.
    ENDLOOP.

  ENDMETHOD.                    "GETALLOBJECTS


  METHOD getinstance.
"&  Declaration Part
"&  Source Part
    TRY.
        DATA(ls_line) = me->gt_nameobject[ name = im_name ].
        return = ls_line-object.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

  ENDMETHOD.                    "GETOBJECT


  METHOD getpartnamesofinstances.
"&  Declaration Part
"&  Source Part
    LOOP AT me->gt_nameobject ASSIGNING FIELD-SYMBOL(<fs_line>) WHERE name CS im_substr.
      APPEND <fs_line>-name TO return.
    ENDLOOP.
  ENDMETHOD.                    "GETPARTNAMESOFOBJECTS


  METHOD setinstance.
"&  Declaration Part
    DATA: ls_line TYPE ya2google_s_objarray.

"&  Source Part
    ls_line-name   = im_name.
    ls_line-object = im_object.
    DELETE TABLE me->gt_nameobject WITH TABLE KEY name = im_name.
    INSERT ls_line INTO TABLE me->gt_nameobject.

  ENDMETHOD.                    "SETOBJECT
ENDCLASS.
