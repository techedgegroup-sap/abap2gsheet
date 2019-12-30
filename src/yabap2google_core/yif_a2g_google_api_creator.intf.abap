INTERFACE yif_a2g_google_api_creator
  PUBLIC .

  METHODS create_instance
    RETURNING VALUE(return) TYPE REF TO yif_a2g_google_api.
ENDINTERFACE.
