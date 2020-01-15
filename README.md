# abap2gsheet
ABAP framework that abstract the Google Sheets API

For more information on this please refer to the [a SCN BLOG](https://blogs.sap.com/2020/01/13/google-sheet-api-integration-with-abap-abap2gsheet-join-to-the-github-project/)

# Pre-requirements

We are targeting to support ABAP2GSheet on SAP S/4HANA 1809 and above (obviously only on-premise).

Since we count on the SAP standard authentication method to Google API (SAP oauth2 client), we have at least the same prerequisites that are:
  - SAP oauth2 client is only available for SAP NetWeaver starting from AS ABAP 7.40 SP08 (Note 2043775 must be applied).
  - As a prerequisite the system administrator needs authorizations to create OAuth 2.0 Client Configurations. Make sure that the system administrator has the authorization S_OA2C_ADM with at least the activities 01, 02 and 03 in the AS ABAP system.
  - Make sure that the end users who should be allowed to use the new OAuth 2.0 Client have the required authorizations assigned. During execution of OAuth 2.0 flows there is a check of the authorization object “S_OA2C_USE”. This authorization object has two fields “PROFILE” and “ACTVT”.
  - Set the authorization field values as follows:
S_OA2C_USE
PROFILE       = Z_GOOGLE_SHEETS
ACTVT           = 16

Setup the Google API Endpoint (see chapter 2.2 in [Export of ALV Grid Data to Google Sheets](https://www.sap.com/documents/2018/07/56e0dd6d-0f7d-0010-87a3-c30de2ffd8ff.html) )
