interface ZIF_PM_DATA
  public .


  types:
    BEGIN OF ts_log,
      type     TYPE string,
      message  TYPE string,
      ui5_type TYPE string,
    END OF ts_log .
  types:
    tt_log TYPE STANDARD TABLE OF ts_log WITH DEFAULT KEY .
  types:
    BEGIN OF ts_attachment_list ,
      document_id   TYPE string,
      document_desc TYPE string,
      file_ext      TYPE string,
      creation_date TYPE string,
      creation_time TYPE string,
      creation_user TYPE string,
      ui5_icon      TYPE string,
      guid          TYPE guid_32,
      counter       TYPE  numc4,
    END OF ts_attachment_list .
  types:
    tt_attachment_list TYPE STANDARD TABLE OF ts_attachment_list WITH DEFAULT KEY .

  constants:
    BEGIN OF cs_msg_type,
      error   TYPE bapi_mtype VALUE 'E',
      warning TYPE bapi_mtype VALUE 'W',
      success TYPE bapi_mtype VALUE 'S',
      info    TYPE bapi_mtype VALUE 'I',
    END OF cs_msg_type .
  constants:
    BEGIN OF cs_ui5_msg_type,
      error   TYPE string VALUE 'Error',
      warning TYPE string VALUE 'Warning',
      success TYPE string VALUE 'Success',
      info    TYPE string VALUE 'Information',
    END OF cs_ui5_msg_type .
  constants CS_MESSAGE_ID type MSGID value 'ZPM_APPS' ##NO_TEXT.
  constants:
    BEGIN OF cs_center,
      villagarcia TYPE werks_d VALUE '1004',
      lugo        TYPE werks_d VALUE '1002',
      granda      TYPE werks_d VALUE '1001',
    END OF cs_center .
  constants CV_PTBO_ST type J_STATUS value 'I0076' ##NO_TEXT.
  constants CV_PTBO_ST2 type J_STATUS value 'I0320' ##NO_TEXT.
  constants CV_WAREHOUSE type LGORT value 'R001' ##NO_TEXT.
  constants CV_MOV_201 type BWART value '201' ##NO_TEXT.
  constants CV_TCODE_MB1A type TCODE value 'MB1A' ##NO_TEXT.
  constants CV_KOKRS_3400 type KOKRS value '3400' ##NO_TEXT.
endinterface.
