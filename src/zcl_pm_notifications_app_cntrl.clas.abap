class ZCL_PM_NOTIFICATIONS_APP_CNTRL definition
  public
  final
  create public .

public section.

  interfaces /NEPTUNE/IF_NAD_SERVER .

  types:
    BEGIN OF ts_tmp_attach_guid,
        guid TYPE guid_32,
      END OF ts_tmp_attach_guid .
  types:
    BEGIN OF ts_document_url,
        url TYPE string,
      END OF ts_document_url .
  types:
    BEGIN OF ts_technical_hierarchy,
        node_id        TYPE string,
        node_parent_id TYPE string,
        node_type      TYPE string, "Equipo o ub. tecnica
        node_desc      TYPE string,
        highlight      TYPE string,
      END OF  ts_technical_hierarchy .
  types:
    tt_technical_hierarchy TYPE STANDARD TABLE OF ts_technical_hierarchy WITH DEFAULT KEY .
  types:
    BEGIN OF ts_notif_detail,
        status                  TYPE string,
        status_desc             TYPE string,
*        status_icon             TYPE string,
        can_create_order        TYPE abap_bool,
        editable                TYPE abap_bool,
        production_stop_yes     TYPE abap_bool,
        production_stop_no      TYPE abap_bool,
        placement               TYPE string,
        placement_desc          TYPE string,
        equipment               TYPE string,
        equipment_desc          TYPE string,
        tec_location            TYPE string,
        tec_location_desc       TYPE string,
        notif_description       TYPE string,
*        notif_long_description  TYPE string,
        job                     TYPE string,
        job_desc                TYPE string,
        user                    TYPE string,
        username                TYPE string,
        notification_id         TYPE string,
        notification_class      TYPE string,
        notification_class_desc TYPE string,
        order                   TYPE string,
*        order_type              TYPE string,
*        order_type_desc         TYPE string,
*        priority                TYPE string,
*        priority_desc           TYPE string,
*        codification            TYPE string,
*        codification_desc       TYPE string,
        desired_start_date      TYPE string,
        desired_end_date        TYPE string,
        center                  TYPE string,
*-->    CAPSA - 25.10.21: Fecha/Hora aviso
        notif_date              TYPE string,
        notif_time              TYPE string,
*<--    CAPSA - 25.10.21

      END OF ts_notif_detail .
  types:
    BEGIN OF ts_master_list,
        status                  TYPE string,
        status_icon             TYPE string,
        can_create_order        TYPE abap_bool,
        placement               TYPE string,
        placement_desc          TYPE string,
        tec_location            TYPE string,
        tec_location_desc       TYPE string,
        equipment               TYPE string,
        equipment_desc          TYPE string,
        notif_description       TYPE string,
        job                     TYPE string,
        job_desc                TYPE string,
        user                    TYPE string,
        username                TYPE string,
        notification_id         TYPE string,
        notification_class      TYPE string,
        notification_class_desc TYPE string,
        order                   TYPE string,
        order_class             TYPE string,
        order_class_desc        TYPE string,
*        priority                TYPE string,
*        priority_desc           TYPE string,
        desired_start_date      TYPE string,
        desired_end_date        TYPE string,
        has_attachment          TYPE abap_bool,
        production_stop         TYPE abap_bool,
*-->    CAPSA - 25.10.21: Fecha/Hora aviso
        notif_date              TYPE string,
        notif_time              TYPE string,
*<--    CAPSA - 25.10.21
      END OF ts_master_list .
  types:
    tt_master_list TYPE STANDARD TABLE OF ts_master_list WITH DEFAULT KEY .
  types:
    BEGIN OF ts_search_filters,
        notification_id   TYPE string,
*        placement         TYPE string,
        tec_location      TYPE string,
        tec_location_desc TYPE string,
        equipment         TYPE string,
        equipment_desc    TYPE string,
*        user              TYPE string,
        date_from         TYPE string,
        date_to           TYPE string,
        shutdown          TYPE string,  " CAPSA - 25.10.21
        shutdown_desc     TYPE string,  " CAPSA - 25.10.21
      END OF ts_search_filters .
  types:
    BEGIN OF ts_key_value,
        key   TYPE string,
        value TYPE string,
      END OF ts_key_value .
  types:
    BEGIN OF ts_search_key,
        key TYPE string,
      END OF ts_search_key .
  types:
    tt_key_value TYPE STANDARD TABLE OF ts_key_value WITH DEFAULT KEY .
  types:
    tt_search_key TYPE STANDARD TABLE OF ts_search_key WITH DEFAULT KEY .
  types:
*    TYPES:
*      BEGIN OF ts_attachment_list ,
*        creation_date TYPE string,
*        creation_time TYPE string,
*        document_desc TYPE string,
*        file_ext      TYPE string,
*      END OF ts_attachment_list .
*    TYPES: tt_attachment_list TYPE STANDARD TABLE OF ts_attachment_list WITH DEFAULT KEY.
    BEGIN OF ts_handle_on_ajax ,
        applid     TYPE string,
        ajax_id    TYPE string,
        ajax_value TYPE string,
        server     TYPE REF TO /neptune/cl_nad_server,
        request    TYPE /neptune/data_request,
        navigation TYPE /neptune/ajax_navigation,
      END OF ts_handle_on_ajax .
  types:
    BEGIN OF ts_camera_file,
        filename TYPE string,
        content  TYPE string,
      END OF ts_camera_file .
  types:
    BEGIN OF ts_order_detail,
        order                   TYPE string,
        order_desc              TYPE string,
        order_class             TYPE string,
        order_class_desc        TYPE string,
        work_status             TYPE string,
        work_status_desc        TYPE string,
        material_status         TYPE string,
        material_status_desc    TYPE string,
        placement               TYPE string,
        placement_desc          TYPE string,
        tec_location            TYPE string,
        tec_location_desc       TYPE string,
        equipment               TYPE string,
        equipment_desc          TYPE string,
        job                     TYPE string,
        job_desc                TYPE string,
        user                    TYPE string,
        username                TYPE string,
        start_date              TYPE string,
        end_date                TYPE string,
        production_stop_yes     TYPE abap_bool,
        production_stop_no      TYPE abap_bool,
        creation_user           TYPE string,
        objnr                   TYPE objnr,

*       Permisos
        editable                TYPE abap_bool,
        job_editable            TYPE abap_bool,
        can_create_notification TYPE abap_bool,
        can_create_material_doc TYPE abap_bool,
        can_close_order         TYPE abap_bool,
        can_assign_oficial      TYPE abap_bool,

      END OF ts_order_detail .
  types:
    BEGIN OF ts_order_assigments,
        assigned      TYPE abap_bool,
        employee_code TYPE string,
        employee_name TYPE string,
        job_id        TYPE cr_objid,
        order         TYPE i,
      END OF ts_order_assigments .
  types:
    tt_order_assigments TYPE STANDARD TABLE OF ts_order_assigments WITH DEFAULT KEY .

  data MS_ON_AJAX type TS_HANDLE_ON_AJAX .
  data MS_USER_INFO type ZCL_PM_USER=>TS_USER .
  data MS_SEARCH_FILTERS type TS_SEARCH_FILTERS .
  data MT_MASTERDATA_STATUS type TT_KEY_VALUE .
  data MT_MASTERDATA_JOBS type TT_KEY_VALUE .
  data MT_MASTERDATA_PLACEMENT type TT_KEY_VALUE .
  data MT_MASTERDATA_PRIORITY type TT_KEY_VALUE .
  data MT_MASTERDATA_USER_PLACEMENT type TT_KEY_VALUE .
  data MT_MASTERDATA_NOTIF_CLASS type TT_KEY_VALUE .
  data MT_MASTERDATA_SHUTDOWN type TT_KEY_VALUE .
  data MT_SEARCH_STATUS type TT_SEARCH_KEY .
  data MT_SEARCH_JOBS type TT_SEARCH_KEY .
  data MT_SEARCH_PLACEMENT type TT_SEARCH_KEY .
  data MT_SEARCH_USER type TT_SEARCH_KEY .
  data MT_MASTER_LIST type TT_MASTER_LIST .
  data MS_NOTIFICATION_DETAIL type TS_NOTIF_DETAIL .
  data MT_HIERARCHY_EQUIPMENT type TT_TECHNICAL_HIERARCHY .
  data MT_HIERARCHY_TEC_LOCATION type TT_TECHNICAL_HIERARCHY .
  data MT_ATTACHMENT_LIST type ZIF_PM_DATA=>TT_ATTACHMENT_LIST .
  data MS_DOCUMENT_URL type TS_DOCUMENT_URL .
  data MT_LOG type ZIF_PM_DATA=>TT_LOG .
  data MT_WIZARD_FILES type ZIF_PM_DATA=>TT_ATTACHMENT_LIST .
  data MS_TMP_ATTACH_GUID type TS_TMP_ATTACH_GUID .
  data MS_CAMERA_FILE type TS_CAMERA_FILE .
  data MS_ORDER_DETAIL type ZCL_PM_ORDERS_APP_CNTRL=>TS_ORDER_DETAIL .
  data MT_ORDER_ASSIGNMENTS type TT_ORDER_ASSIGMENTS .
  data MT_MASTERDATA_ALL_JOBS type TT_KEY_VALUE .
  data MT_MASTERDATA_ORDERCLASS type TT_KEY_VALUE .
protected section.

  constants:
    BEGIN OF  ms_status_icon,
      pending         TYPE string VALUE 'sap-icon://bell',
      with_order      TYPE string VALUE 'sap-icon://employee',
      pending_release TYPE string VALUE 'sap-icon://pending',
      released        TYPE string VALUE 'sap-icon://approvals',
      completed       TYPE string VALUE 'sap-icon://accept',
    END OF ms_status_icon .

  methods AJAX_INIT_APLICATION .
  methods AJAX_GET_USER_INFO .
  methods AJAX_GET_MASTER_LIST .
  methods AJAX_GET_NOTIF_DETAIL .
  methods AJAX_GET_NOTIF_ATTACHEMNTS .
  methods AJAX_CREATE_NEW_NOTIF .
  methods AJAX_MODIFY_NOTIF .
  methods AJAX_GET_EQUIPMENT_HIERARCHY .
  methods AJAX_OPEN_ATTACHMENT .
  methods AJAX_GET_USER_FOR_PLACEMENT .
  methods AJAX_FILEUPLOADER_UPLOAD .
  methods AJAX_ADD_NOTIF_ATTACHMENT .
  methods AJAX_ADD_WIZARD_FILE .
  methods AJAX_ADD_NOTIF_PICTURE .
  methods AJAX_ADD_WIZARD_PICTURE .
  methods AJAX_DELETE_ATTACHMENT .
  methods AJAX_DELETE_TMP_ATTACHMENT .
  methods AJAX_RELEASE_ORDER .
  methods AJAX_PREPARE_ORDER_CREATION .
  methods AJAX_GET_OFICIAL_FOR_JOB .
  methods AJAX_CREATE_NEW_ORDER .
  methods AJAX_CREATE_AND_PRINT_ORDER .
private section.

  methods CONVERT_STATUS
    importing
      !IT_STATUS_CONVERSION type ZCL_PM_MASTERDATA=>TT_STATUS_CONVERSION
    returning
      value(RT_STATUS_CONVERSION) type ZCL_PM_MASTERDATA=>TT_STATUS_CONVERSION .
  methods CONVERT_NOTIF_LIST_OUT
    importing
      !IT_NOTIFICATION_HEADER type ZCL_PM_NOTIFICATION=>TT_NOTIFICATION_HEADER
      !IT_NOTIFICATION_STATUS type ZCL_PM_MASTERDATA=>TT_OBJECT_STATUS
      !IT_NOTIFICATION_ATTACHMENTS type ZCL_PM_ATTACHMENTS=>TT_GOS_ATTACHMENT_LIST
      !IT_ORDER_HEADER type ZCL_PM_ORDER=>TT_ORDER_HEADER
      !IT_ORDER_PARTNER type ZCL_PM_ORDER=>TT_ORDER_PARTNER
      !IT_ENDED_ORDERS type ZCL_PM_ORDER=>TT_ORDER_ID .
  methods CONVERT_NOTIF_DETAIL_OUT
    importing
      !IS_NOTIFICATION_HEADER type ZCL_PM_NOTIFICATION=>TS_NOTIFICATION_HEADER
      !IT_NOTIFICATION_STATUS type ZCL_PM_MASTERDATA=>TT_OBJECT_STATUS
      !IT_ORDER_PARTNER type ZCL_PM_ORDER=>TT_ORDER_PARTNER
      !IV_ORDER_ENDED type ABAP_BOOL default ABAP_FALSE .
  methods CONVERT_NOTIF_DETAIL_IN
    returning
      value(RS_NOTIFICATION_HEADER) type ZCL_PM_NOTIFICATION=>TS_NOTIFICATION_HEADER .
  methods CONVERT_ORDER_ASSIGNMENTS_OUT
    importing
      !IT_ORDER_PARTNER type ZCL_PM_ORDER=>TT_ORDER_PARTNER optional
      !IT_JOB_ID type STANDARD TABLE .
ENDCLASS.



CLASS ZCL_PM_NOTIFICATIONS_APP_CNTRL IMPLEMENTATION.


  METHOD /neptune/if_nad_server~handle_on_ajax.

*    DATA lv_mock TYPE abap_bool VALUE ''.

    ms_on_ajax = VALUE #( applid = applid
                          ajax_id = ajax_id
                          ajax_value = ajax_value
                          navigation = navigation
                          server = server
                          request = request ).

    DATA(ajax_method) = CONV seoclsname( |AJAX_{ ms_on_ajax-ajax_id }| ).

*    TRY.

*        CASE lv_mock.
*         Ejecucion normal
*          WHEN abap_false.
    CALL METHOD (ajax_method).

*         Mock
*          WHEN abap_true.
*            zcl_pm_notifications_app_mock=>ms_on_ajax = ms_on_ajax.
*            zcl_pm_notifications_app_mock=>controller = me.
*            CALL METHOD zcl_pm_notifications_app_mock=>(ajax_method).
*        ENDCASE.
*      CATCH cx_root.

*    ENDTRY.


  ENDMETHOD.


  METHOD ajax_add_notif_attachment.


    DATA: ls_neptune_file TYPE /neptune/wa_request_files,
          ls_file         TYPE zcl_pm_attachments=>ts_file.


*   Se guarda el fichero subido en el servidor para recogerlo en la llamada al servicio ADD_IMAGE
    ms_on_ajax-server->api_parameter_get( EXPORTING name = 'TMP_FILENAME'
                               CHANGING value = ls_neptune_file-filename ).

    ms_on_ajax-server->api_parameter_get( EXPORTING name = 'TMP_CONTENT'
                               CHANGING value = ls_neptune_file-content ).

*   Se guarda el fichero subido en el servidor para recogerlo en la llamada al servicio ADD_IMAGE
    ms_on_ajax-server->api_parameter_set( EXPORTING name = 'TMP_FILENAME'
                                         value = '' ).

    ms_on_ajax-server->api_parameter_set( EXPORTING name = 'TMP_CONTENT'
                                         value = '' ).

    IF ls_neptune_file IS NOT INITIAL.
      ls_file = CORRESPONDING #( ls_neptune_file ).

      DATA(lo_attachment) = NEW zcl_pm_attachments( ).
      lo_attachment->upload_file_2_gos( EXPORTING is_file = ls_file
                                                  iv_object_id = CONV #( ms_on_ajax-ajax_value )
                                                  iv_sap_object = CONV #( zcl_pm_attachments=>ms_bussines_object-notification ) ).

    ENDIF.

  ENDMETHOD.


  METHOD ajax_add_notif_picture.

    DATA: ls_file TYPE zcl_pm_attachments=>ts_file.

    IF ms_camera_file IS NOT INITIAL.
      ls_file = CORRESPONDING #( ms_camera_file ).

      CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
        EXPORTING
          input  = ms_camera_file-content
*         UNESCAPE       = 'X'
        IMPORTING
          output = ls_file-content
        EXCEPTIONS
          failed = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      DATA(lo_attachment) = NEW zcl_pm_attachments( ).
      lo_attachment->upload_file_2_gos( EXPORTING is_file = ls_file
                                                  iv_object_id = CONV #( ms_on_ajax-ajax_value )
                                                  iv_sap_object = CONV #( zcl_pm_attachments=>ms_bussines_object-notification ) ).

    ENDIF.

  ENDMETHOD.


  METHOD ajax_add_wizard_file.


    DATA:
      ls_neptune_file TYPE /neptune/wa_request_files,
      ls_file         TYPE zcl_pm_attachments=>ts_file.

*   Se guarda el fichero subido en el servidor para recogerlo en la llamada al servicio ADD_IMAGE
    ms_on_ajax-server->api_parameter_get( EXPORTING name = 'TMP_FILENAME'
                               CHANGING value = ls_neptune_file-filename ).

    ms_on_ajax-server->api_parameter_get( EXPORTING name = 'TMP_CONTENT'
                               CHANGING value = ls_neptune_file-content ).

*   Se guarda el fichero subido en el servidor para recogerlo en la llamada al servicio ADD_IMAGE
    ms_on_ajax-server->api_parameter_set( EXPORTING name = 'TMP_FILENAME'
                                         value = '' ).

    ms_on_ajax-server->api_parameter_set( EXPORTING name = 'TMP_CONTENT'
                                         value = '' ).

    IF ls_neptune_file IS NOT INITIAL.
      ls_file = CORRESPONDING #( ls_neptune_file ).

      DATA(lo_attachment) = NEW zcl_pm_attachments( ).

*     Si no hay guid se genera
      IF ms_tmp_attach_guid-guid IS INITIAL.
        ms_tmp_attach_guid-guid = lo_attachment->generate_tmp_guid( ).
      ENDIF.

*     Se graba el fichero en la tabla de temporales
      lo_attachment->save_tmp_attachment( EXPORTING iv_guid = ms_tmp_attach_guid-guid is_file = ls_file
                                          EXCEPTIONS error = 1
                                                     OTHERS = 2 ).
*      Se recuperan los ficheros de esta ejecucion
      DATA(lt_tmp_attachments) = lo_attachment->get_tmp_attachments( iv_guid = ms_tmp_attach_guid-guid  ).

*      Se pasa a la estructura del front
      LOOP AT lt_tmp_attachments ASSIGNING FIELD-SYMBOL(<ls_tmp_attachment>).
        APPEND INITIAL LINE TO mt_wizard_files ASSIGNING FIELD-SYMBOL(<ls_wizard_file>).
        <ls_wizard_file> = CORRESPONDING #( <ls_tmp_attachment> MAPPING document_desc = filename ).
        <ls_wizard_file>-ui5_icon = lo_attachment->get_ui5_icon_from_mimetype( <ls_tmp_attachment>-mimetype ).
      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD ajax_add_wizard_picture.

    DATA: ls_file         TYPE zcl_pm_attachments=>ts_file.


    IF ms_camera_file IS NOT INITIAL.
      ls_file = CORRESPONDING #( ms_camera_file ).

      CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
        EXPORTING
          input  = ms_camera_file-content
*         UNESCAPE       = 'X'
        IMPORTING
          output = ls_file-content
        EXCEPTIONS
          failed = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      DATA(lo_attachment) = NEW zcl_pm_attachments( ).

*     Si no hay guid se genera
      IF ms_tmp_attach_guid-guid IS INITIAL.
        ms_tmp_attach_guid-guid = lo_attachment->generate_tmp_guid( ).
      ENDIF.

*     Se graba el fichero en la tabla de temporales
      lo_attachment->save_tmp_attachment( EXPORTING iv_guid = ms_tmp_attach_guid-guid is_file = ls_file
                                          EXCEPTIONS error = 1
                                                     OTHERS = 2 ).
*      Se recuperan los ficheros de esta ejecucion
      DATA(lt_tmp_attachments) = lo_attachment->get_tmp_attachments( iv_guid = ms_tmp_attach_guid-guid  ).

*      Se pasa a la estructura del front
      LOOP AT lt_tmp_attachments ASSIGNING FIELD-SYMBOL(<ls_tmp_attachment>).
        APPEND INITIAL LINE TO mt_wizard_files ASSIGNING FIELD-SYMBOL(<ls_wizard_file>).
        <ls_wizard_file> = CORRESPONDING #( <ls_tmp_attachment> MAPPING document_desc = filename ).
        <ls_wizard_file>-ui5_icon = lo_attachment->get_ui5_icon_from_mimetype( <ls_tmp_attachment>-mimetype ).
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD ajax_create_and_print_order.

    DATA(lo_order) = NEW zcl_pm_order( ).
    DATA(lo_attachment) = NEW zcl_pm_attachments( ).
    DATA: ls_file  TYPE zcl_pm_attachments=>ts_file.
    DATA(lo_order_cntrl) = NEW zcl_pm_orders_app_cntrl( ).
          DATA lv_rqident TYPE  tsp01-rqident.

    DATA(ls_order_header) = lo_order_cntrl->convert_order_detail_in( ms_order_detail ).

* Se crea la orden
    " Informamos el centro del usuario
    ls_order_header-center = ms_user_info-center.
    lo_order->save_order(  EXPORTING iv_notif_id = CONV qmnum( ms_on_ajax-ajax_value ) is_order = ls_order_header it_assigments = mt_order_assignments IMPORTING et_return   = DATA(lt_return) ev_order_id = DATA(lv_order_id) ).

*  Si se ha creado se añaden los anexos
    IF lv_order_id IS NOT INITIAL.

      DATA(lt_tmp_attachment) = lo_attachment->get_tmp_attachments( iv_guid = ms_tmp_attach_guid-guid ).
      LOOP AT lt_tmp_attachment ASSIGNING FIELD-SYMBOL(<ls_tmp_attachment>).
        ls_file = CORRESPONDING #( <ls_tmp_attachment> ).
        lo_attachment->upload_file_2_gos( EXPORTING is_file = ls_file
                                                iv_object_id = CONV #( lv_order_id )
                                                iv_sap_object = CONV #( zcl_pm_attachments=>ms_bussines_object-order ) ).
      ENDLOOP.


      " Imprimir
      CALL FUNCTION 'ZPM_IMPRIMIR_OT'
        EXPORTING
          aufnr   = lv_order_id
          uname   = ms_user_info-username
        IMPORTING
          rqident = lv_rqident.

      APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<fs_log>).
      IF lv_rqident IS INITIAL.
        <fs_log>-type = zif_pm_data=>cs_msg_type-error.
        MESSAGE e012(zpm_apps) INTO <fs_log>-message. "Error al imprimir la orden.
      ELSE.
        <fs_log>-type = <fs_log>-type = zif_pm_data=>cs_msg_type-success.
        MESSAGE e011(zpm_apps) INTO <fs_log>-message. "Orden de impresión lanzada.
      ENDIF.

    ENDIF.

* Convertimos los mensajes para el tipo de la aplicación
    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( it_return = lt_return ).


  ENDMETHOD.


  METHOD ajax_create_new_notif.

    DATA(lo_notification) = NEW zcl_pm_notification( ).
    DATA(lo_attachment) = NEW zcl_pm_attachments( ).
    DATA: ls_file  TYPE zcl_pm_attachments=>ts_file.

    DATA(ls_notif_header) = convert_notif_detail_in( ).

* Se crea el aviso
    " Informamos el centro del usuario
    ls_notif_header-center = ms_user_info-center.
    lo_notification->save_notif( EXPORTING is_notif = ls_notif_header IMPORTING et_return = DATA(lt_return) ev_notification_id = DATA(lv_notification_id) ).

*  Si se ha creado se añaden los anexos
    IF lv_notification_id IS NOT INITIAL.

      DATA(lt_tmp_attachment) = lo_attachment->get_tmp_attachments( iv_guid = ms_tmp_attach_guid-guid ).
      LOOP AT lt_tmp_attachment ASSIGNING FIELD-SYMBOL(<ls_tmp_attachment>).
        ls_file = CORRESPONDING #( <ls_tmp_attachment> ).
        lo_attachment->upload_file_2_gos( EXPORTING is_file = ls_file
                                                iv_object_id = CONV #( lv_notification_id )
                                                iv_sap_object = CONV #( zcl_pm_attachments=>ms_bussines_object-notification ) ).
      ENDLOOP.

    ENDIF.

* Convertimos los mensajes para el tipo de la aplicación
    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( it_return = lt_return ).

  ENDMETHOD.


  METHOD ajax_create_new_order.


    DATA(lo_order) = NEW zcl_pm_order( ).
    DATA(lo_attachment) = NEW zcl_pm_attachments( ).
    DATA: ls_file  TYPE zcl_pm_attachments=>ts_file.
    DATA(lo_order_cntrl) = NEW zcl_pm_orders_app_cntrl( ).

    DATA(ls_order_header) = lo_order_cntrl->convert_order_detail_in( ms_order_detail ).

* Se crea la orden
    " Informamos el centro del usuario
    ls_order_header-center = ms_user_info-center.
    lo_order->save_order(  EXPORTING iv_notif_id = CONV qmnum( ms_on_ajax-ajax_value ) is_order = ls_order_header it_assigments = mt_order_assignments IMPORTING et_return   = DATA(lt_return) ev_order_id = DATA(lv_order_id) ).

*  Si se ha creado se añaden los anexos
    IF lv_order_id IS NOT INITIAL.

      DATA(lt_tmp_attachment) = lo_attachment->get_tmp_attachments( iv_guid = ms_tmp_attach_guid-guid ).
      LOOP AT lt_tmp_attachment ASSIGNING FIELD-SYMBOL(<ls_tmp_attachment>).
        ls_file = CORRESPONDING #( <ls_tmp_attachment> ).
        lo_attachment->upload_file_2_gos( EXPORTING is_file = ls_file
                                                iv_object_id = CONV #( lv_order_id )
                                                iv_sap_object = CONV #( zcl_pm_attachments=>ms_bussines_object-order ) ).
      ENDLOOP.

    ENDIF.

* Convertimos los mensajes para el tipo de la aplicación
    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( it_return = lt_return ).
  ENDMETHOD.


  METHOD ajax_delete_attachment.
    DATA lt_return TYPE bapiret2_t.

    DATA(lo_attachment) = NEW zcl_pm_attachments( ).
    lo_attachment->delete_file_gos( EXPORTING iv_gos_attachment_id = CONV so_entryid(  ms_on_ajax-ajax_value )
                                              iv_sap_object = CONV #( zcl_pm_attachments=>ms_bussines_object-notification )
                                    IMPORTING es_return = DATA(ls_return) ).
    IF ls_return IS NOT INITIAL.
      APPEND ls_return TO lt_return.
* Convertimos los mensajes para el tipo de la aplicación
      mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( it_return = lt_return ).
    ENDIF.
  ENDMETHOD.


  METHOD ajax_delete_tmp_attachment.

    DATA lt_return TYPE bapiret2_t.

    DATA(lo_attachment) = NEW zcl_pm_attachments( ).
    lo_attachment->delete_tmp_attachments( EXPORTING iv_tmp_file = CONV so_entryid(  ms_on_ajax-ajax_value )
                                           IMPORTING es_return = DATA(ls_return) ).
    IF ls_return IS NOT INITIAL.
      APPEND ls_return TO lt_return.
* Convertimos los mensajes para el tipo de la aplicación
      mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( it_return = lt_return ).
    ELSE.
*      Se recuperan los ficheros de esta ejecucion
      DATA(lt_tmp_attachments) = lo_attachment->get_tmp_attachments( iv_guid = ms_tmp_attach_guid-guid  ).

*      Se pasa a la estructura del front
      LOOP AT lt_tmp_attachments ASSIGNING FIELD-SYMBOL(<ls_tmp_attachment>).
        APPEND INITIAL LINE TO mt_wizard_files ASSIGNING FIELD-SYMBOL(<ls_wizard_file>).
        <ls_wizard_file> = CORRESPONDING #( <ls_tmp_attachment> MAPPING document_desc = filename ).
        <ls_wizard_file>-ui5_icon = lo_attachment->get_ui5_icon_from_mimetype( <ls_tmp_attachment>-mimetype ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD ajax_fileuploader_upload.

    READ TABLE ms_on_ajax-request-it_files ASSIGNING FIELD-SYMBOL(<ls_file>) INDEX 1.
    IF sy-subrc IS INITIAL AND
       <ls_file>-content IS NOT INITIAL AND
       <ls_file>-filename IS NOT INITIAL.
*   Se guarda el fichero subido en el servidor para recogerlo en la llamada al servicio ADD_IMAGE
      ms_on_ajax-server->api_parameter_set( name = 'TMP_FILENAME'
                                 value = <ls_file>-filename ).

      ms_on_ajax-server->api_parameter_set( name = 'TMP_CONTENT'
                                 value = <ls_file>-content ).
    ENDIF.

  ENDMETHOD.


  METHOD ajax_get_equipment_hierarchy.

    DATA: lr_center       TYPE RANGE OF werks_d,
          lr_equnr        TYPE RANGE OF equnr,
          lr_tec_location TYPE RANGE OF tplnr.

*   Si no se busca por ubicación se sacan todos los del centro
    IF ms_on_ajax-ajax_value IS NOT INITIAL.
      lr_tec_location = VALUE #( ( sign = 'I' option = 'EQ' low = ms_on_ajax-ajax_value ) ).
    ELSE.
      lr_center = VALUE #( ( sign = 'I' option = 'EQ' low = ms_user_info-center ) ).
    ENDIF.

    DATA(lt_hierarchy_equipment) = zcl_pm_masterdata=>get_tecnical_hierarchy( it_center = lr_center it_tec_location = lr_tec_location iv_include_equipments = abap_true ).
    mt_hierarchy_equipment = CORRESPONDING #( lt_hierarchy_equipment ).

  ENDMETHOD.


  METHOD ajax_get_master_list.

    DATA: lt_notif_id          TYPE RANGE OF qmnum,
          lr_notif_status      TYPE RANGE OF j_status,
          lt_placement         TYPE RANGE OF stort,
          lt_equipment         TYPE RANGE OF equnr,
          lt_job               TYPE RANGE OF arbpl,
          lt_tec_location      TYPE RANGE OF tplnr,
          lt_user_creation     TYPE RANGE OF ernam,
          lt_date_creation     TYPE RANGE OF erdat,
          lt_objnr             TYPE RANGE OF j_objnr,
          lt_center            TYPE RANGE OF werks_d,
          lr_order_id          TYPE RANGE OF aufnr,
          lr_instid            TYPE RANGE OF sibfboriid,
          lt_status_conversion TYPE zcl_pm_masterdata=>tt_status_conversion,
          lr_front_status      TYPE RANGE OF zpm_e_status,
*          lr_order             TYPE RANGE OF aufnr,
          lr_order_status      TYPE RANGE OF j_status.

    DATA(lo_notification) = NEW zcl_pm_notification( ).
    DATA(lo_attachments) = NEW zcl_pm_attachments( ).
    DATA(lo_order) = NEW zcl_pm_order( ).

*   Se llenan los rangos para buscar con los filtros del front

*   El rango de fechas siempre es obligatorio
    lt_date_creation = VALUE #( ( sign = 'I' option = 'BT' low = ms_search_filters-date_from high = ms_search_filters-date_to  ) ).
    lt_center = VALUE #( ( sign = 'I' option = 'EQ' low = ms_user_info-center ) ).

*   Filtros simples
    IF ms_search_filters-notification_id IS NOT INITIAL.
      lt_notif_id      = VALUE #( ( sign = 'I' option = 'EQ' low = CONV qmnum( |{ ms_search_filters-notification_id ALPHA = IN }| ) ) ).
    ENDIF.

    IF ms_search_filters-equipment IS NOT INITIAL.
      lt_equipment     = VALUE #( ( sign = 'I' option = 'EQ' low = CONV equnr( |{ ms_search_filters-equipment ALPHA = IN }| ) ) ).
    ENDIF.

    IF ms_search_filters-tec_location IS NOT INITIAL.
      lt_tec_location = VALUE #( ( sign = 'I' option = 'EQ' low = CONV tplnr( |{ ms_search_filters-tec_location ALPHA = IN }| ) ) ).
    ENDIF.

*   Filtros multiples
    lt_job = VALUE #( FOR <c> IN mt_search_jobs ( sign = 'I' option = 'EQ' low = <c>-key  ) ).
    lt_placement     = VALUE #( FOR <f> IN mt_search_placement ( sign = 'I' option = 'EQ' low = <f>-key ) ).
    lt_user_creation = VALUE #(  FOR <g> IN mt_search_user ( sign = 'I' option = 'EQ' low = <g>-key ) ).

*   Convierte los estados del front al back
    lt_status_conversion = VALUE #( FOR <b> IN mt_search_status ( status_front = <b>-key  ) ).
    lt_status_conversion = convert_status( lt_status_conversion ).
    lr_notif_status  = VALUE #( FOR <d> IN lt_status_conversion ( sign = 'I' option = 'EQ' low = <d>-status_back  ) ).


*   Se buscan los ids de las notificaciones a partir de los filtros
    DATA(lt_notification_id) = lo_notification->search_notifications( it_notif_id = lt_notif_id
                                                                      it_notif_status = lr_notif_status
                                                                      it_placement = lt_placement
                                                                      it_equipment = lt_equipment
                                                                      it_tec_location = lt_tec_location
                                                                      it_job       = lt_job
                                                                      it_user_creation = lt_user_creation
                                                                      it_date_creation = lt_date_creation
                                                                      it_center        = lt_center ).

    lt_notif_id = VALUE #( FOR <a> IN lt_notification_id ( sign = 'I' option = 'EQ' low = <a> ) ).

*   Se recuperan los datos de cabecera de las notificaciones
    DATA(lt_notification_header) = lo_notification->get_header_data( lt_notif_id ).

*   Se recuperan los estados del aviso
    lt_objnr = VALUE #( FOR <e> IN lt_notification_header ( sign = 'I' option = 'EQ' low = <e>-objnr ) ).

*   Se recuperan los datos de cabecera de las notificaciones
    DATA(lt_notification_status) = zcl_pm_masterdata=>get_object_status( lt_objnr ).

*   Se recuperan las ordenes asociadas a los avisos
    lr_order_id = VALUE #( FOR <e> IN lt_notification_header ( sign = 'I' option = 'EQ' low = <e>-order ) ).
*   Se recuperan los datos de cabecera de las ordenes
    DATA(lt_order_header) = NEW zcl_pm_order( )->get_order_header( lr_order_id ).

    IF lt_order_header IS NOT INITIAL.
*     Se recuperan los estados de la orden
      lt_objnr = VALUE #( FOR <h> IN lt_order_header ( sign = 'I' option = 'EQ' low = <h>-objnr ) ).
*     Se recuperan los partners de las ordenes
      DATA(lt_order_partners) = NEW zcl_pm_order( )->get_order_partners( lt_objnr ).

*     De las ordenes asociadas a los avisos se buscan las que esten finalizadas (Notificacion final)
      lr_order_id = VALUE #( FOR <i> IN lt_order_header ( sign = 'I' option = 'EQ' low = <i>-order ) ).
      lr_order_status = VALUE #( ( sign = 'I' option = 'EQ' low = zcl_pm_order=>ms_back_work_status-noti ) ).

      DATA(lt_ended_orders) = lo_order->search_orders( it_order = lr_order_id
                                                       it_order_status = lr_order_status ).

    ENDIF.


*   Se recuperan los anexos de las notificaciones
    lr_instid = VALUE #( FOR <e> IN lt_notification_header ( sign = 'I' option = 'EQ' low = <e>-notification_id ) ).
    DATA(lt_notification_attachment) = lo_attachments->get_gos_attachment_list( ir_instid = lr_instid
                                                                                iv_type_id = zcl_pm_attachments=>ms_bussines_object-notification  ).

    convert_notif_list_out( it_notification_header = lt_notification_header
                            it_notification_status = lt_notification_status
                            it_notification_attachments = lt_notification_attachment
                            it_order_header        = lt_order_header
                            it_order_partner       = lt_order_partners
                            it_ended_orders        = lt_ended_orders ).

*   Se borran de la tabla de salida los estados que no se han podido filtrar previamente (Estados que no dependen de los status del aviso)
*   Por comodidad se añaden todos al rango aunque ciertos estados vendran filtrados previamente
    lr_front_status = VALUE #( FOR <g> IN mt_search_status ( sign = 'I' option = 'EQ' low = <g>-key ) ).

    IF lr_front_status IS NOT INITIAL.
      DELETE mt_master_list WHERE status NOT IN lr_front_status.
    ENDIF.

    SORT mt_master_list BY status ASCENDING placement DESCENDING tec_location_desc equipment_desc DESCENDING.

  ENDMETHOD.


  METHOD ajax_get_notif_attachemnts.

    DATA: lr_instid TYPE RANGE OF sibfboriid.

    DATA(lo_attachments) = NEW zcl_pm_attachments( ).

    lr_instid = VALUE #( ( sign = 'I' option = 'EQ' low = |{ CONV qmnum( ms_on_ajax-ajax_value ) ALPHA = IN }| ) ).
    DATA(lt_gos_attachment) = lo_attachments->get_gos_attachment_list( ir_instid = lr_instid
                                                                       iv_type_id = zcl_pm_attachments=>ms_bussines_object-notification  ).

    LOOP AT lt_gos_attachment ASSIGNING FIELD-SYMBOL(<ls_gos_attachment>).
      APPEND INITIAL LINE TO mt_attachment_list ASSIGNING FIELD-SYMBOL(<ls_attachment_list>).
      <ls_attachment_list> = CORRESPONDING  #( <ls_gos_attachment> ).
      <ls_attachment_list>-document_desc = |{ <ls_attachment_list>-document_desc }.{ <ls_attachment_list>-file_ext }|.
    ENDLOOP.

  ENDMETHOD.


  METHOD ajax_get_notif_detail.

*   Se reutiliza la estructura del listado porque se muestran practicamente todos los datos, si se añaden
*   nuevos campos al detalle hay que cambiarla por rendimiento de los serivios
    DATA: lt_notif_id TYPE RANGE OF qmnum,
          lt_objnr    TYPE RANGE OF j_objnr,
          lr_order_id type range of aufnr,
          lr_order_status type range of j_status.

    DATA(lo_notification) = NEW zcl_pm_notification( ).
    DATA(lo_order) = NEW zcl_pm_order( ).

    lt_notif_id = VALUE #( ( sign = 'I' option = 'EQ' low = CONV qmnum( |{ ms_on_ajax-ajax_value ALPHA = IN }| ) ) ).

*   Se recuperan los datos de cabecera de las notificaciones
    DATA(lt_notification_header) = lo_notification->get_header_data( lt_notif_id ).
    READ TABLE lt_notification_header INTO DATA(ls_notification_header) INDEX 1.

*   Se recuperan los estados del aviso
    lt_objnr = VALUE #( ( sign = 'I' option = 'EQ' low = ls_notification_header-objnr ) ).

*   Se recuperan los datos de cabecera de las notificaciones
    DATA(lt_notification_status) = zcl_pm_masterdata=>get_object_status( lt_objnr ).

*   Se recupera el texto largo/detalle del aviso
    lo_notification->read_notif_long_descr( EXPORTING  iv_notif_id = ls_notification_header-notification_id CHANGING cv_notif_long_desc = ls_notification_header-notif_long_desc ).

*    lo_order
*    CONVERT_NOTIF_DETAIL_OUT
    IF ls_notification_header-order IS NOT INITIAL.
*     Se recuperan las ordenes asociadas a los avisos
      lr_order_id =  VALUE #(  ( sign = 'I' option = 'EQ' low = ls_notification_header-order ) ).
*     Se recuperan los estados de la orden
      lt_objnr = VALUE #( ( sign = 'I' option = 'EQ' low = ls_notification_header-objnr ) ).
      lr_order_status = VALUE #( ( sign = 'I' option = 'EQ' low = zcl_pm_order=>ms_back_work_status-noti ) ).

*     Se recuperan los partners de las ordenes
      DATA(lt_order_partners) = NEW zcl_pm_order( )->get_order_partners( lt_objnr ).

      DATA(lt_ended_orders) = lo_order->search_orders( it_order = lr_order_id
                                                       it_order_status = lr_order_status ).

      IF lt_ended_orders IS NOT INITIAL.
        DATA(lv_order_ended) = CONV abap_bool( abap_true ).
      ENDIF.

    ENDIF.

**   Se recuperan los datos de cabecera de las ordenes
*    DATA(lt_order_header) = NEW zcl_pm_order( )->get_order_header( lr_order_id ).
*
*    IF lt_order_header IS NOT INITIAL.
**     Se recuperan los estados de la orden
*      lt_objnr = VALUE #( FOR <h> IN lt_order_header ( sign = 'I' option = 'EQ' low = <h>-objnr ) ).
**     Se recuperan los partners de las ordenes
*      DATA(lt_order_partners) = NEW zcl_pm_order( )->get_order_partners( lt_objnr ).
*
**     De las ordenes asociadas a los avisos se buscan las que esten finalizadas (Notificacion final)
*      lr_order_id = VALUE #( FOR <i> IN lt_order_header ( sign = 'I' option = 'EQ' low = <i>-order ) ).
*
*
*    ENDIF.

    convert_notif_detail_out( is_notification_header = ls_notification_header
                              it_notification_status = lt_notification_status
                              it_order_partner = lt_order_partners
                              iv_order_ended = lv_order_ended ).


  ENDMETHOD.


  METHOD ajax_get_oficial_for_job.

    DATA: lt_objnr  TYPE RANGE OF j_objnr,
          lt_job_id TYPE RANGE OF cr_objid,
          lr_job    TYPE RANGE OF arbpl,
          lr_center TYPE RANGE OF werks_d.

*   Se recuperan interlocutores de la orden
*    DATA(lo_order) = NEW zcl_pm_order( ).
*    lt_objnr = VALUE #( ( sign = 'I' option = 'EQ' low = ms_order_detail-objnr ) ).
*    DATA(lt_order_partner) = lo_order->get_order_partners( lt_objnr ).

*   Se busca el id del puesto de trabajo
    lr_job = VALUE #( ( sign = 'I' option = 'EQ' low = ms_order_detail-job ) ).
    lr_center =  VALUE #( ( sign = 'I' option = 'EQ' low = ms_user_info-center ) ).
    DATA(lt_jobs) = zcl_pm_masterdata=>get_jobs( it_job = lr_job it_center = lr_center ).
    READ TABLE lt_jobs ASSIGNING FIELD-SYMBOL(<ls_job>) INDEX 1.
*   Si no se encuentra el pto de trab se sale
    CHECK sy-subrc IS INITIAL.

    lt_job_id = VALUE #( ( sign = 'I' option = 'EQ' low = <ls_job>-job_id ) ).
*    lt_job_id = VALUE #( FOR <ls_jobs> IN lt_jobs  ( sign = 'I' option = 'EQ' low = <ls_jobs>-job_id ) ).

*   Se monta la tabla de salida de asignaciones
    convert_order_assignments_out( it_job_id = lt_job_id ).
*                                   it_order_partner = lt_order_partner ).
  ENDMETHOD.


  METHOD ajax_get_user_for_placement.

    DATA: lr_placement TYPE RANGE OF stort.

*  Usuarios por emplazamiento
    DATA(lt_user_placement) = NEW zcl_pm_user( )->get_users_with_placement( ).

    lr_placement = VALUE #( FOR <a> IN mt_search_placement ( sign = 'I' option = 'EQ' low = <a>-key ) ).

    IF lr_placement IS NOT INITIAL.
      DELETE lt_user_placement WHERE placement NOT IN lr_placement.
    ENDIF.

    mt_masterdata_user_placement = CORRESPONDING #( lt_user_placement MAPPING key = username value = username ).
    SORT mt_masterdata_user_placement ASCENDING.
  ENDMETHOD.


  method AJAX_GET_USER_INFO.

    ms_user_info = new zcl_pm_user( )->get_user_info( sy-uname ).

  endmethod.


  METHOD ajax_init_aplication.

    DATA: lv_date_to     TYPE begda,
          lv_date_from   TYPE begda,
          lr_status_type TYPE RANGE OF zpm_e_status_type,
          lr_center      TYPE RANGE OF werks_d,
          lr_equnr       TYPE RANGE OF equnr,
          lr_job         TYPE RANGE OF arbpl,
          lr_placement   TYPE RANGE OF stort.

    DATA lv_days TYPE dlydy.

*   Fechas por defecto
    lv_date_to = sy-datum.
    IF ms_user_info-default_job EQ 'MEC' AND ms_user_info-center EQ zif_pm_data=>cs_center-granda AND ms_user_info-role EQ zcl_pm_user=>mc_roles-oficial.
      lv_days = 2.
    ELSE.
      lv_days = 14.
    ENDIF.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_date_to
        days      = lv_days
        months    = 0
        signum    = '-'
        years     = 0
      IMPORTING
        calc_date = lv_date_from.
    ms_search_filters-date_from = lv_date_from.
    ms_search_filters-date_to = lv_date_to.


*   Recupera el maestro de estados
    lr_status_type = VALUE #( ( sign = 'I' option = 'EQ' low = zcl_pm_notification=>mc_front_status_type ) ).
    DATA(lt_status) = zcl_pm_masterdata=>get_master_front_status( lr_status_type ).
    mt_masterdata_status = CORRESPONDING #( lt_status MAPPING key = status value = status_desc ).

*   Estados marcados por defecto
    mt_search_status = VALUE #( FOR <a> IN lt_status WHERE ( selected_default = abap_true ) ( key = <a>-status  ) ).

*   Recupera los puestos de trabajo responsable
    lr_center = VALUE #( ( sign = 'I' option = 'EQ' low = ms_user_info-center ) ).
*    zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = 'ACTIVE_JOB_%'
*                                                    CHANGING ct_ranges = lr_job ).
    lr_job = zcl_pm_masterdata=>get_active_job_by_center( iv_werks = ms_user_info-center ).
    DATA(lt_job) = zcl_pm_masterdata=>get_jobs( it_center = lr_center
                                                it_job = lr_job ).
    mt_masterdata_jobs = CORRESPONDING #( lt_job MAPPING key = job value = job_desc ).

*   Recupera los emplazamientos
    DATA(lt_placement) = zcl_pm_masterdata=>get_placements( iv_adjust_desc = abap_true it_center = lr_center ).
    mt_masterdata_placement = CORRESPONDING #( lt_placement MAPPING key = placement value = placement_desc ).

*   Recupera las ubicaciones técnicas
    DATA(lt_hierarchy_tec_location) = zcl_pm_masterdata=>get_tecnical_hierarchy( it_center = lr_center ).
    mt_hierarchy_tec_location = CORRESPONDING #( lt_hierarchy_tec_location ).

*   Equipos
    DATA(lt_hierarchy_equipment) = zcl_pm_masterdata=>get_tecnical_hierarchy( it_center = lr_center iv_include_equipments = abap_true ).
    mt_hierarchy_equipment = CORRESPONDING #( lt_hierarchy_equipment ).

*  Prioridades
    DATA(lt_priorities) = zcl_pm_masterdata=>get_priorities(  ).
    mt_masterdata_priority = CORRESPONDING #( lt_priorities MAPPING key = priority value = priority_desc ).

*  Emplazamiento por defecto
    IF ms_user_info-default_placement IS NOT INITIAL.
      APPEND INITIAL LINE TO mt_search_placement ASSIGNING FIELD-SYMBOL(<ls_search_placement>).
      <ls_search_placement>-key = ms_user_info-default_placement.
      lr_placement = VALUE #( ( sign = 'I' option = 'EQ' low = ms_user_info-default_placement ) ).
    ENDIF.

*  Usuarios por emplazamiento
    DATA(lt_user_placement) = NEW zcl_pm_user( )->get_users_with_placement( ).
    IF lr_placement IS NOT INITIAL.
      DELETE lt_user_placement WHERE placement NOT IN lr_placement.
    ENDIF.
    mt_masterdata_user_placement = CORRESPONDING #( lt_user_placement MAPPING key = username value = username ).
    SORT mt_masterdata_user_placement ASCENDING.

*  Tipos de aviso
    DATA(lt_notification_class) = zcl_pm_masterdata=>get_notification_class( ).
    mt_masterdata_notif_class = CORRESPONDING #( lt_notification_class MAPPING key = notification_class value = notification_class_desc ).
**   append initial line to mt_masterdata_notif_class.
    SORT mt_masterdata_notif_class DESCENDING.

* Recupera las clases de ordenes para la creación
    DATA(lt_orderclasses) = zcl_pm_masterdata=>get_order_classes( iv_langu = sy-langu ).
    mt_masterdata_orderclass = CORRESPONDING #( lt_orderclasses MAPPING key = class_id value = class_desc ).

* Puesto de trabajo por defecto
    IF ms_user_info-default_job IS NOT INITIAL.
      mt_search_jobs = VALUE #( ( key = ms_user_info-default_job ) ).
      IF ms_user_info-default_job EQ 'MEC' AND ms_user_info-center EQ zif_pm_data=>cs_center-granda.
        APPEND INITIAL LINE TO mt_search_jobs ASSIGNING FIELD-SYMBOL(<fs_s_job>).
        <fs_s_job>-key = 'TUBINOX'.
      ENDIF.
    ENDIF.

* Estados parada de producción - CAPSA
  mt_masterdata_shutdown = VALUE #( ( key = space value = ' ' ) ( key = abap_false value = 'No' ) ( key = abap_true value = 'Sí' ) ).

  ENDMETHOD.


  METHOD ajax_modify_notif.

    DATA(lo_notification) = NEW zcl_pm_notification( ).

*    data: ls_notif_header type zcl_pm_notification=>ts_notification_header.

    data(ls_notif_header) = convert_notif_detail_in( ).

* Se modifican los datos de la notificación
    lo_notification->save_notif( EXPORTING is_notif = ls_notif_header IMPORTING et_return = DATA(lt_return) ).

* Convertimos los mensajes para el tipo de la aplicación
    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( it_return = lt_return ).


  ENDMETHOD.


  METHOD ajax_open_attachment.

    ms_document_url-url = NEW zcl_pm_attachments( )->get_gos_attachment_url( iv_gos_attachment_id = CONV so_entryid(  ms_on_ajax-ajax_value ) ).

  ENDMETHOD.


  METHOD ajax_prepare_order_creation.

    IF ms_notification_detail IS NOT INITIAL.
      ms_order_detail = CORRESPONDING #( ms_notification_detail MAPPING end_date   = desired_end_date
                                                                        start_date = desired_start_date
                                                                        order_desc = notif_description ).

    ENDIF.
  ENDMETHOD.


  method AJAX_RELEASE_ORDER.

    DATA: lr_order TYPE RANGE OF aufnr,
          lt_notif_id TYPE RANGE OF qmnum.

    SPLIT ms_on_ajax-ajax_value AT '|' INTO DATA(lv_order) DATA(lv_pernr).

*    DATA(lo_notification) = NEW zcl_pm_notification( ).
    DATA(lo_order) = NEW zcl_pm_order( ).


*   Se recuperan los datos de cabecera de las notificaciones
*    DATA(lt_notification_header) = lo_notification->get_header_data( lt_notif_id ).
*    READ TABLE lt_notification_header INTO DATA(ls_notification_header) INDEX 1.

    lr_order = VALUE #( ( sign = 'I' option = 'EQ' low = |{ conv aufnr( lv_order ) }| ) ).

    DATA(lt_order_header) = lo_order->get_order_header( lr_order ).
    READ TABLE lt_order_header INTO DATA(ls_order_header) INDEX 1.

    DATA(lv_enc_prod_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'ENC_PROD_ORDER_PARVW' ) ) .

    DATA(lt_partners) = VALUE zcl_pm_order=>tt_order_partner( ( employee_code = |{ CONV pernr_d( lv_pernr ) ALPHA = IN }| parvw = lv_enc_prod_parvw ) ).

    DATA(lt_return) = lo_order->add_partner_to_order( is_order_header = ls_order_header
                                                      it_partners = lt_partners ).

    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( lt_return ).

  endmethod.


  METHOD convert_notif_detail_in.

    data: lv_long_desc_len type i.

    rs_notification_header = CORRESPONDING #( ms_notification_detail MAPPING notif_short_desc = notif_description
                                                                             production_stop = production_stop_yes
                                                                             notif_user = user ).

    data(lv_desc_len) = strlen( ms_notification_detail-notif_description ).

*   Si la descripcion es larga se hace split a los 40 caracteres
    if lv_desc_len gt 40.
      rs_notification_header-notif_short_desc = ms_notification_detail-notif_description+0(40).

      lv_long_desc_len = lv_desc_len - 40.

      rs_notification_header-notif_long_desc = ms_notification_detail-notif_description+40(lv_long_desc_len).
    else.
      rs_notification_header-notif_short_desc =  ms_notification_detail-notif_description.
    endif.

  ENDMETHOD.


  METHOD convert_notif_detail_out.


    DATA: lr_order  TYPE RANGE OF aufnr,
          lr_status TYPE RANGE OF j_status.



    DATA(lv_enc_prod_order_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'ENC_PROD_ORDER_PARVW' ) ).


*if is_notification_header-order is not initial.
*    DATA(lo_order) = NEW zcl_pm_order( ).
*
**   De las ordenes asociadas a los avisos se buscan las que esten finalizadas (Notificacion final)
*    lr_order = VALUE #( FOR <a> IN it_order_header ( sign = 'I' option = 'EQ' low = <a>-order ) ).
*    lr_status = VALUE #( ( sign = 'I' option = 'EQ' low = zcl_pm_order=>ms_back_work_status-noti ) ).
*
*    DATA(lv_enc_prod_order_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'ENC_PROD_ORDER_PARVW' ) ).
*
*    DATA(lt_ended_orders) = lo_order->search_orders( it_order = lr_order
*                                                        it_order_status = lr_status ).
*
*    endif.
*


*   Se pasan los datos a la estructura del front

    ms_notification_detail = CORRESPONDING #( is_notification_header MAPPING notif_description = notif_short_desc
                                                                             user = notif_user ).
*   Se concatenan las descripciones larga y corta
    ms_notification_detail-notif_description = |{ is_notification_header-notif_short_desc }{ is_notification_header-notif_long_desc }|.

*     Si no hay nombre largo devuelve el nombre de usuario
    ms_notification_detail-username = COND #( WHEN is_notification_header-notif_user_name IS NOT INITIAL THEN is_notification_header-notif_user_name
                                        ELSE is_notification_header-notif_user ).

*     Pueden estar varios estados activos a la vez por lo que se buscan por tiempo. De menos a mas
*     Aviso creado
    READ TABLE it_notification_status ASSIGNING FIELD-SYMBOL(<ls_notification_status>) WITH KEY objnr = is_notification_header-objnr
                                                                                                status = zcl_pm_notification=>ms_back_status-meab.
    IF sy-subrc IS INITIAL.
      ms_notification_detail-status = zcl_pm_notification=>ms_front_status-pending.
      ms_notification_detail-can_create_order = abap_true.
      ms_notification_detail-editable = abap_true.
    ELSE.
*       El aviso tambien puede estar "En tratamiento"
      READ TABLE it_notification_status ASSIGNING <ls_notification_status> WITH KEY objnr = is_notification_header-objnr
                                                                                    status = zcl_pm_notification=>ms_back_status-metr.
      IF sy-subrc IS INITIAL.
        ms_notification_detail-status = zcl_pm_notification=>ms_front_status-pending.
        ms_notification_detail-can_create_order = abap_true.
        ms_notification_detail-editable = abap_true.
      ENDIF.
    ENDIF.

*     Orden asignada / Pdte de liberacion
    READ TABLE it_notification_status ASSIGNING <ls_notification_status> WITH KEY objnr = is_notification_header-objnr
                                                                                                status = zcl_pm_notification=>ms_back_status-oras.
    IF sy-subrc IS INITIAL.
*       TODO - Añadir pendiente de liberación
      ms_notification_detail-status = zcl_pm_notification=>ms_front_status-with_order.
      ms_notification_detail-can_create_order = abap_false.
      ms_notification_detail-editable = abap_false.
*        <ls_master_list>-status_icon = ms_status_icon-completed.
    ENDIF.

*   Si la orden ha finalizado esta pendiente de liberar
    IF iv_order_ended IS NOT INITIAL.
      ms_notification_detail-status = zcl_pm_notification=>ms_front_status-pending_release.
      ms_notification_detail-can_create_order = abap_false.
      ms_notification_detail-editable = abap_false.
    ENDIF.

*   Si tiene como interlocutor al encargado de produccion esta liberado
    READ TABLE it_order_partner ASSIGNING FIELD-SYMBOL(<ls_order_partner>) WITH KEY parvw = lv_enc_prod_order_parvw.
    IF sy-subrc IS INITIAL.
      ms_notification_detail-status = zcl_pm_notification=>ms_front_status-released.
      ms_notification_detail-editable = abap_false.
      ms_notification_detail-can_create_order = abap_false.
    ENDIF.

*   Pueden estar varios estados activos a la vez por lo que se buscan por tiempo. De menos a mas
    READ TABLE it_notification_status ASSIGNING <ls_notification_status> WITH KEY objnr = is_notification_header-objnr
                                                                                                status = zcl_pm_notification=>ms_back_status-mece.
    IF sy-subrc IS INITIAL.
      ms_notification_detail-status = zcl_pm_notification=>ms_front_status-completed.
      ms_notification_detail-can_create_order = abap_false.
      ms_notification_detail-editable = abap_false.
    ENDIF.

    DATA(lt_front_status) = zcl_pm_masterdata=>get_master_front_status( ).
    READ TABLE lt_front_status ASSIGNING FIELD-SYMBOL(<ls_front_status>) WITH KEY status = ms_notification_detail-status.
    IF sy-subrc IS INITIAL.
      ms_notification_detail-status_desc = <ls_front_status>-status_desc.
    ENDIF.
    " Si el flag de parada no viene marcado, marcamos el valor en el NO
    IF is_notification_header-production_stop IS INITIAL.
      ms_notification_detail-production_stop_no = abap_true.
    ELSE.
      ms_notification_detail-production_stop_yes = abap_true.
    ENDIF.

*   Crear orden desactivado
*    ms_notification_detail-can_create_order = abap_false.

  ENDMETHOD.


  METHOD convert_notif_list_out.


    DATA(lv_enc_prod_order_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'ENC_PROD_ORDER_PARVW' ) ).

*   Se pasan los datos a la estructura del front
    LOOP AT it_notification_header ASSIGNING FIELD-SYMBOL(<ls_notification_header>).
      APPEND INITIAL LINE TO mt_master_list ASSIGNING FIELD-SYMBOL(<ls_master_list>).
      <ls_master_list> = CORRESPONDING #( <ls_notification_header> MAPPING notif_description = notif_short_desc
                                                                           user = notif_user ).
*     Si no hay nombre largo devuelve el nombre de usuario
      <ls_master_list>-username = COND #( WHEN <ls_notification_header>-notif_user_name IS NOT INITIAL THEN <ls_notification_header>-notif_user_name
                                          ELSE <ls_notification_header>-notif_user ).


*     Pueden estar varios estados activos a la vez por lo que se buscan por tiempo. De menos a mas
*     Aviso creado
      READ TABLE it_notification_status ASSIGNING FIELD-SYMBOL(<ls_notification_status>) WITH KEY objnr = <ls_notification_header>-objnr
                                                                                                  status = zcl_pm_notification=>ms_back_status-meab.
      IF sy-subrc IS INITIAL.
        <ls_master_list>-status = zcl_pm_notification=>ms_front_status-pending.
        <ls_master_list>-status_icon = ms_status_icon-pending.
        <ls_master_list>-can_create_order = abap_true.
      ELSE.
*       El aviso tambien puede estar "En tratamiento"
        READ TABLE it_notification_status ASSIGNING <ls_notification_status> WITH KEY objnr = <ls_notification_header>-objnr
                                                                                      status = zcl_pm_notification=>ms_back_status-metr.
        IF sy-subrc IS INITIAL.
          <ls_master_list>-status = zcl_pm_notification=>ms_front_status-pending.
          <ls_master_list>-status_icon = ms_status_icon-pending.
          <ls_master_list>-can_create_order = abap_true.
        ENDIF.
      ENDIF.

*     Orden asignada / Pdte de liberacion
      READ TABLE it_notification_status ASSIGNING <ls_notification_status> WITH KEY objnr = <ls_notification_header>-objnr
                                                                                                  status = zcl_pm_notification=>ms_back_status-oras.
      IF sy-subrc IS INITIAL.
        <ls_master_list>-status = zcl_pm_notification=>ms_front_status-with_order.
        <ls_master_list>-status_icon = ms_status_icon-with_order.
        <ls_master_list>-can_create_order = abap_false.
      ENDIF.

*     si la orden asociada al aviso esta notificada completamente. el aviso esta pendiente de liberación
      READ TABLE it_ended_orders ASSIGNING FIELD-SYMBOL(<ls_ended_order>) WITH KEY table_line = <ls_notification_header>-order.
      IF sy-subrc IS INITIAL.
        <ls_master_list>-status = zcl_pm_notification=>ms_front_status-pending_release.
        <ls_master_list>-status_icon = ms_status_icon-pending_release.
        <ls_master_list>-can_create_order = abap_false.

      ENDIF.

      READ TABLE it_order_header INTO DATA(ls_order_header) WITH KEY order = <ls_notification_header>-order.

*       Si la orden tiene notificacion final, se revisan los interlocutores
      READ TABLE it_order_partner ASSIGNING FIELD-SYMBOL(<ls_order_partner>) WITH KEY objnr = ls_order_header-objnr
                                                                                      parvw = lv_enc_prod_order_parvw.
      IF sy-subrc IS INITIAL.
        <ls_master_list>-status = zcl_pm_notification=>ms_front_status-released.
        <ls_master_list>-status_icon = ms_status_icon-released.
        <ls_master_list>-can_create_order = abap_false.
      ENDIF.

*     Pueden estar varios estados activos a la vez por lo que se buscan por tiempo. De menos a mas
      READ TABLE it_notification_status ASSIGNING <ls_notification_status> WITH KEY objnr = <ls_notification_header>-objnr
                                                                                                  status = zcl_pm_notification=>ms_back_status-mece.
      IF sy-subrc IS INITIAL.
        <ls_master_list>-status = zcl_pm_notification=>ms_front_status-completed.
        <ls_master_list>-status_icon = ms_status_icon-completed.
        <ls_master_list>-can_create_order = abap_false.
      ENDIF.

      IF <ls_notification_header>-order IS NOT INITIAL.
*     Se pasan los datos de la orden
        READ TABLE it_order_header ASSIGNING FIELD-SYMBOL(<ls_order_header>) WITH KEY order = <ls_notification_header>-order.
        IF sy-subrc IS INITIAL.
          <ls_master_list>-order_class = <ls_order_header>-order_class.
          <ls_master_list>-order_class_desc = <ls_order_header>-order_class_desc.
        ENDIF.
      ENDIF.

      READ TABLE it_notification_attachments ASSIGNING FIELD-SYMBOL(<ls_notification_attachments>) WITH KEY instid_a = <ls_notification_header>-notification_id.
      IF sy-subrc IS INITIAL.
        <ls_master_list>-has_attachment = abap_true.
      ENDIF.

*    Funcionalidad de crear orden desactivada
      <ls_master_list>-can_create_order = abap_false.

    ENDLOOP.

  ENDMETHOD.


  METHOD convert_order_assignments_out.

    DATA: lr_job_id TYPE RANGE OF cr_objid,
          lr_pernr  TYPE RANGE OF pernr_d.

    lr_job_id = CORRESPONDING #( it_job_id ).

    DATA(lv_oficial_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'OFICIAL_ORDER_PARVW' ) ).

*   Se obtienen los empleados de un puesto de trabajo y se cruzan con los partners de la orden.
*    lr_pernr = VALUE #( FOR <a> IN it_order_partner WHERE ( parvw = lv_oficial_parvw ) ( sign = 'I' option = 'EQ' low = <a>-employee_code ) ).

    DATA(lt_job_employee) =  zcl_pm_masterdata=>get_employee_from_job( it_job_id = lr_job_id
                                                                       iv_date = sy-datum ).

    LOOP AT lt_job_employee ASSIGNING FIELD-SYMBOL(<ls_job_employee>).
      APPEND INITIAL LINE TO mt_order_assignments ASSIGNING FIELD-SYMBOL(<ls_order_assigments>).
      <ls_order_assigments> = CORRESPONDING #( <ls_job_employee>  MAPPING employee_code = pernr
                                                                          employee_name = full_name ).

*      IF lr_pernr[] IS NOT INITIAL AND <ls_job_employee>-pernr IN lr_pernr.
*        <ls_order_assigments>-assigned = abap_true.
*      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD convert_status.

*   El estado pendiente de liberacion se determina con datos de la orden de trabajo.
*   Por lo que aqui se convierte al estado "Con orden" y en otro punto de la aplicacion se vera si los avisos con order estan pendientes de liberar o no

    rt_status_conversion = it_status_conversion.
    LOOP AT rt_status_conversion ASSIGNING FIELD-SYMBOL(<ls_status_conversion>).
      CASE space.
*       Convierte estados del front al back
        WHEN <ls_status_conversion>-status_back.
          <ls_status_conversion>-status_back = SWITCH #( <ls_status_conversion>-status_front
                                                          WHEN zcl_pm_notification=>ms_front_status-pending THEN zcl_pm_notification=>ms_back_status-meab
                                                          WHEN zcl_pm_notification=>ms_front_status-with_order THEN zcl_pm_notification=>ms_back_status-oras
                                                          WHEN zcl_pm_notification=>ms_front_status-pending_release THEN zcl_pm_notification=>ms_back_status-oras
                                                          WHEN zcl_pm_notification=>ms_front_status-completed THEN zcl_pm_notification=>ms_back_status-mece ).



*      Convierte estados del back al front
        WHEN <ls_status_conversion>-status_front.
          <ls_status_conversion>-status_front = SWITCH #( <ls_status_conversion>-status_back
                                                          WHEN  zcl_pm_notification=>ms_back_status-meab THEN zcl_pm_notification=>ms_front_status-pending
                                                          WHEN zcl_pm_notification=>ms_back_status-oras THEN zcl_pm_notification=>ms_front_status-with_order
                                                          WHEN zcl_pm_notification=>ms_back_status-mece THEN zcl_pm_notification=>ms_front_status-completed ).

      ENDCASE.
    ENDLOOP.

*   Si el estado es pendiente, se añade el estado de back METR
    read table rt_status_conversion ASSIGNING <ls_status_conversion> with key status_front = zcl_pm_notification=>ms_front_status-pending.
    if sy-subrc is initial.
      rt_status_conversion = VALUE #( BASE rt_status_conversion ( status_back = zcl_pm_notification=>ms_back_status-metr status_front = zcl_pm_notification=>ms_front_status-pending ) ).
    endif.

  ENDMETHOD.
ENDCLASS.
