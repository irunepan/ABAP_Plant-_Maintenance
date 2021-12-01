CLASS zcl_pm_orders_app_cntrl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /neptune/if_nad_server .

    TYPES:
      BEGIN OF ts_master_list_actions,
        can_create_order   TYPE abap_bool,
        can_massive_assign TYPE abap_bool,
        can_close_order    TYPE abap_bool,
      END OF ts_master_list_actions .
    TYPES:
      BEGIN OF ts_dialog_doc,
        aufnr TYPE aufnr,
        qmnum TYPE qmnum,
      END OF ts_dialog_doc .
    TYPES:
      BEGIN OF ts_master_list,
        work_status          TYPE string,
        work_status_icon     TYPE string,
        material_status      TYPE string,
        material_status_icon TYPE string,
        placement            TYPE string,
        placement_desc       TYPE string,
        tec_location         TYPE string,
        tec_location_desc    TYPE string,
        equipment            TYPE string,
        equipment_desc       TYPE string,
        job                  TYPE string,
        job_desc             TYPE string,
        user                 TYPE string,
        username             TYPE string,
        order                TYPE string,
        order_class          TYPE string,
        order_class_desc     TYPE string,
        order_desc           TYPE string,
        priority             TYPE string,
        priority_desc        TYPE string,
        notification_id      TYPE string,
        ref_date             TYPE string,
        start_date           TYPE string,
        end_date             TYPE string,
        has_attachment       TYPE abap_bool,
      END OF ts_master_list .
    TYPES:
      tt_master_list TYPE STANDARD TABLE OF ts_master_list WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_handle_on_ajax,
        applid     TYPE string,
        ajax_id    TYPE string,
        ajax_value TYPE string,
        server     TYPE REF TO /neptune/cl_nad_server,
        request    TYPE /neptune/data_request,
        navigation TYPE /neptune/ajax_navigation,
      END OF ts_handle_on_ajax .
    TYPES:
      BEGIN OF ts_search_filters,
        order             TYPE string,
        order_class       TYPE string, "PREV Preventivo / NPREV No preventivo
*        placement         TYPE string,
        tec_location      TYPE string,
        tec_location_desc TYPE string,
        equipment         TYPE string,
        equipment_desc    TYPE string,
        employee_number   TYPE string,
        date_from         TYPE string,
        date_to           TYPE string,
      END OF ts_search_filters .
    TYPES:
      BEGIN OF ts_key_value,
        key   TYPE string,
        value TYPE string,
      END OF ts_key_value .
    TYPES:
      BEGIN OF ts_search_key,
        key TYPE string,
      END OF ts_search_key .
    TYPES:
      tt_key_value TYPE STANDARD TABLE OF ts_key_value WITH DEFAULT KEY .
    TYPES: BEGIN OF ts_int_jobs,
             job TYPE arbpl,
           END OF ts_int_jobs.
    TYPES:
      tt_int_jobs TYPE STANDARD TABLE OF ts_int_jobs WITH DEFAULT KEY .
    TYPES:
      tt_search_key TYPE STANDARD TABLE OF ts_search_key WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_technical_hierarchy,
        node_id        TYPE string,
        node_parent_id TYPE string,
        node_type      TYPE string, "Equipo o ub. tecnica
        node_desc      TYPE string,
        highlight      TYPE string,
      END OF  ts_technical_hierarchy .
    TYPES:
      tt_technical_hierarchy TYPE STANDARD TABLE OF ts_technical_hierarchy WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_order_detail,
        order                    TYPE string,
        order_desc               TYPE string,
        order_class              TYPE string,
        order_class_desc         TYPE string,
        work_status              TYPE string,
        work_status_desc         TYPE string,
        material_status          TYPE string,
        material_status_desc     TYPE string,
        placement                TYPE string,
        placement_desc           TYPE string,
        tec_location             TYPE string,
        tec_location_desc        TYPE string,
        equipment                TYPE string,
        equipment_desc           TYPE string,
        job                      TYPE string,
        job_desc                 TYPE string,
        user                     TYPE string,
        username                 TYPE string,
        start_date               TYPE string,
        end_date                 TYPE string,
        production_stop_yes      TYPE abap_bool,
        production_stop_no       TYPE abap_bool,
        creation_user            TYPE string,
        objnr                    TYPE objnr,

*       Permisos
        editable                 TYPE abap_bool,
        job_editable             TYPE abap_bool,
        can_create_notification  TYPE abap_bool,
        can_create_material_doc  TYPE abap_bool,
        can_close_order          TYPE abap_bool,
        can_assign_oficial       TYPE abap_bool,
        can_release_order        TYPE abap_bool,
        can_release_from_prod    TYPE abap_bool,
        mat_doc_oficial_selector TYPE abap_bool, "Habilita o deshabilita el select para seleccionar oficial en doc material

      END OF ts_order_detail .
    TYPES:
      BEGIN OF ts_material_movement,
        material_doc      TYPE string,
        material          TYPE string,
        material_desc     TYPE string,
        quantity          TYPE string,
        unit              TYPE string,
        order             TYPE string,
        employee          TYPE string,
        purchase_order    TYPE string,
        storage           TYPE string,
        movement_type     TYPE string,
        doc_date          TYPE string,
        amount            TYPE string,
        creation_user     TYPE string,
        material_doc_year	TYPE mjahr,
        delete_vis        TYPE abap_bool,
      END OF ts_material_movement .
    TYPES:
      tt_material_movement TYPE STANDARD TABLE OF ts_material_movement WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_order_component,
        material       TYPE string,
        material_desc  TYPE string,
        quantity       TYPE string,
        unit           TYPE string,
        purchase_order TYPE string,
        supplier_name  TYPE string,
        storage        TYPE string,
      END OF ts_order_component .
    TYPES:
      tt_order_component TYPE STANDARD TABLE OF ts_order_component WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_order_notification,
        notification_key     TYPE string,
        notification_counter TYPE string,
        notification_text    TYPE string,
        employee_number      TYPE string,
        employee_name        TYPE string,
*        creation_user        TYPE ernam,
        worked_time          TYPE string,
        worked_time_unit     TYPE string,
        order                TYPE string,
*        operation_key        TYPE co_aufpl,
*        operation_counter    TYPE co_aplzl,
*        operation_number     TYPE vornr,
        work_start_date      TYPE string,
        work_start_time      TYPE string,
        work_end_date        TYPE string,
        work_end_time        TYPE string,
        is_final_yes         TYPE abap_bool,
        is_final_no          TYPE abap_bool,
      END OF ts_order_notification .
    TYPES:
      tt_order_notification TYPE STANDARD TABLE OF ts_order_notification WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_order_assigments,
        assigned      TYPE abap_bool,
        employee_code TYPE string,
        employee_name TYPE string,
        job_id        TYPE cr_objid,
        order         TYPE i,
      END OF ts_order_assigments .
    TYPES:
      tt_order_assigments TYPE STANDARD TABLE OF ts_order_assigments WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_document_url,
        url TYPE string,
      END OF ts_document_url .
    TYPES:
      BEGIN OF ts_historic_order_list,
        order            TYPE string,
        has_attachments  TYPE abap_bool,
        order_short_desc TYPE string,
        creation_date    TYPE string,
      END OF ts_historic_order_list .
    TYPES:
      tt_historic_order_list TYPE STANDARD TABLE OF ts_historic_order_list WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_tmp_attach_guid,
        guid TYPE guid_32,
      END OF ts_tmp_attach_guid .
    TYPES:
      BEGIN OF ts_camera_file,
        filename TYPE string,
        content  TYPE string,
      END OF ts_camera_file .
    TYPES:
      BEGIN OF ts_massive_assign_order,
        order          TYPE string,
        end_date       TYPE string,
        order_desc     TYPE string,
        objnr          TYPE objnr,
        equipment_desc TYPE string,
      END OF ts_massive_assign_order .
    TYPES:
      tt_massive_assign_order TYPE STANDARD TABLE OF ts_massive_assign_order WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_massive_assign_oficial,
        selected       TYPE abap_bool,
        visible        TYPE abap_bool,
        employee_id    TYPE string,
        employee       TYPE string,
        order          TYPE string,
        end_date       TYPE string,
        order_desc     TYPE string,
        equipment      TYPE string,
        equipment_desc TYPE string,
      END OF ts_massive_assign_oficial .
    TYPES:
      tt_massive_assign_oficial TYPE STANDARD TABLE OF ts_massive_assign_oficial WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_massive_oficial,
        selected    TYPE abap_bool,
        visible     TYPE abap_bool,
        employee_id TYPE string,
        employee    TYPE string,
      END OF ts_massive_oficial .
    TYPES:
      tt_massive_oficial TYPE STANDARD TABLE OF ts_massive_oficial WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_employee,
        employee_code TYPE string,
        employee_name TYPE string,
      END OF ts_employee .

    DATA ms_user_info TYPE zcl_pm_user=>ts_user .
    DATA ms_search_filters TYPE ts_search_filters .
    DATA mt_masterdata_work_status TYPE tt_key_value .
    DATA mt_masterdata_material_status TYPE tt_key_value .
    DATA mt_masterdata_all_jobs TYPE tt_key_value .
    DATA mt_internal_jobs TYPE tt_int_jobs .
    DATA mt_masterdata_jobs TYPE tt_key_value .
    DATA mt_masterdata_placement TYPE tt_key_value .
    DATA mt_search_work_status TYPE tt_search_key .
    DATA mt_search_material_status TYPE tt_search_key .
    DATA mt_search_jobs TYPE tt_search_key .
    DATA mt_master_list TYPE tt_master_list .
    DATA mt_hierarchy_equipment TYPE tt_technical_hierarchy .
    DATA mt_hierarchy_tec_location TYPE tt_technical_hierarchy .
    DATA mt_masterdata_orderclass TYPE tt_key_value .
    CONSTANTS:
      BEGIN OF  ms_status_icon,
        open                    TYPE string VALUE 'sap-icon://document',
        pending                 TYPE string VALUE 'sap-icon://bell',
        assigned                TYPE string VALUE 'sap-icon://employee',
        started                 TYPE string VALUE 'sap-icon://pending',
        completed               TYPE string VALUE 'sap-icon://complete',
        released_by_prod        TYPE string VALUE 'sap-icon://approvals',
        closed                  TYPE string VALUE 'sap-icon://locked',
        material_assigned       TYPE string VALUE 'sap-icon://fa-solid/warehouse',
        material_in_preparation TYPE string VALUE 'sap-icon://fa-solid/dolly',
        material_available      TYPE string VALUE 'sap-icon://fa-solid/box',
      END OF ms_status_icon .
    DATA ms_order_detail TYPE ts_order_detail .
    DATA mt_order_assignments TYPE tt_order_assigments .
    DATA mt_historic_orders TYPE tt_historic_order_list .
    DATA mt_historic_notification TYPE tt_order_notification .
    DATA mt_historic_material_movement TYPE tt_material_movement .
    DATA mt_material_movement TYPE tt_material_movement .
    DATA mt_order_notification TYPE tt_order_notification .
    DATA mt_order_component TYPE tt_order_component .
    DATA mt_order_attachment TYPE zif_pm_data=>tt_attachment_list .
    DATA mt_notif_attachment TYPE zif_pm_data=>tt_attachment_list .
    DATA mt_equipment_attachment TYPE zif_pm_data=>tt_attachment_list .
    DATA mt_order_creation_attachment TYPE zif_pm_data=>tt_attachment_list .
    DATA ms_document_url TYPE ts_document_url .
    DATA mt_log TYPE zif_pm_data=>tt_log .
    DATA mt_search_placement TYPE tt_search_key .
    DATA mt_task_list_attachment TYPE zif_pm_data=>tt_attachment_list .
    DATA ms_order_notifi_create TYPE ts_order_notification .
    DATA ms_material_movement_create TYPE ts_material_movement .
    DATA ms_tmp_attach_guid TYPE ts_tmp_attach_guid .
    DATA mt_wizard_files TYPE zif_pm_data=>tt_attachment_list .
    DATA ms_camera_file TYPE ts_camera_file .
    DATA mt_selected_orders TYPE tt_key_value .
    DATA mt_massive_assign_order TYPE tt_massive_assign_order .
    DATA mt_massive_assign_oficial TYPE tt_massive_assign_oficial .
    DATA mt_massive_oficial TYPE tt_massive_oficial .
    DATA ms_employee TYPE ts_employee .
    DATA ms_master_list_actions TYPE ts_master_list_actions .

    METHODS convert_order_detail_in
      IMPORTING
        !is_order_detail       TYPE ts_order_detail
      RETURNING
        VALUE(rs_order_header) TYPE zcl_pm_order=>ts_order_header .
protected section.

  data MS_ON_AJAX type TS_HANDLE_ON_AJAX .

  methods AJAX_INIT_APLICATION .
  methods AJAX_GET_USER_INFO .
  methods AJAX_GET_MASTER_LIST .
  methods AJAX_GET_EQUIPMENT_HIERARCHY .
  methods AJAX_GET_ORDER_DETAIL .
  methods AJAX_GET_EQUIP_ATTACHMENTS .
  methods AJAX_GET_NOTIF_ATTACHMENTS .
  methods AJAX_GET_TASK_LIST_ATTACHMENTS .
  methods AJAX_GET_ORDER_ATTACHMENTS .
  methods AJAX_GET_ORDER_HISTORY .
  methods AJAX_GET_OFICIAL_FOR_JOB .
  methods AJAX_GET_MATERIAL_DETAIL .
  methods AJAX_CREATE_MATERIAL_MOVEMENT .
  methods AJAX_GET_ORDER_ASSIGNMENTS .
  methods AJAX_FILEUPLOADER_UPLOAD .
  methods AJAX_OPEN_ATTACHMENT .
  methods AJAX_DELETE_ATTACHMENT .
  methods AJAX_ADD_ORDER_ATTACHMENT .
  methods AJAX_MODIFY_ORDER .
  methods AJAX_GET_DIALOG_ATTACHMENTS .
  methods AJAX_CREATE_ORDER_NOTIFICATION .
  methods AJAX_ADD_WIZARD_FILE .
  methods AJAX_ADD_WIZARD_PICTURE .
  methods AJAX_ADD_ORDER_PICTURE .
  methods AJAX_DELETE_TMP_ATTACHMENT .
  methods AJAX_CREATE_NEW_ORDER .
  methods AJAX_CANCEL_DOC .
  methods AJAX_CREATE_AND_PRINT_ORDER .
  methods AJAX_CLOSE_ORDER .
  methods AJAX_RELEASE_ORDER .
  methods AJAX_MASSIVE_ORDER_CLOSE .
  methods AJAX_MASSIVE_ASSIGNMENT_INIT .
  methods AJAX_RELEASE_PENDING_ORDER .
  methods AJAX_GET_EMPLOYEE_NAME .
  methods AJAX_MASSIVE_ASSIGNMENT .
  methods AJAX_PRINT_ORDER .
  methods AJAX_CHECK_USER_HAS_NOTIFIED .
private section.

  methods CONVERT_ORDER_ASSIGNMENTS_OUT
    importing
      !IT_ORDER_PARTNER type ZCL_PM_ORDER=>TT_ORDER_PARTNER
      !IT_JOB_ID type STANDARD TABLE .
  methods CONVERT_ORDER_LIST_OUT
    importing
      !IT_ORDER_STATUS type ZCL_PM_MASTERDATA=>TT_OBJECT_STATUS
      !IT_ORDER_ATTACHMENTS type ZCL_PM_ATTACHMENTS=>TT_GOS_ATTACHMENT_LIST
      !IT_ORDER_HEADER type ZCL_PM_ORDER=>TT_ORDER_HEADER
      !IT_ORDER_NOTIFICATION type ZCL_PM_ORDER=>TT_ORDER_NOTIFICATION
      !IT_ORDER_PARTNER type ZCL_PM_ORDER=>TT_ORDER_PARTNER
      !IT_ORDER_MATERIAL_DOC type ZCL_PM_ORDER=>TT_ORDER_MATERIAL_DOC
      !IT_ORDER_COMPONENTS type ZCL_PM_ORDER=>TT_ORDER_COMPONENT .
  methods CONVERT_STATUS_FOR_SEARCH
    importing
      !IT_STATUS_CONVERSION type ZCL_PM_MASTERDATA=>TT_STATUS_CONVERSION
    returning
      value(RT_STATUS_CONVERSION) type ZCL_PM_MASTERDATA=>TT_STATUS_CONVERSION .
  methods CONVERT_MATERIAL_STATUS_OUT
    importing
      !IS_ORDER_HEADER type ZCL_PM_ORDER=>TS_ORDER_HEADER
      !IT_ORDER_MATERIAL_DOC type ZCL_PM_ORDER=>TT_ORDER_MATERIAL_DOC
      !IT_ORDER_COMPONENTS type ZCL_PM_ORDER=>TT_ORDER_COMPONENT
    exporting
      !EV_MATERIAL_STATUS type STRING
      !EV_MATERIAL_STATUS_ICON type STRING .
  methods CONVERT_ORDER_DETAIL_OUT
    importing
      !IS_ORDER_HEADER type ZCL_PM_ORDER=>TS_ORDER_HEADER
      !IT_ORDER_STATUS type ZCL_PM_MASTERDATA=>TT_OBJECT_STATUS
      !IT_ORDER_NOTIFICATION type ZCL_PM_ORDER=>TT_ORDER_NOTIFICATION
      !IT_ORDER_PARTNER type ZCL_PM_ORDER=>TT_ORDER_PARTNER
      !IT_ORDER_MATERIAL_DOC type ZCL_PM_ORDER=>TT_ORDER_MATERIAL_DOC
      !IT_ORDER_COMPONENTS type ZCL_PM_ORDER=>TT_ORDER_COMPONENT .
  methods CONVERT_HISTORIC_DATA_OUT
    importing
      !IT_ORDER_ATTACHMENTS type ZCL_PM_ATTACHMENTS=>TT_GOS_ATTACHMENT_LIST
      !IT_ORDER_HEADER type ZCL_PM_ORDER=>TT_ORDER_HEADER
      !IT_ORDER_NOTIFICATION type ZCL_PM_ORDER=>TT_ORDER_NOTIFICATION
      !IT_ORDER_MATERIAL_DOC type ZCL_PM_ORDER=>TT_ORDER_MATERIAL_DOC .
  methods DETERMINE_ORDER_ACTIONS
    importing
      !IV_ROLE type ZPM_E_ROLE
      !IV_WORK_STATUS type ZPM_E_STATUS
      !IV_CENTER type WERKS_D
      !IV_JOB type ARBPL
    exporting
      !EV_EDITABLE type ABAP_BOOL
      !EV_JOB_EDITABLE type ABAP_BOOL
      !EV_CAN_CREATE_MATERIAL_DOC type ABAP_BOOL
      !EV_CAN_CREATE_NOTIFICATION type ABAP_BOOL
      !EV_CAN_CLOSE_ORDER type ABAP_BOOL
      !EV_CAN_ASSIGN_OFICIAL type ABAP_BOOL
      !EV_CAN_RELEASE_ORDER type ABAP_BOOL
      !EV_CAN_RELEASE_FROM_PROD type ABAP_BOOL
      !EV_MAT_DOC_OFICIAL_SELECTOR type ABAP_BOOL .
  methods DETERMINE_MASTER_LIST_ACTIONS
    importing
      !IV_ROLE type ZPM_E_ROLE
      !IV_CENTER type WERKS_D
    exporting
      !EV_CAN_CREATE_ORDER type ABAP_BOOL
      !EV_CAN_MASSIVE_ASSIGN type ABAP_BOOL
      !EV_CAN_CLOSE_ORDER type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_PM_ORDERS_APP_CNTRL IMPLEMENTATION.


  METHOD /neptune/if_nad_server~handle_on_ajax.

    ms_on_ajax = VALUE #( applid = applid
                          ajax_id = ajax_id
                          ajax_value = ajax_value
                          navigation = navigation
                          server = server
                          request = request ).

    DATA(ajax_method) = CONV seoclsname( |AJAX_{ ms_on_ajax-ajax_id }| ).


*try.
    CALL METHOD (ajax_method).


  ENDMETHOD.


  METHOD ajax_add_order_attachment.


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
                                                  iv_sap_object = CONV #( zcl_pm_attachments=>ms_bussines_object-order ) ).

    ENDIF.
  ENDMETHOD.


  method AJAX_ADD_ORDER_PICTURE.

    DATA: ls_file         TYPE zcl_pm_attachments=>ts_file.


    IF ms_camera_file IS NOT INITIAL.
      ls_file = CORRESPONDING #( ms_camera_file ).

      CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
        EXPORTING
          input          = ms_camera_file-content
*         UNESCAPE       = 'X'
       IMPORTING
         OUTPUT         = ls_file-content
       EXCEPTIONS
         FAILED         = 1
         OTHERS         = 2
                .
      IF sy-subrc <> 0.
      ENDIF.


      DATA(lo_attachment) = NEW zcl_pm_attachments( ).
      lo_attachment->upload_file_2_gos( EXPORTING is_file = ls_file
                                                  iv_object_id = CONV #( ms_on_ajax-ajax_value )
                                                  iv_sap_object = CONV #( zcl_pm_attachments=>ms_bussines_object-order ) ).

    ENDIF.

  endmethod.


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

    DATA:
*          ls_neptune_file TYPE /neptune/wa_request_files,
      ls_file         TYPE zcl_pm_attachments=>ts_file.

**   Se guarda el fichero subido en el servidor para recogerlo en la llamada al servicio ADD_IMAGE
*    ms_on_ajax-server->api_parameter_get( EXPORTING name = 'TMP_FILENAME'
*                               CHANGING value = ls_neptune_file-filename ).
*
*    ms_on_ajax-server->api_parameter_get( EXPORTING name = 'TMP_CONTENT'
*                               CHANGING value = ls_neptune_file-content ).
*
**   Se guarda el fichero subido en el servidor para recogerlo en la llamada al servicio ADD_IMAGE
*    ms_on_ajax-server->api_parameter_set( EXPORTING name = 'TMP_FILENAME'
*                                         value = '' ).
*
*    ms_on_ajax-server->api_parameter_set( EXPORTING name = 'TMP_CONTENT'
*                                         value = '' ).

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


  METHOD ajax_cancel_doc.

    DATA lv_doc TYPE mblnr.
    DATA lv_doc_year TYPE mjahr.


    DATA(lo_material) = NEW zcl_pm_material( ).

    CHECK ms_on_ajax-ajax_value IS NOT INITIAL.

    SPLIT ms_on_ajax-ajax_value AT '|' INTO lv_doc lv_doc_year.

    lo_material->cancel_material_movement( EXPORTING iv_doc = lv_doc iv_year = lv_doc_year IMPORTING et_return =  DATA(lt_return)  ).

    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( lt_return ).

  ENDMETHOD.


  METHOD ajax_check_user_has_notified.

    DATA(lv_aufnr) = |{ CONV aufnr( ms_on_ajax-ajax_value ) ALPHA = IN }|.

* Ini Santiago 21.07.21
*    SELECT SINGLE *
*      INTO @DATA(ls_dummy)
*      FROM zpm_t_log_cuest
*      WHERE aufnr EQ @lv_aufnr AND
*            uname EQ @ms_user_info-username.

    SELECT *
      FROM zpm_t_log_cuest
      WHERE aufnr EQ @lv_aufnr AND
            uname EQ @ms_user_info-username AND
            ndate EQ @sy-datum
      ORDER BY ndate DESCENDING
      INTO @DATA(ls_dummy)
      UP TO 1 ROWS.
    ENDSELECT.
* Fin Santiago 21.07.21

*  Si el usuario ya ha completado el formulario para esta orden, ese dia, se envia un warning a la aplicacion
    IF sy-subrc IS INITIAL.
      mt_log = VALUE #( ( type = 'W' ui5_type = 'Warning' ) ).
    ENDIF.


  ENDMETHOD.


  METHOD ajax_close_order.

    DATA: lr_order TYPE RANGE OF aufnr.

    DATA(lo_order) = NEW zcl_pm_order( ).

    CHECK ms_on_ajax-ajax_value IS NOT INITIAL.

    lr_order = VALUE #( ( sign = 'I' option = 'EQ' low = |{ CONV aufnr( ms_on_ajax-ajax_value ) ALPHA = IN }| ) ).

    DATA(lt_order_header) = lo_order->get_order_header( it_order = lr_order iv_complete_data = abap_false ).

    READ TABLE lt_order_header INTO DATA(ls_order_header) INDEX 1.

    DATA(lt_return) = lo_order->tecnical_close_order( is_order = ls_order_header ).

    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( lt_return ).

  ENDMETHOD.


  METHOD AJAX_CREATE_AND_PRINT_ORDER.

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


  METHOD ajax_create_material_movement.

    DATA(lo_material) = NEW zcl_pm_material( ).
    DATA(lo_user) = NEW zcl_pm_user( ).
    DATA(lo_order) = NEW zcl_pm_order( ).

    DATA: lv_employee_code TYPE pernr_d,
          lv_employee_name TYPE string,
          lr_order         TYPE RANGE OF aufnr,
          lr_outsource_job TYPE RANGE OF arbpl,
          lr_center        TYPE RANGE OF werks_d.

*   Si el documento lo esta creando un oficial o jefe de turno la retirada de material consta a su nombre
    CASE ms_user_info-role.
      WHEN zcl_pm_user=>mc_roles-oficial OR zcl_pm_user=>mc_roles-jefe_turno.
        lv_employee_code = ms_user_info-employee_id.
        lv_employee_name = ms_user_info-fullname.
      WHEN OTHERS.
        lv_employee_code =  ms_material_movement_create-employee.
        lv_employee_name = lo_user->get_user_name( lv_employee_code ).
    ENDCASE.

*   Se busca si la orden es de una contrata
    lr_order = VALUE #( ( sign = 'I' option = 'EQ' low = ms_material_movement_create-order ) ).
    DATA(lt_order_header) = lo_order->get_order_header( it_order = lr_order ).
    READ TABLE lt_order_header INTO DATA(ls_order_header) INDEX 1.

*   Contratas del centro
    lr_center = VALUE #( ( sign = 'I' option = 'EQ' low = ms_user_info-center ) ).
    DATA(lt_outsource_job) = zcl_pm_masterdata=>get_outsource_job( lr_center ).

*   Si la orden es de una contrata se graba con los datos de la misma
    READ TABLE lt_outsource_job ASSIGNING FIELD-SYMBOL(<ls_outsource_job>) WITH KEY job = ls_order_header-job.
    IF sy-subrc IS INITIAL.
      lv_employee_code = <ls_outsource_job>-job.
      lv_employee_name = <ls_outsource_job>-job_desc.
    ENDIF.

    DATA(ls_material_movement_header) = VALUE zcl_pm_material=>ts_material_movement_header( header_text = lv_employee_name ).

    DATA(lt_material_movement_item) = VALUE zcl_pm_material=>tt_material_movement_item(
          (  material  =  |{ CONV matnr( ms_material_movement_create-material ) ALPHA = IN }|
             center    = ms_user_info-center
             storage   = 'R001'
             move_type = '261'
             order     = |{ CONV aufnr( ms_material_movement_create-order ) ALPHA = IN }|
             quantity  = ms_material_movement_create-quantity
             user      = NEW zcl_pm_user( )->get_sap_user_from_pernr( |{ lv_employee_code ALPHA = IN }| )
             unit      = zcl_pm_utilidades=>convert_cunit_in( CONV meins( ms_material_movement_create-unit ) ) ) ).

    lo_material->create_material_movement( EXPORTING is_material_movement_header = ls_material_movement_header
                                                     it_material_movement_item = lt_material_movement_item
                                           IMPORTING ev_materialdocument = DATA(lv_material_doc)
                                                     ev_matdocumentyear	= DATA(lv_material_doc_year)
                                                     et_return          = DATA(lt_return) ).

    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( lt_return ).

  ENDMETHOD.


  METHOD ajax_create_new_order.

    DATA(lo_order) = NEW zcl_pm_order( ).
    DATA(lo_attachment) = NEW zcl_pm_attachments( ).
    DATA: ls_file  TYPE zcl_pm_attachments=>ts_file.

    DATA(ls_order_header) = convert_order_detail_in( ms_order_detail ).

* Se crea la orden
    " Informamos el centro del usuario
    ls_order_header-center = ms_user_info-center.
    lo_order->save_order(  EXPORTING is_order = ls_order_header it_assigments = mt_order_assignments IMPORTING et_return   = DATA(lt_return) ev_order_id = DATA(lv_order_id) ).

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


  METHOD ajax_create_order_notification.

    DATA ls_order_notif TYPE zcl_pm_order=>ts_order_notification.
    DATA(lo_order) = NEW zcl_pm_order( ).

    ls_order_notif = CORRESPONDING #( ms_order_notifi_create MAPPING long_text = notification_text
                                                                     is_final = is_final_yes ).
    ls_order_notif-employee_number = ms_user_info-employee_id.
* Se crea la notificación de la orden
    lo_order->create_order_notification(  EXPORTING   is_order_notif = ls_order_notif  IMPORTING et_return      = DATA(lt_return)    ).

* Convertimos los mensajes para el tipo de la aplicación
    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( it_return = lt_return ).
  ENDMETHOD.


  METHOD ajax_delete_attachment.

    DATA lt_return TYPE bapiret2_t.

    DATA(lo_attachment) = NEW zcl_pm_attachments( ).
    lo_attachment->delete_file_gos( EXPORTING iv_gos_attachment_id = CONV so_entryid(  ms_on_ajax-ajax_value )
                                              iv_sap_object = CONV #( zcl_pm_attachments=>ms_bussines_object-order )
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


  METHOD ajax_get_dialog_attachments.

    DATA ls_doc_dialog TYPE ts_dialog_doc.
    DATA lr_instid TYPE RANGE OF sibfboriid.
    DATA(lo_attachments) = NEW zcl_pm_attachments( ).

    ls_doc_dialog = ms_on_ajax-ajax_value.

* Documentación de la orden
    lr_instid = VALUE #( ( sign = 'I' option = 'EQ' low = ls_doc_dialog-aufnr ) ).
    DATA(lt_gos_attachment) = lo_attachments->get_gos_attachment_list( ir_instid = lr_instid
                                                                       iv_type_id = zcl_pm_attachments=>ms_bussines_object-order  ).

    LOOP AT lt_gos_attachment ASSIGNING FIELD-SYMBOL(<ls_gos_attachment>).
      APPEND INITIAL LINE TO mt_order_attachment ASSIGNING FIELD-SYMBOL(<ls_attachment_list>).
      <ls_attachment_list> = CORRESPONDING  #( <ls_gos_attachment> ).
      <ls_attachment_list>-document_desc = |{ <ls_attachment_list>-document_desc }.{ <ls_attachment_list>-file_ext }|.
    ENDLOOP.

* Documentación de la notificación
    IF ls_doc_dialog-qmnum IS NOT INITIAL.
      CLEAR lr_instid.

      lr_instid = VALUE #( ( sign = 'I' option = 'EQ' low = ls_doc_dialog-qmnum ) ).
      lt_gos_attachment = lo_attachments->get_gos_attachment_list( ir_instid = lr_instid
                                                                   iv_type_id = zcl_pm_attachments=>ms_bussines_object-notification  ).

      LOOP AT lt_gos_attachment ASSIGNING <ls_gos_attachment>.
        APPEND INITIAL LINE TO mt_notif_attachment ASSIGNING FIELD-SYMBOL(<ls_notif_attachment_list>).
        <ls_notif_attachment_list> = CORRESPONDING  #( <ls_gos_attachment> ).
        <ls_notif_attachment_list>-document_desc = |{ <ls_notif_attachment_list>-document_desc }.{ <ls_notif_attachment_list>-file_ext }|.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  method AJAX_GET_EMPLOYEE_NAME.

  data(lo_user) = new zcl_pm_user( ).
*  data(lv_sap_user) = lo_user->get_sap_user_from_pernr(  ).
  data(lv_user_name) = lo_user->get_user_name( |{ conv pernr_d( ms_employee-employee_code ) ALPHA = in }| ).

  ms_employee-employee_name = lv_user_name.

  endmethod.


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


  METHOD AJAX_GET_EQUIP_ATTACHMENTS.

    DATA: lr_instid TYPE RANGE OF sibfboriid.

    DATA(lo_attachments) = NEW zcl_pm_attachments( ).

    lr_instid = VALUE #( ( sign = 'I' option = 'EQ' low = |{ CONV equnr( ms_on_ajax-ajax_value ) ALPHA = IN }| ) ).
    DATA(lt_gos_attachment) = lo_attachments->get_gos_attachment_list( ir_instid = lr_instid
                                                                       iv_type_id = zcl_pm_attachments=>ms_bussines_object-equipment  ).

    LOOP AT lt_gos_attachment ASSIGNING FIELD-SYMBOL(<ls_gos_attachment>).
      APPEND INITIAL LINE TO mt_equipment_attachment ASSIGNING FIELD-SYMBOL(<ls_attachment_list>).
      <ls_attachment_list> = CORRESPONDING  #( <ls_gos_attachment> ).
      <ls_attachment_list>-document_desc = |{ <ls_attachment_list>-document_desc }.{ <ls_attachment_list>-file_ext }|.
    ENDLOOP.
  ENDMETHOD.


  METHOD ajax_get_master_list.

    DATA: lt_order                  TYPE RANGE OF aufnr,
          lt_order_class            TYPE RANGE OF aufart,
          lr_order_status           TYPE RANGE OF j_status,
          lt_objnr                  TYPE RANGE OF objnr,
          lt_placement              TYPE RANGE OF stort,
          lt_tec_location           TYPE RANGE OF tplnr,
          lt_equipment              TYPE RANGE OF equnr,
          lt_job                    TYPE RANGE OF arbpl,
          lr_active_job             TYPE RANGE OF arbpl,
          lt_date_creation          TYPE RANGE OF erdat,
          lt_center                 TYPE RANGE OF werks_d,
          lt_material_movement_type TYPE RANGE OF bwart,
          lr_pernr                  TYPE RANGE OF pernr_d,
          lr_instid                 TYPE RANGE OF sibfboriid,
          lt_status_conversion      TYPE zcl_pm_masterdata=>tt_status_conversion,
          lr_front_work_status      TYPE RANGE OF zpm_e_status,
          lr_front_material_status  TYPE RANGE OF zpm_e_status.

    DATA(lo_order) = NEW zcl_pm_order( ).
    DATA(lo_attachments) = NEW zcl_pm_attachments( ).
    DATA(lv_external_job) = zcl_pm_constants=>obtener_constantes( iv_constante = 'EXTERNAL_JOB_GROUP' ).

*  Se llenan los rangos para buscar con los filtros del front

*   El rango de fechas es obligatorio
    lt_date_creation = VALUE #( ( sign = 'I' option = 'BT' low = ms_search_filters-date_from high = ms_search_filters-date_to ) ).
    lt_center = VALUE #( ( sign = 'I' option = 'EQ' low = ms_user_info-center ) ).

*   Ordenes preventivas o no preventivas
    CASE ms_search_filters-order_class .

*     Se incluyen todas las ordenes preventivas
      WHEN zcl_pm_order=>ms_front_order_type-preventive.
        lt_order_class = VALUE #( ( sign = 'I' option = 'EQ' low = zcl_pm_order=>ms_back_order_type-preventive ) ).

*     Se excluyen las ordenes preventivas
      WHEN zcl_pm_order=>ms_front_order_type-no_preventive.
        lt_order_class = VALUE #( ( sign = 'E' option = 'EQ' low = zcl_pm_order=>ms_back_order_type-preventive ) ).
    ENDCASE.

*   Filtros simples
    IF ms_search_filters-order IS NOT INITIAL.
      lt_order = VALUE #( ( sign = 'I' option = 'EQ' low = CONV aufnr( |{ ms_search_filters-order ALPHA = IN }| ) ) ).
    ENDIF.

    IF ms_search_filters-tec_location IS NOT INITIAL.
      lt_tec_location = VALUE #( ( sign = 'I' option = 'EQ' low = CONV tplnr( |{ ms_search_filters-tec_location ALPHA = IN }| ) ) ).
    ENDIF.

    IF ms_search_filters-equipment IS NOT INITIAL.
      lt_equipment = VALUE #( ( sign = 'I' option = 'EQ' low = CONV equnr( |{ ms_search_filters-equipment ALPHA = IN }| ) ) ).
    ENDIF.

    IF ms_search_filters-employee_number IS NOT INITIAL.
      lr_pernr = VALUE #( ( sign = 'I' option = 'EQ' low = CONV pernr_d( |{ ms_search_filters-employee_number ALPHA = IN } | ) ) ).
    ENDIF.

*   Filtros múltiples
    lt_placement = VALUE #( FOR <f> IN mt_search_placement ( sign = 'I' option = 'EQ' low = <f>-key ) ).
    lt_job = VALUE #( FOR <g> IN mt_search_jobs WHERE ( key NE lv_external_job ) ( sign = 'I' option = 'EQ' low = <g>-key  ) ).

*   Si se han seleccionado las subcontratas se añaden todos los demas puestos de trabajo
    READ TABLE mt_search_jobs ASSIGNING FIELD-SYMBOL(<ls_search_jobs>) WITH KEY key = lv_external_job.
    IF sy-subrc IS INITIAL.
*      zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = 'ACTIVE_JOB_%'
*                                                      CHANGING ct_ranges = lr_active_job ).

      lr_active_job = zcl_pm_masterdata=>get_active_job_by_center( iv_werks = ms_user_info-center ).
      LOOP AT lr_active_job ASSIGNING FIELD-SYMBOL(<ls_active_job>).
        <ls_active_job>-sign = 'E'.
      ENDLOOP.

      DATA(lt_job_external) = zcl_pm_masterdata=>get_jobs( it_center = lt_center
                                                           it_job = lr_active_job ).

      lt_job = VALUE #( BASE lt_job FOR <h> IN lt_job_external ( sign = 'I' option = 'EQ' low = <h>-job ) ).

    ENDIF.

    IF mt_search_work_status IS NOT INITIAL.
*     Se convierten los estados del front al back
      lt_status_conversion = VALUE #( FOR <b> IN mt_search_work_status ( status_front = <b>-key  ) ).
      lt_status_conversion = convert_status_for_search( lt_status_conversion ).
      lr_order_status  = VALUE #( FOR <d> IN lt_status_conversion ( sign = 'I' option = 'EQ' low = <d>-status_back  ) ).
    ENDIF.

*   Se buscan las ordenes
    DATA(lt_order_id) = lo_order->search_orders( it_order = lt_order
                                                 it_order_class = lt_order_class
                                                 it_order_status = lr_order_status
                                                 it_placement = lt_placement
                                                 it_job       = lt_job
                                                 it_tec_location = lt_tec_location
                                                 it_equipment = lt_equipment
                                                 it_date_creation = lt_date_creation
                                                 it_center = lt_center
                                                 it_employee_number = lr_pernr ).

*   Si no encuentra ordenes se sale
    CHECK lt_order_id IS NOT INITIAL.

    lt_order = VALUE #( FOR <a> IN lt_order_id ( sign = 'I' option = 'EQ' low = <a> ) ).

*   Se recuperan los datos de cabecera de las ordenes
    DATA(lt_order_header) = lo_order->get_order_header( lt_order ).

*   Se recuperan los datos de notificaciones de la orden
    DATA(lt_order_notification) = lo_order->get_order_notifications( EXPORTING it_order = lt_order ).

*   Se recuperan los documentos de material de la orden
*    lt_material_movement_type = VALUE #( ( sign = 'I' option = 'EQ' low = zcl_pm_order=>mc_material_doc_warehouse ) ).
    DATA(lt_order_material_doc) = lo_order->get_order_material_docs( EXPORTING it_order = lt_order
                                                                               it_movement_type = lt_material_movement_type  ).

*   Se recuperan los datos de cabecera de las ordenes
    DATA(lt_order_components) = lo_order->get_order_components( EXPORTING it_order = lt_order iv_complete_data = abap_false ).

*   Se recuperan los estados de la orden
    lt_objnr = VALUE #( FOR <e> IN lt_order_header ( sign = 'I' option = 'EQ' low = <e>-objnr ) ).
    DATA(lt_order_status) = zcl_pm_masterdata=>get_object_status( lt_objnr ).

*   Se recuperan los estados de la orden
    DATA(lt_order_partners) = lo_order->get_order_partners( lt_objnr ).

*   Se recuperan los anexos de las ordenes y notificaciones
    lr_instid = VALUE #( FOR <e> IN lt_order_header ( sign = 'I' option = 'EQ' low = <e>-order ) ).
    DATA(lt_order_attachment) = lo_attachments->get_gos_attachment_list( ir_instid = lr_instid
                                                                         iv_type_id = zcl_pm_attachments=>ms_bussines_object-order  ).

    CLEAR lr_instid.
    lr_instid = VALUE #( FOR <e> IN lt_order_header ( sign = 'I' option = 'EQ' low = <e>-notification_id ) ).
    DATA(lt_notif_attachment) = lo_attachments->get_gos_attachment_list( ir_instid = lr_instid
                                                                         iv_type_id = zcl_pm_attachments=>ms_bussines_object-notification  ).
    APPEND LINES OF lt_notif_attachment TO lt_order_attachment.

*   Con todos los datos se monta la tabla de salida
    convert_order_list_out( it_order_header = lt_order_header
                            it_order_status = lt_order_status
                            it_order_attachments = lt_order_attachment
                            it_order_notification = lt_order_notification
                            it_order_partner = lt_order_partners
                            it_order_material_doc = lt_order_material_doc
                            it_order_components = lt_order_components  ).

*   Se borran de la tabla de salida los estados que no se han podido filtrar previamente (Estados que no dependen de los status de la orden)
*   Por comodidad se añaden todos al rango aunque ciertos estados vendran filtrados previamente
    lr_front_work_status = VALUE #( FOR <g> IN mt_search_work_status ( sign = 'I' option = 'EQ' low = <g>-key ) ).
    lr_front_material_status = VALUE #( FOR <f> IN mt_search_material_status ( sign = 'I' option = 'EQ' low = <f>-key ) ).

    IF lr_front_work_status IS NOT INITIAL.
      DELETE mt_master_list WHERE work_status NOT IN lr_front_work_status.
    ENDIF.
    IF lr_front_material_status IS NOT INITIAL.
      DELETE mt_master_list WHERE material_status NOT IN lr_front_material_status.
    ENDIF.

    SORT mt_master_list BY work_status placement tec_location_desc equipment_desc DESCENDING.

  ENDMETHOD.


  METHOD ajax_get_material_detail.

    DATA: lr_material TYPE RANGE OF matnr.

    lr_material = VALUE #( ( sign = 'I' option = 'EQ' low = |{ CONV matnr( ms_material_movement_create-material ) ALPHA = IN }| ) ).

    DATA(lt_material_header) = NEW zcl_pm_material( )->get_material_header( it_material = lr_material ).
    READ TABLE lt_material_header ASSIGNING FIELD-SYMBOL(<ls_material_header>) INDEX 1.
    IF sy-subrc IS INITIAL.
      ms_material_movement_create-material_desc = <ls_material_header>-material_desc.
      ms_material_movement_create-unit = zcl_pm_utilidades=>convert_cunit_out( <ls_material_header>-unit ).
    ENDIF.

  ENDMETHOD.


  METHOD AJAX_GET_NOTIF_ATTACHMENTS.

    DATA: lr_instid TYPE RANGE OF sibfboriid.

    DATA(lo_attachments) = NEW zcl_pm_attachments( ).

    lr_instid = VALUE #( ( sign = 'I' option = 'EQ' low = |{ CONV qmnum( ms_on_ajax-ajax_value ) ALPHA = IN }| ) ).
    DATA(lt_gos_attachment) = lo_attachments->get_gos_attachment_list( ir_instid = lr_instid
                                                                       iv_type_id = zcl_pm_attachments=>ms_bussines_object-notification  ).

    LOOP AT lt_gos_attachment ASSIGNING FIELD-SYMBOL(<ls_gos_attachment>).
      APPEND INITIAL LINE TO mt_notif_attachment ASSIGNING FIELD-SYMBOL(<ls_attachment_list>).
      <ls_attachment_list> = CORRESPONDING  #( <ls_gos_attachment> ).
      <ls_attachment_list>-document_desc = |{ <ls_attachment_list>-document_desc }.{ <ls_attachment_list>-file_ext }|.
    ENDLOOP.
  ENDMETHOD.


  METHOD ajax_get_oficial_for_job.

    DATA: lt_objnr  TYPE RANGE OF j_objnr,
          lt_job_id TYPE RANGE OF cr_objid,
          lr_job    TYPE RANGE OF arbpl,
          lr_center TYPE RANGE OF werks_d.

*   Se recuperan interlocutores de la orden
    DATA(lo_order) = NEW zcl_pm_order( ).
    lt_objnr = VALUE #( ( sign = 'I' option = 'EQ' low = ms_order_detail-objnr ) ).
    DATA(lt_order_partner) = lo_order->get_order_partners( lt_objnr ).

*   Se busca el id del puesto de trabajo
    lr_job = VALUE #( ( sign = 'I' option = 'EQ' low = ms_order_detail-job ) ).
    lr_center =  VALUE #( ( sign = 'I' option = 'EQ' low = ms_user_info-center ) ).
    DATA(lt_jobs) = zcl_pm_masterdata=>get_jobs( it_job = lr_job it_center = lr_center ).
    READ TABLE lt_jobs ASSIGNING FIELD-SYMBOL(<ls_job>) INDEX 1.
*   Si no se encuentra el pto de trab se sale
    CHECK sy-subrc IS INITIAL.

    lt_job_id = VALUE #( FOR <ls_jobs> IN lt_jobs ( sign = 'I' option = 'EQ' low = <ls_jobs>-job_id ) ).

*   Se monta la tabla de salida de asignaciones
    convert_order_assignments_out( it_job_id = lt_job_id
                                   it_order_partner = lt_order_partner ).

  ENDMETHOD.


  METHOD ajax_get_order_assignments.

    DATA: lt_order  TYPE RANGE OF aufnr,
          lt_objnr  TYPE RANGE OF j_objnr,
          lt_job_id TYPE RANGE OF cr_objid,
          lr_center TYPE RANGE OF werks_d,
          lr_job    TYPE RANGE OF arbpl.

    DATA(lo_order) = NEW zcl_pm_order( ).

    lt_order = VALUE #( ( sign = 'I' option = 'EQ' low = CONV aufnr( |{ ms_on_ajax-ajax_value ALPHA = IN }| ) ) ).

*   Se recuperan los datos de cabecera de la orden
    DATA(lt_order_header) = lo_order->get_order_header( lt_order ).
    READ TABLE lt_order_header INTO DATA(ls_order_header) INDEX 1.

*   Se recuperan interlocutores de la orden
    lt_objnr = VALUE #( ( sign = 'I' option = 'EQ' low = ls_order_header-objnr ) ).
    DATA(lt_order_partner) = lo_order->get_order_partners( lt_objnr ).


    " 13.08.2021 - Se muestran todos los oficiales de todos los puestos de trabajo para todos los centros.

    " Para el centro de Villagarcia cargamos todos los oficiales independientemente del puesto de trabajo
    lr_center =  VALUE #( ( sign = 'I' option = 'EQ' low = ls_order_header-center ) ).
    DATA(lt_jobs) = zcl_pm_masterdata=>get_jobs( it_job = lr_job it_center = lr_center ).
    IF lt_jobs IS NOT INITIAL.
      lt_job_id = VALUE #( FOR <ls_jobs> IN lt_jobs ( sign = 'I' option = 'EQ' low = <ls_jobs>-job_id ) ).
    ENDIF.
*    ELSE.
*   Se busca el id del puesto de trabajo
*      lt_job_id = VALUE #( ( sign = 'I' option = 'EQ' low = ls_order_header-job_id ) ).
*    ENDIF.


*   Se monta la tabla de salida de asignaciones
    convert_order_assignments_out( it_job_id = lt_job_id
                                   it_order_partner = lt_order_partner ).

    " 13.08.2021  Se muestran todos los oficiales aunque no estén asignados a la orden
**   Solo se devuelven los asignados
*    DELETE mt_order_assignments WHERE assigned IS INITIAL.

*sort mt_order_component by
    " Para Granda
*    IF ls_order_header-center EQ zif_pm_data=>cs_center-granda.
      SORT mt_order_assignments BY job_id.
*      LOOP AT lt_jobs ASSIGNING FIELD-SYMBOL(<fs_jobs>).
*        " Los puestos de trabajo que no tienen oficiales,se añaden.
*        READ TABLE mt_order_assignments TRANSPORTING NO FIELDS WITH KEY job_id = <fs_jobs>-job_id.
*        IF sy-subrc NE 0.
*          APPEND INITIAL LINE TO mt_order_assignments ASSIGNING FIELD-SYMBOL(<fs_order_ass>).
*          <fs_order_ass>-employee_name = <fs_jobs>-job_desc.
*          <fs_order_ass>-employee_code = <fs_jobs>-job.
*          <fs_order_ass>-job_id = <fs_jobs>-job_id.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.



  ENDMETHOD.


  METHOD ajax_get_order_attachments.

    DATA: lr_instid TYPE RANGE OF sibfboriid.

    DATA(lo_attachments) = NEW zcl_pm_attachments( ).

    lr_instid = VALUE #( ( sign = 'I' option = 'EQ' low = |{ CONV aufnr( ms_on_ajax-ajax_value ) ALPHA = IN }| ) ).
    DATA(lt_gos_attachment) = lo_attachments->get_gos_attachment_list( ir_instid = lr_instid
                                                                       iv_type_id = zcl_pm_attachments=>ms_bussines_object-order  ).

    LOOP AT lt_gos_attachment ASSIGNING FIELD-SYMBOL(<ls_gos_attachment>).
      APPEND INITIAL LINE TO mt_order_attachment ASSIGNING FIELD-SYMBOL(<ls_attachment_list>).
      <ls_attachment_list> = CORRESPONDING  #( <ls_gos_attachment> ).
      <ls_attachment_list>-document_desc = |{ <ls_attachment_list>-document_desc }.{ <ls_attachment_list>-file_ext }|.
    ENDLOOP.
  ENDMETHOD.


  METHOD ajax_get_order_detail.

    DATA: lt_order                  TYPE RANGE OF aufnr,
          lt_objnr                  TYPE RANGE OF j_objnr,
          lt_job_id                 TYPE RANGE OF cr_objid,
          lt_material_movement_type TYPE RANGE OF bwart.

    DATA(lo_order) = NEW zcl_pm_order( ).

    lt_order = VALUE #( ( sign = 'I' option = 'EQ' low = CONV aufnr( |{ ms_on_ajax-ajax_value ALPHA = IN }| ) ) ).

*   Se recuperan los datos de cabecera de la orden
    DATA(lt_order_header) = lo_order->get_order_header( lt_order ).
    READ TABLE lt_order_header INTO DATA(ls_order_header) INDEX 1.

*   Se recuperan los datos de notificaciones de la orden
    DATA(lt_order_notification) = lo_order->get_order_notifications( EXPORTING it_order = lt_order ).

*   Se recuperan los documentos de material de la orden
" De momento recuperamos todos los documentos asociados a la orden.
*    lt_material_movement_type = VALUE #( ( sign = 'I' option = 'EQ' low = zcl_pm_order=>mc_material_doc_warehouse ) ).
    DATA(lt_order_material_doc) = lo_order->get_order_material_docs( EXPORTING it_order = lt_order
                                                                               it_movement_type = lt_material_movement_type  ).

*   Se recuperan los componentes de la orden
    DATA(lt_order_components) = lo_order->get_order_components( EXPORTING it_order = lt_order ).

*   Se recuperan los estados de la orden
    lt_objnr = VALUE #( ( sign = 'I' option = 'EQ' low = ls_order_header-objnr ) ).

*   Se recuperan los datos de cabecera de las notificaciones
    DATA(lt_order_status) = zcl_pm_masterdata=>get_object_status( lt_objnr ).

*   Se recuperan interlocutores de la orden
    DATA(lt_order_partner) = lo_order->get_order_partners( lt_objnr ).

**   Se recupera el texto largo/detalle del aviso

    lo_order->read_order_long_descr( EXPORTING iv_order_id = ls_order_header-order CHANGING cv_notif_long_desc = ls_order_header-order_long_desc ).

    convert_order_detail_out( is_order_header = ls_order_header
                              it_order_status = lt_order_status
                              it_order_notification = lt_order_notification
                              it_order_components = lt_order_components
                              it_order_partner = lt_order_partner
                              it_order_material_doc = lt_order_material_doc ).

  ENDMETHOD.


  METHOD ajax_get_order_history.

    DATA: lt_order                  TYPE RANGE OF aufnr,
          lt_equipment              TYPE RANGE OF equnr,
          lt_historic_dates         TYPE RANGE OF datum,
          lr_instid                 TYPE RANGE OF sibfboriid,
          lt_material_movement_type TYPE RANGE OF bwart,
          lv_date_from              TYPE sy-datum.

    CASE ms_user_info-center.
      WHEN zif_pm_data=>cs_center-granda.
        DATA(lv_historic_months) = CONV dlymo( zcl_pm_constants=>obtener_constantes( iv_constante = 'ORDER_HISTORIC_MONTHS_1001' ) ).
      WHEN zif_pm_data=>cs_center-lugo.
        lv_historic_months = CONV dlymo( zcl_pm_constants=>obtener_constantes( iv_constante = 'ORDER_HISTORIC_MONTHS_1002' ) ).
      WHEN zif_pm_data=>cs_center-villagarcia.
        lv_historic_months = CONV dlymo( zcl_pm_constants=>obtener_constantes( iv_constante = 'ORDER_HISTORIC_MONTHS_1004' ) ).
      WHEN OTHERS.
        lv_historic_months = CONV dlymo( zcl_pm_constants=>obtener_constantes( iv_constante = 'ORDER_HISTORIC_MONTHS_1001' ) ).
    ENDCASE.


    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = 0
        months    = lv_historic_months
*       months    = 40 "Test
        signum    = '-'
        years     = 0
      IMPORTING
        calc_date = lv_date_from.

    DATA(lo_order) = NEW zcl_pm_order( ).
    DATA(lo_attachments) = NEW zcl_pm_attachments( ).

    lt_order = VALUE #( ( sign = 'I' option = 'EQ' low = CONV aufnr( |{ ms_on_ajax-ajax_value ALPHA = IN }| ) ) ).

*   Se recuperan los datos de cabecera de la orden
    DATA(lt_order_header) = lo_order->get_order_header( lt_order ).
    READ TABLE lt_order_header INTO DATA(ls_order_header) INDEX 1.

*   Si la orden no tiene equipo no se busca historial
    CHECK ls_order_header-equipment IS NOT INITIAL.

*   Se buscan las ordenes del equipo en determinadas fechas y excluyendo la orden actual
    lt_equipment = VALUE #( ( sign = 'I' option = 'EQ' low = ls_order_header-equipment ) ).
    lt_historic_dates = VALUE #( ( sign = 'I' option = 'BT' low = lv_date_from high = sy-datum ) ).
    lt_order = VALUE #( ( sign = 'E' option = 'EQ' low = CONV aufnr( |{ ms_on_ajax-ajax_value ALPHA = IN }| ) ) ).
    DATA(lt_historic_orders) = lo_order->search_orders( it_equipment = lt_equipment
                                                        it_date_creation = lt_historic_dates
                                                        it_order = lt_order ).

    lt_order = VALUE #( FOR <a> IN lt_historic_orders ( sign = 'I' option = 'EQ' low = <a> ) ).

    CHECK lt_order IS NOT INITIAL.

*   Se recuperan los datos de cabecera de las ordenes del historico
    DATA(lt_historic_order_header) = lo_order->get_order_header( lt_order ).

*   Se recuperan los datos de notificaciones de las ordenes del historico
    DATA(lt_order_notification) = lo_order->get_order_notifications( EXPORTING it_order = lt_order ).

*   Se recuperan los movimientos de material de la orden
*    lt_material_movement_type = VALUE #( ( sign = 'I' option = 'EQ' low = zcl_pm_order=>mc_material_doc_warehouse ) ).
    DATA(lt_order_material_doc) = lo_order->get_order_material_docs( EXPORTING it_order = lt_order
                                                                               it_movement_type = lt_material_movement_type  ).

*   Se recuperan los anexos de las ordenes
    lr_instid = VALUE #( FOR <b> IN lt_historic_order_header ( sign = 'I' option = 'EQ' low = <b>-order ) ).
    DATA(lt_order_attachment) = lo_attachments->get_gos_attachment_list( ir_instid = lr_instid
                                                                         iv_type_id = zcl_pm_attachments=>ms_bussines_object-order ).

    convert_historic_data_out( it_order_header = lt_historic_order_header
                               it_order_notification = lt_order_notification
                               it_order_attachments = lt_order_attachment
                               it_order_material_doc = lt_order_material_doc ).

  ENDMETHOD.


  METHOD ajax_get_task_list_attachments.

    DATA: lr_instid TYPE RANGE OF sibfboriid.

    DATA(lo_attachments) = NEW zcl_pm_attachments( ).

    " Recuperamos de la orden la hoja de ruta, si la tiene buscamos si tiene documentación
    DATA(lv_aufnr) = |{ CONV aufnr( ms_on_ajax-ajax_value ) ALPHA = IN }|.

    SELECT SINGLE plnty, plnnr, plnal
      FROM viaufkst
      INTO @DATA(ls_task_list)
      WHERE aufnr EQ @lv_aufnr.
    IF ls_task_list-plnnr IS NOT INITIAL.
      INSERT VALUE #( sign = 'I' option = 'EQ' low = |{ ls_task_list-plnty }{ ls_task_list-plnnr }{ ls_task_list-plnal }| ) INTO TABLE lr_instid.
      INSERT VALUE #( sign = 'I' option = 'EQ' low = |{ ls_task_list-plnty }{ ls_task_list-plnnr }| ) INTO TABLE lr_instid.
      DATA(lt_gos_attachment) = lo_attachments->get_gos_attachment_list( ir_instid = lr_instid
                                                                         iv_type_id = zcl_pm_attachments=>ms_bussines_object-task_list  ).

      LOOP AT lt_gos_attachment ASSIGNING FIELD-SYMBOL(<ls_gos_attachment>).
        APPEND INITIAL LINE TO mt_task_list_attachment ASSIGNING FIELD-SYMBOL(<ls_attachment_list>).
        <ls_attachment_list> = CORRESPONDING  #( <ls_gos_attachment> ).
        <ls_attachment_list>-document_desc = |{ <ls_attachment_list>-document_desc }.{ <ls_attachment_list>-file_ext }|.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD ajax_get_user_info.

    ms_user_info = NEW zcl_pm_user( )->get_user_info( sy-uname ).

  ENDMETHOD.


  METHOD ajax_init_aplication.

    DATA: lv_date_to     TYPE begda,
          lv_date_from   TYPE begda,
          lr_status_type TYPE RANGE OF zpm_e_status_type,
          lr_center      TYPE RANGE OF werks_d,
          lr_equnr       TYPE RANGE OF equnr,
          lr_job         TYPE RANGE OF arbpl,
          lr_active_job  TYPE RANGE OF arbpl,
          lr_placement   TYPE RANGE OF stort.

*   Fechas por defecto
    lv_date_to = sy-datum.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_date_to
        days      = 14
        months    = 0
        signum    = '-'
        years     = 0
      IMPORTING
        calc_date = lv_date_from.
    ms_search_filters-date_from = lv_date_from.
    ms_search_filters-date_to = lv_date_to.
    ms_search_filters-order_class = zcl_pm_order=>ms_front_order_type-no_preventive.

*   Recupera los dos maestros de estados
    lr_status_type = VALUE #(  sign = 'I' option = 'EQ' ( low = zcl_pm_order=>mc_front_work_status_type )
                                                        ( low = zcl_pm_order=>mc_front_material_status_type ) ).
    DATA(lt_status) = zcl_pm_masterdata=>get_master_front_status( lr_status_type ).
    mt_masterdata_work_status = VALUE #( FOR <a> IN lt_status WHERE ( status_type = zcl_pm_order=>mc_front_work_status_type ) (  key = <a>-status value = <a>-status_desc ) ).
    mt_masterdata_material_status = VALUE #( FOR <a> IN lt_status WHERE ( status_type = zcl_pm_order=>mc_front_material_status_type ) ( key = <a>-status value = <a>-status_desc ) ).

    SORT mt_masterdata_work_status ASCENDING BY key.
    SORT mt_masterdata_material_status ASCENDING BY key.

    IF ms_user_info-center EQ zif_pm_data=>cs_center-granda.
      DELETE mt_masterdata_work_status WHERE key EQ 'ST00'.
    ENDIF.

*   Recupera los puestos de trabajo responsable
    lr_center = VALUE #( ( sign = 'I' option = 'EQ' low = ms_user_info-center ) ).

    DATA(lt_job) = zcl_pm_masterdata=>get_jobs( it_center = lr_center ).
    lr_active_job = zcl_pm_masterdata=>get_active_job_by_center( iv_werks = ms_user_info-center ).
*    zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = 'ACTIVE_JOB_%'
*                                                    CHANGING ct_ranges = lr_active_job ).

    mt_masterdata_all_jobs = CORRESPONDING #( lt_job MAPPING key = job value = job_desc ).


    DELETE lt_job WHERE job NOT IN lr_active_job.
    mt_masterdata_jobs = CORRESPONDING #( lt_job MAPPING key = job value = job_desc ).

*   Rellenamos la tabla para indentificar los puestos internos
    mt_internal_jobs = CORRESPONDING #( lt_job ).

    DATA(lv_external_job) = zcl_pm_constants=>obtener_constantes( iv_constante = 'EXTERNAL_JOB_GROUP' ).
    DATA(lv_external_job_desc) = zcl_pm_constants=>obtener_constantes( iv_constante = 'EXTERNAL_JOB_GROUP_DESC' ).
    mt_masterdata_jobs = VALUE #( BASE mt_masterdata_jobs ( key = lv_external_job value = lv_external_job_desc ) ).



*   Recupera los emplazamientos
    DATA(lt_placement) = zcl_pm_masterdata=>get_placements( iv_adjust_desc = abap_true it_center = lr_center ).
    mt_masterdata_placement = CORRESPONDING #( lt_placement MAPPING key = placement value = placement_desc ).

*   Recupera las ubicaciones técnicas
    DATA(lt_hierarchy_tec_location) = zcl_pm_masterdata=>get_tecnical_hierarchy( it_center = lr_center ).
    mt_hierarchy_tec_location = CORRESPONDING #( lt_hierarchy_tec_location ).

*   Equipos
    DATA(lt_hierarchy_equipment) = zcl_pm_masterdata=>get_tecnical_hierarchy( it_center = lr_center iv_include_equipments = abap_true ).
    mt_hierarchy_equipment = CORRESPONDING #( lt_hierarchy_equipment ).

*  Emplazamiento por defecto
    IF ms_user_info-default_placement IS NOT INITIAL.
      APPEND INITIAL LINE TO mt_search_placement ASSIGNING FIELD-SYMBOL(<ls_search_placement>).
      <ls_search_placement>-key = ms_user_info-default_placement.
      lr_placement = VALUE #( ( sign = 'I' option = 'EQ' low = ms_user_info-default_placement ) ).
    ENDIF.

* Recupera las clases de ordenes para la creación
    DATA(lt_orderclasses) = zcl_pm_masterdata=>get_order_classes( iv_langu = sy-langu ).
    mt_masterdata_orderclass = CORRESPONDING #( lt_orderclasses MAPPING key = class_id value = class_desc ).

* Datos por defecto en funcion del rol
    CASE ms_user_info-role.
      WHEN zcl_pm_user=>mc_roles-almacenero.
        mt_search_material_status = VALUE #( ( key = zcl_pm_order=>ms_front_material_status-assigned )
                                             ( key = zcl_pm_order=>ms_front_material_status-in_preparation ) ).
      WHEN zcl_pm_user=>mc_roles-oficial.
        ms_search_filters-employee_number = |{ CONV pernr_d( ms_user_info-employee_id ) ALPHA = OUT }|.
        mt_search_work_status = VALUE #( ( key = zcl_pm_order=>ms_front_work_status-assigned )
                                         ( key = zcl_pm_order=>ms_front_work_status-started )
                                         ( key = zcl_pm_order=>ms_front_work_status-completed ) ).

      WHEN zcl_pm_user=>mc_roles-jefe_turno.
        mt_search_work_status = VALUE #( ( key = zcl_pm_order=>ms_front_work_status-open )
                                         ( key = zcl_pm_order=>ms_front_work_status-pending )
                                         ( key = zcl_pm_order=>ms_front_work_status-assigned )
                                         ( key = zcl_pm_order=>ms_front_work_status-started )
                                         ( key = zcl_pm_order=>ms_front_work_status-completed )
                                         ( key = zcl_pm_order=>ms_front_work_status-released_by_prod ) ).

        IF ms_user_info-default_job IS NOT INITIAL.
          mt_search_jobs = VALUE #( ( key = ms_user_info-default_job ) ).
          IF ms_user_info-default_job EQ 'MEC' AND ms_user_info-center EQ zif_pm_data=>cs_center-granda.
            APPEND INITIAL LINE TO mt_search_jobs ASSIGNING FIELD-SYMBOL(<fs_s_job>).
            <fs_s_job>-key = 'TUBINOX'.
          ENDIF.
        ENDIF.

    ENDCASE.

*   Accione sobre el listado
    determine_master_list_actions( EXPORTING iv_role = CONV zpm_e_role( ms_user_info-role )
                                             iv_center = CONV werks_d( ms_user_info-center )
                                   IMPORTING ev_can_create_order = ms_master_list_actions-can_create_order
                                             ev_can_massive_assign = ms_master_list_actions-can_massive_assign
                                             ev_can_close_order = ms_master_list_actions-can_close_order ).

  ENDMETHOD.


  METHOD ajax_massive_assignment.
    DATA(lo_order) = NEW zcl_pm_order( ).

    DATA(ls_order_header) = convert_order_detail_in( ms_order_detail ).

* Se modifican los datos de la notificación
    lo_order->massive_assign( EXPORTING it_orders   = mt_massive_assign_order it_oficials = mt_massive_oficial IMPORTING  et_return = DATA(lt_return) ).

* Convertimos los mensajes para el tipo de la aplicación
    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( it_return = lt_return ).


  ENDMETHOD.


  METHOD ajax_massive_assignment_init.

    DATA: lt_order  TYPE RANGE OF aufnr.
    DATA: lt_order_aux  TYPE RANGE OF aufnr.
    DATA: lt_order_of  TYPE RANGE OF aufnr.
    DATA: lt_objnr             TYPE RANGE OF j_objnr,
          lt_job_id            TYPE RANGE OF cr_objid,
          lr_job               TYPE RANGE OF arbpl,
          lr_center            TYPE RANGE OF werks_d,
          lt_status_conversion TYPE zcl_pm_masterdata=>tt_status_conversion.

    DATA: lt_order_class   TYPE RANGE OF aufart,
          lr_order_status  TYPE RANGE OF j_status,
          lt_placement     TYPE RANGE OF stort,
          lt_tec_location  TYPE RANGE OF tplnr,
          lt_equipment     TYPE RANGE OF equnr,
          lt_job           TYPE RANGE OF arbpl,
          lr_active_job    TYPE RANGE OF arbpl,
          lt_date_creation TYPE RANGE OF erdat,
          lt_center        TYPE RANGE OF werks_d,
          lr_pernr         TYPE RANGE OF pernr_d,
          lr_objnr         TYPE RANGE OF objnr,
          lv_date_to       TYPE begda,
          lv_date_from     TYPE begda.

    " Recuperamos la información de las órdenes
    lt_order = VALUE #( FOR <fs_sel> IN mt_selected_orders ( sign = 'I' option = 'EQ' low = CONV aufnr( |{ <fs_sel>-key ALPHA = IN }| ) ) ).
    DATA(lo_order) = NEW zcl_pm_order( ).
*   Se recuperan los datos de cabecera de la orden
    DATA(lt_order_header) = lo_order->get_order_header( lt_order ).

    " Rellenamos los datos generales de la orden
    LOOP AT lt_order_header ASSIGNING FIELD-SYMBOL(<fs_order>).
      APPEND INITIAL LINE TO mt_massive_assign_order ASSIGNING FIELD-SYMBOL(<fs_massive_assign_order>).
      <fs_massive_assign_order>-order = <fs_order>-order.
      <fs_massive_assign_order>-end_date = <fs_order>-end_date.
      <fs_massive_assign_order>-order_desc = <fs_order>-order_short_desc.
      <fs_massive_assign_order>-objnr = <fs_order>-objnr.
      <fs_massive_assign_order>-equipment_desc = <fs_order>-equipment_desc.
    ENDLOOP.

    " Recuperamos el puesto de trabajo de las órdenes (que tiene que ser el mismo)
    READ TABLE lt_order_header ASSIGNING FIELD-SYMBOL(<fs_ord_job>) INDEX 1.
    IF sy-subrc EQ 0.

*   Se busca el id del puesto de trabajo
      lr_job = VALUE #( ( sign = 'I' option = 'EQ' low = <fs_ord_job>-job ) ).
      lr_center =  VALUE #( ( sign = 'I' option = 'EQ' low = ms_user_info-center ) ).
      DATA(lt_jobs) = zcl_pm_masterdata=>get_jobs( it_job = lr_job it_center = lr_center ).
      READ TABLE lt_jobs ASSIGNING FIELD-SYMBOL(<ls_job>) INDEX 1.
*   Si no se encuentra el pto de trab se sale
      IF sy-subrc IS INITIAL.

        lt_job_id = VALUE #( ( sign = 'I' option = 'EQ' low = <ls_job>-job_id ) ).

        DATA(lt_job_employee) =  zcl_pm_masterdata=>get_employee_from_job( it_job_id = lt_job_id
                                                                           iv_date = sy-datum ).
        LOOP AT lt_job_employee ASSIGNING FIELD-SYMBOL(<fs_job_empl>).
          APPEND INITIAL LINE TO mt_massive_oficial ASSIGNING FIELD-SYMBOL(<fs_mass_of>).
          <fs_mass_of>-employee_id = <fs_job_empl>-pernr.
          <fs_mass_of>-employee = <fs_job_empl>-full_name.
        ENDLOOP.
        SORT mt_massive_oficial BY employee.
      ENDIF.

* Recuperamos las órdenes a las que están asignadas los oficiales

*   Se buscan las ordenes de los oficiales, para unos determinados estados

      lt_job = VALUE #( FOR <g> IN mt_search_jobs ( sign = 'I' option = 'EQ' low = <fs_ord_job>-job  ) ).

      INSERT VALUE #( status_front = zcl_pm_order=>ms_front_work_status-started ) INTO TABLE lt_status_conversion.
      INSERT VALUE #( status_front = zcl_pm_order=>ms_front_work_status-assigned ) INTO TABLE lt_status_conversion.

      lt_status_conversion = convert_status_for_search( lt_status_conversion ).
      lr_order_status  = VALUE #( FOR <d> IN lt_status_conversion ( sign = 'I' option = 'EQ' low = <d>-status_back  ) ).


      lr_pernr = VALUE #( FOR <f> IN lt_job_employee ( sign = 'I' option = 'EQ' low = CONV pernr_d( |{ <f>-pernr ALPHA = IN } | ) ) ).
*   Fechas por defecto
      lv_date_to = sy-datum.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = lv_date_to
          days      = 0
          months    = 3
          signum    = '-'
          years     = 0
        IMPORTING
          calc_date = lv_date_from.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = sy-datum
          days      = 0
          months    = 3
          signum    = '+'
          years     = 0
        IMPORTING
          calc_date = lv_date_to.

      lt_date_creation = VALUE #( ( sign = 'I' option = 'BT' low = lv_date_from high = lv_date_to ) ).

      DATA(lt_order_id) = lo_order->search_orders( it_order = lt_order_aux
                                                   it_order_class = lt_order_class
                                                   it_order_status = lr_order_status
                                                   it_placement = lt_placement
                                                   it_job       = lt_job
                                                   it_tec_location = lt_tec_location
                                                   it_equipment = lt_equipment
                                                   it_date_creation = lt_date_creation
                                                   it_center = lt_center
                                                   it_employee_number = lr_pernr ).

*   Si no encuentra ordenes se sale
      CHECK lt_order_id IS NOT INITIAL.

      lt_order_of = VALUE #( FOR <a> IN lt_order_id ( sign = 'I' option = 'EQ' low = <a> ) ).

*   Se recuperan los datos de cabecera de las ordenes
      DATA(lt_order_header_off) = lo_order->get_order_header( lt_order_of ).

*     Rango con objnr para buscar los interlocutores
      lr_objnr = VALUE #( FOR <c> IN lt_order_header_off ( sign = 'I' option = 'EQ' low = <c>-objnr ) ).

      DATA(lv_oficial_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'OFICIAL_ORDER_PARVW' ) ).

      SELECT objnr, parnr
        FROM ihpa
        INTO TABLE @DATA(lt_ihpa)
        WHERE objnr IN @lr_objnr AND
              parvw EQ @lv_oficial_parvw AND
              parnr IN @lr_pernr AND
              kzloesch EQ @abap_false.

      LOOP AT lt_ihpa ASSIGNING FIELD-SYMBOL(<fs_ihpa>).
        DATA(lv_order) = CONV aufnr( <fs_ihpa>-objnr+2(12) ).
        APPEND INITIAL LINE TO mt_massive_assign_oficial ASSIGNING FIELD-SYMBOL(<fs_massive_assign_off>).
        READ TABLE lt_order_header_off ASSIGNING FIELD-SYMBOL(<fs_order_off>) WITH KEY order = lv_order.
        IF sy-subrc EQ 0.
          <fs_massive_assign_off>-order = <fs_order_off>-order.
          <fs_massive_assign_off>-end_date = <fs_order_off>-end_date.
          <fs_massive_assign_off>-order_desc = <fs_order_off>-order_short_desc.
          <fs_massive_assign_off>-equipment = <fs_order_off>-equipment.
          <fs_massive_assign_off>-equipment_desc = <fs_order_off>-equipment_desc.

          READ TABLE lt_job_employee ASSIGNING <fs_job_empl> WITH KEY pernr = <fs_ihpa>-parnr.
          IF sy-subrc EQ 0.
            <fs_massive_assign_off>-employee = <fs_job_empl>-full_name.
            <fs_massive_assign_off>-employee_id = <fs_job_empl>-pernr.
          ENDIF.
        ENDIF.
      ENDLOOP.

      SORT mt_massive_assign_oficial BY employee.

    ENDIF.

  ENDMETHOD.


  METHOD ajax_massive_order_close.

    DATA: lt_tmp_log TYPE zif_pm_data=>tt_log.

    LOOP AT mt_selected_orders ASSIGNING FIELD-SYMBOL(<ls_selected_orders>).

      ms_on_ajax-ajax_value = |{ CONV aufnr( <ls_selected_orders>-key ) ALPHA = IN }|.

      ajax_close_order( ).

      LOOP AT mt_log ASSIGNING FIELD-SYMBOL(<ls_log>).
        DATA: lv_order TYPE string.
          lv_order = |{ CONV aufnr( <ls_selected_orders>-key ) ALPHA = OUT }|.
        CONDENSE lv_order NO-GAPS.
        <ls_log>-message = |Orden: { lv_order } - { <ls_log>-message }|.
        APPEND <ls_log> TO lt_tmp_log.
      ENDLOOP.
    ENDLOOP.

    mt_log = CORRESPONDING #( lt_tmp_log ).

  ENDMETHOD.


  METHOD ajax_modify_order.

    DATA(lo_order) = NEW zcl_pm_order( ).

    DATA(ls_order_header) = convert_order_detail_in( ms_order_detail ).

* Se modifican los datos de la orden
    lo_order->save_order(  EXPORTING is_order = ls_order_header it_assigments = mt_order_assignments IMPORTING et_return   = DATA(lt_return) ).


* Convertimos los mensajes para el tipo de la aplicación
    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( it_return = lt_return ).

  ENDMETHOD.


  METHOD ajax_open_attachment.

    ms_document_url-url = NEW zcl_pm_attachments( )->get_gos_attachment_url( iv_gos_attachment_id = CONV so_entryid(  ms_on_ajax-ajax_value ) ).

  ENDMETHOD.


  METHOD ajax_print_order.

    DATA lv_rqident TYPE  tsp01-rqident.

    DATA(lv_order) = CONV aufnr( ms_on_ajax-ajax_value ) .

    CALL FUNCTION 'ZPM_IMPRIMIR_OT'
      EXPORTING
        aufnr   = lv_order
        uname   = ms_user_info-username
      IMPORTING
        rqident = lv_rqident.

    APPEND INITIAL LINE TO mt_log ASSIGNING FIELD-SYMBOL(<fs_log>).
    IF lv_rqident IS INITIAL.
      <fs_log>-ui5_type = zif_pm_data=>cs_ui5_msg_type-error.
      <fs_log>-type = zif_pm_data=>cs_msg_type-error.
      MESSAGE e012(zpm_apps) INTO <fs_log>-message. "Error al imprimir la orden.
    ELSE.
      <fs_log>-ui5_type = zif_pm_data=>cs_ui5_msg_type-success.
      <fs_log>-type = <fs_log>-type = zif_pm_data=>cs_msg_type-error.
      MESSAGE e011(zpm_apps) INTO <fs_log>-message. "Orden de impresión lanzada.
    ENDIF.

  ENDMETHOD.


  METHOD ajax_release_order.

*   Desde la aplicacion de ordenes la libera el oficial escaneando

    DATA: lr_order  TYPE RANGE OF aufnr,
          lt_return TYPE bapiret2_t.

    SPLIT ms_on_ajax-ajax_value AT '|' INTO DATA(lv_order) DATA(lv_pernr).

    CHECK lv_order IS NOT INITIAL.
    CHECK lv_pernr IS NOT INITIAL.

    DATA(lo_order) = NEW zcl_pm_order( ).
*    DATA(lo_user) = NEW zcl_pm_user( ).

*   Se obtienen los datos de usuario a partir del pernr
*    DATA(ls_user) = lo_user->get_user_info( lo_user->get_sap_user_from_pernr( |{ CONV pernr_d( lv_pernr ) ALPHA = IN }| ) ).

*   Si el usuario no es un encargado de produccion
*    IF ls_user-role NE lo_user->mc_roles-enc_produccion.
*      APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
*      <ls_return>-type = zif_pm_data=>cs_msg_type-error.
*      MESSAGE ID zif_pm_data=>cs_message_id TYPE zif_pm_data=>cs_msg_type-error NUMBER 009 INTO <ls_return>-message.
*
*    ELSE.

    lr_order = VALUE #( ( sign = 'I' option = 'EQ' low = |{ CONV aufnr( lv_order ) }| ) ).

    DATA(lt_order_header) = lo_order->get_order_header( lr_order ).
    READ TABLE lt_order_header INTO DATA(ls_order_header) INDEX 1.

    DATA(lv_enc_prod_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'ENC_PROD_ORDER_PARVW' ) ) .

    DATA(lt_partners) = VALUE zcl_pm_order=>tt_order_partner( ( employee_code = |{ CONV pernr_d( lv_pernr ) ALPHA = IN }| parvw = lv_enc_prod_parvw ) ).

    lt_return = lo_order->add_partner_to_order( is_order_header = ls_order_header
                                                      it_partners = lt_partners ).

*     Si hay un error se eliminan para no mostrar los pernr en el front
    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>) WHERE type = 'E'.
      DATA(lv_add_generic_error) = CONV abap_bool( abap_true ).
      DELETE lt_return.
      CONTINUE.
    ENDLOOP.

*     Si hay errores se añade un mensaje generico
    IF lv_add_generic_error IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING <ls_return>.
      MESSAGE ID zif_pm_data=>cs_message_id TYPE zif_pm_data=>cs_msg_type-error NUMBER 008  INTO <ls_return>-message.
      <ls_return>-type = zif_pm_data=>cs_msg_type-error.
      <ls_return>-id = zif_pm_data=>cs_message_id.
    ENDIF.

    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( lt_return ).

  ENDMETHOD.


  METHOD ajax_release_pending_order.

    DATA: lr_order TYPE RANGE OF aufnr.

    DATA(lo_order) = NEW zcl_pm_order( ).

    CHECK ms_on_ajax-ajax_value IS NOT INITIAL.

    lr_order = VALUE #( ( sign = 'I' option = 'EQ' low = |{ CONV aufnr( ms_on_ajax-ajax_value ) ALPHA = IN }| ) ).

    DATA(lt_order_header) = lo_order->get_order_header( it_order = lr_order iv_complete_data = abap_false ).

    READ TABLE lt_order_header INTO DATA(ls_order_header) INDEX 1.

    DATA(lt_return) = lo_order->release_pending_order( is_order = ls_order_header ).

    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( lt_return ).


  ENDMETHOD.


  METHOD convert_historic_data_out.

    LOOP AT it_order_header ASSIGNING FIELD-SYMBOL(<ls_order_header>).
      APPEND INITIAL LINE TO mt_historic_orders ASSIGNING FIELD-SYMBOL(<ls_historic_orders>).
      <ls_historic_orders> = CORRESPONDING #( <ls_order_header> ).

      READ TABLE it_order_attachments INTO DATA(ls_dummy) WITH KEY instid_a = <ls_order_header>-order.
      IF sy-subrc IS INITIAL.
        <ls_historic_orders>-has_attachments = abap_true.
      ENDIF.
    ENDLOOP.

    SORT mt_historic_orders BY creation_date DESCENDING.

*    mt_historic_material_movement = CORRESPONDING #( it_order_material_doc ).

*   Documentos de material
    LOOP AT it_order_material_doc ASSIGNING FIELD-SYMBOL(<ls_order_material_doc>).
      APPEND INITIAL LINE TO mt_historic_material_movement ASSIGNING FIELD-SYMBOL(<ls_material_movement>).
      <ls_material_movement> = CORRESPONDING #( <ls_order_material_doc> ).
      <ls_material_movement>-unit = zcl_pm_utilidades=>convert_cunit_out( CONV meins( <ls_material_movement>-unit ) ).

*     Hay dos campos que pueden tener la cantidad, si uno esta vacio se coje del otro
      IF  <ls_order_material_doc>-quantity IS INITIAL.
        <ls_material_movement>-quantity = <ls_order_material_doc>-quantity_2.
      ENDIF.
    ENDLOOP.

    mt_historic_notification = CORRESPONDING #( it_order_notification MAPPING notification_text = short_text ).

*    <ls_front_order_notification>-notification_text = |{ <ls_order_notification>-

  ENDMETHOD.


  METHOD convert_material_status_out.

    TYPES: BEGIN OF ts_material_group,
             material TYPE matnr,
             quantity TYPE menge_d,
           END OF ts_material_group.
    TYPES: tt_material_group TYPE STANDARD TABLE OF ts_material_group WITH DEFAULT KEY.

    DATA: lt_components_grouped   TYPE tt_material_group,
          lt_material_doc_grouped TYPE tt_material_group.

    DATA lt_order_mat_doc LIKE it_order_material_doc.

*   Si la orden no tiene componentes no tiene ningun estado de material
    CHECK it_order_components IS NOT INITIAL.

*       Por defecto tiene el estado material asignado
    ev_material_status = zcl_pm_order=>ms_front_material_status-assigned.
    ev_material_status_icon = ms_status_icon-material_assigned.

* Comparamos sólo los documentos materiales que contengan los materiales del listado de componentes
    LOOP AT it_order_material_doc ASSIGNING FIELD-SYMBOL(<fs_ord_mat_doc>).
      READ TABLE it_order_components ASSIGNING FIELD-SYMBOL(<ls_order_component>) WITH KEY material = <fs_ord_mat_doc>-material.
      IF sy-subrc EQ 0.
        APPEND <fs_ord_mat_doc> TO lt_order_mat_doc.
      ENDIF.
    ENDLOOP.

*   Recorremos los componentes.
    LOOP AT it_order_components ASSIGNING <ls_order_component>.

*     Si no tiene material codificado y tiene marcado el flag IS_AVAILABLE (KZEAR) esta en preparacion
      IF <ls_order_component>-material IS INITIAL AND <ls_order_component>-is_available IS NOT INITIAL.
        ev_material_status = zcl_pm_order=>ms_front_material_status-in_preparation.
        ev_material_status_icon = ms_status_icon-material_in_preparation.
        EXIT.

*     Si tiene algun material codificado y salida de almacen ya esta en preparacion
      ELSEIF <ls_order_component>-material IS NOT INITIAL AND lt_order_mat_doc IS NOT INITIAL.
        ev_material_status = zcl_pm_order=>ms_front_material_status-in_preparation.
        ev_material_status_icon = ms_status_icon-material_in_preparation.
        EXIT.
      ENDIF.
    ENDLOOP.

*   Para que el material este en estado disponible todos los componentes tienen que tener salida de almacen o el flag de IS_AVAILABLE

*   Materiales no codificados sin el flag
    DATA(lt_components_not_available) = VALUE zcl_pm_order=>tt_order_component( FOR <a> IN it_order_components
                                                                                        WHERE ( material EQ space AND
                                                                                                is_available EQ space ) ( <a> ) ).

*   Se agrupan los materials de los componentes de la orden por cantidad
    LOOP AT it_order_components ASSIGNING <ls_order_component> WHERE material NE space.
      READ TABLE lt_components_grouped ASSIGNING FIELD-SYMBOL(<ls_components_grouped>) WITH KEY material = <ls_order_component>-material.
      IF sy-subrc IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_components_grouped ASSIGNING <ls_components_grouped>.
        <ls_components_grouped>-material = <ls_order_component>-material.
      ENDIF.
      ADD <ls_order_component>-quantity TO <ls_components_grouped>-quantity.
    ENDLOOP.

*   Se agrupan los materiales de las salidas de almacen
    LOOP AT lt_order_mat_doc ASSIGNING FIELD-SYMBOL(<ls_order_material_doc>) WHERE movement_type EQ zcl_pm_order=>mc_material_doc_warehouse.
      READ TABLE lt_material_doc_grouped ASSIGNING FIELD-SYMBOL(<ls_material_grouped>) WITH KEY material = <ls_order_material_doc>-material.
      IF sy-subrc IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_material_doc_grouped ASSIGNING <ls_material_grouped>.
        <ls_material_grouped>-material = <ls_order_material_doc>-material.
      ENDIF.
      ADD  <ls_order_material_doc>-quantity TO <ls_material_grouped>-quantity .
    ENDLOOP.

    SORT lt_material_doc_grouped DESCENDING BY material.
    SORT lt_components_grouped DESCENDING BY material.

    IF lt_components_not_available IS INITIAL AND
       ( lt_material_doc_grouped = lt_components_grouped ).
      ev_material_status = zcl_pm_order=>ms_front_material_status-available.
      ev_material_status_icon = ms_status_icon-material_available.
    ENDIF.

  ENDMETHOD.


  METHOD convert_order_assignments_out.

    DATA: lr_job_id TYPE RANGE OF cr_objid,
          lr_pernr  TYPE RANGE OF pernr_d.

    lr_job_id = CORRESPONDING #( it_job_id ).

    DATA(lv_oficial_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'OFICIAL_ORDER_PARVW' ) ).

*   Se obtienen los empleados de un puesto de trabajo y se cruzan con los partners de la orden.
    lr_pernr = VALUE #( FOR <a> IN it_order_partner WHERE ( parvw = lv_oficial_parvw ) ( sign = 'I' option = 'EQ' low = <a>-employee_code ) ).

    DATA(lt_job_employee) =  zcl_pm_masterdata=>get_employee_from_job( it_job_id = lr_job_id
                                                                       iv_date = sy-datum ).

    LOOP AT lt_job_employee ASSIGNING FIELD-SYMBOL(<ls_job_employee>).
      APPEND INITIAL LINE TO mt_order_assignments ASSIGNING FIELD-SYMBOL(<ls_order_assigments>).
      <ls_order_assigments> = CORRESPONDING #( <ls_job_employee>  MAPPING employee_code = pernr
                                                                          employee_name = full_name ).

      IF lr_pernr[] IS NOT INITIAL AND <ls_job_employee>-pernr IN lr_pernr.
        <ls_order_assigments>-assigned = abap_true.
      ENDIF.
    ENDLOOP.

    SORT mt_order_assignments BY assigned DESCENDING employee_name.
    " Para algunos centros un mismo usuarios está dado de alta para más de un puesto de trabajo.
    DELETE ADJACENT DUPLICATES FROM mt_order_assignments.

  ENDMETHOD.


  METHOD convert_order_detail_in.

    DATA: lv_long_desc_len TYPE i.

    rs_order_header = CORRESPONDING #( is_order_detail MAPPING order_short_desc = order_desc ).

    DATA(lv_desc_len) = strlen( is_order_detail-order_desc ).

*   Si la descripcion es larga se hace split a los 40 caracteres
    IF lv_desc_len GT 40.
      rs_order_header-order_short_desc = is_order_detail-order_desc+0(40).

      lv_long_desc_len = lv_desc_len - 40.

      DATA(lv_desc_len_short) = strlen( rs_order_header-order_short_desc ).
      rs_order_header-order_long_desc = is_order_detail-order_desc+40(lv_long_desc_len).
      IF lv_desc_len_short LT 40.
        rs_order_header-order_long_desc = | { rs_order_header-order_long_desc }|.
      ENDIF.


    ELSE.
      rs_order_header-order_short_desc =  is_order_detail-order_desc.
    ENDIF.


  ENDMETHOD.


  METHOD convert_order_detail_out.

    DATA: lr_job_id TYPE RANGE OF cr_objid,
          lr_pernr  TYPE RANGE OF p_pernr,
          lr_center TYPE RANGE OF werks_d,
          lr_job    TYPE RANGE OF arbpl.

    ms_order_detail = CORRESPONDING #( is_order_header ).

*   Se concatenan las descripciones larga y corta
*    ms_order_detail-order_desc = |{ is_order_header-order_short_desc }{ is_order_header-order_long_desc }|.

*   El texto largo ya incluye al corto, por lo que si tiene datos mostramos ese, si no tiene nada mostramos el corto
    IF is_order_header-order_long_desc IS INITIAL.
      ms_order_detail-order_desc =  is_order_header-order_short_desc .
    ELSE.
      ms_order_detail-order_desc =  is_order_header-order_long_desc .
    ENDIF.


*   Notificaciones de la orden
    LOOP AT it_order_notification ASSIGNING FIELD-SYMBOL(<ls_order_notification>).
      APPEND INITIAL LINE TO mt_order_notification ASSIGNING FIELD-SYMBOL(<ls_front_order_notification>).
      <ls_front_order_notification> = CORRESPONDING #( <ls_order_notification> ).

      <ls_front_order_notification>-notification_text = |{ <ls_order_notification>-short_text }{ <ls_order_notification>-long_text }|.

      IF <ls_order_notification>-is_final EQ abap_true.
        <ls_front_order_notification>-is_final_yes = abap_true.
        <ls_front_order_notification>-is_final_no = abap_false.
      ELSE.
        <ls_front_order_notification>-is_final_yes = abap_false.
        <ls_front_order_notification>-is_final_no = abap_true.
      ENDIF.
    ENDLOOP.

    DATA(lv_enc_prod_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'ENC_PROD_ORDER_PARVW' ) ).
    DATA(lv_oficial_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'OFICIAL_ORDER_PARVW' ) ).

    IF is_order_header-center EQ zif_pm_data=>cs_center-villagarcia.
      " Para el centro de Villagarcia cargamos todos los oficiales independientemente del puesto de trabajo
      lr_center =  VALUE #( ( sign = 'I' option = 'EQ' low = is_order_header-center ) ).
      DATA(lt_jobs) = zcl_pm_masterdata=>get_jobs( it_job = lr_job it_center = lr_center ).
      IF lt_jobs IS NOT INITIAL.
        lr_job_id = VALUE #( FOR <ls_jobs> IN lt_jobs ( sign = 'I' option = 'EQ' low = <ls_jobs>-job_id ) ).
      ENDIF.
    ELSE.
*   Se obtienen los empleados de un puesto de trabajo y se cruzan con los partners de la orden.
      lr_job_id = VALUE #( ( sign = 'I' option = 'EQ' low = is_order_header-job_id ) ).
    ENDIF.
    convert_order_assignments_out( it_job_id = lr_job_id
                                   it_order_partner = it_order_partner ).

*   Documentos de material
    LOOP AT it_order_material_doc ASSIGNING FIELD-SYMBOL(<ls_order_material_doc>).
      APPEND INITIAL LINE TO mt_material_movement ASSIGNING FIELD-SYMBOL(<ls_material_movement>).
      <ls_material_movement> = CORRESPONDING #( <ls_order_material_doc> ).
      <ls_material_movement>-unit = zcl_pm_utilidades=>convert_cunit_out( CONV meins( <ls_material_movement>-unit ) ).
      IF <ls_order_material_doc>-movement_type EQ zcl_pm_order=>mc_material_doc_warehouse.
        <ls_material_movement>-delete_vis = abap_true.
      ENDIF.
*     Hay dos campos que pueden tener la cantidad, si uno esta vacio se coje del otro
      IF  <ls_order_material_doc>-quantity IS INITIAL.
        <ls_material_movement>-quantity = <ls_order_material_doc>-quantity_2.
      ENDIF.
    ENDLOOP.

    SORT mt_material_movement BY doc_date DESCENDING.

    LOOP AT it_order_components ASSIGNING FIELD-SYMBOL(<ls_order_components>).
      APPEND INITIAL LINE TO mt_order_component ASSIGNING FIELD-SYMBOL(<ls_order_component>).
      <ls_order_component> = CORRESPONDING #( <ls_order_components> ).
      <ls_order_component>-unit = zcl_pm_utilidades=>convert_cunit_out( CONV meins( <ls_order_components>-unit ) ).
    ENDLOOP.

*   Pueden estar varios estados activos a la vez por lo que se buscan por tiempo. De menos a mas

*   Orden abierta
    READ TABLE it_order_status ASSIGNING FIELD-SYMBOL(<ls_order_status>) WITH KEY status = zcl_pm_order=>ms_back_work_status-abie.
    IF sy-subrc IS INITIAL.
      ms_order_detail-work_status = zcl_pm_order=>ms_front_work_status-open.
    ELSE.


*   Orden pendiente
      READ TABLE it_order_status ASSIGNING <ls_order_status> WITH KEY status = zcl_pm_order=>ms_back_work_status-lib.
      IF sy-subrc IS INITIAL.
        ms_order_detail-work_status = zcl_pm_order=>ms_front_work_status-pending.

      ENDIF.

*   Si la orden tiene un oficial asignado el estado es asignada
      READ TABLE it_order_partner ASSIGNING FIELD-SYMBOL(<ls_order_partner>) WITH KEY parvw = lv_oficial_parvw.
      IF sy-subrc IS INITIAL.
        ms_order_detail-work_status = zcl_pm_order=>ms_front_work_status-assigned.

      ENDIF.

*   Notificacion parcial
      READ TABLE it_order_status ASSIGNING <ls_order_status> WITH KEY status = zcl_pm_order=>ms_back_work_status-notp.
      IF sy-subrc IS INITIAL.
        ms_order_detail-work_status = zcl_pm_order=>ms_front_work_status-started.

      ENDIF.

*   Notificacion final
      READ TABLE it_order_status ASSIGNING <ls_order_status> WITH KEY status = zcl_pm_order=>ms_back_work_status-noti.
      IF sy-subrc IS INITIAL.
        ms_order_detail-work_status = zcl_pm_order=>ms_front_work_status-completed.

      ENDIF.

*     Liberado por produccion
*       Si la orden tiene un encargado de produccion asignado el estado es liberada por producción
      READ TABLE it_order_partner ASSIGNING <ls_order_partner> WITH KEY parvw = lv_enc_prod_parvw.
      IF sy-subrc IS INITIAL.
        ms_order_detail-work_status = zcl_pm_order=>ms_front_work_status-released_by_prod.
      ENDIF.

*     Cierre técnico
      READ TABLE it_order_status ASSIGNING <ls_order_status> WITH KEY status = zcl_pm_order=>ms_back_work_status-ctec.
      IF sy-subrc IS INITIAL.
        ms_order_detail-work_status = zcl_pm_order=>ms_front_work_status-closed.
      ENDIF.

    ENDIF.

*     Estados de material
*     Por la complejidad se hace en otro metodo y para compartirlo con el detalle de la orden
    convert_material_status_out( EXPORTING is_order_header  = is_order_header
                                           it_order_material_doc = it_order_material_doc
                                           it_order_components = it_order_components
                                 IMPORTING ev_material_status = ms_order_detail-material_status ).

*   Determina las acciones que se pueden realizar sobre la orden
    determine_order_actions( EXPORTING iv_role = CONV zpm_e_role( ms_user_info-role )
                                       iv_work_status = CONV zpm_e_status( ms_order_detail-work_status )
                                       iv_center = CONV werks_d( ms_user_info-center )
                                       iv_job = CONV arbpl( ms_order_detail-job )
                             IMPORTING ev_editable = ms_order_detail-editable
                                      ev_job_editable = ms_order_detail-job_editable
                                      ev_can_create_material_doc = ms_order_detail-can_create_material_doc
                                      ev_can_create_notification = ms_order_detail-can_create_notification
                                      ev_can_close_order = ms_order_detail-can_close_order
                                      ev_can_assign_oficial = ms_order_detail-can_assign_oficial
                                      ev_can_release_order = ms_order_detail-can_release_order
                                      ev_can_release_from_prod = ms_order_detail-can_release_from_prod
                                      ev_mat_doc_oficial_selector = ms_order_detail-mat_doc_oficial_selector ).

*   Descripciones de los estados
    DATA(lt_front_status) = zcl_pm_masterdata=>get_master_front_status( ).
    READ TABLE lt_front_status ASSIGNING FIELD-SYMBOL(<ls_front_status>) WITH KEY status_type = zcl_pm_order=>mc_front_work_status_type
                                                                                  status = ms_order_detail-work_status.
    IF sy-subrc IS INITIAL.
      ms_order_detail-work_status_desc = <ls_front_status>-status_desc.
    ENDIF.

    READ TABLE lt_front_status ASSIGNING <ls_front_status> WITH KEY status_type = zcl_pm_order=>mc_front_material_status_type
                                                                                  status = ms_order_detail-material_status.
    IF sy-subrc IS INITIAL.
      ms_order_detail-material_status_desc = <ls_front_status>-status_desc.
    ENDIF.


**  Test
*    ms_order_detail-can_create_material_doc = abap_false.
*    ms_order_detail-can_create_notification = abap_false.

  ENDMETHOD.


  METHOD convert_order_list_out.

    DATA(lv_oficial_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'OFICIAL_ORDER_PARVW' ) ).
    DATA(lv_enc_prod_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'ENC_PROD_ORDER_PARVW' ) ).

*   Se pasan los datos a la estructura del front
    LOOP AT it_order_header ASSIGNING FIELD-SYMBOL(<ls_order_header>).

      APPEND INITIAL LINE TO mt_master_list ASSIGNING FIELD-SYMBOL(<ls_master_list>).
*      <ls_master_list> = CORRESPONDING #( <ls_order_header>  ).
      <ls_master_list> = CORRESPONDING #( <ls_order_header> MAPPING order_desc = order_short_desc user = creation_user ).

*     Anexos
      READ TABLE it_order_attachments ASSIGNING FIELD-SYMBOL(<ls_order_attachments>) WITH KEY instid_a = <ls_order_header>-order.
      IF sy-subrc IS INITIAL.
        <ls_master_list>-has_attachment = abap_true.
      ENDIF.

*     Estados de mano de obra

*     Pueden estar varios estados activos a la vez por lo que se buscan por tiempo. De menos a mas
*     Orden pendiente
      READ TABLE it_order_status ASSIGNING FIELD-SYMBOL(<ls_order_status>) WITH KEY objnr = <ls_order_header>-objnr
                                                                                    status = zcl_pm_order=>ms_back_work_status-abie.
      IF sy-subrc IS INITIAL.
        <ls_master_list>-work_status = zcl_pm_order=>ms_front_work_status-open.
        <ls_master_list>-work_status_icon = ms_status_icon-open.

      ELSE.

*     La orden puede tener el estado liberado en lugar de abierta
        READ TABLE it_order_status ASSIGNING <ls_order_status> WITH KEY objnr = <ls_order_header>-objnr
                                                                                      status = zcl_pm_order=>ms_back_work_status-lib.
        IF sy-subrc IS INITIAL.
          <ls_master_list>-work_status = zcl_pm_order=>ms_front_work_status-pending.
          <ls_master_list>-work_status_icon = ms_status_icon-pending.
        ENDIF.

*       Si la orden tiene un oficial asignado el estado es asignada
        READ TABLE it_order_partner ASSIGNING FIELD-SYMBOL(<ls_order_partner>) WITH KEY objnr = <ls_order_header>-objnr
                                                                                        parvw = lv_oficial_parvw.
        IF sy-subrc IS INITIAL.
          <ls_master_list>-work_status = zcl_pm_order=>ms_front_work_status-assigned.
          <ls_master_list>-work_status_icon = ms_status_icon-assigned.
        ENDIF.


*     Notificacion parcial
        READ TABLE it_order_status ASSIGNING <ls_order_status> WITH KEY objnr = <ls_order_header>-objnr
                                                                        status = zcl_pm_order=>ms_back_work_status-notp.
        IF sy-subrc IS INITIAL.
          <ls_master_list>-work_status = zcl_pm_order=>ms_front_work_status-started.
          <ls_master_list>-work_status_icon = ms_status_icon-started.
        ENDIF.

*     Notificacion final
        READ TABLE it_order_status ASSIGNING <ls_order_status> WITH KEY objnr = <ls_order_header>-objnr
                                                                        status = zcl_pm_order=>ms_back_work_status-noti.
        IF sy-subrc IS INITIAL.
          <ls_master_list>-work_status = zcl_pm_order=>ms_front_work_status-completed.
          <ls_master_list>-work_status_icon = ms_status_icon-completed.
        ENDIF.

*     Liberado por produccion
*       Si la orden tiene un encargado de produccion asignado el estado es liberada por producción
        READ TABLE it_order_partner ASSIGNING <ls_order_partner> WITH KEY objnr = <ls_order_header>-objnr
                                                                          parvw = lv_enc_prod_parvw.
        IF sy-subrc IS INITIAL.
          <ls_master_list>-work_status = zcl_pm_order=>ms_front_work_status-released_by_prod.
          <ls_master_list>-work_status_icon = ms_status_icon-released_by_prod.
        ENDIF.



*     Cierre técnico
        READ TABLE it_order_status ASSIGNING <ls_order_status> WITH KEY objnr = <ls_order_header>-objnr
                                                                        status = zcl_pm_order=>ms_back_work_status-ctec.
        IF sy-subrc IS INITIAL.
          <ls_master_list>-work_status = zcl_pm_order=>ms_front_work_status-closed.
          <ls_master_list>-work_status_icon = ms_status_icon-closed.
        ENDIF.

      ENDIF.

*     Estados de material

      DATA(lt_order_material_doc) = VALUE zcl_pm_order=>tt_order_material_doc( FOR <a> IN it_order_material_doc WHERE ( order = <ls_order_header>-order ) ( <a> ) ).
      DATA(lt_order_components) = VALUE zcl_pm_order=>tt_order_component( FOR <b> IN it_order_components WHERE ( order = <ls_order_header>-order ) ( <b> ) ).

*     Por la complejidad se hace en otro metodo y para compartirlo con el detalle de la orden
      convert_material_status_out( EXPORTING is_order_header  = <ls_order_header>
                                             it_order_material_doc = lt_order_material_doc
                                             it_order_components = lt_order_components
                                   IMPORTING ev_material_status = <ls_master_list>-material_status
                                             ev_material_status_icon =  <ls_master_list>-material_status_icon ).

    ENDLOOP.

    " En caso de que no se determine uno de nuestros estados de mano de obra, los eliminamos
    DELETE mt_master_list WHERE work_status IS INITIAL.

  ENDMETHOD.


  METHOD convert_status_for_search.

*   Para determinar los estados asignado y liberado por produccion hay que buscar mas datos ademas de los estados de la orden
    DATA: lv_add_released TYPE abap_bool.

    rt_status_conversion = it_status_conversion.
    LOOP AT rt_status_conversion ASSIGNING FIELD-SYMBOL(<ls_status_conversion>).
      CASE space.
*       Convierte estados del front al back
        WHEN <ls_status_conversion>-status_back.
          <ls_status_conversion>-status_back = SWITCH #( <ls_status_conversion>-status_front
                                                          WHEN zcl_pm_order=>ms_front_work_status-open THEN zcl_pm_order=>ms_back_work_status-abie
                                                          WHEN zcl_pm_order=>ms_front_work_status-pending THEN zcl_pm_order=>ms_back_work_status-lib
                                                          WHEN zcl_pm_order=>ms_front_work_status-assigned THEN zcl_pm_order=>ms_back_work_status-lib
                                                          WHEN zcl_pm_order=>ms_front_work_status-started THEN zcl_pm_order=>ms_back_work_status-notp
                                                          WHEN zcl_pm_order=>ms_front_work_status-completed THEN zcl_pm_order=>ms_back_work_status-noti
                                                          WHEN zcl_pm_order=>ms_front_work_status-released_by_prod THEN zcl_pm_order=>ms_back_work_status-noti
                                                          WHEN zcl_pm_order=>ms_front_work_status-closed THEN zcl_pm_order=>ms_back_work_status-ctec
*                                                          WHEN zcl_pm_order=>ms_front_material_status-assigned THEN zcl_pm_order=>ms_back_material_status-maco
*                                                          WHEN zcl_pm_order=>ms_front_material_status-in_preparation THEN zcl_pm_order=>ms_back_material_status-movm
*                                                          WHEN zcl_pm_order=>ms_front_material_status-available THEN zcl_pm_order=>ms_back_material_status-movm
  ).

**         Los estados de pendiente y asignado pueden tener ABIE o LIB en el estandar
*          IF <ls_status_conversion>-status_front EQ zcl_pm_order=>ms_front_work_status-pending OR
*             <ls_status_conversion>-status_front EQ zcl_pm_order=>ms_front_work_status-assigned.
*            lv_add_released = abap_true.
*          ENDIF.

*      Convierte estados del back al front
        WHEN <ls_status_conversion>-status_front.
          <ls_status_conversion>-status_front = SWITCH #( <ls_status_conversion>-status_back
                                                          WHEN  zcl_pm_order=>ms_back_work_status-abie THEN zcl_pm_order=>ms_front_work_status-open
                                                          WHEN  zcl_pm_order=>ms_back_work_status-lib THEN zcl_pm_order=>ms_front_work_status-pending
                                                          WHEN  zcl_pm_order=>ms_back_work_status-notp THEN zcl_pm_order=>ms_front_work_status-started
                                                          WHEN  zcl_pm_order=>ms_back_work_status-noti THEN zcl_pm_order=>ms_front_work_status-completed
                                                          WHEN  zcl_pm_order=>ms_back_work_status-ctec THEN zcl_pm_order=>ms_front_work_status-closed
*                                                          WHEN  zcl_pm_order=>ms_back_material_status-maco THEN zcl_pm_order=>ms_front_material_status-assigned
*                                                          WHEN zcl_pm_order=>ms_back_material_status-movm THEN zcl_pm_order=>ms_front_material_status-in_preparation
                                                            ).

      ENDCASE.
    ENDLOOP.

*    IF lv_add_released IS NOT INITIAL.
*      APPEND INITIAL LINE TO rt_status_conversion ASSIGNING <ls_status_conversion>.
*      <ls_status_conversion>-status_back = zcl_pm_order=>ms_back_work_status-lib.
*      <ls_status_conversion>-status_front = zcl_pm_order=>ms_front_work_status-pending.
*    ENDIF.

  ENDMETHOD.


  METHOD determine_master_list_actions.


*   Crear orden

    ev_can_create_order = COND #( WHEN
                               ( iv_role EQ zcl_pm_user=>mc_roles-admin OR
                                 iv_role EQ zcl_pm_user=>mc_roles-jefe_turno OR
                                 iv_role EQ zcl_pm_user=>mc_roles-oficial ) OR
                               ( iv_role EQ zcl_pm_user=>mc_roles-enc_produccion AND iv_center = '1002' )
                               THEN abap_true
                               ELSE abap_false ).



*   Asignacion masiva

    ev_can_massive_assign = COND #( WHEN
                               ( iv_role EQ zcl_pm_user=>mc_roles-admin OR
                                 iv_role EQ zcl_pm_user=>mc_roles-jefe_turno OR
                                 iv_role EQ zcl_pm_user=>mc_roles-oficial ) OR ( iv_role EQ zcl_pm_user=>mc_roles-enc_produccion AND iv_center = '1002' )
                               THEN abap_true
                               ELSE abap_false ).


*   Cierre masivo

    ev_can_close_order  = COND #( WHEN ( ( iv_role EQ zcl_pm_user=>mc_roles-admin OR iv_role EQ zcl_pm_user=>mc_roles-jefe_turno ) AND iv_center NE '1004' )
                                   THEN abap_true
                                   ELSE abap_false ).



  ENDMETHOD.


  METHOD determine_order_actions.

    DATA: lr_center     TYPE RANGE OF werks_d,
          lr_active_job TYPE RANGE OF arbpl.

*   Puestos de trabajo "no contratas"
*    zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = 'ACTIVE_JOB_%'
*                                                    CHANGING ct_ranges = lr_active_job ).
    lr_active_job = zcl_pm_masterdata=>get_active_job_by_center( iv_werks = ms_user_info-center ).

*   Solo se muesta el selector de oficial en la creacion de documentos de material, cuando el usuario es almacenero y
*   la orden no es de subcontrata
    ev_mat_doc_oficial_selector = COND #( WHEN ( iv_role EQ zcl_pm_user=>mc_roles-almacenero OR
                                                 iv_role EQ zcl_pm_user=>mc_roles-admin ) AND
                                                 iv_job IN lr_active_job
                                          THEN abap_true
                                          ELSE abap_false ).


*   EDITABLE

    ev_editable = COND #( WHEN ( iv_work_status EQ zcl_pm_order=>ms_front_work_status-open OR
                                 iv_work_status EQ zcl_pm_order=>ms_front_work_status-pending OR
                                 iv_work_status EQ zcl_pm_order=>ms_front_work_status-assigned OR
                                 iv_work_status EQ zcl_pm_order=>ms_front_work_status-started ) AND
                               ( iv_role EQ zcl_pm_user=>mc_roles-admin OR
                                 iv_role EQ zcl_pm_user=>mc_roles-jefe_turno OR
                                 iv_role EQ zcl_pm_user=>mc_roles-oficial )
                               THEN abap_true
                               ELSE abap_false ).

*   LIBERAR ORDEN

    ev_can_release_order  = COND #( WHEN ( iv_work_status EQ zcl_pm_order=>ms_front_work_status-open ) AND
                                   ( iv_role EQ zcl_pm_user=>mc_roles-admin OR
                                     iv_role EQ zcl_pm_user=>mc_roles-jefe_turno OR
                                     iv_role EQ zcl_pm_user=>mc_roles-oficial OR
                                    ( iv_role EQ zcl_pm_user=>mc_roles-enc_produccion AND iv_center = '1002' ) "Solo los encargados de lugo pueden liberar
                                     )
                                   THEN abap_true
                                   ELSE abap_false ).


*   EDITAR PUESTO DE TRABAJO

    ev_job_editable = COND #( WHEN iv_work_status EQ zcl_pm_order=>ms_front_work_status-open OR
                                  iv_work_status EQ zcl_pm_order=>ms_front_work_status-pending OR
                                  iv_work_status EQ zcl_pm_order=>ms_front_work_status-assigned
                                  THEN abap_true
                                  ELSE abap_false ).

* ASIGNAR

    ev_can_assign_oficial  = COND #( WHEN ( iv_work_status EQ zcl_pm_order=>ms_front_work_status-open OR
                                            iv_work_status EQ zcl_pm_order=>ms_front_work_status-pending OR
                                            iv_work_status EQ zcl_pm_order=>ms_front_work_status-assigned OR
                                            iv_work_status EQ zcl_pm_order=>ms_front_work_status-started ) AND
                                          ( ( iv_role EQ zcl_pm_user=>mc_roles-admin OR
                                            iv_role EQ zcl_pm_user=>mc_roles-jefe_turno OR
                                            iv_role EQ zcl_pm_user=>mc_roles-oficial ) OR ( iv_role EQ zcl_pm_user=>mc_roles-enc_produccion AND iv_center = '1002' ) )
                                      THEN abap_true
                                      ELSE abap_false ).



*   CREAR DOCUMENTO DE MATERIAL

    ev_can_create_material_doc = COND #( WHEN ( iv_work_status EQ zcl_pm_order=>ms_front_work_status-pending OR
                                                iv_work_status EQ zcl_pm_order=>ms_front_work_status-assigned OR
                                                iv_work_status EQ zcl_pm_order=>ms_front_work_status-started OR
                                                iv_work_status EQ zcl_pm_order=>ms_front_work_status-completed OR
                                                iv_work_status EQ zcl_pm_order=>ms_front_work_status-released_by_prod ) AND
                                                ( ( iv_role EQ zcl_pm_user=>mc_roles-admin OR
                                                iv_role EQ zcl_pm_user=>mc_roles-jefe_turno OR
                                                iv_role EQ zcl_pm_user=>mc_roles-oficial OR
                                                iv_role EQ zcl_pm_user=>mc_roles-almacenero ) OR ( iv_role EQ zcl_pm_user=>mc_roles-enc_produccion AND iv_center = '1002' ) )
                                   THEN abap_true
                                   ELSE abap_false ).

    " Deshabilitamos la posibildad de eliminar documentos
    IF ev_can_create_material_doc EQ abap_false.
      DATA ls_material_mov TYPE ts_material_movement.
      ls_material_mov-delete_vis = abap_false.
      MODIFY mt_material_movement FROM ls_material_mov TRANSPORTING delete_vis WHERE material_doc IS NOT INITIAL .
    ENDIF.


*   NOTIFICAR

    ev_can_create_notification  = COND #( WHEN ( iv_work_status EQ zcl_pm_order=>ms_front_work_status-pending OR
                                                 iv_work_status EQ zcl_pm_order=>ms_front_work_status-assigned OR
                                                 iv_work_status EQ zcl_pm_order=>ms_front_work_status-started ) AND
                                          ( ( iv_role EQ zcl_pm_user=>mc_roles-admin OR
                                            iv_role EQ zcl_pm_user=>mc_roles-jefe_turno OR
                                            iv_role EQ zcl_pm_user=>mc_roles-oficial ) OR ( iv_role EQ zcl_pm_user=>mc_roles-enc_produccion AND iv_center = '1002' ) )
                                      THEN abap_true
                                      ELSE abap_false ).


*   CERRAR ORDEN

    ev_can_close_order  = COND #( WHEN ( ( iv_work_status EQ zcl_pm_order=>ms_front_work_status-pending OR
                                     iv_work_status EQ zcl_pm_order=>ms_front_work_status-assigned OR
                                     iv_work_status EQ zcl_pm_order=>ms_front_work_status-started OR
                                     iv_work_status EQ zcl_pm_order=>ms_front_work_status-completed OR
                                     iv_work_status EQ zcl_pm_order=>ms_front_work_status-released_by_prod  ) AND
                                   ( iv_role EQ zcl_pm_user=>mc_roles-admin OR
                                     iv_role EQ zcl_pm_user=>mc_roles-jefe_turno ) AND  iv_center NE '1004' )
                                   THEN abap_true
                                   ELSE abap_false ).



*   LIBERAR PARA PRODUCCION

    ev_can_release_from_prod  = COND #( WHEN ( iv_work_status EQ zcl_pm_order=>ms_front_work_status-completed ) AND
                                   ( ( ( iv_role EQ zcl_pm_user=>mc_roles-jefe_turno OR iv_role EQ zcl_pm_user=>mc_roles-oficial ) AND iv_center = '1001' )
                                      OR ( iv_role EQ zcl_pm_user=>mc_roles-enc_produccion OR iv_role EQ zcl_pm_user=>mc_roles-admin ) )
                                   THEN abap_true
                                   ELSE abap_false ).

  ENDMETHOD.
ENDCLASS.
