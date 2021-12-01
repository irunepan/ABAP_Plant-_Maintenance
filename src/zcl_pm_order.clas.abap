CLASS zcl_pm_order DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_order_header,
        order             TYPE aufnr,
        order_class       TYPE aufart,
        order_class_desc  TYPE auarttext,
        order_short_desc  TYPE auftext,
        order_long_desc   TYPE string,
        objnr             TYPE objnr,
        center            TYPE werks_d,
        placement         TYPE iloan,
        placement_desc    TYPE string,
        equipment         TYPE equnr,
        equipment_desc    TYPE string,
        tec_location      TYPE tplnr,
        tec_location_desc TYPE string,
        priority          TYPE priok,
        priority_desc     TYPE string,
        notification_id   TYPE qmnum,
        iloan             TYPE iloan, "Id emplazamientos (Clave de tabla ILOA)
        creation_user     TYPE uname,

*       Datos puesto de trabajo
        job_id            TYPE cr_objid,
        job               TYPE arbpl,
        job_desc          TYPE string,

*     Fechas
        creation_date     TYPE datum,
        start_date        TYPE datum,
        end_date          TYPE datum,
        ref_date          TYPE datum,


        operation_key     TYPE co_aufpl,


      END OF ts_order_header .
    TYPES:
      BEGIN OF ts_order_operation,
        operation_key        TYPE co_aufpl,
        operation_counter    TYPE co_aplzl,
        operation_number     TYPE vornr,
        notification_key     TYPE co_rueck,
        notification_counter TYPE co_rmzhl,
        operation_desc       TYPE ltxa1,

      END OF ts_order_operation .
    TYPES:
      tt_order_operation TYPE STANDARD TABLE OF ts_order_operation WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_order_notification,
        notification_key     TYPE co_rueck,
        notification_counter TYPE co_rmzhl,
        short_text           TYPE ltxa1,
        long_text            TYPE string,
        employee_number      TYPE pernr_d,
        employee_name        TYPE string,
        creation_user        TYPE ernam,
        creation_date        TYPE datum,
        creation_hour        TYPE erzet,
        worked_time          TYPE ismnw,
        worked_time_unit     TYPE ismne,
        order                TYPE aufnr,
        operation_key        TYPE co_aufpl,
        operation_counter    TYPE co_aplzl,
        operation_number     TYPE vornr,
        work_start_date      TYPE ru_isdd,
        work_start_time      TYPE ru_isdz,
        work_end_date        TYPE ru_iedd,
        work_end_time        TYPE ru_iedz,
        is_final             TYPE aueru_vs,
      END OF ts_order_notification .
    TYPES:
      tt_order_notification TYPE STANDARD TABLE OF ts_order_notification WITH DEFAULT KEY .
    TYPES:
      tt_order_header TYPE STANDARD TABLE OF ts_order_header WITH DEFAULT KEY .
    TYPES:
      tt_order_id TYPE STANDARD TABLE OF aufnr WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_order_partner,
        objnr         TYPE j_objnr,
        parvw         TYPE parvw,
        counter       TYPE i_count,
        employee_code TYPE i_parnr,
        creation_date TYPE erdat,
        creation_time TYPE erzeit,
        creation_user TYPE ernam,
      END OF ts_order_partner .
    TYPES:
      tt_order_partner TYPE STANDARD TABLE OF ts_order_partner WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_order_material_doc,
        material_doc          TYPE mblnr,
        material_doc_year	    TYPE mjahr,
        material_doc_position TYPE mblpo,
        movement_type         TYPE bwart,
        material              TYPE matnr,
        material_desc         TYPE string,
        quantity              TYPE erfmg,
        quantity_2            TYPE erfmg,
        unit                  TYPE erfme,
        order                 TYPE aufnr,
        purchase_order        TYPE ebeln,
        center                TYPE werks_d,
        location              TYPE lgort_d,
        storage               TYPE lgpbe,
        doc_date              TYPE bldat,
        amount                TYPE dmbtr,
        creation_user         TYPE usnam,
        employee              TYPE wempf,
      END OF ts_order_material_doc .
    TYPES:
      tt_order_material_doc TYPE STANDARD TABLE OF ts_order_material_doc WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_order_component,
        order          TYPE aufnr,
        booking_number TYPE rsnum,
        booking_pos    TYPE rspos,
        booking_key    TYPE rsart,
        material       TYPE matnr,
        material_desc  TYPE string,
        quantity       TYPE bdmng,
        unit           TYPE meins,
        is_available   TYPE kzear,
        purchase_order TYPE ebeln,
        center         TYPE werks_d,
        location       TYPE lgort_d,
        storage        TYPE lgpbe,
        supplier_name  TYPE name1,
      END OF ts_order_component .
    TYPES:
      tt_order_component TYPE STANDARD TABLE OF ts_order_component WITH DEFAULT KEY .

    CONSTANTS mc_order_type TYPE auftyp VALUE '30' ##NO_TEXT.
    CONSTANTS mc_front_work_status_type TYPE zpm_e_status_type VALUE 'ORD' ##NO_TEXT.
    CONSTANTS mc_front_material_status_type TYPE zpm_e_status_type VALUE 'MAT' ##NO_TEXT.
    CONSTANTS mc_material_doc_warehouse_c TYPE bwart VALUE '262' ##NO_TEXT.
    CONSTANTS mc_material_doc_warehouse TYPE bwart VALUE '261' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF mc_bapi_ord_method,
        create             TYPE fieldname VALUE 'CREATE',   "#EC NOTEXT
        createtonotif      TYPE fieldname VALUE 'CREATETONOTIF', "#EC NOTEXT
        change             TYPE fieldname VALUE 'CHANGE',   "#EC NOTEXT
        delete             TYPE fieldname VALUE 'DELETE',   "#EC NOTEXT
        delete_single      TYPE fieldname VALUE 'DELETE_SINGLE', "#EC NOTEXT  "Prototype OPROL
        release            TYPE fieldname VALUE 'RELEASE',  "#EC NOTEXT
        reassign           TYPE fieldname VALUE 'REASSIGN', "#EC NOTEXT    "2413877
        technical_complete TYPE fieldname VALUE 'TECHNICALCOMPLETE', "#EC NOTEXT
        atpcheck           TYPE fieldname VALUE 'ATPCHECK', "#EC NOTEXT
        calculate          TYPE fieldname VALUE 'CALCULATE', "#EC NOTEXT
        add_task           TYPE fieldname VALUE 'ADD',      "#EC NOTEXT
        save               TYPE fieldname VALUE 'SAVE',     "#EC NOTEXT
        dialog             TYPE fieldname VALUE 'DIALOG',   "#EC NOTEXT
        trace              TYPE fieldname VALUE 'TRACE',    "#EC NOTEXT
      END OF mc_bapi_ord_method .
    CONSTANTS:
      BEGIN OF mc_bapi_ord_obj_type,
        header           TYPE fieldname VALUE 'HEADER',     "#EC NOTEXT
        partner          TYPE fieldname VALUE 'PARTNER',    "#EC NOTEXT
        userstatus       TYPE fieldname VALUE 'USERSTATUS', "#EC NOTEXT
        operation        TYPE fieldname VALUE 'OPERATION',  "#EC NOTEXT
        relation         TYPE fieldname VALUE 'RELATION',   "#EC NOTEXT
        component        TYPE fieldname VALUE 'COMPONENT',  "#EC NOTEXT
        text             TYPE fieldname VALUE 'TEXT',       "#EC NOTEXT
        srule            TYPE fieldname VALUE 'SRULE',      "#EC NOTEXT
        objectlist       TYPE fieldname VALUE 'OBJECTLIST', "#EC NOTEXT
        olistrelation    TYPE fieldname VALUE 'OLISTRELATION', "#EC NOTEXT
        tasklist         TYPE fieldname VALUE 'TASKLIST',   "#EC NOTEXT
        prt              TYPE fieldname VALUE 'PRT',        "#EC NOTEXT
        srvoutline       TYPE fieldname VALUE 'SERVICEOUTLINE', "#EC NOTEXT
        srvline          TYPE fieldname VALUE 'SERVICELINE', "#EC NOTEXT
        srvlimit         TYPE fieldname VALUE 'SERVICELIMIT', "#EC NOTEXT
        srvcontractlimit TYPE fieldname VALUE 'SERVICECONTRACTLIMIT', "#EC NOTEXT
        permit           TYPE fieldname VALUE 'PERMIT',     "#EC NOTEXT
        empty            TYPE fieldname VALUE '',           "#EC NOTEXT
        cost             TYPE fieldname VALUE 'ESTIMATEDCOST', "#EC NOTEXT "EAMCC PM110
      END OF mc_bapi_ord_obj_type .
    CONSTANTS:
      BEGIN OF ms_back_work_status,
        abie TYPE j_status VALUE 'I0001', "ABIE
        lib  TYPE j_status VALUE 'I0002', "LIB Liberada
        notp TYPE j_status VALUE 'I0010', "NOTP Notificado parcialmente
        noti TYPE j_status VALUE 'I0009', "NOTI Notificado
        ctec TYPE j_status VALUE 'I0045', "CTEC Cierre tecnico
      END OF ms_back_work_status .
    CONSTANTS:
*  constants:
*    BEGIN OF ms_back_material_status,
*        maco TYPE j_status VALUE 'I0340', "MACO
*        movm TYPE j_status VALUE 'I0321', "MOVM
*      END OF ms_back_material_status .
      BEGIN OF ms_front_work_status,
        open             TYPE zpm_e_status VALUE 'ST00',
        pending          TYPE zpm_e_status VALUE 'ST01',
        assigned         TYPE zpm_e_status VALUE 'ST02',
        started          TYPE zpm_e_status VALUE 'ST03',
        completed        TYPE zpm_e_status VALUE 'ST04',
        released_by_prod TYPE zpm_e_status VALUE 'ST05',
        closed           TYPE zpm_e_status VALUE 'ST06',
      END OF ms_front_work_status .
    CONSTANTS:
      BEGIN OF ms_front_material_status,
        assigned       TYPE zpm_e_status VALUE 'ST11',
        in_preparation TYPE zpm_e_status VALUE 'ST12',
        available      TYPE zpm_e_status VALUE 'ST13',
      END OF ms_front_material_status .
    CONSTANTS:
      BEGIN OF ms_front_order_type ,
        no_preventive TYPE string VALUE 'NPREV',
        preventive    TYPE string VALUE 'PREV',
      END OF ms_front_order_type .
    CONSTANTS:
      BEGIN OF ms_back_order_type,
        preventive TYPE aufart VALUE 'ZPRE',
      END OF ms_back_order_type .
    CONSTANTS mc_motivo_notif_others TYPE co_agrnd VALUE '07' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !iv_langu TYPE langu DEFAULT sy-langu .
    METHODS search_orders
      IMPORTING
        !it_order           TYPE STANDARD TABLE OPTIONAL
        !it_order_class     TYPE STANDARD TABLE OPTIONAL
        !it_order_status    TYPE STANDARD TABLE OPTIONAL
        !it_placement       TYPE STANDARD TABLE OPTIONAL
        !it_tec_location    TYPE STANDARD TABLE OPTIONAL
        !it_equipment       TYPE STANDARD TABLE OPTIONAL
        !it_job             TYPE STANDARD TABLE OPTIONAL
        !it_date_creation   TYPE STANDARD TABLE OPTIONAL
        !it_center          TYPE STANDARD TABLE OPTIONAL
        !it_employee_number TYPE STANDARD TABLE OPTIONAL
      RETURNING
        VALUE(rt_order_id)  TYPE tt_order_id .
    METHODS get_order_header
      IMPORTING
        !it_order              TYPE STANDARD TABLE
        !iv_complete_data      TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_order_header) TYPE tt_order_header .
    METHODS get_order_operations
      IMPORTING
        !it_order                 TYPE STANDARD TABLE OPTIONAL
        !it_operation_key         TYPE STANDARD TABLE OPTIONAL
        !iv_complete_data         TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_order_operation) TYPE tt_order_operation .
    METHODS get_order_notifications
      IMPORTING
        !it_order                    TYPE STANDARD TABLE OPTIONAL
        !it_notification_key         TYPE STANDARD TABLE OPTIONAL
        !it_notification_counter     TYPE STANDARD TABLE OPTIONAL
        !iv_complete_data            TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_order_notification) TYPE tt_order_notification .
    METHODS get_order_components
      IMPORTING
        !it_order                  TYPE STANDARD TABLE
        !iv_complete_data          TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_order_components) TYPE tt_order_component .
    METHODS get_order_material_docs
      IMPORTING
        !it_order                    TYPE STANDARD TABLE
        !it_movement_type            TYPE STANDARD TABLE OPTIONAL
        !iv_complete_data            TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_order_material_doc) TYPE tt_order_material_doc .
    METHODS get_order_partners
      IMPORTING
        !it_objnr               TYPE STANDARD TABLE
      RETURNING
        VALUE(rt_order_partner) TYPE tt_order_partner .
    METHODS save_order
      IMPORTING
        !iv_notif_id   TYPE qmnum OPTIONAL
        !is_order      TYPE zcl_pm_order=>ts_order_header
        !it_assigments TYPE zcl_pm_orders_app_cntrl=>tt_order_assigments
      EXPORTING
        !et_return     TYPE bapiret2_t
        !ev_order_id   TYPE aufnr .
    METHODS tecnical_close_order
      IMPORTING
        !is_order        TYPE zcl_pm_order=>ts_order_header
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t .
    METHODS add_partner_to_order
      IMPORTING
        !is_order_header TYPE ts_order_header
        !it_partners     TYPE tt_order_partner
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t .
    METHODS read_order_long_descr
      IMPORTING
        !iv_order_id        TYPE aufnr
      CHANGING
        !cv_notif_long_desc TYPE string .
    METHODS create_order_notification
      IMPORTING
        !is_order_notif TYPE ts_order_notification
      EXPORTING
        !et_return      TYPE bapiret2_t .
    METHODS save_order_long_descr
      IMPORTING
        !iv_order_id TYPE aufnr
        !iv_descr    TYPE string .
    METHODS release_pending_order
      IMPORTING
        !is_order        TYPE zcl_pm_order=>ts_order_header
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t .
    METHODS massive_assign
      IMPORTING
        !it_orders   TYPE zcl_pm_orders_app_cntrl=>tt_massive_assign_order
        !it_oficials TYPE zcl_pm_orders_app_cntrl=>tt_massive_oficial
      EXPORTING
        !et_return   TYPE bapiret2_t .
protected section.

  data MV_LANGU type LANGU .
  data MT_ORDER_HEADER type TT_ORDER_HEADER .
  data MT_ORDER_OPERATION type TT_ORDER_OPERATION .
  data MT_ORDER_NOTIFICATION type TT_ORDER_NOTIFICATION .
  data MT_ORDER_PARTNER type TT_ORDER_PARTNER .
  data MT_ORDER_MATERIAL_DOC type TT_ORDER_MATERIAL_DOC .
  data MT_ORDER_COMPONENT type TT_ORDER_COMPONENT .

  methods COMPLETE_HEADER_DATA .
  methods COMPLETE_NOTIFICATION_DATA .
  methods COMPLETE_MATERIAL_DOC_DATA .
  methods COMPLETE_COMPONENT_DATA .
  methods DELETE_IRRELEVANT_STATUS
    changing
      !CT_STATUS type ZCL_PM_MASTERDATA=>TT_OBJECT_STATUS .
private section.
ENDCLASS.



CLASS ZCL_PM_ORDER IMPLEMENTATION.


  METHOD add_partner_to_order.


    DATA: ls_methods         TYPE bapi_alm_order_method,
          lt_methods         TYPE STANDARD TABLE OF  bapi_alm_order_method,
          ls_header          TYPE bapi_alm_order_headers_i,
          lt_header          TYPE STANDARD TABLE OF  bapi_alm_order_headers_i,
          lt_header_up       TYPE STANDARD TABLE OF  bapi_alm_order_headers_up,
          ls_header_up       TYPE bapi_alm_order_headers_up,
          lt_bapi_partner    TYPE  STANDARD TABLE OF bapi_alm_order_partn_mul,
          lt_bapi_partner_up TYPE STANDARD TABLE OF  bapi_alm_order_partn_mul_up,
          lt_return          TYPE bapiret2_t,
          lt_numbers         TYPE STANDARD TABLE OF bapi_alm_numbers.


**********************************************************************
* DATOS GENERALES DE LA ORDEN
**********************************************************************

    ls_methods-refnumber    = '000001'.
    ls_methods-objecttype   = mc_bapi_ord_obj_type-header.
    ls_methods-method       = mc_bapi_ord_method-change.
    ls_methods-objectkey    = is_order_header-order.
    APPEND ls_methods TO lt_methods.
    CLEAR ls_methods.
    ls_methods-objectkey    = is_order_header-order.
    ls_methods-refnumber    = '000001'.
    ls_methods-method       = mc_bapi_ord_method-save.
    APPEND ls_methods TO lt_methods.


    lt_header = VALUE #( ( orderid = is_order_header-order ) ).
    lt_header_up = VALUE #( ( orderid = is_order_header-order ) ).

**********************************************************************
* ASIGNACIONES
**********************************************************************

    LOOP AT it_partners ASSIGNING FIELD-SYMBOL(<ls_partner>).

      APPEND INITIAL LINE TO lt_bapi_partner ASSIGNING FIELD-SYMBOL(<ls_bapi_partner>).
      <ls_bapi_partner>-orderid =  is_order_header-order.
      <ls_bapi_partner>-partn_role = <ls_partner>-parvw.
      <ls_bapi_partner>-partner = <ls_partner>-employee_code.
      APPEND INITIAL LINE TO lt_bapi_partner_up ASSIGNING FIELD-SYMBOL(<ls_bapi_partner_up>).
      <ls_bapi_partner_up>-orderid = is_order_header-order.
      <ls_bapi_partner_up>-partn_role = abap_true.
      <ls_bapi_partner_up>-partner = abap_true.

    ENDLOOP.

    IF lt_bapi_partner[] IS NOT INITIAL.
      ls_methods-refnumber    = '000001'.
      ls_methods-objecttype   = mc_bapi_ord_obj_type-partner.
      ls_methods-method       = mc_bapi_ord_method-change.
      ls_methods-objectkey    = is_order_header-order.
      APPEND ls_methods TO lt_methods.
    ENDIF.

**********************************************************************
* ACTUALIZAMOS LA ORDEN
**********************************************************************

    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
      TABLES
        it_methods    = lt_methods
        it_header     = lt_header
        it_header_up  = lt_header_up
        it_partner    = lt_bapi_partner
        it_partner_up = lt_bapi_partner_up
        return        = rt_return
        et_numbers    = lt_numbers.


    READ TABLE rt_return ASSIGNING FIELD-SYMBOL(<fs_return>) WITH KEY type = zif_pm_data=>cs_msg_type-error.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD complete_component_data.


    DATA: lr_matnr TYPE RANGE OF matnr,
          lr_werks TYPE RANGE OF werks_d,
          lr_lgort TYPE RANGE OF lgort_d,
          lr_arsnr TYPE RANGE OF arsnr.

*    Materiales
    lr_matnr = VALUE #( FOR <a> IN mt_order_component ( sign = 'I' option = 'EQ' low = <a>-material ) ).
    lr_werks = VALUE #( FOR <a> IN mt_order_component ( sign = 'I' option = 'EQ' low = <a>-center ) ).
    lr_lgort = VALUE #( FOR <a> IN mt_order_component ( sign = 'I' option = 'EQ' low = <a>-location ) ).
    lr_arsnr = VALUE #( FOR <a> IN mt_order_component ( sign = 'I' option = 'EQ' low = <a>-booking_number ) ).

    IF lr_matnr IS NOT INITIAL.

*     Se recuperan los maestros
      SELECT matnr AS material, maktx AS material_desc
        FROM makt
        INTO TABLE @DATA(lt_makt)
        WHERE matnr IN @lr_matnr AND
              spras EQ @mv_langu.

      IF lr_werks IS NOT INITIAL AND
         lr_lgort IS NOT INITIAL.

        SELECT matnr AS material, werks AS center, lgort AS location, lgpbe AS storage
          FROM mard
          INTO TABLE @DATA(lt_mard)
          WHERE matnr IN @lr_matnr AND
                werks IN @lr_werks." AND
*                lgort IN @lr_lgort.

      ENDIF.
      " Recuperamos los pedidos
      IF lr_arsnr IS NOT INITIAL.
        SELECT a~arsnr, a~arsps, a~ebeln, a~ebelp, c~name1
          FROM eban AS a INNER JOIN ekko AS b ON a~ebeln = b~ebeln
                         INNER JOIN lfa1 AS c ON b~lifnr = c~lifnr
          INTO TABLE @DATA(lt_eban)
          WHERE arsnr IN @lr_arsnr.
        SORT lt_eban BY arsnr arsps.
      ENDIF.

    ENDIF.

*   Se pasan a la tabla de notificaciones
    LOOP AT mt_order_component ASSIGNING FIELD-SYMBOL(<ls_order_component>).

      READ TABLE lt_makt ASSIGNING FIELD-SYMBOL(<ls_makt>) WITH KEY material = <ls_order_component>-material.
      IF sy-subrc IS INITIAL.
        <ls_order_component>-material_desc = <ls_makt>-material_desc.
      ENDIF.


      READ TABLE lt_mard ASSIGNING FIELD-SYMBOL(<ls_mard>) WITH KEY material = <ls_order_component>-material
                                                                    center = <ls_order_component>-center.
      "location = <ls_order_component>-location.
      IF sy-subrc IS INITIAL.
        <ls_order_component>-storage = <ls_mard>-storage.
      ENDIF.

      READ TABLE lt_eban ASSIGNING FIELD-SYMBOL(<fs_eban>) WITH KEY arsnr = <ls_order_component>-booking_number
                                                                    arsps = <ls_order_component>-booking_pos BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_order_component>-purchase_order = <fs_eban>-ebeln.
        <ls_order_component>-supplier_name = <fs_eban>-name1.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD complete_header_data.

*   se completan las descripciones de: ubicacion tecnica, emplazamiento, prioridad, equipo, pto. trab. res
    DATA: lr_auart        TYPE RANGE OF aufart,
          lr_priok        TYPE RANGE OF priok,
          lr_tec_location TYPE RANGE OF tplnr,
          lr_center       TYPE RANGE OF swerk,
          lr_placement    TYPE RANGE OF stort_t499s,
          lr_equipment    TYPE RANGE OF equnr,
          lr_job_id       TYPE RANGE OF cr_objid,
          lr_user         TYPE RANGE OF uname.

*  Clase de orden
    lr_auart = VALUE #( FOR <a> IN mt_order_header ( sign = 'I' option = 'EQ' low = <a>-order_class ) ).

*   Centros
    lr_center = VALUE #( FOR <a> IN mt_order_header ( sign = 'I' option = 'EQ' low = <a>-center ) ).
    SORT lr_center DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lr_center.

*   Ubicacion tecnica
    lr_tec_location = VALUE #( FOR <a> IN mt_order_header ( sign = 'I' option = 'EQ' low = <a>-tec_location ) ).
    SORT lr_tec_location DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lr_tec_location.

*   Emplazamiento
    lr_placement = VALUE #( FOR <a> IN mt_order_header ( sign = 'I' option = 'EQ' low = <a>-placement ) ).
    SORT lr_placement DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lr_placement.

*   Equipo
    lr_equipment = VALUE #( FOR <a> IN mt_order_header ( sign = 'I' option = 'EQ' low = <a>-equipment ) ).
    SORT lr_equipment DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lr_equipment.

*   Pto. trab. responsable
    lr_job_id = VALUE #( FOR <a> IN mt_order_header ( sign = 'I' option = 'EQ' low = <a>-job_id ) ).
    SORT lr_job_id DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lr_job_id.

    SELECT auart AS order_class, txt AS order_class_desc
      FROM t003p
      INTO TABLE @DATA(lt_t003p)
      WHERE auart IN @lr_auart AND
            spras EQ @mv_langu.

*   Se recuperan los maestros
    DATA(lt_tec_location) = zcl_pm_masterdata=>get_tecnical_location( it_tec_location = lr_tec_location ).

    DATA(lt_priority) = zcl_pm_masterdata=>get_priorities( ).

    DATA(lt_placement) = zcl_pm_masterdata=>get_placements( EXPORTING it_center = lr_center it_placement = lr_placement ).

    DATA(lt_equipment) = NEW zcl_pm_equipment( )->get_header_data( EXPORTING it_equipment = lr_equipment ).

    DATA(lt_job) = zcl_pm_masterdata=>get_jobs( EXPORTING it_job_id = lr_job_id
                                                          it_center = lr_center ).

*   Se pasan a la tabla de cabecera
    LOOP AT mt_order_header ASSIGNING FIELD-SYMBOL(<ls_order_header>).
      READ TABLE lt_t003p ASSIGNING FIELD-SYMBOL(<ls_t003p>) WITH KEY order_class = <ls_order_header>-order_class.
      IF sy-subrc IS INITIAL.
        <ls_order_header>-order_class_desc = <ls_t003p>-order_class_desc.
      ENDIF.


      READ TABLE lt_tec_location ASSIGNING FIELD-SYMBOL(<ls_tec_location>) WITH KEY tec_location = <ls_order_header>-tec_location.
      IF sy-subrc IS INITIAL.
        <ls_order_header>-tec_location_desc = <ls_tec_location>-tec_location_desc.
      ENDIF.

      READ TABLE lt_priority ASSIGNING FIELD-SYMBOL(<ls_priority>) WITH KEY priority = <ls_order_header>-priority.
      IF sy-subrc IS INITIAL.
        <ls_order_header>-priority_desc = <ls_priority>-priority_desc.
      ENDIF.

      READ TABLE lt_placement ASSIGNING FIELD-SYMBOL(<ls_placement>) WITH KEY placement = <ls_order_header>-placement
                                                                             center = <ls_order_header>-center.
      IF sy-subrc IS INITIAL.
        <ls_order_header>-placement_desc = <ls_placement>-placement_desc.
      ENDIF.

      READ TABLE lt_equipment ASSIGNING FIELD-SYMBOL(<ls_equipment>) WITH KEY equipment = <ls_order_header>-equipment.
      IF sy-subrc IS INITIAL.
        <ls_order_header>-equipment_desc = <ls_equipment>-equipment_desc.
      ENDIF.

      READ TABLE lt_job ASSIGNING FIELD-SYMBOL(<ls_job>) WITH KEY center = <ls_order_header>-center
                                                                  job_id = <ls_order_header>-job_id.
      IF sy-subrc IS INITIAL.
        <ls_order_header>-job = <ls_job>-job.
        <ls_order_header>-job_desc = <ls_job>-job_desc.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD complete_material_doc_data.

    DATA: lr_matnr TYPE RANGE OF matnr,
          lr_werks TYPE RANGE OF werks_d,
          lr_lgort TYPE RANGE OF lgort_d.

*    Materiales
    lr_matnr = VALUE #( FOR <a> IN mt_order_material_doc ( sign = 'I' option = 'EQ' low = <a>-material ) ).
    lr_werks = VALUE #( FOR <a> IN mt_order_material_doc ( sign = 'I' option = 'EQ' low = <a>-center ) ).
    lr_lgort = VALUE #( FOR <a> IN mt_order_material_doc ( sign = 'I' option = 'EQ' low = <a>-location ) ).

    IF lr_matnr IS NOT INITIAL.

*     Se recuperan los maestros
      SELECT matnr AS material, maktx AS material_desc
        FROM makt
        INTO TABLE @DATA(lt_makt)
        WHERE matnr IN @lr_matnr AND
              spras EQ @mv_langu.

      IF lr_werks IS NOT INITIAL AND
         lr_lgort IS NOT INITIAL.

        SELECT matnr AS material, werks AS center, lgort AS location, lgpbe AS storage
          FROM mard
          INTO TABLE @DATA(lt_mard)
          WHERE matnr IN @lr_matnr AND
                werks IN @lr_werks AND
                lgort IN @lr_lgort.

      ENDIF.

    ENDIF.

*   Se pasan a la tabla de notificaciones
    LOOP AT mt_order_material_doc ASSIGNING FIELD-SYMBOL(<ls_order_material_doc>).

      READ TABLE lt_makt ASSIGNING FIELD-SYMBOL(<ls_makt>) WITH KEY material = <ls_order_material_doc>-material.
      IF sy-subrc IS INITIAL.
        <ls_order_material_doc>-material_desc = <ls_makt>-material_desc.
      ENDIF.


      READ TABLE lt_mard ASSIGNING FIELD-SYMBOL(<ls_mard>) WITH KEY material = <ls_order_material_doc>-material
                                                                    center = <ls_order_material_doc>-center
                                                                    location = <ls_order_material_doc>-location.
      IF sy-subrc IS INITIAL.
        <ls_order_material_doc>-storage = <ls_mard>-storage.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD complete_notification_data.

    DATA: lr_employee_code TYPE RANGE OF pernr_d.

*    Empleado
    lr_employee_code = VALUE #( FOR <a> IN mt_order_notification ( sign = 'I' option = 'EQ' low = <a>-employee_number ) ).

*   Se recuperan los maestros
    DATA(lt_employee) = zcl_pm_masterdata=>get_employee_name( it_employee_number = lr_employee_code ).

*   Se pasan a la tabla de notificaciones
    LOOP AT mt_order_notification ASSIGNING FIELD-SYMBOL(<ls_order_notification>).

      READ TABLE lt_employee ASSIGNING FIELD-SYMBOL(<ls_employee>) WITH KEY employee_number = <ls_order_notification>-employee_number.
      IF sy-subrc IS INITIAL.
        <ls_order_notification>-employee_name = <ls_employee>-employee_name.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    mv_langu = iv_langu.
  ENDMETHOD.


  METHOD create_order_notification.

    DATA lt_timetickets TYPE TABLE OF  bapi_alm_timeconfirmation.
    DATA lt_detail_return TYPE TABLE OF bapi_alm_return.
    DATA ls_return TYPE bapiret2.
    DATA lv_datediff TYPE p.
    DATA lv_timediff TYPE p.
    DATA ls_header TYPE thead.
    DATA lt_lines TYPE TABLE OF tline.
    DATA it_text  TYPE STANDARD TABLE OF string.

    APPEND INITIAL LINE TO lt_timetickets ASSIGNING FIELD-SYMBOL(<fs_timetickets>).
    <fs_timetickets>-conf_text = is_order_notif-long_text. ".is_order_notif-short_text.
    <fs_timetickets>-orderid = |{ is_order_notif-order ALPHA = IN }|.
    <fs_timetickets>-operation = '0010'.
    <fs_timetickets>-fin_conf = is_order_notif-is_final.
    <fs_timetickets>-dev_reason = mc_motivo_notif_others.
    <fs_timetickets>-pers_no =  is_order_notif-employee_number .
    <fs_timetickets>-exec_start_date = is_order_notif-work_start_date.
    <fs_timetickets>-exec_start_time = is_order_notif-work_start_time.
    <fs_timetickets>-exec_fin_date = is_order_notif-work_end_date.
    <fs_timetickets>-exec_fin_time = is_order_notif-work_end_time.

    "Calculamos el número de horas
    CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
      EXPORTING
        date1            = <fs_timetickets>-exec_start_date
        time1            = <fs_timetickets>-exec_start_time
        date2            = <fs_timetickets>-exec_fin_date
        time2            = <fs_timetickets>-exec_fin_time
      IMPORTING
        datediff         = lv_datediff
        timediff         = lv_timediff
*       EARLIEST         =
      EXCEPTIONS
        invalid_datetime = 1
        OTHERS           = 2.
    IF sy-subrc EQ 0.
      <fs_timetickets>-act_work = lv_timediff + ( lv_datediff * 24 ).
    ENDIF.

    CALL FUNCTION 'BAPI_ALM_CONF_CREATE'
      IMPORTING
        return        = ls_return
      TABLES
        timetickets   = lt_timetickets
        detail_return = lt_detail_return.

    READ TABLE lt_detail_return ASSIGNING FIELD-SYMBOL(<fs_detail_return>) WITH KEY type = zif_pm_data=>cs_msg_type-error.
    IF sy-subrc EQ 0.
      APPEND <fs_detail_return> TO et_return.
    ELSE.
      COMMIT WORK AND WAIT.
      " Si la notificación es más larga la guardamos
      IF strlen( is_order_notif-long_text ) GT 40.
        " Recuperamos el número de notificación y operación
*        DATA(lv_mandt) = sy-mandt.

        SELECT rueck, rmzhl
          INTO TABLE @DATA(lt_afru)
          FROM afru
          WHERE aufnr EQ @is_order_notif-order.

        SORT lt_afru DESCENDING BY rueck rmzhl.

        READ TABLE lt_afru ASSIGNING FIELD-SYMBOL(<fs_afru>) INDEX 1.
        IF sy-subrc EQ 0.

          CONCATENATE sy-mandt <fs_afru>-rueck <fs_afru>-rmzhl INTO ls_header-tdname.
*          CONCATENATE sy-mandt <fs_afru>-rueck '99999999' INTO ls_header-tdname.

          ls_header-tdobject = 'AUFK'.
          ls_header-tdid = 'RMEL'.
          ls_header-tdspras = 'S'.

          UPDATE afru SET txtsp ='S' WHERE aufnr EQ is_order_notif-order.

          CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
            EXPORTING
              i_string         = is_order_notif-long_text
              i_tabline_length = 132
            TABLES
              et_table         = it_text.

          LOOP AT it_text ASSIGNING FIELD-SYMBOL(<fs_text>).
            APPEND INITIAL LINE TO lt_lines ASSIGNING FIELD-SYMBOL(<fs_lines>).
            <fs_lines>-tdformat = '*'.
            <fs_lines>-tdline   = <fs_text>.
          ENDLOOP.

          CALL FUNCTION 'SAVE_TEXT'
            EXPORTING
              header          = ls_header
*              insert          = 'X'
*              savemode_direct = 'X'
            TABLES
              lines           = lt_lines.

          CALL FUNCTION 'COMMIT_TEXT'
            EXPORTING
              name            = ls_header-tdname
              savemode_direct = 'X'.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD delete_irrelevant_status.

*  Una orden puede tener varios estados activos al mismo tiempo.
* Por ejemplo una orden en estado CTEC ( Cerrado ) aun puede tener el estado NOTI o NOTP ( Notificacion final y notificacion parcial )
* En este metodo se limpian los estados que ya no son relevantes

    DATA: lr_objnr TYPE RANGE OF objnr.

* Se buscan todas las ordenes cerradas
    lr_objnr = VALUE #( FOR <a> IN ct_status WHERE ( status = ms_back_work_status-ctec ) ( sign = 'I' option = 'EQ' low = <a>-objnr ) ).


    IF lr_objnr IS NOT INITIAL.
*     Si la orden esta cerrada se borran las notificaciones parciales y finales
      LOOP AT ct_status INTO DATA(ls_dummy) WHERE objnr IN lr_objnr AND ( status = ms_back_work_status-noti OR
                                                                          status = ms_back_work_status-notp OR
                                                                          status = ms_back_work_status-lib OR
                                                                          status = ms_back_work_status-abie ).
        DELETE ct_status.
      ENDLOOP.
    ENDIF.

* Se buscan todas las ordenes con notificacion final
    lr_objnr = VALUE #( FOR <a> IN ct_status WHERE ( status = ms_back_work_status-noti ) ( sign = 'I' option = 'EQ' low = <a>-objnr ) ).

    IF lr_objnr IS NOT INITIAL.
*     Si la orden tiene notificacion final se borra la notificacion pacial
      LOOP AT ct_status INTO ls_dummy WHERE objnr IN lr_objnr AND ( status = ms_back_work_status-notp OR
                                                                    status = ms_back_work_status-lib OR
                                                                    status = ms_back_work_status-abie ).
        DELETE ct_status.
      ENDLOOP.
    ENDIF.

* Se buscan todas las ordenes liberadas
    lr_objnr = VALUE #( FOR <a> IN ct_status WHERE ( status = ms_back_work_status-lib ) ( sign = 'I' option = 'EQ' low = <a>-objnr ) ).

    IF lr_objnr IS NOT INITIAL.
*     Si la orden esta liberada se borra el estado abierta
      LOOP AT ct_status INTO ls_dummy WHERE objnr IN lr_objnr AND status = ms_back_work_status-abie.
        DELETE ct_status.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_order_components.

    DATA: lr_order     TYPE RANGE OF aufnr.

    CLEAR mt_order_component[].

    CHECK it_order IS NOT INITIAL.

    lr_order = CORRESPONDING #( it_order ).

    SELECT aufnr AS order, rsnum AS booking_number, rspos AS booking_pos, rsart AS booking_key,
           matnr AS material, bdmng AS quantity, meins AS unit, kzear AS is_available, ebeln AS purchase_order, werks AS center,
           lgort AS location, POTX1 as material_desc
      FROM resb
      INTO TABLE @DATA(lt_resb)
        WHERE aufnr IN @lr_order.

    mt_order_component = CORRESPONDING #( lt_resb ).

    IF iv_complete_data IS NOT INITIAL.
      complete_component_data( ).
    ENDIF.

    rt_order_components = CORRESPONDING #( mt_order_component ).

  ENDMETHOD.


  METHOD get_order_header.

    DATA: lr_order TYPE RANGE OF aufnr,
          lr_iloan TYPE RANGE OF iloan.

    CLEAR mt_order_header[].

    CHECK it_order IS NOT INITIAL.

    lr_order = CORRESPONDING #( it_order ).

**   Datos generales de la orden
    SELECT aufnr AS order, auart AS order_class, ktext AS order_short_desc, objnr, equnr AS equipment,  priok AS priority, stort AS placement, qmnum AS notification_id,
     werks AS center, gewrk AS job_id, iloan, aufpl AS operation_key, addat AS ref_date, gstrp AS start_date , gltrp AS end_date, erdat AS creation_date, ernam AS creation_user
      FROM viaufkst
      INTO TABLE @DATA(lt_aufk)
      WHERE aufnr IN @lr_order.

*   Si no encuentra ninguna orden sale
    CHECK sy-subrc IS INITIAL.

*   Rango con ubicaciones de las ordenes encontradas
    lr_iloan = VALUE #( FOR <a> IN lt_aufk ( sign = 'I' option = 'EQ' low = <a>-iloan ) ).
    SORT lr_iloan DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lr_iloan.

*   Se buscan las ubicaciones de las ordenes.
    SELECT iloan, tplnr AS tec_location
      FROM iloa
      INTO TABLE @DATA(lt_iloa)
      WHERE iloan IN @lr_iloan.

*   Se informa la tabla de cabecera
    LOOP AT lt_aufk ASSIGNING FIELD-SYMBOL(<ls_aufk>).
      APPEND INITIAL LINE TO mt_order_header ASSIGNING FIELD-SYMBOL(<ls_order_header>).
      <ls_order_header> = CORRESPONDING #( <ls_aufk> ).

*    Se completan los datos de ILOA
      READ TABLE lt_iloa ASSIGNING FIELD-SYMBOL(<ls_iloa>) WITH KEY iloan = <ls_aufk>-iloan.
      IF sy-subrc IS INITIAL.
        <ls_order_header>-tec_location = <ls_iloa>-tec_location.
      ENDIF.
    ENDLOOP.

    IF iv_complete_data IS NOT INITIAL.
      complete_header_data( ).
    ENDIF.

    rt_order_header = mt_order_header.

  ENDMETHOD.


  METHOD get_order_material_docs.

    DATA: lr_aufnr TYPE RANGE OF aufnr,
          lr_bwart TYPE RANGE OF bwart.

    CLEAR: mt_order_material_doc[].

    CHECK it_order IS NOT INITIAL.
    lr_aufnr = CORRESPONDING #( it_order ).
    lr_bwart = CORRESPONDING #( it_movement_type ).

    SELECT mseg~mblnr AS material_doc, mseg~mjahr AS material_doc_year, zeile AS material_doc_position, bwart AS movement_type, mseg~matnr AS material,
           mseg~menge AS quantity, mseg~erfmg AS quantity_2, mseg~smbln, mseg~sjahr, mseg~smblp, mseg~zeile,
           erfme AS unit, aufnr AS order, mseg~ebeln AS purchase_order, mseg~werks AS center, mseg~lgort AS location, bwart AS mov_type, dmbtr AS amount, wempf AS employee,
           mkpf~bldat AS doc_date, mkpf~usnam AS creation_user, ekpo~txz01 AS material_desc
      FROM mseg INNER JOIN mkpf ON mseg~mblnr EQ mkpf~mblnr AND mseg~mjahr EQ mkpf~mjahr
      LEFT JOIN ekpo ON ekpo~ebeln EQ mseg~ebeln AND ekpo~ebelp EQ mseg~ebelp
      INTO TABLE @DATA(lt_mseg)
      WHERE bwart IN @lr_bwart AND
            aufnr IN @lr_aufnr.

    " Miramos si la anulación de docs 261, es decir, los 262, para no mostrarlos y eliminar su documento original del listado
    LOOP AT lt_mseg ASSIGNING FIELD-SYMBOL(<fs_mseg>).
      DATA(lv_index) = sy-tabix.
      IF <fs_mseg>-movement_type EQ zcl_pm_order=>mc_material_doc_warehouse_c.
        READ TABLE lt_mseg TRANSPORTING NO FIELDS WITH KEY material_doc = <fs_mseg>-smbln material_doc_year = <fs_mseg>-sjahr zeile = <fs_mseg>-smblp.
        IF sy-subrc EQ 0.
          DELETE lt_mseg INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DELETE lt_mseg WHERE movement_type EQ zcl_pm_order=>mc_material_doc_warehouse_c.


    mt_order_material_doc = CORRESPONDING #( lt_mseg ).

    IF iv_complete_data IS NOT INITIAL.
      complete_material_doc_data( ).
    ENDIF.

    rt_order_material_doc = CORRESPONDING #( mt_order_material_doc ).

  ENDMETHOD.


  METHOD get_order_notifications.

    DATA: lr_notification_key     TYPE RANGE OF co_rueck,
          lr_notification_counter TYPE RANGE OF co_rmzhl,
          lr_order                TYPE RANGE OF aufnr.

    DATA ls_thead TYPE thead.
    DATA it_text   TYPE STANDARD TABLE OF string.
    DATA ls_header TYPE thead.
    DATA lt_lines TYPE TABLE OF tline.

    CLEAR mt_order_notification[].

    CHECK it_notification_key IS NOT INITIAL OR
          it_order IS NOT INITIAL.

    lr_notification_key = CORRESPONDING #( it_notification_key ).
    lr_notification_counter = CORRESPONDING #( it_notification_counter ).
    lr_order = CORRESPONDING #( it_order ).

    SELECT rueck AS notification_key, rmzhl AS notification_counter, ltxa1 AS short_text, pernr AS employee_number, ernam AS creation_user,
           ismnw AS worked_time, ismne AS worked_time_unit, aufnr AS order, aufpl AS operation_key, aplzl AS operation_counter, vornr AS operation_number,
           isdd AS work_start_date, isdz AS work_start_time, iedd AS work_end_date, iedz AS work_end_time, aueru AS is_final, erzet AS creation_hour, ersda AS creation_date
      FROM afru
      INTO TABLE @DATA(lt_afru)
      WHERE rueck IN @lr_notification_key AND
            rmzhl IN @lr_notification_counter AND
            aufnr IN @lr_order.

    mt_order_notification = CORRESPONDING #( lt_afru ).

    " Recuperamos las descripciones largas
    LOOP AT mt_order_notification ASSIGNING FIELD-SYMBOL(<fs_order_notification>).

      ls_thead-tdid     = 'RMEL'.
      ls_thead-tdspras    =  'S'.
      ls_thead-tdobject   = 'AUFK'.
      CONCATENATE sy-mandt <fs_order_notification>-notification_key <fs_order_notification>-notification_counter INTO ls_thead-tdname.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = ls_thead-tdid
          language                = ls_thead-tdspras
          name                    = ls_thead-tdname
          object                  = ls_thead-tdobject
        IMPORTING
          header                  = ls_thead
        TABLES
          lines                   = lt_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_line>).
        AT FIRST.
          <fs_order_notification>-long_text = <fs_line>-tdline.
          CONTINUE.
        ENDAT.
        CONCATENATE <fs_order_notification>-long_text <fs_line>-tdline INTO <fs_order_notification>-long_text SEPARATED BY space.
        IF sy-subrc EQ 4.
          EXIT.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    IF iv_complete_data IS NOT INITIAL.
      complete_notification_data( ).
    ENDIF.

    rt_order_notification = CORRESPONDING #( mt_order_notification ).

  ENDMETHOD.


  METHOD get_order_operations.

    DATA: lr_operation_key TYPE RANGE OF co_aufpl.

    CLEAR mt_order_operation[].

    CHECK it_operation_key IS NOT INITIAL.

    lr_operation_key = CORRESPONDING #( it_operation_key ).

    SELECT aufpl AS operation_key, aplzl AS operation_counter, vornr AS operation_number, rueck AS notification_key, rmzhl AS notification_counter, ltxa1 AS operation_desc
      FROM afvc
      INTO TABLE @DATA(lt_afvc)
      WHERE aufpl IN @lr_operation_key .

    mt_order_operation = CORRESPONDING #( lt_afvc ).

    IF iv_complete_data IS NOT INITIAL.

    ENDIF.

    rt_order_operation = CORRESPONDING #( mt_order_operation ).

  ENDMETHOD.


  METHOD get_order_partners.

    DATA: lr_objnr TYPE RANGE OF j_objnr,
          lr_parvw TYPE RANGE OF parvw.

    CLEAR mt_order_partner[].

    CHECK it_objnr IS NOT INITIAL.

    lr_objnr = CORRESPONDING #( it_objnr ).

    DATA(lv_oficial_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'OFICIAL_ORDER_PARVW' ) ).
    DATA(lv_enc_order_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'ENC_PROD_ORDER_PARVW' ) ).

    lr_parvw = VALUE #( sign = 'I' option = 'EQ' ( low = lv_oficial_parvw ) ( low = lv_enc_order_parvw ) ).

    SELECT  objnr, parvw, counter, parnr AS employee_code,
           erdat AS creation_date, erzeit AS creation_time, ernam AS creation_user
      FROM ihpa
      INTO TABLE @DATA(lt_ihpa)
      WHERE objnr IN @lr_objnr AND
            parvw IN @lr_parvw AND
            kzloesch EQ @abap_false.

    mt_order_partner = CORRESPONDING #( lt_ihpa ).

    rt_order_partner = CORRESPONDING #( mt_order_partner ).

  ENDMETHOD.


  METHOD massive_assign.

    DATA:
      lt_partner    TYPE  STANDARD TABLE OF bapi_alm_order_partn_mul,
      lt_partner_up TYPE STANDARD TABLE OF  bapi_alm_order_partn_mul_up,
      ls_methods    TYPE bapi_alm_order_method,
      lt_methods    TYPE STANDARD TABLE OF  bapi_alm_order_method,
      lt_objnr      TYPE RANGE OF j_objnr,
      lt_return     TYPE bapiret2_t,
      lv_objkey     TYPE objidext.

    LOOP AT it_orders ASSIGNING FIELD-SYMBOL(<fs_massive_order>).

      CLEAR: lt_partner, lt_partner_up, ls_methods, lt_methods, lt_return.
      lv_objkey = <fs_massive_order>-order.

**********************************************************************
* ASIGNACIONES
**********************************************************************
*   Se recuperan las personas asignadas a la orden
      lt_objnr = VALUE #( ( sign = 'I' option = 'EQ' low = <fs_massive_order>-objnr ) ).
      DATA(lt_order_partner) = get_order_partners( lt_objnr ).
      DATA(lv_oficial_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'OFICIAL_ORDER_PARVW' ) ) .
      DELETE lt_order_partner WHERE parvw NE lv_oficial_parvw.

      LOOP AT it_oficials ASSIGNING FIELD-SYMBOL(<fs_order_as>) WHERE selected EQ abap_true.
        " Miramos si ya existe esta asignación, y en caso de que no exista la añadimos
        READ TABLE lt_order_partner ASSIGNING FIELD-SYMBOL(<fs_ord_partner>) WITH KEY employee_code = <fs_order_as>-employee_id.
        IF sy-subrc NE 0.
          APPEND INITIAL LINE TO lt_partner ASSIGNING FIELD-SYMBOL(<fs_partner>).
          <fs_partner>-orderid =  <fs_massive_order>-order.
          <fs_partner>-partn_role = lv_oficial_parvw.
          <fs_partner>-partner = <fs_order_as>-employee_id.
          APPEND INITIAL LINE TO lt_partner_up ASSIGNING FIELD-SYMBOL(<fs_partner_up>).
          <fs_partner_up>-orderid = <fs_massive_order>-order.
          <fs_partner_up>-partn_role = abap_true.
          <fs_partner_up>-partner = abap_true.
        ELSE.
          DELETE lt_order_partner INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

**    " Miramos si tenemos que eliminar asignaciones antiguas
*      LOOP AT lt_order_partner ASSIGNING <fs_ord_partner>.
*        READ TABLE lt_partner ASSIGNING FIELD-SYMBOL(<fs_par>) WITH KEY partner = <fs_ord_partner>-employee_code.
*        IF sy-subrc NE 0.
*          APPEND INITIAL LINE TO lt_partner ASSIGNING <fs_partner>.
*          <fs_partner>-orderid =  lv_order.
*          <fs_partner>-partn_role_old = lv_oficial_parvw.
*          <fs_partner>-partner_old = <fs_ord_partner>-employee_code.
*          APPEND INITIAL LINE TO lt_partner_up ASSIGNING <fs_partner_up>.
*          <fs_partner_up>-orderid = lv_order.
*          <fs_partner_up>-partn_role_old = lv_oficial_parvw.
*          <fs_partner_up>-partn_role = abap_true.
*          <fs_partner_up>-partner_old = <fs_ord_partner>-employee_code.
*          <fs_partner_up>-partner = abap_true.
*        ENDIF.
*      ENDLOOP.

      IF lt_partner[] IS NOT INITIAL.
        ls_methods-refnumber    = '000001'.
        ls_methods-objecttype   = mc_bapi_ord_obj_type-partner.
        ls_methods-method       = mc_bapi_ord_method-change.
        ls_methods-objectkey    = lv_objkey.
        APPEND ls_methods TO lt_methods.
        CLEAR ls_methods.
        ls_methods-objectkey    = lv_objkey."lv_objkey_header.
        ls_methods-refnumber    = '000001'.
        ls_methods-method       = mc_bapi_ord_method-save.
        APPEND ls_methods TO lt_methods.


        CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
          TABLES
            it_methods    = lt_methods
*           it_header     = lt_header
*           it_header_up  = lt_header_up
            it_partner    = lt_partner
            it_partner_up = lt_partner_up
            return        = lt_return.

        READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) WITH KEY type = zif_pm_data=>cs_msg_type-error.
        IF sy-subrc NE 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

        ELSE.
          APPEND INITIAL LINE TO et_return ASSIGNING FIELD-SYMBOL(<ls_return>).
          <ls_return>-type = <fs_return>-type.
          MESSAGE ID <fs_return>-id TYPE <fs_return>-type NUMBER <fs_return>-number WITH <fs_return>-message_v1 <fs_return>-message_v2
                   <fs_return>-message_v3 <fs_return>-message_v4 INTO <ls_return>-message.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
        ENDIF.


      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD READ_ORDER_LONG_DESCR.
    DATA ls_thead TYPE thead.
    DATA it_text   TYPE STANDARD TABLE OF string.
    DATA ls_header TYPE thead.
    DATA lt_lines TYPE TABLE OF tline.

    ls_thead-tdid     = 'KOPF'.
    ls_thead-tdspras    =  sy-langu.
    ls_thead-tdname     = sy-mandt && iv_order_id.
    ls_thead-tdobject   = 'AUFK'.
    CLEAR lt_lines.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = ls_thead-tdid
        language                = ls_thead-tdspras
        name                    = ls_thead-tdname
        object                  = ls_thead-tdobject
      IMPORTING
        header                  = ls_thead
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_line>).

      AT FIRST.
        cv_notif_long_desc = <fs_line>-tdline.
        CONTINUE.
      ENDAT.

      CONCATENATE cv_notif_long_desc <fs_line>-tdline INTO cv_notif_long_desc SEPARATED BY space.
      IF sy-subrc EQ 4.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD release_pending_order.


    DATA: ls_methods   TYPE bapi_alm_order_method,
          lt_methods   TYPE STANDARD TABLE OF  bapi_alm_order_method,
          lt_header    TYPE TABLE OF bapi_alm_order_headers_i,
          lt_header_up TYPE STANDARD TABLE OF  bapi_alm_order_headers_up,
          lt_numbers   TYPE STANDARD TABLE OF bapi_alm_numbers,
          lt_return    TYPE bapiret2_t.

    DATA: ls_syststat     TYPE   bapi2080_notsti,
          ls_systemstatus TYPE bapi2080_notadt-systatus,
          ls_userstatus   TYPE   bapi2080_notadt-usrstatus,
          lt_return_notif TYPE STANDARD TABLE OF  bapiret2.

**********************************************************************
* DATOS GENERALES DE LA ORDEN
**********************************************************************

    ls_methods-refnumber    = '000001'.
    ls_methods-objecttype   = mc_bapi_ord_obj_type-header.
    ls_methods-method       = mc_bapi_ord_method-release.
    ls_methods-objectkey    = is_order-order.
    APPEND ls_methods TO lt_methods.
    CLEAR ls_methods.
    ls_methods-objectkey    = is_order-order.
    ls_methods-refnumber    = '000001'.
    ls_methods-method       = mc_bapi_ord_method-save.
    APPEND ls_methods TO lt_methods.

    lt_header = VALUE #( ( orderid = is_order-order ) ).
    lt_header_up = VALUE #( ( orderid = is_order-order ) ).

**********************************************************************
* ACTUALIZAMOS LA ORDEN
**********************************************************************
    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
      TABLES
        it_methods   = lt_methods
        it_header    = lt_header
        it_header_up = lt_header_up
        et_numbers   = lt_numbers
        return       = lt_return.

    READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) WITH KEY type = zif_pm_data=>cs_msg_type-error.
    IF sy-subrc NE 0.

      APPEND INITIAL LINE TO rt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      <ls_return>-type = zif_pm_data=>cs_msg_type-success.
      MESSAGE ID zif_pm_data=>cs_message_id TYPE zif_pm_data=>cs_msg_type-success NUMBER 010 INTO <ls_return>-message.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ELSE.
      APPEND INITIAL LINE TO rt_return ASSIGNING <ls_return>.
      <ls_return>-type = <fs_return>-type.
      MESSAGE ID <fs_return>-id TYPE <fs_return>-type NUMBER <fs_return>-number WITH <fs_return>-message_v1 <fs_return>-message_v2
               <fs_return>-message_v3 <fs_return>-message_v4 INTO <ls_return>-message.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ENDIF.

  ENDMETHOD.


  METHOD save_order.

    DATA: ls_methods      TYPE bapi_alm_order_method,
          lt_methods      TYPE STANDARD TABLE OF  bapi_alm_order_method,
          ls_header       TYPE bapi_alm_order_headers_i,
          lt_header       TYPE STANDARD TABLE OF  bapi_alm_order_headers_i,
          lt_header_up    TYPE STANDARD TABLE OF  bapi_alm_order_headers_up,
          ls_header_up    TYPE bapi_alm_order_headers_up,
          lt_partner      TYPE  STANDARD TABLE OF bapi_alm_order_partn_mul,
          lt_partner_up   TYPE STANDARD TABLE OF  bapi_alm_order_partn_mul_up,
          lt_operation    TYPE STANDARD TABLE OF  bapi_alm_order_operation,
          lt_operation_up TYPE STANDARD TABLE OF  bapi_alm_order_operation_up,
          lt_return       TYPE bapiret2_t,
          lt_numbers      TYPE STANDARD TABLE OF bapi_alm_numbers,
          lt_text         TYPE STANDARD TABLE OF  bapi_alm_text,
          lt_text_lines   TYPE STANDARD TABLE OF  bapi_alm_text_lines,
          it_text         TYPE STANDARD TABLE OF string.

    DATA lv_order TYPE aufnr.
    DATA lv_method TYPE swo_method.
    DATA lv_objkey TYPE objidext.
    DATA lv_objkey_header TYPE objidext.

    DATA lt_objnr  TYPE RANGE OF j_objnr.

    IF is_order-order IS INITIAL.
      lv_order = '%00000000001'.
      IF iv_notif_id IS INITIAL.
        lv_method = mc_bapi_ord_method-create.
        lv_objkey_header = '%00000000001'.
      ELSE.
        lv_method = mc_bapi_ord_method-createtonotif.
        lv_objkey_header = '%00000000001' && iv_notif_id.
      ENDIF.
    ELSE.
      lv_objkey_header = is_order-order.
      lv_order = is_order-order.
      lv_method = mc_bapi_ord_method-change.
    ENDIF.
    lv_objkey = lv_order.

**********************************************************************
* DATOS GENERALES DE LA ORDEN
**********************************************************************

    ls_methods-refnumber    = '000001'.
    ls_methods-objecttype   = mc_bapi_ord_obj_type-header.
    ls_methods-method       = lv_method.
    ls_methods-objectkey    = lv_objkey_header.
    APPEND ls_methods TO lt_methods.
    CLEAR ls_methods.
    ls_methods-objectkey    = lv_objkey_header.
    ls_methods-refnumber    = '000001'.
    ls_methods-method       = mc_bapi_ord_method-save.
    APPEND ls_methods TO lt_methods.

    ls_header-orderid = lv_order.
    ls_header-short_text = is_order-order_short_desc.
    ls_header-funct_loc = is_order-tec_location.
    ls_header-equipment = |{ is_order-equipment ALPHA = IN }|.
    ls_header-start_date = is_order-start_date.
    ls_header-finish_date = is_order-end_date.
    ls_header-mn_wk_ctr = is_order-job. " TUBINOX
    ls_header-order_type = is_order-order_class.
    IF is_order-order IS INITIAL.
      ls_header-maintplant = is_order-center.
    ENDIF.
    IF iv_notif_id IS NOT INITIAL.
      ls_header-notif_no = iv_notif_id.
    ENDIF.
    APPEND ls_header TO lt_header.

    ls_header_up-orderid                            = lv_order.
    ls_header_up-short_text                         = abap_true.
    ls_header_up-funct_loc                         = abap_true.
    ls_header_up-equipment                         = abap_true.
    ls_header_up-start_date                         = abap_true.
    ls_header_up-finish_date                         = abap_true.
    ls_header_up-mn_wk_ctr                          = abap_true.
    IF is_order-order IS INITIAL.
      ls_header_up-maintplant                          = abap_true.
    ENDIF.
    IF iv_notif_id IS NOT INITIAL.
      ls_header_up-notif_no = abap_true.
    ENDIF.
    APPEND ls_header_up TO lt_header_up.

**********************************************************************
* OPERACIONES
**********************************************************************
    " Si estamos creando una orden, rellenamos la información de la operación
    IF is_order-order IS INITIAL.
      DATA lv_uvorn TYPE uvorn.
      APPEND INITIAL LINE TO lt_operation ASSIGNING FIELD-SYMBOL(<fs_operation>).
      <fs_operation>-activity = '0010'.
*      <fs_operation>-sub_activity = lv_uvorn.
      <fs_operation>-work_cntr = is_order-job.
*      <fs_operation>-description = is_order-order_short_desc.
      <fs_operation>-plant = is_order-center.
*      <fs_operation>-control_key = 'ZCAP'.
*      <fs_operation>-calc_key = '1'.
*      <fs_operation>-quantity = '1'.
*      <fs_operation>-acttype = 'MTOGR'.
*      <fs_operation>-number_of_capacities  = '1'.

      APPEND INITIAL LINE TO lt_operation_up ASSIGNING FIELD-SYMBOL(<fs_operation_up>).
      <fs_operation_up>-activity = abap_true.
*      <fs_operation_up>-sub_activity = lv_uvorn.
      <fs_operation_up>-work_cntr = abap_true.
*      <fs_operation_up>-description = abap_true.
      <fs_operation_up>-plant = abap_true.
*      <fs_operation_up>-control_key = abap_true.
*      <fs_operation_up>-calc_key = abap_true.
*      <fs_operation_up>-quantity = abap_true.
*      <fs_operation_up>-acttype = abap_true.
*      <fs_operation_up>-number_of_capacities  = abap_true.

      ls_methods-refnumber    = '000001'.
      ls_methods-objecttype   = mc_bapi_ord_obj_type-operation.
      ls_methods-method       = mc_bapi_ord_method-create.
      ls_methods-objectkey    = lv_order && '0010'.
      APPEND ls_methods TO lt_methods.

*      IF is_order-center = '1001'.                               "Santiago 15.09.21
      IF is_order-center = '1001' OR is_order-center = '1004'.    "Santiago 15.09.21
        ls_methods-refnumber    = '000001'.
        ls_methods-objecttype   = mc_bapi_ord_obj_type-header.
        ls_methods-method       = mc_bapi_ord_method-release.
        ls_methods-objectkey    = lv_objkey .
        APPEND ls_methods TO lt_methods.
      ENDIF.
    ENDIF.
**********************************************************************
* ASIGNACIONES
**********************************************************************
*   Se recuperan las personas asignadas a la orden
    lt_objnr = VALUE #( ( sign = 'I' option = 'EQ' low = is_order-objnr ) ).
    DATA(lt_order_partner) = get_order_partners( lt_objnr ).
    DATA(lv_oficial_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'OFICIAL_ORDER_PARVW' ) ) .
    DELETE lt_order_partner WHERE parvw NE lv_oficial_parvw.

    LOOP AT it_assigments ASSIGNING FIELD-SYMBOL(<fs_order_as>) WHERE assigned EQ abap_true.
      " Miramos si ya existe esta asignación, y en caso de que no exista la añadimos
      READ TABLE lt_order_partner ASSIGNING FIELD-SYMBOL(<fs_ord_partner>) WITH KEY employee_code = <fs_order_as>-employee_code.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO lt_partner ASSIGNING FIELD-SYMBOL(<fs_partner>).
        <fs_partner>-orderid =  lv_order.
        <fs_partner>-partn_role = lv_oficial_parvw.
        <fs_partner>-partner = <fs_order_as>-employee_code.
        APPEND INITIAL LINE TO lt_partner_up ASSIGNING FIELD-SYMBOL(<fs_partner_up>).
        <fs_partner_up>-orderid = lv_order.
        <fs_partner_up>-partn_role = abap_true.
        <fs_partner_up>-partner = abap_true.
      ELSE.
        DELETE lt_order_partner INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

*    " Miramos si tenemos que eliminar asignaciones antiguas
    LOOP AT lt_order_partner ASSIGNING <fs_ord_partner>.
      READ TABLE lt_partner ASSIGNING FIELD-SYMBOL(<fs_par>) WITH KEY partner = <fs_ord_partner>-employee_code.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO lt_partner ASSIGNING <fs_partner>.
        <fs_partner>-orderid =  lv_order.
        <fs_partner>-partn_role_old = lv_oficial_parvw.
        <fs_partner>-partner_old = <fs_ord_partner>-employee_code.
        APPEND INITIAL LINE TO lt_partner_up ASSIGNING <fs_partner_up>.
        <fs_partner_up>-orderid = lv_order.
        <fs_partner_up>-partn_role_old = lv_oficial_parvw.
        <fs_partner_up>-partn_role = abap_true.
        <fs_partner_up>-partner_old = <fs_ord_partner>-employee_code.
        <fs_partner_up>-partner = abap_true.
      ENDIF.
    ENDLOOP.

    IF lt_partner[] IS NOT INITIAL.
      ls_methods-refnumber    = '000001'.
      ls_methods-objecttype   = mc_bapi_ord_obj_type-partner.
      ls_methods-method       = mc_bapi_ord_method-change.
      ls_methods-objectkey    = lv_objkey.
      APPEND ls_methods TO lt_methods.
    ENDIF.

**********************************************************************
* ACTUALIZAMOS LA ORDEN
**********************************************************************


*    lt_text_lines = value #( ( tdformat = '*'  tdline = is_order-order_short_desc ) ).
    DATA(lv_full_text) = CONV string( |{ is_order-order_short_desc }{ is_order-order_long_desc }| ).
    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = lv_full_text
        i_tabline_length = 132
      TABLES
        et_table         = it_text.

    DESCRIBE TABLE it_text LINES DATA(lv_textlen).

    APPEND INITIAL LINE TO lt_text ASSIGNING FIELD-SYMBOL(<fs_text>).
    <fs_text>-orderid = lv_order.
*    <fs_text>-activity = '0010'.
    <fs_text>-langu = sy-langu.
    <fs_text>-textstart = 1.
    <fs_text>-textend = lv_textlen.



    LOOP AT it_text ASSIGNING FIELD-SYMBOL(<fs_text2>).
      APPEND INITIAL LINE TO lt_text_lines ASSIGNING FIELD-SYMBOL(<fs_text_lines>).
      <fs_text_lines>-tdformat = '*'.
      <fs_text_lines>-tdline   = <fs_text2>.
    ENDLOOP.
**
    ls_methods-refnumber    = '000001'.
    ls_methods-objecttype   = mc_bapi_ord_obj_type-text.
    ls_methods-method       = mc_bapi_ord_method-create.
*    ls_methods-method       = lv_method.
*    ls_methods-objectkey    = lv_order && '0010'.
    ls_methods-objectkey    = lv_order.
    APPEND ls_methods TO lt_methods.
*<fs_text_lines>-tdformat =
    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
      TABLES
        it_methods      = lt_methods
        it_header       = lt_header
        it_header_up    = lt_header_up
        it_operation    = lt_operation
        it_operation_up = lt_operation_up
        it_partner      = lt_partner
        it_partner_up   = lt_partner_up
        it_text         = lt_text
        it_text_lines   = lt_text_lines
*       IT_HEADER_SRV   =
*       IT_HEADER_SRV_UP                   =
        return          = lt_return
        et_numbers      = lt_numbers.


    READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) WITH KEY type = zif_pm_data=>cs_msg_type-error.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO et_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      <ls_return>-type = zif_pm_data=>cs_msg_type-success.
*     Si no viene numero de aviso por cabecera ls_hexport si tiene, se ha creado el aviso
      IF is_order-order IS INITIAL.
        READ TABLE lt_numbers ASSIGNING FIELD-SYMBOL(<fs_numbers>) INDEX 1.
        IF sy-subrc EQ 0.
          ev_order_id = <fs_numbers>-aufnr_new.
        ENDIF.
        MESSAGE ID zif_pm_data=>cs_message_id TYPE zif_pm_data=>cs_msg_type-success NUMBER 007 WITH ev_order_id INTO <ls_return>-message.
      ELSE.
        ev_order_id = is_order-order.
        MESSAGE ID zif_pm_data=>cs_message_id TYPE zif_pm_data=>cs_msg_type-success NUMBER 005 INTO <ls_return>-message.
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

*      save_order_long_descr(  EXPORTING iv_order_id = ev_order_id iv_descr = is_order-order_long_desc   ).

    ELSE.
      APPEND INITIAL LINE TO et_return ASSIGNING <ls_return>.
      <ls_return>-type = <fs_return>-type.
      MESSAGE ID <fs_return>-id TYPE <fs_return>-type NUMBER <fs_return>-number WITH <fs_return>-message_v1 <fs_return>-message_v2
               <fs_return>-message_v3 <fs_return>-message_v4 INTO <ls_return>-message.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ENDIF.

  ENDMETHOD.


  METHOD save_order_long_descr.

    DATA it_text   TYPE STANDARD TABLE OF string.
    DATA ls_header TYPE thead.
    DATA lt_lines TYPE TABLE OF tline.
    DATA lt_lines_aux TYPE TABLE OF tline.

*    IF lv_langu IS INITIAL.
*      lv_langu = sy-langu.
*    ENDIF.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = iv_descr
        i_tabline_length = 132
      TABLES
        et_table         = it_text.

    LOOP AT it_text ASSIGNING FIELD-SYMBOL(<fs_text>).
      APPEND INITIAL LINE TO lt_lines ASSIGNING FIELD-SYMBOL(<fs_lines>).
      <fs_lines>-tdformat = '*'.
      <fs_lines>-tdline   = <fs_text>.
    ENDLOOP.

    lt_lines_aux[] = lt_lines[].

    " Para modificaciones
    ls_header-mandt = sy-mandt.
    ls_header-tdid = 'KOPF'.
    ls_header-tdspras = sy-langu.
    ls_header-tdname = sy-mandt && iv_order_id.
    ls_header-tdobject = 'AUFK'.


    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header    = ls_header
      TABLES
        lines     = lt_lines_aux
      EXCEPTIONS
        id        = 1
        language  = 2
        name      = 3
        object    = 4
        OTHERS    = 5.

    IF sy-subrc EQ 0.

      CALL FUNCTION 'COMMIT_TEXT'.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDIF.
  ENDMETHOD.


  METHOD search_orders.

    DATA: lr_aufnr  TYPE RANGE OF aufnr,
          lr_auart  TYPE RANGE OF aufart,
          lr_status TYPE RANGE OF j_status,
          lr_objnr  TYPE RANGE OF objnr,
          lr_erdat  TYPE RANGE OF erdat,
          lr_stort  TYPE RANGE OF stort,
          lr_tplnr  TYPE RANGE OF tplnr,
          lr_equnr  TYPE RANGE OF equnr,
          lr_iloan  TYPE RANGE OF iloan,
          lr_werks  TYPE RANGE OF werks_d,
          lr_pernr  TYPE RANGE OF pernr_d,
          lr_arbpl  TYPE RANGE OF arbpl,
          lr_objid  TYPE RANGE OF cr_objid.

    CHECK it_date_creation IS NOT INITIAL OR it_order IS NOT INITIAL. "Las fechas son obligatorias

    lr_aufnr = CORRESPONDING #( it_order ).
    lr_auart = CORRESPONDING #( it_order_class ).
    lr_status = CORRESPONDING #( it_order_status ).
    lr_erdat = CORRESPONDING #( it_date_creation ).
    lr_stort = CORRESPONDING #( it_placement ).
    lr_tplnr = CORRESPONDING #( it_tec_location ).
    lr_equnr = CORRESPONDING #( it_equipment ).
    lr_werks = CORRESPONDING #( it_center ).
    lr_pernr = CORRESPONDING #( it_employee_number ).
    lr_arbpl = CORRESPONDING #( it_job ).

    SELECT aufnr, iloan, objnr, gewrk AS job_id
      FROM viord
      INTO TABLE @DATA(lt_viord)
      WHERE aufnr IN @lr_aufnr AND
            autyp EQ @mc_order_type AND
            auart IN @lr_auart AND
            equnr IN @lr_equnr AND
            werks IN @lr_werks AND
            erdat IN @lr_erdat.

* Si no encuentra ninguna orden no sigue buscando
    CHECK sy-subrc IS INITIAL.

*   Si se busca por estado se filtran los avisos
    IF lr_status IS NOT INITIAL.

      lr_objnr = VALUE #( FOR <a> IN lt_viord ( sign = 'I' option = 'EQ' low = <a>-objnr ) ).

*     Recupera todos los estados activos de las ordenes
      DATA(lt_jest) = zcl_pm_masterdata=>get_object_status( lr_objnr ).

*     Se limpian los estados para tener solo los relevantes de cada aviso
      delete_irrelevant_status( CHANGING ct_status = lt_jest ).

*     Borra todos los estados de las ordenes que no coincidan con los que vienen por parametro
      DELETE lt_jest WHERE status NOT IN lr_status.

*     Rango con las ordenes cuyos estados coinciden con los que vienen por parametro
      lr_objnr = VALUE #( FOR <b> IN lt_jest ( sign = 'I' option = 'EQ' low = <b>-objnr ) ).
      IF lr_objnr[] IS NOT INITIAL.
        DELETE lt_viord WHERE objnr NOT IN lr_objnr.
      ELSE.
        CLEAR lt_viord[].
      ENDIF.

*     Si no queda ninguna orden sale de la busqueda
      CHECK lt_viord IS NOT INITIAL.

    ENDIF.


*   Si se busca por emplazamiento se filtran las ordenes
    IF lr_tplnr IS NOT INITIAL OR
       lr_stort IS NOT INITIAL.

*     Rango con ubicaciones de los avisso encontrados
      lr_iloan = VALUE #( FOR <a> IN lt_viord ( sign = 'I' option = 'EQ' low = <a>-iloan ) ).
      SORT lr_iloan DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lr_iloan.

*     Se buscan las ubicaciones de las ordenes. Se filtra por emplazamiento y ub tecnica
      SELECT iloan
        FROM iloa
        INTO TABLE @DATA(lt_iloa)
        WHERE iloan IN @lr_iloan AND
              stort IN @lr_stort AND
              tplnr IN @lr_tplnr.

      IF sy-subrc IS NOT INITIAL.
*       Si no encuentra ninguna ubicacion se descartan todas las ordenes. (Con un rango vacio no funciona NOT IN)
        CLEAR lt_viord[].
      ELSE.
*     Nuevo rango para filtrar
        lr_iloan = VALUE #( FOR <c> IN lt_iloa ( sign = 'I' option = 'EQ' low = <c>-iloan ) ).
*     Se borran los avisos que no estan en las ubicaciones
        DELETE lt_viord WHERE iloan NOT IN lr_iloan.
      ENDIF.

*     Si no queda ninguna orden sale de la busqueda
      CHECK lt_viord IS NOT INITIAL.

    ENDIF.


*   Si se busca por puesto de trabajo se filtran los datos
    IF lr_arbpl IS NOT INITIAL.

*     Puede ser lioso pero el campo ARBPL de la cabecera del aviso es en realidad el OBJID del puesto de trabajo
*     En la tabla de puestos de trabajo ARBPL es el puesto en si
      lr_objid = VALUE #( FOR <a> IN lt_viord ( sign = 'I' option = 'EQ' low = <a>-job_id ) ).
      SORT lr_objid DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lr_objid.

*     Se buscan los puestos de trabajo de los avisos preseleccionados que coincidan con los que se buscan
      SELECT objid
        FROM crhd
        INTO TABLE @DATA(lt_crhd)
        WHERE objty EQ @zcl_pm_masterdata=>mc_job_type AND
              objid IN @lr_objid AND
              werks IN @lr_werks AND
              arbpl IN @lr_arbpl AND
              verwe EQ @zcl_pm_masterdata=>mc_job_class.

      IF sy-subrc IS NOT INITIAL.
*       Si no encuentra ninguna ubicacion se descartan todos los avisos. (Con un rango vacio no funciona NOT IN)
        CLEAR lt_viord[].
      ELSE.
        lr_objid = VALUE #( FOR <e> IN lt_crhd ( sign = 'I' option = 'EQ' low = <e>-objid ) ).
*       Se borran los avisos que no tienen el ID del puesto de trabajo
        DELETE lt_viord WHERE job_id NOT IN lr_objid.
      ENDIF.
    ENDIF.

*   Si se selecciona por oficial
    IF lr_pernr IS NOT INITIAL.

*     Rango con objnr para buscar los interlocutores
      lr_objnr = VALUE #( FOR <a> IN lt_viord ( sign = 'I' option = 'EQ' low = <a>-objnr ) ).

      DATA(lv_oficial_parvw) = CONV parvw( zcl_pm_constants=>obtener_constantes( iv_constante = 'OFICIAL_ORDER_PARVW' ) ).

      SELECT objnr
        FROM ihpa
        INTO TABLE @DATA(lt_ihpa)
        WHERE objnr IN @lr_objnr AND
              parvw EQ @lv_oficial_parvw AND
              parnr IN @lr_pernr AND
              kzloesch EQ @abap_false.

      IF sy-subrc IS NOT INITIAL.
        CLEAR lt_viord[].
      ELSE.
        lr_objnr = VALUE #( FOR <d> IN lt_ihpa ( sign = 'I' option = 'EQ' low = <d>-objnr ) ).
*     Se borran los avisos que no estan en las ubicaciones
        DELETE lt_viord WHERE objnr NOT IN lr_objnr.
      ENDIF.
    ENDIF.

    rt_order_id = VALUE #( FOR <a> IN lt_viord ( <a>-aufnr ) ) .

  ENDMETHOD.


  METHOD tecnical_close_order.

    DATA: ls_methods   TYPE bapi_alm_order_method,
          lt_methods   TYPE STANDARD TABLE OF  bapi_alm_order_method,
          lt_header    TYPE TABLE OF bapi_alm_order_headers_i,
          lt_header_up TYPE STANDARD TABLE OF  bapi_alm_order_headers_up,
          lt_numbers   TYPE STANDARD TABLE OF bapi_alm_numbers,
          lt_return    TYPE bapiret2_t.

    DATA: ls_syststat     TYPE   bapi2080_notsti,
          ls_systemstatus TYPE bapi2080_notadt-systatus,
          ls_userstatus   TYPE   bapi2080_notadt-usrstatus,
          lt_return_notif TYPE STANDARD TABLE OF  bapiret2.

**********************************************************************
* DATOS GENERALES DE LA ORDEN
**********************************************************************

    ls_methods-refnumber    = '000001'.
    ls_methods-objecttype   = mc_bapi_ord_obj_type-header.
    ls_methods-method       = mc_bapi_ord_method-technical_complete.
    ls_methods-objectkey    = is_order-order.
    APPEND ls_methods TO lt_methods.
    CLEAR ls_methods.
    ls_methods-objectkey    = is_order-order.
    ls_methods-refnumber    = '000001'.
    ls_methods-method       = mc_bapi_ord_method-save.
    APPEND ls_methods TO lt_methods.

    lt_header = VALUE #( ( orderid = is_order-order ) ).
    lt_header_up = VALUE #( ( orderid = is_order-order ) ).

**********************************************************************
* ACTUALIZAMOS LA ORDEN
**********************************************************************
    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
      TABLES
        it_methods   = lt_methods
        it_header    = lt_header
        it_header_up = lt_header_up
        et_numbers   = lt_numbers
        return       = lt_return.

    READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<ls_return>) WITH KEY type = zif_pm_data=>cs_msg_type-error.
    IF sy-subrc NE 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      APPEND INITIAL LINE TO rt_return ASSIGNING FIELD-SYMBOL(<ls_add_return>).
      <ls_add_return>-type = <ls_return>-type.
      MESSAGE ID <ls_return>-id TYPE <ls_return>-type NUMBER <ls_return>-number WITH <ls_return>-message_v1 <ls_return>-message_v2
               <ls_return>-message_v3 <ls_return>-message_v4 INTO <ls_return>-message.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ENDIF.

    CHECK is_order-notification_id IS NOT INITIAL.

    "En caso de que la orden venga de un aviso, cerramos la orden también
    ls_syststat-langu    = sy-langu.
    ls_syststat-languiso = sy-langu.
    ls_syststat-refdate  = sy-datum.
    ls_syststat-reftime  = sy-uzeit.

    CALL FUNCTION 'BAPI_ALM_NOTIF_CLOSE'
      EXPORTING
        number       = is_order-notification_id
        syststat     = ls_syststat
      IMPORTING
        systemstatus = ls_systemstatus
        userstatus   = ls_userstatus
      TABLES
        return       = lt_return_notif.

    READ TABLE lt_return_notif ASSIGNING <ls_return> WITH KEY type = zif_pm_data=>cs_msg_type-error.
    IF sy-subrc NE 0.

      APPEND INITIAL LINE TO rt_return ASSIGNING <ls_add_return>.
      <ls_add_return>-type = zif_pm_data=>cs_msg_type-success.
      MESSAGE ID zif_pm_data=>cs_message_id TYPE zif_pm_data=>cs_msg_type-success NUMBER 006 INTO <ls_add_return>-message.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ELSE.
      APPEND INITIAL LINE TO rt_return ASSIGNING <ls_add_return>.
      <ls_add_return>-type = <ls_return>-type.
      MESSAGE ID <ls_return>-id TYPE <ls_return>-type NUMBER <ls_return>-number WITH <ls_return>-message_v1 <ls_return>-message_v2
               <ls_return>-message_v3 <ls_return>-message_v4 INTO <ls_add_return>-message.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .

    ENDIF.

  ENDMETHOD.
ENDCLASS.
