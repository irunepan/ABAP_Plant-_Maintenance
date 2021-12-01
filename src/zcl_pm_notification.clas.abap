class ZCL_PM_NOTIFICATION definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_notification_header,
        notification_id         TYPE qmnum,
        objnr                   TYPE objnr,
        notif_short_desc        TYPE string,
        notif_long_desc         TYPE string,
        notification_class      TYPE qmart,
        notification_class_desc TYPE string,
        placement               TYPE iloan,
        placement_desc          TYPE string,
        equipment               TYPE equnr,
        equipment_desc          TYPE string,
        tec_location            TYPE tplnr,
        tec_location_desc       TYPE string,
        production_stop         TYPE msaus, "Parada de produccion
        production_stop_yes     TYPE msaus,
        production_stop_no      TYPE msaus,
        priority                TYPE priok,
        priority_desc           TYPE string,
        order                   TYPE aufnr,

*       Datos codificacion
        codification_catalog    TYPE qmkat,
        codification_group      TYPE qmgrp,
        codification_code       TYPE qmcod,
        codification_desc       TYPE string,
        center                  TYPE swerk,

*       Datos puesto trabajo
        job_id                  TYPE cr_objid, "Pto trab responsable
        job                     TYPE arbpl, "Pto trab responsable
        job_desc                TYPE string,

*       Fechas
        notif_time              TYPE mzeit, "Hora del aviso
        notif_date              TYPE qmdat,  "Fecha del aviso
        desired_start_date      TYPE  strmn, "Fecha de inicio deseada
        desired_start_time      TYPE strur,  "Hora de inicio deseada
        desired_end_date        TYPE ltrmn,  "Fecha de fin deseada
        desired_end_time        TYPE ltrur, "Hora de fin deseada

*       Usuario
        notif_user              TYPE qmnam,  "Nombre autor aviso
        notif_user_name         TYPE string,  "Nombre autor aviso
      END OF ts_notification_header .
  types:
    tt_notification_header TYPE STANDARD TABLE OF ts_notification_header WITH DEFAULT KEY .
  types:
    tt_notification_id TYPE STANDARD TABLE OF qmnum WITH DEFAULT KEY .

  constants MC_FRONT_STATUS_TYPE type ZPM_E_STATUS_TYPE value 'NOT' ##NO_TEXT.
  constants:
    BEGIN OF ms_back_status,
        meab       TYPE j_status VALUE 'I0068', "MEAB
        metr       TYPE j_status VALUE 'I0070', "METR "No esta claro si se usara
        oras       TYPE j_status VALUE 'I0071', "ORAS
        mece       TYPE j_status VALUE 'I0072', "MECE
      END OF ms_back_status .
  constants:
    BEGIN OF ms_front_status,
        pending         TYPE zpm_e_status VALUE 'ST01',
        with_order      TYPE zpm_e_status VALUE 'ST02',
        pending_release TYPE zpm_e_status VALUE 'ST03',
        released        type zpm_e_status value 'ST04',
        completed       TYPE zpm_e_status VALUE 'ST05',
      END OF ms_front_status .

  methods CONSTRUCTOR
    importing
      !IV_LANGU type LANGU default SY-LANGU .
  methods SEARCH_NOTIFICATIONS
    importing
      !IT_NOTIF_ID type STANDARD TABLE optional
      !IT_NOTIF_STATUS type STANDARD TABLE optional
      !IT_PLACEMENT type STANDARD TABLE optional
      !IT_TEC_LOCATION type STANDARD TABLE optional
      !IT_EQUIPMENT type STANDARD TABLE optional
      !IT_JOB type STANDARD TABLE optional
      !IT_USER_CREATION type STANDARD TABLE optional
      !IT_DATE_CREATION type STANDARD TABLE optional
      !IT_CENTER type STANDARD TABLE optional
    returning
      value(RT_NOTIFICATION_ID) type TT_NOTIFICATION_ID .
  methods GET_HEADER_DATA
    importing
      !IR_NOTIFICATION_ID type STANDARD TABLE
      !IV_COMPLETE_DATA type ABAP_BOOL default ABAP_TRUE
    returning
      value(ET_NOTIFICATION_HEADER) type TT_NOTIFICATION_HEADER .
  methods SAVE_NOTIF
    importing
      !IS_NOTIF type ZCL_PM_NOTIFICATION=>TS_NOTIFICATION_HEADER
    exporting
      !ET_RETURN type BAPIRET2_T
      !EV_NOTIFICATION_ID type QMNUM .
  methods READ_NOTIF_LONG_DESCR
    importing
      !IV_NOTIF_ID type QMNUM
    changing
      !CV_NOTIF_LONG_DESC type STRING .
protected section.

  data MV_LANGU type LANGU .
  data MT_NOTIFICATION_HEADER type TT_NOTIFICATION_HEADER .

  methods COMPLETE_HEADER_DATA .
  methods DELETE_IRRELEVANT_STATUS
    changing
      !CT_STATUS type ZCL_PM_MASTERDATA=>TT_OBJECT_STATUS .
private section.

  constants CV_NOTIF_GRP_COD type QMGRP value 'ZCP' ##NO_TEXT.
  constants CV_NOTIF_COD type QMCOD value '00' ##NO_TEXT.
  constants CV_NOTIF_SYMP_GRP_COD type FEGRP value 'ZCP' ##NO_TEXT.
  constants CV_NOTIF_SYMP_COD type FECOD value '14' ##NO_TEXT.
  constants CV_NOTIF_PRIORITY type PRIOK value '0' ##NO_TEXT.

  methods SAVE_NOTIF_LONG_DESCR
    importing
      !IV_NEW_NOTIF type ABAP_BOOL
      !IV_NOTIF_ID type QMNUM
      !IV_DESCR type STRING .
ENDCLASS.



CLASS ZCL_PM_NOTIFICATION IMPLEMENTATION.


  METHOD complete_header_data.

*   Se completan las descripciones de: Ubicacion tecnica, emplazamiento, prioridad, equipo, pto. trab. res

    DATA: lr_priok                TYPE RANGE OF priok,
          lr_tec_location         TYPE RANGE OF tplnr,
          lr_center               TYPE RANGE OF swerk,
          lr_placement            TYPE RANGE OF stort_t499s,
          lr_equipment            TYPE RANGE OF equnr,
          lr_job_id               TYPE RANGE OF cr_objid,
          lr_codification_catalog TYPE RANGE OF qmkat,
          lr_user                 type range of uname.

*   Se generan rangos para buscar las descripciones
*   Centros
    lr_center = VALUE #( FOR <a> IN mt_notification_header ( sign = 'I' option = 'EQ' low = <a>-center ) ).
    SORT lr_center DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lr_center.

    lr_tec_location = VALUE #( FOR <a> IN mt_notification_header ( sign = 'I' option = 'EQ' low = <a>-tec_location ) ).
    SORT lr_tec_location DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lr_tec_location.

    lr_placement = VALUE #( FOR <a> IN mt_notification_header ( sign = 'I' option = 'EQ' low = <a>-placement ) ).
    SORT lr_placement DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lr_placement.

    lr_equipment = VALUE #( FOR <a> IN mt_notification_header ( sign = 'I' option = 'EQ' low = <a>-equipment ) ).
    SORT lr_equipment DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lr_equipment.

    lr_job_id = VALUE #( FOR <a> IN mt_notification_header ( sign = 'I' option = 'EQ' low = <a>-job_id ) ).
    SORT lr_job_id DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lr_job_id.

    lr_codification_catalog  = VALUE #( FOR <a> IN mt_notification_header ( sign = 'I' option = 'EQ' low = <a>-codification_catalog ) ).
    SORT lr_codification_catalog DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lr_codification_catalog.

    lr_user  = VALUE #( FOR <a> IN mt_notification_header ( sign = 'I' option = 'EQ' low = <a>-notif_user ) ).
    SORT lr_user DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lr_user.

*   Se recuperan los maestros

    DATA(lt_tec_location) = zcl_pm_masterdata=>get_tecnical_location( it_tec_location = lr_tec_location ).

    DATA(lt_priority) = zcl_pm_masterdata=>get_priorities( ).

    DATA(lt_placement) = zcl_pm_masterdata=>get_placements( EXPORTING it_center = lr_center it_placement = lr_placement ).

    DATA(lt_equipment) = NEW zcl_pm_equipment( )->get_header_data( EXPORTING it_equipment = lr_equipment ).

    DATA(lt_job) = zcl_pm_masterdata=>get_jobs( EXPORTING it_job_id = lr_job_id
                                                          it_center = lr_center ).

    DATA(lt_codification) =  zcl_pm_masterdata=>get_codification( EXPORTING it_codification_catalog = lr_codification_catalog ).

    DATA(lt_notification_class) =  zcl_pm_masterdata=>get_notification_class( ).

    DATA(lt_username) =  zcl_pm_masterdata=>get_user_name( lr_user ).

*   Se pasan a la tabla de cabecera
    LOOP AT mt_notification_header ASSIGNING FIELD-SYMBOL(<ls_notitication_header>).
      READ TABLE lt_tec_location ASSIGNING FIELD-SYMBOL(<ls_tec_location>) WITH KEY tec_location = <ls_notitication_header>-tec_location.
      IF sy-subrc IS INITIAL.
        <ls_notitication_header>-tec_location_desc = <ls_tec_location>-tec_location_desc.
      ENDIF.

      READ TABLE lt_priority ASSIGNING FIELD-SYMBOL(<ls_priority>) WITH KEY priority = <ls_notitication_header>-priority.
      IF sy-subrc IS INITIAL.
        <ls_notitication_header>-priority_desc = <ls_priority>-priority_desc.
      ENDIF.

      READ TABLE lt_placement ASSIGNING FIELD-SYMBOL(<ls_placement>) WITH KEY placement = <ls_notitication_header>-placement
                                                                             center = <ls_notitication_header>-center.
      IF sy-subrc IS INITIAL.
        <ls_notitication_header>-placement_desc = <ls_placement>-placement_desc.
      ENDIF.

      READ TABLE lt_equipment ASSIGNING FIELD-SYMBOL(<ls_equipment>) WITH KEY equipment = <ls_notitication_header>-equipment.
      IF sy-subrc IS INITIAL.
        <ls_notitication_header>-equipment_desc = <ls_equipment>-equipment_desc.
      ENDIF.

      READ TABLE lt_job ASSIGNING FIELD-SYMBOL(<ls_job>) WITH KEY center = <ls_notitication_header>-center
                                                                  job_id = <ls_notitication_header>-job_id.
      IF sy-subrc IS INITIAL.
        <ls_notitication_header>-job = <ls_job>-job.
        <ls_notitication_header>-job_desc = <ls_job>-job_desc.
      ENDIF.

      READ TABLE lt_codification ASSIGNING FIELD-SYMBOL(<ls_codification>) WITH KEY codification_catalog = <ls_notitication_header>-codification_catalog
                                                                                    codification_code = <ls_notitication_header>-codification_code.
      IF sy-subrc IS INITIAL.
        <ls_notitication_header>-codification_desc = <ls_codification>-codification_desc.
      ENDIF.

      READ TABLE lt_notification_class ASSIGNING FIELD-SYMBOL(<ls_notification_class>) WITH KEY notification_class = <ls_notitication_header>-notification_class.
      IF sy-subrc IS INITIAL.
        <ls_notitication_header>-notification_class_desc = <ls_notification_class>-notification_class_desc.
      ENDIF.

      READ TABLE lt_username ASSIGNING FIELD-SYMBOL(<ls_username>) WITH KEY user = <ls_notitication_header>-notif_user.
      IF sy-subrc IS INITIAL.
        <ls_notitication_header>-notif_user_name = <ls_username>-username.
      ENDIF.

    ENDLOOP.
*    mt_notification_header

*
*    descripcion QMART
*     Descripcion del equipo
*    Descripcion del puesto de trabajo
*    descripcion de codificacion
  ENDMETHOD.


  METHOD constructor.
    mv_langu = sy-langu.
  ENDMETHOD.


  METHOD delete_irrelevant_status.

*  Un aviso puede tener varios estados activos al mismo tiempo.
* Por ejemplo un aviso en estado MECE ( Cerrado ) aun puede tener el estado ORAS ( Orden asignada )
* En este metodo se limpian los estados que ya no son relevantes

    DATA: lr_objnr TYPE RANGE OF objnr.

* Se buscan todos los avisos cerrados
    lr_objnr = VALUE #( FOR <a> IN ct_status WHERE ( status = ms_back_status-mece ) ( sign = 'I' option = 'EQ' low = <a>-objnr ) ).

    IF lr_objnr IS NOT INITIAL.
* Se borran todos los estados ORAS para avisos cerrados
      LOOP AT ct_status INTO DATA(ls_dummy) WHERE objnr IN lr_objnr AND status = ms_back_status-oras.
        DELETE ct_status.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD get_header_data.

    DATA: lr_qmnum TYPE RANGE OF qmnum.

    CHECK ir_notification_id IS NOT INITIAL.

    lr_qmnum = CORRESPONDING #( ir_notification_id ).

*   Datos de cabecera
    SELECT qmnum AS notification_id, objnr, qmtxt AS notif_short_desc, qmart AS notification_class, equnr AS equipment, tplnr AS tec_location, arbpl AS job_id,
           msaus AS production_stop, priok AS priority, mzeit AS notif_time, qmdat AS notif_date, qmnam AS notif_user, swerk AS center,
           strmn AS desired_start_date,  strur AS desired_start_time, ltrmn AS desired_end_date, ltrur AS desired_end_time, qmgrp AS codification_group, qmcod AS codification_code,
           stort AS placement, qmkat AS codification_catalog, aufnr AS order
      FROM viqmel
      INTO CORRESPONDING FIELDS OF TABLE @mt_notification_header
      WHERE qmnum IN @lr_qmnum.

*   Completa datos de cabecera que no estan en la vista principal de avisos (Descripciones etc...)
    IF iv_complete_data EQ abap_true.
      complete_header_data( ).
    ENDIF.

    et_notification_header = CORRESPONDING #( mt_notification_header ).

  ENDMETHOD.


  METHOD read_notif_long_descr.

    DATA ls_thead TYPE thead.
    DATA it_text   TYPE STANDARD TABLE OF string.
    DATA ls_header TYPE thead.
    DATA lt_lines TYPE TABLE OF tline.

    ls_thead-tdid     = 'LTXT'.
    ls_thead-tdspras    =  sy-langu.
    ls_thead-tdname     = iv_notif_id.
    ls_thead-tdobject   = 'QMEL'.
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


  METHOD save_notif.

    DATA ls_notifheader   TYPE bapi2080_nothdri.
    DATA ls_notifheader_x TYPE bapi2080_nothdri_x.
    DATA ls_hexport TYPE bapi2080_nothdre.
    DATA lt_notitem TYPE STANDARD TABLE OF bapi2080_notitemi.
    DATA lt_notitem_add TYPE STANDARD TABLE OF bapi2080_notitemi.
    DATA lt_notitemx TYPE STANDARD TABLE OF bapi2080_notitemi_x.
    DATA lv_text TYPE string.
    DATA ls_lines  TYPE tline.
    DATA lt_return TYPE bapiret2_t.
    DATA ls_header TYPE thead.
    DATA lt_lines TYPE TABLE OF tline.
    DATA lv_tplnr TYPE tplnr.
    DATA lv_qmunm TYPE qmnum.

    " Ubicación
    " ¿Tranformar?
*    CALL FUNCTION 'CONVERSION_EXIT_TPLNR_INPUT'
*      EXPORTING
*        input  = is_notif-tec_location
*      IMPORTING
*        output = lv_tplnr.
    ls_notifheader-funct_loc = is_notif-tec_location.
    ls_notifheader_x-funct_loc = abap_true.
    " Equipo
    ls_notifheader-equipment = is_notif-equipment.
    ls_notifheader_x-equipment = abap_true.
    " Emplazamiento
    ls_notifheader-maintloc = is_notif-placement.
    ls_notifheader_x-maintloc = abap_true.
    " Parada
    ls_notifheader-breakdown = is_notif-production_stop.
    ls_notifheader_x-breakdown = abap_true.
*    " Pto. tbjo. resp & Centro
    ls_notifheader-maintplant = is_notif-center.
    ls_notifheader_x-maintplant = abap_true.
    " Usuario creados
    ls_notifheader-reportedby = is_notif-notif_user.
    ls_notifheader_x-reportedby = abap_true.

*    ls_notifheader-planplant = is_notif-center.
*    ls_notifheader_x-planplant = abap_true.
    SELECT SINGLE objid FROM crhd INTO @DATA(lv_pm_wkctr) WHERE arbpl = @is_notif-job AND werks = @is_notif-center.
    ls_notifheader-pm_wkctr = lv_pm_wkctr.
    ls_notifheader_x-pm_wkctr = abap_true.
    " Fecha inicio deseado
    ls_notifheader-desstdate = is_notif-desired_start_date.
    ls_notifheader_x-desstdate = abap_true.
    " Fecha fin deseado
    ls_notifheader-desenddate = is_notif-desired_end_date.
    ls_notifheader_x-desenddate = abap_true.
    " Prioridad
***    IF is_notif-priority IS INITIAL.
***      ls_notifheader-priority = cv_notif_priority ."is_notif-priority.
***      ls_notifheader_x-priority = abap_true.
***    ENDIF.
    ls_notifheader-refobjectkey = is_notif-notification_id.
    " Descripción aviso
    ls_notifheader-short_text = is_notif-notif_short_desc.
    ls_notifheader_x-short_text = abap_true.

    " Campos para la creación de nuevos avisos
*    ls_notifheader-division    = "".
*    ls_notifheader-sales_org   = "".
*    ls_notifheader-distr_chan  = "".
*    ls_notifheader-reportedby  = sy-uname.
    ls_notifheader-code_group  = cv_notif_grp_cod.
    ls_notifheader-coding  = cv_notif_cod.


*    ls_notifheader_x-division    = abap_true.
*    ls_notifheader_x-sales_org   = abap_true.
*    ls_notifheader_x-distr_chan  = abap_true.

    IF is_notif-notification_id IS INITIAL.
      APPEND INITIAL LINE TO lt_notitem ASSIGNING FIELD-SYMBOL(<fs_item>).
*      <fs_item>-refobjectkey  = <notif>-qmnum.
      <fs_item>-item_key       = '0001'.
      <fs_item>-item_sort_no   = '0001'.
      <fs_item>-d_codegrp      = cv_notif_symp_grp_cod.
      <fs_item>-d_code         = cv_notif_symp_cod.
*      <fs_item>-descript       = <notif>-fetxt.
*      <fs_item>-dl_codegrp     = <notif>-otgrp.
*      <fs_item>-dl_code        = <notif>-oteil.

      APPEND INITIAL LINE TO lt_notitemx ASSIGNING FIELD-SYMBOL(<fs_itemx>).
      <fs_itemx>-item_key       = '0001'.
      <fs_itemx>-item_sort_no   = 'X'.
      <fs_itemx>-d_codegrp      = 'X'.
      <fs_itemx>-d_code         = 'X'.
*      <fs_itemx>-descript       = 'X'.
*      <fs_itemx>-dl_codegrp     = 'X'.
*      <fs_itemx>-dl_code        = 'X'.
    ENDIF.

    IF is_notif-notification_id IS NOT INITIAL.
      " Modificación del aviso
      CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY'
        EXPORTING
          number        = is_notif-notification_id
          notifheader   = ls_notifheader
          notifheader_x = ls_notifheader_x
        TABLES
          return        = et_return.
*    Si todo va bien bapi no devuelve mensajes
      IF et_return IS INITIAL.
        APPEND INITIAL LINE TO et_return ASSIGNING FIELD-SYMBOL(<ls_return>).
        <ls_return>-type = zif_pm_data=>cs_msg_type-success.
        MESSAGE ID zif_pm_data=>cs_message_id TYPE zif_pm_data=>cs_msg_type-success NUMBER 001 INTO <ls_return>-message.
      ENDIF.

      lv_qmunm = is_notif-notification_id.
    ELSE.
      " Creación del aviso
      CALL FUNCTION 'BAPI_ALM_NOTIF_CREATE'
        EXPORTING
          notif_type         = is_notif-notification_class
          notifheader        = ls_notifheader
        IMPORTING
          notifheader_export = ls_hexport
        TABLES
          notitem            = lt_notitem
          return             = et_return.
      lv_qmunm = ls_hexport-notif_no.
    ENDIF.

    READ TABLE et_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      DATA(lv_error) = abap_true.
    ENDIF.
    IF lv_error = abap_false.
      CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
        EXPORTING
          number      = lv_qmunm
        IMPORTING
          notifheader = ls_hexport.

*     Si no viene numero de aviso por cabecera ls_hexport si tiene, se ha creado el aviso
      IF is_notif-notification_id IS INITIAL AND ls_hexport-notif_no IS NOT INITIAL.
        APPEND INITIAL LINE TO et_return ASSIGNING <ls_return>.
        <ls_return>-type = zif_pm_data=>cs_msg_type-success.
        MESSAGE ID zif_pm_data=>cs_message_id TYPE zif_pm_data=>cs_msg_type-success NUMBER 002 WITH ls_hexport-notif_no INTO <ls_return>-message.
      ENDIF.

      ev_notification_id = COND #( WHEN ls_hexport-notif_no IS NOT INITIAL THEN ls_hexport-notif_no
                                   ELSE is_notif-notification_id ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      " Guardamos el comentario largo
*      DATA(lv_length) = strlen( is_notif-notif_long_desc ).
*      IF lv_length GT 40.
      DATA(lv_new_notif) = COND #( WHEN is_notif-notification_id IS INITIAL THEN abap_true
                                   ELSE abap_false ).
      save_notif_long_descr(  EXPORTING  iv_new_notif = lv_new_notif iv_notif_id =  ev_notification_id iv_descr    =  is_notif-notif_long_desc   ).
*      ENDIF.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ENDIF.

  ENDMETHOD.


  METHOD save_notif_long_descr.

    DATA it_text   TYPE STANDARD TABLE OF string.
    DATA ls_header TYPE thead.
    DATA lt_lines TYPE TABLE OF tline.
    DATA lt_lines_aux TYPE TABLE OF tline.

    " Recuperamos el idioma de la descripción larga del aviso
    SELECT SINGLE kzmla FROM qmel INTO @DATA(lv_langu) WHERE qmnum EQ @iv_notif_id.

    IF lv_langu IS INITIAL.
      lv_langu = sy-langu.
    ENDIF.

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

*     Para creaciones
    IF iv_new_notif IS NOT INITIAL.
      CALL FUNCTION 'IQS0_ADD_NOTIFICATION_LONGTEXT'
        EXPORTING
          i_qmnum       = iv_notif_id
          i_post        = 'X'
        TABLES
          t_inlines     = lt_lines
        EXCEPTIONS
          show_messages = 1
          OTHERS        = 2.
    ENDIF.

    " Para modificaciones
    ls_header-mandt = sy-mandt.
    ls_header-tdid = 'LTXT'.
    ls_header-tdspras = lv_langu.
    ls_header-tdname = iv_notif_id.
    ls_header-tdobject = 'QMEL'.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header          = ls_header
        savemode_direct = abap_true
      TABLES
        lines           = lt_lines_aux
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.


  ENDMETHOD.


  METHOD search_notifications.

    DATA: lr_qmnum  TYPE RANGE OF qmnum,
          lr_status TYPE RANGE OF j_status,
          lr_stort  TYPE RANGE OF stort,
          lr_equnr  TYPE RANGE OF equnr,
          lr_lgwid  TYPE RANGE OF lgwid,
          lr_arbpl  TYPE RANGE OF arbpl,
          lr_tplnr  TYPE RANGE OF tplnr,
          lr_ernam  TYPE RANGE OF ernam,
          lr_erdat  TYPE RANGE OF erdat,
          lr_objnr  TYPE RANGE OF j_objnr,
          lr_center TYPE RANGE OF werks_d,
          lr_iloan  TYPE RANGE OF iloan,
          lr_objid  TYPE RANGE OF cr_objid.

    lr_qmnum = CORRESPONDING #( it_notif_id ).
    lr_ernam  = CORRESPONDING #( it_user_creation ).
    lr_erdat  = CORRESPONDING #( it_date_creation ).
    lr_status    = CORRESPONDING #( it_notif_status ).

    lr_arbpl = CORRESPONDING #( it_job ).
    lr_stort = CORRESPONDING #( it_placement ).
    lr_tplnr = CORRESPONDING #( it_tec_location ).
    lr_equnr = CORRESPONDING #( it_equipment ).
    lr_center = CORRESPONDING #( it_center ).

*   Se filtran los avisos por los campos de cabecera
    SELECT qmnum, objnr, iloan, arbpl
      FROM viqmel
      INTO TABLE @DATA(lt_qmel)
      WHERE qmnum IN @lr_qmnum AND
            iloan IN @lr_iloan AND
            equnr IN @lr_equnr AND
            swerk IN @lr_center AND
            erdat IN @lr_erdat AND
            qmnam IN @lr_ernam.

    CHECK lt_qmel IS NOT INITIAL.

*   Si se busca por estado se filtran los avisos
    IF lr_status IS NOT INITIAL.

      lr_objnr = VALUE #( FOR <a> IN lt_qmel ( sign = 'I' option = 'EQ' low = <a>-objnr ) ).

*     Recupera todos los estados activos de los avisos
      DATA(lt_jest) = zcl_pm_masterdata=>get_object_status( lr_objnr ).

*     Se limpian los estados para tener solo los relevantes de cada aviso
      delete_irrelevant_status( CHANGING ct_status = lt_jest ).

*     Borra todos los estados de los avisos que no coincidan con los que vienen por parametro
      DELETE lt_jest WHERE status NOT IN lr_status.





*     Rango con los avisos cuyos estados coinciden con los que vienen por parametro
      lr_objnr = VALUE #( FOR <b> IN lt_jest ( sign = 'I' option = 'EQ' low = <b>-objnr ) ).
      DELETE lt_qmel WHERE objnr NOT IN lr_objnr.

*     Si no queda ningun aviso sale de la busqueda
      CHECK lt_qmel IS NOT INITIAL.

    ENDIF.

*   Si se busca por emplazamiento se busca filtran los avisos
    IF lr_tplnr IS NOT INITIAL OR
       lr_stort IS NOT INITIAL.

*     Rango con ubicaciones de los avisso encontrados
      lr_iloan = VALUE #( FOR <a> IN lt_qmel ( sign = 'I' option = 'EQ' low = <a>-iloan ) ).
      SORT lr_iloan DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lr_iloan.

*     Se buscan para las ubicaciones de los avisos, los que tienen ese emplazamiento
      SELECT iloan
        FROM iloa
        INTO TABLE @DATA(lt_iloa)
        WHERE iloan IN @lr_iloan AND
              stort IN @lr_stort AND
              tplnr IN @lr_tplnr.

      IF sy-subrc IS NOT INITIAL.
*       Si no encuentra ninguna ubicacion se descartan todos los avisos. (Con un rango vacio no funciona NOT IN)
        CLEAR lt_qmel[].
      ELSE.
*     Nuevo rango para filtrar
        lr_iloan = VALUE #( FOR <c> IN lt_iloa ( sign = 'I' option = 'EQ' low = <c>-iloan ) ).
*     Se borran los avisos que no estan en las ubicaciones con el emplazamiento correcto
        DELETE lt_qmel WHERE iloan NOT IN lr_iloan.
      ENDIF.



*     Si no queda ningun aviso sale de la busqueda
      CHECK lt_qmel IS NOT INITIAL.

    ENDIF.

*   Si se busca por puesto de trabajo se filtran los datos
    IF lr_arbpl IS NOT INITIAL.

*     Puede ser lioso pero el campo ARBPL de la cabecera del aviso es en realidad el OBJID del puesto de trabajo
*     En la tabla de puestos de trabajo ARBPL es el puesto en si
      lr_objid = VALUE #( FOR <a> IN lt_qmel ( sign = 'I' option = 'EQ' low = <a>-arbpl ) ).
      SORT lr_objid DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lr_objid.

*     Se buscan los puestos de trabajo de los avisos preseleccionados que coincidan con los que se buscan
      SELECT objid
        FROM crhd
        INTO TABLE @DATA(lt_crhd)
        WHERE objty EQ @zcl_pm_masterdata=>mc_job_type AND
              objid IN @lr_objid AND
              werks IN @lr_center AND
              arbpl IN @lr_arbpl AND
              verwe EQ @zcl_pm_masterdata=>mc_job_class.

      IF sy-subrc IS NOT INITIAL.
*       Si no encuentra ninguna ubicacion se descartan todos los avisos. (Con un rango vacio no funciona NOT IN)
        CLEAR lt_qmel[].
      ELSE.
        lr_objid = VALUE #( FOR <e> IN lt_crhd ( sign = 'I' option = 'EQ' low = <e>-objid ) ).
*       Se borran los avisos que no tienen el ID del puesto de trabajo
        DELETE lt_qmel WHERE arbpl NOT IN lr_objid.
      ENDIF.
    ENDIF.

    rt_notification_id = VALUE #( FOR <a> IN lt_qmel ( <a>-qmnum ) ) .

  ENDMETHOD.
ENDCLASS.
