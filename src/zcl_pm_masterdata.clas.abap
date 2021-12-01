class ZCL_PM_MASTERDATA definition
  public
  final
  create public .

public section.

  types:
    tr_arbpl TYPE RANGE OF arbpl .
  types:
    BEGIN OF ts_codification,
        codification_catalog TYPE qmkat,
        codification_group   TYPE qmgrp,
        codification_code    TYPE qmcod,
        codification_desc    TYPE qtxt_code,
      END OF ts_codification .
  types:
    tt_codification TYPE STANDARD TABLE OF ts_codification WITH DEFAULT KEY .
  types:
    BEGIN OF ts_tecnical_location,
        center            TYPE werks_d,
        tec_location      TYPE tplnr,
        tec_location_desc TYPE pltxt,
      END OF ts_tecnical_location .
  types:
    tt_tecnical_location TYPE STANDARD TABLE OF ts_tecnical_location WITH DEFAULT KEY .
  types:
    BEGIN OF ts_priority,
        priority      TYPE priok,
        priority_desc TYPE priokx,
      END OF ts_priority .
  types:
    tt_priority TYPE STANDARD TABLE OF ts_priority WITH DEFAULT KEY .
  types:
    BEGIN OF ts_job,
        objty    TYPE cr_objty,
        job_id   TYPE cr_objid,
        center   TYPE werks_d,
        job      TYPE arbpl,
        job_desc TYPE string,
      END OF ts_job .
  types:
    tt_job TYPE STANDARD TABLE OF ts_job WITH DEFAULT KEY .
  types:
    tt_center TYPE STANDARD TABLE OF werks_d WITH DEFAULT KEY .
  types:
    tt_front_status TYPE STANDARD TABLE OF zpm_t_status WITH DEFAULT KEY .
  types:
    BEGIN OF ts_object_status,
        objnr      TYPE j_objnr,
        status     TYPE j_status,
        inactive   TYPE j_inact,
        change_num TYPE j_chgnr,
      END OF ts_object_status .
  types:
    tt_object_status TYPE STANDARD TABLE OF ts_object_status WITH DEFAULT KEY .
  types:
    BEGIN OF ts_placement,
        center         TYPE werks_d,
        placement      TYPE stort_t499s,
        placement_desc TYPE text40,
      END OF ts_placement .
  types:
    tt_placement TYPE STANDARD TABLE OF ts_placement WITH DEFAULT KEY .
  types:
    BEGIN OF ts_notification_class,
        notification_class      TYPE qmart,
        notification_class_desc TYPE qmartx,
      END OF ts_notification_class .
  types:
    tt_notification_class TYPE STANDARD TABLE OF ts_notification_class WITH DEFAULT KEY .
  types:
    BEGIN OF ts_status_conversion,
        status_back  TYPE j_status,
        status_front TYPE zpm_e_status,
      END OF ts_status_conversion .
  types:
    tt_status_conversion TYPE STANDARD TABLE OF ts_status_conversion WITH DEFAULT KEY .
  types:
    BEGIN OF ts_username,
        user     TYPE uname,
        username TYPE ad_namtext,
      END OF ts_username .
  types:
    tt_username TYPE STANDARD TABLE OF ts_username WITH DEFAULT KEY .
  types:
    BEGIN OF ts_employee_name,
        employee_number TYPE pernr_d,
        employee_name   TYPE string,
      END OF ts_employee_name .
  types:
    tt_employee_name TYPE STANDARD TABLE OF ts_employee_name WITH DEFAULT KEY .
  types:
    BEGIN OF ts_technical_hierarchy,
        node_id        TYPE string,
        node_parent_id TYPE string,
        node_type      TYPE string, "Equipo o ub. tecnica
        node_desc      TYPE string,
        highlight      TYPE string,
      END OF ts_technical_hierarchy .
  types:
    tt_technical_hierarchy TYPE STANDARD TABLE OF ts_technical_hierarchy WITH DEFAULT KEY .
  types:
    BEGIN OF ts_employee_from_job,
        job_id     TYPE objid,
        pernr      TYPE pernr_d,
        short_name TYPE short_d,
        full_name  TYPE stext,
        center     TYPE werks_d,
      END OF ts_employee_from_job .
  types:
    tt_employee_from_job TYPE STANDARD TABLE OF ts_employee_from_job WITH DEFAULT KEY .
  types:
    BEGIN OF ts_order_class,
        class_id   TYPE aufart,
        class_desc TYPE auarttext,
      END OF ts_order_class .
  types:
    tt_order_class TYPE STANDARD TABLE OF ts_order_class WITH DEFAULT KEY .
  types:
    BEGIN OF ts_center_data,
        center      TYPE werks_d,
        center_desc TYPE string,
      END OF ts_center_data .
  types:
    tt_center_data TYPE STANDARD TABLE OF ts_center_data WITH DEFAULT KEY .
  types:
    BEGIN OF ts_warehouse_data,
        warehouse      TYPE lgort_d,
        warehouse_desc TYPE lgobe,
      END OF ts_warehouse_data .
  types:
    tt_warehouse_data TYPE STANDARD TABLE OF ts_warehouse_data WITH DEFAULT KEY .
  types:
    BEGIN OF ts_bwart,
        bwart      TYPE bwart,
        bwart_desc TYPE text60,
      END OF ts_bwart .
  types:
    tt_bwart TYPE STANDARD TABLE OF ts_bwart WITH DEFAULT KEY .
  types:
    BEGIN OF ts_kostl,
        kostl      TYPE kostl,
        kostl_desc TYPE ktext,
      END OF ts_kostl .
  types:
    tt_kostl TYPE STANDARD TABLE OF ts_kostl WITH DEFAULT KEY .

  constants MC_PRIORITY_CLASS type ARTPR value 'ZC' ##NO_TEXT.
  constants MC_JOB_CLASS type AP_VERWE value 'ZMTO' ##NO_TEXT.
  constants MC_JOB_TYPE type CR_OBJTY value 'A' ##NO_TEXT.
  constants MC_CODIFICATION_GROUP type QMGRP value 'ZCP' ##NO_TEXT.
  constants:
    BEGIN OF mc_tec_hierarchy_node,
        equipment    TYPE string VALUE 'EQUI',
        tec_location TYPE string VALUE 'TECL',
      END OF  mc_tec_hierarchy_node .
  constants:
    BEGIN OF mc_highlight_node,
        none    TYPE string VALUE 'None',
        warning TYPE string VALUE 'Warning',
        error   TYPE string VALUE 'Error',
        success TYPE string VALUE 'Success',
        inf     TYPE string VALUE 'Information',
      END OF  mc_highlight_node .
  constants MC_AUTYP_MANTENIMIENTO type AUFTYP value '30' ##NO_TEXT.

  class-methods GET_OBJECT_STATUS
    importing
      !IR_OBJNR type STANDARD TABLE
      !IV_GET_INACTIVE type ABAP_BOOL default ABAP_FALSE
    returning
      value(ET_STATUS) type TT_OBJECT_STATUS .
  class-methods GET_MASTER_FRONT_STATUS
    importing
      !IT_STATUS_TYPE type STANDARD TABLE optional
    returning
      value(RT_STATUS_FRONT) type TT_FRONT_STATUS .
  class-methods GET_ACTIVE_CENTERS
    returning
      value(RT_CENTER) type TT_CENTER .
  class-methods GET_MOVEMENTS
    importing
      !IT_BWART type STANDARD TABLE optional
    returning
      value(RT_BWART_DATA) type TT_BWART .
  class-methods GET_WAREHOUSE_DATA
    importing
      !IV_WERKS type WERKS_D
      !IT_LGORT type STANDARD TABLE optional
    returning
      value(RT_WAREHOUSE_DATA) type TT_WAREHOUSE_DATA .
  class-methods GET_CENTER_DATA
    importing
      !IT_CENTER type STANDARD TABLE optional
    returning
      value(RT_CENTER_DATA) type TT_CENTER_DATA .
  class-methods GET_JOBS
    importing
      !IT_JOB_ID type STANDARD TABLE optional
      !IT_JOB type STANDARD TABLE optional
      !IT_CENTER type STANDARD TABLE optional
      !IV_LANGU type LANGU default SY-LANGU
    returning
      value(RT_JOB) type TT_JOB .
  class-methods GET_PLACEMENTS
    importing
      !IT_CENTER type STANDARD TABLE optional
      !IT_PLACEMENT type STANDARD TABLE optional
      !IV_ADJUST_DESC type SAP_BOOL optional
    returning
      value(RT_PLACEMENT) type TT_PLACEMENT .
  class-methods GET_PRIORITIES
    importing
      !IV_LANGU type LANGU default SY-LANGU
    returning
      value(RT_PRIORITIES) type TT_PRIORITY .
  class-methods GET_TECNICAL_LOCATION
    importing
      !IV_LANGU type LANGU default SY-LANGU
      !IT_TEC_LOCATION type STANDARD TABLE optional
      !IT_CENTER type STANDARD TABLE optional
    returning
      value(RT_TECNICAL_LOCATION) type TT_TECNICAL_LOCATION .
  class-methods GET_NOTIFICATION_CLASS
    importing
      !IT_NOTIFICATION_CLASS type STANDARD TABLE optional
      !IV_LANGU type LANGU default SY-LANGU
    returning
      value(RT_NOTIFICATION_CLASS) type TT_NOTIFICATION_CLASS .
  class-methods GET_CODIFICATION
    importing
      !IT_CODIFICATION_CATALOG type STANDARD TABLE
      !IT_CODIFICATION_CODE type STANDARD TABLE optional
      !IV_LANGU type LANGU default SY-LANGU
    returning
      value(RT_CODIFICATION) type TT_CODIFICATION .
  class-methods GET_USER_NAME
    importing
      !IT_UNAME type STANDARD TABLE
    returning
      value(RT_USERNAME) type TT_USERNAME .
  class-methods GET_EMPLOYEE_NAME
    importing
      !IT_EMPLOYEE_NUMBER type STANDARD TABLE
    returning
      value(RT_EMPLOYEE_NAME) type TT_EMPLOYEE_NAME .
  class-methods GET_TECNICAL_HIERARCHY
    importing
      !IT_CENTER type STANDARD TABLE optional
      !IT_TEC_LOCATION type STANDARD TABLE optional
      !IV_INCLUDE_EQUIPMENTS type ABAP_BOOL default ABAP_FALSE
      !IV_LANGU type LANGU default SY-LANGU
    returning
      value(RT_HIERARCHY) type TT_TECHNICAL_HIERARCHY .
  class-methods GET_EMPLOYEE_FROM_JOB
    importing
      !IT_JOB_ID type STANDARD TABLE
      !IV_DATE type DATUM default SY-DATUM
    returning
      value(RT_EMPLOYEE_FROM_JOB) type TT_EMPLOYEE_FROM_JOB .
  class-methods GET_MATERIAL_DATA
    importing
      !IT_MATERIAL type STANDARD TABLE optional
    returning
      value(RT_MATERIAL) type CHAR1 .
  class-methods GET_ORDER_CLASSES
    importing
      !IV_LANGU type LANGU default SY-LANGU
    returning
      value(RT_CLASSES) type TT_ORDER_CLASS .
  class-methods GET_OUTSOURCE_JOB
    importing
      !IT_CENTER type STANDARD TABLE
    returning
      value(RT_OUTSOURCE_JOB) type TT_JOB .
  class-methods GET_CECOS
    importing
      !IV_WERKS type WERKS_D
    returning
      value(RT_KOSTL) type TT_KOSTL .
  class-methods GET_ACTIVE_JOB_BY_CENTER
    importing
      !IV_WERKS type WERKS_D
    returning
      value(RR_ACTIVE_JOBS) type TR_ARBPL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PM_MASTERDATA IMPLEMENTATION.


  METHOD get_active_centers.
    DATA: lr_center TYPE RANGE OF werks_d.
    zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = 'ACTIVE_CENTER_%'
                                                    CHANGING ct_ranges = lr_center ).

    rt_center = VALUE #( FOR <a> IN lr_center ( <a>-low ) ).

  ENDMETHOD.


  METHOD GET_ACTIVE_JOB_BY_CENTER.

    CASE iv_werks.

      WHEN zif_pm_data=>cs_center-granda.

        zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = '1001_ACTIVE_JOB_%'
                                                        CHANGING ct_ranges = rr_active_jobs ).
      WHEN zif_pm_data=>cs_center-lugo.

        zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = '1002_ACTIVE_JOB_%'
                                                CHANGING ct_ranges = rr_active_jobs ).
      WHEN zif_pm_data=>cs_center-villagarcia.

        zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = '1004_ACTIVE_JOB_%'
                                                CHANGING ct_ranges = rr_active_jobs ).

    ENDCASE.
  ENDMETHOD.


  METHOD get_cecos.

    DATA lr_kostl TYPE RANGE OF kostl.


    CASE iv_werks.

      WHEN zif_pm_data=>cs_center-granda.

        zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = '1001_KOSTL__%'
                                                        CHANGING ct_ranges = lr_kostl ).
      WHEN zif_pm_data=>cs_center-lugo.

        zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = '1002_KOSTL__%'
                                                CHANGING ct_ranges = lr_kostl ).
      WHEN zif_pm_data=>cs_center-villagarcia.

        zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = '1004_KOSTL__%'
                                                CHANGING ct_ranges = lr_kostl ).

    ENDCASE.

    SELECT a~kostl AS kostl b~ktext AS kostl_desc
      FROM csks AS a INNER JOIN cskt AS b ON a~kokrs EQ b~kokrs AND a~kostl EQ b~kostl AND a~datbi EQ b~datbi
      INTO CORRESPONDING FIELDS OF TABLE rt_kostl
      WHERE a~kostl IN lr_kostl
      AND   a~kokrs EQ zif_pm_data=>cv_kokrs_3400
      AND   a~datbi GE sy-datum
      AND   a~datab LE sy-datum
      AND   b~spras EQ sy-langu.

    SORT rt_kostl BY kostl_desc.

  ENDMETHOD.


  METHOD get_center_data.

    DATA:lr_werks TYPE RANGE OF werks_d.
    lr_werks = CORRESPONDING #( it_center ).

    SELECT werks AS center, name1 AS center_desc
      FROM t001w
      INTO CORRESPONDING FIELDS OF TABLE @rt_center_data
      WHERE werks IN @lr_werks.


  ENDMETHOD.


  METHOD get_codification.

    DATA: lr_codification_catalog TYPE RANGE OF qmkat,
*          lr_codification_group   TYPE RANGE OF qmgrp,
          lr_codification_code    TYPE RANGE OF qmcod.

*   Hay 4 catalogos de codigos ZCP

    lr_codification_catalog = CORRESPONDING #( it_codification_catalog ).
*    lr_codification_group = CORRESPONDING #(  it_codification_group ).
    lr_codification_code  = CORRESPONDING #(  it_codification_code ).

    CHECK lr_codification_catalog IS NOT INITIAL.

    SELECT katalogart AS codification_catalog
           codegruppe AS codification_group
           code AS codification_code
           kurztext AS codification_desc
      FROM qpct
      INTO CORRESPONDING FIELDS OF TABLE rt_codification
      WHERE katalogart IN lr_codification_catalog AND
*            codegruppe IN lr_codification_group AND
            codegruppe eq MC_CODIFICATION_GROUP and
            code       IN lr_codification_code AND
            sprache    EQ iv_langu.

  ENDMETHOD.


  METHOD get_employee_from_job.


    DATA: lc_objty  TYPE  cr_objty VALUE 'A'.

    DATA: lr_job_id      TYPE RANGE OF cr_objid,
          lt_in_object   TYPE TABLE OF rcrid,
          lt_out_persons TYPE TABLE OF object_person_assignment.


    CHECK iv_date IS NOT INITIAL AND it_job_id IS NOT INITIAL.

    lr_job_id = CORRESPONDING #( it_job_id ).

    lt_in_object = VALUE #( FOR <a> IN lr_job_id ( objty = lc_objty  objid = <a>-low ) ).


    CALL FUNCTION 'COI2_PERSON_OF_WORKCENTER'
      EXPORTING
        begda           = iv_date
        endda           = iv_date
      TABLES
        out_persons     = lt_out_persons
        in_object       = lt_in_object
      EXCEPTIONS
        no_person_found = 01.

    rt_employee_from_job = CORRESPONDING #( lt_out_persons MAPPING job_id = objid
                                                                   short_name = short
                                                                   full_name = stext
                                                                   center = werks ).


  ENDMETHOD.


  METHOD get_employee_name.

    DATA: lr_pernr TYPE RANGE OF pernr_d.

    CHECK it_employee_number IS NOT INITIAL.

    lr_pernr = CORRESPONDING #( it_employee_number ).

    SELECT pernr AS employee_number, vorna, nachn, nach2
      FROM pa0002
      INTO TABLE @DATA(lt_pa0002)
      WHERE pernr IN @lr_pernr.


    LOOP AT lt_pa0002 ASSIGNING FIELD-SYMBOL(<ls_pa0002>).

      APPEND INITIAL LINE TO rt_employee_name ASSIGNING FIELD-SYMBOL(<ls_employee_name>).
      <ls_employee_name> = CORRESPONDING #( <ls_pa0002> ).
      <ls_employee_name>-employee_name = |{ <ls_pa0002>-vorna } { <ls_pa0002>-nachn } { <ls_pa0002>-nach2 }|.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_jobs.

    DATA: lr_center TYPE RANGE OF werks_d,
          lr_job_id TYPE RANGE OF cr_objid,
          lr_job    TYPE RANGE OF arbpl.

    lr_center = CORRESPONDING #( it_center ).
    lr_job    = CORRESPONDING #( it_job ).
    lr_job_id = CORRESPONDING #( it_job_id ).

*   Si no se pide algun centro en concreto se recuperan los que estan activos
    IF lr_center IS INITIAL.
      DATA(lt_center) = get_active_centers( ).
      lr_center = VALUE #( FOR <a> IN lt_center ( sign = 'I' option = 'EQ' low = <a> ) ).
    ENDIF.

** En el caso de Villgarcia al ser poca gente si mezclan especialidades en las ordenes por lo que quieren el listado completo, sin filtrar por puesto de trabajo.
    READ TABLE lr_center TRANSPORTING NO FIELDS WITH KEY  low = zif_pm_data=>cs_center-villagarcia.
    IF sy-subrc EQ 0.
      CLEAR: lr_job, lr_job_id.
    ENDIF.

*   Puestos de trabajo
    SELECT objid AS job_id, werks AS center, arbpl AS job
      FROM crhd
      INTO TABLE @DATA(lt_crhd)
      WHERE objty EQ @mc_job_type AND
            objid IN @lr_job_id AND
            werks IN @lr_center AND
            arbpl IN @lr_job AND
            verwe EQ @mc_job_class.

*   Descripcion puestos de trabajo
    IF sy-subrc IS INITIAL.

      lr_job_id = VALUE #( FOR <b> IN lt_crhd ( sign = 'I' option = 'EQ' low = <b>-job_id ) ).

      SELECT *
        FROM crtx
        INTO TABLE @DATA(lt_crtx)
        WHERE objty EQ @mc_job_type AND
              objid IN @lr_job_id AND
              spras EQ @iv_langu.
      rt_job = CORRESPONDING #( lt_crhd ).

      LOOP AT rt_job ASSIGNING FIELD-SYMBOL(<ls_job>).
        ASSIGN lt_crtx[ objid = <ls_job>-job_id ] TO FIELD-SYMBOL(<ls_crtx>).
        IF <ls_crtx> IS ASSIGNED.
          <ls_job>-job_desc = <ls_crtx>-ktext.
          UNASSIGN <ls_crtx>.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_master_front_status.

    DATA: lr_status_type TYPE RANGE OF zpm_e_status_type.

    lr_status_type = CORRESPONDING #( it_status_type ).

    SELECT *
      FROM zpm_t_status
      INTO CORRESPONDING FIELDS OF TABLE rt_status_front
      WHERE status_type IN lr_status_type.

  ENDMETHOD.


  method GET_MATERIAL_DATA.
  endmethod.


  METHOD get_movements.

    DATA:lr_bwart TYPE RANGE OF bwart.
    lr_bwart = CORRESPONDING #( it_bwart ).

    SELECT bwart AS bwart, htext AS bwart_desc
      FROM t157h
      INTO CORRESPONDING FIELDS OF TABLE @rt_bwart_data
      WHERE bwart IN @lr_bwart
      AND   sobkz EQ ' '
      AND   spras EQ @sy-langu
      AND   tcode EQ @zif_pm_data=>cv_tcode_mb1a.


  ENDMETHOD.


  METHOD get_notification_class.

    DATA: lr_notification_class TYPE RANGE OF qmart.

    lr_notification_class = CORRESPONDING #( it_notification_class ).

*   Si no se busca una clase de aviso en concreto recupera los activos
    IF it_notification_class IS INITIAL.
      zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = 'ACTIVE_QMART_%'
                                                      CHANGING ct_ranges = lr_notification_class ).
    ENDIF.

    SELECT qmart AS notification_class qmartx AS notification_class_desc
      FROM tq80_t
      INTO CORRESPONDING FIELDS OF TABLE rt_notification_class
      WHERE qmart IN lr_notification_class AND
            spras EQ iv_langu.


  ENDMETHOD.


  METHOD get_object_status.

*  La traduccion a los estados que se ven en las transacciones estan en TJ20T

*  Avisos
*I0068 = MEAB
*I0070 = METR
*I0071 = ORAS
*I0072 = MECE





    DATA: lr_objnr TYPE RANGE OF j_objnr.

    lr_objnr = CORRESPONDING #( ir_objnr ).

    CHECK lr_objnr IS NOT INITIAL.

*   Recupera los estados de un objeto
    SELECT objnr stat AS status inact AS inactive chgnr AS change_num
      FROM jest
      INTO CORRESPONDING FIELDS OF TABLE et_status
      WHERE objnr IN lr_objnr AND
            inact EQ iv_get_inactive.

*   Los estados van en orden numerico, si hay mas de uno activo nos quedamos con el ultimo
    sort et_status DESCENDING by objnr status.
    delete ADJACENT DUPLICATES FROM et_status COMPARING objnr status.

  ENDMETHOD.


  METHOD get_order_classes.

    SELECT a~auart AS class_id a~txt AS class_desc
      FROM t003p AS a INNER JOIN t003o AS b ON a~auart EQ b~auart
      INTO CORRESPONDING FIELDS OF TABLE rt_classes
      WHERE a~spras EQ iv_langu AND
            a~auart LIKE 'Z%' AND
            b~autyp EQ mc_autyp_mantenimiento.


    SORT rt_classes BY class_desc.
  ENDMETHOD.


  METHOD get_outsource_job.

    DATA: lr_center     TYPE RANGE OF werks_d,
          lr_active_job TYPE RANGE OF arbpl.
*   Recupera los puestos de trabajo responsable
    lr_center = CORRESPONDING #( it_center ).



    DATA(lt_all_job) = zcl_pm_masterdata=>get_jobs( it_center = lr_center ).
*    zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = 'ACTIVE_JOB_%'
*                                                    CHANGING ct_ranges = lr_active_job ).
    READ TABLE lr_center ASSIGNING FIELD-SYMBOL(<fs_center>) INDEX 1.
    IF sy-subrc EQ 0.
      lr_active_job = zcl_pm_masterdata=>get_active_job_by_center( iv_werks = <fs_center>-low ).
    ENDIF.
    DELETE lt_all_job WHERE job IN lr_active_job.
    rt_outsource_job = CORRESPONDING #( lt_all_job ).

  ENDMETHOD.


  METHOD get_placements.

    DATA: lr_center       TYPE RANGE OF werks_d,
          lr_placement    TYPE RANGE OF stort_t499s,
          lr_placement_ex TYPE RANGE OF stort_t499s,
          lv_order        TYPE string.

    lr_center = CORRESPONDING #( it_center ).
    lr_placement = CORRESPONDING #( it_placement ).

*   Si no se busca algun centro en concreto se recuperan los que estan activos en la aplicacion
    IF lr_center IS INITIAL.
      DATA(lt_center) = get_active_centers( ).
      lr_center = VALUE #( FOR <a> IN lt_center ( sign = 'I' option = 'EQ' low = <a> ) ).
    ENDIF.

    SELECT werks AS center stand AS placement ktext AS placement_desc
      FROM t499s
      INTO CORRESPONDING FIELDS OF TABLE rt_placement
      WHERE werks IN lr_center AND
            stand IN lr_placement.

    " Para el 1001 excluímos ciertos emplazamientos.
    READ TABLE lr_center TRANSPORTING NO FIELDS WITH KEY low = '1001'.
    IF sy-subrc EQ 0.
      zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = 'PLACEMENT_EX_1001_%'
                                                      CHANGING ct_ranges = lr_placement_ex ).


      DELETE rt_placement WHERE placement IN lr_placement_ex.
      " Ordenamos y quitarmos el número
      SORT rt_placement BY placement_desc.
      IF iv_adjust_desc EQ abap_true.
        LOOP AT rt_placement ASSIGNING FIELD-SYMBOL(<fs_placement>).
          SPLIT <fs_placement> AT '-' INTO lv_order <fs_placement>-placement_desc.
          CONDENSE <fs_placement>-placement_desc.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_priorities.

    SELECT priok AS priority priokx AS priority_desc
      FROM t356_t
      INTO CORRESPONDING FIELDS OF TABLE rt_priorities
      WHERE spras EQ iv_langu AND
            artpr EQ mc_priority_class.

  ENDMETHOD.


  METHOD get_tecnical_hierarchy.

    DATA: lr_center       TYPE RANGE OF werks_d,
          lr_tec_location TYPE RANGE OF tplnr,
          lt_tplnr        TYPE TABLE OF tplnr,
          lt_iflot        TYPE TABLE OF iflot,
          lr_equnr        TYPE RANGE OF equnr.

    DATA: lr_objnr TYPE RANGE OF j_objnr.

    lr_center = CORRESPONDING #( it_center ).
    lr_tec_location = CORRESPONDING #( it_tec_location ).

    CHECK lr_center IS NOT INITIAL OR lr_tec_location IS NOT INITIAL.

*   Si no vienen ubicaciones se buscan las "padres" de los centros
    IF lr_tec_location IS INITIAL.
      SELECT tplnr
        FROM iflot
        INTO TABLE @DATA(lt_tec_loc)
        WHERE tplma EQ @space AND
              iwerk IN @lr_center.

      CHECK sy-subrc IS INITIAL.

      lt_tplnr = VALUE #( FOR <a> IN lt_tec_loc ( <a>-tplnr ) ).

*   Si viene una ubicacion se busca la jerarquia a partir de ahi
    ELSE.
      lt_tplnr = VALUE #( FOR <b> IN lr_tec_location ( <b>-low ) ).
    ENDIF.

*   Se busca la jerarquia
    CALL FUNCTION 'FUNC_LOCATION_HIERARCHY'
      EXPORTING
*       spras                  = sy-langu
*       level_limit            = '0'
*       tabstructure           = 'IFLOT'
        including_root         = 'X'
*       buffer_bypass          = ' '
* IMPORTING
*       LEVEL_FOUND            =
      TABLES
        root_tab               = lt_tplnr
        iflo_tab               = lt_iflot
      EXCEPTIONS
        root_not_found         = 1
        read_error             = 2
        structure_not_possible = 3
        OTHERS                 = 4.
    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ENDIF.

*   Se buscan las descripciones
    lr_tec_location = VALUE #( FOR <c> IN lt_iflot ( sign = 'I' option = 'EQ' low = <c>-tplnr ) ).

    SELECT tplnr AS tec_location, pltxt AS tec_location_desc
      FROM  iflotx
      INTO  TABLE @DATA(lt_iflotx)
      WHERE tplnr IN @lr_tec_location AND
            spras EQ @iv_langu.

* Recuperamos los estados de las ubicaciones
    lr_objnr = VALUE #( FOR <d> IN lt_iflot ( sign = 'I' option = 'EQ' low = <d>-objnr ) ).

    SELECT objnr, stat
      FROM jest
      INTO TABLE @DATA(lt_status)
      WHERE objnr IN @lr_objnr AND
            inact NE 'X' AND        "Santiago 19.10.21 Eliminar Status Inactivos
            ( stat EQ @zif_pm_data=>cv_ptbo_st OR   "Status Pet. Borrado
              stat EQ @zif_pm_data=>cv_ptbo_st2 ).  "Status Inactivo

*   Se completa la jerarquia con las ubicactiones tecnicas.
    LOOP AT lt_iflot ASSIGNING FIELD-SYMBOL(<ls_iflot>).
      READ TABLE lt_status TRANSPORTING NO FIELDS WITH KEY objnr = <ls_iflot>-objnr.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO rt_hierarchy ASSIGNING FIELD-SYMBOL(<ls_hierarchy>).
        <ls_hierarchy> = CORRESPONDING #( <ls_iflot> MAPPING node_id = tplnr node_parent_id = tplma ).
        <ls_hierarchy>-node_type = mc_tec_hierarchy_node-tec_location.
        DATA(lv_length) = strlen( <ls_hierarchy>-node_id ).
        CASE lv_length.
          WHEN '5'.
            <ls_hierarchy>-highlight = mc_highlight_node-success.
          WHEN '8'.
            <ls_hierarchy>-highlight = mc_highlight_node-error.
          WHEN '11'.
            <ls_hierarchy>-highlight = mc_highlight_node-warning.
          WHEN OTHERS.
            <ls_hierarchy>-highlight = mc_highlight_node-none.
        ENDCASE.

*      <ls_hierarchy>-highlight = mc_highlight_node-none.

        READ TABLE lt_iflotx ASSIGNING FIELD-SYMBOL(<ls_iflotx>) WITH KEY tec_location = <ls_iflot>-tplnr.
        IF sy-subrc IS INITIAL.
          <ls_hierarchy>-node_desc = <ls_iflotx>-tec_location_desc.
        ENDIF.
      ENDIF.
    ENDLOOP.

*  Si no se buscan los equipos termina aqui
    IF iv_include_equipments IS NOT INITIAL.

      DATA(lo_equipment) = NEW zcl_pm_equipment( ).

*  Se buscan los equipos para las ubicaciones técnicas seleccionadas
      DATA(lt_equnr) = lo_equipment->search_equipment( it_tec_location = lr_tec_location ).
      lr_equnr = VALUE #( FOR <w> IN lt_equnr ( sign = 'I' option = 'EQ' low = <w> ) ).
      CHECK lr_equnr IS NOT INITIAL.

*  Se recuperan los datos de cabecera de los equipos encontrados
      DATA(lt_equipments) = lo_equipment->get_header_data( it_equipment = lr_equnr ).

*  Se añaden a la jerarquia
      LOOP AT lt_equipments ASSIGNING FIELD-SYMBOL(<ls_equipment>).

        APPEND INITIAL LINE TO rt_hierarchy ASSIGNING <ls_hierarchy>.
        <ls_hierarchy> = CORRESPONDING #( <ls_equipment> MAPPING node_id = equipment node_parent_id = tec_location node_desc = equipment_desc ).
        <ls_hierarchy>-node_type = mc_tec_hierarchy_node-equipment.
        <ls_hierarchy>-highlight = mc_highlight_node-inf.
      ENDLOOP.

    ENDIF.

*   Se limpian los nodos raiz
    lr_tec_location = VALUE #( FOR <e> IN lt_tplnr ( sign = 'I' option = 'EQ' low = <e> ) ).

    LOOP AT rt_hierarchy ASSIGNING <ls_hierarchy> WHERE node_id IN lr_tec_location.
      CLEAR <ls_hierarchy>-node_parent_id.
    ENDLOOP.


    LOOP AT lt_tplnr ASSIGNING FIELD-SYMBOL(<ls_tplnr>).

    ENDLOOP.

*    SEARCH_EQUIPMENT
*GET_HEADER_DATA

*
*BEGIN OF mc_tec_hierarchy_node,
*                 equipment    TYPE string VALUE 'EQUI',
*                 tec_location TYPE string VALUE 'TECL',
*               END OF  mc_tec_hierarchy_node.
*      rt_hierarchy = CORRESPONDING #( lt_iflot MAPPING node_id = tplnr node_parent_id = tplma node_desc

*
*    IT_CENTER
*IT_TEC_LOCATION
*IV_INCLUDE_EQUIPMENTS
*RT_HIERARCHY

  ENDMETHOD.


  METHOD get_tecnical_location.

    DATA: lr_center            TYPE RANGE OF werks_d,
          lr_tecnical_location TYPE RANGE OF tplnr.

    lr_center = CORRESPONDING #( it_center ).
    lr_tecnical_location = CORRESPONDING #( it_tec_location ).

*   Si no se busca algun centro en concreto se recuperan los que estan activos en la aplicacion
    IF lr_center IS INITIAL.
      DATA(lt_center) = get_active_centers( ).
      lr_center = VALUE #( FOR <a> IN lt_center ( sign = 'I' option = 'EQ' low = <a> ) ).
    ENDIF.

    SELECT a~iwerk AS center a~tplnr AS tec_location b~pltxt AS tec_location_desc
      FROM iflot AS a INNER JOIN iflotx AS b ON a~tplnr EQ b~tplnr
      INTO CORRESPONDING FIELDS OF TABLE rt_tecnical_location
      WHERE a~tplnr IN lr_tecnical_location AND
            a~iwerk IN lr_center AND
            b~spras EQ iv_langu.

  ENDMETHOD.


  METHOD get_user_name.

    DATA: lr_uname TYPE RANGE OF uname.

    CHECK it_uname IS NOT INITIAL .


    lr_uname = CORRESPONDING #( it_uname ) .

    SELECT a~bname AS user b~name_text AS username
      INTO CORRESPONDING FIELDS OF TABLE rt_username
      FROM usr21 AS a JOIN adrp AS b ON a~persnumber EQ b~persnumber
      WHERE a~bname IN lr_uname.


  ENDMETHOD.


  METHOD get_warehouse_data.

    DATA:lr_lgort TYPE RANGE OF lgort_d.
    lr_lgort = CORRESPONDING #( it_lgort ).

    SELECT lgort AS warehouse, lgobe AS warehouse_desc
      FROM t001l
      INTO CORRESPONDING FIELDS OF TABLE @rt_warehouse_data
      WHERE werks EQ @iv_werks
      AND   lgort IN @lr_lgort.


  ENDMETHOD.
ENDCLASS.
