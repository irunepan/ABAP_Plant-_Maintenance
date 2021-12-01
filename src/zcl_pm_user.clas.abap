CLASS zcl_pm_user DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_user_placement,
        username  TYPE uname,
        placement TYPE string,
      END OF ts_user_placement .
    TYPES:
      tt_user_placement TYPE STANDARD TABLE OF ts_user_placement WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_user,
        username           TYPE uname,
        langu              TYPE sy-langu,
        center             TYPE werks_d,
        sap_role           TYPE string,
        role               TYPE string,
        default_placement  TYPE iloan,
        default_job        TYPE arbpl,
        employee_id        TYPE pernr_d,
        fullname           TYPE string,
        questionary_active TYPE abap_bool,

*       Estos campos quizas se necesiten en algun momento
        currency           TYPE waers,
        dec_separator      TYPE string, "Separador de decimales XX,XX
        grp_separator      TYPE string, "Separador de miles XX.XXX.XXX
        date_pattern       TYPE string, "Patron para fechas dd.mm.yyyy
      END OF ts_user .

    CONSTANTS:
      BEGIN OF mc_roles,
        admin          TYPE zpm_e_role VALUE 'ADM',
        oficial        TYPE zpm_e_role VALUE 'OFI',
        jefe_turno     TYPE zpm_e_role VALUE 'JEF',
        almacenero     TYPE zpm_e_role VALUE 'ALM',
        enc_produccion TYPE zpm_e_role VALUE 'PRO',
      END OF mc_roles .
    CONSTANTS:
      BEGIN OF mc_sap_roles,
        admin          TYPE agr_name VALUE 'Z_PM_ADMINISTRADOR',
        oficial        TYPE agr_name VALUE 'Z_PM_OPERARIO',
        jefe_turno     TYPE agr_name VALUE 'Z_PM_JEFE_TURNO',
        almacenero     TYPE agr_name VALUE 'Z_PM_ALMACENERO',
        enc_produccion TYPE agr_name VALUE 'Z_PM_PRODUCCION',
      END OF mc_sap_roles .

    METHODS get_user_info
      IMPORTING
        !iv_username        TYPE uname
      RETURNING
        VALUE(rs_user_data) TYPE ts_user .
    METHODS get_users_with_placement
      RETURNING
        VALUE(rt_users_with_placement) TYPE tt_user_placement .
    METHODS get_sap_user_from_pernr
      IMPORTING
        !iv_pernr      TYPE pernr_d
      RETURNING
        VALUE(rv_user) TYPE uname .
    METHODS get_user_name
      IMPORTING
        !iv_pernr           TYPE pernr_d
      RETURNING
        VALUE(rv_full_name) TYPE string .
protected section.

  methods GET_USER_PERNR
    importing
      !IV_USER type UNAME
    returning
      value(RV_PERNR) type PERNR_D
    exceptions
      ERROR .
  methods GET_USER_CENTER
    importing
      !IV_USER_ROLE type STRING
    returning
      value(RV_CENTER) type WERKS_D .
  methods GET_DEFAULT_PLACEMENT
    importing
      !IV_USER type UNAME
    returning
      value(RV_PLACEMENT) type ILOAN .
  methods GET_DEFAULT_JOB
    importing
      !IV_USER type UNAME
    returning
      value(RV_JOB) type ARBPL .
  methods GET_USER_ROLE
    importing
      !IV_USER type UNAME
    exporting
      !EV_ROLE type STRING
      !EV_SAP_ROLE type STRING .
  methods PREVENTION_QUESTIONARY_ACTIVE
    importing
      !IV_CENTER type WERKS_D
    returning
      value(RV_QUESTIONARY_ACTIVE) type ABAP_BOOL .
private section.
ENDCLASS.



CLASS ZCL_PM_USER IMPLEMENTATION.


  METHOD get_default_job.

    SELECT SINGLE parva
        FROM usr05
        INTO @DATA(lv_parva)
        WHERE bname EQ @iv_user AND
              parid EQ 'Z_PM_PTO_TRABAJO'.

    rv_job = CONV arbpl( lv_parva ).

  ENDMETHOD.


  METHOD get_default_placement.

    SELECT SINGLE parva
        FROM usr05
        INTO @DATA(lv_parva)
        WHERE bname EQ @iv_user AND
              parid EQ 'Z_PM_EMPL_DEFECTO'.

    rv_placement = CONV iloan( lv_parva ).
  ENDMETHOD.


  METHOD get_sap_user_from_pernr.

*   Algunos usuarios estan en un subtipo y otros en otro, hacemos dos selects
    SELECT SINGLE usrid
      INTO rv_user
      FROM pa0105
      WHERE subty EQ '0001'  AND
            pernr EQ iv_pernr AND
            begda LE sy-datum AND
            endda GE sy-datum.

  ENDMETHOD.


  method GET_USERS_WITH_PLACEMENT.

    SELECT bname as username parva as placement
        FROM usr05
        INTO CORRESPONDING FIELDS OF TABLE rt_users_with_placement
        WHERE parid EQ 'Z_PM_EMPL_DEFECTO'.

  endmethod.


  METHOD get_user_center.

    CHECK iv_user_role IS NOT INITIAL.

    DATA(lv_center_start) = strlen( iv_user_role ) - 4.

    TRY.
        rv_center = iv_user_role+lv_center_start(4).
      CATCH cx_root.

    ENDTRY.

*    rv_center = '1001'.
  ENDMETHOD.


  METHOD get_user_info.

    CHECK iv_username IS NOT INITIAL.

    rs_user_data-username          = iv_username.
    rs_user_data-langu             = sy-langu.

    get_user_role( EXPORTING iv_user = rs_user_data-username
                   IMPORTING ev_role = rs_user_data-role
                             ev_sap_role = rs_user_data-sap_role ).
    rs_user_data-center            = get_user_center( rs_user_data-sap_role ).
    rs_user_data-questionary_active = prevention_questionary_active( rs_user_data-center ).


    rs_user_data-default_placement = get_default_placement( iv_user = rs_user_data-username ).
    rs_user_data-default_job = get_default_job( iv_user = rs_user_data-username ).
    rs_user_data-employee_id       = get_user_pernr( iv_user = rs_user_data-username ).
    rs_user_data-fullname       = get_user_name( iv_pernr = rs_user_data-employee_id ).

    rs_user_data-currency = 'EUR'.
    rs_user_data-dec_separator = ','. "Separador de decimales XX,XX
    rs_user_data-grp_separator = '.'. "Separador de miles XX.XXX.XXX
    rs_user_data-date_pattern  = 'dd.mm.yyyy'. "Patron para fechas dd.mm.yyyy

  ENDMETHOD.


  METHOD get_user_name.

    SELECT SINGLE vorna AS name, nachn AS lastname1, nach2 AS lastname2
      FROM pa0002
      INTO @DATA(ls_user_name)
      WHERE pernr EQ @iv_pernr AND
            begda LE @sy-datum AND
            endda GE @sy-datum AND
            vorna NE @space.

    rv_full_name = |{ ls_user_name-name } { ls_user_name-lastname1 } { ls_user_name-lastname2 }|.

  ENDMETHOD.


  METHOD get_user_pernr.

*   Usuario de test para probar o simular un rol
    DATA(lv_test_user_1) = zcl_pm_constants=>obtener_constantes( iv_constante = 'TEST_USER_1' ).
    DATA(lv_test_pernr_1) = zcl_pm_constants=>obtener_constantes( iv_constante = 'TEST_USER_PERNR_1' ).

*   Usuario de test para probar o simular un rol
    DATA(lv_test_user_2) = zcl_pm_constants=>obtener_constantes( iv_constante = 'TEST_USER_2' ).
    DATA(lv_test_pernr_2) = zcl_pm_constants=>obtener_constantes( iv_constante = 'TEST_USER_PERNR_2' ).

*   Si el usuario es uno de test le asigna el rol de la tabla de constantes
    rv_pernr = COND #( WHEN iv_user EQ lv_test_user_1 THEN lv_test_pernr_1
                       WHEN iv_user EQ lv_test_user_2 THEN lv_test_pernr_2 ).


    CHECK rv_pernr IS INITIAL.

    SELECT SINGLE pernr
      FROM pa0105
      INTO rv_pernr
      WHERE subty EQ '0001'  AND
            usrid EQ iv_user AND
            begda LE sy-datum AND
            endda GE sy-datum.



  ENDMETHOD.


  METHOD get_user_role.

*   Usuario de test para probar o simular un rol
    DATA(lv_test_user_1) = zcl_pm_constants=>obtener_constantes( iv_constante = 'TEST_USER_1' ).
    DATA(lv_test_role_1) = zcl_pm_constants=>obtener_constantes( iv_constante = 'TEST_USER_ROLE_1' ).
    DATA(lv_test_sap_role_1) = zcl_pm_constants=>obtener_constantes( iv_constante = 'TEST_USER_SAP_ROLE_1' ).

*   Usuario de test para probar o simular un rol
    DATA(lv_test_user_2) = zcl_pm_constants=>obtener_constantes( iv_constante = 'TEST_USER_2' ).
    DATA(lv_test_role_2) = zcl_pm_constants=>obtener_constantes( iv_constante = 'TEST_USER_ROLE_2' ).
    DATA(lv_test_sap_role_2) = zcl_pm_constants=>obtener_constantes( iv_constante = 'TEST_USER_SAP_ROLE_2' ).

*   Si el usuario es uno de test le asigna el rol de la tabla de constantes
    ev_role = COND #( WHEN iv_user EQ lv_test_user_1 THEN lv_test_role_1
                      WHEN iv_user EQ lv_test_user_2 THEN lv_test_role_2 ).

    ev_sap_role = COND #( WHEN iv_user EQ lv_test_user_1 THEN lv_test_sap_role_1
                  WHEN iv_user EQ lv_test_user_2 THEN lv_test_sap_role_2 ).

    CHECK ev_role IS INITIAL.

*   Si no es un usuario de test se busca en los roles de SAP
    SELECT agr_name
      FROM agr_users
      INTO TABLE @DATA(lt_agr_users)
      WHERE agr_name LIKE 'Z_PM_%' AND
            uname EQ @iv_user AND
            from_dat LE @sy-datum AND
            to_dat GE @sy-datum.

*   Admin
    LOOP AT lt_agr_users ASSIGNING FIELD-SYMBOL(<ls_agr_user>) WHERE agr_name CS mc_sap_roles-admin.
      ev_sap_role = <ls_agr_user>-agr_name.
      ev_role = mc_roles-admin.
    ENDLOOP.
    CHECK ev_role IS INITIAL.

*   Oficial
    LOOP AT lt_agr_users ASSIGNING <ls_agr_user> WHERE agr_name CS mc_sap_roles-oficial.
      ev_sap_role = <ls_agr_user>-agr_name.
      ev_role = mc_roles-oficial.
    ENDLOOP.
    CHECK ev_role IS INITIAL.

*   Jefe de turno
    LOOP AT lt_agr_users ASSIGNING <ls_agr_user> WHERE agr_name CS mc_sap_roles-jefe_turno.
      ev_sap_role = <ls_agr_user>-agr_name.
      ev_role =  mc_roles-jefe_turno.
    ENDLOOP.
    CHECK ev_role IS INITIAL.

*   Âlmacenero
    LOOP AT lt_agr_users ASSIGNING <ls_agr_user> WHERE agr_name CS mc_sap_roles-almacenero.
      ev_sap_role = <ls_agr_user>-agr_name.
      ev_role =  mc_roles-almacenero.
    ENDLOOP.
    CHECK ev_role IS INITIAL.

*   Encargado de produccion
    LOOP AT lt_agr_users ASSIGNING <ls_agr_user> WHERE agr_name CS mc_sap_roles-enc_produccion.
      ev_sap_role = <ls_agr_user>-agr_name.
      ev_role =  mc_roles-enc_produccion.
    ENDLOOP.

  ENDMETHOD.


  METHOD prevention_questionary_active.

    DATA: lr_center TYPE RANGE OF werks_d.
    zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = 'CENTER_QUESTIONARY_%'
                                                    CHANGING ct_ranges = lr_center ).

    IF lr_center IS INITIAL.
      rv_questionary_active = abap_false.
      EXIT.
    ENDIF.

    IF iv_center IN lr_center.
      rv_questionary_active = abap_true.
      EXIT.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
