CLASS zcl_pm_stock_app_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /neptune/if_nad_server .

    TYPES:
      BEGIN OF ts_key_value,
        key   TYPE string,
        value TYPE string,
      END OF ts_key_value .
    TYPES:
      tt_key_value TYPE STANDARD TABLE OF ts_key_value WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_matchcode,
        key TYPE string,
      END OF ts_matchcode .
    TYPES:
      tt_matchcode TYPE STANDARD TABLE OF ts_matchcode WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_offline_material_list,
        material_id          TYPE string,
        material_desc        TYPE string,
        supplier_material_id TYPE string,
        location             TYPE string,
        equipment            TYPE string,
        brand                TYPE string,
        model                TYPE string,
        stock                TYPE string,
        section              TYPE string,
        purchase_quan        TYPE string,
        center               TYPE string,
        punto_pedido         TYPE string,

*       Campo para buscar la descripcion del material y codigo proveedor
        search_field         TYPE string,
*        storage_location     TYPE string,
      END OF ts_offline_material_list .
    TYPES:
      tt_offline_material_list TYPE STANDARD TABLE OF ts_offline_material_list WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_handle_on_ajax ,
        applid     TYPE string,
        ajax_id    TYPE string,
        ajax_value TYPE string,
        server     TYPE REF TO /neptune/cl_nad_server,
        request    TYPE /neptune/data_request,
        navigation TYPE /neptune/ajax_navigation,
      END OF ts_handle_on_ajax .

    DATA ms_on_ajax TYPE ts_handle_on_ajax .
    DATA mt_offline_material_list TYPE tt_offline_material_list .
    DATA mt_material_list TYPE tt_offline_material_list .
    DATA ms_material_detail TYPE ts_offline_material_list .
    DATA ms_user_info TYPE zcl_pm_user=>ts_user .
    DATA mt_matchcode_marca TYPE tt_matchcode .
    DATA mt_matchcode_modelo TYPE tt_matchcode .
    DATA mt_matchcode_equipo TYPE tt_matchcode .
    DATA mt_matchcode_elemento TYPE tt_matchcode .
    DATA mt_matchcode_grupo TYPE tt_matchcode .
    DATA mt_matchcode_numero TYPE tt_matchcode .
    DATA mt_matchcode_seccion TYPE tt_matchcode .
    DATA mt_matchcode_item TYPE tt_matchcode .
    DATA mt_center_master TYPE tt_key_value .
protected section.

  methods AJAX_INIT_APLICATION .
  methods AJAX_GET_USER_INFO .
  methods AJAX_GET_MATERIAL_LIST .
private section.
ENDCLASS.



CLASS ZCL_PM_STOCK_APP_CONTROLLER IMPLEMENTATION.


  METHOD /neptune/if_nad_server~handle_on_ajax.

    DATA lv_mock TYPE abap_bool VALUE ''.

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
*  WHEN abap_false.
    CALL METHOD (ajax_method).

**         Mock
*          WHEN abap_true.
**            zcl_pm_notifications_app_mock=>ms_on_ajax = ms_on_ajax.
**            zcl_pm_notifications_app_mock=>controller = me.
**            CALL METHOD zcl_pm_notifications_app_mock=>(ajax_method).
*        ENDCASE.
*      CATCH cx_root.

*    ENDTRY.


  ENDMETHOD.


  method /NEPTUNE/IF_NAD_SERVER~HANDLE_ON_SUBMIT.
  endmethod.


  METHOD ajax_get_material_list.

    DATA: lt_repuestos TYPE zpm_repuestos,
          lr_center    TYPE RANGE OF werks_d.

    if ms_on_ajax-ajax_value is not initial.
      lr_center = value #( ( sign = 'I' option = 'EQ' low = ms_on_ajax-ajax_value ) ).
    else.
      zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = 'STOCK_CENTER_%'
                                                    CHANGING ct_ranges = lr_center ).
    endif.



    LOOP AT lr_center ASSIGNING FIELD-SYMBOL(<ls_center>).

      CLEAR lt_repuestos[].

      CALL FUNCTION 'ZPM_REPUESTOS'
        EXPORTING
          werks         = <ls_center>-low
        TABLES
          zpm_repuestos = lt_repuestos.

      LOOP AT lt_repuestos ASSIGNING FIELD-SYMBOL(<ls_repuesto>).
        APPEND INITIAL LINE TO mt_offline_material_list ASSIGNING FIELD-SYMBOL(<ls_offline_material_list>).
        <ls_offline_material_list> = CORRESPONDING #( <ls_repuesto> MAPPING material_id = objek
              material_desc        = maktx
              supplier_material_id = idnlf
              location             = lgpbe
              equipment            = car3
              brand                = car1
              model                = car2
              stock                = stlibre
              section              = car7
              purchase_quan        = bstfe
              center               = werks
              punto_pedido         = minbe  ).

        <ls_offline_material_list>-search_field = |{ <ls_offline_material_list>-material_desc }{ <ls_offline_material_list>-supplier_material_id }|.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  method AJAX_GET_USER_INFO.

     ms_user_info = NEW zcl_pm_user( )->get_user_info( sy-uname ).

  endmethod.


  METHOD ajax_init_aplication.


    DATA: lc_marca          TYPE string VALUE 'MARCA',
          lc_modelo         TYPE string VALUE 'MODELO',
          lc_equipo         TYPE string VALUE 'EQUIPO',
          lc_elemento       TYPE string VALUE 'ELEMENTO',
          lc_grupo          TYPE string VALUE 'GRUPO',
          lc_numero         TYPE string VALUE 'NUMERO',
          lc_seccion        TYPE string VALUE 'SECCION',
          lc_item           TYPE string VALUE 'ITEM',
          lr_characteristic TYPE RANGE OF atnam,
          lr_center         TYPE RANGE OF werks_d.

    lr_characteristic = VALUE #( sign = 'I' option = 'EQ'
                                 ( low = lc_marca )
                                 ( low = lc_modelo )
                                 ( low = lc_equipo )
                                 ( low = lc_elemento )
                                 ( low = lc_grupo )
                                 ( low = lc_numero )
                                 ( low = lc_seccion )
                                 ( low = lc_item ) ).

    SELECT atnam AS characteristic, atwrt AS characteristic_value
      FROM ausp
      INNER JOIN cabn ON ausp~atinn = cabn~atinn AND
                         ausp~adzhl = cabn~adzhl
      INTO TABLE @DATA(lt_char_result)
      WHERE cabn~atnam IN @lr_characteristic.

    SORT lt_char_result BY characteristic characteristic_value.
    DELETE ADJACENT DUPLICATES FROM lt_char_result COMPARING ALL FIELDS.


    LOOP AT lt_char_result ASSIGNING FIELD-SYMBOL(<ls_char_result>).
      CASE <ls_char_result>-characteristic.
        WHEN lc_marca.
          APPEND INITIAL LINE TO mt_matchcode_marca ASSIGNING FIELD-SYMBOL(<ls_matchcode>).
        WHEN lc_modelo.
          APPEND INITIAL LINE TO mt_matchcode_modelo ASSIGNING <ls_matchcode>.
        WHEN lc_equipo.
          APPEND INITIAL LINE TO mt_matchcode_equipo ASSIGNING <ls_matchcode>.
        WHEN lc_elemento.
          APPEND INITIAL LINE TO mt_matchcode_elemento ASSIGNING <ls_matchcode>.
        WHEN lc_grupo.
          APPEND INITIAL LINE TO mt_matchcode_grupo ASSIGNING <ls_matchcode>.
        WHEN lc_numero.
          APPEND INITIAL LINE TO mt_matchcode_numero ASSIGNING <ls_matchcode>.
        WHEN lc_seccion.
          APPEND INITIAL LINE TO mt_matchcode_seccion ASSIGNING <ls_matchcode>.
        WHEN lc_item.
          APPEND INITIAL LINE TO mt_matchcode_item ASSIGNING <ls_matchcode>.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      <ls_matchcode>-key = <ls_char_result>-characteristic_value.

    ENDLOOP.

    zcl_pm_constants=>obtener_constantes_en_ranges( EXPORTING iv_patron = 'STOCK_CENTER_%'
                                                    CHANGING ct_ranges = lr_center ).
    DATA(lt_center) = zcl_pm_masterdata=>get_center_data( lr_center  ).
    mt_center_master = CORRESPONDING #( lt_center MAPPING key = center value = center_desc ).
    APPEND INITIAL LINE TO mt_center_master ASSIGNING FIELD-SYMBOL(<ls_center_master>).
    <ls_center_master>-value = text-001.
    SORT mt_center_master ASCENDING.

  ENDMETHOD.
ENDCLASS.
