class ZCL_PM_DOC_MAT_APP_CNTRL definition
  public
  final
  create public .

public section.

  interfaces /NEPTUNE/IF_NAD_SERVER .

  types:
    BEGIN OF ts_header_doc,
             werks        TYPE werks_d,
             lgort        TYPE lgort_d,
             bwart        TYPE bwart,
             kostl        TYPE kostl,
             comment      TYPE bktxt,
             destinatario TYPE wempf,
           END OF ts_header_doc .
  types:
    BEGIN OF ts_key_value,
        key   TYPE string,
        value TYPE string,
      END OF ts_key_value .
  types:
    tt_key_value TYPE STANDARD TABLE OF ts_key_value WITH DEFAULT KEY .
  types:
    BEGIN OF ts_material_movement,
        material      TYPE string,
        material_desc TYPE string,
        quantity      TYPE string,
        unit          TYPE string,
        dest          TYPE string,
      END OF ts_material_movement .
  types:
    tt_material_movement TYPE STANDARD TABLE OF ts_material_movement WITH DEFAULT KEY .
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

  data MS_USER_INFO type ZCL_PM_USER=>TS_USER .
  data MT_CECOS type TT_KEY_VALUE .
  data MT_CENTER type TT_KEY_VALUE .
  data MT_LGORT type TT_KEY_VALUE .
  data MT_BWART type TT_KEY_VALUE .
  data MS_MATERIAL_MOVEMENT_CREATE type TS_MATERIAL_MOVEMENT .
  data MT_MATERIAL_MOVEMENT_CREATE type TT_MATERIAL_MOVEMENT .
  data MS_ON_AJAX type TS_HANDLE_ON_AJAX .
  data MS_HEADER_DOS type TS_HEADER_DOC .
  data MT_LOG type ZIF_PM_DATA=>TT_LOG .
protected section.

  methods AJAX_GET_USER_INFO .
  methods AJAX_INIT_APLICATION .
  methods AJAX_GET_MATERIAL_DETAIL .
  methods AJAX_CREATE_MATERIAL_MOVEMENT .
private section.
ENDCLASS.



CLASS ZCL_PM_DOC_MAT_APP_CNTRL IMPLEMENTATION.


  METHOD /neptune/if_nad_server~handle_on_ajax.



    ms_on_ajax = VALUE #( applid = applid
                          ajax_id = ajax_id
                          ajax_value = ajax_value
                          navigation = navigation
                          server = server
                          request = request ).

    DATA(ajax_method) = CONV seoclsname( |AJAX_{ ms_on_ajax-ajax_id }| ).


    CALL METHOD (ajax_method).

  ENDMETHOD.


  METHOD ajax_create_material_movement.

    DATA(lo_material) = NEW zcl_pm_material( ).
    DATA lt_material_movement_item  TYPE zcl_pm_material=>tt_material_movement_item.

    DATA(ls_material_movement_header) = VALUE zcl_pm_material=>ts_material_movement_header( header_text = ms_header_dos-comment ).

    LOOP AT mt_material_movement_create ASSIGNING FIELD-SYMBOL(<fs_mat_mov>).
      INSERT VALUE #( material  =  |{ CONV matnr( <fs_mat_mov>-material ) ALPHA = IN }|
                      center    = ms_header_dos-werks
                      storage   = ms_header_dos-lgort
                      move_type = ms_header_dos-bwart
                      quantity  = <fs_mat_mov>-quantity
                      user      = ms_header_dos-destinatario
                      kostl      = ms_header_dos-kostl
                      unit      = zcl_pm_utilidades=>convert_cunit_in( CONV meins( <fs_mat_mov>-unit ) )  )
                INTO TABLE lt_material_movement_item.
    ENDLOOP.
    lo_material->create_material_movement( EXPORTING is_material_movement_header = ls_material_movement_header
                                                     it_material_movement_item = lt_material_movement_item
                                           IMPORTING ev_materialdocument = DATA(lv_material_doc)
                                                     ev_matdocumentyear	= DATA(lv_material_doc_year)
                                                     et_return          = DATA(lt_return) ).

    IF lv_material_doc IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      <ls_return>-type = zif_pm_data=>cs_msg_type-success.
      MESSAGE ID zif_pm_data=>cs_message_id TYPE zif_pm_data=>cs_msg_type-success NUMBER 013 WITH lv_material_doc INTO <ls_return>-message.
    ENDIF.

    mt_log = zcl_pm_utilidades=>convert_msg_to_ui5( lt_return ).
  ENDMETHOD.


  METHOD AJAX_GET_MATERIAL_DETAIL.

    DATA: lr_material TYPE RANGE OF matnr.

    lr_material = VALUE #( ( sign = 'I' option = 'EQ' low = |{ CONV matnr( ms_material_movement_create-material ) ALPHA = IN }| ) ).

    DATA(lt_material_header) = NEW zcl_pm_material( )->get_material_header( it_material = lr_material ).
    READ TABLE lt_material_header ASSIGNING FIELD-SYMBOL(<ls_material_header>) INDEX 1.
    IF sy-subrc IS INITIAL.
      ms_material_movement_create-material_desc = <ls_material_header>-material_desc.
      ms_material_movement_create-unit = zcl_pm_utilidades=>convert_cunit_out( <ls_material_header>-unit ).
    ENDIF.

  ENDMETHOD.


  METHOD AJAX_GET_USER_INFO.

    ms_user_info = NEW zcl_pm_user( )->get_user_info( sy-uname ).

  ENDMETHOD.


  METHOD ajax_init_aplication.

    DATA lr_center         TYPE RANGE OF werks_d.
    DATA lr_lgort          TYPE RANGE OF lgort_d.
    DATA lr_bwart         TYPE RANGE OF bwart.

    " Recuperamos el centro del usuario
    lr_center = VALUE #( ( sign = 'I' option = 'EQ' low = ms_user_info-center  ) ).

    DATA(lt_center) = zcl_pm_masterdata=>get_center_data( lr_center  ).
    mt_center = CORRESPONDING #( lt_center MAPPING key = center value = center_desc ).
*    APPEND INITIAL LINE TO mt_center.
*    SORT mt_center ASCENDING.

    " Recuperamos el almacén de repuestos
    lr_lgort = VALUE #( ( sign = 'I' option = 'EQ' low = zif_pm_data=>cv_warehouse  ) ).

    DATA(lt_lgort) = zcl_pm_masterdata=>get_warehouse_data( EXPORTING iv_werks = ms_user_info-center it_lgort = lr_lgort ).

    mt_lgort = CORRESPONDING #( lt_lgort MAPPING key = warehouse value = warehouse_desc ).
*    APPEND INITIAL LINE TO mt_lgort.
*    SORT mt_lgort ASCENDING.

    " Recuperamos las clases de movimiento
    lr_bwart = VALUE #( ( sign = 'I' option = 'EQ' low = zif_pm_data=>cv_mov_201  ) ).

    DATA(lt_bwart) = zcl_pm_masterdata=>get_movements( it_bwart = lr_bwart ).

    mt_bwart = CORRESPONDING #( lt_bwart MAPPING key = bwart value = bwart_desc ).
*    APPEND INITIAL LINE TO mt_bwart.
*    SORT mt_bwart ASCENDING.

    " Recuperamos los centros de coste
    DATA(lt_kostl) = zcl_pm_masterdata=>get_cecos( iv_werks = ms_user_info-center  ).
    mt_cecos = CORRESPONDING #( lt_kostl MAPPING key = kostl value = kostl_desc ).
*    APPEND INITIAL LINE TO mt_cecos.
    SORT mt_cecos ASCENDING.

  ENDMETHOD.
ENDCLASS.
