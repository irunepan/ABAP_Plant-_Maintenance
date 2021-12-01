CLASS zcl_pm_attachments DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_tmp_file,
             guid    TYPE guid_32,
             counter TYPE numc4,
           END OF ts_tmp_file .
    TYPES:
      BEGIN OF ts_delete_file ,
        foltp TYPE so_fol_tp,
        folyr TYPE so_fol_yr,
        folno TYPE so_fol_no,
        objtp TYPE so_doc_tp,
        objyr TYPE so_doc_yr,
        objno TYPE so_doc_no,
        qmnum TYPE qmnum,
      END OF ts_delete_file .
    TYPES:
      tt_bbdd_tmp_atta TYPE TABLE OF zpm_t_tmp_atta WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_file,
        filename   TYPE string, "Nombre con extension
        name       TYPE string, "Nombre sin extension
        extension  TYPE file_ext,
        mimetype   TYPE w3conttype,
        bin_length TYPE i,
        content    TYPE xstring,
      END OF ts_file .
    TYPES:
      tt_file TYPE STANDARD TABLE OF ts_file WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_publish_attachment,
        content   TYPE xstring,
        extension TYPE string,
        mimetype  TYPE string,
      END OF ts_publish_attachment .
    TYPES:
      BEGIN OF ts_gos_attachment_list,
        instid_a      TYPE sibfboriid,
        typeid_a      TYPE sibftypeid,
        catid_a       TYPE sibfcatid,
        document_id   TYPE sibfboriid,
        typeid_b      TYPE sibftypeid,
        foltp         TYPE so_fol_tp,
        folyr         TYPE so_fol_yr,
        folno         TYPE so_fol_no,
        doctp         TYPE so_doc_tp,
        docyr         TYPE so_doc_yr,
        docno         TYPE so_doc_no,
        creation_date TYPE so_dat_cr,
        creation_time TYPE so_tim_cr,
        creation_user TYPE string,
        document_desc TYPE so_obj_des,
        file_ext      TYPE file_ext,
        mimetype      TYPE w3conttype,
        ui5_icon      TYPE string,
      END OF ts_gos_attachment_list .
    TYPES:
      tt_gos_attachment_list TYPE STANDARD TABLE OF ts_gos_attachment_list WITH DEFAULT KEY .

    CONSTANTS:
      BEGIN OF ms_bussines_object,
        notification TYPE sibftypeid VALUE 'BUS2038',
        order        TYPE sibftypeid VALUE 'BUS2007',
        equipment    TYPE sibftypeid VALUE 'EQUI',
        task_list    TYPE sibftypeid VALUE 'BUS1019',
      END OF ms_bussines_object .

    METHODS constructor
      IMPORTING
        !iv_langu TYPE langu DEFAULT sy-langu .
    METHODS upload_file_2_gos
      IMPORTING
        !iv_sap_object TYPE saeanwdid
        !iv_object_id  TYPE saeobjid
        !is_file       TYPE ts_file
        !iv_commit     TYPE abap_bool DEFAULT abap_true
      EXCEPTIONS
        error .
    METHODS delete_file_gos
      IMPORTING
        !iv_sap_object        TYPE saeanwdid
        !iv_gos_attachment_id TYPE so_entryid
      EXPORTING
        !es_return            TYPE bapiret2 .
    METHODS get_gos_attachment_list
      IMPORTING
        !ir_instid                TYPE STANDARD TABLE
        !iv_type_id               TYPE sibftypeid
        !iv_complete_data         TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_gos_attachments) TYPE tt_gos_attachment_list .
    METHODS get_gos_attachment_url
      IMPORTING
        !iv_gos_attachment_id TYPE so_entryid
      RETURNING
        VALUE(rv_url)         TYPE string .
    METHODS generate_tmp_guid
      RETURNING
        VALUE(rv_guid) TYPE guid_32 .
    METHODS save_tmp_attachment
      IMPORTING
        !iv_guid TYPE guid_32
        !is_file TYPE ts_file
      EXCEPTIONS
        error .
    METHODS get_tmp_attachments
      IMPORTING
        !iv_guid             TYPE guid_32
      RETURNING
        VALUE(rt_tmp_attach) TYPE tt_bbdd_tmp_atta .
    METHODS delete_tmp_attachments
      IMPORTING
        !iv_tmp_file TYPE so_entryid
      EXPORTING
        !es_return   TYPE bapiret2 .
    METHODS get_extension_from_filename
      IMPORTING
        !iv_filename  TYPE string
      EXPORTING
        !ev_extension TYPE file_ext
        !ev_name      TYPE string .
    METHODS get_mimetype_from_extension
      IMPORTING
        !iv_extension      TYPE file_ext
      RETURNING
        VALUE(rv_mimetype) TYPE w3conttype .
    METHODS get_ui5_icon_from_mimetype
      IMPORTING
        !iv_mimetype       TYPE w3conttype
      RETURNING
        VALUE(rv_ui5_icon) TYPE string .
protected section.

  data MT_GOS_ATTACHMENTS type TT_GOS_ATTACHMENT_LIST .
  data MS_PUBLISH_ATTACHMENT type TS_PUBLISH_ATTACHMENT .
  data MV_LANGU type LANGU .

  methods GET_GOS_ATTACHMENT_CONTENT
    importing
      !IV_GOS_ATTACHMENT_ID type SO_ENTRYID .
  methods PUBLISH_ATTACHMENT
    returning
      value(RV_URL) type STRING .
private section.
ENDCLASS.



CLASS ZCL_PM_ATTACHMENTS IMPLEMENTATION.


  METHOD constructor.

    mv_langu = iv_langu.

  ENDMETHOD.


  METHOD delete_file_gos.

    DATA ls_gos_doc TYPE ts_delete_file.

    DATA: ls_folder_id TYPE soodk,
          ls_object_id TYPE soodk,
          ls_object    TYPE sibflporb,
          ls_object_a  TYPE sibflporb,
          ls_object_b  TYPE sibflporb,
          lv_objkey    TYPE swo_typeid,
          lt_relat     TYPE obl_t_relt,
          ls_relat     TYPE obl_s_relt,
          lt_links     TYPE obl_t_link,
          lv_rel_typ   TYPE char4.

    CONSTANTS: lc_url  TYPE string VALUE 'URL',
               lc_raw  TYPE string VALUE 'RAW',
               lc_note TYPE string VALUE 'NOTE',
               lc_ext  TYPE string VALUE 'EXT',
               lc_atta TYPE string VALUE 'ATTA'.
    FIELD-SYMBOLS: <ls_link> LIKE LINE OF lt_links.

    ls_gos_doc = iv_gos_attachment_id.

* Se obtiene el tipo de relacion
    CASE ls_gos_doc-objtp.

      WHEN lc_url. "--> Url
        lv_rel_typ = lc_url.

      WHEN lc_raw. "--> Nota
        lv_rel_typ = lc_note.

      WHEN lc_ext. "--> Anexo
        lv_rel_typ = lc_atta.
      WHEN OTHERS.

    ENDCASE.

    ls_object-instid = CONV sibfboriid( ls_gos_doc-qmnum ).
    ls_object-typeid = iv_sap_object.
    ls_object-catid  = 'BO'.

    ls_relat-sign   = 'I'.
    ls_relat-option = 'EQ'.
    ls_relat-low    = lv_rel_typ.
    APPEND ls_relat TO lt_relat.

* Se leen todos los enlaces del tipo del documento que se quiere borrar
    CALL METHOD cl_binary_relation=>read_links
      EXPORTING
        is_object           = ls_object
        it_relation_options = lt_relat
      IMPORTING
        et_links            = lt_links.

    lv_objkey = ls_gos_doc(34).

    READ TABLE lt_links ASSIGNING <ls_link> WITH KEY instid_b = lv_objkey.
* Se borra el enlace
    IF sy-subrc IS INITIAL.

      ls_object_a-instid = <ls_link>-instid_a.
      ls_object_a-typeid = <ls_link>-typeid_a.
      ls_object_a-catid  = <ls_link>-catid_a.
      ls_object_b-instid = <ls_link>-instid_b.
      ls_object_b-typeid = <ls_link>-typeid_b.
      ls_object_b-catid  = <ls_link>-catid_b.

      CALL METHOD cl_binary_relation=>delete_link
        EXPORTING
          is_object_a = ls_object_a
          is_object_b = ls_object_b
          ip_reltype  = <ls_link>-reltype.

      ls_folder_id-objtp = ls_gos_doc-foltp.
      ls_folder_id-objyr = ls_gos_doc-folyr.
      ls_folder_id-objno = ls_gos_doc-folno.
      ls_object_id-objtp = ls_gos_doc-objtp.
      ls_object_id-objyr = ls_gos_doc-objyr.
      ls_object_id-objno = ls_gos_doc-objno.

*   Se borra el documento fisico
      CALL FUNCTION 'SO_OBJECT_DELETE'
        EXPORTING
          folder_id                  = ls_folder_id
          object_id                  = ls_object_id
        EXCEPTIONS
          communication_failure      = 1
          folder_not_empty           = 2
          folder_not_exist           = 3
          folder_no_authorization    = 4
          forwarder_not_exist        = 5
          object_not_exist           = 6
          object_no_authorization    = 7
          operation_no_authorization = 8
          owner_not_exist            = 9
          substitute_not_active      = 10
          substitute_not_defined     = 11
          system_failure             = 12
          x_error                    = 13
          OTHERS                     = 14.

      IF sy-subrc IS NOT INITIAL.
*     Error eliminando el documento
        es_return-type = zif_pm_data=>cs_msg_type-error.
        MESSAGE ID zif_pm_data=>cs_message_id TYPE zif_pm_data=>cs_msg_type-error NUMBER 003 INTO es_return-message.

      ENDIF.

    ELSE.
*   Error
      es_return-type = zif_pm_data=>cs_msg_type-error.
      MESSAGE ID zif_pm_data=>cs_message_id TYPE zif_pm_data=>cs_msg_type-error NUMBER 003 INTO es_return-message.
    ENDIF.
    COMMIT WORK.

  ENDMETHOD.


  METHOD delete_tmp_attachments.
    DATA ls_tmp_file TYPE ts_tmp_file.

    ls_tmp_file = iv_tmp_file.

    IF ls_tmp_file IS NOT INITIAL.
      DELETE FROM zpm_t_tmp_atta
        WHERE guid EQ ls_tmp_file-guid AND
              counter   EQ ls_tmp_file-counter.

      IF sy-subrc IS NOT INITIAL.
      es_return-type = zif_pm_data=>cs_msg_type-error.
      MESSAGE ID zif_pm_data=>cs_message_id TYPE zif_pm_data=>cs_msg_type-error NUMBER 004 INTO es_return-message.
        EXIT.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method GENERATE_TMP_GUID.

  CL_RECA_GUID=>guid_create( IMPORTING ed_guid_32 = rv_guid ).

  endmethod.


  method GET_EXTENSION_FROM_FILENAME.

*    IF ls_file-extension IS INITIAL.
    SPLIT IV_FILENAME AT '.' INTO TABLE DATA(lt_split).
    DESCRIBE TABLE lt_split LINES DATA(lv_lines).
    READ TABLE lt_split ASSIGNING FIELD-SYMBOL(<lv_split>) INDEX lv_lines.
    IF sy-subrc IS INITIAL.
      EV_EXTENSION = <lv_split>.
      UNASSIGN <lv_split>.
      DELETE lt_split index lv_lines.
    ENDIF.

    LOOP AT lt_split ASSIGNING <lv_split>.
      IF sy-tabix EQ 1.
        EV_NAME = <lv_split>.
      ELSE.
        EV_NAME = |{ EV_NAME }.{ <lv_split> }|.
      ENDIF.
    ENDLOOP.

  endmethod.


  METHOD get_gos_attachment_content.

    DATA: ls_doc           TYPE sofolenti1,
          lt_hex           TYPE solix_tab,
          lt_object_header TYPE TABLE OF solisti1,
          lv_mimetype      TYPE w3conttype.

    CONSTANTS: lc_mimetype_gos TYPE string VALUE '&SO_CONTTYPE='.

    CALL FUNCTION 'SO_DOCUMENT_READ_API1'
      EXPORTING
        document_id                = iv_gos_attachment_id
      IMPORTING
        document_data              = ls_doc
      TABLES
        object_header              = lt_object_header
        contents_hex               = lt_hex
      EXCEPTIONS
        document_id_not_exist      = 1
        operation_no_authorization = 2
        x_error                    = 3
        OTHERS                     = 4.

    LOOP AT lt_hex ASSIGNING FIELD-SYMBOL(<ls_hex>).
      CONCATENATE ms_publish_attachment-content
                  <ls_hex>-line
                  INTO ms_publish_attachment-content IN BYTE MODE.
    ENDLOOP.

*   Se busca el mimetype en los atributos del GOS
    LOOP AT lt_object_header ASSIGNING FIELD-SYMBOL(<ls_content_header>) WHERE line CS lc_mimetype_gos.
      ms_publish_attachment-mimetype = <ls_content_header>-line.
      REPLACE lc_mimetype_gos IN ms_publish_attachment-mimetype WITH ''.
    ENDLOOP.

*   Si el mimetype no se encuentra en los atributos del GOS se busca a partir de la extensión
    IF ms_publish_attachment-mimetype IS INITIAL.
      CALL FUNCTION 'SDOK_MIMETYPE_GET'
        EXPORTING
          extension = ls_doc-obj_type
        IMPORTING
          mimetype  = lv_mimetype.

      ms_publish_attachment-mimetype = lv_mimetype.
    ENDIF.

    ms_publish_attachment-extension = ls_doc-obj_type.

  ENDMETHOD.


  METHOD get_gos_attachment_list.

    DATA: lr_instid TYPE RANGE OF sibfboriid,
          lr_foltp  TYPE RANGE OF so_fol_tp,
          lr_folyr  TYPE RANGE OF so_fol_yr,
          lr_folno  TYPE RANGE OF so_fol_no,
          lr_doctp  TYPE RANGE OF so_doc_tp,
          lr_docyr  TYPE RANGE OF so_doc_yr,
          lr_docno  TYPE RANGE OF so_doc_no.

    CHECK ir_instid IS NOT INITIAL.

    lr_instid = CORRESPONDING #( ir_instid ).

*   Relacion documento con anexos
    SELECT instid_a typeid_a catid_a instid_b AS document_id typeid_b
      FROM srgbtbrel
      INTO CORRESPONDING FIELDS OF TABLE mt_gos_attachments
      WHERE instid_a IN lr_instid AND
            typeid_a EQ iv_type_id.

    CHECK sy-subrc IS INITIAL.

    IF  iv_complete_data IS NOT INITIAL.

      LOOP AT mt_gos_attachments ASSIGNING FIELD-SYMBOL(<ls_gos_attachment>).
        <ls_gos_attachment>-foltp = <ls_gos_attachment>-document_id+0(3).
        <ls_gos_attachment>-folyr = <ls_gos_attachment>-document_id+3(2).
        <ls_gos_attachment>-folno = <ls_gos_attachment>-document_id+5(12).
        <ls_gos_attachment>-doctp = <ls_gos_attachment>-document_id+17(3).
        <ls_gos_attachment>-docyr = <ls_gos_attachment>-document_id+20(2).
        <ls_gos_attachment>-docno = <ls_gos_attachment>-document_id+22(34).

        lr_foltp = VALUE #( BASE lr_foltp ( sign = 'I' option = 'EQ' low = <ls_gos_attachment>-foltp ) ).
        lr_folyr = VALUE #( BASE lr_folyr ( sign = 'I' option = 'EQ' low = <ls_gos_attachment>-folyr ) ).
        lr_folno = VALUE #( BASE lr_folno ( sign = 'I' option = 'EQ' low = <ls_gos_attachment>-folno ) ).
        lr_doctp = VALUE #( BASE lr_doctp ( sign = 'I' option = 'EQ' low = <ls_gos_attachment>-doctp ) ).
        lr_docyr = VALUE #( BASE lr_docyr ( sign = 'I' option = 'EQ' low = <ls_gos_attachment>-docyr ) ).
        lr_docno = VALUE #( BASE lr_docno ( sign = 'I' option = 'EQ' low = <ls_gos_attachment>-docno ) ).
      ENDLOOP.

*     Get Document Info
      SELECT foltp, folyr, folno, doctp, docyr, docno,
             crdat AS creation_date, crtim AS creation_time, docdes AS document_desc, file_ext
             FROM v_sofc
             INTO TABLE @DATA(lt_sofc)
             WHERE foltp IN @lr_foltp AND
                   folyr IN @lr_folyr AND
                   folno IN @lr_folno AND
                   doctp IN @lr_doctp AND
                   docyr IN @lr_docyr AND
                   docno IN @lr_docno.

*     Se recupera el nombre de usuario
      SELECT  objtp, objyr, objno, ownnam AS creation_user
        FROM sood
        INTO TABLE @DATA(lt_sood)
              WHERE objtp IN @lr_doctp AND
                   objyr IN @lr_docyr AND
                   objno IN @lr_docno.

*        SOOD MANDT = SOFM  MANDT
*SOOD OBJTP = SOFM  DOCTP
*SOOD OBJYR = SOFM  DOCYR
*SOOD OBJNO = SOFM  DOCNO

*      SELECT OWNNO *
*             FROM v_sofc
*             INTO TABLE @DATA(lt_sofc)
*             WHERE foltp IN @lr_foltp AND
*                   folyr IN @lr_folyr AND
*                   folno IN @lr_folno AND
*                   doctp IN @lr_doctp AND
*                   docyr IN @lr_docyr AND
*                   docno IN @lr_docno.


      LOOP AT mt_gos_attachments ASSIGNING <ls_gos_attachment>.

        ASSIGN lt_sofc[ foltp = <ls_gos_attachment>-foltp
                        folyr = <ls_gos_attachment>-folyr
                        folno = <ls_gos_attachment>-folno
                        doctp = <ls_gos_attachment>-doctp
                        docyr = <ls_gos_attachment>-docyr
                        docno = <ls_gos_attachment>-docno ] TO FIELD-SYMBOL(<ls_sofc>).
        IF <ls_sofc> IS ASSIGNED.
          <ls_gos_attachment>-creation_date = <ls_sofc>-creation_date.
          <ls_gos_attachment>-creation_time = <ls_sofc>-creation_time.
          <ls_gos_attachment>-document_desc = <ls_sofc>-document_desc.
          <ls_gos_attachment>-file_ext = <ls_sofc>-file_ext.
*          <ls_gos_attachment> = CORRESPONDING #( <ls_sofc> ).
          UNASSIGN <ls_sofc>.
        ENDIF.

        ASSIGN lt_sood[ objtp = <ls_gos_attachment>-doctp
                        objyr = <ls_gos_attachment>-docyr
                        objno = <ls_gos_attachment>-docno ] TO FIELD-SYMBOL(<ls_sood>).
        IF <ls_sood> IS ASSIGNED.
          <ls_gos_attachment>-creation_user = <ls_sood>-creation_user.
          UNASSIGN <ls_sood>.
        ENDIF.

        <ls_gos_attachment>-mimetype = get_mimetype_from_extension( CONV char04( <ls_gos_attachment>-file_ext ) ).
        <ls_gos_attachment>-ui5_icon = get_ui5_icon_from_mimetype( <ls_gos_attachment>-mimetype ).

      ENDLOOP.

    ENDIF.

    rt_gos_attachments = CORRESPONDING #( mt_gos_attachments ).

  ENDMETHOD.


  METHOD GET_GOS_ATTACHMENT_URL.
    get_gos_attachment_content( iv_gos_attachment_id ).

    rv_url = publish_attachment( ).
  ENDMETHOD.


  METHOD get_mimetype_from_extension.

    CALL FUNCTION 'SDOK_MIMETYPE_GET'
      EXPORTING
        extension = iv_extension
*       X_USE_LOCAL_REGISTRY       =
      IMPORTING
        mimetype  = rv_mimetype.

  ENDMETHOD.


  METHOD get_tmp_attachments.

    CHECK iv_guid IS NOT INITIAL.

    SELECT *
      FROM zpm_t_tmp_atta
      INTO TABLE rt_tmp_attach
      WHERE guid EQ iv_guid.

  ENDMETHOD.


  method GET_UI5_ICON_FROM_MIMETYPE.

*       Aunque esto es de front lo ponemos aqui para compartirlo en las aplicaciones
        CASE IV_MIMETYPE.
          WHEN 'application/vnd.openxmlformats-officedocument.wordprocessingml.document' or 'application/msword'.
            rv_ui5_icon = 'sap-icon://doc-attachment'.
          WHEN 'application/pdf'.
            rv_ui5_icon = 'sap-icon://pdf-attachment'.
          WHEN 'application/vnd.ms-excel'.
            rv_ui5_icon = 'sap-icon://excel-attachment'.
          WHEN 'image/gif' or 'image/png' or 'image/jpeg' or 'image/bmp' or 'image/webp'.
            rv_ui5_icon = 'sap-icon://attachment-photo'.
          WHEN OTHERS.
            rv_ui5_icon = 'sap-icon://attachment'.
        ENDCASE.

  endmethod.


  METHOD publish_attachment.

    DATA(lv_rfc) = zcl_dso_constants=>obtener_constantes('GATEWAY_RFC' ).

*   Para poder ver el PDF desde fuera de la red de CAPSA, es necesario publicarlo en el Gateway
    CALL FUNCTION 'ZPM_PUBLISH_DOCUMENT' DESTINATION lv_rfc
      EXPORTING
        iv_xstring   = ms_publish_attachment-content
        iv_extension = ms_publish_attachment-extension
        iv_mimetype  = ms_publish_attachment-mimetype
      IMPORTING
        ev_url       = rv_url.

    DATA(lv_ext_host) = zcl_dso_constants=>obtener_constantes('GATEWAY_EXT_HOST' ).

    SPLIT rv_url AT '/' INTO TABLE DATA(lt_string).

*   Se borran los 3 primeros registros
    DELETE lt_string INDEX 1.
    DELETE lt_string INDEX 1.
    DELETE lt_string INDEX 1.

    INSERT lv_ext_host INTO lt_string INDEX 1.

    CONCATENATE LINES OF lt_string INTO rv_url SEPARATED BY '/'.

  ENDMETHOD.


  METHOD save_tmp_attachment.

    DATA: ls_bbdd_tmp_attachment TYPE zpm_t_tmp_atta.

    DATA(lt_attachments) = get_tmp_attachments( iv_guid = iv_guid ).

    SORT lt_attachments BY counter DESCENDING.

    READ TABLE lt_attachments INTO DATA(ls_last_attachment) INDEX 1.

    ls_bbdd_tmp_attachment = CORRESPONDING #( is_file ).
    ls_bbdd_tmp_attachment-guid = iv_guid.
    ls_bbdd_tmp_attachment-counter = COND #( WHEN ls_last_attachment IS INITIAL THEN '0000'
                                             ELSE ls_last_attachment-counter + 1 ).

    get_extension_from_filename( EXPORTING iv_filename = ls_bbdd_tmp_attachment-filename
                                 IMPORTING ev_extension = ls_bbdd_tmp_attachment-extension
                                           ev_name = ls_bbdd_tmp_attachment-name ).

    ls_bbdd_tmp_attachment-mimetype = get_mimetype_from_extension( ls_bbdd_tmp_attachment-extension ).

    INSERT zpm_t_tmp_atta FROM ls_bbdd_tmp_attachment.
    IF sy-subrc IS NOT INITIAL.
      RAISE error.
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD upload_file_2_gos.

    DATA: lv_folder_id     TYPE soodk,
          lt_bin           TYPE TABLE OF solix,
*          lv_xstring    TYPE xstring,
*          lv_bin_length TYPE i,
          ls_file          TYPE ts_file,
*          lv_ext           TYPE char100,
          lv_document_type TYPE so_obj_tp,
          ls_document_data TYPE sodocchgi1,
          ls_document_info TYPE sofolenti1,
          ls_object_a      TYPE sibflporb,
          ls_object_b      TYPE sibflporb,
*          lv_mimetype      TYPE w3conttype,
          lt_object_header TYPE TABLE OF solisti1.
    .

*    FIELD-SYMBOLS: <ls_object_header> LIKE LINE OF lt_object_header.

*    CLEAR: es_return, es_document_info.
    ls_file = CORRESPONDING #( is_file ).
    CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
      EXPORTING
        region    = 'B'
      IMPORTING
        folder_id = lv_folder_id.

*    lv_xstring = IS_FILE-contenido.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = ls_file-content
      IMPORTING
        output_length = ls_file-bin_length
      TABLES
        binary_tab    = lt_bin.

    IF ls_file-extension IS INITIAL.
      get_extension_from_filename( EXPORTING iv_filename = ls_file-filename
                                   IMPORTING ev_extension = ls_file-extension
                                             ev_name = ls_file-name ).
    ENDIF.

    IF ls_file-mimetype IS INITIAL.
      ls_file-mimetype = get_mimetype_from_extension( ls_file-extension ).
    ENDIF.


    ls_document_data-obj_name  = 'ATTACHMENT'.
    ls_document_data-obj_descr = ls_file-name.
    ls_document_data-obj_langu = mv_langu.
    ls_document_data-doc_size  = ls_file-bin_length.
    lv_document_type           = ls_file-extension.

    APPEND INITIAL LINE TO lt_object_header ASSIGNING FIELD-SYMBOL(<ls_object_header>).
    <ls_object_header>-line =  |&SO_FILENAME={ ls_file-filename }|.

    CALL FUNCTION 'SO_DOCUMENT_INSERT_API1'
      EXPORTING
        folder_id                  = lv_folder_id
        document_data              = ls_document_data
        document_type              = lv_document_type
      IMPORTING
        document_info              = ls_document_info
      TABLES
        object_header              = lt_object_header
        contents_hex               = lt_bin
      EXCEPTIONS
        folder_not_exist           = 1
        document_type_not_exist    = 2
        operation_no_authorization = 3
        parameter_error            = 4
        x_error                    = 5
        enqueue_error              = 6
        OTHERS                     = 7.
    IF sy-subrc IS NOT INITIAL.
      RAISE error.
    ENDIF.

    ls_object_a-instid = iv_object_id.
    ls_object_a-typeid = iv_sap_object.
    ls_object_a-catid  = 'BO'.

    ls_object_b-instid = ls_document_info-doc_id.
    ls_object_b-typeid = 'MESSAGE'.
    ls_object_b-catid  = 'BO'.

    TRY.
        CALL METHOD cl_binary_relation=>create_link
          EXPORTING
            is_object_a = ls_object_a
            is_object_b = ls_object_b
            ip_reltype  = 'ATTA'.

        " Si no hay errores se devuelve la información con el documento generado
*        es_document_info = ls_document_info.

      CATCH cx_root.
        RAISE error.
    ENDTRY.

    IF iv_commit IS NOT INITIAL.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
