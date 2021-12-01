class ZCL_NAD_APP_PM_NOTIF_TPL definition
  public
  final
  create public .

public section.

  interfaces /NEPTUNE/IF_NAD_SERVER .

  types:
      "User name and id.
    BEGIN OF ty_user,
        uname    TYPE uname,
        fullname TYPE string,
      END OF ty_user .
  types:
          "Search parameter values.
          "Add new field to this type if you need more parameters in you app.
          "If added here they will be available in NAD to assign to the search screen.
    BEGIN OF ty_search_params,
                  qmnum             TYPE qmel-qmnum,
                  qmart             TYPE qmel-qmart,
                  tplnr             TYPE tplnr,
                  ername            TYPE qmel-ernam,
                  erdat_f           TYPE qmel-erdat,
                  erdat_t           TYPE qmel-erdat,
                  get_attachments   TYPE ddbool_d,
                  app_field_enabled TYPE ddbool_d,
                  filter            TYPE string,
                  count             TYPE string,
                  unit_type_s       TYPE string,
                  unit_type_p       TYPE string,
            END OF ty_search_params .
  types:
          "Notification type values:
    BEGIN OF ty_notif_type,
                   qmart    TYPE qmart,
                   qmartx   TYPE qmartx,
                 END OF ty_notif_type .
  types:
          "Priority types:
    BEGIN OF ty_priority,
                   priox    TYPE priox,
                   priokx   TYPE priokx,
                 END OF ty_priority .
  types:
          "Phase description
    BEGIN OF ty_phase,
                  phase TYPE valpos,
                  desc  TYPE string,
                 END OF ty_phase .
  types:
          "Planner group
    BEGIN OF ty_pg,
                  iwerk TYPE iwerk,
                  ingrp TYPE ingrp,
                  innam TYPE innam,
                END OF ty_pg .
  types:
          "Maint. plants
    BEGIN OF ty_mp,
                  werks TYPE werks_d,
                  name1 TYPE string,
                  iwerk TYPE iwerk,
                END OF ty_mp .
  types:
          "Locations
    BEGIN OF ty_loc,
                  werks TYPE werks_d,
                  stort TYPE pmloc,
                  ktext TYPE string,
                END OF ty_loc .
  types:
          "Planning Plants
    BEGIN OF ty_planplant,
                  iwerk TYPE iwerk,
                  name1 TYPE string,
                END OF ty_planplant .
  types:
    ty_planplant_tt TYPE SORTED TABLE OF ty_planplant
                        WITH NON-UNIQUE KEY iwerk .
  types:
          "Notificaton data:
    BEGIN OF ty_qmel,
                              qmnum             TYPE qmel-qmnum,
                              qmart             TYPE qmel-qmart,
                              qmtxt             TYPE qmel-qmtxt,
                              artpr             TYPE qmel-artpr,
                              priok             TYPE qmel-priok,
                              ernam             TYPE qmel-ernam,
                              erdat             TYPE qmel-erdat,
                              mzeit             TYPE qmel-mzeit,
                              qmdat             TYPE qmel-qmdat,
                              strmn             TYPE qmel-strmn,
                              strur             TYPE qmel-strur,
                              ltrmn             TYPE qmel-ltrmn,
                              ltrur             TYPE qmel-ltrur,
                              aufnr             TYPE qmel-aufnr,
                              objnr             TYPE qmel-objnr,
                              vkorg             TYPE qmel-vkorg,
                              qmgrp             TYPE qmel-qmgrp,
                              qmcod             TYPE qmel-qmcod,
                              qmnam             TYPE qmel-qmnam,
                              arbpl             TYPE qmel-arbpl,
                              phase             TYPE qmel-phase,
                              spart             TYPE spart,
                              vtweg             TYPE vtweg,
                              rbnr              TYPE rbnr,
                              tplnr             TYPE tplnr,
                              tplnr_t           TYPE string,
                              iwerk             TYPE iwerk,
                              swerk             TYPE swerk,
                              swerk_t           TYPE string,
                              ingrp             TYPE ingrp,
                              ingrp_t           TYPE string,
                              stort             TYPE stort,
                              stort_t           TYPE string,
                              text              TYPE string,
                              text_o            TYPE string,
                              otgrp             TYPE otgrp,
                              oteil             TYPE oteil,
                              fegrp             TYPE fegrp,
                              fecod             TYPE fecod,
                              fetxt             TYPE string,
                              urgrp             TYPE urgrp,
                              urcod             TYPE urcod,
                              urtxt             TYPE string,
                              txtcdot           TYPE string,
                              txtcdfe           TYPE string,
                              txtcdur           TYPE string,
                              status            TYPE string,
                              app_marker        TYPE string,
                              obj_new           TYPE ddbool_d,
                              dam_new           TYPE ddbool_d,
                              qmnum_old         TYPE qmel-qmnum,
                              app_field_enabled TYPE ddbool_d, "used to dynamically set enabled true/false on fields
                              app_cause_enabled TYPE ddbool_d, "used to dynamically set enabled true/false on cause fields
                              app_att_loaded    TYPE ddbool_d, "used to know if the attachments were loaded
                              app_att_count     TYPE int4,     "used to know how many attachments are available
                              app_unsaved       TYPE ddbool_d,
                              sync_status       TYPE string,
                            END OF ty_qmel .
  types:
    ty_t_qmel TYPE STANDARD TABLE OF ty_qmel .
  types:
    BEGIN OF ty_attachments,
                              qmnum    TYPE qmel-qmnum,
                              aufnr    TYPE aufnr,
                              instid_a TYPE sibfboriid,
                              instid_b TYPE sibfboriid,
                              crdat    TYPE so_dat_cr,
                              crtim    TYPE so_tim_cr,
                              docdes   TYPE so_obj_des,
                              file_ext TYPE file_ext,
                              content  TYPE string,
                        END OF ty_attachments .
  types:
    ty_attachments_tt TYPE STANDARD TABLE OF ty_attachments .
  types:
    BEGIN OF ty_v_qpcd_f4,
                                  katalogart  TYPE v_qpcd_f4-katalogart,
                                  codegruppe  TYPE v_qpcd_f4-codegruppe,
                                  code        TYPE v_qpcd_f4-code,
                                  kurztext    TYPE v_qpcd_f4-kurztext,
                                  sprache     TYPE v_qpcd_f4-sprache,
                            END OF ty_v_qpcd_f4 .
  types:
          "Sync messages
    BEGIN OF ty_sync_message,
                      qmnum   TYPE string,
                      type    TYPE bapi_mtype,
                      message	TYPE bapi_msg,
                      state   TYPE string,
            END OF ty_sync_message .
  types:
    BEGIN OF ty_iflot,
            tplnr TYPE tplnr,
            pltxt TYPE pltxt,
            point TYPE point,
            objnr TYPE j_objnr,
            tplma TYPE tplma,
            eqfnr TYPE eqfnr,
            filter TYPE string,
            iwerk TYPE iwerk,
            work_cntr TYPE arbpl,
            barnr TYPE string,
          END OF ty_iflot .

  data:
    mt_iflot TYPE STANDARD TABLE OF ty_iflot .
  data MS_IFLOT type TY_IFLOT .
  data MS_SEARCH_PARAMS type TY_SEARCH_PARAMS .           "Model structure to hold the search parameters
  data:
    mt_notif_type       TYPE STANDARD TABLE OF ty_notif_type .           "Model table that will contain possible notification types.
  data:
    mt_priority_values  TYPE STANDARD TABLE OF ty_priority .             "Model table that will contain possilbe priority values.
  data MS_NOTIFICATION type TY_QMEL .
  data:
    mt_notification     TYPE STANDARD TABLE OF ty_qmel .
  data:
    mt_notification_off TYPE STANDARD TABLE OF ty_qmel .                  "Notifications stored offline (changed and new)
  data:
    mt_phase_desc       TYPE STANDARD TABLE OF ty_phase .
  data:
    mt_attachments      TYPE STANDARD TABLE OF ty_attachments .
  data:
    mt_attachments_add  TYPE STANDARD TABLE OF ty_attachments .
  data MS_ATTACHMENT type TY_ATTACHMENTS .
  data:
    mt_code_data        TYPE STANDARD TABLE OF ty_v_qpcd_f4 .
  data:
    mt_sync_message     TYPE STANDARD TABLE OF ty_sync_message .
  data MS_SYNC_MESSAGE type TY_SYNC_MESSAGE .
  data:
    mt_pg               TYPE STANDARD TABLE OF ty_pg .
  data:
    mt_mp               TYPE STANDARD TABLE OF ty_mp .
  data:
    mt_user        TYPE STANDARD TABLE OF ty_user .
  data:
    mt_loc              TYPE STANDARD TABLE OF ty_loc .
  constants GC_FLTYP type FLTYP value 'S'. "#EC NOTEXT
  data MT_PLANPLANT type TY_PLANPLANT_TT .
protected section.
private section.

  methods __GET_USERS .
  methods __GET_LOCATION .
  methods __GET_MAINT_PLANT .
  methods __GET_PLAN_GROUP .
  methods GET_ATTACHMENTS .
  methods GET_NOTIFICATIONS
    importing
      value(IV_VALUE) type STRING optional .
  methods HANDLE_OUTBOX .
  methods INITAPP .
  methods __GET_CODE_DATA .
  methods __GET_NOTIFICATION_TEXT
    importing
      !IV_TDID type STXH-TDID
      !IV_TDSPRAS type STXH-TDSPRAS
      !IV_TDNAME type STXH-TDNAME
      !IV_TDOBJECT type STXH-TDOBJECT
    returning
      value(RV_TEXT) type STRING .
  methods __GET_NOTIFICATION_TEXTS
    changing
      !CT_NOTIFICATION type TY_T_QMEL .
  methods __GET_NOTIF_TYPES .
  methods __GET_PHASE_DESC .
  methods __GET_PRIORITY_VALUES .
  methods __SAVE_NOTIFICATION .
  methods __SAVE_COMMENT
    importing
      !IV_QMNUM type QMNUM
      !IV_COMMENT type STRING .
  methods FUNCTIONAL_LOCATIONS .
  methods GET_NOTIFICATION_ATTACHMENTS .
  methods __GET_NOTIFICATION_ATTACHMENTS
    changing
      !CS_NOTIFICATION type TY_QMEL
      !CT_ATTACHMENTS type TY_ATTACHMENTS_TT .
  type-pools ABAP .
  methods __NOTIFICATION_HAS_ATTACHMENTS
    importing
      !IV_QMNUM type QMNUM
    returning
      value(RV_BOOLEAN) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_NAD_APP_PM_NOTIF_TPL IMPLEMENTATION.


method /NEPTUNE/IF_NAD_SERVER~HANDLE_ON_AJAX.
  case ajax_id.

    "Initialize the app
    when 'INIT'.
      initapp( ).

    "Functional locations - handle the F.L. in a seperate Ajax call since this slower search than rest of the init values.
    when 'FUNC_LOC'.
      functional_locations( ).

    "Search Notifications
    when 'SEARCH'.
      get_notifications( ).

    " Download the attachments (optional part of search)
    when 'SEARCH_ATTACH'.
      get_attachments( ).

    " Download the attachments for one notification only
    when 'GET_NOTIF_ATTACH'.
      get_notification_attachments( ).

    "Sunc offline changes:
    when 'SAVE_DATA'.
      handle_outbox( ).

  endcase.

endmethod.


method FUNCTIONAL_LOCATIONS.
  data: it_iflotx   type standard table of iflotx,
        wa_iflotx   like line of it_iflotx,
        wa_iflot    like line of mt_iflot,
        lv_tabix    type sy-tabix,
        lt_fariflot type sorted table of ty_iflot
                         with non-unique key tplma tplnr.

  data: ra_fltyp  type range of iflot-fltyp,
        wa_fltyp  like line of ra_fltyp,
        lv_tplnr  type string,
        lv_pltxt  type string.

  field-symbols: <ls_iflot> type ty_iflot.
*--------------------------------------------------------------------*
* GET DATA
*--------------------------------------------------------------------*


*  wa_fltyp-sign   = 'I'.
*  wa_fltyp-option = 'EQ'.
*  wa_fltyp-low    = gc_fltyp.
*  append wa_fltyp to ra_fltyp.


* Get IFLOT
  select tplnr tplma fltyp objnr iwerk
         from iflot
         into corresponding fields of table mt_iflot
         where fltyp in ra_fltyp.

* Get IFLOTX
  select *
         from iflotx
         into table it_iflotx
         where spras eq sy-langu.

* Sorting
  sort it_iflotx by tplnr.


*--------------------------------------------------------------------*
* BUILD DATA
*--------------------------------------------------------------------*
  loop at mt_iflot assigning <ls_iflot>.

    lv_tabix = sy-tabix.

    read table it_iflotx into wa_iflotx with key tplnr = <ls_iflot>-tplnr binary search.

    if sy-subrc eq 0.
      <ls_iflot>-pltxt = wa_iflotx-pltxt.
    endif.


    call function 'CONVERSION_EXIT_TPLNR_OUTPUT'
      exporting
        input  = <ls_iflot>-tplnr
      importing
        output = <ls_iflot>-tplnr.

    call function 'CONVERSION_EXIT_TPLNR_OUTPUT'
      exporting
        input  = <ls_iflot>-tplma
      importing
        output = <ls_iflot>-tplma.

  endloop.

  lt_fariflot[] = mt_iflot[].
  loop at lt_fariflot[] assigning <ls_iflot>.
    loop at lt_fariflot[] transporting no fields
      where tplma eq <ls_iflot>-tplnr.
      exit.
    endloop.
    if ( sy-subrc eq 0 ).
      <ls_iflot>-barnr = 'true'.
    else.
      <ls_iflot>-barnr = 'false'.
    endif.
  endloop.
  mt_iflot[] = lt_fariflot[]. " Autom. ordered by TPLMA and then by TPLNR which is ok for value help
endmethod.


  method GET_ATTACHMENTS.


    data: begin of lv_key,
        foltp type so_fol_tp,
        folyr type so_fol_yr,
        folno type so_fol_no,
        doctp type so_doc_tp,
        docyr type so_doc_yr,
        docno type so_doc_no,
    end of lv_key.

    types: begin of ty_instid,
          instid_a  type sibfboriid,
      end of ty_instid.

    data: it_instid  type standard table of ty_instid,
          it_hex     type standard table of solix,
          wa_instid  type ty_instid,
          wa_sofc    type v_sofc,
          wa_hex     like line of it_hex,
          wa_doc     type sofolenti1,
          lv_xstring type xstring,
          lv_doc_id  type sofolenti1-doc_id,
          lv_pdf     type xstring,
          lv_tabix   type sy-tabix.

    field-symbols: <lw_qmel> like line of mt_notification.

* Any Notifications ?
    check mt_notification is not initial.

    loop at mt_notification assigning <lw_qmel>.
      <lw_qmel>-app_att_loaded = abap_true.
      <lw_qmel>-sync_status    = 'None'.
      wa_instid-instid_a = <lw_qmel>-qmnum.
      append wa_instid to it_instid.
      clear  wa_instid.
    endloop.

* Attachment
    select *
           from srgbtbrel
           into corresponding fields of table mt_attachments
           for all entries in it_instid
           where instid_a eq it_instid-instid_a
             and typeid_a eq 'BUS2038'.

    loop at mt_attachments into ms_attachment.

      lv_tabix = sy-tabix.
      lv_key   = ms_attachment-instid_b.

*   Get Document Info
      select single *
             from v_sofc
             into wa_sofc
             where foltp eq lv_key-foltp
               and folyr eq lv_key-folyr
               and folno eq lv_key-folno
               and doctp eq lv_key-doctp
               and docyr eq lv_key-docyr
               and docno eq lv_key-docno.

      move-corresponding wa_sofc to ms_attachment.
      ms_attachment-qmnum = ms_attachment-instid_a.
      translate ms_attachment-file_ext to upper case.

      if ms_attachment-file_ext eq 'JPE'.
        ms_attachment-file_ext = 'JPG'.
      endif.

      lv_doc_id = ms_attachment-instid_b.
      call function 'SO_DOCUMENT_READ_API1'
        exporting
          document_id                = lv_doc_id
        importing
          document_data              = wa_doc
        tables
          contents_hex               = it_hex
        exceptions
          document_id_not_exist      = 1
          operation_no_authorization = 2
          x_error                    = 3
          others                     = 4.

*   Build String
      clear lv_xstring.
      loop at it_hex into wa_hex.
        concatenate lv_xstring
                    wa_hex-line
                    into lv_xstring in byte mode.
      endloop.

*   Base64 Encoding
      call function 'SCMS_BASE64_ENCODE_STR'
        exporting
          input  = lv_xstring
        importing
          output = ms_attachment-content.
      " ms_attachment-aufnr = ''. "TODO
      modify mt_attachments from ms_attachment index lv_tabix.

    endloop.

*    et_qmel_gos = mt_attachments.

  endmethod.                    "GET_ATTACHMENTS


method GET_NOTIFICATIONS.
  types:
  begin of tp_v_qpcd_f4_key,
        katalogart type v_qpcd_f4-katalogart,
        codegruppe type v_qpcd_f4-codegruppe,
        code       type v_qpcd_f4-code,
      end of tp_v_qpcd_f4_key,
      tp_t_v_qpcd_f4_key type hashed table of tp_v_qpcd_f4_key with unique key table_line,

      begin of tp_v_qpcd_f4,
        katalogart type v_qpcd_f4-katalogart,
        codegruppe type v_qpcd_f4-codegruppe,
        code       type v_qpcd_f4-code,
        kurztext   type v_qpcd_f4-kurztext,
      end of tp_v_qpcd_f4,
      tp_t_v_qpcd_f4 type sorted table of tp_v_qpcd_f4 with non-unique key katalogart codegruppe code,

       begin of tp_viqmur,
         qmnum type viqmur-qmnum,
         urgrp type viqmur-urgrp,
         urcod type viqmur-urcod,
         urtxt type viqmur-urtxt,
       end of tp_viqmur,
       tp_t_viqmur type sorted table of tp_viqmur with non-unique key qmnum,

       begin of tp_viqmfe,
         qmnum type viqmfe-qmnum,
         otgrp type viqmfe-otgrp,
         oteil type viqmfe-oteil,
         fegrp type viqmfe-fegrp,
         fecod type viqmfe-fecod,
         fetxt type viqmfe-fetxt,
       end of tp_viqmfe,
       tp_t_viqmfe type sorted table of tp_viqmfe with non-unique key qmnum,

             begin of tp_jest,
        objnr type jest-objnr,
        stat  type jest-stat,
      end of tp_jest,
      tp_t_jest type sorted table of tp_jest with non-unique key objnr,

          begin of tp_tj02t,
        istat type tj02t-istat,
        spras type tj02t-spras,
        txt04 type tj02t-txt04,
        txt30 type tj02t-txt30,
      end of tp_tj02t,
      tp_t_tj02t type hashed table of tp_tj02t with unique key istat spras.


  "Define ranges for the available search parameters to be used in the select
  data:
  ra_tplnr         type range of  viqmelst-tplnr,
  wa_tplnr         like line of   ra_tplnr,
  ra_erdat         type range of  qmel-erdat,
  wa_erdat         like line  of  ra_erdat,
  ra_qmart         type range of  qmel-qmart,
  wa_qmart         like line  of  ra_qmart,
  ra_ernam         type range of  qmel-ernam,
  wa_ernam         like line  of  ra_ernam,
  ra_qmnum         type range of  qmel-qmnum,
  wa_qmnum         like line  of  ra_qmnum,
  it_viqmfe        type           tp_t_viqmfe,
  it_viqmur        type           tp_t_viqmur,
  wa_viqmfe        like line of it_viqmfe,
  wa_viqmur        like line of it_viqmur,
  lt_qpcd_f4_key   type tp_t_v_qpcd_f4_key,
  lt_qpcd_f4       type tp_t_v_qpcd_f4,
  wa_qpcd_f4_key   like line of lt_qpcd_f4_key,
  lt_jest          type tp_t_jest,
  it_tj02t         type tp_t_tj02t,
  lv_len           type i,
  lv_year_restr    type sydatum.

  field-symbols:
  <notif>        like line of mt_notification,
  <fw_qmfe>      like line of it_viqmfe,
  <fw_qmur>      like line of it_viqmur,
  <fw_qpcd_f4>   like line of lt_qpcd_f4,
  <fw_jest>      like line of lt_jest,
  <fw_tj02t>     like line of it_tj02t.

  free mt_attachments[]. " Makes sure that the decoupling notification/attachments works

  "If Notification number is given only search for this.
  if ms_search_params-qmnum is not initial.
    wa_qmnum-sign   = 'I'.
    lv_len = strlen( ms_search_params-qmnum ).
    if ( lv_len EQ 12 ).
      wa_qmnum-option = 'EQ'.
      wa_qmnum-low    = ms_search_params-qmnum.
    else.
      wa_qmnum-option = 'CP'.
      wa_qmnum-low    = '*{ ms_search_params-qmnum }*'.
    endif.
    append wa_qmnum to ra_qmnum.
  else.
    wa_erdat-sign   = 'I'.
    if not ( ( ms_search_params-erdat_f is initial )
             or
             ( ms_search_params-erdat_t is initial ) ).
      "
      " BOTH are filled in
      wa_erdat-option = 'BT'.
      wa_erdat-low    = ms_search_params-erdat_f.
      wa_erdat-high   = ms_search_params-erdat_t.
      append wa_erdat to ra_erdat.
    elseif not ( ms_search_params-erdat_f is initial ).
      "
      " only FROM is filled in
      wa_erdat-option = 'GE'.
      wa_erdat-low    = ms_search_params-erdat_f.
      append wa_erdat to ra_erdat.
    elseif not ( ms_search_params-erdat_f is initial ).
        "
        " only TO is filled in, but the YEAR restriction is imposed
        wa_erdat-option = 'BT'.
        "
        " Calculates the year restriction
        CALL FUNCTION 'HR_99S_DATE_MINUS_TIME_UNIT'
          EXPORTING
            i_idate                     = ms_search_params-erdat_t
            i_time                      = 1
            i_timeunit                  = 'Y'
          IMPORTING
            O_IDATE                     = wa_erdat-low.
        "
        " Sets the end date
        wa_erdat-high   = ms_search_params-erdat_t.
        append wa_erdat to ra_erdat.
    else.
      "
      " None is filled in. Calculates the YEAR restriction from today
        wa_erdat-option = 'GE'.
        "
        " Calculates the year restriction
        CALL FUNCTION 'HR_99S_DATE_MINUS_TIME_UNIT'
          EXPORTING
            i_idate                     = sy-datum
            i_time                      = 1
            i_timeunit                  = 'Y'
          IMPORTING
            O_IDATE                     = wa_erdat-low.
        "
        " Sets the end date
        wa_erdat-high   = ms_search_params-erdat_t.
        append wa_erdat to ra_erdat.
    endif.
  endif.

* TPLNR - Functional Locations
  if not ms_search_params-tplnr is initial.
    wa_tplnr-sign   = 'I'.
    wa_tplnr-option = 'CP'.
    wa_tplnr-low    = '{ ms_search_params-tplnr }*'.
    append wa_tplnr to ra_tplnr.
  endif.

* QMART - Notification type
  if ms_search_params-qmart is not initial.
    wa_qmart-sign   = 'I'.
    wa_qmart-option = 'EQ'.
    wa_qmart-low    = ms_search_params-qmart.
    append wa_qmart to ra_qmart.
  endif.

* ERNAM - Created by
  if ms_search_params-ername is not initial.
    wa_ernam-sign   = 'I'.
    wa_ernam-option = 'EQ'.
    wa_ernam-low    = ms_search_params-ername.
    append wa_ernam to ra_ernam.
  endif.


* Get Notification data:
  "From view VIQMELST since this contains most data needed and is faster than using BAPI/FM
  select distinct
         qmnum qmart qmtxt artpr priok ernam erdat mzeit qmdat strmn strur ltrmn ltrur aufnr objnr vkorg qmgrp qmcod qmnam arbpl phase
         swerk ingrp stort tplnr iwerk rbnr
         spart vkorg vtweg
    from viqmelst
    into corresponding fields of table mt_notification
   where qmnum in ra_qmnum
     and tplnr in ra_tplnr
     and qmdat in ra_erdat
     and erdat in ra_erdat
     and ernam in ra_ernam
     and qmart in ra_qmart.


  __get_notification_texts( changing ct_notification = mt_notification ).

  "Get data for object part and cause codes:
* Get VIQMFE
  select qmnum otgrp oteil fegrp fecod fetxt
    from viqmfe
    into corresponding fields of table it_viqmfe
     for all entries in mt_notification
   where qmnum eq mt_notification-qmnum.
*      and KZMLA eq'E'.

* Get VIQMUR
  select qmnum urgrp urcod urtxt
    from viqmur
    into corresponding fields of table it_viqmur
     for all entries in mt_notification
   where qmnum eq mt_notification-qmnum.


  "Object parts text
  loop at it_viqmfe assigning <fw_qmfe>.
    wa_qpcd_f4_key-katalogart = 'B'.
    wa_qpcd_f4_key-codegruppe = <fw_qmfe>-otgrp.
    wa_qpcd_f4_key-code       = <fw_qmfe>-oteil.

    collect wa_qpcd_f4_key into lt_qpcd_f4_key.

    wa_qpcd_f4_key-katalogart = 'C'.
    wa_qpcd_f4_key-codegruppe = <fw_qmfe>-fegrp.
    wa_qpcd_f4_key-code       = <fw_qmfe>-fecod.

    collect wa_qpcd_f4_key into lt_qpcd_f4_key.
  endloop.

  loop at it_viqmur assigning <fw_qmur>.
    wa_qpcd_f4_key-katalogart = '5'.
    wa_qpcd_f4_key-codegruppe = <fw_qmur>-urgrp.
    wa_qpcd_f4_key-code       = <fw_qmur>-urcod.

    collect wa_qpcd_f4_key into lt_qpcd_f4_key.
  endloop.


  if lt_qpcd_f4_key[] is not initial.
    select katalogart codegruppe code kurztext
      from v_qpcd_f4
      into table lt_qpcd_f4
       for all entries in lt_qpcd_f4_key
     where katalogart eq lt_qpcd_f4_key-katalogart
       and codegruppe eq lt_qpcd_f4_key-codegruppe
       and code       eq lt_qpcd_f4_key-code
       and sprache    eq 'E'.
  endif.


* Get JEST - Status for Notification
  select objnr stat
    from jest
    into table lt_jest
     for all entries in mt_notification
   where objnr eq mt_notification-objnr
     and inact eq space.

* Get TJ02T
  select istat spras txt04 txt30
    from tj02t
    into table it_tj02t
     for all entries in lt_jest
   where istat eq lt_jest-stat
     and spras eq sy-langu.

  "get additional data:
  refresh: mt_attachments[].
  loop at mt_notification assigning <notif>.
    "Get Func.Loc. text:
    select single pltxt from iflotx
      into <notif>-tplnr_t
      where tplnr eq <notif>-tplnr
      and   spras eq sy-langu.

    "Get maint plant text:
    select single name1 from t001w
      into <notif>-swerk_t
      where werks eq <notif>-swerk.

    "Get planner group text:
    select single innam from t024i
      into <notif>-ingrp_t
      where iwerk eq <notif>-iwerk
      and   ingrp eq <notif>-ingrp.

    "Get location text
    select single ktext from t499s
      into <notif>-stort_t
      where werks eq <notif>-swerk
      and   stand eq <notif>-stort.

*   VIQMFE
    clear wa_viqmfe.
    read table it_viqmfe into wa_viqmfe with table key qmnum = <notif>-qmnum.
    if sy-subrc = 0.
      move-corresponding wa_viqmfe to <notif>.
    endif.

*   VIQMUR
    clear wa_viqmur.
    read table it_viqmur into wa_viqmur with table key qmnum = <notif>-qmnum.
    if sy-subrc = 0.
      move-corresponding wa_viqmur to <notif>.
    endif.

* code or damage
      if <notif>-otgrp is initial.
        <notif>-obj_new = abap_true.
      else.
        <notif>-obj_new = abap_false.
      endif.

      if <notif>-fegrp is initial.
        <notif>-dam_new = abap_true.
      else.
        <notif>-dam_new = abap_false.
      endif.

*   Get object texts
    if not <notif>-rbnr is initial.

      "Object parts text
      read table lt_qpcd_f4 assigning <fw_qpcd_f4> with table key katalogart = 'B' codegruppe = wa_viqmfe-otgrp code = wa_viqmfe-oteil.

      if sy-subrc is initial.
        <notif>-txtcdot = <fw_qpcd_f4>-kurztext.
      endif.

      "Damage text
      read table lt_qpcd_f4 assigning <fw_qpcd_f4> with table key katalogart = 'C' codegruppe = wa_viqmfe-fegrp code = wa_viqmfe-fecod.

      if sy-subrc is initial.
        <notif>-txtcdfe = <fw_qpcd_f4>-kurztext.
      endif.

      "Cause text
      read table lt_qpcd_f4 assigning <fw_qpcd_f4> with table key katalogart = '5' codegruppe = wa_viqmur-urgrp code = wa_viqmur-urcod.

      if sy-subrc is initial.
        <notif>-txtcdur = <fw_qpcd_f4>-kurztext.
      endif.

    endif.

*   Status
    loop at lt_jest assigning <fw_jest> where objnr eq <notif>-objnr.

      read table it_tj02t assigning <fw_tj02t> with table key istat = <fw_jest>-stat spras = sy-langu.

      if sy-subrc is initial.
        if not <notif>-status cs <fw_tj02t>-txt04.
          concatenate <notif>-status
                      <fw_tj02t>-txt04
                      into <notif>-status separated by space.
        endif.
      endif.

    endloop.

    "
    " Converts FunLoc InternalID to ExternalID
    CALL FUNCTION 'CONVERSION_EXIT_TPLNR_OUTPUT'
      EXPORTING
        INPUT         = <notif>-tplnr
      IMPORTING
        OUTPUT        = <notif>-tplnr.

    "this field is used for enabling and disabling input fields in the UI.
    <notif>-app_field_enabled = abap_false.

    "
    " Detects beforehand the amount of attachments that this notification has
    SELECT COUNT(*)
      FROM srgbtbrel
      INTO <notif>-app_att_count
      WHERE instid_a EQ <notif>-qmnum
        AND reltype  EQ 'ATTA'
        AND typeid_a EQ 'BUS2038'.

    "Sync status can have one of the following values with the following meanings:
    "  1. None: The notification is complete and was not changed/saved;
    "  2. Warning: The notification was loaded without attachments;
    "  2. Error: the notification was changed and its sync(save) was NOT successfull;
    "  4. Success: the notification was saved (from the outbox) successfully;

    IF ( __notification_has_attachments( <notif>-qmnum ) EQ 'X' ).
      <notif>-app_att_loaded  = abap_false.
      <notif>-sync_status     = 'Warning'.
    ELSE.
      <notif>-app_att_loaded  = abap_true.
      <notif>-sync_status     = 'None'.
    ENDIF.
    "Marker field used to indicate different statuses in app (changed, sync error etc) - Default to draft
    <notif>-app_marker = 'Draft'.
    <notif>-app_unsaved = abap_false.
  endloop.

endmethod.


METHOD GET_NOTIFICATION_ATTACHMENTS.
  __get_notification_attachments( CHANGING cs_notification = ms_notification
                                           ct_attachments  = mt_attachments  ).
ENDMETHOD.


method HANDLE_OUTBOX.

  "Save or create notification
  __save_notification( ).

  "errorhandling (clear ok)


  refresh mt_attachments_add.
*refresh mt_notification_off.

endmethod.


method INITAPP.
  "In this method all master data and data for dropdown/search helps etc. are fetched.

   "Get notification types:
   __get_notif_types( ).

   "Get priority values:
   __get_priority_values( ).

   "Get phase descriptions:
   __get_phase_desc( ).

   "Get Code structure
   __get_code_data( ).

   "Get Maint. Plants && Planning plants
   __get_maint_plant( ).

   "Get Planner Groups
   __get_plan_group( ).

   "Get Locations
   __get_location( ).

   "Get Users for Created by
   __get_users( ).


endmethod.


METHOD __GET_CODE_DATA.
*---------------------------------------------------------------------
* Code Text
*---------------------------------------------------------------------

* get v_qpcd_f4
* NOTE: this query is preferrably instead of directly querying V_QPCD_F4. this way the
*       codes will always be gathered even if the texts are not customized
  SELECT qpgr~katalogart qpgr~codegruppe qpcd~code qpct~kurztext qpct~sprache
    FROM qpgr
      INNER JOIN qpcd
        ON ( qpcd~katalogart EQ qpgr~katalogart AND
             qpcd~codegruppe EQ qpgr~codegruppe )
      LEFT JOIN qpct
        ON ( qpct~katalogart EQ qpcd~katalogart AND
             qpct~codegruppe EQ qpcd~codegruppe AND
             qpct~code       EQ qpcd~code       AND
             qpct~version    EQ qpcd~version    AND
             qpct~sprache    EQ sy-langu        )
    INTO CORRESPONDING FIELDS OF TABLE mt_code_data
    WHERE qpgr~katalogart IN ('D','B','C','5','A').
*
** Adds an Initial Item
*  APPEND INITIAL LINE TO mt_code_data .
ENDMETHOD.


method __GET_LOCATION.
  select werks stand as stort ktext into corresponding fields of table mt_loc
    from t499s.

endmethod.


METHOD __GET_MAINT_PLANT.
  SELECT werks name1 iwerk INTO CORRESPONDING FIELDS OF TABLE mt_mp
    FROM t001w.

  SELECT pp~iwerk mp~name1
    FROM t399i AS pp
    JOIN t001w AS mp
      ON ( mp~werks EQ pp~iwerk )
    INTO CORRESPONDING FIELDS OF TABLE mt_planplant.
ENDMETHOD.


METHOD __GET_NOTIFICATION_ATTACHMENTS.
  DATA: lv_instid_a    TYPE srgbtbrel-instid_a,
        lt_attachments TYPE ty_attachments_tt,
        ls_sofc        TYPE v_sofc,
        lv_doc_id      TYPE sofolenti1-doc_id,
        ls_doc         TYPE sofolenti1,
        lt_hex         TYPE solix_tab,
        ls_hex         TYPE solix,
        lv_xstring     TYPE xstring.

  DATA: BEGIN OF lv_key,
      foltp TYPE so_fol_tp,
      folyr TYPE so_fol_yr,
      folno TYPE so_fol_no,
      doctp TYPE so_doc_tp,
      docyr TYPE so_doc_yr,
      docno TYPE so_doc_no,
  END OF lv_key.

  FIELD-SYMBOLS: <ls_attachment> TYPE ty_attachments.

  cs_notification-app_att_loaded = abap_true.
  IF ( cs_notification-sync_status EQ 'Warning' ).
    cs_notification-sync_status = 'None'.
  ENDIF.
  lv_instid_a = cs_notification-qmnum.

  DELETE ct_attachments WHERE qmnum EQ cs_notification-qmnum.

* Attachment
  SELECT *
         FROM srgbtbrel
         INTO CORRESPONDING FIELDS OF TABLE lt_attachments
         WHERE instid_a EQ lv_instid_a
           AND typeid_a EQ 'BUS2038'.

  LOOP AT lt_attachments ASSIGNING <ls_attachment>.

    lv_key   = <ls_attachment>-instid_b.

*   Get Document Info
    SELECT SINGLE *
           FROM v_sofc
           INTO ls_sofc
           WHERE foltp EQ lv_key-foltp
             AND folyr EQ lv_key-folyr
             AND folno EQ lv_key-folno
             AND doctp EQ lv_key-doctp
             AND docyr EQ lv_key-docyr
             AND docno EQ lv_key-docno.

    MOVE-CORRESPONDING ls_sofc TO <ls_attachment>.
    <ls_attachment>-qmnum = <ls_attachment>-instid_a.
    TRANSLATE <ls_attachment>-file_ext TO UPPER CASE.

    IF <ls_attachment>-file_ext EQ 'JPE'.
      <ls_attachment>-file_ext = 'JPG'.
    ENDIF.

    lv_doc_id = <ls_attachment>-instid_b.
    CALL FUNCTION 'SO_DOCUMENT_READ_API1'
      EXPORTING
        document_id                = lv_doc_id
      IMPORTING
        document_data              = ls_doc
      TABLES
        contents_hex               = lt_hex
      EXCEPTIONS
        document_id_not_exist      = 1
        operation_no_authorization = 2
        x_error                    = 3
        OTHERS                     = 4.

*   Build String
    CLEAR lv_xstring.
    LOOP AT lt_hex INTO ls_hex.
      CONCATENATE lv_xstring
                  ls_hex-line
                  INTO lv_xstring IN BYTE MODE.
    ENDLOOP.

*   Base64 Encoding
    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = lv_xstring
      IMPORTING
        output = <ls_attachment>-content.

  ENDLOOP.

  IF ( sy-subrc EQ 0 ).
    APPEND LINES OF lt_attachments TO ct_attachments.
  ENDIF.
ENDMETHOD.


method __GET_NOTIFICATION_TEXT.


    data:
      it_lines type standard table of tline.

    field-symbols:
      <fl_lines> like line of it_lines.

*     Get Text Header
    call function 'READ_TEXT'
      exporting
        id                      = iv_tdid
        language                = iv_tdspras
        name                    = iv_tdname
        object                  = iv_tdobject
      tables
        lines                   = it_lines
      exceptions
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        others                  = 8.

    loop at it_lines assigning <fl_lines>.

      shift <fl_lines>-tdline left deleting leading '*'.
      shift <fl_lines>-tdline left deleting leading space.

      concatenate rv_text
                  <fl_lines>-tdline
                  cl_abap_char_utilities=>cr_lf
                  into rv_text.

    endloop.
endmethod.


method __get_notification_texts.

  types:
    ty_t_stxh_key type hashed table of stxh-tdname with unique key table_line,
    begin of ty_stxh,
      tdname   type stxh-tdname,
      tdspras  type stxh-tdspras,
      tdobject type stxh-tdobject,
      tdid     type stxh-tdid,
    end of ty_stxh,
    ty_t_stxh type sorted table of ty_stxh with unique key tdname tdspras.

  data:
    lt_stxh_key type ty_t_stxh_key,
    lt_stxh     type ty_t_stxh,
    lv_tdname   type stxh-tdname.

  field-symbols:
    <fw_qmel> like line of ct_notification,
    <fw_stxh> like line of lt_stxh.

*---------------------------------------------------------------------*
  "HEADER TEXT
*---------------------------------------------------------------------*
  loop at ct_notification assigning <fw_qmel>.
    lv_tdname = <fw_qmel>-qmnum.
    collect lv_tdname into lt_stxh_key.
  endloop.

  check lt_stxh_key[] is not initial.

  "Get Text Header
  select tdname tdspras tdobject tdid
    from stxh
    into table lt_stxh
     for all entries in lt_stxh_key
   where tdobject eq 'QMEL'
     and tdname	  eq lt_stxh_key-table_line.

  check sy-subrc is initial.

  loop at ct_notification assigning <fw_qmel>.
    loop at lt_stxh assigning <fw_stxh> where tdname eq <fw_qmel>-qmnum.
      <fw_qmel>-text_o = '{ <fw_qmel>-text_o } { __get_notification_text( iv_tdname = <fw_stxh>-tdname iv_tdspras = <fw_stxh>-tdspras iv_tdobject = <fw_stxh>-tdobject iv_tdid = <fw_stxh>-tdid ) }'.
    endloop.
  endloop.

endmethod.


method __GET_NOTIF_TYPES.
  " Get Notifications types and description
  " QMTYP 01 = Plant Maintenance
  " SPRAS    = User language

* Get data from TQ80 and TQ80_T
  select a~qmart b~qmartx
        from tq80 as a
        inner join tq80_t as b
        on a~qmart eq b~qmart
        into table mt_notif_type
        where a~qmtyp eq '01'
        and   b~spras eq sy-langu.

* Initial Item
  append initial line to mt_notif_type.

* Sorting
  sort mt_notif_type.
endmethod.


method __GET_PHASE_DESC.
* Get DD07T for text on Phase
  select VALPOS as phase ddtext as desc
         from dd07t
         into table mt_phase_desc
         where domname eq 'QM_PHASE'
           and ddlanguage eq sy-langu.

endmethod.


method __GET_PLAN_GROUP.

  select iwerk ingrp innam into corresponding fields of table mt_pg
    from t024i.


endmethod.


method __GET_PRIORITY_VALUES.

  " Get priority values with description
  " ARTPR PM = Plant Maintenance
  " SPRAS    = User language

  select a~priok b~priokx
         from t356 as a
          inner join t356_t as b
          on a~priok eq b~priok
         into table mt_priority_values
         where a~artpr eq 'PM'
         and   b~spras eq sy-langu
         and   a~artpr eq b~artpr.

  append initial line to mt_priority_values.

  sort mt_priority_values.







endmethod.


METHOD __get_users.
  SELECT bname AS uname name_text AS fullname FROM
      v_usr_name INTO CORRESPONDING FIELDS OF TABLE mt_user.

ENDMETHOD.


METHOD __NOTIFICATION_HAS_ATTACHMENTS.
  DATA: lt_attachments TYPE ty_attachments_tt,
        lv_instid_a    TYPE srgbtbrel-instid_a.

  lv_instid_a = iv_qmnum.

  SELECT *
    FROM srgbtbrel
    INTO CORRESPONDING FIELDS OF TABLE lt_attachments
    WHERE instid_a EQ lv_instid_a
      AND typeid_a EQ 'BUS2038'.

  IF ( lt_attachments[] IS INITIAL ).
    rv_boolean = abap_false.
  ELSE.
    rv_boolean = abap_true.
  ENDIF.
ENDMETHOD.


  method __SAVE_COMMENT.

    data: it_text   type standard table of string,
          it_tline  type standard table of tline,
          wa_tline  like line of it_tline,
          wa_text   like line of it_text,
          wa_stxh   type stxh,
          lv_tdname type stxh-tdname,
          lv_thead  type thead,
          lv_date   type string,
          lv_time   type string,
          lv_user   type string,
          lv_langu  type sy-langu.


*---------------------------------------------------------------------*
* COMMON
*---------------------------------------------------------------------*
    call function 'CONVERSION_EXIT_LDATE_OUTPUT'
      exporting
        input  = sy-datum
      importing
        output = lv_date.

    concatenate '('
               sy-uname
               ')'
               into lv_user.

    " if the notification was already saved with a long text it should already have a default language
    " we must continue using this language if we want all the texts to appears correctly in sap gui
    select single kzmla from qmel into lv_langu where qmnum eq iv_qmnum.

    if lv_langu is initial.
      lv_langu = 'E'."sy-langu.
    endif.

*---------------------------------------------------------------------*
* HEADER
*---------------------------------------------------------------------*
**    Insert TimeStamp
*    concatenate lv_date
*                lv_time
*                lv_user
*                into wa_text separated by space respecting blanks.
*
*    append wa_text to it_text.
    clear  wa_text.

    call function 'CONVERT_STRING_TO_TABLE'
      exporting
        i_string         = iv_comment
        i_tabline_length = 132
      tables
        et_table         = it_text.

    append wa_text to it_text.

*   Text Header
    lv_thead-tdid     = 'LTXT'.
    lv_thead-tdobject = 'QMEL'.
    lv_thead-tdspras  = lv_langu.
    lv_thead-tdname   = iv_qmnum.

    loop at it_text into wa_text.
      wa_tline-tdformat = '*'.
      wa_tline-tdline   = wa_text.
      append wa_tline to it_tline.
      clear  wa_tline.
    endloop.


    call function 'IQS0_ADD_NOTIFICATION_LONGTEXT'
      exporting
        i_qmnum       = iv_qmnum
        i_post        = 'X'
*       I_RESET       =
      tables
        t_inlines     = it_tline
      exceptions
        show_messages = 1
        others        = 2.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

* Commit
    commit work and wait.



* Clear new comment
    clear it_text.

  endmethod.


METHOD __SAVE_NOTIFICATION.
  DATA: lv_tabix           TYPE sy-tabix,
        lv_header          TYPE bapi2080_nothdri,
        lv_headerx         TYPE bapi2080_nothdri_x,
        lv_hexport         TYPE bapi2080_nothdre,
        lv_error           TYPE string,
        lt_return          TYPE STANDARD TABLE OF bapiret2,
        lt_notifcausi      TYPE STANDARD TABLE OF bapi2080_notcausi,
        lt_notifcausi_add  TYPE STANDARD TABLE OF bapi2080_notcausi,
        lt_notifcausix     TYPE STANDARD TABLE OF bapi2080_notcausi_x,
        lt_notitem         TYPE STANDARD TABLE OF bapi2080_notitemi,
        lt_notitem_add     TYPE STANDARD TABLE OF bapi2080_notitemi,
        lt_notitemx        TYPE STANDARD TABLE OF bapi2080_notitemi_x,
        lt_item_e          TYPE STANDARD TABLE OF bapi2080_notiteme,
        lt_caus_e          TYPE STANDARD TABLE OF bapi2080_notcause,
        lt_temp_notif      TYPE ty_t_qmel.

  FIELD-SYMBOLS:
        <notif>            LIKE LINE OF mt_notification_off,
        <notif_text>       LIKE LINE OF mt_notification_off,
        <msg>              LIKE LINE OF mt_sync_message,
        <ret>              LIKE LINE OF lt_return,
        <causi>            LIKE LINE OF lt_notifcausi,
        <causix>           LIKE LINE OF lt_notifcausix,
        <item>             LIKE LINE OF lt_notitem,
        <itemx>            LIKE LINE OF lt_notitemx,
        <ls_notitem_i>     TYPE bapi2080_notitemi,
        <ls_notitem_e>     TYPE bapi2080_notiteme,
        <ls_notcaus_i>     TYPE bapi2080_notcausi,
        <ls_notcaus_e>     TYPE bapi2080_notcause.

  CHECK mt_notification_off IS NOT INITIAL.

  FREE mt_sync_message[].

  LOOP AT mt_notification_off ASSIGNING <notif>.

* Init
    FREE lt_return.
    lv_error = space.
    <notif>-qmnum_old = <notif>-qmnum. "Important for pairing when returning
*--------------------------------------------------------------------*
* HEADER
*--------------------------------------------------------------------*

* Map Header
    lv_header-short_text  = <notif>-qmtxt.
    lv_header-funct_loc   = <notif>-tplnr.
    lv_header-priority    = <notif>-priok.
    lv_header-desstdate   = <notif>-strmn.
    lv_header-dessttime   = <notif>-strur.
    lv_header-desenddate  = <notif>-ltrmn.
    lv_header-desendtm    = <notif>-ltrur.
    lv_header-code_group  = <notif>-qmgrp.
    lv_header-coding      = <notif>-qmcod.
    lv_header-maintplant  = <notif>-swerk.
    lv_header-maintloc    = <notif>-stort.
    lv_header-reportedby  = <notif>-qmnam.
    lv_header-division    = <notif>-spart.
    lv_header-sales_org   = <notif>-vkorg.
    lv_header-distr_chan  = <notif>-vtweg.
    lv_header-plangroup   = <notif>-ingrp.
*    lv_header-strmlfndate = <notif>-ausvn.
*    lv_header-strmlfntime = <notif>-auztv.
*    lv_header-breakdown =   <notif>-msaus.

* Fields to Update - All fields to update needs to be marked as X to actually be changed
    lv_headerx-funct_loc   = 'X'.
    lv_headerx-short_text  = 'X'.
    lv_headerx-priority    = 'X'.
    lv_headerx-desstdate   = 'X'.
    lv_headerx-dessttime   = 'X'.
    lv_headerx-desenddate  = 'X'.
    lv_headerx-desendtm    = 'X'.
    lv_headerx-code_group  = 'X'.
    lv_headerx-coding      = 'X'.
    lv_headerx-maintroom   = 'X'.
    lv_headerx-maintplant	 = 'X'.
    lv_headerx-maintloc	   = 'X'.
    lv_headerx-reportedby  = 'X'.
    lv_headerx-division    = 'X'.
    lv_headerx-sales_org   = 'X'.
    lv_headerx-distr_chan  = 'X'.
    lv_headerx-plangroup   = 'X'.
*    lv_headerx-breakdown   = 'X'.
*    lv_headerx-strmlfndate = 'X'.
*    lv_headerx-strmlfntime = 'X'.

* Format Functional Location
    CALL FUNCTION 'CONVERSION_EXIT_TPLNR_INPUT'
      EXPORTING
        input  = lv_header-funct_loc
      IMPORTING
        output = lv_header-funct_loc.


*--------------------------------------------------------------------*
* ITEM
*--------------------------------------------------------------------
* Set Damage cause table
    REFRESH lt_notitem.
    REFRESH lt_notitemx.

    IF ( NOT <notif>-otgrp IS INITIAL ).
      APPEND INITIAL LINE TO lt_notitem ASSIGNING <item>.

      <item>-refobjectkey  = <notif>-qmnum.
      <item>-item_key       = '0001'.
      <item>-item_sort_no   = '0001'.
      <item>-d_codegrp      = <notif>-fegrp.
      <item>-d_code         = <notif>-fecod.
      <item>-descript       = <notif>-fetxt.
      <item>-dl_codegrp     = <notif>-otgrp.
      <item>-dl_code        = <notif>-oteil.

      APPEND INITIAL LINE TO lt_notitemx ASSIGNING <itemx>.
      <itemx>-item_key       = '0001'.
      <itemx>-item_sort_no   = 'X'.
      <itemx>-d_codegrp      = 'X'.
      <itemx>-d_code         = 'X'.
      <itemx>-descript       = 'X'.
      <itemx>-dl_codegrp     = 'X'.
      <itemx>-dl_code        = 'X'.
    ENDIF.

* Set Cause cause table
    REFRESH lt_notifcausi.
    REFRESH lt_notifcausix.

    IF ( NOT <notif>-urgrp IS INITIAL ).
      APPEND INITIAL LINE TO lt_notifcausi ASSIGNING <causi>.
      <causi>-refobjectkey    = <notif>-qmnum.
      <causi>-cause_key       = '0001'.
      <causi>-cause_sort_no   = '0001'.
      <causi>-item_key        = '0001'.
      <causi>-cause_codegrp   = <notif>-urgrp.
      <causi>-cause_code      = <notif>-urcod.
      <causi>-causetext       = <notif>-urtxt.
      <causi>-item_sort_no    = '0001'.

      APPEND INITIAL LINE TO lt_notifcausix ASSIGNING <causix>.
      <causix>-cause_key      = '0001'.
      <causix>-cause_sort_no  = '0001'.
      <causix>-item_key       = '0001'.
      <causix>-cause_codegrp  = 'X'.
      <causix>-cause_code     = 'X'.
      <causix>-causetext      = 'X'.
      <causix>-item_sort_no   = 'X'.
    ENDIF.

*--------------------------------------------------------------------*
* SAVE / CREATE
*--------------------------------------------------------------------*

    "CREATE - Notifications created in the app will get a temp number starting with $
    IF <notif>-qmnum(1) EQ '$'.
*     Create Notification
      CLEAR <notif>-qmnum.
      CALL FUNCTION 'BAPI_ALM_NOTIF_CREATE'
        EXPORTING
          notif_type         = <notif>-qmart
          notifheader        = lv_header
        IMPORTING
          notifheader_export = lv_hexport
        TABLES
          notifcaus          = lt_notifcausi
          notitem            = lt_notitem
          return             = lt_return.
      LOOP AT lt_return TRANSPORTING NO FIELDS
        WHERE type EQ 'E'
           OR type EQ 'A'.
        EXIT.
      ENDLOOP.
      IF ( sy-subrc EQ 0 ).
        <notif>-qmnum = <notif>-qmnum_old. " In case of an error, it maintains the old number
      ELSE.
        <notif>-qmnum = lv_hexport-notif_no.
      ENDIF.
    ELSE.
*    Update Notification
      "
      " Gets the current notification information for item and causes
      " and prepares the tables for processing
      " NOTE: in this example, we only process 1 item and
      "       1 cause for that item
      CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL'
        EXPORTING
          number    = <notif>-qmnum
        TABLES
          notitem   = lt_item_e
          notifcaus = lt_caus_e
          return    = lt_return.
      LOOP AT lt_return TRANSPORTING NO FIELDS
        WHERE type EQ 'E'.
        EXIT.
      ENDLOOP.
      IF NOT ( sy-subrc EQ 0 ).
        "
        " Details were obtained
        "
        " Check the item
        IF ( NOT lt_notitem IS INITIAL ).
          IF lt_item_e IS INITIAL.
            lt_notitem_add  = lt_notitem.
            REFRESH: lt_notitem, lt_notitemx.
          ELSE.
            READ TABLE lt_notitem INDEX 1 ASSIGNING <ls_notitem_i>.
            READ TABLE lt_item_e  INDEX 1 ASSIGNING <ls_notitem_e>.
            <ls_notitem_i>-refobjectkey = <ls_notitem_e>-notif_no.
            <ls_notitem_i>-item_key     = <ls_notitem_e>-item_key.
            <ls_notitem_i>-item_sort_no = <ls_notitem_e>-item_sort_no.
          ENDIF.
        ENDIF.
        "
        " Check the cause
        IF ( NOT lt_notifcausi IS INITIAL ).
          IF lt_caus_e IS INITIAL.
            lt_notifcausi_add  = lt_notifcausi.
            REFRESH: lt_notifcausi, lt_notifcausix.
          ELSE.
            READ TABLE lt_notifcausi INDEX 1 ASSIGNING <ls_notcaus_i>.
            READ TABLE lt_caus_e     INDEX 1 ASSIGNING <ls_notcaus_e>.
            <ls_notcaus_i>-refobjectkey  = <ls_notcaus_e>-notif_no.
            <ls_notcaus_i>-item_key      = <ls_notcaus_e>-item_key.
            <ls_notcaus_i>-cause_key     = <ls_notcaus_e>-cause_key.
            <ls_notcaus_i>-cause_sort_no = <ls_notcaus_e>-cause_sort_no.
          ENDIF.
        ENDIF.
        "
        " Updates the notification (with item/cause if possible)
        CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY'
          EXPORTING
            number             = <notif>-qmnum
            notifheader        = lv_header
            notifheader_x      = lv_headerx
          IMPORTING
            notifheader_export = lv_hexport
          TABLES
            notifitem          = lt_notitem
            notifitem_x        = lt_notitemx
            notifcaus          = lt_notifcausi
            notifcaus_x        = lt_notifcausix
            return             = lt_return.
        " Adds if necessary (item first)
        IF ( lt_return[] IS INITIAL ) AND ( NOT lt_notitem_add[] IS INITIAL ).
          CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_ADD'
            EXPORTING
              number  = <notif>-qmnum
            TABLES
              notitem = lt_notitem_add
              return  = lt_return.
        ENDIF.
        IF ( lt_return[] IS INITIAL ) AND ( NOT lt_notifcausi_add[] IS INITIAL ).
          CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_ADD'
            EXPORTING
              number    = <notif>-qmnum
            TABLES
              notifcaus = lt_notifcausi_add
              return    = lt_return.
        ENDIF.
      ENDIF.

    ENDIF.

    "Set OK status, if not this will be changed later
    <notif>-app_unsaved = abap_false.
    LOOP AT lt_return ASSIGNING <ret> WHERE type EQ 'E' OR type EQ 'A'.
      lv_error = 'X'.
      APPEND INITIAL LINE TO mt_sync_message ASSIGNING <msg>.
      <msg>-qmnum   = <notif>-qmnum.
      <msg>-message = <ret>-message.
      <msg>-state   = 'Error'.
      <msg>-type    = 'E'.
    ENDLOOP.
    IF ( sy-subrc EQ 0 ).
      "
      " Errors were found.
      <notif>-sync_status = 'Error'.
      <notif>-app_marker  = 'Flagged'.
    ELSE.
      "
      " No errors were found
      <notif>-sync_status = 'Success'.
      <notif>-app_marker  = 'Draft'.
    ENDIF.

    CHECK lv_error IS INITIAL.

**--------------------------------------------------------------------*
** ATTACHMENT
**--------------------------------------------------------------------*
    DATA: lv_folder_id TYPE soodk,
          lv_object_id TYPE soobjinfi1-object_id,
          lv_rolea     TYPE borident,
          lv_roleb     TYPE borident,
          lv_pre       TYPE string,
          lv_data      TYPE string,
          lv_datax     TYPE xstring,
          lv_doc_type  TYPE soodk-objtp,
          lv_doc_data  TYPE sodocchgi1,
          lv_doc_info  TYPE sofolenti1,
          it_solix     TYPE STANDARD TABLE OF solix.

    IF ( <notif>-app_att_loaded EQ abap_true ).
*     Gets Root Folder
      CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
        EXPORTING
          owner     = sy-uname
          region    = 'B'
        IMPORTING
          folder_id = lv_folder_id.

      lv_object_id = lv_folder_id.

      LOOP AT mt_attachments_add INTO ms_attachment WHERE qmnum EQ <notif>-qmnum_old.

        SPLIT ms_attachment-content AT ',' INTO lv_pre lv_data.

        TRANSLATE lv_pre TO LOWER CASE.

        IF lv_pre CS 'pdf'.
          lv_doc_type = 'PDF'.
        ELSE.
          lv_doc_type = 'PNG'.
        ENDIF.

*     Document Information
        lv_doc_data-obj_name   = ms_attachment-docdes.
        lv_doc_data-obj_descr  = ms_attachment-docdes.
        lv_doc_data-obj_langu  = sy-langu.
        lv_doc_data-doc_size   = strlen( lv_data ).

*     Decode Base64
        CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
          EXPORTING
            input  = lv_data
          IMPORTING
            output = lv_datax
          EXCEPTIONS
            failed = 1
            OTHERS = 2.

*     Convert to table
        CALL METHOD cl_document_bcs=>xstring_to_solix
          EXPORTING
            ip_xstring = lv_datax
          RECEIVING
            rt_solix   = it_solix.

*     Insert Document
        CALL FUNCTION 'SO_DOCUMENT_INSERT_API1'
          EXPORTING
            folder_id                  = lv_object_id
            document_data              = lv_doc_data
            document_type              = lv_doc_type
          IMPORTING
            document_info              = lv_doc_info
          TABLES
            contents_hex               = it_solix
          EXCEPTIONS
            folder_not_exist           = 1
            document_type_not_exist    = 2
            operation_no_authorization = 3
            parameter_error            = 4
            x_error                    = 5
            enqueue_error              = 6
            OTHERS                     = 7.

*      Relation Keys
        lv_rolea-objtype = 'BUS2038'.
        lv_rolea-objkey  = <notif>-qmnum.

        lv_roleb-objtype = 'MESSAGE'.
        CONCATENATE lv_folder_id
                    lv_doc_info-object_id
                    INTO lv_roleb-objkey RESPECTING BLANKS.

*     Create Relation
        CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
          EXPORTING
            obj_rolea      = lv_rolea
            obj_roleb      = lv_roleb
            relationtype   = 'ATTA'
          EXCEPTIONS
            no_model       = 1
            internal_error = 2
            unknown        = 3
            OTHERS         = 4.


      ENDLOOP.

*      if mt_attachments_add is not initial.
*        wait up to '1' seconds.
*      endif.
    ELSE.   " NOT ( <notif>-app_att_loaded EQ abap_true ).
      "
      " The attachments were not syncronized previously, but it is done now that
      " the notification is being updated and the app is online
      me->__get_notification_attachments(
        CHANGING cs_notification = <notif>
                 ct_attachments  = mt_attachments ).
    ENDIF.  "     ( <notif>-app_att_loaded EQ abap_true ).
* Error handling
    LOOP AT lt_return ASSIGNING <ret> WHERE type EQ 'E' OR type EQ 'A'.
      lv_error = 'X'.
      APPEND INITIAL LINE TO mt_sync_message ASSIGNING <msg>.
      <msg>-qmnum   = <notif>-qmnum.
      <msg>-message = <ret>-message.
      <msg>-state   = 'Error'.
      <msg>-type    = 'E'.

      <notif>-sync_status = 'Error'.
      <notif>-app_marker  = 'Flagged'.
    ENDLOOP.

    CHECK lv_error IS INITIAL.


* Save Notification
    CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
      EXPORTING
        number      = <notif>-qmnum
      IMPORTING
        notifheader = lv_hexport
      TABLES
        return      = lt_return.

    LOOP AT lt_return TRANSPORTING NO FIELDS
      WHERE type EQ 'E'
         OR type EQ 'A'.
      EXIT.
    ENDLOOP.
    IF ( sy-subrc EQ 0 ).
      <notif>-qmnum = <notif>-qmnum_old.
    ELSE.
      <notif>-qmnum = lv_hexport-notif_no.
    ENDIF.

* Error handling
    LOOP AT lt_return ASSIGNING <ret> WHERE type EQ 'E' OR type EQ 'A'.
      lv_error = 'X'.
      APPEND INITIAL LINE TO mt_sync_message ASSIGNING <msg>.
      <msg>-qmnum   = <notif>-qmnum.
      <msg>-message = <ret>-message.
      <msg>-state   = 'Error'.
      <msg>-type    = 'E'.

      <notif>-sync_status = 'Error'.
      <notif>-app_marker  = 'Flagged'.
    ENDLOOP.

    CHECK lv_error IS INITIAL.

    "
    " Updates information back after saving
    MOVE lv_hexport-priority    TO <notif>-priok.
    MOVE lv_hexport-desstdate   TO <notif>-strmn.
    MOVE lv_hexport-dessttime   TO <notif>-strur.
    MOVE lv_hexport-desenddate  TO <notif>-ltrmn.
    MOVE lv_hexport-desendtm    TO <notif>-ltrur.
    MOVE lv_hexport-reportedby  TO <notif>-qmnam.
    MOVE lv_hexport-created_by  TO <notif>-ernam.
    MOVE lv_hexport-notif_date  TO <notif>-qmdat.
    MOVE lv_hexport-orderid     TO <notif>-aufnr.

    REFRESH mt_attachments_add.

* Commit changes
    COMMIT WORK AND WAIT.

* Save comments:
    IF <notif>-text IS NOT INITIAL.
      __save_comment( iv_qmnum   = <notif>-qmnum
                      iv_comment = <notif>-text ).
    ENDIF.

  ENDLOOP. "End loop at MT_NOTIFICATION_OFF

  "
  " Handles the comments for the successful saves:
  lt_temp_notif[] = mt_notification_off[].
  DELETE lt_temp_notif[] WHERE sync_status NE 'Success'.
  LOOP AT lt_temp_notif ASSIGNING <notif>
    WHERE NOT ( text IS INITIAL AND text_o IS INITIAL ).
    CLEAR: <notif>-text, <notif>-text_o.
  ENDLOOP.
  __get_notification_texts( CHANGING ct_notification = lt_temp_notif ).
  LOOP AT mt_notification_off ASSIGNING <notif>.
    LOOP AT lt_temp_notif ASSIGNING <notif_text>
      WHERE qmnum EQ <notif>-qmnum.
      EXIT.
    ENDLOOP.
    IF ( sy-subrc EQ 0 ).
      <notif>-text   = <notif_text>-text.
      <notif>-text_o = <notif_text>-text_o.
    ENDIF.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
