/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: fnc_estabelecimento_selec_espec
** Descricao.............: Seleá∆o Especial de Estabelecimentos
** Versao................:  1.00.00.014
** Procedimento..........: man_estabelecimento
** Nome Externo..........: prgint/utb/utb071za.p
** Data Geracao..........: 12/08/2011 - 16:00:14
** Criado por............: bre18490
** Criado em.............: 18/06/1999 09:46:27
** Alterado por..........: fut42625
** Alterado em...........: 14/02/2011 18:35:12
** Gerado por............: fut42625_2
*****************************************************************************/

/*-- Filtro Multi-idioma Aplicado --*/

def var c-versao-prg as char initial " 1.00.00.014":U no-undo.
def var c-versao-rcode as char initial "[[[1.00.00.014[[[":U no-undo. /* Controle de Versao R-CODE - Nao retirar do Fonte */

{include/i_dbinst.i}
{include/i_dbtype.i}

/*{include/i_fcldef.i}
{include/i_trddef.i}  */


&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i fnc_estabelecimento_selec_espec UTB}
&ENDIF

/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=15":U.
/*************************************  *************************************/

&if "{&emsuni_dbinst}" <> "yes" &then
run pi_messages (input "show",
                 input 5884,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", 
                                    "EMSUNI")) /*msg_5884*/.
&elseif "{&emsuni_version}" < "1.00" &then
run pi_messages (input "show",
                 input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", 
                                    "FNC_ESTABELECIMENTO_SELEC_ESPEC","~~EMSUNI", "~~{~&emsuni_version}", "~~1.00")) /*msg_5009*/.
&else

/********************* Temporary Table Definition Begin *********************/

def temp-table tt_empresa no-undo
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    index tt_id                           
          tta_cod_empresa                  ascending
    .

def temp-table tt_estabelecimento_empresa no-undo like estabelecimento
&IF "{&emsuni_version}" >= "" AND "{&emsuni_version}" < "5.07A" &THEN
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
&ENDIF
&IF "{&emsuni_version}" >= "5.07A" AND "{&emsuni_version}" < "9.99" &THEN
    field tta_cod_estab                    as Character format "x(5)" label "Estabelecimento" column-label "Estab"
&ENDIF
    field tta_nom_pessoa                   as character format "x(40)" label "Nome" column-label "Nome"
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field tta_nom_razao_social             as character format "x(40)" label "Raz∆o Social" column-label "Raz∆o Social"
    field ttv_log_selec                    as logical format "Sim/N∆o" initial no column-label "Gera"
    index tt_cod_estab                     is primary unique
          tta_cod_estab                    ascending
    .

def temp-table tt_usuar_grp_usuar no-undo like usuar_grp_usuar
    .



/********************** Temporary Table Definition End **********************/

/************************ Parameter Definition Begin ************************/

def Input param p_cod_modul_exec
    as character
    format "x(8)"
    no-undo.


/************************* Parameter Definition End *************************/

/************************** Buffer Definition Begin *************************/

def buffer b_segur_unid_organ
    for ems.segur_unid_organ.
def buffer b_unid_organ
    for ems.unid_organ.


/*************************** Buffer Definition End **************************/

/************************* Variable Definition Begin ************************/

def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def new global shared var v_cod_ccusto_corren
    as character
    format "x(11)":U
    label "Centro Custo"
    column-label "Centro Custo"
    no-undo.
def new global shared var v_cod_dwb_user
    as character
    format "x(21)":U
    label "Usu†rio"
    column-label "Usu†rio"
    no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def new global shared var v_cod_funcao_negoc_empres
    as character
    format "x(50)":U
    no-undo.
def new global shared var v_cod_grp_usuar_lst
    as character
    format "x(3)":U
    label "Grupo Usu†rios"
    column-label "Grupo"
    no-undo.
def new global shared var v_cod_idiom_usuar
    as character
    format "x(8)":U
    label "Idioma"
    column-label "Idioma"
    no-undo.
def new global shared var v_cod_modul_dtsul_corren
    as character
    format "x(3)":U
    label "M¢dulo Corrente"
    column-label "M¢dulo Corrente"
    no-undo.
def new global shared var v_cod_modul_dtsul_empres
    as character
    format "x(100)":U
    no-undo.
def var v_cod_modul_exec
    as character
    format "x(8)":U
    no-undo.
def new global shared var v_cod_pais_empres_usuar
    as character
    format "x(3)":U
    label "Pa°s Empresa Usu†rio"
    column-label "Pa°s"
    no-undo.
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def new global shared var v_cod_unid_negoc_usuar
    as character
    format "x(3)":U
    view-as combo-box
    &if "{&FNC_MULTI_IDIOMA}" = "YES" &then
    list-item-pairs "",""
    &else
    list-items ""
    &endif
    inner-lines 5
    bgcolor 15 font 2
    label "Unidade Neg¢cio"
    column-label "Unid Neg¢cio"
    no-undo.
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)":U
    label "Usu†rio Corrente"
    column-label "Usu†rio Corrente"
    no-undo.
def new global shared var v_cod_usuar_corren_criptog
    as character
    format "x(16)":U
    no-undo.
def shared var v_des_estab_select
    as character
    format "x(2000)":U
    view-as editor max-chars 2000 no-word-wrap
    size 30 by 1
    bgcolor 15 font 2
    label "Selecionados"
    column-label "Selecionados"
    no-undo.
def var v_des_estab_select_aux_2
    as character
    format "x(2000)":U
    view-as editor max-chars 2000 no-word-wrap
    size 30 by 1
    bgcolor 15 font 2
    no-undo.
def var v_log_method
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def var v_log_return
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def var v_nom_title_aux
    as character
    format "x(60)":U
    no-undo.
def var v_num_cont
    as integer
    format ">,>>9":U
    initial 0
    no-undo.
def var v_num_contador
    as integer
    format ">>>>,>>9":U
    initial 0
    no-undo.
def var v_num_lin
    as integer
    format ">>>>,>>9":U
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9":U
    no-undo.
def var v_num_row_a
    as integer
    format ">>>,>>9":U
    no-undo.
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_num_select_row                 as integer         no-undo. /*local*/


/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

.

def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conte£do"
    menu-item mi_sobre              label "&Sobre".



/**************************** Menu Definition End ***************************/

/************************** Query Definition Begin **************************/

def query qr_estabelecimento_selec_espec
    for tt_estabelecimento_empresa
    scrolling.


/*************************** Query Definition End ***************************/

/************************** Browse Definition Begin *************************/

def browse br_estabelecimento_selec_espec query qr_estabelecimento_selec_espec display 
&IF "{&emsuni_version}" >= "" AND "{&emsuni_version}" < "5.07A" &THEN
    tt_estabelecimento_empresa.tta_cod_estab
    width-chars 03.00
        column-label "Est"
&ENDIF
&IF "{&emsuni_version}" >= "5.07A" AND "{&emsuni_version}" < "9.99" &THEN
    tt_estabelecimento_empresa.tta_cod_estab
    width-chars 05.00
        column-label "Est"
&ENDIF
    tt_estabelecimento_empresa.tta_nom_pessoa
    width-chars 40.00
        column-label "Nome"
    tt_estabelecimento_empresa.tta_cod_empresa
    width-chars 03.00
        column-label "Emp"
    tt_estabelecimento_empresa.tta_nom_razao_social
    width-chars 40.00
        column-label "Raz∆o Social"
    with separators multiple 
         size 66.29 by 07.00
         font 1
         bgcolor 15
         title "Estabelecimentos".


/*************************** Browse Definition End **************************/

/************************ Rectangle Definition Begin ************************/

def rectangle rt_001
    size 1 by 1
    edge-pixels 2.
def rectangle rt_cxcf
    size 1 by 1
    fgcolor 1 edge-pixels 2.
def rectangle rt_mold
    size 1 by 1
    edge-pixels 2.


/************************* Rectangle Definition End *************************/

/************************** Button Definition Begin *************************/

def button bt_can
    label "Cancela"
    tooltip "Cancela"
    size 1 by 1
    auto-endkey.
def button bt_empresa1
    label "Empresa"
    tooltip "Empresa"
    size 1 by 1.
def button bt_hel2
    label "Ajuda"
    tooltip "Ajuda"
    size 1 by 1.
def button bt_nenhum
    label "Nenhum"
    tooltip "Desmarca Todas Ocorràncias"
    size 1 by 1.
def button bt_ok
    label "OK"
    tooltip "OK"
    size 1 by 1
    auto-go.
def button bt_todos
    label "Todos"
    tooltip "Marca Todas Ocorràncias"
    size 1 by 1.
/****************************** Function Button *****************************/


/*************************** Button Definition End **************************/

/************************ Radio-Set Definition Begin ************************/

def var rs_estabelecimento_selec_espec
    as character
    initial "Por Estabelecimento"
    view-as radio-set Horizontal
    radio-buttons "Por Estabelecimento", "Por Estabelecimento", "Por Raz∆o Social", "Por Raz∆o Social", "Por Empresa", "Por Empresa"
     /*l_por_estabelecimento*/ /*l_por_estabelecimento*/ /*l_por_razao_social*/ /*l_por_razao_social*/ /*l_por_empresa*/ /*l_por_empresa*/
    bgcolor 15 
    no-undo.


/************************* Radio-Set Definition End *************************/

/************************** Frame Definition Begin **************************/

def frame f_dlg_01_estabelecimento_selec_espec
    rt_mold
         at row 01.21 col 02.00
    rt_cxcf
         at row 11.08 col 02.00 bgcolor 7 
    rt_001
         at row 01.21 col 02.00 bgcolor 15 
    rs_estabelecimento_selec_espec
         at row 01.38 col 04.00
         help "" no-label
    br_estabelecimento_selec_espec
         at row 02.33 col 02.00
    bt_empresa1
         at row 09.50 col 03.57 font ?
         help "Empresa"
    bt_todos
         at row 09.50 col 15.57 font ?
         help "Marca Todas Ocorràncias"
    bt_nenhum
         at row 09.50 col 27.57 font ?
         help "Desmarca Todas Ocorràncias"
    bt_ok
         at row 11.29 col 03.00 font ?
         help "OK"
    bt_can
         at row 11.29 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 11.29 col 57.29 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 69.72 by 12.92 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Seleá∆o de Estabelecimentos".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars       in frame f_dlg_01_estabelecimento_selec_espec = 10.00
           bt_can:height-chars      in frame f_dlg_01_estabelecimento_selec_espec = 01.00
           bt_empresa1:width-chars  in frame f_dlg_01_estabelecimento_selec_espec = 10.00
           bt_empresa1:height-chars in frame f_dlg_01_estabelecimento_selec_espec = 01.00
           bt_hel2:width-chars      in frame f_dlg_01_estabelecimento_selec_espec = 10.00
           bt_hel2:height-chars     in frame f_dlg_01_estabelecimento_selec_espec = 01.00
           bt_nenhum:width-chars    in frame f_dlg_01_estabelecimento_selec_espec = 10.00
           bt_nenhum:height-chars   in frame f_dlg_01_estabelecimento_selec_espec = 01.00
           bt_ok:width-chars        in frame f_dlg_01_estabelecimento_selec_espec = 10.00
           bt_ok:height-chars       in frame f_dlg_01_estabelecimento_selec_espec = 01.00
           bt_todos:width-chars     in frame f_dlg_01_estabelecimento_selec_espec = 10.00
           bt_todos:height-chars    in frame f_dlg_01_estabelecimento_selec_espec = 01.00
           rt_001:width-chars       in frame f_dlg_01_estabelecimento_selec_espec = 66.29
           rt_001:height-chars      in frame f_dlg_01_estabelecimento_selec_espec = 01.13
           rt_cxcf:width-chars      in frame f_dlg_01_estabelecimento_selec_espec = 66.29
           rt_cxcf:height-chars     in frame f_dlg_01_estabelecimento_selec_espec = 01.42
           rt_mold:width-chars      in frame f_dlg_01_estabelecimento_selec_espec = 66.29
           rt_mold:height-chars     in frame f_dlg_01_estabelecimento_selec_espec = 09.50.
&if '{&emsbas_version}' >= '5.06' &then
if OPSYS = 'WIN32':U then do:
assign br_estabelecimento_selec_espec:ALLOW-COLUMN-SEARCHING in frame f_dlg_01_estabelecimento_selec_espec = no
       br_estabelecimento_selec_espec:COLUMN-MOVABLE in frame f_dlg_01_estabelecimento_selec_espec = no.
end.
&endif
    /* set private-data for the help system */
    assign rs_estabelecimento_selec_espec:private-data in frame f_dlg_01_estabelecimento_selec_espec = "HLP=000000000":U
           br_estabelecimento_selec_espec:private-data in frame f_dlg_01_estabelecimento_selec_espec = "HLP=000000000":U
           bt_empresa1:private-data                    in frame f_dlg_01_estabelecimento_selec_espec = "HLP=000000000":U
           bt_todos:private-data                       in frame f_dlg_01_estabelecimento_selec_espec = "HLP=000013336":U
           bt_nenhum:private-data                      in frame f_dlg_01_estabelecimento_selec_espec = "HLP=000013335":U
           bt_ok:private-data                          in frame f_dlg_01_estabelecimento_selec_espec = "HLP=000010721":U
           bt_can:private-data                         in frame f_dlg_01_estabelecimento_selec_espec = "HLP=000011050":U
           bt_hel2:private-data                        in frame f_dlg_01_estabelecimento_selec_espec = "HLP=000011326":U
           frame f_dlg_01_estabelecimento_selec_espec:private-data                                   = "HLP=000000000".



/*{include/i_fclfrm.i f_dlg_01_estabelecimento_selec_espec }*/
/*************************** Frame Definition End ***************************/

/*********************** User Interface Trigger Begin ***********************/


ON CHOOSE OF bt_empresa1 IN FRAME f_dlg_01_estabelecimento_selec_espec
DO:

    if  can-find( first tt_estabelecimento_empresa )
    and br_estabelecimento_selec_espec:num-selected-rows > 0 then do:
        assign v_log_method = browse br_estabelecimento_selec_espec:deselect-rows().
    end.

    for each tt_estabelecimento_empresa no-lock
        where tt_estabelecimento_empresa.tta_cod_empresa = v_cod_empres_usuar:
        reposition qr_estabelecimento_selec_espec to recid(recid(tt_estabelecimento_empresa)) no-error.
        if  br_estabelecimento_selec_espec:select-focused-row() then.
    end.

    find first tt_estabelecimento_empresa no-lock no-error.
    reposition qr_estabelecimento_selec_espec to recid(recid(tt_estabelecimento_empresa)) no-error.    





END. /* ON CHOOSE OF bt_empresa1 IN FRAME f_dlg_01_estabelecimento_selec_espec */

ON CHOOSE OF bt_hel2 IN FRAME f_dlg_01_estabelecimento_selec_espec
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_dlg_01_estabelecimento_selec_espec */

ON CHOOSE OF bt_nenhum IN FRAME f_dlg_01_estabelecimento_selec_espec
DO:

    if  can-find( first tt_estabelecimento_empresa )
    and br_estabelecimento_selec_espec:num-selected-rows > 0 then do:
        assign v_log_method = browse br_estabelecimento_selec_espec:deselect-rows().
    end.


END. /* ON CHOOSE OF bt_nenhum IN FRAME f_dlg_01_estabelecimento_selec_espec */

ON CHOOSE OF bt_todos IN FRAME f_dlg_01_estabelecimento_selec_espec
DO:

    if  not can-find(first tt_estabelecimento_empresa ) then
        return no-apply. 

    assign v_log_method = session:set-wait-state('general')
           v_num_lin = br_estabelecimento_selec_espec:num-iterations.
    br_estabelecimento_selec_espec:deselect-rows().
    apply "home" to br_estabelecimento_selec_espec in frame f_dlg_01_estabelecimento_selec_espec.
    do  v_num_cont = 1 to v_num_lin:
        if  br_estabelecimento_selec_espec:is-row-selected(v_num_lin) then leave.
        if  br_estabelecimento_selec_espec:select-row(v_num_cont) then.
        if  v_num_cont mod v_num_lin = 0 then do:
            apply "page-down" to br_estabelecimento_selec_espec in frame f_dlg_01_estabelecimento_selec_espec.
            assign v_num_cont = 0.
        end.  
    end.  
    assign v_log_method = session:set-wait-state('').

END. /* ON CHOOSE OF bt_todos IN FRAME f_dlg_01_estabelecimento_selec_espec */

ON VALUE-CHANGED OF rs_estabelecimento_selec_espec IN FRAME f_dlg_01_estabelecimento_selec_espec
DO:

    run pi_open_estabelecimento_selec_espec.
END. /* ON VALUE-CHANGED OF rs_estabelecimento_selec_espec IN FRAME f_dlg_01_estabelecimento_selec_espec */


/************************ User Interface Trigger End ************************/

/**************************** Frame Trigger Begin ***************************/


ON HELP OF FRAME f_dlg_01_estabelecimento_selec_espec ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_dlg_01_estabelecimento_selec_espec */

ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_01_estabelecimento_selec_espec ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
        no-undo.


    /************************** Variable Definition End *************************/


    /* Begin_Include: i_right_mouse_down_dialog_box */
    if  (self:type <> "DIALOG-BOX" /*l_dialog_box*/ )
    and (self:type <> "FRAME" /*l_frame*/      )
    and (self:type <> "text" /*l_text*/       )
    and (self:type <> "IMAGE" /*l_image*/      )
    and (self:type <> "RECTANGLE" /*l_rectangle*/  )
    then do:

        assign v_wgh_frame = self:parent.

        if  self:type        = "fill-in" /*l_fillin*/ 
        and v_wgh_frame:type = "Browse" /*l_browse*/  then
            return no-apply.

        if  valid-handle(self:popup-menu) = yes then
            return no-apply.

        assign v_wgh_frame = self:frame.

        if  (v_wgh_frame:type <> "DIALOG-BOX" /*l_dialog_box*/ ) and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame     = v_wgh_frame:frame.
        end /* if */.
        assign v_nom_title_aux    = v_wgh_frame:title
               v_wgh_frame:title  = self:help.
    end /* if */.
    /* End_Include: i_right_mouse_down_dialog_box */

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_01_estabelecimento_selec_espec */

ON RIGHT-MOUSE-UP OF FRAME f_dlg_01_estabelecimento_selec_espec ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
        no-undo.


    /************************** Variable Definition End *************************/


    /* Begin_Include: i_right_mouse_up_dialog_box */
    if  (self:type <> "DIALOG-BOX" /*l_dialog_box*/ )
    and (self:type <> "FRAME" /*l_frame*/      )
    and (self:type <> "text" /*l_text*/       )
    and (self:type <> "IMAGE" /*l_image*/      )
    and (self:type <> "RECTANGLE" /*l_rectangle*/  )
    then do:

        assign v_wgh_frame = self:parent.

        if  self:type        = "fill-in" /*l_fillin*/ 
        and v_wgh_frame:type = "Browse" /*l_browse*/  then
            return no-apply.

        if  valid-handle(self:popup-menu) = yes then
            return no-apply.

        assign v_wgh_frame        = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX" /*l_dialog_box*/ ) and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame     = v_wgh_frame:frame.
        end /* if */.
        assign v_wgh_frame:title  = v_nom_title_aux.
    end /* if */.

    /* End_Include: i_right_mouse_up_dialog_box */

END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_01_estabelecimento_selec_espec */

ON WINDOW-CLOSE OF FRAME f_dlg_01_estabelecimento_selec_espec
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_dlg_01_estabelecimento_selec_espec */


/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/


ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:


        apply "choose" to bt_hel2 in frame f_dlg_01_estabelecimento_selec_espec.





END. /* ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help */

ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help
DO:

    /************************* Variable Definition Begin ************************/

    def var v_cod_release
        as character
        format "x(12)":U
        no-undo.
    def var v_nom_prog
        as character
        format "x(8)":U
        no-undo.
    def var v_nom_prog_ext
        as character
        format "x(8)":U
        label "Nome Externo"
        no-undo.


    /************************** Variable Definition End *************************/


        assign v_nom_prog     = substring(frame f_dlg_01_estabelecimento_selec_espec:title, 1, max(1, length(frame f_dlg_01_estabelecimento_selec_espec:title) - 10)).
        if  v_nom_prog = ? then
            assign v_nom_prog = "".

        assign v_nom_prog     = v_nom_prog
                              + chr(10)
                              + "fnc_estabelecimento_selec_espec":U.




    assign v_nom_prog_ext = "prgint/utb/utb071za.p":U
           v_cod_release  = trim(" 1.00.00.014":U).
/*    run prgtec/btb/btb901zb.p (Input v_nom_prog,
                               Input v_nom_prog_ext,
                               Input v_cod_release) /*prg_fnc_about*/. */
{include/sobre5.i}
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */


/***************************** Menu Trigger End *****************************/


/****************************** Main Code Begin *****************************/


/* Begin_Include: i_version_extract */
/*{include/i-ctrlrp5.i fnc_estabelecimento_selec_espec}*/


def new global shared var v_cod_arq
    as char  
    format 'x(60)'
    no-undo.
def new global shared var v_cod_tip_prog
    as character
    format 'x(8)'
    no-undo.

def stream s-arq.

if  v_cod_arq <> '' and v_cod_arq <> ?
then do:
    run pi_version_extract ('fnc_estabelecimento_selec_espec':U, 'prgint/utb/utb071za.p':U, '1.00.00.014':U, 'pro':U).
end /* if */.



/* End_Include: i_version_extract */

if  search("prgtec/btb/btb906za.r") = ? and search("prgtec/btb/btb906za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/btb/btb906za.py".
    else do:
        message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/btb/btb906za.py"
               view-as alert-box error buttons ok.
        stop.
    end.
end.
else
    run prgtec/btb/btb906za.py /*prg_fnc_verify_controls*/.

/* Begin_Include: i_verify_security */
if  search("prgtec/men/men901za.r") = ? and search("prgtec/men/men901za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/men/men901za.py".
    else do:
        message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/men/men901za.py"
               view-as alert-box error buttons ok.
        return.
    end.
end.
else
    run prgtec/men/men901za.py (Input 'fnc_estabelecimento_selec_espec') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n∆o Ç um programa v†lido Datasul ! */
    run pi_messages (input "show",
                     input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'fnc_estabelecimento_selec_espec')) /*msg_2014*/.
    return.
end /* if */.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show",
                     input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'fnc_estabelecimento_selec_espec')) /*msg_2012*/.
    return.
end /* if */.
/* End_Include: i_verify_security */



/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'fnc_estabelecimento_selec_espec' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'fnc_estabelecimento_selec_espec'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.


/* End_Include: i_log_exec_prog_dtsul_ini */



/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_dlg_01_estabelecimento_selec_espec:title = frame f_dlg_01_estabelecimento_selec_espec:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.00.014":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_dlg_01_estabelecimento_selec_espec = menu m_help:handle.


/* End_Include: i_std_dialog_box */
{include/title5.i f_dlg_01_estabelecimento_selec_espec FRAME}


assign v_cod_modul_exec = p_cod_modul_exec.

pause 0 before-hide.
view frame f_dlg_01_estabelecimento_selec_espec.

main_block:
do on endkey undo main_block, leave main_block
                on error undo main_block, leave main_block.

    /* ix_p10_fnc_estabelecimento_selec_espec */
    enable rs_estabelecimento_selec_espec
           br_estabelecimento_selec_espec
           bt_ok
           bt_can
           bt_hel2
           bt_todos
           bt_empresa1
           bt_nenhum
           with frame f_dlg_01_estabelecimento_selec_espec.

    /* ix_p20_fnc_estabelecimento_selec_espec */

    if not retry 
    then do:
        assign v_des_estab_select_aux_2 = v_des_estab_select.

        run pi_vld_permissao_usuar_estab_empres (Input v_cod_modul_exec).

        assign v_des_estab_select = v_des_estab_select_aux_2.
        run pi_open_estabelecimento_selec_espec.      

        if v_des_estab_select = " "
        or v_des_estab_select = ? then
            apply "choose" to bt_empresa1 in frame f_dlg_01_estabelecimento_selec_espec.
        else do:
            do v_num_contador = 1 to num-entries(v_des_estab_select):
                find tt_estabelecimento_empresa no-lock
                    where tt_estabelecimento_empresa.tta_cod_estab = entry(v_num_contador, v_des_estab_select) no-error.
                reposition qr_estabelecimento_selec_espec to recid(recid(tt_estabelecimento_empresa)) no-error.

                if avail tt_estabelecimento_empresa then do:
                   if  br_estabelecimento_selec_espec:select-focused-row() then.
                end.
            end.
            find first tt_estabelecimento_empresa no-lock no-error.
            reposition qr_estabelecimento_selec_espec to recid(recid(tt_estabelecimento_empresa)) no-error.    
        end.
    end.            

    wait-for go of frame f_dlg_01_estabelecimento_selec_espec.

    assign v_num_select_row = browse br_estabelecimento_selec_espec:num-selected-rows.

    if  v_num_select_row <> 0 then do:
        assign v_des_estab_select = " ".
        do v_num_row_a = 1 to v_num_select_row:
            assign v_log_method = browse br_estabelecimento_selec_espec:fetch-selected-row(v_num_row_a).
            if v_des_estab_select = " " then
                assign v_des_estab_select = tt_estabelecimento_empresa.tta_cod_estab.
            else       
                assign v_des_estab_select = v_des_estab_select + "," + tt_estabelecimento_empresa.tta_cod_estab.                
        end.        
    end.
    else do:
       assign v_des_estab_select = " ".
    end.

    assign v_log_method = session:set-wait-state('').

end /* do main_block */.

hide frame f_dlg_01_estabelecimento_selec_espec.


/* Begin_Include: i_log_exec_prog_dtsul_fim */
if v_rec_log <> ? then do transaction:
    find log_exec_prog_dtsul where recid(log_exec_prog_dtsul) = v_rec_log exclusive-lock no-error.
    if  avail log_exec_prog_dtsul
    then do:
        assign log_exec_prog_dtsul.dat_fim_exec_prog_dtsul = today
               log_exec_prog_dtsul.hra_fim_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":":U,"":U).
    end /* if */.
    release log_exec_prog_dtsul.
end.

/* End_Include: i_log_exec_prog_dtsul_fim */

return.


/******************************* Main Code End ******************************/

/************************* Internal Procedure Begin *************************/

/*****************************************************************************
** Procedure Interna.....: pi_open_estabelecimento_selec_espec
** Descricao.............: pi_open_estabelecimento_selec_espec
** Criado por............: bre18490
** Criado em.............: 21/06/1999 14:14:20
** Alterado por..........: bre18490
** Alterado em...........: 28/06/1999 09:32:49
*****************************************************************************/
PROCEDURE pi_open_estabelecimento_selec_espec:

    /************************* Variable Definition Begin ************************/

    def var v_des_estab_select_aux
        as character
        format "x(2000)":U
        view-as editor max-chars 2000 no-word-wrap
        size 30 by 1
        bgcolor 15 font 2
        no-undo.


    /************************** Variable Definition End *************************/

    assign v_num_select_row = browse br_estabelecimento_selec_espec:num-selected-rows.

    if  v_num_select_row <> 0 then do:
        assign v_des_estab_select_aux = " ".
        do v_num_row_a = 1 to v_num_select_row:
            assign v_log_method = browse br_estabelecimento_selec_espec:fetch-selected-row(v_num_row_a).
            if v_des_estab_select_aux = " " then
                assign v_des_estab_select_aux = tt_estabelecimento_empresa.tta_cod_estab.
            else       
                assign v_des_estab_select_aux = v_des_estab_select_aux + "," + tt_estabelecimento_empresa.tta_cod_estab.                
        end.        
    end.
    else
        assign v_des_estab_select_aux = " ".
    assign v_log_method = session:set-wait-state('').

    /* case_block: */
    case input frame f_dlg_01_estabelecimento_selec_espec rs_estabelecimento_selec_espec:
        when "Por Estabelecimento" /*l_por_estabelecimento*/  then
            cod_estab_block:
            do:
                open query qr_estabelecimento_selec_espec
                    for each tt_estabelecimento_empresa
                        by tt_estabelecimento_empresa.tta_cod_estab.
            end /* do cod_estab_block */.
        when "Por Raz∆o Social" /*l_por_razao_social*/  then
            des_estab_block:
            do:
                open query qr_estabelecimento_selec_espec
                    for each tt_estabelecimento_empresa
                        by tt_estabelecimento_empresa.tta_nom_razao_social.
            end /* do des_estab_block */.
        when "Por Empresa" /*l_por_empresa*/  then
            empresa_block:
            do:
                open query qr_estabelecimento_selec_espec
                    for each tt_estabelecimento_empresa
                        by tt_estabelecimento_empresa.tta_cod_empresa.
            end /* do empresa_block */.
    end /* case case_block */.

    if v_des_estab_select_aux <> " " then do:
        do v_num_contador = 1 to num-entries(v_des_estab_select_aux):
            find tt_estabelecimento_empresa no-lock
                 where tt_estabelecimento_empresa.tta_cod_estab = entry(v_num_contador, v_des_estab_select_aux) no-error.
            reposition qr_estabelecimento_selec_espec to recid(recid(tt_estabelecimento_empresa)) no-error.
            if  br_estabelecimento_selec_espec:select-focused-row() then.
        end.
    end.
END PROCEDURE. /* pi_open_estabelecimento_selec_espec */
/*****************************************************************************
** Procedure Interna.....: pi_version_extract
** Descricao.............: pi_version_extract
** Criado por............: jaison
** Criado em.............: 31/07/1998 09:33:22
** Alterado por..........: tech14020
** Alterado em...........: 12/06/2006 09:09:21
*****************************************************************************/
PROCEDURE pi_version_extract:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_program
        as character
        format "x(08)"
        no-undo.
    def Input param p_cod_program_ext
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_version
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_program_type
        as character
        format "x(8)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_event_dic
        as character
        format "x(20)":U
        label "Evento"
        column-label "Evento"
        no-undo.
    def var v_cod_tabela
        as character
        format "x(28)":U
        label "Tabela"
        column-label "Tabela"
        no-undo.


    /************************** Variable Definition End *************************/

    if  can-do(v_cod_tip_prog, p_cod_program_type)
    then do:
        if p_cod_program_type = 'dic' then 
           assign p_cod_program_ext = replace(p_cod_program_ext, 'database/', '').

        output stream s-arq to value(v_cod_arq) append.

        put stream s-arq unformatted
            p_cod_program            at 1 
            p_cod_program_ext        at 43 
            p_cod_version            at 69 
            today                    at 84 format "99/99/99"
            string(time, 'HH:MM:SS') at 94 skip.

        if  p_cod_program_type = 'pro' then do:
            &if '{&emsbas_version}' > '1.00' &then
            find prog_dtsul 
                where prog_dtsul.cod_prog_dtsul = p_cod_program 
                no-lock no-error.
            if  avail prog_dtsul
            then do:
                &if '{&emsbas_version}' > '5.00' &then
                    if  prog_dtsul.nom_prog_dpc <> '' then
                        put stream s-arq 'DPC : ' at 5 prog_dtsul.nom_prog_dpc  at 15 skip.
                &endif
                if  prog_dtsul.nom_prog_appc <> '' then
                    put stream s-arq 'APPC: ' at 5 prog_dtsul.nom_prog_appc at 15 skip.
                if  prog_dtsul.nom_prog_upc <> '' then
                    put stream s-arq 'UPC : ' at 5 prog_dtsul.nom_prog_upc  at 15 skip.
            end /* if */.
            &endif
        end.

        if  p_cod_program_type = 'dic' then do:
            &if '{&emsbas_version}' > '1.00' &then
            assign v_cod_event_dic = ENTRY(1,p_cod_program ,'/':U)
                   v_cod_tabela    = ENTRY(2,p_cod_program ,'/':U). /* FO 1100.980 */
            find tab_dic_dtsul 
                where tab_dic_dtsul.cod_tab_dic_dtsul = v_cod_tabela 
                no-lock no-error.
            if  avail tab_dic_dtsul
            then do:
                &if '{&emsbas_version}' > '5.00' &then
                    if  tab_dic_dtsul.nom_prog_dpc_gat_delete <> '' and v_cod_event_dic = 'Delete':U then
                        put stream s-arq 'DPC-DELETE : ' at 5 tab_dic_dtsul.nom_prog_dpc_gat_delete  at 25 skip.
                &endif
                if  tab_dic_dtsul.nom_prog_appc_gat_delete <> '' and v_cod_event_dic = 'Delete':U then
                    put stream s-arq 'APPC-DELETE: ' at 5 tab_dic_dtsul.nom_prog_appc_gat_delete at 25 skip.
                if  tab_dic_dtsul.nom_prog_upc_gat_delete <> '' and v_cod_event_dic = 'Delete':U then
                    put stream s-arq 'UPC-DELETE : ' at 5 tab_dic_dtsul.nom_prog_upc_gat_delete  at 25 skip.
                &if '{&emsbas_version}' > '5.00' &then
                    if  tab_dic_dtsul.nom_prog_dpc_gat_write <> '' and v_cod_event_dic = 'Write':U then
                        put stream s-arq 'DPC-WRITE : ' at 5 tab_dic_dtsul.nom_prog_dpc_gat_write  at 25 skip.
                &endif
                if  tab_dic_dtsul.nom_prog_appc_gat_write <> '' and v_cod_event_dic = 'Write':U then
                    put stream s-arq 'APPC-WRITE: ' at 5 tab_dic_dtsul.nom_prog_appc_gat_write at 25 skip.
                if  tab_dic_dtsul.nom_prog_upc_gat_write <> '' and v_cod_event_dic = 'Write':U  then
                    put stream s-arq 'UPC-WRITE : ' at 5 tab_dic_dtsul.nom_prog_upc_gat_write  at 25 skip.
            end /* if */.
            &endif
        end.

        output stream s-arq close.
    end /* if */.

END PROCEDURE. /* pi_version_extract */
/*****************************************************************************
** Procedure Interna.....: pi_vld_permissao_usuar_estab_empres
** Descricao.............: pi_vld_permissao_usuar_estab_empres
** Criado por............: bre18732
** Criado em.............: 21/06/2002 16:19:21
** Alterado por..........: fut42625
** Alterado em...........: 12/08/2011 10:26:24
*****************************************************************************/
PROCEDURE pi_vld_permissao_usuar_estab_empres:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_modul_dtsul
        as character
        format "x(3)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_log_reg_corporat
        as logical
        format "Sim/N∆o"
        initial no
        view-as toggle-box
        label "Registro Corporativo"
        column-label "Registro Corporativo"
        no-undo.
    def var v_log_restric_estab
        as logical
        format "Sim/N∆o"
        initial no
        view-as toggle-box
        label "Usa Segur Estab"
        column-label "Usa Segur Estab"
        no-undo.
    def var v_nom_razao_social
        as character
        format "x(30)":U
        label "Raz∆o Social"
        column-label "Raz∆o Social"
        no-undo.


    /************************** Variable Definition End *************************/


    /* Begin_Include: i_vld_permissao_usuar_estab_empres */
    find last param_geral_apb no-lock no-error.
    if p_cod_modul_dtsul = "EMS2" /*l_ems_2*/  then do:
        assign v_log_reg_corporat = yes.
    end.
    else do:
        if avail param_geral_apb then 
            assign v_log_reg_corporat = param_geral_apb.log_reg_corporat.
    end.

    assign v_log_restric_estab = no.
    &IF DEFINED(BF_FIN_SEGUR_ESTABELEC) &THEN
        case p_cod_modul_dtsul:
            when "ACR" /*l_acr*/  then do:
                find last param_geral_acr no-lock no-error.
                if avail param_geral_acr then
                    assign v_log_restric_estab = param_geral_acr.log_restric_estab.
            end.
            when "APB" /*l_apb*/  then do:
                find last param_geral_apb no-lock no-error.
                if avail param_geral_apb then
                    assign v_log_restric_estab = param_geral_apb.log_restric_estab.
            end.
            when "EMS2" /*l_ems_2*/  then do:
            /* QUANDO FOR CHAMADO PELO EMS2 DEVERµ FAZER A RESTRIÄ«O DOS ESTABELECIMENTOS */        
                assign v_log_restric_estab = yes.
            end.
            otherwise
                assign v_log_restric_estab = no.
        end case.
    &ELSE
        if v_log_reg_corporat then
            assign v_log_restric_estab = yes.
    &ENDIF
    /* End_Include: i_vld_permissao_usuar_estab_empres */


    for each tt_usuar_grp_usuar.
        delete tt_usuar_grp_usuar.
    end.

    /* Cria TT com os grupos de usu†rios */
    for each usuar_grp_usuar where 
             usuar_grp_usuar.cod_usuario = v_cod_usuar_corren no-lock:
        find first tt_usuar_grp_usuar where 
                   tt_usuar_grp_usuar.cod_grp_usuar = usuar_grp_usuar.cod_grp_usuar
               and tt_usuar_grp_usuar.cod_usuario   = usuar_grp_usuar.cod_usuario no-lock no-error.
        if not avail tt_usuar_grp_usuar then do:
            create tt_usuar_grp_usuar.
            buffer-copy usuar_grp_usuar to tt_usuar_grp_usuar.
        end.
    end.
    /* Cria Grupo '*' */
    find first tt_usuar_grp_usuar where 
               tt_usuar_grp_usuar.cod_grp_usuar = "*"
           and tt_usuar_grp_usuar.cod_usuario   = v_cod_usuar_corren no-lock no-error.
    if not avail tt_usuar_grp_usuar then do:
        create tt_usuar_grp_usuar.
        assign tt_usuar_grp_usuar.cod_grp_usuar = "*"
               tt_usuar_grp_usuar.cod_usuario   = v_cod_usuar_corren.
    end.

    for each tt_empresa:
        delete tt_empresa.

    end.

    ASSIGN v_log_reg_corporat = YES.

    if v_log_reg_corporat = yes then do:
        for each ems.empresa no-lock:
            for each tt_usuar_grp_usuar:
                /* Verifica se o Usu†rio tem permiss∆o na Empresa */
                if not can-find(first ems.segur_unid_organ where
                                      segur_unid_organ.cod_unid_organ = empresa.cod_empresa 
                                 and (segur_unid_organ.cod_grp_usuar  = tt_usuar_grp_usuar.cod_grp_usuar
                                  or  segur_unid_organ.cod_grp_usuar  = "*")) then 
                   next.

                create tt_empresa.
                assign tt_empresa.tta_cod_empresa = empresa.cod_empresa.
                leave.
            end.
        end.
    end.
    else do:
        create tt_empresa.
        assign tt_empresa.tta_cod_empresa = v_cod_empres_usuar.
    end.

    for each tt_estabelecimento_empresa:
        delete tt_estabelecimento_empresa.
    end.

    for each tt_empresa no-lock:
        /* *****  ALTERACAO SOB DEMANDA PARA EVITAR MULTIPLOS ACESSOS A TABELA EMPRESA ******/
        assign v_nom_razao_social = "" /*l_*/ .
        find empresa no-lock where empresa.cod_empresa = tt_empresa.tta_cod_empresa no-error.
        if avail empresa then          
            assign v_nom_razao_social = empresa.nom_razao_social.

        for each estabelecimento where 
                 estabelecimento.cod_empresa = tt_empresa.tta_cod_empresa no-lock:
            if v_log_restric_estab then do:
                for each tt_usuar_grp_usuar:
                   /* Verifica se o Usu†rio tem permiss∆o no Estabelecimento */
                    if not can-find(first ems.segur_unid_organ where
                                            segur_unid_organ.cod_unid_organ = estabelecimento.cod_estab
                                       and (segur_unid_organ.cod_grp_usuar  = tt_usuar_grp_usuar.cod_grp_usuar
                                         or segur_unid_organ.cod_grp_usuar  = "*")) then 
                        next.

                    create tt_estabelecimento_empresa. 
                    assign tt_estabelecimento_empresa.tta_cod_estab        = estabelecimento.cod_estab
                           tt_estabelecimento_empresa.tta_nom_pessoa       = estabelecimento.nom_pessoa
                           tt_estabelecimento_empresa.tta_cod_empresa      = estabelecimento.cod_empresa
                           /* *****  ALTERACAO SOB DEMANDA PARA EVITAR MULTIPLOS ACESSOS A TABELA EMPRESA ******/
                           tt_estabelecimento_empresa.tta_nom_razao_social = v_nom_razao_social.
                    leave.
                end.
            end.
            else do:
                create tt_estabelecimento_empresa. 
                assign tt_estabelecimento_empresa.tta_cod_estab        = estabelecimento.cod_estab
                       tt_estabelecimento_empresa.tta_nom_pessoa       = estabelecimento.nom_pessoa
                       tt_estabelecimento_empresa.tta_cod_empresa      = estabelecimento.cod_empresa
                       /* *****  ALTERACAO SOB DEMANDA PARA EVITAR MULTIPLOS ACESSOS A TABELA EMPRESA ******/
                       tt_estabelecimento_empresa.tta_nom_razao_social = v_nom_razao_social.
            end.
        end.
    end.

    assign v_des_estab_select = "".
    for each tt_estabelecimento_empresa:
        if v_des_estab_select = "" then
            assign v_des_estab_select = tt_estabelecimento_empresa.tta_cod_estab.
        else
            assign v_des_estab_select = v_des_estab_select + "," + tt_estabelecimento_empresa.tta_cod_estab.
    end.
END PROCEDURE. /* pi_vld_permissao_usuar_estab_empres */


/************************** Internal Procedure End **************************/

/************************* External Procedure Begin *************************/



/************************** External Procedure End **************************/
&endif

/*************************************  *************************************/
/*****************************************************************************
**  Procedure Interna: pi_messages
**  Descricao........: Mostra Mensagem com Ajuda
*****************************************************************************/
PROCEDURE pi_messages:

    def input param c_action    as char    no-undo.
    def input param i_msg       as integer no-undo.
    def input param c_param     as char    no-undo.

    def var c_prg_msg           as char    no-undo.

    assign c_prg_msg = "messages/":U
                     + string(trunc(i_msg / 1000,0),"99":U)
                     + "/msg":U
                     + string(i_msg, "99999":U).

    if search(c_prg_msg + ".r":U) = ? and search(c_prg_msg + ".p":U) = ? then do:
        message "Mensagem nr. " i_msg "!!!":U skip
                "Programa Mensagem" c_prg_msg "n∆o encontrado."
                view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */
/******************  End of fnc_estabelecimento_selec_espec *****************/
