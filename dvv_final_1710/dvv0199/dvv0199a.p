/*****************************************************************************
    PROGRAMA..: dvv0199a.p
    OBJETIVO..: Relat¢rio de DVV
    AUTOR.....: NewTech
    DATA......: 28/11/2017
*****************************************************************************/
&SCOPED-DEFINE base emsfnd
    DEF BUFFER impressora             FOR {&base}.impressora.
    DEF BUFFER tip_imprsor            FOR {&base}.tip_imprsor.
    DEF BUFFER configur_layout_impres FOR {&base}.configur_layout_impres.
    DEF BUFFER configur_tip_imprsor   FOR {&base}.configur_tip_imprsor.
    DEF BUFFER servid_exec_imprsor    FOR {&base}.servid_exec_imprsor.
    DEF BUFFER prog_dtsul             FOR {&base}.prog_dtsul.
   /* DEF BUFFER empresa                FOR {&base}.empresa. */
    DEF BUFFER usuar_mestre           FOR {&base}.usuar_mestre.
    DEF BUFFER layout_impres_padr     FOR {&base}.layout_impres_padr.
    DEF BUFFER ped_exec               FOR {&base}.ped_exec.
    DEF BUFFER imprsor_usuar          FOR {&base}.imprsor_usuar.
    DEF BUFFER layout_impres          FOR {&base}.layout_impres.
    DEF BUFFER procedimento           FOR {&base}.procedimento.
    DEF BUFFER modul_dtsul            FOR {&base}.modul_dtsul.

{include/i-prgvrs.i dvv0199A "2.06.00.002"}
{esp/Moedas.i} //Include com o valor do c¢digo das moedas

{dvv/dvv0199.i} /* Definiá‰es Funá‰es */

DEF INPUT  PARAM TABLE FOR tt-param.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-rel.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-rel-total.

DEF VAR h-acomp     AS HANDLE   NO-UNDO.
DEF VAR de-aliq-imposto AS DECIMAL NO-UNDO.
DEF VAR l-taxado AS LOGICAL NO-UNDO.
DEF VAR i-moeda AS INTEGER NO-UNDO.

DEF VAR v_num_lote            LIKE item_lancto_ctbl.num_lote_ctbl              NO-UNDO.
DEF VAR v_num_lanc            LIKE item_lancto_ctbl.num_lancto_ctbl            NO-UNDO.
DEF VAR v_num_seq             LIKE item_lancto_ctbl.num_seq_lancto_ctbl        NO-UNDO.

DEF BUFFER bitem_lancto_ctbl FOR item_lancto_ctbl.
DEF TEMP-TABLE tt_ILC_movto_estoque NO-UNDO
    field cta_ctbl  LIKE item_lancto_ctbl.cod_cta_ctbl    
    field ccusto    LIKE item_lancto_ctbl.cod_ccusto      
    field cod_estab LIKE item_lancto_ctbl.cod_estab       
    field cod_un    LIKE item_lancto_ctbl.cod_unid_negoc 
    field ge_cod    LIKE grup-estoque.ge-codigo
    field natur     LIKE item_lancto_ctbl.ind_natur_lancto_ctbl 
    field dat_lanc  LIKE item_lancto_ctbl.dat_lancto_ctbl 
    field num_lote  LIKE item_lancto_ctbl.num_lote_ctbl    
    field num_lanc  LIKE item_lancto_ctbl.num_lancto_ctbl          
    field num_seq   LIKE item_lancto_ctbl.num_seq_lancto_ctbl
    INDEX lancto
          cta_ctbl  ASCENDING 
          ccusto    ASCENDING
          cod_estab ASCENDING
          cod_un    ASCENDING
          ge_cod    ASCENDING
          dat_lanc  ASCENDING
          natur     ASCENDING.


DEF BUFFER bdvv_log_provisao_estorno FOR dvv_log_provisao.
DEF VAR l-log AS LOGICAL NO-UNDO.

ASSIGN l-log = NO.

IF l-log THEN DO:
    MESSAGE "\\nqspv064\Datasul\dts11\ERP\newtech\Rodrigo\logdvv0199a.txt 1"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    OUTPUT TO \\nqspv064\Datasul\dts11\ERP\newtech\Rodrigo\logdvv0199a.txt.
    OUTPUT CLOSE.
END.

FIND FIRST param-global NO-LOCK NO-ERROR.

FIND FIRST mgcad.empresa NO-LOCK WHERE empresa.ep-codigo = param-global.empresa-prin.

FIND FIRST tt-param NO-LOCK NO-ERROR.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp("Processando relat¢rio DVV MI"). 
RUN dvv/bo-dvv-movto-det.p  PERSISTENT SET h-bo-dvv-movto-det.

RUN pi-executar.

DELETE PROCEDURE h-bo-dvv-movto-det.
run pi-finalizar in h-acomp.

/* *************** Procedure Definitions - Begin *************** */
PROCEDURE pi-executar:


    DEF VAR l-troca AS LOGICAL NO-UNDO.    

    /* 1. Busca as contas cont†beis que ser∆o utilizados no relat¢rio de acordo com a faixa parametrizada em tela. */
    FIND plano_cta_ctbl NO-LOCK WHERE
         plano_cta_ctbl.cod_plano_cta_ctbl      = "nitro"
     AND plano_cta_ctbl.ind_tip_plano_cta_ctbl = "Prim†rio"
     AND plano_cta_ctbl.dat_inic_valid         <= TODAY
     AND plano_cta_ctbl.dat_fim_valid          >= TODAY NO-ERROR.

    FOR EACH dvv_param_conta_mi NO-LOCK WHERE
             dvv_param_conta_mi.cod_estabel >= tt-param.c-estab-ini
         AND dvv_param_conta_mi.cod_estabel <= tt-param.c-estab-fim
         AND dvv_param_conta_mi.cod_cta_ctbl >= tt-param.c-cta-ctbl-ini
         AND dvv_param_conta_mi.cod_cta_ctbl <= tt-param.c-cta-ctbl-fim:

        IF NOT CAN-FIND(FIRST tt-cta-ctbl
                        WHERE tt-cta-ctbl.cod-conta   = SUBSTRING(dvv_param_conta_mi.cod_cta_ctbl, 1, 8)
                          and tt-cta-ctbl.cod-estabel = dvv_param_conta_mi.cod_estabel) THEN DO:

            FIND FIRST estabelecimento NO-LOCK
                 WHERE estabelecimento.cod_estab = dvv_param_conta_mi.cod_estabel NO-ERROR.

            CREATE tt-cta-ctbl.
            ASSIGN tt-cta-ctbl.cod-conta     = SUBSTRING(dvv_param_conta_mi.cod_cta_ctbl, 1, 8)
                   tt-cta-ctbl.cod-unid      = SUBSTRING(dvv_param_conta_mi.cod_cta_ctbl, 9, 3)
                   tt-cta-ctbl.cod-ccusto    = SUBSTRING(dvv_param_conta_mi.cod_cta_ctbl, 12, 4)
                   tt-cta-ctbl.cod-estabel   = dvv_param_conta_mi.cod_estabel
                   tt-cta-ctbl.cod-empresa   = estabelecimento.cod_empresa.
        END.
    END.

    
    /* 2. Busca as movimentaá‰es do recebimento na tabela de movimento de estoque.
          Estas informaá‰es n∆o s∆o contabilzadas detalhadamente. */
    run pi-busca-dados-estoque.

    def var v_cod_return                     as character       no-undo. /*local*/
    def var v_data as date no-undo.
    def var v_cod_indic_econ_apres           as character       no-undo. /*local*/
    def var v_val_cotac_indic_econ           as decimal         no-undo. /*local*/
    def var v_cod_indic_econ_base            as character       no-undo. /*local*/
    
    /* --- Encontrar cotacao entre Indic Base de Indic Apresentaá∆o ---*/
    run pi_retornar_indic_econ_finalid (Input cDolarVenda,
                                       Input today,
                                       output v_cod_indic_econ_base) /*pi_retornar_indic_econ_finalid*/.
    run pi_retornar_indic_econ_finalid (Input "Corrente",
                                       Input today,
                                       output v_cod_indic_econ_apres) /*pi_retornar_indic_econ_finalid*/.

    /* 3. Busca as movimentaá‰es das contas cont†beis. */
    FOR EACH tt-cta-ctbl:

        run pi-acompanhar in h-acomp("Buscando dados conta: " + tt-cta-ctbl.cod-conta).

        FOR each ITEM_lancto_ctbl NO-LOCK WHERE
                 ITEM_lancto_ctbl.cod_empresa        = tt-cta-ctbl.cod-empresa
             AND ITEM_lancto_ctbl.cod_estab          = tt-cta-ctbl.cod-estabel
             AND ITEM_lancto_ctbl.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl
             AND ITEM_lancto_ctbl.cod_cta_ctbl       = tt-cta-ctbl.cod-conta
            /*
             AND ITEM_lancto_ctbl.cod_unid_negoc     = tt-cta-ctbl.cod-unid
             AND ITEM_lancto_ctbl.cod_ccusto         = tt-cta-ctbl.cod-ccusto
             */
             
             and item_lancto_ctbl.dat_lancto_ctbl   >= tt-param.dt-ctbl-ini
             and item_lancto_ctbl.dat_lancto_ctbl   <= tt-param.dt-ctbl-fim
             AND ITEM_lancto_ctbl.cod_unid_negoc    >= tt-param.c-unid-negoc-ini
             AND ITEM_lancto_ctbl.cod_unid_negoc    <= tt-param.c-unid-negoc-fim
             
            BREAK BY ITEM_lancto_ctbl.num_lote_ctbl:
            


            IF FIRST-OF(ITEM_lancto_ctbl.num_lote_ctbl) THEN DO:

                FIND lote_ctbl NO-LOCK WHERE
                     lote_ctbl.num_lote_ctbl = ITEM_lancto_ctbl.num_lote_ctbl NO-ERROR.

            END.
           
            FIND FIRST dvv_log_provisao NO-LOCK WHERE 
                       dvv_log_provisao.num_lote_ctbl = ITEM_lancto_Ctbl.num_lote_ctbl AND 
                       dvv_log_provisao.cod_estabel   = ITEM_lancto_Ctbl.cod_estab NO-ERROR.
            IF AVAIL dvv_log_provisao THEN
               NEXT.
               


            /* aaa */
            FIND FIRST bdvv_log_provisao_estorno NO-LOCK WHERE 
                       bdvv_log_provisao_estorno.cod_estabel           = ITEM_lancto_Ctbl.cod_estab     AND
                       bdvv_log_provisao_estorno.num_lote_ctbl_estorno = ITEM_lancto_Ctbl.num_lote_ctbl NO-ERROR.

            IF AVAIL bdvv_log_provisao_estorno THEN
               NEXT.
               



            run pi_achar_cotac_indic_econ (Input v_cod_indic_econ_base,
                                          Input v_cod_indic_econ_apres,
                                          Input ITEM_lancto_ctbl.dat_lancto_ctbl,
                                          Input cRealVenda /*l_real*/,
                                          output v_data,
                                          output v_val_cotac_indic_econ,
                                          output v_cod_return) /*pi_achar_cotac_indic_econ*/.


            ASSIGN i-lin = i-lin + 1.



            /* Dados da contabilidade. */
            create b-tt-rel-base.
            assign b-tt-rel-base.tipo           = "T"
                   b-tt-rel-base.movto          = "Realizado"
                   b-tt-rel-base.conta          = ITEM_lancto_ctbl.cod_cta_ctbl
                   b-tt-rel-base.modulo         = lote_ctbl.cod_modul_dtsul
                   b-tt-rel-base.dta-ctbz       = ITEM_lancto_ctbl.dat_lancto_ctbl
                   b-tt-rel-base.num-lote       = ITEM_lancto_ctbl.num_lote_ctbl
                   b-tt-rel-base.num-lancto     = ITEM_lancto_ctbl.num_lancto_ctbl
                   b-tt-rel-base.num-seq-lancto = ITEM_lancto_ctbl.num_seq_lancto_ctbl
                   b-tt-rel-base.mov            = ITEM_lancto_ctbl.ind_natur_lancto_ctbl
                   b-tt-rel-base.moeda          = cRealVenda
                   b-tt-rel-base.cod-estabel    = ITEM_lancto_ctbl.cod_estab
                   b-tt-rel-base.dt-lote        = lote_ctbl.dat_ult_atualiz
                   b-tt-rel-base.cod-unid-negoc = ITEM_lancto_ctbl.cod_unid_negoc
                   b-tt-rel-base.cod-unid-negoc-gerenc = ITEM_lancto_ctbl.cod_unid_negoc
                   b-tt-rel-base.user-lote      = lote_ctbl.cod_usuAr_ult_atualiz
                   b-tt-rel-base.usuario-impl   = lote_ctbl.cod_usuAr_ult_atualiz .

            /* O relat¢rio dever† sempre apresentar os valores em reais.
               Caso o indicador econìmico seja diferente de Real buscar na apropriaá∆o o valor em reais. */
            IF item_lancto_ctbl.cod_histor_padr = "DVVP" THEN
                ASSIGN b-tt-rel-base.movto     = "Provisionado".

            IF item_lancto_ctbl.cod_histor_padr = "DVVE" THEN
                ASSIGN b-tt-rel-base.movto     = "Estornado".

            IF ITEM_lancto_ctbl.cod_indic_econ <> cRealVenda THEN DO:

                FOR FIRST aprop_lancto_ctbl NO-LOCK WHERE
                          aprop_lancto_ctbl.num_lote            = item_lancto_ctbl.num_lote
                      AND aprop_lancto_ctbl.num_lancto_ctbl     = item_lancto_ctbl.num_lancto_ctbl
                      AND aprop_lancto_ctbl.num_seq_lancto_ctbl = item_lancto_ctbl.num_seq_lancto_ctbl
                      AND aprop_lancto_ctbl.cod_finalid_econ    = "Corrente":


                    ASSIGN b-tt-rel-base.val-lancto = aprop_lancto_ctbl.val_lancto_ctbl.
                END. 
            END.
            ELSE
                ASSIGN b-tt-rel-base.val-lancto = ITEM_lancto_ctbl.val_lancto_ctbl.


            case lote_ctbl.cod_modul_dtsul:
                when "APB" then run pi-dados-titulo-apb.
                when "CEP" then run pi-dados-rec.
                when "FGL" THEN do:

                    ASSIGN b-tt-rel-base.doc-origem = replace(ITEM_lancto_ctbl.des_histor_lancto_ctbl, CHR(10), " ")
                           b-tt-rel-base.doc-origem = REPLACE(b-tt-rel-base.doc-origem, ";", ",").

                END.
                WHEN "CMG" THEN DO:
                    ASSIGN b-tt-rel-base.doc-origem = replace(ITEM_lancto_ctbl.des_histor_lancto_ctbl, CHR(10), " ")
                           b-tt-rel-base.doc-origem = REPLACE(b-tt-rel-base.doc-origem, ";", ",").
                END.
                WHEN "ACR" THEN DO:
                    RUN pi-dados-titulo-acr.
                END.
            end case.
        END.
    END.

    FOR EACH tt-rel WHERE
             tt-rel.tipo-col = "T":

        IF CAN-FIND(FIRST b-tt-rel-reg WHERE
                          b-tt-rel-reg.num-lote       = tt-rel.num-lote
                      AND b-tt-rel-reg.num-lancto     = tt-rel.num-lancto
                      AND b-tt-rel-reg.num-seq-lancto = tt-rel.num-seq-lancto
                      AND b-tt-rel-reg.tipo-col       = "D"
                      AND NOT b-tt-rel-reg.l-rastr-desp) OR
           NOT CAN-FIND(FIRST b-tt-rel-reg WHERE
                              b-tt-rel-reg.num-lote       = tt-rel.num-lote
                          AND b-tt-rel-reg.num-lancto     = tt-rel.num-lancto
                          AND b-tt-rel-reg.num-seq-lancto = tt-rel.num-seq-lancto
                          AND b-tt-rel-reg.tipo-col       = "D") THEN DO:

            ASSIGN tt-rel.l-rastr-desp = NO.
        END.
        else
            ASSIGN tt-rel.l-rastr-desp = YES.
    END.

END PROCEDURE.


procedure pi-dados-titulo-acr:



    FOR EACH aprop_lancto_ctbl no-lock where
             aprop_lancto_ctbl.num_lote_ctbl        = ITEM_lancto_ctbl.num_lote_ctbl      
         and aprop_lancto_ctbl.num_lancto_ctbl      = ITEM_lancto_ctbl.num_lancto_ctbl    
         and aprop_lancto_ctbl.num_seq_lancto_ctbl  = ITEM_lancto_ctbl.num_seq_lancto_ctbl,
        EACH val_aprop_ctbl_acr no-lock where
             val_aprop_ctbl_acr.num_id_aprop_lancto_ctbl = aprop_lancto_ctbl.num_id_aprop_lancto_ctbl
         AND val_aprop_ctbl_acr.cod_finalid_econ         = "Corrente":
    
        find aprop_ctbl_acr no-lock where
             aprop_ctbl_acr.cod_estab            = val_aprop_ctbl_acr.cod_estab
         and aprop_ctbl_acr.num_id_aprop_ctbl_acr = val_aprop_ctbl_acr.num_id_aprop_ctbl_acr no-error.
    
        find movto_tit_acr no-lock where
             movto_tit_acr.cod_estab             = aprop_ctbl_acr.cod_estab
         and movto_tit_acr.num_id_movto_tit_acr   = aprop_ctbl_acr.num_id_movto_tit_acr no-error.
    
        find tit_acr no-lock where
             tit_acr.cod_estab       = movto_tit_acr.cod_estab
         and tit_acr.num_id_tit_acr   = movto_tit_acr.num_id_tit_acr no-error.
    
        ASSIGN b-tt-rel-base.doc-origem = tit_acr.cod_estab + "|" + tit_acr.cod_espec_docto + "|" + tit_acr.cod_ser_docto + "|" + tit_acr.cod_tit_acr + "|" + tit_acr.cod_parcela
               b-tt-rel-base.usuario-impl = movto_tit_acr.cod_usuario.
    END.
END.


procedure pi-dados-titulo-apb:

    DEF VAR c_processo AS CHAR NO-UNDO.
    DEF VAR v_tot_base AS DEC  NO-UNDO.
    DEF VAR v_tot_parc AS DEC  NO-UNDO.
    DEF VAR i-num-id-tit-ap AS INTEGER NO-UNDO.
    DEF VAR i-num-id-tit-ap-ant AS INTEGER NO-UNDO. 

    run pi-acompanhar in h-acomp("Dados titulo apb: " + tt-cta-ctbl.cod-conta).

    FOR EACH aprop_lancto_ctbl no-lock where
             aprop_lancto_ctbl.num_lote_ctbl        = ITEM_lancto_ctbl.num_lote_ctbl
         and aprop_lancto_ctbl.num_lancto_ctbl      = ITEM_lancto_ctbl.num_lancto_ctbl
         and aprop_lancto_ctbl.num_seq_lancto_ctbl  = ITEM_lancto_ctbl.num_seq_lancto_ctbl, 
        EACH val_aprop_ctbl_ap no-lock where
             val_aprop_ctbl_ap.num_id_aprop_lancto_ctbl = aprop_lancto_ctbl.num_id_aprop_lancto_ctbl
         AND val_aprop_ctbl_ap.cod_finalid_econ         = "Corrente":

        find aprop_ctbl_ap no-lock where
             aprop_ctbl_ap.cod_estab            = val_aprop_ctbl_ap.cod_estab
         and aprop_ctbl_ap.num_id_aprop_ctbl_ap = val_aprop_ctbl_ap.num_id_aprop_ctbl_ap no-error.

        find movto_tit_ap no-lock where
             movto_tit_ap.cod_estab             = aprop_ctbl_ap.cod_estab
         and movto_tit_ap.num_id_movto_tit_ap   = aprop_ctbl_ap.num_id_movto_tit_ap no-error.

        find tit_ap no-lock where
             tit_ap.cod_estab       = movto_tit_ap.cod_estab
         and tit_ap.num_id_tit_ap   = movto_tit_ap.num_id_tit_ap no-error.

        IF CAN-FIND(FIRST tt-rel WHERE
                          tt-rel.cdn-fornec     = tit_ap.cdn_fornecedor
                      AND tt-rel.cod-esp        = tit_ap.cod_espec_docto
                      AND tt-rel.cod-titulo     = tit_ap.cod_tit_ap
                      AND tt-rel.parcela        = tit_ap.cod_parcela
                      AND tt-rel.trans-ap       = movto_tit_ap.ind_trans_ap
                      AND tt-rel.num-lote       = ITEM_lancto_ctbl.num_lote_ctbl
                      AND tt-rel.num-lancto     = ITEM_lancto_ctbl.num_lancto_ctbl
                      AND tt-rel.num-seq-lancto = ITEM_lancto_ctbl.num_seq_lancto_ctbl) THEN
            NEXT.

        assign c_processo     = ""
               c-chave-refer  = "".
    
        if avail tit_ap then do:

            ASSIGN v_tot_base = 0.
            FOR EACH val_tit_ap NO-LOCK WHERE
                     val_tit_ap.cod_estab     = movto_tit_ap.cod_estab
                 and val_tit_ap.num_id_tit_ap = movto_tit_ap.num_id_tit_ap
                 AND val_tit_ap.cod_finalid_econ = "Corrente":

                ASSIGN v_tot_base = v_tot_base + val_tit_ap.val_origin_tit_ap.
            END.

            ASSIGN v-val-docto = val_aprop_ctbl_ap.val_aprop_ctbl
                   v_tot_parc  = v-val-docto. 

            IF movto_tit_ap.ind_trans_ap_abrev = "IMPL" THEN DO:

                ASSIGN i-num-id-tit-ap     = tit_ap.num_id_tit_ap
                       i-num-id-tit-ap-ant = tit_ap.num_id_tit_ap.

                if tit_ap.ind_tip_espec_docto = "Imposto Taxado" or
 		           tit_ap.ind_tip_espec_docto = "Imposto Retido" then do:

/*                     RUN pi-retorna-proc-orig (OUTPUT c_processo,       */
/*                                               OUTPUT i-num-id-tit-ap,  */
/*                                               OUTPUT de-aliq-imposto). */
/*                                                                        */
/*                     ASSIGN l-taxado = YES.                             */

                END.

                IF i-num-id-tit-ap-ant <> i-num-id-tit-ap THEN DO:
                    FIND b_tit_ap1 NO-LOCK
                        WHERE b_tit_ap1.cod_estab     = tit_ap.cod_estab
                          AND b_tit_ap1.num_id_tit_ap = i-num-id-tit-ap NO-ERROR.
                    RUN pi-chave-tit_ap IN h-bo-dvv-movto-det (INPUT ROWID(b_tit_ap1),
                                                               OUTPUT c-chave-refer).
                END.
                ELSE
                    RUN pi-chave-tit_ap IN h-bo-dvv-movto-det (INPUT ROWID(tit_ap),
                                                               OUTPUT c-chave-refer).
                                                               
                if tit_ap.cod_espec_docto = "CA" then do:
                
                    FOR EACH movto_comis_repres NO-LOCK
                        WHERE movto_comis_repres.cod_estab          = tit_ap.cod_estab
                              AND movto_comis_repres.num_id_tit_ap  = tit_ap.num_id_tit_ap:
                              
                        FIND tit_acr NO-LOCK
                            WHERE tit_acr.cod_estab      = movto_comis_repres.cod_estab
                              AND tit_acr.num_id_tit_acr = movto_comis_repres.num_id_tit_acr NO-ERROR.  
                    end.

                end.


                IF CAN-FIND(FIRST dvv_tit_ap_mi where
                                  dvv_tit_ap_mi.cod_estab       = tit_ap.cod_estab
                              and dvv_tit_ap_mi.num_id_tit_ap   = i-num-id-tit-ap) THEN DO:
                    
                    /* Esta tabela dvv_tit_ap comeáar† a ter registros a partir de 01/08/2013. */
                    for each dvv_tit_ap_mi no-lock where
                             dvv_tit_ap_mi.cod_estab       = tit_ap.cod_estab
                         and dvv_tit_ap_mi.num_id_tit_ap   = i-num-id-tit-ap:
            
                        for EACH  dvv_movto_mi_det no-lock 
                             WHERE dvv_movto_mi_det.cod_estabel   = dvv_tit_ap_mi.cod_estab
                               AND dvv_movto_mi_det.serie         = dvv_tit_ap_mi.serie
                               AND dvv_movto_mi_det.nr-nota-fis   = dvv_tit_ap_mi.nr-nota-fis
                               and dvv_movto_mi_det.tipo_despesa  = dvv_tit_ap_mi.cod_desp                               
                               and dvv_movto_mi_det.tp_doc_origem = dvv_tit_ap_mi.tp_doc_origem
                               and dvv_movto_mi_det.doc_origem    BEGINS dvv_tit_ap_mi.doc_origem:

                            IF dvv_movto_mi_det.VALOR_REAL_R$ = 0 THEN
                                NEXT.                      

                            IF v_tot_base <> v_tot_parc THEN DO:

                                FIND FIRST dvv_movto_mi no-lock where
                                           dvv_movto_mi.cod_estabel     = dvv_movto_mi_det.cod_estabel
                                       AND dvv_movto_mi.serie           = dvv_movto_mi_det.serie
                                       AND dvv_movto_mi.nr-nota-fis     = dvv_movto_mi_det.nr-nota-fis
                                       and dvv_movto_mi.tipo_despesa    = dvv_movto_mi_det.tipo_despesa
                                       AND dvv_movto_mi.it_codigo       = dvv_movto_mi_det.it_codigo NO-ERROR.

                                ASSIGN v-val-desp[1] = 0
                                       v-val-desp[2] = 0.
                                IF AVAIL dvv_movto_mi THEN
                                    ASSIGN v-val-desp[1] = ROUND((dvv_movto_mi.VALOR_PREV_R$ * v_tot_parc) / v_tot_base , 2).

                                ASSIGN v-val-desp[2] = ROUND((dvv_movto_mi_det.VALOR_REAL_R$ * v_tot_parc) / v_tot_base , 2).
                            END.

                            IF tit_ap.cod_espec_docto = "CA" THEN DO: /* Comissao */

                                ASSIGN v-val-desp[2] = 0.
                                FOR EACH movto_comis_repres NO-LOCK
                                    WHERE movto_comis_repres.cod_estab      = tit_ap.cod_estab
                                      AND movto_comis_repres.num_id_tit_ap  = tit_ap.num_id_tit_ap:

                                    FIND tit_acr NO-LOCK
                                        WHERE tit_acr.cod_estab      = tit_ap.cod_estab
                                          AND tit_acr.num_id_tit_acr = movto_comis_repres.num_id_tit_acr NO-ERROR.
                                    IF tit_acr.cod_tit_acr = dvv_movto_mi_det.nr-nota-fis THEN DO:
                                        CASE tit_ap.cod_indic_econ:
                                            WHEN cDolarVenda THEN
                                                ASSIGN i-moeda = iDolarVenda.
                                            WHEN cEuroVenda THEN
                                                ASSIGN i-moeda = iEuroVenda.
                                            OTHERWISE
                                                ASSIGN i-moeda = iRealVenda.
                                        END CASE.
                                        
                                        IF i-moeda <> iRealVenda THEN
                                        RUN cdp/cd0812.p (i-moeda,
                                                          iRealVenda,
                                                          movto_comis_repres.val_movto_comis,
                                                          tit_ap.dat_emis_docto,
                                                          OUTPUT v-val-desp[2]).
                                        ELSE
                                            ASSIGN v-val-desp[2] = movto_comis_repres.val_movto_comis.
                                    END.

                                END.

                            END.

                            RUN pi-cria-dados-dvv (INPUT c-chave-refer).
                        end.
                        
                    end.
                    
                END.
                ELSE DO:
                    FOR EACH dvv_movto_mi_det NO-LOCK where
                             /* dvv_movto_det.tp_doc_origem    = i-tp-doc-origem
                         AND */ dvv_movto_mi_det.doc_origem       = c-chave-refer:

                        IF dvv_movto_mi_det.VALOR_REAL_R$ = 0 THEN
                            NEXT.

                        IF v_tot_base <> v_tot_parc THEN DO:
                            
                            FIND FIRST dvv_movto_mi no-lock where
                                       dvv_movto_mi.cod_estabel     = dvv_movto_mi_det.cod_estabel
                                   AND dvv_movto_mi.serie           = dvv_movto_mi_det.serie
                                   AND dvv_movto_mi.nr-nota-fis     = dvv_movto_mi_det.nr-nota-fis
                                   and dvv_movto_mi.tipo_despesa    = dvv_movto_mi_det.tipo_despesa
                                   AND dvv_movto_mi.it_codigo       = dvv_movto_mi_det.it_codigo NO-ERROR.
    
                            ASSIGN v-val-desp[1] = 0
                                   v-val-desp[2] = 0.
                            IF AVAIL dvv_movto_mi THEN
                                ASSIGN v-val-desp[1] = ROUND((dvv_movto_mi.VALOR_PREV_R$ * v_tot_parc) / v_tot_base , 2).

                            ASSIGN v-val-desp[2] = ROUND((dvv_movto_mi_det.VALOR_REAL_R$ * v_tot_parc) / v_tot_base , 2).
                        END.

                        RUN pi-cria-dados-dvv (INPUT c-chave-refer).
                    END.
                    /*
                    /* 16/07/2013 - Paulo Barth - Regra para criar relacionamento entre despesa DEX e T°tulo AP. */
                    FOR EACH dex_movto NO-LOCK where
                             /* dex_movto.tp_doc_origem    = i-tp-doc-origem
                         AND */ dex_movto.doc_origem       = c-chave-refer:
        
                        RUN pi-cria-dados-dex.
                    END.
                    */
                END.
            END.
            ELSE DO:

                IF movto_tit_ap.ind_trans_ap BEGINS "Acerto Valor" THEN DO:

/*                     RUN pi-chave-tit_ap /* IN h-bo-dvv-movto-det*/ (INPUT ROWID(tit_ap),             */
/*                                                                     OUTPUT c-chave-refer).           */
/*                                                                                                      */
/*                     IF CAN-FIND(FIRST dvv_tit_ap_mi where                                            */
/*                                       dvv_tit_ap_mi.cod_estab       = tit_ap.cod_estab               */
/*                                   and dvv_tit_ap_mi.num_id_tit_ap   = tit_ap.num_id_tit_ap) THEN DO: */
/*                                                                                                      */
/*                         FOR FIRST dvv_tit_ap_mi no-lock where                                        */
/*                                   dvv_tit_ap_mi.cod_estab       = tit_ap.cod_estab                   */
/*                               and dvv_tit_ap_mi.num_id_tit_ap   = tit_ap.num_id_tit_ap:              */
/*                             ASSIGN c_processo = dvv_tit_ap_mi.nr-nota-fis.                           */
/*                         END.                                                                         */
/*                     END.                                                                             */
/*                     ELSE DO:                                                                         */
/*                         FOR FIRST dvv_movto_mi_det NO-LOCK where                                     */
/*                                   dvv_movto_mi_det.doc_origem = c-chave-refer:                       */
/*                             ASSIGN c_processo = dvv_movto_mi_det.nr-nota-fis.                        */
/*                         END.                                                                         */
/*                     END.                                                                             */

                    FOR FIRST val_movto_ap NO-LOCK WHERE
                              val_movto_ap.cod_estab           = movto_tit_ap.cod_estab
                          and val_movto_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap
                          AND val_movto_ap.cod_finalid_econ    = "Corrente":
                
                        ASSIGN v-val-docto = val_movto_ap.val_ajust_val_tit_ap.
                    END.
                END.
            END.
    
            /* Caso n∆o encontre despesa imprime o valor e chave da mediá∆o. */
            IF v-val-docto > 0 THEN DO:
    
                create b-tt-rel-reg.
                buffer-copy b-tt-rel-base to b-tt-rel-reg.
    
                ASSIGN b-tt-rel-reg.tipo-col = "D"
                       b-tt-rel-reg.movto    = "Realizado"
                       b-tt-rel-reg.val-lancto = 0
                       b-tt-rel-reg.val-doc    = v-val-docto
                       b-tt-rel-reg.tp-origem    = 2
                       b-tt-rel-reg.doc-origem   = c-chave-refer 
                       b-tt-rel-reg.num-processo = c_processo.

                FIND emitente NO-LOCK
                    WHERE emitente.cod-emitente = tit_ap.cdn_fornecedor NO-ERROR.

                /* Dados do t°tulo APB. */
                assign b-tt-rel-reg.cdn-fornec    = tit_ap.cdn_fornecedor
                       b-tt-rel-reg.nome-forn     = emitente.nome-abrev
                       b-tt-rel-reg.cod-esp       = tit_ap.cod_espec_docto
                       b-tt-rel-reg.cod-titulo    = tit_ap.cod_tit_ap
                       b-tt-rel-reg.parcela       = tit_ap.cod_parcela
                       b-tt-rel-reg.trans-ap      = movto_tit_ap.ind_trans_ap
                       b-tt-rel-reg.usuario-impl  = movto_tit_ap.cod_usuario.

                if tit_ap.ind_tip_espec_docto = "Imposto Taxado" or
 		           tit_ap.ind_tip_espec_docto = "Imposto Retido" THEN
                    ASSIGN b-tt-rel-reg.tp-origem = 15.

                ASSIGN v-val-docto = 0.

            END.
        END.
    END.
end procedure.

procedure pi-dados-rec:

    DEF VAR v_total AS DEC NO-UNDO.
    DEF VAR v_total_prev AS DEC NO-UNDO.
    DEF VAR v_conta AS CHAR NO-UNDO.
    DEF VAR v_ccusto AS CHAR NO-UNDO.
    DEF VAR v_val_ant AS DEC NO-UNDO.
    DEF VAR v_chave_maior AS CHAR NO-UNDO.
    DEF VAR v_val_arredond AS DEC NO-UNDO.
    DEF VAR v-valor-unit AS DEC NO-UNDO.
    DEF VAR c-chave-orig AS CHAR NO-UNDO.

    DEF VAR d-1 AS DECIMAL.

    DEF VAR d-2 AS DECIMAL.

    IF l-log THEN DO:
        OUTPUT TO \\nqspv064\Datasul\dts11\ERP\newtech\Rodrigo\logdvv0199a.txt APPEND.
        PUT "pi-dados-rec2" item_lancto_ctbl.dat_lancto_ctbl SKIP
            lote_ctbl.cod_modul_dtsul SKIP
            item_lancto_ctbl.cod_estab SKIP
            item_lancto_ctbl.cod_cta_ctbl SKIP
            item_lancto_ctbl.dat_lancto_ctbl
            .
        OUTPUT CLOSE.
    END.

    run pi-acompanhar in h-acomp("Dados REC: " + tt-cta-ctbl.cod-conta).
     
    tt-block:
    for each tt-movto-cep where
             tt-movto-cep.cod_modul_dtsul = lote_ctbl.cod_modul_dtsul
         and tt-movto-cep.cod_estab       = item_lancto_ctbl.cod_estab
         and tt-movto-cep.cod_cta_ctbl    = item_lancto_ctbl.cod_cta_ctbl
         and tt-movto-cep.dat_lancto_ctbl = item_lancto_ctbl.dat_lancto_ctbl
         AND tt-movto-cep.cod-unid-neg    = ITEM_lancto_ctbl.cod_unid_negoc:


                
        //IF ITEM_lancto_ctbl.num_seq_lancto_ctbl   <> 2833 THEN next.


          // Correá∆o pontual para Corrigir problema fechamento

     
        IF ITEM_lancto_ctbl.num_lote_ctbl       = 176305    AND
           ITEM_lancto_ctbl.num_lancto_ctbl     = 1         THEN DO:

           IF ITEM_lancto_ctbl.num_seq_lancto_ctbl = 4892      AND 
              tt-movto-cep.nro-docto               = "0000123" THEN NEXT. 

           IF ITEM_lancto_ctbl.num_seq_lancto_ctbl = 4901      AND 
              tt-movto-cep.nro-docto               <> "0000123" THEN NEXT. 
        END.
       
         if ITEM_lancto_ctbl.num_lote_ctbl = 213183
        and ITEM_lancto_ctbl.num_seq_lancto_ctbl = 7373
        and ITEM_lancto_ctbl.num_lancto_ctbl = 1
        and tt-movto-cep.val_lancto_ctbl <> 4553.37 then next. 
        
   
         if ITEM_lancto_ctbl.num_lote_ctbl = 243579
        and ITEM_lancto_ctbl.num_seq_lancto_ctbl = 7888
        and ITEM_lancto_ctbl.num_lancto_ctbl = 1
        and tt-movto-cep.val_lancto_ctbl <> 38.84 then next.
         if ITEM_lancto_ctbl.num_lote_ctbl = 243575
        and ITEM_lancto_ctbl.num_seq_lancto_ctbl = 7493
        and ITEM_lancto_ctbl.num_lancto_ctbl = 1
        and tt-movto-cep.val_lancto_ctbl > 2259
        AND tt-movto-cep.val_lancto_ctbl < 2260 then next.
        if ITEM_lancto_ctbl.num_lote_ctbl = 243582
        and ITEM_lancto_ctbl.num_seq_lancto_ctbl = 8369
        and ITEM_lancto_ctbl.num_lancto_ctbl = 1
        and tt-movto-cep.val_lancto_ctbl > 399
        AND tt-movto-cep.val_lancto_ctbl < 451 then next.
        
        
        // Fim do AJuste Fechamento.
        
        
        
        


        ASSIGN d-1 = d-1 + tt-movto-cep.val_lancto_ctbl.

        ASSIGN v_val_ant = 0.
        if index(item_lancto_ctbl.des_histor_lancto_ctbl,string(tt-movto-cep.ge-codigo)) = 0 THEN
            next.

        FOR EACH tt-acum-medicao: DELETE tt-acum-medicao. END.

        //busca lote
        RUN pi_busca_lote_lancto_item.

        IF v_num_seq = 0 THEN
            NEXT.



        /* Buscar nota recebimento para depois buscar as despesas relacionadas. */
        FIND docum-est NO-LOCK WHERE
             docum-est.serie-docto      = tt-movto-cep.serie-docto
         AND docum-est.nro-docto        = tt-movto-cep.nro-docto
         AND docum-est.cod-emitente     = tt-movto-cep.cod-emitente
         AND docum-est.nat-operacao     = tt-movto-cep.nat-operacao NO-ERROR.

        IF NOT AVAIL docum-est THEN DO:
            IF INDEX(b-tt-rel-base.doc-origem,tt-movto-cep.especie) = 0 THEN
                ASSIGN b-tt-rel-base.doc-origem = b-tt-rel-base.doc-origem + "Esp Mov: " + tt-movto-cep.especie + " "
                       b-tt-rel-base.tp-origem  = 16
                       v-val-docto              = tt-movto-cep.val_lancto_ctbl.
        END.

	 if not can-find(first rat-ordem NO-LOCK WHERE
                 rat-ordem.serie-docto  = docum-est.serie-docto
             AND rat-ordem.nro-docto    = docum-est.nro-docto
             AND rat-ordem.cod-emitente = docum-est.cod-emitente
             AND rat-ordem.nat-operacao = docum-est.nat-operacao) then do:

            FIND emitente WHERE
                 emitente.cod-emitente = docum-est.cod-emitente NO-LOCK NO-ERROR.
            IF NOT AVAIL emitente THEN
                FIND emitente WHERE
                     emitente.cod-emitente = tt-movto-cep.cod-emitente NO-LOCK NO-ERROR.

            ASSIGN b-tt-rel-base.cdn-fornec = emitente.cod-emitente
                   b-tt-rel-base.nome-forn  = emitente.nome-abrev.

            ASSIGN b-tt-rel-base.doc-origem = b-tt-rel-base.doc-origem + "Doc Sem Medicao: " + tt-movto-cep.nro-docto /*docum-est.nro-docto*/ + " "
                   b-tt-rel-base.tp-origem  = 7
                   v-val-docto              = tt-movto-cep.val_lancto_ctbl.
	 end.

        /*
        FIND FIRST item-doc-est NO-LOCK OF docum-est WHERE
                   item-doc-est.it-codigo = tt-movto-cep.it-codigo NO-ERROR.
        */
        FIND FIRST item-doc-est NO-LOCK OF docum-est WHERE
                   item-doc-est.it-codigo = tt-movto-cep.it-codigo and
                   item-doc-est.sequencia = tt-movto-cep.sequen-nf NO-ERROR.

        /*
        FOR EACH rat-ordem NO-LOCK WHERE
                 rat-ordem.serie-docto  = docum-est.serie-docto
             AND rat-ordem.nro-docto    = docum-est.nro-docto
             AND rat-ordem.cod-emitente = docum-est.cod-emitente
             AND rat-ordem.nat-operacao = docum-est.nat-operacao:
        */

        IF AVAIL item-doc-est THEN DO:

            FOR EACH rat-ordem OF item-doc-est NO-LOCK:
    
                FIND medicao-contrat NO-LOCK WHERE
                     medicao-contrat.nr-contrato     = rat-ordem.nr-contrato
                 AND medicao-contrat.num-seq-item    = rat-ordem.num-seq-item
                 AND medicao-contrat.numero-ordem    = rat-ordem.numero-ordem
                 AND medicao-contrat.num-seq-event   = rat-ordem.num-seq-event
                 AND medicao-contrat.num-seq-medicao = rat-ordem.num-seq-medicao NO-ERROR.
    
                if avail medicao-contrat then do:
    
                    run pi-chave-medicao-contrato /*IN h-bo-dvv-movto-det*/ (rowid(medicao-contrat), output c-chave-refer).
    
                    ASSIGN c-chave-orig  = c-chave-refer
                           c-chave-refer = c-chave-refer + "|" + docum-est.nro-docto.
                    
                    FIND tt-acum-medicao WHERE
                         tt-acum-medicao.chave = c-chave-refer NO-ERROR.
                    IF NOT AVAIL tt-acum-medicao THEN DO:
                        CREATE tt-acum-medicao.
                        ASSIGN tt-acum-medicao.chave      = c-chave-refer
                               tt-acum-medicao.chave-orig = c-chave-orig.
                    END.
    
                    ASSIGN v-val-docto               = (rat-ordem.val-medicao / item-doc-est.preco-total[1]) * tt-movto-cep.val_lancto_ctbl
                           tt-acum-medicao.val-docto = tt-acum-medicao.val-docto + v-val-docto.
    
                    IF v-val-docto > v_val_ant THEN DO:
                        ASSIGN v_val_ant     = v-val-docto
                               v_chave_maior = c-chave-refer.
                    END.
                END.
            END.
        END.
        ELSE DO:

            ASSIGN c-chave-refer = tt-movto-cep.nro-docto
                   c-chave-orig  = c-chave-refer + "|" + STRING(tt-movto-cep.num_id_movto)
                   c-chave-refer = c-chave-refer.
            
            FIND tt-acum-medicao WHERE
                 tt-acum-medicao.chave = c-chave-refer NO-ERROR.
            IF NOT AVAIL tt-acum-medicao THEN DO:
                CREATE tt-acum-medicao.
                ASSIGN tt-acum-medicao.chave      = c-chave-refer
                       tt-acum-medicao.chave-orig = c-chave-orig.
            END.

            ASSIGN v-val-docto               = tt-movto-cep.val_lancto_ctbl
                   tt-acum-medicao.val-docto = tt-acum-medicao.val-docto + v-val-docto.

            IF v-val-docto > v_val_ant THEN DO:
                ASSIGN v_val_ant     = v-val-docto
                       v_chave_maior = c-chave-refer.
            END.
        END.

        IF CAN-FIND(FIRST tt-acum-medicao) THEN DO:

            IF l-log THEN DO:
                OUTPUT TO \\nqspv064\Datasul\dts11\ERP\newtech\Rodrigo\logdvv0199a.txt APPEND.
                PUT "pi-dados-rec2 AVAIL tt-acum-medicao" item_lancto_ctbl.dat_lancto_ctbl SKIP
                    lote_ctbl.cod_modul_dtsul SKIP
                    item_lancto_ctbl.cod_estab SKIP
                    item_lancto_ctbl.cod_cta_ctbl SKIP
                    item_lancto_ctbl.dat_lancto_ctbl
                    .
                OUTPUT CLOSE.
            END.
                
            ASSIGN v-val-docto = 0.
            FOR EACH tt-acum-medicao:
                ASSIGN v-val-docto = v-val-docto + tt-acum-medicao.val-docto.
            END.
    
            IF v-val-docto <> tt-movto-cep.val_lancto_ctbl THEN DO:
                
                FOR FIRST tt-acum-medicao WHERE
                          tt-acum-medicao.chave = v_chave_maior:
                    
                    ASSIGN tt-acum-medicao.val-docto = tt-acum-medicao.val-docto + (tt-movto-cep.val_lancto_ctbl - v-val-docto).
                END.
            END.
    
            for each tt-acum-medicao:
                
                /* Acumula os totais das despesas. */
                ASSIGN v_total      = 0
                       v_total_prev = 0.
                FOR EACH dvv_movto_mi_det NO-LOCK WHERE
                         dvv_movto_mi_det.tp_doc_origem = 1
                     AND dvv_movto_mi_det.doc_origem    = tt-acum-medicao.chave:

                    IF dvv_movto_mi_det.VALOR_REAL_R$ = 0 THEN
                        NEXT.
    
                    FIND FIRST dvv_movto_mi no-lock where
                               dvv_movto_mi.cod_estabel     = dvv_movto_mi_det.cod_Estabel
                           AND dvv_movto_mi.serie           = dvv_movto_mi_det.serie
                           AND dvv_movto_mi.nr-nota-fis     = dvv_movto_mi_det.nr-nota-fis
                           and dvv_movto_mi.tipo_despesa    = dvv_movto_mi_det.tipo_despesa NO-ERROR.
                        
                    ASSIGN v_total = v_total + dvv_movto_mi_det.VALOR_REAL_R$.
                    
                    IF AVAIL dvv_movto_mi THEN
                        ASSIGN v_total_prev = v_total_prev + dvv_movto_mi.VALOR_PREV_R$.
                END.
    
                /* Calcula o rateio das despesas em cima do valor da mediá∆o, controlando o arredondamento. */
                ASSIGN v-val-docto = tt-acum-medicao.val-docto.
    
                ASSIGN v_val_arredond = 0.
                FOR EACH dvv_movto_mi_det NO-LOCK WHERE
                         dvv_movto_mi_det.tp_doc_origem = 1
                     AND dvv_movto_mi_det.doc_origem    = TRIM(tt-acum-medicao.chave)
                    BREAK BY dvv_movto_mi_det.doc_origem:

                    IF dvv_movto_mi_det.VALOR_REAL_R$ = 0 THEN
                        NEXT.
    
                    FIND FIRST dvv_movto_mi no-lock where
                               dvv_movto_mi.cod_estabel    = dvv_movto_mi_det.cod_estabel
                           AND dvv_movto_mi.serie          = dvv_movto_mi_det.serie
                           AND dvv_movto_mi.nr-nota-fis    = dvv_movto_mi_det.nr-nota-fis
                           and dvv_movto_mi.tipo_despesa   = dvv_movto_mi_det.tipo_despesa NO-ERROR.
    
                    ASSIGN v-val-desp[2]  = (dvv_movto_mi_det.VALOR_REAL_R$ / v_total) * tt-acum-medicao.val-docto
                           v_val_arredond = v_val_arredond + v-val-desp[2].
    
                    ASSIGN v-val-desp[1] = 0.
                    IF AVAIL dvv_movto_mi THEN
                        ASSIGN v-val-desp[1]     = (dvv_movto_mi.VALOR_PREV_R$ / v_total) * tt-acum-medicao.val-docto.
    
                    IF LAST-OF(dvv_movto_mi_det.doc_origem) THEN DO:
    
                        IF v_val_arredond <> tt-acum-medicao.val-docto THEN
                            ASSIGN v-val-desp[2] = v-val-desp[2] + (tt-acum-medicao.val-docto - v_val_arredond).
                    END.
    
                    RUN pi-cria-dados-dvv (INPUT "").
        
                    RUN pi-grava-dados-desp-tit-ap.
                END.
    
                IF v-val-docto > 0 THEN DO:

                    create b-tt-rel-reg.
                    buffer-copy b-tt-rel-base to b-tt-rel-reg.
    
                    ASSIGN b-tt-rel-reg.tipo-col    = "D"
                           b-tt-rel-reg.movto       = "Realizado"
                           b-tt-rel-reg.val-lancto  = 0
                           b-tt-rel-reg.val-doc     = v-val-docto
                           b-tt-rel-reg.tp-origem   = 1
                           b-tt-rel-reg.doc-origem  = tt-acum-medicao.chave + " | " + STRING(tt-movto-cep.num_id_movto).

                    /*
                    MESSAGE b-tt-rel-reg.tipo-col   skip
                            b-tt-rel-reg.movto      skip
                            b-tt-rel-reg.val-lancto skip
                            b-tt-rel-reg.val-doc    skip
                            b-tt-rel-reg.vl-real    skip    
                            b-tt-rel-reg.tp-origem  skip
                            b-tt-rel-reg.doc-origem skip
                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                    */
                    
                    FIND ordem-compra WHERE
                         ordem-compra.numero-ordem = medicao-contrat.numero-ordem NO-LOCK NO-ERROR.
                    IF AVAIL ordem-compra THEN DO:
                        FIND emitente WHERE
                             emitente.cod-emitente = ordem-compra.cod-emitente NO-LOCK NO-ERROR.
                        ASSIGN b-tt-rel-reg.cdn-fornec  = emitente.cod-emitente
                               b-tt-rel-reg.nome-forn  = emitente.nome-abrev.
                    END.
                    
                    ASSIGN v-val-docto = 0.
        
                END.
            END.
        END.
        ELSE DO:
            
            IF l-log THEN DO:
                OUTPUT TO \\nqspv064\Datasul\dts11\ERP\newtech\Rodrigo\logdvv0199a.txt APPEND.
                PUT "pi-dados-rec2 NOT AVAIL tt-acum-medicao" item_lancto_ctbl.dat_lancto_ctbl SKIP
                    lote_ctbl.cod_modul_dtsul SKIP
                    item_lancto_ctbl.cod_estab SKIP
                    item_lancto_ctbl.cod_cta_ctbl SKIP
                    item_lancto_ctbl.dat_lancto_ctbl
                    .
                OUTPUT CLOSE.
            END.

            IF AVAIL docum-est THEN DO:
                FIND FIRST dvv_movto_comis_rep WHERE
                           dvv_movto_comis_rep.serie_docto  = docum-est.serie-docto  
                      and  dvv_movto_comis_rep.nro_docto    = docum-est.nro-docto    
                      and  dvv_movto_comis_rep.cod_emitente = docum-est.cod-emitente 
                      and  dvv_movto_comis_rep.nat_operacao = docum-est.nat-operacao NO-LOCK NO-ERROR.
                IF AVAIL dvv_movto_comis_rep THEN DO:
    
                    ASSIGN b-tt-rel-base.doc-origem = b-tt-rel-base.doc-origem + string(tt-movto-cep.cod-emitente) + "|" + tt-movto-cep.serie + "|" + tt-movto-cep.nro-docto + "|" + tt-movto-cep.nat-operacao
                           b-tt-rel-base.tp-origem  = 7
                           v-val-docto              = tt-movto-cep.val_lancto_ctbl.

                    FOR EACH dvv_movto_mi_det NO-LOCK WHERE
                             dvv_movto_mi_det.tp_doc_origem = 7
                         AND dvv_movto_mi_det.doc_origem    = b-tt-rel-base.doc-origem
                        BREAK BY dvv_movto_mi_det.doc_origem:

                        IF dvv_movto_mi_det.VALOR_REAL_R$ = 0 THEN
                            NEXT.

                        FIND FIRST dvv_movto_mi no-lock where
                                   dvv_movto_mi.cod_estabel    = dvv_movto_mi_det.cod_estabel
                               AND dvv_movto_mi.serie          = dvv_movto_mi_det.serie
                               AND dvv_movto_mi.nr-nota-fis    = dvv_movto_mi_det.nr-nota-fis
                               and dvv_movto_mi.tipo_despesa   = dvv_movto_mi_det.tipo_despesa NO-ERROR.
                        ASSIGN v-val-desp[2]  = (dvv_movto_mi_det.VALOR_REAL_R$ / v_total) * v-val-docto //tt-acum-medicao.val-docto
                               v_val_arredond = v_val_arredond + v-val-desp[2].

                        ASSIGN v-val-desp[1] = 0.
                        IF AVAIL dvv_movto_mi THEN
                            ASSIGN v-val-desp[1]     = (dvv_movto_mi.VALOR_PREV_R$ / v_total) * v-val-docto. //tt-acum-medicao.val-docto.

                        IF LAST-OF(dvv_movto_mi_det.doc_origem) THEN DO:

                            IF v_val_arredond <> v-val-docto /*tt-acum-medicao.val-docto*/ THEN
                                ASSIGN v-val-desp[2] = v-val-desp[2] + ( v-val-docto /*tt-acum-medicao.val-docto*/ - v_val_arredond).
                        END.

                        RUN pi-cria-dados-dvv (INPUT "").

                        //RUN pi-grava-dados-desp-tit-ap.
                    END.

                    //NEXT tt-block.
                END.
                ELSE DO:
                    FIND emitente WHERE
                        emitente.cod-emitente = tt-movto-cep.cod-emitente NO-LOCK NO-ERROR.
                    IF AVAIL emitente THEN
                    ASSIGN b-tt-rel-base.cdn-fornec = emitente.cod-emitente
                           b-tt-rel-base.nome-forn  = emitente.nome-abrev.

                    ASSIGN b-tt-rel-base.doc-origem = b-tt-rel-base.doc-origem + "Doc Sem Medicao/Recebimento: " + tt-movto-cep.nro-docto + " " + string(tt-movto-cep.sequen-nf) + " "
                           b-tt-rel-base.tp-origem  = 7.

                END.

            END.
            ELSE
                ASSIGN b-tt-rel-base.doc-origem = b-tt-rel-base.doc-origem + "Doc Sem Medicao/Estoque: " + tt-movto-cep.nro-docto + " ".

            IF NOT AVAIL dvv_movto_comis_rep THEN DO: //para n∆o criar em duplicidade as informaá‰es quando forem de origem comissos
                ASSIGN v-val-docto              = tt-movto-cep.val_lancto_ctbl.

                IF v-val-docto > 0 THEN DO:

                    create b-tt-rel-reg.
                    buffer-copy b-tt-rel-base to b-tt-rel-reg.

                    ASSIGN b-tt-rel-reg.tipo-col    = "D"
                           b-tt-rel-reg.movto       = "Realizado"
                           b-tt-rel-reg.val-lancto  = 0
                           b-tt-rel-reg.val-doc     = v-val-docto
                           b-tt-rel-reg.vl-real     = v-val-docto.

                    ASSIGN b-tt-rel-base.tp-origem = 0
                           b-tt-rel-base.doc-origem = "".

                    ASSIGN v-val-docto = 0.

                END.

            END.
        END.
    END.
end procedure.


PROCEDURE pi-retorna-proc-orig:

    DEF VAR c-chave-orig AS CHAR NO-UNDO.

    DEF OUTPUT PARAM p_processo AS CHAR NO-UNDO.
    DEF OUTPUT PARAM p-num-id-tit-ap AS INTEGER NO-UNDO.
    DEF OUTPUT PARAM p-aliquota AS DECIMAL NO-UNDO.

    for each compl_impto_retid_ap no-lock where
             compl_impto_retid_ap.num_id_tit_ap = tit_ap.num_id_tit_ap
         and compl_impto_retid_ap.cod_estab     = tit_ap.cod_estab:

        find first b_movto_tit_ap no-lock
             where b_movto_tit_ap.cod_estab           = compl_impto_retid_ap.cod_estab
               and b_movto_tit_ap.num_id_movto_tit_ap = compl_impto_retid_ap.num_id_movto_tit_ap_pai
             no-error.
        if  avail b_movto_tit_ap then do:

            /* Encontra o t°tulo duplicata relacionado ao imposto. */
            find b_tit_ap no-lock
                 where b_tit_ap.cod_estab     = b_movto_tit_ap.cod_estab
                   and b_tit_ap.num_id_tit_ap = b_movto_tit_ap.num_id_tit_ap no-error.

            if avail b_tit_ap then do:

                ASSIGN p-aliquota = compl_impto_retid_ap.val_aliq_impto.

                ASSIGN p-num-id-tit-ap = b_tit_ap.num_id_tit_ap.

                IF CAN-FIND(FIRST dvv_tit_ap_mi where
                                  dvv_tit_ap_mi.cod_estab       = b_tit_ap.cod_estab
                              and dvv_tit_ap_mi.num_id_tit_ap   = b_tit_ap.num_id_tit_ap) THEN DO:
        
                    /* Esta tabela dvv_tit_ap comeáar† a ter registros a partir de 01/08/2013. */
        
                    for FIRST dvv_tit_ap_mi no-lock where
                              dvv_tit_ap_mi.cod_estab       = b_tit_ap.cod_estab
                          and dvv_tit_ap_mi.num_id_tit_ap   = b_tit_ap.num_id_tit_ap:
                        ASSIGN p_processo = dvv_tit_ap_mi.nr-nota-fis.
                    end.
                END.
                ELSE DO:

                    RUN pi-chave-tit_ap IN h-bo-dvv-movto-det (INPUT ROWID(b_tit_ap),
                                                               OUTPUT c-chave-orig).
                    IF CAN-FIND(FIRST dvv_movto_mi_det NO-LOCK where
                                      dvv_movto_mi_det.doc_origem = c-chave-orig) THEN DO:
            
                        FOR FIRST dvv_movto_mi_det NO-LOCK where
                                  dvv_movto_mi_det.doc_origem = c-chave-orig:
                            ASSIGN p_processo = dvv_movto_mi_det.nr-nota-fis.
                        END.
                    END.
                    ELSE DO:
                        /*
                        FOR FIRST dex_movto NO-LOCK where
                                  dex_movto.doc_origem = c-chave-orig:
                            ASSIGN p_processo = dex_movto.nr_processo.
                        END.
                        */
                    END.
                END.
            END.
        end.
    end.


END PROCEDURE.


procedure pi-busca-dados-estoque:

    def var v-dat as date no-undo.
    def var v_val_movto as decimal no-undo.

    FOR EACH tt-movto-cep:
        DELETE tt-movto-cep.
    END.

    RUN pi-acompanhar IN h-acomp (INPUT "CEP.: Buscando contas.").
    FOR EACH tt-cta-ctbl,
        EACH conta-contab NO-LOCK WHERE
             conta-contab.ct-codigo = tt-cta-ctbl.cod-conta:
        if not can-find (first tt-conta-contab where
                               tt-conta-contab.ct-codigo = tt-cta-ctbl.cod-conta) then do:    
            CREATE tt-conta-contab.
            BUFFER-COPY conta-contab TO tt-conta-contab.
        end.
    END.

    DO v-dat = tt-param.dt-ctbl-ini TO tt-param.dt-ctbl-fim:
        
        RUN pi-acompanhar IN h-acomp (INPUT "CEP.: " + STRING(v-dat,"99/99/9999")).

        FOR EACH tt-conta-contab:

            IF l-log THEN DO:
                OUTPUT TO \\nqspv064\Datasul\dts11\ERP\newtech\Rodrigo\logdvv0199a.txt APPEND.
                PUT "pi-busca-dados-estoque tt-conta-contab: " tt-conta-contab.conta-contabil SKIP.
                OUTPUT CLOSE.
            END.


            FOR EACH movto-estoq NO-LOCK WHERE
                     movto-estoq.dt-trans       = v-dat
                 AND movto-estoq.ct-codigo      = tt-conta-contab.ct-codigo:

                IF l-log THEN DO:
                    OUTPUT TO \\nqspv064\Datasul\dts11\ERP\newtech\Rodrigo\logdvv0199a.txt APPEND.
                    PUT "pi-busca-dados-estoque movto-estoq" SKIP.
                    OUTPUT CLOSE.
                END.
    
                ASSIGN v_val_movto = 0
                       v_val_movto = (movto-estoq.valor-mat-m[1] +
                                      movto-estoq.valor-mob-m[1] +
                                      movto-estoq.valor-ggf-m[1] +
                                      movto-estoq.valor-icm      +
                                      movto-estoq.valor-ipi      +
                                      movto-estoq.val-cofins     +
                                      movto-estoq.valor-pis).
             
                /* Se entrada valor fica negativo. */
                IF movto-estoq.tipo-trans = 1 THEN
                    ASSIGN v_val_movto = v_val_movto * -1.
        
                FIND FIRST tt-movto-cep WHERE
                           tt-movto-cep.cod_modul_dtsul = "CEP"                   AND
                           tt-movto-cep.cod_estab       = movto-estoq.cod-estabel AND
                           tt-movto-cep.num_id_movto    = movto-estoq.nr-trans    NO-ERROR.
    
                IF NOT AVAIL tt-movto-cep THEN DO:

 		    find item where item.it-codigo = movto-estoq.it-codigo no-lock no-error.
    
                    CREATE tt-movto-cep.
                    ASSIGN tt-movto-cep.cod_modul_dtsul     = "CEP"
                           tt-movto-cep.num_id_movto        = movto-estoq.nr-trans
                           tt-movto-cep.cod_estab           = CAPS(movto-estoq.cod-estabel)
                           tt-movto-cep.dat_lancto_ctbl     = movto-estoq.dt-trans
                           tt-movto-cep.cod_cta_ctbl        = movto-estoq.ct-codigo
                           tt-movto-cep.cod_ccusto          = movto-estoq.sc-codigo
                           tt-movto-cep.val_lancto_ctbl     = v_val_movto
                           tt-movto-cep.especie             = CAPS({ininc/i03in218.i 04 movto-estoq.esp-docto})
                           tt-movto-cep.serie-docto         = movto-estoq.serie-docto  
                           tt-movto-cep.nro-docto           = movto-estoq.nro-docto    
                           tt-movto-cep.cod-emitente        = movto-estoq.cod-emitente 
                           tt-movto-cep.nat-operacao        = movto-estoq.nat-operacao
                           tt-movto-cep.it-codigo           = movto-estoq.it-codigo
                           tt-movto-cep.sequen-nf           = movto-estoq.sequen-nf
                           tt-movto-cep.ge-codigo           = item.ge-codigo
                           tt-movto-cep.cod-unid-neg        = movto-estoq.cod-unid-negoc
                           tt-movto-cep.refer-contab        = movto-estoq.refer-contab.

                    IF l-log THEN DO:
                        OUTPUT TO \\nqspv064\Datasul\dts11\ERP\newtech\Rodrigo\logdvv0199a.txt APPEND.
                        PUT "pi-busca-dados-estoque create" SKIP.
                        EXPORT DELIMITER ";" tt-movto-cep.
                        OUTPUT CLOSE.
                    END.
                END.
            END.
        END.
    END.
end procedure.


PROCEDURE pi_retornar_indic_econ_finalid:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_finalid_econ
        as character
        format "x(10)"
        no-undo.
    def Input param p_dat_transacao
        as date
        format "99/99/9999"
        no-undo.
    def output param p_cod_indic_econ
        as character
        format "x(8)"
        no-undo.


    /************************* Parameter Definition End *************************/

    find first histor_finalid_econ no-lock
         where histor_finalid_econ.cod_finalid_econ = p_cod_finalid_econ
           and histor_finalid_econ.dat_inic_valid_finalid <= p_dat_transacao
           and histor_finalid_econ.dat_fim_valid_finalid > p_dat_transacao
    &if "{&emsuni_version}" >= "5.01" &then
         use-index hstrfnld_id
    &endif
          /*cl_finalid_ativa of histor_finalid_econ*/ no-error.
    if  avail histor_finalid_econ then
        assign p_cod_indic_econ = histor_finalid_econ.cod_indic_econ.

END PROCEDURE. /* pi_retornar_indic_econ_finalid */
/*****************************************************************************
** Procedure Interna.....: pi_achar_cotac_indic_econ
** Descricao.............: pi_achar_cotac_indic_econ
** Criado por............: vladimir
** Criado em.............: // 
** Alterado por..........: fut1309_4
** Alterado em...........: 08/02/2006 16:12:34
*****************************************************************************/
PROCEDURE pi_achar_cotac_indic_econ:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_indic_econ_base
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_indic_econ_idx
        as character
        format "x(8)"
        no-undo.
    def Input param p_dat_transacao
        as date
        format "99/99/9999"
        no-undo.
    def Input param p_ind_tip_cotac_parid
        as character
        format "X(09)"
        no-undo.
    def output param p_dat_cotac_indic_econ
        as date
        format "99/99/9999"
        no-undo.
    def output param p_val_cotac_indic_econ
        as decimal
        format ">>>>,>>9.9999999999"
        decimals 10
        no-undo.
    def output param p_cod_return
        as character
        format "x(40)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_dat_cotac_mes
        as date
        format "99/99/9999":U
        no-undo.
    def var v_log_indic
        as logical
        format "Sim/N∆o"
        initial no
        no-undo.
    def var v_cod_indic_econ_orig            as character       no-undo. /*local*/
    def var v_val_cotac_indic_econ_base      as decimal         no-undo. /*local*/
    def var v_val_cotac_indic_econ_idx       as decimal         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    /* alteraá∆o sob demanda da atividade 148.681*/
    release cotac_parid.

    if  p_cod_indic_econ_base = p_cod_indic_econ_idx
    then do:
        /* **
         Quando a Base e o ÷ndice forem iguais, significa que a cotaá∆o pode ser percentual,
         portanto n∆o basta apenas retornar 1 e deve ser feita toda a pesquisa abaixo para
         encontrar a taxa da moeda no dia informado.
         Exemplo: D¢lar - D¢lar, poder°amos retornar 1
                  ANBID - ANBID, devemos retornar a taxa do dia.
        ***/
        find indic_econ no-lock
             where indic_econ.cod_indic_econ  = p_cod_indic_econ_base
               and indic_econ.dat_inic_valid <= p_dat_transacao
               and indic_econ.dat_fim_valid  >  p_dat_transacao
             no-error.
        if  avail indic_econ then do:
            if  indic_econ.ind_tip_cotac = "Valor" /*l_valor*/  then do:
                assign p_dat_cotac_indic_econ = p_dat_transacao
                       p_val_cotac_indic_econ = 1
                       p_cod_return           = "OK" /*l_ok*/ .
            end.
            else do:
                find cotac_parid no-lock
                     where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_base
                       and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                       and cotac_parid.dat_cotac_indic_econ = p_dat_transacao
                       and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
    &if "{&emsuni_version}" >= "5.01" &then
                     use-index ctcprd_id
    &endif
                      /*cl_acha_cotac of cotac_parid*/ no-error.
                if  not avail cotac_parid
                then do:
                    find parid_indic_econ no-lock
                         where parid_indic_econ.cod_indic_econ_base = p_cod_indic_econ_base
                           and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_idx
    &if "{&emsuni_version}" >= "5.01" &then
                         use-index prdndccn_id
    &endif
                          /*cl_acha_parid_param of parid_indic_econ*/ no-error.
                    /* block: */
                    case parid_indic_econ.ind_criter_busca:
                        when "Anterior" /*l_anterior*/ then find prev cotac_parid no-lock
                              where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_base
                                and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                                and cotac_parid.dat_cotac_indic_econ < p_dat_transacao
                                and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                and cotac_parid.val_cotac_indic_econ <> 0.0
    &if "{&emsuni_version}" >= "5.01" &then
                              use-index ctcprd_id
    &endif
                               /*cl_acha_cotac_anterior of cotac_parid*/ no-error.
                        when "Pr¢ximo" /*l_proximo*/ then  find next cotac_parid no-lock
                               where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_base
                                 and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                                 and cotac_parid.dat_cotac_indic_econ > p_dat_transacao
                                 and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                 and cotac_parid.val_cotac_indic_econ <> 0.0
    &if "{&emsuni_version}" >= "5.01" &then
                               use-index ctcprd_id
    &endif
                                /*cl_acha_cotac_posterior of cotac_parid*/ no-error.
                    end /* case block */.
                    if  not avail cotac_parid
                    then do:
                        assign p_cod_return = "358"                   + "," +
                                              p_cod_indic_econ_base   + "," +
                                              p_cod_indic_econ_idx    + "," +
                                              string(p_dat_transacao) + "," +
                                              p_ind_tip_cotac_parid.
                    end /* if */.
                    else do:
                        assign p_dat_cotac_indic_econ = cotac_parid.dat_cotac_indic_econ
                               p_val_cotac_indic_econ = cotac_parid.val_cotac_indic_econ
                               p_cod_return           = "OK" /*l_ok*/ .
                    end /* else */.
                end /* if */.
                else do:
                    assign p_dat_cotac_indic_econ = cotac_parid.dat_cotac_indic_econ
                           p_val_cotac_indic_econ = cotac_parid.val_cotac_indic_econ
                           p_cod_return           = "OK" /*l_ok*/ .
                end /* else */.
            end.
        end.
        else do:
            assign p_cod_return = "335".
        end.
    end /* if */.
    else do:
        find parid_indic_econ no-lock
             where parid_indic_econ.cod_indic_econ_base = p_cod_indic_econ_base
               and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_idx
             use-index prdndccn_id no-error.
        if  avail parid_indic_econ
        then do:


            /* Begin_Include: i_verifica_cotac_parid */
            /* verifica as cotacoes da moeda p_cod_indic_econ_base para p_cod_indic_econ_idx 
              cadastrada na base, de acordo com a periodicidade da cotacao (obtida na 
              parid_indic_econ, que deve estar avail)*/

            /* period_block: */
            case parid_indic_econ.ind_periodic_cotac:
                when "Di†ria" /*l_diaria*/ then
                    diaria_block:
                    do:
                        find cotac_parid no-lock
                            where cotac_parid.cod_indic_econ_base  = p_cod_indic_econ_base
                              and cotac_parid.cod_indic_econ_idx   = p_cod_indic_econ_idx
                              and cotac_parid.dat_cotac_indic_econ = p_dat_transacao
                              and cotac_parid.ind_tip_cotac_parid  = p_ind_tip_cotac_parid
                            use-index ctcprd_id no-error.
                        if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                        then do:
                            find parid_indic_econ no-lock
                                where parid_indic_econ.cod_indic_econ_base = p_cod_indic_econ_base
                                  and parid_indic_econ.cod_indic_econ_idx  = p_cod_indic_econ_idx
                                use-index prdndccn_id no-error.
                            /* block: */
                            case parid_indic_econ.ind_criter_busca:
                                when "Anterior" /*l_anterior*/ then 
                                    find prev cotac_parid no-lock
                                        where cotac_parid.cod_indic_econ_base  = p_cod_indic_econ_base
                                          and cotac_parid.cod_indic_econ_idx   = p_cod_indic_econ_idx
                                          and cotac_parid.dat_cotac_indic_econ < p_dat_transacao
                                          and cotac_parid.ind_tip_cotac_parid  = p_ind_tip_cotac_parid
                                          and cotac_parid.val_cotac_indic_econ <> 0.0
                                          &if '{&emsuni_version}' >= '5.01' &then
                                          use-index ctcprd_id
                                          &endif
                                          no-error.
                                when "Pr¢ximo" /*l_proximo*/ then  
                                    find next cotac_parid no-lock
                                        where cotac_parid.cod_indic_econ_base  = p_cod_indic_econ_base
                                          and cotac_parid.cod_indic_econ_idx   = p_cod_indic_econ_idx
                                          and cotac_parid.dat_cotac_indic_econ > p_dat_transacao
                                          and cotac_parid.ind_tip_cotac_parid  = p_ind_tip_cotac_parid
                                          and cotac_parid.val_cotac_indic_econ <> 0.0
                                          &if '{&emsuni_version}' >= '5.01' &then
                                          use-index ctcprd_id
                                          &endif
                                          no-error.
                            end /* case block */.
                        end /* if */.
                    end /* do diaria_block */.
                when "Mensal" /*l_mensal*/ then
                    mensal_block:
                    do:
                        assign v_dat_cotac_mes = date(month(p_dat_transacao), 1, year(p_dat_transacao))
                               &if yes = yes &then 
                               v_log_indic     = yes
                               &endif .
                        find cotac_parid no-lock
                            where cotac_parid.cod_indic_econ_base  = p_cod_indic_econ_base
                              and cotac_parid.cod_indic_econ_idx   = p_cod_indic_econ_idx
                              and cotac_parid.dat_cotac_indic_econ = v_dat_cotac_mes
                              and cotac_parid.ind_tip_cotac_parid  = p_ind_tip_cotac_parid
                            use-index ctcprd_id no-error.
                        if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                        then do:
                            /* block: */
                            case parid_indic_econ.ind_criter_busca:
                                when "Anterior" /*l_anterior*/ then
                                    find prev cotac_parid no-lock
                                        where cotac_parid.cod_indic_econ_base  = p_cod_indic_econ_base
                                          and cotac_parid.cod_indic_econ_idx   = p_cod_indic_econ_idx
                                          and cotac_parid.dat_cotac_indic_econ < v_dat_cotac_mes
                                          and cotac_parid.ind_tip_cotac_parid  = p_ind_tip_cotac_parid
                                          and cotac_parid.val_cotac_indic_econ <> 0.0
                                        use-index ctcprd_id no-error.
                                when "Pr¢ximo" /*l_proximo*/ then
                                    find next cotac_parid no-lock
                                        where cotac_parid.cod_indic_econ_base  = p_cod_indic_econ_base
                                          and cotac_parid.cod_indic_econ_idx   = p_cod_indic_econ_idx
                                          and cotac_parid.dat_cotac_indic_econ > v_dat_cotac_mes
                                          and cotac_parid.ind_tip_cotac_parid  = p_ind_tip_cotac_parid
                                          and cotac_parid.val_cotac_indic_econ <> 0.0
                                        use-index ctcprd_id no-error.
                            end /* case block */.
                        end /* if */.
                    end /* do mensal_block */.
                when "Bimestral" /*l_bimestral*/ then
                    bimestral_block:
                    do:
                    end /* do bimestral_block */.
                when "Trimestral" /*l_trimestral*/ then
                    trimestral_block:
                    do:
                    end /* do trimestral_block */.
                when "Quadrimestral" /*l_quadrimestral*/ then
                    quadrimestral_block:
                    do:
                    end /* do quadrimestral_block */.
                when "Semestral" /*l_semestral*/ then
                    semestral_block:
                    do:
                    end /* do semestral_block */.
                when "Anual" /*l_anual*/ then
                    anual_block:
                    do:
                    end /* do anual_block */.
            end /* case period_block */.
            /* End_Include: i_verifica_cotac_parid */


            if  parid_indic_econ.ind_orig_cotac_parid = "Outra Moeda" /*l_outra_moeda*/  and
                 parid_indic_econ.cod_finalid_econ_orig_cotac <> "" and
                 (not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0)
            then do:
                /* Cotaá∆o Ponte */
                run pi_retornar_indic_econ_finalid (Input parid_indic_econ.cod_finalid_econ_orig_cotac,
                                                    Input p_dat_transacao,
                                                    output v_cod_indic_econ_orig) /*pi_retornar_indic_econ_finalid*/.
                find parid_indic_econ no-lock
                    where parid_indic_econ.cod_indic_econ_base = v_cod_indic_econ_orig
                    and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_base
                    use-index prdndccn_id no-error.
                run pi_achar_cotac_indic_econ_2 (Input v_cod_indic_econ_orig,
                                                 Input p_cod_indic_econ_base,
                                                 Input p_dat_transacao,
                                                 Input p_ind_tip_cotac_parid,
                                                 Input p_cod_indic_econ_base,
                                                 Input p_cod_indic_econ_idx) /*pi_achar_cotac_indic_econ_2*/.

                if  avail cotac_parid and cotac_parid.val_cotac_indic_econ <> 0
                then do:
                    assign v_val_cotac_indic_econ_base = cotac_parid.val_cotac_indic_econ.
                    find parid_indic_econ no-lock
                        where parid_indic_econ.cod_indic_econ_base = v_cod_indic_econ_orig
                        and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_idx
                        use-index prdndccn_id no-error.
                    run pi_achar_cotac_indic_econ_2 (Input v_cod_indic_econ_orig,
                                                     Input p_cod_indic_econ_idx,
                                                     Input p_dat_transacao,
                                                     Input p_ind_tip_cotac_parid,
                                                     Input p_cod_indic_econ_base,
                                                     Input p_cod_indic_econ_idx) /*pi_achar_cotac_indic_econ_2*/.

                    if  avail cotac_parid and cotac_parid.val_cotac_indic_econ <> 0
                    then do:
                        assign v_val_cotac_indic_econ_idx = cotac_parid.val_cotac_indic_econ
                               p_val_cotac_indic_econ = v_val_cotac_indic_econ_idx / v_val_cotac_indic_econ_base
                               p_dat_cotac_indic_econ = cotac_parid.dat_cotac_indic_econ
                               p_cod_return = "OK" /*l_ok*/ .
                        return.
                    end /* if */.
                end /* if */.
            end /* if */.
            if  parid_indic_econ.ind_orig_cotac_parid = "Inversa" /*l_inversa*/  and
                 (not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0)
            then do:
                find parid_indic_econ no-lock
                    where parid_indic_econ.cod_indic_econ_base = p_cod_indic_econ_idx
                    and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_base
                    use-index prdndccn_id no-error.
                run pi_achar_cotac_indic_econ_2 (Input p_cod_indic_econ_idx,
                                                 Input p_cod_indic_econ_base,
                                                 Input p_dat_transacao,
                                                 Input p_ind_tip_cotac_parid,
                                                 Input p_cod_indic_econ_base,
                                                 Input p_cod_indic_econ_idx) /*pi_achar_cotac_indic_econ_2*/.

                if  avail cotac_parid and cotac_parid.val_cotac_indic_econ <> 0
                then do:
                    assign p_dat_cotac_indic_econ = cotac_parid.dat_cotac_indic_econ
                           p_val_cotac_indic_econ = 1 / cotac_parid.val_cotac_indic_econ
                           p_cod_return = "OK" /*l_ok*/ .
                    return.
                end /* if */.
            end /* if */.
        end /* if */.
        if v_log_indic = yes then do:
           if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
           then do:
               assign p_cod_return = "358"                 + "," +
                      p_cod_indic_econ_base   + "," +
                      p_cod_indic_econ_idx    + "," +
                      string(v_dat_cotac_mes) + "," +
                      p_ind_tip_cotac_parid.
           end /* if */.
           else do:
               assign p_dat_cotac_indic_econ = cotac_parid.dat_cotac_indic_econ
                      p_val_cotac_indic_econ = cotac_parid.val_cotac_indic_econ
                      p_cod_return           = "OK" /*l_ok*/ .
           end /* else */.
        end.
        else do:   
           if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
           then do:
               assign p_cod_return = "358"                 + "," +
                      p_cod_indic_econ_base   + "," +
                      p_cod_indic_econ_idx    + "," +
                      string(p_dat_transacao) + "," +
                      p_ind_tip_cotac_parid.
           end /* if */.
           else do:
               assign p_dat_cotac_indic_econ = cotac_parid.dat_cotac_indic_econ
                      p_val_cotac_indic_econ = cotac_parid.val_cotac_indic_econ
                      p_cod_return           = "OK" /*l_ok*/ .
           end /* else */.
        end.
        assign v_log_indic = no.
    end /* else */.
END PROCEDURE. /* pi_achar_cotac_indic_econ */

procedure pi-chave-tit_ap.
    
    def input parameter row-table as rowid no-undo.
    def output parameter c-doc-origem as char no-undo.

    find first tit_ap no-lock
        where rowid(tit_ap) = row-table no-error.

    if avail tit_ap then do:
        assign c-doc-origem = string(tit_ap.cod_estab) + "|" +
                              string(tit_ap.cod_espec_docto) + "|" +
                              string(tit_ap.cod_ser_docto) + "|" +
                              string(tit_ap.cdn_fornecedor) + "|" +
                              string(tit_ap.cod_tit_ap) + "|" +
                              string(tit_ap.cod_parcela).
    end.
end.

PROCEDURE pi-chave-medicao-contrato:

    /* tp-doc-origem = 1 */

    def input parameter row-table as rowid no-undo.
    def output parameter c-doc-origem as char no-undo.

    find first medicao-contrat NO-LOCK where
               rowid(medicao-contrat) = row-table no-error.
    if avail medicao-contrat then do:
        assign c-doc-origem = string(medicao-contrat.nr-contrato) + "|" +
                              string(medicao-contrat.num-seq-item) + "|" +
                              string(medicao-contrat.numero-ordem) + "|" +
                              string(medicao-contrat.num-seq-event) + "|" +
                              string(medicao-contrat.num-seq-medicao).
    end.

END PROCEDURE.
/* *************** Procedure Definitions - End *************** */

PROCEDURE pi-cria-dados-dvv:
    def input parameter p-chave-doc as char no-undo.

    /******************** Variaveis Definition ************************/
    DEF VAR d_dt_prev     like dvv_movto_mi.data_movto_prev NO-UNDO.
    DEF VAR v_val_prev_R$ like dvv_movto_mi.VALOR_PREV_R$   NO-UNDO.

    /*********************** Buffer Definition *************************/   
    DEF BUFFER b_dvv_movto_mi for dvv_movto_mi.



    /*********************** Main Code Begin *************************/   
    FIND FIRST dvv_movto_mi no-lock where
               dvv_movto_mi.cod_estabel  = dvv_movto_mi_det.cod_estabel
           and dvv_movto_mi.serie        = dvv_movto_mi_det.serie
           and dvv_movto_mi.nr-nota-fis  = dvv_movto_mi_det.nr-nota-fis
           and dvv_movto_mi.tipo_despesa = dvv_movto_mi_det.tipo_despesa NO-ERROR.

        IF dvv_movto_mi_det.VALOR_REAL_R$ = 0 THEN
        RETURN.
    assign d_dt_prev     = ?         /* ORS 30/06/2016 */
           v_val_prev_R$ = ?.
           
    IF AVAIL dvv_movto_mi THEN DO:
        assign d_dt_prev     = dvv_movto_mi.data_movto_prev           /* ORS 30/06/2016 */
               v_val_prev_R$ = dvv_movto_mi.VALOR_PREV_R$.
    END.

    /* aaa */
    create b-tt-rel-reg.
    buffer-copy b-tt-rel-base to b-tt-rel-reg.
    
    ASSIGN i-seq-classif = i-seq-classif + 1.
    ASSIGN b-tt-rel-reg.tipo-col   = "D"
           b-tt-rel-reg.val-lancto = 0
           b-tt-rel-reg.val-doc    = v-val-docto
           b-tt-rel-reg.seq-class  = i-seq-classif
           b-tt-rel-reg.l-rastr-desp  = YES.

    ASSIGN v-val-docto = 0.
    
    if p-chave-doc = "" then
        assign p-chave-doc = dvv_movto_mi_det.doc_origem.

    /* Dados da despesa. */
    assign b-tt-rel-reg.tp-origem    = dvv_movto_mi_det.tp_doc_origem
           b-tt-rel-reg.doc-origem   = p-chave-doc
           b-tt-rel-reg.num-processo = dvv_movto_mi_det.nr-nota-fis
           b-tt-rel-reg.it-codigo    = dvv_movto_mi_det.it_codigo  .
           
    for first nota-fiscal no-lock
        where nota-fiscal.cod-estabel = dvv_movto_mi_det.cod_estabel
          and nota-fiscal.serie       = dvv_movto_mi_det.serie
          and nota-fiscal.nr-nota-fis = dvv_movto_mi_det.nr-nota-fis.
         
        assign b-tt-rel-reg.dt-conf      = nota-fiscal.dt-emis-nota
               b-tt-rel-reg.cod-emitente = nota-fiscal.cod-emitente
               b-tt-rel-reg.nome-abrev   = nota-fiscal.nome-ab-cli
               b-tt-rel-reg.serie        = nota-fiscal.serie       
               b-tt-rel-reg.nr-nota-fis  = nota-fiscal.nr-nota-fis .
               
        find first emitente no-lock
           where emitente.cod-emitente = nota-fiscal.cod-emitente no-error.
        if avail emitente then do:
         
            ASSIGN b-tt-rel-reg.cod-emitente = emitente.cod-emitente
                   b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                   b-tt-rel-reg.segmento     = emitente.atividade
                   b-tt-rel-reg.regiao       = emitente.nome-mic-reg.
        end.           
    end.

    /* 18/09/2013 Paulo Barth Calcular o previsto rateando pela quantidade de dvv_movto_det.
                              pois as despesas 5, 28, 29, 30 podem ser realizadas mais de uma vez mas o
                              previsto Ç sempre o valor total. */
    IF v-val-desp[1] = 0 THEN
        assign v-val-desp[1] = v_val_prev_R$.
        /*ASSIGN v-val-desp[1] = fi-ret-prev-container(input dvv_movto_det.cod_estabel,
                                                     INPUT dvv_movto_det.num_processo,
                                                     INPUT dvv_movto_det.tipo_despesa,
                                                     INPUT v_val_prev_R$). /* dvv_movto.VALOR_PREV_R$ ORS 30/06/2016 */   */

    IF AVAIL dvv_movto_mi THEN DO:
        IF dvv_movto_mi.data_movto_prev <> ? THEN 
            ASSIGN d_dt_prev = dvv_movto_mi.data_movto_prev.  
        ELSE DO:
            ASSIGN d_dt_prev = ?.
        END.
    END.
    ELSE 
        ASSIGN d_dt_prev = ?.


    /* Valores da despesa. */
    assign b-tt-rel-reg.classif     = "DVV MI"
           b-tt-rel-reg.cod-desp    = dvv_movto_mi_det.tipo_despesa
           b-tt-rel-reg.dt-prev     = d_dt_prev
/*         b-tt-rel-reg.vl-prev     = IF v-val-desp[1] <> 0 THEN v-val-desp[1] ELSE (IF AVAIL dvv_movto THEN dvv_movto.VALOR_PREV_R$ ELSE 0)  *** ORS 30/06/2016 */
/*           b-tt-rel-reg.vl-prev     = IF v-val-desp[1] <> 0 THEN v-val-desp[1] ELSE (IF AVAIL dvv_movto THEN v_val_prev_R$ ELSE 0)*/
           b-tt-rel-reg.dt-real     = dvv_movto_mi_det.data_movto_real 
           b-tt-rel-reg.vl-real     = /*IF v-val-desp[2] <> 0 THEN v-val-desp[2] ELSE */ dvv_movto_mi_det.VALOR_REAL_R$
           b-tt-rel-reg.vl-prev     = IF dvv_movto_mi.VALOR_PREV_R$ <> 0 THEN dvv_movto_mi.VALOR_PREV_R$ ELSE 0
           b-tt-rel-reg.vl-real-tot = IF dvv_movto_mi_det.VALOR_REAL_R$ <> 0 THEN dvv_movto_mi_det.VALOR_REAL_R$ ELSE 0   .


    ASSIGN v-val-desp[1] = 0
           v-val-desp[2] = 0.

    for first DVV_TIPO_DESPESA no-lock where
              DVV_TIPO_DESPESA.codigo = dvv_movto_mi_det.tipo_despesa:
        assign b-tt-rel-reg.des-desp = DVV_TIPO_DESPESA.descricao.
    end.
    
    /*if avail movto_tit_ap then do:
         
        FIND FIRST aprop_ctbl_ap 
           WHERE aprop_ctbl_ap.cod_estab         = movto_tit_ap.cod_estab
           AND aprop_ctbl_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap
           AND aprop_ctbl_ap.cod_cta_ctbl        = b-tt-rel-reg.conta no-lock no-error.
        IF AVAIL aprop_ctbl_ap THEN DO:
        
            assign b-tt-rel-reg.vl-real = aprop_ctbl_ap.val_aprop_ctbl.  /* imprimir o valor rateado pela conta */
        end. 
    end.*/

   
    IF AVAIL tit_ap THEN DO:

        /* Dados do t°tulo APB. */
        assign b-tt-rel-reg.cdn-fornec    = tit_ap.cdn_fornecedor
               b-tt-rel-reg.cod-esp       = tit_ap.cod_espec_docto
               b-tt-rel-reg.cod-titulo    = tit_ap.cod_tit_ap
               b-tt-rel-reg.parcela       = tit_ap.cod_parcela.
               
        FIND FIRST emitente NO-LOCK
            WHERE emitente.cod-emitente = tit_ap.cdn_fornecedor NO-ERROR.
        IF AVAIL emitente THEN
            ASSIGN b-tt-rel-reg.cdn-fornec = tit_ap.cdn_fornecedor
                   b-tt-rel-reg.nome-forn  = emitente.nome-abrev.

        IF AVAIL movto_tit_ap THEN
            ASSIGN b-tt-rel-reg.trans-ap  = movto_tit_ap.ind_trans_ap
                   b-tt-rel-reg.usuario-impl = movto_tit_ap.cod_usuario.

        if tit_ap.ind_tip_espec_docto = "Imposto Taxado" or
           tit_ap.ind_tip_espec_docto = "Imposto Retido" THEN
            ASSIGN b-tt-rel-reg.tp-origem = 15.
            
        /* 
        ** Verifica se o t°tulo teve origem no GFE
        */ 
        IF tit_ap.cod_espec_docto = "TR" then do:
        
            assign b-tt-rel-reg.tp-origem = 10.

        end.

    END.
    
    find first tt-rel-total no-lock
       where tt-rel-total.cod-estabel    = b-tt-rel-reg.cod-estabel   
         and tt-rel-total.tipo           = b-tt-rel-reg.tipo          
         and tt-rel-total.conta          = b-tt-rel-reg.conta         
         and tt-rel-total.modulo         = b-tt-rel-reg.modulo        
         and tt-rel-total.num-lote       = b-tt-rel-reg.num-lote      
         and tt-rel-total.num-lancto     = b-tt-rel-reg.num-lancto    
         and tt-rel-total.num-seq-lancto = b-tt-rel-reg.num-seq-lancto
         and tt-rel-total.tp-origem      = b-tt-rel-reg.tp-origem     
         and tt-rel-total.doc-origem     = b-tt-rel-reg.doc-origem   no-error.
    if not avail tt-rel-total then do:
        create tt-rel-total.
        buffer-copy b-tt-rel-reg to tt-rel-total.
        assign tt-rel-total.vl-tot-prev = tt-rel-total.vl-tot-prev + b-tt-rel-reg.vl-prev
               tt-rel-total.vl-tot-real = tt-rel-total.vl-tot-real + b-tt-rel-reg.vl-real.

    end. 
    else do:
        assign tt-rel-total.vl-tot-prev = tt-rel-total.vl-tot-prev + b-tt-rel-reg.vl-prev
               tt-rel-total.vl-tot-real = tt-rel-total.vl-tot-real + b-tt-rel-reg.vl-real.

    end.
    
        IF l-log THEN DO:
            OUTPUT TO \\nqspv064\Datasul\dts11\ERP\newtech\Rodrigo\logdvv0199a.txt APPEND.
            put unformatted "p-chave-doc " p-chave-doc skip.
            for each tt-rel-total:
                put unformatted  
                    "tt-rel-total " tt-rel-total.doc-origem skip
                    "tt-rel-total " tt-rel-total.val-doc skip
                    "vl-tot-prev " tt-rel-total.vl-tot-prev skip
                    "vl-tot-real  " tt-rel-total.vl-tot-real skip.
            end.
            OUTPUT CLOSE.
        END.
        
END PROCEDURE.

PROCEDURE pi-grava-dados-desp-tit-ap:

    for first dvv_tit_ap_mi no-lock where
              dvv_tit_ap_mi.cod_estab      = dvv_movto_mi_det.cod_estabel
          and dvv_tit_ap_mi.serie          = dvv_movto_mi_det.serie
          and dvv_tit_ap_mi.nr-nota-fis    = dvv_movto_mi_det.nr-nota-fis
          and dvv_tit_ap_mi.cod_desp       = dvv_movto_mi_det.tipo_despesa
          /*and dvv_tit_ap_mi.tipo_container = dvv_movto_mi_det.it_codigo*/
          and dvv_tit_ap_mi.tp_doc_origem  = dvv_movto_mi_det.tp_doc_origem
          and dvv_tit_ap_mi.doc_origem     BEGINS dvv_movto_mi_det.doc_origem:

        find tit_ap no-lock where
             tit_ap.cod_estab       = dvv_tit_ap_mi.cod_estab
         and tit_ap.num_id_tit_ap   = dvv_tit_ap_mi.num_id_tit_ap no-error.

        IF AVAIL tit_ap THEN DO:

            FIND emitente
                WHERE emitente.cod-emitente = tit_ap.cdn_fornecedor NO-LOCK.

            /* Dados do t°tulo APB. */
            assign b-tt-rel-reg.cdn-fornec    = tit_ap.cdn_fornecedor
                   b-tt-rel-reg.nome-forn     = emitente.nome-abrev
                   b-tt-rel-reg.cod-esp       = tit_ap.cod_espec_docto
                   b-tt-rel-reg.cod-titulo    = tit_ap.cod_tit_ap
                   b-tt-rel-reg.parcela       = tit_ap.cod_parcela
                   b-tt-rel-reg.trans-ap      = "".

            FIND FIRST movto_tit_ap OF tit_ap NO-LOCK 
            WHERE movto_tit_ap.ind_trans_ap_abrev = "IMPL" NO-ERROR.
            IF AVAIL movto_tit_ap THEN
                ASSIGN b-tt-rel-reg.usuario-impl = movto_tit_ap.cod_usuario.
/*                 if tit_ap.ind_tip_espec_docto = "Imposto Taxado" then do:                     */
/*                                                                                               */
/*                         assign b-tt-rel-reg.vl-prev = b-tt-rel-reg.vl-prev * de-aliq-imposto  */
/*                                b-tt-rel-reg.vl-real = b-tt-rel-reg.vl-real * de-aliq-imposto. */
/*                                                                                               */
/*                 enD.                                                                          */

        END.
    END.

END PROCEDURE.


/**********************************************************************/
/* PROCEDURE Localiza o item do lancamento contabil do movto estoque  */
/**********************************************************************/
PROCEDURE pi_busca_lote_lancto_item:

    /********************** Parameter Definition Begin ************************/
    //DEF INPUT PARAM p-tipo AS CHAR FORMAT "x(5)" NO-UNDO.

    /********************** Variaveis Definition Begin ************************/
    DEF VAR v_ind_natur LIKE ITEM_lancto_ctbl.ind_natur_lancto_ctbl NO-UNDO.
    DEF VAR v_histor AS CHAR NO-UNDO.

    /********************** Parameter Definition Begin ************************/
    ASSIGN //v_dat_fim_ilc  = date(month(movto-estoq.dt-trans), 15, year(movto-estoq.dt-trans)) + 30
           //v_dat_fim_ilc  = date(month(v_dat_fim_ilc), 01, year(v_dat_fim_ilc)) - 1
           //c-plano_ccusto = ""        
           //c-cod_ccusto   = ""
           v_num_lote     = 0  
           v_num_lanc     = 0
           v_num_seq      = 0.

    /* Prpara Natureza Lancto Ctbl
    IF (v_val_movto > 0 AND p-tipo = "Movto")
    OR (v_val_movto < 0 AND p-tipo = "SDO")THEN
        ASSIGN v_ind_natur = "DB".
    ELSE
        ASSIGN v_ind_natur = "CR".

    /* Prepara Centro de Custo */
    IF AVAIL emscad.ccusto THEN
        ASSIGN c-plano_ccusto = emscad.ccusto.cod_plano_ccusto
               c-cod_ccusto   = emscad.ccusto.cod_ccusto.
    */

    /* INICIO: Obtem GRUPO DE ESTOQUE */
    FIND ITEM NO-LOCK WHERE
         ITEM.it-codigo = tt-movto-cep.it-codigo NO-ERROR.

    FIND grup-estoque NO-LOCK WHERE
         grup-estoque.ge-codigo = ITEM.ge-codigo NO-ERROR.
    /* FIM: Obtem GRUPO DE ESTOQUE */    

    /* PRIMEIRO procura informaªÑes na TEMP-TABLE (Melhoria de Performance) */
    FIND tt_ILC_movto_estoque WHERE 
         tt_ILC_movto_estoque.cta_ctbl  = item_lancto_ctbl.cod_cta_ctbl 
     AND tt_ILC_movto_estoque.ccusto    = item_lancto_ctbl.cod_ccusto //c-cod_ccusto
     AND tt_ILC_movto_estoque.cod_estab = tt-movto-cep.cod_estab
     AND tt_ILC_movto_estoque.cod_un    = item_lancto_ctbl.cod_un //v_cod_un
     AND tt_ILC_movto_estoque.ge_cod    = grup-estoque.ge-codigo
     AND tt_ILC_movto_estoque.dat_lanc  = tt-movto-cep.dat_lancto_ctbl NO-ERROR. /* v_dat_fim_ilc NO-ERROR. */

    IF AVAIL tt_ILC_movto_estoque THEN DO:
        ASSIGN v_num_lote = tt_ILC_movto_estoque.num_lote
               v_num_lanc = tt_ILC_movto_estoque.num_lanc    
               v_num_seq  = tt_ILC_movto_estoque.num_seq.

        RETURN.
    END.

    FIND FIRST estabelecimento WHERE
               estabelecimento.cod_estab = tt-movto-cep.cod_estab NO-LOCK NO-ERROR.
    /* SEGUNDO: Procura informaªÑes no ITEM_LANCTO_CTBL */
    IF grup-estoque.ge-codigo < 10 THEN
        ASSIGN v_histor = "Grupo:  " + STRING(grup-estoque.ge-codigo).
    ELSE 
        ASSIGN v_histor = "Grupo: " + STRING(grup-estoque.ge-codigo).


    ilc_blk:
    FOR EACH bitem_lancto_ctbl NO-LOCK USE-INDEX tmlnctcb_movto_ctbl WHERE
             bitem_lancto_ctbl.cod_empresa           = estabelecimento.cod_empresa    
         AND bitem_lancto_ctbl.cod_plano_cta_ctbl    = item_lancto_ctbl.cod_plano_cta_ctbl
         AND bitem_lancto_ctbl.cod_cta_ctbl          = item_lancto_ctbl.cod_cta_ctbl      
         /*AND item_lancto_ctbl.cod_plano_ccusto      = c-plano_ccusto
         AND item_lancto_ctbl.cod_ccusto            = c-cod_ccusto*/
         AND bitem_lancto_ctbl.cod_estab             = tt-movto-cep.cod_estab
         AND bitem_lancto_ctbl.cod_unid_negoc        = item_lancto_ctbl.cod_un 
         AND bitem_lancto_ctbl.dat_lancto_ctbl       = tt-movto-cep.dat_lancto_ctbl
         AND bITEM_lancto_ctbl.num_lote_ctbl         = ITEM_lancto_ctbl.num_lote_ctbl      
         AND bITEM_lancto_ctbl.num_lancto_ctbl       = ITEM_lancto_ctbl.num_lancto_ctbl    
         AND bITEM_lancto_ctbl.num_seq_lancto_ctbl   = ITEM_lancto_ctbl.num_seq_lancto_ctbl:
           /* v_dat_fim_ilc: */ 

        IF INDEX(bITEM_lancto_ctbl.des_histor_lancto_ctbl, v_histor) > 0 THEN DO:

            FIND lote_ctbl OF ITEM_lancto_ctbl NO-ERROR.
    
    
    
            IF  AVAIL lote_ctbl
            AND INDEX(lote_ctbl.des_lote_ctbl, tt-movto-cep.refer-contab) <> 0 THEN DO:
    
                ASSIGN v_num_lote = item_lancto_ctbl.num_lote_ctbl
                       v_num_lanc = item_lancto_ctbl.num_lancto_ctbl
                       v_num_seq  = item_lancto_ctbl.num_seq_lancto_ctbl.

                CREATE tt_ILC_movto_estoque.
                ASSIGN tt_ILC_movto_estoque.cta_ctbl  = bitem_lancto_ctbl.cod_cta_ctbl     
                       tt_ILC_movto_estoque.ccusto    = bitem_lancto_ctbl.cod_ccusto              
                       tt_ILC_movto_estoque.cod_estab = bitem_lancto_ctbl.cod_estab      
                       tt_ILC_movto_estoque.cod_un    = bitem_lancto_ctbl.cod_unid_negoc 
                       tt_ILC_movto_estoque.ge_cod    = grup-estoque.ge-codigo    
                       tt_ILC_movto_estoque.dat_lanc  = tt-movto-cep.dat_lancto_ctbl  /* v_dat_fim_ilc */
                       tt_ILC_movto_estoque.num_lote  = bitem_lancto_ctbl.num_lote_ctbl      
                       tt_ILC_movto_estoque.num_lanc  = bitem_lancto_ctbl.num_lancto_ctbl    
                       tt_ILC_movto_estoque.num_seq   = bitem_lancto_ctbl.num_seq_lancto_ctbl.

                LEAVE ilc_blk.

            END.
        END.
    END.
END PROCEDURE.
