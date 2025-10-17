// Vers‰es
// -----------
// 2.00.00.002 - Eduardo Barth (Newtech) - 26/04/2023
//                  - inclus∆o do parÉmetro "Fechamento prÇvio", executado em RPW diariamente ao longo do màs, cujos registros de provis∆o n∆o s∆o consult†veis no DVV5300-Monitor, saem apenas no relat¢rio DVV0199.
//                  - Fechamento prÇvio considera sempre desde o dia 1o do màs corrente.
//                  - limpeza de c¢digo comentado.
//                  - melhorias de performance.
//                  - usar ut-acomp somente quando for online, n∆o no RPW.
//

{include/i-prgvrs.i DVV5100 2.00.00.002}
{esp/Moedas.i} //Include com o valor do c¢digo das moedas



DEF TEMP-TABLE tt_erro_import NO-UNDO
    FIELD cod_empresa         LIKE estabelecimento.cod_empresa
    FIELD cd-erro             AS INT  FORMAT ">>>>,>>9"
    FIELD mensagem            AS CHAR FORMAT "x(150)"
    FIELD ajuda               AS CHAR FORMAT "x(150)".

DEF TEMP-TABLE tt_movto_import NO-UNDO
    FIELD cod_empresa      LIKE lote_ctbl.cod_empresa 
    FIELD cod_estab        LIKE item_lancto_ctbl.cod_estab
    FIELD dat_lancto_ctbl  LIKE lote_ctbl.dat_lote_ctbl
    FIELD dat_convert_val  LIKE lote_ctbl.dat_lote_ctbl
    FIELD des_lote_ctbl    LIKE lote_ctbl.des_lote_ctbl   
    FIELD ind_natur        LIKE item_lancto_ctbl.ind_natur_lancto_ctbl
    FIELD cod_un           LIKE item_lancto_ctbl.cod_unid_negoc
    FIELD cod_plano_cta    LIKE item_lancto_ctbl.cod_plano_cta_ctbl
    FIELD cod_cta_db       LIKE item_lancto_ctbl.cod_cta_ctbl      
    FIELD cod_plano_ccu    LIKE item_lancto_ctbl.cod_plano_ccusto 
    FIELD cod_ccu          LIKE item_lancto_ctbl.cod_ccusto       
    FIELD cod_cta_cr       LIKE item_lancto_ctbl.cod_cta_ctbl      
    FIELD cod_ie           LIKE item_lancto_ctbl.cod_indic_econ
    FIELD val_lancto       LIKE item_lancto_ctbl.val_lancto_ctbl
    FIELD des_hist         LIKE item_lancto_ctbl.des_histor_lancto_ctbl 
    FIELD log_estorno      AS   LOG 
    FIELD num_lote_ger     LIKE lancto_ctbl.num_lote_ctbl.


DEF TEMP-TABLE tt-despesa NO-UNDO
    FIELD cod_estabel       AS CHAR FORMAT "x(05)"
    FIELD serie             AS CHAR 
    FIELD nr-nota-fis       AS CHAR FORMAT "x(16)"
    FIELD cliente           AS CHAR
    FIELD cli-nom-abrev     AS CHAR FORMAT "x(16)"
    FIELD fat-total         AS DEC
    FIELD cod-despesa       AS INT  
    FIELD despesa           AS CHAR
    FIELD vl-desp-orcado    AS DEC
    FIELD vl-desp-previsto  AS DEC
    FIELD vl-desp-prev-liq  AS DEC
    FIELD vl-desp-real      AS DEC
    FIELD vl-desp-real-liq  AS DEC
    FIELD vl-provisao       AS DEC
    FIELD vl-frete-comp     AS DEC
    FIELD fornecedor        LIKE emitente.cod-emitente
    FIELD for-nom-abrev     AS CHAR FORMAT "x(16)"
    FIELD dt-prev           AS DATE
    FIELD dt-real           AS DATE
    FIELD dt-emissao        AS DATE
    FIELD cidade-cif        AS CHAR FORMAT "x(25)"
    FIELD peso-liq          LIKE nota-fiscal.peso-bru-tot
    FIELD peso-brut         LIKE nota-fiscal.peso-liq-tot
    FIELD it-codigo         LIKE it-nota-fisc.it-codigo
    FIELD familia           LIKE fam-comerc.descricao
    FIELD embal             LIKE item-embal.sigla-emb
    FIELD frete-nota        LIKE nota-fiscal.vl-frete
    FIELD cod-unid-negoc    LIKE it-nota-fisc.cod-unid-negoc
    INDEX despesa IS PRIMARY 
          cod_estabel 
          serie
          nr-nota-fis
          despesa.

DEF TEMP-TABLE tt-item NO-UNDO
    FIELD it-codigo LIKE it-nota-fisc.it-codigo
    FIELD cod-unid  LIKE it-nota-fisc.cod-unid-negoc
    FIELD peso_liq  LIKE it-nota-fisc.peso-liq-fat
    FIELD peso_brut LIKE it-nota-fisc.peso-bruto
    FIELD familia   LIKE fam-comerc.descricao
    FIELD embal     LIKE item-embal.sigla-emb.

def temp-table tt-desp NO-UNDO like dvv_tipo_despesa.


define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    FIELD dt-movto-ini     AS DATE
    FIELD dt-movto-fim     AS DATE
    FIELD cod-estabel      AS CHAR
    FIELD lg-dex           AS LOGICAL
    FIELD lg-dvv-me        AS LOGICAL
    FIELD lg-dvv-mi        AS LOGICAL
	FIELD lg-dvv-rem       AS LOGICAL
    FIELD tipo-exec        AS INTEGER.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEF VAR de-peso-liq       LIKE it-nota-fisc.peso-liq-fat NO-UNDO.
DEF VAR de-peso-bru       LIKE it-nota-fisc.peso-bruto   NO-UNDO.
/***************************** Temp Table Definiton Begin ********************/
/* Include pre-processadores */
{cdp/cdcfgmat.i}

/* Vari†veis de Sistema */
{utp/ut-glob.i}

DEF STREAM a.
                                            

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

{include/boerrtab.i}

DEF TEMP-TABLE tt-bo-erro-aux LIKE tt-bo-erro.

def temp-table tt-raw-digita
   field raw-digita      as raw.

DEF TEMP-TABLE tt-processo-exp
    FIELD nr-proc-exp LIKE processo-exp.nr-proc-exp
    FIELD cod-estabel LIKE processo-exp.cod-estabel
    index id is primary
        nr-proc-exp
        cod-estabel.

DEF TEMP-TABLE tt-processo-exp-consig
    FIELD nr-proc-exp LIKE processo-exp.nr-proc-exp
    FIELD cod-estabel LIKE processo-exp.cod-estabel
    index id is primary
        nr-proc-exp
        cod-estabel.

DEF TEMP-TABLE tt-dvv_log_prov_desp NO-UNDO LIKE dvv_log_prov_desp
    FIELD origem          AS CHAR
    FIELD cod_cta_ctbl_cr LIKE dvv_log_provisao.cod_cta_ctbl_cr
    FIELD cod_cta_ctbl_db LIKE dvv_log_provisao.cod_cta_ctbl_db
    FIELD cod_unid_negoc  LIKE dvv_log_provisao.cod_un
    index id is primary
       origem
       cod_cta_ctbl_cr
       cod_cta_ctbl_db
       cod_unid_negoc
    index desp_unique
        cod_estabel
        nr_proc_exp
        cod_despesa
        num_id_provisao.

DEF TEMP-TABLE tt-erro NO-UNDO
    FIELD cod-erro AS INT
    FIELD mensagem AS CHAR.

DEF BUFFER bfdvv_log_provisao FOR dvv_log_provisao.
DEF BUFFER bdvv_log_provisao  FOR dvv_log_provisao.
DEF BUFFER b1dvv_log_provisao FOR dvv_log_provisao.
DEF BUFFER bf-dvv_movto_mi FOR dvv_movto_mi.
DEF BUFFER b-dex_movto FOR dex_movto.
DEF BUFFER b-dvv-movto FOR dvv_movto.
DEF BUFFER bbbemitente FOR emitente.

def new global shared var v_cod_usuar_corren as char no-undo.
def new global shared var i-num-ped-exec-rpw  as int no-undo.

DEF VAR l-imprime-header  AS LOGICAL NO-UNDO.
DEF VAR i-seq-linha AS INTEGER FORMAT "999999" NO-UNDO.
DEF VAR i-qtd-reg   AS INTEGER FORMAT "999999" NO-UNDO.
DEF VAR c-des-diretorio AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEF VAR c-linha AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEF VAR c-nome-arq AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEF VAR i_num_id_provisao AS INT NO-UNDO.
DEF VAR c-arq-lista-desp AS CHAR NO-UNDO.
DEF VAR de_dvv_movto_mi_vl_orc_r$ AS DEC NO-UNDO.
DEF VAR de-valor-provisao AS DECIMAL NO-UNDO.
DEF VAR c-trading         AS CHAR NO-UNDO.
DEF VAR c-origem          AS CHAR NO-UNDO.
DEF VAR c-estab-destino   AS CHAR NO-UNDO.

DEF VAR v_tot_peso_liq  AS DEC NO-UNDO.
DEF VAR v_tot_peso_brut AS DEC NO-UNDO.
DEF VAR v_tot_vl_orc    AS DEC NO-UNDO.
DEF VAR v_tot_vl_prev   AS DEC NO-UNDO.
DEF VAR v_tot_vl_real   AS DEC NO-UNDO.
DEF VAR i-pto-contr     AS INT NO-UNDO.

DEFINE VARIABLE h-acomp AS HANDLE       NO-UNDO.

DEF VAR c-tp-requis  AS CHAR NO-UNDO.
DEF VAR c_dados      AS CHAR NO-UNDO.
DEF VAR c-unid-negoc AS CHAR NO-UNDO.

DEF VAR l-duplicatas AS LOGICAL NO-UNDO.
DEF BUFFER b-nota-fiscal FOR nota-fiscal.
DEF VAR i    AS INT  NO-UNDO.
DEF VAR c-lista AS CHAR NO-UNDO.

DEFINE VARIABLE dtEstornoPreviaIni AS DATE        NO-UNDO.
DEFINE VARIABLE dtEstornoPreviaFim AS DATE        NO-UNDO.

DEFINE INPUT PARAMETER raw-param AS raw NO-UNDO.
DEFINE INPUT PARAMETER table FOR tt-raw-digita.

create tt-param.
Raw-Transfer raw-param To tt-param.  

for each tt-raw-digita NO-LOCK:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

FIND FIRST tt-param NO-LOCK NO-ERROR.

IF i-num-ped-exec-rpw = 0 then
    run utp/ut-acomp.p persistent set h-acomp.
ELSE IF  tt-param.tipo-exec = 2 THEN DO:  // gera prÇvia das provis‰es em RPW.
    find ped_exec no-lock
        where ped_exec.num_ped_exec = i-num-ped-exec-rpw no-error.
    IF  NOT AVAIL ped_exec THEN
        RETURN "ERRO: pedido exec " + STRING(i-num-ped-exec-rpw) + " n∆o encontrado (dvv5100rp.p).".

    ASSIGN tt-param.dt-movto-ini = tt-param.dt-movto-ini + (TODAY - ped_exec.dat_criac_ped_exec)
           tt-param.dt-movto-fim = tt-param.dt-movto-fim + (TODAY - ped_exec.dat_criac_ped_exec).
    // Executa sempre o periodo selecionado em tela, atualizado para o futuro, no caso de pedidos peri¢dicos agendados para execuá∆o futura em RPW.
    // O cliente executa sempre os ultimos 6 ou 12 meses.

END.

{include/i-rpvar.i}
/*include com a definicao da frame de cabecalho e rodape*/
{include/i-rpout.i}

{include/i-rpcab.i}

assign c-titulo-relat = "Provis∆o DVV"
       c-programa     = 'DVV5100'.

/* vizualiza?ío dos frames padrÑes do relatΩrio */
VIEW FRAME f-cabec. 
VIEW FRAME f-rodape.
DEF STREAM s_ret.
DEF STREAM s_conta.

ASSIGN l-imprime-header = YES
       i-seq-linha      = 1
       i-qtd-reg        = 0.

IF i-num-ped-exec-rpw = 0 then
    run pi-inicializar in h-acomp (input "Provis∆o DVV").

IF tt-param.lg-dvv-me OR tt-param.lg-dex OR tt-param.lg-dvv-rem THEN DO:
empty temp-table tt-processo-exp-consig.
    
    DO  i = 1 TO NUM-ENTRIES(tt-param.cod-estabel,",") :
        FOR EACH estabelec
            WHERE estabelec.cod-estabel = ENTRY(i,tt-param.cod-estabel,",") NO-LOCK:

            IF NOT tt-param.lg-dvv-me AND tt-param.lg-dex AND estabelec.cod-estabel = "001" THEN NEXT.

            IF i-num-ped-exec-rpw = 0 then
                run pi-acompanhar in h-acomp ("Coletando informaá‰es estab " + estabelec.cod-estabel ).

            FOR EACH es_nota_fiscal WHERE
                     es_Nota_fiscal.cod_estabel = estabelec.cod-estabel AND                
                     es_nota_fiscal.DT_RECEITA >= tt-param.dt-movto-ini AND
                     es_nota_fiscal.DT_RECEITA <= tt-param.dt-movto-fim NO-LOCK,
                FIRST nota-fiscal WHERE
                      nota-fiscal.cod-estabel = es_nota_fiscal.cod_estabel AND
                      nota-fiscal.serie       = es_nota_fiscal.serie       AND
                      nota-fiscal.nr-nota-fis = es_Nota_fiscal.nr_nota_fis AND
                      nota-fiscal.nr-proc-exp <> "" NO-LOCK,
                FIRST processo-exp WHERE
                     processo-exp.cod-estabel = es_nota_fiscal.cod_estabel AND
                     processo-exp.nr-proc-exp = nota-fiscal.nr-proc-exp NO-LOCK:
                
        
                //para desconsiderar os processos com origem compras trading
                IF SUBSTR(processo-exp.nr-proc-exp,LENGTH(processo-exp.nr-proc-exp) - 1,1) = 'T' THEN
                   if  can-find(FIRST es_ped_compra_trading NO-LOCK WHERE es_ped_compra_trading.num_pedido_brasil = INT(ENTRY(1,processo-exp.nr-proc-exp,'T')))
                   or  can-find(FIRST es_ped_compra_trading NO-LOCK WHERE es_ped_compra_trading.num_pedido_trading = INT(ENTRY(1,processo-exp.nr-proc-exp,'T'))) then
                   NEXT.

                FIND FIRST tt-processo-exp WHERE
                     tt-processo-exp.cod-estabel = processo-exp.cod-estabel AND
                     tt-processo-exp.nr-proc-exp = processo-exp.nr-proc-exp NO-LOCK NO-ERROR.
        
                IF NOT AVAIL tt-processo-exp THEN DO:
        
                    CREATE tt-processo-exp.
                    ASSIGN tt-processo-exp.cod-estabel = processo-exp.cod-estabel
                           tt-processo-exp.nr-proc-exp = processo-exp.nr-proc-exp.
                END.
                
                FIND es_natur_oper WHERE
                     es_natur_oper.nat_operacao = nota-fiscal.nat-operacao NO-LOCK NO-ERROR.
                IF AVAIL es_natur_oper AND es_natur_oper.tip_oper_exp = 5 THEN DO: /*REMESSA*/
                    FIND FIRST tt-processo-exp-consig WHERE
                         tt-processo-exp-consig.cod-estabel = processo-exp.cod-estabel AND
                         tt-processo-exp-consig.nr-proc-exp = processo-exp.nr-proc-exp NO-LOCK NO-ERROR.
            
                    IF NOT AVAIL tt-processo-exp-consig THEN DO:
            
                        CREATE tt-processo-exp-consig.
                        ASSIGN tt-processo-exp-consig.cod-estabel = processo-exp.cod-estabel
                               tt-processo-exp-consig.nr-proc-exp = processo-exp.nr-proc-exp.
                    END.
                end.
            END.
        END.
    END.
END.


EMPTY TEMP-TABLE tt-dvv_log_prov_desp.
EMPTY TEMP-TABLE tt-erro.

FOR EACH dvv_param NO-LOCK:
    IF  dvv_param.cod_cta_provis_dex = "" THEN DO:
        CREATE tt-erro.
        ASSIGN tt-erro.cod-erro = 17006
               tt-erro.mensagem = "Conta provis∆o DEX n∆o informado no estabel " + dvv_param.cod_estabel .
    END.
END.

IF  tt-param.tipo-exec = 2 THEN do:

    ASSIGN c-lista = "".
    IF  tt-param.lg-dex    THEN ASSIGN c-lista = c-lista + "PDEX" + ",EPDEX".
    IF  tt-param.lg-dvv-me THEN ASSIGN c-lista = c-lista + ",PDVV ME" + ",EPDVV ME".
    IF  tt-param.lg-dvv-mi THEN ASSIGN c-lista = c-lista + ",PDVV MI" + ",EPDVV MI".
	IF  tt-param.lg-dvv-rem THEN ASSIGN c-lista = c-lista + ",PDVV REM" + ",EPDVV REM".
    
    // A execuá∆o di†ria do Fechamento PrÇvio deve renovar os registros, eliminando os antigos e criando novos.
    FOR each dvv_log_provisao exclusive-lock
        where CAN-DO(c-lista, dvv_log_provisao.origem_movto) // registro de fechamento prÇvio
        AND   CAN-DO(tt-param.cod-estabel, dvv_log_provisao.cod_estabel):   // estab. est† na lista

        FOR each dvv_log_prov_desp exclusive-lock
            where dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao:
            
            DELETE dvv_log_prov_desp.
        END.

        DELETE dvv_log_provisao.
    END.
END.


IF NOT CAN-FIND (FIRST tt-erro) THEN DO:
    IF tt-param.lg-dex THEN DO:

        IF i-num-ped-exec-rpw = 0 then
            run pi-acompanhar in h-acomp ("DEX... cancelando provis‰es pendentes").
        
        IF  tt-param.tipo-exec = 1 THEN DO:  // Na execuá∆o para fechamento prÇvio (tipo 2) n∆o deve mexer nas demais provis‰es.
            FOR EACH bdvv_log_provisao WHERE
                     bdvv_log_provisao.dt_provisao     <= TODAY  AND
                     bdvv_log_provisao.origem           = "DEX"  AND
                     bdvv_log_provisao.situacao        = 1       AND
                     CAN-DO(tt-param.cod-estabel, bdvv_log_provisao.cod_estabel)   // estab. est† na lista de seleá∆o
                     EXCLUSIVE-LOCK:
    
                /*IF AVAIL bdvv_log_provisao THEN DO:*/
                 IF bdvv_log_provisao.situacao = 1 THEN
                     ASSIGN bdvv_log_provisao.situacao = 3.
    
                 RUN pi-gera-estorno.
            END.
        END.
        ELSE DO:
            IF MONTH(tt-param.dt-movto-fim) = 01 THEN   
                ASSIGN dtEstornoPreviaIni = DATE(12,01,YEAR(tt-param.dt-movto-fim) - 1)
                       dtEstornoPreviaFim = DATE(12,31,YEAR(tt-param.dt-movto-fim) - 1).
            ELSE IF MONTH(tt-param.dt-movto-fim) = 12 THEN   
                ASSIGN dtEstornoPreviaIni = DATE(11,01,YEAR(tt-param.dt-movto-fim))
                       dtEstornoPreviaFim = DATE(11,30,YEAR(tt-param.dt-movto-fim)).                       
            ELSE
                ASSIGN dtEstornoPreviaIni = DATE(MONTH(tt-param.dt-movto-fim) - 1,01,YEAR(tt-param.dt-movto-fim))
                       dtEstornoPreviaFim = DATE(MONTH(tt-param.dt-movto-fim),1,YEAR(tt-param.dt-movto-fim)) - 1.
                       
            FOR EACH bdvv_log_provisao WHERE
                     bdvv_log_provisao.dt_lancto_provis     >= dtEstornoPreviaIni  AND
                     bdvv_log_provisao.dt_lancto_provis     <= dtEstornoPreviaFim  AND
                     bdvv_log_provisao.origem           = "DEX"  AND
                     bdvv_log_provisao.situacao         = 2       AND
                     CAN-DO(tt-param.cod-estabel, bdvv_log_provisao.cod_estabel)   // estab. est† na lista de seleá∆o
                     EXCLUSIVE-LOCK:
    
                RUN pi-gera-estorno-previa.
            END.         
        END.
        
        FIND FIRST dvv_param WHERE
                   dvv_param.cod_estabel = "002" NO-LOCK NO-ERROR.

        //PARCIAL "P"
        
        FOR EACH tt-processo-exp WHERE
                 tt-processo-exp.cod-estabel = "002":
        
            BLOCK_dexMovto:
            for each  dex_movto no-lock
                  where dex_movto.cod_estabel = tt-processo-exp.cod-estabel
                  AND   dex_movto.nr_pedido   = tt-processo-exp.nr-proc-exp
                  and   substring(dex_movto.char_2,1,1) = "1"    /*gera provis∆o somente das despesas provisionads no programa dex0050*/
                  and   dex_movto.valor <> ?
                  AND   dex_movto.valor <> 0,
                FIRST dex_tp_despesa no-lock
                  WHERE dex_tp_despesa.codigo = dex_movto.tp_despesa
                  and   dex_tp_despesa.LOG_PROVIS_CONTABILIDADE = yes:

                FOR EACH estabelec no-lock
                    where estabelec.cod-estabel <> "999",
                    each b-dex_movto
                      WHERE b-dex_movto.cod_estabel = estabelec.cod-estabel
                        AND b-dex_movto.nr_processo = tt-processo-exp.nr-proc-exp
                        AND b-dex_movto.tp_despesa = dex_movto.tp_despesa NO-LOCK:
                    IF (b-dex_movto.data_atualiz <> ? AND b-dex_movto.data_atualiz <= tt-param.dt-movto-fim) AND
                       (decimal(substring(b-dex_movto.char_1,11,10)) <> 0 AND decimal(substring(b-dex_movto.char_1,11,10)) <> ?) THEN
                        NEXT BLOCK_dexMovto.
                END.

                IF dex_tp_despesa.cod_estabel_prov = "" OR dex_tp_despesa.cod_estabel_prov = ? THEN DO:
                    CREATE tt-erro.
                    ASSIGN tt-erro.cod-erro = 17006
                           tt-erro.mensagem = "Estabel Provis∆o n∆o informado para a despesa " + string(dex_movto.tp_despesa)  .
                    NEXT BLOCK_dexMovto.
                END.

                FIND FIRST es_grp_dex_desp WHERE
                           es_grp_dex_desp.cod_desp = dex_movto.tp_despesa NO-LOCK NO-ERROR.

                IF NOT AVAIL es_grp_dex_desp THEN DO:
                    CREATE tt-erro.
                    ASSIGN tt-erro.cod-erro = 17006
                           tt-erro.mensagem = "Despesa " + string(dex_movto.tp_despesa) + "n∆o cadastrada em nenhum grupo de despesa (dex0011) " .
                    NEXT BLOCK_dexMovto.
                END.

                FIND es_grp_dex WHERE
                     es_grp_dex.cod_grupo = es_grp_dex_desp.cod_grupo NO-LOCK NO-ERROR.
                IF NOT AVAIL es_grp_dex THEN DO:
                    CREATE tt-erro.
                    ASSIGN tt-erro.cod-erro = 17006
                           tt-erro.mensagem = "Grupo de despesa n∆o cadastrada - (dex0011) " .
                    NEXT BLOCK_dexMovto.
                END.
    
                IF es_grp_dex.cod_cta_ctbl = "" OR es_grp_dex.cod_cta_ctbl = ? THEN DO:
                    CREATE tt-erro.
                    ASSIGN tt-erro.cod-erro = 17006
                           tt-erro.mensagem = "Nao informado a conta contabil para o grupo de despesa " + string(es_grp_dex_desp.cod_grupo) + " - (dex0011) " .
                    NEXT BLOCK_dexMovto.
                END.

                /* Segregaá∆o EUA */
                /* fazer a busca do pais pelo pais do fornecedor da despesa */
                ASSIGN c-unid-negoc = "000".

                FOR FIRST proc-ped-venda WHERE
                          proc-ped-venda.cod-estabel = dex_movto.cod_estabel  AND
                          proc-ped-venda.nr-proc-exp = dex_movto.nr_processo NO-LOCK:
                    FOR FIRST proc-ped-ent WHERE
                              proc-ped-ent.cod-estabel = proc-ped-venda.cod-estabel AND
                              proc-ped-ent.nr-proc-exp = proc-ped-venda.nr-proc-exp NO-LOCK:

                        FOR FIRST ped-item OF proc-ped-ent NO-LOCK:
                            ASSIGN c-unid-negoc = ped-item.cod-unid-negoc.
                        END.
                    END.
                END.
                IF NOT AVAIL proc-ped-venda THEN DO:
                    FOR FIRST proc-ped-venda WHERE
                              proc-ped-venda.cod-estabel <> "999" AND
                              proc-ped-venda.nr-proc-exp = dex_movto.nr_processo NO-LOCK:
                        FOR FIRST proc-ped-ent WHERE
                                  proc-ped-ent.cod-estabel = proc-ped-venda.cod-estabel AND
                                  proc-ped-ent.nr-proc-exp = proc-ped-venda.nr-proc-exp NO-LOCK:

                            FOR FIRST ped-item OF proc-ped-ent NO-LOCK:
                                ASSIGN c-unid-negoc = ped-item.cod-unid-negoc.
                            END.
                        END.
                    END.
                    IF NOT AVAIL proc-ped-venda THEN DO: 
                        FOR FIRST processo-exp WHERE
                                  processo-exp.cod-estabel = tt-processo-exp.cod-estabel AND
                                  processo-exp.nr-proc-exp = tt-processo-exp.nr-proc-exp NO-LOCK:
                            FOR FIRST bbbemitente WHERE
                                      bbbemitente.cod-emitente = processo-exp.cod-emitente NO-LOCK:
                                FOR FIRST ped-venda WHERE
                                          ped-venda.nome-abrev = bbbemitente.nome-abrev AND
                                          ped-venda.nr-pedcli  = dex_movto.nr_processo NO-LOCK:
                                    FOR FIRST ped-item OF ped-venda NO-LOCK:
                                       ASSIGN c-unid-negoc = ped-item.cod-unid-negoc.
                                    END.
                                END.
                            END.

                        END.
                    END.
                END.

                FIND FIRST processo-exp NO-LOCK 
                    WHERE processo-exp.cod-estabel = tt-processo-exp.cod-estabel 
                      AND processo-exp.nr-proc-exp = tt-processo-exp.nr-proc-exp NO-ERROR.
                IF AVAIL processo-exp THEN DO:

                    FIND FIRST emitente NO-LOCK
                        WHERE emitente.cod-emitente = processo-exp.cod-emitente NO-ERROR.
                    IF AVAIL emitente THEN DO:
    
                        FIND FIRST mgcad.pais NO-LOCK
                            WHERE pais.nome-pais = emitente.pais NO-ERROR.
                        IF AVAIL pais and pais.nome-pais = "EUA" THEN DO:
                
                            FIND FIRST es-pais-unid-negoc NO-LOCK
                                WHERE es-pais-unid-negoc.cod-pais = pais.cod-pais NO-ERROR.
                            IF AVAIL es-pais-unid-negoc THEN DO:
                                ASSIGN c-unid-negoc = es-pais-unid-negoc.cod-unid-negoc.
                            END.
                        END.
                    END.

                    RUN pi-cria-dvv_log_provisao (INPUT dex_tp_despesa.cod_estabel_prov,
                                                  INPUT tt-processo-exp.nr-proc-exp,
                                                  INPUT TODAY,
                                                  INPUT "DEX",
                                                  INPUT es_grp_dex.cod_cta_ctbl,      /*DEBITO*/
                                                  INPUT dvv_param.cod_cta_provis_dex, /*CREDITO*/
                                                  INPUT dex_movto.tp_despesa,
                                                  INPUT dex_movto.valor,
                                                  INPUT "",     /*centro de custo*/
                                                  INPUT c-unid-negoc). /*unidade negocio*/
                END.
            
            END.
        END.
        
        
        //venda consignado
        run pi-gera-dex-consig.
        
    END.
	
	/* =========================
	   BLOCO DE CONTROLE DVV REM
	   - Cancelamento/Estorno
	   ========================= */
	IF tt-param.lg-dvv-rem THEN DO:

		/* Cancelamento de pendentes (execuÁ„o NORMAL) */
		IF tt-param.tipo-exec = 1 THEN DO:
			IF i-num-ped-exec-rpw = 0 then
				run pi-acompanhar in h-acomp ("DVV REM... cancelando provisıes pendentes").

			FOR EACH bdvv_log_provisao WHERE
					 bdvv_log_provisao.dt_provisao <= TODAY
				 AND bdvv_log_provisao.origem      = "DVV REM"
				 AND bdvv_log_provisao.situacao    = 1
				 AND CAN-DO(tt-param.cod-estabel, bdvv_log_provisao.cod_estabel)
				 EXCLUSIVE-LOCK:

				IF bdvv_log_provisao.situacao = 1 THEN
					ASSIGN bdvv_log_provisao.situacao = 3.

				RUN pi-gera-estorno.
			END.
		END.
		ELSE DO:
			/* Estorno de PR…VIA (mesma janela que vocÍ usa no ME) */
			IF MONTH(tt-param.dt-movto-fim) = 01 THEN   
				ASSIGN dtEstornoPreviaIni = DATE(12,01,YEAR(tt-param.dt-movto-fim) - 1)
					   dtEstornoPreviaFim = DATE(12,31,YEAR(tt-param.dt-movto-fim) - 1).
			ELSE IF MONTH(tt-param.dt-movto-fim) = 12 THEN   
				ASSIGN dtEstornoPreviaIni = DATE(11,01,YEAR(tt-param.dt-movto-fim))
					   dtEstornoPreviaFim = DATE(11,30,YEAR(tt-param.dt-movto-fim)).
			ELSE
				ASSIGN dtEstornoPreviaIni = DATE(MONTH(tt-param.dt-movto-fim) - 1,01,YEAR(tt-param.dt-movto-fim))
					   dtEstornoPreviaFim = DATE(MONTH(tt-param.dt-movto-fim),1,YEAR(tt-param.dt-movto-fim)) - 1.

			FOR EACH bdvv_log_provisao WHERE
					 bdvv_log_provisao.dt_lancto_provis >= dtEstornoPreviaIni
				 AND bdvv_log_provisao.dt_lancto_provis <= dtEstornoPreviaFim
				 AND bdvv_log_provisao.origem           = "DVV REM"
				 AND bdvv_log_provisao.situacao         = 2
				 AND CAN-DO(tt-param.cod-estabel, bdvv_log_provisao.cod_estabel)
				 EXCLUSIVE-LOCK:

				RUN pi-gera-estorno-previa.
			END.
		END.

	END.
    

    IF tt-param.lg-dvv-me THEN DO:

        IF  tt-param.tipo-exec = 1 THEN DO:  // Na execuá∆o para fechamento prÇvio n∆o deve mexer nas demais provis‰es.
            
            IF i-num-ped-exec-rpw = 0 then
                run pi-acompanhar in h-acomp ("ME... cancelando provis‰es pendentes").

            /*cancela provisoes pendente*/
            FOR EACH bdvv_log_provisao WHERE
                     bdvv_log_provisao.dt_provisao     <= TODAY   AND
                     bdvv_log_provisao.origem           = "DVV ME"  AND
                     bdvv_log_provisao.situacao        = 1          AND
                     CAN-DO(tt-param.cod-estabel, bdvv_log_provisao.cod_estabel)   // estab. est† na lista de seleá∆o
                     EXCLUSIVE-LOCK:
    
                /*IF AVAIL bdvv_log_provisao THEN DO:*/
                 IF bdvv_log_provisao.situacao = 1 THEN
                     ASSIGN bdvv_log_provisao.situacao = 3.
    
                 RUN pi-gera-estorno.
            END.
        END.
        ELSE DO:
            IF MONTH(tt-param.dt-movto-fim) = 01 THEN   
                ASSIGN dtEstornoPreviaIni = DATE(12,01,YEAR(tt-param.dt-movto-fim) - 1)
                       dtEstornoPreviaFim = DATE(12,31,YEAR(tt-param.dt-movto-fim) - 1).
            ELSE IF MONTH(tt-param.dt-movto-fim) = 12 THEN   
                ASSIGN dtEstornoPreviaIni = DATE(11,01,YEAR(tt-param.dt-movto-fim))
                       dtEstornoPreviaFim = DATE(11,30,YEAR(tt-param.dt-movto-fim)).                       
            ELSE
                ASSIGN dtEstornoPreviaIni = DATE(MONTH(tt-param.dt-movto-fim) - 1,01,YEAR(tt-param.dt-movto-fim))
                       dtEstornoPreviaFim = DATE(MONTH(tt-param.dt-movto-fim),1,YEAR(tt-param.dt-movto-fim)) - 1.
                                
            FOR EACH bdvv_log_provisao WHERE
                     bdvv_log_provisao.dt_lancto_provis     >= dtEstornoPreviaIni  AND
                     bdvv_log_provisao.dt_lancto_provis     <= dtEstornoPreviaFim  AND
                     bdvv_log_provisao.origem           = "DVV ME"  AND
                     bdvv_log_provisao.situacao        = 2       AND
                     CAN-DO(tt-param.cod-estabel, bdvv_log_provisao.cod_estabel)   // estab. est† na lista de seleá∆o
                     EXCLUSIVE-LOCK:
    
                RUN pi-gera-estorno-previa.
            END.
        
        END.

        FOR EACH tt-processo-exp :
                                                                            
            FIND FIRST dvv_param WHERE
                       dvv_param.cod_estabel = tt-processo-exp.cod-estabel NO-LOCK NO-ERROR.

            BLOCK_DvvMovtoME:
            FOR EACH dvv_movto NO-LOCK
                  WHERE dvv_movto.cod_estabel  = tt-processo-exp.cod-estabel
                  AND   dvv_movto.num_processo = tt-processo-exp.nr-proc-exp 
                  AND   SUBSTRING(dvv_movto.CHAR_2,1,1) = "1"  /*0 - N∆o provisiona, 1 - Provisiona*/
                  AND   DVV_MOVTO.VALOR_PREV_R$ <> 0
                  AND   DVV_MOVTO.VALOR_PREV_R$ <> ?,
                FIRST dvv_tipo_despesa NO-LOCK
                  WHERE dvv_tipo_despesa.codigo       = dvv_movto.tipo_despesa 
                  AND   dvv_tipo_despesa.tipo_mercado = 1
                  AND   dvv_tipo_despesa.LOG_PROVIS_CONTABILIDADE:
				  
				 IF CAN-FIND(FIRST nota-fiscal NO-LOCK
						WHERE nota-fiscal.cod-estabel  = "001"
						  AND nota-fiscal.nr-proc-exp  = tt-processo-exp.nr-proc-exp
						  AND nota-fiscal.nat-operacao = "7949CW") THEN
				 NEXT BLOCK_DvvMovtoME. 

                FOR EACH estabelec no-lock
                      where estabelec.cod-estabel <> "999",
                    EACH b-dvv-movto
                      WHERE b-dvv-movto.cod_estabel     = estabelec.cod-estabel
                        AND b-dvv-movto.num_processo    = tt-processo-exp.nr-proc-exp
                        AND b-dvv-movto.tipo_despesa    = dvv_movto.tipo_despesa NO-LOCK:
                    IF (b-dvv-movto.data_movto_real <> ? AND b-dvv-movto.data_movto_real <= tt-param.dt-movto-fim) AND 
                       (b-DVV-MOVTO.VALOR_REAL_R$ <> 0 AND b-DVV-MOVTO.VALOR_REAL_R$ <> ?) THEN 
                        NEXT BLOCK_DvvMovtoME.
                END.
                                                                            

                /* Buscar da tabela es-de-para-estab-despesa*/
                ASSIGN c-trading = "".
                FIND FIRST es_param_estab 
                     WHERE es_param_estab.cod_estabel     = tt-processo-exp.cod-estabel 
                       AND es_param_estab.cod_estabel_trd <> "" NO-LOCK NO-ERROR.

                IF  AVAIL es_param_estab 
                    AND CAN-FIND (FIRST processo-exp NO-LOCK 
                              WHERE processo-exp.nr-proc-exp = tt-processo-exp.nr-proc-exp
                                AND processo-exp.cod-estabel = es_param_estab.cod_estabel_trd) 
                THEN ASSIGN c-trading = es_param_estab.cod_estabel_trd
                            c-origem  = es_param_estab.cod_estabel.
                ELSE ASSIGN c-trading = ""
                            c-origem = tt-processo-exp.cod-estabel.
                

                IF c-trading = "" THEN DO:

                    FOR EACH proc-ped-venda NO-LOCK
                       WHERE proc-ped-venda.cod-estabel = tt-processo-exp.cod-estabel
                         AND proc-ped-venda.nr-proc-exp = tt-processo-exp.nr-proc-exp:
                        {esp/es_estab_br.i proc-ped-venda.nome-abrev proc-ped-venda.nr-pedcli}
                    END.
                    //IF c-cod-estabel-br = "" THEN c-cod-estabel-br = "001".
                    IF c-cod-estabel-br = "" OR c-cod-estabel-br = ? THEN c-cod-estabel-br = "001".
                    FIND FIRST es_param_estab 
                         WHERE es_param_estab.cod_estabel_trd = tt-processo-exp.cod-estabel 
                           AND es_param_estab.cod_estabel     = c-cod-estabel-br NO-LOCK NO-ERROR.
                    IF  AVAIL es_param_estab 
                    AND CAN-FIND (FIRST processo-exp NO-LOCK 
                                  WHERE processo-exp.nr-proc-exp = tt-processo-exp.nr-proc-exp
                                    AND processo-exp.cod-estabel = es_param_estab.cod_estabel) 
                    THEN ASSIGN c-trading = es_param_estab.cod_estabel_trd
                                c-origem  = es_param_estab.cod_estabel.
                    ELSE ASSIGN c-trading = ""
                                 c-origem = tt-processo-exp.cod-estabel.

                END.

                FIND FIRST es-de-para-estab-despesa NO-LOCK
                     WHERE es-de-para-estab-despesa.codigo             = dvv_movto.tipo_despesa     
                       AND es-de-para-estab-despesa.cod-estabel-origem = c-origem //tt-processo-exp.cod-estabel
                       AND es-de-para-estab-despesa.cod-estabel-trading = c-trading NO-ERROR.
                IF NOT AVAIL es-de-para-estab-despesa THEN DO:
                   ASSIGN c-estab-destino = tt-processo-exp.cod-estabel.
                END.
                ELSE ASSIGN c-estab-destino = es-de-para-estab-despesa.cod-estabel-destino.

                FIND FIRST es_grp_dvv_desp WHERE
                           es_grp_dvv_desp.cod_desp = dvv_movto.tipo_despesa NO-LOCK NO-ERROR.

                IF NOT AVAIL es_grp_dvv_desp THEN DO:
                    CREATE tt-erro.
                    ASSIGN tt-erro.cod-erro = 17006
                           tt-erro.mensagem = "Despesa " + string(dvv_movto.tipo_despesa) + " n∆o cadastrada em nenhum grupo de despesa (dvv0011) " .
                    NEXT BLOCK_dvvMovtoME.
                END.

                FIND es_grp_dvv WHERE
                     es_grp_dvv.cod_grupo = es_grp_dvv_desp.cod_grupo NO-LOCK NO-ERROR.
                IF NOT AVAIL es_grp_dvv THEN DO:
                    CREATE tt-erro.
                    ASSIGN tt-erro.cod-erro = 17006
                           tt-erro.mensagem = "Grupo de despesa " + string(es_grp_dvv_desp.cod_grupo) + " n∆o cadastrada - (dvv0011) " .
                    NEXT BLOCK_dvvMovtoME.
                END.
    
                IF es_grp_dvv.cod_cta_ctbl = "" OR es_grp_dvv.cod_cta_ctbl = ? THEN DO:
                    CREATE tt-erro.
                    ASSIGN tt-erro.cod-erro = 17006
                           tt-erro.mensagem = "Nao informado a conta contabil para o grupo de despesa " + string(es_grp_dvv_desp.cod_grupo) + " - (dvv0011) " .
                    NEXT BLOCK_dvvMovtoME.
                END.

                ASSIGN de-valor-provisao = DVV_MOVTO.VALOR_PREV_R$.

                FOR EACH dvv_movto_det NO-LOCK
                    WHERE dvv_movto_det.cod_estabel     = dvv_movto.cod_estabel
                      AND dvv_movto_det.num_processo    = dvv_movto.num_processo
                      AND dvv_movto_det.tipo_despesa    = dvv_movto.tipo_despesa:
                    IF dvv_movto_det.data_movto_real   <= tt-param.dt-movto-fim AND dvv_movto_det.ind_movto    = 2 THEN 
                        ASSIGN de-valor-provisao = de-valor-provisao - dvv_movto_det.valor_real_R$.
                END.

                IF de-valor-provisao <= 0 THEN
                    NEXT BLOCK_dvvMovtoME.

                /* Segregaá∆o EUA */
                /* fazer a busca do pais pelo pais do fornecedor da despesa */
                ASSIGN c-unid-negoc = "000".

                FOR FIRST proc-ped-venda WHERE
                          proc-ped-venda.cod-estabel = dvv_movto.cod_estabel  AND
                          proc-ped-venda.nr-proc-exp = dvv_movto.num_processo NO-LOCK:
                    FOR FIRST proc-ped-ent WHERE
                              proc-ped-ent.cod-estabel = proc-ped-venda.cod-estabel AND
                              proc-ped-ent.nr-proc-exp = proc-ped-venda.nr-proc-exp NO-LOCK:

                        FOR FIRST ped-item OF proc-ped-ent NO-LOCK:
                            ASSIGN c-unid-negoc = ped-item.cod-unid-negoc.
                        END.
                    END.
                END.
                IF NOT AVAIL proc-ped-venda THEN DO:
                    FOR FIRST proc-ped-venda WHERE
                              proc-ped-venda.cod-estabel <> "999" AND
                              proc-ped-venda.nr-proc-exp = dvv_movto.num_processo NO-LOCK:
                        FOR FIRST proc-ped-ent WHERE
                                  proc-ped-ent.cod-estabel = proc-ped-venda.cod-estabel AND
                                  proc-ped-ent.nr-proc-exp = proc-ped-venda.nr-proc-exp NO-LOCK:

                            FOR FIRST ped-item OF proc-ped-ent NO-LOCK:
                                ASSIGN c-unid-negoc = ped-item.cod-unid-negoc.
                            END.
                        END.
                    END.
                    IF NOT AVAIL proc-ped-venda THEN DO: 
                        FOR FIRST processo-exp WHERE
                                  processo-exp.cod-estabel = tt-processo-exp.cod-estabel AND
                                  processo-exp.nr-proc-exp = tt-processo-exp.nr-proc-exp NO-LOCK:
                            FOR FIRST bbbemitente WHERE
                                      bbbemitente.cod-emitente = processo-exp.cod-emitente NO-LOCK:
                                FOR FIRST ped-venda WHERE
                                          ped-venda.nome-abrev = bbbemitente.nome-abrev AND
                                          ped-venda.nr-pedcli  = dvv_movto.num_processo NO-LOCK:
                                    FOR FIRST ped-item OF ped-venda NO-LOCK:
                                       ASSIGN c-unid-negoc = ped-item.cod-unid-negoc.
                                    END.
                                END.
                            END.

                        END.
                    END.
                END.
                
                FIND FIRST emitente NO-LOCK
                    WHERE emitente.cod-emitente = dvv_movto.cod_fornecedor NO-ERROR.
                IF AVAIL emitente THEN DO:

                    FIND FIRST mgcad.pais NO-LOCK
                        WHERE pais.nome-pais = emitente.pais NO-ERROR.
                    IF AVAIL pais
                    and pais.nome-pais = "EUA" THEN DO:
            
                        FIND FIRST es-pais-unid-negoc NO-LOCK
                            WHERE es-pais-unid-negoc.cod-pais = pais.cod-pais NO-ERROR.
                        IF AVAIL es-pais-unid-negoc THEN DO:

                                ASSIGN c-unid-negoc = es-pais-unid-negoc.cod-unid-negoc.
                        END.
                    END.
                END.

                RUN pi-cria-dvv_log_provisao (INPUT c-estab-destino, //dvv_tipo_despesa.cod_estabel_prov,
                                              INPUT tt-processo-exp.nr-proc-exp,
                                              INPUT TODAY,
                                              INPUT "DVV ME",
                                              INPUT es_grp_dvv.cod_cta_ctbl,
                                              INPUT dvv_param.cod_cta_provis_dvvme,
                                              INPUT dvv_movto.tipo_despesa,
                                              INPUT de-valor-provisao,
                                              INPUT "",     /*centro de custo*/
                                              INPUT c-unid-negoc). /*unidade negocio*/
            
            END.
        END.
    END.
	

	/* =========================
	   BLOCO NOVO ó DVV REM
	   ========================= */
	IF tt-param.lg-dvv-rem THEN DO:

		FOR EACH tt-processo-exp-consig:

			FIND FIRST dvv_param WHERE
					   dvv_param.cod_estabel = "001" NO-LOCK NO-ERROR.

			/* Confere se o processo tem NF de Remessa 7949CW no estab 001 dentro do perÌodo */
			FOR EACH nota-fiscal NO-LOCK
							 WHERE nota-fiscal.cod-estabel   = "001"
							   AND nota-fiscal.dt-emis-nota >= tt-param.dt-movto-ini
							   AND nota-fiscal.dt-emis-nota <= tt-param.dt-movto-fim
							   AND nota-fiscal.nat-operacao  = "7949CW":

			BLOCK_DvvMovtoREM:
			FOR EACH dvv_movto NO-LOCK
				  WHERE dvv_movto.cod_estabel  = tt-processo-exp-consig.cod-estabel
					AND dvv_movto.num_processo = tt-processo-exp-consig.nr-proc-exp 
					AND SUBSTRING(dvv_movto.CHAR_2,1,1) = "1"      /* provisiona */
					AND DVV_MOVTO.VALOR_PREV_R$ <> 0
					AND DVV_MOVTO.VALOR_PREV_R$ <> ?,
				FIRST dvv_tipo_despesa NO-LOCK
				  WHERE dvv_tipo_despesa.codigo       = dvv_movto.tipo_despesa 
					AND dvv_tipo_despesa.tipo_mercado = 1
					AND dvv_tipo_despesa.LOG_PROVIS_CONTABILIDADE:

				/* Regras N√O alteradas ó mesmÌssimo filtro de ìrealizadoî do ME */
				FOR EACH estabelec NO-LOCK
					  WHERE estabelec.cod-estabel <> "999",
					EACH b-dvv-movto
					  WHERE b-dvv-movto.cod_estabel   = estabelec.cod-estabel
						AND b-dvv-movto.num_processo  = dvv_movto.num_processo
						AND b-dvv-movto.tipo_despesa  = dvv_movto.tipo_despesa NO-LOCK:
					IF (b-dvv-movto.data_movto_real <> ? AND b-dvv-movto.data_movto_real <= tt-param.dt-movto-fim) AND 
					   (b-dvv-movto.valor_real_r$    <> 0 AND b-dvv-movto.valor_real_r$    <> ?) THEN 
						NEXT BLOCK_DvvMovtoREM.
				END.

				/* Mapeia estab destino (igual ao ME) */
				ASSIGN c-trading = "".
				FIND FIRST es_param_estab 
					 WHERE es_param_estab.cod_estabel     = tt-processo-exp-consig.cod-estabel 
					   AND es_param_estab.cod_estabel_trd <> "" NO-LOCK NO-ERROR.

				IF  AVAIL es_param_estab 
				AND CAN-FIND (FIRST processo-exp NO-LOCK 
						  WHERE processo-exp.nr-proc-exp = tt-processo-exp-consig.nr-proc-exp
							AND processo-exp.cod-estabel = es_param_estab.cod_estabel_trd) 
				THEN ASSIGN c-trading = es_param_estab.cod_estabel_trd
							c-origem  = es_param_estab.cod_estabel.
				ELSE ASSIGN c-trading = ""
							c-origem  = tt-processo-exp-consig.cod-estabel.

				FIND FIRST es-de-para-estab-despesa NO-LOCK
					 WHERE es-de-para-estab-despesa.codigo               = dvv_movto.tipo_despesa     
					   AND es-de-para-estab-despesa.cod-estabel-origem   = c-origem
					   AND es-de-para-estab-despesa.cod-estabel-trading  = c-trading NO-ERROR.
				IF NOT AVAIL es-de-para-estab-despesa THEN
					ASSIGN c-estab-destino = tt-processo-exp-consig.cod-estabel.
				ELSE
					ASSIGN c-estab-destino = es-de-para-estab-despesa.cod-estabel-destino.

				/* Grupo/Contas do DVV (mesmo do ME) */
				FIND FIRST es_grp_dvv_desp WHERE
						   es_grp_dvv_desp.cod_desp = dvv_movto.tipo_despesa NO-LOCK NO-ERROR.
				IF NOT AVAIL es_grp_dvv_desp THEN DO:
					CREATE tt-erro.
					ASSIGN tt-erro.cod-erro = 17006
						   tt-erro.mensagem = "Despesa " + STRING(dvv_movto.tipo_despesa) + " n„o cadastrada em grupo (dvv0011) - DVV REM".
					NEXT BLOCK_DvvMovtoREM.
				END.

				FIND es_grp_dvv WHERE
					 es_grp_dvv.cod_grupo = es_grp_dvv_desp.cod_grupo NO-LOCK NO-ERROR.
				IF NOT AVAIL es_grp_dvv THEN DO:
					CREATE tt-erro.
					ASSIGN tt-erro.cod-erro = 17006
						   tt-erro.mensagem = "Grupo de despesa " + STRING(es_grp_dvv_desp.cod_grupo) + " n„o cadastrado - (dvv0011) - DVV REM".
					NEXT BLOCK_DvvMovtoREM.
				END.

				IF es_grp_dvv.cod_cta_ctbl = "" OR es_grp_dvv.cod_cta_ctbl = ? THEN DO:
					CREATE tt-erro.
					ASSIGN tt-erro.cod-erro = 17006
						   tt-erro.mensagem = "Conta do grupo " + STRING(es_grp_dvv_desp.cod_grupo) + " n„o informada - (dvv0011) - DVV REM".
					NEXT BLOCK_DvvMovtoREM.
				END.

				/* Valor da provis„o: 100% do previsto da remessa (sem proporÁ„o de venda)
				   MantÈm a deduÁ„o de realizados, como no ME */
				ASSIGN de-valor-provisao = DVV_MOVTO.VALOR_PREV_R$.

				FOR EACH dvv_movto_det NO-LOCK
					WHERE dvv_movto_det.cod_estabel   = dvv_movto.cod_estabel
					  AND dvv_movto_det.num_processo  = dvv_movto.num_processo
					  AND dvv_movto_det.tipo_despesa  = dvv_movto.tipo_despesa:
					IF dvv_movto_det.data_movto_real <= tt-param.dt-movto-fim
					   AND dvv_movto_det.ind_movto   = 2 THEN
						ASSIGN de-valor-provisao = de-valor-provisao - dvv_movto_det.valor_real_R$.
				END.

				IF de-valor-provisao <= 0 THEN
					NEXT BLOCK_DvvMovtoREM.

				/* UN de negÛcio (mesmo do ME) */
				ASSIGN c-unid-negoc = "000".
				FOR FIRST proc-ped-venda WHERE
						  proc-ped-venda.cod-estabel = dvv_movto.cod_estabel
					  AND proc-ped-venda.nr-proc-exp = dvv_movto.num_processo NO-LOCK:
					FOR FIRST proc-ped-ent WHERE
							  proc-ped-ent.cod-estabel = proc-ped-venda.cod-estabel
						  AND proc-ped-ent.nr-proc-exp = proc-ped-venda.nr-proc-exp NO-LOCK:
						FOR FIRST ped-item OF proc-ped-ent NO-LOCK:
							ASSIGN c-unid-negoc = ped-item.cod-unid-negoc.
						END.
					END.
				END.
				IF NOT AVAIL proc-ped-venda THEN DO:
					FOR FIRST proc-ped-venda WHERE
							  proc-ped-venda.cod-estabel <> "999"
						  AND proc-ped-venda.nr-proc-exp = dvv_movto.num_processo NO-LOCK:
						FOR FIRST proc-ped-ent WHERE
								  proc-ped-ent.cod-estabel = proc-ped-venda.cod-estabel
							  AND proc-ped-ent.nr-proc-exp = proc-ped-venda.nr-proc-exp NO-LOCK:
							FOR FIRST ped-item OF proc-ped-ent NO-LOCK:
								ASSIGN c-unid-negoc = ped-item.cod-unid-negoc.
							END.
						END.
					END.
				END.

				/* Grava provis„o ó origem "DVV REM" (P + DVV REM para prÈvia) */
				RUN pi-cria-dvv_log_provisao (INPUT c-estab-destino,
											  INPUT tt-processo-exp-consig.nr-proc-exp,
											  INPUT TODAY,
											  INPUT "DVV REM",
											  INPUT es_grp_dvv.cod_cta_ctbl,          /* dÈbito */
											  INPUT dvv_param.cod_cta_provis_dvvme,   /* crÈdito: mesmo param do ME */
											  INPUT dvv_movto.tipo_despesa,
											  INPUT de-valor-provisao,
											  INPUT "",                                /* centro de custo */
											  INPUT c-unid-negoc).                     /* UN */

			END. /* dvv_movto */
		END.     /* tt-processo-exp-consig */

	END. /* tt-param.lg-dvv-rem */


END.


IF tt-param.lg-dvv-mi THEN DO:
    /* Carrega TT com os tipos de despesa */
    DEF VAR vdat AS DATE NO-UNDO.
    
    EMPTY TEMP-TABLE tt-desp.

    IF i-num-ped-exec-rpw = 0 then
        run pi-acompanhar in h-acomp ("MI...").

    FOR EACH dvv_tipo_despesa where 
             dvv_tipo_despesa.tipo_mercado = 2:

        IF NOT dvv_tipo_despesa.LOG_PROVIS_CONTABILIDADE THEN
            NEXT.

        CREATE tt-desp.
        BUFFER-COPY dvv_tipo_despesa to tt-desp.
    END. /* for each dvv_tipo_despesa */
    
    EMPTY TEMP-TABLE tt-despesa.

    DO  i = 1 TO NUM-ENTRIES(tt-param.cod-estabel,",") :

        FOR EACH estabelec no-lock
            where estabelec.cod-estabel = ENTRY(i,tt-param.cod-estabel,","):
                 
            DO vdat = tt-param.dt-movto-ini TO tt-param.dt-movto-fim:
                
                nota-fiscal-blk:
                FOR EACH nota-fiscal NO-LOCK WHERE 
                         nota-fiscal.cod-estabel  = estabelec.cod-estabel
                     AND nota-fiscal.dt-emis-nota = vdat
                     AND nota-fiscal.pais         = "BRASIL":
    
                    ASSIGN l-duplicatas = NO.
    
                    /* Verifica se nota GEROU DUPLICATAS */
                    IF NOT CAN-FIND (FIRST fat-duplic WHERE 
                                           fat-duplic.cod-estabel = nota-fiscal.cod-estabel 
                                       AND fat-duplic.serie       = nota-fiscal.serie       
                                       AND fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis) THEN DO: 
                        
                        FIND nota-fisc-adc NO-LOCK
                            WHERE nota-fisc-adc.cod-estab          = nota-fiscal.cod-estabel
                              AND nota-fisc-adc.cod-ser-docto-referado = nota-fiscal.serie
                              AND nota-fisc-adc.cod-docto-referado = nota-fiscal.nr-nota-fis NO-ERROR.
                        IF AVAIL nota-fisc-adc THEN DO:
    
                            FIND b-nota-fiscal 
                                WHERE b-nota-fiscal.cod-estabel = nota-fisc-adc.cod-estab 
                                  AND b-nota-fiscal.serie       = nota-fisc-adc.cod-serie
                                  AND b-nota-fiscal.nr-nota-fis = nota-fisc-adc.cod-nota-fisc NO-LOCK NO-ERROR.
                            IF CAN-FIND (FIRST fat-duplic 
                                            WHERE fat-duplic.cod-estabel = b-nota-fiscal.cod-estabel 
                                              AND fat-duplic.serie       = b-nota-fiscal.serie       
                                              AND fat-duplic.nr-fatura   = b-nota-fiscal.nr-nota-fis) THEN
                                ASSIGN l-duplicatas = YES.
                            
                        END.
    
                        IF NOT l-duplicatas THEN
                            NEXT nota-fiscal-blk.
                    END.
        
                    FIND FIRST bf-dvv_movto_mi 
                        WHERE bf-dvv_movto_mi.cod_estabel        = nota-fiscal.cod-estabel
                          AND   bf-dvv_movto_mi.serie            = nota-fiscal.serie
                          AND   bf-dvv_movto_mi.nr-nota-fis      = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
        
                    IF NOT AVAIL bf-dvv_movto_mi THEN
                        NEXT nota-fiscal-blk.
        
                    IF i-num-ped-exec-rpw = 0 then
                        run pi-acompanhar in h-acomp ("MI... calculando... " + nota-fiscal.nr-nota-fis).
                    
                    EMPTY TEMP-TABLE tt-item.
            
                    ASSIGN v_tot_peso_liq  = 0
                           v_tot_peso_brut = 0.
                                
                    FIND FIRST nota-embal NO-LOCK WHERE
                               nota-embal.cod-estabel = nota-fiscal.cod-estabel
                           AND nota-embal.serie       = nota-fiscal.serie
                           AND nota-embal.nr-nota-fis = nota-fiscal.nr-nota-fis NO-ERROR.
        
                    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:                    
                    
                        FIND FIRST item-embal NO-LOCK WHERE
                                   item-embal.cod-estabel = it-nota-fisc.cod-estabel
                               AND item-embal.serie       = it-nota-fisc.serie
                               AND item-embal.nr-nota-fis = it-nota-fisc.nr-nota-fis
                               AND item-embal.nr-seq-fat  = it-nota-fisc.nr-seq-fat
                               AND item-embal.it-codigo   = it-nota-fisc.it-codigo NO-ERROR.
        
                        FIND ITEM NO-LOCK WHERE
                             ITEM.it-codigo = it-nota-fisc.it-codigo NO-ERROR.
        
                        //FIND fam-comerc NO-LOCK
                        //    WHERE fam-comerc.fm-cod-com = ITEM.fm-cod-com NO-ERROR.
        
                        //FRANK - 06/-7/2022 - colocado a regra abaixo para tratar quando as notas fiscais estao com os pesos zerados.
                        ASSIGN de-peso-liq = it-nota-fisc.peso-liq-fat
                               de-peso-bru = it-nota-fisc.peso-bruto.

                        IF de-peso-liq = 0 THEN
                            ASSIGN de-peso-liq = IF ITEM.peso-liquido <> 0 THEN ITEM.peso-liquido * it-nota-fisc.qt-faturada[1] ELSE 1 * it-nota-fisc.qt-faturada[1].
                        IF de-peso-bru = 0 THEN
                            ASSIGN de-peso-bru = IF ITEM.peso-bruto <> 0  THEN ITEM.peso-bruto * it-nota-fisc.qt-faturada[1] ELSE 1 * it-nota-fisc.qt-faturada[1].

                        CREATE tt-item.
                        ASSIGN tt-item.it-codigo = it-nota-fisc.it-codigo
                               tt-item.peso_liq  = de-peso-liq //it-nota-fisc.peso-liq-fat
                               tt-item.peso_brut = de-peso-bru //it-nota-fisc.peso-bruto
                               tt-item.familia   = ITEM.fm-cod-com
                               tt-item.embal     = IF AVAIL nota-embal THEN nota-embal.sigla-emb ELSE ""
                               tt-item.cod-un    = IF it-nota-fisc.cod-unid-negoc = "" THEN "000" ELSE it-nota-fisc.cod-unid-negoc.
                               /*tt-item.embal     = IF AVAIL item-embal THEN item-embal.sigla-emb ELSE tt-item.embal*/ .
                    
                        ASSIGN v_tot_peso_liq  = v_tot_peso_liq  + de-peso-liq //it-nota-fisc.peso-liq-fat
                               v_tot_peso_brut = v_tot_peso_brut + de-peso-bru. //it-nota-fisc.peso-bruto.
                    END.
        
                    /* Le Despesas da NOTA */
                    FOR EACH dvv_movto_mi NO-LOCK WHERE 
                             dvv_movto_mi.cod_estabel = nota-fiscal.cod-estabel  
                         AND dvv_movto_mi.serie       = nota-fiscal.serie       
                         AND dvv_movto_mi.nr-nota-fis = nota-fiscal.nr-nota-fis: 
        
                        IF substring(dvv_movto_mi.char_3,1,1) <>  "1" THEN NEXT.
                                                                                                                                                                                                                                     
                        IF (dvv_movto_mi.valor_prev_R$ <> 0 AND dvv_movto_mi.valor_prev_R$ <> ?) AND 
                           ((dvv_movto_mi.valor_real_R$ = 0 OR dvv_movto_mi.valor_real_R$ = ?) OR dvv_movto_mi.data_movto_real > tt-param.dt-movto-fim) THEN DO:
                            /* Impress∆o Detalhada */
            
                            FIND emitente NO-LOCK WHERE 
                                 emitente.cod-emitente = dvv_movto_mi.cod_fornecedor NO-ERROR.
            
                            /* Imprime detalhando ITEM */
                            FIND FIRST tt-desp WHERE
                                       tt-desp.codigo = dvv_movto_mi.tipo_despesa NO-ERROR.
            
                            IF NOT AVAIL tt-desp THEN
                                NEXT.
    
                            ASSIGN v_tot_vl_orc  = 0
                                   v_tot_vl_prev = 0
                                   v_tot_vl_real = 0.
            
                            FOR EACH tt-item:
                                CREATE tt-despesa.
                                ASSIGN tt-despesa.cod_estabel       = nota-fiscal.cod-estabel
                                       tt-despesa.serie             = nota-fiscal.serie
                                       tt-despesa.nr-nota-fis       = nota-fiscal.nr-nota-fis
                                       tt-despesa.dt-emissao        = nota-fiscal.dt-emis-nota
                                       tt-despesa.cod-despesa       = dvv_movto_mi.tipo_despesa
                                       tt-despesa.despesa           = IF AVAIL dvv_tipo_despesa THEN dvv_tipo_despesa.descricao ELSE ""
                                       tt-despesa.vl-desp-previsto  = dvv_movto_mi.valor_prev_R$
                                       tt-despesa.vl-desp-prev-liq  = DECIMAL(substring(dvv_movto_mi.CHAR_2,1,20))
                                       tt-despesa.vl-desp-real      = dvv_movto_mi.valor_real_R$
                                       tt-despesa.vl-desp-real-liq  = DECIMAL(substring(dvv_movto_mi.CHAR_2,21,20))
                                       tt-despesa.vl-provisao       = tt-despesa.vl-desp-prev-liq - tt-despesa.vl-desp-real-liq /*dvv_movto_mi.valor_prev_R$ - dvv_movto_mi.valor_real_R$*/
                                       tt-despesa.vl-frete-comp     = DEC(SUBSTRING(DVV_MOVTO_MI.CHAR_2,71,20))
                                       tt-despesa.dt-prev           = dvv_movto_mi.data_movto_prev
                                       tt-despesa.dt-real           = dvv_movto_mi.data_movto_real
                                       tt-despesa.it-codigo         = tt-item.it-codigo
                                       tt-despesa.familia           = tt-item.familia
                                       tt-despesa.embal             = tt-item.embal  
                                       tt-despesa.peso-liq          = tt-item.peso_liq
                                       tt-despesa.peso-brut         = tt-item.peso_brut 
                                       tt-despesa.fornecedor        = emitente.cod-emitente
                                       tt-despesa.cod-unid-negoc    = tt-item.cod-un.
            
                                IF SUBSTRING(tt-desp.CHAR_3,61,20) = "Peso L°quido" THEN
                                    ASSIGN tt-despesa.vl-desp-orcado   = ROUND(tt-item.peso_liq * de_dvv_movto_mi_vl_orc_r$ / v_tot_peso_liq, 2)
                                           tt-despesa.vl-desp-previsto = ROUND(tt-item.peso_liq * dvv_movto_mi.vl_desp_prev_net /*dvv_movto_mi.valor_prev_R$*/ / v_tot_peso_liq, 2)
                                           tt-despesa.vl-desp-prev-liq = ROUND(tt-item.peso_liq * tt-despesa.vl-desp-prev-liq / v_tot_peso_liq, 2)
                                           tt-despesa.vl-desp-real     = ROUND(tt-item.peso_liq * dvv_movto_mi.valor_real_R$ / v_tot_peso_liq, 2)
                                           tt-despesa.vl-desp-real-liq = ROUND(tt-item.peso_liq * tt-despesa.vl-desp-real-liq / v_tot_peso_liq, 2)
                                           tt-despesa.vl-frete-comp    = ROUND(tt-item.peso_liq * tt-despesa.vl-frete-comp / v_tot_peso_liq, 2)
                                           tt-despesa.vl-provisao      = tt-despesa.vl-desp-prev-liq - tt-despesa.vl-desp-real-liq.
                                ELSE 
                                    IF SUBSTRING(tt-desp.CHAR_3,61,20) = "Peso Bruto" THEN
                                        ASSIGN tt-despesa.vl-desp-orcado   = ROUND(tt-item.peso_brut * de_dvv_movto_mi_vl_orc_r$ / v_tot_peso_brut, 2)
                                               tt-despesa.vl-desp-previsto = ROUND(tt-item.peso_brut * dvv_movto_mi.vl_desp_prev_net /*dvv_movto_mi.valor_prev_R$*/ / v_tot_peso_brut, 2)
                                               tt-despesa.vl-desp-prev-liq = ROUND(tt-item.peso_brut * tt-despesa.vl-desp-prev-liq / v_tot_peso_brut, 2)
                                               tt-despesa.vl-desp-real     = ROUND(tt-item.peso_brut * dvv_movto_mi.valor_real_R$ / v_tot_peso_brut, 2)
                                               tt-despesa.vl-desp-real-liq = ROUND(tt-item.peso_brut  * tt-despesa.vl-desp-real-liq / v_tot_peso_brut, 2)
                                               tt-despesa.vl-frete-comp    = ROUND(tt-item.peso_brut * tt-despesa.vl-frete-comp / v_tot_peso_brut, 2)
                                               tt-despesa.vl-provisao      = tt-despesa.vl-desp-prev-liq - tt-despesa.vl-desp-real-liq.
            
                                ASSIGN v_tot_vl_orc  = v_tot_vl_orc + tt-despesa.vl-desp-orcado
                                       v_tot_vl_prev = v_tot_vl_prev + tt-despesa.vl-desp-previsto 
                                       v_tot_vl_real = v_tot_vl_real + tt-despesa.vl-desp-real.
            
                            END. /* FOR EACH tt-item: */
            
                            /* Verifica Valor Rateado */
                            IF de_dvv_movto_mi_vl_orc_r$ <> v_tot_vl_orc THEN 
                                ASSIGN tt-despesa.vl-desp-orcado = tt-despesa.vl-desp-orcado + (de_dvv_movto_mi_vl_orc_r$ - v_tot_vl_orc).
            
                            IF dvv_movto_mi.valor_prev_R$ <> v_tot_vl_prev THEN 
                                ASSIGN tt-despesa.vl-desp-previsto = tt-despesa.vl-desp-previsto + (/*dvv_movto_mi.valor_prev_R$*/ dvv_movto_mi.vl_desp_prev_net - v_tot_vl_prev).
            
                            IF dvv_movto_mi.valor_real_R$ <> v_tot_vl_real THEN 
                                ASSIGN tt-despesa.vl-desp-real = tt-despesa.vl-desp-real + (dvv_movto_mi.valor_real_R$ - v_tot_vl_real).
            
                        END.
                    END.
        
                END.
            END.
        END.
    END. /*DO I = 1 TO NUM-ENTREIS*/

    IF  tt-param.tipo-exec = 1 THEN DO:  // Na execuá∆o para fechamento prÇvio (tipo 2) n∆o deve mexer nas provis‰es normais.
        
        IF i-num-ped-exec-rpw = 0 then
            run pi-acompanhar in h-acomp ("Cancela Provis∆o MI... ").
        
        /*cancela provisoes pendente*/
        FOR EACH bdvv_log_provisao WHERE
                 bdvv_log_provisao.dt_provisao     <= TODAY   AND
                 bdvv_log_provisao.origem           = "DVV MI"  AND
                 bdvv_log_provisao.situacao        = 1          AND
                 CAN-DO(tt-param.cod-estabel, bdvv_log_provisao.cod_estabel)   // estab. est† na lista de seleá∆o
                 EXCLUSIVE-LOCK:
    
            /*IF AVAIL bdvv_log_provisao THEN DO:*/
             IF bdvv_log_provisao.situacao = 1 THEN
                 ASSIGN bdvv_log_provisao.situacao = 3.
    
             RUN pi-gera-estorno.
        END.
    END.
    ELSE DO:
            IF MONTH(tt-param.dt-movto-fim) = 01 THEN   
                ASSIGN dtEstornoPreviaIni = DATE(12,01,YEAR(tt-param.dt-movto-fim) - 1)
                       dtEstornoPreviaFim = DATE(12,31,YEAR(tt-param.dt-movto-fim) - 1).
            ELSE IF MONTH(tt-param.dt-movto-fim) = 12 THEN   
                ASSIGN dtEstornoPreviaIni = DATE(11,01,YEAR(tt-param.dt-movto-fim))
                       dtEstornoPreviaFim = DATE(11,30,YEAR(tt-param.dt-movto-fim)).                       
            ELSE
                ASSIGN dtEstornoPreviaIni = DATE(MONTH(tt-param.dt-movto-fim) - 1,01,YEAR(tt-param.dt-movto-fim))
                       dtEstornoPreviaFim = DATE(MONTH(tt-param.dt-movto-fim),1,YEAR(tt-param.dt-movto-fim)) - 1.
            
        FOR EACH bdvv_log_provisao WHERE
                 bdvv_log_provisao.dt_lancto_provis     >= dtEstornoPreviaIni  AND
                 bdvv_log_provisao.dt_lancto_provis     <= dtEstornoPreviaFim  AND
                 bdvv_log_provisao.origem           = "DVV MI"  AND
                 bdvv_log_provisao.situacao        = 2       AND
                 CAN-DO(tt-param.cod-estabel, bdvv_log_provisao.cod_estabel)   // estab. est† na lista de seleá∆o
                 EXCLUSIVE-LOCK:

            RUN pi-gera-estorno-previa.
        END.
    
    END.

    BLOCK_dvvMovtoMI:
    FOR EACH tt-despesa:

        IF i-num-ped-exec-rpw = 0 then
            run pi-acompanhar in h-acomp (tt-despesa.nr-nota-fis).

        FIND FIRST dvv_frete_ibc_mi WHERE
                   dvv_frete_ibc_mi.cod_estabel = tt-despesa.cod_estabel NO-LOCK NO-ERROR.
        FIND FIRST dvv_tipo_despesa WHERE
                   dvv_tipo_despesa.codigo = tt-despesa.cod-despesa AND
                   dvv_tipo_despesa.tipo_mercado = 2 NO-LOCK NO-ERROR.

        /* Buscar da tabela es-de-para-estab-despesa*/
        FIND FIRST es-de-para-estab-despesa NO-LOCK
             WHERE es-de-para-estab-despesa.codigo              = tt-despesa.cod-despesa 
               AND es-de-para-estab-despesa.cod-estabel-origem  = tt-despesa.cod_estabel 
               AND es-de-para-estab-despesa.cod-estabel-trading = "" 
               NO-ERROR.
        IF NOT AVAIL es-de-para-estab-despesa THEN DO:
            ASSIGN c-estab-destino = tt-despesa.cod_estabel.
        END. 
        ELSE ASSIGN c-estab-destino = es-de-para-estab-despesa.cod-estabel-destino.


        FIND FIRST es_grp_dvv_desp WHERE
                   es_grp_dvv_desp.cod_desp = tt-despesa.cod-despesa NO-LOCK NO-ERROR.

        IF NOT AVAIL es_grp_dvv_desp THEN DO:
            CREATE tt-erro.
            ASSIGN tt-erro.cod-erro = 17006
                   tt-erro.mensagem = "Despesa " + string(tt-despesa.cod-despesa) + " n∆o cadastrada em nenhum grupo de despesa (dvv0011) " .
            NEXT BLOCK_dvvMovtoMI.
        END.

        FIND es_grp_dvv WHERE
             es_grp_dvv.cod_grupo = es_grp_dvv_desp.cod_grupo NO-LOCK NO-ERROR.
        IF NOT AVAIL es_grp_dvv THEN DO:
            CREATE tt-erro.
            ASSIGN tt-erro.cod-erro = 17006
                   tt-erro.mensagem = "Grupo de despesa " + string(es_grp_dvv_desp.cod_grupo) + " n∆o cadastrada - (dvv0011) " .
            NEXT BLOCK_dvvMovtoMI.
        END.

        IF es_grp_dvv.cod_cta_ctbl = "" OR es_grp_dvv.cod_cta_ctbl = ? THEN DO:
            CREATE tt-erro.
            ASSIGN tt-erro.cod-erro = 17006
                   tt-erro.mensagem = "Nao informado a conta contabil para o grupo de despesa " + string(es_grp_dvv_desp.cod_grupo) + " - (dvv0011) " .
            NEXT BLOCK_dvvMovtoMI.
        END.

        /* Segregaá∆o EUA */
        /* fazer a busca do pais pelo pais do fornecedor da despesa */
        ASSIGN c-unid-negoc  = tt-despesa.cod-unid-negoc. //alterado para gravar a unidade de negocio do item-nota-fisc.
        FIND FIRST emitente NO-LOCK
            WHERE emitente.cod-emitente = tt-despesa.fornecedor NO-ERROR.
        IF AVAIL emitente THEN DO:

            FIND FIRST mgcad.pais NO-LOCK
                WHERE pais.nome-pais = emitente.pais NO-ERROR.
            IF AVAIL pais THEN DO:
    
                FIND FIRST es-pais-unid-negoc NO-LOCK
                    WHERE es-pais-unid-negoc.cod-pais = pais.cod-pais NO-ERROR.
                IF AVAIL es-pais-unid-negoc THEN DO:

                    IF pais.nome-pais = "EUA" THEN
                        ASSIGN c-unid-negoc = es-pais-unid-negoc.cod-unid-negoc.
                END.
            END.
        END.

        RUN pi-cria-dvv_log_provisao (INPUT c-estab-destino, //dvv_tipo_despesa.cod_estabel_prov,
                                      INPUT tt-despesa.nr-nota-fis,
                                      INPUT TODAY,
                                      INPUT "DVV MI",
                                      INPUT es_grp_dvv.cod_cta_ctbl,
                                      INPUT dvv_frete_ibc_mi.cod_cta_provis_dvvmi,
                                      INPUT tt-despesa.cod-despesa,
                                      INPUT tt-despesa.vl-desp-previsto,
                                      INPUT "",     /*centro de custo*/
                                      INPUT c-unid-negoc). /*unidade negocio*/
    END.
END.

IF i-num-ped-exec-rpw = 0 then
    run pi-finalizar in h-acomp.

IF CAN-FIND (FIRST tt-dvv_log_prov_desp) THEN DO:

    ASSIGN c-arq-lista-desp = session:temp-directory + "dvv5100"
                            + (if tt-param.tipo-exec = 1 then "" else "previa")
                            + replace(string(today),"/","") + REPLACE(string(TIME,"HH:MM"),":","") + ".csv" .
    OUTPUT STREAM a TO VALUE (c-arq-lista-desp).
    PUT STREAM a UNFORMATTED "Num Id Provis∆o" ";"
                    "Origem"          ";"
                    "Estabel"         ";"
                    "Proc Exp"        ";"
                    "Despesa"         ";"
                    "Vl.Despesa"      ";"
                    "Conta CrÇdito"   ";"
                    "Conta DÇbito"    ";" 
                    "Unid.Negoc"      SKIP.
                     
    FOR EACH tt-dvv_log_prov_desp:
        PUT STREAM a UNFORMATTED 
            tt-dvv_log_prov_desp.num_id_provisao ";"
            tt-dvv_log_prov_desp.origem          ";"
            tt-dvv_log_prov_desp.cod_estabel     ";"
            tt-dvv_log_prov_desp.nr_proc_exp     ";"
            tt-dvv_log_prov_desp.cod_despesa     ";"
            tt-dvv_log_prov_desp.vl_despesa      ";"
            tt-dvv_log_prov_desp.cod_cta_ctbl_cr ";"
            tt-dvv_log_prov_desp.cod_cta_ctbl_db ";" 
            tt-dvv_log_prov_desp.cod_unid_negoc  SKIP.

    END.
    OUTPUT STREAM a CLOSE.
END.


IF CAN-FIND ( FIRST  tt-erro) THEN DO:
    FOR EACH tt-erro:
        DISP tt-erro.cod-erro COLUMN-LABEL "Erro"
             tt-erro.mensagem COLUMN-LABEL "Mensagem" FORMAT "x(70)"
        WITH DOWN FRAME f-param WIDTH 300 STREAM-IO.
    END.
    PUT SKIP (1).
END.
ELSE DO :
    bloco_aaa:
    DO  i = 1 TO NUM-ENTRIES(tt-param.cod-estabel,",") :

        FOR EACH estabelec where
                 estabelec.cod-estabel = ENTRY(i,tt-param.cod-estabel,",") NO-LOCK:

            IF NOT CAN-FIND (FIRST dvv_param WHERE
                                   dvv_param.cod_estabel = estabelec.cod-estabel) THEN DO:

                PUT UNFORMATTED SKIP (2)
                    "Estabelecimento: " + estabelec.cod-estabel + " n∆o cadastrado nos parametros do DVV (dvv0150)"  SKIP.
                   
                LEAVE BLOco_aaa.
            END.
        END.
    END.

    PUT UNFORMATTED SKIP (2)
        "Gerado com sucesso os dados de provis∆o. Favor consultar o arquivo abaixo:"  SKIP
        c-arq-lista-desp.
    
END.



{include/i-rpclo.i}

return "OK":U.

////////////////////////////////////////////////////////////// PROCEDURES INTERNAS

PROCEDURE pi-cria-dvv_log_provisao:
    DEF VAR i_num_id_provisao AS INT NO-UNDO.

    DEF INPUT  PARAMETER p-cod-estab  AS CHAR NO-UNDO.
    DEF INPUT  PARAMETER p-nr-docto   AS CHAR NO-UNDO.
    DEF INPUT  PARAMETER p-dt-movto   AS DATE NO-UNDO.
    DEF INPUT  PARAMETER p-origem     AS CHAR NO-UNDO.
    DEF INPUT  PARAMETER p-ct-debito  AS CHAR NO-UNDO.
    DEF INPUT  PARAMETER p-ct-credito AS CHAR NO-UNDO.
    DEF INPUT  PARAMETER p-cod-desp   AS INT  NO-UNDO.
    DEF INPUT  PARAMETER p-vl-desp    AS DEC  NO-UNDO.
    DEF INPUT  PARAMETER p-cod-ccusto AS CHAR NO-UNDO.
    DEF INPUT  PARAMETER p-cod-un     AS CHAR NO-UNDO.

    IF i-num-ped-exec-rpw = 0 then
        run pi-acompanhar in h-acomp (p-origem + " doc " + p-nr-docto).    
    process events.
    
    if  tt-param.tipo-exec = 2 then
        assign p-origem = "P" + p-origem.  // Fechamento prÇvio, todas as provis‰es comeáam com P na origem. Exemplo: PDEX, PDVV MI, PDVV ME.
    
    /*os est'ao em comentario pq nao foram criados ainda na base de dados.*/
    FIND FIRST dvv_log_provisao WHERE
               dvv_log_provisao.cod_estabel     = p-cod-estab AND
               dvv_log_provisao.dt_provisao     = p-dt-movto   AND
               dvv_log_provisao.cod_cta_ctbl_db = p-ct-debito  AND
               dvv_log_provisao.cod_ccusto      = p-cod-ccusto AND
               dvv_log_provisao.cod_un          = p-cod-un     AND
               dvv_log_provisao.origem          = p-origem     and
               dvv_log_provisao.situacao        = 1 EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAIL dvv_log_provisao THEN DO:

         ASSIGN i_num_id_provisao = 1.
         FIND LAST bdvv_log_provisao NO-LOCK NO-ERROR.
         IF AVAIL bdvv_log_provisao THEN
             ASSIGN i_num_id_provisao = bdvv_log_provisao.num_id_provisao + 1.

         CREATE dvv_log_provisao.
         ASSIGN dvv_log_provisao.num_id_provisao = i_num_id_provisao
                dvv_log_provisao.cod_estabel     = p-cod-estab
                dvv_log_provisao.dt_provisao     = p-dt-movto 
                dvv_log_provisao.origem_movto    = p-origem 
                dvv_log_provisao.situacao        = 1 /*"Pendente"*/
                dvv_log_provisao.cod_cta_ctbl_cr = p-ct-credito
                dvv_log_provisao.cod_cta_ctbl_db = p-ct-debito
                dvv_log_provisao.cod_ccusto      = p-cod-ccusto
                dvv_log_provisao.cod_un          = p-cod-un
                .

    END.

    ASSIGN dvv_log_provisao.vl_provisao = dvv_log_provisao.vl_provisao + p-vl-desp.

    FIND CURRENT dvv_log_provisao NO-LOCK NO-ERROR.

    FIND FIRST dvv_log_prov_desp WHERE
               dvv_log_prov_desp.cod_estabel = p-cod-estab AND
               dvv_log_prov_desp.nr_proc_exp = p-nr-docto  AND
               dvv_log_prov_desp.cod_despesa = p-cod-desp  AND 
               dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL dvv_log_prov_desp THEN DO:

        CREATE dvv_log_prov_desp.
        ASSIGN dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao
               dvv_log_prov_desp.cod_estabel     = p-cod-estab
               dvv_log_prov_desp.nr_proc_exp     = p-nr-docto 
               dvv_log_prov_desp.cod_despesa     = p-cod-desp
               overlay(dvv_log_prov_desp.CHAR_1,1,1) = IF tt-param.tipo-exec = 1 THEN "1" ELSE "0" /*grava 1-sim para provisionar, e 0-n∆o provisionar para prÇvias das provis‰es */.

    END.

    ASSIGN dvv_log_prov_desp.vl_despesa      = dvv_log_prov_desp.vl_despesa + p-vl-desp.

    FIND FIRST tt-dvv_log_prov_desp WHERE
               tt-dvv_log_prov_desp.cod_estabel = p-cod-estab AND
               tt-dvv_log_prov_desp.nr_proc_exp = p-nr-docto  AND
               tt-dvv_log_prov_desp.cod_despesa = p-cod-desp  AND 
               tt-dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao use-index desp_unique EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL tt-dvv_log_prov_desp THEN DO:

        CREATE tt-dvv_log_prov_desp.
        ASSIGN tt-dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao
               tt-dvv_log_prov_desp.cod_estabel     = p-cod-estab
               tt-dvv_log_prov_desp.nr_proc_exp     = p-nr-docto 
               tt-dvv_log_prov_desp.cod_despesa     = p-cod-desp
               tt-dvv_log_prov_desp.origem          = p-origem
               tt-dvv_log_prov_desp.cod_cta_ctbl_cr = dvv_log_provisao.cod_cta_ctbl_cr
               tt-dvv_log_prov_desp.cod_cta_ctbl_db = dvv_log_provisao.cod_cta_ctbl_db
               tt-dvv_log_prov_desp.cod_unid_negoc  = p-cod-un.
    END.
    ASSIGN tt-dvv_log_prov_desp.vl_despesa      = tt-dvv_log_prov_desp.vl_despesa + dvv_log_prov_desp.vl_despesa.

END PROCEDURE.

PROCEDURE pi-gera-estorno-previa:
    DEF BUFFER bf_dvv_log_prov_desp FOR dvv_log_prov_desp.

    DEF VAR i-seq AS INT NO-UNDO.
    
    IF AVAIL bdvv_log_provisao THEN DO:
        
        ASSIGN i-seq = 1.
        FIND LAST bfdvv_log_provisao NO-LOCK NO-ERROR.
        IF AVAIL bfdvv_log_provisao THEN
            ASSIGN i-seq = bfdvv_log_provisao.num_id_provisao + 1.
            
        CREATE bfdvv_log_provisao.
        BUFFER-COPY bdvv_log_provisao EXCEPT dt_lancto_estorno dt_lancto_provis dt_provisao num_id_provisao origem_movto origem TO bfdvv_log_provisao.
        ASSIGN bfdvv_log_provisao.dt_lancto_provis = ?
               bfdvv_log_provisao.dt_provisao      = TODAY
               bfdvv_log_provisao.dt_lancto_estorno      = DATE(MONTH(TODAY),01,YEAR(TODAY))
               bfdvv_log_provisao.num_id_provisao = i-seq
               bfdvv_log_provisao.origem       = 'EP' + bdvv_log_provisao.origem
               bfdvv_log_provisao.origem_movto = 'EP' + bdvv_log_provisao.origem_movto
               bfdvv_log_provisao.situacao = 1.
               
        FOR EACH dvv_log_prov_desp WHERE
             dvv_log_prov_desp.num_id_provisao = bdvv_log_provisao.num_id_provisao NO-LOCK:
             
             /*MESSAGE 'i-seq - ' i-seq SKIP
                     CAN-FIND(FIRST bf_dvv_log_prov_desp WHERE
                        bf_dvv_log_prov_desp.num_id_provisao = i-seq)
                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */
             
             CREATE bf_dvv_log_prov_desp.
             BUFFER-COPY dvv_log_prov_desp EXCEPT num_id_provisao TO bf_dvv_log_prov_desp.
             ASSIGN bf_dvv_log_prov_desp.num_id_provisao = i-seq /*bfdvv_log_provisao.num_id_provisao*/ .
             
             FIND FIRST tt-dvv_log_prov_desp WHERE
                       tt-dvv_log_prov_desp.cod_estabel = bf_dvv_log_prov_desp.cod_estabel AND
                       tt-dvv_log_prov_desp.nr_proc_exp = bf_dvv_log_prov_desp.nr_proc_exp  AND
                       tt-dvv_log_prov_desp.cod_despesa = bf_dvv_log_prov_desp.cod_despesa  AND 
                       tt-dvv_log_prov_desp.num_id_provisao = bf_dvv_log_prov_desp.num_id_provisao use-index desp_unique EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAIL tt-dvv_log_prov_desp THEN DO:

                CREATE tt-dvv_log_prov_desp.
                ASSIGN tt-dvv_log_prov_desp.num_id_provisao = bf_dvv_log_prov_desp.num_id_provisao
                       tt-dvv_log_prov_desp.cod_estabel     = bf_dvv_log_prov_desp.cod_estabel
                       tt-dvv_log_prov_desp.nr_proc_exp     = bf_dvv_log_prov_desp.nr_proc_exp 
                       tt-dvv_log_prov_desp.cod_despesa     = bf_dvv_log_prov_desp.cod_despesa
                       tt-dvv_log_prov_desp.origem          = bfdvv_log_provisao.origem
                       tt-dvv_log_prov_desp.cod_cta_ctbl_cr = bfdvv_log_provisao.cod_cta_ctbl_cr
                       tt-dvv_log_prov_desp.cod_cta_ctbl_db = bfdvv_log_provisao.cod_cta_ctbl_db
                       tt-dvv_log_prov_desp.cod_unid_negoc  = bfdvv_log_provisao.cod_un.
            END.
            ASSIGN tt-dvv_log_prov_desp.vl_despesa      = tt-dvv_log_prov_desp.vl_despesa + bf_dvv_log_prov_desp.vl_despesa.

        END.
        
        /*FIND CURRENT dvv_log_provisao NO-LOCK NO-ERROR.

        FIND FIRST dvv_log_prov_desp WHERE
                   dvv_log_prov_desp.cod_estabel = dvv_log_provisao.cod_estabel AND
                   dvv_log_prov_desp.nr_proc_exp = p-nr-docto  AND
                   dvv_log_prov_desp.cod_despesa = p-cod-desp  AND 
                   dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL dvv_log_prov_desp THEN DO:

            CREATE dvv_log_prov_desp.
            ASSIGN dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao
                   dvv_log_prov_desp.cod_estabel     = p-cod-estab
                   dvv_log_prov_desp.nr_proc_exp     = p-nr-docto 
                   dvv_log_prov_desp.cod_despesa     = p-cod-desp
                   overlay(dvv_log_prov_desp.CHAR_1,1,1) = IF tt-param.tipo-exec = 1 THEN "1" ELSE "0" /*grava 1-sim para provisionar, e 0-n∆o provisionar para prÇvias das provis‰es */.

        END.  */             


    END.


END PROCEDURE.

PROCEDURE pi-gera-estorno:

    DEF VAR i-seq AS INT NO-UNDO.
    DEF BUFFER bdvv_log_provisao_exec FOR dvv_log_provisao_exec.

    FIND estabelec WHERE
         estabelec.cod-estabel = bdvv_log_provisao.cod_estabel NO-LOCK NO-ERROR.

    EMPTY TEMP-TABLE tt_movto_import.

    CREATE tt_movto_import.
    ASSIGN tt_movto_import.cod_empresa     = IF estabelec.ep-codigo = "2" THEN "02" ELSE string(int(estabelec.ep-codigo),"99")
           tt_movto_import.cod_estab       = bdvv_log_provisao.cod_estabel
           tt_movto_import.dat_lancto_ctbl = TODAY // tt-dvv_log_provisao.dt_provisao
           tt_movto_import.des_lote_ctbl   = "Estorno Lancamento Provisao DVV"
           tt_movto_import.ind_natur       = ""
           tt_movto_import.cod_un          = bdvv_log_provisao.cod_un //"000"
           tt_movto_import.cod_plano_cta   = "NITROC"
           tt_movto_import.cod_cta_db      = bdvv_log_provisao.cod_cta_ctbl_db
           tt_movto_import.cod_plano_ccu   = ""
           tt_movto_import.cod_ccu         = ""
           tt_movto_import.cod_cta_cr      = bdvv_log_provisao.cod_cta_ctbl_cr
           tt_movto_import.cod_ie          = cRealVenda
           tt_movto_import.val_lancto      = bdvv_log_provisao.vl_provisao
           tt_movto_import.des_hist        = "Estorno Lancamento Provisao DVV"
           tt_movto_import.log_estorno     = YES
           tt_movto_import.num_lote_ger    = 0.

    RELEASE tt_movto_import.

    RUN esp/esfgl0001.p (INPUT-OUTPUT TABLE tt_movto_import,
                         OUTPUT TABLE tt_erro_import).


    IF NOT CAN-FIND (FIRST tt_erro_import) THEN DO:
        FIND FIRST tt_movto_import NO-ERROR.
            ASSIGN bdvv_log_provisao.situacao = 4
                   bdvv_log_provisao.num_lote_ctbl = tt_movto_import.num_lote_ger.
        ASSIGN i-seq = 1.
        FIND LAST bdvv_log_provisao_exec WHERE
                  bdvv_log_provisao_exec.num_id_provisao = bdvv_log_provisao.num_id_provisao NO-LOCK NO-ERROR.
        IF AVAIL bdvv_log_provisao_exec THEN
            ASSIGN i-seq = bdvv_log_provisao_exec.sequencia + 1.

        CREATE dvv_log_provisao_exec.
        ASSIGN dvv_log_provisao_exec.num_id_provisao = bdvv_log_provisao.num_id_provisao
               dvv_log_provisao_exec.sequencia       = i-seq
               dvv_log_provisao_exec.dt_atualiz      = TODAY
               dvv_log_provisao_exec.cod_usuario     = v_cod_usuar_corren
               dvv_log_provisao_exec.hr_atualiz      = replace(STRING(TIME,"HH:MM:SS"),":","")
               dvv_log_provisao_exec.mensagem        = "Lote Gerado com sucesso. Numero lote: " + STRING(tt_movto_import.num_lote_ger).
               dvv_log_provisao_exec.status_execucao = "Sucesso".


    END.
    ELSE DO:

        FIND FIRST tt_erro_import NO-ERROR.

        ASSIGN i-seq = 1.
        FIND LAST bdvv_log_provisao_exec WHERE
                  bdvv_log_provisao_exec.num_id_provisao = bdvv_log_provisao.num_id_provisao NO-LOCK NO-ERROR.
        IF AVAIL bdvv_log_provisao_exec THEN
            ASSIGN i-seq = bdvv_log_provisao_exec.sequencia + 1.

        CREATE dvv_log_provisao_exec.
        ASSIGN dvv_log_provisao_exec.num_id_provisao = bdvv_log_provisao.num_id_provisao
               dvv_log_provisao_exec.sequencia       = i-seq
               dvv_log_provisao_exec.dt_atualiz      = TODAY
               dvv_log_provisao_exec.cod_usuario     = v_cod_usuar_corren
               dvv_log_provisao_exec.hr_atualiz      = replace(STRING(TIME,"HH:MM:SS"),":","")
               dvv_log_provisao_exec.mensagem        = string(tt_erro_import.cd-erro) + " - " + tt_erro_import.mensagem + " - " + tt_erro_import.ajuda
               dvv_log_provisao_exec.status_execucao = "Erro".

    END.
    RELEASE dvv_log_provisao_exec.


END PROCEDURE.

PROCEDURE pi-gera-dex-consig:
    FOR EACH tt-processo-exp-consig WHERE
             tt-processo-exp-consig.cod-estabel <> "002":
    
        BLOCK_dexMovto:
        for each  dex_movto no-lock
              where dex_movto.cod_estabel = tt-processo-exp-consig.cod-estabel
              AND   dex_movto.nr_pedido   = tt-processo-exp-consig.nr-proc-exp
              and   substring(dex_movto.char_2,1,1) = "1"    /*gera provis∆o somente das despesas provisionads no programa dex0050*/
              and   dex_movto.valor <> ?
              AND   dex_movto.valor <> 0,
            FIRST dex_tp_despesa no-lock
              WHERE dex_tp_despesa.codigo = dex_movto.tp_despesa
              and   dex_tp_despesa.LOG_PROVIS_CONTABILIDADE = yes:

            FOR EACH estabelec no-lock
                where estabelec.cod-estabel <> "999",
                each b-dex_movto
                  WHERE b-dex_movto.cod_estabel = estabelec.cod-estabel
                    AND b-dex_movto.nr_processo = tt-processo-exp-consig.nr-proc-exp
                    AND b-dex_movto.tp_despesa = dex_movto.tp_despesa NO-LOCK:
                IF (b-dex_movto.data_atualiz <> ? AND b-dex_movto.data_atualiz <= tt-param.dt-movto-fim) AND
                   (decimal(substring(b-dex_movto.char_1,11,10)) <> 0 AND decimal(substring(b-dex_movto.char_1,11,10)) <> ?) THEN
                    NEXT BLOCK_dexMovto.
            END.

            IF dex_tp_despesa.cod_estabel_prov = "" OR dex_tp_despesa.cod_estabel_prov = ? THEN DO:
                CREATE tt-erro.
                ASSIGN tt-erro.cod-erro = 17006
                       tt-erro.mensagem = "Estabel Provis∆o (CONSIG) n∆o informado para a despesa " + string(dex_movto.tp_despesa)  .
                NEXT BLOCK_dexMovto.
            END.

            FIND FIRST es_grp_dex_desp WHERE
                       es_grp_dex_desp.cod_desp = dex_movto.tp_despesa NO-LOCK NO-ERROR.

            IF NOT AVAIL es_grp_dex_desp THEN DO:
                CREATE tt-erro.
                ASSIGN tt-erro.cod-erro = 17006
                       tt-erro.mensagem = "Despesa " + string(dex_movto.tp_despesa) + "n∆o cadastrada em nenhum grupo de despesa (dex0011) - CONSIG " .
                NEXT BLOCK_dexMovto.
            END.

            FIND es_grp_dex WHERE
                 es_grp_dex.cod_grupo = es_grp_dex_desp.cod_grupo NO-LOCK NO-ERROR.
            IF NOT AVAIL es_grp_dex THEN DO:
                CREATE tt-erro.
                ASSIGN tt-erro.cod-erro = 17006
                       tt-erro.mensagem = "Grupo de despesa n∆o cadastrada - (dex0011) " .
                NEXT BLOCK_dexMovto.
            END.

            IF es_grp_dex.cod_cta_ctbl = "" OR es_grp_dex.cod_cta_ctbl = ? THEN DO:
                CREATE tt-erro.
                ASSIGN tt-erro.cod-erro = 17006
                       tt-erro.mensagem = "Nao informado a conta contabil para o grupo de despesa " + string(es_grp_dex_desp.cod_grupo) + " - (dex0011) - CONSIG " .
                NEXT BLOCK_dexMovto.
            END.

            /* Segregaá∆o EUA */
            /* fazer a busca do pais pelo pais do fornecedor da despesa */
            ASSIGN c-unid-negoc = "000".

            FOR FIRST proc-ped-venda WHERE
                      proc-ped-venda.cod-estabel = dex_movto.cod_estabel  AND
                      proc-ped-venda.nr-proc-exp = dex_movto.nr_processo NO-LOCK:
                FOR FIRST proc-ped-ent WHERE
                          proc-ped-ent.cod-estabel = proc-ped-venda.cod-estabel AND
                          proc-ped-ent.nr-proc-exp = proc-ped-venda.nr-proc-exp NO-LOCK:

                    FOR FIRST ped-item OF proc-ped-ent NO-LOCK:
                        ASSIGN c-unid-negoc = ped-item.cod-unid-negoc.
                    END.
                END.
            END.
            IF NOT AVAIL proc-ped-venda THEN DO:
                FOR FIRST proc-ped-venda WHERE
                          proc-ped-venda.cod-estabel <> "999" AND
                          proc-ped-venda.nr-proc-exp = dex_movto.nr_processo NO-LOCK:
                    FOR FIRST proc-ped-ent WHERE
                              proc-ped-ent.cod-estabel = proc-ped-venda.cod-estabel AND
                              proc-ped-ent.nr-proc-exp = proc-ped-venda.nr-proc-exp NO-LOCK:

                        FOR FIRST ped-item OF proc-ped-ent NO-LOCK:
                            ASSIGN c-unid-negoc = ped-item.cod-unid-negoc.
                        END.
                    END.
                END.
                IF NOT AVAIL proc-ped-venda THEN DO: 
                    FOR FIRST processo-exp WHERE
                              processo-exp.cod-estabel = tt-processo-exp-consig.cod-estabel AND
                              processo-exp.nr-proc-exp = tt-processo-exp-consig.nr-proc-exp NO-LOCK:
                        FOR FIRST bbbemitente WHERE
                                  bbbemitente.cod-emitente = processo-exp.cod-emitente NO-LOCK:
                            FOR FIRST ped-venda WHERE
                                      ped-venda.nome-abrev = bbbemitente.nome-abrev AND
                                      ped-venda.nr-pedcli  = dex_movto.nr_processo NO-LOCK:
                                FOR FIRST ped-item OF ped-venda NO-LOCK:
                                   ASSIGN c-unid-negoc = ped-item.cod-unid-negoc.
                                END.
                            END.
                        END.

                    END.
                END.
            END.

            FIND FIRST processo-exp NO-LOCK 
                WHERE processo-exp.cod-estabel = tt-processo-exp-consig.cod-estabel 
                  AND processo-exp.nr-proc-exp = tt-processo-exp-consig.nr-proc-exp NO-ERROR.
            IF AVAIL processo-exp THEN DO:

                FIND FIRST emitente NO-LOCK
                    WHERE emitente.cod-emitente = processo-exp.cod-emitente NO-ERROR.
                IF AVAIL emitente THEN DO:

                    FIND FIRST mgcad.pais NO-LOCK
                        WHERE pais.nome-pais = emitente.pais NO-ERROR.
                    IF AVAIL pais and pais.nome-pais = "EUA" THEN DO:
            
                        FIND FIRST es-pais-unid-negoc NO-LOCK
                            WHERE es-pais-unid-negoc.cod-pais = pais.cod-pais NO-ERROR.
                        IF AVAIL es-pais-unid-negoc THEN DO:
                            ASSIGN c-unid-negoc = es-pais-unid-negoc.cod-unid-negoc.
                        END.
                    END.
                END.
                
                RUN pi-cria-dvv_log_provisao (INPUT dex_tp_despesa.cod_estabel_prov,
                                              INPUT tt-processo-exp-consig.nr-proc-exp,
                                              INPUT TODAY,
                                              INPUT "DEX",
                                              INPUT es_grp_dex.cod_cta_ctbl,      /*DEBITO*/
                                              INPUT dvv_param.cod_cta_provis_dex, /*CREDITO*/
                                              INPUT dex_movto.tp_despesa,
                                              INPUT dex_movto.valor,
                                              INPUT "",     /*centro de custo*/
                                              INPUT c-unid-negoc). /*unidade negocio*/
            END.
        
        END.
    END.

end procedure.






