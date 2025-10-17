//CURRENT-LANGUAGE = CURRENT-LANGUAGE.

/*****************************************************************************
    PROGRAMA..: dvv0199rp.p
    OBJETIVO..: Relat¢rio de DVV
    AUTOR.....: Paulo Roberto Barth NewTechs
    DATA......: 04/06/2013
    data: 07/12/2021 - Frank. Alterado a busca da tabela emitente para buscar 
                              pelo codigo quando n∆o encontrar pelo nome-abrev.
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

{include/i-prgvrs.i dvv0199RP "2.06.00.003"}
{esp/Moedas.i}

{dvv/dvv0199.i} /* Definiá‰es Funá‰es */
    DEF TEMP-TABLE tt_estab NO-UNDO
        FIELD cod_estab LIKE estabelecimento.cod_estab.

//MESSAGE 4 VIEW-AS ALERT-BOX.        
        
DEF TEMP-TABLE tt-valores NO-UNDO
    FIELD cod-estabel LIKE processo-exp.cod-estabel
    FIELD nr-proc-exp LIKE processo-exp.nr-proc-exp
    FIELD cod-itiner  LIKE processo-exp.cod-itiner
    FIELD sigla-moeda LIKE moeda.sigla
    FIELD vl-tot-liq     AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99"
    FIELD vl-tot-desp    AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99"
    FIELD vl-incoterm    AS DECIMAL FORMAT ">>,>>>,>>>,>>9.99"
    FIELD peso-liq-tot   AS DECIMAL FORMAT ">>>,>>>,>>9.9999"
    FIELD peso-bruto-tot AS DECIMAL FORMAT ">>>,>>>,>>9.9999"
    FIELD volume-tot     AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD desp-desc      AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    INDEX pk_valor IS UNIQUE PRIMARY
        cod-estabel
        nr-proc-exp
    ASCENDING.

DEFINE TEMP-TABLE tt-acumula NO-UNDO
    FIELD cod-estabel      AS CHAR FORMAT "X(05)"
    FIELD conta            AS CHAR FORMAT "X(13)"
    FIELD natureza         AS CHAR FORMAT "X(02)"
    FIELD valor-base       AS DECIMAL
    FIELD valor-rat        AS DECIMAL.

DEF TEMP-TABLE tt-movto-estoq-trans NO-UNDO                         
    FIELD cod-cta-ctbl AS CHAR
    FIELD nro-docto    AS CHAR
    FIELD nr-trans     AS CHAR.

/*---- DEINICAO DE PARAMETROS DO PROGRAMA -----*/
define input param raw-param as raw no-undo.
define input param table for tt-raw-digita.

DEF VAR h-acomp     AS HANDLE   NO-UNDO.
DEF VAR de-aliq-imposto AS DECIMAL NO-UNDO.
DEF VAR l-taxado AS LOGICAL NO-UNDO.
DEF VAR i-moeda AS INTEGER NO-UNDO.
DEF VAR d-vl-prev AS DECIMAL NO-UNDO.
DEF VAR d-vl-real AS DECIMAL NO-UNDO.

DEFINE VARIABLE d-total-qt AS DECIMAL     NO-UNDO.

def var v_cod_indic_econ_apres           as character       no-undo. /*local*/
def var v_cod_indic_econ_base            as character       no-undo. /*local*/
def var v_data as date no-undo.
def var v_cod_return                     as character       no-undo. /*local*/
DEF VAR l-troca AS LOGICAL NO-UNDO. 

DEF VAR de-val-doc AS DEC NO-UNDO.
DEF VAR de-val-mov AS DEC NO-UNDO.

DEF VAR l-log AS LOGICAL NO-UNDO.

DEF VAR c1-doc-origem AS CHAR NO-UNDO.
DEF VAR d1-doc-origem AS CHAR NO-UNDO.

DEFINE VARIABLE c-cod-estabel-br AS CHARACTER   NO-UNDO INITIAL "001".

DEF VAR v_num_lote            LIKE item_lancto_ctbl.num_lote_ctbl              NO-UNDO.
DEF VAR v_num_lanc            LIKE item_lancto_ctbl.num_lancto_ctbl            NO-UNDO.
DEF VAR v_num_seq             LIKE item_lancto_ctbl.num_seq_lancto_ctbl        NO-UNDO.
def var c-num_lote_ctbl       like  aprop_lancto_ctbl.num_lote_ctbl       no-undo.
def var c-num_lancto_ctbl     like  aprop_lancto_ctbl.num_lancto_ctbl     no-undo.
def var c-num_seq_lancto_ctbl like  aprop_lancto_ctbl.num_seq_lancto_ctbl no-undo.

DEF BUFFER bitem_lancto_ctbl FOR item_lancto_ctbl.
def buffer bmovto-estoq for movto-estoq.

DEF VAR c-chave1 AS CHAR NO-UNDO.

DEF VAR c-moeda AS CHAR NO-UNDO.
def var c-esp-docto    as char extent 39 init [
    "ACA" , "ACT" , "NU1" , "DD" ,  "DEV" , "DIV" , "DRM" ,
    "EAC" , "EGF" , "BEM" , "NU2",  "NU3" , "NU4" , "ICM" ,
    "INV" , "IPL" , "MOB" , "NC" ,  "NF"  , "NFD" , "NFE" ,
    "NFS" , "NFT" , "NU5" , "REF",  "RCS" , "RDD" , "REQ" ,
    "RFS" , "RM"  , "RFS" , "RM" ,  "RRQ" , "STR" , "TRA" ,
    "ZZZ" , "SOB" , "EDD" , "VAR" ].

DEF BUFFER bdvv_log_provisao_estorno FOR dvv_log_provisao.
DEF BUFFER bdvv_log_provisao FOR dvv_log_provisao.
DEF BUFFER b-dvv-movto FOR dvv_movto.
DEF BUFFER b-emit-fornec FOR emitente.
DEF BUFFER b-tt-provisao FOR tt-provisao.
DEF BUFFER b-tt-rel-item FOR tt-rel-item.
DEF BUFFER b-item         FOR ITEM.
DEF BUFFER b-processo-exp FOR processo-exp.

DEF TEMP-TABLE tt-rel-item-dif LIKE tt-rel-item 
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-docs NO-UNDO
    FIELD c-doc AS CHAR
    FIELD i-seq AS INT
    FIELD de-val-1 AS DEC
    FIELD de-val-2 AS DEC
    INDEX id IS PRIMARY
       c-doc
       i-seq.


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


ASSIGN l-log = NO.

IF l-log THEN DO:
   // MESSAGE "c:\temp\dvv0199log.txt 2"
   //     VIEW-AS ALERT-BOX INFO BUTTONS OK.
    OUTPUT TO c:\temp\dvv0199log.txt.
    OUTPUT CLOSE.
END.


//MESSAGE "1dentro" VIEW-AS ALERT-BOX.

DEF BUFFER cliente FOR ems.cliente.

{include/i-rpvar.i}

create tt-param.
raw-transfer raw-param to tt-param.

FIND FIRST param-global NO-LOCK NO-ERROR.

FIND FIRST mgcad.empresa NO-LOCK WHERE empresa.ep-codigo = param-global.empresa-prin.

assign c-programa     = "dvv0199"
       c-versao       = "2.06"
       c-revisao      = ".00.312"
       c-empresa      = empresa.razao
       c-sistema      = "Espec°fico"
       c-titulo-relat = "Relat¢rio de DVV"
       c-rodape       = "DATASUL - " + c-sistema + " - " + c-prg-obj + " - V:" + c-prg-vrs
       c-rodape       = fill("-", 241 - length(c-rodape)) + c-rodape.

{include/i-rpout.i}

/*---- DEFINICAO DE FRAMES -----*/
FORM HEADER
     FILL("-", 241) FORMAT "x(241)" SKIP
     c-empresa c-titulo-relat   AT 95  "Folha: "  AT 231 PAGE-NUMBER FORMAT ">>>9" SKIP
     FILL("-", 219) + " " + STRING(TODAY,"99/99/9999") + " - " + STRING(TIME, "hh:mm:ss") FORMAT "x(241)"
     SKIP(1)
     WITH FRAME f-cabec WIDTH 250 NO-LABELS NO-BOX PAGE-TOP.

FORM HEADER
     c-rodape FORMAT "x(241)"
     WITH STREAM-IO WIDTH 250 NO-LABELS NO-BOX PAGE-BOTTOM FRAME f-rodape.

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp("Processando relat¢rio").

RUN dvv/bo-dvv-movto-det.p  PERSISTENT SET h-bo-dvv-movto-det.

RUN pi-executar.

DELETE PROCEDURE h-bo-dvv-movto-det.

{include/i-rpclo.i}

run pi-finalizar in h-acomp.

/* *************** Procedure Definitions - Begin *************** */
PROCEDURE pi-executar:
    DEF VAR d AS DECIMAL.        
    DEF VAR d1 AS DECIMAL.
    DEF VAR h-dvv0199-api AS HANDLE NO-UNDO.
    DEF VAR c-nome-ab-reg AS CHAR   NO-UNDO.

    DEFINE VARIABLE h-bocx185     AS HANDLE  NO-UNDO.
    DEF VAR de-val-doc-aux AS DEC NO-UNDO.
    def var v-dat as date no-undo.

    IF tt-param.l-imprime-bi THEN DO:

        RUN pi-imprime-bi.

    END.
    ELSE DO:
    
        IF tt-param.l-fech-previo THEN DO:
            RUN pi-monta-fech-previo.
        END.

        IF tt-param.l-realizacao-dvv-me THEN DO:
    
            RUN pi-monta-dvv-me.
    
        END.
        
        IF l-log THEN DO:
            OUTPUT TO c:\temp\dvvlog.txt APPEND.
            FOR EACH tt-rel
                WHERE INDEX(tt-rel.doc-origem,"FM1408") > 0:
                PUT UNFORMATTED " pi-monta-dvv-me tt-rel.doc-origem " tt-rel.doc-origem " tt-rel.vl-real " tt-rel.vl-real SKIP.
            END.
            FOR EACH tt-rel-total
                WHERE INDEX(tt-rel-total.doc-origem,"FM1408") > 0:
                PUT UNFORMATTED " pi-monta-dvv-me tt-rel-total.doc-origem " tt-rel-total.doc-origem " tt-rel-total.vl-real " tt-rel-total.vl-real SKIP.
            END.      
            OUTPUT CLOSE.
        END.
    
        IF tt-param.l-realizacao-dvv-mi THEN DO:
    
            RUN pi-monta-dvv-mi.
    
        END.
    
        /* Aba Provis∆o */
        IF tt-param.l-provisao-dvv-me OR tt-param.l-provisao-dvv-mi OR tt-param.l-previa-provisoes THEN DO:
    
            run pi-acompanhar in h-acomp("Buscando Provis∆o.").
    
            IF tt-param.l-provisao-dvv-me OR tt-param.l-provisao-dvv-mi THEN
                RUN pi-monta-provisao.

            IF tt-param.l-previa-provisoes THEN
                RUN pi-monta-previa-provisao.
    
            FOR EACH tt-provisao:
                CREATE tt-rel.
                BUFFER-COPY tt-provisao TO tt-rel.
                RUN pi-estab-br (INPUT tt-rel.nome-abrev, INPUT tt-rel.num-processo).
                /* ASSIGN tt-rel.cod-estab-orig = tt-rel.cod-estabel. // todo */ //NT Carlos Jr - regra substituida 5/4/24
                IF tt-rel.cod-estabel <> "002" then ASSIGN tt-rel.cod-estab-orig = tt-rel.cod-estabel.
                ELSE DO:
                    RUN pi-estab-br(INPUT tt-rel.nome-abrev, INPUT tt-rel.num-processo).
                    assign tt-rel.cod-estab-orig = c-cod-estabel-br.
                END.
            END.
    
        END.
        
        RUN pi-rateio-despesa.
    
        IF l-log THEN DO:
            OUTPUT TO c:\temp\dvvlog.txt APPEND.
            FOR EACH tt-rel
                WHERE INDEX(tt-rel.doc-origem,"FM1408") > 0:
                PUT UNFORMATTED "AA pi-rateio-despesa tt-rel.doc-origem " tt-rel.doc-origem " tt-rel.vl-real " tt-rel.vl-real SKIP.
            END.
            FOR EACH tt-rel-total
                WHERE INDEX(tt-rel-total.doc-origem,"FM1408") > 0:
                PUT UNFORMATTED "BB pi-monta-dvv-me tt-rel-total.doc-origem " tt-rel-total.doc-origem " tt-rel-total.vl-real " tt-rel-total.vl-real SKIP.
            END.      
            OUTPUT CLOSE.
        END.
    
        RUN pi-quebra-item.
    
        RUN pi-ger-remessa-estoque.
    
        RUN pi-ger-rtnrast.
    
        IF l-log THEN DO:
            OUTPUT TO c:\temp\dvvlog.txt APPEND.
            FOR EACH tt-rel
                WHERE INDEX(tt-rel.doc-origem,"FM1408") > 0:
                PUT "CC pi-quebra-item tt-rel.doc-origem " tt-rel.doc-origem " tt-rel.vl-real " tt-rel.vl-real SKIP.
            END.
            OUTPUT CLOSE.
        END.
    
    
    END.

    DEF VAR c-doc-origem AS CHAR NO-UNDO.

    IF CAN-FIND(FIRST tt-rel-item) THEN DO:
    
        /* 1. Imprimindo relat¢rio no arquivo texto. */
        ASSIGN i-lin = 2.
        ASSIGN c-arq-imp = SESSION:TEMP-DIRECTORY + "dvv0199.txt".
        OUTPUT TO VALUE(c-arq-imp) NO-CONVERT.         

        EMPTY TEMP-TABLE tt-grp-lancto.

        EMPTY TEMP-TABLE tt-rel-item-dif.

        /*
        ** Ajuste realizado 
        */

        FOR EACH tt-rel-item
            WHERE (tt-rel-item.movto = "Realizado" 
                OR (tt-rel-item.movto = "Previsto" AND NOT tt-rel-item.classif BEGINS "P")) // as origens de prÇvia de provis∆o n∆o requerem ajustes.
            BY tt-rel-item.num-lote
            BY tt-rel-item.num-lancto
            BY tt-rel-item.num-seq-lancto
            BY tt-rel-item.tipo-col       DESC
            BY tt-rel-item.seq-class
            //BY tt-rel-item.doc-origem
            BY tt-rel-item.it-codigo /*
            BY tt-rel-item.doc-origem*/:

            ASSIGN de-val-doc-aux = tt-rel-item.val-doc.

            IF c-doc-origem <> "" AND c-doc-origem = tt-rel-item.doc-origem AND tt-rel-item.tipo-col <> "T" THEN
                ASSIGN de-val-doc-aux = 0.

            run pi-acompanhar in h-acomp("Imprimindo relat¢rio. Data/Lote: " + string(tt-rel-item.dta-ctbz) + "/" + tt-rel-item.it-codigo).

            IF tt-rel-item.tipo-col <> "T" THEN DO:

                FIND FIRST tt-rel-item-dif WHERE
                           tt-rel-item-dif.num-lote       = tt-rel-item.num-lote       AND
                           tt-rel-item-dif.num-lancto     = tt-rel-item.num-lancto     AND
                           tt-rel-item-dif.num-seq-lancto = tt-rel-item.num-seq-lancto AND
                           tt-rel-item-dif.doc-origem     = tt-rel-item.doc-origem NO-ERROR.

                IF NOT AVAIL tt-rel-item-dif THEN DO:
                    CREATE tt-rel-item-dif.
                    ASSIGN tt-rel-item-dif.tipo-col         = tt-rel-item.tipo-col   
                           tt-rel-item-dif.movto            = tt-rel-item.movto      
                           tt-rel-item-dif.cod-estabel      = tt-rel-item.cod-estabel
                           /* tt-rel-item.cod-estab-orig       = tt-rel-item.cod-estabel // todo */  //NT Carlos Jr - regra substituida 5/4/24
                           tt-rel-item-dif.conta            = tt-rel-item.conta      
                           tt-rel-item-dif.desc-conta       = tt-rel-item.desc-conta 
                           tt-rel-item-dif.modulo           = tt-rel-item.modulo     
                           tt-rel-item-dif.dta-ctbz         = tt-rel-item.dta-ctbz   
                           tt-rel-item-dif.cod-estabel      = tt-rel-item.cod-estabel    
                           tt-rel-item-dif.num-lote         = tt-rel-item.num-lote       
                           tt-rel-item-dif.num-lancto       = tt-rel-item.num-lancto     
                           tt-rel-item-dif.num-seq-lancto   = tt-rel-item.num-seq-lancto 
                           tt-rel-item-dif.doc-origem       = tt-rel-item.doc-origem 
                           tt-rel-item-dif.dt-lote          = tt-rel-item.dt-lote   
                           tt-rel-item-dif.user-lote        = tt-rel-item.user-lote 
                           tt-rel-item-dif.mov              = tt-rel-item.mov       
                           tt-rel-item-dif.moeda            = tt-rel-item.moeda     
                           tt-rel-item-dif.l-rastr-desp     = NO
                           tt-rel-item-dif.tp-origem        = tt-rel-item.tp-origem
                           tt-rel-item-dif.trans-ap         = tt-rel-item.trans-ap
                           tt-rel-item-dif.cdn-fornec       = tt-rel-item.cdn-fornec
                           tt-rel-item-dif.nome-forn        = tt-rel-item.nome-forn .                   

                END.
                
                /* ASSIGN tt-rel-item-dif.cod-estab-orig = tt-rel-item-dif.cod-estabel. // todo */ //NT Carlos Jr - regra substituida 5/4/24
                IF tt-rel-item-dif.cod-estabel <> "002" then ASSIGN tt-rel-item-dif.cod-estab-orig = tt-rel-item-dif.cod-estabel.
                ELSE DO:
                    RUN pi-estab-br(INPUT tt-rel-item-dif.nome-abrev, INPUT tt-rel-item-dif.num-processo).
                    assign tt-rel-item-dif.cod-estab-orig = c-cod-estabel-br.
                END.

                ASSIGN tt-rel-item-dif.vl-real = tt-rel-item-dif.vl-real + de-val-doc-aux - tt-rel-item.vl-real
                       tt-rel-item-dif.r-rowid = ROWID(tt-rel-item).

                ASSIGN de-val-doc = 0.
            END.
            ASSIGN c-doc-origem = tt-rel-item.doc-origem.
        END.

        RUN dvv/dvv0199-api.p PERSISTENT SET h-dvv0199-api.

        /*
        ** Ajustes tt-rel-item
        */
        IF NOT tt-param.l-imprime-bi THEN
            FOR EACH tt-rel-item
                BY tt-rel-item.dta-ctbz:
    
                run pi-acompanhar in h-acomp("Ajuste: " + string(tt-rel-item.dta-ctbz) + "/" + tt-rel-item.cod-estabel + "/" + tt-rel-item.conta) .

                IF tt-rel-item.classif <> "RtDvvDex" THEN DO:

                    FIND cta_ctbl NO-LOCK WHERE
                        cta_ctbl.cod_plano_cta_ctbl = "Nitro" AND
                        cta_ctbl.cod_cta_ctbl      = tt-rel-item.conta NO-ERROR.
                    IF AVAIL cta_ctbl THEN
                        ASSIGN tt-rel-item.desc-conta = cta_ctbl.des_tit_ctbl.  

                END.
        

                IF tt-rel-item.l-rastr-desp THEN DO:

                    IF tt-rel-item.classif <> "RtDvvDex" AND tt-rel-item.classif <> "RtNRastr" AND tt-rel-item.classif <> "RtNRastrP" THEN DO:
    
                        IF tt-rel-item.it-codigo <> "" THEN DO:
    
                            RUN pi-retorna-item IN h-dvv0199-api (
                                INPUT tt-rel-item.it-codigo,
                                OUTPUT tt-rel-item.familia,
                                OUTPUT tt-rel-item.classe-item,
                                OUTPUT tt-rel-item.grupo-classe).
    
                        END.
    
                        IF tt-rel-item.num-processo <> "" AND tt-rel-item.classif <> "DVV MI" AND tt-rel-item.classif <> "PDVV MI" THEN DO:
    
                            RUN pi-retorna-regiao-segmento IN h-dvv0199-api (
                                INPUT  tt-rel-item.cod-estabel,
                                INPUT  tt-rel-item.num-processo,
                                OUTPUT tt-rel-item.regiao,
                                OUTPUT tt-rel-item.segmento,
                                OUTPUT tt-rel-item.tip-oper-exp,
                                OUTPUT tt-rel-item.cod-depos,
                                OUTPUT c-nome-ab-reg).
    
                        END.
    
                    END.
                END.
            END.

        //converte valor em moeda estrangeira

            FIND FIRST moeda NO-LOCK WHERE moeda.mo-codigo = tt-param.i-mo-codigo NO-ERROR.
            IF AVAIL moeda THEN ASSIGN c-moeda = moeda.descricao.
            ELSE ASSIGN c-moeda = "".


            FOR EACH tt-rel-item NO-LOCK:
                IF tt-param.de-cotacao <> 0 THEN DO:
                    ASSIGN tt-rel-item.val-lancto-2 = tt-rel-item.val-lancto / tt-param.de-cotacao
                           tt-rel-item.val-doc-2 = tt-rel-item.val-doc / tt-param.de-cotacao
                           tt-rel-item.vl-real-2 = tt-rel-item.vl-real / tt-param.de-cotacao.
                END.
                ELSE DO:
                    ASSIGN tt-rel-item.val-lancto-2 = tt-rel-item.val-lancto 
                          tt-rel-item.val-doc-2 = tt-rel-item.val-doc 
                          tt-rel-item.vl-real-2 = tt-rel-item.vl-real .
                END.

                FIND FIRST tt-docs NO-LOCK
                    WHERE tt-docs.c-doc = tt-rel-item.doc-origem
                    AND tt-docs.i-seq = tt-rel-item.num-seq-lancto NO-ERROR.
                IF AVAIL tt-docs THEN DO:
                    ASSIGN tt-docs.de-val-2 = tt-docs.de-val-2 + tt-rel-item.vl-real-2.
                END.
                ELSE DO:
                    CREATE tt-docs.
                    ASSIGN tt-docs.c-doc = tt-rel-item.doc-origem 
                           tt-docs.i-seq = tt-rel-item.num-seq-lancto
                           tt-docs.de-val-2 = tt-docs.de-val-2 + tt-rel-item.vl-real-2.
                END.
            END.

            FOR EACH tt-docs NO-LOCK:
                FIND FIRST tt-rel-item NO-LOCK
                    WHERE tt-rel-item.doc-origem = tt-docs.c-doc
                    AND tt-rel-item.num-seq-lancto = tt-docs.i-seq
                    AND tt-rel-item.val-doc-2 <> 0 NO-ERROR.
                IF AVAIL tt-rel-item THEN
                    ASSIGN tt-docs.de-val-1 = tt-rel-item.val-doc-2.
                IF tt-docs.de-val-1 <> tt-docs.de-val-2 THEN DO:
                    FOR LAST tt-rel-item NO-LOCK
                        WHERE tt-rel-item.doc-origem = tt-docs.c-doc
                        AND tt-rel-item.num-seq-lancto = tt-docs.i-seq
                        BREAK BY tt-rel-item.num-lote
                        BY tt-rel-item.vl-real-2:
                        ASSIGN tt-rel-item.vl-real-2 = tt-rel-item.vl-real-2 - (de-val-2 - de-val-1). 
                    END.
                END.
            END.



        /*
        ** Impressao tt-rel-item
        */
        FOR EACH tt-rel-item
            BREAK BY tt-rel-item.num-lote
            BY tt-rel-item.movto
            BY tt-rel-item.num-lancto
            BY tt-rel-item.num-seq-lancto
            BY tt-rel-item.tipo-col DESC
            BY tt-rel-item.seq-class
            //BY tt-rel-item.doc-origem
            BY tt-rel-item.it-codigo:

            
            run pi-acompanhar in h-acomp("Imprimindo Data/Lote: " + string(tt-rel-item.dta-ctbz) + "/" + STRING(tt-rel-item.num-lancto)).

            /* Projeto FUI - n∆o atualizar valor do dccumento */
            IF SUBSTRING(tt-rel-item.movto,1,2) <> "RT" THEN DO:

                IF  c-doc-origem <> "" 
                AND c-doc-origem = tt-rel-item.doc-origem 
                AND tt-rel-item.tipo-col <> "T" THEN
                    ASSIGN tt-rel-item.val-doc = 0
                           tt-rel-item.val-doc-2 = 0.

            END.
                
            IF  tt-param.l-val-neg
            AND tt-param.l-imprime-bi = NO
            AND tt-rel-item.mov = "CR" 
            AND tt-rel-item.movto <> "RtDVVDEX"
            AND tt-rel-item.movto <> "RtNRastP"
            AND tt-rel-item.movto <> "RtNRastr" THEN DO:

                ASSIGN tt-rel-item.val-lancto = tt-rel-item.val-lancto * -1
                       tt-rel-item.vl-real    = tt-rel-item.vl-real    * -1 
                       tt-rel-item.val-doc    = tt-rel-item.val-doc    * -1
                       tt-rel-item.val-lancto-2 = tt-rel-item.val-lancto-2 * -1
                       tt-rel-item.vl-real-2    = tt-rel-item.vl-real-2    * -1 
                       tt-rel-item.val-doc-2    = tt-rel-item.val-doc-2    * -1.

            END.
    
            ASSIGN i-lin = i-lin + 1.
    
            IF tt-rel-item.tipo-col = "T" THEN DO:
                CREATE tt-grp-lancto.
                ASSIGN tt-grp-lancto.lin-ini = i-lin.
            END.

            IF AVAIL tt-grp-lancto THEN
                ASSIGN tt-grp-lancto.lin-fim = i-lin.

            IF tt-rel-item.tipo-col = "T" THEN DO:
                /*ASSIGN tt-rel-item.val-doc = 0.*/
            END.
            ELSE DO:
                IF (tt-rel-item.movto = "Provisionado" 
                OR  tt-rel-item.movto  = "Estornado"
                OR  (tt-rel-item.movto  = "Previsto" AND tt-rel-item.classif BEGINS "P")) // PDEX, PDVV ME, PDVV MI - prÇvia das provis‰es.
                AND tt-rel-item.l-rastr-desp THEN DO:

                    ASSIGN tt-rel-item.val-doc   = tt-rel-item.vl-real
                           tt-rel-item.val-doc-2 = tt-rel-item.vl-real-2.
                END.
            END.

            IF  tt-rel-item.it-codigo <> "" 
            AND tt-rel-item.tipo-col  <> "T"  THEN DO:
                FIND b-ITEM
                    WHERE b-ITEM.it-codigo = tt-rel-item.it-codigo NO-LOCK NO-ERROR.
                FIND FIRST es_grupo_produto 
                     WHERE es_grupo_produto.it_codigo = b-item.it-codigo NO-LOCK NO-ERROR.
                IF NOT AVAIL es_grupo_produto THEN 
                    FIND FIRST es_grupo_produto 
                         WHERE es_grupo_produto.fm_codigo = b-ITEM.fm-cod-com NO-LOCK NO-ERROR.
            END.
            
            IF NOT tt-param.l-imprime-bi THEN DO:
                
            
            
            
                EXPORT DELIMITER ";"
                    tt-rel-item.tipo-col
                    tt-rel-item.movto
                    tt-rel-item.cod-estabel
                    tt-rel-item.cod-estab-orig
                    tt-rel-item.conta
                    tt-rel-item.desc-conta
                    tt-rel-item.modulo
                    tt-rel-item.dta-ctbz
                    string(tt-rel-item.cod-estabel + "/" + string(tt-rel-item.num-lote) + "/" + string(tt-rel-item.num-lancto) + "/" + string(tt-rel-item.num-seq-lancto))
                    tt-rel-item.dt-lote
                    tt-rel-item.user-lote
                    tt-rel-item.mov
                    tt-rel-item.moeda       
                    IF tt-rel-item.val-lancto = 0 THEN "" ELSE string(tt-rel-item.val-lancto)
                    IF tt-rel-item.val-doc    = 0 THEN "" ELSE string(tt-rel-item.val-doc)
                    IF tt-rel-item.vl-real    = 0 THEN "" ELSE string(tt-rel-item.vl-real)
                    fi-tp-doc-origem(tt-rel-item.tp-origem)
                    tt-rel-item.doc-origem
                    tt-rel-item.trans-ap
                    string(tt-rel-item.l-rastr-desp, "Sim/N∆o")
                    (IF tt-rel-item.classif = "DVV MI" OR tt-rel-item.classif = "PDVV MI" THEN tt-rel-item.nr-nota-fis ELSE tt-rel-item.num-processo)
                    tt-rel-item.dt-conf
                    tt-rel-item.cod-emitente
                    tt-rel-item.nome-abrev
                    tt-rel-item.cod-depos
                    tt-rel-item.regiao
                    tt-rel-item.segmento
                    tt-rel-item.it-codigo 
                    IF AVAIL es_grupo_produto AND tt-rel-item.it-codigo <> "" AND tt-rel-item.tipo-col  <> "T" THEN trim(es_grupo_produto.cod_grupo) + " - " + trim(es_grupo_produto.des_grupo) ELSE ""
                    tt-rel-item.familia
                    tt-rel-item.grupo-classe
                    tt-rel-item.classe-item
                    IF tt-rel-item.cdn-fornec = 0 THEN "" ELSE STRING(tt-rel-item.cdn-fornec)
                    tt-rel-item.nome-forn
                    tt-rel-item.classif
                    IF tt-rel-item.cod-desp = 0 THEN "" ELSE STRING(tt-rel-item.cod-desp)
                    tt-rel-item.des-desp
                    IF tt-rel-item.dt-real = ? THEN "" ELSE STRING(tt-rel-item.dt-real)
                    /*tt-rel-item.vl-prev-item    
                    tt-rel-item.vl-real-tot-item */
                    tt-rel-item.cod-grupo
                    tt-rel-item.desc-grup
                    tt-rel-item.cod-unid-negoc
                    tt-rel-item.vl-prev-item 
                    tt-rel-item.vl-base
                    tt-rel-item.tip-oper-exp 
                    tt-rel-item.regra-rateio
                    tt-rel-item.usuario-impl
                    IF tt-rel-item.val-lancto-2 = 0 THEN "" ELSE string(tt-rel-item.val-lancto-2)
                    IF tt-rel-item.val-doc-2 = 0 THEN "" ELSE string(tt-rel-item.val-doc-2)
                    IF tt-rel-item.vl-real-2 = 0 THEN "" ELSE string(tt-rel-item.vl-real-2)
                    IF tt-rel-item.qt-faturada = 0 THEN "" ELSE string(tt-rel-item.qt-faturada)
                    tt-rel-item.un-fatur
                    tt-rel-item.cod-unid-negoc-gerenc.

            END.
            ELSE DO:
                EXPORT DELIMITER ";"
                    tt-rel-item.tipo-col
                    tt-rel-item.movto
                    tt-rel-item.cod-estabel
                    tt-rel-item.cod-estab-orig
                    tt-rel-item.conta
                    tt-rel-item.desc-conta
                    tt-rel-item.modulo
                    tt-rel-item.dta-ctbz
                    string(tt-rel-item.cod-estabel + "/" + string(tt-rel-item.num-lote) + "/" + string(tt-rel-item.num-lancto) + "/" + string(tt-rel-item.num-seq-lancto))
                    tt-rel-item.dt-lote
                    tt-rel-item.user-lote
                    tt-rel-item.mov
                    tt-rel-item.moeda       
                    IF tt-rel-item.val-lancto = 0 THEN "" ELSE string(tt-rel-item.val-lancto)
                    IF tt-rel-item.val-doc    = 0 THEN "" ELSE string(tt-rel-item.val-doc)
                    IF tt-rel-item.vl-real    = 0 THEN "" ELSE string(tt-rel-item.vl-real)
                    fi-tp-doc-origem(tt-rel-item.tp-origem)
                    tt-rel-item.doc-origem
                    tt-rel-item.trans-ap
                    string(tt-rel-item.l-rastr-desp, "Sim/N∆o")
                    IF tt-rel-item.classif = "DVV MI" OR tt-rel-item.classif = "PDVV MI" THEN tt-rel-item.nr-nota-fis ELSE tt-rel-item.num-processo
                    tt-rel-item.dt-conf
                    tt-rel-item.cod-emitente
                    tt-rel-item.nome-abrev
                    tt-rel-item.cod-depos
                    tt-rel-item.regiao
                    tt-rel-item.segmento
                    tt-rel-item.it-codigo 
                    IF AVAIL es_grupo_produto AND tt-rel-item.it-codigo <> "" AND tt-rel-item.tipo-col  <> "T" THEN trim(es_grupo_produto.cod_grupo) + " - " + trim(es_grupo_produto.des_grupo) ELSE ""
                    tt-rel-item.familia
                    tt-rel-item.grupo-classe
                    tt-rel-item.classe-item
                    IF tt-rel-item.cdn-fornec = 0 THEN "" ELSE STRING(tt-rel-item.cdn-fornec)
                    tt-rel-item.nome-forn
                    tt-rel-item.classif
                    IF tt-rel-item.cod-desp = 0 THEN "" ELSE STRING(tt-rel-item.cod-desp)
                    tt-rel-item.des-desp
                    IF tt-rel-item.dt-real = ? THEN "" ELSE STRING(tt-rel-item.dt-real)
                    /*tt-rel-item.vl-prev-item    
                    tt-rel-item.vl-real-tot-item */
                    tt-rel-item.cod-grupo
                    tt-rel-item.desc-grup
                    tt-rel-item.cod-unid-negoc
                    tt-rel-item.vl-prev-item 
                    tt-rel-item.vl-base
                    tt-rel-item.tip-oper-exp 
                    tt-rel-item.regra-rateio
                    tt-rel-item.usuario-impl
                    IF tt-rel-item.val-lancto-2 = 0 THEN "" ELSE string(tt-rel-item.val-lancto-2)
                    IF tt-rel-item.val-doc-2 = 0 THEN "" ELSE string(tt-rel-item.val-doc-2)
                    IF tt-rel-item.vl-real-2 = 0 THEN "" ELSE string(tt-rel-item.vl-real-2)
                    //tt-rel-item.cod-unid-negoc-gerenc
                        .

            END.


            ASSIGN c-doc-origem = tt-rel-item.doc-origem.

            /*******************/
            FIND FIRST tt-rel-item-dif WHERE
                       tt-rel-item-dif.r-rowid = rowid(tt-rel-item) NO-ERROR.

            IF AVAIL tt-rel-item-dif AND NOT tt-param.l-imprime-bi
                THEN do:

                IF round(tt-rel-item-dif.vl-real,2) <> 0 THEN DO:

                    ASSIGN i-lin = i-lin + 1.

                    ASSIGN tt-grp-lancto.lin-fim = i-lin.

                    EXPORT DELIMITER ";"
                        tt-rel-item-dif.tipo-col                                                                                                                                  
                        tt-rel-item-dif.movto                                                                                                                                   
                        tt-rel-item-dif.cod-estabel  
                        tt-rel-item.cod-estab-orig
                        tt-rel-item-dif.conta                                                                                                                                     
                        tt-rel-item.desc-conta                                                                                                                                
                        tt-rel-item-dif.modulo                                                                                                                                    
                        tt-rel-item-dif.dta-ctbz                                                                                                                                  
                        string(tt-rel-item-dif.cod-estabel + "/" + string(tt-rel-item-dif.num-lote) + "/" + string(tt-rel-item-dif.num-lancto) + "/" + string(tt-rel-item-dif.num-seq-lancto))
                        tt-rel-item-dif.dt-lote                                                                                                                                   
                        tt-rel-item-dif.user-lote                                                                                                                                 
                        tt-rel-item-dif.mov                                                                                                                                       
                        tt-rel-item-dif.moeda       
                        ""
                        ""
                        IF tt-rel-item-dif.vl-real    = 0 THEN "" ELSE string(tt-rel-item-dif.vl-real)
                        fi-tp-doc-origem(tt-rel-item-dif.tp-origem)
                        tt-rel-item-dif.doc-origem
                        tt-rel-item-dif.trans-ap    
                        string(tt-rel-item-dif.l-rastr-desp,"Sim/N∆o")
                        ""
                        ""
                        ""
                        ""
                        ""
                        ""
                        ""
                        ""
                        ""
                        ""
                        ""
                        ""
                        tt-rel-item-dif.cdn-fornec  
                        tt-rel-item-dif.nome-forn 
                        tt-rel-item.classif    
                        tt-rel-item-dif.usuario-impl
                        "".

                END.

            END.
            /**********/
        END.

        DELETE PROCEDURE h-dvv0199-api.

        IF l-log  THEN DO:
        
            OUTPUT TO c:/temp/dvvfrank.txt.
            FOR EACH tt-rel-item.
                EXPORT DELIMITER ";" tt-rel-item. // EXCEPT r-rowid.
            END.
            OUTPUT CLOSE.

        END.

        OUTPUT CLOSE.

        /*
        ** Grava BI
        */
        IF tt-param.l-atualiza-bi THEN DO:
		RUN dvv/dvv0199-bi.p(INPUT NO, 
							 INPUT TABLE tt-rel-item, 
							 INPUT TABLE tt-rel-item-dif,
							 INPUT tt-param.dt-ctbl-ini,
							 INPUT tt-param.dt-ctbl-fim,
							 INPUT tt-param.l-fech-previo).
	END.

    END. 
                     
    /* 2. Criaá∆o do arquivo excel e finalizaá∆o do relat¢rio. */
    ASSIGN i-col = 0.
    RUN pi-cabecalho (INPUT 1).

    ASSIGN chImp = excelAppl:sheets:ITEM(1):QueryTables:Add("TEXT;" + c-arq-imp, excelAppl:sheets:ITEM(1):Range("A3")).
 
    ASSIGN chImp:Name = "Rel. Rastreabilidade"
           chImp:FieldNames = True
           chImp:RowNumbers = False
           chImp:FillAdjacentFormulas = False
           chImp:PreserveFormatting = True
           chImp:RefreshOnFileOpen = False
           chImp:RefreshStyle = 2
           chImp:SavePassword = False
           chImp:SaveData = True
           chImp:AdjustColumnWidth = True
           chImp:RefreshPeriod = 0
           chImp:TextFilePromptOnRefresh = False
           chImp:TextFilePlatform = 1252
           chImp:TextFileStartRow = 1
           chImp:TextFileParseType = 1
           chImp:TextFileTextQualifier = 1
           chImp:TextFileConsecutiveDelimiter = False
           chImp:TextFileTabDelimiter = False
           chImp:TextFileSemicolonDelimiter = True
           chImp:TextFileCommaDelimiter = False
           chImp:TextFileSpaceDelimiter = False
           chImp:TextFileTrailingMinusNumbers = True.
    
    chImp:Refresh() NO-ERROR.

    excelAppl:sheets:ITEM(1):range("A2:" + c-last-col + "2"):select.
    excelAppl:selection:font:bold = 1.
    excelAppl:selection:borders(9):linestyle = 1.

    excelAppl:sheets:ITEM(1):range("1:1"):EntireColumn:AutoFit().

    FOR EACH tt-grp-lancto:

        ASSIGN l-troca = NOT l-troca.

        excelAppl:Range("A" + STRING(tt-grp-lancto.lin-ini) + ":" + c-last-col + STRING(tt-grp-lancto.lin-fim)):SELECT.

        excelAppl:Selection:Interior:Pattern = 1.
        excelAppl:Selection:Interior:PatternColorIndex = -4105.
        IF l-troca THEN
            excelAppl:Selection:Interior:ThemeColor = 9.
        ELSE
            excelAppl:Selection:Interior:ThemeColor = 6.
        excelAppl:Selection:Interior:TintAndShade = 0.799981688894314.
        excelAppl:Selection:Interior:PatternTintAndShade = 0.

    END.

	excelAppl:Range("A1:A1"):SELECT.


	DEFINE VARIABLE cExcelDir   AS CHARACTER NO-UNDO INITIAL "C:\temp\".
	DEFINE VARIABLE cExcelFile  AS CHARACTER NO-UNDO.
	ASSIGN
	  cExcelFile = cExcelDir
				 + "dvv0199_"
				 + STRING(TODAY, "99999999") + "_"
				 + REPLACE(STRING(TIME, "HH:MM:SS"), ":", "")
				 + ".xls".

	OS-CREATE-DIR VALUE(cExcelDir) NO-ERROR.


	excelAppl:APPLICATION:DisplayAlerts = FALSE.


	excelAppl:ActiveWorkbook:SaveAs(cExcelFile).


	IF tt-param.destino = 3 THEN DO:
		excelAppl:VISIBLE = TRUE.
	END.
	ELSE DO:

		excelAppl:ActiveWorkbook:SaveAs(session:temp-dir + "dvv0199.xls").
		excelAppl:Workbooks:CLOSE().
		excelAppl:QUIT().
	END.

	RELEASE OBJECT excelAppl     NO-ERROR.
	RELEASE OBJECT chWorkbook    NO-ERROR.
	RELEASE OBJECT chWorksheet   NO-ERROR.

END PROCEDURE.

PROCEDURE pi-cabecalho:

    DEFINE INPUT PARAM p-aba AS INT NO-UNDO.

    IF p-aba = 1 THEN DO:
        CREATE "Excel.Application" excelAppl.
        
        excelAppl:visible = false.      
        
        chWorkbook = excelAppl:Workbooks:ADD().
        
        excelAppl:SheetsInNewWorkbook = 2.
    END.
                    
    ASSIGN i-lin = 1.

    excelAppl:Range("A1"):Value = "CONTABILIDADE".
    excelAppl:Range("A1:AV1"):SELECT.
    excelAppl:SELECTION:Merge().
    excelAppl:SELECTION:FONT:bold = TRUE.
    excelAppl:SELECTION:horizontalalignment = 3.
    excelAppl:selection:borders(9):linestyle = 1.
    excelAppl:selection:borders(2):linestyle = 1.
    /*
    excelAppl:Range("N1"):Value = "ORIGEM".
    excelAppl:Range("M1:P1"):SELECT.
    excelAppl:SELECTION:Merge().
    excelAppl:SELECTION:FONT:bold = TRUE.
    excelAppl:SELECTION:horizontalalignment = 3.
    excelAppl:selection:borders(9):linestyle = 1.
    excelAppl:selection:borders(2):linestyle = 1.
    
    
    excelAppl:Range("Q1"):Value = "DESPESA".
    excelAppl:Range("Q1:AS1"):SELECT.
    excelAppl:SELECTION:Merge().
    excelAppl:SELECTION:FONT:bold = TRUE.
    excelAppl:SELECTION:horizontalalignment = 3.
    excelAppl:selection:borders(9):linestyle = 1.
    excelAppl:selection:borders(2):linestyle = 1.
    */

    ASSIGN i-lin = 2.

    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Tipo".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Movto".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Estab".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Estab BR".
    excelAppl:Columns(fi-c()):NumberFormat = "@".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Conta Ctbl".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Desc Ctbl".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "M¢dulo".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Data Ctbz".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Lote Ctbl".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Dt Atlz Lote".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "User Lote Ctbl".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Mov".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Moeda".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Valor Lancto".
    excelAppl:Columns(fi-c()):NumberFormat = "#.##0,00".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Valor Docto".
    excelAppl:Columns(fi-c()):NumberFormat = "#.##0,00".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Vl Movto".
    excelAppl:Columns(fi-c()):NumberFormat = "#.##0,00".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Tp Origem".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Doc Origem".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Transaá∆o APB".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Rastr.".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "OC/NF".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Dt Receita".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Cod Cliente".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Nome Cliente".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Dep¢sito".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Regi∆o".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Segmento".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Item". 
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Grupo". 
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Familia". 
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Gr Classe". 
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Classe Item". 

    /* Dados do t°tulo APB. */

    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Cod Forn".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Nome Forn".
    /* Valores da despesa. */
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Classif".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Despesa".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Descriá∆o".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Dt. Movto".
    /*excelAppl:Range(fi-nc() + string(i-lin)):Value = "Vl.Previsto".
    excelAppl:Columns(fi-c()):NumberFormat = "#.##0,00".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Vl.Realizado".
    excelAppl:Columns(fi-c()):NumberFormat = "#.##0,00".*/
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Grupo".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Descriá∆o".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Unid. Negoc Fiscal".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Qtd Vendida".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Vl Base".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Tp Oper EXP".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Regra Rateio".
	excelAppl:Range(fi-nc() + string(i-lin)):Value = "User Implant".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Val Lancto " + c-moeda.
    excelAppl:Columns(fi-c()):NumberFormat = "#.##0,00".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Vl Docto " + c-moeda.
    excelAppl:Columns(fi-c()):NumberFormat = "#.##0,00".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Vl Movto " + c-moeda.
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Quantidade".
    excelAppl:Columns(fi-c()):NumberFormat = "#.##0".
    excelAppl:Range(fi-nc() + string(i-lin)):Value = "Medida".

    IF NOT tt-param.l-imprime-bi THEN DO:
        excelAppl:Columns(fi-c()):NumberFormat = "#.##0,00".
        excelAppl:Range(fi-nc() + string(i-lin)):Value = "Unid. Negoc Gerencial".

    END.

    ASSIGN c-last-col = fi-c().

END PROCEDURE.

procedure pi-dados-titulo-apb:

    DEF VAR c_processo AS CHAR NO-UNDO.
    DEF VAR v_tot_base AS DEC  NO-UNDO.
    DEF VAR v_tot_parc AS DEC  NO-UNDO.
    DEF VAR i-num-id-tit-ap AS INTEGER NO-UNDO.
    DEF VAR i-num-id-tit-ap-ant AS INTEGER NO-UNDO.

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
         and tit_ap.num_id_tit_ap   = movto_tit_ap.num_id_tit_ap 
         
         
         no-error.

        /* aqui */
        IF l-log THEN DO:
            OUTPUT TO c:\temp\dvvlog.txt APPEND.
            PUT "val_aprop_ctbl_ap.val_aprop_ctbl " val_aprop_ctbl_ap.val_aprop_ctbl SKIP
                "titulo " tit_ap.cod_tit_Ap " / " tit_Ap.num_id_tit_ap SKIP.
            OUTPUT CLOSE.
        END.

        IF CAN-FIND(FIRST tt-rel WHERE
                          tt-rel.cdn-fornec = tit_ap.cdn_fornecedor
                      AND tt-rel.cod-esp    = tit_ap.cod_espec_docto
                      AND tt-rel.cod-titulo = tit_ap.cod_tit_ap
                      AND tt-rel.parcela    = tit_ap.cod_parcela
                      AND tt-rel.trans-ap   = movto_tit_ap.ind_trans_ap
                      AND tt-rel.num-lote   = ITEM_lancto_ctbl.num_lote_ctbl
                      AND tt-rel.num-lancto = ITEM_lancto_ctbl.num_lancto_ctbl
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
                    IF l-log THEN DO:
                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                        PUT "Titulo Original " tit_ap.cod_tit_ap " " tit_ap.cod_espec_docto " parcela " tit_ap.cod_parcela  SKIP.
                        OUTPUT CLOSE.
                    END.                                                                
                END.
                ELSE
                    RUN pi-chave-tit_ap IN h-bo-dvv-movto-det (INPUT ROWID(tit_ap),
                                                               OUTPUT c-chave-refer).                                                               
                    
                    ASSIGN d1-doc-origem = "".
                    IF NUM-ENTRIES(c-chave-refer,"|") > 6 THEN DO:                    
                        ASSIGN d1-doc-origem = ENTRY(7,c-chave-refer,"|") NO-ERROR.
                        IF d1-doc-origem <> "" THEN
                            ASSIGN ENTRY(7,c-chave-refer,"|") = "".
                    END.
                    
                    IF l-log THEN DO:
                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                        PUT "Alteracao Origem c-chave-refer " c-chave-refer  SKIP.
                        OUTPUT CLOSE.
                    END.  
                                                               
                    if tit_ap.cod_espec_docto = "CA" then do:
                    
                        FOR EACH movto_comis_repres NO-LOCK
                            WHERE movto_comis_repres.cod_estab          = tit_ap.cod_estab
                                  AND movto_comis_repres.num_id_tit_ap  = tit_ap.num_id_tit_ap:
                                  
                            FIND tit_acr NO-LOCK
                                WHERE tit_acr.cod_estab      = movto_comis_repres.cod_estab
                                  AND tit_acr.num_id_tit_acr = movto_comis_repres.num_id_tit_acr NO-ERROR.  
                                  
                        end.

                    end.       

                    IF l-log THEN DO:
                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                        PUT "v_tot_parc " v_tot_parc SKIP
                            "v_tot_Base " v_tot_base SKIP.
                        OUTPUT CLOSE.
                    END.                                                                

                IF CAN-FIND(FIRST dvv_tit_ap where
                                  dvv_tit_ap.cod_estab       = tit_ap.cod_estab
                              and dvv_tit_ap.num_id_tit_ap   = i-num-id-tit-ap) THEN DO:
                              
                    for each dvv_tit_ap no-lock where
                             dvv_tit_ap.cod_estab       = tit_ap.cod_estab
                         and dvv_tit_ap.num_id_tit_ap   = i-num-id-tit-ap:
                    
                        if dvv_tit_ap.classif = "DVV" then do:
                            FIND FIRST es_grp_dvv WHERE
                                       es_grp_dvv.cod_cta_ctbl_real = aprop_ctbl_ap.cod_cta_ctbl NO-LOCK NO-ERROR.

                            IF NOT CAN-FIND (FIRST es_grp_dvv_desp WHERE
                                                   es_grp_dvv_desp.cod_grupo = es_grp_dvv.cod_grupo AND
                                                   es_grp_dvv_desp.cod_desp  = dvv_tit_ap.cod_desp) THEN
                                                       NEXT.
            
                            for first  dvv_movto_det no-lock 
                                 WHERE dvv_movto_Det.cod_estabel   = dvv_tit_ap.cod_estab
                                   AND dvv_movto_det.num_processo  = dvv_tit_ap.nr_proc_exp
                                   and dvv_movto_det.tipo_despesa  = dvv_tit_ap.cod_desp
                                   and dvv_movto_det.it_codigo     = dvv_tit_ap.tipo_container
                                   and dvv_movto_det.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                                   and dvv_movto_det.doc_origem    = dvv_tit_ap.doc_origem:

                                IF v_tot_base <> v_tot_parc THEN DO:

                                    FIND FIRST dvv_movto no-lock where
                                               dvv_movto.cod_estabel     = dvv_movto_det.cod_estabel
                                           AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                                           and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.

                                    ASSIGN v-val-desp[1] = 0
                                           v-val-desp[2] = 0.
                                    IF AVAIL dvv_movto THEN
                                        ASSIGN v-val-desp[1] = ROUND((dvv_movto.VALOR_PREV_R$ * v_tot_parc) / v_tot_base , 2).

                                    ASSIGN v-val-desp[2] = ROUND((dvv_movto_det.VALOR_REAL_R$ * v_tot_parc) / v_tot_base , 2).

                                END.

                                IF l-log THEN DO:
                                    OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                    PUT "pi-cria-dados-dvv v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[2] "dvv" STRING(dvv_movto_det.VALOR_REAL_R$) SKIP.
                                    OUTPUT CLOSE.
                                END.

                                IF tit_ap.cod_espec_docto = "CA" THEN DO: /* Comissao */

                                    ASSIGN v-val-desp[2] = 0.
                                    FOR EACH movto_comis_repres NO-LOCK
                                        WHERE movto_comis_repres.cod_estab      = tit_ap.cod_estab
                                          AND movto_comis_repres.num_id_tit_ap  = tit_ap.num_id_tit_ap:

                                        FIND tit_acr NO-LOCK
                                            WHERE tit_acr.cod_estab      = tit_ap.cod_estab
                                              AND tit_acr.num_id_tit_acr = movto_comis_repres.num_id_tit_acr NO-ERROR.
                                        IF tit_acr.cod_tit_acr = dvv_movto_det.num_processo THEN DO:
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

                                RUN pi-cria-dados-dvv (INPUT "").

                            end.
                        end.

                        else
                            if dvv_tit_ap.classif = "DEX" then do:

                                for first dex_movto no-lock where
                                          dex_movto.cod_estabel   = dvv_tit_ap.cod_estab
                                      AND dex_movto.nr_pedido     = dvv_tit_ap.nr_proc_exp
                                      and dex_movto.nr_processo   = dvv_tit_ap.nr_proc_exp
                                      and dex_movto.tp_despesa    = dvv_tit_ap.cod_desp
                                      and dex_movto.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                                      and dex_movto.doc_origem    = dvv_tit_ap.doc_origem:

                                    IF v_tot_base <> v_tot_parc THEN DO:

                                        ASSIGN v-val-desp[1] = 0
                                               v-val-desp[2] = 0.

                                        ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                                        ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).

                                    END.

                                    IF l-log THEN DO:
                                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                        PUT "pi-cria-dados-dex v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[1] "dex" substring(dex_movto.char_1, 11, 10) SKIP.
                                        OUTPUT CLOSE.
                                    END.

                                    RUN pi-cria-dados-dex (INPUT "Realizado").

                                end.
                            end.
                    end.

                END. //carlos inicio
                ELSE IF CAN-FIND(FIRST dvv_tit_nc where
                                  dvv_tit_nc.cod_estab       = tit_ap.cod_estab
                              and dvv_tit_nc.num_id_tit_ap   = i-num-id-tit-ap) THEN DO:
                             
                    for each dvv_tit_nc no-lock where
                             dvv_tit_nc.cod_estab       = tit_ap.cod_estab
                         and dvv_tit_nc.num_id_tit_ap   = i-num-id-tit-ap:
                    
                        if dvv_tit_nc.classif = "DVV" then do:
            
                            for first  dvv_movto_det no-lock //carlos
                                 WHERE dvv_movto_Det.cod_estabel   <> "" //carlos - questionar
                                   AND dvv_movto_det.num_processo  = dvv_tit_nc.nr_proc_exp
                                   and dvv_movto_det.tipo_despesa  = dvv_tit_nc.cod_desp
                                   and dvv_movto_det.it_codigo     = ""
                                   and dvv_movto_det.tp_doc_origem = dvv_tit_nc.tp_doc_origem
                                   and dvv_movto_det.doc_origem    = dvv_tit_nc.doc_origem:

                                IF v_tot_base <> v_tot_parc THEN DO:

                                    FIND FIRST dvv_movto no-lock where
                                               dvv_movto.cod_estabel     = dvv_movto_det.cod_estabel
                                           AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                                           and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.

                                    ASSIGN v-val-desp[1] = 0
                                           v-val-desp[2] = 0.
                                    IF AVAIL dvv_movto THEN
                                        ASSIGN v-val-desp[1] = ROUND((dvv_movto.VALOR_PREV_R$ * v_tot_parc) / v_tot_base , 2).

                                    ASSIGN v-val-desp[2] = ROUND((dvv_movto_det.VALOR_REAL_R$ * v_tot_parc) / v_tot_base , 2).

                                END.

                                IF l-log THEN DO:
                                    OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                    PUT "pi-cria-dados-dvv v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[2] "dvv" STRING(dvv_movto_det.VALOR_REAL_R$) SKIP.
                                    OUTPUT CLOSE.
                                END.

                                IF tit_ap.cod_espec_docto = "CA" THEN DO: /* Comissao */

                                    ASSIGN v-val-desp[2] = 0.
                                    FOR EACH movto_comis_repres NO-LOCK
                                        WHERE movto_comis_repres.cod_estab      = tit_ap.cod_estab
                                          AND movto_comis_repres.num_id_tit_ap  = tit_ap.num_id_tit_ap:

                                        FIND tit_acr NO-LOCK
                                            WHERE tit_acr.cod_estab      = tit_ap.cod_estab
                                              AND tit_acr.num_id_tit_acr = movto_comis_repres.num_id_tit_acr NO-ERROR.
                                        IF tit_acr.cod_tit_acr = dvv_movto_det.num_processo THEN DO:
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

                                RUN pi-cria-dados-dvv (INPUT "").
                            end.
                        end.
                        else 
                            if dvv_tit_ap.classif = "DEX" then do:

                                /*for first dex_movto no-lock where
                                          dex_movto.cod_estabel   = dvv_tit_ap.cod_estab
                                      AND dex_movto.nr_pedido     = dvv_tit_ap.nr_proc_exp
                                      and dex_movto.nr_processo   = dvv_tit_ap.nr_proc_exp
                                      and dex_movto.tp_despesa    = dvv_tit_ap.cod_desp
                                      and dex_movto.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                                      and dex_movto.doc_origem    = dvv_tit_ap.doc_origem:

                                    IF v_tot_base <> v_tot_parc THEN DO:

                                        ASSIGN v-val-desp[1] = 0
                                               v-val-desp[2] = 0.
    
                                        ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                                        ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).

                                    END.

                                    IF l-log THEN DO:
                                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                        PUT "pi-cria-dados-dex v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[1] "dex" substring(dex_movto.char_1, 11, 10) SKIP.
                                        OUTPUT CLOSE.
                                    END.
                                    
                                    RUN pi-cria-dados-dex (INPUT "Realizado").

                                end.*/



                                /* leitura da nova tabela dex_movto_det */

                                for first dex_movto_det no-lock where
                                          dex_movto_det.cod_estabel   = dvv_tit_ap.cod_estab
                                      /*AND dex_movto_det.nr_pedido     = dvv_tit_ap.nr_proc_exp*/
                                      and dex_movto_det.nr_processo   = dvv_tit_ap.nr_proc_exp
                                      and dex_movto_det.tp_despesa    = dvv_tit_ap.cod_desp
                                      and dex_movto_det.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                                      and dex_movto_det.doc_origem    = dvv_tit_ap.doc_origem:

                                    IF v_tot_base <> v_tot_parc THEN DO:

                                        ASSIGN v-val-desp[1] = 0
                                               v-val-desp[2] = 0.
    
                                        ASSIGN v-val-desp[1] = ROUND((dex_movto_det.valor_real_r$ * v_tot_parc) / v_tot_base , 2).
                                        ASSIGN v-val-desp[2] = ROUND((dex_movto_det.valor_real_r$ * v_tot_parc) / v_tot_base , 2).

                                    END.

                                    IF l-log THEN DO:
                                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                        PUT "pi-cria-dados-dex v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[1] "dex" dex_movto_det.valor_real_r$ SKIP.
                                        OUTPUT CLOSE.
                                    END.
                                    
                                    RUN pi-cria-dados-dex (INPUT "Realizado").

                                end.

                            end.
                    end.
                    
                END. //carlos fim
                ELSE DO: /* Se nao achar dvv_tit_ap */
            
                    FOR EACH dvv_movto_det NO-LOCK where
                             /* dvv_movto_det.tp_doc_origem    = i-tp-doc-origem
                         AND */ dvv_movto_det.doc_origem       = c-chave-refer:

                        IF v_tot_base <> v_tot_parc THEN DO:
                            
                            FIND FIRST dvv_movto no-lock where
                                       dvv_movto.cod_estabel     = dvv_movto_det.cod_estabel
                                   AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                                   and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.
    
                            ASSIGN v-val-desp[1] = 0
                                   v-val-desp[2] = 0.
                            IF AVAIL dvv_movto THEN
                                ASSIGN v-val-desp[1] = ROUND((dvv_movto.VALOR_PREV_R$ * v_tot_parc) / v_tot_base , 2).

                            ASSIGN v-val-desp[2] = ROUND((dvv_movto_det.VALOR_REAL_R$ * v_tot_parc) / v_tot_base , 2).
                        END.

                        RUN pi-cria-dados-dvv (INPUT "").
                    END.
                
                    /* 16/07/2013 - Paulo Barth - Regra para criar relacionamento entre despesa DEX e T°tulo AP. */
                    /*FOR EACH dex_movto NO-LOCK where
                             /* dex_movto.tp_doc_origem    = i-tp-doc-origem
                         AND */ dex_movto.doc_origem       = c-chave-refer:

                        ASSIGN v-val-desp[1] = 0
                               v-val-desp[2] = 0.

                        ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                        ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).
        
                        RUN pi-cria-dados-dex (INPUT "Realizado").
                    END.*/



                    /* regra para leitura da nova tabela dex_movto_det */

                    IF CAN-FIND (FIRST dex_movto_det WHERE
                                       dex_movto_det.doc_origem = c-chave-refer) THEN do:
                        FOR EACH dex_movto_det NO-LOCK where
                                 /* dex_movto.tp_doc_origem    = i-tp-doc-origem
                             AND */ dex_movto_det.doc_origem   = c-chave-refer:
    
                            ASSIGN v-val-desp[1] = 0
                                   v-val-desp[2] = 0.
    
                            ASSIGN v-val-desp[1] = ROUND((dex_movto_det.valor_real_r$ * v_tot_parc) / v_tot_base , 2).
                            ASSIGN v-val-desp[2] = ROUND((dex_movto_det.valor_real_r$ * v_tot_parc) / v_tot_base , 2).
            
                            RUN pi-cria-dados-dex (INPUT "Realizado").
                        END.
                    END.
                    ELSE DO:
                    /* 16/07/2013 - Paulo Barth - Regra para criar relacionamento entre despesa DEX e T°tulo AP. */
                        FOR EACH dex_movto NO-LOCK where
                                 /* dex_movto.tp_doc_origem    = i-tp-doc-origem
                             AND */ dex_movto.doc_origem       = c-chave-refer:
    
                            ASSIGN v-val-desp[1] = 0
                                   v-val-desp[2] = 0.
    
                            ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                            ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).
            
                            RUN pi-cria-dados-dex (INPUT "Realizado").
                        END.
                    END.
                END.
            END.
            ELSE DO: /* Fim IF movto_tit_ap.ind_trans_ap_abrev = "IMPL" THEN DO:*/

                IF movto_tit_ap.ind_trans_ap BEGINS "Acerto Valor" THEN DO:

                    RUN pi-chave-tit_ap IN h-bo-dvv-movto-det (INPUT ROWID(tit_ap),
                                                               OUTPUT c-chave-refer).

                    ASSIGN d1-doc-origem = "".
                    IF NUM-ENTRIES(c-chave-refer,"|") > 6 THEN DO:                    
                        ASSIGN d1-doc-origem = ENTRY(7,c-chave-refer,"|") NO-ERROR.
                        IF d1-doc-origem <> "" THEN
                            ASSIGN ENTRY(7,c-chave-refer,"|") = "".
                    END.

/*                     IF CAN-FIND(FIRST dvv_tit_ap where                                                       */
/*                                       dvv_tit_ap.cod_estab       = tit_ap.cod_estab                          */
/*                                   and dvv_tit_ap.num_id_tit_ap   = tit_ap.num_id_tit_ap) THEN DO:            */
/*                                                                                                              */
/*                         FOR FIRST dvv_tit_ap no-lock where                                                   */
/*                                   dvv_tit_ap.cod_estab       = tit_ap.cod_estab                              */
/*                               and dvv_tit_ap.num_id_tit_ap   = tit_ap.num_id_tit_ap:                         */
/*                             ASSIGN c_processo = dvv_tit_ap.nr_proc_exp.                                      */
/*                         END.                                                                                 */
/*                                                                                                              */
/*                         IF l-log THEN DO:                                                                    */
/*                             OUTPUT TO \\10.55.0.144\datasul\dts11\ERP\newtech\Rodrigo\dvv\dvvlog.txt APPEND. */
/*                             PUT "pi-dados-titulo-apb -  Acerto Valor - 1processo " c_processo SKIP.          */
/*                             OUTPUT CLOSE.                                                                    */
/*                         END.                                                                                 */
/*                                                                                                              */
/*                     END.                                                                                     */
/*                     ELSE DO:                                                                                 */
/*                                                                                                              */
/*                         FOR FIRST dvv_movto_det NO-LOCK where                                                */
/*                                   dvv_movto_det.doc_origem = c-chave-refer:                                  */
/*                             ASSIGN c_processo = dvv_movto_det.num_processo.                                  */
/*                         END.                                                                                 */
/*                                                                                                              */
/*                         IF l-log THEN DO:                                                                    */
/*                             OUTPUT TO \\10.55.0.144\datasul\dts11\ERP\newtech\Rodrigo\dvv\dvvlog.txt APPEND. */
/*                             PUT "pi-dados-titulo-apb -  Acerto Valor - 2processo " c_processo SKIP.          */
/*                             OUTPUT CLOSE.                                                                    */
/*                         END.                                                                                 */
/*                                                                                                              */
/*                     END.                                                                                     */

                    FOR FIRST val_movto_ap NO-LOCK WHERE
                              val_movto_ap.cod_estab           = movto_tit_ap.cod_estab
                          and val_movto_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap
                          AND val_movto_ap.cod_finalid_econ    = "Corrente"
                          AND val_movto_ap.cod_unid_negoc      = aprop_ctbl_ap.cod_unid_negoc:
                
                        ASSIGN v-val-docto = val_movto_ap.val_ajust_val_tit_ap.

                    END.

                END.
            END.
    
            /* Caso n∆o encontre despesa imprime o valor e chave da mediá∆o. */
            IF v-val-docto > 0 THEN DO:
    
                create b-tt-rel-reg.
                buffer-copy b-tt-rel-base to b-tt-rel-reg.

                //carlos
                FIND FIRST dvv_tit_ap NO-LOCK
                    WHERE dvv_tit_ap.num_id_tit_ap = tit_ap.num_id_tit_ap NO-ERROR.
                IF AVAIL dvv_tit_ap THEN DO:
                    ASSIGN b-tt-rel-reg.tp-origem = dvv_tit_ap.tp_doc_orig.
                END.
                ELSE DO:
                    FIND FIRST dvv_tit_nc NO-LOCK
                        WHERE dvv_tit_nc.num_id_tit_ap = tit_ap.num_id_tit_ap NO-ERROR.
                        IF AVAIL dvv_tit_nc THEN
                            ASSIGN b-tt-rel-reg.tp-origem = dvv_tit_nc.tp_doc_orig.
                        ELSE ASSIGN b-tt-rel-reg.tp-origem = 2.
                END.
    
                ASSIGN b-tt-rel-reg.tipo-col = "D"
                       b-tt-rel-reg.movto    = "Realizado"
                       b-tt-rel-reg.val-lancto = 0
                       b-tt-rel-reg.val-doc    = v-val-docto
                       b-tt-rel-reg.tp-origem    = 2
                       b-tt-rel-reg.doc-origem   = c-chave-refer
                       b-tt-rel-reg.num-processo = c_processo.

                

                for first es_param_estab no-lock
                    where es_param_estab.cod_estabel = b-tt-rel-reg.cod-estabel
                      and es_param_Estab.cod_estabel_trd ne "".        
                   
                    FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                        WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel_trd
                          AND processo-exp.nr-proc-exp = c_processo,
                        FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                        
                    
                        ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                               b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                               /*111 - b-tt-rel-reg.regiao       = emitente.nome-mic-reg**/
                               .
                    END. 
                    IF NOT AVAIL processo-exp THEN DO:
                        FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                            WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel
                              AND processo-exp.nr-proc-exp = c_processo,
                            FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                    
                        
                            ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                                   b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                                   /*222 - b-tt-rel-reg.regiao       = emitente.nome-mic-reg**/.
                        END.    
                    END.
                end.
                if not avail es_param_estab then do:
                    FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                        WHERE processo-exp.cod-estabel = b-tt-rel-reg.cod-estabel
                          AND processo-exp.nr-proc-exp = c_processo,
                        FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                        
                    
                        ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                               b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                               /* 3- b-tt-rel-reg.regiao       = emitente.nome-mic-reg**/
                               .
                    END.
                end.

                FIND emitente
                    WHERE emitente.cod-emitente = tit_ap.cdn_fornecedor NO-LOCK.

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
    DEF INPUT PARAMETER p-movto AS CHAR NO-UNDO.                  
    def input parameter p-cod_modul_dtsul  like lote_ctbl.cod_modul_dtsul        no-undo.    
    def input parameter p-cod_estab        like item_lancto_ctbl.cod_estab       no-undo.
    def input parameter p-cod_cta_ctbl     like item_lancto_ctbl.cod_cta_ctbl    no-undo.
    def input parameter p-dat_lancto_ctbl  like item_lancto_ctbl.dat_lancto_ctbl no-undo.
    def input parameter p-cod_unid_negoc   like ITEM_lancto_ctbl.cod_unid_negoc  no-undo.

    DEF VAR v_total AS DEC NO-UNDO.
    DEF VAR v_total_prev AS DEC NO-UNDO.
    DEF VAR v_conta AS CHAR NO-UNDO.
    DEF VAR v_ccusto AS CHAR NO-UNDO.
    DEF VAR v_val_ant AS DEC NO-UNDO.
    DEF VAR v_chave_maior AS CHAR NO-UNDO.
    DEF VAR v_val_arredond AS DEC NO-UNDO.
    DEF VAR v-valor-unit AS DEC NO-UNDO.
    DEF VAR c-chave-orig AS CHAR NO-UNDO.
    DEF VAR c-desc AS CHAR FORMAT "X(100)" NO-UNDO.


    DEF VAR d-1 AS DECIMAL.

    DEF VAR d-2 AS DECIMAL.

    IF l-log THEN DO:
        OUTPUT TO c:\temp\dvvlog.txt APPEND.
        PUT "pi-dados-rec2" 
            p-cod_modul_dtsul SKIP
            p-cod_estab       SKIP
            p-cod_cta_ctbl    SKIP
            p-dat_lancto_ctbl SKIP
            p-cod_unid_negoc  SKIP 
            b-tt-rel-base.num-seq-lancto SKIP 
            .
        OUTPUT CLOSE.
    END.
    
    tt-block:
    for each tt-movto-cep where
             tt-movto-cep.cod_modul_dtsul = p-cod_modul_dtsul
         and tt-movto-cep.cod_estab       = p-cod_estab      
         and tt-movto-cep.cod_cta_ctbl    = p-cod_cta_ctbl   
         and tt-movto-cep.dat_lancto_ctbl = p-dat_lancto_ctbl
         AND tt-movto-cep.cod-unid-neg    = p-cod_unid_negoc  :

        ASSIGN d-1 = d-1 + tt-movto-cep.val_lancto_ctbl.

        IF p-movto = "Realizado" THEN
            if index(item_lancto_ctbl.des_histor_lancto_ctbl,string(tt-movto-cep.ge-codigo)) = 0 then
    		next.

        ASSIGN v_val_ant = 0.
        FOR EACH tt-acum-medicao: DELETE tt-acum-medicao. END.

        ASSIGN  c-chave-refer = ""
                c-chave-orig  = ""
                c-chave-refer = "".

        /*.MESSAGE "NR-TRANS = " tt-movto-cep.num_id_movto VIEW-AS ALERT-BOX.*/
        
        //busca lote
        RUN pi_busca_lote_lancto_item.

        IF v_num_seq = 0 THEN
            NEXT.
        /*.MESSAGE "v_num_seq = " v_num_seq VIEW-AS ALERT-BOX.*/

        //PUT "a" SKIP.
        
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
            ASSIGN //b-tt-rel-base.doc-origem = b-tt-rel-base.doc-origem + "Doc Sem Medicao: " + tt-movto-cep.nro-docto /*docum-est.nro-docto*/ + " "
                   b-tt-rel-base.tp-origem  = 7
                   v-val-docto              = tt-movto-cep.val_lancto_ctbl.

            ASSIGN c-desc = "".
            FIND FIRST ITEM WHERE
                       ITEM.it-codigo = tt-movto-cep.it-codigo NO-LOCK NO-ERROR.
            //IF ITEM.narrativa <> "" THEN
            //   ASSIGN c-desc = REPLACE(ITEM.narrativa,"CHR(10)", "")
            //          c-desc = REPLACE(ITEM.narrativa,"CHR(13)", "").
            //ELSE 
               ASSIGN c-desc = ITEM.desc-item.

            ASSIGN b-tt-rel-base.doc-origem = b-tt-rel-base.doc-origem + substr(("Docto: " + tt-movto-cep.nro-docto + " - " + tt-movto-cep.it-codigo +
                                       //" - " + tt-movto-cep.especie) +
                                       " - " + c-desc) + " - " +
                                      "Trans: " + string(tt-movto-cep.num_id_movto), 1, 100).
	    end.

        //PUT "b" SKIP.
        
        IF AVAIL docum-est THEN
            FIND FIRST item-doc-est NO-LOCK OF docum-est WHERE
                       item-doc-est.it-codigo = tt-movto-cep.it-codigo and
                       item-doc-est.sequencia = tt-movto-cep.sequen-nf NO-ERROR.
        /***
        FOR EACH rat-ordem NO-LOCK WHERE
                 rat-ordem.serie-docto  = docum-est.serie-docto
             AND rat-ordem.nro-docto    = docum-est.nro-docto
             AND rat-ordem.cod-emitente = docum-est.cod-emitente
             AND rat-ordem.nat-operacao = docum-est.nat-operacao:
        **/

        IF AVAIL item-doc-est THEN DO:
            /*.MESSAGE "1" VIEW-AS ALERT-BOX.*/
            //PUT "c" SKIP.

            FOR EACH rat-ordem OF item-doc-est NO-LOCK:
    
                FIND medicao-contrat NO-LOCK WHERE
                     medicao-contrat.nr-contrato     = rat-ordem.nr-contrato
                 AND medicao-contrat.num-seq-item    = rat-ordem.num-seq-item
                 AND medicao-contrat.numero-ordem    = rat-ordem.numero-ordem
                 AND medicao-contrat.num-seq-event   = rat-ordem.num-seq-event
                 AND medicao-contrat.num-seq-medicao = rat-ordem.num-seq-medicao NO-ERROR.
    
                if avail medicao-contrat then do:
    
                    run pi-chave-medicao-contrato IN h-bo-dvv-movto-det (rowid(medicao-contrat), output c-chave-refer).

                    //PUT "d" SKIP.
                    
                    ASSIGN c-chave-orig  = c-chave-refer
                           c-chave-refer = c-chave-refer + "|" + docum-est.nro-docto. //item-doc-est.nro-docto.
		    //aqui testar...sen„o retirar estava na vers„o do Carlos.
                    //PUT "e" SKIP.
                    IF NOT CAN-FIND(FIRST dvv_movto_det NO-LOCK 
                                    WHERE dvv_movto_det.tp_doc_origem = 1
                                      AND dvv_movto_det.doc_origem    = c-chave-orig) THEN NEXT.
                    //PUT "f" SKIP.
                    FIND tt-acum-medicao WHERE
                         tt-acum-medicao.chave = c-chave-refer NO-ERROR.
                    IF NOT AVAIL tt-acum-medicao THEN DO:
                        CREATE tt-acum-medicao.
                        ASSIGN tt-acum-medicao.chave      = c-chave-refer
                               tt-acum-medicao.chave-orig = c-chave-orig.
                    END.
                    //PUT "g" SKIP.
    
                    ASSIGN v-val-docto               = (rat-ordem.val-medicao / item-doc-est.preco-total[1]) * tt-movto-cep.val_lancto_ctbl
                           tt-acum-medicao.val-docto = tt-acum-medicao.val-docto + v-val-docto.
                    //PUT "h" SKIP.
                    IF v-val-docto > v_val_ant THEN DO:
                        ASSIGN v_val_ant     = v-val-docto
                               v_chave_maior = c-chave-refer.
                    END.
                END.
            END.
            //PUT "ZZZ" SKIP.
        END.
        ELSE DO:

            /*.MESSAGE "tt-movto-cep.val_lancto_ctbl - " tt-movto-cep.val_lancto_ctbl VIEW-AS ALERT-BOX.*/
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
                FOR EACH dvv_movto_det NO-LOCK WHERE
                         dvv_movto_det.tp_doc_origem = 1
                     AND dvv_movto_det.doc_origem    = tt-acum-medicao.chave-orig:
    
                    FIND FIRST dvv_movto no-lock where
                               dvv_movto.cod_estabel     = dvv_movto_Det.cod_Estabel
                           AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                           and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.
                        
                    ASSIGN v_total = v_total + dvv_movto_det.VALOR_REAL_R$.
                    
                    IF AVAIL dvv_movto THEN
                        ASSIGN v_total_prev = v_total_prev + dvv_movto.VALOR_PREV_R$.

                END.
    
                /* Calcula o rateio das despesas em cima do valor da mediá∆o, controlando o arredondamento. */
                ASSIGN v-val-docto = tt-acum-medicao.val-docto.
    
                ASSIGN v_val_arredond = 0.
                FOR EACH dvv_movto_det NO-LOCK WHERE
                         dvv_movto_det.tp_doc_origem = 1
                     AND dvv_movto_det.doc_origem    = tt-acum-medicao.chave-orig
                    BREAK BY dvv_movto_det.doc_origem:
    
                    FIND FIRST dvv_movto no-lock where
                               dvv_movto.cod_estabel     = dvv_movto_det.cod_estabel
                           AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                           and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.
    
                    ASSIGN v-val-desp[2]  = (dvv_movto_det.VALOR_REAL_R$ / v_total) * tt-acum-medicao.val-docto
                           v_val_arredond = v_val_arredond + v-val-desp[2].
    
                    ASSIGN v-val-desp[1] = 0.
                    IF AVAIL dvv_movto THEN
                        ASSIGN v-val-desp[1]     = (dvv_movto.VALOR_PREV_R$ / v_total) * tt-acum-medicao.val-docto.
    
                    IF LAST-OF(dvv_movto_det.doc_origem) THEN DO:
    
                        IF v_val_arredond <> tt-acum-medicao.val-docto THEN
                            ASSIGN v-val-desp[2] = v-val-desp[2] + (tt-acum-medicao.val-docto - v_val_arredond).
                    END.
    
                    IF l-log THEN DO:
                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                        PUT "pi-dados-rec5"             .
                        OUTPUT CLOSE.
                    END.
    
                    RUN pi-cria-dados-dvv (INPUT tt-acum-medicao.chave).
    
                    ASSIGN d-2 = d-2 + b-tt-rel-reg.vl-real.
        
                    RUN pi-grava-dados-desp-tit-ap.
                END.


                /******************
                //colocado de forma fixa pois esta dando um erro na geraá∆o deste lote e esta transaá∆o, esta considerando em duas realizaá‰es.
                IF b-tt-rel-base.num-seq-lancto = 6309 AND
                    (tt-movto-cep.num_id_movto = 63991608 or
                     tt-movto-cep.num_id_movto = 63991625) THEN do:
                    NEXT.
                END.

                IF b-tt-rel-base.num-seq-lancto = 5212 AND
                    (tt-movto-cep.num_id_movto = 64490460 or
                     tt-movto-cep.num_id_movto = 64490725 OR
                     tt-movto-cep.num_id_movto = 64490726) THEN do:
                     .MESSAGE "num-id = " tt-movto-cep.num_id_movto SKIP 
                             "val    = " v-val-docto VIEW-AS ALERT-BOX.
                    NEXT.
                END.

                IF b-tt-rel-base.num-seq-lancto = 2227
                    AND
                    (tt-movto-cep.num_id_movto = 64434326 or
                     tt-movto-cep.num_id_movto = 64434328 OR
                     tt-movto-cep.num_id_movto = 64434334 OR
                     tt-movto-cep.num_id_movto = 64434335) 
                     
                    THEN do:
                    NEXT.
                END.


                IF b-tt-rel-base.num-seq-lancto = 879
                    AND
                    (tt-movto-cep.num_id_movto = 64639413) 
                     
                    THEN do:
                    NEXT.
                END.
                ****************/
                /*
                //001/218063/1/5542
                if b-tt-rel-base.num-seq-lancto = 5542 and (tt-movto-cep.num_id_movto = 67569842) then
                    next.
                    
                if b-tt-rel-base.num-seq-lancto = 5544 and (tt-movto-cep.num_id_movto = 67569812) then
                    next.
                    */    

                /*.MESSAGE "1num-id = " tt-movto-cep.num_id_movto SKIP 
                        "seq     = " b-tt-rel-base.num-seq-lancto SKIP
                        "val    = " v-val-docto SKIP 
                        "nro-docto = " tt-movto-cep.nro-docto VIEW-AS ALERT-BOX.*/

                IF v-val-docto > 0 THEN DO:
        
                    /*.MESSAGE "1.1num-id = " tt-movto-cep.num_id_movto SKIP 
                            "seq     = " b-tt-rel-base.num-seq-lancto SKIP
                            "val    = " v-val-docto VIEW-AS ALERT-BOX.   */
                    FIND FIRST tt-movto-estoq-trans WHERE
                               tt-movto-estoq-trans.cod-cta-ctbl = p-cod_cta_ctbl AND
                               tt-movto-estoq-trans.nro-docto    = tt-movto-cep.nro-docto AND
                               tt-movto-estoq-trans.nr-trans     = STRING(tt-movto-cep.num_id_movto) NO-ERROR.
                    IF NOT AVAIL tt-movto-estoq-trans THEN DO:
                    
                        /*.MESSAGE "1.2num-id = " tt-movto-cep.num_id_movto SKIP 
                                "seq     = " b-tt-rel-base.num-seq-lancto SKIP
                                "val    = " v-val-docto VIEW-AS ALERT-BOX.*/

                        CREATE tt-movto-estoq-trans.
                        ASSIGN tt-movto-estoq-trans.cod-cta-ctbl = p-cod_cta_ctbl               
                               tt-movto-estoq-trans.nro-docto    = tt-movto-cep.nro-docto      
                               tt-movto-estoq-trans.nr-trans     = STRING(tt-movto-cep.num_id_movto).


                        create b-tt-rel-reg.
                        buffer-copy b-tt-rel-base to b-tt-rel-reg.
            
                        ASSIGN b-tt-rel-reg.tipo-col    = "D"
                               b-tt-rel-reg.movto       = p-movto //"Realizado"
                               b-tt-rel-reg.val-lancto  = 0
                               b-tt-rel-reg.val-doc     = v-val-docto
                               b-tt-rel-reg.tp-origem   = 1
                               //b-tt-rel-reg.doc-origem  = tt-acum-medicao.chave.
                               b-tt-rel-reg.doc-origem  = tt-acum-medicao.chave + " | " + STRING(tt-movto-cep.num_id_movto).
        
                        FIND ordem-compra WHERE
                             ordem-compra.numero-ordem = medicao-contrat.numero-ordem NO-LOCK NO-ERROR.
                        IF AVAIL ordem-compra THEN DO:
                            FIND emitente WHERE
                                 emitente.cod-emitente = ordem-compra.cod-emitente NO-LOCK NO-ERROR.
                            ASSIGN b-tt-rel-reg.cdn-fornec = emitente.cod-emitente
                                   b-tt-rel-reg.nome-forn  = emitente.nome-abrev.
                        END.
                        
                        ASSIGN v-val-docto = 0.
                        
                        //aqui 1
                        //RUN pi-busca-data-receita-2.
                        
                    END.
                END.
            END.
        END.
        ELSE DO:
            IF AVAIL docum-est THEN DO:
                FIND emitente WHERE
                    emitente.cod-emitente = tt-movto-cep.cod-emitente NO-LOCK NO-ERROR.
                IF AVAIL emitente THEN
                ASSIGN b-tt-rel-base.cdn-fornec = emitente.cod-emitente
                       b-tt-rel-base.nome-forn  = emitente.nome-abrev
                       b-tt-rel-base.usuario-impl = docum-est.usuario.

                ASSIGN b-tt-rel-base.doc-origem = b-tt-rel-base.doc-origem + "Doc Sem Medicao/Recebimento: " + tt-movto-cep.nro-docto + "/" + string(tt-movto-cep.sequen-nf) + " "
                       b-tt-rel-base.tp-origem  = 7.
            END.
            ELSE
                ASSIGN b-tt-rel-base.doc-origem = b-tt-rel-base.doc-origem + "Doc Sem Medicao/Estoque: " + tt-movto-cep.nro-docto + "/" + string(tt-movto-cep.sequen-nf) + " ".

            ASSIGN v-val-docto              = tt-movto-cep.val_lancto_ctbl.
            
            /*****************
            IF    b-tt-rel-base.num-seq-lancto = 6309 AND
                (tt-movto-cep.num_id_movto = 63991608 or
                 tt-movto-cep.num_id_movto = 63991625) THEN do:

                .MESSAGE b-tt-rel-base.num-seq-lancto SKIP
                        tt-movto-cep.num_id_movto VIEW-AS ALERT-BOX.
                NEXT.
            END.

            IF b-tt-rel-base.num-seq-lancto = 5212 AND
                (tt-movto-cep.num_id_movto = 64490460 or
                 tt-movto-cep.num_id_movto = 64490725 OR
                 tt-movto-cep.num_id_movto = 64490726) THEN do:
                 .MESSAGE "num-id = " tt-movto-cep.num_id_movto SKIP 
                         "val    = " v-val-docto VIEW-AS ALERT-BOX.
                NEXT.
            END.

            IF b-tt-rel-base.num-seq-lancto = 2227
                AND
                (tt-movto-cep.num_id_movto = 64434326 or
                 tt-movto-cep.num_id_movto = 64434328 OR
                 tt-movto-cep.num_id_movto = 64434334 OR
                 tt-movto-cep.num_id_movto = 64434335) 
                 
                THEN do:
                 .MESSAGE "num-id = " tt-movto-cep.num_id_movto SKIP 
                         "val    = " v-val-docto VIEW-AS ALERT-BOX.
                NEXT.
            END.

            IF b-tt-rel-base.num-seq-lancto = 879
                AND
                (tt-movto-cep.num_id_movto = 64639413) 

                THEN do:
                NEXT.
            END.
            *****************/
            /*
            //001/218063/1/5542
            if b-tt-rel-base.num-seq-lancto = 5542 and (tt-movto-cep.num_id_movto = 67569842) then
                next.
                
            if b-tt-rel-base.num-seq-lancto = 5544 and (tt-movto-cep.num_id_movto = 67569812) then
                next.   
                */ 

            
            
            /*.MESSAGE "2num-id = " tt-movto-cep.num_id_movto SKIP 
                     "seq     = " b-tt-rel-base.num-seq-lancto SKIP
                    "val    = " v-val-docto VIEW-AS ALERT-BOX. */

            IF v-val-docto > 0 THEN DO:
    
                
                FIND FIRST tt-movto-estoq-trans WHERE
                           tt-movto-estoq-trans.cod-cta-ctbl = p-cod_cta_ctbl AND
                           tt-movto-estoq-trans.nro-docto    = tt-movto-cep.nro-docto AND
                           tt-movto-estoq-trans.nr-trans     = STRING(tt-movto-cep.num_id_movto) NO-ERROR.
                IF NOT AVAIL tt-movto-estoq-trans THEN DO:
                
                    CREATE tt-movto-estoq-trans.
                    ASSIGN tt-movto-estoq-trans.cod-cta-ctbl = p-cod_cta_ctbl               
                           tt-movto-estoq-trans.nro-docto    = tt-movto-cep.nro-docto      
                           tt-movto-estoq-trans.nr-trans     = STRING(tt-movto-cep.num_id_movto).
                    create b-tt-rel-reg.
                    buffer-copy b-tt-rel-base to b-tt-rel-reg.
        
                    ASSIGN b-tt-rel-reg.tipo-col    = "D"
                           b-tt-rel-reg.movto       = p-movto //"Realizado"
                           b-tt-rel-reg.val-lancto  = 0
                           b-tt-rel-reg.val-doc     = v-val-docto
                           b-tt-rel-reg.vl-real     = v-val-docto.
    
                    ASSIGN b-tt-rel-base.tp-origem = 0
                           b-tt-rel-base.doc-origem = "".
                    
                    ASSIGN v-val-docto = 0.
                    
                    //aqui 2
                    //RUN pi-busca-data-receita-2.
                END.
            END.
        END.
    END.

end procedure.

procedure pi-dados-rec-previa-me:
    DEF INPUT PARAMETER p-movto AS CHAR NO-UNDO.                  
    def input parameter p-cod_modul_dtsul  like lote_ctbl.cod_modul_dtsul        no-undo.    
    def input parameter p-cod_estab        like item_lancto_ctbl.cod_estab       no-undo.
    def input parameter p-cod_cta_ctbl     like item_lancto_ctbl.cod_cta_ctbl    no-undo.
    def input parameter p-dat_lancto_ctbl  like item_lancto_ctbl.dat_lancto_ctbl no-undo.
    def input parameter p-cod_unid_negoc   like ITEM_lancto_ctbl.cod_unid_negoc  no-undo.

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
        OUTPUT TO c:\temp\dvvlog.txt APPEND.
        PUT "pi-dados-rec2" 
            p-cod_modul_dtsul SKIP
            p-cod_estab       SKIP
            p-cod_cta_ctbl    SKIP
            p-dat_lancto_ctbl SKIP
            p-cod_unid_negoc 
            .
        OUTPUT CLOSE.
    END.

    
    /*tt-block:
    
    for each tt-movto-cep where
             tt-movto-cep.cod_modul_dtsul = p-cod_modul_dtsul
         and tt-movto-cep.cod_estab       = p-cod_estab      
         and tt-movto-cep.cod_cta_ctbl    = p-cod_cta_ctbl   
         and tt-movto-cep.dat_lancto_ctbl = p-dat_lancto_ctbl
         AND tt-movto-cep.cod-unid-neg    = p-cod_unid_negoc  :

        ASSIGN d-1 = d-1 + tt-movto-cep.val_lancto_ctbl.
    */

    FIND FIRST tt-movto-cep WHERE
               tt-movto-cep.num_id_movto = movto-estoq.nr-trans NO-LOCK NO-ERROR.

    IF AVAIL tt-movto-cep THEN do: 
        ASSIGN d-1 = d-1 + tt-movto-cep.val_lancto_ctbl.
        IF p-movto = "Realizado" THEN
            if index(item_lancto_ctbl.des_histor_lancto_ctbl,string(tt-movto-cep.ge-codigo)) = 0 then
    		next.

        ASSIGN v_val_ant = 0.
        FOR EACH tt-acum-medicao: DELETE tt-acum-medicao. END.

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

        IF AVAIL docum-est THEN
            FIND FIRST item-doc-est NO-LOCK OF docum-est WHERE
                       item-doc-est.it-codigo = tt-movto-cep.it-codigo and
                       item-doc-est.sequencia = tt-movto-cep.sequen-nf NO-ERROR.
        /***
        FOR EACH rat-ordem NO-LOCK WHERE
                 rat-ordem.serie-docto  = docum-est.serie-docto
             AND rat-ordem.nro-docto    = docum-est.nro-docto
             AND rat-ordem.cod-emitente = docum-est.cod-emitente
             AND rat-ordem.nat-operacao = docum-est.nat-operacao:
        **/
        IF AVAIL item-doc-est THEN DO:

            FOR EACH rat-ordem OF item-doc-est NO-LOCK:
    
                FIND medicao-contrat NO-LOCK WHERE
                     medicao-contrat.nr-contrato     = rat-ordem.nr-contrato
                 AND medicao-contrat.num-seq-item    = rat-ordem.num-seq-item
                 AND medicao-contrat.numero-ordem    = rat-ordem.numero-ordem
                 AND medicao-contrat.num-seq-event   = rat-ordem.num-seq-event
                 AND medicao-contrat.num-seq-medicao = rat-ordem.num-seq-medicao NO-ERROR.
    
                if avail medicao-contrat then do:
    
                    run pi-chave-medicao-contrato IN h-bo-dvv-movto-det (rowid(medicao-contrat), output c-chave-refer).

                    ASSIGN c-chave-orig  = c-chave-refer
                           c-chave-refer = c-chave-refer + "|" + docum-est.nro-docto.
		    //aqui testar...sen„o retirar estava na vers„o do Carlos.
                    IF NOT CAN-FIND(FIRST dvv_movto_det NO-LOCK 
                                    WHERE dvv_movto_det.tp_doc_origem = 1
                                      AND dvv_movto_det.doc_origem    = c-chave-orig) THEN NEXT.
                    
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
                FOR EACH dvv_movto_det NO-LOCK WHERE
                         dvv_movto_det.tp_doc_origem = 1
                     AND dvv_movto_det.doc_origem    = tt-acum-medicao.chave-orig:
    
                    FIND FIRST dvv_movto no-lock where
                               dvv_movto.cod_estabel     = dvv_movto_Det.cod_Estabel
                           AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                           and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.
                        
                    ASSIGN v_total = v_total + dvv_movto_det.VALOR_REAL_R$.
                    
                    IF AVAIL dvv_movto THEN
                        ASSIGN v_total_prev = v_total_prev + dvv_movto.VALOR_PREV_R$.

                END.
    
                /* Calcula o rateio das despesas em cima do valor da mediá∆o, controlando o arredondamento. */
                ASSIGN v-val-docto = tt-acum-medicao.val-docto.
    
                ASSIGN v_val_arredond = 0.
                FOR EACH dvv_movto_det NO-LOCK WHERE
                         dvv_movto_det.tp_doc_origem = 1
                     AND dvv_movto_det.doc_origem    = tt-acum-medicao.chave-orig
                    BREAK BY dvv_movto_det.doc_origem:
    
                    FIND FIRST dvv_movto no-lock where
                               dvv_movto.cod_estabel     = dvv_movto_det.cod_estabel
                           AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                           and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.
    
                    ASSIGN v-val-desp[2]  = (dvv_movto_det.VALOR_REAL_R$ / v_total) * tt-acum-medicao.val-docto
                           v_val_arredond = v_val_arredond + v-val-desp[2].
    
                    ASSIGN v-val-desp[1] = 0.
                    IF AVAIL dvv_movto THEN
                        ASSIGN v-val-desp[1]     = (dvv_movto.VALOR_PREV_R$ / v_total) * tt-acum-medicao.val-docto.
    
                    IF LAST-OF(dvv_movto_det.doc_origem) THEN DO:
    
                        IF v_val_arredond <> tt-acum-medicao.val-docto THEN
                            ASSIGN v-val-desp[2] = v-val-desp[2] + (tt-acum-medicao.val-docto - v_val_arredond).
                    END.
    
                    IF l-log THEN DO:
                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                        PUT "pi-dados-rec5"             .
                        OUTPUT CLOSE.
                    END.
    
                    RUN pi-cria-dados-dvv (INPUT tt-acum-medicao.chave).
    
                    ASSIGN d-2 = d-2 + b-tt-rel-reg.vl-real.
        
                    RUN pi-grava-dados-desp-tit-ap.
                END.

                IF v-val-docto > 0 THEN DO:
        
                    create b-tt-rel-reg.
                    buffer-copy b-tt-rel-base to b-tt-rel-reg.
        
                    ASSIGN b-tt-rel-reg.tipo-col    = "D"
                           b-tt-rel-reg.movto       = p-movto //"Realizado"
                           b-tt-rel-reg.val-lancto  = 0
                           b-tt-rel-reg.val-doc     = v-val-docto
                           b-tt-rel-reg.tp-origem   = 1
                           //b-tt-rel-reg.doc-origem  = tt-acum-medicao.chave.
                           b-tt-rel-reg.doc-origem  = tt-acum-medicao.chave + " | " + STRING(tt-movto-cep.num_id_movto).
    
                    FIND ordem-compra WHERE
                         ordem-compra.numero-ordem = medicao-contrat.numero-ordem NO-LOCK NO-ERROR.
                    IF AVAIL ordem-compra THEN DO:
                        FIND emitente WHERE
                             emitente.cod-emitente = ordem-compra.cod-emitente NO-LOCK NO-ERROR.
                        ASSIGN b-tt-rel-reg.cdn-fornec = emitente.cod-emitente
                               b-tt-rel-reg.nome-forn  = emitente.nome-abrev.
                    END.
                    
                    ASSIGN v-val-docto = 0.
        
                END.
            END.
        END.
        ELSE DO:
            IF AVAIL docum-est THEN DO:
                FIND emitente WHERE
                    emitente.cod-emitente = tt-movto-cep.cod-emitente NO-LOCK NO-ERROR.
                IF AVAIL emitente THEN
                ASSIGN b-tt-rel-base.cdn-fornec = emitente.cod-emitente
                       b-tt-rel-base.nome-forn  = emitente.nome-abrev
                       b-tt-rel-base.usuario-impl = docum-est.usuario.

                ASSIGN b-tt-rel-base.doc-origem = b-tt-rel-base.doc-origem + "Doc Sem Medicao/Recebimento: " + tt-movto-cep.nro-docto + "/" + string(tt-movto-cep.sequen-nf) + " "
                       b-tt-rel-base.tp-origem  = 7.
            END.
            ELSE
                ASSIGN b-tt-rel-base.doc-origem = b-tt-rel-base.doc-origem + "Doc Sem Medicao/Estoque: " + tt-movto-cep.nro-docto + "/" + string(tt-movto-cep.sequen-nf) + " ".

            ASSIGN v-val-docto              = tt-movto-cep.val_lancto_ctbl.
            
            IF v-val-docto > 0 THEN DO:
    
                create b-tt-rel-reg.
                buffer-copy b-tt-rel-base to b-tt-rel-reg.
    
                ASSIGN b-tt-rel-reg.tipo-col    = "D"
                       b-tt-rel-reg.movto       = p-movto //"Realizado"
                       b-tt-rel-reg.val-lancto  = 0
                       b-tt-rel-reg.val-doc     = v-val-docto
                       b-tt-rel-reg.vl-real     = v-val-docto.

                ASSIGN b-tt-rel-base.tp-origem = 0
                       b-tt-rel-base.doc-origem = "".
                
                ASSIGN v-val-docto = 0.
    
            END.
        END.
    END.

end procedure.

PROCEDURE pi-dados-tms-apb:

    RUN dvv/dvv0199-tms.p PERSISTENT SET h-dvv0199-tms.
    RUN pi-dados-tms IN h-dvv0199-tms (INPUT item_lancto_ctbl.des_histor_lancto_ctbl,
                                       INPUT TABLE tt-tms).
    DELETE PROCEDURE h-dvv0199-tms.
    
END PROCEDURE.

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

/*                 IF CAN-FIND(FIRST dvv_tit_ap where                                                */
/*                                   dvv_tit_ap.cod_estab       = b_tit_ap.cod_estab                 */
/*                               and dvv_tit_ap.num_id_tit_ap   = b_tit_ap.num_id_tit_ap) THEN DO:   */
/*                                                                                                   */
/*                     /* Esta tabela dvv_tit_ap comeáar† a ter registros a partir de 01/08/2013. */ */
/*                                                                                                   */
/*                     for FIRST dvv_tit_ap no-lock where                                            */
/*                               dvv_tit_ap.cod_estab       = b_tit_ap.cod_estab                     */
/*                           and dvv_tit_ap.num_id_tit_ap   = b_tit_ap.num_id_tit_ap:                */
/*                         ASSIGN p_processo = dvv_tit_ap.nr_proc_exp.                               */
/*                     end.                                                                          */
/*                 END.                                                                              */
/*                 ELSE DO:                                                                          */
/*                                                                                                   */
/*                     RUN pi-chave-tit_ap IN h-bo-dvv-movto-det (INPUT ROWID(b_tit_ap),             */
/*                                                                OUTPUT c-chave-orig).              */
/*                     IF CAN-FIND(FIRST dvv_movto_det NO-LOCK where                                 */
/*                                       dvv_movto_det.doc_origem = c-chave-orig) THEN DO:           */
/*                                                                                                   */
/*                         FOR FIRST dvv_movto_det NO-LOCK where                                     */
/*                                   dvv_movto_det.doc_origem = c-chave-orig:                        */
/*                             ASSIGN p_processo = dvv_movto_det.num_processo.                       */
/*                         END.                                                                      */
/*                     END.                                                                          */
/*                     ELSE DO:                                                                      */
/*                         FOR FIRST dex_movto NO-LOCK where                                         */
/*                                   dex_movto.doc_origem = c-chave-orig:                            */
/*                             ASSIGN p_processo = dex_movto.nr_processo.                            */
/*                         END.                                                                      */
/*                     END.                                                                          */
/*                 END.                                                                              */
            END.
        end.
    end.


END PROCEDURE.

PROCEDURE pi-cria-dados-dex:
    DEF INPUT PARAMETER p-movto AS CHAR NO-UNDO.

    create b-tt-rel-reg.
    buffer-copy b-tt-rel-base to b-tt-rel-reg.
    
    ASSIGN i-seq-classif = i-seq-classif + 1.
    ASSIGN b-tt-rel-reg.tipo-col = "D"
           b-tt-rel-reg.movto = p-movto //"Realizado"
           b-tt-rel-reg.val-lancto = 0
           b-tt-rel-reg.val-doc    = v-val-docto
           b-tt-rel-reg.seq-class  = i-seq-classif
           b-tt-rel-reg.l-rastr-desp  = YES.

    /* Dados da despesa. */
    assign b-tt-rel-reg.tp-origem    = /*dex_movto.tp_doc_origem*/ IF AVAIL dex_movto_det THEN dex_movto_det.tp_doc_origem ELSE dex_movto.tp_doc_origem
           b-tt-rel-reg.doc-origem   = /*dex_movto.doc_origem*/    IF AVAIL dex_movto_det THEN dex_movto_det.doc_origem    ELSE dex_movto.doc_origem 
           b-tt-rel-reg.num-processo = /*dex_movto.nr_processo*/   IF AVAIL dex_movto_det THEN dex_movto_det.nr_processo   ELSE dex_movto.nr_processo.

    ASSIGN d1-doc-origem = "".
    
    IF NUM-ENTRIES(b-tt-rel-reg.doc-origem,"|") > 6 THEN DO:    
        ASSIGN d1-doc-origem = ENTRY(7,b-tt-rel-reg.doc-origem,"|") NO-ERROR.
        
        IF d1-doc-origem <> "" THEN
            ASSIGN ENTRY(7,b-tt-rel-reg.doc-origem,"|") = "".
    END.

        
    FOR FIRST processo-exp FIELDS(nr-proc-exp cod-estabel cod-emitente) NO-LOCK
        WHERE processo-exp.cod-estabel <> "999" /*"002"*/
          AND processo-exp.nr-proc-exp = /*dex_movto.nr_processo*/ dex_movto.nr_processo,
        FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.
        
        
        ASSIGN b-tt-rel-reg.cod-emitente = IF AVAIL processo-exp THEN processo-exp.cod-emitente ELSE 0
               b-tt-rel-reg.nome-abrev   = emitente.nome-abrev.
               
        RUN pi-busca-data-receita.       
    END.

    ASSIGN v-val-docto = 0.

    /* Valores da despesa. */
    assign b-tt-rel-reg.classif     = "DEX"
           b-tt-rel-reg.cod-desp    = /*dex_movto.tp_despesa*/ IF AVAIL dex_movto_det THEN dex_movto_det.tp_despesa      ELSE dex_movto.tp_despesa
           b-tt-rel-reg.dt-prev     = /*dex_movto.dt_movto*/   IF AVAIL dex_movto_det THEN dex_movto_det.data_movto_real ELSE dex_movto.dt_movto
           b-tt-rel-reg.vl-prev     = /*dex_movto.valor*/      IF AVAIL dex_movto_det THEN dex_movto_det.valor_real_r$   ELSE dex_movto.valor
           b-tt-rel-reg.dt-real     = /*dex_movto.data_atualiz*/ ? .

    IF AVAIL dex_movto_det THEN
        ASSIGN b-tt-rel-reg.vl-real     = IF v-val-desp[1] <> 0 THEN v-val-desp[1] ELSE /*dec(substring(dex_movto.char_1, 11, 10))*/ dex_movto_det.valor_real_r$
               b-tt-rel-reg.vl-prev     = IF v-val-desp[2] <> 0 THEN v-val-desp[2] ELSE /*dex_movto.valor*/ dex_movto_det.valor_real_r$
               b-tt-rel-reg.vl-real-tot = IF v-val-desp[1] <> 0 THEN v-val-desp[1] ELSE /*dec(substring(dex_movto.char_1, 11, 10))*/ dex_movto_det.valor_real_r$.
    ELSE
        ASSIGN b-tt-rel-reg.vl-real     = IF v-val-desp[1] <> 0 THEN v-val-desp[1] ELSE dec(substring(dex_movto.char_1, 11, 10))
               b-tt-rel-reg.vl-prev     = IF v-val-desp[2] <> 0 THEN v-val-desp[2] ELSE dex_movto.valor
               b-tt-rel-reg.vl-real-tot = IF v-val-desp[1] <> 0 THEN v-val-desp[1] ELSE dec(substring(dex_movto.char_1, 11, 10)).

        
    

    FIND FIRST es_grp_dex_desp WHERE
              es_grp_dex_desp.cod_desp = b-tt-rel-reg.cod-desp NO-LOCK NO-ERROR.
    IF AVAIL es_grp_dex_desp THEN DO:
       FIND es_grp_dex WHERE
            es_grp_dex.cod_grupo = es_grp_dex_desp.cod_grupo NO-LOCK NO-ERROR.
       IF AVAIL es_grp_dex THEN
           ASSIGN b-tt-rel-reg.cod-grupo = es_grp_dex.cod_grupo
                  b-tt-rel-reg.desc-grup = es_grp_dex.descricao.

    END.

    IF (AVAIL dvv_tit_ap OR AVAIL dvv_tit_nc) AND AVAIL tit_ap THEN DO:

        FIND emitente
            WHERE emitente.cod-emitente = tit_ap.cdn_fornecedor NO-LOCK.

        /* Dados do t°tulo APB. */
        assign b-tt-rel-reg.cdn-fornec    = tit_ap.cdn_fornecedor
               b-tt-rel-reg.nome-forn     = emitente.nome-abrev
               b-tt-rel-reg.cod-esp       = tit_ap.cod_espec_docto
               b-tt-rel-reg.cod-titulo    = tit_ap.cod_tit_ap
               b-tt-rel-reg.parcela       = tit_ap.cod_parcela.

        IF AVAIL movto_tit_ap THEN
            ASSIGN b-tt-rel-reg.trans-ap  = movto_tit_ap.ind_trans_ap
                   b-tt-rel-reg.usuario-impl = movto_tit_ap.cod_usuario.

        if tit_ap.ind_tip_espec_docto = "Imposto Taxado" or
           tit_ap.ind_tip_espec_docto = "Imposto Retido" THEN DO:
            ASSIGN b-tt-rel-reg.tp-origem = 15.
        END.

    END.

    //asdfasdfasdfasdf
    
    
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
               tt-rel-total.vl-tot-real = tt-rel-total.vl-tot-real + b-tt-rel-reg.vl-real .
    end. 
    else do:
        assign tt-rel-total.vl-tot-prev = tt-rel-total.vl-tot-prev + b-tt-rel-reg.vl-prev
               tt-rel-total.vl-tot-real = tt-rel-total.vl-tot-real + b-tt-rel-reg.vl-real .
    end.
    for first DEX_TP_DESPESA no-lock where
              DEX_TP_DESPESA.codigo = /*dex_movto.tp_despesa*/ dex_movto_det.tp_despesa:
        assign b-tt-rel-reg.des-desp = DEX_TP_DESPESA.descricao.
    END.

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

        IF NOT CAN-FIND(tt-conta-contab
                        WHERE tt-conta-contab.ep-codigo = conta-contab.ep-codigo
                          AND tt-conta-contab.ct-codigo = conta-contab.ct-codigo
                          AND tt-conta-contab.sc-codigo = conta-contab.sc-codigo) THEN DO:
            CREATE tt-conta-contab.
            BUFFER-COPY conta-contab TO tt-conta-contab.
        END.
    END.

    DO v-dat = tt-param.dt-ctbl-ini TO tt-param.dt-ctbl-fim:
        
        RUN pi-acompanhar IN h-acomp (INPUT "CEP.: " + STRING(v-dat,"99/99/9999")).

        FOR EACH tt-conta-contab:

            IF l-log THEN DO:
                OUTPUT TO c:\temp\dvvlog.txt APPEND.
                PUT "pi-busca-dados-estoque tt-conta-contab: " tt-conta-contab.conta-contabil SKIP.
                OUTPUT CLOSE.
            END.


            FOR EACH movto-estoq NO-LOCK WHERE
                     movto-estoq.dt-trans       = v-dat
                 AND movto-estoq.ct-codigo      = tt-conta-contab.ct-codigo
                 AND movto-estoq.cod-estabel    >= tt-param.c-estab-ini
                 AND movto-estoq.cod-estabel    <= tt-param.c-estab-fim
                 and (movto-estoq.esp-docto      <> 7 AND //DRM
                      movto-estoq.esp-docto      <> 5)     //DEV 
                 ,
                FIRST ITEM WHERE
                      ITEM.it-codigo = movto-estoq.it-codigo:
                if movto-estoq.esp-docto = 30 then do: //especie RM
                    find first bmovto-estoq where
                               bmovto-estoq.dt-trans       = movto-estoq.dt-trans    and
                               bmovto-estoq.ct-codigo      = movto-estoq.ct-codigo   and
                               bmovto-estoq.cod-estabel    = movto-estoq.cod-estabel and
                               bmovto-estoq.it-codigo      = movto-estoq.it-codigo   and
                               bmovto-estoq.esp-docto      = 7                       and //DRM
                               bmovto-estoq.nro-docto      = movto-estoq.nro-docto   and
                               bmovto-estoq.sequen-nf      = movto-estoq.sequen-nf   and
                               bmovto-estoq.cod-unid-negoc = movto-estoq.cod-unid-negoc and
                               bmovto-estoq.refer-contab   = movto-estoq.refer-contab and
                               bmovto-estoq.nr-trans       > movto-estoq.nr-trans no-lock no-error.
                                                              
                    if avail bmovto-estoq then
                        next.         
                end.
                else do:
                    if movto-estoq.esp-docto = 28 THEN DO: //especie REQ
                    
                        find first bmovto-estoq where
                                   bmovto-estoq.dt-trans       = movto-estoq.dt-trans    and
                                   bmovto-estoq.ct-codigo      = movto-estoq.ct-codigo   and
                                   bmovto-estoq.cod-estabel    = movto-estoq.cod-estabel and
                                   bmovto-estoq.it-codigo      = movto-estoq.it-codigo   and
                                   bmovto-estoq.esp-docto      = 5                       and //DRM
                                   bmovto-estoq.nro-docto      = movto-estoq.nro-docto   and
                                   bmovto-estoq.sequen-nf      = movto-estoq.sequen-nf   and
                                   bmovto-estoq.cod-unid-negoc = movto-estoq.cod-unid-negoc and
                                   bmovto-estoq.refer-contab   = movto-estoq.refer-contab and
                                   bmovto-estoq.nr-trans       > movto-estoq.nr-trans no-lock no-error.
                                                                  
                        if avail bmovto-estoq then
                            next.           
                    end.
                END. 

            IF l-log THEN DO:
                OUTPUT TO c:\temp\dvvlog.txt APPEND.
                PUT "pi-busca-dados-estoque movto-estoq" SKIP.
                OUTPUT CLOSE.
            END.
    
            ASSIGN v_val_movto = 0
                   v_val_movto = (movto-estoq.valor-mat-m[1] + /*
                                  movto-estoq.valor-mob-m[1] +
                                  movto-estoq.valor-ggf-m[1] +*/
                                  movto-estoq.valor-icm      +
                                  movto-estoq.valor-ipi      +
                                  movto-estoq.val-cofins     +
                                  movto-estoq.valor-pis).
         
            //incluido para tratar valores no movto-estoque = 0...copiado mesma logica do progama esct001. incluido no datasul 4.0 frente dvv.
            IF ITEM.tipo-contr = 2 THEN DO: /* Carrega Valor somente para Itens com Controle Total */
                if  v_val_movto = 0 then do:
                    /*-----> Busca Èltimo Valor do Material qdo o Movimento for Zero <----------------------*/
                    find first item-estab no-lock
                         where item-estab.it-codigo   = movto-estoq.it-codigo
                           and item-estab.cod-estabel = movto-estoq.cod-estabel no-error.
                    if  avail item-estab then
                        assign v_val_movto = (item-estab.val-unit-mat-m[1] + item-estab.val-unit-mob-m[1]
                                       +  item-estab.val-unit-ggf-m[1]) * movto-estoq.quantidade.
                    else
                        assign v_val_movto = 0.
                   /*--------------------------------------------------------------------------------------*/
                end.

                if  v_val_movto = 0 then do:
                    /*-----> Busca Èltimo Entrada Material  <----------------------------------------------*/
                    find first item-uni-estab no-lock
                         where item-uni-estab.it-codigo   = movto-estoq.it-codigo
                           and item-uni-estab.cod-estabel = movto-estoq.cod-estabel no-error.
                    if  avail item-uni-estab then
                        assign v_val_movto = item-uni-estab.preco-ul-ent * movto-estoq.quantidade.
                    else
                        assign v_val_movto = 0.
                   /*--------------------------------------------------------------------------------------*/
                end.
            END.
            //atÇ aqui alteraá∆o

            /* Se entrada valor fica negativo. */
            IF movto-estoq.tipo-trans = 1 THEN
                ASSIGN v_val_movto = v_val_movto * -1.
    
            FIND FIRST tt-movto-cep WHERE
                       tt-movto-cep.cod_modul_dtsul = "CEP"                   AND
                       tt-movto-cep.cod_estab       = movto-estoq.cod-estabel AND
                       tt-movto-cep.num_id_movto    = movto-estoq.nr-trans    NO-ERROR.

            IF NOT AVAIL tt-movto-cep THEN DO:

         		    //find item where item.it-codigo = movto-estoq.it-codigo no-lock no-error.
    
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
                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                        PUT "pi-busca-dados-estoque create" SKIP.
                        EXPORT DELIMITER ";" tt-movto-cep.
                        OUTPUT CLOSE.
                    END.
                END.
            END.
        END.
    END.  


end procedure.

PROCEDURE pi-busca-dados-tms-apb:

    RUN pi-acompanhar IN h-acomp (INPUT "CEP.: Buscando dados TMS.").

    RUN dvv/dvv0199-tms.p PERSISTENT SET h-dvv0199-tms.
    RUN pi-busca-dados-tms IN h-dvv0199-tms (INPUT TABLE tt-param,
                                             OUTPUT TABLE tt-tms).
    DELETE PROCEDURE h-dvv0199-tms.
    
END PROCEDURE.


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

PROCEDURE pi-dados-tms:
    DEF VAR h-dvv0199-tms AS HANDLE NO-UNDO.

    RUN dvv/dvv0199-tms.p PERSISTENT SET h-dvv0199-tms.
    RUN pi-dados-tms1 IN h-dvv0199-tms (BUFFER ITEM_lancto_ctbl).
    DELETE PROCEDURE h-dvv0199-tms.
    
END PROCEDURE.

PROCEDURE pi-busca-dados-tms:
    DEF VAR h-dvv0199-tms AS HANDLE NO-UNDO.

    RUN dvv/dvv0199-tms.p PERSISTENT SET h-dvv0199-tms.
    RUN pi-busca-dados-tms1 IN h-dvv0199-tms (BUFFER estabelec, 
                                             BUFFER tt-param).
    DELETE PROCEDURE h-dvv0199-tms.


END PROCEDURE.

procedure pi-chave-docto-frete.
    /* tp-doc-origem = 3 */
    def input parameter row-table as rowid no-undo.
    def output parameter c-doc-origem as char no-undo.

    DEF VAR h-dvv0199-tms AS HANDLE NO-UNDO.

    RUN dvv/dvv0199-tms.p PERSISTENT SET h-dvv0199-tms.
    RUN pi-chave-docto-frete1 IN h-dvv0199-tms (INPUT row-table,
                                                INPUT c-doc-origem).
    DELETE PROCEDURE h-dvv0199-tms.

end.

/* run pi-rowid-docto-frete-nf in h-bo-dvv-movto-det(c-origem, output c-rowid).               */
procedure pi-chave-docto-frete-nf.
    /* tp-doc-origem = 3 */

    def input parameter row-table as rowid no-undo.
    def output parameter c-doc-origem as char no-undo.

    DEF VAR h-dvv0199-tms AS HANDLE NO-UNDO.

    RUN dvv/dvv0199-tms.p PERSISTENT SET h-dvv0199-tms.
    RUN pi-chave-docto-frete-nf1 IN h-dvv0199-tms (INPUT row-table,
                                                   INPUT c-doc-origem).
    DELETE PROCEDURE h-dvv0199-tms.

END.

/* *************** Procedure Definitions - End *************** */


PROCEDURE pi-cria-dados-dvv:
    DEF INPUT PARAMETER p-chave-medicao AS CHAR NO-UNDO.

    /******************** Variaveis Definition ************************/
    DEF VAR d_dt_prev     like dvv_movto.data_movto_prev NO-UNDO.
    DEF VAR v_val_prev_R$ like dvv_movto.VALOR_PREV_R$   NO-UNDO.

    /*********************** Buffer Definition *************************/   
    DEF BUFFER b_dvv_movto for dvv_movto.
    
    /*********************** Main Code Begin *************************/   
    FIND FIRST dvv_movto no-lock where
               dvv_movto.cod_estabel  = dvv_movto_det.cod_estabel
           and dvv_movto.num_processo = dvv_movto_det.num_processo
           and dvv_movto.tipo_despesa = dvv_movto_det.tipo_despesa NO-ERROR.

    assign d_dt_prev     = ?         /* ORS 30/06/2016 */
           v_val_prev_R$ = ?.
           
    IF AVAIL dvv_movto THEN DO:
        assign d_dt_prev     = dvv_movto.data_movto_prev           /* ORS 30/06/2016 */
               v_val_prev_R$ = dvv_movto.VALOR_PREV_R$.

        /* VERIFICA TRADING ********************************** ORS 30/06/2016 */
        IF NOT CAN-FIND (FIRST es_param_estab WHERE 
                               es_param_estab.cod_estabel_trd = dvv_movto.cod_estabel) THEN DO:
        
            FIND FIRST es_param_estab NO-LOCK WHERE 
                       es_param_estab.cod_estabel      = dvv_movto.cod_estabel
                   AND es_param_estab.cod_estabel_trd NE "" NO-ERROR.
        
            FIND FIRST b_dvv_movto no-lock where
                       b_dvv_movto.cod_estabel  = es_param_estab.cod_estabel_trd
                   and b_dvv_movto.num_processo = dvv_movto_det.num_processo
                   and b_dvv_movto.tipo_despesa = dvv_movto_det.tipo_despesa NO-ERROR.            
                
            if avail b_dvv_movto then 
                ASSIGN d_dt_prev     = b_dvv_movto.data_movto_prev
                       v_val_prev_R$ = b_dvv_movto.VALOR_PREV_R$.
        END.
    END.

    /* aaa */
    create b-tt-rel-reg.
    buffer-copy b-tt-rel-base to b-tt-rel-reg.
    
    ASSIGN i-seq-classif                = i-seq-classif + 1.
    ASSIGN b-tt-rel-reg.tipo-col        = "D"
           b-tt-rel-reg.val-lancto      = 0
           b-tt-rel-reg.val-doc         = v-val-docto
           b-tt-rel-reg.seq-class       = i-seq-classif
           b-tt-rel-reg.l-rastr-desp    = YES.

    ASSIGN v-val-docto = 0.

    /* Dados da despesa. */
    assign b-tt-rel-reg.tp-origem    = dvv_movto_det.tp_doc_origem
           b-tt-rel-reg.doc-origem   = dvv_movto_det.doc_origem
           b-tt-rel-reg.num-processo = dvv_movto_det.num_processo
         //  b-tt-rel-reg.dt-conf      = dvv_movto_det.data_movto_real  - NT Carlos Jr - retirada esta regra por solicitaá∆o do Rios 21.05.24 chamado 0524-003623
         .

    IF AVAIL dvv_tit_nc  THEN
        ASSIGN b-tt-rel-reg.doc-origem = c-chave-refer.

        
    IF p-chave-medicao <> "" THEN
        ASSIGN b-tt-rel-reg.doc-origem   = p-chave-medicao.
        
    IF NUM-ENTRIES(b-tt-rel-reg.doc-origem,"|") > 6 THEN DO:
        ASSIGN d1-doc-origem = ENTRY(7,b-tt-rel-reg.doc-origem,"|") NO-ERROR.
        IF d1-doc-origem <> "" THEN DO:

            ASSIGN ENTRY(7,b-tt-rel-reg.doc-origem,"|") = "".
            
        END.    
    END.


        
           
    for first es_param_estab no-lock
        where es_param_estab.cod_estabel = b-tt-rel-reg.cod-estabel
          and es_param_Estab.cod_estabel_trd ne "".        
       
        FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
            WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel_trd
              AND processo-exp.nr-proc-exp = b-tt-rel-reg.num-processo,
            FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.
                           
        
            ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                   b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                   /* 7 b-tt-rel-reg.regiao       = emitente.nome-mic-reg*/. 

            RUN pi-busca-data-receita.

        END.    
        IF NOT AVAIL processo-exp THEN DO:
            FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel
                  AND processo-exp.nr-proc-exp = b-tt-rel-reg.num-processo,
                FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                    
                    
                ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                       b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                       /* 8 b-tt-rel-reg.regiao       = emitente.nome-mic-reg*/.

                RUN pi-busca-data-receita.
            END.    
        END.

    end.

    if not avail es_param_estab then do:
        FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
            WHERE processo-exp.cod-estabel = b-tt-rel-reg.cod-estabel
              AND processo-exp.nr-proc-exp = b-tt-rel-reg.num-processo,
            FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                                   
        
            ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                   b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                   /* 9 -b-tt-rel-reg.regiao       = emitente.nome-mic-reg*/.

            RUN pi-busca-data-receita.
        END. 
    end.

    IF NOT AVAIL processo-exp THEN DO:
        FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
            WHERE processo-exp.cod-estabel <> "999"
              AND processo-exp.nr-proc-exp = b-tt-rel-reg.num-processo,
            FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                                   
                               
            ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                   b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                   /* 9 -b-tt-rel-reg.regiao       = emitente.nome-mic-reg*/.

            RUN pi-busca-data-receita.
        END. 

    END.
    
    
    IF AVAIL processo-exp THEN
        RUN pi-busca-data-receita.

    /* 18/09/2013 Paulo Barth Calcular o previsto rateando pela quantidade de dvv_movto_det.
                              pois as despesas 5, 28, 29, 30 podem ser realizadas mais de uma vez mas o
                              previsto Ç sempre o valor total. */
    IF v-val-desp[1] = 0 THEN
        ASSIGN v-val-desp[1] = fi-ret-prev-container(input dvv_movto_det.cod_estabel,
                                                     INPUT dvv_movto_det.num_processo,
                                                     INPUT dvv_movto_det.tipo_despesa,
                                                     INPUT v_val_prev_R$). /* dvv_movto.VALOR_PREV_R$ ORS 30/06/2016 */   

    IF AVAIL dvv_movto THEN DO:

        if d_dt_prev = ? then do:
            FIND processo-exp WHERE 
                 processo-exp.cod-estabel = dvv_movto.cod_estabel
             AND processo-exp.nr-proc-exp = dvv_movto.num_processo NO-LOCK NO-ERROR.
             
            IF AVAIL processo-exp THEN DO:
                IF processo-exp.dt-embarque <> ? THEN
                    ASSIGN d_dt_prev = processo-exp.dt-embarque.
                ELSE DO:
                    FIND FIRST proc-ped-venda OF processo-exp NO-LOCK NO-ERROR.
                    IF AVAIL proc-ped-venda THEN DO:
                        FIND ped-venda WHERE 
                             ped-venda.nome-abrev = proc-ped-venda.nome-abrev
                         AND ped-venda.nr-pedcli  = proc-ped-venda.nr-pedcli NO-LOCK NO-ERROR.
                        IF AVAIL ped-venda THEN
                            ASSIGN d_dt_prev = ped-venda.dt-entrega.
                        ELSE
                            ASSIGN d_dt_prev = ?.
                    END.
                END.
            END.
            ELSE
                ASSIGN d_dt_prev = ?.
        END.
    END.
    ELSE 
        ASSIGN d_dt_prev = ?.

    /* Valores da despesa. */
    assign b-tt-rel-reg.classif     = "DVV ME"
           b-tt-rel-reg.cod-desp    = dvv_movto_det.tipo_despesa
           b-tt-rel-reg.dt-prev     = d_dt_prev
/*         b-tt-rel-reg.vl-prev     = IF v-val-desp[1] <> 0 THEN v-val-desp[1] ELSE (IF AVAIL dvv_movto THEN dvv_movto.VALOR_PREV_R$ ELSE 0)  *** ORS 30/06/2016 */
/*           b-tt-rel-reg.vl-prev     = IF v-val-desp[1] <> 0 THEN v-val-desp[1] ELSE (IF AVAIL dvv_movto THEN v_val_prev_R$ ELSE 0)*/
           b-tt-rel-reg.dt-real     = dvv_movto_det.data_movto_real
           b-tt-rel-reg.vl-real     = IF v-val-desp[2] <> 0 THEN v-val-desp[2] ELSE dvv_movto_det.VALOR_REAL_R$
           b-tt-rel-reg.vl-prev     = IF dvv_movto.VALOR_PREV_R$ <> 0 THEN dvv_movto.VALOR_PREV_R$ ELSE 0
           b-tt-rel-reg.vl-real-tot = IF dvv_movto_det.VALOR_REAL_R$ <> 0 THEN dvv_movto_det.VALOR_REAL_R$ ELSE 0.

           
           

    /*
    ** Previs∆o Trading Ç em outro estabelecimento
    */
    IF b-tt-rel-reg.vl-prev = 0 THEN DO:
        FIND b-dvv-movto
            WHERE b-dvv-movto.cod_estabel = "002"
              AND b-dvv-movto.num_processo = dvv_movto.num_processo
              AND b-dvv-movto.tipo_despesa = dvv_movto.tipo_despesa NO-LOCK NO-ERROR.
        IF AVAIL b-dvv-movto THEN
            ASSIGN b-tt-rel-reg.vl-prev = b-dvv-movto.valor_prev_r$.
    END.

    FIND FIRST es_grp_dvv_desp WHERE
               es_grp_dvv_desp.cod_desp = b-tt-rel-reg.cod-desp NO-LOCK NO-ERROR.
    IF AVAIL es_grp_dvv_desp THEN DO:
        FIND es_grp_dvv WHERE
             es_grp_dvv.cod_grupo = es_grp_dvv_desp.cod_grupo NO-LOCK NO-ERROR.
        IF AVAIL es_grp_dvv THEN
            ASSIGN b-tt-rel-reg.cod-grupo = es_grp_dvv.cod_grupo
                   b-tt-rel-reg.desc-grup = es_grp_dvv.descricao.
    END.

    IF AVAIL tit_ap THEN DO:

         FIND FIRST emitente
            WHERE emitente.cod-emitente = tit_ap.cdn_fornecedor NO-LOCK NO-ERROR.
         IF AVAIL emitente THEN DO:
            /* Dados do t°tulo APB. */
            assign b-tt-rel-reg.cdn-fornec    = tit_ap.cdn_fornecedor
                   b-tt-rel-reg.nome-forn     = emitente.nome-abrev
                   b-tt-rel-reg.cod-esp       = tit_ap.cod_espec_docto
                   b-tt-rel-reg.cod-titulo    = tit_ap.cod_tit_ap
                   b-tt-rel-reg.parcela       = tit_ap.cod_parcela.
         END.

        IF AVAIL movto_tit_ap THEN
            ASSIGN b-tt-rel-reg.trans-ap  = movto_tit_ap.ind_trans_ap.
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

    IF AVAIL tit_acr THEN DO:

         FIND FIRST cliente
            WHERE cliente.cdn_cliente = tit_acr.cdn_cliente NO-LOCK NO-ERROR.
         IF AVAIL cliente THEN DO:
            /* Dados do t°tulo ACR. */
            assign b-tt-rel-reg.cdn-fornec    = tit_acr.cdn_cliente
                   b-tt-rel-reg.nome-forn     = cliente.nom_abrev
                   b-tt-rel-reg.cod-esp       = tit_acr.cod_espec_docto
                   b-tt-rel-reg.cod-titulo    = tit_acr.cod_tit_acr
                   b-tt-rel-reg.parcela       = tit_acr.cod_parcela.
         END.

        IF AVAIL movto_tit_acr THEN
            ASSIGN b-tt-rel-reg.trans-ap  = movto_tit_acr.ind_trans_acr.
                   b-tt-rel-reg.usuario-impl = movto_tit_acr.cod_usuario.

        if tit_acr.ind_tip_espec_docto = "Imposto Taxado" or
           tit_acr.ind_tip_espec_docto = "Imposto Retido" THEN
            ASSIGN b-tt-rel-reg.tp-origem = 15.
            
        /* 
        ** Verifica se o t°tulo teve origem no GFE
        */ 
        IF tit_acr.cod_espec_docto = "TR" then do:
        
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
               tt-rel-total.vl-tot-real = tt-rel-total.vl-tot-real + b-tt-rel-reg.vl-real .
    end. 
    else do:
        assign tt-rel-total.vl-tot-prev = tt-rel-total.vl-tot-prev + b-tt-rel-reg.vl-prev
               tt-rel-total.vl-tot-real = tt-rel-total.vl-tot-real + b-tt-rel-reg.vl-real .
    end.

    ASSIGN v-val-desp[1] = 0
           v-val-desp[2] = 0.

    for first DVV_TIPO_DESPESA no-lock where
              DVV_TIPO_DESPESA.codigo = dvv_movto_det.tipo_despesa:
        assign b-tt-rel-reg.des-desp = DVV_TIPO_DESPESA.descricao.
    end.

    IF l-log THEN DO:
        OUTPUT TO c:\temp\dvvlog.txt APPEND.
        PUT "fim pi-cria-dados-dvv tt-rel-total.vl-tot-real " tt-rel-total.vl-tot-real " b-tt-rel-reg.vl-real " b-tt-rel-reg.vl-real SKIP.
        OUTPUT CLOSE.
    END.
        
END PROCEDURE.

PROCEDURE pi-grava-dados-desp-tit-ap:

    for first dvv_tit_ap no-lock where
              dvv_tit_ap.cod_estab      = dvv_movto_det.cod_estabel
          and dvv_tit_ap.nr_proc_exp    = dvv_movto_det.num_processo
          and dvv_tit_ap.cod_desp       = dvv_movto_det.tipo_despesa
          and dvv_tit_ap.tipo_container = dvv_movto_det.it_codigo
          and dvv_tit_ap.tp_doc_origem  = dvv_movto_det.tp_doc_origem
          and dvv_tit_ap.doc_origem     = dvv_movto_det.doc_origem:

        find tit_ap no-lock where
             tit_ap.cod_estab       = dvv_tit_ap.cod_estab
         and tit_ap.num_id_tit_ap   = dvv_tit_ap.num_id_tit_ap no-error.

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
        ELSE DO: //carlos inicio
            for first dvv_tit_nc no-lock where
                      dvv_tit_nc.cod_estab      = dvv_movto_det.cod_estabel
                  and dvv_tit_nc.nr_proc_exp    = dvv_movto_det.num_processo
                  and dvv_tit_nc.cod_desp       = dvv_movto_det.tipo_despesa
                  and dvv_tit_nc.tipo_container = dvv_movto_det.it_codigo
                  and dvv_tit_nc.tp_doc_origem  = dvv_movto_det.tp_doc_origem
                  and dvv_tit_nc.doc_origem     = dvv_movto_det.doc_origem:
        
                find tit_ap no-lock where
                     tit_ap.cod_estab       = dvv_tit_nc.cod_estab
                 and tit_ap.num_id_tit_ap   = dvv_tit_nc.num_id_tit_ap no-error.
        
                IF AVAIL tit_ap THEN DO:
                    FIND emitente
                        WHERE emitente.cod-emitente = tit_ap.cdn_fornecedor NO-LOCK.
        
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
                END.
            END.
        END. //carlos fim
    END.

END PROCEDURE.

PROCEDURE pi-monta-provisao:

        DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.
        DEFINE VARIABLE c-empresa AS CHARACTER   NO-UNDO.
        
        DEF VAR i-lote-ctbl AS INT NO-UNDO.
        DEF VAR d-tot-desp AS DECIMAL NO-UNDO.

        EMPTY TEMP-TABLE tt-cta-provisao.
        EMPTY TEMP-TABLE tt-provisao.

        FOR EACH es_grp_dvv NO-LOCK.
            
            FIND FIRST tt-cta-provisao
                 WHERE tt-cta-provisao.cod-conta = es_grp_dvv.cod_cta_ctbl NO-ERROR.
            IF NOT AVAIL tt-cta-provisao THEN DO:
                CREATE tt-cta-provisao.
                ASSIGN tt-cta-provisao.cod-conta = es_grp_dvv.cod_cta_ctbl.
            END.

        END.

        FOR EACH es_grp_dex NO-LOCK.
            
            FIND FIRST tt-cta-provisao
                 WHERE tt-cta-provisao.cod-conta = es_grp_dex.cod_cta_ctbl NO-ERROR.
            IF NOT AVAIL tt-cta-provisao THEN DO:
                CREATE tt-cta-provisao.
                ASSIGN tt-cta-provisao.cod-conta = es_grp_dex.cod_cta_ctbl.
            END.

        END.

        FOR EACH tt-cta-provisao WHERE
                 tt-cta-provisao.cod-conta >= tt-param.c-cta-ctbl-ini AND
                 tt-cta-provisao.cod-conta <= tt-param.c-cta-ctbl-fim:

            DO i-cont = 1 TO 2.

                IF i-cont = 1 THEN
                    ASSIGN c-empresa = "01".
                ELSE
                    ASSIGN c-empresa = "02".

                FOR EACH  ITEM_lancto_ctbl NO-LOCK 
                    WHERE ITEM_lancto_Ctbl.cod_empresa         = c-empresa
                      and item_lancto_ctbl.dat_lancto_ctbl    >= tt-param.dt-ctbl-ini
                      and item_lancto_ctbl.dat_lancto_ctbl    <= tt-param.dt-ctbl-fim
                      AND ITEM_lancto_ctbl.cod_plano_cta_ctbl  = "nitro"
                      AND ITEM_lancto_ctbl.cod_cta_ctbl        = tt-cta-provisao.cod-conta
                      AND ITEM_lancto_ctbl.cod_estab          >= tt-param.c-estab-ini
                      AND ITEM_lancto_ctbl.cod_estab          <= tt-param.c-estab-fim
                      AND ITEM_lancto_ctbl.cod_unid_negoc     >= tt-param.c-unid-negoc-ini
                      AND ITEM_lancto_ctbl.cod_unid_negoc     <= tt-param.c-unid-negoc-fim
                      //AND item_lancto_ctbl.num_lote_ctbl = 259469
                    BREAK BY ITEM_lancto_ctbl.num_lote_ctbl:

                    RELEASE bdvv_log_provisao.
                    RELEASE dvv_log_provisao.

                    FIND first dvv_log_provisao NO-LOCK
                         WHERE dvv_log_provisao.num_lote_ctbl = ITEM_lancto_Ctbl.num_lote_ctbl
                           AND dvv_log_provisao.cod_estabel   = ITEM_lancto_Ctbl.cod_estab NO-ERROR.
                    IF NOT AVAIL dvv_log_provisao THEN DO:

                        FIND first bdvv_log_provisao NO-LOCK
                             WHERE bdvv_log_provisao.num_lote_ctbl_estorno = ITEM_lancto_Ctbl.num_lote_ctbl
                               AND bdvv_log_provisao.cod_estabel           = ITEM_lancto_Ctbl.cod_estab NO-ERROR.
                        IF NOT AVAIL bdvv_log_provisao THEN do:
                            NEXT.
                        END.
                        ELSE DO:
                            IF NOT tt-param.l-provisao-dvv-me AND (/*bdvv_log_provisao.origem_movto = "EPDEX" OR bdvv_log_provisao.origem_movto = "EPDVV ME" OR*/ bdvv_log_provisao.origem_movto = "DEX" OR bdvv_log_provisao.origem_movto = "DVV ME") then 
                                NEXT.
    
                            IF NOT tt-param.l-provisao-dvv-mi AND (/*bdvv_log_provisao.origem_movto = "EPDVV MI" OR*/ bdvv_log_provisao.origem_movto = "DVV MI")  THEN
                                NEXT.
                        END.
                    END.
                    ELSE DO:

                        IF NOT tt-param.l-provisao-dvv-me AND (/*dvv_log_provisao.origem_movto = "EPDEX" OR dvv_log_provisao.origem_movto = "EPDVV ME" OR*/ dvv_log_provisao.origem_movto = "DEX" OR dvv_log_provisao.origem_movto = "DVV ME") then 
                            NEXT.

                        IF NOT tt-param.l-provisao-dvv-mi AND (/*dvv_log_provisao.origem_movto = "EPDVV MI" OR*/ dvv_log_provisao.origem_movto = "DVV MI")  THEN
                            NEXT.

                    END.
                    
                    run pi-acompanhar in h-acomp("CT: " + tt-cta-provisao.cod-conta + " Data: " + STRING(ITEM_lancto_ctbl.dat_lancto_ctbl)).
                    
                    IF FIRST-OF(ITEM_lancto_ctbl.num_lote_ctbl) THEN DO:
                    
                        FIND lote_ctbl NO-LOCK WHERE
                             lote_ctbl.num_lote_ctbl = ITEM_lancto_ctbl.num_lote_ctbl NO-ERROR.
                    
                    END.   

                    run pi_achar_cotac_indic_econ (Input v_cod_indic_econ_base,
                                                   Input v_cod_indic_econ_apres,
                                                   Input ITEM_lancto_ctbl.dat_lancto_ctbl,
                                                   Input cRealVenda /*l_real*/,
                                                   output v_data,
                                                   output v_val_cotac_indic_econ,
                                                   output v_cod_return) /*pi_achar_cotac_indic_econ*/.

                    IF lote_ctbl.cod_modul_dtsul = "FGL" THEN DO:

                        create tt-provisao.
                        assign tt-provisao.tipo           = "T"
                               tt-provisao.movto          = IF AVAIL bdvv_log_provisao THEN "Estornado" ELSE "Provisionado"
                               tt-provisao.conta          = ITEM_lancto_ctbl.cod_cta_ctbl
                               tt-provisao.modulo         = lote_ctbl.cod_modul_dtsul
                               tt-provisao.dta-ctbz       = ITEM_lancto_ctbl.dat_lancto_ctbl
                               tt-provisao.num-lote       = ITEM_lancto_ctbl.num_lote_ctbl
                               tt-provisao.num-lancto     = ITEM_lancto_ctbl.num_lancto_ctbl
                               tt-provisao.num-seq-lancto = ITEM_lancto_ctbl.num_seq_lancto_ctbl
                               tt-provisao.mov            = ITEM_lancto_ctbl.ind_natur_lancto_ctbl
                               tt-provisao.moeda          = cRealVenda
                               tt-provisao.cod-estabel    = ITEM_lancto_ctbl.cod_estab
                               tt-provisao.dt-lote        = lote_ctbl.dat_ult_atualiz
                               tt-provisao.user-lote      = lote_ctbl.cod_usuAr_ult_atualiz
                               tt-provisao.usuario-impl   = lote_ctbl.cod_usuAr_ult_atualiz  
                               tt-provisao.cod-unid-negoc = ITEM_lancto_ctbl.cod_unid_negoc
                               tt-provisao.cod-unid-negoc-gerenc = ITEM_lancto_ctbl.cod_unid_negoc.

                        ASSIGN tt-provisao.doc-origem = replace(ITEM_lancto_ctbl.des_histor_lancto_ctbl, CHR(10), " ")
                               tt-provisao.doc-origem = REPLACE(tt-provisao.doc-origem, ";", ",").
                        /* O relat¢rio dever† sempre apresentar os valores em reais.
                           Caso o indicador econìmico seja diferente de Real buscar na apropriaá∆o o valor em reais. */
                        IF ITEM_lancto_ctbl.cod_indic_econ <> cRealVenda THEN DO:

                            FOR FIRST aprop_lancto_ctbl NO-LOCK WHERE
                                      aprop_lancto_ctbl.num_lote            = item_lancto_ctbl.num_lote
                                  AND aprop_lancto_ctbl.num_lancto_ctbl     = item_lancto_ctbl.num_lancto_ctbl
                                  AND aprop_lancto_ctbl.num_seq_lancto_ctbl = item_lancto_ctbl.num_seq_lancto_ctbl
                                  AND aprop_lancto_ctbl.cod_finalid_econ    = "Corrente":
                                ASSIGN tt-provisao.val-lancto = aprop_lancto_ctbl.val_lancto_ctbl
                                       /*tt-provisao.val-doc    = aprop_lancto_ctbl.val_lancto_ctbl*/.
                            END.
                        END.
                        ELSE
                            ASSIGN tt-provisao.val-lancto = ITEM_lancto_ctbl.val_lancto_ctbl
                                /*
                                   tt-provisao.val-doc    = ITEM_lancto_ctbl.val_lancto_ctbl */.

                    /*IF lote_ctbl.cod_modul_dtsul = "FGL" THEN DO:*/


                        ASSIGN i-seq-classif = 0.
                        IF tt-provisao.movto = "Estornado" AND NOT tt-param.l-estorno THEN NEXT.

                        IF tt-provisao.movto = "Estornado" AND item_lancto_ctbl.num_lancto_ctbl = 10 THEN DO:
                            ASSIGN d-tot-desp = 0.
                            FOR EACH  dvv_log_provisao NO-LOCK
                                WHERE dvv_log_provisao.num_lote_ctbl_estorno = ITEM_lancto_Ctbl.num_lote_ctbl
                                  AND dvv_log_provisao.cod_estabel   = ITEM_lancto_Ctbl.cod_estab,
                                EACH  dvv_log_prov_desp NO-LOCK
                                WHERE dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao.
                                ASSIGN d-tot-desp = d-tot-desp + dvv_log_prov_desp.vl_desp.
                            END.
                            FOR EACH  dvv_log_provisao NO-LOCK
                                WHERE dvv_log_provisao.num_lote_ctbl_estorno = ITEM_lancto_Ctbl.num_lote_ctbl
                                  AND dvv_log_provisao.cod_estabel   = ITEM_lancto_Ctbl.cod_estab,
                                EACH  dvv_log_prov_desp NO-LOCK
                                WHERE dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao.

                                /* N∆o provisiona */
                                /*IF dvv_log_provisao.num_lote_ctbl_estorno = 0 THEN
                                    NEXT.*/

                                    
                                    
                                create b-tt-provisao.
                                buffer-copy tt-provisao to b-tt-provisao.

                                ASSIGN i-seq-classif = i-seq-classif + 1.
                                ASSIGN b-tt-provisao.tipo-col   = "D"
                                       b-tt-provisao.movto      = "Estornado"
                                       b-tt-provisao.val-lancto = 0
                                       /*b-tt-provisao.val-doc    = 0*/
                                       b-tt-provisao.seq-class  = i-seq-classif
                                       b-tt-provisao.l-rastr-desp  = YES
                                       b-tt-provisao.tp-origem = 13.

                                ASSIGN b-tt-provisao.doc-origem = replace(ITEM_lancto_ctbl.des_histor_lancto_ctbl, CHR(10), " ")
                                       b-tt-provisao.doc-origem = REPLACE(b-tt-provisao.doc-origem, ";", ",").

                                IF dvv_log_provisao.origem_movto = "DVV MI" THEN DO:
                                    ASSIGN b-tt-provisao.nr-nota-fis  = dvv_log_prov_desp.nr_proc_exp
                                           b-tt-provisao.serie        = "6".
                                END.
                                ELSE DO:
                                
                                    ASSIGN b-tt-provisao.num-processo  = dvv_log_prov_desp.nr_proc_exp.


                                END.

                                for first es_param_estab no-lock
                                    where es_param_estab.cod_estabel = b-tt-provisao.cod-estabel
                                      and es_param_Estab.cod_estabel_trd ne "".        

                                    FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                                        WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel_trd
                                          AND processo-exp.nr-proc-exp = b-tt-provisao.num-processo,
                                        FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.

                                        ASSIGN b-tt-provisao.cod-emitente = processo-exp.cod-emitente
                                               b-tt-provisao.nome-abrev   = emitente.nome-abrev
                                               b-tt-provisao.regiao       = emitente.nome-mic-reg.                   
                                    END.    
                                    IF NOT AVAIL processo-exp THEN DO:
                                        FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                                            WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel
                                              AND processo-exp.nr-proc-exp = b-tt-provisao.num-processo,
                                            FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                    

                                            ASSIGN b-tt-provisao.cod-emitente = processo-exp.cod-emitente
                                                   b-tt-provisao.nome-abrev   = emitente.nome-abrev
                                                   b-tt-provisao.regiao       = emitente.nome-mic-reg.

                                        END.    
                                    END.
                                end.
                                if not avail es_param_estab then do:
                                    FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                                        WHERE processo-exp.cod-estabel = b-tt-provisao.cod-estabel
                                          AND processo-exp.nr-proc-exp = b-tt-provisao.num-processo,
                                        FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                                   
                                    END. 
                                end.

                                IF NOT AVAIL processo-exp THEN DO:
                                    FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                                        //WHERE processo-exp.cod-estabel = "001"
                                        WHERE processo-exp.nr-proc-exp = b-tt-provisao.num-processo
                                          AND processo-exp.cod-estabel <> "002",
                                        FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                                   
                                    END. 
                                END.

                                IF AVAIL processo-exp THEN DO:
                                    ASSIGN b-tt-provisao.cod-emitente = processo-exp.cod-emitente
                                           b-tt-provisao.nome-abrev   = emitente.nome-abrev
                                           b-tt-provisao.regiao       = emitente.nome-mic-reg.
                                    RUN pi-busca-data-receita-3.
                                END.
                                ELSE DO:
                                    IF dvv_log_provisao.origem_movto = "DVV MI" THEN DO:
                                        RUN pi-busca-data-receita-5.
                                    END.
                                
                                END.
                                    //RUN pi-busca-data-receita-2.

                                 /* Valores da despesa. */
                                assign b-tt-provisao.classif    = dvv_log_provisao.origem_movto
                                       b-tt-provisao.cod-desp   = dvv_log_prov_desp.cod_despesa
                                       b-tt-provisao.dt-prev    = dvv_log_provisao.dt_provisao
                                       b-tt-provisao.val-doc    = 0
                                       b-tt-provisao.vl-prev    = (dvv_log_prov_desp.vl_desp * tt-provisao.val-lancto) / d-tot-desp
                                       b-tt-provisao.vl-real    = (dvv_log_prov_desp.vl_desp * tt-provisao.val-lancto) / d-tot-desp.
                                       /*b-tt-rel-reg.dt-real     = 
                                       b-tt-rel-reg.vl-real     = IF v-val-desp[2] <> 0 THEN v-val-desp[2] ELSE dvv_movto_det.VALOR_REAL_R$.*/

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
                                    IF AVAIL tit_ap THEN DO:
                                       FIND emitente
                                            WHERE emitente.cod-emitente = tit_ap.cdn_fornecedor NO-LOCK.
                                        /* Dados do t°tulo APB. */
                                        assign b-tt-provisao.cdn-fornec    = tit_ap.cdn_fornecedor
                                               b-tt-provisao.nome-forn     = emitente.nome-abrev
                                               b-tt-provisao.cod-esp       = tit_ap.cod_espec_docto
                                               b-tt-provisao.cod-titulo    = tit_ap.cod_tit_ap
                                               b-tt-provisao.parcela       = tit_ap.cod_parcela.
                                    END.
                                END.
                            END.
                        END.
                        ELSE do:

                            ASSIGN d-tot-desp = 0.
                            FOR EACH  dvv_log_provisao NO-LOCK
                                WHERE dvv_log_provisao.num_lote_ctbl = ITEM_lancto_Ctbl.num_lote_ctbl
                                  AND dvv_log_provisao.cod_estabel   = ITEM_lancto_Ctbl.cod_estab,
                                EACH  dvv_log_prov_desp NO-LOCK
                                WHERE dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao.
                                
                                IF dvv_log_provisao.origem_movto = "EPDVV MI" THEN
                                    NEXT.
                                    
                                IF dvv_log_provisao.origem_movto = "EPDVV ME" THEN
                                    NEXT.
    
                                    
                                
                                ASSIGN d-tot-desp = d-tot-desp + dvv_log_prov_desp.vl_desp.
                            END.
                            FOR EACH  dvv_log_provisao NO-LOCK
                                WHERE dvv_log_provisao.num_lote_ctbl = ITEM_lancto_Ctbl.num_lote_ctbl
                                  AND dvv_log_provisao.cod_estabel   = ITEM_lancto_Ctbl.cod_estab,
                                EACH  dvv_log_prov_desp NO-LOCK
                                WHERE dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao.

                                /* N∆o provisiona */
                                /*IF dvv_log_provisao.num_lote_ctbl_estorno = 0 THEN
                                    NEXT.*/
                                    
                                IF dvv_log_provisao.origem_movto = "EPDVV MI" THEN
                                    NEXT.
                                    
                                IF dvv_log_provisao.origem_movto = "EPDVV ME" THEN
                                    NEXT.
                                    
                                /*    
                                IF dvv_log_prov_desp.nr_proc_exp = "0008231" THEN
                                DO:
                                    MESSAGE  dvv_log_prov_desp.num_id_provisao SKIP dvv_log_prov_desp.vl_desp SKIP dvv_log_provisao.origem_movto SKIP d-tot-desp VIEW-AS ALERT-BOX.
                                    
                                END.
                                */
                                
                                IF dvv_log_prov_desp.vl_desp = 0 THEN NEXT.   

                                create b-tt-provisao.
                                buffer-copy tt-provisao to b-tt-provisao.

                                ASSIGN i-seq-classif = i-seq-classif + 1.
                                ASSIGN b-tt-provisao.tipo-col   = "D"
                                       b-tt-provisao.movto      = "Provisionado"
                                       b-tt-provisao.val-lancto = 0
                                       /*b-tt-provisao.val-doc    = 0*/
                                       b-tt-provisao.seq-class  = i-seq-classif
                                       b-tt-provisao.l-rastr-desp  = YES
                                       b-tt-provisao.tp-origem = 14.

                                ASSIGN b-tt-provisao.doc-origem = replace(ITEM_lancto_ctbl.des_histor_lancto_ctbl, CHR(10), " ")
                                       b-tt-provisao.doc-origem = REPLACE(b-tt-provisao.doc-origem, ";", ",").

                                IF dvv_log_provisao.origem_movto = "DVV MI" THEN DO:
                                    ASSIGN b-tt-provisao.nr-nota-fis  = dvv_log_prov_desp.nr_proc_exp
                                           b-tt-provisao.serie = "6".
                                END.
                                ELSE 
                                    ASSIGN b-tt-provisao.num-processo  = dvv_log_prov_desp.nr_proc_exp.

                                for first es_param_estab no-lock
                                    where es_param_estab.cod_estabel = b-tt-provisao.cod-estabel
                                      and es_param_Estab.cod_estabel_trd ne "".        

                                    FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                                        WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel_trd
                                          AND processo-exp.nr-proc-exp = b-tt-provisao.num-processo,
                                        FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.


                                        ASSIGN b-tt-provisao.cod-emitente = processo-exp.cod-emitente
                                               b-tt-provisao.nome-abrev   = emitente.nome-abrev
                                               b-tt-provisao.regiao       = emitente.nome-mic-reg.                   
                                    END.    
                                    IF NOT AVAIL processo-exp THEN DO:
                                        FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                                            WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel
                                              AND processo-exp.nr-proc-exp = b-tt-provisao.num-processo,
                                            FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                    

                                            ASSIGN b-tt-provisao.cod-emitente = processo-exp.cod-emitente
                                                   b-tt-provisao.nome-abrev   = emitente.nome-abrev
                                                   b-tt-provisao.regiao       = emitente.nome-mic-reg.
                                        END.    
                                    END.
                                end.
                                if not avail es_param_estab then do:
                                    FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                                        WHERE processo-exp.cod-estabel = b-tt-provisao.cod-estabel
                                          AND processo-exp.nr-proc-exp = b-tt-provisao.num-processo,
                                        FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                                   

                                        ASSIGN b-tt-provisao.cod-emitente = processo-exp.cod-emitente
                                               b-tt-provisao.nome-abrev   = emitente.nome-abrev
                                               b-tt-provisao.regiao       = emitente.nome-mic-reg.

                                    END. 
                                end.

                                IF NOT AVAIL processo-exp THEN DO:
                                    FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                                        WHERE processo-exp.cod-estabel = "001"
                                          AND processo-exp.nr-proc-exp = b-tt-provisao.num-processo,
                                        FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.  

                                        ASSIGN b-tt-provisao.cod-emitente = processo-exp.cod-emitente
                                               b-tt-provisao.nome-abrev   = emitente.nome-abrev
                                               b-tt-provisao.regiao       = emitente.nome-mic-reg.

                                    END. 
                                END.

                                IF b-tt-provisao.nr-nota-fis = "0286875" THEN DO:
                                    PUT "1-" AVAIL processo-exp "-" dvv_log_provisao.origem_movto SKIP.
                                END.    
                                
                                IF AVAIL processo-exp THEN do:
                                    RUN pi-busca-data-receita-3.
                                END.
                                ELSE DO:
                                    IF dvv_log_provisao.origem_movto = "DVV MI" THEN DO:
                                        RUN pi-busca-data-receita-5.
                                    END.
                                
                                END.
                                    

                                 /* Valores da despesa. */
                                assign b-tt-provisao.classif    = dvv_log_provisao.origem_movto
                                       b-tt-provisao.cod-desp   = dvv_log_prov_desp.cod_despesa
                                       b-tt-provisao.dt-prev    = dvv_log_provisao.dt_provisao
                                       b-tt-provisao.vl-prev    = (dvv_log_prov_desp.vl_desp * tt-provisao.val-lancto) / d-tot-desp
                                       b-tt-provisao.vl-real    = (dvv_log_prov_desp.vl_desp * tt-provisao.val-lancto) / d-tot-desp.
                                       /*b-tt-rel-reg.dt-real     = 
                                       b-tt-rel-reg.vl-real     = IF v-val-desp[2] <> 0 THEN v-val-desp[2] ELSE dvv_movto_det.VALOR_REAL_R$.*/
                                       
                                /*       
                                IF dvv_log_prov_desp.nr_proc_exp = "0008231" THEN DO:
                                    MESSAGE b-tt-provisao.vl-real VIEW-AS ALERT-BOX.
                                END.
                                */

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
                                    IF AVAIL tit_ap THEN DO:

                                       FIND emitente
                                            WHERE emitente.cod-emitente = tit_ap.cdn_fornecedor NO-LOCK.
                                        /* Dados do t°tulo APB. */

                                        assign b-tt-provisao.cdn-fornec    = tit_ap.cdn_fornecedor
                                               b-tt-provisao.nome-forn     = emitente.nome-abrev
                                               b-tt-provisao.cod-esp       = tit_ap.cod_espec_docto
                                               b-tt-provisao.cod-titulo    = tit_ap.cod_tit_ap
                                               b-tt-provisao.parcela       = tit_ap.cod_parcela.
                                    END.
                                END.
                            END.
                        END.
                    END.


                    RELEASE bdvv_log_provisao.
                    RELEASE dvv_log_provisao.
                END.
            END.
        END.

END PROCEDURE.



PROCEDURE pi-monta-dvv-me:

/* 1. Busca as contas cont†beis que ser∆o utilizados no relat¢rio de acordo com a faixa parametrizada em tela. */
    FIND plano_cta_ctbl NO-LOCK WHERE
         plano_cta_ctbl.cod_plano_cta_ctbl      = "nitro"
     AND plano_cta_ctbl.ind_tip_plano_cta_ctbl = "Prim†rio"
     AND plano_cta_ctbl.dat_inic_valid         <= TODAY
     AND plano_cta_ctbl.dat_fim_valid          >= TODAY NO-ERROR.

    FOR EACH dvv_param_conta NO-LOCK WHERE
             dvv_param_conta.cod_estabel >= tt-param.c-estab-ini
         and dvv_param_conta.cod_estabel <= tt-param.c-estab-fim
         and substring(dvv_param_conta.conta_contabil,1,8) >= tt-param.c-cta-ctbl-ini
         AND substring(dvv_param_conta.conta_contabil,1,8) <= tt-param.c-cta-ctbl-fim:        

        FIND FIRST estabelecimento NO-LOCK
             WHERE estabelecimento.cod_estab = dvv_param_conta.cod_estabel NO-ERROR.

        IF NOT CAN-FIND(FIRST tt-cta-ctbl
                        WHERE tt-cta-ctbl.cod-conta   = SUBSTRING(dvv_param_conta.conta_contabil, 1, 8)
                          AND tt-cta-ctbl.cod-empresa = estabelecimento.cod_empresa
                          and tt-cta-ctbl.cod-estabel = estabelecimento.cod_estab) THEN DO:

            CREATE tt-cta-ctbl.
            ASSIGN tt-cta-ctbl.cod-conta     = SUBSTRING(dvv_param_conta.conta_contabil, 1, 8)
                   tt-cta-ctbl.cod-unid      = "000" /*SUBSTRING(dvv_param_conta.conta_contabil, 9, 3)*/
                   tt-cta-ctbl.cod-ccusto    = "" /*SUBSTRING(dvv_param_conta.conta_contabil, 12, 4)*/
                   tt-cta-ctbl.cod-estabel   = dvv_param_conta.cod_estabel
                   tt-cta-ctbl.cod-empresa   = estabelecimento.cod_empresa.
        END. 
        
    END.
    
    /* 2. Busca as movimentaá‰es do recebimento na tabela de movimento de estoque.
          Estas informaá‰es n∆o s∆o contabilzadas detalhadamente. */
    run pi-busca-dados-estoque.
    
    /*run pi-busca-dados-tms.*/    
    
    /* --- Encontrar cotacao entre Indic Base de Indic Apresentaá∆o ---*/
    run pi_retornar_indic_econ_finalid (Input cDolarVenda,
                                       Input today,
                                       output v_cod_indic_econ_base) /*pi_retornar_indic_econ_finalid*/.
    run pi_retornar_indic_econ_finalid (Input "Corrente",
                                       Input today,
                                       output v_cod_indic_econ_apres) /*pi_retornar_indic_econ_finalid*/.
    
    /* 3. Busca as movimentaá‰es das contas cont†beis. */
    FOR EACH tt-cta-ctbl
        WHERE tt-cta-ctbl.cod-estabel >= tt-param.c-estab-ini
          AND tt-cta-ctbl.cod-estabel <= tt-param.c-estab-fim:      

        FOR EACH ITEM_lancto_ctbl NO-LOCK WHERE
                 ITEM_lancto_Ctbl.cod_empresa        = tt-cta-ctbl.cod-empresa
             and item_lancto_ctbl.dat_lancto_ctbl   >= tt-param.dt-ctbl-ini
             and item_lancto_ctbl.dat_lancto_ctbl   <= tt-param.dt-ctbl-fim
             AND ITEM_lancto_ctbl.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl
             AND ITEM_lancto_ctbl.cod_cta_ctbl       = tt-cta-ctbl.cod-conta
             and ITEM_lancto_ctbl.cod_estab          = tt-cta-ctbl.cod-estabel
             //AND ITEM_lancto_ctbl.cod_estab         >= tt-param.c-estab-ini
             //AND ITEM_lancto_ctbl.cod_estab         <= tt-param.c-estab-fim
             AND ITEM_lancto_ctbl.cod_unid_negoc    >= tt-param.c-unid-negoc-ini
             AND ITEM_lancto_ctbl.cod_unid_negoc    <= tt-param.c-unid-negoc-fim
             //AND ITEM_lancto_ctbl.num_lote_ctbl =     236838 //194868
             //AND ITEM_lancto_ctbl.num_seq_lancto_ctbl = 126
            BREAK BY ITEM_lancto_ctbl.num_lote_ctbl
                  BY ITEM_lancto_ctbl.num_lancto_ctbl    
                  BY ITEM_lancto_ctbl.num_seq_lancto_ctbl:

            IF can-find(FIRST dvv_log_provisao NO-LOCK
                            WHERE dvv_log_provisao.num_lote_ctbl = ITEM_lancto_Ctbl.num_lote_ctbl
                              AND dvv_log_provisao.cod_estabel   = ITEM_lancto_Ctbl.cod_estab) THEN
                NEXT.

            FIND FIRST bdvv_log_provisao_estorno NO-LOCK WHERE 
                       bdvv_log_provisao_estorno.cod_estabel           = ITEM_lancto_Ctbl.cod_estab     AND
                       bdvv_log_provisao_estorno.num_lote_ctbl_estorno = ITEM_lancto_Ctbl.num_lote_ctbl NO-ERROR.

            IF AVAIL bdvv_log_provisao_estorno THEN
               NEXT.

            run pi-acompanhar in h-acomp("CT: " + tt-cta-ctbl.cod-conta + " Lote: " + string(ITEM_lancto_Ctbl.num_lote_ctbl) + " Data: " + STRING(ITEM_lancto_ctbl.dat_lancto_Ctbl)).

            IF FIRST-OF(ITEM_lancto_ctbl.num_lote_ctbl) THEN DO:

                FIND lote_ctbl NO-LOCK WHERE
                     lote_ctbl.num_lote_ctbl = ITEM_lancto_ctbl.num_lote_ctbl NO-ERROR.

            END.

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
            assign b-tt-rel-base.tipo      = "T"
                   b-tt-rel-base.movto     = "Realizado"
                   b-tt-rel-base.conta     = ITEM_lancto_ctbl.cod_cta_ctbl
                   b-tt-rel-base.modulo    = lote_ctbl.cod_modul_dtsul
                   b-tt-rel-base.dta-ctbz  = ITEM_lancto_ctbl.dat_lancto_ctbl
                   b-tt-rel-base.num-lote  = ITEM_lancto_ctbl.num_lote_ctbl
                   b-tt-rel-base.num-lancto = ITEM_lancto_ctbl.num_lancto_ctbl
                   b-tt-rel-base.num-seq-lancto = ITEM_lancto_ctbl.num_seq_lancto_ctbl
                   b-tt-rel-base.mov       = ITEM_lancto_ctbl.ind_natur_lancto_ctbl
                   b-tt-rel-base.moeda          = cRealVenda
                   b-tt-rel-base.cod-estabel    = ITEM_lancto_ctbl.cod_estab
                   b-tt-rel-base.dt-lote        = lote_ctbl.dat_ult_atualiz
                   b-tt-rel-base.user-lote      = lote_ctbl.cod_usuAr_ult_atualiz
                   b-tt-rel-base.cod-unid-negoc = ITEM_lancto_ctbl.cod_unid_negoc
                   b-tt-rel-base.cod-unid-negoc-gerenc = ITEM_lancto_ctbl.cod_unid_negoc
                   b-tt-rel-base.usuario-impl   = lote_ctbl.cod_usuAr_ult_atualiz.

            IF item_lancto_ctbl.cod_histor_padr = "DVVP" THEN
                ASSIGN b-tt-rel-base.movto     = "Provisionado".

            IF item_lancto_ctbl.cod_histor_padr = "DVVE" THEN
                ASSIGN b-tt-rel-base.movto     = "Estornado".

            /* O relatΩrio deverˇ sempre apresentar os valores em reais.
               Caso o indicador econÀmico seja diferente de Real buscar na apropriaªío o valor em reais. */
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
                WHEN "ACR" THEN RUN pi-dados-titulo-acr.
                when "CEP" then run pi-dados-rec (INPUT "Realizado",
                                                  INPUT lote_ctbl.cod_modul_dtsul,       
                                                  INPUT item_lancto_ctbl.cod_estab,      
                                                  INPUT item_lancto_ctbl.cod_cta_ctbl,   
                                                  INPUT item_lancto_ctbl.dat_lancto_ctbl,
                                                  INPUT ITEM_lancto_ctbl.cod_unid_negoc  
                                                  ).
                when "FGL" THEN do:

                    IF tt-param.dt-ctbl-ini < 01/01/2017 THEN
                        RUN pi-dados-tms.

                    ASSIGN b-tt-rel-base.doc-origem = replace(ITEM_lancto_ctbl.des_histor_lancto_ctbl, CHR(10), " ")
                           b-tt-rel-base.doc-origem = REPLACE(b-tt-rel-base.doc-origem, ";", ",").

                END.
                WHEN "CMG" THEN DO:
                    ASSIGN b-tt-rel-base.doc-origem = replace(ITEM_lancto_ctbl.des_histor_lancto_ctbl, CHR(10), " ")
                           b-tt-rel-base.doc-origem = REPLACE(b-tt-rel-base.doc-origem, ";", ",").
                END.
            end case.
        END.
    END.


END PROCEDURE.

PROCEDURE pi-monta-dvv-mi:

    RUN pi-acompanhar IN h-acomp (INPUT "Buscando dados DVV MI.").

    RUN dvv/dvv0199a.p(INPUT  TABLE tt-param,
                       INPUT-OUTPUT TABLE tt-rel,
                       INPUT-OUTPUT TABLE tt-rel-total).

END PROCEDURE.

PROCEDURE pi-busca-data-receita-2:

    FOR EACH  proc-nota-fiscal NO-LOCK
        WHERE proc-nota-fiscal.cod-estabel = processo-exp.cod-estabel
          AND proc-nota-fiscal.nr-proc-exp = processo-exp.nr-proc-exp,
        FIRST nota-fiscal NO-LOCK
          WHERE nota-fiscal.cod-estabel = proc-nota-fiscal.cod-estab-faturam
            AND nota-fiscal.serie       = proc-nota-fiscal.serie
            AND nota-fiscal.nr-nota-fis = proc-nota-fiscal.nr-nota-fis
            AND nota-fiscal.dt-cancela  = ?: 

        /*ASSIGN b-tt-rel-reg.serie        = nota-fiscal.serie       
               b-tt-rel-reg.nr-nota-fis  = nota-fiscal.nr-nota-fis .*/
    
        FIND FIRST es_nota_fiscal NO-LOCK
           WHERE es_nota_fiscal.cod_estabel = proc-nota-fiscal.cod-estab-faturam
             AND es_nota_fiscal.serie       = proc-nota-fiscal.serie
             AND es_nota_fiscal.nr_nota_fis = proc-nota-fiscal.nr-nota-fis NO-ERROR.
        IF AVAIL es_nota_fiscal THEN DO:

            IF es_nota_fiscal.dt_receita <> ? THEN DO:
                
                ASSIGN b-tt-rel-reg.dt-conf = es_nota_fiscal.dt_receita.
                LEAVE.

            END.
            ELSE ASSIGN b-tt-rel-reg.dt-conf = ?.
        END.
        ELSE ASSIGN b-tt-rel-reg.dt-conf = ?.   //NT Carlos Jr - incluida esta regra para atender ao Rios 21.05.24 chamado 0524-003623
    END.

END PROCEDURE.

PROCEDURE pi-busca-data-receita-3:

    FOR EACH  proc-nota-fiscal NO-LOCK
        WHERE proc-nota-fiscal.cod-estabel = processo-exp.cod-estabel
          AND proc-nota-fiscal.nr-proc-exp = processo-exp.nr-proc-exp,
        FIRST nota-fiscal NO-LOCK
          WHERE nota-fiscal.cod-estabel = proc-nota-fiscal.cod-estab-faturam
            AND nota-fiscal.serie       = proc-nota-fiscal.serie
            AND nota-fiscal.nr-nota-fis = proc-nota-fiscal.nr-nota-fis
            AND nota-fiscal.dt-cancela  = ?: 

        /*ASSIGN b-tt-rel-reg.serie        = nota-fiscal.serie       
               b-tt-rel-reg.nr-nota-fis  = nota-fiscal.nr-nota-fis .*/
    
        FIND FIRST es_nota_fiscal NO-LOCK
           WHERE es_nota_fiscal.cod_estabel = proc-nota-fiscal.cod-estab-faturam
             AND es_nota_fiscal.serie       = proc-nota-fiscal.serie
             AND es_nota_fiscal.nr_nota_fis = proc-nota-fiscal.nr-nota-fis NO-ERROR.
        IF AVAIL es_nota_fiscal THEN DO:

            IF es_nota_fiscal.dt_receita <> ? THEN DO:
                
                ASSIGN b-tt-provisao.dt-conf = es_nota_fiscal.dt_receita.
                LEAVE.

            END.
            ELSE ASSIGN b-tt-provisao.dt-conf = ?.
        END.
        ELSE ASSIGN b-tt-provisao.dt-conf = ?.   //NT Carlos Jr - incluida esta regra para atender ao Rios 21.05.24 chamado 0524-003623
    END.

END PROCEDURE.

PROCEDURE pi-busca-data-receita-5:

    //PUT "pi-busca - " b-tt-provisao.nr-nota-fis SKIP.


    FOR FIRST nota-fiscal NO-LOCK
          WHERE nota-fiscal.cod-estabel = b-tt-provisao.cod-estabel
            AND nota-fiscal.serie       = b-tt-provisao.serie
            AND nota-fiscal.nr-nota-fis = b-tt-provisao.nr-nota-fis
            AND nota-fiscal.dt-cancela  = ?
            : 

        /*ASSIGN b-tt-rel-reg.serie        = nota-fiscal.serie       
               b-tt-rel-reg.nr-nota-fis  = nota-fiscal.nr-nota-fis .*/
    
        FIND FIRST es_nota_fiscal NO-LOCK
           WHERE es_nota_fiscal.cod_estabel = nota-fiscal.cod-estabel
             AND es_nota_fiscal.serie       = nota-fiscal.serie
             AND es_nota_fiscal.nr_nota_fis = nota-fiscal.nr-nota-fis NO-ERROR.
        IF AVAIL es_nota_fiscal THEN DO:
            IF es_nota_fiscal.dt_receita <> ? THEN DO:
                
                ASSIGN b-tt-provisao.dt-conf = es_nota_fiscal.dt_receita.
                LEAVE.

            END.
            ELSE ASSIGN b-tt-provisao.dt-conf = nota-fiscal.dt-emis-nota.
        END.
        ELSE ASSIGN b-tt-provisao.dt-conf = nota-fiscal.dt-emis-nota.   //NT Carlos Jr - incluida esta regra para atender ao Rios 21.05.24 chamado 0524-003623
    END.

END PROCEDURE.

PROCEDURE pi-busca-data-receita-6:

    //PUT "pi-busca - " b-tt-provisao.nr-nota-fis SKIP.


    FOR FIRST nota-fiscal NO-LOCK
          WHERE nota-fiscal.cod-estabel = dvv_contabil.cod-estabel
            AND nota-fiscal.serie       = dvv_contabil.serie
            AND nota-fiscal.nr-nota-fis = dvv_contabil.nr-nota-fis
            AND nota-fiscal.dt-cancela  = ?
            : 

        /*ASSIGN b-tt-rel-reg.serie        = nota-fiscal.serie       
               b-tt-rel-reg.nr-nota-fis  = nota-fiscal.nr-nota-fis .*/
    
        FIND FIRST es_nota_fiscal NO-LOCK
           WHERE es_nota_fiscal.cod_estabel = nota-fiscal.cod-estabel
             AND es_nota_fiscal.serie       = nota-fiscal.serie
             AND es_nota_fiscal.nr_nota_fis = nota-fiscal.nr-nota-fis NO-ERROR.
        IF AVAIL es_nota_fiscal THEN DO:
            IF es_nota_fiscal.dt_receita <> ? THEN DO:
                
                ASSIGN tt-rel-item.dt-conf = es_nota_fiscal.dt_receita.
                LEAVE.

            END.
            ELSE ASSIGN tt-rel-item.dt-conf = nota-fiscal.dt-emis-nota.
        END.
        ELSE ASSIGN tt-rel-item.dt-conf = nota-fiscal.dt-emis-nota.   //NT Carlos Jr - incluida esta regra para atender ao Rios 21.05.24 chamado 0524-003623
    END.

END PROCEDURE.

PROCEDURE pi-busca-data-receita:


    FOR EACH  proc-nota-fiscal NO-LOCK
        WHERE proc-nota-fiscal.cod-estabel = processo-exp.cod-estabel
          AND proc-nota-fiscal.nr-proc-exp = processo-exp.nr-proc-exp,
        FIRST nota-fiscal NO-LOCK
          WHERE nota-fiscal.cod-estabel = proc-nota-fiscal.cod-estab-faturam
            AND nota-fiscal.serie       = proc-nota-fiscal.serie
            AND nota-fiscal.nr-nota-fis = proc-nota-fiscal.nr-nota-fis
            AND nota-fiscal.dt-cancela  = ?: 
    
        FIND FIRST es_nota_fiscal NO-LOCK
           WHERE es_nota_fiscal.cod_estabel = proc-nota-fiscal.cod-estab-faturam
             AND es_nota_fiscal.serie       = proc-nota-fiscal.serie
             AND es_nota_fiscal.nr_nota_fis = proc-nota-fiscal.nr-nota-fis NO-ERROR.
        IF AVAIL es_nota_fiscal THEN DO:


            IF es_nota_fiscal.dt_receita <> ? THEN DO:
                
                ASSIGN b-tt-rel-reg.dt-conf = es_nota_fiscal.dt_receita.

                
                LEAVE.

            END.
            ELSE ASSIGN b-tt-rel-reg.dt-conf = ?.
        END.
        ELSE ASSIGN b-tt-rel-reg.dt-conf = ?.   //NT Carlos Jr - incluida esta regra para atender ao Rios 21.05.24 chamado 0524-003623
    END.

END PROCEDURE.

PROCEDURE pi-quebra-item:
    DEF VAR h-bocx185      AS HANDLE NO-UNDO.
    DEF VAR i-cod-estabel  AS CHAR NO-UNDO.
    DEF VAR d-total-rateio AS DECIMAL NO-UNDO.

    /*** FAZER QUEBRA POR ITEM ***/

    RUN pi-acompanhar in h-acomp("Quebra por item.").

    FOR EACH tt-rel
        BREAK 
        BY tt-rel.num-lote
        BY tt-rel.num-lancto
        BY tt-rel.num-seq-lancto
        //BY tt-rel.doc-origem
        :

        IF tt-rel.nr-nota-fis = "" AND tt-rel.num-processo = "" THEN DO:
            CREATE tt-rel-item.
            BUFFER-COPY tt-rel TO tt-rel-item.
            /* ASSIGN tt-rel-item.cod-estab-orig = tt-rel-item.cod-estabel. // todo */ //NT Carlos Jr - regra substituida 5/4/24
            IF tt-rel-item.cod-estabel <> "002" then ASSIGN tt-rel-item.cod-estab-orig = tt-rel-item.cod-estabel.
            ELSE DO:
                RUN pi-estab-br(INPUT tt-rel-item.nome-abrev, INPUT tt-rel-item.num-processo).
                assign tt-rel-item.cod-estab-orig = c-cod-estabel-br.
            END.
        END.

        /********** BUSCA DADOS DOS ITENS DA NOTA FISCAL *********/
        IF tt-rel.nr-nota-fis <> "" THEN DO:
            
            FIND FIRST nota-fiscal NO-LOCK
              WHERE nota-fiscal.cod-estabel = tt-rel.cod-estabel
                AND nota-fiscal.serie       = tt-rel.serie
                AND nota-fiscal.nr-nota-fis = tt-rel.nr-nota-fis NO-ERROR.
            IF AVAIL nota-fiscal THEN DO:
            
                FIND FIRST dvv_movto_mi WHERE         
                           dvv_movto_mi.cod_estabel  = nota-fiscal.cod-estabel AND
                           dvv_movto_mi.serie        = nota-fiscal.serie AND
                           dvv_movto_mi.nr-nota-fis  = nota-fiscal.nr-nota-fis AND
                           dvv_movto_mi.tipo_despesa = tt-rel.cod-desp NO-LOCK NO-ERROR.

                //alterado o codigo para buscar o emitente pelo cod-emitente quando n∆o encontrar pelo nome-abrev
                //tivemos casos onde o nome-abrev estava com acento e no cd0704 estava sem e n∆o encontrava.
                FIND emitente WHERE
                     emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.
                IF NOT AVAIL emitente THEN
                    FOR FIRST emitente WHERE
                              emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK:
                    END.
   

                ASSIGN tt-rel.cod-emitente = emitente.cod-emitente
                       tt-rel.nome-abrev   = emitente.nome-abrev
                       //aqui tt-rel.dt-conf      = nota-fiscal.dt-emis-nota - NT Carlos Jr - retirada esta regra por solicitaá∆o do Rios 21.05.24 chamado 0524-003623
                       .                   
                  

                FIND canal-venda NO-LOCK
                    WHERE canal-venda.cod-canal-venda = emitente.cod-canal-venda NO-ERROR.
                IF AVAIL canal-venda THEN
                    ASSIGN tt-rel.segmento = STRING(emitente.cod-canal-venda) + canal-venda.descricao.
    
                FIND repres NO-LOCK WHERE
                     repres.cod-rep = emitente.cod-rep NO-ERROR.
                IF AVAIL repres THEN
                    FIND regiao NO-LOCK WHERE
                         regiao.nome-ab-reg = repres.nome-ab-reg NO-ERROR.
                IF AVAIL regiao THEN
                    ASSIGN tt-rel.regiao       = regiao.nome-regiao.

                IF AVAIL dvv_movto_mi THEN DO:
                    FIND b-emit-fornec WHERE
                         b-emit-fornec.cod-emitente = dvv_movto_mi.cod_fornecedor NO-LOCK NO-ERROR.

                    ASSIGN tt-rel.cdn-fornec = IF AVAIL b-emit-fornec THEN b-emit-fornec.cod-emitente ELSE 0
                           tt-rel.nome-forn  = IF AVAIL b-emit-fornec THEN b-emit-fornec.nome-abrev ELSE "".
                END.
                ASSIGN d-total-rateio = 0.

                
                /*
                ASSIGN d-total-qt = 0.
                FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
                    ASSIGN d-total-qt = d-total-qt + it-nota-fisc.qt-faturada[1].
                END.
                */
                FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK 
                    BY it-nota-fisc.it-codigo:

                    CREATE tt-rel-item.
                    BUFFER-COPY tt-rel TO tt-rel-item.
                    /* ASSIGN tt-rel-item.cod-estab-orig = tt-rel-item.cod-estabel. // todo */ //NT Carlos Jr - regra substituida 5/4/24
                    IF tt-rel-item.cod-estabel <> "002" then ASSIGN tt-rel-item.cod-estab-orig = tt-rel-item.cod-estabel.
                    ELSE DO:
                        RUN pi-estab-br(INPUT tt-rel-item.nome-abrev, INPUT tt-rel-item.num-processo).
                        assign tt-rel-item.cod-estab-orig = c-cod-estabel-br.
                    END.

                    run pi-acompanhar in h-acomp("Item: " + STRING(it-nota-fisc.it-codigo)).  /* Valdinei */

                    ASSIGN tt-rel-item.it-codigo    = it-nota-fisc.it-codigo
                           tt-rel-item.vl-real-item = it-nota-fisc.vl-tot-item
                           tt-rel-item.vl-real-orig = tt-rel.vl-real
                           tt-rel-item.vl-real      = (tt-rel.vl-real * it-nota-fisc.vl-tot-item) / nota-fiscal.vl-tot-nota
                           tt-rel-item.vl-base      = nota-fiscal.vl-tot-nota
                           tt-rel-item.vl-prev-item = (tt-rel.vl-prev * it-nota-fisc.vl-tot-item) / nota-fiscal.vl-tot-nota   
                           tt-rel-item.vl-real-tot-item = (tt-rel.vl-real-tot * it-nota-fisc.vl-tot-item) / nota-fiscal.vl-tot-nota
                           tt-rel-item.cod-unid-negoc-gerenc = IF tt-rel.cod-unid-negoc-gerenc = "000" THEN it-nota-fisc.cod-unid-negoc ELSE tt-rel.cod-unid-negoc-gerenc
                           tt-rel-item.qt-faturada  = it-nota-fisc.qt-faturada[1]
                           tt-rel-item.un-fatur     = it-nota-fisc.un-fatur[1].

                    ASSIGN d-total-rateio = d-total-rateio + tt-rel-item.vl-real.
                    
                    IF  tt-rel-item.un-fatur = "" THEN DO:
                        FIND item OF it-nota-fisc NO-LOCK NO-ERROR.
                        IF  AVAIL item THEN
                            ASSIGN tt-rel-item.un-fatur = item.un. // Unid.Medida
                    END.
                END.
                IF tt-rel.vl-real <> d-total-rateio THEN
                    ASSIGN tt-rel-item.vl-real = tt-rel-item.vl-real + (tt-rel.vl-real - d-total-rateio).

            END. 
        END.
        ELSE DO:

            IF tt-rel.num-processo = "" THEN
                NEXT.

            EMPTY TEMP-TABLE tt-valores.

            ASSIGN i-cod-estabel = tt-rel.cod-estabel.

            IF NOT CAN-FIND(FIRST proc-ped-ent
                                WHERE proc-ped-ent.cod-estabel = i-cod-estabel
                                  AND proc-ped-ent.nr-proc-exp = tt-rel.num-processo) THEN DO:
                for first es_param_estab no-lock
                    where es_param_estab.cod_estabel = i-cod-estabel
                      and es_param_Estab.cod_estabel_trd ne "":
                      ASSIGN i-cod-estabel = "002".
                      
                END.   
                IF NOT AVAIL es_param_estab THEN 
                    ASSIGN i-cod-estabel = "001".
                    
                /*                      
                                  
                IF tt-rel.cod-estabel = "001" OR tt-rel.cod-estabel = "101" THEN
                    ASSIGN i-cod-estabel = "002".
                ELSE 
                    ASSIGN i-cod-estabel = "001".
                */    
            end.            
            //ASSIGN i-cod-estabel = IF tt-rel.cod-estabel = "001" THEN "002" ELSE "001".

            IF NOT CAN-FIND(FIRST proc-ped-ent
                                WHERE proc-ped-ent.cod-estabel = i-cod-estabel
                                  AND proc-ped-ent.nr-proc-exp = tt-rel.num-processo) THEN DO:

                CREATE tt-rel-item.
                BUFFER-COPY tt-rel TO tt-rel-item.
                /* ASSIGN tt-rel-item.cod-estab-orig = tt-rel-item.cod-estabel. // todo */ //NT Carlos Jr - regra substituida 5/4/24
                IF tt-rel-item.cod-estabel <> "002" then ASSIGN tt-rel-item.cod-estab-orig = tt-rel-item.cod-estabel.
                ELSE DO:
                    RUN pi-estab-br(INPUT tt-rel-item.nome-abrev, INPUT tt-rel-item.num-processo).
                    assign tt-rel-item.cod-estab-orig = c-cod-estabel-br.
                END.
                NEXT.
            END.

            CREATE tt-valores.
            ASSIGN tt-valores.cod-estabel = i-cod-estabel
                   tt-valores.nr-proc-exp = tt-rel.num-processo.

            RUN cxbo/bocx185.p PERSISTENT SET h-bocx185.

            RUN UpdatePriceWeight IN h-bocx185 (INPUT-OUTPUT TABLE tt-valores).
        
            DELETE PROCEDURE h-bocx185.
        
            FIND FIRST tt-valores NO-LOCK NO-ERROR.
            /*
            d-total-qt = 0.
            FOR EACH proc-ped-ent NO-LOCK
              WHERE proc-ped-ent.cod-estabel = i-cod-estabel
                AND proc-ped-ent.nr-proc-exp = tt-rel.num-processo
                BY proc-ped-ent.it-codigo:
                
                ASSIGN d-total-qt = d-total-qt +  proc-ped-ent.quantidade[1].
                
            END.
            */
            

            ASSIGN d-total-rateio = 0.
            FOR EACH proc-ped-ent NO-LOCK
              WHERE proc-ped-ent.cod-estabel = i-cod-estabel
                AND proc-ped-ent.nr-proc-exp = tt-rel.num-processo
                BY proc-ped-ent.it-codigo:

                CREATE tt-rel-item.
                BUFFER-COPY tt-rel TO tt-rel-item.
                /* ASSIGN tt-rel-item.cod-estab-orig = tt-rel-item.cod-estabel. // todo */ //NT Carlos Jr - regra substituida 5/4/24
                IF tt-rel-item.cod-estabel <> "002" then ASSIGN tt-rel-item.cod-estab-orig = tt-rel-item.cod-estabel.
                ELSE DO:
                    RUN pi-estab-br(INPUT tt-rel-item.nome-abrev, INPUT tt-rel-item.num-processo).
                    assign tt-rel-item.cod-estab-orig = c-cod-estabel-br.
                END.

                ASSIGN tt-rel-item.it-codigo        = proc-ped-ent.it-codigo  /* valdinei */
                       tt-rel-item.vl-real-item     = proc-ped-ent.vl-preuni * proc-ped-ent.quantidade[1]
                       tt-rel-item.vl-real-orig     = tt-rel.vl-real
                       tt-rel-item.vl-real          = (tt-rel-item.vl-real-item * tt-rel.vl-real) / tt-valores.vl-tot-liq
                       tt-rel-item.vl-base          = tt-valores.vl-tot-liq
                       tt-rel-item.vl-prev-item     = (tt-rel.vl-prev * tt-rel-item.vl-real-item) / tt-valores.vl-tot-liq   
                       tt-rel-item.vl-real-tot-item = (tt-rel.vl-real-tot * tt-rel-item.vl-real-item) / tt-valores.vl-tot-liq
                       tt-rel-item.qt-faturada      = proc-ped-ent.quantidade[1]
                       tt-rel-item.un-fatur         = proc-ped-ent.un-fatur.
                       /*tt-rel-item.dt-conf        = nota-fiscal.dt-emis-nota*/

                ASSIGN d-total-rateio = d-total-rateio + tt-rel-item.vl-real.
                FOR FIRST ped-item OF proc-ped-ent NO-LOCK:
                    ASSIGN tt-rel-item.cod-unid-negoc-gerenc = IF tt-rel.cod-unid-negoc-gerenc = "000" THEN ped-item.cod-unid-negoc ELSE tt-rel.cod-unid-negoc-gerenc .
                END.

                IF  tt-rel-item.un-fatur = "" THEN DO:
                    FIND item OF proc-ped-ent NO-LOCK NO-ERROR.
                    IF  AVAIL item THEN
                        ASSIGN tt-rel-item.un-fatur = item.un. // Unid.Medida
                END.
            END.


            IF tt-rel.vl-real <> d-total-rateio THEN
                ASSIGN tt-rel-item.vl-real = tt-rel-item.vl-real + (tt-rel.vl-real - d-total-rateio).

 


        END.
    END.

END PROCEDURE.

PROCEDURE pi-rateio-despesa:
    DEF VAR i-cod-itiner AS INT NO-UNDO.
    DEF VAR de-total-val-doc AS DEC NO-UNDO.
    DEF VAR de-total-vl-tot-real AS DEC NO-UNDO.
    
    for each tt-rel
        BREAK 
        //BY tt-rel.doc-origem
        BY tt-rel.num-lote
        :
        IF tt-rel.movto = "realizado" OR tt-rel.movto = "previsto" THEN DO:

            ASSIGN de-total-val-doc     = 0
                   de-total-vl-tot-real = 0.
            /****** TICKET 374 - ratear o total entre as despesas ****/
            IF tt-rel.tp-origem = 1 /*"Mediá∆o"*/ THEN DO:

                FOR EACH tt-rel-total no-lock
                   where tt-rel-total.cod-estabel    = tt-rel.cod-estabel   
                     and tt-rel-total.tipo           = tt-rel.tipo          
                     and tt-rel-total.conta          = tt-rel.conta         
                     and tt-rel-total.modulo         = tt-rel.modulo        
                     and tt-rel-total.num-lote       = tt-rel.num-lote      
                     and tt-rel-total.num-lancto     = tt-rel.num-lancto    
                     and tt-rel-total.num-seq-lancto = tt-rel.num-seq-lancto
                     and tt-rel-total.tp-origem      = tt-rel.tp-origem     
                     and tt-rel-total.doc-origem     = tt-rel.doc-origem   :
                    ASSIGN de-total-val-doc     = de-total-val-doc + tt-rel-total.val-doc
                           de-total-vl-tot-real = de-total-vl-tot-real + tt-rel-total.vl-tot-real.
                END.

                IF de-total-val-doc > 0 THEN DO:
                    ASSIGN d-vl-real = ROUND((de-total-val-doc * (tt-rel.vl-real / de-total-vl-tot-real)),2).
                END.
                ELSE DO:
                    ASSIGN d-vl-real = tt-rel.vl-real.
                END.

            END.
            ELSE DO:

                    IF l-log THEN DO:
                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                        IF INDEX(tt-rel.doc-origem,"138907") > 0 THEN
                            PUT UNFORMATTED "teste rateio tt-rel.tp-origem " string(tt-rel.tp-origem) " | tt-rel.doc-origem " tt-rel.doc-origem SKIP.
                        OUTPUT CLOSE.
                    END.

                    FIND FIRST tt-rel-total no-lock
                       where tt-rel-total.cod-estabel    = tt-rel.cod-estabel   
                         and tt-rel-total.tipo           = tt-rel.tipo          
                         and tt-rel-total.conta          = tt-rel.conta         
                         and tt-rel-total.modulo         = tt-rel.modulo        
                         and tt-rel-total.num-lote       = tt-rel.num-lote      
                         and tt-rel-total.num-lancto     = tt-rel.num-lancto    
                         and tt-rel-total.num-seq-lancto = tt-rel.num-seq-lancto
                         and tt-rel-total.tp-origem      = tt-rel.tp-origem     
                         and tt-rel-total.doc-origem     = tt-rel.doc-origem NO-ERROR.

                    if avail tt-rel-total then do:

                    ASSIGN d-vl-real = ROUND((tt-rel-total.val-doc /*de-total-val-doc*/ * (tt-rel.vl-real / tt-rel-total.vl-tot-real)),2).

                    IF l-log THEN DO:
                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                        IF INDEX(tt-rel.doc-origem,"138907") > 0 THEN
                            PUT UNFORMATTED " pi-rateio-despesa1 tt-rel.doc-origem " tt-rel.doc-origem " tt-rel.vl-real " tt-rel.vl-real " tt-rel-total.vl-tot-real " tt-rel-total.vl-tot-real " d-vl-real " d-vl-real SKIP.
                        OUTPUT CLOSE.
                    END.
      
                end.
                ELSE DO:

                    ASSIGN d-vl-real = tt-rel.vl-real.

                    IF l-log THEN DO:
                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                        IF INDEX(tt-rel.doc-origem,"138907") > 0 THEN
                            PUT UNFORMATTED " pi-rateio-despesa2 tt-rel.doc-origem " tt-rel.doc-origem " tt-rel.vl-real " tt-rel.vl-real " d-vl-real " d-vl-real SKIP.
                        OUTPUT CLOSE.
                    END.

                END.
            END.
    
            ASSIGN tt-rel.vl-real = d-vl-real.

        END.

        IF tt-rel.tipo-col = "D" THEN DO:

            find first emitente no-lock
               where emitente.cod-emitente = tt-rel.cod-emitente no-error.
            if avail emitente AND emitente.cod-emitente <> 0 then do:
    
                FIND canal-venda NO-LOCK
                    WHERE canal-venda.cod-canal-venda = emitente.cod-canal-venda NO-ERROR.
                IF AVAIL canal-venda THEN
                    ASSIGN tt-rel.segmento = STRING(emitente.cod-canal-venda) + canal-venda.descricao.
    
                FIND repres NO-LOCK WHERE
                     repres.cod-rep = emitente.cod-rep NO-ERROR.
                IF AVAIL repres THEN
                    FIND regiao NO-LOCK WHERE
                         regiao.nome-ab-reg = repres.nome-ab-reg NO-ERROR.
                IF AVAIL regiao THEN
                    ASSIGN tt-rel.regiao       = regiao.nome-regiao.
    
            end.

            /*
            ** Descriá∆o da despesa
            */
            IF tt-rel.classif = "DVV MI"
            OR tt-rel.classif = "PDVV MI"
            OR tt-rel.classif = "DVV ME"
            OR tt-rel.classif = "PDVV ME"
            OR tt-rel.classif = "EPDVV MI"
            OR tt-rel.classif = "EPDVV ME"
            OR tt-rel.classif = "DVVDEX"
            THEN DO:

                for first DVV_TIPO_DESPESA no-lock where
                          DVV_TIPO_DESPESA.codigo = tt-rel.cod-desp:
                    ASSIGN tt-rel.des-desp = DVV_TIPO_DESPESA.descricao.
                end.

            END.
            ELSE DO:
            
                for first DEX_TP_DESPESA no-lock where
                          DEX_TP_DESPESA.codigo = tt-rel.cod-Desp:
                    assign tt-rel.des-desp = DEX_TP_DESPESA.descricao.
                end.

            END.

            IF NOT tt-rel.l-rastr-desp THEN DO:
                ASSIGN tt-rel.vl-real = tt-rel.val-doc.
                IF INDEX(tt-rel.doc-origem,"RM") > 0 THEN
                    ASSIGN tt-rel.tp-origem = 12.
            END.

        END.
        ELSE DO:
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
                ASSIGN tt-rel.l-rastr-desp  = NO.
                IF tt-rel.MODULO = "FGL" THEN
                       tt-rel.tp-origem = 11. /* Manual */
                IF INDEX(tt-rel.doc-origem,"RM") > 0 THEN
                    ASSIGN tt-rel.tp-origem = 12.
            END.
            ELSE DO:
                ASSIGN tt-rel.l-rastr-desp = YES.
            END.

            IF NOT CAN-FIND(FIRST b-tt-rel-reg WHERE
                                  b-tt-rel-reg.num-lote       = tt-rel.num-lote
                              AND b-tt-rel-reg.num-lancto     = tt-rel.num-lancto
                              AND b-tt-rel-reg.num-seq-lancto = tt-rel.num-seq-lancto
                              AND b-tt-rel-reg.tipo-col       = "D") THEN DO:

                ASSIGN tt-rel.vl-real = tt-rel.val-lancto
                       tt-rel.val-doc = tt-rel.val-lancto.

            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pi-ger-remessa-estoque:
    /*
    ** RtDvvDex - FUI
    */
    DEF VAR v-dat AS DATE NO-UNDO.

    IF tt-param.l-remessa-venda-estoq = YES THEN DO:

        DO v-dat = tt-param.dt-ctbl-ini TO tt-param.dt-ctbl-fim:

            FOR EACH dvv_contabil NO-LOCK
                WHERE dvv_contabil.dta-ctbz = v-dat
                  AND dvv_contabil.movto    = "RtDvvDex"
                  AND dvv_contabil.conta   >= tt-param.c-cta-ctbl-ini
                  AND dvv_contabil.conta   <= tt-param.c-cta-ctbl-fim
                  AND dvv_contabil.cod-estabel >= tt-param.c-estab-ini
                  AND dvv_contabil.cod-estabel <= tt-param.c-estab-fim:
                  
    
                CREATE tt-rel-item.
                BUFFER-COPY dvv_contabil TO tt-rel-item.
                /* ASSIGN tt-rel-item.cod-estab-orig = tt-rel-item.cod-estabel. // todo */ //NT Carlos Jr - regra substituida 5/4/24
                IF tt-rel-item.cod-estabel <> "002" then ASSIGN tt-rel-item.cod-estab-orig = tt-rel-item.cod-estabel.
                ELSE DO:
                    RUN pi-estab-br(INPUT tt-rel-item.nome-abrev, INPUT tt-rel-item.num-processo).
                    assign tt-rel-item.cod-estab-orig = c-cod-estabel-br.
                END.
    
                ASSIGN tt-rel-item.MODULO         = "DVV/DEX"
                       tt-rel-item.val-lancto     = tt-rel-item.vl-real
                       tt-rel-item.val-doc        = tt-rel-item.vl-real
                       tt-rel-item.cod-depos      = dvv_contabil.char-1
                       tt-rel-item.regra-rateio   = INT(dvv_contabil.vl-prev)
                       tt-rel-item.cod-unid-negoc = STRING(TRIM(dvv_contabil.char-2))
                       tt-rel-item.cod-unid-negoc-gerenc = STRING(TRIM(dvv_contabil.char-2)).

            END.
        END.

    END.

END PROCEDURE.

PROCEDURE pi-ger-rtnrast:
    /*
    ** RtNRast - FUI
    */
    DEF VAR v-dat AS DATE NO-UNDO.

    IF tt-param.l-despesa-nao-rast = YES THEN DO:

        DO v-dat = tt-param.dt-ctbl-ini TO tt-param.dt-ctbl-fim:

            FOR EACH dvv_contabil NO-LOCK
                WHERE dvv_contabil.dta-ctbz = v-dat
                  AND dvv_contabil.conta   >= tt-param.c-cta-ctbl-ini
                  AND dvv_contabil.conta   <= tt-param.c-cta-ctbl-fim
                  AND dvv_contabil.cod-estabel >= tt-param.c-estab-ini
                  AND dvv_contabil.cod-estabel <= tt-param.c-estab-fim:
                       
                IF dvv_contabil.movto <> "RtNRastr" AND dvv_contabil.movto <> "RtNRastP" THEN NEXT.

                CREATE tt-rel-item.
                BUFFER-COPY dvv_contabil TO tt-rel-item.
                /* ASSIGN tt-rel-item.cod-estab-orig = tt-rel-item.cod-estabel. // todo */ //NT Carlos Jr - regra substituida 5/4/24
                IF tt-rel-item.cod-estabel <> "002" then ASSIGN tt-rel-item.cod-estab-orig = tt-rel-item.cod-estabel.
                ELSE DO:
                    RUN pi-estab-br(INPUT tt-rel-item.nome-abrev, INPUT tt-rel-item.num-processo).
                    assign tt-rel-item.cod-estab-orig = c-cod-estabel-br.
                END.
    
                ASSIGN tt-rel-item.MODULO       = "N∆o Rast"
                       tt-rel-item.val-lancto   = tt-rel-item.vl-real 
                       tt-rel-item.val-doc      = tt-rel-item.vl-real
                       tt-rel-item.cod-depos    = dvv_contabil.char-1
                       tt-rel-item.regra-rateio = INT(dvv_contabil.vl-prev). /* Grava o codigo da regra utilizada */
                       tt-rel-item.cod-unid-negoc = STRING(TRIM(dvv_contabil.char-2)).
                ASSIGN tt-rel-item.cod-unid-negoc-gerenc = STRING(TRIM(dvv_contabil.char-2)).

                IF tt-rel-item.num-processo <> "" THEN
                    ASSIGN tt-rel-item.l-rastr-desp = YES.

                    
                IF tt-rel-item.classif = "DVV MI" AND tt-rel-item.dt-conf = ? THEN
                    RUN pi-busca-data-receita-6.    
                    
                    
                         //aqui aaa
                FIND FIRST DVV_TIPO_DESPESA NO-LOCK
                     WHERE DVV_TIPO_DESPESA.codigo = tt-rel-item.cod-desp NO-ERROR.
                IF AVAIL DVV_TIPO_DESPESA THEN
                    ASSIGN tt-rel-item.des-desp = DVV_TIPO_DESPESA.descricao NO-ERROR. 

                IF tt-rel-item.classif = "DEX" THEN
                    for first DEX_TP_DESPESA no-lock where
                              DEX_TP_DESPESA.codigo = tt-rel-item.cod-Desp:
                        assign tt-rel-item.des-desp = DEX_TP_DESPESA.descricao.
                    end.
                    
                    
                /*
                /* Acumula valor para conferir o valor rateado com o base */
                IF tt-rel-item.it-codigo <> "" THEN DO:

                    FIND FIRST tt-acumula NO-LOCK
                        WHERE tt-acumula.conta        = dvv_contabil.conta      
                          AND tt-acumula.natureza     = dvv_contabil.mov  
                          AND tt-acumula.valor-base   = dvv_contabil.vl-base NO-ERROR.
                    IF NOT AVAIL tt-acumula THEN DO:
            
                        CREATE tt-acumula.
                        ASSIGN tt-acumula.cod-estabel  = dvv_contabil.cod-estabel
                               tt-acumula.conta        = dvv_contabil.conta      
                               tt-acumula.natureza     = dvv_contabil.mov        
                               tt-acumula.valor-base   = dvv_contabil.vl-base
                               tt-acumula.valor-rat    = dvv_contabil.vl-real.
                    END.
                    ELSE DO:
                        ASSIGN tt-acumula.valor-rat = tt-acumula.valor-rat + dvv_contabil.vl-real.
                    END.
                END.
                */
            END.
        END.
    END.

    /* 
    /* Confere valores rateados */
    FOR EACH tt-acumula NO-LOCK:

        
        IF tt-acumula.valor-base > tt-acumula.valor-rat THEN DO:

            /* Acumula valor para conferir o valor rateado com o base */
            FIND FIRST tt-rel-item NO-LOCK
                WHERE  tt-rel-item.conta       = tt-acumula.conta
                  AND  tt-rel-item.mov         = tt-acumula.natureza
                  AND  tt-rel-item.vl-base     = tt-acumula.valor-base    NO-ERROR.
            IF AVAIL tt-rel-item THEN DO:

                ASSIGN tt-rel-item.vl-real     = tt-rel-item.vl-real + (tt-acumula.valor-base - tt-acumula.valor-rat)
                       tt-rel-item.val-lancto  = tt-rel-item.vl-real
                       tt-rel-item.val-doc     = tt-rel-item.vl-real.

                
            END.
        END.
        ELSE DO:

            

            IF tt-acumula.valor-rat > tt-acumula.valor-base THEN DO:

                /* Acumula valor para conferir o valor rateado com o base */
                FIND FIRST tt-rel-item NO-LOCK
                    WHERE  tt-rel-item.conta       = tt-acumula.conta
                      AND  tt-rel-item.mov         = tt-acumula.natureza
                      AND  tt-rel-item.vl-base     = tt-acumula.valor-base    NO-ERROR.
                IF AVAIL tt-rel-item THEN DO:

                    IF l-log 
                    AND tt-rel-item.num-processo = "16766" THEN DO:
                        OUTPUT TO c:\temp\dvv0199log.txt APPEND.
                        PUT UNFORMAT "Jeremias 1 -------------- " + STRING(TIME,"HH:MM:SS") SKIP.
                        EXPORT DELIMITER ";" tt-acumula .
                        EXPORT DELIMITER ";" tt-rel-item .
                        OUTPUT CLOSE.
                    END.

                    IF  tt-rel-item.num-processo = "16766" THEN
                          MESSAGE "antes" SKIP
                                  " tt-rel-item.nr-nota-fis = " tt-rel-item.nr-nota-fis   SKIP
                                  " tt-rel-item.vl-real    = "  tt-rel-item.vl-real       skip
                                  " tt-rel-item.val-lancto = "  tt-rel-item.val-lancto    skip
                                  " tt-rel-item.val-doc    = "  tt-rel-item.val-doc       skip
                              VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    ASSIGN tt-rel-item.vl-real     = tt-rel-item.vl-real - (tt-acumula.valor-rat - tt-acumula.valor-base)
                           tt-rel-item.val-lancto  = tt-rel-item.vl-real
                           tt-rel-item.val-doc     = tt-rel-item.vl-real.

                    IF  tt-rel-item.num-processo = "16766" THEN
                          MESSAGE "depois" SKIP
                                  " tt-rel-item.nr-nota-fis = " tt-rel-item.nr-nota-fis   SKIP
                                  " tt-rel-item.vl-real    = "  tt-rel-item.vl-real       skip
                                  " tt-rel-item.val-lancto = "  tt-rel-item.val-lancto    skip
                                  " tt-rel-item.val-doc    = "  tt-rel-item.val-doc       skip
                              VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    IF l-log 
                    AND tt-rel-item.num-processo = "16766" THEN DO:
                        OUTPUT TO c:\temp\dvv0199log.txt APPEND.
                        PUT UNFORMAT "Jeremias 2 -------------- " + STRING(TIME,"HH:MM:SS") SKIP.
                        EXPORT DELIMITER ";" tt-acumula .
                        EXPORT DELIMITER ";" tt-rel-item .
                        OUTPUT CLOSE.
                    END.

                    IF tt-rel-item.val-doc = 6229.357 
                    OR tt-rel-item.val-doc = -6229.357 THEN
                        MESSAGE "Jeremias - acumula 2" SKIP
                                "tt-rel-item.vl-real     = " tt-rel-item.vl-real    skip
                                "tt-rel-item.val-lancto  = " tt-rel-item.val-lancto skip
                                "tt-rel-item.val-doc     = " tt-rel-item.val-doc    skip
                                "tt-rel-item.val-doc = " tt-rel-item.val-doc
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
                END.
            END.
        END.
    END.
    */

END PROCEDURE.

PROCEDURE pi-imprime-bi:
/*     /*                                                              */
/*     ** Imprimi BI                                                   */
/*     */                                                              */
/*     DEF VAR v-dat AS DATE NO-UNDO.                                  */
/* /*                                                                  */
/*     MESSAGE tt-param.l-imprime-bi                                   */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                          */
/* */                                                                  */
/*     DO v-dat = tt-param.dt-ctbl-ini TO tt-param.dt-ctbl-fim:        */
/*                                                                     */
/*         FOR EACH dvv_contabil NO-LOCK                               */
/*             WHERE dvv_contabil.dta-ctbz = v-dat                     */
/*               AND dvv_contabil.conta   >= tt-param.c-cta-ctbl-ini   */
/*               AND dvv_contabil.conta   <= tt-param.c-cta-ctbl-fim:  */
/*                                                                     */
/*             CREATE tt-rel-item.                                     */
/*             BUFFER-COPY dvv_contabil TO tt-rel-item.                */
/*                                                                     */
/*             IF dvv_contabil.movto = "RTDvvDEX" THEN                 */
/*                 ASSIGN tt-rel-item.cod-depos  = dvv_contabil.moeda. */
/*                                                                     */
/*             ASSIGN tt-rel-item.val-lancto = tt-rel-item.vl-real     */
/*                    tt-rel-item.val-doc    = tt-rel-item.vl-real     */
/*                    tt-rel-item.moeda      = cRealVenda.                 */
/*                                                                     */
/*         END.                                                        */
/*                                                                     */
/*     END.                                                            */

    RUN dvv/dvv0199-imp.p(INPUT TABLE tt-param,
                          OUTPUT TABLE tt-rel-item).

END PROCEDURE.

PROCEDURE pi-monta-fech-previo:
    DEF VAR l-apb   AS LOG NO-UNDO.
    DEF VAR l-cep   AS LOG NO-UNDO.
    DEF VAR l-fgl   AS LOG NO-UNDO.
    DEF VAR v-dat   AS DATE NO-UNDO.
    DEF VAR v_cta_mov_ok AS LOG NO-UNDO.
    DEF VAR v_val_movto AS DEC NO-UNDO.
    DEF VAR c-db-cr     AS CHAR NO-UNDO.
    DEF VAR v-log-conv-fgl AS LOG NO-UNDO.
    DEF VAR v_cod_ie_orig  AS CHAR NO-UNDO.

    DEF BUFFER b_aprop_lancto_ctbl FOR aprop_lancto_ctbl.

    ASSIGN l-log = NO
           l-apb = YES
           l-cep = YES
           l-fgl = YES.

    RUN pi-acompanhar IN h-acomp (INPUT "Buscando dados FECHAMENTO PRêVIO.").
    FIND plano_cta_ctbl NO-LOCK WHERE
         plano_cta_ctbl.cod_plano_cta_ctbl      = "nitro"
     AND plano_cta_ctbl.ind_tip_plano_cta_ctbl = "Prim†rio"
     AND plano_cta_ctbl.dat_inic_valid         <= TODAY
     AND plano_cta_ctbl.dat_fim_valid          >= TODAY NO-ERROR.
    FOR EACH estabelecimento NO-LOCK WHERE
             estabelecimento.cod_estab   >= tt-param.c-estab-ini 
         AND estabelecimento.cod_estab   <= tt-param.c-estab-fim:

         FIND FIRST tt_estab 
            WHERE tt_estab.cod_estab = estabelecimento.cod_estab NO-ERROR.
        IF NOT AVAIL tt_estab THEN DO:
            CREATE tt_estab.
            ASSIGN tt_estab.cod_estab = estabelecimento.cod_estab.

            FOR EACH dvv_param_conta NO-LOCK WHERE
                     dvv_param_conta.cod_estabel                    = tt_estab.cod_estab
                 AND substring(dvv_param_conta.conta_contabil,1,8) >= tt-param.c-cta-ctbl-ini 
                 AND substring(dvv_param_conta.conta_contabil,1,8) <= tt-param.c-cta-ctbl-fim :        

                IF NOT CAN-FIND(FIRST tt-cta-ctbl
                                WHERE tt-cta-ctbl.cod-conta   = SUBSTRING(dvv_param_conta.conta_contabil, 1, 8)
                                  AND tt-cta-ctbl.cod-empresa = estabelecimento.cod_empresa) THEN DO:

                    CREATE tt-cta-ctbl.
                    ASSIGN tt-cta-ctbl.cod-conta     = SUBSTRING(dvv_param_conta.conta_contabil, 1, 8)
                           tt-cta-ctbl.cod-unid      = "000" /*SUBSTRING(dvv_param_conta.conta_contabil, 9, 3)*/
                           tt-cta-ctbl.cod-ccusto    = "" /*SUBSTRING(dvv_param_conta.conta_contabil, 12, 4)*/
                           tt-cta-ctbl.cod-estabel   = dvv_param_conta.cod_estabel
                           tt-cta-ctbl.cod-empresa   = estabelecimento.cod_empresa
                           tt-cta-ctbl.origem        = "DVVME".
                END. 

            END.

            /* 1. Busca as contas cont†beis que ser∆o utilizados no relat¢rio de acordo com a faixa parametrizada em tela. */
            FOR EACH dvv_param_conta_mi NO-LOCK WHERE
                     dvv_param_conta_mi.cod_estab     = tt_estab.cod_estab
                 AND dvv_param_conta_mi.cod_cta_ctbl >= tt-param.c-cta-ctbl-ini 
                 AND dvv_param_conta_mi.cod_cta_ctbl <= tt-param.c-cta-ctbl-fim :

                IF NOT CAN-FIND(FIRST tt-cta-ctbl
                                WHERE tt-cta-ctbl.cod-conta = SUBSTRING(dvv_param_conta_mi.cod_cta_ctbl, 1, 8)) THEN DO:

                    CREATE tt-cta-ctbl.
                    ASSIGN tt-cta-ctbl.cod-conta     = SUBSTRING(dvv_param_conta_mi.cod_cta_ctbl, 1, 8)
                           tt-cta-ctbl.cod-unid      = SUBSTRING(dvv_param_conta_mi.cod_cta_ctbl, 9, 3)
                           tt-cta-ctbl.cod-ccusto    = SUBSTRING(dvv_param_conta_mi.cod_cta_ctbl, 12, 4)
                           tt-cta-ctbl.cod-estabel   = dvv_param_conta_mi.cod_estabel
                           tt-cta-ctbl.cod-empresa   = estabelecimento.cod_empresa
                           tt-cta-ctbl.origem        = "DVVMI".
                END.
            END.

        END.
    END.
    
    run pi-busca-dados-estoque.

    ASSIGN c-num_lote_ctbl       = 0
           c-num_lancto_ctbl     = 0
           c-num_seq_lancto_ctbl = 0.

    IF l-apb THEN DO:
        FOR EACH tt_estab,
        FIRST estabelecimento WHERE 
              estabelecimento.cod_estab = tt_estab.cod_estab NO-LOCK
            BREAK BY tt_estab.cod_estab:
    
            /***** Busca Plano de Centro de Custo */
            FIND plano_cta_unid_organ NO-LOCK WHERE
                 plano_cta_unid_organ.cod_unid_organ          = estabelecimento.cod_empresa
             AND plano_cta_unid_organ.ind_tip_plano_cta_ctbl  = "Prim†rio"
             AND plano_cta_unid_organ.dat_inic_valid         <= TODAY
             AND plano_cta_unid_organ.dat_fim_valid          >= TODAY NO-ERROR.
            IF NOT AVAIL plano_cta_unid_organ THEN NEXT.

            FOR EACH tt-cta-ctbl WHERE
                     tt-cta-ctbl.cod-estabel = estabelecimento.cod_estab:
                DO v-dat = tt-param.dt-ctbl-ini TO tt-param.dt-ctbl-fim:
    
                    RUN pi-acompanhar IN h-acomp (INPUT "APB.: " + CAPS(tt_estab.cod_estab)   + " - " +
                                                                   tt-cta-ctbl.cod-conta   + " - " +
                                                                   STRING(v-dat,"99/99/9999")).

                    FOR EACH aprop_ctbl_ap NO-LOCK WHERE
                             aprop_ctbl_ap.cod_estab           = tt_estab.cod_estab        
                         AND aprop_ctbl_ap.cod_plano_cta_ctbl  = plano_cta_unid_organ.cod_plano_cta_ctbl
                         AND aprop_ctbl_ap.cod_cta_ctbl        = tt-cta-ctbl.cod-conta  
                         AND aprop_ctbl_ap.dat_transacao       = v-dat                     
                         //AND aprop_ctbl_ap.log_ctbz_aprop_ctbl = YES                       
                         AND aprop_ctbl_ap.cod_unid_negoc     >= tt-param.c-unid-negoc-ini           
                         AND aprop_ctbl_ap.cod_unid_negoc     <= tt-param.c-unid-negoc-fim,             
                       FIRST cta_ctbl NO-LOCK WHERE
                             cta_ctbl.cod_plano_cta_ctbl       = aprop_ctbl_ap.cod_plano_cta_ctbl AND
                             cta_ctbl.cod_cta_ctbl             = aprop_ctbl_ap.cod_cta_ctbl       AND
                             cta_ctbl.ind_espec_cta_ctbl      <> "SintÇtica",
                       EACH val_aprop_ctbl_ap OF aprop_ctbl_ap NO-LOCK WHERE
                            val_aprop_ctbl_ap.cod_finalid_econ = "corrente" /*tt-param.cod-finalidade-econ*/:
            
                       FIND FIRST movto_tit_ap NO-LOCK WHERE 
                                  movto_tit_ap.cod_estab           = aprop_ctbl_ap.cod_estab   
                              AND movto_tit_ap.num_id_movto_tit_ap = aprop_ctbl_ap.num_id_movto_tit_ap NO-ERROR.

                       FIND tit_ap WHERE
                            tit_ap.cod_estab = movto_tit_ap.cod_estab AND
                            tit_ap.num_id_tit_ap = movto_tit_ap.num_id_tit_ap NO-LOCK NO-ERROR.

                       /* Filtro MOVIMENTOS CONTABILIZADOS */
                       IF NOT AVAIL movto_tit_ap
                       OR (AVAIL movto_tit_ap
                             AND movto_tit_ap.log_aprop_ctbl_ctbzda = yes) THEN NEXT.
    
                       FIND FIRST movto_cta_corren NO-LOCK WHERE
                                  movto_cta_corren.num_id_movto_cta_corren = val_aprop_ctbl_ap.num_id_aprop_lancto_ctbl NO-ERROR.
            
    
                       FOR FIRST aprop_lancto_ctbl NO-LOCK WHERE
                                 aprop_lancto_ctbl.num_id_aprop_lancto_ctbl = val_aprop_ctbl_ap.num_id_aprop_lancto_ctbl.
            
                           ASSIGN c-num_lote_ctbl       = aprop_lancto_ctbl.num_lote_ctbl
                                  c-num_lancto_ctbl     = aprop_lancto_ctbl.num_lancto_ctbl
                                  c-num_seq_lancto_ctbl = aprop_lancto_ctbl.num_seq_lancto_ctbl.
                       END.
            
                       FIND FIRST emscad.ccusto NO-LOCK WHERE
                                  emscad.ccusto.cod_empresa      = aprop_ctbl_ap.cod_empresa     
                              AND emscad.ccusto.cod_plano_ccusto = aprop_ctbl_ap.cod_plano_ccusto
                              AND emscad.ccusto.cod_ccusto       = aprop_ctbl_ap.cod_ccusto NO-ERROR.
            
                       FIND FIRST ems.unid_negoc NO-LOCK WHERE
                                  unid_negoc.cod_unid_negoc = aprop_ctbl_ap.cod_unid_negoc NO-ERROR.
            
                       ASSIGN c-num_lote_ctbl       = c-num_lote_ctbl      + 1   
                              c-num_lancto_ctbl     = c-num_lancto_ctbl    + 1
                              c-num_seq_lancto_ctbl = c-num_seq_lancto_ctbl + 1.

                       ASSIGN i-lin = i-lin + 1.
                       /* Dados da contabilidade. */
                       //MESSAGE "1" VIEW-AS ALERT-BOX.

                       create b-tt-rel-base.
                       assign b-tt-rel-base.tipo           = "T"
                              b-tt-rel-base.movto          = "Previsto"
                              b-tt-rel-base.conta          = aprop_ctbl_ap.cod_cta_ctbl
                              b-tt-rel-base.modulo         = "APB" //lote_ctbl.cod_modul_dtsul
                              b-tt-rel-base.dta-ctbz       = tit_ap.dat_transacao
                              b-tt-rel-base.num-lote       = c-num_lote_ctbl      
                              b-tt-rel-base.num-lancto     = c-num_lancto_ctbl    
                              b-tt-rel-base.num-seq-lancto = c-num_seq_lancto_ctbl
                              b-tt-rel-base.mov            = aprop_ctbl_ap.ind_natur_lancto_ctbl
                              b-tt-rel-base.moeda          = cRealVenda
                              b-tt-rel-base.cod-estabel    = tit_ap.cod_estab
                              b-tt-rel-base.dt-lote        = tit_ap.dat_emis_docto
                              b-tt-rel-base.user-lote      = "" //lote_ctbl.cod_usuAr_ult_atualiz
                              b-tt-rel-base.cod-unid-negoc = aprop_ctbl_ap.cod_unid_negoc
                              b-tt-rel-base.cod-unid-negoc-gerenc = aprop_ctbl_ap.cod_unid_negoc
                              b-tt-rel-base.usuario-impl   = "" //lote_ctbl.cod_usuAr_ult_atualiz.
                              b-tt-rel-base.val-lancto     = val_aprop_ctbl_ap.val_aprop_ctbl. //aprop_ctbl_ap.val_aprop_ctbl.
                       /* O relatΩrio deverˇ sempre apresentar os valores em reais.
                          Caso o indicador econÀmico seja diferente de Real buscar na apropriaªío o valor em reais. */
                       IF tt-cta-ctbl.origem = "DVVME" THEN
                            RUN pi-dados-titulo-apb-previa-dvvme.
                       IF tt-cta-ctbl.origem = "DVVMI" THEN
                            RUN pi-dados-titulo-apb-previa-dvvmi.

                    END. /* FOR EACH aprop_ctbl_ap */
                    //ACR

                    RUN pi-acompanhar IN h-acomp (INPUT "ACR.: " + CAPS(tt_estab.cod_estab)   + " - " +
                                                                   tt-cta-ctbl.cod-conta   + " - " +
                                                                   STRING(v-dat,"99/99/9999")).
                    FOR EACH aprop_ctbl_acr NO-LOCK WHERE
                             aprop_ctbl_acr.cod_estab           = tt_estab.cod_estab        
                         AND aprop_ctbl_acr.cod_plano_cta_ctbl  = plano_cta_unid_organ.cod_plano_cta_ctbl
                         AND aprop_ctbl_acr.cod_cta_ctbl        = tt-cta-ctbl.cod-conta  
                         AND aprop_ctbl_acr.dat_transacao       = v-dat                     
                         //AND aprop_ctbl_acr.log_ctbz_acrrop_ctbl = YES                       
                         AND aprop_ctbl_acr.cod_unid_negoc     >= tt-param.c-unid-negoc-ini           
                         AND aprop_ctbl_acr.cod_unid_negoc     <= tt-param.c-unid-negoc-fim,             
                       FIRST cta_ctbl NO-LOCK WHERE
                             cta_ctbl.cod_plano_cta_ctbl       = aprop_ctbl_acr.cod_plano_cta_ctbl AND
                             cta_ctbl.cod_cta_ctbl             = aprop_ctbl_acr.cod_cta_ctbl       AND
                             cta_ctbl.ind_espec_cta_ctbl      <> "SintÇtica",
                       EACH val_aprop_ctbl_acr OF aprop_ctbl_acr NO-LOCK WHERE
                            val_aprop_ctbl_acr.cod_finalid_econ = "corrente" /*tt-param.cod-finalidade-econ*/:
            
                       FIND FIRST movto_tit_acr NO-LOCK WHERE 
                                  movto_tit_acr.cod_estab           = aprop_ctbl_acr.cod_estab   
                              AND movto_tit_acr.num_id_movto_tit_acr = aprop_ctbl_acr.num_id_movto_tit_acr NO-ERROR.

                       FIND tit_acr WHERE
                            tit_acr.cod_estab = movto_tit_acr.cod_estab AND
                            tit_acr.num_id_tit_acr = movto_tit_acr.num_id_tit_acr NO-LOCK NO-ERROR.

                       //IF tit_acr.cod_tit_acr <> "735895" THEN NEXT.
    
                       /* Filtro MOVIMENTOS CONTABILIZADOS */
                       IF NOT AVAIL movto_tit_acr
                       OR (AVAIL movto_tit_acr
                             AND movto_tit_acr.log_aprop_ctbl_ctbzda = yes) THEN NEXT.
    
                       FIND FIRST movto_cta_corren NO-LOCK WHERE
                                  movto_cta_corren.num_id_movto_cta_corren = val_aprop_ctbl_acr.num_id_aprop_lancto_ctbl NO-ERROR.
            
    
                       FOR FIRST aprop_lancto_ctbl NO-LOCK WHERE
                                 aprop_lancto_ctbl.num_id_aprop_lancto_ctbl = val_aprop_ctbl_acr.num_id_aprop_lancto_ctbl.
            
                           ASSIGN c-num_lote_ctbl       = aprop_lancto_ctbl.num_lote_ctbl
                                  c-num_lancto_ctbl     = aprop_lancto_ctbl.num_lancto_ctbl
                                  c-num_seq_lancto_ctbl = aprop_lancto_ctbl.num_seq_lancto_ctbl.
                       END.
            
                       FIND FIRST emscad.ccusto NO-LOCK WHERE
                                  emscad.ccusto.cod_empresa      = aprop_ctbl_acr.cod_empresa     
                              AND emscad.ccusto.cod_plano_ccusto = aprop_ctbl_acr.cod_plano_ccusto
                              AND emscad.ccusto.cod_ccusto       = aprop_ctbl_acr.cod_ccusto NO-ERROR.
            
                       FIND FIRST ems.unid_negoc NO-LOCK WHERE
                                  unid_negoc.cod_unid_negoc = aprop_ctbl_acr.cod_unid_negoc NO-ERROR.
            
                       ASSIGN c-num_lote_ctbl       = c-num_lote_ctbl      + 1   
                              c-num_lancto_ctbl     = c-num_lancto_ctbl    + 1
                              c-num_seq_lancto_ctbl = c-num_seq_lancto_ctbl + 1.

                       ASSIGN i-lin = i-lin + 1.
                       /* Dados da contabilidade. */
                       //MESSAGE "1" VIEW-AS ALERT-BOX.

                       create b-tt-rel-base.
                       assign b-tt-rel-base.tipo           = "T"
                              b-tt-rel-base.movto          = "Previsto"
                              b-tt-rel-base.conta          = aprop_ctbl_acr.cod_cta_ctbl
                              b-tt-rel-base.modulo         = "ACR" //lote_ctbl.cod_modul_dtsul
                              b-tt-rel-base.dta-ctbz       = tit_acr.dat_emis_docto
                              b-tt-rel-base.num-lote       = c-num_lote_ctbl      
                              b-tt-rel-base.num-lancto     = c-num_lancto_ctbl    
                              b-tt-rel-base.num-seq-lancto = c-num_seq_lancto_ctbl
                              b-tt-rel-base.mov            = aprop_ctbl_acr.ind_natur_lancto_ctbl
                              b-tt-rel-base.moeda          = cRealVenda
                              b-tt-rel-base.cod-estabel    = tit_acr.cod_estab
                              b-tt-rel-base.dt-lote        = tit_acr.dat_emis_docto
                              b-tt-rel-base.user-lote      = "" //lote_ctbl.cod_usuAr_ult_atualiz
                              b-tt-rel-base.cod-unid-negoc = aprop_ctbl_acr.cod_unid_negoc
                              b-tt-rel-base.usuario-impl   = "" //lote_ctbl.cod_usuAr_ult_atualiz.
                              b-tt-rel-base.val-lancto     = val_aprop_ctbl_acr.val_aprop_ctbl //aprop_ctbl_ap.val_aprop_ctbl.
                              b-tt-rel-base.cod-unid-negoc-gerenc = aprop_ctbl_acr.cod_unid_negoc. //campo novo criado para tratar a unidade gerencial
                       /* O relatΩrio deverˇ sempre apresentar os valores em reais.
                          Caso o indicador econÀmico seja diferente de Real buscar na apropriaªío o valor em reais. */
                       IF tt-cta-ctbl.origem = "DVVME" THEN
                            RUN pi-dados-titulo-acr-previa-dvvme.

                       //IF tt-cta-ctbl.origem = "DVVMI" THEN
                            //RUN pi-dados-titulo-apb-previa-dvvmi.

                    END. /* FOR EACH aprop_ctbl_ap */

                    //fim acr
                    
                    RUN pi-acompanhar IN h-acomp (INPUT "CEP.: " + CAPS(tt_estab.cod_esta)   + " - " +
                                                                   STRING(v-dat,"99/99/9999")).


                    FOR EACH movto-estoq NO-LOCK USE-INDEX estab-dep WHERE 
                             movto-estoq.cod-estabel       = tt_estab.cod_estab
                         AND movto-estoq.dt-trans          = v-dat            
                         AND movto-estoq.ct-codigo         = tt-cta-ctbl.cod-conta :

                        /* Filtra MOVIMENTOS CONTABILIZADOS */
                        IF  movto-estoq.contabilizado = YES THEN NEXT. 
                
                       ASSIGN v_cta_mov_ok = NO .
                       IF   movto-estoq.cod-unid-negoc       >= tt-param.c-unid-negoc-ini                         
                       AND movto-estoq.cod-unid-negoc       <= tt-param.c-unid-negoc-fim THEN                   
                           ASSIGN v_cta_mov_ok = YES.                                                 
                       
                       IF  v_cta_mov_ok = NO THEN NEXT.
                       FIND FIRST ITEM NO-LOCK WHERE
                                  ITEM.it-codigo = movto-estoq.it-codigo NO-ERROR.
                       IF NOT AVAIL ITEM THEN NEXT.
                
                       FIND FIRST emitente NO-LOCK WHERE
                                  emitente.cod-emitente = movto-estoq.cod-emitente NO-ERROR.
                
                       /*FIND FIRST estabelec NO-LOCK WHERE
                                  estabelec.cod-estabel = movto-estoq.cod-estabel NO-ERROR.
                       IF NOT AVAIL estabelec THEN NEXT.*/
                
                
                       /********** Gera Raz∆o para a conta MOVIMENTO */
                       IF v_cta_mov_ok THEN DO:

                           ASSIGN c-num_lote_ctbl       = c-num_lote_ctbl      + 1   
                                  c-num_lancto_ctbl     = c-num_lancto_ctbl    + 1
                                  c-num_seq_lancto_ctbl = c-num_seq_lancto_ctbl + 1.

                           ASSIGN i-lin = i-lin + 1.
                           /* Dados da contabilidade. */

                           ASSIGN v_val_movto = 0
                                  v_val_movto = (movto-estoq.valor-mat-m[1] +
                                                 movto-estoq.valor-mob-m[1] +
                                                 movto-estoq.valor-ggf-m[1] +
                                                 movto-estoq.valor-icm      +
                                                 movto-estoq.valor-ipi      +
                                                 movto-estoq.val-cofins     +
                                                 movto-estoq.valor-pis).
                           IF ITEM.tipo-contr = 2 THEN DO: /* Carrega Valor somente para Itens com Controle Total */
                               if  v_val_movto = 0 then do:
                                   /*-----> Busca Èltimo Valor do Material qdo o Movimento for Zero <----------------------*/
                                   find first item-estab no-lock
                                        where item-estab.it-codigo   = movto-estoq.it-codigo
                                          and item-estab.cod-estabel = movto-estoq.cod-estabel no-error.
                                   if  avail item-estab then
                                       assign v_val_movto = (item-estab.val-unit-mat-m[1] + item-estab.val-unit-mob-m[1]
                                                      +  item-estab.val-unit-ggf-m[1]) * movto-estoq.quantidade.
                                   else
                                       assign v_val_movto = 0.
                                  /*--------------------------------------------------------------------------------------*/
                               end.

                               if  v_val_movto = 0 then do:
                                   /*-----> Busca Èltimo Entrada Material  <----------------------------------------------*/
                                   find first item-uni-estab no-lock
                                        where item-uni-estab.it-codigo   = movto-estoq.it-codigo
                                          and item-uni-estab.cod-estabel = movto-estoq.cod-estabel no-error.
                                   if  avail item-uni-estab then
                                       assign v_val_movto = item-uni-estab.preco-ul-ent * movto-estoq.quantidade.
                                   else
                                       assign v_val_movto = 0.
                                  /*--------------------------------------------------------------------------------------*/
                               end.
                           END.

                           /* Se entrada valor fica negativo. */
                           IF movto-estoq.tipo-trans = 1 THEN
                               ASSIGN v_val_movto = v_val_movto * -1.

                           IF v_val_movto > 0 THEN
                               ASSIGN c-db-cr = "DB".
                           ELSE 
                               ASSIGN c-db-cr = "CR".

                           create b-tt-rel-base.
                           assign b-tt-rel-base.tipo           = "T"
                                  b-tt-rel-base.movto          = "Previsto"
                                  b-tt-rel-base.conta          = movto-estoq.ct-codigo
                                  b-tt-rel-base.modulo         = "CEP" //lote_ctbl.cod_modul_dtsul
                                  b-tt-rel-base.dta-ctbz       = movto-estoq.dt-trans
                                  b-tt-rel-base.num-lote       = c-num_lote_ctbl       
                                  b-tt-rel-base.num-lancto     = c-num_lancto_ctbl    
                                  b-tt-rel-base.num-seq-lancto = c-num_seq_lancto_ctbl
                                  b-tt-rel-base.mov            = c-db-cr
                                  b-tt-rel-base.moeda          = cRealVenda
                                  b-tt-rel-base.cod-estabel    = movto-estoq.cod-estabel
                                  b-tt-rel-base.dt-lote        = movto-estoq.dt-trans
                                  b-tt-rel-base.user-lote      = "" //lote_ctbl.cod_usuAr_ult_atualiz
                                  b-tt-rel-base.cod-unid-negoc = movto-estoq.cod-unid-negoc
                                  b-tt-rel-base.cod-unid-negoc-gerenc = movto-estoq.cod-unid-negoc
                                  b-tt-rel-base.usuario-impl   = "" //lote_ctbl.cod_usuAr_ult_atualiz.
                                  b-tt-rel-base.val-lancto     = v_val_movto.
                           /* O relatΩrio deverˇ sempre apresentar os valores em reais.
                              Caso o indicador econÀmico seja diferente de Real buscar na apropriaªío o valor em reais. */
                           IF tt-cta-ctbl.origem = "DVVME" THEN
                                RUN pi-dados-rec-previa-me (INPUT "Previsto",
                                                  input "CEP",                     
                                                  input movto-estoq.cod-estabel ,  
                                                  input movto-estoq.ct-codigo,     
                                                  input movto-estoq.dt-trans,      
                                                  input movto-estoq.cod-unid-negoc
                                                  
                                                  ).
                           IF tt-cta-ctbl.origem = "DVVMI" THEN
                                RUN pi-dados-rec-mi.

                       END.
                
                       /******
                       /********** Gera Raz∆o para a conta SALDO */
                       IF v_cta_sdo_ok THEN
                           RUN pi_gera_movto_estoque (INPUT sc-saldo,
                                                      INPUT movto-estoq.ct-saldo,
                                                      INPUT "SDO").
                
                       /********** Gera Raz∆o para a conta IPI */
                       IF v_cta_ipi_ok THEN  
                           RUN pi_gera_movto_estoque (INPUT estabelec.sc-ipi,
                                                      INPUT estabelec.ct-ipi,
                                                      INPUT "IPI").
                
                       /********** Gera Raz∆o para a conta ICM */
                       IF v_cta_icm_ok THEN 
                           RUN pi_gera_movto_estoque (INPUT estabelec.sc-icm,
                                                      INPUT estabelec.ct-icm,     
                                                      INPUT "ICMS").
                
                       /********** Gera Raz∆o para a conta PIS */
                       IF v_cta_pis_ok THEN 
                           RUN pi_gera_movto_estoque (INPUT estabelec.cod-ccusto-pis-recup,                                               
                                                      INPUT substring(estabelec.cod-cta-pis-recup, 1, 8),
                                                      INPUT "PIS").
                
                       /********** Gera Raz∆o para a conta COFINS */
                       IF v_cta_cof_ok THEN
                           RUN pi_gera_movto_estoque (INPUT estabelec.cod-ccusto-cofins-recup,                
                                                      INPUT substring(estabelec.cod-cta-cofins-recup, 1, 8),
                                                      INPUT "COFINS").
                       ****/
                    END. /* FOR EACH movto-estoq */


                    RUN pi-acompanhar IN h-acomp (INPUT "FGL.: " + CAPS(tt_estab.cod_estab)   + " - " +
                                                                   tt-cta-ctbl.cod-conta   + " - " +
                                                                   STRING(v-dat,"99/99/9999")).


                    FOR EACH item_lancto_ctbl   NO-LOCK USE-INDEX tmlnctcb_estab WHERE
                             item_lancto_ctbl.cod_estab            = tt_estab.cod_estab          
                         AND item_lancto_ctbl.cod_plano_cta_ctbl   = plano_cta_unid_organ.cod_plano_cta_ctbl
                         AND item_lancto_ctbl.cod_cta_ctbl         = tt-cta-ctbl.cod-conta   
                         AND item_lancto_ctbl.ind_sit_lancto_ctbl  = "CTBZ"                      
                         AND item_lancto_ctbl.dat_lancto_ctbl      = v-dat                       
                         AND item_lancto_ctbl.cod_unid_negoc      >= tt-param.c-unid-negoc-ini             
                         AND item_lancto_ctbl.cod_unid_negoc      <= tt-param.c-unid-negoc-fim,
                        FIRST cta_ctbl NO-LOCK WHERE
                              cta_ctbl.cod_plano_cta_ctbl          = item_lancto_ctbl.cod_plano_cta_ctbl
                          AND cta_ctbl.cod_cta_ctbl                = item_lancto_ctbl.cod_cta_ctbl      
                          AND cta_ctbl.ind_espec_cta_ctbl         <> "SintÇtica",
                        FIRST lancto_ctbl OF item_lancto_ctbl NO-LOCK WHERE
                              lancto_ctbl.cod_modul_dtsul          = "FGL",
                        FIRST lote_ctbl OF lancto_ctbl NO-LOCK:
    
                        ASSIGN v-log-conv-fgl = NO.
    
                        FIND FIRST aprop_lancto_ctbl NO-LOCK WHERE
                                   aprop_lancto_ctbl.num_lote_ctbl       = ITEM_lancto_ctbl.num_lote_ctbl       
                               AND aprop_lancto_ctbl.num_lancto_ctbl     = ITEM_lancto_ctbl.num_lancto_ctbl     
                               AND aprop_lancto_ctbl.num_seq_lancto_ctbl = ITEM_lancto_ctbl.num_seq_lancto_ctbl 
                               AND aprop_lancto_ctbl.cod_finalid_econ    = "corrente" NO-ERROR.
        
                        IF NOT AVAIL aprop_lancto_ctbl THEN DO:
                            ASSIGN v-log-conv-fgl = YES.
    
                            FIND FIRST aprop_lancto_ctbl NO-LOCK WHERE
                                       aprop_lancto_ctbl.num_lote_ctbl       = ITEM_lancto_ctbl.num_lote_ctbl       
                                   AND aprop_lancto_ctbl.num_lancto_ctbl     = ITEM_lancto_ctbl.num_lancto_ctbl     
                                   AND aprop_lancto_ctbl.num_seq_lancto_ctbl = ITEM_lancto_ctbl.num_seq_lancto_ctbl 
                                   AND aprop_lancto_ctbl.cod_finalid_econ    = "Corrente" NO-ERROR.  
                             
                            IF NOT AVAIL aprop_lancto_ctbl THEN DO: 
    
                                FIND FIRST aprop_lancto_ctbl NO-LOCK WHERE
                                           aprop_lancto_ctbl.num_lote_ctbl       = ITEM_lancto_ctbl.num_lote_ctbl       
                                       AND aprop_lancto_ctbl.num_lancto_ctbl     = ITEM_lancto_ctbl.num_lancto_ctbl     
                                       AND aprop_lancto_ctbl.num_seq_lancto_ctbl = ITEM_lancto_ctbl.num_seq_lancto_ctbl NO-ERROR.
    
                                RUN pi_retornar_indic_econ_finalid (INPUT aprop_lancto_ctbl.cod_finalid_econ,
                                                                    INPUT item_lancto_ctbl.dat_lancto_ctbl,
                                                                    OUTPUT v_cod_ie_orig).
    
                            END.
                        END.
    
                        FIND FIRST b_aprop_lancto_ctbl NO-LOCK WHERE
                                   b_aprop_lancto_ctbl.num_lote_ctbl       = ITEM_lancto_ctbl.num_lote_ctbl      AND 
                                   b_aprop_lancto_ctbl.num_lancto_ctbl     = ITEM_lancto_ctbl.num_lancto_ctbl     AND 
                                   b_aprop_lancto_ctbl.num_seq_lancto_ctbl = ITEM_lancto_ctbl.num_seq_lancto_ctbl  AND 
                                   b_aprop_lancto_ctbl.cod_finalid_econ    = ITEM_lancto_ctbl.cod_indic_econ NO-ERROR.  
                        IF AVAIL b_aprop_lancto_ctbl THEN
                            v_cod_ie_orig = ITEM_lancto_ctbl.cod_indic_econ.
                        IF NOT AVAIL b_aprop_lancto_ctbl THEN DO:
                            FIND FIRST b_aprop_lancto_ctbl NO-LOCK WHERE
                                       b_aprop_lancto_ctbl.num_lote_ctbl       = ITEM_lancto_ctbl.num_lote_ctbl      AND 
                                       b_aprop_lancto_ctbl.num_lancto_ctbl     = ITEM_lancto_ctbl.num_lancto_ctbl    AND 
                                       b_aprop_lancto_ctbl.num_seq_lancto_ctbl = ITEM_lancto_ctbl.num_seq_lancto_ctbl AND 
                                       b_aprop_lancto_ctbl.cod_finalid_econ    = "Corrente" NO-ERROR. 
                            v_cod_ie_orig = cRealVenda.
                        END.
    
                        IF  item_lancto_ctbl.cod_cenar_ctbl <> ""
                        AND item_lancto_ctbl.cod_cenar_ctbl <> "Fiscal" /*tt-param.cod-cenar-ctbl*/ THEN NEXT.
    
                        /* Filtra movimentos do modulo de EXPORTAÄ«O
                        IF  lancto_ctbl.cod_modul_dtsul = "FGL"
                        AND INDEX(item_lancto_ctbl.des_histor_lancto_ctbl, "Movto ref faturamento de") > 0 THEN NEXT.
                        */
                                                                    
        
                        /*
                        ASSIGN c-num_lote_ctbl       = c-num_lote_ctbl      + 1   
                               c-num_lancto_ctbl     = c-num_lancto_ctbl    + 1
                               c-num_seq_lancto_ctbl = c-num_seq_lancto_ctbl + 1.
                        */
                        ASSIGN i-lin = i-lin + 1.
                        /* Dados da contabilidade. */
                        //MESSAGE "1" VIEW-AS ALERT-BOX.

                        /* Dados da contabilidade. */
                        create b-tt-rel-base.
                        assign b-tt-rel-base.tipo       = "T"
                               b-tt-rel-base.movto      = "Previsto"
                               b-tt-rel-base.conta      = ITEM_lancto_ctbl.cod_cta_ctbl
                               b-tt-rel-base.modulo     = lote_ctbl.cod_modul_dtsul
                               b-tt-rel-base.dta-ctbz   = ITEM_lancto_ctbl.dat_lancto_ctbl
                               b-tt-rel-base.num-lote   = ITEM_lancto_ctbl.num_lote_ctbl
                               b-tt-rel-base.num-lancto = ITEM_lancto_ctbl.num_lancto_ctbl
                               b-tt-rel-base.num-seq-lancto = ITEM_lancto_ctbl.num_seq_lancto_ctbl
                               b-tt-rel-base.mov            = ITEM_lancto_ctbl.ind_natur_lancto_ctbl
                               b-tt-rel-base.moeda          = cRealVenda
                               b-tt-rel-base.cod-estabel    = ITEM_lancto_ctbl.cod_estab
                               b-tt-rel-base.dt-lote        = lote_ctbl.dat_ult_atualiz
                               b-tt-rel-base.user-lote      = lote_ctbl.cod_usuAr_ult_atualiz
                               b-tt-rel-base.cod-unid-negoc = ITEM_lancto_ctbl.cod_unid_negoc
                               b-tt-rel-base.cod-unid-negoc-gerenc = ITEM_lancto_ctbl.cod_unid_negoc
                               b-tt-rel-base.usuario-impl   = lote_ctbl.cod_usuAr_ult_atualiz.

                        /* O relatΩrio deverˇ sempre apresentar os valores em reais.
                           Caso o indicador econÀmico seja diferente de Real buscar na apropriaªío o valor em reais. */
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
                            when "FGL" THEN do:

                                IF tt-param.dt-ctbl-ini < 01/01/2017 THEN
                                    RUN pi-dados-tms.

                                ASSIGN b-tt-rel-base.doc-origem = replace(ITEM_lancto_ctbl.des_histor_lancto_ctbl, CHR(10), " ")
                                       b-tt-rel-base.doc-origem = REPLACE(b-tt-rel-base.doc-origem, ";", ",").

                            END.
                        end case.
                    END. /* FOR EACH item_lancto_ctbl */
                END. /* DO v-dat = tt-param.data-i TO tt-param.data-f */
            END. /* FOR EACH tt-cta-ctbl */
        END. /* FOR EACH tt_estab */
    END. /* IF tt-param.apb */

END PROCEDURE.

PROCEDURE pi-monta-previa-provisao:

    /*MESSAGE 'aqui'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.   */
    DEFINE VARIABLE dtEstornoFinal AS DATE    NO-UNDO.
    
    IF month(tt-param.dt-ctbl-fim)  = 12 THEN 
        ASSIGN dtEstornoFinal = DATE(01,01,YEAR(tt-param.dt-ctbl-fim) + 1) - 1.
    ELSE
        ASSIGN dtEstornoFinal = date(month(tt-param.dt-ctbl-fim) + 1,01,YEAR(tt-param.dt-ctbl-fim)) - 1.

    // Lista PrÇvia das Provis‰es (geradas no DVV5100)
    IF  tt-param.l-previa-provisoes = YES THEN DO:

        ASSIGN c-num_lote_ctbl       = c-num_lote_ctbl + 1
               c-num_lancto_ctbl     = 1
               c-num_seq_lancto_ctbl = 0.
               
        FOR EACH tt_estab,
            EACH dvv_log_provisao NO-LOCK USE-INDEX id_es_dt_ct_cc_un
               WHERE dvv_log_provisao.cod_estabel     = tt_estab.cod_estab
               AND  dvv_log_provisao.dt_provisao     >= tt-param.dt-ctbl-ini
               AND  dvv_log_provisao.dt_provisao     <= tt-param.dt-ctbl-fim
               AND  dvv_log_provisao.cod_un          >= tt-param.c-unid-negoc-ini
               AND  dvv_log_provisao.cod_un          <= tt-param.c-unid-negoc-fim
               AND  dvv_log_provisao.origem_movto    BEGINS "EP":  // PDEX, PDVV ME, PDVV MI - prÇvias das provis‰es, geradas pelo dvv5100.

            // Conta CR da Provis∆o prevista
            IF  dvv_log_provisao.cod_cta_ctbl_db >= tt-param.c-cta-ctbl-ini
            AND dvv_log_provisao.cod_cta_ctbl_db <= tt-param.c-cta-ctbl-fim THEN DO:

                find first ITEM_lancto_ctbl no-lock
                    where ITEM_lancto_ctbl.num_lote_ctbl = dvv_log_provisao.num_lote_ctbl no-error.
    
                create tt-provisao.
                assign c-num_seq_lancto_ctbl      = c-num_seq_lancto_ctbl + 1
                       tt-provisao.tipo           = "T"
                       tt-provisao.movto          = "PrÇvia Estorno"
                       tt-provisao.conta          = IF AVAIL ITEM_lancto_ctbl THEN ITEM_lancto_ctbl.cod_cta_ctbl ELSE "34120101" /*dvv_log_provisao.cod_cta_ctbl_cr*/
                       tt-provisao.modulo         = "FGL"
                       tt-provisao.dta-ctbz       = date(month(tt-param.dt-ctbl-fim),01,YEAR(tt-param.dt-ctbl-fim))
                       tt-provisao.num-lote       = c-num_lote_ctbl      
                       tt-provisao.num-lancto     = c-num_lancto_ctbl    
                       tt-provisao.num-seq-lancto = c-num_seq_lancto_ctbl
                       tt-provisao.mov            = "CR"
                       tt-provisao.moeda          = cRealVenda                  
                       tt-provisao.cod-estabel    = dvv_log_provisao.cod_estab  
                       tt-provisao.dt-lote        = ?                           
                       tt-provisao.user-lote      = ""                          
                       tt-provisao.usuario-impl   = ""
                       tt-provisao.cod-unid-negoc = dvv_log_provisao.cod_un 
                       tt-provisao.cod-unid-negoc-gerenc = dvv_log_provisao.cod_un
                       tt-provisao.doc-origem     = "Estorno da PrÇvia de Lancamento CR de Provis∆o " + dvv_log_provisao.origem_movto
                       tt-provisao.val-lancto     = dvv_log_provisao.vl_provisao.

                 FOR EACH dvv_log_prov_desp NO-LOCK
                     WHERE dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao:

                     RUN pi-monta-previa-provisao-linha-d.
                 END.
            END.
        END.               
        
        FOR EACH tt_estab,
            EACH dvv_log_provisao NO-LOCK USE-INDEX id_es_dt_ct_cc_un
               WHERE dvv_log_provisao.cod_estabel     = tt_estab.cod_estab
               AND  dvv_log_provisao.dt_provisao     >= tt-param.dt-ctbl-ini
               AND  dvv_log_provisao.dt_provisao     <= tt-param.dt-ctbl-fim
               AND  dvv_log_provisao.cod_un          >= tt-param.c-unid-negoc-ini
               AND  dvv_log_provisao.cod_un          <= tt-param.c-unid-negoc-fim
               AND  dvv_log_provisao.origem_movto    BEGINS "P":  // PDEX, PDVV ME, PDVV MI - prÇvias das provis‰es, geradas pelo dvv5100.

            // Conta DB da Provis∆o prevista
            IF  dvv_log_provisao.cod_cta_ctbl_db >= tt-param.c-cta-ctbl-ini
            AND dvv_log_provisao.cod_cta_ctbl_db <= tt-param.c-cta-ctbl-fim THEN DO:

                create tt-provisao.
                assign c-num_seq_lancto_ctbl      = c-num_seq_lancto_ctbl + 1
                       tt-provisao.tipo           = "T"
                       tt-provisao.movto          = "PrÇvia Previs∆o"
                       tt-provisao.conta          = dvv_log_provisao.cod_cta_ctbl_db
                       tt-provisao.modulo         = "FGL"
                       tt-provisao.dta-ctbz       = tt-param.dt-ctbl-fim
                       tt-provisao.num-lote       = c-num_lote_ctbl      
                       tt-provisao.num-lancto     = c-num_lancto_ctbl    
                       tt-provisao.num-seq-lancto = c-num_seq_lancto_ctbl 
                       tt-provisao.mov            = "DB"                        
                       tt-provisao.moeda          = cRealVenda                  
                       tt-provisao.cod-estabel    = dvv_log_provisao.cod_estab  
                       tt-provisao.dt-lote        = ?                           
                       tt-provisao.user-lote      = ""                          
                       tt-provisao.usuario-impl   = ""
                       tt-provisao.cod-unid-negoc = dvv_log_provisao.cod_un 
                       tt-provisao.cod-unid-negoc-gerenc = dvv_log_provisao.cod_un
                       tt-provisao.doc-origem     = "PrÇvia de Lancamento DB de Provis∆o " + dvv_log_provisao.origem_movto              /*aqui*/
                       tt-provisao.val-lancto     = dvv_log_provisao.vl_provisao.

                FOR EACH dvv_log_prov_desp NO-LOCK
                    WHERE dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao:

                    RUN pi-monta-previa-provisao-linha-d.
                 END.
            END.

            /*
            // Conta CR da Provis∆o prevista
            IF  dvv_log_provisao.cod_cta_ctbl_cr >= tt-param.c-cta-ctbl-ini
            AND dvv_log_provisao.cod_cta_ctbl_cr <= tt-param.c-cta-ctbl-fim THEN DO:

                create tt-provisao.
                assign c-num_seq_lancto_ctbl      = c-num_seq_lancto_ctbl + 1
                       tt-provisao.tipo           = "T"
                       tt-provisao.movto          = "PrÇvia Previs∆o"
                       tt-provisao.conta          = dvv_log_provisao.cod_cta_ctbl_cr
                       tt-provisao.modulo         = "FGL"
                       tt-provisao.dta-ctbz       = tt-param.dt-ctbl-fim
                       tt-provisao.num-lote       = c-num_lote_ctbl      
                       tt-provisao.num-lancto     = c-num_lancto_ctbl    
                       tt-provisao.num-seq-lancto = c-num_seq_lancto_ctbl
                       tt-provisao.mov            = "CR"
                       tt-provisao.moeda          = cRealVenda                  
                       tt-provisao.cod-estabel    = dvv_log_provisao.cod_estab  
                       tt-provisao.dt-lote        = ?                           
                       tt-provisao.user-lote      = ""                          
                       tt-provisao.usuario-impl   = ""
                       tt-provisao.cod-unid-negoc = dvv_log_provisao.cod_un 
                       tt-provisao.cod-unid-negoc-gerenc = dvv_log_provisao.cod_un
                       tt-provisao.doc-origem     = "PrÇvia de Lancamento CR de Provis∆o " + dvv_log_provisao.origem_movto
                       tt-provisao.val-lancto     = dvv_log_provisao.vl_provisao.

                 FOR EACH dvv_log_prov_desp NO-LOCK
                     WHERE dvv_log_prov_desp.num_id_provisao = dvv_log_provisao.num_id_provisao:

                     RUN pi-monta-previa-provisao-linha-d.
                 END.
            END.  */
        END.
    END.

END PROCEDURE.


PROCEDURE pi-monta-previa-provisao-linha-d:

    create b-tt-provisao.
    buffer-copy tt-provisao to b-tt-provisao.

    ASSIGN i-seq-classif = i-seq-classif + 1.
    ASSIGN b-tt-provisao.tipo-col   = "D"
           b-tt-provisao.val-lancto = 0
           b-tt-provisao.seq-class  = i-seq-classif
           b-tt-provisao.l-rastr-desp  = YES
           b-tt-provisao.tp-origem = 14.

    IF dvv_log_provisao.origem_movto = "PDVV MI" THEN DO:
        ASSIGN b-tt-provisao.nr-nota-fis  = dvv_log_prov_desp.nr_proc_exp
               b-tt-provisao.serie = "6".
    END.
    ELSE 
        ASSIGN b-tt-provisao.num-processo  = dvv_log_prov_desp.nr_proc_exp.

    for first es_param_estab no-lock
        where es_param_estab.cod_estabel = b-tt-provisao.cod-estabel
          and es_param_Estab.cod_estabel_trd ne "".        

        FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
            WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel_trd
              AND processo-exp.nr-proc-exp = b-tt-provisao.num-processo,
            FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.

            ASSIGN b-tt-provisao.cod-emitente = processo-exp.cod-emitente
                   b-tt-provisao.nome-abrev   = emitente.nome-abrev
                   b-tt-provisao.regiao       = emitente.nome-mic-reg.                   
        END.    
        IF NOT AVAIL processo-exp THEN DO:
            FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel
                  AND processo-exp.nr-proc-exp = b-tt-provisao.num-processo,
                FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                    

                ASSIGN b-tt-provisao.cod-emitente = processo-exp.cod-emitente
                       b-tt-provisao.nome-abrev   = emitente.nome-abrev
                       b-tt-provisao.regiao       = emitente.nome-mic-reg.
            END.    
        END.
    end.
    if not avail es_param_estab then do:
        FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
            WHERE processo-exp.cod-estabel = b-tt-provisao.cod-estabel
              AND processo-exp.nr-proc-exp = b-tt-provisao.num-processo,
            FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                                   

            ASSIGN b-tt-provisao.cod-emitente = processo-exp.cod-emitente
                   b-tt-provisao.nome-abrev   = emitente.nome-abrev
                   b-tt-provisao.regiao       = emitente.nome-mic-reg.

        END. 
    end.

    IF NOT AVAIL processo-exp THEN DO:
        FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
            WHERE processo-exp.cod-estabel = "001"
              AND processo-exp.nr-proc-exp = b-tt-provisao.num-processo,
            FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.  

            ASSIGN b-tt-provisao.cod-emitente = processo-exp.cod-emitente
                   b-tt-provisao.nome-abrev   = emitente.nome-abrev
                   b-tt-provisao.regiao       = emitente.nome-mic-reg.

        END. 
    END.

    IF AVAIL processo-exp THEN
        RUN pi-busca-data-receita-2.

     /* Valores da despesa. */
    assign b-tt-provisao.classif    = dvv_log_provisao.origem_movto
           b-tt-provisao.cod-desp   = dvv_log_prov_desp.cod_despesa
           b-tt-provisao.dt-prev    = dvv_log_provisao.dt_provisao
           b-tt-provisao.vl-prev    = dvv_log_prov_desp.vl_desp
           b-tt-provisao.vl-real    = dvv_log_prov_desp.vl_desp
           b-tt-provisao.val-doc    = dvv_log_prov_desp.vl_desp.

END PROCEDURE.



/***********
PREVIA
***********/
procedure pi-dados-titulo-apb-previa-dvvme:

    DEF VAR c_processo AS CHAR NO-UNDO.
    DEF VAR v_tot_base AS DEC  NO-UNDO.
    DEF VAR v_tot_parc AS DEC  NO-UNDO.
    DEF VAR i-num-id-tit-ap AS INTEGER NO-UNDO.
    DEF VAR i-num-id-tit-ap-ant AS INTEGER NO-UNDO.
    DEF VAR c-chave-origem-busca AS CHAR NO-UNDO.


    /* aqui */
    IF l-log THEN DO:
        OUTPUT TO c:\temp\dvvlog.txt APPEND.
        PUT "val_aprop_ctbl_ap.val_aprop_ctbl " val_aprop_ctbl_ap.val_aprop_ctbl SKIP
            "titulo " tit_ap.cod_tit_Ap " / " tit_Ap.num_id_tit_ap SKIP.
        OUTPUT CLOSE.
    END.

    IF CAN-FIND(FIRST tt-rel WHERE
                      tt-rel.cdn-fornec = tit_ap.cdn_fornecedor
                  AND tt-rel.cod-esp    = tit_ap.cod_espec_docto
                  AND tt-rel.cod-titulo = tit_ap.cod_tit_ap
                  AND tt-rel.parcela    = tit_ap.cod_parcela
                  AND tt-rel.trans-ap   = movto_tit_ap.ind_trans_ap
                  AND tt-rel.num-lote   = ITEM_lancto_ctbl.num_lote_ctbl
                  AND tt-rel.num-lancto = ITEM_lancto_ctbl.num_lancto_ctbl
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
                IF l-log THEN DO:
                    OUTPUT TO c:\temp\dvvlog.txt APPEND.
                    PUT "Titulo Original " tit_ap.cod_tit_ap " " tit_ap.cod_espec_docto " parcela " tit_ap.cod_parcela  SKIP.
                    OUTPUT CLOSE.
                END.                                                                
            END.
            ELSE
                RUN pi-chave-tit_ap IN h-bo-dvv-movto-det (INPUT ROWID(tit_ap),
                                                           OUTPUT c-chave-refer).
                
                ASSIGN d1-doc-origem = "".
                IF NUM-ENTRIES(c-chave-refer,"|") > 6 THEN DO:
                    ASSIGN d1-doc-origem = ENTRY(7,c-chave-refer,"|") NO-ERROR.
                    IF d1-doc-origem <> "" THEN
                        ASSIGN ENTRY(7,c-chave-refer,"|") = "".
                    
                END.

                IF l-log THEN DO:
                    OUTPUT TO c:\temp\dvvlog.txt APPEND.
                    PUT "Alteracao Origem c-chave-refer " c-chave-refer  SKIP.
                    OUTPUT CLOSE.
                END.  
                                                           
            if tit_ap.cod_espec_docto = "CA" then do:
            
                FOR EACH movto_comis_repres NO-LOCK
                    WHERE movto_comis_repres.cod_estab          = tit_ap.cod_estab
                          AND movto_comis_repres.num_id_tit_ap  = tit_ap.num_id_tit_ap:
                          
                    FIND tit_acr NO-LOCK
                        WHERE tit_acr.cod_estab      = movto_comis_repres.cod_estab
                          AND tit_acr.num_id_tit_acr = movto_comis_repres.num_id_tit_acr NO-ERROR.  
                          
                end.

            end.       

            IF l-log THEN DO:
                OUTPUT TO c:\temp\dvvlog.txt APPEND.
                PUT "v_tot_parc " v_tot_parc SKIP
                    "v_tot_Base " v_tot_base SKIP.
                OUTPUT CLOSE.
            END.                                                                

            IF CAN-FIND(FIRST dvv_tit_ap WHERE //carlos
                              dvv_tit_ap.cod_estab       = tit_ap.cod_estab
                          and dvv_tit_ap.num_id_tit_ap   = i-num-id-tit-ap) THEN DO:
                          
                for each dvv_tit_ap no-lock where
                         dvv_tit_ap.cod_estab       = tit_ap.cod_estab
                     and dvv_tit_ap.num_id_tit_ap   = i-num-id-tit-ap:
                
                    if dvv_tit_ap.classif = "DVV" then do:
        
                        for first  dvv_movto_det no-lock 
                             WHERE dvv_movto_Det.cod_estabel   = dvv_tit_ap.cod_estab
                               AND dvv_movto_det.num_processo  = dvv_tit_ap.nr_proc_exp
                               and dvv_movto_det.tipo_despesa  = dvv_tit_ap.cod_desp
                               and dvv_movto_det.it_codigo     = dvv_tit_ap.tipo_container
                               and dvv_movto_det.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                               and dvv_movto_det.doc_origem    = dvv_tit_ap.doc_origem:

                            IF v_tot_base <> v_tot_parc THEN DO:

                                FIND FIRST dvv_movto no-lock where
                                           dvv_movto.cod_estabel     = dvv_movto_det.cod_estabel
                                       AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                                       and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.

                                ASSIGN v-val-desp[1] = 0
                                       v-val-desp[2] = 0.
                                IF AVAIL dvv_movto THEN
                                    ASSIGN v-val-desp[1] = ROUND((dvv_movto.VALOR_PREV_R$ * v_tot_parc) / v_tot_base , 2).

                                ASSIGN v-val-desp[2] = ROUND((dvv_movto_det.VALOR_REAL_R$ * v_tot_parc) / v_tot_base , 2).

                            END.

                            IF l-log THEN DO:
                                OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                PUT "pi-cria-dados-dvv v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[2] "dvv" STRING(dvv_movto_det.VALOR_REAL_R$) SKIP.
                                OUTPUT CLOSE.
                            END.

                            IF tit_ap.cod_espec_docto = "CA" THEN DO: /* Comissao */

                                ASSIGN v-val-desp[2] = 0.
                                FOR EACH movto_comis_repres NO-LOCK
                                    WHERE movto_comis_repres.cod_estab      = tit_ap.cod_estab
                                      AND movto_comis_repres.num_id_tit_ap  = tit_ap.num_id_tit_ap:

                                    FIND tit_acr NO-LOCK
                                        WHERE tit_acr.cod_estab      = tit_ap.cod_estab
                                          AND tit_acr.num_id_tit_acr = movto_comis_repres.num_id_tit_acr NO-ERROR.
                                    IF tit_acr.cod_tit_acr = dvv_movto_det.num_processo THEN DO:
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
                            RUN pi-cria-dados-dvv (INPUT "").
                        end.
                    end.
                    else 
                        if dvv_tit_ap.classif = "DEX" then do:

                            /*for first dex_movto no-lock where
                                      dex_movto.cod_estabel   = dvv_tit_ap.cod_estab
                                  AND dex_movto.nr_pedido     = dvv_tit_ap.nr_proc_exp
                                  and dex_movto.nr_processo   = dvv_tit_ap.nr_proc_exp
                                  and dex_movto.tp_despesa    = dvv_tit_ap.cod_desp
                                  and dex_movto.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                                  and dex_movto.doc_origem    = dvv_tit_ap.doc_origem:

                                IF v_tot_base <> v_tot_parc THEN DO:

                                    ASSIGN v-val-desp[1] = 0
                                           v-val-desp[2] = 0.

                                    ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                                    ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).

                                END.

                                IF l-log THEN DO:
                                    OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                    PUT "pi-cria-dados-dex v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[1] "dex" substring(dex_movto.char_1, 11, 10) SKIP.
                                    OUTPUT CLOSE.
                                END.
                                
                                RUN pi-cria-dados-dex (INPUT "Previsto").

                            end.*/



                            /* leitura da nova tabela dex_movto_det */

                            for FIRST dex_movto_det no-lock where
                                      dex_movto_det.cod_estabel   = dvv_tit_ap.cod_estab
                                  /*AND dex_movto_det.nr_pedido     = dvv_tit_ap.nr_proc_exp*/
                                  and dex_movto_det.nr_processo   = dvv_tit_ap.nr_proc_exp
                                  and dex_movto_det.tp_despesa    = dvv_tit_ap.cod_desp
                                  and dex_movto_det.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                                  and dex_movto_det.doc_origem    = dvv_tit_ap.doc_origem:

                                IF v_tot_base <> v_tot_parc THEN DO:

                                    ASSIGN v-val-desp[1] = 0
                                           v-val-desp[2] = 0.

                                    ASSIGN v-val-desp[1] = ROUND((dex_movto_det.valor_real_r$ * v_tot_parc) / v_tot_base , 2).
                                    ASSIGN v-val-desp[2] = ROUND((dex_movto_det.valor_real_r$ * v_tot_parc) / v_tot_base , 2).

                                END.

                                IF l-log THEN DO:
                                    OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                    PUT "pi-cria-dados-dex v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[1] "dex" dex_movto_det.valor_real_r$ SKIP.
                                    OUTPUT CLOSE.
                                END.

                                RUN pi-cria-dados-dex (INPUT "Realizado").

                            end.
                            IF NOT AVAIL dex_movto_det THEN DO: //caso n∆o encontre o dex_movto_det
                                for first dex_movto no-lock where
                                          dex_movto.cod_estabel   = dvv_tit_ap.cod_estab
                                      AND dex_movto.nr_pedido     = dvv_tit_ap.nr_proc_exp
                                      and dex_movto.nr_processo   = dvv_tit_ap.nr_proc_exp
                                      and dex_movto.tp_despesa    = dvv_tit_ap.cod_desp
                                      and dex_movto.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                                      and dex_movto.doc_origem    = dvv_tit_ap.doc_origem:
    
                                    IF v_tot_base <> v_tot_parc THEN DO:
    
                                        ASSIGN v-val-desp[1] = 0
                                               v-val-desp[2] = 0.
    
                                        ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                                        ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).
    
                                    END.
    
                                    IF l-log THEN DO:
                                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                        PUT "pi-cria-dados-dex v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[1] "dex" substring(dex_movto.char_1, 11, 10) SKIP.
                                        OUTPUT CLOSE.
                                    END.
                                    
                                    RUN pi-cria-dados-dex (INPUT "Realizado").
    
                                end.
                            END.
                        end.
                end.
                
            END.
            ELSE DO: /* Se nao achar dvv_tit_ap */
        
                ASSIGN c-chave-origem-busca = c-chave-refer.
                FIND FIRST dvv_tit_nc WHERE
                     //dvv_tit_nc.cod_estab = tit_ap.cod_estab AND
                     dvv_tit_nc.num_id_tit_ap = tit_ap.num_id_tit_ap NO-LOCK NO-ERROR.
                IF AVAIL dvv_tit_nc THEN
                    ASSIGN c-chave-origem-busca = dvv_tit_nc.doc_origem.

                FOR EACH dvv_movto_det NO-LOCK where
                         /* dvv_movto_det.tp_doc_origem    = i-tp-doc-origem
                     AND */ dvv_movto_det.doc_origem       = c-chave-origem-busca: //c-chave-refer:

                    IF v_tot_base <> v_tot_parc THEN DO:
                        
                        FIND FIRST dvv_movto no-lock where
                                   dvv_movto.cod_estabel     = dvv_movto_det.cod_estabel
                               AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                               and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.

                        ASSIGN v-val-desp[1] = 0
                               v-val-desp[2] = 0.
                        IF AVAIL dvv_movto THEN
                            ASSIGN v-val-desp[1] = ROUND((dvv_movto.VALOR_PREV_R$ * v_tot_parc) / v_tot_base , 2).

                        ASSIGN v-val-desp[2] = ROUND((dvv_movto_det.VALOR_REAL_R$ * v_tot_parc) / v_tot_base , 2).
                    END.

                    RUN pi-cria-dados-dvv (INPUT "").
                END.
            
                /* 16/07/2013 - Paulo Barth - Regra para criar relacionamento entre despesa DEX e T°tulo AP. */
                /*FOR EACH dex_movto NO-LOCK where
                         /* dex_movto.tp_doc_origem    = i-tp-doc-origem
                     AND */ dex_movto.doc_origem       = c-chave-refer:

                    ASSIGN v-val-desp[1] = 0
                           v-val-desp[2] = 0.

                    ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                    ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).
    
                    RUN pi-cria-dados-dex (INPUT "Previsto").
                END.*/


                /* regra para leitura da nova tabela dex_movto_det */

                FOR EACH dex_movto_det NO-LOCK where
                         /* dex_movto.tp_doc_origem    = i-tp-doc-origem
                     AND */ dex_movto_det.doc_origem   = c-chave-refer:

                    ASSIGN v-val-desp[1] = 0
                           v-val-desp[2] = 0.

                    ASSIGN v-val-desp[1] = ROUND((dex_movto_det.valor_real_r$ * v_tot_parc) / v_tot_base , 2).
                    ASSIGN v-val-desp[2] = ROUND((dex_movto_det.valor_real_r$ * v_tot_parc) / v_tot_base , 2).
    
                    RUN pi-cria-dados-dex (INPUT "Previsto").
                END.

            END.
        END.
        ELSE DO: /* Fim IF movto_tit_ap.ind_trans_ap_abrev = "IMPL" THEN DO:*/

            IF movto_tit_ap.ind_trans_ap BEGINS "Acerto Valor" THEN DO:

                RUN pi-chave-tit_ap IN h-bo-dvv-movto-det (INPUT ROWID(tit_ap),
                                                           OUTPUT c-chave-refer).

                ASSIGN d1-doc-origem = "".
                IF NUM-ENTRIES(c-chave-refer,"|") > 6 THEN DO:
                    ASSIGN d1-doc-origem = ENTRY(7,c-chave-refer,"|") NO-ERROR.
                    IF d1-doc-origem <> "" THEN
                        ASSIGN ENTRY(7,c-chave-refer,"|") = "".
                END.
                /* retirado pois estava considerando outra conta
                FOR FIRST val_movto_ap NO-LOCK WHERE
                          val_movto_ap.cod_estab           = movto_tit_ap.cod_estab
                      and val_movto_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap
                      AND val_movto_ap.cod_finalid_econ    = "Corrente"
                      AND val_movto_ap.cod_unid_negoc      = aprop_ctbl_ap.cod_unid_negoc:
            
                    ASSIGN v-val-docto = val_movto_ap.val_ajust_val_tit_ap.

                END.
                */
            END.
        END.

        /* Caso n∆o encontre despesa imprime o valor e chave da mediá∆o. */
        IF v-val-docto > 0 THEN DO:

            create b-tt-rel-reg.
            buffer-copy b-tt-rel-base to b-tt-rel-reg.

            ASSIGN b-tt-rel-reg.tipo-col = "D"
                   b-tt-rel-reg.movto    = "Previsto"
                   b-tt-rel-reg.val-lancto = 0
                   b-tt-rel-reg.val-doc    = v-val-docto
                   b-tt-rel-reg.tp-origem    = 2
                   b-tt-rel-reg.doc-origem   = c-chave-refer
                   b-tt-rel-reg.num-processo = c_processo.

            for first es_param_estab no-lock
                where es_param_estab.cod_estabel = b-tt-rel-reg.cod-estabel
                  and es_param_Estab.cod_estabel_trd ne "".        
               
                FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                    WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel_trd
                      AND processo-exp.nr-proc-exp = c_processo,
                    FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                        
                
                    ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                           b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                           /*111 - b-tt-rel-reg.regiao       = emitente.nome-mic-reg**/
                           .
                END. 
                IF NOT AVAIL processo-exp THEN DO:
                    FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                        WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel
                          AND processo-exp.nr-proc-exp = c_processo,
                        FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                    
                    
                        ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                               b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                               /*222 - b-tt-rel-reg.regiao       = emitente.nome-mic-reg**/.
                    END.    
                END.
            end.
            if not avail es_param_estab then do:
                FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                    WHERE processo-exp.cod-estabel = b-tt-rel-reg.cod-estabel
                      AND processo-exp.nr-proc-exp = c_processo,
                    FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                        
                
                    ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                           b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                           /* 3- b-tt-rel-reg.regiao       = emitente.nome-mic-reg**/
                           .
                END.
            end.

            FIND emitente
                WHERE emitente.cod-emitente = tit_ap.cdn_fornecedor NO-LOCK.

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

end procedure.
          

procedure pi-dados-titulo-acr-previa-dvvme:

    DEF VAR c_processo AS CHAR NO-UNDO.
    DEF VAR v_tot_base AS DEC  NO-UNDO.
    DEF VAR v_tot_parc AS DEC  NO-UNDO.
    DEF VAR i-num-id-tit-ap AS INTEGER NO-UNDO.
    DEF VAR i-num-id-tit-ap-ant AS INTEGER NO-UNDO.
    DEF VAR c-chave-origem-busca AS CHAR NO-UNDO.


    /* aqui */
    IF l-log THEN DO:
        OUTPUT TO c:\temp\dvvlog.txt APPEND.
        PUT "val_aprop_ctbl_ap.val_aprop_ctbl " val_aprop_ctbl_ap.val_aprop_ctbl SKIP
            "titulo " tit_ap.cod_tit_Ap " / " tit_Ap.num_id_tit_ap SKIP.
        OUTPUT CLOSE.
    END.

    IF CAN-FIND(FIRST tt-rel WHERE
                      tt-rel.cdn-fornec = tit_acr.cdn_cliente
                  AND tt-rel.cod-esp    = tit_acr.cod_espec_docto
                  AND tt-rel.cod-titulo = tit_acr.cod_tit_acr
                  AND tt-rel.parcela    = tit_acr.cod_parcela
                  AND tt-rel.trans-ap   = movto_tit_acr.ind_trans_acr
                  AND tt-rel.num-lote   = ITEM_lancto_ctbl.num_lote_ctbl
                  AND tt-rel.num-lancto = ITEM_lancto_ctbl.num_lancto_ctbl
                  AND tt-rel.num-seq-lancto = ITEM_lancto_ctbl.num_seq_lancto_ctbl) THEN
        NEXT.

    assign c_processo     = ""
           c-chave-refer  = "".

    if avail tit_acr then do:

        ASSIGN v_tot_base = 0.
        FOR EACH val_tit_acr NO-LOCK WHERE
                 val_tit_acr.cod_estab     = movto_tit_acr.cod_estab
             and val_tit_acr.num_id_tit_acr = movto_tit_acr.num_id_tit_acr
             AND val_tit_acr.cod_finalid_econ = "Corrente":

            ASSIGN v_tot_base = v_tot_base + val_tit_acr.val_origin_tit_acr.

        END.

        ASSIGN v-val-docto = val_aprop_ctbl_acr.val_aprop_ctbl
               v_tot_parc  = v-val-docto.
        
        IF movto_tit_acr.ind_trans_acr_abrev = "IMPL" THEN DO:

            ASSIGN i-num-id-tit-ap     = tit_acr.num_id_tit_acr
                   i-num-id-tit-ap-ant = tit_acr.num_id_tit_acr.

            if tit_acr.ind_tip_espec_docto = "Imposto Taxado" or
               tit_acr.ind_tip_espec_docto = "Imposto Retido" then do:

/*                     RUN pi-retorna-proc-orig (OUTPUT c_processo,       */
/*                                               OUTPUT i-num-id-tit-ap,  */
/*                                               OUTPUT de-aliq-imposto). */
/*                                                                        */
/*                     ASSIGN l-taxado = YES.                             */

            END.

            IF i-num-id-tit-ap-ant <> i-num-id-tit-ap THEN DO:
                FIND b_tit_acr1 NO-LOCK
                    WHERE b_tit_acr1.cod_estab     = tit_acr.cod_estab
                      AND b_tit_acr1.num_id_tit_acr = i-num-id-tit-ap NO-ERROR.
                RUN pi-chave-tit_acr IN h-bo-dvv-movto-det (INPUT ROWID(b_tit_acr1),
                                                           OUTPUT c-chave-refer).
                IF l-log THEN DO:
                    OUTPUT TO c:\temp\dvvlog.txt APPEND.
                    PUT "Titulo Original " tit_acr.cod_tit_acr " " tit_acr.cod_espec_docto " parcela " tit_acr.cod_parcela  SKIP.
                    OUTPUT CLOSE.
                END.                                                                
            END.
            ELSE
                RUN pi-chave-tit_acr IN h-bo-dvv-movto-det (INPUT ROWID(tit_acr),
                                                           OUTPUT c-chave-refer).
                
            ASSIGN d1-doc-origem = "".
            IF NUM-ENTRIES(c-chave-refer,"|") > 6 THEN DO:
                ASSIGN d1-doc-origem = ENTRY(7,c-chave-refer,"|") NO-ERROR.
                IF d1-doc-origem <> "" THEN
                    ASSIGN ENTRY(7,c-chave-refer,"|") = "".
            END.
            IF l-log THEN DO:
                OUTPUT TO c:\temp\dvvlog.txt APPEND.
                PUT "Alteracao Origem c-chave-refer " c-chave-refer  SKIP.
                OUTPUT CLOSE.
            END.  
                                                           
            if tit_acr.cod_espec_docto = "CA" then do:
            
                FOR EACH movto_comis_repres NO-LOCK
                    WHERE movto_comis_repres.cod_estab          = tit_acr.cod_estab
                          AND movto_comis_repres.num_id_tit_acr  = tit_acr.num_id_tit_acr:
                          
                    FIND tit_acr NO-LOCK
                        WHERE tit_acr.cod_estab      = movto_comis_repres.cod_estab
                          AND tit_acr.num_id_tit_acr = movto_comis_repres.num_id_tit_acr NO-ERROR.  
                          
                end.

            end.       

            IF l-log THEN DO:
                OUTPUT TO c:\temp\dvvlog.txt APPEND.
                PUT "v_tot_parc " v_tot_parc SKIP
                    "v_tot_Base " v_tot_base SKIP.
                OUTPUT CLOSE.
            END.                                                                

            IF CAN-FIND(FIRST dvv_tit_ap WHERE //carlos
                              dvv_tit_ap.cod_estab       = tit_acr.cod_estab
                          and dvv_tit_ap.num_id_tit_ap   = i-num-id-tit-ap) THEN DO:
                          
                for each dvv_tit_ap no-lock where
                         dvv_tit_ap.cod_estab       = tit_acr.cod_estab
                     and dvv_tit_ap.num_id_tit_ap   = i-num-id-tit-ap:
                
                    if dvv_tit_ap.classif = "DVV" then do:
        
                        for first  dvv_movto_det no-lock 
                             WHERE dvv_movto_Det.cod_estabel   = dvv_tit_ap.cod_estab
                               AND dvv_movto_det.num_processo  = dvv_tit_ap.nr_proc_exp
                               and dvv_movto_det.tipo_despesa  = dvv_tit_ap.cod_desp
                               and dvv_movto_det.it_codigo     = dvv_tit_ap.tipo_container
                               and dvv_movto_det.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                               and dvv_movto_det.doc_origem    = dvv_tit_ap.doc_origem:

                            IF v_tot_base <> v_tot_parc THEN DO:

                                FIND FIRST dvv_movto no-lock where
                                           dvv_movto.cod_estabel     = dvv_movto_det.cod_estabel
                                       AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                                       and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.

                                ASSIGN v-val-desp[1] = 0
                                       v-val-desp[2] = 0.
                                IF AVAIL dvv_movto THEN
                                    ASSIGN v-val-desp[1] = ROUND((dvv_movto.VALOR_PREV_R$ * v_tot_parc) / v_tot_base , 2).

                                ASSIGN v-val-desp[2] = ROUND((dvv_movto_det.VALOR_REAL_R$ * v_tot_parc) / v_tot_base , 2).

                            END.

                            IF l-log THEN DO:
                                OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                PUT "pi-cria-dados-dvv v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[2] "dvv" STRING(dvv_movto_det.VALOR_REAL_R$) SKIP.
                                OUTPUT CLOSE.
                            END.

                            IF tit_ap.cod_espec_docto = "CA" THEN DO: /* Comissao */

                                ASSIGN v-val-desp[2] = 0.
                                FOR EACH movto_comis_repres NO-LOCK
                                    WHERE movto_comis_repres.cod_estab      = tit_acr.cod_estab
                                      AND movto_comis_repres.num_id_tit_acr  = tit_acr.num_id_tit_acr:

                                    FIND tit_acr NO-LOCK
                                        WHERE tit_acr.cod_estab      = tit_ap.cod_estab
                                          AND tit_acr.num_id_tit_acr = movto_comis_repres.num_id_tit_acr NO-ERROR.
                                    IF tit_acr.cod_tit_acr = dvv_movto_det.num_processo THEN DO:
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
                            RUN pi-cria-dados-dvv (INPUT "").
                        end.
                    end.
                    else 
                        if dvv_tit_ap.classif = "DEX" then do:

                            /*for first dex_movto no-lock where
                                      dex_movto.cod_estabel   = dvv_tit_ap.cod_estab
                                  AND dex_movto.nr_pedido     = dvv_tit_ap.nr_proc_exp
                                  and dex_movto.nr_processo   = dvv_tit_ap.nr_proc_exp
                                  and dex_movto.tp_despesa    = dvv_tit_ap.cod_desp
                                  and dex_movto.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                                  and dex_movto.doc_origem    = dvv_tit_ap.doc_origem:

                                IF v_tot_base <> v_tot_parc THEN DO:

                                    ASSIGN v-val-desp[1] = 0
                                           v-val-desp[2] = 0.

                                    ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                                    ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).

                                END.

                                IF l-log THEN DO:
                                    OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                    PUT "pi-cria-dados-dex v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[1] "dex" substring(dex_movto.char_1, 11, 10) SKIP.
                                    OUTPUT CLOSE.
                                END.
                                
                                RUN pi-cria-dados-dex (INPUT "Previsto").

                            end.*/



                            /* leitura da nova tabela dex_movto_det */

                            for FIRST dex_movto_det no-lock where
                                      dex_movto_det.cod_estabel   = dvv_tit_ap.cod_estab
                                  /*AND dex_movto_det.nr_pedido     = dvv_tit_ap.nr_proc_exp*/
                                  and dex_movto_det.nr_processo   = dvv_tit_ap.nr_proc_exp
                                  and dex_movto_det.tp_despesa    = dvv_tit_ap.cod_desp
                                  and dex_movto_det.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                                  and dex_movto_det.doc_origem    = dvv_tit_ap.doc_origem:

                                IF v_tot_base <> v_tot_parc THEN DO:

                                    ASSIGN v-val-desp[1] = 0
                                           v-val-desp[2] = 0.

                                    ASSIGN v-val-desp[1] = ROUND((dex_movto_det.valor_real_r$ * v_tot_parc) / v_tot_base , 2).
                                    ASSIGN v-val-desp[2] = ROUND((dex_movto_det.valor_real_r$ * v_tot_parc) / v_tot_base , 2).

                                END.

                                IF l-log THEN DO:
                                    OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                    PUT "pi-cria-dados-dex v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[1] "dex" dex_movto_det.valor_real_r$ SKIP.
                                    OUTPUT CLOSE.
                                END.

                                RUN pi-cria-dados-dex (INPUT "Realizado").

                            end.
                            IF NOT AVAIL dex_movto_det THEN DO:
                                for first dex_movto no-lock where
                                          dex_movto.cod_estabel   = dvv_tit_ap.cod_estab
                                      AND dex_movto.nr_pedido     = dvv_tit_ap.nr_proc_exp
                                      and dex_movto.nr_processo   = dvv_tit_ap.nr_proc_exp
                                      and dex_movto.tp_despesa    = dvv_tit_ap.cod_desp
                                      and dex_movto.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                                      and dex_movto.doc_origem    = dvv_tit_ap.doc_origem:
    
                                    IF v_tot_base <> v_tot_parc THEN DO:
    
                                        ASSIGN v-val-desp[1] = 0
                                               v-val-desp[2] = 0.
    
                                        ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                                        ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).
    
                                    END.
    
                                    IF l-log THEN DO:
                                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                        PUT "pi-cria-dados-dex v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[1] "dex" substring(dex_movto.char_1, 11, 10) SKIP.
                                        OUTPUT CLOSE.
                                    END.
                                    
                                    RUN pi-cria-dados-dex (INPUT "Realizado").
    
                                end.

                            END.
                        end.
                end.
                
            END.
            ELSE DO: /* Se nao achar dvv_tit_ap */
                ASSIGN c-chave-origem-busca = c-chave-refer.
                FIND FIRST dvv_tit_nc WHERE
                     //dvv_tit_nc.cod_estab = tit_ap.cod_estab AND
                     dvv_tit_nc.num_id_tit_ap = tit_acr.num_id_tit_acr NO-LOCK NO-ERROR.
                IF AVAIL dvv_tit_nc THEN
                    ASSIGN c-chave-origem-busca = dvv_tit_nc.doc_origem.
        
                FOR EACH dvv_movto_det NO-LOCK where
                         /* dvv_movto_det.tp_doc_origem    = i-tp-doc-origem
                     AND */ dvv_movto_det.doc_origem       = c-chave-origem-busca: //c-chave-refer:

                    IF v_tot_base <> v_tot_parc THEN DO:
                        
                        FIND FIRST dvv_movto no-lock where
                                   dvv_movto.cod_estabel     = dvv_movto_det.cod_estabel
                               AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                               and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.

                        ASSIGN v-val-desp[1] = 0
                               v-val-desp[2] = 0.
                        IF AVAIL dvv_movto THEN
                            ASSIGN v-val-desp[1] = ROUND((dvv_movto.VALOR_PREV_R$ * v_tot_parc) / v_tot_base , 2).

                        ASSIGN v-val-desp[2] = ROUND((dvv_movto_det.VALOR_REAL_R$ * v_tot_parc) / v_tot_base , 2).
                    END.

                    RUN pi-cria-dados-dvv (INPUT "").
                END.
            
                /* 16/07/2013 - Paulo Barth - Regra para criar relacionamento entre despesa DEX e T°tulo AP. */
                /*FOR EACH dex_movto NO-LOCK where
                         /* dex_movto.tp_doc_origem    = i-tp-doc-origem
                     AND */ dex_movto.doc_origem       = c-chave-refer:

                    ASSIGN v-val-desp[1] = 0
                           v-val-desp[2] = 0.

                    ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                    ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).
    
                    RUN pi-cria-dados-dex (INPUT "Previsto").
                END.*/


                /* regra para leitura da nova tabela dex_movto_det */

                IF CAN-FIND (FIRST dex_movto_det WHERE
                                   dex_movto_det.doc_origem = c-chave-refer) THEN do:
                    FOR EACH dex_movto_det NO-LOCK where
                             /* dex_movto.tp_doc_origem    = i-tp-doc-origem
                         AND */ dex_movto_det.doc_origem   = c-chave-refer:
    
                        ASSIGN v-val-desp[1] = 0
                               v-val-desp[2] = 0.
    
                        ASSIGN v-val-desp[1] = ROUND((dex_movto_det.valor_real_r$ * v_tot_parc) / v_tot_base , 2).
                        ASSIGN v-val-desp[2] = ROUND((dex_movto_det.valor_real_r$ * v_tot_parc) / v_tot_base , 2).
        
                        RUN pi-cria-dados-dex (INPUT "Previsto").
                    END.
                END.
                ELSE DO:
                    FOR EACH dex_movto NO-LOCK where
                             /* dex_movto.tp_doc_origem    = i-tp-doc-origem
                         AND */ dex_movto.doc_origem       = c-chave-refer:
    
                        ASSIGN v-val-desp[1] = 0
                               v-val-desp[2] = 0.
    
                        ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                        ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).
        
                        RUN pi-cria-dados-dex (INPUT "Previsto").
                    END.

                END.

            END.
        END.
        ELSE DO: /* Fim IF movto_tit_ap.ind_trans_ap_abrev = "IMPL" THEN DO:*/

            IF movto_tit_ap.ind_trans_ap BEGINS "Acerto Valor" THEN DO:

                RUN pi-chave-tit_ap IN h-bo-dvv-movto-det (INPUT ROWID(tit_ap),
                                                           OUTPUT c-chave-refer).

                ASSIGN d1-doc-origem = "".
                IF NUM-ENTRIES(c-chave-refer,"|") > 6 THEN DO:                
                    ASSIGN d1-doc-origem = ENTRY(7,c-chave-refer,"|") NO-ERROR.
                    IF d1-doc-origem <> "" THEN
                        ASSIGN ENTRY(7,c-chave-refer,"|") = "".
                END.
                /* retirado pois estava considerando outra conta
                FOR FIRST val_movto_ap NO-LOCK WHERE
                          val_movto_ap.cod_estab           = movto_tit_ap.cod_estab
                      and val_movto_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap
                      AND val_movto_ap.cod_finalid_econ    = "Corrente"
                      AND val_movto_ap.cod_unid_negoc      = aprop_ctbl_ap.cod_unid_negoc:
            
                    ASSIGN v-val-docto = val_movto_ap.val_ajust_val_tit_ap.

                END.
                */
            END.
        END.

        /* Caso n∆o encontre despesa imprime o valor e chave da mediá∆o. */
        IF v-val-docto > 0 THEN DO:

            create b-tt-rel-reg.
            buffer-copy b-tt-rel-base to b-tt-rel-reg.

            ASSIGN b-tt-rel-reg.tipo-col = "D"
                   b-tt-rel-reg.movto    = "Previsto"
                   b-tt-rel-reg.val-lancto = 0
                   b-tt-rel-reg.val-doc    = v-val-docto
                   b-tt-rel-reg.tp-origem    = 2
                   b-tt-rel-reg.doc-origem   = c-chave-refer
                   b-tt-rel-reg.num-processo = c_processo.

            for first es_param_estab no-lock
                where es_param_estab.cod_estabel = b-tt-rel-reg.cod-estabel
                  and es_param_Estab.cod_estabel_trd ne "".        
               
                FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                    WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel_trd
                      AND processo-exp.nr-proc-exp = c_processo,
                    FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                        
                
                    ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                           b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                           /*111 - b-tt-rel-reg.regiao       = emitente.nome-mic-reg**/
                           .
                END. 
                IF NOT AVAIL processo-exp THEN DO:
                    FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                        WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel
                          AND processo-exp.nr-proc-exp = c_processo,
                        FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                    
                    
                        ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                               b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                               /*222 - b-tt-rel-reg.regiao       = emitente.nome-mic-reg**/.
                    END.    
                END.
            end.
            if not avail es_param_estab then do:
                FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                    WHERE processo-exp.cod-estabel = b-tt-rel-reg.cod-estabel
                      AND processo-exp.nr-proc-exp = c_processo,
                    FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.                        
                
                    ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                           b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                           /* 3- b-tt-rel-reg.regiao       = emitente.nome-mic-reg**/
                           .
                END.
            end.

            FIND emitente
                WHERE emitente.cod-emitente = tit_acr.cdn_cliente NO-LOCK.

            /* Dados do t°tulo ACR. */
            assign b-tt-rel-reg.cdn-fornec    = tit_acr.cdn_cliente
                   b-tt-rel-reg.nome-forn     = emitente.nome-abrev
                   b-tt-rel-reg.cod-esp       = tit_acr.cod_espec_docto
                   b-tt-rel-reg.cod-titulo    = tit_acr.cod_tit_acr
                   b-tt-rel-reg.parcela       = tit_acr.cod_parcela
                   b-tt-rel-reg.trans-ap      = movto_tit_acr.ind_trans_acr
                   b-tt-rel-reg.usuario-impl  = movto_tit_acr.cod_usuario.

            if tit_acr.ind_tip_espec_docto = "Imposto Taxado" or
               tit_acr.ind_tip_espec_docto = "Imposto Retido" THEN
                ASSIGN b-tt-rel-reg.tp-origem = 15.

            ASSIGN v-val-docto = 0.

        END.
    END.

end procedure.




procedure pi-dados-titulo-apb-previa-dvvmi:

    DEF VAR c_processo AS CHAR NO-UNDO.
    DEF VAR v_tot_base AS DEC  NO-UNDO.
    DEF VAR v_tot_parc AS DEC  NO-UNDO.
    DEF VAR i-num-id-tit-ap AS INTEGER NO-UNDO.
    DEF VAR i-num-id-tit-ap-ant AS INTEGER NO-UNDO. 

    run pi-acompanhar in h-acomp("Dados titulo apb: " + tt-cta-ctbl.cod-conta).

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

        //MESSAGE aprop_ctbl_ap.val_aprop_ctbl SKIP val_aprop_ctbl_ap.val_aprop_ctbl VIEW-AS ALERT-BOX.
        ASSIGN v-val-docto = val_aprop_ctbl_ap.val_aprop_ctbl
               v_tot_parc  = v-val-docto. 

        IF movto_tit_ap.ind_trans_ap_abrev = "IMPL" THEN DO:

            ASSIGN i-num-id-tit-ap     = tit_ap.num_id_tit_ap
                   i-num-id-tit-ap-ant = tit_ap.num_id_tit_ap.

            if tit_ap.ind_tip_espec_docto = "Imposto Taxado" or
               tit_ap.ind_tip_espec_docto = "Imposto Retido" then do:

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
                                                           
            //MESSAGE "c-chave-refer = " c-chave-refer VIEW-AS ALERT-BOX.

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

                        RUN pi-cria-dados-dvv-mi (INPUT c-chave-refer).
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

                    RUN pi-cria-dados-dvv-mi (INPUT c-chave-refer).
                END.
            END.
        END.
        ELSE DO:

            IF movto_tit_ap.ind_trans_ap BEGINS "Acerto Valor" THEN DO:


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
                   b-tt-rel-reg.movto    = "Previsto"
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
    //END.
end procedure.


PROCEDURE pi-cria-dados-dvv-mi:
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

    //MESSAGE "av-val-docto = " v-val-docto VIEW-AS ALERT-BOX.
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
         
        assign //b-tt-rel-reg.dt-conf      = nota-fiscal.dt-emis-nota -	0524-003623
               b-tt-rel-reg.cod-emitente = nota-fiscal.cod-emitente
               b-tt-rel-reg.nome-abrev   = nota-fiscal.nome-ab-cli
               b-tt-rel-reg.serie        = nota-fiscal.serie       
               b-tt-rel-reg.nr-nota-fis  = nota-fiscal.nr-nota-fis .
               
        find first emitente no-lock
           where emitente.cod-emitente = nota-fiscal.cod-emitente no-error.
        if avail emitente then do:
         
            ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
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

    //MESSAGE b-tt-rel-reg.vl-real SKIP b-tt-rel-reg.vl-real-tot VIEW-AS ALERT-BOX.
    ASSIGN v-val-desp[1] = 0
           v-val-desp[2] = 0.

    for first DVV_TIPO_DESPESA no-lock where
              DVV_TIPO_DESPESA.codigo = dvv_movto_mi_det.tipo_despesa:
        assign b-tt-rel-reg.des-desp = DVV_TIPO_DESPESA.descricao.
    end.
    
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

    IF AVAIL tit_acr THEN DO:

        /* Dados do t°tulo ACR. */
        assign b-tt-rel-reg.cdn-fornec    = tit_acr.cdn_cliente
               b-tt-rel-reg.cod-esp       = tit_acr.cod_espec_docto
               b-tt-rel-reg.cod-titulo    = tit_acr.cod_tit_acr
               b-tt-rel-reg.parcela       = tit_acr.cod_parcela.
               
        FIND FIRST cliente NO-LOCK
            WHERE cliente.cdn_cliente = tit_acr.cdn_cliente NO-ERROR.
        IF AVAIL cliente THEN
            ASSIGN b-tt-rel-reg.cdn-fornec = tit_acr.cdn_cliente
                   b-tt-rel-reg.nome-forn  = cliente.nom_abrev.

        IF AVAIL movto_tit_acr THEN
            ASSIGN b-tt-rel-reg.trans-ap  = movto_tit_acr.ind_trans_acr
                   b-tt-rel-reg.usuario-impl = movto_tit_acr.cod_usuario.

        if tit_ap.ind_tip_espec_docto = "Imposto Taxado" or
           tit_ap.ind_tip_espec_docto = "Imposto Retido" THEN
            ASSIGN b-tt-rel-reg.tp-origem = 15.
            
        /* 
        ** Verifica se o t°tulo teve origem no GFE
        */ 
        IF tit_acr.cod_espec_docto = "TR" then do:
        
            assign b-tt-rel-reg.tp-origem = 10.

        end.

    END.

    //MESSAGE "tt-rel-total" VIEW-AS ALERT-BOX.
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
    //MESSAGE "l-log = " l-log VIEW-AS ALERT-BOX.
        IF l-log THEN DO:
            OUTPUT TO c:\temp\logdvv0199ab.txt APPEND.
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


procedure pi-dados-rec-mi:

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
        PUT "pi-dados-rec2" 
            movto-estoq.cod-estabel   
            movto-estoq.ct-codigo     
            movto-estoq.dt-trans      
            movto-estoq.cod-unid-negoc SKIP

            .
        OUTPUT CLOSE.
    END.

    run pi-acompanhar in h-acomp("Dados REC: " + tt-cta-ctbl.cod-conta).

    /*
    tt-block:
    for each tt-movto-cep where
             tt-movto-cep.cod_modul_dtsul = "CEP"
         and tt-movto-cep.cod_estab       = movto-estoq.cod-estabel
         and tt-movto-cep.cod_cta_ctbl    = movto-estoq.ct-codigo
         and tt-movto-cep.dat_lancto_ctbl = movto-estoq.dt-trans
         AND tt-movto-cep.cod-unid-neg    = movto-estoq.cod-unid-negoc:
        */
    FIND FIRST tt-movto-cep WHERE
               tt-movto-cep.num_id_movto = movto-estoq.nr-trans NO-LOCK NO-ERROR.

    IF AVAIL tt-movto-cep THEN do: 
        ASSIGN d-1 = d-1 + tt-movto-cep.val_lancto_ctbl.

        ASSIGN v_val_ant = 0.
        FOR EACH tt-acum-medicao: DELETE tt-acum-medicao. END.

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
    
                    run pi-chave-medicao-contrato IN h-bo-dvv-movto-det (rowid(medicao-contrat), output c-chave-refer).
    
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
    
                    RUN pi-cria-dados-dvv-mi (INPUT "").
        
                    RUN pi-grava-dados-desp-tit-ap-mi.
                END.
    
                IF v-val-docto > 0 THEN DO:

                    create b-tt-rel-reg.
                    buffer-copy b-tt-rel-base to b-tt-rel-reg.
    
                    ASSIGN b-tt-rel-reg.tipo-col    = "D"
                           b-tt-rel-reg.movto       = "Previsto"
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
                PUT "pi-dados-rec2 NOT AVAIL tt-acum-medicao" SKIP
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
                           b-tt-rel-reg.movto       = "Previsto"
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


PROCEDURE pi-grava-dados-desp-tit-ap-mi:

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

/* *************** Procedure Definitions - End *************** */

PROCEDURE pi-dados-titulo-acr :

DEF VAR c_processo AS CHAR NO-UNDO.
    DEF VAR v_tot_base AS DEC  NO-UNDO.
    DEF VAR v_tot_parc AS DEC  NO-UNDO.
    DEF VAR i-num-id-tit-acr AS INTEGER NO-UNDO.
    DEF VAR i-num-id-tit-acr-ant AS INTEGER NO-UNDO.


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

        find tit_acr no-lock WHERE
             tit_acr.cod_estab       = movto_tit_acr.cod_estab
         and tit_acr.num_id_tit_acr   = movto_tit_acr.num_id_tit_acr no-error.

        FIND FIRST dvv_tit_nc
            WHERE dvv_tit_nc.cod_estab = tit_acr.cod_estab
            AND dvv_tit_nc.num_id_tit_ap = tit_acr.num_id_tit_acr NO-LOCK NO-ERROR.
        IF NOT AVAIL dvv_tit_nc THEN NEXT.

        IF l-log THEN DO:
            OUTPUT TO c:\temp\dvvlog.txt APPEND.
            PUT "val_aprop_ctbl_ap.val_aprop_ctbl " val_aprop_ctbl_ap.val_aprop_ctbl SKIP
                "titulo " tit_acr.cod_tit_acr " / " tit_acr.num_id_tit_acr SKIP.
            OUTPUT CLOSE.
        END.

        IF CAN-FIND(FIRST tt-rel WHERE
                          tt-rel.cdn-fornec = tit_acr.cdn_cliente
                      AND tt-rel.cod-esp    = tit_acr.cod_espec_docto
                      AND tt-rel.cod-titulo = tit_acr.cod_tit_acr
                      AND tt-rel.parcela    = tit_acr.cod_parcela
                      AND tt-rel.trans-ap   = movto_tit_acr.ind_trans_acr
                      AND tt-rel.num-lote   = ITEM_lancto_ctbl.num_lote_ctbl
                      AND tt-rel.num-lancto = ITEM_lancto_ctbl.num_lancto_ctbl
                      AND tt-rel.num-seq-lancto = ITEM_lancto_ctbl.num_seq_lancto_ctbl) THEN
            NEXT.

        assign c_processo     = ""
               c-chave-refer  = "".

        if avail tit_acr then do:

            ASSIGN v_tot_base = 0.
            FOR EACH val_tit_acr NO-LOCK WHERE
                     val_tit_acr.cod_estab     = movto_tit_acr.cod_estab
                 and val_tit_acr.num_id_tit_acr = movto_tit_acr.num_id_tit_acr
                 AND val_tit_acr.cod_finalid_econ = "Corrente":

                ASSIGN v_tot_base = v_tot_base + val_tit_acr.val_origin_tit_acr.

            END.

            ASSIGN v-val-docto = val_aprop_ctbl_acr.val_aprop_ctbl
                   v_tot_parc  = v-val-docto.

            IF movto_tit_acr.ind_trans_acr_abrev = "IMPL" THEN DO:

                ASSIGN i-num-id-tit-acr     = tit_acr.num_id_tit_acr
                       i-num-id-tit-acr-ant = tit_acr.num_id_tit_acr.

                IF i-num-id-tit-acr-ant <> i-num-id-tit-acr THEN DO:

                    FIND b_tit_acr1 NO-LOCK
                        WHERE b_tit_acr1.cod_estab     = tit_acr.cod_estab
                          AND b_tit_acr1.num_id_tit_acr = i-num-id-tit-acr NO-ERROR.
                    RUN pi-chave-tit_ap IN h-bo-dvv-movto-det (INPUT ROWID(b_tit_acr1),
                                                               OUTPUT c-chave-refer).
                    IF l-log THEN DO:
                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                        PUT "Titulo Original " tit_acr.cod_tit_acr " " tit_acr.cod_espec_docto " parcela " tit_acr.cod_parcela  SKIP.
                        OUTPUT CLOSE.
                    END.
                END.
                ELSE
                    RUN pi-chave-tit_acr IN h-bo-dvv-movto-det (INPUT ROWID(tit_acr),
                                                               OUTPUT c-chave-refer).

                    ASSIGN d1-doc-origem = "".
                    IF NUM-ENTRIES(c-chave-refer,"|") > 6 THEN DO:
                        ASSIGN d1-doc-origem = ENTRY(7,c-chave-refer,"|") NO-ERROR.
                        IF d1-doc-origem <> "" THEN
                            ASSIGN ENTRY(7,c-chave-refer,"|") = "".
                    END.
                    IF l-log THEN DO:
                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                        PUT "Alteracao Origem c-chave-refer " c-chave-refer  SKIP.
                        OUTPUT CLOSE.
                    END.

                if tit_acr.cod_espec_docto = "CA" then do:

                    FOR EACH movto_comis_repres NO-LOCK
                        WHERE movto_comis_repres.cod_estab          = tit_acr.cod_estab
                              AND movto_comis_repres.num_id_tit_acr  = tit_acr.num_id_tit_acr:

                        FIND tit_acr NO-LOCK
                            WHERE tit_acr.cod_estab      = movto_comis_repres.cod_estab
                              AND tit_acr.num_id_tit_acr = movto_comis_repres.num_id_tit_ap NO-ERROR.

                    end.

                end.

                    IF l-log THEN DO:
                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                        PUT "v_tot_parc " v_tot_parc SKIP
                            "v_tot_Base " v_tot_base SKIP.
                        OUTPUT CLOSE.
                    END.

                IF CAN-FIND(FIRST dvv_tit_ap where
                                  dvv_tit_ap.cod_estab       = tit_acr.cod_estab
                              and dvv_tit_ap.num_id_tit_ap   = i-num-id-tit-acr) THEN DO:

                    for each dvv_tit_ap no-lock where
                             dvv_tit_ap.cod_estab       = tit_acr.cod_estab
                         and dvv_tit_ap.num_id_tit_ap   = i-num-id-tit-acr:

                        if dvv_tit_ap.classif = "DVV" then do:

                            FOR first  dvv_movto_det no-lock
                                 WHERE dvv_movto_Det.cod_estabel   = dvv_tit_ap.cod_estab
                                   AND dvv_movto_det.num_processo  = dvv_tit_ap.nr_proc_exp
                                   and dvv_movto_det.tipo_despesa  = dvv_tit_ap.cod_desp
                                   and dvv_movto_det.it_codigo     = dvv_tit_ap.tipo_container
                                   and dvv_movto_det.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                                   and dvv_movto_det.doc_origem    = dvv_tit_ap.doc_origem:

                                IF v_tot_base <> v_tot_parc THEN DO:

                                    FIND FIRST dvv_movto no-lock where
                                               dvv_movto.cod_estabel     = dvv_movto_det.cod_estabel
                                           AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                                           and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.

                                    ASSIGN v-val-desp[1] = 0
                                           v-val-desp[2] = 0.
                                    IF AVAIL dvv_movto THEN
                                        ASSIGN v-val-desp[1] = ROUND((dvv_movto.VALOR_PREV_R$ * v_tot_parc) / v_tot_base , 2).

                                    ASSIGN v-val-desp[2] = ROUND((dvv_movto_det.VALOR_REAL_R$ * v_tot_parc) / v_tot_base , 2).

                                END.

                                IF l-log THEN DO:
                                    OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                    PUT "pi-cria-dados-dvv v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[2] "dvv" STRING(dvv_movto_det.VALOR_REAL_R$) SKIP.
                                    OUTPUT CLOSE.
                                END.

                                IF tit_acr.cod_espec_docto = "CA" THEN DO: /* Comissao */

                                    ASSIGN v-val-desp[2] = 0.
                                    FOR EACH movto_comis_repres NO-LOCK
                                        WHERE movto_comis_repres.cod_estab      = tit_acr.cod_estab
                                          AND movto_comis_repres.num_id_tit_acr  = tit_acr.num_id_tit_acr:

                                        FIND tit_acr NO-LOCK
                                            WHERE tit_acr.cod_estab      = tit_acr.cod_estab
                                              AND tit_acr.num_id_tit_acr = movto_comis_repres.num_id_tit_acr NO-ERROR.
                                        IF tit_acr.cod_tit_acr = dvv_movto_det.num_processo THEN DO:
                                            CASE tit_acr.cod_indic_econ:
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
                                                              tit_acr.dat_emis_docto,
                                                              OUTPUT v-val-desp[2]).
                                            ELSE
                                                ASSIGN v-val-desp[2] = movto_comis_repres.val_movto_comis.
                                        END.

                                    END.

                                END.

                                RUN pi-cria-dados-dvv (INPUT "").

                            end.
                        end.

                        else
                            if dvv_tit_ap.classif = "DEX" then do:

                                for first dex_movto no-lock where
                                          dex_movto.cod_estabel   = dvv_tit_ap.cod_estab
                                      AND dex_movto.nr_pedido     = dvv_tit_ap.nr_proc_exp
                                      and dex_movto.nr_processo   = dvv_tit_ap.nr_proc_exp
                                      and dex_movto.tp_despesa    = dvv_tit_ap.cod_desp
                                      and dex_movto.tp_doc_origem = dvv_tit_ap.tp_doc_origem
                                      and dex_movto.doc_origem    = dvv_tit_ap.doc_origem:

                                    IF v_tot_base <> v_tot_parc THEN DO:

                                        ASSIGN v-val-desp[1] = 0
                                               v-val-desp[2] = 0.

                                        ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                                        ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).

                                    END.

                                    IF l-log THEN DO:
                                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                        PUT "pi-cria-dados-dex v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[1] "dex" substring(dex_movto.char_1, 11, 10) SKIP.
                                        OUTPUT CLOSE.
                                    END.

                                    RUN pi-cria-dados-dex (INPUT "Realizado").

                                end.
                            end.
                    end.

                END. //carlos inicio
                ELSE IF CAN-FIND(FIRST dvv_tit_nc where
                                  dvv_tit_nc.cod_estab       = tit_acr.cod_estab
                              and dvv_tit_nc.num_id_tit_ap   = i-num-id-tit-acr) THEN DO:

                    for each dvv_tit_nc no-lock where
                             dvv_tit_nc.cod_estab       = tit_acr.cod_estab
                         and dvv_tit_nc.num_id_tit_ap   = i-num-id-tit-acr:

                        if dvv_tit_nc.classif = "DVV" then do:

                            for first  dvv_movto_det no-lock //carlos
                                 WHERE dvv_movto_Det.cod_estabel   <> ""
                                   AND dvv_movto_det.num_processo  = dvv_tit_nc.nr_proc_exp
                                   and dvv_movto_det.tipo_despesa  = dvv_tit_nc.cod_desp
                                   and dvv_movto_det.it_codigo     = ""
                                   and dvv_movto_det.tp_doc_origem = dvv_tit_nc.tp_doc_origem
                                   and dvv_movto_det.doc_origem    = dvv_tit_nc.doc_origem:

                                IF v_tot_base <> v_tot_parc THEN DO:

                                    FIND FIRST dvv_movto no-lock where
                                               dvv_movto.cod_estabel     = dvv_movto_det.cod_estabel
                                           AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                                           and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.

                                    ASSIGN v-val-desp[1] = 0
                                           v-val-desp[2] = 0.
                                    IF AVAIL dvv_movto THEN
                                        ASSIGN v-val-desp[1] = ROUND((dvv_movto.VALOR_PREV_R$ * v_tot_parc) / v_tot_base , 2).

                                    ASSIGN v-val-desp[2] = ROUND((dvv_movto_det.VALOR_REAL_R$ * v_tot_parc) / v_tot_base , 2).

                                END.

                                IF l-log THEN DO:
                                    OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                    PUT "pi-cria-dados-dvv v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[2] "dvv" STRING(dvv_movto_det.VALOR_REAL_R$) SKIP.
                                    OUTPUT CLOSE.
                                END.

                                IF tit_acr.cod_espec_docto = "CA" THEN DO: /* Comissao */

                                    ASSIGN v-val-desp[2] = 0.
                                    FOR EACH movto_comis_repres NO-LOCK
                                        WHERE movto_comis_repres.cod_estab      = tit_acr.cod_estab
                                          AND movto_comis_repres.num_id_tit_acr  = tit_acr.num_id_tit_acr:

                                        FIND tit_acr NO-LOCK
                                            WHERE tit_acr.cod_estab      = tit_acr.cod_estab
                                              AND tit_acr.num_id_tit_acr = movto_comis_repres.num_id_tit_acr NO-ERROR.
                                        IF tit_acr.cod_tit_acr = dvv_movto_det.num_processo THEN DO:
                                            CASE tit_acr.cod_indic_econ:
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
                                                              tit_acr.dat_emis_docto,
                                                              OUTPUT v-val-desp[2]).
                                            ELSE
                                                ASSIGN v-val-desp[2] = movto_comis_repres.val_movto_comis.
                                        END.

                                    END.

                                END.

                                RUN pi-cria-dados-dvv (INPUT "").

                            end.
                        end.
                        else
                            if dvv_tit_nc.classif = "DEX" then do:

                                for first dex_movto no-lock where
                                          dex_movto.cod_estabel   <> ""
                                      AND dex_movto.nr_pedido     = dvv_tit_nc.nr_proc_exp
                                      and dex_movto.nr_processo   = dvv_tit_nc.nr_proc_exp
                                      and dex_movto.tp_despesa    = dvv_tit_nc.cod_desp
                                      and dex_movto.tp_doc_origem = dvv_tit_nc.tp_doc_origem
                                      and dex_movto.doc_origem    = dvv_tit_nc.doc_origem:

                                    IF v_tot_base <> v_tot_parc THEN DO:

                                        ASSIGN v-val-desp[1] = 0
                                               v-val-desp[2] = 0.

                                        ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                                        ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).

                                    END.

                                    IF l-log THEN DO:
                                        OUTPUT TO c:\temp\dvvlog.txt APPEND.
                                        PUT "pi-cria-dados-dex v_Tot_parc " v_tot_parc " v_tot_base " v_tot_base " v-val-desp[2] " v-val-desp[1] "dex" substring(dex_movto.char_1, 11, 10) SKIP.
                                        OUTPUT CLOSE.
                                    END.

                                    RUN pi-cria-dados-dex (INPUT "Realizado").

                                end.
                            end.
                    end.

                END. //carlos fim
                ELSE DO: /* Se nao achar dvv_tit_acr */

                    FOR EACH dvv_movto_det NO-LOCK where
                             /* dvv_movto_det.tp_doc_origem    = i-tp-doc-origem
                         AND */ dvv_movto_det.doc_origem       = c-chave-refer:

                        IF v_tot_base <> v_tot_parc THEN DO:

                            FIND FIRST dvv_movto no-lock where
                                       dvv_movto.cod_estabel     = dvv_movto_det.cod_estabel
                                   AND dvv_movto.num_processo    = dvv_movto_det.num_processo
                                   and dvv_movto.tipo_despesa    = dvv_movto_det.tipo_despesa NO-ERROR.

                            ASSIGN v-val-desp[1] = 0
                                   v-val-desp[2] = 0.
                            IF AVAIL dvv_movto THEN
                                ASSIGN v-val-desp[1] = ROUND((dvv_movto.VALOR_PREV_R$ * v_tot_parc) / v_tot_base , 2).

                            ASSIGN v-val-desp[2] = ROUND((dvv_movto_det.VALOR_REAL_R$ * v_tot_parc) / v_tot_base , 2).
                        END.

                        RUN pi-cria-dados-dvv (INPUT "").
                    END.

                    /* 16/07/2013 - Paulo Barth - Regra para criar relacionamento entre despesa DEX e T°tulo AP. */
                    FOR EACH dex_movto NO-LOCK where
                             /* dex_movto.tp_doc_origem    = i-tp-doc-origem
                         AND */ dex_movto.doc_origem       = c-chave-refer:

                        ASSIGN v-val-desp[1] = 0
                               v-val-desp[2] = 0.

                        ASSIGN v-val-desp[1] = ROUND((dec(substring(dex_movto.char_1, 11, 10)) * v_tot_parc) / v_tot_base , 2).
                        ASSIGN v-val-desp[2] = ROUND((dex_movto.valor * v_tot_parc) / v_tot_base , 2).

                        RUN pi-cria-dados-dex (INPUT "Realizado").
                    END.
                END.
            END.
            ELSE DO: /* Fim IF movto_tit_acr.ind_trans_ap_abrev = "IMPL" THEN DO:*/

                IF movto_tit_acr.ind_trans_acr BEGINS "Acerto Valor" THEN DO:

                    RUN pi-chave-tit_ap IN h-bo-dvv-movto-det (INPUT ROWID(tit_acr),
                                                               OUTPUT c-chave-refer).

                    ASSIGN d1-doc-origem = "".
                    IF NUM-ENTRIES(c-chave-refer,"|") > 6 THEN DO:                    
                        ASSIGN d1-doc-origem = ENTRY(7,c-chave-refer,"|") NO-ERROR.
                        IF d1-doc-origem <> "" THEN
                            ASSIGN ENTRY(7,c-chave-refer,"|") = "".
                    END.    

                    FOR FIRST val_movto_tit_acr NO-LOCK WHERE
                              val_movto_tit_acr.cod_estab           = movto_tit_acr.cod_estab
                          and val_movto_tit_acr.num_id_movto_tit_acr = movto_tit_acr.num_id_movto_tit_acr
                          AND val_movto_tit_acr.cod_finalid_econ    = "Corrente"
                          AND val_movto_tit_acr.cod_unid_negoc      = aprop_ctbl_ap.cod_unid_negoc:

                        ASSIGN v-val-docto = val_movto_tit_acr.val_ajust_val_tit_acr.

                    END.

                END.
            END.

            /* Caso n∆o encontre despesa imprime o valor e chave da mediá∆o. */
            IF v-val-docto > 0 THEN DO:

                create b-tt-rel-reg.
                buffer-copy b-tt-rel-base to b-tt-rel-reg.

                //carlos
                FIND FIRST dvv_tit_ap NO-LOCK
                    WHERE dvv_tit_ap.num_id_tit_ap = tit_acr.num_id_tit_acr NO-ERROR.
                IF AVAIL dvv_tit_ap THEN DO:
                    ASSIGN b-tt-rel-reg.tp-origem = dvv_tit_ap.tp_doc_orig.
                END.
                ELSE DO:
                    FIND FIRST dvv_tit_nc NO-LOCK
                        WHERE dvv_tit_nc.num_id_tit_ap = tit_acr.num_id_tit_acr NO-ERROR.
                        IF AVAIL dvv_tit_nc THEN
                            ASSIGN b-tt-rel-reg.tp-origem = dvv_tit_nc.tp_doc_orig.
                        ELSE ASSIGN b-tt-rel-reg.tp-origem = 2.
                END.

                ASSIGN b-tt-rel-reg.tipo-col = "D"
                       b-tt-rel-reg.movto    = "Realizado"
                       b-tt-rel-reg.val-lancto = 0
                       b-tt-rel-reg.val-doc    = v-val-docto
                       // b-tt-rel-reg.tp-origem    = 2
                       b-tt-rel-reg.doc-origem   = c-chave-refer
                       b-tt-rel-reg.num-processo = c_processo.



                for first es_param_estab no-lock
                    where es_param_estab.cod_estabel = b-tt-rel-reg.cod-estabel
                      and es_param_Estab.cod_estabel_trd ne "".

                    FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                        WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel_trd
                          AND processo-exp.nr-proc-exp = c_processo,
                        FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.

                        ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                               b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                               /*111 - b-tt-rel-reg.regiao       = emitente.nome-mic-reg**/
                               .
                    END.
                    IF NOT AVAIL processo-exp THEN DO:
                        FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                            WHERE processo-exp.cod-estabel = es_param_estab.cod_estabel
                              AND processo-exp.nr-proc-exp = c_processo,
                            FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.

                            ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                                   b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                                   /*222 - b-tt-rel-reg.regiao       = emitente.nome-mic-reg**/.
                        END.
                    END.
                end.
                if not avail es_param_estab then do:
                    FOR FIRST processo-exp FIELDS(cod-emitente) NO-LOCK
                        WHERE processo-exp.cod-estabel = b-tt-rel-reg.cod-estabel
                          AND processo-exp.nr-proc-exp = c_processo,
                        FIRST emitente FIELDS(nome-abrev atividade nome-mic-reg) OF processo-exp NO-LOCK.

                        ASSIGN b-tt-rel-reg.cod-emitente = processo-exp.cod-emitente
                               b-tt-rel-reg.nome-abrev   = emitente.nome-abrev
                               /* 3- b-tt-rel-reg.regiao       = emitente.nome-mic-reg**/
                               .
                    END.
                end.

                FIND cliente
                    WHERE cliente.cdn_cliente = tit_acr.cdn_cliente NO-LOCK.

                /* Dados do t°tulo ACR. */
                assign b-tt-rel-reg.cdn-fornec    = tit_acr.cdn_cliente
                       b-tt-rel-reg.nome-forn     = cliente.nom_abrev
                       b-tt-rel-reg.cod-esp       = tit_acr.cod_espec_docto
                       b-tt-rel-reg.cod-titulo    = tit_acr.cod_tit_acr
                       b-tt-rel-reg.parcela       = tit_acr.cod_parcela
                       b-tt-rel-reg.trans-ap      = movto_tit_acr.ind_trans_acr
                       b-tt-rel-reg.usuario-impl  = movto_tit_acr.cod_usuario.

                if tit_acr.ind_tip_espec_docto = "Imposto Taxado" or
                   tit_acr.ind_tip_espec_docto = "Imposto Retido" THEN
                    ASSIGN b-tt-rel-reg.tp-origem = 15.

                ASSIGN v-val-docto = 0.

            END.
        END.
    END.

end procedure.


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

            FIND lote_ctbl OF bITEM_lancto_ctbl NO-ERROR.
    
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

PROCEDURE pi-estab-br:

def input param nome-abrev like ped-venda.nome-abrev.
def input param nr-pedcli like ped-venda.nr-pedcli.

IF NOT AVAIL es_ped_venda_cex THEN DO:
    FIND FIRST  es_ped_venda_cex NO-LOCK
          WHERE es_ped_venda_cex.nome_abrev = nome-abrev
            AND es_ped_venda_cex.nr_pedcli  = nr-pedcli NO-ERROR.

END.

IF NOT AVAIL es_ped_venda_cex OR es_ped_venda_cex.cod-estabel-br = "" THEN DO:
    FIND FIRST  ped-venda NO-LOCK
          WHERE ped-venda.nome-abrev = nome-abrev
            AND ped-venda.nr-pedcli  = nr-pedcli NO-ERROR.
            IF AVAIL ped-venda THEN ASSIGN c-cod-estabel-br = ped-venda.cod-estab.

END.

IF AVAIL es_ped_venda_cex AND es_ped_venda_cex.cod-estabel-br <> "" THEN
    ASSIGN c-cod-estabel-br = es_ped_venda_cex.cod-estabel-br.
    
END.
