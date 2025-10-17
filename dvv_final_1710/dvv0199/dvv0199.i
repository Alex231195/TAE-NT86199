/*----- DEFINICAO DE VARIAVEIS LOCAIS -----*/

define temp-table tt-param
    field destino                 as integer
    field arquivo                 as char
    field usuario                 as char
    field data-exec               as date
    field hora-exec               as integer
    FIELD dt-ctbl-ini             AS DATE
    FIELD dt-ctbl-fim             AS DATE
    FIELD c-cta-ctbl-ini          AS CHAR
    FIELD c-cta-ctbl-fim          AS CHAR
    FIELD l-val-neg               AS LOG
    FIELD l-realizacao-dvv-me     AS LOG
    FIELD l-realizacao-dvv-mi     AS LOG
    FIELD l-provisao-dvv-me       AS LOG
    FIELD l-provisao-dvv-mi       AS LOG
    FIELD l-previa-provisoes      AS LOG
    FIELD l-estorno               AS LOG
    FIELD c-estab-ini             AS CHAR
    FIELD c-estab-fim             AS CHAR
    FIELD c-unid-negoc-ini        AS CHAR
    FIELD c-unid-negoc-fim        AS CHAR
    FIELD l-atualiza-bi           AS LOGICAL
    FIELD l-imprime-bi            AS LOGICAL
    FIELD l-remessa-venda-estoq   AS LOGICAL
    FIELD l-despesa-nao-rast      AS LOGICAL
    FIELD l-fech-previo           AS LOGICAL
    FIELD i-mo-codigo             AS INT
    FIELD de-cotacao              AS DEC.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem.

DEF TEMP-TABLE tt-cta-ctbl no-undo
    FIELD cod-estabel   AS CHAR
    FIELD cod-conta     as char
    FIELD cod-unid      as char
    FIELD cod-ccusto    as char
    FIELD cod-empresa   LIKE estabelecimento.cod_empresa
    FIELD origem        AS CHAR
    INDEX conta IS PRIMARY
        cod-conta
        cod-empresa.

def temp-table tt-rel no-undo
    FIELD tipo-col      AS CHAR
    field conta         as char
    field modulo        as char
    field dta-ctbz      as date
    field num-lote      as int
    FIELD num-lancto    AS INT
    FIELD num-seq-lancto AS INT
    FIELD dt-lote       AS DATE
    FIELD user-lote     AS CHAR
    field mov           as char
    field moeda         as char
    FIELD val-lancto    AS DEC
    FIELD val-doc       AS DEC
    field classif       as char
    field tp-origem     as INT
    field doc-origem    as char
    FIELD l-rastr-desp  AS LOG
    FIELD num-processo  AS CHAR
    field trans-ap      as char
    field cdn-fornec    as int
    field cod-esp       as char
    field cod-titulo    as char
    field parcela       as char
    field cod-desp      as int
    field des-desp      as char
    field vl-prev       as dec
    field dt-prev       as date
    field vl-real       as dec
    field dt-real       as DATE
    FIELD seq-class     AS INT
    FIELD cod-estabel   AS CHAR
    FIELD cod-estab-orig AS CHAR 
    FIELD serie         AS CHAR
    FIELD nr-nota-fis   AS CHAR
    FIELD cod-emitente  AS INT
    FIELD nome-abrev    AS CHAR
    FIELD cod-depos     AS CHAR
    FIELD regiao        AS CHAR
    FIELD segmento      AS CHAR
    FIELD familia       AS CHAR
    FIELD movto         AS CHAR
    FIELD nome-forn     AS CHAR
    FIELD dt-conf       AS DATE
    FIELD it-codigo     AS CHAR
    FIELD desc-conta    AS CHAR
    FIELD grupo-classe  AS CHAR
    FIELD classe-item   AS CHAR
    FIELD grupo-despesa AS CHAR
    FIELD cod-grupo     AS INT
    FIELD desc-grup     AS CHAR
    FIELD vl-real-tot   AS DEC
    FIELD cod-unid-negoc AS CHAR
    FIELD vl-base       AS DEC
	FIELD usuario-impl  AS CHAR
    FIELD cod-unid-negoc-gerenc AS CHAR.

def temp-table tt-rel-item LIKE tt-rel
    field vl-real-item  as dec
    FIELD vl-real-orig  AS DEC
    FIELD vl-prev-item    AS DEC
    FIELD vl-real-tot-item AS DEC
    FIELD tip-oper-exp AS CHAR
    FIELD regra-rateio AS INT
    FIELD val-lancto-2 AS DEC
    FIELD val-doc-2 AS DEC
    FIELD vl-real-2 AS DEC
    FIELD qt-faturada LIKE it-nota-fisc.qt-faturada[1]
    FIELD un-fatur LIKE it-nota-fisc.un-fatur[1]
    .

def temp-table tt-rel-total LIKE tt-rel
    field vl-tot-prev    as dec
    field vl-tot-real    as dec.

def temp-table tt-movto-cep no-undo
    field cod_modul_dtsul       as char
    field cod_estab             as char
    field num_id_movto          as int
    field dat_lancto_ctbl       as date
    field cod_cta_ctbl          as char
    FIELD cod_ccusto            AS CHAR
    field val_lancto_ctbl       as dec
    field especie               as char
    FIELD serie-docto           AS CHAR
    FIELD nat-operacao          AS CHAR
    field nro-docto             as char
    field cod-emitente          as INT
    FIELD it-codigo             AS CHAR
    FIELD sequen-nf             AS INT
    FIELD cod-unid-neg          AS CHAR
    FIELD cod-gr-cli            AS CHAR
    field ge-codigo	            as integer
    FIELD refer-contab AS CHAR.

DEF TEMP-TABLE tt-tms NO-UNDO
    FIELD ep-codigo     AS INT
    FIELD cod-estabel   as char
    FIELD data          as DATE
    FIELD ct-db         as char     FORMat "x(17)"
    FIELD conta-db      as char
    FIELD ct-cr         as char     format "x(17)"
    FIELD conta-cr      AS CHAR
    field tp-docto      as INT
    field cnpj-emissor  as CHAR
    field nr-docto      as INT
    field cd-serie      as char
    field dt-emissao    as DATE
    FIELD valor         as DEC
    .

DEF TEMP-TABLE tt-conta-contab NO-UNDO LIKE conta-contab.

DEF TEMP-TABLE tt-acum-medicao NO-UNDO
    FIELD chave      AS CHAR
    FIELD chave-orig AS CHAR
    FIELD val-docto  AS DEC.

def temp-table tt-raw-digita
   field raw-digita      as raw.

DEF TEMP-TABLE tt-grp-lancto no-undo
    FIELD lin-ini   AS INTEGER
    FIELD lin-fim   AS INTEGER.

DEF TEMP-TABLE tt-cta-provisao NO-UNDO    
    FIELD cod-conta     as CHAR    
    FIELD cod-empresa   LIKE estabelecimento.cod_empresa
    INDEX conta
        cod-conta
        cod-empresa.

def temp-table tt-provisao no-undo
    LIKE tt-rel.



def new global shared var v_hdl_func_padr_glob
    as Handle
    format ">>>>>>9":U
    label "Fun‡äes Pad Glob"
    column-label "Fun‡äes Pad Glob"
    no-undo.

DEF VAR excelAppl           AS COM-HANDLE   NO-UNDO.
DEF VAR chWorksheet         AS COM-HANDLE   NO-UNDO.
DEF VAR chWorkbook          AS COM-HANDLE   NO-UNDO.
DEF VAR chImp               AS COM-HANDLE NO-UNDO.
def var i-lin               as int          NO-UNDO.
def var i-col               as int          NO-UNDO.
DEF VAR h-bo-dvv-movto-det  AS HANDLE       NO-UNDO.
DEF VAR c-chave-refer             AS CHAR FORMAT "X(100)" NO-UNDO.
DEF VAR i-tp-doc-origem     AS INT          NO-UNDO.
DEF VAR c-arq-imp           AS CHARACTER    NO-UNDO.
DEF VAR c-last-col          AS CHARACTER    NO-UNDO.
DEF VAR c-doc-origem        AS CHARACTER    NO-UNDO.
def var v_val_cotac_indic_econ           as decimal         no-undo. /*local*/
DEF VAR v-val-docto         AS DECIMAL      NO-UNDO.
DEF VAR v-val-desp          AS DECIMAL EXTENT 2 NO-UNDO.
DEF VAR i-seq-classif       AS INTEGER      NO-UNDO.

DEF VAR h-dvv0199-tms AS HANDLE NO-UNDO.

def buffer b-tt-rel-base for tt-rel.
def buffer b-tt-rel-reg  for tt-rel.
DEF BUFFER b_tit_ap FOR tit_ap.
DEF BUFFER b_tit_ap1 FOR tit_ap.
DEF BUFFER b_tit_acr FOR tit_acr.
DEF BUFFER b_tit_acr1 FOR tit_acr.
DEF BUFFER b-tt-rel-total FOR tt-rel-total.
DEF BUFFER b_movto_tit_ap FOR movto_tit_ap.
DEF BUFFER b_dvv_movto_det FOR dvv_movto_det.

/* Fun‡Æo para obter nova coluna. */
FUNCTION fi-c RETURNS CHAR:

    RETURN ENTRY(i-col, "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY,AZ").

END FUNCTION.

FUNCTION fi-nc RETURNS CHAR:

    ASSIGN i-col = i-col + 1.
    RETURN fi-c().

END FUNCTION.

FUNCTION fi-tp-doc-origem RETURNS CHAR (INPUT p-tp-doc-origem AS INT):

    CASE p-tp-doc-origem:
        when 1 THEN RETURN "Medicao".
        when 2 THEN RETURN "Titulo APB".
        when 3 THEN RETURN "Frete".
        when 7 THEN RETURN "Recebimento".
        when 8 THEN RETURN "RDD".
        when 9 THEN RETURN "Comissao".
        WHEN 10 THEN RETURN "GFE".
        WHEN 11 THEN RETURN "Manual FGL".
        WHEN 12 THEN RETURN "Requisicao".
        WHEN 13 THEN RETURN "Estorno DVV".
        WHEN 14 THEN RETURN "Provisao DVV".
        WHEN 15 THEN RETURN "Titulo AP Imposto".
		WHEN 20 THEN RETURN "Tit NC ACR".
		WHEN 21 THEN RETURN "Tit NC APB".
		WHEN 22 THEN RETURN "Tit ND ACR".
		WHEN 23 THEN RETURN "Tit ND APB".		
    END CASE.

    IF p-tp-doc-origem > 0 THEN
        RETURN STRING(p-tp-doc-origem).
    ELSE
        RETURN "".
                                                                 
END FUNCTION.

FUNCTION fi-ret-prev-container RETURNS DECIMAL (INPUT p_estab AS CHAR, INPUT p_num_processo AS CHAR, INPUT p_tipo_despesa AS INT, INPUT p_val_prev AS DEC):

    DEF VAR i AS INTEGER NO-UNDO.

    FOR EACH b_dvv_movto_det NO-LOCK WHERE
             b_dvv_movto_det.cod_estabel  = p_estab
         AND b_dvv_movto_det.num_processo = p_num_processo
         AND b_dvv_movto_det.tipo_despesa = p_tipo_despesa:

        ASSIGN i = i + 1.
    END.

    IF i > 1 THEN
        RETURN round(DEC(p_val_prev / i), 2).
    ELSE
        RETURN DEC(p_val_prev).

END FUNCTION.
