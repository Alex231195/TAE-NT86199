{dvv/dvv0199.i}

DEFINE INPUT  PARAMETER TABLE FOR tt-param.
DEFINE OUTPUT PARAMETER TABLE FOR tt-rel-item.

DEFINE VARIABLE h-acomp AS HANDLE  NO-UNDO.
DEFINE VARIABLE i-row   AS INTEGER NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp(INPUT "Gerando Tabela BI").

FIND FIRST tt-param NO-LOCK NO-ERROR.

EMPTY TEMP-TABLE tt-rel-item.

/* =========================
   FECHAMENTO PRÉVIO = YES
   => Somente PREVISTOS/PRÉVIA (dvv_contabil_previa)
   ========================= */
IF AVAILABLE tt-param AND tt-param.l-fech-previo = YES THEN DO:

  FOR EACH mgnitro.dvv_contabil_previa NO-LOCK
       WHERE mgnitro.dvv_contabil_previa.dta-ctbz    >= tt-param.dt-ctbl-ini
         AND mgnitro.dvv_contabil_previa.dta-ctbz    <= tt-param.dt-ctbl-fim
         AND mgnitro.dvv_contabil_previa.conta       >= tt-param.c-cta-ctbl-ini
         AND mgnitro.dvv_contabil_previa.conta       <= tt-param.c-cta-ctbl-fim
         AND mgnitro.dvv_contabil_previa.cod-estabel >= tt-param.c-estab-ini
         AND mgnitro.dvv_contabil_previa.cod-estabel <= tt-param.c-estab-fim
         /* Garante que só entram PREVISTOS/PRÉVIA: movto começando com "P" */
         AND CAPS(SUBSTRING(mgnitro.dvv_contabil_previa.movto,1,1)) = "P"
         AND (  mgnitro.dvv_contabil_previa.val-lancto <> 0
             OR mgnitro.dvv_contabil_previa.val-doc    <> 0
             OR mgnitro.dvv_contabil_previa.vl-real    <> 0):

    ASSIGN i-row = i-row + 1.
    RUN pi-acompanhar IN h-acomp(STRING(i-row)).

    CREATE tt-rel-item.
    BUFFER-COPY mgnitro.dvv_contabil_previa TO tt-rel-item.
    ASSIGN tt-rel-item.cod-unid-negoc = mgnitro.dvv_contabil_previa.CHAR-2
           tt-rel-item.cod-depos      = mgnitro.dvv_contabil_previa.CHAR-1
           tt-rel-item.grupo-classe   = mgnitro.dvv_contabil_previa.CHAR-3.
  END.

END.

/* =========================
   FECHAMENTO PRÉVIO = NO
   => Somente REALIZADOS (dvv_contabil)
   ========================= */
IF AVAILABLE tt-param AND tt-param.l-fech-previo = NO THEN DO:

  FOR EACH mgnitro.dvv_contabil NO-LOCK
       WHERE mgnitro.dvv_contabil.dta-ctbz    >= tt-param.dt-ctbl-ini
         AND mgnitro.dvv_contabil.dta-ctbz    <= tt-param.dt-ctbl-fim
         AND mgnitro.dvv_contabil.conta       >= tt-param.c-cta-ctbl-ini
         AND mgnitro.dvv_contabil.conta       <= tt-param.c-cta-ctbl-fim
         AND mgnitro.dvv_contabil.cod-estabel >= tt-param.c-estab-ini
         AND mgnitro.dvv_contabil.cod-estabel <= tt-param.c-estab-fim
         /* Exclui qualquer coisa que pareça PREVISTA */
         AND CAPS(SUBSTRING(mgnitro.dvv_contabil.movto,1,1)) <> "P"
         AND (  mgnitro.dvv_contabil.val-lancto <> 0
             OR mgnitro.dvv_contabil.val-doc    <> 0
             OR mgnitro.dvv_contabil.vl-real    <> 0):

    ASSIGN i-row = i-row + 1.
    RUN pi-acompanhar IN h-acomp(STRING(i-row)).

    CREATE tt-rel-item.
    BUFFER-COPY mgnitro.dvv_contabil TO tt-rel-item.
    ASSIGN tt-rel-item.cod-unid-negoc = mgnitro.dvv_contabil.CHAR-2
           tt-rel-item.cod-depos      = mgnitro.dvv_contabil.CHAR-1
           tt-rel-item.grupo-classe   = mgnitro.dvv_contabil.CHAR-3.
  END.

END.

RUN pi-finalizar IN h-acomp.
IF VALID-HANDLE(h-acomp) THEN DELETE PROCEDURE h-acomp.
