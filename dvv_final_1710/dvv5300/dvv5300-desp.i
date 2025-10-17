/* =======================================================================
   DEX / PDEX / EPDEX  (SEM ALTERAÇÕES)
   ======================================================================= */
IF dvv_log_provisao.origem_movto = "DEX"
OR dvv_log_provisao.origem_movto = "PDEX"
OR dvv_log_provisao.origem_movto = "EPDEX"
THEN DO:
        
    IF INDEX(dvv_log_prov_desp.nr_proc_exp,"C") > 0 THEN
        FIND processo-exp WHERE
             processo-exp.cod-estabel = dvv_log_prov_desp.cod_estabel AND
             processo-exp.nr-proc-exp = dvv_log_prov_desp.nr_proc_exp NO-LOCK NO-ERROR.
    ELSE
        FIND processo-exp WHERE
             processo-exp.cod-estabel = "002" AND
             processo-exp.nr-proc-exp = dvv_log_prov_desp.nr_proc_exp NO-LOCK NO-ERROR.

    FIND emitente WHERE
         emitente.cod-emitente = processo-exp.cod-emitente NO-LOCK NO-ERROR.
               
    ASSIGN tt-dvv_log_prov_desp.cod-cliente  = emitente.cod-emitente
           tt-dvv_log_prov_desp.nome_cliente = emitente.nome-emit.

    FIND FIRST repres NO-LOCK WHERE repres.cod-rep = emitente.cod-rep NO-ERROR.
    FIND FIRST regiao WHERE regiao.nome-ab-reg = repres.nome-ab-reg NO-LOCK NO-ERROR.
    ASSIGN tt-dvv_log_prov_desp.reg-seg = IF AVAIL regiao THEN regiao.nome-regiao ELSE emitente.nome-mic-reg.

    ASSIGN tt-dvv_log_prov_desp.novo = YES.
    IF CAN-FIND (FIRST bdvv_log_prov_desp WHERE
                       bdvv_log_prov_desp.cod_estabel = dvv_log_prov_desp.cod_estabel  AND
                       bdvv_log_prov_desp.nr_proc_exp = dvv_log_prov_desp.nr_proc_exp  AND
                       ROWID(bdvv_log_prov_desp) <> ROWID(dvv_log_prov_desp)) THEN
        ASSIGN tt-dvv_log_prov_desp.novo = NO.
    
    /*retirado para tratar as devolucoes, pois o pedido e desvinculado do processo*/
    /*FIND FIRST proc-ped-ent OF processo-exp NO-LOCK NO-ERROR.*/
    FIND FIRST ped-item WHERE
               ped-item.nome-abrev = emitente.nome-abrev AND
               ped-item.nr-pedcli  = processo-exp.nr-proc-exp NO-LOCK NO-ERROR.

    FIND ITEM WHERE ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
    FIND fam-comerc WHERE fam-comerc.fm-cod-com = ITEM.fm-cod-com NO-LOCK NO-ERROR.
    ASSIGN tt-dvv_log_prov_desp.fm_codigo = IF AVAIL fam-comerc THEN fam-comerc.descricao ELSE ITEM.fm-cod-com.

    FIND FIRST dex_tp_despesa WHERE
               dex_tp_despesa.codigo = dvv_log_prov_desp.cod_despesa NO-LOCK NO-ERROR.
    ASSIGN tt-dvv_log_prov_desp.DESC_despesa = dex_tp_despesa.descricao.

    bloco_estab:
    FOR EACH estabelec NO-LOCK
        WHERE estabelec.cod-estabel <> "999":
        FIND FIRST dex_movto 
            WHERE dex_movto.cod_estabel = estabelec.cod-estabel
              AND dex_movto.Nr_PROCESSO = processo-exp.nr-proc-exp
              AND dex_movto.TP_DeSPESA  = dvv_log_prov_desp.cod_despesa NO-LOCK NO-ERROR.
        IF  AVAIL dex_movto THEN LEAVE bloco_estab.
    END.

    FIND b-emit-fornec WHERE
         b-emit-fornec.cod-emitente = dex_movto.cod_fornecedor NO-LOCK NO-ERROR.
    
    ASSIGN tt-dvv_log_prov_desp.cod_fornec  = IF AVAIL dex_movto THEN dex_movto.cod_fornecedor ELSE 0
           tt-dvv_log_prov_desp.DESC_fornec = IF AVAIL b-emit-fornec THEN b-emit-fornec.nome-emit ELSE "".

    /*IF substring(dex_movto.CHAR_2,1,1) <> "" AND dex_movto.CHAR_2 <> ? THEN*/
    ASSIGN tt-dvv_log_prov_desp.provisiona = IF AVAIL dex_movto AND substring(dex_movto.CHAR_2,1,1) = "1" THEN "Sim" ELSE "NÆo".

    FOR FIRST ext_incoterm NO-LOCK
        WHERE ext_incoterm.incoterm = processo-exp.cod-incoterm
          AND ext_incoterm.log-receita:

        ASSIGN i-pto-contr = processo-exp.pto-chegada.
    END.
    IF NOT AVAIL ext_incoterm THEN
        ASSIGN i-pto-contr = processo-exp.pto-embarque.            

    FIND FIRST historico-proc-exp NO-LOCK 
        WHERE historico-proc-exp.cod-estabel    = processo-exp.cod-estabel
          AND historico-proc-exp.nr-proc-exp    = processo-exp.nr-proc-exp
          AND historico-proc-exp.cod-itiner     = processo-exp.cod-itiner
          AND historico-proc-exp.cod-pto-contr  = i-pto-contr NO-ERROR.

    ASSIGN tt-dvv_log_prov_desp.dt_emissao =
           IF historico-proc-exp.dt-efetiva <> ? THEN historico-proc-exp.dt-efetiva ELSE historico-proc-exp.dt-ult-previsao.
END.

/* =======================================================================
   DVV ME / PDVV ME / EPDVV ME  (SEM ALTERAÇÕES)
   ======================================================================= */
IF dvv_log_provisao.origem_movto = "DVV ME"
OR dvv_log_provisao.origem_movto = "PDVV ME"
OR dvv_log_provisao.origem_movto = "EPDVV ME"
THEN DO:
        
    ASSIGN tt-dvv_log_prov_desp.novo = YES.

    IF CAN-FIND (FIRST bdvv_log_prov_desp WHERE
                       bdvv_log_prov_desp.cod_estabel = dvv_log_prov_desp.cod_estabel  AND
                       bdvv_log_prov_desp.nr_proc_exp = dvv_log_prov_desp.nr_proc_exp  AND
                       ROWID(bdvv_log_prov_desp) <> ROWID(dvv_log_prov_desp)) THEN
        ASSIGN tt-dvv_log_prov_desp.novo = NO.
        
    FIND FIRST processo-exp WHERE 
         processo-exp.cod-estabel = "002" AND
         processo-exp.nr-proc-exp = dvv_log_prov_desp.nr_proc_exp NO-LOCK NO-ERROR.
    IF NOT AVAIL processo-exp THEN
        FIND FIRST processo-exp WHERE 
             processo-exp.nr-proc-exp = dvv_log_prov_desp.nr_proc_exp NO-LOCK NO-ERROR.

    FIND emitente WHERE emitente.cod-emitente = processo-exp.cod-emitente NO-LOCK NO-ERROR.

    ASSIGN tt-dvv_log_prov_desp.cod-cliente  = emitente.cod-emitente
           tt-dvv_log_prov_desp.nome_cliente = emitente.nome-emit.

    FIND FIRST repres NO-LOCK WHERE repres.cod-rep = emitente.cod-rep NO-ERROR.
    FIND FIRST regiao WHERE regiao.nome-ab-reg = repres.nome-ab-reg NO-LOCK NO-ERROR.
    ASSIGN tt-dvv_log_prov_desp.reg-seg = IF AVAIL regiao THEN regiao.nome-regiao ELSE emitente.nome-mic-reg.

    FIND FIRST dvv_tipo_despesa WHERE
               dvv_tipo_despesa.codigo = dvv_log_prov_desp.cod_despesa NO-LOCK NO-ERROR.
    ASSIGN tt-dvv_log_prov_desp.DESC_despesa = dvv_tipo_despesa.descricao.

    /*retirado para tratar as devolucoes, pois o pedido e desvinculado do processo*/
    /*FIND FIRST proc-ped-ent OF processo-exp NO-LOCK NO-ERROR.*/
    FIND FIRST ped-item WHERE
               ped-item.nome-abrev = emitente.nome-abrev AND
               ped-item.nr-pedcli  = processo-exp.nr-proc-exp NO-LOCK NO-ERROR.

    FIND ITEM WHERE ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
    FIND fam-comerc WHERE fam-comerc.fm-cod-com = ITEM.fm-cod-com NO-LOCK NO-ERROR.
    ASSIGN tt-dvv_log_prov_desp.fm_codigo = IF AVAIL fam-comerc THEN fam-comerc.descricao ELSE ITEM.fm-cod-com.

    FIND FIRST dvv_movto
        WHERE dvv_movto.cod_estabel  = processo-exp.cod-estabel
          AND dvv_movto.NUM_PROCESSO = processo-exp.nr-proc-exp
          AND dvv_movto.TIPO_DESPESA = dvv_log_prov_desp.cod_despesa NO-LOCK NO-ERROR.
    IF NOT AVAIL dvv_movto THEN DO:
        FIND FIRST dvv_movto
            WHERE dvv_movto.cod_estabel  = dvv_log_provisao.cod_estabel
              AND dvv_movto.NUM_PROCESSO = processo-exp.nr-proc-exp
              AND dvv_movto.TIPO_DESPESA = dvv_log_prov_desp.cod_despesa NO-LOCK NO-ERROR.
        IF NOT AVAIL dvv_movto THEN DO:
            bloco_estab2:
            FOR EACH estabelec NO-LOCK:
                FIND FIRST dvv_movto
                    WHERE dvv_movto.cod_estabel  = dvv_log_provisao.cod_estabel
                      AND dvv_movto.NUM_PROCESSO = processo-exp.nr-proc-exp
                      AND dvv_movto.TIPO_DESPESA = dvv_log_prov_desp.cod_despesa NO-LOCK NO-ERROR.
                IF AVAIL dvv_movto THEN LEAVE bloco_estab2.
            END.
        END.
    END.
    IF NOT AVAIL dvv_movto THEN NEXT.

    /*IF substring(dvv_movto.char_2,1,1) <> "" AND dvv_movto.char_2 <> ? THEN*/
    ASSIGN tt-dvv_log_prov_desp.provisiona = IF substring(dvv_movto.char_2,1,1) = "1" THEN "Sim" ELSE "NÆo".

    FIND b-emit-fornec WHERE b-emit-fornec.cod-emitente = dvv_movto.cod_fornecedor NO-LOCK NO-ERROR.
    ASSIGN tt-dvv_log_prov_desp.cod_fornec  = dvv_movto.cod_fornecedor
           tt-dvv_log_prov_desp.DESC_fornec = IF AVAIL b-emit-fornec THEN b-emit-fornec.nome-emit ELSE "".

    FOR FIRST ext_incoterm NO-LOCK
        WHERE ext_incoterm.incoterm = processo-exp.cod-incoterm
          AND ext_incoterm.log-receita:

        ASSIGN i-pto-contr = processo-exp.pto-chegada.
    END.
    IF NOT AVAIL ext_incoterm THEN
        ASSIGN i-pto-contr = processo-exp.pto-embarque.            

    FIND FIRST historico-proc-exp NO-LOCK 
        WHERE historico-proc-exp.cod-estabel    = processo-exp.cod-estabel
          AND historico-proc-exp.nr-proc-exp    = processo-exp.nr-proc-exp
          AND historico-proc-exp.cod-itiner     = processo-exp.cod-itiner
          AND historico-proc-exp.cod-pto-contr  = i-pto-contr NO-ERROR.
                       
    ASSIGN tt-dvv_log_prov_desp.dt_emissao =
           IF historico-proc-exp.dt-efetiva <> ? THEN historico-proc-exp.dt-efetiva ELSE historico-proc-exp.dt-ult-previsao.
END.

/* =======================================================================
   DVV MI / PDVV MI / EPDVV MI + DVV-REM / PDVV-REM / EPDVV-REM  (CORRIGIDO)
   ======================================================================= */
IF  dvv_log_provisao.origem_movto = "DVV MI"
 OR dvv_log_provisao.origem_movto = "PDVV MI"
 OR dvv_log_provisao.origem_movto = "EPDVV MI"
THEN DO:

    ASSIGN tt-dvv_log_prov_desp.novo = YES.
    IF CAN-FIND (FIRST bdvv_log_prov_desp WHERE
                       bdvv_log_prov_desp.cod_estabel = dvv_log_prov_desp.cod_estabel  AND
                       bdvv_log_prov_desp.nr_proc_exp = dvv_log_prov_desp.nr_proc_exp AND
                       ROWID(bdvv_log_prov_desp) <> ROWID(dvv_log_prov_desp)) THEN
        ASSIGN tt-dvv_log_prov_desp.novo = NO.

    /* Nota fiscal: tenta série "6" e, se não achar, percorre séries do estabelecimento */
    FIND nota-fiscal WHERE
         nota-fiscal.cod-estabel = dvv_log_prov_desp.cod_estabel
     AND nota-fiscal.serie       = "6"
     AND nota-fiscal.nr-nota-fis = dvv_log_prov_desp.nr_proc_exp NO-LOCK NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN DO:
        blk_series3:
        FOR EACH ser-estab NO-LOCK
            WHERE ser-estab.cod-estabel = dvv_log_prov_desp.cod_estabel:
            FIND FIRST nota-fiscal WHERE
                   nota-fiscal.cod-estabel = dvv_log_prov_desp.cod_estabel
               AND nota-fiscal.serie       = ser-estab.serie
               AND nota-fiscal.nr-nota-fis = dvv_log_prov_desp.nr_proc_exp NO-LOCK NO-ERROR.
            IF AVAIL nota-fiscal THEN LEAVE blk_series3.
        END.
    END.

    /* Cliente / Canal / Região */
    IF AVAIL nota-fiscal THEN DO:
        FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
        FIND canal-venda WHERE canal-venda.cod-canal-venda = emitente.cod-canal-venda NO-LOCK NO-ERROR.

        ASSIGN
            tt-dvv_log_prov_desp.cod-cliente  = IF AVAIL emitente THEN emitente.cod-emitente ELSE 0
            tt-dvv_log_prov_desp.nome_cliente = IF AVAIL emitente THEN emitente.nome-emit ELSE ""
            tt-dvv_log_prov_desp.reg-seg      = IF AVAIL canal-venda THEN canal-venda.descricao
                                                ELSE IF AVAIL emitente THEN emitente.nome-mic-reg
                                                ELSE ""
            tt-dvv_log_prov_desp.dt_emissao   = nota-fiscal.dt-emis-nota.
    END.

    /* Descrição da despesa */
    FIND FIRST dvv_tipo_despesa WHERE
               dvv_tipo_despesa.codigo = dvv_log_prov_desp.cod_despesa NO-LOCK NO-ERROR.
    ASSIGN tt-dvv_log_prov_desp.DESC_despesa = dvv_tipo_despesa.descricao.

    /* Fonte principal: dvv_movto_mi (quando existir) */
    FIND FIRST dvv_movto_mi NO-LOCK WHERE
         dvv_movto_mi.cod_estabel  = (IF AVAIL nota-fiscal THEN nota-fiscal.cod-estabel ELSE dvv_log_prov_desp.cod_estabel)
     AND dvv_movto_mi.serie        = (IF AVAIL nota-fiscal THEN nota-fiscal.serie ELSE "6")
     AND dvv_movto_mi.nr-nota-fis  = (IF AVAIL nota-fiscal THEN nota-fiscal.nr-nota-fis ELSE INTEGER(dvv_log_prov_desp.nr_proc_exp))
     AND dvv_movto_mi.tipo_despesa = dvv_log_prov_desp.cod_despesa NO-ERROR.

    IF AVAIL dvv_movto_mi THEN DO:
        FIND b-emit-fornec WHERE b-emit-fornec.cod-emitente = dvv_movto_mi.cod_fornecedor NO-LOCK NO-ERROR.

        ASSIGN
            tt-dvv_log_prov_desp.cod_fornec  = dvv_movto_mi.cod_fornecedor
            tt-dvv_log_prov_desp.DESC_fornec = IF AVAIL b-emit-fornec THEN b-emit-fornec.nome-emit ELSE ""
            /*IF substring(dvv_movto_mi.char_3,1,1) <> "" AND dvv_movto_mi.char_3 <> ? THEN*/
            tt-dvv_log_prov_desp.provisiona  = IF substring(dvv_movto_mi.char_3,1,1) = "1" THEN "Sim" ELSE "NÆo".
    END.
    ELSE DO:
        /* Fallback amigável para Remessa: usa o emitente da nota como fornecedor quando não houver MI */
        IF AVAIL nota-fiscal THEN DO:
            FIND b-emit-fornec WHERE b-emit-fornec.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
            ASSIGN
                tt-dvv_log_prov_desp.cod_fornec  = nota-fiscal.cod-emitente
                tt-dvv_log_prov_desp.DESC_fornec = IF AVAIL b-emit-fornec THEN b-emit-fornec.nome-emit ELSE ""
                tt-dvv_log_prov_desp.provisiona  = "Sim".
        END.
    END.

    /* Família comercial (fm) a partir do item da nota */
    IF AVAIL nota-fiscal THEN DO:
        FIND FIRST it-nota-fisc OF nota-fiscal NO-LOCK NO-ERROR.
        IF AVAIL it-nota-fisc THEN DO:
            FIND ITEM       WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
            FIND fam-comerc WHERE fam-comerc.fm-cod-com = ITEM.fm-cod-com NO-LOCK NO-ERROR.
            ASSIGN tt-dvv_log_prov_desp.fm_codigo = IF AVAIL fam-comerc THEN fam-comerc.descricao ELSE ITEM.fm-cod-com.
        END.
    END.
END.

IF  dvv_log_provisao.origem_movto = "DVV REM"     /* NOVO: Remessa */
 OR dvv_log_provisao.origem_movto = "PDVV REM"    /* CORREÇÃO: hífen (antes estava PDVV_REM) */
 OR dvv_log_provisao.origem_movto = "EPDVV REM"   /* NOVO: simetria com EPDVV */
THEN DO:

    ASSIGN tt-dvv_log_prov_desp.novo = YES.
    IF CAN-FIND (FIRST bdvv_log_prov_desp WHERE
                       bdvv_log_prov_desp.cod_estabel = dvv_log_prov_desp.cod_estabel  AND
                       bdvv_log_prov_desp.nr_proc_exp = dvv_log_prov_desp.nr_proc_exp AND
                       ROWID(bdvv_log_prov_desp) <> ROWID(dvv_log_prov_desp)) THEN
        ASSIGN tt-dvv_log_prov_desp.novo = NO.

    /* Nota fiscal: tenta série "6" e, se não achar, percorre séries do estabelecimento */
    FIND nota-fiscal WHERE
         nota-fiscal.cod-estabel = dvv_log_prov_desp.cod_estabel
     AND nota-fiscal.serie       = "6"
     AND nota-fiscal.nr-nota-fis = dvv_log_prov_desp.nr_proc_exp NO-LOCK NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN DO:
        blk_series3:
        FOR EACH ser-estab NO-LOCK
            WHERE ser-estab.cod-estabel = dvv_log_prov_desp.cod_estabel:
            FIND FIRST nota-fiscal WHERE
                   nota-fiscal.cod-estabel = dvv_log_prov_desp.cod_estabel
               AND nota-fiscal.serie       = ser-estab.serie
               AND nota-fiscal.nr-nota-fis = dvv_log_prov_desp.nr_proc_exp NO-LOCK NO-ERROR.
            IF AVAIL nota-fiscal THEN LEAVE blk_series3.
        END.
    END.

    /* Cliente / Canal / Região */
    IF AVAIL nota-fiscal THEN DO:
        FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
        FIND canal-venda WHERE canal-venda.cod-canal-venda = emitente.cod-canal-venda NO-LOCK NO-ERROR.

        ASSIGN
            tt-dvv_log_prov_desp.cod-cliente  = IF AVAIL emitente THEN emitente.cod-emitente ELSE 0
            tt-dvv_log_prov_desp.nome_cliente = IF AVAIL emitente THEN emitente.nome-emit ELSE ""
            tt-dvv_log_prov_desp.reg-seg      = IF AVAIL canal-venda THEN canal-venda.descricao
                                                ELSE IF AVAIL emitente THEN emitente.nome-mic-reg
                                                ELSE ""
            tt-dvv_log_prov_desp.dt_emissao   = nota-fiscal.dt-emis-nota.
    END.

    /* Descrição da despesa */
    FIND FIRST dvv_tipo_despesa WHERE
               dvv_tipo_despesa.codigo = dvv_log_prov_desp.cod_despesa NO-LOCK NO-ERROR.
    ASSIGN tt-dvv_log_prov_desp.DESC_despesa = dvv_tipo_despesa.descricao.

    /* Fonte principal: dvv_movto_mi (quando existir) */
    FIND FIRST dvv_movto_mi NO-LOCK WHERE
         dvv_movto_mi.cod_estabel  = (IF AVAIL nota-fiscal THEN nota-fiscal.cod-estabel ELSE dvv_log_prov_desp.cod_estabel)
     AND dvv_movto_mi.serie        = (IF AVAIL nota-fiscal THEN nota-fiscal.serie ELSE "6")
     AND dvv_movto_mi.nr-nota-fis  = (IF AVAIL nota-fiscal THEN nota-fiscal.nr-nota-fis ELSE INTEGER(dvv_log_prov_desp.nr_proc_exp))
     AND dvv_movto_mi.tipo_despesa = dvv_log_prov_desp.cod_despesa NO-ERROR.

    IF AVAIL dvv_movto_mi THEN DO:
        FIND b-emit-fornec WHERE b-emit-fornec.cod-emitente = dvv_movto_mi.cod_fornecedor NO-LOCK NO-ERROR.

        ASSIGN
            tt-dvv_log_prov_desp.cod_fornec  = dvv_movto_mi.cod_fornecedor
            tt-dvv_log_prov_desp.DESC_fornec = IF AVAIL b-emit-fornec THEN b-emit-fornec.nome-emit ELSE ""
            /*IF substring(dvv_movto_mi.char_3,1,1) <> "" AND dvv_movto_mi.char_3 <> ? THEN*/
            tt-dvv_log_prov_desp.provisiona  = IF substring(dvv_movto_mi.char_3,1,1) = "1" THEN "Sim" ELSE "NÆo".
    END.
    ELSE DO:
        /* Fallback amigável para Remessa: usa o emitente da nota como fornecedor quando não houver MI */
        IF AVAIL nota-fiscal THEN DO:
            FIND b-emit-fornec WHERE b-emit-fornec.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
            ASSIGN
                tt-dvv_log_prov_desp.cod_fornec  = nota-fiscal.cod-emitente
                tt-dvv_log_prov_desp.DESC_fornec = IF AVAIL b-emit-fornec THEN b-emit-fornec.nome-emit ELSE ""
                tt-dvv_log_prov_desp.provisiona  = "Sim".
        END.
    END.

    /* Família comercial (fm) a partir do item da nota */
    IF AVAIL nota-fiscal THEN DO:
        FIND FIRST it-nota-fisc OF nota-fiscal NO-LOCK NO-ERROR.
        IF AVAIL it-nota-fisc THEN DO:
            FIND ITEM       WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
            FIND fam-comerc WHERE fam-comerc.fm-cod-com = ITEM.fm-cod-com NO-LOCK NO-ERROR.
            ASSIGN tt-dvv_log_prov_desp.fm_codigo = IF AVAIL fam-comerc THEN fam-comerc.descricao ELSE ITEM.fm-cod-com.
        END.
    END.
END.
