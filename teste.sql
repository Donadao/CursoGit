 SELECT DISTINCT T.MESTREHANDLE,
     T.MESTRENOME,
     T.EXECUTORHANDLE,
     T.EXECUTORNOME,
     T.TP,
     T.PESOMESTRE,
     T.PESOEXECUTOR,
     T.EXECUTOR,
     T.RECEBEDOR,
     T.notas
      FROM(
    SELECT TMP.PRESTADOR MESTREHANDLE, TMP.NOME MESTRENOME, TMP.PRESTADOR EXECUTORHANDLE, TMP.NOME EXECUTORNOME, 'C' TP,(
        case 
        when X.PESO IS NULL then 0 
        else X.PESO end) PESOMESTRE,(
        case 
        when X.PESO IS NULL then 0 
        else X.PESO end) PESOEXECUTOR, X.EXECUTOR EXECUTOR, X.RECEBEDOR RECEBEDOR, TMP.ENDERECO, TMP.CEP, TMP.MUNICIPIO 
      FROM #TMP_PRESTADOR_CONSULTA TMP 
      JOIN SAM_PRESTADOR X ON(X.HANDLE = TMP.PRESTADOR) 
     WHERE TMP.USUARIO = @p_Usuario 
       and TMP.CHAVE = @p_Chave 
       and 'A' not IN(
    SELECT 'A' 
      FROM SAM_PRESTADOR_REGRA A 
     WHERE A.PRESTADOR = TMP.PRESTADOR 
       and A.EVENTO = @p_Evento 
       and A.REGRAEXCECAO = 'E' 
       and A.DATAINICIAL <= @p_Data 
       and(A.DATAFINAL is NULL 
        or A.DATAFINAL >= @p_Data) 
       and A.PERMITEVISUALIZARCENTRAL = 'S') 
       and('A' not IN(
    SELECT 'A' 
      FROM SAM_PRESTADOR_REGRA A 
     WHERE A.PRESTADOR = TMP.PRESTADOR 
       and A.EVENTO = @p_Evento 
       and A.REGRAEXCECAO = 'R' 
       and A.DATAINICIAL <= @p_Data 
       and A.DATAFINAL < @p_Data 
       and(A.PERMITEEXECUTAR = 'S' 
        or A.PERMITERECEBER = 'S' 
        or A.PERMITEVISUALIZARCENTRAL = 'S')) 
        or 'A' IN(
    SELECT 'A' 
      FROM SAM_PRESTADOR_REGRA A 
     WHERE A.PRESTADOR = TMP.PRESTADOR 
       and A.EVENTO = @p_Evento 
       and A.REGRAEXCECAO = 'R' 
       and A.DATAINICIAL <= @p_Data 
       and(A.DATAFINAL is NULL 
        or A.DATAFINAL >= @p_Data) 
       and A.PERMITEVISUALIZARCENTRAL = 'S')) UNION 
    SELECT TMP.PRESTADOR MESTREHANDLE, TMP.NOME MESTRENOME, P.HANDLE EXECUTORHANDLE, P.NOME EXECUTORNOME, 'M' TP,(
        case 
        when X.PESO IS NULL then 0 
        else X.PESO end) PESOMESTRE,(
        case 
        when P.PESO IS NULL then 0 
        else P.PESO end) PESOEXECUTOR, P.EXECUTOR EXECUTOR, P.RECEBEDOR RECEBEDOR, TMP.ENDERECO, TMP.CEP, TMP.MUNICIPIO 
      FROM #TMP_PRESTADOR_CONSULTA TMP 
      JOIN SAM_PRESTADOR X ON(X.HANDLE = TMP.PRESTADOR) 
      JOIN SAM_PRESTADOR_PRESTADORDAENTID M ON(M.ENTIDADE = X.HANDLE) 
      JOIN SAM_PRESTADOR P ON(P.HANDLE = M.PRESTADOR) 
      JOIN SAM_PRESTADOR_ENDERECO E ON(E.PRESTADOR = X.HANDLE) 

      FROM SAM_PRESTADOR_REGRA A 
     WHERE A.PRESTADOR = P.HANDLE 
       and A.EVENTO = @p_Evento 
       and A.REGRAEXCECAO = 'E' 
       and A.DATAINICIAL <= @p_Data 
       and(A.DATAFINAL is NULL 
        or A.DATAFINAL >= @p_Data) 
       and A.PERMITEVISUALIZARCENTRAL = 'S') 
       and('A' not IN(
    SELECT 'A' 
      FROM SAM_PRESTADOR_REGRA A 
     WHERE A.PRESTADOR = TMP.PRESTADOR 
       and A.EVENTO = @p_Evento 
       and A.REGRAEXCECAO = 'R' 
       and A.DATAINICIAL <= @p_Data 
       and A.DATAFINAL < @p_Data 
       and(A.PERMITEEXECUTAR = 'S' 
        or A.PERMITEVISUALIZARCENTRAL = 'S')) 
        or 'A' IN(
    SELECT 'A' 
      FROM SAM_PRESTADOR_REGRA A 
     WHERE A.PRESTADOR = TMP.PRESTADOR 
       and A.EVENTO = @p_Evento 
       and A.REGRAEXCECAO = 'R' 
       and A.DATAINICIAL <= @p_Data 
 
        or A.DATAFINAL >= @p_Data) 
       and A.PERMITEVISUALIZARCENTRAL = 'S'))) T 
     WHERE(@p_Regiao = 0 
        or('A' IN(
    SELECT 'A' 
      FROM SAM_REGIAOCEP CR 
     WHERE CR.REGIAO = @p_Regiao 
       and(CR.CEPINICIAL <= T.CEP) 
       and(T.CEP <= CR.CEPFINAL)) 
        or 'A' IN(
    SELECT 'A' 
      FROM MUNICIPIOS AX 
     WHERE AX.REGIAO = @p_Regiao 
       and AX.HANDLE = T.MUNICIPIO))) 
       and 'A' not IN(
    SELECT 'A' 
      FROM SAM_PRESTADOR_GRAU A 
     WHERE(A.PRESTADOR = T.EXECUTORHANDLE) 
       and(A.GRAU = @p_Grau) 
       and(A.REGRAEXCECAO = 'E') 
       and(A.DATAINICIAL <= @p_Data) 
       and(A.DATAFINAL is NULL 
        or A.DATAFINAL >= @p_Data)) 
       and 'A' not IN(
    SELECT 'A' 
      FROM SAM_TGE_HABILITACAO A 
     WHERE(A.EVENTO = @p_Evento) 
       and(A.DATAATUALIZACAO <= @p_Data) 
       and 'A' not IN(
    SELECT 'A' 
      FROM SAM_PRESTADOR_HABILITACAO PH 
     WHERE(PH.PRESTADOR = T.EXECUTORHANDLE) 
       and(PH.EVENTO = @p_Evento) 
       and(PH.HABILITACAO = A.HABILITACAO) 
       and(PH.DATA <= @p_Data))) 
       and((@p_Grau = -1) 
        or('A' IN(
    SELECT 'A' 
      FROM SAM_PRESTADOR_GRAU B 
     WHERE(B.PRESTADOR = T.EXECUTORHANDLE) 
       and(B.GRAU = @p_Grau) 
       and(B.REGRAEXCECAO = 'R') 
       and(B.DATAINICIAL <= @p_Data 
       and(@p_Data <= B.DATAFINAL 
        or B.DATAFINAL is NULL)))) 
        or('A' IN(
    SELECT 'A' 
      FROM SAM_TIPOPRESTADOR_GRAU A 
     WHERE A.TIPOPRESTADOR = (
    SELECT TIPOPRESTADOR 
      FROM SAM_PRESTADOR B 
     WHERE B.HANDLE = T.EXECUTORHANDLE) 
       and(A.GRAU = @p_Grau)))) 
    open BuscaInicial
    fetch next from BuscaInicial INTO 
      @v_MestreHandle, 
      @v_MestreNome, 
      @v_ExecutorHandle, 
      @v_ExecutorNome, 
      @v_TP, 
      @v_PesoMestre, 
      @v_PesoExecutor, 
      @v_Executor, 
      @v_Recebedor, 
      @vEnderecoPrestador 
    set @v_DataDeCriacao = getdate() 
    while(@@fetch_status = 0)
    begin
      set @v_Handle_1 = NULL 
      set @v_Handle_2 = NULL 
      set @v_Handle_3 = NULL 
      set @v_Handle_4 = NULL 
      
      SELECT @v_Handle_1 = COUNT(1) 
        FROM SAM_PRESTADOR_REGRA A 
       WHERE(A.PRESTADOR = @v_ExecutorHandle) 
         and(A.EVENTO = @p_Evento) 
         and(A.REGRAEXCECAO = 'R') 
         and(A.PERMITEVISUALIZARCENTRAL = 'S') 
         and(A.PERMITEEXECUTAR = 'S' 
          or @v_SoExecutar = 'N') 
         and((A.PERMITERECEBER = 'S' 
          or @v_SoReceber = 'N') 
          or 'A' IN(
      SELECT 'A' 
        FROM SAM_PRESTADOR_PRESTADORDAENTID P 
       WHERE P.ENTIDADE = @v_ExecutorHandle)) 
         and 1 = (
      SELECT IsNull(SUM((
          case 
          when X.REGIMEATENDIMENTO = @p_RegimeAtendimento then 1 
          else 0 end)), 1) 
        FROM SAM_PRESTADOR_REGRAREGIME X 
       WHERE X.REGRA = A.HANDLE) 
      if(@v_Handle_1 = 0)
      begin
                declare Cursor_Espec cursor local static for        
        SELECT A.HANDLE,
         A.ESPECIALIDADE 
          FROM SAM_PRESTADOR_ESPECIALIDADE A 
         WHERE A.PRESTADOR = @v_ExecutorHandle 
           and A.DATAINICIAL <= @p_Data 
           and(A.DATAFINAL is NULL 
            or A.DATAFINAL >= @p_Data) 
           and A.VISUALIZARCENTRAL = 'S' 
        open Cursor_Espec
        fetch next from Cursor_Espec INTO 
          @v_HandlePrestEsp, 
          @v_Especialidade 
        while(@@fetch_status = 0)
        begin
          
          SELECT @v_Handle_2 = COUNT(B.HANDLE) 
            FROM SAM_PRESTADOR_ESPECIALIDADEGRP B 
            JOIN SAM_ESPECIALIDADEGRUPO_EXEC E ON(E.ESPECIALIDADEGRUPO = B.ESPECIALIDADEGRUPO) 
            LEFT JOIN SAM_PRESTADOR_REGRA R ON(R.EVENTO = E.EVENTO) 
             and(R.PRESTADOR = @v_ExecutorHandle) 
           WHERE(B.PRESTADOR = @v_ExecutorHandle) 
             and(E.EVENTO = @p_Evento) 
             and(B.ESPECIALIDADE = @v_Especialidade) 
             and(B.PRESTADORESPECIALIDADE = @v_HandlePrestEsp) 
             and B.DATAINICIAL <= @p_Data 
             and(B.DATAFINAL is NULL 
              or B.DATAFINAL >= @p_Data) 
             and(R.PERMITEVISUALIZARCENTRAL is NULL 
              or R.PERMITEVISUALIZARCENTRAL = 'N') 
             and(R.HANDLE is NULL 
              or(R.DATAINICIAL <= @p_Data 
             and(R.DATAFINAL is NULL) 
              or(R.DATAFINAL is not NULL 
             and R.DATAFINAL <= @p_Data))) 
             and(B.PERMITEEXECUTAR = 'S' 
              or @v_SoExecutar = 'N') 
             and((B.PERMITERECEBER = 'S' 
              or @v_SoReceber = 'N') 
              or 'A' IN(
          SELECT 'A' 
            FROM SAM_PRESTADOR_PRESTADORDAENTID P 
           WHERE P.ENTIDADE = @v_ExecutorHandle)) 
             and 1 = (
          SELECT IsNull(SUM((
              case 
              when X.REGIMEATENDIMENTO = @p_RegimeAtendimento then 1 
              else 0 end)), 1) 
            FROM SAM_PRESTADOR_ESPECIALIDADEREG X 
           WHERE X.PRESTADORESPECIALIDADEGRP = B.HANDLE) 