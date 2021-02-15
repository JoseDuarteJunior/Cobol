      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:14/01/2021
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:15 PAG:169
      * Tectonics: cobc
      * Objetivo: LISTAGEM DE PESQUISA COM TABELA
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER15.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADPROD ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS.
           SELECT SORCADPROD ASSIGN TO DISK
           SORT STATUS IS ST.
           SELECT RELATO ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       FD  CADPROD LABEL RECORD STANDARD
           VALUE OF FILE-ID "CADPROD".
       01  REG-CADPROD.
           02 FD-CODIGO-PRODUTO      PIC 9(05)      VALUE ZEROS.
           02 FD-NOME-PRODUTO        PIC X(25)      VALUE ZEROS.
           02 FD-LIMITE-SUPER        PIC 9(05)      VALUE ZEROS.
           02 FD-LIMITE-INFER        PIC 9(05)      VALUE ZEROS.
           02 FD-PRECO-UNIT          PIC 9(05)V99   VALUE ZEROS.
           02 FD-QTDADE-ESTOQUE      PIC 9(05)      VALUE ZEROS.
       SD  SORCADPROD VALUE OF FILE-ID IS "SORCADPROD".
       01  REG-SORCADPROD.
           02 SOR-COD                PIC 9(05).
           02 SOR-NOME               PIC X(25).
           02 SOR-LIMI-S             PIC 9(05).
           02 SOR-LIMI-I             PIC 9(05).
           02 SOR-PRE                PIC 9(05)V99.
           02 SOR-QTDE               PIC 9(05).
       FD  RELATO LABEL RECORD OMITTED LINAGE 110 TOP 2 BOTTOM 5.
       01  REG-RELATO PIC X(132).
       WORKING-STORAGE SECTION.
           77  CONFIRMA          PIC X(01)     VALUE SPACES.
           77  WS-STATUS         PIC 9(02)     VALUE ZEROS.
           77  ST                PIC 9(02)     VALUE ZEROS.
           77  VALOR-ESTOQUE     PIC 9(05)V99  VALUE ZEROS.
           77  ICM               PIC 9(05)V99  VALUE ZEROS.
           77  REG-LIDOS         PIC 9(05)     VALUE ZEROS.
           77  REG-IMPRE         PIC 9(05)     VALUE ZEROS.
       01  DATA-SISTEMA.
           02 ANO            PIC 9(02) VALUE ZEROS.
           02 MES            PIC 9(02) VALUE ZEROS.
           02 DIA            PIC 9(02) VALUE ZEROS.
       01  CAB01.
           02 FILLER         PIC X(02)    VALUE SPACES.
           02 FILLER         PIC X(12)    VALUE "SPaulo, SP, ".
           02 DATA-CAB01.
               03 DIA-CAB01  PIC 99/ VALUE ZEROS.
               03 MES-CAB01  PIC 99/ VALUE ZEROS.
               03 ANO-CAB01  PIC 99  VALUE ZEROS.
           02 FILLER         PIC X(04)     VALUE SPACES.
           02 FILLER         PIC X(26)
           VALUE "VALOR DO ICMS POR PRODUTO".
           02 FILLER         PIC X(84) VALUE SPACES.
       01  CAB02.
           02 FILLER         PIC X(02) VALUE SPACES.
           02 FILLER         PIC X(11) VALUE "COD.PRODUTO".
           02 FILLER         PIC X(06) VALUE SPACES.
           02 FILLER         PIC X(25) VALUE "NOME".
           02 FILLER         PIC X(06) VALUE SPACES.
           02 FILLER         PIC X(10) VALUE "VALOR".
           02 FILLER         PIC X(04) VALUE SPACES.
           02 FILLER         PIC X(09) VALUE "ICM".
           02 FILLER         PIC X(63) VALUE SPACES.
       01  DETALHE.
           02 FILLER         PIC X(02)    VALUE SPACES.
           02 DET-COD-PROD   PIC 9(05)    VALUE ZEROS.
           02 FILLER         PIC X(12)    VALUE SPACES.
           02 DET-NOME       PIC X(25)    VALUE SPACES.
           02 FILLER         PIC X(05)    VALUE SPACES.
           02 DET-VALOR      PIC ZZ.ZZ9,99.
           02 FILLER         PIC X(03)    VALUE SPACES.
           02 DET-ICM        PIC ZZ.ZZ9,99.
           02 FILLER         PIC X(66)    VALUE SPACES.
       01  RODAPE.
           02 FILLER         PIC X(07) VALUE SPACES.
           02 DET-TEXTO      PIC X(35).
           02 FILLER         PIC X(02) VALUE SPACES.
           02 DET-TOTAIS     PIC ZZ9.
       PROCEDURE DIVISION.
       INICIO.
           SORT SORCADPROD ASCENDING KEY SOR-NOME
               INPUT PROCEDURE ENTRADA
               OUTPUT PROCEDURE SAIDA.
           STOP RUN.
       ENTRADA SECTION.
       INI-ENTRADA.
           OPEN INPUT CADPROD.
           IF WS-STATUS NOT = "00"
              IF WS-STATUS NOT = "30"
                  DISPLAY "ARQUIVO CADPROD NAO ENCONTRADO " AT 2020
                  STOP RUN
               ELSE
               DISPLAY "ARQUIVO DANIFICADO" AT 2020
               DISPLAY "CODIGO STATUS = " WS-STATUS AT 2120
               STOP RUN
           ELSE
              NEXT SENTENCE
           END-IF.
       LER-ENTRADA.
           READ CADPROD AT END CLOSE CADPROD
           GO TO FIM-ENTRADA.
           ADD 1 TO REG-LIDOS.
           IF FD-QTDADE-ESTOQUE < FD-LIMITE-INFER
               GO TO LER-ENTRADA
           ELSE
               MOVE REG-CADPROD TO REG-SORCADPROD
           END-IF.
           RELEASE REG-SORCADPROD.
           IF ST = "00" NEXT SENTENCE
               ELSE DISPLAY "ERRO" AT 2020 ST CLOSE CADPROD STOP RUN.
                   GO TO LER-ENTRADA.
       FIM-ENTRADA.
           EXIT.
       SAIDA SECTION.
       INI-SAIDA.
           ACCEPT DATA-SISTEMA FROM DATE.
           MOVE DIA TO DIA-CAB01.
           MOVE MES TO MES-CAB01.
           MOVE ANO TO ANO-CAB01.
           OPEN OUTPUT RELATO.
           PERFORM CABECALHO.
       LER-SAIDA.
           RETURN SORCADPROD AT END GO TO FIM-SAIDA.
           IF ST = "00"
               NEXT SENTENCE
           ELSE
               DISPLAY "ERRO" AT 2020 STOP RUN.
           COMPUTE VALOR-ESTOQUE = SOR-PRE * SOR-QTDE.
           MOVE SOR-COD TO DET-COD-PROD.
           MOVE VALOR-ESTOQUE TO DET-VALOR.
           MOVE SOR-NOME TO DET-NOME.
           COMPUTE ICM = (VALOR-ESTOQUE *15)/100.
           MOVE ICM TO DET-ICM.
           WRITE REG-RELATO FROM DETALHE BEFORE ADVANCING 2 LINES
           AT EOP PERFORM CABECALHO.
           ADD 1 TO REG-IMPRE.
           GO TO LER-SAIDA.
       CABECALHO.
           MOVE SPACES TO REG-RELATO.
           WRITE REG-RELATO FROM CAB01 BEFORE ADVANCING 2 LINES.
           WRITE REG-RELATO FROM CAB02 BEFORE ADVANCING 1 LINES.
       FIM-SAIDA.
           MOVE REG-LIDOS TO DET-TOTAIS.
           MOVE "TOTAL DE REGISTROS LIDOS.....>" TO DET-TEXTO.
           WRITE REG-RELATO FROM RODAPE AFTER ADVANCING 3 LINES.
           MOVE REG-IMPRE TO DET-TOTAIS.
           MOVE "TOTAL DE REGISTROS IMPRESSOS......>" TO DET-TEXTO.
           WRITE REG-RELATO FROM RODAPE BEFORE ADVANCING 1 LINES.
           CLOSE CADPROD RELATO.
       FIM.
           DISPLAY "PROGRAMA EXECUTADO COM SUCESSO" AT 0101.
           ACCEPT CONFIRMA AT 0201.
           STOP RUN.
           END PROGRAM EXER15.
