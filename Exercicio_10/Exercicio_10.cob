
      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:08/12/2020
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:7 PAG:112
      * Tectonics: cobc
      * Objetivo: EMISSAO DE RELATORIO EM ARQUIVO SEQUENCIAL 2
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER10.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQCAR ASSIGN TO DISK
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-STATUS.
           SELECT RELATO ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQCAR LABEL RECORD STANDARD
       VALUE OF FILE-ID IS "ARQCAR".
       01  REG-ARQCAR.
           02  FD-CODIGO        PIC 9(03)    VALUE ZEROS.
           02  FD-MARCA         PIC X(15)    VALUE SPACES.
           02  FD-COR           PIC X(10)    VALUE SPACES.
           02  FD-ANO           PIC 9(04)    VALUE ZEROS.
           02  FD-CHAPA         PIC X(06)    VALUE SPACES.
           02  FD-MODELO        PIC X(10)    VALUE SPACES.
       FD RELATO LABEL RECORD OMITTED
       LINAGE 52 TOP 6 BOTTOM 8.
       01  REG-RELATO PIC X(132).
       WORKING-STORAGE SECTION.
       77  WS-STATUS              PIC 9(02)     VALUE ZEROS.
       77  CONFIRMA               PIC X(01)     VALUE SPACES.
       77  CT-PAG                 PIC 9(02)     VALUE ZEROS.
       77  QT-82                  PIC 9(04)     VALUE ZEROS.
       77  QT-83                  PIC 9(04)     VALUE ZEROS.
       77  QT-84                  PIC 9(04)     VALUE ZEROS.
       77  QT-85                  PIC 9(04)     VALUE ZEROS.
       77  QT-86                  PIC 9(04)     VALUE ZEROS.
       77  QT-87                  PIC 9(04)     VALUE ZEROS.
       77  QT-88                  PIC 9(04)     VALUE ZEROS.
       01  DATA-SISTEMA.
           02 ANO           PIC 9(02) VALUE ZEROS.
           02 MES           PIC 9(02) VALUE ZEROS.
           02 DIA           PIC 9(02) VALUE ZEROS.
       01  CAB01.
           02 FILLER    PIC X(04)    VALUE SPACES.
           02 FILLER    PIC X(06)    VALUE "DATA:".
           02 DATA-CAB01.
               03 DIA-CAB01   PIC 99/ VALUE ZEROS.
               03 MES-CAB01   PIC 99/ VALUE ZEROS.
               03 ANO-CAB01   PIC 99  VALUE ZEROS.
           02 FILLER    PIC X(07)     VALUE SPACES.
           02 FILLER    PIC X(33)
           VALUE "BRASILCAR REVENDEDORA DE VEICULOS".
           02 FILLER     PIC X(05)   VALUE SPACES.
           02 FILLER     PIC X(04)   VALUE "PAG.".
           02 PAG-CAB01  PIC ZZ9     VALUE ZEROS.
           02 FILLER     PIC X(05)   VALUE SPACES.
       01  CAB02.
           02 FILLER  PIC X(04) VALUE SPACES.
           02 FILLER  PIC X(08) VALUE "CODIGO".
           02 FILLER  PIC X(15) VALUE "MARCA".
           02 FILLER  PIC X(14) VALUE "COR".
           02 FILLER  PIC X(17) VALUE "ANO".
           02 FILLER  PIC X(11) VALUE "MODELO".
       01  DETALHE.
           02 FILLER            PIC X(04)    VALUE SPACES.
           02 CODIGO-DET        PIC 9(03)    VALUE ZEROS.
           02 FILLER            PIC X(05)    VALUE SPACES.
           02 MARCA-DET         PIC X(15)    VALUE SPACES.
           02 COR-DET           PIC 9(10)    VALUE ZEROS.
           02 FILLER            PIC X(04)    VALUE SPACES.
           02 ANO-DET           PIC 9(04)    VALUE ZEROS.
           02 FILLER            PIC X(13)    VALUE SPACES.
           02 MODELO-DET        PIC X(10)    VALUE SPACES.
       01  CAB03.
           02 FILLER  PIC X(14) VALUE SPACES.
           02 FILLER  PIC X(28) VALUE "RESUMO DE EXISTENCIA POR ANO".
       01  CAB04.
           02 FILLER  PIC X(04) VALUE SPACES.
           02 FILLER  PIC X(06) VALUE "-1982-".
           02 FILLER  PIC X(04) VALUE SPACES.
           02 FILLER  PIC X(06) VALUE "-1983-".
           02 FILLER  PIC X(04) VALUE SPACES.
           02 FILLER  PIC X(06) VALUE "-1984-".
           02 FILLER  PIC X(04) VALUE SPACES.
           02 FILLER  PIC X(06) VALUE "-1985-".
           02 FILLER  PIC X(04) VALUE SPACES.
           02 FILLER  PIC X(06) VALUE "-1986-".
           02 FILLER  PIC X(04) VALUE SPACES.
           02 FILLER  PIC X(06) VALUE "-1987-".
           02 FILLER  PIC X(04) VALUE SPACES.
           02 FILLER  PIC X(06) VALUE "-1988-".
           02 FILLER  PIC X(04) VALUE SPACES.
       01  ULTIMA-PAGINA.
           02 FILLER                 PIC X(04)        VALUE SPACES.
           02 QTD-82                 PIC Z.ZZ9        VALUE ZEROS.
           02 FILLER                 PIC X(05)        VALUE SPACES.
           02 QTD-83                 PIC Z.ZZ9        VALUE ZEROS.
           02 FILLER                 PIC X(05)        VALUE SPACES.
           02 QTD-84                 PIC Z.ZZ9        VALUE ZEROS.
           02 FILLER                 PIC X(05)        VALUE SPACES.
           02 QTD-85                 PIC Z.ZZ9        VALUE ZEROS.
           02 FILLER                 PIC X(05)        VALUE SPACES.
           02 QTD-86                 PIC Z.ZZ9        VALUE ZEROS.
           02 FILLER                 PIC X(05)        VALUE SPACES.
           02 QTD-87                 PIC Z.ZZ9        VALUE ZEROS.
           02 FILLER                 PIC X(05)        VALUE SPACES.
           02 QTD-88                 PIC Z.ZZ9        VALUE ZEROS.
           02 FILLER                 PIC X(06)        VALUE SPACES.
       SCREEN SECTION.
       01  TELA.
           02 BLANK SCREEN.
           02 LINE 10 COLUMN 20 VALUE "I M P R I M I N D O . . ."
           REVERSE-VIDEO BLINK.
       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT ARQCAR.
           IF WS-STATUS NOT = "00"
               IF WS-STATUS NOT = "30"
                   DISPLAY "ARQUIVO NAO SE ENCONTRA NO DISCO" AT 2020
                   STOP RUN
                ELSE
                DISPLAY "ARQUIVO DANIFICADO" AT 2020
                DISPLAY "CODIGO STATUS = " WS-STATUS AT 2120
                STOP RUN
           ELSE
               NEXT SENTENCE
           END-IF.
           OPEN OUTPUT RELATO.
           ACCEPT DATA-SISTEMA FROM DATE.
           ACCEPT CONFIRMA AT 3130.
           MOVE DIA TO DIA-CAB01.
           MOVE MES TO MES-CAB01.
           MOVE ANO TO ANO-CAB01.
           DISPLAY TELA.
       CABECALHO.
           MOVE SPACES TO REG-RELATO.
           ADD 1 TO CT-PAG.
           MOVE CT-PAG TO PAG-CAB01.
           WRITE REG-RELATO FROM CAB01 BEFORE ADVANCING 3 LINES.
           WRITE REG-RELATO FROM CAB02 BEFORE ADVANCING 2 LINES.
       LER.
           READ ARQCAR AT END GO TO FIM.
           MOVE FD-CODIGO TO CODIGO-DET.
           MOVE FD-MARCA TO MARCA-DET.
           MOVE FD-COR TO COR-DET.
           MOVE FD-ANO TO ANO-DET.
           MOVE FD-MODELO TO MODELO-DET.
           IF FD-ANO <= 1982
               ADD 1 TO QT-82
           ELSE
           IF FD-ANO = 1983
               ADD 1 TO QT-83 
           ELSE
           IF FD-ANO = 1984
               ADD 1 TO QT-84 
           ELSE
           IF FD-ANO = 1985
               ADD 1 TO QT-85 
           ELSE
           IF FD-ANO = 1986
               ADD 1 TO QT-86 
           ELSE
           IF FD-ANO = 1987
               ADD 1 TO QT-87 
           ELSE
           IF FD-ANO >= 1988
               ADD 1 TO QT-88
           END-IF.
           WRITE REG-RELATO FROM DETALHE BEFORE ADVANCING 2 LINES
           AT EOP PERFORM CABECALHO.
           GO TO LER.
       CABECALHO-FINAL.
           ADD 1 TO CT-PAG.
           MOVE CT-PAG TO PAG-CAB01.
           MOVE QT-82 TO QTD-82.
           MOVE QT-83 TO QTD-83.
           MOVE QT-84 TO QTD-84.
           MOVE QT-85 TO QTD-85.
           MOVE QT-86 TO QTD-86.
           MOVE QT-87 TO QTD-87.
           MOVE QT-88 TO QTD-88.
           
           WRITE REG-RELATO FROM CAB01 BEFORE ADVANCING 3 LINES.
           WRITE REG-RELATO FROM CAB03 BEFORE ADVANCING 2 LINES.
           WRITE REG-RELATO FROM CAB04 BEFORE ADVANCING 2 LINES.
           WRITE REG-RELATO FROM ULTIMA-PAGINA.
       FIM.
           MOVE SPACES TO REG-RELATO.
           WRITE REG-RELATO BEFORE PAGE.
           PERFORM CABECALHO-FINAL.
           CLOSE RELATO ARQCAR.
           STOP RUN.
           END PROGRAM EXER10.
