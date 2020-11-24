      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.
      * Date:24/11/2020
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:1 PAG:72
      * Tectonics: cobc
      * Objetivo: CONSISTENCIA DE UMA DATA
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER4.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  CONFIRMA    PIC X(01) VALUE SPACES.
       77  RESULTADO   PIC 9(03) VALUE ZEROS.
       77  BISSEXTO    PIC 9(02) VALUE ZEROS.
       01  DATA-SISTEMA.
           02   ANO    PIC 9(02) VALUE ZEROS.
           02   MES    PIC 9(02) VALUE ZEROS.
           02   DIA    PIC 9(02) VALUE ZEROS.
       01  DATA-CONSISTIR.
           02   DIA-C  PIC 9(02) VALUE ZEROS.
           02   MES-C  PIC 9(02) VALUE ZEROS.
           02   ANO-C  PIC 9(02) VALUE ZEROS.
       SCREEN SECTION.
       01  LIMPA-TELA.
           02 BLANK SCREEN.
       01  TELA-BASE.
           02 LINE 01 COLUMN 02
           VALUE "DATA:".
           02 LINE 02 COLUMN 09
           VALUE "COLEGIO PASSA-QUEM-QUER".
           02 LINE 04 COLUMN 06
           VALUE "PROGRAMA DE CONSISTENCIA DE DATA".
           02 LINE 06 COLUMN 09
           VALUE "DIGITE A DATA A SER CONSISTIDA".
           02 LINE 08 COLUMN 18
           VALUE "(        )                    ".
           02 LINE 10 COLUMN 02
           VALUE "Mensagem:".
       01  MSG-ERRO-DIA.
           02 LINE 10 COLUMN 12 VALUE "O DIA DEVE SER ENTRE 1 E 31".
       01  MSG-ERRO-DIA-2.
           02 LINE 10 COLUMN 12 VALUE "EM FEVEREIRO MAXIMO ATE DIA 29".
       01  MSG-ERRO-MES.
           02 LINE 10 COLUMN 12 VALUE "O MES DEVE SER ENTRE 1 E 12".
       01  MSG-ERRO-MES-2.
           02 LINE 10 COLUMN 12 VALUE "MES DEVE SER 1,3,5,7,8,10 OU 12".
       01  MSG-LIMPA.
           02 LINE 10 COLUMN 12 VALUE "                               ".
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY LIMPA-TELA.
           DISPLAY TELA-BASE.
           ACCEPT DATA-SISTEMA FROM DATE.
           DISPLAY DIA AT 0109 '/' MES '/' ANO.
           ACCEPT DATA-CONSISTIR AT 0819.
           DISPLAY DIA-C AT 0819 '/' MES-C '/' ANO-C.
       CONSISTENCIA-A.
           IF DIA-C <=0 OR >31
               DISPLAY MSG-ERRO-DIA
               ACCEPT CONFIRMA AT 1050
               GO TO INICIO.
       CONSISTENCIA-B.
           IF MES-C <=0 OR > 12
               DISPLAY MSG-ERRO-MES
               ACCEPT CONFIRMA AT 1050
               GO TO INICIO.
       CONSISTENCIA-C.
           IF DIA-C = 31
               IF MES-C = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
                   GO TO NEXT SENTENCE
               ELSE
                   DISPLAY MSG-ERRO-MES-2
                   ACCEPT CONFIRMA AT 1050
                   GO TO INICIO
               END-IF
           ELSE
               GO TO NEXT SENTENCE
           END-IF.
       CONSISTENCIA-D.
           IF MES-C = 2
               IF DIA-C > 29
                   DISPLAY MSG-ERRO-DIA-2
                   ACCEPT CONFIRMA AT 1050
                   GO TO INICIO
               END-IF
               IF DIA-C = 29
                   GO TO VERIFICA-BISSEXTO
               END-IF
           GO TO FINALIZA
           END-IF.
       VERIFICA-BISSEXTO.
           DISPLAY "VERIFICA BISSEXT0" AT 1020.
           DIVIDE ANO-C BY 4 GIVING RESULTADO REMAINDER BISSEXTO.
           IF BISSEXTO = 0
            DISPLAY "ESTE ANO E' BISSEXTO" AT 0829.
       FINALIZA.
           DISPLAY "DATA CONSISTENTE, DESEJA TESTAR OUTRA DATA ? S-SIM"
           AT 1012.
           ACCEPT CONFIRMA AT 1063.
           IF CONFIRMA = 'S'
               GO TO INICIO.
           STOP RUN.
       END PROGRAM EXER4.
