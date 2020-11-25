      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.
      * Date:25/11/2020
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:1 PAG:78
      * Tectonics: cobc
      * Objetivo: CONSISTENCIA DE UMA DATA  E CALCULO DE DIFERENCA
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER4.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  CONFIRMA    PIC X(01)      VALUE SPACES.
       77  LACOS       PIC 9(01)      VALUE ZEROS.
       77  RESULTADO   PIC 9(03)      VALUE ZEROS.
       77  BISSEXTO    PIC 9(02)      VALUE ZEROS.
       77  DIAS-DATA-1 PIC 9(20)      VALUE ZEROS.
       77  DIAS-DATA-2 PIC 9(07)      VALUE ZEROS.
       77  MEDIA-MES   PIC 9(04)V99   VALUE 30,41.
       77  DIFERENCA   PIC 9(06)      VALUE ZEROS.
       01  DATA-SISTEMA.
           02   ANO    PIC 9(02) VALUE ZEROS.
           02   MES    PIC 9(02) VALUE ZEROS.
           02   DIA    PIC 9(02) VALUE ZEROS.
       01  DATA-CONSISTIR.
           02   DIA-C  PIC 9(02) VALUE ZEROS.
           02   MES-C  PIC 9(02) VALUE ZEROS.
           02   ANO-C  PIC 9(04) VALUE ZEROS.
       01  DATA-A1.
           02   DIA-1  PIC 9(02) VALUE ZEROS.
           02   MES-1  PIC 9(02) VALUE ZEROS.
           02   ANO-1  PIC 9(04) VALUE ZEROS.
       SCREEN SECTION.
       01  LIMPA-TELA.
           02 BLANK SCREEN.
       01  TELA-BASE.
           02 LINE 01 COLUMN 02
           VALUE "DATA:".
           02 LINE 02 COLUMN 09
           VALUE "COLEGIO PASSA-QUEM-QUER".
           02 LINE 04 COLUMN 06
           VALUE "PROGRAMA DE CALCULO DE DIFRENCA".
           02 LINE 05 COLUMN 13
           VALUE "ENTRE DUAS DATAS".
           02 LINE 07 COLUMN 09
           VALUE "DIGITE A 1.DATA:".
           02 LINE 07 COLUMN 27 
           VALUE "  /  /  ".
           02 LINE 09 COLUMN 09
           VALUE "DIGITE A 2.DATA:".
           02 LINE 09 COLUMN 27 
           VALUE "  /  /  ".
           02 LINE 11 COLUMN 02
           VALUE "Mensagem:".
           
       01  MSG-ERRO-DIA.
           02 LINE 11 COLUMN 12 VALUE "O DIA DEVE SER ENTRE 1 E 31".
       01  MSG-ERRO-DIA-2.
           02 LINE 11 COLUMN 12 VALUE "EM FEVEREIRO MAXIMO ATE DIA 29".
       01  MSG-ERRO-MES.
           02 LINE 11 COLUMN 12 VALUE "O MES DEVE SER ENTRE 1 E 12".
       01  MSG-ERRO-MES-2.
           02 LINE 11 COLUMN 12 VALUE "MES DEVE SER 1,3,5,7,8,10 OU 12".
       01  MSG-LIMPA.
           02 LINE 11 COLUMN 12 
           VALUE "                                                    ".
           02 LINE 11 COLUMN 62 VALUE "                               ".
       PROCEDURE DIVISION.
       CONTROLE-LACOS.
           MOVE 1 TO LACOS.
       INICIO.
           DISPLAY LIMPA-TELA.
           DISPLAY TELA-BASE.
           ACCEPT DATA-SISTEMA FROM DATE.
           DISPLAY DIA AT 0109 '/' MES '/' ANO.
           IF LACOS =1
               ACCEPT DATA-CONSISTIR AT 0727
               DISPLAY DIA-C AT 0727 '/' MES-C '/' ANO-C
           ELSE
               DISPLAY DIA-1 AT 0727 '/' MES-1 '/' ANO-1
               ACCEPT DATA-CONSISTIR AT 0927
               DISPLAY DIA-C AT 0927 '/' MES-C '/' ANO-C
           END-IF.
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
               ELSE
               IF DIA-C = 29
                   GO TO VERIFICA-BISSEXTO
               END-IF
           ELSE
               GO TO FINALIZA
           END-IF.
       VERIFICA-BISSEXTO.
           DIVIDE ANO-C BY 4 GIVING RESULTADO REMAINDER BISSEXTO.
           IF BISSEXTO = 0
            DISPLAY "ESTE ANO E' BISSEXTO" AT 0738.
       FINALIZA.
           IF LACOS = 1
               DISPLAY "DATA CONSISTENTE, DIGITE A PROXIMA DATA"
               AT 1112
               MOVE DATA-CONSISTIR TO DATA-A1
               ACCEPT CONFIRMA AT 1163
               MOVE 2 TO LACOS
               GO TO INICIO
           ELSE 
               DISPLAY "DATA CONSISTENTE, ENTER PARA CALCULAR DIFERENCA"
               AT 1112
               ACCEPT CONFIRMA AT 1163
               GO TO CALCULO-DE-DATAS
           END-IF.
       CALCULO-DE-DATAS.
           COMPUTE DIAS-DATA-1 = ANO-1 * 365.
           COMPUTE DIAS-DATA-2 = ANO-C * 365.
           COMPUTE DIAS-DATA-1 ROUNDED = 
           DIAS-DATA-1 + ((MES-1 - 1)*MEDIA-MES).
           COMPUTE DIAS-DATA-2 ROUNDED = 
           DIAS-DATA-2 + ((MES-C - 1) *MEDIA-MES).
           COMPUTE DIAS-DATA-1 = DIAS-DATA-1 + DIA-1.
           COMPUTE DIAS-DATA-2 = DIAS-DATA-2 + DIA-C.
           COMPUTE DIFERENCA = DIAS-DATA-1 - DIAS-DATA-2.
           DISPLAY "DIFERENCA EM DIAS DAS DATAS:                       " 
           AT 1112.
           DISPLAY DIFERENCA AT 1141.
           ACCEPT CONFIRMA AT 1149.
           DISPLAY MSG-LIMPA AT 1112.
           DISPLAY "TESTAR OUTRAS DUAS DATAS: S-SIM                    " 
           AT 1112.
           ACCEPT CONFIRMA AT 1163.
           IF CONFIRMA = 'S'
               MOVE 1 TO LACOS
               GO TO INICIO.
           STOP RUN.
       END PROGRAM EXER4.
