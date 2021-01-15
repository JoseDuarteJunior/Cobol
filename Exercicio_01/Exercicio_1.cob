      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.
      * Date:13/11/2020
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:1 PAG:46
      * Tectonics: cobc
      * Objetivo: CALCULAR AREA DE UMA CIRCUNFERENCIA
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  VALOR-RAIO   PIC 9(03)   VALUE ZEROS.
       77  RESULTADO    PIC 9(02)V9 VALUE ZEROS.
       77  OK           PIC X(01)   VALUE SPACES.
       77  RESULTADO-F  PIC ZZ,Z     VALUE ZEROS.
       SCREEN SECTION.
       01  LIMPA-TELA.
           02 BLANK SCREEN.
       01  TELA-BASE.
           02 LINE 01 COLUMN 20
           VALUE "CALCULO DA AREA DE UMA CIRCUNFERENCIA".
           02 LINE 04 COLUMN 20
           VALUE "Entre com o valor do raio....>".
           02 LINE 05 COLUMN 20
           VALUE "Resultado do calculo da area.>".
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY LIMPA-TELA.
           DISPLAY TELA-BASE.
           ACCEPT VALOR-RAIO AT 0451.
           COMPUTE RESULTADO = (3,14)*(VALOR-RAIO**2).
           MOVE RESULTADO TO RESULTADO-F.
           DISPLAY RESULTADO-F AT 0551.
           ACCEPT OK AT 2020.
           STOP RUN.
       END PROGRAM EXER1.
