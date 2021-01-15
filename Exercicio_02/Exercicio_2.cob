      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.
      * Date:13/11/2020
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:2 PAG:47
      * Tectonics: cobc
      * Objetivo: CALCULAR A MEDIA ARITMETICA DAS NOTAS DE UM ALUNO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  NOTA1        PIC 9(02) VALUE ZEROS.
       77  NOTA2        PIC 9(02) VALUE ZEROS.
       77  NOTA3        PIC 9(02) VALUE ZEROS.
       77  NOTA4        PIC 9(02) VALUE ZEROS.
       77  MEDIA        PIC 9(04) VALUE ZEROS.
       77  MEDIA-FINAL  PIC ZZ,ZZ VALUE ZEROS.
       77  OK           PIC X(01) VALUE SPACES.
       
       SCREEN SECTION.
       01  LIMPA-TELA.
           02 BLANK SCREEN.
       01  TELA-BASE.
           02 LINE 01 COLUMN 20
           VALUE "MEDIA FINAL DE ALUNOS".
           02 LINE 03 COLUMN 20 VALUE "NOME DO ALUNO:".
           02 LINE 05 COLUMN 20 VALUE "1. NOTA:_______".
           02 LINE 06 COLUMN 20 VALUE "2. NOTA:_______".
           02 LINE 07 COLUMN 20 VALUE "3. NOTA:_______".
           02 LINE 08 COLUMN 20 VALUE "4. NOTA:_______".
           02 LINE 10 COLUMN 20 VALUE "MEDIA FINAL __".
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY LIMPA-TELA.
           DISPLAY TELA-BASE.
           ACCEPT NOTA1 AT 0529.
           ACCEPT NOTA2 AT 0629.
           ACCEPT NOTA3 AT 0729.
           ACCEPT NOTA4 AT 0829.
           COMPUTE MEDIA = (NOTA1 + NOTA2 + NOTA3 + NOTA4)/4.
           MOVE MEDIA TO MEDIA-FINAL.
           DISPLAY MEDIA-FINAL AT 1032.
           ACCEPT OK AT 2020.
            STOP RUN.
       END PROGRAM EXER2.

