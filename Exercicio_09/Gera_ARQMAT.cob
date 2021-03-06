      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:07/12/2020
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:9 PAG:110
      * Tectonics: cobc
      * Objetivo: ARQUIVO DE APOIO EXERC 9
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER91.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQMAT ASSIGN TO DISK
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  ARQMAT
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "ARQMAT".
       01  REG-ARQMAT.
           02 FD-CODIGO              PIC 9(05)       VALUE ZEROS.
           02 FD-NOME                PIC X(15)       VALUE SPACES.
           02 FD-QUANTIDADE-ESTOQUE  PIC 9(07)       VALUE ZEROS.
           02 FD-CUSTO-UNITARIO      PIC 9(05)V99    VALUE ZEROS.
           02 FD-CUSTO-TOTAL         PIC 9(06)V99    VALUE ZEROS.
       WORKING-STORAGE SECTION.
       77  CONFIRMA PIC X(01) VALUE SPACES.
       77  WS-STATUS PIC X(02) VALUE SPACES.
       SCREEN SECTION.
       01  LIMPA-TELA.
           02 BLANK SCREEN.
       01  TELA-BASE.
           02 LINE 01 COLUMN 02
           VALUE "PROGRAMA QUE GERA E POPULA ARQMAT".
           02 LINE 03 COLUMN 02 VALUE "CODIGO:".
           02 LINE 04 COLUMN 02 VALUE "NOME:".
           02 LINE 05 COLUMN 02 VALUE "QTTDE ESTOQUE:".
           02 LINE 06 COLUMN 02 VALUE "PRECO UNIT:".
           02 LINE 07 COLUMN 02 VALUE "PRECO TOTAL:".
       PROCEDURE DIVISION.
       INICIO.
           OPEN OUTPUT ARQMAT.
       GRAVA-DADOS.
           DISPLAY LIMPA-TELA.
           DISPLAY TELA-BASE.
           ACCEPT FD-CODIGO AT 0317.
           ACCEPT FD-NOME AT   0417.
           ACCEPT FD-QUANTIDADE-ESTOQUE AT 0517.
           ACCEPT FD-CUSTO-UNITARIO AT 0617.
           COMPUTE FD-CUSTO-TOTAL =
           FD-QUANTIDADE-ESTOQUE * FD-CUSTO-UNITARIO.
           DISPLAY FD-CUSTO-TOTAL AT 0717.
           ACCEPT CONFIRMA AT 0817.
           WRITE REG-ARQMAT.
           DISPLAY "PARA DE INSERIR ? S-SIM" AT 0902.
           ACCEPT CONFIRMA AT 0917.
           IF CONFIRMA = "S"
               CLOSE ARQMAT
               DISPLAY "ARQUIVO GERADO E GRAVADO SAINDO" AT 1002
               ACCEPT CONFIRMA AT 1017
               STOP RUN
           ELSE
               GO TO GRAVA-DADOS.
       END PROGRAM EXER91.
