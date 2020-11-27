      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:26/11/2020
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:6 PAG:90
      * Tectonics: cobc
      * Objetivo: CRIACAO DE ARQUIVO SEQUENCIAL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER6.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CRIA01 ASSIGN TO DISK
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  CRIA01
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CRIA01".
       01  REG-CRIA01.
           02 FD-CODIGO-PRODUTO      PIC 9(04)    VALUE ZEROS.
           02 FD-NOME-PRODUTO        PIC X(30)    VALUE SPACES.
           02 FD-QUANTIDADE-ESTOQUE  PIC 9(04)    VALUE ZEROS.
           02 FD-CUSTO-UNITARIO      PIC 9(05)V99 VALUE ZEROS.
           02 FD-CUSTO-TOTAL         PIC 9(06)V99 VALUE ZEROS.
       WORKING-STORAGE SECTION.
       01  DATA-SISTEMA.
           02 DIA  PIC 9(02) VALUE ZEROS.
           02 MES  PIC 9(02) VALUE ZEROS.
           02 ANO  PIC 9(02) VALUE ZEROS.
       77  CONFIRMA    PIC X(01) VALUE SPACES.
       77  WS-STATUS   PIC X(02) VALUE SPACES.
       77  WS-LIMPA    PIC X(50) VALUE SPACES.
       77  CONTADOR    PIC 9(03) VALUE 1.
       77  CONTADOR-F  PIC ZZ9   VALUE '1'.
       SCREEN SECTION.
       01  LIMPA-TELA.
           02 BLANK SCREEN.
       01  TELA-BASE.
           02 LINE 01 COLUMN 02
           VALUE "DATA:".
           02 LINE 01 COLUMN 16
           VALUE "CONTROLE DE ALMOXARIFADO".
           02 LINE 03 COLUMN 02
           VALUE "Codigo do Produto__________".
           02 LINE 04 COLUMN 02
           VALUE "Nome do Produto____________".
           02 LINE 05 COLUMN 02
           VALUE "Quantidade em Estoque______".
           02 LINE 06 COLUMN 02
           VALUE "Custo Unitario_____________".
           02 LINE 07 COLUMN 02
           VALUE "Custo Total________________".
           02 LINE 08 COLUMN 02
           VALUE "Mensagem:".
           02 LINE 09 COLUMN 31
           VALUE "Contador:<   >".
       01  MSG-ERRO-1.
           02 LINE 08 COLUMN 12
           VALUE "O CODIGO NAO PODE SER 0 !".
       01  MSG-ERRO-2.
           02 LINE 08 COLUMN 12
           VALUE "NOME NAO PODER FICAR EM BRANCO".
       01  MSG-ERRO-3.
           02 LINE 08 COLUMN 12
           VALUE "ESTOQUE ACIMA OU IGUAL A 10".
       01  MSG-ERRO-4.
           02 LINE 08 COLUMN 12
           VALUE "CUSTO UNITARIO DIFERENTE DE ZERO".

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY LIMPA-TELA.
           OPEN OUTPUT CRIA01.
           IF WS-STATUS = "00"
               NEXT SENTENCE
           ELSE
               DISPLAY "Erro de abertura" AT 2029
               STOP RUN.
           ACCEPT DATA-SISTEMA FROM DATE.
       LIMPA-DADOS.
           DISPLAY WS-LIMPA AT 0320.
           DISPLAY WS-LIMPA AT 0418.
           DISPLAY WS-LIMPA AT 0524.
           DISPLAY WS-LIMPA AT 0617.
           DISPLAY WS-LIMPA AT 0714.
           DISPLAY WS-LIMPA AT 0812.
       VIDEO.
           DISPLAY TELA-BASE.
           DISPLAY DIA AT 0107'/' MES '/' ANO.
           DISPLAY CONTADOR-F AT 0941.
       CONSISTENCIA-A.
           ACCEPT FD-CODIGO-PRODUTO AT 0320.
           IF FD-CODIGO-PRODUTO = 9999
               STOP RUN
           END-IF
           IF FD-CODIGO-PRODUTO = 0
               DISPLAY MSG-ERRO-1
               ACCEPT CONFIRMA AT 2022
               DISPLAY WS-LIMPA AT 0812
               GO TO CONSISTENCIA-A
           ELSE
               NEXT SENTENCE
           END-IF.
       CONSISTENCIA-B.
           ACCEPT FD-NOME-PRODUTO AT 0419.
           IF FD-NOME-PRODUTO = SPACES
               DISPLAY MSG-ERRO-2
               ACCEPT CONFIRMA AT 2022
               DISPLAY WS-LIMPA AT 0812
               GO TO CONSISTENCIA-B
           END-IF.
       CONSISTENCIA-C.
           ACCEPT FD-QUANTIDADE-ESTOQUE AT 0524.
           IF FD-QUANTIDADE-ESTOQUE < 10
               DISPLAY MSG-ERRO-3
               ACCEPT CONFIRMA AT 2022
               DISPLAY WS-LIMPA AT 0812
               GO TO CONSISTENCIA-C
           ELSE
               NEXT SENTENCE
           END-IF.
       CONSISTENCIA-D.
           ACCEPT FD-CUSTO-UNITARIO AT 0617.
           IF FD-CUSTO-UNITARIO NOT = 0
               NEXT SENTENCE
           ELSE
               DISPLAY MSG-ERRO-4
               ACCEPT CONFIRMA AT 2022
               DISPLAY WS-LIMPA AT 0812
               GO TO CONSISTENCIA-D
           END-IF.
       CUSTO-TOTAL.
           COMPUTE FD-CUSTO-TOTAL =
           FD-QUANTIDADE-ESTOQUE * FD-CUSTO-UNITARIO.
           DISPLAY FD-CUSTO-TOTAL AT 0714.
           ACCEPT CONFIRMA AT 2022.
       GRAVAR-REGISTRO.
           WRITE REG-CRIA01.
           IF WS-STATUS = "00"
               NEXT SENTENCE
           ELSE
               DISPLAY "ERRO DE GRAVACAO" WS-STATUS AT 2029
               STOP RUN
           END-IF.
           ADD 1 TO CONTADOR.
           MOVE CONTADOR TO CONTADOR-F.
           DISPLAY CONTADOR-F AT 0842.
           DISPLAY "DESEJA INSERIR MAIS UM REGISTRO ? S-SIM" AT 0812.
           ACCEPT CONFIRMA AT 0853.
           IF CONFIRMA ='S'
               GO TO LIMPA-DADOS
           ELSE 
               GO TO FIM
           END-IF.
       FIM.
           CLOSE CRIA01.
           STOP RUN.
       END PROGRAM EXER6.
