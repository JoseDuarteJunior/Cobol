      ******************************************************************
      * Author:JOSE ANTONIO DUARTE JR.80x25
      * Date:01/12/2020
      * Purpose:COBOL PARA MICROCOMPUTADORES EXERCICIO:7 PAG:92
      * Tectonics: cobc
      * Objetivo: MANUTENÇÃO DE ARQUIVO SEQUENCIAL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXER8.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CRIA03 ASSIGN TO DISK
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-STATUS.
           SELECT TEMP ASSIGN TO DISK
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  CRIA03
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CRIA03".
       01  REG-CRIA03.
           02 FD-CODIGO     PIC 9(04)       VALUE ZEROS.
           02 FD-NOME       PIC X(30)       VALUE SPACES.
           02 FD-ENDERECO   PIC X(30)       VALUE SPACES.
           02 FD-BAIRRO     PIC X(20)       VALUE SPACES.
           02 FD-CEP        PIC 9(05)       VALUE ZEROS.
           02 FD-CIDADE     PIC X(20)       VALUE SPACES.
       FD  TEMP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "TEMP03".
       01  REG-TEMP.
           02 FD-CODIGO-TMP     PIC 9(04)       VALUE ZEROS.
           02 FD-NOME-TMP       PIC X(30)       VALUE SPACES.
           02 FD-ENDERECO-TMP   PIC X(30)       VALUE SPACES.
           02 FD-BAIRRO-TMP     PIC X(20)       VALUE SPACES.
           02 FD-CEP-TMP        PIC 9(05)       VALUE ZEROS.
           02 FD-CIDADE-TMP     PIC X(20)       VALUE SPACES.
       WORKING-STORAGE SECTION.
       01  DATA-SISTEMA.
           02 DIA           PIC 9(02) VALUE ZEROS.
           02 MES           PIC 9(02) VALUE ZEROS.
           02 ANO           PIC 9(02) VALUE ZEROS.
       01  WS-STATUS        PIC 9(02) VALUE ZEROS.
       01  CONFIRMA         PIC X(01) VALUE ZEROS.
       01  LIMPA-LINHA      PIC X(50) VALUE SPACES.
       77  CODIGO           PIC 9(04) VALUE ZEROS.
       77  OPERACAO         PIC X(10) VALUE SPACES.
       77  ARQUIVO-EXISTE   PIC 9(01) VALUE ZEROS.
       SCREEN SECTION.
       01  LIMPA-TELA.
           02 BLANK SCREEN.
       01  TELA-BASE.
           02 LINE 01 COLUMN 02
           VALUE "DATA:".
           02 LINE 01 COLUMN 16
           VALUE "MANUTENCAO DO ARQUIVO DE CLIENTES".
           02 LINE 03 COLUMN 06
           VALUE "Autor:.....".
           02 LINE 03 COLUMN 18
           VALUE "Nr:...".
           02 LINE 03 COLUMN 28
           VALUE "Serie:...".
           02 LINE 03 COLUMN 40
           VALUE "Turma:...".
           02 LINE 12 COLUMN 02
           VALUE"Mensagem:".
       01  MENU-1.
           02 LINE 05 COLUMN 13
           VALUE "TELA PRINCIPAL".
           02 LINE 06 COLUMN 13
           VALUE "(1) INCLUSAO".
           02 LINE 07 COLUMN 13
           VALUE"(2) ALTERACAO".
           02 LINE 08 COLUMN 13
           VALUE "(3) EXCLUSAO".
           02 LINE 09 COLUMN 13
           VALUE"(4) CONSULTA".
           02 LINE 10 COLUMN 13
           VALUE "(5) FIM".
       01  MENU-2.
           02 LINE 05 COLUMN 21
           VALUE"(*)__________".
           02 LINE 06 COLUMN 21
           VALUE "(0)CODIGO:".
           02 LINE 07 COLUMN 21
           VALUE "(1)NOME:".
           02 LINE 08 COLUMN 21
           VALUE"(2)ENDERECO:".
           02 LINE 09 COLUMN 21
           VALUE "(3)BAIRRO:".
           02 LINE 10 COLUMN 21
           VALUE "(4)CEP:".
           02 LINE 11 COLUMN 21
           VALUE "(5)CIDADE:".
       01  MSG-ERRO-1.
           02 LINE 12 COLUMN 12
           VALUE "CAMPO INVALIDO !".
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY LIMPA-TELA.
           DISPLAY TELA-BASE.
           ACCEPT DATA-SISTEMA FROM DATE.
           DISPLAY DIA AT 0107'/' MES '/' ANO.
       TELA-MENU-1.
           DISPLAY MENU-1.
           ACCEPT CONFIRMA AT 1212.
           EVALUATE CONFIRMA
           WHEN "1"
              MOVE 'INCLUSAO' TO OPERACAO
              GO TO INCLUSAO
           WHEN "2"
              MOVE 'ALTERACAO' TO OPERACAO
              GO TO ALTERACAO
           WHEN "3"
              MOVE 'EXCLUSAO' TO OPERACAO
              GO TO EXCLUSAO
           WHEN "4"
              MOVE 'CONSULTA' TO OPERACAO
              GO TO BUSCA
           WHEN "5"
              MOVE 'FIM' TO OPERACAO
              GO TO FIM
           WHEN OTHER
               GO TO TELA-MENU-1.
                  INCLUSAO.
           PERFORM INICIO.
           DISPLAY MENU-2.
           DISPLAY OPERACAO AT 0525.
       INCLUSAO-CODIGO.
           DISPLAY LIMPA-LINHA AT 0631.
           DISPLAY LIMPA-LINHA AT 1212.
           ACCEPT FD-CODIGO AT 0634.
           IF FD-CODIGO = 0
               DISPLAY MSG-ERRO-1
               ACCEPT CONFIRMA AT 1239
               GO TO INCLUSAO-CODIGO
           ELSE NEXT SENTENCE
           END-IF.
       INCLUSAO-NOME.
           DISPLAY LIMPA-LINHA AT 0731.
           DISPLAY LIMPA-LINHA AT 1212.
           ACCEPT FD-NOME AT 0734.
           IF FD-NOME = SPACES
               DISPLAY MSG-ERRO-1
               ACCEPT CONFIRMA AT 1239
               GO TO INCLUSAO-NOME
           ELSE NEXT SENTENCE
           END-IF.
       INCLUSAO-ENDERECO.
           DISPLAY LIMPA-LINHA AT 0833.
           DISPLAY LIMPA-LINHA AT 1212.
           ACCEPT FD-ENDERECO AT 0834.
           IF FD-ENDERECO = SPACES
               DISPLAY MSG-ERRO-1
               ACCEPT CONFIRMA AT 1239
               GO TO INCLUSAO-ENDERECO
           ELSE NEXT SENTENCE
           END-IF.
       INCLUSAO-BAIRRO.
           DISPLAY LIMPA-LINHA AT 0931.
           DISPLAY LIMPA-LINHA AT 1212.
           ACCEPT FD-BAIRRO AT 0934.
           IF FD-BAIRRO = SPACES
               DISPLAY MSG-ERRO-1
               ACCEPT CONFIRMA AT 1239
               GO TO INCLUSAO-BAIRRO
           ELSE NEXT SENTENCE
           END-IF.
       INCLUSAO-CEP.
           DISPLAY LIMPA-LINHA AT 1031.
           DISPLAY LIMPA-LINHA AT 1212.
           ACCEPT FD-CEP AT 1034.
           IF FD-CEP = ZEROS
               DISPLAY MSG-ERRO-1
               ACCEPT CONFIRMA AT 1239
               GO TO INCLUSAO-CEP
           ELSE NEXT SENTENCE
           END-IF.
       INCLUSAO-CIDADE.
           DISPLAY LIMPA-LINHA AT 1131.
           DISPLAY LIMPA-LINHA AT 1212.
           ACCEPT FD-CIDADE AT 1134.
           IF FD-CIDADE = SPACES
               DISPLAY MSG-ERRO-1
               ACCEPT CONFIRMA AT 1239
               GO TO INCLUSAO-CIDADE
           ELSE
               IF OPERACAO = 'INCLUSAO'
               GO TO GRAVA-INCLUSAO
           END-IF.
       GRAVA-INCLUSAO.
           OPEN EXTEND CRIA03.
           IF WS-STATUS = "00"
               WRITE REG-CRIA03
           ELSE
               DISPLAY "Erro de abertura" AT 2029
               OPEN OUTPUT CRIA03
               CLOSE CRIA03
               GO TO GRAVA-INCLUSAO
           END-IF.
           CLOSE CRIA03.
           GO TO INICIO.
       ALTERACAO.
           PERFORM INICIO.
           DISPLAY MENU-2.
           DISPLAY OPERACAO AT 0525.
           PERFORM INCLUSAO-CODIGO.
           MOVE FD-CODIGO TO CODIGO.
           OPEN I-O CRIA03.
           GO TO BUSCA-ALTERA.
       BUSCA-ALTERA.
           READ CRIA03 INTO REG-CRIA03 AT END GO TO FIM-BUSCA.
           IF FD-CODIGO = CODIGO
             DISPLAY FD-NOME AT 0734
             DISPLAY FD-ENDERECO AT 0834
             DISPLAY FD-BAIRRO   AT 0934
             DISPLAY FD-CEP      AT 1034
             DISPLAY FD-CIDADE   AT 1134
             PERFORM EDITAR-CAMPO
             END-IF.
           GO TO BUSCA-ALTERA.
       FIM-BUSCA.
           DISPLAY "ESTE ARQUIVO NAO EXISTE" AT 1212.
           ACCEPT CONFIRMA AT 1236.
           CLOSE CRIA03.
           GO TO INICIO.
       EDITAR-CAMPO.
           DISPLAY "DIGITE O NUMERO DO CAMPO QUE VAI ALTERAR:"AT 1212.
           ACCEPT CONFIRMA AT 1255.
           IF CONFIRMA = "1"
                 PERFORM INCLUSAO-NOME
                 REWRITE REG-CRIA03
                 CLOSE CRIA03
                 GO TO INICIO
           END-IF
           IF CONFIRMA = "2"
                 PERFORM INCLUSAO-ENDERECO
                 REWRITE REG-CRIA03
                 CLOSE CRIA03
                 GO TO INICIO
           END-IF
           IF CONFIRMA = "3"
                 PERFORM INCLUSAO-BAIRRO
                 REWRITE REG-CRIA03
                 CLOSE CRIA03
                 GO TO INICIO
           END-IF
           IF CONFIRMA = "4"
                 PERFORM INCLUSAO-CEP
                 REWRITE REG-CRIA03
                 CLOSE CRIA03
                 GO TO INICIO
           END-IF
           IF CONFIRMA = "5"
                 PERFORM INCLUSAO-CIDADE
                 REWRITE REG-CRIA03
                 CLOSE CRIA03
                 GO TO INICIO
           END-IF
           DISPLAY LIMPA-LINHA AT 1212
           DISPLAY MSG-ERRO-1
           ACCEPT CONFIRMA AT 1230
           GO TO EDITAR-CAMPO.
       EXCLUSAO.
           PERFORM INICIO.
           DISPLAY MENU-2.
           DISPLAY OPERACAO AT 0525.
           PERFORM INCLUSAO-CODIGO.
           MOVE FD-CODIGO TO CODIGO.
           OPEN INPUT CRIA03.
           OPEN OUTPUT TEMP.
           GO TO BUSCA-EXCLUI.
       BUSCA-EXCLUI.
           READ CRIA03 INTO REG-CRIA03 AT END GO TO GRAVA-EXCLUSAO.
           IF FD-CODIGO = CODIGO
             DISPLAY FD-NOME AT 0734
             DISPLAY FD-ENDERECO AT 0834
             DISPLAY FD-BAIRRO   AT 0934
             DISPLAY FD-CEP      AT 1034
             DISPLAY FD-CIDADE   AT 1134
             DISPLAY "DESEJA APAGAR MESMO? S-SIM" AT 1212
             ACCEPT CONFIRMA AT 1239
             IF CONFIRMA = 'S'
                GO TO BUSCA-EXCLUI
             ELSE
                CLOSE CRIA03
                CLOSE TEMP
                GO TO INICIO
             END-IF
           ELSE
                   MOVE REG-CRIA03 TO REG-TEMP
                   WRITE REG-TEMP
                   GO TO BUSCA-EXCLUI
           END-IF.
           GO TO BUSCA-EXCLUI.
       GRAVA-EXCLUSAO.
           CLOSE CRIA03
           CLOSE TEMP
           OPEN INPUT TEMP
           OPEN OUTPUT CRIA03
           GO TO TRANSFERENCIA-DADOS.   
       TRANSFERENCIA-DADOS.
           READ TEMP INTO REG-TEMP AT END GO TO CONFIRMA-EXCLUSAO.
           MOVE REG-TEMP TO REG-CRIA03.
           WRITE REG-CRIA03.
           GO TO TRANSFERENCIA-DADOS.
       CONFIRMA-EXCLUSAO.
           DISPLAY "REGISTRO REMOVIDO DO ARQUIVO" AT 1212.
           ACCEPT CONFIRMA AT 1242.
           CLOSE CRIA03.
           CLOSE TEMP.
           GO TO INICIO.
       BUSCA.
           PERFORM INICIO.
           DISPLAY MENU-2.
           DISPLAY OPERACAO AT 0525.
           PERFORM INCLUSAO-CODIGO.
           MOVE FD-CODIGO TO CODIGO.
           OPEN INPUT CRIA03.
           GO TO CONSULTA-REG.
       CONSULTA-REG.
           READ CRIA03 INTO REG-CRIA03 AT END GO TO FIM-CONSULTA.
           IF FD-CODIGO = CODIGO
             DISPLAY FD-NOME AT 0734
             DISPLAY FD-ENDERECO AT 0834
             DISPLAY FD-BAIRRO   AT 0934
             DISPLAY FD-CEP      AT 1034
             DISPLAY FD-CIDADE   AT 1134
             ACCEPT CONFIRMA AT 1212
             CLOSE CRIA03
             GO TO INICIO
           END-IF.
           GO TO CONSULTA-REG.
       FIM-CONSULTA.
           DISPLAY "REGISTRO NAO ENCOTRADO NO ARQUIVO !" AT 1212.
           ACCEPT CONFIRMA AT 1250.
           GO TO INICIO.
       FIM.
           DISPLAY "SAINDO DO PROGRAMA" AT 1212.
           ACCEPT CONFIRMA AT 1232.
           STOP RUN.
       END PROGRAM EXER8.
