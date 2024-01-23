      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB956.
       AUTHOR.                     ULISSES & MORAES - TI.
       DATE-WRITTEN.               09/03/2014.
       SECURITY.
      *
      *****************************************************************
      * SISTEMA......: SIGPF - SISTEMA DE GESTAO DE PECAS/FILIAIS     *
      *****************************************************************
      * ANALISTA.....: ULISSES & MORAES - TI                          *
      * LINGUAGEM....: COBOL/BATCH/DB2                                *
      * PROGRAMADOR..: ULISSES & MORAES - TI                          *
      * DATA.........: 09/09/2014                                     *
      *****************************************************************
      * OBJETIVO.....: EXPURGO ENTIDADES VENCIDAS                     *
      *****************************************************************
      *
      *****************************************************************
       ENVIRONMENT                 DIVISION.
      *****************************************************************
       CONFIGURATION               SECTION.
      *****************************************************************
       SPECIAL-NAMES.              DECIMAL-POINT   IS    COMMA.
      *****************************************************************
       INPUT-OUTPUT                SECTION.
      *****************************************************************
       FILE-CONTROL.
      *****************************************************************
       DATA                        DIVISION.
      *****************************************************************
       FILE                        SECTION.
      *****************************************************************
       WORKING-STORAGE             SECTION.
      *****************************************************************
      *
       01      WS-LID-TBEAPRA     PIC     9(018) VALUE ZEROS.
      *
       01      WS-EDICAO           PIC     Z.ZZZ.ZZ9.
      *
      *****************************************************************
      *        VARIAVEIS PARA TRATAMENTO DE ABEND                     *
      *****************************************************************
      *
       01      WS-ACESSO-ARQ       PIC     X(013) VALUE SPACES.
       01      WS-DDNAME-ARQ       PIC     X(008) VALUE SPACES.
       01      WS-FS-ARQ           PIC     9(002) VALUE ZEROS.
      *
       01      WS-ACESSO-DB2       PIC     X(006) VALUE SPACES.
       01      WS-TABELA-DB2       PIC     X(008) VALUE SPACES.
       01      WS-SQLCOD-DB2       PIC     +++9.
      *
       01      WS-PTO-ERRO         PIC     9(003) VALUE ZEROS.
      *
      *****************************************************************
      *        AREA DE TRATAMENTO DB2                                 *
      *****************************************************************
      *
           EXEC    SQL
                   INCLUDE SQLCA
           END-EXEC.
      /
           EXEC    SQL
                   INCLUDE TBEAPRA
           END-EXEC.
      *
      *****************************************************************
      *            AREA DE TRATAMENTO DE CURSORES DB2                 *
      *****************************************************************
      *
           EXEC    SQL      DECLARE     C-TBEAPRA  CURSOR  FOR

                   SELECT   DISTINCT(TBEAPRA_NMPRA)

                   FROM     BVISANET.TBEAPRA

                   ORDER BY TBEAPRA_NMPRA

                   FOR      FETCH       ONLY

           END-EXEC.
      *
      *****************************************************************
       LINKAGE                     SECTION.
      *****************************************************************
       PROCEDURE                   DIVISION.
      *****************************************************************
      *
           PERFORM 0100-00-PROCED-INICIAIS.

           PERFORM 1000-00-PROCED-PRINCIPAIS.

           PERFORM 3000-00-PROCED-FINAIS.

           GOBACK.
      *
      *****************************************************************
       0100-00-PROCED-INICIAIS     SECTION.
      *****************************************************************
      *
           DISPLAY '******************* GPFPB956 ******************'.
           DISPLAY '*  NOME DOS PRAZOS EXISTENTES E DISPONIVEIS   *'.
           DISPLAY '******************* GPFPB956 ******************'.
      *
       0100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1000-00-PROCED-PRINCIPAIS   SECTION.
      *****************************************************************
      *
           PERFORM 1100-00-OPEN-C-TBEAPRA.

           PERFORM 1200-00-FETCH-C-TBEAPRA.

           IF      SQLCODE         EQUAL   +100
                   DISPLAY
                   '******************* GPFPB956 ******************'
                   DISPLAY
                   '*                                             *'
                   DISPLAY
                   '*     NAO EXISTE DADOS NA TABELA TBEAPRA      *'

                   DISPLAY
                   '*                                             *'
           END-IF.

           PERFORM 1400-00-TRATA-C-TBEAPRA
             UNTIL SQLCODE EQUAL +100.

           PERFORM 1300-00-CLOSE-C-TBEAPRA.
      *
       1000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1100-00-OPEN-C-TBEAPRA     SECTION.
      *****************************************************************
      *
           EXEC    SQL
                   OPEN    C-TBEAPRA
           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000
                   MOVE ' OPEN '   TO      WS-ACESSO-DB2
                   MOVE 'TBEAPRA ' TO      WS-TABELA-DB2
                   MOVE  002       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.
      *
       1100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1200-00-FETCH-C-TBEAPRA    SECTION.
      *****************************************************************
      *
           EXEC    SQL

                   FETCH   C-TBEAPRA

                   INTO   :TBEAPRA-NMPRA

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000 AND +100
                   MOVE 'FETCH '   TO      WS-ACESSO-DB2
                   MOVE 'TBEAPRA ' TO      WS-TABELA-DB2
                   MOVE  003       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.

           IF      SQLCODE         EQUAL   +000
                   ADD 001         TO      WS-LID-TBEAPRA
           END-IF.
      *
       1200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1300-00-CLOSE-C-TBEAPRA    SECTION.
      *****************************************************************
      *
           EXEC    SQL
                   CLOSE   C-TBEAPRA
           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000
                   MOVE 'CLOSE '   TO      WS-ACESSO-DB2
                   MOVE 'TBEAPRA ' TO      WS-TABELA-DB2
                   MOVE  004       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.
      *
       1300-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1400-00-TRATA-C-TBEAPRA    SECTION.
      *****************************************************************
      *
           DISPLAY '* ' TBEAPRA-NMPRA '                       *'.

           PERFORM 1200-00-FETCH-C-TBEAPRA.
      *
       1400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
           DISPLAY '******************* GPFPB956 ******************'.
           DISPLAY '                                               '.

           PERFORM 3100-00-MONTA-ESTATISTICA.
      *
       3000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3100-00-MONTA-ESTATISTICA   SECTION.
      *****************************************************************
      *
           DISPLAY '******************* GPFPB956 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*        ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB956 ******************'.
           DISPLAY '*                                             *'.
           MOVE    WS-LID-TBEAPRA TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- TBEAPRA.: ' WS-EDICAO
           ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB956 ******************'.
      *
       3100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0998-00-ABEND-DB2           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           MOVE    SQLCODE         TO      WS-SQLCOD-DB2.

           DISPLAY '******************* GPFPB956 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB956 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*   PROBLEMAS NO ' WS-ACESSO-DB2 ' DA TABELA '
           WS-TABELA-DB2 '    *'.
           DISPLAY '*                                             *'.
           DISPLAY '*                SQLCODE = ' WS-SQLCOD-DB2
           '               *'.
           DISPLAY '*                                             *'.
           DISPLAY '*           PONTO COM ERRO...: ' WS-PTO-ERRO
           '            *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB956 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB956 ******************'.

           EXEC    SQL     ROLLBACK

           END-EXEC.

      *    CALL    'IBM'.

           GOBACK.
      *
       0998-00-EXIT.
           EXIT.
      *
      *****************************************************************
       0999-00-ABEND-ARQ           SECTION.
      *****************************************************************
      *
           MOVE    12              TO      RETURN-CODE.

           DISPLAY '******************* GPFPB956 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB956 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '* PROBLEMAS ' WS-ACESSO-ARQ ' DO ARQUIVO '
           WS-DDNAME-ARQ ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '*             FILE STATUS....: ' WS-FS-ARQ
           '             *'.
           DISPLAY '*                                             *'.
           DISPLAY '*           PONTO COM ERRO...: ' WS-PTO-ERRO
           '            *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB956 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB956 ******************'.

           EXEC    SQL     ROLLBACK

           END-EXEC.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-00-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB956                  *
      *****************************************************************
