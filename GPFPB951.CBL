      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPB951.
       AUTHOR.                     ULISSES & MORAES - TI.
       DATE-WRITTEN.               09/02/2014.
       SECURITY.
      *
      *****************************************************************
      * SISTEMA......: SIGPF - SISTEMA DE GESTAO DE PECAS/FILIAIS     *
      *****************************************************************
      * ANALISTA.....: ULISSES & MORAES - TI                          *
      * LINGUAGEM....: COBOL/BATCH                                    *
      * PROGRAMADOR..: ULISSES & MORAES - TI                          *
      * DATA.........: 09/02/2014                                     *
      *****************************************************************
      * OBJETIVO.....: A PARTIR DO ARQUIVO CADMANC, REALIZAR          *
      *                MANUTENCAO NA TABELA DE CONVENIOS.             *
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
      * INPUT..: CADMANC - CADASTRO DE MANUTENCOES      - LRECL = 100 *
      *****************************************************************
      *
           SELECT  CADMANC  ASSIGN  TO  UT-S-CADMANC
                   FILE     STATUS  IS  WS-FS-CADMANC.
      *
      *****************************************************************
       DATA                        DIVISION.
      *****************************************************************
       FILE                        SECTION.
      *****************************************************************
      * INPUT..: CADMANC - CADASTRO DE MANUTENCOES      - LRECL = 100 *
      *****************************************************************
      *
       FD  CADMANC
           RECORDING  MODE      IS  F
           LABEL      RECORD    IS  STANDARD
           BLOCK      CONTAINS  0   RECORDS.
      *
       01      REG-CADMANC         PIC     X(100).
      *
      *****************************************************************
       WORKING-STORAGE             SECTION.
      *****************************************************************
      *
       01      WS-FS-CADMANC       PIC     9(002) VALUE ZEROS.
      *
       01      WS-LID-CADMANC      PIC     9(018) VALUE ZEROS.
      *
       01      WS-INS-TCSTBS61     PIC     9(018) VALUE ZEROS.
       01      WS-UPD-TCSTBS61     PIC     9(018) VALUE ZEROS.
       01      WS-DEL-TCSTBS61     PIC     9(018) VALUE ZEROS.
      *
       01      WS-EDICAO           PIC     Z.ZZZ.ZZ9.
      *
      *****************************************************************
      *        TRATA CONVERSAO DE DATAS                               *
      *****************************************************************
      *
       01      WS-DAT-AMD          PIC     9(008) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-DAT-AMD.
         03    WS-ANO-AMD          PIC     9(004).
         03    WS-MES-AMD          PIC     9(002).
         03    WS-DIA-AMD          PIC     9(002).
      *
       01      WS-DAT-DB2          PIC     X(010) VALUE '99.99.9999'.
       01      FILLER              REDEFINES      WS-DAT-DB2.
         03    WS-DIA-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-MES-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-ANO-DB2          PIC     9(004).
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
      * INPUT..: CADMANC - CADASTRO DE MANUTENCOES      - LRECL = 100 *
      *****************************************************************
      *
           COPY    COBI951.
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
                   INCLUDE TCSTBS61
           END-EXEC.
      *
      *****************************************************************
       LINKAGE                     SECTION.
      *****************************************************************
       PROCEDURE                   DIVISION.
      *****************************************************************
      *
           PERFORM 0100-00-PROCED-INICIAIS.

           PERFORM 1000-00-PROCED-PRINCIPAIS
             UNTIL WS-FS-CADMANC EQUAL 10.

           PERFORM 3000-00-PROCED-FINAIS.

           GOBACK.
      *
      *****************************************************************
       0100-00-PROCED-INICIAIS     SECTION.
      *****************************************************************
      *
           OPEN    INPUT   CADMANC.

           MOVE   ' NA ABERTURA '  TO      WS-ACESSO-ARQ.

           MOVE    001             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FS-CADMANC.

           PERFORM 0300-00-LEITURA-CADMANC.

           IF      WS-FS-CADMANC   EQUAL   10
                   DISPLAY
                   '******************* GPFPB951 ******************'
                   DISPLAY
                   '*                                             *'
                   DISPLAY
                   '*          ARQUIVO CADMANC ESTA VAZIO         *'
                   DISPLAY
                   '*                                             *'
           END-IF.
      *
       0100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0200-00-TESTA-FS-CADMANC    SECTION.
      *****************************************************************
      *
           IF      WS-FS-CADMANC NOT EQUAL 00 AND 10
                   MOVE 'CADMANC'  TO      WS-DDNAME-ARQ
                   MOVE  WS-FS-CADMANC
                                   TO      WS-FS-ARQ
                   PERFORM         0999-00-ABEND-ARQ
           END-IF.
      *
       0200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       0300-00-LEITURA-CADMANC     SECTION.
      *****************************************************************
      *
           READ    CADMANC         INTO    REG-MANC.

           MOVE   ' NA LEITURA '   TO      WS-ACESSO-ARQ.

           MOVE    002             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FS-CADMANC.

           IF      WS-FS-CADMANC   EQUAL   00
                   ADD 001         TO      WS-LID-CADMANC
           END-IF.
      *
       0300-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1000-00-PROCED-PRINCIPAIS   SECTION.
      *****************************************************************
      *
           PERFORM 1100-00-SELECT-TCSTBS61.

           IF      SQLCODE         EQUAL   +100
                   PERFORM         1200-00-INSERT-TCSTBS61
           ELSE
                   PERFORM         1300-00-PROC-MANUTENCAO
           END-IF.

           PERFORM 0300-00-LEITURA-CADMANC.
      *
       1000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1100-00-SELECT-TCSTBS61     SECTION.
      *****************************************************************
      *
           MOVE    MANC-CO-ENTID   TO      CO-ENTDE-SNDCL-S01.
           MOVE    MANC-TP-ENTID   TO      CO-TIPO-ENTDE-S01.

           EXEC SQL

                SELECT DT_VENCIMENTO,
                       QT_GRCS      ,
                       VR_CONTRIBUICAO

                INTO  :TCSTBS61.DT-VENCIMENTO,
                      :TCSTBS61.QT-GRCS      ,
                      :TCSTBS61.VR-CONTRIBUICAO

                FROM   TCS.TCSTBS61_SMRZOGRCS

                WHERE  CO_ENTDE_SNDCL_S01 = :TCSTBS61.CO-ENTDE-SNDCL-S01
                AND    CO_TIPO_ENTDE_S01  = :TCSTBS61.CO-TIPO-ENTDE-S01

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000 AND +100
                   MOVE 'SELECT'   TO      WS-ACESSO-DB2
                   MOVE 'TCSTBS61' TO      WS-TABELA-DB2
                   MOVE  003       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.
      *
       1100-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1200-00-INSERT-TCSTBS61     SECTION.
      *****************************************************************
      *
           MOVE    MANC-CO-ENTID   TO      CO-ENTDE-SNDCL-S01.
           MOVE    MANC-TP-ENTID   TO      CO-TIPO-ENTDE-S01.

           MOVE    MANC-DT-VENCI   TO      WS-DAT-AMD.

           MOVE    WS-DIA-AMD      TO      WS-DIA-DB2.
           MOVE    WS-MES-AMD      TO      WS-MES-DB2.
           MOVE    WS-ANO-AMD      TO      WS-ANO-DB2.

           MOVE    WS-DAT-DB2      TO      DT-VENCIMENTO.

           MOVE    MANC-QT-GUIAS   TO      QT-GRCS.
           MOVE    MANC-VL-CONTR   TO      VR-CONTRIBUICAO.

           EXEC    SQL      INSERT

                   INTO     TCS.TCSTBS61_SMRZOGRCS

                          ( CO_ENTDE_SNDCL_S01,
                            CO_TIPO_ENTDE_S01 ,
                            DT_VENCIMENTO     ,
                            QT_GRCS           ,
                            VR_CONTRIBUICAO   )

                   VALUES (:CO-ENTDE-SNDCL-S01,
                           :CO-TIPO-ENTDE-S01 ,
                           :DT-VENCIMENTO     ,
                           :QT-GRCS           ,
                           :VR-CONTRIBUICAO   )

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000
                   MOVE 'INSERT'   TO      WS-ACESSO-DB2
                   MOVE 'TCSTBS61' TO      WS-TABELA-DB2
                   MOVE  004       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.

           ADD     001             TO      WS-INS-TCSTBS61.
      *
       1200-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1300-00-PROC-MANUTENCAO     SECTION.
      *****************************************************************
      *
           IF      MANC-TP-MANUT   EQUAL  'A'
                   PERFORM         1400-00-UPDATE-TCSTBS61
           END-IF.

           IF      MANC-TP-MANUT   EQUAL  'E'
                   PERFORM         1500-00-DELETE-TCSTBS61
           END-IF.
      *
       1300-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1400-00-UPDATE-TCSTBS61     SECTION.
      *****************************************************************
      *
           MOVE    MANC-CO-ENTID   TO      CO-ENTDE-SNDCL-S01.
           MOVE    MANC-TP-ENTID   TO      CO-TIPO-ENTDE-S01.

           MOVE    MANC-DT-VENCI   TO      WS-DAT-AMD.

           MOVE    WS-DIA-AMD      TO      WS-DIA-DB2.
           MOVE    WS-MES-AMD      TO      WS-MES-DB2.
           MOVE    WS-ANO-AMD      TO      WS-ANO-DB2.

           MOVE    WS-DAT-DB2      TO      DT-VENCIMENTO.

           MOVE    MANC-QT-GUIAS   TO      QT-GRCS.
           MOVE    MANC-VL-CONTR   TO      VR-CONTRIBUICAO.

           EXEC    SQL

                   UPDATE   TCS.TCSTBS61_SMRZOGRCS

                   SET      DT_VENCIMENTO      = :DT-VENCIMENTO  ,
                            QT_GRCS            = :QT-GRCS        ,
                            VR_CONTRIBUICAO    = :VR-CONTRIBUICAO

                   WHERE    CO_ENTDE_SNDCL_S01 = :CO-ENTDE-SNDCL-S01
                   AND      CO_TIPO_ENTDE_S01  = :CO-TIPO-ENTDE-S01

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000
                   MOVE 'UPDATE'   TO      WS-ACESSO-DB2
                   MOVE 'TCSTBS61' TO      WS-TABELA-DB2
                   MOVE  005       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.

           ADD     SQLERRD(3)      TO      WS-UPD-TCSTBS61.
      *
       1400-99-EXIT.
           EXIT.
      *
      *****************************************************************
       1500-00-DELETE-TCSTBS61     SECTION.
      *****************************************************************
      *
           MOVE    MANC-CO-ENTID   TO      CO-ENTDE-SNDCL-S01.
           MOVE    MANC-TP-ENTID   TO      CO-TIPO-ENTDE-S01.

           EXEC    SQL      DELETE

                   FROM     TCS.TCSTBS61_SMRZOGRCS

                   WHERE    CO_ENTDE_SNDCL_S01 = :CO-ENTDE-SNDCL-S01
                   AND      CO_TIPO_ENTDE_S01  = :CO-TIPO-ENTDE-S01

           END-EXEC.

           IF      SQLCODE     NOT EQUAL   +000
                   MOVE 'DELETE'   TO      WS-ACESSO-DB2
                   MOVE 'TCSTBS61' TO      WS-TABELA-DB2
                   MOVE  006       TO      WS-PTO-ERRO
                   PERFORM         0998-00-ABEND-DB2
           END-IF.

           ADD     SQLERRD(3)      TO      WS-DEL-TCSTBS61.
      *
       1500-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3000-00-PROCED-FINAIS       SECTION.
      *****************************************************************
      *
           CLOSE   CADMANC.

           MOVE   'NO FECHAMENTO'  TO      WS-ACESSO-ARQ.

           MOVE    004             TO      WS-PTO-ERRO.

           PERFORM 0200-00-TESTA-FS-CADMANC.

           PERFORM 3100-00-MONTA-ESTATISTICA.
      *
       3000-99-EXIT.
           EXIT.
      *
      *****************************************************************
       3100-00-MONTA-ESTATISTICA   SECTION.
      *****************************************************************
      *
           DISPLAY '******************* GPFPB001 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*        ESTATISTICA DE PROCESSAMENTO         *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB001 ******************'.
           DISPLAY '*                                             *'.
           MOVE    WS-LID-CADMANC  TO      WS-EDICAO.
           DISPLAY '* REGISTROS LIDOS.......- CADMANC.: ' WS-EDICAO
           ' *'.
           MOVE    WS-INS-TCSTBS61 TO      WS-EDICAO.
           DISPLAY '* REGISTROS GRAVADOS....- TCSTBS61: ' WS-EDICAO
           ' *'.
           MOVE    WS-UPD-TCSTBS61 TO      WS-EDICAO.
           DISPLAY '* REGISTROS ATUALIZADOS.- TCSTBS61: ' WS-EDICAO
           ' *'.
           MOVE    WS-DEL-TCSTBS61 TO      WS-EDICAO.
           DISPLAY '* REGISTROS DELETADOS...- TCSTBS61: ' WS-EDICAO
           ' *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB001 ******************'.
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

           DISPLAY '******************* GPFPB951 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB951 ******************'.
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
           DISPLAY '******************* GPFPB951 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB951 ******************'.

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

           DISPLAY '******************* GPFPB951 ******************'.
           DISPLAY '*                                             *'.
           DISPLAY '*      TERMINO ANORMAL DE PROCESSAMENTO       *'.
           DISPLAY '*                                             *'.
           DISPLAY '******************* GPFPB951 ******************'.
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
           DISPLAY '******************* GPFPB951 ******************'.
           DISPLAY '*     P R O G R A M A  C A N C E L A D O      *'.
           DISPLAY '******************* GPFPB951 ******************'.

           EXEC    SQL     ROLLBACK

           END-EXEC.

      *    CALL    'IBM'.

           GOBACK.
      *
       0999-00-EXIT.
           EXIT.
      *
      *****************************************************************
      *                   FIM DO PROGRAMA - GPFPB951                  *
      *****************************************************************
