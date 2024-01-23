      *****************************************************************
       IDENTIFICATION              DIVISION.
      *****************************************************************
      *
       PROGRAM-ID.                 GPFPBCHM.
       AUTHOR.                     ULISSES & MORAES - TI.
       DATE-WRITTEN.               26/01/2014.
       SECURITY.
      *
      *****************************************************************
      * SISTEMA......: SIGPF - SISTEMA DE GESTAO DE PECAS/FILIAIS     *
      *****************************************************************
      * ANALISTA.....: ULISSES & MORAES - TI                          *
      * LINGUAGEM....: COBOL/BATCH/DB2                                *
      * PROGRAMADOR..: ULISSES & MORAES - TI                          *
      * DATA.........: 26/01/2014                                     *
      *****************************************************************
      * OBJETIVO.....: A  PARTIR  DA  DATA/HORA/TIMESTAMP DO DB2      *
      *                FORMATAR A LINKAGE.                            *
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
       01      WS-DAT-DB2          PIC     X(010) VALUE '99.99.9999'.
       01      FILLER              REDEFINES      WS-DAT-DB2.
         03    WS-DIA-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-MES-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-ANO-DB2          PIC     9(004).
      *
       01      WS-HMS-DB2          PIC     X(008) VALUE '99:99:99'.
       01      FILLER              REDEFINES      WS-HMS-DB2.
         03    WS-HOR-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-MIN-DB2          PIC     9(002).
         03    FILLER              PIC     X(001).
         03    WS-SEG-DB2          PIC     9(002).
      *
       01      WS-TIMESTAMP        PIC     X(026) VALUE ZEROS.
      *
       01      WS-DAT-AMD          PIC     9(008) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-DAT-AMD.
         03    WS-ANO-AMD          PIC     9(004).
         03    WS-MES-AMD          PIC     9(002).
         03    WS-DIA-AMD          PIC     9(002).
      *
       01      WS-DAT-DMA          PIC     9(008) VALUE ZEROS.
       01      FILLER              REDEFINES      WS-DAT-DMA.
         03    WS-DIA-DMA          PIC     9(002).
         03    WS-MES-DMA          PIC     9(002).
         03    WS-ANO-DMA          PIC     9(004).
      *
           COPY    COBI901.
      *
       01      WS-GPFBB901         PIC     X(008) VALUE 'GPFBB901'.
      *
      *****************************************************************
      *        AREA DE TRATAMENTO DB2                                 *
      *****************************************************************
      *
           EXEC    SQL
                   INCLUDE SQLCA
           END-EXEC.
      *
      *****************************************************************
       LINKAGE                     SECTION.
      *****************************************************************
       PROCEDURE                   DIVISION.
      *****************************************************************
      *
           CALL    WS-GPFBB901     USING   W901-REG

           END-CALL.

           DISPLAY '* W901-TIM-DB2 = ' W901-TIM-DB2
           DISPLAY '* W901-DAT-DB2 = ' W901-DAT-DB2
           DISPLAY '* W901-DAT-EDI = ' W901-DAT-EDI
           DISPLAY '* W901-DAT-AMD = ' W901-DAT-AMD
           DISPLAY '* W901-DAT-DMA = ' W901-DAT-DMA
           DISPLAY '* W901-HOR-DB2 = ' W901-HOR-DB2
           DISPLAY '* W901-HOR-EDI = ' W901-HOR-EDI
           DISPLAY '* W901-COD-RET = ' W901-COD-RET

           GOBACK.

