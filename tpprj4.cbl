       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPPRJ4.
      ******************************
      *  4                         *
      ******************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ASSUR  ASSIGN ASSUR
             FILE STATUS  WS-ASR.

       DATA DIVISION.
       FILE SECTION.
       FD  ASSUR RECORDING F.
       01  EASR.
           05 MATRICUL       PIC 9(6).
           05 NOM-CLIENT     PIC X(20).
           05 ASUR-ADRS      PIC X(18).
           05 ASUR-CP        PIC 9(5).
           05 ASUR-VLL       PIC X(12).
           05 ASUR-TP-VHCL   PIC X.
           05 ASUR-PRM-BS    PIC 9(4)V99.
           05 ASUR-BM        PIC X.
           05 ASUR-TAUX      PIC 99.
           05                PIC X(9).
       WORKING-STORAGE SECTION.
       77  WS-ASR         PIC XX.
       77  LIGNE-H        PIC X(40).
       77  LIGNE-B        PIC X(40).
       77  LIGNE-I        PIC X(40).
       77  DISP-TAUX      PIC Z9.
       77  DISP-PRM-BS    PIC ZB999V,99.
       77  CPT            PIC 999.
       PROCEDURE DIVISION.
           SORT ASSUR ON ASCENDING KEY ASUR-CP
                INPUT PROCEDURE EXTRACTION
                OUTPUT PROCEDURE EDITION STOP RUN.
           
           OPEN INPUT ASSUR
           PERFORM TEST-STAT-ASR
           MOVE ALL '_' TO LIGNE-H
           MOVE ALL '-' TO LIGNE-B LIGNE-I
           MOVE " " TO LIGNE-H(1:1) LIGNE-H(40:1)
           MOVE "`" TO LIGNE-B(1:1)
           MOVE "'" TO LIGNE-B(40:1)
           MOVE "|" TO LIGNE-I(1:1)
           DISPLAY LIGNE-H
           READ ASSUR
           MOVE 0 TO CPT
           PERFORM UNTIL WS-ASR = '10'
              
           END-PERFORM
           DISPLAY 'NOMBRE D''ENREGISTREMENTS LUS : ' CPT
           CLOSE ASSUR
           PERFORM TEST-STAT-ASR
           GOBACK.
       TEST-STAT-ASR.
           IF WS-ASR NOT = '00'
             DISPLAY 'ERREUR FICHIER ASSURE ' WS-ASR
             MOVE 16 TO RETURN-CODE
             STOP RUN
           END-IF
           .
