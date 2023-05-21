       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPPRJ1.
      ******************************
      *  1ER PROJET                *
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
               MOVE ASUR-TAUX TO DISP-TAUX
               MOVE ASUR-PRM-BS TO DISP-PRM-BS
               DISPLAY '|MATRICULE     : ' MATRICUL
               DISPLAY '|NOM-PRENOM    : ' NOM-CLIENT
               DISPLAY '|RUE-ADRESSE   : ' ASUR-ADRS
               DISPLAY '|CODE-POSTAL   : ' ASUR-CP
               DISPLAY '|VILLE         : ' ASUR-VLL
               DISPLAY '|TYPE-VEHICULE : ' ASUR-TP-VHCL
               DISPLAY '|PRIME         : ' DISP-PRM-BS
               DISPLAY '|BONUS-MALUS   : ' ASUR-BM
               DISPLAY '|TAUX          : ' DISP-TAUX '%'
               DISPLAY LIGNE-I
               READ ASSUR
               ADD 1 TO CPT
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
