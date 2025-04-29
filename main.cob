IDENTIFICATION DIVISION.
PROGRAM-ID. ARMY.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 FIN         PIC X VALUE "N".
    01 CHOICE             PIC 99.
    01 NB-UNITES          PIC 9 VALUE 0.
    01 NAME     PIC X(15).
    01 CLASSE     PIC X(15).
    01 LEVEL     PIC 99.
    01 FIND-UNITE     PIC X(15).
    01 UNITES.
        05 UNITE OCCURS 5 TIMES INDEXED BY I.
            10 NOM-UNITE     PIC X(15).
            10 CLASSE-UNITE  PIC X(10).
            10 NIVEAU-UNITE  PIC 99.
            10 STATS.
                15 HP-UNITE      PIC 99.
                15 ATK-UNITE     PIC 99.
                15 MAG-UNITE     PIC 99.
                15 TEC-UNITE     PIC 99.
                15 SPD-UNITE     PIC 99.
                15 LCK-UNITE     PIC 99. 
                15 CPT-UNITE     PIC 99. 
                15 DEF-UNITE     PIC 99.
                15 RES-UNITE     PIC 99. 
                15 MVT-UNITE     PIC 99.

PROCEDURE DIVISION.
    PERFORM UNTIL FIN = "O"
        DISPLAY "------------------------------"
        DISPLAY "Bienvenue dans ta gestion d'armee"
        DISPLAY "1. Ajouter une unite"
        DISPLAY "2. Monter de niveau"
        DISPLAY "3. Promotion"
        DISPLAY "4. Afficher les informations sur une unite"
        DISPLAY "5. Lister toutes les unites"
        DISPLAY "6. Rechercher par nom"
        DISPLAY "0. Quitter"
        ACCEPT CHOICE

        EVALUATE CHOICE
            WHEN 1
                ADD 1 TO NB-UNITES
                ACCEPT NAME
                MOVE NAME TO NOM-UNITE(NB-UNITES)
                ACCEPT CLASSE
                MOVE CLASSE TO CLASSE-UNITE(NB-UNITES)
                ACCEPT LEVEL
                MOVE LEVEL TO NIVEAU-UNITE(NB-UNITES)
                WHEN 2
                    ACCEPT FIND-UNITE
                    PERFORM VARYING I FROM 1 BY 1 UNTIL I > NB-UNITES
                        IF FIND-UNITE = NOM-UNITE(I)
                            IF NIVEAU-UNITE(I) < 20
                                ADD 1 TO NIVEAU-UNITE(I)
                                DISPLAY "Niveau augmente ! Lancement de la monte de stats."
                
                                DISPLAY "Entrer un chiffre entre 0 et 99 pour le hasard :"
                                ACCEPT CHOICE
                
                                IF CHOICE < 70
                                    ADD 1 TO HP-UNITE(I)
                                    DISPLAY "HP +1"
                                END-IF
                                IF CHOICE >= 30 AND CHOICE <= 80
                                    ADD 1 TO ATK-UNITE(I)
                                    DISPLAY "ATK +1"
                                END-IF
                                IF CHOICE > 40
                                    ADD 1 TO DEF-UNITE(I)
                                    DISPLAY "DEF +1"
                                END-IF
                                IF CHOICE <= 60
                                    ADD 1 TO SPD-UNITE(I)
                                    DISPLAY "SPD +1"
                                END-IF
                                IF CHOICE > 50
                                    ADD 1 TO MAG-UNITE(I)
                                    DISPLAY "MAG +1"
                                END-IF
                                IF CHOICE <= 20
                                    ADD 1 TO LCK-UNITE(I)
                                    DISPLAY "LCK +1"
                                END-IF
                
                            ELSE
                                DISPLAY "Level impossible (deja niveau 20)"
                            END-IF
                        END-IF
                    END-PERFORM

            WHEN 3
                ACCEPT FIND-UNITE
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > NB-UNITES
                    IF FIND-UNITE = NOM-UNITE(I)
                        IF NIVEAU-UNITE(I) = 20
                            DISPLAY "Passage classe supérieur possible"
                        ELSE
                            DISPLAY "Passage impossible"
                        END-IF
                    END-IF
                END-PERFORM
            WHEN 5
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > NB-UNITES
                    DISPLAY "------------------------------"
                    DISPLAY "Nom    : " NOM-UNITE(I)
                    DISPLAY "Classe : " CLASSE-UNITE(I)
                    DISPLAY "Niveau : " NIVEAU-UNITE(I)
                END-PERFORM
            WHEN 6
                ACCEPT FIND-UNITE
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > NB-UNITES
                    IF FIND-UNITE = NOM-UNITE(I)
                        DISPLAY "Nom    : " NOM-UNITE(I)
                        DISPLAY "Classe : " CLASSE-UNITE(I)
                        DISPLAY "Niveau : " NIVEAU-UNITE(I)
                    END-IF
                END-PERFORM
            WHEN 0
                MOVE "O" TO FIN
            WHEN OTHER
                DISPLAY "Choix invalide"
        END-EVALUATE
    END-PERFORM

STOP RUN.
