        IDENTIFICATION DIVISION.
        PROGRAM-ID. GestionAlchimie.
        AUTHOR. LEAU-LARTIGUE-LE BERRE.
        DATE-WRITTEN. 19-02-2022.

        ENVIRONMENT DIVISION.

        CONFIGURATION SECTION.
        SPECIAL-NAMES.
                DECIMAL-POINT IS COMMA.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
                select fIn assign to "StockIngredient.dat"
                organization indexed
                access mode is dynamic
                record key is fIn_nomIn
                alternate record key is fIn_type WITH DUPLICATES
                file status is cr_fIn.

                select fRec assign to "Recettes.dat"
                organization indexed
                access mode is dynamic
                record key is fRec_id
                alternate record key is fRec_nom WITH DUPLICATES
                alternate record key is fRec_ingredient WITH DUPLICATES
                file status is cr_fRec.

                select fPot assign to "Potions.dat"
                organization indexed
                access mode is dynamic
                record key is fPot_nom
                alternate record key is fPot_effet WITH DUPLICATES
                file status is cr_fPot.

                select fVen assign to "Ventes.dat"
                organization sequential
                access mode is sequential
                file status is cr_fVen.


                select fCom assign to "Comptes.dat"
                organization indexed
                access mode is dynamic
                record key is fCom_id
                file status is cr_fCom.

        DATA DIVISION.

        FILE SECTION.
        FD fIn.
        01 tamp_fIn.
                02 fIn_nomIn PIC A(30).
                02 fIn_quantite PIC 9(4).
                02 fIn_type PIC A(10).
                02 fIn_prix PIC 9(10).
        FD fRec.
        01 tamp_fRec.
                02 fRec_id PIC 9(3).
                02 fRec_nom PIC A(30).
                02 fRec_ingredient PIC A(30).
                02 fRec_quantite PIC 9(4).
                02 fRec_ordre PIC 9(3).
        FD fPot.
        01 tamp_fPot.
                02 fPot_nom PIC A(30).
                02 fPot_quantite PIC 9(4).
                02 fPot_effet PIC A(30).
                02 fPot_sold PIC 9(4).
                02 fPot_prix PIC 9(4).
        FD fVen.
        01 tamp_fVen.
                02 fVen_date PIC A(22).
                02 fVen_nomPotion PIC A(30).
                02 fVen_quantite PIC 9(4).
                02 fVen_Prix PIC 9(10).

        FD fCom.
        01 tamp_fCom.
                02 fCom_id PIC A(30).
                02 fCom_motDePasse PIC X(30).
                02 fCom_role PIC 9.

        WORKING-STORAGE SECTION.

        77 cr_fIn PIC 9(2).
        77 cr_fRec PIC 9(2).
        77 cr_fPot PIC 9(2).
        77 cr_fVen PIC 9(2).
        77 cr_fCom PIC 9(2).
        77 connexionChoix PIC 9.
        77 connexionId PIC 9.
        77 connexionMotDePasse PIC X(30).
        77 connexionOk PIC 9.
        77 alchimisteOk PIC 9.
        77 alchimisteChoix PIC 9.
        77 clientOk PIC 9.
        77 clientChoix PIC 9.
        77 registreOk PIC 9.
        77 registreChoix PIC 9.
        77 recetteOK PIC 9.
        77 recetteChoix PIC 9.
        77 recettePotionOK PIC 9.
        77 recettePotionChoix PIC A(30).
        77 recetteEffetOK PIC 9.
        77 recetteEffetOK2 PIC 9.
        77 recetteEffetChoix PIC A(30).
        77 recetteEffetCnt PIC 9(3).
        77 recettePotionCnt PIC 9(3).
        77 stockPotionEffet PIC 9.
        77 stockPotionOk PIC 9.
        77 stockPotionChoix PIC 9.
        77 potionFin PIC 9.
        77 potionDispo pic 9.
        77 effetPot PIC A(30).
        77 nomPot PIC A(30).
        77 createOK PIC 9.
        77 createChoix PIC 9.
        77 createRecettePotionCnt PIC 9(3).
        77 createRecettePotionOk PIC 9.
        77 createRecettePotionVrf PIC 9.
        77 createRecettePotionTrg PIC A(30).
        77 createRecettePotionOK2 PIC 9.
        77 createRecettePotionChoix PIC A(30).
        77 StatsPotFin PIC 9.
        77 totalMoney PIC 9(10).
        77 StatsMaxEff PIC 9(4).
        77 StatsMaxEffF PIC 9(4).
        77 StatsMaxPot PIC 9(4).
        77 SDPF2 PIC 9.
        77 StatsDisplayPotFin PIC 9.
        77 BestPot PIC A(30).
        77 BestEff PIC A(30).
        77 ValStoInFin PIC 9.
        77 ValStoInT PIC 9(10).
        77 ValStoIn PIC 9(10).
        77 createRecettePotionValid PIC 9.
        77 ZoneAct PIC A(30).
        77 ZoneTemp PIC A(30).
        77 roleUser pic 9.
        77 ListEffZoneAct PIC A(30).
        77 ListEffZoneTemp PIC A(30).
        77 ListEffetsFin PIC 9.
        77 recetteInChoix PIC A(30).
        77 recetteInCnt PIC 9(3).
        77 recetteInOK PIC 9.
        77 stockIngredientsChoix PIC 9.
        77 stockIngredientsOk PIC 9.
        77 ingredientFin PIC 9.
        77 typeIng PIC A(10).
        77 nomIng PIC A(30).
        77 potionAchatNom PIC A(30).
        77 potionAchatQuantite PIC 9(4).
        77 potionAchatPrix PIC 9(4).
        77 ingQuantite PIC 9(4).
        77 ingPrix PIC 9(4).
        77 result PIC 9(5).
        77 ventesFin PIC 9.
        77 ventesJFin PIC 9.
        77 datedo PIC A(22).



        PROCEDURE DIVISION.

        open i-o fIn
        if cr_fIn = 35
        then
                open output fIn
        end-if
        close fIn

        open i-o fRec
        if cr_fRec = 35
        then
                open output fRec
        end-if
        close fRec

        open i-o fPot
        if cr_fPot = 35
        then
                open output fPot
        end-if
        close fPot

        open i-o fVen
        if cr_fVen = 35
        then
                open output fVen
        end-if
        close fVen

        open i-o fCom
        if cr_fCom = 35
        then
                open output fCom
        end-if
         close fCom

        open i-o fIn
        move "Cloche de feu" to fIn_nomIn
        move 771 to fIn_quantite
        move "plante" to fIn_type
        move 54 to fIn_prix
        write tamp_fIn

        move "Cloche doree" to fIn_nomIn
        move 869 to fIn_quantite
        move "plante" to fIn_type
        move 134 to fIn_prix
        write tamp_fIn

        move "Banane epineuse" to fIn_nomIn
        move 674 to fIn_quantite
        move "plante" to fIn_type
        move 136 to fIn_prix
        write tamp_fIn

        move "Fleur gelee" to fIn_nomIn
        move 845 to fIn_quantite
        move "plante" to fIn_type
        move 97 to fIn_prix
        write tamp_fIn

        move "Racine de lave" to fIn_nomIn
        move 921 to fIn_quantite
        move "plante" to fIn_type
        move 123 to fIn_prix
        write tamp_fIn

        move "Herbe nouee" to fIn_nomIn
        move 9 to fIn_quantite
        move "plante" to fIn_type
        move 91 to fIn_prix
        write tamp_fIn

        move "Terraria" to fIn_nomIn
        move 958 to fIn_quantite
        move "plante" to fIn_type
        move 32 to fIn_prix
        write tamp_fIn

        move "Baton epineux" to fIn_nomIn
        move 634 to fIn_quantite
        move "plante" to fIn_type
        move 85 to fIn_prix
        write tamp_fIn

        move "Chardon orageux" to fIn_nomIn
        move 325 to fIn_quantite
        move "plante" to fIn_type
        move 97 to fIn_prix
        write tamp_fIn

        move "Fleur des lacs" to fIn_nomIn
        move 149 to fIn_quantite
        move "plante" to fIn_type
        move 48 to fIn_prix
        write tamp_fIn

        move "Fleur des vents" to fIn_nomIn
        move 10 to fIn_quantite
        move "plante" to fIn_type
        move 65 to fIn_prix
        write tamp_fIn

        move "Betterave grumeleuse" to fIn_nomIn
        move 35 to fIn_quantite
        move "plante" to fIn_type
        move 96 to fIn_prix
        write tamp_fIn

        move "Belladonne" to fIn_nomIn
        move 17 to fIn_quantite
        move "plante" to fIn_type
        move 110 to fIn_prix
        write tamp_fIn

        move "Champignon noir" to fIn_nomIn
        move 642 to fIn_quantite
        move "champignon" to fIn_type
        move 64 to fIn_prix
        write tamp_fIn

        move "Selle de dryade" to fIn_nomIn
        move 390 to fIn_quantite
        move "champignon" to fIn_type
        move 59 to fIn_prix
        write tamp_fIn

        move "Champi gobelin" to fIn_nomIn
        move 356 to fIn_quantite
        move "champignon" to fIn_type
        move 54 to fIn_prix
        write tamp_fIn

        move "Champignon vert" to fIn_nomIn
        move 453 to fIn_quantite
        move "champignon" to fIn_type
        move 98 to fIn_prix
        write tamp_fIn

        move "Parasite du marais" to fIn_nomIn
        move 989 to fIn_quantite
        move "champignon" to fIn_type
        move 75 to fIn_prix
        write tamp_fIn

        move "Champignon sanglant" to fIn_nomIn
        move 199 to fIn_quantite
        move "champignon" to fIn_type
        move 102 to fIn_prix
        write tamp_fIn

        move "Chanterelle ombrageuse" to fIn_nomIn
        move 213 to fIn_quantite
        move "champignon" to fIn_type
        move 107 to fIn_prix
        write tamp_fIn

        move "Etagere de soufre" to fIn_nomIn
        move 240 to fIn_quantite
        move "champignon" to fIn_type
        move 118 to fIn_prix
        write tamp_fIn

        move "Champignon etrange" to fIn_nomIn
        move 450 to fIn_quantite
        move "champignon" to fIn_type
        move 91 to fIn_prix
        write tamp_fIn

        move "Pythonissam boletus" to fIn_nomIn
        move 266 to fIn_quantite
        move "champignon" to fIn_type
        move 139 to fIn_prix
        write tamp_fIn

        move "Truffe des tombes" to fIn_nomIn
        move 906 to fIn_quantite
        move "champignon" to fIn_type
        move 108 to fIn_prix
        write tamp_fIn

        move "Amanite mouchetee " to fIn_nomIn
        move 12 to fIn_quantite
        move "champignon" to fIn_type
        move 254 to fIn_prix
        write tamp_fIn

        move "Lactere duveteux" to fIn_nomIn
        move 10 to fIn_quantite
        move "champignon" to fIn_type
        move 112 to fIn_prix
        write tamp_fIn

        move "Cristal nuageux" to fIn_nomIn
        move 10 to fIn_quantite
        move "pierre" to fIn_type
        move 643 to fIn_prix
        write tamp_fIn

        move "Pyrite" to fIn_nomIn
        move 595 to fIn_quantite
        move "pierre" to fIn_type
        move 321 to fIn_prix
        write tamp_fIn

        move "Saphir gele" to fIn_nomIn
        move 995 to fIn_quantite
        move "pierre" to fIn_type
        move 536 to fIn_prix
        write tamp_fIn

        move "Citrine incendiaire" to fIn_nomIn
        move 277 to fIn_quantite
        move "pierre" to fIn_type
        move 964 to fIn_prix
        write tamp_fIn

        move "Rubis sanglant" to fIn_nomIn
        move 277 to fIn_quantite
        move "pierre" to fIn_type
        move 964 to fIn_prix
        write tamp_fIn

        move "Pierre de lune" to fIn_nomIn
        move 320 to fIn_quantite
        move "pierre" to fIn_type
        move 678 to fIn_prix
        write tamp_fIn

        close fIn

        open i-o fRec
        move 1 to fRec_id
        move "Vol d'hirondelle" to fRec_nom
        move "Cloche-doree" to fRec_ingredient
        move "2" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec


        move 2 to fRec_id
        move "Vol d hirondelle" to fRec_nom
        move "Cristal nuageux" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec


        move 3 to fRec_id
        move "Vol d hirondelle" to fRec_nom
        move "Fleur des vents" to fRec_ingredient
        move "3" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 4 to fRec_id
        move "Vol d hirondelle" to fRec_nom
        move "Lactere duveteux" to fRec_ingredient
        move "2" to fRec_quantite
        move "4" to fRec_ordre
        write tamp_fRec

        move 5 to fRec_id
        move "Vision de chat" to fRec_nom
        move "Herbe nouee" to fRec_ingredient
        move "2" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 6 to fRec_id
        move "Vision de chat" to fRec_nom
        move "Pierre de lune" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 7 to fRec_id
        move "Vision de chat" to fRec_nom
        move "Lactere duveteux" to fRec_ingredient
        move "2" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 8 to fRec_id
        move "Tache de salamandre" to fRec_nom
        move "Cloche de feu" to fRec_ingredient
        move "4" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 9 to fRec_id
        move "Tache de salamandre" to fRec_nom
        move "Champignon noir" to fRec_ingredient
        move "2" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 10 to fRec_id
        move "Tache de salamandre" to fRec_nom
        move "Citrine incendiaire" to fRec_ingredient
        move "1" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 11 to fRec_id
        move "Poudre de perlinpinpin" to fRec_nom
        move "Cloche doree" to fRec_ingredient
        move "1" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 12 to fRec_id
        move "Poudre de perlinpinpin" to fRec_nom
        move "Selle de dryade" to fRec_ingredient
        move "2" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 13 to fRec_id
        move "Poudre de perlinpinpin" to fRec_nom
        move "Pyrite" to fRec_ingredient
        move "2" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 14 to fRec_id
        move "Suraux mortel" to fRec_nom
        move "Banane epineuse" to fRec_ingredient
        move "5" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 15 to fRec_id
        move "Suraux mortel" to fRec_nom
        move "Amanite mouchetee" to fRec_ingredient
        move "4" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 16 to fRec_id
        move "Suraux mortel" to fRec_nom
        move "Rubis sanglant" to fRec_ingredient
        move "2" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 17 to fRec_id
        move "Tempete de Zeus" to fRec_nom
        move "Chardon orageux" to fRec_ingredient
        move "2" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 18 to fRec_id
        move "Tempete de Zeus" to fRec_nom
        move "Chanterelle ombrageuse" to fRec_ingredient
        move "3" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 19 to fRec_id
        move "Tempete de Zeus" to fRec_nom
        move "Cristal nuageux" to fRec_ingredient
        move "1" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 20 to fRec_id
        move "Caresse d Aphrodite" to fRec_nom
        move "Herbe nouee" to fRec_ingredient
        move "2" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 21 to fRec_id
        move "Caresse d'Aphrodite" to fRec_nom
        move "Champignon etrange" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 22 to fRec_id
        move "Caresse d Aphrodite" to fRec_nom
        move "Rubis sanglant" to fRec_ingredient
        move "1" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 23 to fRec_id
        move "Crachat de goblin" to fRec_nom
        move "Betterave grumeleuse" to fRec_ingredient
        move "1" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 24 to fRec_id
        move "Crachat de goblin" to fRec_nom
        move "Champi gobelin" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 25 to fRec_id
        move "Crachat de goblin" to fRec_nom
        move "Pyrite" to fRec_ingredient
        move "2" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 26 to fRec_id
        move "Rage d Odin" to fRec_nom
        move "Belladone" to fRec_ingredient
        move "2" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 27 to fRec_id
        move "Rage d Odin" to fRec_nom
        move "Pythonissam boletus" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 28 to fRec_id
        move "Rage d Odin" to fRec_nom
        move "Pierre de lune" to fRec_ingredient
        move "3" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 29 to fRec_id
        move "Souffle de volcan" to fRec_nom
        move "Racine de lave" to fRec_ingredient
        move "3" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 30 to fRec_id
        move "Souffle de volcan" to fRec_nom
        move "Etagere de soufre" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 31 to fRec_id
        move "Souffle de volcan" to fRec_nom
        move "Citrine incendiaire" to fRec_ingredient
        move "2" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 32 to fRec_id
        move "Reve hesoterique" to fRec_nom
        move "Terraria" to fRec_ingredient
        move "2" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 33 to fRec_id
        move "Reve hesoterique" to fRec_nom
        move "Champignon etrange" to fRec_ingredient
        move "2" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 34 to fRec_id
        move "Reve hesoterique" to fRec_nom
        move "Parasite du marais" to fRec_ingredient
        move "2" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 35 to fRec_id
        move "Source divine" to fRec_nom
        move "Fleur des lacs" to fRec_ingredient
        move "2" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 36 to fRec_id
        move "Source divine" to fRec_nom
        move "Lactere duveteux" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 37 to fRec_id
        move "Source divine" to fRec_nom
        move "Pierre de lune" to fRec_ingredient
        move "1" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 38 to fRec_id
        move "Disimilation du doppelganger" to fRec_nom
        move "Fleur des lacs" to fRec_ingredient
        move "1" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 39 to fRec_id
        move "Disimilation du doppelganger" to fRec_nom
        move "Selle de dryade" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 40 to fRec_id
        move "Disimilation du doppelganger" to fRec_nom
        move "Cristal nuageux" to fRec_ingredient
        move "2" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 41 to fRec_id
        move "Etoile du matin" to fRec_nom
        move "Cloche doree" to fRec_ingredient
        move "2" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 42 to fRec_id
        move "Etoile du matin" to fRec_nom
        move "Champignon vert" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 43 to fRec_id
        move "Etoile du matin" to fRec_nom
        move "Pierre de lune" to fRec_ingredient
        move "1" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 44 to fRec_id
        move "Voile misterieux" to fRec_nom
        move "Betterave grumeleuse" to fRec_ingredient
        move "1" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 45 to fRec_id
        move "Voile misterieux" to fRec_nom
        move "Chanterelle ombrageuse" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 46 to fRec_id
        move "Voile misterieux" to fRec_nom
        move "Cristal nuageux" to fRec_ingredient
        move "1" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 47 to fRec_id
        move "Benediction de fee" to fRec_nom
        move "Fleur des lacs" to fRec_ingredient
        move "2" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 48 to fRec_id
        move "Benediction de fee" to fRec_nom
        move "Pythonissam boletus" to fRec_ingredient
        move "2" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 49 to fRec_id
        move "Benediction de fee" to fRec_nom
        move "Saphire gele" to fRec_ingredient
        move "1" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 50 to fRec_id
        move "Soulevement des tombes" to fRec_nom
        move "Epine de sang" to fRec_ingredient
        move "1" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 51 to fRec_id
        move "Soulevement des tombes" to fRec_nom
        move "Truffe des tombes" to fRec_ingredient
        move "4" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 52 to fRec_id
        move "Soulevement des tombes" to fRec_nom
        move "Rubis sanglant" to fRec_ingredient
        move "1" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 53 to fRec_id
        move "Euphytose nuit" to fRec_nom
        move "Herbe nouee" to fRec_ingredient
        move "3" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 54 to fRec_id
        move "Euphytose nuit" to fRec_nom
        move "Champignon Etrange" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 55 to fRec_id
        move "Euphytose nuit" to fRec_nom
        move "Pierre de lune" to fRec_ingredient
        move "2" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 56 to fRec_id
        move "Pointe de quenouille" to fRec_nom
        move "Herbe nouee" to fRec_ingredient
        move "3" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 57 to fRec_id
        move "Pointe de quenouille" to fRec_nom
        move "Champignon Etrange" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 58 to fRec_id
        move "Pointe de quenouille" to fRec_nom
        move "Saphir gele" to fRec_ingredient
        move "2" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 59 to fRec_id
        move "Aide du destin" to fRec_nom
        move "Belladonne" to fRec_ingredient
        move "1" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 60 to fRec_id
        move "Aide du destin" to fRec_nom
        move "Champi gobelin" to fRec_ingredient
        move "2" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 61 to fRec_id
        move "Aide du destin" to fRec_nom
        move "Rubis sanglant" to fRec_ingredient
        move "3" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 62 to fRec_id
        move "Brume gelante" to fRec_nom
        move "Fleur gelee" to fRec_ingredient
        move "2" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 63 to fRec_id
        move "Brume gelante" to fRec_nom
        move "Champignon noir" to fRec_ingredient
        move "3" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 64 to fRec_id
        move "Brume gelante" to fRec_nom
        move "Saphir gele" to fRec_ingredient
        move "1" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 65 to fRec_id
        move "Action furtive" to fRec_nom
        move "Fleur des vents" to fRec_ingredient
        move "2" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 66 to fRec_id
        move "Action furtive" to fRec_nom
        move "Parasite du marais" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 67 to fRec_id
        move "Action furtive" to fRec_nom
        move "Cristal nuageux" to fRec_ingredient
        move "1" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 68 to fRec_id
        move "Poudre d escampette" to fRec_nom
        move "Feur des lacs" to fRec_ingredient
        move "1" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 69 to fRec_id
        move "Poudre d escampette" to fRec_nom
        move "Lactere duveteux" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 70 to fRec_id
        move "Poudre d escampette" to fRec_nom
        move "Cristal nueageux" to fRec_ingredient
        move "1" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 71 to fRec_id
        move "Veritaphilis" to fRec_nom
        move "Terraria" to fRec_ingredient
        move "1" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 72 to fRec_id
        move "Veritaphilis" to fRec_nom
        move "Truffe des tombes" to fRec_ingredient
        move "2" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 73 to fRec_id
        move "Veritaphilis" to fRec_nom
        move "Cristal nuageux" to fRec_ingredient
        move "1" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 74 to fRec_id
        move "Scaphandre de triton" to fRec_nom
        move "Fleur des lacs" to fRec_ingredient
        move "1" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 75 to fRec_id
        move "Scaphandre de triton" to fRec_nom
        move "Selle de dryade" to fRec_ingredient
        move "2" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 76 to fRec_id
        move "Scaphandre de triton" to fRec_nom
        move "Saphir gele" to fRec_ingredient
        move "1" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        move 77 to fRec_id
        move "Rythe des statues" to fRec_nom
        move "Epine de sang" to fRec_ingredient
        move "1" to fRec_quantite
        move "1" to fRec_ordre
        write tamp_fRec

        move 78 to fRec_id
        move "Rythe des statues" to fRec_nom
        move "Amanite mouchetee" to fRec_ingredient
        move "1" to fRec_quantite
        move "2" to fRec_ordre
        write tamp_fRec

        move 79 to fRec_id
        move "Rythe des statues" to fRec_nom
        move "Saphir gele" to fRec_ingredient
        move "5" to fRec_quantite
        move "3" to fRec_ordre
        write tamp_fRec

        if cr_fRec = 35
        then
                open output fRec
        end-if
        close fRec

        open i-o fPot
        move "Vol d hirondelle" to fPot_nom
        move 0 to fPot_quantite
        move "Levitation" to fPot_effet
        write tamp_fPot

        move "Vision de chat" to fPot_nom
        move 2 to fPot_quantite
        move "Nyctalopie" to fPot_effet
        write tamp_fPot

        move "Tache de salamandre" to fPot_nom
        move 1 to fPot_quantite
        move "Potion de feu" to fPot_effet
        write tamp_fPot

        move "Poudre de perlinpinpin" to fPot_nom
        move 0 to fPot_quantite
        move "Prestance" to fPot_effet
        write tamp_fPot

        move "Suraux mortel" to fPot_nom
        move 0 to fPot_quantite
        move "Poison" to fPot_effet
        write tamp_fPot

        move "Tempete de Zeus" to fPot_nom
        move 5 to fPot_quantite
        move "Foudre" to fPot_effet
        write tamp_fPot

        move "Caresse d Aphrodite" to fPot_nom
        move 0 to fPot_quantite
        move "Charme" to fPot_effet
        write tamp_fPot

        move "Crachat de goblin" to fPot_nom
        move 0 to fPot_quantite
        move "Acide" to fPot_effet
        write tamp_fPot

        move "Rage d Odin" to fPot_nom
        move 11 to fPot_quantite
        move "Bersserker" to fPot_effet
        write tamp_fPot

        move "Soufle de volcan" to fPot_nom
        move 0 to fPot_quantite
        move "Explosion" to fPot_effet
        write tamp_fPot

        move "Reve hesoterique" to fPot_nom
        move 0 to fPot_quantite
        move "Halucination (LSD)" to fPot_effet
        write tamp_fPot

        move "Source Divine" to fPot_nom
        move 0 to fPot_quantite
        move "Soin" to fPot_effet
        write tamp_fPot

        move "Disimulation du doppelganger" to fPot_nom
        move 0 to fPot_quantite
        move "Changement d apparence" to fPot_effet
        write tamp_fPot

        move "Etoile du matin" to fPot_nom
        move 0 to fPot_quantite
        move "Lumiere" to fPot_effet
        write tamp_fPot

        move "Voile misterieux" to fPot_nom
        move 0 to fPot_quantite
        move "Viosion etherer" to fPot_effet
        write tamp_fPot

        move "Benediction des fee" to fPot_nom
        move 0 to fPot_quantite
        move "Potion de mana" to fPot_effet
        write tamp_fPot

        move "Soulevement " to fPot_nom
        move 0 to fPot_quantite
        move "Necromancy" to fPot_effet
        write tamp_fPot

        move "Euphytose nuit" to fPot_nom
        move 0 to fPot_quantite
        move "Sommeil" to fPot_effet
        write tamp_fPot

        move "Pointe de quenouille" to fPot_nom
        move 0 to fPot_quantite
        move "Sommeil" to fPot_effet
        write tamp_fPot

        move "Aide du destin" to fPot_nom
        move 0 to fPot_quantite
        move "Resistance" to fPot_effet
        write tamp_fPot

        move "Brume gelante" to fPot_nom
        move 0 to fPot_quantite
        move "Gele" to fPot_effet
        write tamp_fPot

        move "Action furtive" to fPot_nom
        move 0 to fPot_quantite
        move "Invisibilite" to fPot_effet
        write tamp_fPot

        move "Poudre d escampette" to fPot_nom
        move 0 to fPot_quantite
        move "Vitesse" to fPot_effet
        write tamp_fPot

        move "Veritaphilis" to fPot_nom
        move 0 to fPot_quantite
        move "Veriter" to fPot_effet
        write tamp_fPot

        move "Scaphandre de triton" to fPot_nom
        move 0 to fPot_quantite
        move "Respiration sous l eau" to fPot_effet
        write tamp_fPot

        move "Rythe des statues" to fPot_nom
        move 0 to fPot_quantite
        move "Immobilisation" to fPot_effet
        write tamp_fPot
        display cr_fPot
        if cr_fPot = 35
        then
                open output fPot
        end-if
        close fPot


        display "Witch and Apothecary"
        display "Menu de connexion"
        display "1 - Connexion"
        display "2 - Creer un compte"
        display "0 - Quitter"
        accept connexionChoix
        if connexionChoix < 0 and connexionChoix > 2 then
                        display "Saisie incorrecte"
                end-if
        evaluate connexionChoix
                when 1
                        open input fCom
                perform with test after until connexionOk = 1
                        display "Identifiant : "
                        accept fCom_id
                        display "Mot de Passe : "
                        accept connexionMotDePasse
                        read fCom
                        invalid key
                                display "L'identifiant n'existe pas"
                        not invalid key
                                if connexionMotDePasse = fCom_motDePasse
                                then
                                        move 1 to connexionOk
                                        if fCom_role = 0 then
                                                move 0 to roleUser
                                                perform Alchimiste
                                        else
                                                move 1 to roleUser
                                                perform Client
                                        end-if
                                end-if
                        end-read
                 end-perform
                 close fCom
                when 2
                    OPEN i-o fCom
                        DISPLAY "===============NOUVEAU COMPTE======",
                        "============"
                        DISPLAY "Identifiant :"
                        ACCEPT fCom_id
                        DISPLAY "----------------------------"
                        DISPLAY "Mot de passe :"
                        ACCEPT fCom_motDePasse
                        DISPLAY "----------------------------"
                        DISPLAY "Role : 0- ALCHIMISTE | 1- CLIENT"
                        ACCEPT fCom_role

                        if fCom_role < 0
                        and fCom_role > 2
                        then
                            display "Saisie incorrecte"
                       end-if

                        DISPLAY "===================================",
                        "=============="

                        WRITE tamp_fCom END-WRITE
                        IF cr_fCom = 00 THEN
                            DISPLAY "COMPTE CREE"
                        END-IF
                        close fCom

                when 0
                        display "Vous quittez."
        end-evaluate



        STOP RUN.


      *> Menu Alchimiste
        Alchimiste.
        move 0 to alchimisteOk
        perform with test after until alchimisteOk = 1
                DISPLAY " "
                display "=======MENU ALCHIMISTE======="
                display "1- Creer une potion"
                display "2- Acheter des ingredients"
                display "3- Livre des recettes"
                display "4- Consulter le stock de potions"
                display "5- Consulter le stock d’ingredients"
                display "6- Consulter le registre des ventes"
                display "7- Statistiques"
                display "0- Quitter"
                accept alchimisteChoix
                if alchimisteChoix >= 0 and alchimisteChoix < 8 then
                        move 1 to alchimisteOk
                else
                        display "Saisie incorrecte"
                end-if
        end-perform
        evaluate alchimisteChoix
                when 1
                        perform CreerPotion
                when 2
                        perform AcheterIngredients
                when 3
                        perform ConsulterRecettes
                when 4
                        perform ConsulterStockPotion
                when 5
                        perform ConsulterStockIngredients
                when 6
                        perform ConsulterRegistreVentes
            when 7
                Display "here"
                     perform Stats
                when 0
                        display "Vous quittez."
        end-evaluate.




      *> Menu Client
        Client.

        move 0 to clientOk
        perform with test after until clientOk = 1
                DISPLAY " "
                display "=======MENU CLIENT======="
                display "1- Faire une recherche de potion"
                display "2- Acheter une potion"
                display "0- Quitter"
                accept clientChoix
                if clientChoix >= 0 and clientChoix < 3 then
                        move 1 to clientOk
                else
                        display "Saisie incorrecte"
                end-if
        end-perform
        evaluate clientChoix
                when 1
                        perform ConsulterStockPotion
                when 2
                        perform AcheterPotion
                when 0
                        display "Vous quittez."
        end-evaluate.


      *> Menu Consulter Potions
        ConsulterStockPotion.
        move 0 to stockPotionOk
        perform with test after until stockPotionOk = 1
                display "1- Afficher toutes les potions"
                display "2- Afficher les potions disponibles"
                display "3- Rechercher une potion selon son effet"
                display "4- rechercher une potion selon son nom"
                display "0- Quitter"
                accept stockPotionChoix
                if stockPotionChoix >= 0 and stockPotionChoix < 5 then
                        move 1 to stockPotionOk
                else
                        display "Saisie incorrecte"
                end-if
        end-perform
        evaluate stockPotionChoix
                when 1
                        perform AfficherPotions
                when 2
                        perform AfficherPotionDispoFin
                when 3
                        perform RechercherPotionEffet
                when 4
                        perform RechercherPotionNom
                when 0
                        display "Vous quittez."
                        if roleUser = 0 then
                                perform Alchimiste
                        else
                                perform Client
                        end-if
        end-evaluate.


      *> Menu Consulter Stock d'ingredients
        ConsulterStockIngredients.
        move 0 to stockIngredientsOk
        perform with test after until stockIngredientsOk = 1
                display "1- Afficher toutes les ingredients"
                display "2- Rechercher un ingredient selon son type"
                display "3- rechercher un ingredient selon son nom"
                display "0- Quitter"
                accept stockIngredientsChoix
                if stockIngredientsChoix >= 0 and
                   stockIngredientsChoix < 4  then
                        move 1 to stockIngredientsOk
                else
                        display "Saisie incorrecte"
                end-if
        end-perform
        evaluate stockIngredientsChoix
                when 1
                        perform AfficherIngredients
                when 2
                        perform RechercherIngredientsType
                when 3
                        perform RechercherIngredientsNom
                when 0
                        display "Vous quittez."
                        if roleUser = 0 then

                                perform Alchimiste
                        else
                                perform Client
                        end-if
        end-evaluate.

       AcheterIngredients.

       OPEN input fVen
       MOVE 0 TO StatsPotFin
       MOVE 0 TO totalMoney
       PERFORM WITH TEST AFTER UNTIL StatsPotFin = 1
           READ fVen
           AT END MOVE 1 TO StatsPotFin
           NOT AT END
              ADD fVen_Prix TO totalMoney
              END-READ
       END-PERFORM
       CLOSE fVen

        display "Entrez un nom de l ingredient a acheter : "
        accept fIn_nomIn
        move fIn_nomIn to nomIng
        open input fIn
        move 0 to ingredientFin
        start fPot, key is = fIn_nomIn
        invalid key display "Pas d'ingredient avec ce nom existant"
        not invalid key
                perform with test after until ingredientFin = 1
                        read fIn next
                        at end move 1 to ingredientFin
                        not at end
                                if fIn_nomIn = nomIng
                                then
                                   move fIn_prix to ingPrix
                                end-if
                        end-read
                end-perform
        end-start
        close fIn

       display "Entrez le nombre voulu :"
       accept fIn_quantite
       multiply fIn_quantite by ingPrix giving result
       move fIn_quantite to ingQuantite
       if result > totalMoney then
           move nomIng to fIn_nomIn
           open input fIn
           move 0 to ingredientFin
           start fPot, key is = fIn_nomIn
           invalid key display "Pas d'ingredient avec ce nom existant"
           not invalid key
                   perform with test after until ingredientFin = 1
                           read fIn next
                           at end move 1 to ingredientFin
                           not at end
                                   if fIn_nomIn = nomIng
                                   then
                                      subtract ingQuantite
                                            from fIn_quantite
                                      rewrite tamp_fIn end-rewrite
                                   end-if
                           end-read
                   end-perform
           end-start
           close fIn
        else
           display "Pas assez d'argent pour acheter les ingredients"
        end-if
        if roleUser = 0 then
              perform Alchimiste
        else
              perform Client
        end-if.


       AfficherIngredients.

       open input fIn
       move 0 to ingredientFin
       PERFORM WITH TEST AFTER UNTIL ingredientFin = 1

              READ fIn NEXT
              AT END
                move 1 to ingredientFin
              NOT AT END
                        display " "
                        display "Nom :", fIn_nomIn
                        display "--------------------------------------"
                        display "Quantite :", fIn_quantite
                        display "Type :", fIn_type
                        display "Prix :", fIn_prix
              END-READ
       END-PERFORM
       close fIn
       if roleUser = 0 then
              perform Alchimiste
       else
              perform Client
       end-if.


       RechercherIngredientsType.

        display "Entrez un type"
        accept fIn_type
        move fIn_type to typeIng
        open input fIn
        move 0 to ingredientFin
        display fIn_type
        start fIn, key is = fIn_type
        invalid key display "Pas d'ingredient de ce type existant"
        not invalid key
                display "---------------------------------------"
                perform with test after until ingredientFin = 1
                        read fIn next
                        AT END
                        display fIn_type

                         move 1 to ingredientFin
                        NOT AT END
                        display fIn_type
                        display typeIng
                                if fIn_type = typeIng
                                then

                        display " "
                        display "Nom :", fIn_nomIn
                        display "--------------------------------------"
                        display "Quantite :", fIn_quantite
                        display "Type :", fIn_type
                        display "Prix :", fIn_prix

                                end-if
                        end-read
                end-perform
        end-start
        close fIn
        if roleUser = 0 then
              perform Alchimiste
        else
              perform Client
        end-if.

        RechercherIngredientsNom.

        display "Entrez un nom"
        accept fIn_nomIn
        move fIn_nomIn to nomIng
        open input fIn
        move 0 to ingredientFin
        start fIn, key is = fIn_nomIn
        invalid key display "Pas d'ingredient avec ce nom existant"
        not invalid key
                display "---------------------------------------"
                perform with test after until ingredientFin = 1
                        read fIn next
                        at end move 1 to ingredientFin
                        not at end
                                if fIn_nomIn = nomIng
                                then

                        display " "
                        display "Nom :", fIn_nomIn
                        display "--------------------------------------"
                        display "Quantite :", fIn_quantite
                        display "Type :", fIn_type
                        display "Prix :", fIn_prix

                                end-if
                        end-read
                end-perform
        end-start
        close fIn
        if roleUser = 0 then
              perform Alchimiste
        else
              perform Client
        end-if.

      *> Menu RegistreVentes
        ConsulterRegistreVentes.

        move 0 to registreOk
        perform with test after until registreOk = 1
                display "1- Afficher toutes les ventes"
                display "2- Rechercher les ventes date a un jour"
                display "0- Quitter"
                accept registreChoix
                if registreChoix >= 0 and registreChoix < 3 then
                        move 1 to registreOk
                else
                        display "Saisie incorrecte"
                end-if
        end-perform
        evaluate registreChoix
                when 1
                        perform AfficherVentes
                when 2
                        perform RechercherVentesJour
                when 0
                        display "Vous quittez."
                        if roleUser = 0 then
                                perform Alchimiste
                        else
                                perform Client
                        end-if
        end-evaluate.

        AfficherVentes.

        open input fVen
        move 0 to ventesFin
        PERFORM WITH TEST AFTER UNTIL ventesFin = 1

               READ fVen NEXT
               AT END

                 display "fin du registre"
                 move 1 to ventesFin
               NOT AT END
                         display " "
                         display "Date :", fVen_date
                         display "-------------------------------------"
                         display "Potion :", fVen_nomPotion
                         display "Quantite :", fVen_quantite
                         display "Prix :", fVen_prix
               END-READ
        END-PERFORM
        close fVen
        if roleUser = 0 then
               perform Alchimiste
        else
               perform Client
        end-if.


        RechercherVentesJour.
         display "Entrez une date YYYYMMDD"
          accept fVen_date
          move fVen_date to datedo
         open input fVen
         move 0 to ventesJFin
         PERFORM WITH TEST AFTER UNTIL ventesJFin = 1

               READ fVen NEXT
               AT END
                 display "fin du registre"
                 move 1 to ventesJFin
               NOT AT END
                 if fVen_date = datedo then
                         display " "
                         display "Date :", fVen_date
                         display "-------------------------------------"
                         display "Potion :", fVen_nomPotion
                         display "Quantite :", fVen_quantite
                         display "Prix :", fVen_prix
                 end-if
               END-READ
        END-PERFORM
        close fVen
        if roleUser = 0 then
               perform Alchimiste
        else
               perform Client
        end-if.


      *> a deplacer
       AfficherPotions.

       open input fPot
       move 0 to potionFin
       PERFORM WITH TEST AFTER UNTIL potionFin = 1

              READ fPot NEXT
              AT END
                move 1 to potionFin
              NOT AT END
                        display " "
                        display "Nom :", fPot_nom
                        display "--------------------------------------"
                        display "Quantite :", fPot_quantite
                        display "Effet :", fPot_effet
                        display "Prix :", fPot_prix
              END-READ
       END-PERFORM
       close fPot
       if roleUser = 0 then
              perform Alchimiste
       else
              perform Client
       end-if.

       AfficherPotionDispo.

       open input fPot
       move 0 to potionFin
       PERFORM WITH TEST AFTER UNTIL potionFin = 1

              READ fPot NEXT
              AT END
                move 1 to potionFin
              NOT AT END
                        if fPot_quantite > 0 then

                        display " "
                        display "Nom :", fPot_nom
                        display "--------------------------------------"
                        display "Quantite :", fPot_quantite
                        display "Effet :", fPot_effet
                        display "Prix :", fPot_prix

                        end-if
              END-READ
       END-PERFORM
       close fPot.

       AfficherPotionDispoFin.

       open input fPot
       move 0 to potionFin
       PERFORM WITH TEST AFTER UNTIL potionFin = 1

              READ fPot NEXT
              AT END
                move 1 to potionFin
              NOT AT END
                        if fPot_quantite > 0 then

                        display " "
                        display "Nom :", fPot_nom
                        display "--------------------------------------"
                        display "Quantite :", fPot_quantite
                        display "Effet :", fPot_effet
                        display "Prix :", fPot_prix

                        end-if
              END-READ
       END-PERFORM
       close fPot
       if roleUser = 0 then
              perform Alchimiste
       else
              perform Client
       end-if.

       RechercherPotionEffet.

        display "Entrez un effet"
        accept fPot_effet
        move fPot_effet to effetPot
        open input fPot
        move 0 to potionFin
        start fPot, key is = fPot_effet
        invalid key display "Pas d'effet existant"
        not invalid key
                display "---------------------------------------"
                perform with test after until potionFin = 1
                        read fPot next
                        at end move 1 to potionFin
                        not at end
                                if fPot_effet = effetPot
                                then

                        display " "
                        display "Nom :", fPot_nom
                        display "--------------------------------------"
                        display "Quantite :", fPot_quantite
                        display "Effet :", fPot_effet
                        display "Prix :", fPot_prix

                                end-if
                        end-read
                end-perform
        end-start
        close fPot
        if roleUser = 0 then
              perform Alchimiste
        else
              perform Client
        end-if.

        RechercherPotionNom.

        display "Entrez un nom"
        accept fPot_nom
        move fPot_nom to nomPot
        open input fPot
        move 0 to potionFin
        start fPot, key is = fPot_nom
        invalid key display "Pas de potion portant ce nom"
        not invalid key
                perform with test after until potionFin = 1
                        read fPot next
                        at end move 1 to potionFin
                        not at end
                                if fPot_nom = nomPot
                                then
                                     display "Nom :", fPot_nom
                                     display "Quantite :",fPot_quantite
                                     display "Effet :", fPot_effet
                                     display "Prix :", fPot_prix
                                end-if
                        end-read
                end-perform
        end-start
        close fPot
        if roleUser = 0 then
              perform Alchimiste
        else
              perform Client
        end-if.

       RechercherPotionNomDispo.

        display "Entrez un nom"
        accept fPot_nom
        move fPot_nom to nomPot
        open input fPot
        move 0 to potionFin
        move 0 to potionDispo
        start fPot, key is = fPot_nom
        invalid key display "Pas de potion portant ce nom"
        not invalid key
                perform with test after until potionFin = 1
                        read fPot next
                        at end
                                move 1 to potionFin
                        not at end
                                if fPot_nom = nomPot
                                then
                                        move 1 to potionDispo
                                        move fPot_nom to potionAchatNom
                                        move fPot_quantite
                                           to potionAchatQuantite
                                        move fPot_prix
                                           to potionAchatPrix
                                end-if
                        end-read
                end-perform
        end-start
        close fPot.

       AcheterPotion.

        perform AfficherPotionDispo
        perform RechercherPotionNomDispo
        if potionDispo = 1 then
               perform SoustraireQuantitePotion
               perform AjoutVente
               display "Cette potion a bien ete achetee"
        else
               display "Cette potion n'est pas disponible en stock."
        end-if
        if roleUser = 0 then
              perform Alchimiste
        else
              perform Client
        end-if.


       SoustraireQuantitePotion.

        move fPot_nom to nomPot
        open i-o fPot
        move 0 to potionFin
        start fPot, key is = fPot_nom
        invalid key display "Pas de potion portant ce nom"
        not invalid key
                perform with test after until potionFin = 1
                        read fPot next
                        at end move 1 to potionFin
                        not at end
                                if fPot_nom = nomPot
                                then
                                        subtract 1 from fPot_quantite
                                        rewrite tamp_fPot end-rewrite
                                end-if
                        end-read
                end-perform
        end-start
        close fPot.

       AjoutVente.

       open extend fVen
       move Function CURRENT-DATE to fVen_date
       move potionAchatNom to fVen_nomPotion
       move potionAchatPrix to fVen_Prix
       move potionAchatQuantite to fVen_quantite
       write tamp_fVen
       close fVen.


      *> Menu Livre Recette
        ConsulterRecettes.

        move 0 to recetteOK
        perform with test after until recetteOK = 1
            display "=======LIVRE DE RECETTES======="
                display "1- Afficher toutes les recettes"
                display "2- Afficher les recettes utilisant un",
                "ingredient"
                display "3- Rechercher une recette par effet",
                        "de la potion"
                display "4- Rechercher une recette par nom de la potion"
                display "0- Quitter"
                accept recetteChoix
                if recetteChoix >= 0 and recetteChoix < 5 then
                        move 1 to recetteOK
                else
                        display "Saisie incorrecte"
                end-if
        end-perform
        evaluate recetteChoix
                when 1
                        perform AfficherTouteRecette
                when 2
                        perform AfficherRecetteIngredient
                when 3
                        perform AfficherRecetteEffet
                when 4
                        perform AfficherRecetteNom
                when 0
                        display "Vous quittez."
                        if roleUser = 0 then
                                perform Alchimiste
                        else
                                perform Client
                        end-if
        end-evaluate.

      *> Consulter toutes recette
        AfficherTouteRecette.

        MOVE 0 TO recettePotionCnt
        MOVE 0 TO recettePotionOk
        OPEN INPUT fPot
        OPEN INPUT fRec
        PERFORM WITH TEST AFTER UNTIL recettePotionOk = 1
                READ fPot NEXT
                AT END MOVE 1 TO recettePotionOk
                NOT AT END

                MOVE fPot_nom TO fRec_nom
                START fRec, KEY IS = fRec_nom
                INVALID KEY DISPLAY "la potion", fRec_nom,
                "n'existe pas"
                NOT INVALID KEY
                        DISPLAY "#", recettePotionCnt
                        DISPLAY fPot_nom
                        display "--------------------------------------"
                        MOVE 0 TO recettePotionOK
                        PERFORM WITH TEST AFTER UNTIL recettePotionOk =1
                                READ fRec NEXT
                                AT END MOVE 1 TO recettePotionOk
                                NOT AT END
                                        IF fRec_nom = fPot_nom THEN
                                                DISPLAY "Ordre :",
                                                 fRec_ordre
                                                DISPLAY "Nom de ",
                                                "l'ingredient :",
                                                fRec_ingredient
                                                DISPLAY "Quantite :",
                                                fRec_quantite
                                                display " "
                                                add 1 to
                                                RecettePotionCnt
                                        END-IF
                                END-READ
                       END-PERFORM
                       display "---------------------------------------"
               END-START
               END-READ
        END-PERFORM
        CLOSE fRec
        CLOSE fPot
        if roleUser = 0 then
              perform Alchimiste
        else
              perform Client
        end-if.

      *> Consulter recettes par nom de la potion resultante
        AfficherRecetteNom.

        DISPLAY "Entrer un nom de potion"
        OPEN INPUT fRec
        ACCEPT recettePotionChoix
        MOVE recettePotionChoix TO fRec_nom

        START fRec, KEY IS = fRec_nom
        INVALID KEY DISPLAY "la potion", fRec_nom,
        "n'existe pas"
        NOT INVALID KEY
        DISPLAY fRec_nom
        display "---------------------------------------"
        MOVE 0 TO recettePotionOK
        PERFORM WITH TEST AFTER UNTIL recettePotionOk = 1
                READ fRec NEXT
                AT END MOVE 1 TO recettePotionOk
                NOT AT END
                        IF fRec_nom = recettePotionChoix THEN
                                DISPLAY "Ordre :",
                                fRec_ordre
                                DISPLAY "Nom de ",
                                "l'ingredient :",
                                fRec_ingredient
                                DISPLAY "Quantite :",
                                fRec_quantite
                                display " "
                        END-IF
                END-READ
        END-PERFORM
        display "---------------------------------------"
        END-START
        CLOSE fRec
        if roleUser = 0 then
              perform Alchimiste
        else
              perform Client
        end-if.





       AfficherRecetteIngredient.

      *> Lister les ingredients ici

       DISPLAY "Entrer un ingredient de potion"
       OPEN INPUT fRec
       ACCEPT recetteInChoix
       MOVE recetteInChoix TO fRec_ingredient

       START fRec, KEY IS = fRec_ingredient
       INVALID KEY DISPLAY "ERR:NO SUCH INGREDIENT"
       NOT INVALID KEY
               DISPLAY "Potion ateignable avec :" ,
               recetteInCnt, fRec_ingredient
               display "---------------------------------------"
               MOVE 0 TO recetteInOK
               PERFORM WITH TEST AFTER UNTIL recetteInOK = 1
                       READ fRec NEXT
                       AT END MOVE 1 TO recetteInOK
                       NOT AT END
                               IF fRec_ingredient = recetteInChoix THEN
                                      DISPLAY fRec_nom
                               END-IF
                       END-READ
               END-PERFORM
               display "---------------------------------------"
       END-START

       CLOSE fRec
       if roleUser = 0 then
             perform Alchimiste
       else
             perform Client
       end-if.




      *> Consulter les recettes par effet de la potion resultante

        AfficherRecetteEffet.

        DISPLAY "Entrer un effet de potion"
        OPEN INPUT fPot
        OPEN INPUT fRec
        ACCEPT recetteEffetChoix
        MOVE recetteEffetChoix TO fPot_effet

        START fPot, KEY IS = fPot_effet
        INVALID KEY DISPLAY "aucune potion de ", fPot_effet,
        NOT INVALID KEY
        MOVE 0 TO recetteEffetOK
        MOVE 0 TO recetteEffetCnt
        PERFORM WITH TEST AFTER UNTIL recetteEffetOk = 1
                READ fPot NEXT
                AT END MOVE 1 TO recetteEffetOk
                NOT AT END
                        IF fPot_effet = recetteEffetChoix THEN
      *> Retour                 -THERE
        MOVE fPot_nom TO fRec_nom
        START fRec, KEY IS = fRec_nom
        INVALID KEY DISPLAY "ERR:NO SUCH POT"
        NOT INVALID KEY
                DISPLAY recetteEffetCnt
                DISPLAY fPot_nom
                display "---------------------------------------"
                MOVE 0 TO recetteEffetOK2
                PERFORM WITH TEST AFTER UNTIL recetteEffetOK2 = 1
                        READ fRec NEXT
                        AT END MOVE 1 TO recetteEffetOK2
                        NOT AT END
                                IF fRec_nom = fPot_nom THEN
                                       DISPLAY "Ordre :",
                                       fRec_ordre
                                       DISPLAY "Nom de ",
                                       "l'ingredient :",
                                       fRec_ingredient
                                       DISPLAY "Quantite :",
                                       fRec_quantite
                                       display " "
                                       add 1 to
                                       RecetteEffetCnt
                                END-IF
                        END-READ
                END-PERFORM
                display "---------------------------------------"
        END-START
      *> Fin Retour     -THERE

                        END-IF
                END-READ
        END-PERFORM
        END-START
        CLOSE fRec
        CLOSE fPot
        if roleUser = 0 then
              perform Alchimiste
        else
              perform Client
        end-if.



      *> Menu creer une potion
        CreerPotion.


        move 0 to createOK
        perform with test after until createOK = 1
            display "=======CRÉATION POTION======="
            display "Selectionner une potion a creer a  partir de :"
            display " "
                display "1- Livre de recette complet"
                display "2- Recherche de recette par effet",
                        "de la potion"
                display "3- Recherche de recette par nom",
                        "de la potion"
                display "0- Quitter"
                accept createChoix
                if createChoix >= 0 and createChoix < 4 then
                        move 1 to createOK
                else
                        display "Saisie incorrecte"
                end-if
        end-perform
        evaluate createChoix
                when 1
                        perform CreateAfficherTouteRecette
                when 2
                        perform CreateAfficherRecetteEffet
                when 3
                        perform CreateAfficherRecetteNom
                when 0
                        display "Vous quittez."
                        if roleUser = 0 then
                                perform Alchimiste
                        else
                                perform Client
                        end-if
        end-evaluate
        if roleUser = 0 then
              perform Alchimiste
        else
              perform Client
        end-if.



      *> Creer potion �  partir de l'affichage de toutes les recettes
        CreateAfficherTouteRecette.

        MOVE 0 TO createRecettePotionCnt
        MOVE 0 TO createRecettePotionOk
        OPEN INPUT fPot
        OPEN INPUT fRec
        PERFORM WITH TEST AFTER UNTIL createRecettePotionOk = 1
                READ fPot NEXT
                AT END MOVE 1 TO createRecettePotionOk
                NOT AT END
                MOVE fPot_nom TO fRec_nom
                START fRec, KEY IS = fRec_nom
                INVALID KEY DISPLAY "la potion", fRec_nom,
                "n'existe pas"
                NOT INVALID KEY
                        DISPLAY createRecettePotionCnt,
                        " - ", fPot_nom
                        display "--------------------------------------"
                        MOVE 0 TO createRecettePotionOK
                        PERFORM WITH TEST
                        AFTER UNTIL createRecettePotionOk=1
                                READ fRec NEXT
                                AT END MOVE 1 TO createRecettePotionOk
                                NOT AT END
      *>                                   IF fRec_nom = fPot_nom THEN
                                                DISPLAY "Ordre :",
                                                 fRec_ordre

                                                DISPLAY "Nom de ",
                                                "l'ingredient :",
                                                fRec_ingredient
                                                DISPLAY "Quantite :",
                                                fRec_quantite
                                                display " "
                                                add 1 to
                                                createRecettePotionCnt
      *>                                   END-IF
                                END-READ
                       END-PERFORM
                       display "---------------------------------------"
               END-START
               END-READ
        END-PERFORM
        CLOSE fRec
        CLOSE fPot

      *>Vrf = verification, Trg = target
        OPEN I-O fRec
        OPEN I-O fPot
        OPEN I-O fIn
        MOVE 1 TO createRecettePotionValid
        MOVE 0 TO createRecettePotionVrf
        DISPLAY "entrer le nom de la potion souhaite"
        ACCEPT createRecettePotionTrg
        MOVE createRecettePotionTrg TO fPot_nom
        PERFORM WITH TEST AFTER UNTIL createRecettePotionVrf = 1
                READ fPot
                INVALID KEY MOVE 0 TO createRecettePotionVrf
                NOT INVALID KEY MOVE 1 TO createRecettePotionVrf
                        MOVE createRecettePotionTrg TO fRec_nom
                        display fRec_nom
                        START fRec, KEY IS = fRec_nom
                        INVALID KEY DISPLAY "ERR:potionWithoutRecipe"
                                MOVE 1 TO createRecettePotionVrf
                        NOT INVALID KEY
      *> Retour                 -THERE

         MOVE 0 TO createRecettePotionOK2
         display "ok"
                PERFORM WITH TEST AFTER UNTIL createRecettePotionOk2 = 1
                        READ fRec NEXT
                        AT END MOVE 1 TO createRecettePotionOk2
                        NOT AT END
                          Display fRec_nom, ":", createRecettePotionTrg
                              IF fRec_nom = createRecettePotionTrg THEN
                                      MOVE fRec_ingredient TO fIn_nomIn
                                      display fIn_nomIn
                                      READ fIn
      *> Retour2                        -THERE

        INVALID KEY DISPLAY "ERR:noSuchIngredient"
        NOT INVALID KEY
                IF fIn_quantite < fRec_quantite THEN

      *> Retour3        -THERE

         DISPLAY "vous ne disposez pas suffisament de", fRec_ingredient
         MOVE 0 TO createRecettePotionValid


      *> Fin Retour3
      *>        -THERE
      *> Fin Retour2                                            -THERE
      *> Fin Retour             -THERE

                                                                END-IF
                                                        END-READ
                                                END-IF
                                        END-READ
                                END-PERFORM
                        END-START

                        IF createRecettePotionValid = 1 THEN

                        MOVE createRecettePotionTrg TO fRec_nom
                        display fRec_nom
                        START fRec, KEY IS = fRec_nom
                        INVALID KEY DISPLAY "ERR:potionWithoutRecipe"
                                MOVE 1 TO createRecettePotionVrf
                        NOT INVALID KEY
                            MOVE 0 TO createRecettePotionOK2

                PERFORM WITH TEST AFTER UNTIL createRecettePotionOk2 = 1
                READ fRec NEXT
                AT END MOVE 1 TO createRecettePotionOk2
                NOT AT END
                IF fRec_nom = createRecettePotionTrg THEN
                MOVE fRec_ingredient TO fIn_nomIn
                READ fIn
                INVALID KEY DISPLAY "ERR:noSuchIngredient"
                NOT INVALID KEY
                subtract fRec_quantite from fIn_quantite
                REWRITE tamp_fIn END-REWRITE
                DISPLAY "..."


                                            END-READ
                                            END-IF
                                    END-READ
                            END-PERFORM
                    END-START

                    add 1 to fPot_quantite
                    rewrite tamp_fPot end-rewrite
                    DISPLAY "INGREDIENTS CONSOMMES"
                    DISPLAY "POTION CREEE"
                END-READ
        END-PERFORM


        CLOSE fIn
        CLOSE fRec
        CLOSE fPot
        if roleUser = 0 then
              perform Alchimiste
        else
              perform Client
        end-if.




      *> Creer une potion �  partir de la recherche par nom

        CreateAfficherRecetteNom.

        DISPLAY "Entrer un nom de potion"
        OPEN INPUT fRec
        ACCEPT createRecettePotionChoix
        MOVE createRecettePotionChoix TO fRec_nom

        START fRec, KEY IS = fRec_nom
        INVALID KEY DISPLAY "la potion ", fRec_nom,
        "n'existe pas"
        NOT INVALID KEY
        DISPLAY fRec_nom
        display "---------------------------------------"
        MOVE 0 TO recettePotionOK
        PERFORM WITH TEST AFTER UNTIL recettePotionOk = 1
                READ fRec NEXT
                AT END MOVE 1 TO recettePotionOk
                NOT AT END
                        IF fRec_nom = createRecettePotionChoix THEN
                                DISPLAY "Ordre :", fRec_ordre
                                DISPLAY "Nom de ", "l'ingredient :",
                                fRec_ingredient
                                DISPLAY "Quantite :",fRec_quantite
                                DISPLAY " "
                        END-IF
                END-READ
        END-PERFORM
        display "---------------------------------------"

        OPEN I-O fRec
        OPEN I-O fPot
        OPEN I-O fIn
        MOVE 0 TO createRecettePotionVrf
        MOVE 1 TO createRecettePotionValid
        MOVE createRecettePotionChoix TO createRecettePotionTrg
        DISPLAY "Souhaitez-vous creer cette potion ? 1- OUI 0- NON"
        ACCEPT createRecettePotionVrf

        IF createRecettePotionVrf = 1 THEN

        MOVE createRecettePotionChoix TO fPot_nom
        PERFORM WITH TEST AFTER UNTIL createRecettePotionVrf = 1
                READ fPot
                INVALID KEY MOVE 0 TO createRecettePotionVrf
                NOT INVALID KEY MOVE 1 TO createRecettePotionVrf
                        MOVE createRecettePotionTrg TO fRec_nom
                        START fRec, KEY IS = fRec_nom
                        INVALID KEY DISPLAY "ERR:potionWithoutRecipe",
                        cr_fRec
                        NOT INVALID KEY
      *> Retour                 -THERE

         MOVE 0 TO createRecettePotionOK2
                PERFORM WITH TEST AFTER UNTIL createRecettePotionOk2 = 1
                        READ fRec NEXT
                        AT END MOVE 1 TO createRecettePotionOk2
                        NOT AT END
                                IF fRec_nom = recettePotionChoix THEN
                                      MOVE fRec_ingredient TO fIn_nomIn
                                      READ fIn
      *> Retour2                        -THERE

        INVALID KEY DISPLAY "ERR:noSuchIngredient"
        NOT INVALID KEY
                IF fIn_quantite < fRec_quantite THEN
      *> Retour3        -THERE

         DISPLAY "vous ne disposez pas suffisament de", fRec_ingredient
         MOVE 0 TO createRecettePotionValid


      *> Fin Retour3
      *>        -THERE
      *> Fin Retour2                                            -THERE
      *> Fin Retour             -THERE

                                                                END-IF
                                                        END-READ
                                                END-IF
                                        END-READ
                                END-PERFORM
                        END-START


                     IF createRecettePotionValid = 1 THEN

                        MOVE createRecettePotionTrg TO fRec_nom
                        display fRec_nom
                        START fRec, KEY IS = fRec_nom
                        INVALID KEY DISPLAY "ERR:potionWithoutRecipe"
                                MOVE 1 TO createRecettePotionVrf
                        NOT INVALID KEY
                            MOVE 0 TO createRecettePotionOK2


                PERFORM WITH TEST AFTER UNTIL createRecettePotionOk2 = 1
                READ fRec NEXT
                AT END MOVE 1 TO createRecettePotionOk2
                NOT AT END
                IF fRec_nom = createRecettePotionTrg THEN
                MOVE fRec_ingredient TO fIn_nomIn
                READ fIn
                INVALID KEY DISPLAY "ERR:noSuchIngredient"
                NOT INVALID KEY
                subtract fRec_quantite from fIn_quantite
                REWRITE tamp_fIn END-REWRITE
                DISPLAY "..." , "#", cr_fIn

                                                END-READ
                                                END-IF
                                        END-READ
                                END-PERFORM
                        END-START

                    add 1 to fPot_quantite
                    rewrite tamp_fPot end-rewrite
                    display cr_fPot
                    DISPLAY "INGREDIENTS CONSOMMES"
                    DISPLAY "POTION CREEE"


                END-READ
        END-PERFORM

        ELSE DISPLAY "Vous quittez. Fin."

        END-IF

        CLOSE fIn
        CLOSE fRec
        CLOSE fPot


        END-START
        CLOSE fRec
        if roleUser = 0 then
              perform Alchimiste
        else
              perform Client
        end-if.



       *> Cr�er une potion selon l'effet resultant

        CreateAfficherRecetteEffet.

        DISPLAY "Entrer un effet de potion"
        OPEN INPUT fPot
        OPEN INPUT fRec
        ACCEPT recetteEffetChoix
        MOVE recetteEffetChoix TO fPot_effet

        START fPot, KEY IS = fPot_effet
        INVALID KEY DISPLAY "aucune potion de ", fPot_effet,
        NOT INVALID KEY
        MOVE 0 TO recetteEffetOK
        MOVE 0 TO recetteEffetCnt
        PERFORM WITH TEST AFTER UNTIL recetteEffetOk = 1
                READ fPot NEXT
                AT END MOVE 1 TO recetteEffetOk
                NOT AT END
                        IF fPot_effet = recetteEffetChoix THEN
      *> Retour                 -THERE
        MOVE fPot_nom TO fRec_nom
        START fRec, KEY IS = fRec_nom
        INVALID KEY DISPLAY "ERR:NO SUCH POT"
        NOT INVALID KEY
                DISPLAY recetteEffetCnt, " - ", fPot_nom
                display "---------------------------------------"
                MOVE 0 TO recetteEffetOK2
                PERFORM WITH TEST AFTER UNTIL recetteEffetOK2 = 1
                        READ fRec NEXT
                        AT END MOVE 1 TO recetteEffetOK2
                        NOT AT END
                                IF fRec_nom = fPot_nom THEN
                                        DISPLAY "Ordre :",
                                        fRec_ordre
                                        DISPLAY "Nom de ",
                                        "l'ingredient :",
                                        fRec_ingredient
                                        DISPLAY "Quantite :",
                                        fRec_quantite
                                        DISPLAY " "
                                        add 1 to recetteEffetCnt
                                END-IF
                        END-READ
                END-PERFORM
                display "---------------------------------------"
        END-START
      *> Fin Retour     -THERE

                        END-IF
                END-READ
        END-PERFORM
        END-START
        CLOSE fRec
        CLOSE fPot

        OPEN I-O fRec
        OPEN I-O fPot
        OPEN I-O fIn
        MOVE 0 TO createRecettePotionVrf
        DISPLAY "entrer le nom de la potion souhaite"
        ACCEPT createRecettePotionTrg
        MOVE createRecettePotionTrg TO fPot_nom
        PERFORM WITH TEST AFTER UNTIL createRecettePotionVrf = 1
                READ fPot
                INVALID KEY MOVE 0 TO createRecettePotionVrf
                NOT INVALID KEY MOVE 1 TO createRecettePotionVrf
                        MOVE createRecettePotionTrg TO fRec_nom
                        START fRec, KEY IS = fRec_nom
                        INVALID KEY DISPLAY "ERR:potionWithoutRecipe"
                        NOT INVALID KEY
      *> Retour                 -THERE

         MOVE 0 TO createRecettePotionOK2
                PERFORM WITH TEST AFTER UNTIL createRecettePotionOk2 = 1
                        READ fRec NEXT
                        AT END MOVE 1 TO createRecettePotionOk2
                        NOT AT END
                                IF fRec_nom = recettePotionChoix THEN
                                      MOVE fRec_ingredient TO fIn_nomIn
                                      READ fIn
      *> Retour2                        -THERE

        INVALID KEY DISPLAY "ERR:noSuchIngredient"
        NOT INVALID KEY
                IF fIn_quantite < fRec_quantite THEN
      *> Retour3        -THERE

         DISPLAY "vous ne disposez pas suffisament de", fRec_ingredient
         MOVE 0 TO createRecettePotionValid


      *> Fin Retour3
      *>        -THERE
      *> Fin Retour2                                            -THERE
      *> Fin Retour             -THERE

                                                                END-IF
                                                        END-READ
                                                END-IF
                                        END-READ
                                END-PERFORM
                        END-START

                        IF createRecettePotionValid = 1 THEN

                        MOVE createRecettePotionTrg TO fRec_nom
                        display fRec_nom
                        START fRec, KEY IS = fRec_nom
                        INVALID KEY DISPLAY "ERR:potionWithoutRecipe"
                                MOVE 1 TO createRecettePotionVrf
                        NOT INVALID KEY
                            MOVE 0 TO createRecettePotionOK2


                PERFORM WITH TEST AFTER UNTIL createRecettePotionOk2 = 1
                READ fRec NEXT
                AT END MOVE 1 TO createRecettePotionOk2
                NOT AT END
                IF fRec_nom = createRecettePotionTrg THEN
                MOVE fRec_ingredient TO fIn_nomIn
                READ fIn
                INVALID KEY DISPLAY "ERR:noSuchIngredient"
                NOT INVALID KEY
                subtract fRec_quantite from fIn_quantite
                REWRITE tamp_fIn END-REWRITE
                DISPLAY "..." , "#", cr_fIn



                                                END-READ
                                                END-IF
                                        END-READ
                                END-PERFORM
                        END-START

                    add 1 to fPot_quantite
                    rewrite tamp_fPot end-rewrite
                    display cr_fPot
                    DISPLAY "INGREDIENTS CONSOMMES"
                    DISPLAY "POTION CREEE"

                END-READ
        END-PERFORM
        CLOSE fIn
        CLOSE fRec
        CLOSE fPot
        if roleUser = 0 then
              perform Alchimiste
        else
              perform Client
        end-if.




       Stats.

       OPEN input fVen
       OPEN i-o fPot
       MOVE 0 TO StatsPotFin
       MOVE 0 TO totalMoney
       PERFORM WITH TEST AFTER UNTIL StatsPotFin = 1
           READ fVen
           AT END MOVE 1 TO StatsPotFin
           NOT AT END MOVE fVen_nomPotion TO fPot_nom
           ADD fVen_Prix TO totalMoney
           READ fPot
               INVALID KEY DISPLAY "Err:No such a potion"
               NOT INVALID KEY ADD fVen_quantite TO fPot_Sold
                   WRITE tamp_fVen,
                   END-WRITE
           END-READ
           END-READ
       END-PERFORM
       CLOSE fVen
       CLOSE fPot


       MOVE 0 TO StatsMaxEffF
       MOVE 0 TO StatsMaxPot
       MOVE 0 TO SDPF2
       MOVE 0 TO StatsDisplayPotFin
       MOVE "NULL" TO BestPot
       MOVE "NULL" TO BestEff
       OPEN input fPot
       PERFORM WITH TEST AFTER UNTIL StatsDisplayPotFin = 1
       MOVE fPot_effet TO ZoneAct
           READ fPot NEXT
           AT END MOVE 1 TO StatsDisplayPotFin
           NOT AT END IF fPot_Sold > StatsMaxPot THEN
               MOVE fPot_Sold TO StatsMaxPot
               MOVE fPot_nom TO BestPot
               END-IF
               MOVE fPot_effet TO ZoneTemp
               IF ZoneAct NOT = ZoneTemp THEN
                   IF StatsMaxEff > StatsMaxEffF THEN
                       MOVE StatsMaxEff TO StatsMaxEffF
                       MOVE fPot_effet TO BestEff
                   END-IF
                   MOVE 0 TO StatsMaxEff
                   DISPLAY fPot_effet
               END-IF
           END-READ
           display StatsDisplayPotFin
       END-PERFORM
       CLOSE fPot
       *> init de  effet ça va passer sur tt les zones, faut verifier quand ça change de zone
       *> et chaque changement de zone tu refait le calcul

       OPEN input fIn
       PERFORM WITH TEST AFTER UNTIL ValStoInFin = 1
                READ fIn NEXT
                AT END MOVE 1 TO ValStoInFin
                NOT AT END
                     MULTIPLY fIn_quantite BY fIn_prix GIVING ValStoInT
                     ADD ValStoInT TO ValStoIn
                END-READ
        END-PERFORM
        CLOSE fIn

       Display "===============Statistiques===================="

       DISPLAY "Meilleur Ventes Potions :" ,BestPot, "(",StatsMaxPot,")"
       DISPLAY "Meilleur Ventes Effets :" ,BestEff, "(",StatsMaxEffF,")"
       DISPLAY "Argent total ce mois ci :", totalMoney
       DISPLAY "Valeur du stock d'ingredient :", ValStoIn
       if roleUser = 0 then
              perform Alchimiste
       else
              perform Client
       end-if.


       AfficherListeEffets.

       MOVE 0 TO ListEffetsFin
       OPEN input fPot
       PERFORM WITH TEST AFTER UNTIL ListEffetsFin = 1
       MOVE fPot_effet TO ListEffZoneAct
           READ fPot NEXT
           AT END MOVE 1 TO ListEffetsFin
           NOT AT END
               MOVE fPot_effet TO ListEffZoneTemp
               IF ListEffZoneAct NOT = ListEffZoneTemp THEN
                 display "---------------------------------------"
                   DISPLAY fPot_effet
            display "---------------------------------------"
               END-IF
           END-READ
       END-PERFORM
       CLOSE fPot.
