################################################################################
#            3. NETTOYAGE                                                      #
################################################################################

################################################################################
#            3.1. NETTOYAGE DE LA BASE MENAGES 2014                            #
################################################################################


menage2014_brut<-data_frame(menage2014_brut)
base_2014 <- subset.data.frame(menage2014_brut, select=c(IDENT,MPANEL, SEXECJ,SEXEPR,AGEPR,AGECJ,
                                                         SITUAPR,SITUACJ, FORCJVIE,
                                                         NACTIFS,PATRI_BRUT_HORSRESTE,PATPROFENT, PATPROFHENT, PATIMM, 
                                                         PATFISOM,PATRI_NET,PATRI_NET_HORSRESTE, PATRI_BRUT, 
                                          MTRESTE,GMTAVIC,GMTCAPIC,GMTCCC,GMTELOC,GMTLIVC, GMTRETC, GMTSALC, NBASS,NE,
                                                         
                                                         ZREVDEC,ZREVDISP,ZFONCIER, ZREVFIN, ZSALAIRES, ZRIC, ZRNC, 
                                                         ZVALM,ZVAMM,POND, RESPRIACH, MTDETTES, MTVEHIC, MTVOIT, GALGMAX, GALGMIN, NCOUPLES, MATRIPR, DONVERS, HERTABD, DIPLOMPR, DIPLOMCJ, ZIMPOM, ZCSGCRDS, ZCSGDM))



setnames(base_2014, c("ident_2014","indic_panel_2014", "sexe_conjoint_2014", "sexe_personne_reference_2014", "age_personne_reference_2014", "age_conjoint_2014", "situation_personne_reference_2014",
                      "situation_conjoint_2014", "raisons_separation_2014", "nombre_actifs_menage_2014", "patri_brut_hors_reste_2014"
                      , "pat_pro_avec_entreprise_2014", "pat_pro_hors_entreprise_2014", "pat_immo_2014", "pat_financier_2014", "pat_net_2014", "pat_net_hors_reste_2014", "pat_brut_2014",
                       "reste_2014", "assurance_vie_montant_2014", "actions_montant_2014", "compte_courant_montant_2014", "epargne_logement_montant_2014",
                      "livrets_montant_2014", "montant_livret_epargne_retraite_2014", "montant_livret_epargne_salariale_2014","nombre_assurances_vie_2014", "nombre_entreprises_2014", "revenu_declare_2014", "revenu_dispo_2014"
                      , "revenu_foncier_2014", "revenu_financier_2014", "salaires_2014", "revenu_indus_et_commerciaux_2014", "revenus_non_commerciaux_pro_2014", "revenus_capitaux_soum_PL_2014","revenus_capitaux_non_soum_PL_2014", "ponderation_2014", "valeur_acquisition_residence_principale_2014",
                      "montant_dettes_2014", "montant_vehicules_2014", "montant_voitures_2014", "montant_max_immo_hors_res_principale_2014", "montant_min_immo_hors_res_principale_2014", "nombre_couples_menage_2014", "statut_matrimonial_2014", "indic_donation_faite_2014", "indic_donation_recue_2014", "diplome_personne_reference_2014", "diplome_conjoint_2014", "ir_2014", "ps_2014", "csg_deductible_2014"))

base_2014

#source(paste(repo_prgm , "03_nettoyage.R" , sep = "/"))

#On recode certaines variables 

#1 Variables de genre (conjoint et personne de reference)

base_2014$sexe_conjoint_2014[base_2014$sexe_conjoint_2014==1]<-0
base_2014$sexe_conjoint_2014[base_2014$sexe_conjoint_2014==2]<-1 

base_2014$sexe_personne_reference_2014[base_2014$sexe_personne_reference_2014==1]<-0

base_2014$sexe_personne_reference_2014[base_2014$sexe_personne_reference_2014==2]<-1 

#2. Indicatrice d'appartenance au panel

base_2014$indic_panel_2014[base_2014$indic_panel_2014==2]<-0 

#On labelise les variables

#Label du sexe des individus du menage

base_2014$sexe_personne_reference_2014 <- factor(base_2014$sexe_personne_reference_2014, levels=c(0,1), labels= c("Homme", "Femme"))
base_2014$sexe_conjoint_2014 <- factor(base_2014$sexe_conjoint_2014, levels=c(0,1), labels= c("Homme", "Femme"))

base_2014

#Creation de variable de parts du patrimoine financier, immobilier et professionnel dans le patrimoine brut hors reste total

base_2014$part_pat_immo_2014<-(base_2014$pat_immo_2014/base_2014$patri_brut_hors_reste_2014*100)
base_2014$part_pat_financier_2014<-(base_2014$pat_financier_2014/base_2014$patri_brut_hors_reste_2014*100)
base_2014$part_pat_pro_avec_entreprise_2014<-(base_2014$pat_pro_avec_entreprise_2014/base_2014$patri_brut_hors_reste_2014*100)



################################################################################
#            3.2. NETTOYAGE DE LA BASE PRODUITS 2014           -------------------------------
################################################################################


base_produit_2014<-data_frame(produit2014_brut)
base_produit_2014<- subset.data.frame(produit2014_brut, select=c(IDENTPOS, IDENT, IDENTPROD, FINNA, LOGNA, PROFNA, BATRP, GMTCTITC, 
                                                                 LOGPRIACH, MONTCLA, MONTCLA_CORR, PARMEN, NATURE, NATPRO, POND, DETQUA, KDU_CORR, DETPMEN ))



setnames(base_produit_2014, c("ident_pos_2014", "ident_2014","identifiant_produit_2014", "nature_actif_2014", "nature_actif_immobilier_2014", "nature_actif_professionnel_2014", 
                              "batiment_pro_contenant_residence_principale_2014", "montant_comptes_titres_2014", "valeur_acquisition_logement_2014", "montant_capital_2014", "montant_capital_corr_2014"
                              , "part_propriete_menage_2014", "nature_produit_2014", "nature_et_usage_actif_pro_2014", "ponderation_produit_2014", "nature_dettes_2014", "capital_restant_du_2014", "part_menage_emprunt_pro_2014"))



################################################################################
#                    3.3. CREATION D'UNE SOUS-BASE CONTENANT                   #
#             UNIQUEMENT LES MENAGES AYANT DES ACTIFS PRO EXPLOITES            #  
#                         PROFESSIONNELLEMENT EN 2014                          #
################################################################################


#Creation d'une sous base contenant uniquement les menages ayant des actifs pro exploités professionellement en 2014

base_actifs_pro_2014<-base_produit_2014[base_produit_2014$nature_et_usage_actif_pro_2014==1 |base_produit_2014$nature_et_usage_actif_pro_2014==3,]


is.na(base_actifs_pro_2014$nature_et_usage_actif_pro_2014)

base_actifs_pro_2014<-base_actifs_pro_2014[!is.na(base_actifs_pro_2014$nature_et_usage_actif_pro_2014),]

table(base_actifs_pro_2014$ident_2014)


base_actifs_pro_2014$valeur_actif_pro_exploite_finale_2014<-(base_actifs_pro_2014$part_propriete_menage_2014/100*base_actifs_pro_2014$montant_capital_2014)

base_actifs_pro_exploite_synthese_2014<-aggregate(x = base_actifs_pro_2014$valeur_actif_pro_exploite_finale_2014 , by = list(base_actifs_pro_2014$ident_2014), FUN = sum)  
base_actifs_pro_exploite_synthese_2014<-data.frame(base_actifs_pro_exploite_synthese_2014)

setnames(base_actifs_pro_exploite_synthese_2014, c("ident_2014", "montant_actifs_pro_exploites_pro_2014"))
table(base_actifs_pro_exploite_synthese_2014$ident_2014)

base_2014<-dplyr::right_join(base_actifs_pro_exploite_synthese_2014, base_2014, by="ident_2014")

#Adaptation pour ceux rentrant dans l'ifi (juste les actifs pro exploités pro immobiliers)

base_actifs_pro_exploites_ifi_2014<-base_actifs_pro_2014[base_actifs_pro_2014$nature_produit_2014==2,]

#base_actifs_pro_exploite_synthese_IFI_2014<-aggregate(x = base_actifs_pro_exploites_ifi_2014$valeur_actif_pro_exploite_finale_2014 , by = list(base_actifs_pro_exploites_ifi_2014$ident_2014), FUN = sum)  
#base_actifs_pro_exploite_synthese_IFI_2014<-data.frame(base_actifs_pro_exploite_synthese_IFI_2014)

#setnames(base_actifs_pro_exploite_synthese_IFI_2014, c("ident_2014", "montant_actifs_pro_exploites_ifi_2014"))

#base_2014<-dplyr::right_join(base_actifs_pro_exploite_synthese_2014, base_2014, by="ident_2014")


################################################################################
#                    3.3. CREATION D'UNE SOUS-BASE CONTENANT                   #
#             UNIQUEMENT LES MENAGES AYANT DES DETTES EXONERABLES D'IFI        #  
#                                     EN 2014                                  #
################################################################################


#Creation d'une sous base contenant uniquement les menages ayant des actifs pro exploités professionellement en 2014


base_produit_2014$nature_dettes_2014<-as.numeric(base_produit_2014$nature_dettes_2014)
base_produit_2014$nature_dettes_2014[base_produit_2014$nature_dettes_2014==01]<-1 
base_produit_2014$nature_dettes_2014[base_produit_2014$nature_dettes_2014==02]<-2 
base_produit_2014$nature_dettes_2014[base_produit_2014$nature_dettes_2014==03]<-3 
base_produit_2014$nature_dettes_2014[base_produit_2014$nature_dettes_2014==09]<-9 


base_dettes_exonerables_ifi_2014<-base_produit_2014[base_produit_2014$nature_dettes_2014==1 |base_produit_2014$nature_dettes_2014==2 |base_produit_2014$nature_dettes_2014==3 |base_produit_2014$nature_dettes_2014==10 |base_produit_2014$nature_dettes_2014==16 |base_produit_2014$nature_dettes_2014==9 , ]

table(base_dettes_exonerables_ifi_2014$nature_dettes_2014)

is.na(base_dettes_exonerables_ifi_2014$nature_dettes_2014)

base_dettes_exonerables_ifi_2014<-base_dettes_exonerables_ifi_2014[!is.na(base_dettes_exonerables_ifi_2014$nature_dettes_2014),]

table(base_dettes_exonerables_ifi_2014$nature_dettes_2014)


base_dettes_exonerables_ifi_2014<-aggregate(x = base_dettes_exonerables_ifi_2014$capital_restant_du_2014 , by = list(base_dettes_exonerables_ifi_2014$ident_2014), FUN = sum)  
base_dettes_exonerables_ifi_2014<-data.frame(base_dettes_exonerables_ifi_2014)

setnames(base_dettes_exonerables_ifi_2014, c("ident_2014", "dettes_deductibles_ifi_2014"))
table(base_dettes_exonerables_ifi_2014$ident_2014)

base_2014<-dplyr::right_join(base_dettes_exonerables_ifi_2014, base_2014, by="ident_2014")

################################################################################
#            3.2. NETTOYAGE DE LA BASE ENTREPRISES 2014           -------------------------------
################################################################################


base_entreprise_2014<-data_frame(entreprise2014_brut)
base_entreprise_2014<- subset.data.frame(base_entreprise_2014, select=c(IDENT, INST, MTENTREP ))

setnames(base_entreprise_2014, c("ident_2014","mode_acquisition_entreprise_2014", "valeur_entreprise_2014"))

base_actifs_dutreil_2014<-base_entreprise_2014[base_entreprise_2014$mode_acquisition_entreprise_2014==2, ]

is.na(base_actifs_dutreil_2014$mode_acquisition_entreprise_2014)

base_actifs_dutreil_2014<-base_actifs_dutreil_2014[!is.na(base_actifs_dutreil_2014$mode_acquisition_entreprise_2014),]

table(base_actifs_dutreil_2014$mode_acquisition_entreprise_2014)

base_actifs_dutreil_2014<-aggregate(x = base_actifs_dutreil_2014$valeur_entreprise_2014 , by = list(base_actifs_dutreil_2014$ident_2014), FUN = sum)  


base_actifs_dutreil_2014<-data.frame(base_actifs_dutreil_2014)

setnames(base_actifs_dutreil_2014, c("ident_2014", "montant_actifs_dutreil_2014"))
table(base_actifs_dutreil_2014$ident_2014)

base_2014<-dplyr::right_join(base_actifs_dutreil_2014, base_2014, by="ident_2014")


################################################################################
#            3.4. NETTOYAGE DE LA BASE MENAGES 2017                            #
################################################################################

base_2017 <- subset.data.frame(menage2017_brut, select=c(IDENT,IDENTINDLPR, IDENTINDLCJ, COUPLEIDEM, COUPLEIDEM_HBS, MPANEL,MPANEL_NEXT, SEXECJ,SEXEPR,AGEPR,AGECJ,
                                                         SITUAPR,SITUACJ, FORCJVIE,
                                                         NACTIFS,PATRI_BRUT_HORSRESTE, PATRI_BRUT_HORSRESTE_CORR, PATPROFENT, PATPROFHENT,PATPROFHENT_CORR, PATIMM, 
                                                         PATFISOM,PATRI_NET,PATRI_NET_CORR, PATRI_NET_HORSRESTE,PATRI_NET_HORSRESTE_CORR, PATRI_BRUT, PATRI_BRUT_CORR,
                                                         MTRESTE,GMTAVIC,GMTCAPIC,GMTCCC,GMTELOC,GMTLIVC,GMTRETC, GMTSALC,NBASS,NE,
                                                         
                                                         ZREVDEC,ZREVDISP,ZFONCIER, ZREVFIN, ZSALAIRES, ZRIC, ZRNC, 
                                                         ZVALM,ZVAMM,POND_TRANS, RESPRIACH, MTDETTES, MTVEHIC, MTVOIT, GALGMAX, GALGMIN, NCOUPLES, MATRIPR, DONVERS, HERTABD, DIPLOMPR, DIPLOMCJ, ZIMPOM, ZCSGCRDS, ZCSGDM))


setnames(base_2017, c("ident_2017", "identifiant_longitudinal_personnne_reference_2017","identifiant_longitudinal_conjoint_2017","couple_stable_2017", "couple_stable_hors_budget_partage_2017","indic_panel_2017","indic_panel_prochaine_vague", "sexe_conjoint_2017", "sexe_personne_reference_2017", "age_personne_reference_2017", "age_conjoint_2017", "situation_personne_reference_2017",
                      "situation_conjoint_2017", "raisons_separation_2017", "nombre_actifs_menage_2017", "patri_brut_hors_reste_2017","patri_brut_hors_reste_corr_2017"
                      , "pat_pro_avec_entreprise_2017", "pat_pro_hors_entreprise_2017","pat_pro_hors_entreprise_corr_2017", "pat_immo_2017", "pat_financier_2017", "pat_net_2017", "pat_net_corr_2017", "pat_net_hors_reste_2017","pat_net_hors_reste_corr_2017", "pat_brut_2017","pat_brut_corr_2017",
                      "reste_2017", "assurance_vie_montant_2017", "actions_montant_2017", "compte_courant_montant_2017", "epargne_logement_montant_2017",
                      "livrets_montant_2017","montant_livret_epargne_retraite_2017", "montant_livret_epargne_salariale_2017", "nombre_assurances_vie_2017", "nombre_entreprises_2017", "revenu_declare_2017", "revenu_dispo_2017"
                      , "revenu_foncier_2017", "revenu_financier_2017", "salaires_2017", "revenu_indus_et_commerciaux_2017", "revenus_non_commerciaux_pro_2017", "revenus_capitaux_soum_PL_2017","revenus_capitaux_non_soum_PL_2017", "ponderation_2017_trans", "valeur_acquisition_residence_principale_2017","montant_dettes_2017", "montant_vehicules_2017", "montant_voitures_2017" ,
                      "montant_max_immo_hors_res_principale_2017", "montant_min_immo_hors_res_principale_2017", "nombre_couples_menage_2017", "statut_matrimonial_2017", "indic_donnation_faite_2017", "indic_donnation_recue_2017", "diplome_personne_reference_2017", "diplome_conjoint_2017", "ir_2017", "ps_2017", "csg_deductible_2017"))

base_2017

#source(paste(repo_prgm , "03_nettoyage.R" , sep = "/"))

#On recode certaines variables 

#1 Variables de genre (conjoint et personne de reference)

base_2017$sexe_conjoint_2017[base_2017$sexe_conjoint_2017==1]<-0
base_2017$sexe_conjoint_2017[base_2017$sexe_conjoint_2017==2]<-1 

base_2017$sexe_personne_reference_2017[base_2017$sexe_personne_reference_2017==1]<-0
base_2017$sexe_personne_reference_2017[base_2017$sexe_personne_reference_2017==2]<-1 

#2. Indicatrice d'appartenance au panel

base_2017$indic_panel_2017[base_2017$indic_panel_2017==2]<-0 
base_2017$indic_panel_prochaine_vague[base_2017$indic_panel_prochaine_vague==2]<-0 

#3 Indicatrice de couple stable par rapport a l'enquete precedente

base_2017$couple_stable_2017[base_2017$couple_stable_2017==2]<-0 
base_2017$couple_stable_hors_budget_partage_2017[base_2017$couple_stable_hors_budget_partage_2017==2]<-0 



#On labelise les variables

#Label du sexe (variable par ailleurs passage en facteur)

base_2017$sexe_personne_reference_2017 <- factor(base_2017$sexe_personne_reference_2017, levels=c(0,1), labels= c("Homme", "Femme"))
base_2017$sexe_conjoint_2017 <- factor(base_2017$sexe_conjoint_2017, levels=c(0,1), labels= c("Homme", "Femme"))

base_2017

#Creation de variable de parts du patrimoine financier, immobilier et professionnel dans le patrimoine brut hors reste total

base_2017$part_pat_immo_2017<-(base_2017$pat_immo_2017/base_2017$patri_brut_hors_reste_corr_2017*100)
base_2017$part_pat_financier_2017<-(base_2017$pat_financier_2017/base_2017$patri_brut_hors_reste_corr_2017*100)
base_2017$part_pat_pro_avec_entreprise_2017<-(base_2017$pat_pro_avec_entreprise_2017/base_2017$patri_brut_hors_reste_corr_2017*100)


################################################################################
#            3.5. NETTOYAGE DE LA BASE PRODUITS 2017                           #
################################################################################

base_produit_2017<-data_frame(produit2017_brut)
base_produit_2017<- subset.data.frame(produit2017_brut, select=c(IDENTPOS, IDENT, IDENTPROD, IDENTPROD14, FINNA, LOGNA, PROFNA, BATRP, GMTCTITC, 
                                                                 LOGPRIACH, MONTCLA, MONTCLA_CORR, PARMEN, NATURE, NATPRO, POND_TRANS, DETQUA, KDU_CORR, DETPMEN ))



setnames(base_produit_2017, c("ident_pos_2017", "ident_2017","identifiant_produit_2017","identifiant_produit_2014", "nature_actif_2017", "nature_actif_immobilier_2017", "nature_actif_professionnel_2017", 
                              "batiment_pro_contenant_residence_principale_2017", "montant_comptes_titres_2017", "valeur_acquisition_logement_2017", "montant_capital_2017", "montant_capital_corr_2017"
                              , "part_propriete_menage_2017", "nature_produit_2017", "nature_et_usage_actif_pro_2017", "ponderation_produit_2017", "nature_dettes_2017", "capital_restant_du_2017", "part_menage_emprunt_pro_2017"))



################################################################################
#                    3.6. CREATION D'UNE SOUS-BASE CONTENANT                   #
#             UNIQUEMENT LES MENAGES AYANT DES ACTIFS PRO EXPLOITES            #  
#                         PROFESSIONNELLEMENT EN 2017                          #
################################################################################
base_actifs_pro_2017<-base_produit_2017[base_produit_2017$nature_et_usage_actif_pro_2017==1 | base_produit_2017$nature_et_usage_actif_pro_2017==3,]


is.na(base_actifs_pro_2017$nature_et_usage_actif_pro_2017)

base_actifs_pro_2017<-base_actifs_pro_2017[!is.na(base_actifs_pro_2017$nature_et_usage_actif_pro_2017),]

table(base_actifs_pro_2017$ident_2017)


base_actifs_pro_2017$valeur_actif_pro_exploite_finale_2017<-(base_actifs_pro_2017$part_propriete_menage_2017/100*base_actifs_pro_2017$montant_capital_2017)

base_actifs_pro_exploite_synthese_2017<-aggregate(x = base_actifs_pro_2017$valeur_actif_pro_exploite_finale_2017 , by = list(base_actifs_pro_2017$ident_2017), FUN = sum)  
base_actifs_pro_exploite_synthese_2017<-data.frame(base_actifs_pro_exploite_synthese_2017)

setnames(base_actifs_pro_exploite_synthese_2017, c("ident_2017", "montant_actifs_pro_exploites_pro_2017"))
table(base_actifs_pro_exploite_synthese_2017$ident_2017)

base_2017<-dplyr::right_join(base_actifs_pro_exploite_synthese_2017, base_2017, by="ident_2017")

#Adaptation pour ceux rentrant dans l'ifi (juste les actifs pro exploités pro immobiliers)

base_actifs_pro_exploites_ifi_2017<-base_actifs_pro_2017[base_actifs_pro_2017$nature_produit_2017==2,]

#on grise la suite car pas de lignes à agréger

#base_actifs_pro_exploite_synthese_IFI_2017<-aggregate(x = base_actifs_pro_exploites_ifi_2017$valeur_actif_pro_exploite_finale_2017 , by = list(base_actifs_pro_exploites_ifi_2017$ident_2017), FUN = sum)  
#base_actifs_pro_exploite_synthese_IFI_2017<-data.frame(base_actifs_pro_exploite_synthese_IFI_2017)

#setnames(base_actifs_pro_exploite_synthese_IFI_2017, c("ident_2017", "montant_actifs_pro_exploites_ifi_2017"))

#base_2017<-dplyr::right_join(base_actifs_pro_exploite_synthese_2017, base_2017, by="ident_2017")

################################################################################
#                    3.3. CREATION D'UNE SOUS-BASE CONTENANT                   #
#             UNIQUEMENT LES MENAGES AYANT DES DETTES EXONERABLES D'IFI        #  
#                                     EN 2017                                  #
################################################################################


#Creation d'une sous base contenant uniquement les menages ayant des actifs pro exploités professionellement en 2014


base_produit_2017$nature_dettes_2017<-as.numeric(base_produit_2017$nature_dettes_2017)
base_produit_2017$nature_dettes_2017[base_produit_2017$nature_dettes_2017==01]<-1 
base_produit_2017$nature_dettes_2017[base_produit_2017$nature_dettes_2017==02]<-2 
base_produit_2017$nature_dettes_2017[base_produit_2017$nature_dettes_2017==03]<-3 
base_produit_2017$nature_dettes_2017[base_produit_2017$nature_dettes_2017==09]<-9 


base_dettes_exonerables_ifi_2017<-base_produit_2017[base_produit_2017$nature_dettes_2017==1 |base_produit_2017$nature_dettes_2017==2 |base_produit_2017$nature_dettes_2017==3 |base_produit_2017$nature_dettes_2017==10 |base_produit_2017$nature_dettes_2017==16 |base_produit_2017$nature_dettes_2017==9 , ]

table(base_dettes_exonerables_ifi_2017$nature_dettes_2017)

is.na(base_dettes_exonerables_ifi_2017$nature_dettes_2017)

base_dettes_exonerables_ifi_2017<-base_dettes_exonerables_ifi_2017[!is.na(base_dettes_exonerables_ifi_2017$nature_dettes_2017),]

table(base_dettes_exonerables_ifi_2017$nature_dettes_2017)



base_dettes_exonerables_ifi_2017<-aggregate(x = base_dettes_exonerables_ifi_2017$capital_restant_du_2017 , by = list(base_dettes_exonerables_ifi_2017$ident_2017), FUN = sum)  
base_dettes_exonerables_ifi_2017<-data.frame(base_dettes_exonerables_ifi_2017)

setnames(base_dettes_exonerables_ifi_2017, c("ident_2017", "dettes_deductibles_ifi_2017"))
table(base_dettes_exonerables_ifi_2017$ident_2017)

base_2017<-dplyr::right_join(base_dettes_exonerables_ifi_2017, base_2017, by="ident_2017")


################################################################################
#            3.2. NETTOYAGE DE LA BASE ENTREPRISES 2017         -------------------------------
################################################################################


base_entreprise_2017<-data_frame(entreprise2017_brut)
base_entreprise_2017<- subset.data.frame(base_entreprise_2017, select=c(IDENT, INST, MTENTREP ))

setnames(base_entreprise_2017, c("ident_2017","mode_acquisition_entreprise_2017", "valeur_entreprise_2017"))

base_actifs_dutreil_2017<-base_entreprise_2017[base_entreprise_2017$mode_acquisition_entreprise_2017==2, ]

is.na(base_actifs_dutreil_2017$mode_acquisition_entreprise_2017)

base_actifs_dutreil_2017<-base_actifs_dutreil_2017[!is.na(base_actifs_dutreil_2017$mode_acquisition_entreprise_2017),]

table(base_actifs_dutreil_2017$mode_acquisition_entreprise_2017)

base_actifs_dutreil_2017<-aggregate(x = base_actifs_dutreil_2017$valeur_entreprise_2017 , by = list(base_actifs_dutreil_2017$ident_2017), FUN = sum)  


base_actifs_dutreil_2017<-data.frame(base_actifs_dutreil_2017)

setnames(base_actifs_dutreil_2017, c("ident_2017", "montant_actifs_dutreil_2017"))
table(base_actifs_dutreil_2017$ident_2017)

base_2017<-dplyr::right_join(base_actifs_dutreil_2017, base_2017, by="ident_2017")



################################################################################
#            3.4. NETTOYAGE DE LA BASE MENAGES 2021                            #
################################################################################

base_2021 <- subset.data.frame(menage2021_brut, select=c(IDENT,IDENTINDLPR, IDENTINDLCJ, COUPLEIDEM, COUPLEIDEM_HBS, MPANEL,MPANEL_NEXT, SEXECJ,SEXEPR,AGEPR,AGECJ,
                                                         SITUAPR,SITUACJ, FORCJVIE,
                                                         NACTIFS, PATRI_BRUT_HORSRESTE, PATPROFENT,PATPROFHENT, PATIMM, 
                                                         PATFISOM,PATRI_NET, PATRI_NET_HORSRESTE, PATRI_BRUT,
                                                         MTRESTE,GMTAVIC,GMTCCC,GMTELOC,GMTLIVC,GMTRETC, GMTSALC,NBASS,NE,
                                                         
                                                         ZREVDEC,ZREVDISP,ZFONCIER, ZREVFIN, ZSALAIRES, ZRIC, ZRNC, 
                                                         ZVALM,ZVAMM,POND_TRANS, RESPRIACH, MTDETTES, MTVEHIC, MTVOIT, GALGMAX, GALGMIN, NCOUPLES, MATRIPR, DONVERS, HERTABD, DIPLOMPR, DIPLOMCJ, ZIMPOM, ZCSGCRDS, ZCSGDM))


setnames(base_2021, c("ident_2021", "identifiant_longitudinal_personnne_reference_2021","identifiant_longitudinal_conjoint_2021","couple_stable_2021", "couple_stable_hors_budget_partage_2021","indic_panel_2021","indic_panel_prochaine_vague", "sexe_conjoint_2021", "sexe_personne_reference_2021", "age_personne_reference_2021", "age_conjoint_2021", "situation_personne_reference_2021",
                      "situation_conjoint_2021", "raisons_separation_2021", "nombre_actifs_menage_2021","patri_brut_hors_reste_2021"
                      , "pat_pro_avec_entreprise_2021", "pat_pro_hors_entreprise_2021", "pat_immo_2021", "pat_financier_2021", "pat_net_2021", "pat_net_hors_reste_2021", "pat_brut_2021",
                      "reste_2021", "assurance_vie_montant_2021", "compte_courant_montant_2021", "epargne_logement_montant_2021",
                      "livrets_montant_2021","montant_livret_epargne_retraite_2021", "montant_livret_epargne_salariale_2021", "nombre_assurances_vie_2021", "nombre_entreprises_2021", "revenu_declare_2021", "revenu_dispo_2021"
                      , "revenu_foncier_2021", "revenu_financier_2021", "salaires_2021", "revenu_indus_et_commerciaux_2021", "revenus_non_commerciaux_pro_2021", "revenus_capitaux_soum_PL_2021","revenus_capitaux_non_soum_PL_2021", "ponderation_2021_trans", "valeur_acquisition_residence_principale_2021","montant_dettes_2021", "montant_vehicules_2021", "montant_voitures_2021" ,
                      "montant_max_immo_hors_res_principale_2021", "montant_min_immo_hors_res_principale_2021", "nombre_couples_menage_2021", "statut_matrimonial_2021", "indic_donnation_faite_2021", "indic_donnation_recue_2021", "diplome_personne_reference_2021", "diplome_conjoint_2021", "ir_2021", "ps_2021", "csg_deductible_2021" ))

base_2021

#source(paste(repo_prgm , "03_nettoyage.R" , sep = "/"))

#On recode certaines variables 

#1 Variables de genre (conjoint et personne de reference)

base_2021$sexe_conjoint_2021[base_2021$sexe_conjoint_2021==1]<-0
base_2021$sexe_conjoint_2021[base_2021$sexe_conjoint_2021==2]<-1 

base_2021$sexe_personne_reference_2021[base_2021$sexe_personne_reference_2021==1]<-0
base_2021$sexe_personne_reference_2021[base_2021$sexe_personne_reference_2021==2]<-1 

#2. Indicatrice d'appartenance au panel

base_2021$indic_panel_2021[base_2021$indic_panel_2021==2]<-0 
base_2021$indic_panel_prochaine_vague[base_2021$indic_panel_prochaine_vague==2]<-0 

#3 Indicatrice de couple stable par rapport a l'enquete precedente

base_2021$couple_stable_2021[base_2021$couple_stable_2021==2]<-0 
base_2021$couple_stable_hors_budget_partage_2021[base_2021$couple_stable_hors_budget_partage_2021==2]<-0 



#On labelise les variables

#Label du sexe (variable par ailleurs passage en facteur)

base_2021$sexe_personne_reference_2021 <- factor(base_2021$sexe_personne_reference_2021, levels=c(0,1), labels= c("Homme", "Femme"))
base_2021$sexe_conjoint_2021 <- factor(base_2021$sexe_conjoint_2021, levels=c(0,1), labels= c("Homme", "Femme"))

base_2021

#Creation de variable de parts du patrimoine financier, immobilier et professionnel dans le patrimoine brut hors reste total

base_2021$part_pat_immo_2021<-(base_2021$pat_immo_2021/base_2021$patri_brut_hors_reste_2021*100)
base_2021$part_pat_financier_2021<-(base_2021$pat_financier_2021/base_2021$patri_brut_hors_reste_2021*100)
base_2021$part_pat_pro_avec_entreprise_2021<-(base_2021$pat_pro_avec_entreprise_2021/base_2021$patri_brut_hors_reste_2021*100)


################################################################################
#            3.5. NETTOYAGE DE LA BASE PRODUITS 2021                           #
################################################################################

base_produit_2021<-data_frame(produit2021_brut)
base_produit_2021<- subset.data.frame(produit2021_brut, select=c(IDENTPOS, IDENT, IDENTPROD, FINNA, LOGNA, PROFNA, BATRP, GMTCTITC, 
                                                                 LOGPRIACH, MONTCLA, MONTCLA_CORR, PARMEN, NATURE, NATPRO, DETQUA, KDU_CORR, DETPMEN ))



setnames(base_produit_2021, c("ident_pos_2021", "ident_2021","identifiant_produit_2021", "nature_actif_2021", "nature_actif_immobilier_2021", "nature_actif_professionnel_2021", 
                              "batiment_pro_contenant_residence_principale_2021", "montant_comptes_titres_2021", "valeur_acquisition_logement_2021", "montant_capital_2021", "montant_capital_corr_2021"
                              , "part_propriete_menage_2021", "nature_produit_2021", "nature_et_usage_actif_pro_2021", "nature_dettes_2021", "capital_restant_du_2021", "part_menage_emprunt_pro_2021"))



################################################################################
#                    3.6. CREATION D'UNE SOUS-BASE CONTENANT                   #
#             UNIQUEMENT LES MENAGES AYANT DES ACTIFS PRO EXPLOITES            #  
#                         PROFESSIONNELLEMENT EN 2021                          #
################################################################################
base_actifs_pro_2021<-base_produit_2021[base_produit_2021$nature_et_usage_actif_pro_2021==1 | base_produit_2021$nature_et_usage_actif_pro_2021==3,]


is.na(base_actifs_pro_2021$nature_et_usage_actif_pro_2021)

base_actifs_pro_2021<-base_actifs_pro_2021[!is.na(base_actifs_pro_2021$nature_et_usage_actif_pro_2021),]

table(base_actifs_pro_2021$ident_2021)


base_actifs_pro_2021$valeur_actif_pro_exploite_finale_2021<-(base_actifs_pro_2021$part_propriete_menage_2021/100*base_actifs_pro_2021$montant_capital_2021)

base_actifs_pro_exploite_synthese_2021<-aggregate(x = base_actifs_pro_2021$valeur_actif_pro_exploite_finale_2021 , by = list(base_actifs_pro_2021$ident_2021), FUN = sum)  
base_actifs_pro_exploite_synthese_2021<-data.frame(base_actifs_pro_exploite_synthese_2021)

setnames(base_actifs_pro_exploite_synthese_2021, c("ident_2021", "montant_actifs_pro_exploites_pro_2021"))
table(base_actifs_pro_exploite_synthese_2021$ident_2021)

base_2021<-dplyr::right_join(base_actifs_pro_exploite_synthese_2021, base_2021, by="ident_2021")


################################################################################
#                    3.3. CREATION D'UNE SOUS-BASE CONTENANT                   #
#             UNIQUEMENT LES MENAGES AYANT DES DETTES EXONERABLES D'IFI        #  
#                                     EN 2021                                  #
################################################################################


#Creation d'une sous base contenant uniquement les menages ayant des actifs pro exploités professionellement en 2014


base_produit_2021$nature_dettes_2021<-as.numeric(base_produit_2021$nature_dettes_2021)
base_produit_2021$nature_dettes_2021[base_produit_2021$nature_dettes_2021==01]<-1 
base_produit_2021$nature_dettes_2021[base_produit_2021$nature_dettes_2021==02]<-2 
base_produit_2021$nature_dettes_2021[base_produit_2021$nature_dettes_2021==03]<-3 
base_produit_2021$nature_dettes_2021[base_produit_2021$nature_dettes_2021==09]<-9 


base_dettes_exonerables_ifi_2021<-base_produit_2021[base_produit_2021$nature_dettes_2021==1 |base_produit_2021$nature_dettes_2021==2 |base_produit_2021$nature_dettes_2021==3 |base_produit_2021$nature_dettes_2021==10 |base_produit_2021$nature_dettes_2021==16 |base_produit_2021$nature_dettes_2021==9 , ]

table(base_dettes_exonerables_ifi_2021$nature_dettes_2021)

is.na(base_dettes_exonerables_ifi_2021$nature_dettes_2021)

base_dettes_exonerables_ifi_2021<-base_dettes_exonerables_ifi_2021[!is.na(base_dettes_exonerables_ifi_2021$nature_dettes_2021),]

table(base_dettes_exonerables_ifi_2021$nature_dettes_2021)



base_dettes_exonerables_ifi_2021<-aggregate(x = base_dettes_exonerables_ifi_2021$capital_restant_du_2021 , by = list(base_dettes_exonerables_ifi_2021$ident_2021), FUN = sum)  
base_dettes_exonerables_ifi_2021<-data.frame(base_dettes_exonerables_ifi_2021)

setnames(base_dettes_exonerables_ifi_2021, c("ident_2021", "dettes_deductibles_ifi_2021"))
table(base_dettes_exonerables_ifi_2021$ident_2021)

base_2021<-dplyr::right_join(base_dettes_exonerables_ifi_2021, base_2021, by="ident_2021")



################################################################################
#            3.2. NETTOYAGE DE LA BASE ENTREPRISES 2021           -------------------------------
################################################################################


base_entreprise_2021<-data_frame(entreprise2021_brut)
base_entreprise_2021<- subset.data.frame(base_entreprise_2021, select=c(IDENT, INST, MTENTREP ))

setnames(base_entreprise_2021, c("ident_2021","mode_acquisition_entreprise_2021", "valeur_entreprise_2021"))

base_actifs_dutreil_2021<-base_entreprise_2021[base_entreprise_2021$mode_acquisition_entreprise_2021==2, ]

is.na(base_actifs_dutreil_2021$mode_acquisition_entreprise_2021)

base_actifs_dutreil_2021<-base_actifs_dutreil_2021[!is.na(base_actifs_dutreil_2021$mode_acquisition_entreprise_2021),]

table(base_actifs_dutreil_2021$mode_acquisition_entreprise_2021)

base_actifs_dutreil_2021<-aggregate(x = base_actifs_dutreil_2021$valeur_entreprise_2021 , by = list(base_actifs_dutreil_2021$ident_2021), FUN = sum)  

base_actifs_dutreil_2021<-data.frame(base_actifs_dutreil_2021)

setnames(base_actifs_dutreil_2021, c("ident_2021", "montant_actifs_dutreil_2021"))
table(base_actifs_dutreil_2021$ident_2021)

base_2021<-dplyr::right_join(base_actifs_dutreil_2021, base_2021, by="ident_2021")




###########################################################################################################
#                    3.7. CREATION DE BASES DE TOP 1 ET TOP 0.5% EN 2014 ET 2017                          #
###########################################################################################################


#2014


#Determinantion des seuils pour appartenir aux top 10% et top 1%

dwt2014 <- svydesign(ids = ~1, data = base_2014, weights = ~ base_2014$ponderation)

svyquantile(~patri_brut_hors_reste_2014,dwt2014, quantiles=0.995, ci=FALSE)
svyquantile(~patri_brut_hors_reste_2014,dwt2014, quantiles=0.99, ci=FALSE)

svyquantile(~pat_immo_2014,dwt2014, quantiles=0.995, ci=FALSE)
svyquantile(~pat_immo_2014,dwt2014, quantiles=0.99, ci=FALSE)

svyquantile(~pat_financier_2014,dwt2014, quantiles=0.995, ci=FALSE)
svyquantile(~pat_financier_2014,dwt2014, quantiles=0.99, ci=FALSE)

svyquantile(~pat_pro_avec_entreprise_2014,dwt2014, quantiles=0.995, ci=FALSE)
svyquantile(~pat_pro_avec_entreprise_2014,dwt2014, quantiles=0.99, ci=FALSE)

#On cree une une indicatrice d'appartenance au top 1% et au top 10% de patrimoine immobilier, financier, professionnel et brut 

base_2014$indic_top1_immo_2014  <- 0
base_2014$indic_top1_immo_2014[base_2014$pat_immo_2014 > 1023154] <- 1

base_2014$indic_top1_financier_2014  <- 0
base_2014$indic_top1_financier_2014[base_2014$pat_financier_2014 > 578333] <- 1

base_2014$indic_top1_pro_2014  <- 0
base_2014$indic_top1_pro_avec_entreprise_2014[base_2014$pat_pro_avec_entreprise_2014 > 453527] <- 1

base_2014$indic_top1_brut_corr_2014  <- 0
base_2014$indic_top1_brut_corr_2014[base_2014$patri_brut_hors_reste_2014 > 1841849] <- 1

base_2014$indic_top05_immo_2014  <- 0
base_2014$indic_top05_immo_2014[base_2014$pat_immo_2014 > 1350788] <- 1

base_2014$indic_top05_financier_2014  <- 0
base_2014$indic_top05_financier_2014[base_2014$pat_financier_2014 > 947259] <- 1

base_2014$indic_top05_pro_2014  <- 0
base_2014$indic_top05_pro_2014[base_2014$pat_pro_avec_entreprise_2014 > 678968] <- 1

base_2014$indic_top05_brut_corr_2014  <- 0
base_2014$indic_top05_brut_corr_2014[base_2014$patri_brut_hors_reste_2014 > 2541633] <- 1

#On cree une sous-base pour le top 10% et 1% de patrimoine immobilier, financier, professionnel et brut 

base_2014_top05_immo<-base_2014[base_2014$pat_immo_2014>1350788,]
base_2014_top05_financier<-base_2014[base_2014$pat_financier_2014>947259,]
base_2014_top05_pro<-base_2014[base_2014$pat_pro_avec_entreprise_2014>678968,]
base_2014_top05_brut<-base_2014[base_2014$patri_brut_hors_reste_2014>2541633,]

base_2014_top1_immo<-base_2014[base_2014$pat_immo_2014>1023154,]
base_2014_top1_financier<-base_2014[base_2014$pat_financier_2014>578333,]
base_2014_top1_pro<-base_2014[base_2014$pat_pro_avec_entreprise_2014>453527,]
base_2014_top1_brut<-base_2014[base_2014$patri_brut_hors_reste_2014>1841849,]

#2017

#On cree une une indicatrice d'appartenance au top 1% et au top 10% de patrimoine immobilier, financier, professionnel et brut 

#On considere la base ponderee et calculons les seuils des top 1 et 10%

dwt2017 <- svydesign(ids = ~1, data = base_2017, weights = ~ base_2017$ponderation_2017_trans)

svyquantile(~patri_brut_hors_reste_corr_2017,dwt2017, quantiles=0.995, ci=FALSE)
svyquantile(~patri_brut_hors_reste_corr_2017,dwt2017, quantiles=0.99, ci=FALSE)

svyquantile(~pat_immo_2017,dwt2017, quantiles=0.995, ci=FALSE)
svyquantile(~pat_immo_2017,dwt2017, quantiles=0.99, ci=FALSE)

svyquantile(~pat_financier_2017,dwt2017, quantiles=0.995, ci=FALSE)
svyquantile(~pat_financier_2017,dwt2017, quantiles=0.99, ci=FALSE)

svyquantile(~pat_pro_avec_entreprise_2017,dwt2017, quantiles=0.995, ci=FALSE)
svyquantile(~pat_pro_avec_entreprise_2017,dwt2017, quantiles=0.99, ci=FALSE)

#Creation d'indicatrices d'appartenance aux tops 1 et 10%

base_2017$indic_top1_immo_2017  <- 0
base_2017$indic_top1_immo_2017[base_2017$pat_immo_2017 > 1078310] <- 1

base_2017$indic_top1_financier_2017  <- 0
base_2017$indic_top1_financier_2017[base_2017$pat_financier_2017 > 573665] <- 1

base_2017$indic_top1_pro_2017  <- 0
base_2017$indic_top1_pro_2017[base_2017$pat_pro_avec_entreprise_2017 > 486583] <- 1

base_2017$indic_top1_brut_corr_2017  <- 0
base_2017$indic_top1_brut_corr_2017[base_2017$patri_brut_hors_reste_corr_2017 > 1861777] <- 1

base_2017$indic_top05_immo_2017  <- 0
base_2017$indic_top05_immo_2017[base_2017$pat_immo_2017 > 1387398] <- 1

base_2017$indic_top05_financier_2017  <- 0
base_2017$indic_top05_financier_2017[base_2017$pat_financier_2017 > 934673] <- 1

base_2017$indic_top05_pro_2017  <- 0
base_2017$indic_top05_pro_2017[base_2017$pat_pro_avec_entreprise_2017 > 806000] <- 1

base_2017$indic_top05_brut_2017  <- 0
base_2017$indic_top05_brut_2017[base_2017$patri_brut_hors_reste_corr_2017 > 2756466] <- 1

#On cree une sous-base pour le top 10% et 1% de patrimoine immobilier, financier, professionnel et brut 

base_2017_top05_immo<-base_2017[base_2017$pat_immo_2017>1387398,]
base_2017_top05_financier<-base_2017[base_2017$pat_financier_2017>934673,]
base_2017_top05_pro<-base_2017[base_2017$pat_pro_avec_entreprise_2017>806000,]
base_2017_top05_brut<-base_2017[base_2017$patri_brut_hors_reste_corr_2017>2756466,]

base_2017_top1_immo<-base_2017[base_2017$pat_immo_2017>1078310,]
base_2017_top1_financier<-base_2017[base_2017$pat_financier_2017>573665,]
base_2017_top1_pro<-base_2017[base_2017$pat_pro_avec_entreprise_2017>486583,]
base_2017_top1_brut<-base_2017[base_2017$patri_brut_hors_reste_corr_2017>1861777,]

#2021


#On cree une une indicatrice d'appartenance au top 1% et au top 10% de patrimoine immobilier, financier, professionnel et brut 

#On considere la base ponderee et calculons les seuils des top 1 et 10%

dwt2021 <- svydesign(ids = ~1, data = base_2021, weights = ~ base_2021$ponderation_2021_trans)

svyquantile(~patri_brut_hors_reste_2021,dwt2021, quantiles=0.995, ci=FALSE)
svyquantile(~patri_brut_hors_reste_2021,dwt2021, quantiles=0.99, ci=FALSE)

svyquantile(~pat_immo_2021,dwt2021, quantiles=0.995, ci=FALSE)
svyquantile(~pat_immo_2021,dwt2021, quantiles=0.99, ci=FALSE)

svyquantile(~pat_financier_2021,dwt2021, quantiles=0.995, ci=FALSE)
svyquantile(~pat_financier_2021,dwt2021, quantiles=0.99, ci=FALSE)

svyquantile(~pat_pro_avec_entreprise_2021,dwt2021, quantiles=0.995, ci=FALSE)
svyquantile(~pat_pro_avec_entreprise_2021,dwt2021, quantiles=0.99, ci=FALSE)

#Creation d'indicatrices d'appartenance aux tops 1 et 0,5%

base_2021$indic_top1_immo_2021  <- 0
base_2021$indic_top1_immo_2021[base_2021$pat_immo_2021 > 1294797] <- 1

base_2021$indic_top1_financier_2021  <- 0
base_2021$indic_top1_financier_2021[base_2021$pat_financier_2021 > 681791] <- 1

base_2021$indic_top1_pro_2021  <- 0
base_2021$indic_top1_pro_2021[base_2021$pat_pro_avec_entreprise_2021 > 518443] <- 1

base_2021$indic_top1_brut_corr_2021  <- 0
base_2021$indic_top1_brut_corr_2021[base_2021$patri_brut_hors_reste_2021 > 2175919] <- 1

base_2021$indic_top05_immo_2021  <- 0
base_2021$indic_top05_immo_2021[base_2021$pat_immo_2021 > 1794548] <- 1

base_2021$indic_top05_financier_2021  <- 0
base_2021$indic_top05_financier_2021[base_2021$pat_financier_2021 > 1025149] <- 1

base_2021$indic_top05_pro_2021  <- 0
base_2021$indic_top05_pro_2021[base_2021$pat_pro_avec_entreprise_2021 > 983000] <- 1

base_2021$indic_top05_brut_2021  <- 0
base_2021$indic_top05_brut_2021[base_2021$patri_brut_hors_reste_2021 > 3463276] <- 1

#On cree une sous-base pour le top 10% et 1% de patrimoine immobilier, financier, professionnel et brut 

base_2021_top05_immo<-base_2021[base_2021$pat_immo_2021>1794548,]
base_2021_top05_financier<-base_2021[base_2021$pat_financier_2021>1025149,]
base_2021_top05_pro<-base_2021[base_2021$pat_pro_avec_entreprise_2021>983000,]
base_2021_top05_brut<-base_2021[base_2021$patri_brut_hors_reste_2021>3463276,]

base_2021_top1_immo<-base_2021[base_2021$pat_immo_2021>1294797,]
base_2021_top1_financier<-base_2021[base_2021$pat_financier_2021>681791,]
base_2021_top1_pro<-base_2021[base_2021$pat_pro_avec_entreprise_2021>518443,]
base_2021_top1_brut<-base_2021[base_2021$patri_brut_hors_reste_2021>2175919,]


################################################################################
#            3.8. NETTOYAGE DE LA BASE INDIVIDUS 2014                          #
################################################################################


base_individu_2014 <- subset.data.frame(individu2014_brut, select=c(IDENT,IDENTINDL, INDIV_PANEL, RANG_PANEL, IDENTIND14, LIENPREF, LIENPRRP,NOI, SEXE,AGE,
                                                                    SITUA, ZCHOMAGE_I,ZINVALIDITE_I,ZPENALIR_I, ZRAG_I, ZRENTES_I, ZRETRAITES_I, ZRETRAITES_STRICTES_I, ZRIC_I, ZRNC_I, ZSALAIRES_I
                                                                    
                                                                    ,POND, JEFRSO))


setnames(base_individu_2014, c("ident_2014", "identifiant_longitudinal_individuel","indic_indiv_panel_2014","rang_panel_2014","identifiant_individuel_2014", "lien_avec_personne_reference_2014","lien_avec_personne_reference_recensement_2014","numero_indiv_dans_menage_2014", "sexe_indiv_2014", "age_indiv_2014", "situa_indiv_2014",
                               "revenu_chomage_indiv_2014", "revenu_invalidite_indiv_2014","revenu_pension_alim_indiv_2014", "revenu_agricole_indiv_2014", "revenu_rentes_indiv_2014", "revenu_retraite_indiv_2014", "revenu_retraite_stricte_indiv_2014", "revenu_indus_commerciaux_indiv_2014", "revenu_non_commerciaux_indiv_2014", "salaire_indiv_2014", "ponderation_finale_menage_2014", "nombre_freres_soeurs_2014"))


#On recode certaines variables 

#1 Variables de genre (conjoint et personne de reference)

base_individu_2014$sexe_indiv_2014[base_individu_2014$sexe_indiv_2014==1]<-0
base_individu_2014$sexe_indiv_2014[base_individu_2014$sexe_indiv_2014==2]<-1 

#2. Indicatrice d'appartenance au panel

base_individu_2014$indic_indiv_panel_2014[base_individu_2014$indic_indiv_panel_2014==2]<-0 


#On labelise les variables

#Label du sexe (variable par ailleurs passage en facteur)

base_individu_2014$sexe_indiv_2014 <- factor(base_individu_2014$sexe_indiv_2014, levels=c(0,1), labels= c("Homme", "Femme"))

base_individu_2014

################################################################################
#            3.9. NETTOYAGE DE LA BASE INDIVIDUS 2017            -------------------------------
################################################################################


base_individu_2017 <- subset.data.frame(individu2017_brut, select=c(IDENT,IDENTINDL, INDIV_PANEL, IDENTIND17, ANSORTIE_PANEL, LIENPREF, LIENPRRP,NOI, SEXE,AGE,
                                                                    SITUA, ZCHOMAGE_I,ZINVALIDITE_I,ZPENALIR_I, ZRAG_I, ZRENTES_I, ZRETRAITES_I, ZRETRAITES_STRICTES_I, ZRIC_I, ZRNC_I, ZSALAIRES_I
                                                                    
                                                                    ,POND_TRANS, POND_LONG_IND, JEFRSO))


setnames(base_individu_2017, c("ident_2017", "identifiant_longitudinal_individuel","indic_indiv_panel_2017","identifiant_individuel_2017","annee_sortie_panel", "lien_avec_personne_reference_2017","lien_avec_personne_reference_recensement_2017","numero_indiv_dans_menage_2017", "sexe_indiv_2017", "age_indiv_2017", "situa_indiv_2017",
                               "revenu_chomage_indiv_2017", "revenu_invalidite_indiv_2017","revenu_pension_alim_indiv_2017", "revenu_agricole_indiv_2017", "revenu_rentes_indiv_2017", "revenu_retraite_indiv_2017", "revenu_retraite_stricte_indiv_2017", "revenu_indus_commerciaux_indiv_2017", "revenu_non_commerciaux_indiv_2017", "salaire_indiv_2017", "ponderation_indiv_trans_2017", "ponderation_longitudinal_indiv_2017", "nombre_freres_soeurs_2017"))


#On recode certaines variables 

#1 Variables de genre (conjoint et personne de reference)

base_individu_2017$sexe_indiv_2017[base_individu_2017$sexe_indiv_2017==1]<-0
base_individu_2017$sexe_indiv_2017[base_individu_2017$sexe_indiv_2017==2]<-1 

#2. Indicatrice d'appartenance au panel

base_individu_2017$indic_indiv_panel_2017[base_individu_2017$indic_indiv_panel_2017==2]<-0 


#On labelise les variables

#Label du sexe (variable par ailleurs passage en facteur)

base_individu_2017$sexe_indiv_2017 <- factor(base_individu_2017$sexe_indiv_2017, levels=c(0,1), labels= c("Homme", "Femme"))

base_individu_2017


################################################################################
#            3.10. NETTOYAGE DE LA BASE INDIVIDUS 2021           -------------------------------
################################################################################


base_individu_2021 <- subset.data.frame(individu2021_brut, select=c(IDENT,IDENTINDL, INDIV_PANEL, IDENTIND20, ANSORTIE_PANEL, LIENPREF, LIENPRRP,NOI, SEXE,AGE,
                                                                    SITUA, ZCHOMAGE_I,ZINVALIDITE_I,ZPENALIR_I, ZRAG_I, ZRENTES_I, ZRETRAITES_I, ZRETRAITES_STRICTES_I, ZRIC_I, ZRNC_I, ZSALAIRES_I
                                                                    
                                                                    , POND_LONG_IND_2017, JEFRSO))


setnames(base_individu_2021, c("ident_2021", "identifiant_longitudinal_individuel","indic_indiv_panel_2021","identifiant_individuel_2021","annee_sortie_panel", "lien_avec_personne_reference_2021","lien_avec_personne_reference_recensement_2021","numero_indiv_dans_menage_2021", "sexe_indiv_2021", "age_indiv_2021", "situa_indiv_2021",
                               "revenu_chomage_indiv_2021", "revenu_invalidite_indiv_2021","revenu_pension_alim_indiv_2021", "revenu_agricole_indiv_2021", "revenu_rentes_indiv_2021", "revenu_retraite_indiv_2021", "revenu_retraite_stricte_indiv_2021", "revenu_indus_commerciaux_indiv_2021", "revenu_non_commerciaux_indiv_2021", "salaire_indiv_2021", "ponderation_longitudinal_indiv_2017_bis_2021", "nombre_freres_soeurs_2021"))


#On recode certaines variables 

#1 Variables de genre (conjoint et personne de reference)

base_individu_2021$sexe_indiv_2021[base_individu_2021$sexe_indiv_2021==1]<-0
base_individu_2021$sexe_indiv_2021[base_individu_2021$sexe_indiv_2021==2]<-1 

#2. Indicatrice d'appartenance au panel

base_individu_2021$indic_indiv_panel_2021[base_individu_2021$indic_indiv_panel_2021==2]<-0 


#On labelise les variables

#Label du sexe (variable par ailleurs passage en facteur)

base_individu_2021$sexe_indiv_2021 <- factor(base_individu_2021$sexe_indiv_2021, levels=c(0,1), labels= c("Homme", "Femme"))

base_individu_2021

