################################################################################
#            07. SIMULATION ISF/IFI pour DID                                   #
################################################################################
#ifelse(maCondition, actionSiVrai, actionSiFaux) 

#Bout préalable: calcul de tranches de patrimoines par patrimoine net en 2017

panel_cylindre$tranche_pat <- "a - m500k"
panel_cylindre$tranche_pat[panel_cylindre$pat_net_corr_2017>=500000 & panel_cylindre$pat_net_corr_2017 < 1000000] <- "b - 0.5-1"
panel_cylindre$tranche_pat[panel_cylindre$pat_net_corr_2017>=1000000 & panel_cylindre$pat_net_corr_2017 < 1500000] <- "c - 1-1.5"
panel_cylindre$tranche_pat[panel_cylindre$pat_net_corr_2017>=1500000 & panel_cylindre$pat_net_corr_2017 < 2000000] <- "d - 1.5-2"
panel_cylindre$tranche_pat[panel_cylindre$pat_net_corr_2017>=2000000 & panel_cylindre$pat_net_corr_2017 < 2500000] <- "e - 2-2.5"
panel_cylindre$tranche_pat[panel_cylindre$pat_net_corr_2017>=2500000 & panel_cylindre$pat_net_corr_2017 < 3000000] <- "f - 2.5-3"
panel_cylindre$tranche_pat[panel_cylindre$pat_net_corr_2017>=3000000 & panel_cylindre$pat_net_corr_2017 < 4000000] <- "g - 3-4"
panel_cylindre$tranche_pat[panel_cylindre$pat_net_corr_2017>=4000000 & panel_cylindre$pat_net_corr_2017 < 5000000] <- "i - 4-5"
panel_cylindre$tranche_pat[panel_cylindre$pat_net_corr_2017>=5000000 & panel_cylindre$pat_net_corr_2017 < 6000000] <- "j - 5-6"
panel_cylindre$tranche_pat[panel_cylindre$pat_net_corr_2017>=6000000 & panel_cylindre$pat_net_corr_2017 < 7000000] <- "k - 6-7"
panel_cylindre$tranche_pat[panel_cylindre$pat_net_corr_2017>=7000000 & panel_cylindre$pat_net_corr_2017 < 10000000] <- "l - 7-10"
panel_cylindre$tranche_pat[panel_cylindre$pat_net_corr_2017>=10000000] <- "o - p10"

panel_cylindre$tranche_pat_bis<- "a - 0 - 1 "
panel_cylindre$tranche_pat_bis[panel_cylindre$pat_net_corr_2017>=1000000 & panel_cylindre$pat_net_corr_2017 < 2500000] <- "c - 1 -2.5"
panel_cylindre$tranche_pat_bis[panel_cylindre$pat_net_corr_2017>=2500000 & panel_cylindre$pat_net_corr_2017 < 5000000] <- "d - 2.5-5"
panel_cylindre$tranche_pat_bis[panel_cylindre$pat_net_corr_2017>=5000000 & panel_cylindre$pat_net_corr_2017 < 10000000] <- "e - 5-10"
panel_cylindre$tranche_pat_bis[panel_cylindre$pat_net_corr_2017>=10000000] <- "f - p10"

################################################################################
#  7.0.PASSAGE DE NA A 0 POUR LA VARIABLE MONTANT ACTIFS PRO EXPLOITES PRO     #                       #
################################################################################

colSums(is.na(panel_cylindre))

panel_cylindre <- mutate_at(panel_cylindre, c("montant_actifs_pro_exploites_pro_2017", "montant_actifs_pro_exploites_pro_2014"), ~replace(., is.na(.), 0))
panel_cylindre <- mutate_at(panel_cylindre, c("dettes_deductibles_ifi_2014", "dettes_deductibles_ifi_2017"), ~replace(., is.na(.), 0))
panel_cylindre <- mutate_at(panel_cylindre, c("montant_actifs_dutreil_2014", "montant_actifs_dutreil_2017"), ~replace(., is.na(.), 0))
panel_cylindre <- mutate_at(panel_cylindre, c("montant_dettes_2014", "montant_dettes_2017"), ~replace(., is.na(.), 0))


#Calcul d'une valeur estimee de la residence principale par calage sur donnees fiscales 

panel_cylindre$valeur_residence_principale_estimee_2014<-0.38*panel_cylindre$patri_brut_hors_reste_2014
panel_cylindre<-as.data.frame(panel_cylindre)
panel_cylindre<-panel_cylindre%>%
  mutate(valeur_residence_principale_estimee_2014=ifelse(patri_brut_hors_reste_2014>2000000 & patri_brut_hors_reste_2014<5000000, 0.29*patri_brut_hors_reste_2014, valeur_residence_principale_estimee_2014))%>%
  mutate(valeur_acquisition_residence_principale_2014=ifelse(patri_brut_hors_reste_2014>5000000 & patri_brut_hors_reste_2014<10000000, 0.19*patri_brut_hors_reste_2014, valeur_residence_principale_estimee_2014))%>%
  mutate(valeur_residence_principale_estimee_2014=ifelse(patri_brut_hors_reste_2014>10000000 & patri_brut_hors_reste_2014<20000000, 0.13*patri_brut_hors_reste_2014, valeur_residence_principale_estimee_2014))%>%
  mutate(valeur_residence_principale_estimee_2014=ifelse(patri_brut_hors_reste_2014>20000000, 0.07*patri_brut_hors_reste_2014, valeur_residence_principale_estimee_2014))

panel_cylindre$valeur_residence_principale_estimee_2017<-0.38*panel_cylindre$patri_brut_hors_reste_corr_2017
panel_cylindre<-as.data.frame(panel_cylindre)
panel_cylindre<-panel_cylindre%>%
  mutate(valeur_residence_principale_estimee_2017=ifelse(patri_brut_hors_reste_corr_2017>2000000 & patri_brut_hors_reste_corr_2017<5000000, 0.29*patri_brut_hors_reste_corr_2017, valeur_residence_principale_estimee_2017))%>%
  mutate(valeur_acquisition_residence_principale_2017=ifelse(patri_brut_hors_reste_corr_2017>5000000 & patri_brut_hors_reste_corr_2017<10000000, 0.19*patri_brut_hors_reste_corr_2017, valeur_residence_principale_estimee_2017))%>%
  mutate(valeur_residence_principale_estimee_2017=ifelse(patri_brut_hors_reste_corr_2017>10000000 & patri_brut_hors_reste_corr_2017<20000000, 0.13*patri_brut_hors_reste_corr_2017, valeur_residence_principale_estimee_2017))%>%
  mutate(valeur_residence_principale_estimee_2017=ifelse(patri_brut_hors_reste_corr_2017>20000000, 0.07*patri_brut_hors_reste_corr_2017, valeur_residence_principale_estimee_2017))


#Calcul d'une estimation des actifs exoneres pacte Dutreil


panel_cylindre$montant_actifs_dutreil_2014[panel_cylindre$nombre_freres_soeurs_2014 > 0]<-0
panel_cylindre$montant_actifs_dutreil_2017[panel_cylindre$nombre_freres_soeurs_2017 > 0]<-0
panel_cylindre$montant_actifs_dutreil_2021[panel_cylindre$nombre_freres_soeurs_2021 > 0]<-0

panel_cylindre$exoneration_pacte_dutreil_2014<-0
panel_cylindre$exoneration_pacte_dutreil_2014[panel_cylindre$montant_actifs_dutreil_2014 > 0]<-0.75*panel_cylindre$pat_pro_avec_entreprise_2014

panel_cylindre$exoneration_pacte_dutreil_2017<-0
panel_cylindre$exoneration_pacte_dutreil_2017[panel_cylindre$montant_actifs_dutreil_2017 > 0]<-0.75*panel_cylindre$pat_pro_avec_entreprise_2017

summary(panel_cylindre$exoneration_pacte_dutreil_2017)

################################################################################
#            7.1.CALCUL DU PATRIMOINE NET IMPOSABLE ISF                        #
################################################################################

#panel_cylindre <- mutate_all(panel_cylindre, ~replace(., is.na(.), 0))


panel_cylindre$patrimoine_imposable_isf_2014<-panel_cylindre$pat_immo_2014 - 0.3*panel_cylindre$valeur_residence_principale_estimee_2014 + panel_cylindre$pat_financier_2014+panel_cylindre$reste_2014 - panel_cylindre$montant_dettes_2014 - panel_cylindre$pat_pro_avec_entreprise_2014 - panel_cylindre$exoneration_pacte_dutreil_2014

panel_cylindre$patrimoine_imposable_isf_2017<-panel_cylindre$pat_immo_2017 - 0.3*panel_cylindre$valeur_residence_principale_estimee_2017 + panel_cylindre$pat_financier_2017+panel_cylindre$reste_2017 - panel_cylindre$montant_dettes_2017 - panel_cylindre$pat_pro_avec_entreprise_2017 - panel_cylindre$exoneration_pacte_dutreil_2017

#panel_cylindre$patrimoine_imposable_isf_2021<-panel_cylindre$pat_immo_2021 - 0.3*panel_cylindre$valeur_residence_principale_estimee_2021 + panel_cylindre$pat_financier_2021+panel_cylindre$reste_2021 - panel_cylindre$montant_dettes_2021 + panel_cylindre$pat_pro_avec_entreprise_2021 - panel_cylindre$montant_actifs_pro_exploites_pro_2021 - panel_cylindre$exoneration_pacte_dutreil_2021


panel_cylindre$contribuable_isf_2014<-0
panel_cylindre$contribuable_isf_2014[panel_cylindre$patrimoine_imposable_isf_2014 > 1300000]<-1

summary(panel_cylindre$contribuable_isf_2014)
panel_cylindre$contribuable_isf_reel_2014<-panel_cylindre$ponderation_longitudinal_indiv_2017*panel_cylindre$contribuable_isf_2014


sum(panel_cylindre$contribuable_isf_reel_2014)


panel_cylindre$contribuable_isf_2017<-0
panel_cylindre$contribuable_isf_2017[panel_cylindre$patrimoine_imposable_isf_2017 > 1300000]<-1

summary(panel_cylindre$contribuable_isf_2017)
panel_cylindre$contribuable_isf_reel_2017<-panel_cylindre$ponderation_longitudinal_indiv_2017*panel_cylindre$contribuable_isf_2017

sum(panel_cylindre$contribuable_isf_reel_2017)


#sur base 2014:
#base_2014 <- mutate_all(base_2014, ~replace(., is.na(.), 0))


#base_2014$patrimoine_imposable_isf_2014<-base_2014$pat_immo_2014 - 0.3*base_2014$valeur_acquisition_residence_principale_2014 + base_2014$pat_financier_2014+base_2014$reste_2014 - base_2014$montant_dettes_2014 + base_2014$pat_pro_avec_entreprise_2014 - base_2014$montant_actifs_pro_exploites_pro_2014

#base_2014$contribuable_isf_2014<-0
#base_2014$contribuable_isf_2014[base_2014$patrimoine_imposable_isf_2014 > 1300000]<-1


#dwt2014 <- svydesign(ids = ~1, data = base_2014, weights = ~ base_2014$ponderation_2014)

#svytotal(~contribuable_isf_2014,dwt2014)

#Sur base 2017

#base_2017 <- mutate_all(base_2017, ~replace(., is.na(.), 0))


#base_2017$patrimoine_imposable_isf_2017<-base_2017$pat_immo_2017 - 0.3*base_2017$valeur_acquisition_residence_principale_2017 + base_2017$pat_financier_2017+base_2017$reste_2017 - base_2017$montant_dettes_2017 + base_2017$pat_pro_avec_entreprise_2017 - base_2017$montant_actifs_pro_exploites_pro_2017
#base_2017$contribuable_isf_2017<-0
#base_2017$contribuable_isf_2017[base_2017$patrimoine_imposable_isf_2017 > 1300000]<-1


#dwt2017 <- svydesign(ids = ~1, data = base_2017, weights = ~ base_2017$ponderation_2017_trans)

#svytotal(~contribuable_isf_2017,dwt2017)


#if patrimoine net_taxable>= 1300000, then contribuable_isf=1



################################################################################
#            7.2. CALCUL DU TAUX ET MONTANT D'ISF                              #
################################################################################

##################### 2017 #############################

#calcul des montants d'isf par tranche

panel_cylindre$tranche_1_isf_2017<-0
panel_cylindre$tranche_2_isf_2017<-0
panel_cylindre$tranche_3_isf_2017<-0
panel_cylindre$tranche_4_isf_2017<-0
panel_cylindre$tranche_5_isf_2017<-0
panel_cylindre$tranche_5_isf_2017[panel_cylindre$patrimoine_imposable_isf_2017>1300000]<-2500

panel_cylindre<-as.data.frame(panel_cylindre)
panel_cylindre<-panel_cylindre%>%
  mutate(tranche_1_isf_2017=ifelse(patrimoine_imposable_isf_2017>10000000, (patrimoine_imposable_isf_2017 - 10000000)*0.015, tranche_1_isf_2017 ))%>%
  mutate(tranche_2_isf_2017=ifelse(patrimoine_imposable_isf_2017>10000000,62500, tranche_2_isf_2017 ))%>%
  mutate(tranche_2_isf_2017=ifelse(patrimoine_imposable_isf_2017>5000000 & patrimoine_imposable_isf_2017<10000000,(patrimoine_imposable_isf_2017 - 5000000)*0.0125, tranche_2_isf_2017 ))%>%
  mutate(tranche_3_isf_2017=ifelse(patrimoine_imposable_isf_2017>5000000, 24300, tranche_3_isf_2017))%>%
  mutate(tranche_3_isf_2017=ifelse(patrimoine_imposable_isf_2017>2570000 & patrimoine_imposable_isf_2017< 5000000,(patrimoine_imposable_isf_2017 - 2570000)*0.01, tranche_3_isf_2017 ))%>%
  mutate(tranche_4_isf_2017=ifelse(patrimoine_imposable_isf_2017>2570000, 8890, tranche_4_isf_2017))%>%
  mutate(tranche_4_isf_2017=ifelse(patrimoine_imposable_isf_2017>1300000 & patrimoine_imposable_isf_2017< 2570000, (patrimoine_imposable_isf_2017 - 1300000)*0.007, tranche_4_isf_2017))

#calcul du montant total d'isf du

panel_cylindre$isf_total_2017<-panel_cylindre$tranche_1_isf_2017+panel_cylindre$tranche_2_isf_2017+panel_cylindre$tranche_3_isf_2017+panel_cylindre$tranche_4_isf_2017+panel_cylindre$tranche_5_isf_2017

#Simulation d'un montant d'IR et d'un montant de prelevement sociaux pour prendre en compte le plafonnement

#Prise en compte des parts fiscales pour le revenu fiscal de reference

#panel_cylindre_revenu_fiscal_2017<-0
#panel_cylindre<-panel_cylindre%>%
  #mutate(revenu_fiscal_2017=ifelse(nombre_enfants_2017==0 & couple_2017==0, revenu_declare_2017, revenu_fiscal_2017 ))%>%
  #mutate(revenu_fiscal_2017=ifelse(nombre_enfants_2017>2 & couple_2017==0, revenu_declare_2017/nombre_enfants_2017, revenu_fiscal_2017 ))%>%
  #mutate(revenu_fiscal_2017=ifelse(nombre_enfants_2017>0 & nombre_enfants_2017<3 & couple_2017==0, revenu_declare_2017/(2+0.5*nombre_enfants_2017), revenu_fiscal_2017 ))%>%
  #mutate(revenu_fiscal_2017=ifelse(nombre_enfants_2017>2 & couple_2017==1, revenu_declare_2017/(nombre_enfants_2017+1), revenu_fiscal_2017 ))%>%
  #mutate(revenu_fiscal_2017=ifelse(nombre_enfants_2017>0 & nombre_enfants_2017<3 & couple_2017==0, revenu_declare_2017/(2+0.5*nombre_enfants_2017), revenu_fiscal_2017 ))%>%
  #mutate(revenu_fiscal_2017=ifelse(nombre_enfants_2017==0 & couple_2017==1, revenu_declare_2017/2, revenu_fiscal_2017 ))

#Bareme

#panel_cylindre$tranche_1_ir_2017<-0
#panel_cylindre$tranche_2_ir_2017<-0
#panel_cylindre$tranche_3_ir_2017<-0
#panel_cylindre$tranche_4_ir_2017<-0
#panel_cylindre$tranche_5_ir_2017<-0
#panel_cylindre<-as.data.frame(panel_cylindre)
#panel_cylindre<-panel_cylindre%>%
  #mutate(tranche_1_ir_2017=ifelse(revenu_fiscal_2017>152261, (revenu_fiscal_2017 - 152261)*0.45, tranche_1_ir_2017 ))%>%
  #mutate(tranche_2_ir_2017=ifelse(revenu_fiscal_2017>152261,32144, tranche_2_ir_2017 ))%>%
  #mutate(tranche_2_ir_2017=ifelse(revenu_fiscal_2017>71899 & revenu_fiscal_2017<152261,(revenu_fiscal_2017 - 71899)*0.4, tranche_2_isf_2017 ))%>%
  #mutate(tranche_3_ir_2017=ifelse(revenu_fiscal_2017>71898, 13523, tranche_3_isf_2017))%>%
  #mutate(tranche_3_ir_2017=ifelse(revenu_fiscal_2017>26819 & revenu_fiscal_2017< 71898,(revenu_fiscal_2017 - 26819)*0.3, tranche_3_isf_2017 ))%>%
  #mutate(tranche_4_ir_2017=ifelse(revenu_fiscal_2017>26818, 2394, tranche_4_isf_2017))%>%
  #mutate(tranche_4_ir_2017=ifelse(revenu_fiscal_2017>9711 & revenu_fiscal_2017< 26818, (revenu_fiscal_2017 - 9711)*0.14, tranche_4_isf_2017))

#panel_cylindre$ir_2017<-panel_cylindre$tranche_1_ir_2017 + panel_cylindre$tranche_2_ir_2017 +panel_cylindre$tranche_3_ir_2017 + panel_cylindre$tranche_4_ir_2017

#Decote IR

#panel_cylindre$decote_ir_2017<-0
#panel_cylindre$decote_ir_2017[panel_cylindre$couple_2017==0]<-1165-0.75*panel_cylindre$ir_2017
#panel_cylindre$decote_ir_2017[panel_cylindre$couple_2017==1]<-1920-0.75*panel_cylindre$ir_2017
#panel_cylindre$decote_ir_2017[panel_cylindre$decote_ir_2017<0]<-0

#panel_cylindre$ir_2017<-panel_cylindre$ir_2017 - panel_cylindre$decote_ir_2017

#Prelevements sociaux 2017

#Plafond de l'isf à 75% des revenus declares

panel_cylindre$prelevements_obligatoires_2017<-panel_cylindre$ir_2017 + panel_cylindre$isf_total_2017 +panel_cylindre$ps_2017 

#panel_cylindre$taux_po_2017<-panel_cylindre$prelevements_obligatoires_2017/panel_cylindre$revenu_declare_2017
#plot(panel_cylindre$taux_po_2017)
 
#panel_cylindre<-panel_cylindre%>%
  #mutate(isf_total_2017=ifelse(prelevements_obligatoires_2017>0.75*(revenu_declare_2017 -ir_2017 - ps_2017), 0.75*revenu_declare_2017, isf_total_2017 ))

#panel_cylindre$indic_plafone<-0
#panel_cylindre$indic_plafone[panel_cylindre$prelevements_obligatoires_2017>0.75*(panel_cylindre$revenu_declare_2017 -panel_cylindre$ir_2017 - panel_cylindre$ps_2017)]<-1

#panel_cylindre$total_plafone_2017<-sum(panel_cylindre$indic_plafone*panel_cylindre$ponderation_2017_trans)

#Test du nombre de plafonnés 

#sum(panel_cylindre$indic_plafone*panel_cylindre$ponderation_2017_trans)
#sum(panel_cylindre$indic_plafone*panel_cylindre$ponderation_longitudinal_indiv_2017)
#sum(panel_cylindre$indic_plafone*panel_cylindre$ponderation_longitudinal_indiv_2017_bis_2021)
#sum(panel_cylindre$contribuable_isf_2017*panel_cylindre$ponderation_2017_trans)
#sum(panel_cylindre$contribuable_isf_2017*panel_cylindre$ponderation_longitudinal_indiv_2017)
#sum(panel_cylindre$contribuable_isf_2017*panel_cylindre$ponderation_longitudinal_indiv_2017_bis_2021)


#Decote pour les patrimoines nets taxables entre 1million3 et 1million4

panel_cylindre$decote_isf_2017<-0

panel_cylindre<-panel_cylindre%>%
  mutate(decote_isf_2017=ifelse(patrimoine_imposable_isf_2017>1300000 & patrimoine_imposable_isf_2017< 1400000,17500-(1.25*patrimoine_imposable_isf_2017), decote_isf_2017 ))

panel_cylindre$isf_total_2017<-panel_cylindre$isf_total_2017 - panel_cylindre$decote_isf_2017

#Calcul du taux d'imposition à l'ISF

panel_cylindre$taux_imposition_isf_2017<-panel_cylindre$isf_total_2017/panel_cylindre$patrimoine_imposable_isf_2017

#Identification des contribuables plafonés 

#panel_cylindre$contribuable_plafone_2017<-0

#panel_cylindre$contribuable_plafone_2017[panel_cylindre$prelevements_obligatoires_2017>0.75*panel_cylindre$revenu_declare_2017]<-1

#sum(panel_cylindre$contribuable_plafone_2017)

#panel_cylindre$contribuable_plafone_reel_2017<-panel_cylindre$ponderation_longitudinal_indiv_2017*panel_cylindre$contribuable_plafone_2017

#sum(panel_cylindre$contribuable_plafone_reel_2017)

##################### 2014 #############################

#calcul des montants d'isf par tranche

panel_cylindre$tranche_1_isf_2014<-0
panel_cylindre$tranche_2_isf_2014<-0
panel_cylindre$tranche_3_isf_2014<-0
panel_cylindre$tranche_4_isf_2014<-0
panel_cylindre$tranche_5_isf_2014<-0
panel_cylindre$tranche_5_isf_2014[panel_cylindre$patrimoine_imposable_isf_2014>1300000]<-2500

panel_cylindre<-as.data.frame(panel_cylindre)
panel_cylindre<-panel_cylindre%>%
  mutate(tranche_1_isf_2014=ifelse(patrimoine_imposable_isf_2014>10000000, (patrimoine_imposable_isf_2014 - 10000000)*0.015, tranche_1_isf_2014 ))%>%
  mutate(tranche_2_isf_2014=ifelse(patrimoine_imposable_isf_2014>10000000,62500, tranche_2_isf_2014 ))%>%
  mutate(tranche_2_isf_2014=ifelse(patrimoine_imposable_isf_2014>5000000 & patrimoine_imposable_isf_2014<10000000,(patrimoine_imposable_isf_2014 - 5000000)*0.0125, tranche_2_isf_2014 ))%>%
  mutate(tranche_3_isf_2014=ifelse(patrimoine_imposable_isf_2014>5000000, 24300, tranche_3_isf_2014))%>%
  mutate(tranche_3_isf_2014=ifelse(patrimoine_imposable_isf_2014>2570000 & patrimoine_imposable_isf_2014< 5000000,(patrimoine_imposable_isf_2014 - 2570000)*0.01, tranche_3_isf_2014 ))%>%
  mutate(tranche_4_isf_2014=ifelse(patrimoine_imposable_isf_2014>2570000, 8890, tranche_4_isf_2014))%>%
  mutate(tranche_4_isf_2014=ifelse(patrimoine_imposable_isf_2014>1300000 & patrimoine_imposable_isf_2014< 2570000, (patrimoine_imposable_isf_2014 - 1300000)*0.007, tranche_4_isf_2014))

#calcul du montant total d'isf du

panel_cylindre$isf_total_2014<-panel_cylindre$tranche_1_isf_2014+panel_cylindre$tranche_2_isf_2014+panel_cylindre$tranche_3_isf_2014+panel_cylindre$tranche_4_isf_2014+panel_cylindre$tranche_5_isf_2014

#Plafond de l'isf à 75% des revenus declares

#panel_cylindre$prelevements_obligatoires_2014<-panel_cylindre$ir_2014 + panel_cylindre$isf_total_2014 + panel_cylindre$ps_2014

#panel_cylindre<-panel_cylindre%>%
  #mutate(isf_total_2014=ifelse(prelevements_obligatoires_2014>0.75*panel_cylindre$revenu_declare_2014, 0.75*revenu_declare_2014, isf_total_2014 ))

#Decote pour les patrimoines nets taxables entre 1million3 et 1million4

panel_cylindre$decote_isf_2014<-0

panel_cylindre<-panel_cylindre%>%
  mutate(decote_isf_2014=ifelse(patrimoine_imposable_isf_2014>1300000 & patrimoine_imposable_isf_2014< 1400000,17500-(1.25*patrimoine_imposable_isf_2014), decote_isf_2014 ))

panel_cylindre$isf_total_2014<-panel_cylindre$isf_total_2014 - panel_cylindre$decote_isf_2014

#Calcul du taux d'imposition à l'ISF pour la patrimoine

panel_cylindre$taux_imposition_isf_2014<-panel_cylindre$isf_total_2014/panel_cylindre$patrimoine_imposable_isf_2014
panel_cylindre$taux_imposition_isf_2017<-panel_cylindre$isf_total_2017/panel_cylindre$patrimoine_imposable_isf_2017

#Calcul du taux d'imposition à l'ISF pour les revenus

panel_cylindre$taux_imposition_isf_revenus_2014<-panel_cylindre$isf_total_2014/panel_cylindre$revenu_declare_2014
panel_cylindre$taux_imposition_isf_revenus_2017<-panel_cylindre$isf_total_2017/panel_cylindre$revenu_declare_2017

#Identification des contribuables plafonés 

#panel_cylindre$contribuable_plafone_2014<-0

#panel_cylindre$contribuable_plafone_2014[panel_cylindre$isf_total_2014>0.75*panel_cylindre$revenu_declare_2014]<-1

#sum(panel_cylindre$contribuable_plafone_2014)

#panel_cylindre$contribuable_plafone_reel_2014<-panel_cylindre$ponderation_longitudinal_indiv_2017*panel_cylindre$contribuable_plafone_2014

#sum(panel_cylindre$contribuable_plafone_reel_2014)

################################################################################
#            7.3.CALCUL DU PATRIMOINE NET IMPOSABLE IFI                        #
################################################################################

#Suppression des valeurs manquantes IFI



panel_cylindre$patrimoine_imposable_ifi_2014<-panel_cylindre$pat_immo_2014 - 0.3*panel_cylindre$valeur_residence_principale_estimee_2014 - panel_cylindre$dettes_deductibles_ifi_2014 

panel_cylindre$patrimoine_imposable_ifi_2017<-panel_cylindre$pat_immo_2017 - 0.3*panel_cylindre$valeur_residence_principale_estimee_2017 - panel_cylindre$dettes_deductibles_ifi_2017

panel_cylindre$contribuable_ifi_2014<-0
panel_cylindre$contribuable_ifi_2014[panel_cylindre$patrimoine_imposable_ifi_2014 > 1300000]<-1

summary(panel_cylindre$contribuable_ifi_2014)
panel_cylindre$contribuable_ifi_reel_2014<-panel_cylindre$ponderation_longitudinal_indiv_2017*panel_cylindre$contribuable_ifi_2014


sum(panel_cylindre$contribuable_ifi_reel_2014)


panel_cylindre$contribuable_ifi_2017<-0
panel_cylindre$contribuable_ifi_2017[panel_cylindre$patrimoine_imposable_ifi_2017 > 1300000]<-1

summary(panel_cylindre$contribuable_ifi_2017)
panel_cylindre$contribuable_ifi_reel_2017<-panel_cylindre$ponderation_longitudinal_indiv_2017*panel_cylindre$contribuable_ifi_2017


sum(panel_cylindre$contribuable_ifi_reel_2017)


################################################################################
#            7.4. CALCUL DU TAUX D'IFI                                         #
################################################################################

##################### 2017 #############################

#calcul des montants d'ifi par tranche

panel_cylindre$tranche_1_ifi_2017<-0
panel_cylindre$tranche_2_ifi_2017<-0
panel_cylindre$tranche_3_ifi_2017<-0
panel_cylindre$tranche_4_ifi_2017<-0
panel_cylindre$tranche_5_ifi_2017<-0
panel_cylindre$tranche_5_ifi_2017[panel_cylindre$patrimoine_imposable_ifi_2017>1300000]<-2500

panel_cylindre<-as.data.frame(panel_cylindre)
panel_cylindre<-panel_cylindre%>%
  mutate(tranche_1_ifi_2017=ifelse(patrimoine_imposable_ifi_2017>10000000, (patrimoine_imposable_ifi_2017 - 10000000)*0.015, tranche_1_ifi_2017 ))%>%
  mutate(tranche_2_ifi_2017=ifelse(patrimoine_imposable_ifi_2017>10000000,62500, tranche_2_ifi_2017 ))%>%
  mutate(tranche_2_ifi_2017=ifelse(patrimoine_imposable_ifi_2017>5000000 & patrimoine_imposable_ifi_2017<10000000,(patrimoine_imposable_ifi_2017 - 5000000)*0.0125, tranche_2_ifi_2017 ))%>%
  mutate(tranche_3_ifi_2017=ifelse(patrimoine_imposable_ifi_2017>5000000, 24300, tranche_3_ifi_2017))%>%
  mutate(tranche_3_ifi_2017=ifelse(patrimoine_imposable_ifi_2017>2570000 & patrimoine_imposable_ifi_2017< 5000000,(patrimoine_imposable_ifi_2017 - 2570000)*0.01, tranche_3_ifi_2017 ))%>%
  mutate(tranche_4_ifi_2017=ifelse(patrimoine_imposable_ifi_2017>2570000, 8890, tranche_4_ifi_2017))%>%
  mutate(tranche_4_ifi_2017=ifelse(patrimoine_imposable_ifi_2017>1300000 & patrimoine_imposable_ifi_2017< 2570000, (patrimoine_imposable_ifi_2017 - 1300000)*0.007, tranche_4_ifi_2017))

#calcul du montant total d'ifi du

panel_cylindre$ifi_total_2017<-panel_cylindre$tranche_1_ifi_2017+panel_cylindre$tranche_2_ifi_2017+panel_cylindre$tranche_3_ifi_2017+panel_cylindre$tranche_4_ifi_2017+panel_cylindre$tranche_5_ifi_2017

#Plafond de l'ifi à 75% des revenus declares

#panel_cylindre$prelevements_obligatoires_ifi_2017<-panel_cylindre$ir_2017 + panel_cylindre$ifi_total_2017 +panel_cylindre$ps_2017 - panel_cylindre$csg_deductible_2017


#panel_cylindre<-panel_cylindre%>%
  #mutate(ifi_total_2017=ifelse(prelevements_obligatoires_ifi_2017>0.75*panel_cylindre$revenu_declare_2017, 0.75*revenu_declare_2017, ifi_total_2017 ))

#Decote pour les patrimoines nets taxables entre 1million3 et 1million4

panel_cylindre$decote_ifi_2017<-0

panel_cylindre<-panel_cylindre%>%
  mutate(decote_ifi_2017=ifelse(patrimoine_imposable_ifi_2017>1300000 & patrimoine_imposable_ifi_2017< 1400000,17500-(1.25*patrimoine_imposable_ifi_2017), decote_ifi_2017 ))

panel_cylindre$ifi_total_2017<-panel_cylindre$ifi_total_2017 - panel_cylindre$decote_ifi_2017

#Calcul du taux d'imposition à l'ifi

panel_cylindre$taux_imposition_ifi_2017<-panel_cylindre$ifi_total_2017/panel_cylindre$patrimoine_imposable_isf_2017
panel_cylindre$taux_imposition_ifi_revenus_2017<-panel_cylindre$ifi_total_2017/panel_cylindre$revenu_declare_2017

#Contribuables IFI plafones

#panel_cylindre$contribuable_plafone_ifi_2017<-0

#panel_cylindre$contribuable_plafone_ifi_2017[panel_cylindre$prelevements_obligatoires_ifi_2017>0.75*panel_cylindre$revenu_declare_2017]<-1

#sum(panel_cylindre$contribuable_plafone_ifi_2017)

#panel_cylindre$contribuable_plafone_ifi_reel_2017<-panel_cylindre$ponderation_longitudinal_indiv_2017*panel_cylindre$contribuable_plafone_ifi_2017

#sum(panel_cylindre$contribuable_plafone_ifi_reel_2017)


##################### 2014 #############################

#calcul des montants d'ifi par tranche

panel_cylindre$tranche_1_ifi_2014<-0
panel_cylindre$tranche_2_ifi_2014<-0
panel_cylindre$tranche_3_ifi_2014<-0
panel_cylindre$tranche_4_ifi_2014<-0
panel_cylindre$tranche_5_ifi_2014<-0
panel_cylindre$tranche_5_ifi_2014[panel_cylindre$patrimoine_imposable_ifi_2014>1300000]<-2500

panel_cylindre<-as.data.frame(panel_cylindre)
panel_cylindre<-panel_cylindre%>%
  mutate(tranche_1_ifi_2014=ifelse(patrimoine_imposable_ifi_2014>10000000, (patrimoine_imposable_ifi_2014 - 10000000)*0.015, tranche_1_ifi_2014 ))%>%
  mutate(tranche_2_ifi_2014=ifelse(patrimoine_imposable_ifi_2014>10000000,62500, tranche_2_ifi_2014 ))%>%
  mutate(tranche_2_ifi_2014=ifelse(patrimoine_imposable_ifi_2014>5000000 & patrimoine_imposable_ifi_2014<10000000,(patrimoine_imposable_ifi_2014 - 5000000)*0.0125, tranche_2_ifi_2014 ))%>%
  mutate(tranche_3_ifi_2014=ifelse(patrimoine_imposable_ifi_2014>5000000, 24300, tranche_3_ifi_2014))%>%
  mutate(tranche_3_ifi_2014=ifelse(patrimoine_imposable_ifi_2014>2570000 & patrimoine_imposable_ifi_2014< 5000000,(patrimoine_imposable_ifi_2014 - 2570000)*0.01, tranche_3_ifi_2014 ))%>%
  mutate(tranche_4_ifi_2014=ifelse(patrimoine_imposable_ifi_2014>2570000, 8890, tranche_4_ifi_2014))%>%
  mutate(tranche_4_ifi_2014=ifelse(patrimoine_imposable_ifi_2014>1300000 & patrimoine_imposable_ifi_2014< 2570000, (patrimoine_imposable_ifi_2014 - 1300000)*0.007, tranche_4_ifi_2014))

#calcul du montant total d'ifi 

panel_cylindre$ifi_total_2014<-panel_cylindre$tranche_1_ifi_2014+panel_cylindre$tranche_2_ifi_2014+panel_cylindre$tranche_3_ifi_2014+panel_cylindre$tranche_4_ifi_2014+panel_cylindre$tranche_5_ifi_2014

#Decote pour les patrimoines nets taxables entre 1million3 et 1million4

panel_cylindre$decote_ifi_2014<-0

panel_cylindre<-panel_cylindre%>%
  mutate(decote_ifi_2014=ifelse(patrimoine_imposable_ifi_2014>1300000 & patrimoine_imposable_ifi_2014< 1400000,17500-(1.25*patrimoine_imposable_ifi_2014), decote_ifi_2014 ))

panel_cylindre$ifi_total_2014<-panel_cylindre$ifi_total_2014 - panel_cylindre$decote_ifi_2014

#Calcul du taux d'imposition à l'ifi

panel_cylindre$taux_imposition_ifi_2014<-panel_cylindre$ifi_total_2014/panel_cylindre$patrimoine_imposable_isf_2014
panel_cylindre$taux_imposition_ifi_revenus_2014<-panel_cylindre$ifi_total_2014/panel_cylindre$revenu_declare_2014


#Ajout d'un taux moyennisé 

panel_cylindre$taux_moyenne_isf<-((panel_cylindre$isf_total_2017 + panel_cylindre$isf_total_2014)/2)/((panel_cylindre$patrimoine_imposable_isf_2014 + panel_cylindre$patrimoine_imposable_isf_2017)/2)
panel_cylindre$taux_moyenne_ifi<-((panel_cylindre$ifi_total_2017 + panel_cylindre$ifi_total_2014)/2)/((panel_cylindre$patrimoine_imposable_ifi_2014 + panel_cylindre$patrimoine_imposable_ifi_2017)/2)
panel_cylindre$difference_moyen_taux<-panel_cylindre$taux_moyenne_isf - panel_cylindre$taux_moyenne_ifi
summary(panel_cylindre$difference_moyen_taux)

dwtpanelcylindre <- svydesign(ids = ~1, data = panel_cylindre, weights = ~ panel_cylindre$ponderation_longitudinal_indiv_2017_bis_2021)

