################################################################################
#                                  08. DID                                      #
################################################################################


#Considerer ceux qui n ont jamais ete contribuable comme hors champ (pour ne pas que leur variation de taux d imposition soit 0 etr qu'ils rentrent dans le groupe de controle)

panel_cylindre$taux_imposition_isf_2014<-as.numeric(panel_cylindre$taux_imposition_isf_2014)
panel_cylindre$taux_imposition_ifi_2014<-as.numeric(panel_cylindre$taux_imposition_ifi_2014)
panel_cylindre$taux_imposition_isf_2017<-as.numeric(panel_cylindre$taux_imposition_isf_2017)
panel_cylindre$taux_imposition_ifi_2017<-as.numeric(panel_cylindre$taux_imposition_ifi_2017)

# On crée quand on est pas imposé à l'isf sur les deux année un taux d'imposition NA et pas 0 => sinon on a un différentiel à 0 qui rentre dans le groype de controle alors que ca n'a pas de sens
panel_cylindre<-as.data.frame(panel_cylindre)
panel_cylindre<-panel_cylindre%>%
  mutate(taux_imposition_isf_2017=ifelse(taux_imposition_isf_2014==0 & taux_imposition_isf_2017==0, is.na(taux_imposition_isf_2017), taux_imposition_isf_2017))%>%
  mutate(taux_imposition_isf_2014=ifelse(taux_imposition_isf_2014==0 & taux_imposition_isf_2017==0, is.na(taux_imposition_isf_2014), taux_imposition_isf_2014))


panel_cylindre$individus_exclus<-0
#panel_cylindre$individus_exclus[panel_cylindre$contribuable_isf_2014==0 & panel_cylindre$contribuable_isf_2017==0 & panel_cylindre$contribuable_ifi_2017==0 & panel_cylindre$contribuable_ifi_2014==0]<-1
panel_cylindre$individus_exclus[panel_cylindre$pat_net_corr_2017<1000000]<-1
panel_cylindre_isf<-panel_cylindre[panel_cylindre$individus_exclus==0,]

#panel_cylindre_isf<-panel_cylindre[panel_cylindre$contribuable_isf_2014==0 & !panel_cylindre$contribuable_isf_2017==0,]

sum(panel_cylindre_isf$ponderation_longitudinal_indiv_2017)

################################################################################
#                      7.1. CONTROLE ET TRAITEMENT                             #
################################################################################


#I.Groupe de controle via patrimoine

#I.1. Creation de la variable de difference de taux isf/ifi via patrimoine


#panel_cylindre_isf$difference_taux_2014<-panel_cylindre_isf$taux_imposition_isf_2014 - panel_cylindre_isf$taux_imposition_ifi_2014
panel_cylindre_isf$difference_taux_2017<-panel_cylindre_isf$taux_imposition_isf_2017 - panel_cylindre_isf$taux_imposition_ifi_2017
#panel_cylindre_isf$moyenne_difference_taux<-(panel_cylindre_isf$difference_taux_2017+panel_cylindre_isf$difference_taux_2014)/2
#summary(panel_cylindre_isf$difference_taux_2014)
summary(panel_cylindre_isf$difference_taux_2017)
#summary(panel_cylindre_isf$moyenne_difference_taux)
summary(panel_cylindre$difference_moyen_taux)

#panel_cylindre_isf$difference_taux_moyenne<-panel_cylindre_isf$taux_imposition_isf_moyenne - panel_cylindre_isf$taux_imposition_ifi_moyenne
#panel_cylindre_isf$delta_taux_moyenne<-panel_cylindre_isf$difference_taux_moyenne/panel_cylindre_isf$moyenne_ISF

#I.2. Determination groupe de controle via patrimoine

#summary(panel_cylindre_isf$difference_taux_moyenne)
#summary(panel_cylindre_isf$delta_taux_moyenne)

panel_cylindre_isf$controle1<-0
panel_cylindre_isf$controle1[panel_cylindre_isf$difference_taux_2017>0.00434]<-1
table(panel_cylindre_isf$controle1)

panel_cylindre_isf$controle2<-0
panel_cylindre_isf$controle2[panel_cylindre_isf$difference_taux_2017>0.0039]<-1
table(panel_cylindre_isf$controle2)


#Premieres stats sur les groupes


#Groupe de controle via revenus declares

#II.1. Creation de la variable de difference de taux isf/ifi via revenus

#panel_cylindre_isf$difference_taux_revenus_2014<-panel_cylindre_isf$taux_imposition_isf_revenus_2014 - panel_cylindre_isf$taux_imposition_ifi_revenus_2014
#panel_cylindre_isf$difference_taux_revenus_2017<-panel_cylindre_isf$taux_imposition_isf_revenus_2017 - panel_cylindre_isf$taux_imposition_ifi_revenus_2017
#panel_cylindre_isf$moyenne_difference_taux_revenus<-(panel_cylindre_isf$difference_taux_revenus_2017+panel_cylindre_isf$difference_taux_revenus_2014)/2
#summary(panel_cylindre_isf$difference_taux_revenus_2014)
#summary(panel_cylindre_isf$difference_taux_revenus_2017)
#summary(panel_cylindre_isf$moyenne_difference_taux_revenus)

#panel_cylindre_isf$controle2<-1
#panel_cylindre_isf$controle2[panel_cylindre_isf$moyenne_difference_taux_revenus>1]<-0
#table(panel_cylindre_isf$controle2)
#summary(panel_cylindre_isf$moyenne_difference_taux_revenus)


#Preparation de la base pour DiD

#Selection des variables pour DiD

panel_isf_2014<- subset.data.frame(panel_cylindre_isf, select=c(tranche_age, tranche_pat_bis, tranche_age, ponderation_longitudinal_indiv_2017_bis_2021, tranche_pat, controle1, identifiant_longitudinal_individuel, pat_net_2014, pat_immo_2014, pat_financier_2014, pat_pro_avec_entreprise_2014, part_pat_immo_2014, part_pat_financier_2014, part_pat_pro_avec_entreprise_2014, revenu_declare_2014, revenu_foncier_2014, revenu_financier_2014, sexe_personne_reference_2014, sexe_conjoint_2014, diplome_conjoint_2014, diplome_personne_reference_2014 ))
panel_isf_2021<- subset.data.frame(panel_cylindre_isf, select=c(tranche_age, tranche_pat_bis, tranche_age,ponderation_longitudinal_indiv_2017_bis_2021, tranche_pat,controle1, identifiant_longitudinal_individuel, pat_net_2021, pat_immo_2021, pat_financier_2021, pat_pro_avec_entreprise_2021, part_pat_immo_2021, part_pat_financier_2021, part_pat_pro_avec_entreprise_2021, revenu_declare_2021, revenu_foncier_2021, revenu_financier_2021, sexe_personne_reference_2021, sexe_conjoint_2021, diplome_conjoint_2021, diplome_personne_reference_2021 ))
panel_isf_2017<- subset.data.frame(panel_cylindre_isf, select=c(tranche_age, tranche_pat_bis,tranche_age, ponderation_longitudinal_indiv_2017_bis_2021, tranche_pat,pat_net_corr_2017, identifiant_longitudinal_individuel, controle1, pat_immo_2017, pat_financier_2017, pat_pro_avec_entreprise_2017, part_pat_immo_2017, part_pat_financier_2017, part_pat_pro_avec_entreprise_2017, revenu_declare_2017, revenu_foncier_2017, revenu_financier_2017, sexe_personne_reference_2017, sexe_conjoint_2017, diplome_conjoint_2017, diplome_personne_reference_2017 ))

#Une sous base par année pour préparer la base empilée avec les 3 années

panel_isf_2014$patrimoine<-panel_isf_2014$pat_net_2014
panel_isf_2014$patrimoine_immobilier<-panel_isf_2014$pat_immo_2014
panel_isf_2014$patrimoine_financier<-panel_isf_2014$pat_financier_2014
panel_isf_2014$patrimoine_professionnel_global<-panel_isf_2014$pat_pro_avec_entreprise_2014
panel_isf_2014$part_patrimoine_immobilier<-panel_isf_2014$part_pat_immo_2014
panel_isf_2014$part_patrimoine_financier<-panel_isf_2014$part_pat_financier_2014
panel_isf_2014$part_patrimoine_professionnel<-panel_isf_2014$part_pat_pro_avec_entreprise_2014
panel_isf_2014$revenu_declare<-panel_isf_2014$revenu_declare_2014
panel_isf_2014$revenu_foncier<-panel_isf_2014$revenu_foncier_2014
panel_isf_2014$revenu_financier<-panel_isf_2014$revenu_financier_2014
panel_isf_2014$sexe_conj<-panel_isf_2014$sexe_conjoint_2014
panel_isf_2014$sexe_pr<-panel_isf_2014$sexe_personne_reference_2014
panel_isf_2014$diplome_conj<-panel_isf_2014$diplome_conjoint_2014
panel_isf_2014$diplome_pr<-panel_isf_2014$diplome_personne_reference_2014
panel_isf_2014$annee<-2014
panel_isf_2014<- subset.data.frame(panel_isf_2014, select=-c(pat_net_2014,  pat_immo_2014, pat_financier_2014, pat_pro_avec_entreprise_2014, part_pat_immo_2014, part_pat_financier_2014, part_pat_pro_avec_entreprise_2014, revenu_declare_2014, revenu_foncier_2014, revenu_financier_2014, sexe_conjoint_2014, sexe_personne_reference_2014, diplome_personne_reference_2014, diplome_conjoint_2014 ))

panel_isf_2017$patrimoine<-panel_isf_2017$pat_net_corr_2017
panel_isf_2017$patrimoine_immobilier<-panel_isf_2017$pat_immo_2017
panel_isf_2017$patrimoine_financier<-panel_isf_2017$pat_financier_2017
panel_isf_2017$patrimoine_professionnel_global<-panel_isf_2017$pat_pro_avec_entreprise_2017
panel_isf_2017$part_patrimoine_immobilier<-panel_isf_2017$part_pat_immo_2017
panel_isf_2017$part_patrimoine_financier<-panel_isf_2017$part_pat_financier_2017
panel_isf_2017$part_patrimoine_professionnel<-panel_isf_2017$part_pat_pro_avec_entreprise_2017
panel_isf_2017$revenu_declare<-panel_isf_2017$revenu_declare_2017
panel_isf_2017$revenu_foncier<-panel_isf_2017$revenu_foncier_2017
panel_isf_2017$revenu_financier<-panel_isf_2017$revenu_financier_2017
panel_isf_2017$sexe_conj<-panel_isf_2017$sexe_conjoint_2017
panel_isf_2017$sexe_pr<-panel_isf_2017$sexe_personne_reference_2017
panel_isf_2017$diplome_conj<-panel_isf_2017$diplome_conjoint_2017
panel_isf_2017$diplome_pr<-panel_isf_2017$diplome_personne_reference_2017
panel_isf_2017$annee<-2017
panel_isf_2017<- subset.data.frame(panel_isf_2017, select=-c(pat_net_corr_2017, pat_immo_2017, pat_financier_2017, pat_pro_avec_entreprise_2017, part_pat_immo_2017, part_pat_financier_2017, part_pat_pro_avec_entreprise_2017, revenu_declare_2017, revenu_foncier_2017, revenu_financier_2017, diplome_conjoint_2017, diplome_personne_reference_2017, sexe_conjoint_2017, sexe_personne_reference_2017))


panel_isf_2021$patrimoine<-panel_isf_2021$pat_net_2021
panel_isf_2021$patrimoine_immobilier<-panel_isf_2021$pat_immo_2021
panel_isf_2021$patrimoine_financier<-panel_isf_2021$pat_financier_2021
panel_isf_2021$patrimoine_professionnel_global<-panel_isf_2021$pat_pro_avec_entreprise_2021
panel_isf_2021$part_patrimoine_immobilier<-panel_isf_2021$part_pat_immo_2021
panel_isf_2021$part_patrimoine_financier<-panel_isf_2021$part_pat_financier_2021
panel_isf_2021$part_patrimoine_professionnel<-panel_isf_2021$part_pat_pro_avec_entreprise_2021
panel_isf_2021$revenu_declare<-panel_isf_2021$revenu_declare_2021
panel_isf_2021$revenu_foncier<-panel_isf_2021$revenu_foncier_2021
panel_isf_2021$revenu_financier<-panel_isf_2021$revenu_financier_2021
panel_isf_2021$sexe_conj<-panel_isf_2021$sexe_conjoint_2021
panel_isf_2021$sexe_pr<-panel_isf_2021$sexe_personne_reference_2021
panel_isf_2021$diplome_conj<-panel_isf_2021$diplome_conjoint_2021
panel_isf_2021$diplome_pr<-panel_isf_2021$diplome_personne_reference_2021
panel_isf_2021$annee<-2021
panel_isf_2021<- subset.data.frame(panel_isf_2021, select=-c(pat_net_2021, pat_immo_2021, pat_financier_2021, pat_pro_avec_entreprise_2021, part_pat_immo_2021, part_pat_financier_2021, part_pat_pro_avec_entreprise_2021, revenu_declare_2021, revenu_foncier_2021, revenu_financier_2021, sexe_conjoint_2021, sexe_personne_reference_2021, diplome_conjoint_2021, diplome_personne_reference_2021))

#Compilation des 3 sous bases par annee pour une base DiD exploitable

panel_isf_DiD<-rbind(panel_isf_2014, panel_isf_2017)
panel_isf_DiD<-rbind(panel_isf_DiD, panel_isf_2021)

#Travail sur les variables pour mener des tests de robusesse avec option log, normalisation à 100 en 2017 et sans max de chaque variable

panel_isf_DiD$part_patrimoine_professionnel[panel_isf_DiD$part_patrimoine_professionnel==-Inf]<-1

#Variables en log

panel_isf_DiD$patrimoine[panel_isf_DiD$patrimoine <= 0 & panel_isf_DiD$annee==2021] <- 10000
panel_isf_DiD$patrimoine_immobilier[panel_isf_DiD$patrimoine_immobilier <= 0 & panel_isf_DiD$annee==2021] <- 10000
panel_isf_DiD$patrimoine_financier[panel_isf_DiD$patrimoine_financier <= 0 & panel_isf_DiD$annee==2021] <- 10000
panel_isf_DiD$pat_log <- log(panel_isf_DiD$patrimoine)
panel_isf_DiD$part_patrimoine_financier[panel_isf_DiD$part_patrimoine_financier <= 0]<-1
panel_isf_DiD$part_patrimoine_immobilier[panel_isf_DiD$part_patrimoine_immobilier <= 0]<-1                                 
panel_isf_DiD$pat_fi_log <- log(panel_isf_DiD$patrimoine_financier)
panel_isf_DiD$pat_immo_log <- log(panel_isf_DiD$patrimoine_immobilier)
panel_isf_DiD$revenu_declare[panel_isf_DiD$revenu_declare<=0]<-1
panel_isf_DiD$rev_log <- log(panel_isf_DiD$revenu_declare)
panel_isf_DiD$revenu_foncier[panel_isf_DiD$revenu_foncier<=0]<-1
panel_isf_DiD$rev_foncier_log <- log(panel_isf_DiD$revenu_foncier)
panel_isf_DiD$revenu_financier[panel_isf_DiD$revenu_financier<=0]<-1
panel_isf_DiD$rev_financier_log <- log(panel_isf_DiD$revenu_financier)
panel_isf_DiD$part_patrimoine_financier_log<-log(panel_isf_DiD$part_patrimoine_financier)
panel_isf_DiD$part_patrimoine_immobilier_log<-log(panel_isf_DiD$part_patrimoine_immobilier)
panel_isf_DiD$part_patrimoine_professionnel_log<-log(panel_isf_DiD$part_patrimoine_professionnel)

#Echantillon robustesse sans les max 

panel_isf_DiD_robustesse<-panel_isf_DiD[-which.max(panel_isf_DiD$patrimoine),]
panel_isf_DiD_robustesse<-panel_isf_DiD[-which.max(panel_isf_DiD$part_patrimoine_immobilier),]
panel_isf_DiD_robustesse<-panel_isf_DiD[-which.max(panel_isf_DiD$part_patrimoine_financier),]
panel_isf_DiD_robustesse<-panel_isf_DiD[-which.max(panel_isf_DiD$part_patrimoine_professionnel),]
panel_isf_DiD_robustesse<-panel_isf_DiD[-which.max(panel_isf_DiD$revenu_declare),]
panel_isf_DiD_robustesse<-panel_isf_DiD[-which.max(panel_isf_DiD$revenu_foncier),]
panel_isf_DiD_robustesse<-panel_isf_DiD[-which.max(panel_isf_DiD$revenu_financier),]

#Echantillon en normalisé



panel_isf_DiD$pat_immo_2017<-panel_isf_DiD$patrimoine_immobilier[panel_isf_DiD$annee==2017]
panel_isf_DiD$pat_immo_norm<-(panel_isf_DiD$patrimoine_immobilier/panel_isf_DiD$pat_immo_2017)*100

panel_isf_DiD$pat_fi_2017<-panel_isf_DiD$patrimoine_financier[panel_isf_DiD$annee==2017]
panel_isf_DiD$pat_fi_norm<-(panel_isf_DiD$patrimoine_financier/panel_isf_DiD$pat_fi_2017)*100

panel_isf_DiD$pat_2017<-panel_isf_DiD$pat_log[panel_isf_DiD$annee==2017]
panel_isf_DiD$pat_log_norm<-(panel_isf_DiD$pat_log/panel_isf_DiD$pat_2017)*100
panel_isf_DiD$rev_dec_2017<-panel_isf_DiD$revenu_declare[panel_isf_DiD$annee==2017]
panel_isf_DiD$revenu_declare_norm<-(panel_isf_DiD$revenu_declare/panel_isf_DiD$rev_dec_2017)*100
panel_isf_DiD$rev_foncier_2017<-panel_isf_DiD$revenu_foncier[panel_isf_DiD$annee==2017]
panel_isf_DiD$rev_foncier_norm<-(panel_isf_DiD$revenu_foncier/panel_isf_DiD$rev_foncier_2017)*100
panel_isf_DiD$rev_fi_2017<-panel_isf_DiD$revenu_financier[panel_isf_DiD$annee==2017]
panel_isf_DiD$rev_fi_norm<-(panel_isf_DiD$revenu_financier/panel_isf_DiD$rev_fi_2017)*100
panel_isf_DiD$part_pat_fi_2017<-panel_isf_DiD$part_patrimoine_financier[panel_isf_DiD$annee==2017]
panel_isf_DiD$part_pat_fi_norm<-(panel_isf_DiD$part_patrimoine_financier/panel_isf_DiD$part_pat_fi_2017)*100
panel_isf_DiD$part_pat_immo_2017<-panel_isf_DiD$part_patrimoine_immobilier[panel_isf_DiD$annee==2017]
panel_isf_DiD$part_pat_immo_norm<-(panel_isf_DiD$part_patrimoine_immobilier/panel_isf_DiD$part_pat_immo_2017)*100
panel_isf_DiD$part_pat_pro_2017<-panel_isf_DiD$part_patrimoine_professionnel[panel_isf_DiD$annee==2017]
panel_isf_DiD$part_pat_pro_norm<-(panel_isf_DiD$part_patrimoine_professionnel/panel_isf_DiD$part_pat_pro_2017)*100

panel_isf_DiD$an2021 <- as.numeric(panel_isf_DiD$annee==2021)
panel_isf_DiD$an2014 <- as.numeric(panel_isf_DiD$annee==2014)

panel_isf_DiD$an2021_t <- as.numeric(panel_isf_DiD$annee==2021)*as.numeric(panel_isf_DiD$controle1==1)
panel_isf_DiD$an2014_t <- as.numeric(panel_isf_DiD$annee==2014)*as.numeric(panel_isf_DiD$controle1==1)


panel_isf_DiD$an2021 <- as.numeric(panel_isf_DiD$annee==2021)
panel_isf_DiD$an2014 <- as.numeric(panel_isf_DiD$annee==2014)

panel_isf_DiD$an2021_t <- as.numeric(panel_isf_DiD$annee==2021)*as.numeric(panel_isf_DiD$controle1==1)
panel_isf_DiD$an2014_t <- as.numeric(panel_isf_DiD$annee==2014)*as.numeric(panel_isf_DiD$controle1==1)


panel_isf_DiD_robustesse$an2021 <- as.numeric(panel_isf_DiD_robustesse$annee==2021)
panel_isf_DiD_robustesse$an2014 <- as.numeric(panel_isf_DiD_robustesse$annee==2014)

panel_isf_DiD_robustesse$an2021_t <- as.numeric(panel_isf_DiD_robustesse$annee==2021)*as.numeric(panel_isf_DiD_robustesse$controle1==1)
panel_isf_DiD_robustesse$an2014_t <- as.numeric(panel_isf_DiD_robustesse$annee==2014)*as.numeric(panel_isf_DiD_robustesse$controle1==1)




#############################################################################################


panel_isf_DiD$part_patrimoine_professionnel[panel_isf_DiD$pat_immo_log==-Inf]<-0
#############################################################################################

panel_isf_DiD[is.na(panel_isf_DiD) | panel_isf_DiD=="Inf"] <- NA

dd <- lm(data = panel_isf_DiD,part_patrimoine_immobilier ~
           an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
#dd2 <- lm(data = panel_isf_DiD,part_patrimoine_immobilier_log ~
           #an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(dd)

#stargazer(dd, dd2, dep.var.labels = c("part pat fi", "part pat immo"),style = "aer", digits=5, out="models.txt", covariate.labels=c("2014xTraitement", "2021xTraitement", "2014","2021", "tranche d'age", "tranche de patrimoine" ))

panel_isf_did_c<-panel_isf_DiD[panel_isf_DiD$controle1==0, ]
panel_isf_did_t<-panel_isf_DiD[panel_isf_DiD$controle1==1, ]

panel_isf_did_c_2014<-panel_isf_DiD[panel_isf_DiD$controle1==0 & panel_isf_DiD$annee ==2014, ]
panel_isf_did_t_2014<-panel_isf_DiD[panel_isf_DiD$controle1==1 & panel_isf_DiD$annee ==2014, ]
panel_isf_did_c_2017<-panel_isf_DiD[panel_isf_DiD$controle1==0 & panel_isf_DiD$annee ==2017, ]
panel_isf_did_t_2017<-panel_isf_DiD[panel_isf_DiD$controle1==1 & panel_isf_DiD$annee ==2017, ]
panel_isf_did_c_2021<-panel_isf_DiD[panel_isf_DiD$controle1==0 & panel_isf_DiD$annee ==2021, ]
panel_isf_did_t_2021<-panel_isf_DiD[panel_isf_DiD$controle1==1 & panel_isf_DiD$annee ==2021, ]

dwt_isf_c_14<-svydesign(ids = ~1, data = panel_isf_did_c_2014, weights = ~ panel_isf_did_c_2014$ponderation_longitudinal_indiv_2017_bis_2021)
dwt_isf_c_17<-svydesign(ids = ~1, data = panel_isf_did_c_2017, weights = ~ panel_isf_did_c_2017$ponderation_longitudinal_indiv_2017_bis_2021)
dwt_isf_c_21<-svydesign(ids = ~1, data = panel_isf_did_c_2021, weights = ~ panel_isf_did_c_2021$ponderation_longitudinal_indiv_2017_bis_2021)
dwt_isf_t_14<-svydesign(ids = ~1, data = panel_isf_did_t_2014, weights = ~ panel_isf_did_t_2014$ponderation_longitudinal_indiv_2017_bis_2021)
dwt_isf_t_17<-svydesign(ids = ~1, data = panel_isf_did_t_2017, weights = ~ panel_isf_did_t_2017$ponderation_longitudinal_indiv_2017_bis_2021)
dwt_isf_t_21<-svydesign(ids = ~1, data = panel_isf_did_t_2021, weights = ~ panel_isf_did_t_2021$ponderation_longitudinal_indiv_2017_bis_2021)


svymean(~revenu_declare, dwt_isf_c_14 )
svymean(~revenu_declare,na.rm = TRUE, dwt_isf_c_17 )
svymean(~revenu_declare, dwt_isf_c_21 )
svymean(~revenu_declare, dwt_isf_t_14 )
svymean(~revenu_declare,na.rm = TRUE, dwt_isf_t_17 )
svymean(~revenu_declare, dwt_isf_t_21 )
summary(panel_isf_DiD$annee)
panel_isf_DiD$controle1bis[panel_isf_DiD$controle1==1]<-2021

t<-att_gt(yname="pat_log",tname="annee", idname = "identifiant_longitudinal_individuel",gname="controle1bis",xformla= ~tranche_age + tranche_pat_bis, panel = TRUE,control_group = "nevertreated", weightsname = "ponderation_longitudinal_indiv_2017_bis_2021",  data=panel_isf_DiD )
