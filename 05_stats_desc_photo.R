################################################################################
#            5. STATISTIQUES DESCRIPTIVES EN PHOTO                             #
################################################################################

base_2014_hors_top<-base_2014[base_2014$indic_top1_brut_corr_2014==0,]
base_2017_hors_top<-base_2017[base_2017$indic_top1_brut_corr_2017==0,]
base_2021_hors_top<-base_2021[base_2021$indic_top1_brut_corr_2021==0,]

dwt2014horstop <- svydesign(ids = ~1, data = base_2014_hors_top, weights = ~ base_2014_hors_top$ponderation_2014)
dwt2017horstop <- svydesign(ids = ~1, data = base_2017_hors_top, weights = ~ base_2017_hors_top$ponderation_2017_trans)
dwt2021horstop <- svydesign(ids = ~1, data = base_2021_hors_top, weights = ~ base_2021_hors_top$ponderation_2021_trans)


svymean(~pat_immo_2014,dwt2014horstop)
svymean(~pat_pro_avec_entreprise_2014,dwt2014horstop)
svymean(~pat_net_2014,dwt2014horstop)
svymean(~pat_financier_2014,dwt2014horstop)
svymean(~pat_immo_2017,dwt2017horstop)
svymean(~pat_pro_avec_entreprise_2017,dwt2017horstop)
svymean(~pat_net_2017,dwt2017horstop)
svymean(~pat_financier_2017,dwt2017horstop)
svymean(~pat_immo_2021,dwt2021horstop)
svymean(~pat_pro_avec_entreprise_2021,dwt2021horstop)
svymean(~pat_net_2021,dwt2021horstop)
svymean(~pat_financier_2021,dwt2021horstop)



################################################################################
#            5.0. STATISTIQUES DESCRIPTIVES EN PHOTO SUR L'AGE                            #
################################################################################

###################################### 2014 #####################################

#Age moyen

#Age moyen des femmes personnes de référence des 0.5

base_2014_top05_brut_F_age<-base_2014_top05_brut[base_2014_top05_brut$sexe_personne_reference_2014=="Femme",]
dwttop05brut2014_F_A <- svydesign(ids = ~1, data = base_2014_top05_brut_F_age, weights = ~ base_2014_top05_brut_F_age$ponderation_2014)
svymean(~age_personne_reference_2014,dwttop05brut2014_F_A)


#Age moyen des femmes personnes de référence des 1

base_2014_top1_brut_F_age<-base_2014_top1_brut[base_2014_top1_brut$sexe_personne_reference_2014=="Femme",]
dwttop1brut2014_F_A <- svydesign(ids = ~1, data = base_2014_top1_brut_F_age, weights = ~ base_2014_top1_brut_F_age$ponderation_2014)
svymean(~age_personne_reference_2014,dwttop1brut2014_F_A)

#Age moyen des hommes personnes de référence des 0.5

base_2014_top05_brut_H_age<-base_2014_top05_brut[base_2014_top05_brut$sexe_personne_reference_2014=="Homme",]
dwttop05brut2014_H_A <- svydesign(ids = ~1, data = base_2014_top05_brut_H_age, weights = ~ base_2014_top05_brut_H_age$ponderation_2014)
svymean(~age_personne_reference_2014,dwttop05brut2014_H_A)

#Age moyen des hommes personnes de référence des 1

base_2014_top1_brut_H_age<-base_2014_top1_brut[base_2014_top1_brut$sexe_personne_reference_2014=="Homme",]
dwttop1brut2014_H_A <- svydesign(ids = ~1, data = base_2014_top1_brut_H_age, weights = ~ base_2014_top1_brut_H_age$ponderation_2014)
svymean(~age_personne_reference_2014,dwttop1brut2014_H_A)


###################################### 2017 #####################################



#Age moyen

#Age moyen des femmes personnes de référence des 0.5

base_2017_top05_brut_F_age<-base_2017_top05_brut[base_2017_top05_brut$sexe_personne_reference_2017=="Femme",]
dwttop05brut2017_F_A <- svydesign(ids = ~1, data = base_2017_top05_brut_F_age, weights = ~ base_2017_top05_brut_F_age$ponderation_2017_trans)
svymean(~age_personne_reference_2017,dwttop05brut2017_F_A)


#Age moyen des femmes personnes de référence des 1

base_2017_top1_brut_F_age<-base_2017_top1_brut[base_2017_top1_brut$sexe_personne_reference_2017=="Femme",]
dwttop1brut2017_F_A <- svydesign(ids = ~1, data = base_2017_top1_brut_F_age, weights = ~ base_2017_top1_brut_F_age$ponderation_2017_trans)
svymean(~age_personne_reference_2017,dwttop1brut2017_F_A)

#Age moyen des hommes personnes de référence des 0.5

base_2017_top05_brut_H_age<-base_2017_top05_brut[base_2017_top05_brut$sexe_personne_reference_2017=="Homme",]
dwttop05brut2017_H_A <- svydesign(ids = ~1, data = base_2017_top05_brut_H_age, weights = ~ base_2017_top05_brut_H_age$ponderation_2017_trans)
svymean(~age_personne_reference_2017,dwttop05brut2017_H_A)

#Age moyen des hommes personnes de référence des 1

base_2017_top1_brut_H_age<-base_2017_top1_brut[base_2017_top1_brut$sexe_personne_reference_2017=="Homme",]
dwttop1brut2017_H_A <- svydesign(ids = ~1, data = base_2017_top1_brut_H_age, weights = ~ base_2017_top1_brut_H_age$ponderation_2017_trans)
svymean(~age_personne_reference_2017,dwttop1brut2017_H_A)



###################################### 2021 #####################################

#Age moyen

#Age moyen des femmes personnes de référence des 0.5

base_2021_top05_brut_F_age<-base_2021_top05_brut[base_2021_top05_brut$sexe_personne_reference_2021=="Femme",]
dwttop05brut2021_F_A <- svydesign(ids = ~1, data = base_2021_top05_brut_F_age, weights = ~ base_2021_top05_brut_F_age$ponderation_2021_trans)
svymean(~age_personne_reference_2021,dwttop05brut2021_F_A)


#Age moyen des femmes personnes de référence des 1

base_2021_top1_brut_F_age<-base_2021_top1_brut[base_2021_top1_brut$sexe_personne_reference_2021=="Femme",]
dwttop1brut2021_F_A <- svydesign(ids = ~1, data = base_2021_top1_brut_F_age, weights = ~ base_2021_top1_brut_F_age$ponderation_2021_trans)
svymean(~age_personne_reference_2021,dwttop1brut2021_F_A)

#Age moyen des hommes personnes de référence des 0.5

base_2021_top05_brut_H_age<-base_2021_top05_brut[base_2021_top05_brut$sexe_personne_reference_2021=="Homme",]
dwttop05brut2021_H_A <- svydesign(ids = ~1, data = base_2021_top05_brut_H_age, weights = ~ base_2021_top05_brut_H_age$ponderation_2021_trans)
svymean(~age_personne_reference_2021,dwttop05brut2021_H_A)

#Age moyen des hommes personnes de référence des 1

base_2021_top1_brut_H_age<-base_2021_top1_brut[base_2021_top1_brut$sexe_personne_reference_2021=="Homme",]
dwttop1brut2021_H_A <- svydesign(ids = ~1, data = base_2021_top1_brut_H_age, weights = ~ base_2021_top1_brut_H_age$ponderation_2021_trans)
svymean(~age_personne_reference_2021,dwttop1brut2021_H_A)






################################################################################
#            5.1. BASE PATRIMOINE BRUT HORS RESTE                              #
################################################################################


################################################################################
#            5.1.1. TOP 0.5% 2014                                               #
################################################################################



dwttop05brut2014 <- svydesign(ids = ~1, data = base_2014_top05_brut, weights = ~ base_2014_top05_brut$ponderation_2014)
dwttop1brut2014 <- svydesign(ids = ~1, data = base_2014_top1_brut, weights = ~ base_2014_top1_brut$ponderation_2014)


#Montants de patrimoine


svymean(~pat_financier_2014,dwttop05brut2014)

svymean(~pat_immo_2014,dwttop05brut2014)

svymean(~pat_pro_avec_entreprise_2014,dwttop05brut2014)

svymean(~patri_brut_hors_reste_2014,dwttop05brut2014)

svymean(~pat_net_2014,dwttop05brut2014)

#Revenus

svymean(~revenu_foncier_2014,dwttop05brut2014)

svymean(~revenu_financier_2014,dwttop05brut2014)

svymean(~revenu_declare_2014,dwttop05brut2014)

#Parts de patrimoine

svymean(~part_pat_financier_2014,dwttop05brut2014)

svymean(~part_pat_immo_2014,dwttop05brut2014)

svymean(~part_pat_pro_avec_entreprise_2014,dwttop05brut2014)


svyquantile(~part_pat_immo_2014,dwttop05brut2014, quantiles=0.5, ci=FALSE)

################################################################################
#            5.1.2. TOP 1% 2014                                 #
################################################################################

#Montants de patrimoine

dwttop1brut2014 <- svydesign(ids = ~1, data = base_2014_top1_brut, weights = ~ base_2014_top1_brut$ponderation_2014)

svymean(~pat_financier_2014,dwttop1brut2014)

svymean(~pat_immo_2014,dwttop1brut2014)

svymean(~pat_pro_avec_entreprise_2014,dwttop1brut2014)

svymean(~patri_brut_hors_reste_2014,dwttop1brut2014)

svymean(~pat_net_2014,dwttop1brut2014)


#Revenus

svymean(~revenu_foncier_2014,na.rm = TRUE, dwttop1brut2014)

svymean(~revenu_financier_2014,dwttop1brut2014)

svymean(~revenu_declare_2014,na.rm = TRUE, dwttop1brut2014)

#Parts de patrimoine

svymean(~part_pat_financier_2014,dwttop1brut2014)

svymean(~part_pat_immo_2014,dwttop1brut2014)

svymean(~part_pat_pro_avec_entreprise_2014,dwttop1brut2014)




################################################################################
#            5.1.3. TOP 0.5% 2017                                               #
################################################################################



#Montants de patrimoine

dwttop05brut2017 <- svydesign(ids = ~1, data = base_2017_top05_brut, weights = ~ base_2017_top05_brut$ponderation_2017_trans)

svymean(~pat_financier_2017,dwttop05brut2017)

svymean(~pat_immo_2017,dwttop05brut2017)

svymean(~pat_pro_avec_entreprise_2017,dwttop05brut2017)

svymean(~patri_brut_hors_reste_corr_2017,dwttop05brut2017)

svymean(~pat_net_corr_2017,dwttop05brut2017)


#Revenus

svymean(~revenu_foncier_2017,na.rm = TRUE,dwttop05brut2017)

svymean(~revenu_financier_2017,dwttop05brut2017)

svymean(~revenu_declare_2017,na.rm=TRUE, dwttop05brut2017)

#Parts de patrimoine

svymean(~part_pat_financier_2017,dwttop05brut2017)

svymean(~part_pat_immo_2017,dwttop05brut2017)

svymean(~part_pat_pro_avec_entreprise_2017,dwttop05brut2017)


################################################################################
#            5.1.4. TOP 1% 2017                                                #
################################################################################

#Montants de patrimoine

dwttop1brut2017 <- svydesign(ids = ~1, data = base_2017_top1_brut, weights = ~ base_2017_top1_brut$ponderation_2017_trans)

svymean(~pat_financier_2017,dwttop1brut2017)

svymean(~pat_immo_2017,dwttop1brut2017)

svymean(~pat_pro_avec_entreprise_2017,dwttop1brut2017)

svymean(~patri_brut_hors_reste_corr_2017,dwttop1brut2017)

svymean(~pat_net_corr_2017,dwttop1brut2017)


#Revenus

svymean(~revenu_foncier_2017,na.rm = TRUE, dwttop1brut2017)

svymean(~revenu_financier_2017,dwttop1brut2017)

svymean(~revenu_declare_2017,na.rm = TRUE, dwttop1brut2017)

#Parts de patrimoine

svymean(~part_pat_financier_2017,dwttop1brut2017)

svymean(~part_pat_immo_2017,dwttop1brut2017)

svymean(~part_pat_pro_avec_entreprise_2017,dwttop1brut2017)



################################################################################
#            5.1.5. TOP 0.5% 2021                                              #
################################################################################



dwttop05brut2021 <- svydesign(ids = ~1, data = base_2021_top05_brut, weights = ~ base_2021_top05_brut$ponderation_2021)
dwttop1brut2021 <- svydesign(ids = ~1, data = base_2021_top1_brut, weights = ~ base_2021_top1_brut$ponderation_2021)


#Montants de patrimoine


svymean(~pat_financier_2021,dwttop05brut2021)

svymean(~pat_immo_2021,dwttop05brut2021)

svymean(~pat_pro_avec_entreprise_2021,dwttop05brut2021)

svymean(~patri_brut_hors_reste_2021,dwttop05brut2021)

svymean(~pat_net_2021,dwttop05brut2021)


#Revenus

svymean(~revenu_foncier_2021,dwttop05brut2021)

svymean(~revenu_financier_2021,dwttop05brut2021)

svymean(~revenu_declare_2021,dwttop05brut2021)

#Parts de patrimoine

svymean(~part_pat_financier_2021,dwttop05brut2021)

svymean(~part_pat_immo_2021,dwttop05brut2021)

svymean(~part_pat_pro_avec_entreprise_2021,dwttop05brut2021)


svyquantile(~part_pat_immo_2021,dwttop05brut2021, quantiles=0.5, ci=FALSE)

################################################################################
#            5.1.6. TOP 1% 2021                                 #
################################################################################

#Montants de patrimoine

dwttop1brut2021 <- svydesign(ids = ~1, data = base_2021_top1_brut, weights = ~ base_2021_top1_brut$ponderation_2021)

svymean(~pat_financier_2021,dwttop1brut2021)

svymean(~pat_immo_2021,dwttop1brut2021)

svymean(~pat_pro_avec_entreprise_2021,dwttop1brut2021)

svymean(~patri_brut_hors_reste_2021,dwttop1brut2021)

svymean(~pat_net_2021,dwttop1brut2021)


#Revenus

svymean(~revenu_foncier_2021,na.rm = TRUE, dwttop1brut2021)

svymean(~revenu_financier_2021,dwttop1brut2021)

svymean(~revenu_declare_2021,na.rm = TRUE, dwttop1brut2021)

#Parts de patrimoine

svymean(~part_pat_financier_2021,dwttop1brut2021)

svymean(~part_pat_immo_2021,dwttop1brut2021)

svymean(~part_pat_pro_avec_entreprise_2021,dwttop1brut2021)




################################################################################
#            5.2. BASE PATRIMOINE IMMOBILIER                                   #
################################################################################


################################################################################
#            5.2.1. TOP 0.5% 2014                                               #
################################################################################



#Montants de patrimoine

dwttop05immo2014 <- svydesign(ids = ~1, data = base_2014_top05_immo, weights = ~ base_2014_top05_immo$ponderation_2014)

svymean(~pat_financier_2014,dwttop05immo2014)

svymean(~pat_immo_2014,dwttop05immo2014)

svymean(~pat_pro_avec_entreprise_2014,dwttop05immo2014)

svymean(~patri_brut_hors_reste_2014,dwttop05immo2014)

#Revenus

svymean(~revenu_foncier_2014,dwttop05immo2014)

svymean(~revenu_financier_2014,dwttop05immo2014)

svymean(~revenu_declare_2014,dwttop05immo2014)

#Parts de patrimoine

svymean(~part_pat_financier_2014,dwttop05immo2014)

svymean(~part_pat_immo_2014,dwttop05immo2014)

svymean(~part_pat_pro_avec_entreprise_2014,dwttop05immo2014)



################################################################################
#            5.2.2. TOP 1% 2014                                                #
################################################################################

#Montants de patrimoine

dwttop1immo2014 <- svydesign(ids = ~1, data = base_2014_top1_immo, weights = ~ base_2014_top1_immo$ponderation_2014)

svymean(~pat_financier_2014,dwttop1immo2014)

svymean(~pat_immo_2014,dwttop1immo2014)

svymean(~pat_pro_avec_entreprise_2014,dwttop1immo2014)

svymean(~patri_brut_hors_reste_2014,dwttop1immo2014)

#Revenus

svymean(~revenu_foncier_2014,dwttop1immo2014)

svymean(~revenu_financier_2014,dwttop1immo2014)

svymean(~revenu_declare_2014,dwttop1immo2014)

#Parts de patrimoine

svymean(~part_pat_financier_2014,dwttop1immo2014)

svymean(~part_pat_immo_2014,dwttop1immo2014)

svymean(~part_pat_pro_avec_entreprise_2014,dwttop1immo2014)

################################################################################
#            5.2.3. TOP 0.5% 2017                                               #
################################################################################



#Montants de patrimoine

dwttop05immo2017 <- svydesign(ids = ~1, data = base_2017_top05_immo, weights = ~ base_2017_top05_immo$ponderation_2017_trans)

svymean(~pat_financier_2017,dwttop05immo2017)

svymean(~pat_immo_2017,dwttop05immo2017)

svymean(~pat_pro_avec_entreprise_2017,dwttop05immo2017)

svymean(~patri_brut_hors_reste_corr_2017,dwttop05immo2017)

#Revenus

svymean(~revenu_foncier_2017,na.rm = TRUE,dwttop05immo2017)

svymean(~revenu_financier_2017,dwttop05immo2017)

svymean(~revenu_declare_2017,na.rm = TRUE, dwttop05immo2017)

#Parts de patrimoine

svymean(~part_pat_financier_2017,dwttop05immo2017)

svymean(~part_pat_immo_2017,dwttop05immo2017)

svymean(~part_pat_pro_avec_entreprise_2017,dwttop05immo2017)


################################################################################
#            5.2.4. TOP 1% 2017                                                #
################################################################################

#Montants de patrimoine

dwttop1immo2017 <- svydesign(ids = ~1, data = base_2017_top1_immo, weights = ~ base_2017_top1_immo$ponderation_2017_trans)

svymean(~pat_financier_2017,dwttop1immo2017)

svymean(~pat_immo_2017,dwttop1immo2017)

svymean(~pat_pro_avec_entreprise_2017,dwttop1immo2017)

svymean(~patri_brut_hors_reste_corr_2017,dwttop1immo2017)

#Revenus

svymean(~revenu_foncier_2017,na.rm = TRUE,dwttop1immo2017)

svymean(~revenu_financier_2017,dwttop1immo2017)

svymean(~revenu_declare_2017,na.rm = TRUE, dwttop1immo2017)

#Parts de patrimoine

svymean(~part_pat_financier_2017,dwttop1immo2017)

svymean(~part_pat_immo_2017,dwttop1immo2017)

svymean(~part_pat_pro_avec_entreprise_2017,dwttop1immo2017)




################################################################################
#            5.3. BASE PATRIMOINE FINANCIER                                    #
################################################################################


################################################################################
#            5.3.1. TOP 0.5% 2014                                               #
################################################################################



#Montants de patrimoine

dwttop05financier2014 <- svydesign(ids = ~1, data = base_2014_top05_financier, weights = ~ base_2014_top05_financier$ponderation_2014)

svymean(~pat_financier_2014,dwttop05financier2014)

svymean(~pat_immo_2014,dwttop05financier2014)

svymean(~pat_pro_avec_entreprise_2014,dwttop05financier2014)

svymean(~patri_brut_hors_reste_2014,dwttop05financier2014)

#Revenus

svymean(~revenu_foncier_2014,dwttop05financier2014)

svymean(~revenu_financier_2014,dwttop05financier2014)

svymean(~revenu_declare_2014,dwttop05financier2014)

#Parts de patrimoine

svymean(~part_pat_financier_2014,dwttop05financier2014)

svymean(~part_pat_immo_2014,dwttop05financier2014)

svymean(~part_pat_pro_avec_entreprise_2014,dwttop05financier2014)



################################################################################
#            5.3.2. TOP 1% 2014                                 #
################################################################################

#Montants de patrimoine

dwttop1financier2014 <- svydesign(ids = ~1, data = base_2014_top1_financier, weights = ~ base_2014_top1_financier$ponderation_2014)

svymean(~pat_financier_2014,dwttop1financier2014)

svymean(~pat_immo_2014,dwttop1financier2014)

svymean(~pat_pro_avec_entreprise_2014,dwttop1financier2014)

svymean(~patri_brut_hors_reste_2014,dwttop1financier2014)

#Revenus

svymean(~revenu_foncier_2014,dwttop1financier2014)

svymean(~revenu_financier_2014,dwttop1financier2014)

svymean(~revenu_declare_2014,dwttop1financier2014)

#Parts de patrimoine

svymean(~part_pat_financier_2014,dwttop1financier2014)

svymean(~part_pat_immo_2014,dwttop1financier2014)

svymean(~part_pat_pro_avec_entreprise_2014,dwttop1financier2014)

################################################################################
#            5.3.3. TOP 0.5% 2017                                               #
################################################################################

#Montants de patrimoine

dwttop05financier2017 <- svydesign(ids = ~1, data = base_2017_top05_financier, weights = ~ base_2017_top05_financier$ponderation_2017_trans)

svymean(~pat_financier_2017,dwttop05financier2017)

svymean(~pat_immo_2017,dwttop05financier2017)

svymean(~pat_pro_avec_entreprise_2017,dwttop05financier2017)

svymean(~patri_brut_hors_reste_corr_2017,dwttop05financier2017)

#Revenus

svymean(~revenu_foncier_2017,na.rm = TRUE,dwttop05financier2017)

svymean(~revenu_financier_2017,dwttop05financier2017)

svymean(~revenu_declare_2017,na.rm = TRUE,dwttop05financier2017)

#Parts de patrimoine

svymean(~part_pat_financier_2017,dwttop05financier2017)

svymean(~part_pat_immo_2017,dwttop05financier2017)

svymean(~part_pat_pro_avec_entreprise_2017,dwttop05financier2017)

################################################################################
#            5.3.4. TOP 1% 2017                                                #
################################################################################


#Montants de patrimoine

dwttop1financier2017 <- svydesign(ids = ~1, data = base_2017_top1_financier, weights = ~ base_2017_top1_financier$ponderation_2017_trans)

svymean(~pat_financier_2017,dwttop1financier2017)

svymean(~pat_immo_2017,dwttop1financier2017)

svymean(~pat_pro_avec_entreprise_2017,dwttop1financier2017)

svymean(~patri_brut_hors_reste_corr_2017,dwttop1financier2017)

#Revenus

svymean(~revenu_foncier_2017,na.rm = TRUE,dwttop1financier2017)

svymean(~revenu_financier_2017,dwttop1financier2017)

svymean(~revenu_declare_2017,na.rm = TRUE,dwttop1financier2017)

#Parts de patrimoine

svymean(~part_pat_financier_2017,dwttop1financier2017)

svymean(~part_pat_immo_2017,dwttop1financier2017)

svymean(~part_pat_pro_avec_entreprise_2017,dwttop1financier2017)


