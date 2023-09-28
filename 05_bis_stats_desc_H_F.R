
################################################################################
#            5.bis. STATISTIQUES DESCRIPTIVES HOMMES/FEMMES                                                       #
################################################################################

#I. HOMMES 2021



################################################################################
#            5.1.6. TOP 0.5% 2021                                 #
################################################################################

#Montants de patrimoine

base_2021_top05_brut_H<-base_2021[base_2021_top05_brut$sexe_pJersonne_reference_2021=="Homme",]

dwttop05brutH2021 <- svydesign(ids = ~1, data = base_2021_top05_brut_H, weights = ~ base_2021_top05_brut_H$ponderation_2021)

svymean(~pat_financier_2021,dwttop05brutH2021)

svymean(~pat_immo_2021,dwttop05brutH2021)

svymean(~pat_pro_avec_entreprise_2021,dwttop05brutH2021)

svymean(~patri_brut_hors_reste_2021,dwttop05brutH2021)

#Revenus

svymean(~revenu_foncier_2021,na.rm = TRUE, dwttop05brutH2021)

svymean(~revenu_financier_2021,dwttop05brutH2021)

svymean(~revenu_declare_2021,na.rm = TRUE, dwttop05brutH2021)

#Parts de patrimoine

svymean(~part_pat_financier_2021,dwttop05brutH2021)

svymean(~part_pat_immo_2021,dwttop05brutH2021)

svymean(~part_pat_pro_avec_entreprise_2021,dwttop05brutH2021)


################################################################################
#            5.1.6. TOP 1% 2021                                 #
################################################################################

#Montants de patrimoine

base_2021_top1_brut_H<-base_2021[base_2021_top05_brut$sexe_personne_reference_2021=="Homme",]

dwttop1brutH2021 <- svydesign(ids = ~1, data = base_2021_top1_brut_H, weights = ~ base_2021_top1_brut_H$ponderation_2021)

svymean(~pat_financier_2021,dwttop1brutH2021)

svymean(~pat_immo_2021,dwttop1brutH2021)

svymean(~pat_pro_avec_entreprise_2021,dwttop1brutH2021)

svymean(~patri_brut_hors_reste_2021,dwttop1brutH2021)

#Revenus

svymean(~revenu_foncier_2021,na.rm = TRUE, dwttop1brutH2021)

svymean(~revenu_financier_2021,dwttop1brutH2021)

svymean(~revenu_declare_2021,na.rm = TRUE, dwttop1brutH2021)

#Parts de patrimoine

svymean(~part_pat_financier_2021,dwttop1brutH2021)

svymean(~part_pat_immo_2021,dwttop1brutH2021)

svymean(~part_pat_pro_avec_entreprise_2021,dwttop1brutH2021)


#II. FEMMES 2021



################################################################################
#            5.1.6. TOP 0.5% 2021                                 #
################################################################################

#Montants de patrimoine

base_2021_top05_brut_F<-base_2021[base_2021_top05_brut$sexe_personne_reference_2021=="Femme",]

dwttop05brutF2021 <- svydesign(ids = ~1, data = base_2021_top05_brut_F, weights = ~ base_2021_top05_brut_F$ponderation_2021)

svymean(~pat_financier_2021,dwttop05brutF2021)

svymean(~pat_immo_2021,dwttop05brutF2021)

svymean(~pat_pro_avec_entreprise_2021,dwttop05brutF2021)

svymean(~patri_brut_hors_reste_2021,dwttop05brutF2021)

#Revenus

svymean(~revenu_foncier_2021,na.rm = TRUE, dwttop05brutF2021)

svymean(~revenu_financier_2021,dwttop05brutF2021)

svymean(~revenu_declare_2021,na.rm = TRUE, dwttop05brutF2021)

#Parts de patrimoine

svymean(~part_pat_financier_2021,dwttop05brutF2021)

svymean(~part_pat_immo_2021,dwttop05brutF2021)

svymean(~part_pat_pro_avec_entreprise_2021,dwttop05brutF2021)


################################################################################
#            5.1.6. TOP 1% 2021                                 #
################################################################################
#Montants de patrimoine

base_2021_top1_brut_F<-base_2021[base_2021_top1_brut$sexe_personne_reference_2021=="Femme",]

dwttop1brutF2021 <- svydesign(ids = ~1, data = base_2021_top1_brut_F, weights = ~ base_2021_top1_brut_F$ponderation_2021)

svymean(~pat_financier_2021,dwttop1brutF2021)

svymean(~pat_immo_2021,dwttop1brutF2021)

svymean(~pat_pro_avec_entreprise_2021,dwttop1brutF2021)

svymean(~patri_brut_hors_reste_2021,dwttop1brutF2021)

#Revenus

svymean(~revenu_foncier_2021,na.rm = TRUE, dwttop1brutF2021)

svymean(~revenu_financier_2021,dwttop1brutF2021)

svymean(~revenu_declare_2021,na.rm = TRUE, dwttop1brutF2021)

#Parts de patrimoine

svymean(~part_pat_financier_2021,dwttop1brutF2021)

svymean(~part_pat_immo_2021,dwttop1brutF2021)

svymean(~part_pat_pro_avec_entreprise_2021,dwttop1brutF2021)

#I. HOMMES 2017



################################################################################
#            5.1.6. TOP 0.5% 2021                                 #
################################################################################

#Montants de patrimoine

base_2017_top05_brut_H<-base_2017[base_2017_top05_brut$sexe_personne_reference_2017=="Homme",]

dwttop05brutH2017 <- svydesign(ids = ~1, data = base_2017_top05_brut_H, weights = ~ base_2017_top05_brut_H$ponderation_2017)

svymean(~pat_financier_2017,dwttop05brutH2017)

svymean(~pat_immo_2017,dwttop05brutH2017)

svymean(~pat_pro_avec_entreprise_2017,dwttop05brutH2017)

svymean(~patri_brut_hors_reste_2017,dwttop05brutH2017)

#Revenus

svymean(~revenu_foncier_2017,na.rm = TRUE, dwttop05brutH2017)

svymean(~revenu_financier_2017,dwttop05brutH2017)

svymean(~revenu_declare_2017,na.rm = TRUE, dwttop05brutH2017)

#Parts de patrimoine

svymean(~part_pat_financier_2017,dwttop05brutH2017)

svymean(~part_pat_immo_2017,dwttop05brutH2017)

svymean(~part_pat_pro_avec_entreprise_2017,dwttop05brutH2017)


################################################################################
#            5.1.6. TOP 1% 2017                                 #
################################################################################

#Montants de patrimoine

base_2017_top1_brut_H<-base_2017[base_2017_top05_brut$sexe_personne_reference_2017=="Homme",]

dwttop1brutH2017 <- svydesign(ids = ~1, data = base_2017_top1_brut_H, weights = ~ base_2017_top1_brut_H$ponderation_2017)

svymean(~pat_financier_2017,dwttop1brutH2017)

svymean(~pat_immo_2017,dwttop1brutH2017)

svymean(~pat_pro_avec_entreprise_2017,dwttop1brutH2017)

svymean(~patri_brut_hors_reste_2017,dwttop1brutH2017)

#Revenus

svymean(~revenu_foncier_2017,na.rm = TRUE, dwttop1brutH2017)

svymean(~revenu_financier_2017,dwttop1brutH2017)

svymean(~revenu_declare_2017,na.rm = TRUE, dwttop1brutH2017)

#Parts de patrimoine

svymean(~part_pat_financier_2017,dwttop1brutH2017)

svymean(~part_pat_immo_2017,dwttop1brutH2017)

svymean(~part_pat_pro_avec_entreprise_2017,dwttop1brutH2017)


#II. FEMMES 2017



################################################################################
#            5.1.6. TOP 0.5% 2017                                 #
################################################################################

#Montants de patrimoine

base_2017_top05_brut_F<-base_2017[base_2017_top05_brut$sexe_personne_reference_2017=="Femme",]

dwttop05brutF2017 <- svydesign(ids = ~1, data = base_2017_top05_brut_F, weights = ~ base_2017_top05_brut_F$ponderation_2017)

svymean(~pat_financier_2017,dwttop05brutF2017)

svymean(~pat_immo_2017,dwttop05brutF2017)

svymean(~pat_pro_avec_entreprise_2017,dwttop05brutF2017)

svymean(~patri_brut_hors_reste_2017,dwttop05brutF2017)

#Revenus

svymean(~revenu_foncier_2017,na.rm = TRUE, dwttop05brutF2017)

svymean(~revenu_financier_2017,dwttop05brutF2017)

svymean(~revenu_declare_2017,na.rm = TRUE, dwttop05brutF2017)

#Parts de patrimoine

svymean(~part_pat_financier_2017,dwttop05brutF2017)

svymean(~part_pat_immo_2017,dwttop05brutF2017)

svymean(~part_pat_pro_avec_entreprise_2017,dwttop05brutF2017)


################################################################################
#            5.1.6. TOP 1% 2017                                 #
################################################################################
#Montants de patrimoine

base_2017_top1_brut_F<-base_2017[base_2017_top1_brut$sexe_personne_reference_2017=="Femme",]

dwttop1brutF2017 <- svydesign(ids = ~1, data = base_2017_top1_brut_F, weights = ~ base_2017_top1_brut_F$ponderation_2017)

svymean(~pat_financier_2017,dwttop1brutF2017)

svymean(~pat_immo_2017,dwttop1brutF2017)

svymean(~pat_pro_avec_entreprise_2017,dwttop1brutF2017)

svymean(~patri_brut_hors_reste_2017,dwttop1brutF2017)

#Revenus

svymean(~revenu_foncier_2017,na.rm = TRUE, dwttop1brutF2017)

svymean(~revenu_financier_2017,dwttop1brutF2017)

svymean(~revenu_declare_2017,na.rm = TRUE, dwttop1brutF2017)

#Parts de patrimoine

svymean(~part_pat_financier_2017,dwttop1brutF2017)

svymean(~part_pat_immo_2017,dwttop1brutF2017)

svymean(~part_pat_pro_avec_entreprise_2017,dwttop1brutF2017)


#I. HOMMES 2021



################################################################################
#            5.1.6. TOP 0.5% 2021                                 #
################################################################################

#Montants de patrimoine

base_2014_top05_brut_H<-base_2014[base_2014_top05_brut$sexe_personne_reference_2014=="Homme",]

dwttop05brutH2014 <- svydesign(ids = ~1, data = base_2014_top05_brut_H, weights = ~ base_2014_top05_brut_H$ponderation_2014)

svymean(~pat_financier_2014,dwttop05brutH2014)

svymean(~pat_immo_2014,dwttop05brutH2014)

svymean(~pat_pro_avec_entreprise_2014,dwttop05brutH2014)

svymean(~patri_brut_hors_reste_2014,dwttop05brutH2014)

#Revenus

svymean(~revenu_foncier_2014,na.rm = TRUE, dwttop05brutH2014)

svymean(~revenu_financier_2014,dwttop05brutH2014)

svymean(~revenu_declare_2014,na.rm = TRUE, dwttop05brutH2014)

#Parts de patrimoine

svymean(~part_pat_financier_2014,dwttop05brutH2014)

svymean(~part_pat_immo_2014,dwttop05brutH2014)

svymean(~part_pat_pro_avec_entreprise_2014,dwttop05brutH2014)


################################################################################
#            5.1.6. TOP 1% 2014                                 #
################################################################################

#Montants de patrimoine

base_2014_top1_brut_H<-base_2014[base_2014_top05_brut$sexe_personne_reference_2014=="Homme",]

dwttop1brutH2014 <- svydesign(ids = ~1, data = base_2014_top1_brut_H, weights = ~ base_2014_top1_brut_H$ponderation_2014)

svymean(~pat_financier_2014,dwttop1brutH2014)

svymean(~pat_immo_2014,dwttop1brutH2014)

svymean(~pat_pro_avec_entreprise_2014,dwttop1brutH2014)

svymean(~patri_brut_hors_reste_2014,dwttop1brutH2014)

#Revenus

svymean(~revenu_foncier_2014,na.rm = TRUE, dwttop1brutH2014)

svymean(~revenu_financier_2014,dwttop1brutH2014)

svymean(~revenu_declare_2014,na.rm = TRUE, dwttop1brutH2014)

#Parts de patrimoine

svymean(~part_pat_financier_2014,dwttop1brutH2014)

svymean(~part_pat_immo_2014,dwttop1brutH2014)

svymean(~part_pat_pro_avec_entreprise_2014,dwttop1brutH2014)


#II. FEMMES 2014



################################################################################
#            5.1.6. TOP 0.5% 2014                                 #
################################################################################

#Montants de patrimoine

base_2014_top05_brut_F<-base_2014[base_2014_top05_brut$sexe_personne_reference_2014=="Femme",]

dwttop05brutF2014 <- svydesign(ids = ~1, data = base_2014_top05_brut_F, weights = ~ base_2014_top05_brut_F$ponderation_2014)

svymean(~pat_financier_2014,dwttop05brutF2014)

svymean(~pat_immo_2014,dwttop05brutF2014)

svymean(~pat_pro_avec_entreprise_2014,dwttop05brutF2014)

svymean(~patri_brut_hors_reste_2014,dwttop05brutF2014)

#Revenus

svymean(~revenu_foncier_2014,na.rm = TRUE, dwttop05brutF2014)

svymean(~revenu_financier_2014,dwttop05brutF2014)

svymean(~revenu_declare_2014,na.rm = TRUE, dwttop05brutF2014)

#Parts de patrimoine

svymean(~part_pat_financier_2014,dwttop05brutF2014)

svymean(~part_pat_immo_2014,dwttop05brutF2014)

svymean(~part_pat_pro_avec_entreprise_2014,dwttop05brutF2014)


################################################################################
#            5.1.6. TOP 1% 2014                                 #
################################################################################
#Montants de patrimoine

base_2014_top1_brut_F<-base_2014[base_2014_top1_brut$sexe_personne_reference_2014=="Femme",]

dwttop1brutF2014 <- svydesign(ids = ~1, data = base_2014_top1_brut_F, weights = ~ base_2014_top1_brut_F$ponderation_2014)

svymean(~pat_financier_2014,dwttop1brutF2014)

svymean(~pat_immo_2014,dwttop1brutF2014)

svymean(~pat_pro_avec_entreprise_2014,dwttop1brutF2014)

svymean(~patri_brut_hors_reste_2014,dwttop1brutF2014)

#Revenus

svymean(~revenu_foncier_2014,na.rm = TRUE, dwttop1brutF2014)

svymean(~revenu_financier_2014,dwttop1brutF2014)

svymean(~revenu_declare_2014,na.rm = TRUE, dwttop1brutF2014)

#Parts de patrimoine

svymean(~part_pat_financier_2014,dwttop1brutF2014)

svymean(~part_pat_immo_2014,dwttop1brutF2014)

svymean(~part_pat_pro_avec_entreprise_2014,dwttop1brutF2014)
