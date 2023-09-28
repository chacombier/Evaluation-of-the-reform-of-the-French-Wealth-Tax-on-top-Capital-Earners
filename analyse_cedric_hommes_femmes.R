##############################################################################
#            7. ANALYSE CEDRIC HOMMES FEMMES                                 #
##############################################################################

#summary(base_2014_top1_brut$nombre_couples_menage_2014)

#Creation des bases top5% et top 10% en 2014, 2017 et 2021

svyquantile(~patri_brut_hors_reste_2014,dwt2014, quantiles=0.95, ci=FALSE)
svyquantile(~patri_brut_hors_reste_2014,dwt2014, quantiles=0.90, ci=FALSE)

svyquantile(~patri_brut_hors_reste_corr_2017,dwt2017, quantiles=0.95, ci=FALSE)
svyquantile(~patri_brut_hors_reste_corr_2017,dwt2017, quantiles=0.90, ci=FALSE)

svyquantile(~patri_brut_hors_reste_2021,dwt2021, quantiles=0.95, ci=FALSE)
svyquantile(~patri_brut_hors_reste_2021,dwt2021, quantiles=0.90, ci=FALSE)

base_2014_top5_brut<-base_2014[base_2014$patri_brut_hors_reste_2014>831145,]
base_2017_top5_brut<-base_2017[base_2017$patri_brut_hors_reste_corr_2017>834239,]
base_2021_top5_brut<-base_2021[base_2021$patri_brut_hors_reste_2021>987785,]
base_2014_top10_brut<-base_2014[base_2014$patri_brut_hors_reste_2014>553747,]
base_2017_top10_brut<-base_2017[base_2017$patri_brut_hors_reste_corr_2017>569078,]
base_2021_top10_brut<-base_2021[base_2021$patri_brut_hors_reste_2021>681945,]



#version top1

base_celib_H_2014<-base_2014_top1_brut[base_2014_top1_brut$nombre_couples_menage_2014==0,]
base_celib_H_2014<-base_celib_H_2014[base_celib_H_2014$sexe_personne_reference_2014=="Homme",]

base_celib_F_2014<-base_2014_top1_brut[base_2014_top1_brut$nombre_couples_menage_2014==0,]
base_celib_F_2014<-base_celib_F_2014[base_celib_F_2014$sexe_personne_reference_2014=="Femme",]

base_celib_H_2017<-base_2017_top1_brut[base_2017_top1_brut$nombre_couples_menage_2017==0,]
base_celib_H_2017<-base_celib_H_2017[base_celib_H_2017$sexe_personne_reference_2017=="Homme",]

base_celib_F_2017<-base_2017_top1_brut[base_2017_top1_brut$nombre_couples_menage_2017==0,]
base_celib_F_2017<-base_celib_F_2017[base_celib_F_2017$sexe_personne_reference_2017=="Femme",]

base_celib_H_2021<-base_2021_top1_brut[base_2021_top1_brut$nombre_couples_menage_2021==0,]
base_celib_H_2021<-base_celib_H_2021[base_celib_H_2021$sexe_personne_reference_2021=="Homme",]

base_celib_F_2021<-base_2021_top1_brut[base_2021_top1_brut$nombre_couples_menage_2021==0,]
base_celib_F_2021<-base_celib_F_2021[base_celib_F_2021$sexe_personne_reference_2021=="Femme",]

base_couple_2014<-base_2014_top1_brut[base_2014_top1_brut$nombre_couples_menage_2014>0,]
base_couple_2017<-base_2017_top1_brut[base_2017_top1_brut$nombre_couples_menage_2017>0,]
base_couple_2021<-base_2021_top1_brut[base_2021_top1_brut$nombre_couples_menage_2021>0,]

#Version top 5

base_celib_H_2014<-base_2014_top5_brut[base_2014_top5_brut$nombre_couples_menage_2014==0,]
base_celib_H_2014<-base_celib_H_2014[base_celib_H_2014$sexe_personne_reference_2014=="Homme",]

base_celib_F_2014<-base_2014_top5_brut[base_2014_top5_brut$nombre_couples_menage_2014==0,]
base_celib_F_2014<-base_celib_F_2014[base_celib_F_2014$sexe_personne_reference_2014=="Femme",]

base_celib_H_2017<-base_2017_top5_brut[base_2017_top5_brut$nombre_couples_menage_2017==0,]
base_celib_H_2017<-base_celib_H_2017[base_celib_H_2017$sexe_personne_reference_2017=="Homme",]

base_celib_F_2017<-base_2017_top5_brut[base_2017_top5_brut$nombre_couples_menage_2017==0,]
base_celib_F_2017<-base_celib_F_2017[base_celib_F_2017$sexe_personne_reference_2017=="Femme",]

base_celib_H_2021<-base_2021_top5_brut[base_2021_top5_brut$nombre_couples_menage_2021==0,]
base_celib_H_2021<-base_celib_H_2021[base_celib_H_2021$sexe_personne_reference_2021=="Homme",]

base_celib_F_2021<-base_2021_top5_brut[base_2021_top5_brut$nombre_couples_menage_2021==0,]
base_celib_F_2021<-base_celib_F_2021[base_celib_F_2021$sexe_personne_reference_2021=="Femme",]

base_couple_2014<-base_2014_top5_brut[base_2014_top5_brut$nombre_couples_menage_2014>0,]
base_couple_2017<-base_2017_top5_brut[base_2017_top5_brut$nombre_couples_menage_2017>0,]
base_couple_2021<-base_2021_top5_brut[base_2021_top5_brut$nombre_couples_menage_2021>0,]

#version top 10

base_celib_H_2014<-base_2014_top10_brut[base_2014_top10_brut$nombre_couples_menage_2014==0,]
base_celib_H_2014<-base_celib_H_2014[base_celib_H_2014$sexe_personne_reference_2014=="Homme",]

base_celib_F_2014<-base_2014_top10_brut[base_2014_top10_brut$nombre_couples_menage_2014==0,]
base_celib_F_2014<-base_celib_F_2014[base_celib_F_2014$sexe_personne_reference_2014=="Femme",]

base_celib_H_2017<-base_2017_top10_brut[base_2017_top10_brut$nombre_couples_menage_2017==0,]
base_celib_H_2017<-base_celib_H_2017[base_celib_H_2017$sexe_personne_reference_2017=="Homme",]

base_celib_F_2017<-base_2017_top10_brut[base_2017_top10_brut$nombre_couples_menage_2017==0,]
base_celib_F_2017<-base_celib_F_2017[base_celib_F_2017$sexe_personne_reference_2017=="Femme",]

base_celib_H_2021<-base_2021_top10_brut[base_2021_top10_brut$nombre_couples_menage_2021==0,]
base_celib_H_2021<-base_celib_H_2021[base_celib_H_2021$sexe_personne_reference_2021=="Homme",]

base_celib_F_2021<-base_2021_top10_brut[base_2021_top10_brut$nombre_couples_menage_2021==0,]
base_celib_F_2021<-base_celib_F_2021[base_celib_F_2021$sexe_personne_reference_2021=="Femme",]

base_couple_2014<-base_2014_top10_brut[base_2014_top10_brut$nombre_couples_menage_2014>0,]
base_couple_2017<-base_2017_top10_brut[base_2017_top10_brut$nombre_couples_menage_2017>0,]
base_couple_2021<-base_2021_top10_brut[base_2021_top10_brut$nombre_couples_menage_2021>0,]

#creation variable veuf/veuve

base_celib_F_2021$veuf<-0
base_celib_F_2021$veuf[base_celib_F_2021$statut_matrimonial_2021==3]<-1
base_celib_H_2021$veuf<-0
base_celib_H_2021$veuf[base_celib_H_2021$statut_matrimonial_2021==3]<-1
base_celib_F_2017$veuf<-0
base_celib_F_2017$veuf[base_celib_F_2017$statut_matrimonial_2017==3]<-1
base_celib_H_2017$veuf<-0
base_celib_H_2017$veuf[base_celib_H_2017$statut_matrimonial_2017==3]<-1
base_celib_F_2014$veuf<-0
base_celib_F_2014$veuf[base_celib_F_2014$statut_matrimonial_2014==3]<-1
base_celib_H_2014$veuf<-0
base_celib_H_2014$veuf[base_celib_H_2014$statut_matrimonial_2014==3]<-1


#Nombre de ménages concernés par chaque base
sum(base_celib_H_2014$ponderation_2014, na.rm = TRUE)
sum(base_celib_F_2014$ponderation_2014, na.rm = TRUE)
sum(base_celib_H_2017$ponderation_2017_trans, na.rm = TRUE)
sum(base_celib_F_2017$ponderation_2017_trans, na.rm = TRUE)
sum(base_celib_H_2021$ponderation_2021_trans, na.rm = TRUE)
sum(base_celib_F_2021$ponderation_2021_trans, na.rm = TRUE)
sum(base_couple_2014$ponderation_2014, na.rm = TRUE)
sum(base_couple_2017$ponderation_2017_trans, na.rm = TRUE)
sum(base_couple_2021$ponderation_2021_trans, na.rm = TRUE)


#version top05

base_celib_H_2014<-base_2014_top05_brut[base_2014_top05_brut$nombre_couples_menage_2014==0,]
base_celib_H_2014<-base_celib_H_2014[base_celib_H_2014$sexe_personne_reference_2014=="Homme",]

base_celib_F_2014<-base_2014_top05_brut[base_2014_top05_brut$nombre_couples_menage_2014==0,]
base_celib_F_2014<-base_celib_F_2014[base_celib_F_2014$sexe_personne_reference_2014=="Femme",]

base_celib_H_2017<-base_2017_top05_brut[base_2017_top05_brut$nombre_couples_menage_2017==0,]
base_celib_H_2017<-base_celib_H_2017[base_celib_H_2017$sexe_personne_reference_2017=="Homme",]

base_celib_F_2017<-base_2017_top05_brut[base_2017_top05_brut$nombre_couples_menage_2017==0,]
base_celib_F_2017<-base_celib_F_2017[base_celib_F_2017$sexe_personne_reference_2017=="Femme",]

base_celib_H_2021<-base_2021_top05_brut[base_2021_top05_brut$nombre_couples_menage_2021==0,]
base_celib_H_2021<-base_celib_H_2021[base_celib_H_2021$sexe_personne_reference_2021=="Homme",]

base_celib_F_2021<-base_2021_top05_brut[base_2021_top05_brut$nombre_couples_menage_2021==0,]
base_celib_F_2021<-base_celib_F_2021[base_celib_F_2021$sexe_personne_reference_2021=="Femme",]

base_couple_2014<-base_2014_top05_brut[base_2014_top05_brut$nombre_couples_menage_2014>0,]
base_couple_2017<-base_2017_top05_brut[base_2017_top05_brut$nombre_couples_menage_2017>0,]
base_couple_2021<-base_2021_top05_brut[base_2021_top05_brut$nombre_couples_menage_2021>0,]


#Création de bases pondérées

base_celib_H_2014<-base_celib_H_2014[!is.na(base_celib_H_2014$ponderation_2014),]
dwtbase_celib_H_2014 <- svydesign(ids = ~1, data = base_celib_H_2014, weights = ~ base_celib_H_2014$ponderation_2014)

base_celib_F_2014<-base_celib_F_2014[!is.na(base_celib_F_2014$ponderation_2014),]
dwtbase_celib_F_2014 <- svydesign(ids = ~1, data = base_celib_F_2014, weights = ~ base_celib_F_2014$ponderation_2014)

base_celib_H_2017<-base_celib_H_2017[!is.na(base_celib_H_2017$ponderation_2017_trans),]
dwtbase_celib_H_2017 <- svydesign(ids = ~1, data = base_celib_H_2017, weights = ~ base_celib_H_2017$ponderation_2017_trans)

base_celib_F_2017<-base_celib_F_2017[!is.na(base_celib_F_2017$ponderation_2017_trans),]
dwtbase_celib_F_2017 <- svydesign(ids = ~1, data = base_celib_F_2017, weights = ~ base_celib_F_2017$ponderation_2017_trans)

base_celib_H_2021<-base_celib_H_2021[!is.na(base_celib_H_2021$ponderation_2021_trans),]
dwtbase_celib_H_2021 <- svydesign(ids = ~1, data = base_celib_H_2021, weights = ~ base_celib_H_2021$ponderation_2021_trans)

base_celib_F_2021<-base_celib_F_2021[!is.na(base_celib_F_2021$ponderation_2021_trans),]
dwtbase_celib_F_2021 <- svydesign(ids = ~1, data = base_celib_F_2021, weights = ~ base_celib_F_2021$ponderation_2021_trans)

base_couple_2014<-base_couple_2014[!is.na(base_couple_2014$ponderation_2014),]
dwt_couple_2014<-svydesign(ids = ~1, data = base_couple_2014, weights = ~ base_couple_2014$ponderation_2014)

base_couple_2017<-base_couple_2017[!is.na(base_couple_2017$ponderation_2017_trans),]
dwt_couple_2017<-svydesign(ids = ~1, data = base_couple_2017, weights = ~ base_couple_2017$ponderation_2017_trans)

base_couple_2021<-base_couple_2021[!is.na(base_couple_2021$ponderation_2021_trans),]
dwt_couple_2021<-svydesign(ids = ~1, data = base_couple_2021, weights = ~ base_couple_2021$ponderation_2021_trans)



#sTATS SUR BASE

#bASE hOMME 2014

svymean(~patri_brut_hors_reste_2014,dwtbase_celib_H_2014)
svymean(~pat_net_2014,dwtbase_celib_H_2014)
svymean(~part_pat_immo_2014,dwtbase_celib_H_2014)
svymean(~part_pat_financier_2014,dwtbase_celib_H_2014)
svymean(~part_pat_pro_avec_entreprise_2014,dwtbase_celib_H_2014)
svymean(~age_personne_reference_2014,dwtbase_celib_H_2014)
svymean(~veuf, dwtbase_celib_H_2014)

#BASE FEMME 2014

svymean(~patri_brut_hors_reste_2014,dwtbase_celib_F_2014)
svymean(~pat_net_2014,dwtbase_celib_F_2014)

svymean(~part_pat_immo_2014,dwtbase_celib_F_2014)
svymean(~part_pat_financier_2014,dwtbase_celib_F_2014)
svymean(~part_pat_pro_avec_entreprise_2014,dwtbase_celib_F_2014)
svymean(~age_personne_reference_2014,dwtbase_celib_F_2014)
svymean(~veuf, dwtbase_celib_F_2014)


#BASE HOMME 2017

svymean(~patri_brut_hors_reste_corr_2017,dwtbase_celib_H_2017)
svymean(~pat_net_corr_2017,dwtbase_celib_H_2017)

svymean(~part_pat_immo_2017,dwtbase_celib_H_2017)
svymean(~part_pat_financier_2017,dwtbase_celib_H_2017)
svymean(~part_pat_pro_avec_entreprise_2017,dwtbase_celib_H_2017)
svymean(~age_personne_reference_2017,dwtbase_celib_H_2017)
svymean(~veuf, dwtbase_celib_H_2017)


#BASE FEMME 2017

svymean(~patri_brut_hors_reste_corr_2017,dwtbase_celib_F_2017)
svymean(~pat_net_corr_2017,dwtbase_celib_F_2017)

svymean(~part_pat_immo_2017,dwtbase_celib_F_2017)
svymean(~part_pat_financier_2017,dwtbase_celib_F_2017)
svymean(~part_pat_pro_avec_entreprise_2017,dwtbase_celib_F_2017)
svymean(~age_personne_reference_2017,dwtbase_celib_F_2017)
svymean(~veuf, dwtbase_celib_F_2017)


#BASE HOMME 2021

svymean(~patri_brut_hors_reste_2021,dwtbase_celib_H_2021)
svymean(~pat_net_2021,dwtbase_celib_H_2021)

svymean(~part_pat_immo_2021,dwtbase_celib_H_2021)
svymean(~part_pat_financier_2021,dwtbase_celib_H_2021)
svymean(~part_pat_pro_avec_entreprise_2021,dwtbase_celib_H_2021)
svymean(~age_personne_reference_2021,dwtbase_celib_H_2021)
svymean(~veuf, dwtbase_celib_H_2021)


#BASE FEMME 2021

svymean(~patri_brut_hors_reste_2021,dwtbase_celib_F_2021)
svymean(~pat_net_2021,dwtbase_celib_F_2021)

svymean(~part_pat_immo_2021,dwtbase_celib_F_2021)
svymean(~part_pat_financier_2021,dwtbase_celib_F_2021)
svymean(~part_pat_pro_avec_entreprise_2021,dwtbase_celib_F_2021)
svymean(~age_personne_reference_2021,dwtbase_celib_F_2021)
svymean(~veuf, dwtbase_celib_F_2021)


#Base couple 2014

svymean(~patri_brut_hors_reste_2014,dwt_couple_2014)
svymean(~pat_net_2014,dwt_couple_2014)

svymean(~part_pat_immo_2014,dwt_couple_2014)
svymean(~part_pat_financier_2014,dwt_couple_2014)
svymean(~part_pat_pro_avec_entreprise_2014,dwt_couple_2014)
svymean(~age_personne_reference_2014,dwt_couple_2014)

#Base couple 2017

svymean(~patri_brut_hors_reste_corr_2017,dwt_couple_2017)
svymean(~pat_net_corr_2017,dwt_couple_2017)

svymean(~part_pat_immo_2017,dwt_couple_2017)
svymean(~part_pat_financier_2017,dwt_couple_2017)
svymean(~part_pat_pro_avec_entreprise_2017,dwt_couple_2017)
svymean(~age_personne_reference_2017,dwt_couple_2017)

#Base_couple_2021

svymean(~patri_brut_hors_reste_2021,dwt_couple_2021)
svymean(~pat_net_2021,dwt_couple_2021)

svymean(~part_pat_immo_2021,dwt_couple_2021)
svymean(~part_pat_financier_2021,dwt_couple_2021)
svymean(~part_pat_pro_avec_entreprise_2021,dwt_couple_2021)
svymean(~age_personne_reference_2021,dwt_couple_2021)

svymean(~age_personne_reference_2021,dwt_couple_2021)
