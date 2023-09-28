################################################################################
#            6. STATISTIQUES DESCRIPTIVES EN PANEL                             #
################################################################################

svyquantile(~pat_immo_2014,dwtpanelcylindre, quantiles=0.5, ci=FALSE)
svyquantile(~pat_pro_avec_entreprise_2014,dwtpanelcylindre, quantiles=0.5, ci=FALSE)
svyquantile(~pat_net_2014,dwtpanelcylindre, quantiles=0.5, ci=FALSE)
svyquantile(~pat_financier_2014,dwtpanelcylindre, quantiles=0.5, ci=FALSE)
svyquantile(~pat_immo_2017,dwtpanelcylindre, quantiles=0.5, ci=FALSE)
svyquantile(~pat_pro_avec_entreprise_2017,dwtpanelcylindre, quantiles=0.5, ci=FALSE)
svyquantile(~pat_net_2017,dwtpanelcylindre, quantiles=0.5, ci=FALSE)
svyquantile(~pat_financier_2017,dwtpanelcylindre, quantiles=0.5, ci=FALSE)
svyquantile(~pat_immo_2021,dwtpanelcylindre, quantiles=0.5, ci=FALSE)
svyquantile(~pat_pro_avec_entreprise_2021,dwtpanelcylindre, quantiles=0.5, ci=FALSE)
svyquantile(~pat_net_2021,dwtpanelcylindre, quantiles=0.5, ci=FALSE)
svyquantile(~pat_financier_2021,dwtpanelcylindre, quantiles=0.5, ci=FALSE)

#Patrimoine moyen hors top 1%

panel_cylindre_hors_top<-panel_cylindre[panel_cylindre$indic_top1_brut_corr_2014==0,]
panel_cylindre_hors_top<-panel_cylindre_hors_top[panel_cylindre_hors_top$indic_top1_brut_corr_2017==0,]
dwtpanelcylindrehorstop <- svydesign(ids = ~1, data = panel_cylindre_hors_top, weights = ~ panel_cylindre_hors_top$ponderation_longitudinal_indiv_2017_bis_2021)

svymean(~pat_immo_2014,dwtpanelcylindrehorstop)
svymean(~pat_pro_avec_entreprise_2014,dwtpanelcylindrehorstop)
svymean(~pat_net_2014,dwtpanelcylindrehorstop)
svymean(~pat_financier_2014,dwtpanelcylindrehorstop)
svymean(~pat_immo_2017,dwtpanelcylindrehorstop)
svymean(~pat_pro_avec_entreprise_2017,dwtpanelcylindrehorstop)
svymean(~pat_net_2017,dwtpanelcylindrehorstop)
svymean(~pat_financier_2017,dwtpanelcylindrehorstop)
svymean(~pat_immo_2021,dwtpanelcylindrehorstop)
svymean(~pat_pro_avec_entreprise_2021,dwtpanelcylindrehorstop)
svymean(~pat_net_2021,dwtpanelcylindrehorstop)
svymean(~pat_financier_2021,dwtpanelcylindrehorstop)


#Test de la bonne prise en compte de la suppression du top1
#summary(panel_cylindre_hors_top$indic_top1_brut_corr_2017)

################################################################################
#                               6.1. 2017                                      #
################################################################################


#Statistiques descriptives top 1% en panel

#Composition du patrimoine

svymean(~part_pat_financier_2017,dwtpanelcylindre_top1)

svymean(~part_pat_immo_2017,dwtpanelcylindre_top1)

svymean(~part_pat_pro_avec_entreprise_2017,dwtpanelcylindre_top1)

#Montants de patrimoine

svymean(~pat_immo_2017,dwtpanelcylindre_top1)

svymean(~pat_financier_2017,dwtpanelcylindre_top1)

svymean(~pat_pro_avec_entreprise_2017,dwtpanelcylindre_top1)

svymean(~patri_brut_hors_reste_corr_2017,dwtpanelcylindre_top1)

svymean(~pat_net_corr_2017,dwtpanelcylindre_top1)

#Revenus

svymean(~revenu_foncier_2017,na.rm = TRUE,dwtpanelcylindre_top1)

svymean(~revenu_financier_2017,na.rm = TRUE,dwtpanelcylindre_top1)

svymean(~revenu_declare_2017,na.rm = TRUE,dwtpanelcylindre_top1)


#Statistiques descriptives top 0.5% en panel 

#Composition du patrimoine

svymean(~part_pat_financier_2017,dwtpanelcylindre_top05)

svymean(~part_pat_immo_2017,dwtpanelcylindre_top05)

svymean(~part_pat_pro_avec_entreprise_2017,dwtpanelcylindre_top05)

#Montants de patrimoine

svymean(~pat_immo_2017,dwtpanelcylindre_top05)

svymean(~pat_financier_2017,dwtpanelcylindre_top05)

svymean(~pat_pro_avec_entreprise_2017,dwtpanelcylindre_top05)

svymean(~patri_brut_hors_reste_corr_2017,dwtpanelcylindre_top05)

svymean(~pat_net_corr_2017,dwtpanelcylindre_top05)


#Revenus

svymean(~revenu_foncier_2017,na.rm = TRUE,dwtpanelcylindre_top05)

svymean(~revenu_financier_2017,na.rm = TRUE,dwtpanelcylindre_top05)

svymean(~revenu_declare_2017,na.rm = TRUE,dwtpanelcylindre_top05)

################################################################################
#                               6.1. 2021                                      #
################################################################################


#Statistiques descriptives top 1% en panel

#Composition du patrimoine

svymean(~part_pat_financier_2021,dwtpanelcylindre_top1)

svymean(~part_pat_immo_2021,dwtpanelcylindre_top1)

svymean(~part_pat_pro_avec_entreprise_2021,dwtpanelcylindre_top1)

#Montants de patrimoine

svymean(~pat_immo_2021,dwtpanelcylindre_top1)

svymean(~pat_financier_2021,dwtpanelcylindre_top1)

svymean(~pat_pro_avec_entreprise_2021,dwtpanelcylindre_top1)

svymean(~patri_brut_hors_reste_2021,dwtpanelcylindre_top1)

svymean(~pat_net_2021,dwtpanelcylindre_top1)


#Revenus

svymean(~revenu_foncier_2021,na.rm = TRUE,dwtpanelcylindre_top1)

svymean(~revenu_financier_2021,na.rm = TRUE,dwtpanelcylindre_top1)

svymean(~revenu_declare_2021,na.rm = TRUE,dwtpanelcylindre_top1)


#Statistiques descriptives top 0.5% en panel 

#Composition du patrimoine

svymean(~part_pat_financier_2021,dwtpanelcylindre_top05)

svymean(~part_pat_immo_2021,dwtpanelcylindre_top05)

svymean(~part_pat_pro_avec_entreprise_2021,dwtpanelcylindre_top05)

#Montants de patrimoine

svymean(~pat_immo_2021,dwtpanelcylindre_top05)

svymean(~pat_financier_2021,dwtpanelcylindre_top05)

svymean(~pat_pro_avec_entreprise_2021,dwtpanelcylindre_top05)

svymean(~patri_brut_hors_reste_2021,dwtpanelcylindre_top05)

svymean(~pat_net_2021,dwtpanelcylindre_top05)


#Revenus

svymean(~revenu_foncier_2021,na.rm = TRUE,dwtpanelcylindre_top05)

svymean(~revenu_financier_2021,na.rm = TRUE,dwtpanelcylindre_top05)

svymean(~revenu_declare_2021,na.rm = TRUE,dwtpanelcylindre_top05)


################################################################################
#                               6.1. 2014                                      #
################################################################################


#Statistiques descriptives top 1% en panel

#Composition du patrimoine

svymean(~part_pat_financier_2014,dwtpanelcylindre_top1)

svymean(~part_pat_immo_2014,dwtpanelcylindre_top1)

svymean(~part_pat_pro_avec_entreprise_2014,dwtpanelcylindre_top1)

#Montants de patrimoine

svymean(~pat_immo_2014,dwtpanelcylindre_top1)

svymean(~pat_financier_2014,dwtpanelcylindre_top1)

svymean(~pat_pro_avec_entreprise_2014,dwtpanelcylindre_top1)

svymean(~patri_brut_hors_reste_2014,dwtpanelcylindre_top1)

svymean(~pat_net_2014,dwtpanelcylindre_top1)


#Revenus

svymean(~revenu_foncier_2014,na.rm = TRUE,dwtpanelcylindre_top1)

svymean(~revenu_financier_2014,na.rm = TRUE,dwtpanelcylindre_top1)

svymean(~revenu_declare_2014,na.rm = TRUE,dwtpanelcylindre_top1)


#Statistiques descriptives top 0.5% en panel 

#Composition du patrimoine

svymean(~part_pat_financier_2014,dwtpanelcylindre_top05)

svymean(~part_pat_immo_2014,dwtpanelcylindre_top05)

svymean(~part_pat_pro_avec_entreprise_2014,dwtpanelcylindre_top05)

#Montants de patrimoine

svymean(~pat_immo_2014,dwtpanelcylindre_top05)

svymean(~pat_financier_2014,dwtpanelcylindre_top05)

svymean(~pat_pro_avec_entreprise_2014,dwtpanelcylindre_top05)

svymean(~patri_brut_hors_reste_2014,dwtpanelcylindre_top05)

svymean(~pat_net_2014,dwtpanelcylindre_top05)


#Revenus

svymean(~revenu_foncier_2014,na.rm = TRUE,dwtpanelcylindre_top05)

svymean(~revenu_financier_2014,na.rm = TRUE,dwtpanelcylindre_top05)

svymean(~revenu_declare_2014,na.rm = TRUE,dwtpanelcylindre_top05)


