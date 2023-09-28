##################################################################################################
##############################    IV. FUSION DES BASES        ####################################
##################################################################################################


#Base generale panelisee

merge_individu<- dplyr::inner_join(base_individu_2017, base_individu_2014, by="identifiant_longitudinal_individuel")

merge_individu<-dplyr::inner_join(merge_individu, base_individu_2021,  by="identifiant_longitudinal_individuel")

merge_2017<-dplyr::left_join(merge_individu, base_2017, by="ident_2017")

merge_total<-dplyr::left_join(merge_2017, base_2014, by="ident_2014")

merge_total<-dplyr::left_join(merge_total, base_2021, by="ident_2021")

panel_cylindre<-merge_total[merge_total$couple_stable_2017==1 & merge_total$couple_stable_2021==1,]

panel_cylindre$tranche_age<-"20-30"
panel_cylindre$tranche_age[panel_cylindre$age_personne_reference_2014>=30 & panel_cylindre$age_personne_reference_2014<40]<-"30-40"
panel_cylindre$tranche_age[panel_cylindre$age_personne_reference_2014>=40 & panel_cylindre$age_personne_reference_2014<50]<-"40-50"
panel_cylindre$tranche_age[panel_cylindre$age_personne_reference_2014>=50 & panel_cylindre$age_personne_reference_2014<60]<-"50-60"
panel_cylindre$tranche_age[panel_cylindre$age_personne_reference_2014>=60 & panel_cylindre$age_personne_reference_2014<70]<-"60-70"
panel_cylindre$tranche_age[panel_cylindre$age_personne_reference_2014>=70 ]<-"plus de 70"


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

dwtpanelcylindre <- svydesign(ids = ~1, data = panel_cylindre, weights = ~ panel_cylindre$ponderation_longitudinal_indiv_2017_bis_2021)

#Base panelisee du top05

panel_cylindre_top05<-panel_cylindre[panel_cylindre$indic_top05_brut_corr_2014==1,]
dwtpanelcylindre_top05 <- svydesign(ids = ~1, data = panel_cylindre_top05, weights = ~ panel_cylindre_top05$ponderation_longitudinal_indiv_2017_bis_2021)

#Base panelisee du top1

panel_cylindre_top1<-panel_cylindre[panel_cylindre$indic_top1_brut_corr_2014==1,]
dwtpanelcylindre_top1 <- svydesign(ids = ~1, data = panel_cylindre_top1, weights = ~ panel_cylindre_top1$ponderation_longitudinal_indiv_2017_bis_2021)




