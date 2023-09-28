#Script pour régression sur patrimoine de départ et patrimoine d arrivee

#base_entreprise_2017<- subset.data.frame(base_entreprise_2017, select=c(IDENT, INST, MTENTREP ))

panel_cylindre_min<-panel_cylindre[panel_cylindre$pat_net_2014>500000, ]
dwtpanelcylindre_min <- svydesign(ids = ~1, data = panel_cylindre_min, weights = ~ panel_cylindre_min$ponderation_longitudinal_indiv_2017)
svymean(~age_personne_reference_2014,dwtpanelcylindre_top05)

svymean(~pat_immo_2014,dwtpanelcylindre)
svymean(~pat_pro_avec_entreprise_2014,dwtpanelcylindre)
svymean(~pat_net_2014,dwtpanelcylindre)
svymean(~pat_financier_2014,dwtpanelcylindre)
svymean(~pat_immo_2017,dwtpanelcylindre)
svymean(~pat_pro_avec_entreprise_2017,dwtpanelcylindre)
svymean(~pat_net_2017,dwtpanelcylindre)
svymean(~pat_financier_2017,dwtpanelcylindre)
svymean(~pat_immo_2021,dwtpanelcylindre)
svymean(~pat_pro_avec_entreprise_2021,dwtpanelcylindre)
svymean(~pat_net_2021,dwtpanelcylindre)
svymean(~pat_financier_2021,dwtpanelcylindre)



panel_cylindre_2014<- subset.data.frame(panel_cylindre, select = c(age_personne_reference_2014, pat_net_2014, pat_net_corr_2017, ponderation_longitudinal_indiv_2017_bis_2021, indic_donnation_faite_2017, indic_donnation_recue_2017))
panel_cylindre_2017<- subset.data.frame(panel_cylindre, select = c(age_personne_reference_2017, pat_net_2021, pat_net_corr_2017, ponderation_longitudinal_indiv_2017_bis_2021, indic_donnation_faite_2021, indic_donnation_recue_2021))
setnames(panel_cylindre_2014, c("age_depart", "pat_D", "pat_A", "pond", "donnation_faite", "donnation_recue"))
setnames(panel_cylindre_2017, c("age_depart", "pat_A", "pat_D", "pond", "donnation_faite", "donnation_recue"))
panel_cylindre_2014$indic_cohorte_2014<-1
panel_cylindre_2014$indic_cohorte_2017<-0
panel_cylindre_2017$indic_cohorte_2014<-0
panel_cylindre_2017$indic_cohorte_2017<-1
panel_pat_A_D<-rbind(panel_cylindre_2014, panel_cylindre_2017)
panel_pat_A_D<-panel_pat_A_D[panel_pat_A_D$pat_D>500000, ]
summary(panel_pat_A_D$pat_D)

#Autre version à partir d'un panel par cohorte 2014/2017 et 2017/2021 et non pas par panel total sous-échantilloné
merge_individu_2014_2017<- dplyr::inner_join(base_individu_2017, base_individu_2014, by="identifiant_longitudinal_individuel")
merge_individu_2017_2021<-dplyr::inner_join(base_individu_2017, base_individu_2021, by="identifiant_longitudinal_individuel")
pat_A_D_2014_2017<-dplyr::left_join(merge_individu_2014_2017, base_2017, by="ident_2017")
pat_A_D_2014_2017<-dplyr::left_join(pat_A_D_2014_2017, base_2014, by="ident_2014")
pat_A_D_2017_2021<-dplyr::left_join(merge_individu_2017_2021, base_2017, by="ident_2017")
pat_A_D_2017_2021<-dplyr::left_join(pat_A_D_2017_2021, base_2021, by="ident_2021")

pat_A_D_2014_2017<-pat_A_D_2014_2017[pat_A_D_2014_2017$pat_net_2014>500000, ]
pat_A_D_2017_2021<-pat_A_D_2017_2021[pat_A_D_2017_2021$pat_net_corr_2017>500000, ]


pat_A_D_2014_2017<- subset.data.frame(pat_A_D_2014_2017, select = c(age_personne_reference_2014, pat_net_2014, pat_net_corr_2017, ponderation_longitudinal_indiv_2017, indic_donnation_faite_2017, indic_donnation_recue_2017))
pat_A_D_2017_2021<- subset.data.frame(pat_A_D_2017_2021, select = c(age_personne_reference_2017, pat_net_2021, pat_net_corr_2017, ponderation_longitudinal_indiv_2017_bis_2021, indic_donnation_faite_2021, indic_donnation_recue_2021))
setnames(pat_A_D_2014_2017, c("age_depart", "pat_D", "pat_A", "pond", "donnation_faite", "donnation_recue"))
setnames(pat_A_D_2017_2021, c("age_depart", "pat_A", "pat_D", "pond", "donnation_faite", "donnation_recue"))
pat_A_D_2014_2017$indic_cohorte_2014<-1
pat_A_D_2014_2017$indic_cohorte_2017<-0
pat_A_D_2017_2021$indic_cohorte_2014<-0
pat_A_D_2017_2021$indic_cohorte_2017<-1

panel_pat_A_D$pat_D_reel<-panel_pat_A_D$pat_D/svymean(~pat_net_2014,dwtpanelcylindre)
panel_pat_A_D$pat_A_reel<-panel_pat_A_D$pat_A/svymean(~pat_net_2017,dwtpanelcylindre)

###########Bloc de complement pour reg Clément (panel sans top 1% et reg quantile 0.5)#################

dwtpanelAD_top_1 <- svydesign(ids = ~1, data = panel_pat_A_D, weights = ~ panel_pat_A_D$pond)
svyquantile(~pat_D,dwtpanelAD_top_1, quantiles=0.99, ci=FALSE)
svyquantile(~pat_D,dwtpanelAD_top1, quantiles=0.01, ci=FALSE)
panel_A_D_hors_top_1<-panel_pat_A_D[panel_pat_A_D$pat_D<7776835, ]

panel_pat_A_D$ratio_evol <- panel_pat_A_D$pat_A/panel_pat_A_D$pat_D
panel_pat_A_D$ratio_evol_2 <- panel_pat_A_D$pat_A_reel/panel_pat_A_D$pat_D_reel

library(reldist)
panel_pat_A_D_wins <- panel_pat_A_D[which(panel_pat_A_D$ratio_evol>wtd.quantile(panel_pat_A_D$ratio_evol,.01,weights=panel_pat_A_D$pond)
                                          & panel_pat_A_D$ratio_evol<wtd.quantile(panel_pat_A_D$ratio_evol,.99,weights=panel_pat_A_D$pond)),]

reg_hors_top <- lm(data = panel_pat_A_D_wins,ratio_evol_2 ~
           pat_D + age_depart + indic_cohorte_2017)
summary(reg_hors_top)

reg_quant<-rq(data = panel_pat_A_D,ratio_evol_2 ~
                pat_D + age_depart + indic_cohorte_2017 , weights = pond, tau = 0.5)

summary(reg_quant)
library(quantreg)
#########################################

panel_pat_A_D_bis<-rbind(pat_A_D_2014_2017, pat_A_D_2017_2021)


#Reg (avec et sans donation)
reg <- lm(data = panel_pat_A_D,pat_A_reel ~
           pat_D_reel + age_depart + indic_cohorte_2017 , weights = pond )
summary(reg)

reg_avec_donation <- lm(data = panel_pat_A_D_bis,pat_A ~
            pat_D + age_depart + indic_cohorte_2017 + donnation_faite + donnation_recue , weights = pond )
summary(reg_avec_donation)

#Reg proba de faire une donation pare cohorte 2014/2017

#On recode la variable "donation_faite" pour que ses valeurs soient entre 0 et 1

summary(panel_pat_A_D_bis$donnation_faite)

panel_pat_A_D_bis$donnation_faite[panel_pat_A_D_bis$donnation_faite==2]<-0
panel_pat_A_D$donnation_faite[panel_pat_A_D$donnation_faite==2]<-0

summary(panel_pat_A_D_bis$donnation_faite)

#Reg
#reg_donation<-probitmfx(donnation_faite ~ pat_D + age_depart + indic_cohorte_2017, data=panel_pat_A_D_bis, weights = pond) )

reg_donation<-glm(donnation_faite ~ pat_D + age_depart + indic_cohorte_2017, data=panel_pat_A_D_bis, family = binomial(link = probit), weights = pond )
summary(reg_donation)

#reg_donation_panel_cylindre<-glm(donnation_faite ~ pat_D + age_depart + indic_cohorte_2017, data=panel_pat_A_D, family = binomial(link = probit), weights = pond )
#summary(reg_donation_panel_cylindre)