
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

panel_cylindre$difference_taux_2017<-panel_cylindre$taux_imposition_isf_2017 - panel_cylindre$taux_imposition_ifi_2017

taux <- ddply(panel_cylindre, ~panel_cylindre$tranche_pat,function(panel_cylindre) wtd.quantile(panel_cylindre$difference_taux_2017,w=panel_cylindre$ponderation_2017_trans,probs=c(.1,.25,.75,.9)) )
taux
pisf <- panel_cylindre

pisf$intensite <- 0
pisf$intensite[pisf$difference_taux_2017>=0.001] <- 1

pisf$benef_IFI <- 0
pisf$benef_IFI[pisf$difference_taux_2017>0] <- 1


