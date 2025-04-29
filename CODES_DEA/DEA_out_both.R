### ANALYZA LONG pozicia
# DEA
Y <- rbind(good.agr(long1[1:3], 0.05),
           good.agr(long2.5[1:3], 0.05),
           good.agr(long5[1:3], 0.05))

B <- rbind(apply(long1[[7]],1, sum ),
           apply(long2.5[[7]],1, sum ),
           apply(long5[[7]],1, sum ))
names(long1)

B <- t(apply(B,1,function(z) z/max(z)))
Y <- t(apply(Y,1,function(z) z/max(z)))

B <- round(B, digits = 5)
Y <- round(Y, digits = 5)

efektivity.out.both.l <- c()
for(j in 1:28)
{
  print(j)
  efektivity.out.both.l <- rbind(efektivity.out.both.l,RusselDEA.out.both(j))
}

rownames(efektivity.out.both.l) <- colnames(Y)
names_mod <- c("overall",
               "Partial statistical efficiency at VaR99%",
               "Partial statistical efficiency at VaR97.5%",
               "Partial statistical efficiency at VaR95%",
               "Partial loss efficiency at VaR99%",
               "Partial loss efficiency at VaR97.5%",
               "Partial loss efficiency at VaR95%")
# for(i in 1:7)
# {
  
#   efekt.df <- data.frame(Models = colnames(Y),
#                          Efficiency = efektivity.out.both.l[,i])
  
#   # vykreslenie Hyperbolic vs Russel 
#   g <-  ggplot(data=efekt.df, aes(x=Models, y=Efficiency)) +
#     geom_bar(stat="identity", color="black", position=position_dodge())+
#     theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 15),
#           axis.text.y = element_text(size = 22),
#           axis.title.x = element_text(size = 22),
#           axis.title.y = element_text(size = 22),
#           legend.text = element_text(size=20))+
#     ggtitle(names_mod[i]) +
#     theme(plot.title = element_text(hjust = 0.5))
  
  
#   print(g)
# }

names_mod2 <- c("Overall partial efficiency at VaR99%",
                "Overall partial efficiency at VaR97.5%",
                "Overall partial efficiency at VaR95%")
# for(i in 1:3)
# {
  
#   efekt.df <- data.frame(Models = colnames(Y),
#                          Efficiency = (efektivity.out.both.l[,i+1] +
#                                          efektivity.out.both.l[,i+4])/2)
  
#   # vykreslenie Hyperbolic vs Russel 
#   g <-  ggplot(data=efekt.df, aes(x=Models, y=Efficiency)) +
#     geom_bar(stat="identity", color="black", position=position_dodge())+
#     theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 15),
#           axis.text.y = element_text(size = 22),
#           axis.title.x = element_text(size = 22),
#           axis.title.y = element_text(size = 22),
#           legend.text = element_text(size=20))+
#     ggtitle(names_mod2[i]) +
#     theme(plot.title = element_text(hjust = 0.5))
  
  
#   print(g)
# }


# efektvine vzory
eff.image.l.out <- c()
for(j in 1:28)
{
  print(j)
  eff.image.l.out <- cbind(eff.image.l.out,rbind(Y,B)%*%efektivity.out.both.l[j,8:35])
}
rownames(eff.image.l.out) <- c("stat_1","stat_2.5","stat_5","Loss_1","Loss_2","Loss_3")
colnames(eff.image.l.out) <- colnames(Y)


# surplas
surplas.out.both.l <- eff.image.l.out - rbind(Y,B)
rownames(surplas.out.both.l) <- c("stat_1","stat_2.5","stat_5","Loss_1","Loss_2","Loss_3")
colnames(surplas.out.both.l) <- colnames(Y)



write.csv(surplas.out.both.l,"surplas")


ind <- which(round(efektivity.out.both.l[,1],digits = 5) == 1)


# superefektivita pre efektivne 
seff.out.both.l <- c()
for(j in ind)
{
  print(j)
  seff.out.both.l <- rbind(seff.out.both.l,supeff.out.both(j))
}

rownames(seff.out.both.l) <- colnames(Y)[ind]


# kolko krat boli sucast ref mnoziny
cnt.ref.out.both.l <- apply(efektivity.out.both.l[,8:35], 2, function(x) sum(x != 0))
names(cnt.ref.out.both.l) <- colnames(Y)
cnt.ref.out.both.l <- cnt.ref.out.both.l[ind]


write.csv(cbind(seff.out.both.l,cnt.ref.out.both.l),"super_eff")

##########################################################################################
### ANALYZA SHORT pozicia


Y <- rbind(good.agr(short1[1:3], 0.05),
           good.agr(short2.5[1:3], 0.05),
           good.agr(short5[1:3], 0.05))

B <- rbind(apply(short1[[7]],1, sum ),
           apply(short2.5[[7]],1, sum ),
           apply(short5[[7]],1, sum ))



B <- t(apply(B,1,function(z) z/max(z)))
Y <- t(apply(Y,1,function(z) z/max(z)))

B <- round(B, digits = 5)
Y <- round(Y, digits = 5)


efektivity.out.both.s <- c()
for(j in 1:28)
{
  print(j)
  efektivity.out.both.s <- rbind(efektivity.out.both.s,RusselDEA.out.both(j))
}

rownames(efektivity.out.both.s) <- colnames(Y)


# for(i in 1:7)
# {
  
#   efekt.df <- data.frame(Models = colnames(Y),
#                          Efficiency = efektivity.out.both.s[,i])
  
#   # vykreslenie Hyperbolic vs Russel 
#   g <-  ggplot(data=efekt.df, aes(x=Models, y=Efficiency)) +
#     geom_bar(stat="identity", color="black", position=position_dodge())+
#     theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 15),
#           axis.text.y = element_text(size = 22),
#           axis.title.x = element_text(size = 22),
#           axis.title.y = element_text(size = 22),
#           legend.text = element_text(size=20))+
#     ggtitle(names_mod[i])+
#     theme(plot.title = element_text(hjust = 0.5)) 
  
  
#   print(g)
# }

# for(i in 1:3)
# {
  
#   efekt.df <- data.frame(Models = colnames(Y),
#                          Efficiency = (efektivity.out.both.s[,i+1] +
#                                          efektivity.out.both.s[,i+4])/2)
  
#   # vykreslenie Hyperbolic vs Russel 
#   g <-  ggplot(data=efekt.df, aes(x=Models, y=Efficiency)) +
#     geom_bar(stat="identity", color="black", position=position_dodge())+
#     theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 15),
#           axis.text.y = element_text(size = 22),
#           axis.title.x = element_text(size = 22),
#           axis.title.y = element_text(size = 22),
#           legend.text = element_text(size=20))+
#     ggtitle(names_mod2[i])+
#     theme(plot.title = element_text(hjust = 0.5)) 
  
  
#   print(g)
# }


# efektvine vzory
eff.image.s.both <- c()
for(j in 1:28)
{
  print(j)
  eff.image.s.both <- cbind(eff.image.s.both,rbind(Y,B)%*%efektivity.out.both.s[j,8:35])
}
rownames(eff.image.s.both) <- c("stat_1","stat_2.5","stat_5","Loss_1","Loss_2","Loss_3")
colnames(eff.image.s.both) <- colnames(Y)


# surplas
surplas.out.both.s <- eff.image.s.both - rbind(Y,B)
rownames(surplas.out.both.s) <- c("stat_1","stat_2.5","stat_5","Loss_1","Loss_2","Loss_3")
colnames(surplas.out.both.s) <- colnames(Y)


write.csv(surplas.out.both.s,"surplas")


ind <- which(round(efektivity.out.both.s[,1],digits = 5) == 1)



# superefektivita pre efektivne 
seff.out.both.s <- c()
for(j in ind)
{
  print(j)
  seff.out.both.s <- rbind(seff.out.both.s,supeff.out.both(j))
}

rownames(seff.out.both.s) <- colnames(Y)[ind]

# kolko krat boli sucast ref mnoziny
cnt.ref.out.both.s <- apply(efektivity.out.both.s[,8:35], 2, function(x) sum(x != 0))
names(cnt.ref.out.both.s) <- colnames(Y)
cnt.ref.out.both.s <- cnt.ref.out.both.s[ind]

write.csv(cbind(seff.out.both.s,cnt.ref.out.both.s),"super_eff")

#long-table
long.tab <- efektivity.out.both.l[,1:7]
long.sup <- as.data.frame(cbind(seff.out.both.l,cnt.ref.out.both.l))


long.tab <- cbind(signif(long.tab, digits = 3),signif(long.sup, digits = 4)[match(rownames(long.tab),
                                                                                     rownames(long.sup)),]) 
colnames(long.tab) <- c("Eff","Parc1","Parc2","Parc3","Parc11","Parc22","Parc33",
                        "Supeff","Supparc1","Supparc2","Supparc3",
                        "Supparc11","Supparc22","Supparc33",
                        "#image")

long.tab$nazvy <- rownames(long.tab)
rownames(long.tab) <- 1:28

long.tab <- long.tab[c(c(1,8,15,22),
                         3 + c(1,8,15,22),
                         4 + c(1,8,15,22),
                         5 + c(1,8,15,22),
                         2 + c(1,8,15,22),
                         6 + c(1,8,15,22),
                         1 + c(1,8,15,22)),]
rownames(long.tab) <- 1:28


long.tab$nazvy <- NULL

latex(long.tab, file="Dea_long.tex",cgroup = c("Efficiency", "Supper-efficiency",""),
      n.cgroup = c(7,7,1),
      rgroup = c("GARCH", "iGARCH","fiGARCH","csGARCH","eGARCH","tGARCH","gjrGARCH"),
      n.rgroup = c(4,4,4,4,4,4,4),
      rowname = rep(c("Norm","sNorm","Std","sStd"),7),
      landscape = TRUE, na.blank = FALSE, na = 2,booktabs=TRUE,
      colheads = c("Eff","@ 1\\%","@ 2.5\\%","@ 5\\%","@ 1\\%","@ 2.5\\%","@ 5\\%","SEff","@ 1\\%",
                   "@ 2.5\\%","@ 5\\%","@ 1\\%","@ 2.5\\%","@ 5\\%","\\#EI"),
      extracolheads = c("","","Statistical","","","Loss function","","","","Statistical","","",
                        "Loss function","",""))

#short-table
short.tab <- efektivity.out.both.s[,1:7]
short.sup <- as.data.frame(cbind(seff.out.both.s,cnt.ref.out.both.s))


short.tab <- cbind(signif(short.tab, digits = 3),signif(short.sup, digits = 4)[match(rownames(short.tab),
                                                                                     rownames(short.sup)),]) 
colnames(short.tab) <- c("Eff","Parc1","Parc2","Parc3","Parc11","Parc22","Parc33",
                         "Supeff","Supparc1","Supparc2","Supparc3",
                         "Supparc11","Supparc22","Supparc33",
                         "#image")


short.tab$nazvy <- rownames(short.tab)
rownames(short.tab) <- 1:28

short.tab <- short.tab[c(c(1,8,15,22),
           3 + c(1,8,15,22),
           4 + c(1,8,15,22),
           5 + c(1,8,15,22),
           2 + c(1,8,15,22),
           6 + c(1,8,15,22),
           1 + c(1,8,15,22)),]
rownames(short.tab) <- 1:28


short.tab$nazvy <- NULL

#library(Hmisc)

latex(short.tab, file='my.tex',cgroup = c("eff","parc","suppeff","parcsup","image"),
      n.cgroup = c(1,6,1,6,1),
      rgroup = c("GARCH", "iGARCH","fiGARCH","csGARCH","eGARCH","tGARCH","gjrGARCH"),
      n.rgroup = c(4,4,4,4,4,4,4),
      rowname = rep(c("Norm","sNorm","Std","sStd"),7))

latex(short.tab, file="Dea_short.tex",cgroup = c("Efficiency", "Supper-efficiency",""),
      n.cgroup = c(7,7,1),
      rgroup = c("GARCH", "iGARCH","fiGARCH","csGARCH","eGARCH","tGARCH","gjrGARCH"),
      n.rgroup = c(4,4,4,4,4,4,4),
      rowname = rep(c("Norm","sNorm","Std","sStd"),7),
      landscape = TRUE, na.blank = FALSE, na = 2,booktabs=TRUE,
      colheads = c("Eff","@ 1\\%","@ 2.5\\%","@ 5\\%","@ 1\\%","@ 2.5\\%","@ 5\\%","SEff","@ 1\\%",
                       "@ 2.5\\%","@ 5\\%","@ 1\\%","@ 2.5\\%","@ 5\\%","\\#EI"),
      extracolheads = c("","","Statistical","","","Loss function","","","","Statistical","","",
                        "Loss function","",""))


