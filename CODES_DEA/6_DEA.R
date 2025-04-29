### ANALYZA LONG pozicia
# DEA
Y <- rbind(good.agr(long1[1:3], 0.05),
           good.agr(long2.5[1:3], 0.05),
           good.agr(long5[1:3], 0.05))

B <- rbind(apply(long1[[7]],1, sum ),
           apply(long2.5[[7]],1, sum ),
           apply(long5[[7]],1, sum ))


B <- t(apply(B,1,function(z) z/max(z)))
Y <- t(apply(Y,1,function(z) z/max(z)))

B <- round(B, digits = 5)
Y <- round(Y, digits = 5)

efektivity.out.both.l.2 <- c()
for(j in 1:28){
  print(j)
  efektivity.out.both.l.2 <- rbind(efektivity.out.both.l.2,RusselDEA.out.both(j))
}
rownames(efektivity.out.both.l.2) <- colnames(Y)


cbind(efektivity.out.both.l.2[,1],efektivity.out.both.l.2_new[,1])

# for(i in 1:7){
  
#   efekt.df <- data.frame(modely = colnames(Y),
#                          efektivity = efektivity.out.both.l.2[,i])
  
#   # vykreslenie Hyperbolic vs Russel 
#   g <-  ggplot(data=efekt.df, aes(x=modely, y=efektivity)) +
#     geom_bar(stat="identity", color="black", position=position_dodge())+
#     theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 15),
#           axis.text.y = element_text(size = 22),
#           axis.title.x = element_text(size = 22),
#           axis.title.y = element_text(size = 22),
#           legend.text = element_text(size=20))+
#     ggtitle(colnames(efektivity.out.both.l.2)[i]) 
  
  
#   print(g)
# }

# efektvine vzory
eff.image.l.out.2 <- c()
for(j in 1:28){
  print(j)
  eff.image.l.out.2 <- cbind(eff.image.l.out.2,rbind(Y,B)%*%efektivity.out.both.l.2[j,8:35])
}
rownames(eff.image.l.out.2) <- c("stat_1","stat_2.5","stat_5","Loss_1","Loss_2","Loss_3")
colnames(eff.image.l.out.2) <- colnames(Y)


# surplas
surplas.out.both.l.2 <- eff.image.l.out.2 - rbind(Y,B)
rownames(surplas.out.both.l.2) <- c("stat_1","stat_2.5","stat_5","Loss_1","Loss_2","Loss_3")
colnames(surplas.out.both.l.2) <- colnames(Y)


# superefektivita pre efektivne 
seff.out.both.l.2 <- c()
for(j in c(9,23))
{
  print(j)
  seff.out.both.l.2 <- rbind(seff.out.both.l.2,supeff.out.both(j))
}

rownames(seff.out.both.l.2) <- colnames(Y)[c(9,23)]


# kolko krat boli sucast ref mnoziny
cnt.ref.out.both.l.2 <- apply(efektivity.out.both.l.2[,8:35], 2, function(x) sum(x != 0))
names(cnt.ref.out.both.l.2) <- colnames(Y)
cnt.ref.out.both.l.2 <- cnt.ref.out.both.l.2[c(9,23)]


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


efektivity.out.both.s.2 <- c()
for(j in 1:28)
{
  print(j)
  efektivity.out.both.s.2 <- rbind(efektivity.out.both.s.2,RusselDEA.out.both(j))
}

rownames(efektivity.out.both.s.2) <- colnames(Y)

# for(i in 1:7)
# {
  
#   efekt.df <- data.frame(modely = colnames(Y),
#                          efektivity = efektivity.out.both.s.2[,i])
  
#   # vykreslenie Hyperbolic vs Russel 
#   g <-  ggplot(data=efekt.df, aes(x=modely, y=efektivity)) +
#     geom_bar(stat="identity", color="black", position=position_dodge())+
#     theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 15),
#           axis.text.y = element_text(size = 22),
#           axis.title.x = element_text(size = 22),
#           axis.title.y = element_text(size = 22),
#           legend.text = element_text(size=20))+
#     ggtitle(colnames(efektivity.out.both.s.2)[i]) 
  
  
#   print(g)
# }

# efektvine vzory
eff.image.s.both.2 <- c()
for(j in 1:28)
{
  print(j)
  eff.image.s.both.2 <- cbind(eff.image.s.both.2,rbind(Y,B)%*%efektivity.out.both.s.2[j,8:35])
}
rownames(eff.image.s.both.2) <- c("stat_1","stat_2.5","stat_5","Loss_1","Loss_2","Loss_3")
colnames(eff.image.s.both.2) <- colnames(Y)


# surplas
surplas.out.both.s.2 <- eff.image.s.both.2 - rbind(Y,B)
rownames(surplas.out.both.s.2) <- c("stat_1","stat_2.5","stat_5","Loss_1","Loss_2","Loss_3")
colnames(surplas.out.both.s.2) <- colnames(Y)


apply(efektivity.out.both.s.2[,8:35],2,function(x) sum(x != 0))

# superefektivita pre efektivne 
seff.out.both.s.2 <- c()
for(j in c(1,4,5,8,12,15,18,19,25,26))
{
  print(j)
  seff.out.both.s.2 <- rbind(seff.out.both.s.2,supeff.out.both(j))
}

rownames(seff.out.both.s.2) <- colnames(Y)[c(1,4,5,8,12,15,18,19,25,26)]

# kolko krat boli sucast ref mnoziny
cnt.ref.out.both.s.2 <- apply(efektivity.out.both.s.2[,8:35], 2, function(x) sum(x != 0))
names(cnt.ref.out.both.s.2) <- colnames(Y)
cnt.ref.out.both.s.2 <- cnt.ref.out.both.s.2[c(1,4,5,8,12,15,18,19,25,26)]



##### DOLEZITE POROVNANIA
efekt.df1 <- data.frame(modely = colnames(Y),
                       efektivity = (efektivity.out.both.l[,3] + efektivity.out.both.l[,6])/2 )

# vykreslenie Hyperbolic vs Russel 
g <-  ggplot(data=efekt.df, aes(x=modely, y=efektivity)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 15),
        axis.text.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        legend.text = element_text(size=20))+
  ggtitle(colnames(efektivity.out.both.l)[i]) 
print(g)

efekt.df2 <- data.frame(modely = colnames(Y),
                       efektivity = efektivity.out.only.l[,3])

# vykreslenie Hyperbolic vs Russel 
g <-  ggplot(data=efekt.df, aes(x=modely, y=efektivity)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 15),
        axis.text.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        legend.text = element_text(size=20))+
  ggtitle(colnames(efektivity.out.only.l)[i]) 
print(g)


efektivity.out.both.s[,c(3,6)]



## vykreslovanie moznost 1
ggplot(mapping = aes(x=modely, y=efektivity)) +
  geom_bar(data = efekt.df2, width = 0.8, stat = 'identity', fill = 'green', alpha = 0.5) +
  geom_bar(data = efekt.df1, width = 0.8, stat = 'identity', fill = 'orange', alpha = 0.5) +
  #theme_classic() + scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 15),
        axis.text.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        legend.text = element_text(size=20))
