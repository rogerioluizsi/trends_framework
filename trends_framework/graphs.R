library(hrbrthemes)
library(viridis)
library(ggplot2)
library(dplyr)
library(envalysis)
library(ggcorrplot)
outputs<- read.csv("~/projects/panel/features_engineering/result_paperII.csv")
outputs<- read.csv("~/projects/BRACIS21/result_paperII.csv")


school_features = c(
  'IN_LABORATORIO_INFORMATICA',
  'IN_LABORATORIO_CIENCIAS',
  'IN_SALA_ATENDIMENTO_ESPECIAL',
  'IN_BIBLIOTECA',
  'IN_SALA_LEITURA',
  'IN_BANHEIRO',
  'IN_BANHEIRO_PNE',
  'QT_SALAS_UTILIZADAS',
  'QT_EQUIP_TV',
  'QT_EQUIP_DVD',
  'QT_EQUIP_COPIADORA',
  'QT_EQUIP_IMPRESSORA',
  'QT_COMP_ALUNO',
  'IN_BANDA_LARGA',
  'QT_FUNCIONARIOS',
  'IN_ALIMENTACAO',
  'IN_COMUM_MEDIO_MEDIO',
  'IN_COMUM_MEDIO_INTEGRADO',
  'IN_COMUM_MEDIO_NORMAL',
  'IN_SALA_PROFESSOR',
  'IN_COZINHA',
  'IN_EQUIP_PARABOLICA',
  'IN_QUADRA_ESPORTES',
  'IN_ATIV_COMPLEMENTAR',
  'QT_MATRICULAS' 
)

teacher_features = c('TITULACAO', 'IN_FORM_DOCENTE','NU_LICENCIADOS', 
                     'NU_CIENCIA_NATUREZA','NU_CIENCIAS_HUMANAS', 'NU_LINGUAGENS_CODIGOS', 'NU_MATEMATICA', 
                     'NU_ESCOLAS', 'DIVERSIDADE')

student_features =c('RENDA_PERCAPITA',  'EDU_PAI', 'EDU_MAE', 'NU_IDADE')


non_actionable_features = c('TP_COR_RACA_0.0', 'TP_COR_RACA_1.0',
                            'TP_COR_RACA_2.0', 'TP_COR_RACA_3.0', 'TP_COR_RACA_4.0',
                            'TP_COR_RACA_5.0',  'TP_SEXO')

labels = c(EDU_MAE = "Mother's education", EDU_PAI = "Father's education", RENDA_PERCAPITA = "Income (per capita)",
            TP_COR_RACA_1.0 = "White students", IN_BIBLIOTECA = "School library", DIVERSIDADE = "Faculty work overload", 
            IN_ATIV_COMPLEMENTAR = "Complementary activity", IN_EQUIP_PARABOLICA = "Satellite Dish",
            IN_FORM_DOCENTE = "Faculty adequate training", IN_LABORATORIO_CIENCIAS = "Science lab", 
            IN_SALA_ATENDIMENTO_ESPECIAL = "Special attendence room", IN_SALA_LEITURA ="Reading room", 
            NU_ESCOLAS = "Faculty jobs", QT_COMP_ALUNO = "Student's Computer", QT_EQUIP_COPIADORA = "Copy machine",
            QT_FUNCIONARIOS = "Number of employess", TITULACAO = "Faculty education", IN_BANHEIRO="Bathroom",
            IN_BANHEIRO_PNE ="Handicap bathroom",IN_QUADRA_ESPORTES = "Sport's court", 
            QT_EQUIP_DVD = "DVD player", QT_EQUIP_IMPRESSORA = "Printer", 
            TP_COR_RACA_2.0="Black students", TP_COR_RACA_3.0 = "Brown students", 
            TP_COR_RACA_5.0 = "Indigenous students", NU_CIENCIA_NATUREZA = "Natural Science faculty",
            NU_IDADE = "Student's age", QT_EQUIP_TV = "Television", 
            NU_CIENCIAS_HUMANAS = "Humanities faculty", NU_LINGUAGENS_CODIGOS = "Languages faculty", 
            IN_BANDA_LARGA="Fast Internet", NU_LICENCIADOS="Pedagogical Training",QT_MATRICULAS ="Enrollments",
            NU_MATEMATICA = "Math faculty", TP_COR_RACA_4.0="Yellow students", TP_SEXO = "Gender",
            QT_SALAS_UTILIZADAS = "Classrooms", IN_LABORATORIO_INFORMATICA = "Computers lab", IN_ALIMENTACAO = 'School lunch',
           IN_SALA_PROFESSOR = "Faculty room", IN_COZINHA = "School Kitchen")



#chosse the classifier (RandomForestClassifier or LogisticRegression)
#bluelow = #85a6f2
#bluehigh = #06297a
#greelow =#d1f2b6
#greehigh =#1d3d03

classifier = "RandomForestClassifier"
df<-outputs%>%filter(clas==classifier)%>%
  group_by(name, year)%>%
  summarise(mua=mean(max), mem= (mean(mem)), std=mean(std), aabsa=mean(ale_0))  

if (classifier == "LogisticRegression"){
  low <- "#85a6f2"
  high <- "#06297a"
} else {
  low <- "#d1f2b6"
  high <- "#1d3d03" 
}


#BoxPlot all features
plot1<- df%>% 
  ggplot(aes(name, mua))+ geom_boxplot(outlier.shape = NA) +geom_jitter(aes(colour = year)) +
  scale_colour_gradient(low = low, high = high)+
  scale_x_discrete(labels=labels) + geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+theme_publish()+
  theme(axis.text.x = element_text(angle = 0, hjust =1, color = 'gray15'),
        axis.text.y = element_text(angle = 0, hjust =1, color = 'gray15'),
        legend.position="right", 
        legend.title = element_text())+xlab("Features")+ylab("Max Uncentered ALE")+labs(colour="Years")
ggsave(filename = "mua_lr_all-variables.png", plot = plot1, dpi = 800,width = 8, height = 6)

#plot2<- df%>% 
#  ggplot(aes(name, std))+ geom_boxplot(outlier.shape = NA) +geom_jitter(aes(colour = year)) +
#  scale_colour_gradient(low = low, high = high)+
#  scale_x_discrete(labels=labels) + geom_hline(yintercept=0, linetype="dashed", color = "red")+
#  coord_flip()+theme_publish()+
#  theme(axis.text.x = element_text(angle = 0, hjust =1, color = 'gray15'),
#        axis.text.y = element_text(angle = 0, hjust =1, color = 'gray15'),
#        legend.position="right", 
#        legend.title = element_text())+xlab("Variables")+ylab("Max Uncentered ALE")+labs(colour="Years")

#ggsave(filename = "smua_rf_all-variables.png", plot = plot2, dpi = 400, width = 9, height = 7)

#Evolution of feature importance (MUA) of selected features set
selected = c('NU_CIENCIA_NATUREZA','NU_MATEMATICA','NU_LINGUAGENS_CODIGOS', 'NU_CIENCIAS_HUMANAS', 'DIVERSIDADE', 'IN_FORM_DOCENTE' )
selected = c('NU_MATEMATICA', 'DIVERSIDADE', 'NU_CIENCIAS_HUMANAS')
selected = c('NU_LINGUAGENS_CODIGOS', 'IN_FORM_DOCENTE')


plot3<-ggplot(df%>%filter(name %in% selected)%>%
         select(mua, year, name), aes(x=as.character(year), y=mua, group=name, color=name))+
  scale_color_viridis_d(labels = labels) + geom_point( size = 5)+geom_line()+geom_smooth(method=lm, se = FALSE)+ 
  guides(colour = guide_legend(override.aes = list(size = 5)))+theme_publish()+
  ylab("Max Uncentered ALE") +xlab("Years")+ ylim(-0.1, 0.2) + theme(legend.position="right", 
                                                                      legend.title = element_blank(), 
                                                                      text=element_text(size=18),
                                                                      #legend.key=element_rect(fill=NA),
                                                                      axis.text.x = element_text(color = 'gray15'),
                                                                      axis.text.y = element_text(color = 'gray15'))
ggsave(filename = "some_var_2.png", plot = plot3, dpi = 800, width = 8, height = 6)


##CORRELATION
corr<- df%>%group_by(name, year)%>%summarise(mua=mean(mua))%>%tidyr::pivot_wider(names_from = name, values_from = mua)%>%
  select(-year)


corr <- round(cor(corr%>%select(selected)), 1)
colnames(corr)<-c("Natural science faculty", "Math faculty",'Languages faculty',"Humanities faculty",
                 "Faculty work overload", "Faculty adequate training")
rownames(corr)<-c("Natural science faculty", "Math faculty",'Languages faculty',"Humanities faculty",
                  "Faculty work overload", "Faculty adequate training")
plot4<-ggcorrplot(corr, hc.order = FALSE, type = "lower",
           lab = TRUE)
plot4
ggsave(filename = "correlation.png", plot = plot4, dpi = 400, width = 8, height = 6)
  
p1<-df%>%filter(name %in% teacher_features)%>%
  select(name, mua,mem,aabsa,year)%>%mutate(grupo = "Teacher's Features")
p2<-df%>%filter(name %in% school_features)%>%
  select(name, mua,mem,aabsa,year)%>%mutate(grupo = "School's Features")
p3<-df%>%filter(name %in% student_features)%>%
  select(name, mua, mem,aabsa,year)%>%mutate(grupo = "Student's Features")
p4<-df%>%filter(name %in% non_actionable_features)%>%
  select(name, mua,mem, aabsa,year)%>%mutate(grupo = "Non-actionable features")
histo<- rbind(p1,p2,p3,p4)



plot5<-histo%>%ggplot(aes(x=grupo, y =mem, color=grupo))+geom_boxplot()+geom_jitter()+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + theme_publish()+
  xlab("")+ylab(" UAS (at average)")+theme(legend.title = element_blank(),
                                                  text=element_text(size=18),
                                                  axis.text.x = element_text(color = 'gray15'),
                                                  axis.text.y = element_text(color = 'gray15'))


ggsave(filename = "histo.png", plot = plot5, dpi = 800, width = 8, height = 6)
                                                  #legend.text = element_text(size = 12))
#+
 # ggtitle(classifier)

#set thresold to better vizualisation
min = quantile(histo$mem)[[2]]
max = quantile(histo$mem)[[4]]



histo%>%filter(abs(mem) >=0.01)%>%
  ggplot(aes(x=mem, color=grupo))+geom_density(alpha=0.6)+
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + theme_publish()+
  xlab("Max Uncentered ALE - MUA")+ylab("Density")+theme(legend.title = element_blank(),
                                                  text=element_text(size=12),
                                                  axis.text.x = element_text(size = 12),
                                                  axis.text.y = element_text(size = 12),
                                                  legend.text = element_text(size = 12))+
  geom_vline(xintercept=0, linetype="dashed", color = "red")+ 
  guides(colour = guide_legend(override.aes = list(size = 10)))+
  ggtitle(classifier)

histo%>%filter(mem >= min & mem<= max)%>%group_by(grupo)%>%count()

#filtering the average top 4 features by group for beteter balanced
top_4<-histo%>%filter(abs(mem)>0.01)%>%
  group_by(grupo)%>%arrange(desc(abs(mem)))%>%do(tail(., n=39))

top_4%>%ggplot(aes(x=mem, color=grupo))+geom_density(alpha=0.6)+
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + theme_publish()+
  xlab("Max Uncentered ALE - MUA")+ylab("Density")+theme(legend.title = element_blank(),
                                                         text=element_text(size=20),
                                                         axis.text.x = element_text(size = 20),
                                                         axis.text.y = element_text(size = 20),
                                                         legend.text = element_text(size = 20))+
  geom_vline(xintercept=0, linetype="dashed", color = "red")+ 
  guides(colour = guide_legend(override.aes = list(size = 10)))+
  ggtitle(classifier)

track_importance<-histo%>%group_by(year)%>%arrange(desc(aabsa))%>%do(head(., n=10))%>%
  group_by(year, grupo)%>%mutate(n = n()/10)
ggplot(data=track_importance, aes(x=as.character(year), group=grupo, fill=grupo)) +
  geom_density(adjust=0.2,position="fill") +
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE)+
  theme_publish()+xlab(element_blank())+ylab("Fraction")+theme(legend.title = element_blank(),
                                                               text=element_text(size=12),
                                                               axis.text.x = element_text(color = 'gray15'),
                                                               axis.text.y = element_text(color = 'gray15'))
                                                               #legend.text = element_text(size = 12)) +
  ggtitle(classifier)  

aucs<-outputs%>%group_by(year, clas)%>%summarise(AUC = mean(auc), MAX_KS2 = mean(ks2_max))%>%distinct()%>%ungroup()
aucs<- aucs%>%tidyr::gather(metric, value, MAX_KS2, AUC)
aucs$clas<- if_else(aucs$clas=="LogisticRegression", "Logistic Regression", "Random Forest")
#Performance of Models based on AUC
plot7<-ggplot(aucs, aes(as.character(year), value, shape=metric, linetype=clas, group=interaction(metric, clas)))+
  geom_point(size=3) +geom_line()+ylab("Evaluation Metrics")+ ylim(0.5, 1)+theme_publish()+
  theme(legend.title = element_blank(), legend.text=element_text())+ xlab(element_blank())+
  theme(legend.title = element_blank(),
        legend.text=element_text(),
        #axis.title.y = element_text(size = 20),
        axis.text.x = element_text(color = 'gray15'),
        axis.text.y = element_text(color = 'gray15'),
        legend.spacing.y = unit(1, 'cm'))

ggsave(filename = "aucs.png", plot = plot7, dpi = 400, width = 8, height = 6)


#t-testggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) +
geom_area(position = "fill", colour = "black", size = .2, alpha = .4) +
  scale_fill_brewer(palette = "Blues")


ggplot(track_importance, aes(fill=grupo, y=n, x=as.character(year))) + 
  geom_bar(position="stack", stat="identity")


ggplot(track_importance, aes(x = year, y = freq, fill = grupo)) +
  geom_col(position = "fill")

ggplot(track_importance, aes(x = year, y = n, fill = grupo)) +
  geom_area(position = "fill")

##
https://r-graphics.org/recipe-line-graph-stacked-area


for (i in colnames(df1)){
  if (!is.na(labels[i])){
    print(labels[[i]])
    print(i)
    df1<-df1%>%rename(!!labels[[i]]:=i)
  }
}
