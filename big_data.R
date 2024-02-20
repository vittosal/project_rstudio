library(readxl)
library(sjPlot) 

data <- read_excel("C:/Users/atlas/Downloads/Inserimento laureati 2015 ridotto.xlsx")
data<-data[!is.na(data$`Regione ateneo`),]

str(data)
names(data)

table(data$`Situazione lavorativa`)
barplot(table(data$`Voto di laurea conseguito`))
table(data$`GRUPPO di Laurea`)

summary(as.numeric(data$`voto_diploma_se conseguito in Italia`))

data$gl<-data$`GRUPPO di Laurea`
data$gl<-ifelse(data$gl=="Chimico-farmaceutico","Chim.-Farm.",
                ifelse(data$gl=="Economico-statistico","Econ.-Stet.",data$gl))
barplot(prop.table(table(data$`Situazione lavorativa`,data$gl),2),las=2,cex.names = .55)
data$gl<-NULL

table(data$`Partecipazione a programmi volti a promuovere la mobilità studentesca in ambito europeo o extraeuropeo`)
prop.table(table(data$`Situazione lavorativa`,data$`Partecipazione a programmi volti a promuovere la mobilità studentesca in ambito europeo o extraeuropeo`),2)

table(data$`Tipo di lavoro`) # tolgo NA
data$`Tipo di lavoro`<-ifelse(is.na(data$`Tipo di lavoro`)|data$`Tipo di lavoro`=="NA","NTL",data$`Tipo di lavoro`)

table(data$`Contratto di lavoro a tempo indeterminato o a termine`) # tolgo NA
data$`Contratto di lavoro a tempo indeterminato o a termine`<-ifelse(is.na(data$`Contratto di lavoro a tempo indeterminato o a termine`)|data$`Contratto di lavoro a tempo indeterminato o a termine`=="NA","NC",data$`Contratto di lavoro a tempo indeterminato o a termine`)

table(data$`Attualmente cerca lavoro`)
table(data$`Regione di attuale domicilio o estero`)
data$reddito<-as.numeric(data$`reddito mensile totale`)
summary(as.numeric(data$reddito)) # tolgo NA
data$reddito<-ifelse(is.na(data$reddito),0,data$reddito)
data$VD<-data$`voto_diploma_se conseguito in Italia`

dt<-data[,c(2,8,11,12,13,21,43,48,60,61)]
names(dt)<-c("GL","ERASMUS","VOTOL","SL","TL","TC","CERCAL","REGIONEDOM","REDDITO","VD")

boxplot(dt$VOTOL~dt$SL)

dt$SL<-ifelse(dt$SL=="si",1,0)
dt$VD1<-as.numeric(dt$VD)
mod<-glm(SL~GL+ERASMUS+VOTOL+VD1+CERCAL+REGIONEDOM,family = binomial(link = "logit"),data = dt)
summary(mod)

mod_s<-step(mod,direction = "both")
summary(mod_s)

anova( mod_s,mod, test="Chisq")

#

dt$VOTOLCLASSI<-as.factor(ifelse(dt$VOTOL<81,1,
                       ifelse(dt$VOTOL<96,2,
                              ifelse(dt$VOTOL<105,3,4))))
table(dt$VOTOL,dt$VOTOLCLASSI)

dt$VDCLASSI<-as.factor(ifelse(dt$VD=="NA",0,
                    ifelse(dt$VD<76,1,
                       ifelse(dt$VD<86,2,
                              ifelse(dt$VD<96,3,4)))))
table(dt$VD,dt$VDCLASSI)

dt$RIP<-as.factor(ifelse(dt$REGIONEDOM=="Abruzzo"|dt$REGIONEDOM=="Sardegna"|dt$REGIONEDOM=="Sicilia"|dt$REGIONEDOM=="Puglia"|dt$REGIONEDOM=="Campania"|dt$REGIONEDOM=="Molise"|dt$REGIONEDOM=="Basilicata"|dt$REGIONEDOM=="Calabria",3,
                    ifelse(dt$REGIONEDOM=="Lazio"|dt$REGIONEDOM=="Toscana"|dt$REGIONEDOM=="Marche"|dt$REGIONEDOM=="Umbria",2,
                           ifelse(dt$REGIONEDOM=="Estero",4,1))))
table(dt$REGIONEDOM,dt$RIP)

dt$CL<-ifelse(dt$GL=="Agrario"|dt$GL=="Geo-biologico"|dt$GL=="Architettura"|dt$GL=="Ingegneria"|dt$GL=="Chimico-farmaceutico"|dt$GL=="Economico-statistico"|dt$GL=="Scientifico","STEM",
              ifelse(dt$GL=="Medico"|dt$GL=="Psicologico","Sanitaria","Umanistica"))
table(dt$GL,dt$CL)

mod_n<-glm(SL~CL+ERASMUS+VOTOLCLASSI+VDCLASSI+CERCAL+RIP,family = binomial(link = "logit"),data = dt)
summary(mod_n)

mod_sn<-step(mod_n,direction = "both")
summary(mod_sn)

anova(mod_n,mod_sn, test="Chisq")

   
plot_model(mod_sn, vline.color = "red")    
       

