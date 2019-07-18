############################ Pacotes ##########################
library(dplyr)
library(panelView)
library(gsynth)
library(Synth)
library(readxl)
require(Rcpp)
require(ggplot2)
require(GGally)
require(foreach)
require(doParallel)
require(abind)
####################### Tratando os Dados #####################
########## Importando os dados ##################
local<-"/home/alexandre/Documentos/Controle Sintetico/Painel_Balanceado.xlsx"
dados<-read_xlsx(local)
########## Manipulando os dados #################
dados<-as.data.frame(dados)
dados[is.na(dados)]<-0
dados<-dados[-c(1954,1964),]
########## Estados com Programas de Combate a Pobreza #########
dados_fecop<-dados%>%select(Estado,Tratamento,Ano,ID,Pobreza,Crescimento,Gini)%>%filter(ID %in% c("11","12","13","14","15","16","29","23","24","35","41","43","42"))
########## Estados com Programas de Combate a Pobreza com Balanceamento ########
dados_fecopb<-dados%>%select(Estado,Tratamento,Ano,ID,Pobreza,Crescimento,Gini)%>%filter(ID %in% c("11","12","13","14","15","16","29","23","24","35","41","43","42") & Ano<=2010 & Ano>=1986)
########## Estados com Programas de Combate a Pobreza com Balanceamento Sintetico ############
dados_fecopbs<-dados
dados_fecopbs[is.na(dados_fecopbs)]<-0
dados_fecopbs<-dados_fecopbs[-c(1054:1064),]
####################### Controle Sintetico #####################
########## Ajustando os dados ###############
dataprep.fecopb<-dataprep(foo=dados_fecop,predictors =c("Gini","Crescimento"),predictors.op = "mean",dependent = "Pobreza",unit.variable ="ID",time.variable ="Ano",treatment.identifier = c(23),controls.identifier = c(11,12,13,14,15,16,29,24,35,41,43,42),unit.names.variable = c("Estado"),time.predictors.prior = (1987:2003),time.optimize.ssr = c(1987:2010),time.plot = c(1986:2010))
dataprep.fecop<-dataprep(foo=dados_fecop,predictors = c("Gini","Crescimento"),predictors.op = "mean",dependent = "Pobreza",unit.variable ="ID",time.variable ="Ano",treatment.identifier =c(23),controls.identifier = c(11,12,13,14,15,16,29,24,35,41,43,42),unit.names.variable = c("Estado"),time.predictors.prior = c(1986:2003),time.optimize.ssr = c(1986:2010),time.plot = c(1986:2010))                  
  dataprep.fecopbs<-dataprep(foo=dados_fecopbs,predictors = c("Gini","Crescimento"),predictors.op = "mean",dependent = "Pobreza",unit.variable ="ID",time.variable ="Ano",treatment.identifier =c(23),controls.identifier = c(11,12,13,14,15,16,29,24,35,41,43,42),unit.names.variable = c("Estado"),time.predictors.prior = c(1986:2003),time.optimize.ssr = c(1986:2010),time.plot = c(1986:2010))

########## Estimando o Controle Sintetico ####################
####### Estados com Programas de Combate a Pobreza #########
sintetico_fecop<-synth(dataprep.fecop)
###
####### Estados com Programas de Combate a Pobreza com Balanceamento ################
sintetico_fecopb<-synth(dataprep.fecopb)

####### Estados com Programas de Combate a Pobreza com Balanceamento Sintetico ########
sintetico_fecopbs<-synth(dataprep.fecopbs)
###################### Analise Grafica ############################
path.plot(synth.res = sintetico_fecop,dataprep.res = dataprep.fecop,Ylab = c("Proporção de Domicilios Vivendo Abaixo da Linha da Pobreza"), Xlab=c("Ano"), Main=c("Comparação dos Efeitos da Politica de Combate a Pobreza"))
path.plot(synth.res = sintetico_fecopb,dataprep.res = dataprep.fecopb,Ylab = c("Proporção de Domicilios Vivendo Abaixo da Linha da Pobreza"), Xlab=c("Ano"), Main=c("Comparação dos Efeitos da Politica de Combate a Pobreza"))
path.plot(synth.res = sintetico_fecopbs,dataprep.res = dataprep.fecopbs,Ylab = c("Proporção de Domicilios Vivendo Abaixo da Linha da Pobreza"), X=c("Ano"), Main=c("Comparação dos Efeitos da Politica de Combate a Pobreza"))          
gaps.plot(synth.res = sintetico_fecop,dataprep.res = dataprep.fecop,Ylab = c("Proporção de Domicilios Vivendo Abaixo da Linha da Pobreza"), X=c("Ano"), Main=c("Comparação dos Efeitos da Politica de Combate a Pobreza"))
gaps.plot(synth.res = sintetico_fecopb,dataprep.res = dataprep.fecopb,Ylab = c("Proporção de Domicilios Vivendo Abaixo da Linha da Pobreza"), X=c("Ano"), Main=c("Comparação dos Efeitos da Politica de Combate a Pobreza"))
gaps.plot(synth.res = sintetico_fecop,dataprep.res = dataprep.fecop,Ylab = c("Proporção de Domicilios Vivendo Abaixo da Linha da Pobreza"), X=c("Ano"), Main=c("Comparação dos Efeitos da Politica de Combate a Pobreza"))
gaps.plot(synth.res = sintetico_fecopbs,dataprep.res = dataprep.fecopbs,Ylab = c("Proporção de Domicilios Vivendo Abaixo da Linha da Pobreza"), X=c("Ano"), Main=c("Comparação dos Efeitos da Politica de Combate a Pobreza"))

###################### Matriz de Pesos #########################
resumo<-synth.tab(synth.res = sintetico_fecop,dataprep.res = dataprep.fecop)
resumob<-synth.tab(synth.res = sintetico_fecopb,dataprep.res = dataprep.fecopb)
resumobs<-synth.tab(synth.res = sintetico_fecopbs,dataprep.res = dataprep.fecopbs)
