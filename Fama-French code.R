library(quantmod)
library(readxl)
library(TTR)
library(dplyr)
library(tidyverse)
library(magrittr)
library(xts)


#Dados da carteira IBOV. Queremos fazer somente com os ativos mais liquidos. Não
#queremos que premio pela liquidez afete nossa analise.

dados_ibov <- read_excel("C:/Users/Amarildo/Desktop/Quant/Fama French/IBOVDia.xlsx",
                     skip = 1)[,c(1,5)]

dados_ibov$`Código com SA`<- paste0(dados_ibov$Código, ".SA")

#Pegamos os dados de valor de mercado e valor patrimonial da base do economatica.
dados_economatica <- read_excel("C:/Users/Amarildo/Downloads/economatica (51).xlsx",
                                                   skip = 3)[,7:9]
colnames(dados_economatica) <- c("Código", "valor de mercado", "valor patrimonial")

#Juntando os dados do ibov e do economatica
base_fatores <- merge(dados_ibov, dados_economatica, by = "Código")



#Baixando os preços historicos dos ativos do yahoo finance

tickers <- base_fatores$`Código com SA`

preco_ativos <- xts()
for (i in 1:length(tickers)){
  preco_ativo <- getSymbols(tickers[i], auto.assign = F, from = "2020-01-01")[,6]
  preco_ativos <- merge(preco_ativos,preco_ativo)
}

retorno <- ROC(preco_ativos)
retorno <- na.omit(retorno)
colnames(retorno) <- substring(colnames(retorno), 1, nchar(colnames(retorno))-12)

View(retorno)


#Indice de mercado

mkt <- xts(x = (t(t(base_fatores[,2]) %*% t(retorno))/sum(base_fatores[,2])),
              order.by = index(retorno))
colnames(mkt) <- c('Retornos')

mkt$`Retorno Acumulado` <- cumprod(1+mkt$Retornos)

plot(mkt$`Retorno Acumulado`)

# SMB

percentil_smb <-1/3 
n.tickers <- nrow(base_fatores)

corte_smb <-  round(n.tickers*percentil_smb)

small <- base_fatores %>%
  filter(`valor de mercado`<=sort(base_fatores$`valor de mercado`)[corte_smb]) %>% 
  select("Código")
small.tickers <- small$Código
  


big <- base_fatores %>%
  filter(`valor de mercado`>=sort(base_fatores$`valor patrimonial`)[n.tickers - corte_smb]) %>% 
  select("Código")
big.tickers <- big$Código


df.retorno <- data.frame(date=index(retorno), coredata(retorno))

retornos_small <- df.retorno %>% select(date, small.tickers)
retornos_small$Portfolio <- rowMeans(retornos_small[2:ncol(retornos_small)])


retornos_big <- df.retorno %>% select(date,big.tickers)
retornos_big$Portfolio <- rowMeans(retornos_big[2:ncol(retornos_big)])

smb <- xts(x = retornos_small$Portfolio - retornos_big$Portfolio, 
                   order.by = retornos_small$date)

colnames(smb) <- "Retornos"

smb$`Retorno Acumulado` <-  cumprod(1+smb$Retornos)

plot(smb$`Retorno Acumulado`)

#HML

base_fatores$`Book to Market` <- as.numeric(base_fatores$`valor patrimonial`)/
  as.numeric(base_fatores$`valor de mercado`)

percentil_hml <-1/3 

corte_hml <-  round(n.tickers*percentil_hml)

low <- base_fatores %>%
  filter(`Book to Market`<=sort(base_fatores$`Book to Market`)[corte_hml]) %>% 
  select("Código")

low.tickers <- low$Código


high <- base_fatores %>%
  filter(`Book to Market`>=sort(base_fatores$`Book to Market`)[n.tickers - corte_hml]) %>% 
  select("Código")

high.tickers <- high$Código

retornos_low <- df.retorno %>% select(date, low.tickers)
retornos_low$Portfolio <- rowMeans(retornos_low[2:ncol(retornos_low)])


retornos_high <- df.retorno %>% select(date,high.tickers)
retornos_high$Portfolio <- rowMeans(retornos_high[2:ncol(retornos_high)])

hml <- xts(x = retornos_low$Portfolio - retornos_high$Portfolio, 
           order.by = retornos_high$date)

colnames(hml) <- "Retornos"

hml$`Retorno Acumulado` <-  cumprod(1+hml$Retornos)

plot(hml$`Retorno Acumulado`)

#Indice retornos

returns_factors <- merge(mkt$Retornos,smb$Retornos ,hml$Retornos)

cum.return_factors <- merge(mkt$`Retorno Acumulado`,smb$`Retorno Acumulado` ,hml$`Retorno Acumulado`)

plot(cum.return_factors)












