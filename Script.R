library(readxl)
library(ggplot2)

main <- function() {
  
  valori <- view("valori.xls")
  
  #Boxplot
  boxplot.stats(valori$Componenti)
  boxplot.stats(valori$Componenti_SA)
  
  dati_confronto <- data.frame(colonna1 = valori$Componenti_SA, colonna2 = valori$Componenti)
  
  ggplot(dati_confronto, aes(x = factor(1), y = colonna1)) 
    geom_boxplot(color = "black") 
    geom_boxplot(aes(x = factor(2), y = colonna2), color = "black") 
    xlab("Gruppi") 
    ylab("Valori") 
    ggtitle("Confronto tra Boxplot") 
    scale_x_discrete(labels = c("Vendita Componenti ", "Vendita Componenti SA")) 
    theme_bw()
  
  #Test di Ipotesi Indipendenti
  t.test(valori$Componenti_SA, valori$Componenti, paired = T, alternative = "greater", conf.level=0.95)
  
  #Test di Ipotesi Accoppiati
  t.test(valori$Componenti_SA, valori$Componenti, paired = F, alternative = "greater", conf.level=0.95)
  
  #Test di Regressione Lineare
  x <- valori$Componenti_SA
  y <- valori$Riparazione
  fit <- lm(y ~ x)
  
  plot(x, y, 
       main = "Regressione lineare", 
       xlab = "Servizi Assistenza", 
       ylab = "Servizi Riparazione")
  
  abline(fit, col = "red")
  
  
}



