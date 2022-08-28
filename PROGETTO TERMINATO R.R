anno    = c(1950:2021)
vincitore_gara_Monza  = c("Nino Farina","Alberto Ascari","Alberto Ascari","Juan Manuel Fangio","Juan Manuel Fangio","Juan Manuel Fangio","Stirling Moss","Stirling Moss","Tony Brooks","Stirling Moss","Phil Hill","Phil Hill"," Graham Hill","Jim Clark ","John Surtees","Jackie Stewart","Ludovico Scarfiotti","John Surtees","Denny Hulme","Jackie Stewart","Clay Regazzoni","Peter Gethin","Emerson Fittipaldi","Ronnie Peterson","Ronnie Peterson","Clay Regazzoni","Ronnie Peterson","Mario Andretti","Niki Lauda","Jody Scheckter","Renè Arn","Alain Prost","Renè Arnoux","Nelson Piquet","Niki Lauda","Alain Prost","Nelson Piquet","Nelson Piquet","Gerhard Berger","Alain Prost","Ayrton Senna","Nigel Mansell","Ayrton Senna","Damon Hill","Damon Hill","Johnn Herbert","Michael Schumacher","David Coulthard","Michael Schumacher","Heinz-Harald Frentzen","Michael Schumacher","Juan Pablo Montoya","Rubens Barrichello","Michael Schumacher","Rubens Barrichello","Juan Pablo Montoya","Michael Schumacher"," Fernando Alonso","Sebastian Vettel","Rubens Barrichello","Fernando Alonso","Sebastian Vettel","Lewis Hamilton"," Sebastian Vettel","Lewis Hamilton","Lewis Hamilton","Nico Rosberg","Lewis Hamilton","Lewis Hamilton","Charles Leclerc","Pierre Gasly","Daniel Ricciardo")
età      = c(44,33,34,42,43,44,27,28,26,30,33,34,33,27,30,26,33,33,32,30,31,31,26,29,30,36,37,32,29,39,32,26,34,31,35,30,34,35,29,34,30,38,32,33,34,29,27,26,29,32,31,27,30,34,32,30,37,26,21,37,29,24,27,26,28,29,31,31,32,22,24,32)
scuderia = c("Alfa Romeo","Ferrari","Ferrari","Maserati","Mercedes","Mercedes","Maserati","Vanwall","Vanwall","Cooper Climax","Ferrari","Ferrari","B R M","Lotus","Ferrari","B R M","Ferrari","Honda","McLaren","Matra Ford","Ferrari","B R M","Lotus","Lotus","Lotus","Ferrari","March","Lotus","Brabham","Ferrari","Renault","Renault","Renault","Brabham","McLaren","McLaren","Williams","Williams","Ferrari","McLaren","McLaren","Williams","McLaren","Williams","Williams","Benetton","Ferrari","McLaren","Ferrari","Jordan","Ferrari","Williams","Ferrari","Ferrari","Ferrari","McLaren","Ferrari","McLaren","Toro Rosso","Brawn","Ferrari","Red Bull Racing","McLaren","Red Bull Racing","Mercedes","Mercedes","Mercedes","Mercedes","Mercedes","Ferrari","AlphaTauri","McLaren")
tempo_più_veloce_qualifica = c(1.58,1.53,2.05,2.02,1.59,2.46,2.42,1.42,1.40,1.39,2.41,2.46,1.40,1.37,1.37,1.36,1.31,1.28,1.26,1.25,1.24,1.22,1.35,1.35,1.33,1.32,1.41,1.38,1.40,1.34,1.34,1.33,1.28,1.29,1.26,1.25,1.24,1.23,1.25,1.23,1.22,1.24,1.22,1.21,1.24,1.24,1.24,1.24,1.25,1.22,1.23,1.22,1.20,1.21,1.20,1.21,1.21,1.21,1.35,1.23,1.22,1.22,1.24,1.23,1.24,1.23,1.21,1.34,1.19,1.19,1.18,1.19)
peso_vettura =c(710,850,560,590,600,590,610,560,560,560,500,470,450,460,468,450,500,500,523,523,530,534,550,575,575,575,575,557,542,590,575,585,585,540,540,540,540,540,540,505,500,505,505,500,520,595,595,530,550,600,600,600,500,600,600,605,600,600,605,605,620,640,640,642,691,798,728,728,733,798,748,752)
cavalli_vettura = c(350,450,250,250,250,280,260,290,295,295,190,190,300,230,250,250,360,390,415,440,430,430,450,480,495,495,495,580,580,580,580,580,580,750,800,800,800,850,880,610,620,680,700,745,750,800,750,750,750,780,750,810,840,880,845,915,720,800,820,820,745,740,750,750,745,960,960,1000,1000,1000,1010,970)
formula_frames <- data.frame(anno,
                             vincitore_gara_Monza,
                             età,
                             scuderia,
                             tempo_più_veloce_qualifica,
                             peso_vettura,
                             cavalli_vettura)

formula_frames

#grafico che mostra l'evoluzione dell'età con gli anni
par(mar=rep(2,4))
plot (anno,età,xlab = "anni", ylab = "età" )
lines(lowess(anno,età, f = 1/10),lwd = 2)


#grafico a torta età
valoriG <- c(1,1,2,7,5,2,7,8,6,8,6,7,2,1,3,1,1,1,1,2)
labelG <- c(21,22,24,26,27,28,29,30,31,32,33,34,35,36,37,38,39,42,43,44)
perc<- round(valoriG/sum(valoriG)*100)
labelG <- paste(labelG, perc)
labelG <- paste(labelG,"%",sep="")
pie(valoriG, labels = labelG,col=rainbow(length(labelG)), main="Grafico a torta dell' età")

#grafico che mi fa vedere i tempi in base agli anni 
par(mar=rep(2,4))
plot(anno,tempo_più_veloce_qualifica , xlab = "year", ylab = "tempo_qualifica" ,type = "b", pch = 1)
lines(lowess(anno,tempo_qualifica, f = 1/12), lwd = 2)

library(ggplot2)

#grafico andamento del peso delle vetture nel tempo
ggplot(formula_frames, aes(x=anno,y=peso_vettura)) +
  labs(x="anno", 
       y="peso [kg]")+
  geom_jitter() +
  geom_smooth(method = loess)

#grafico andamento dei cavalli delle vetture nel tempo
ggplot(formula_frames, aes(x=anno,y=cavalli_vettura)) +
  labs(x="anno", 
       y="cavalli [CV]")+
  geom_jitter() +
  geom_smooth(method = loess)

#grafico che mi mostra che con l'aumentare del peso aumentano anche i cavalli

ggplot(formula_frames, aes(x=cavalli_vettura,y=peso_vettura)) +
  labs(x="cavalli [CV]", 
       y="peso [kg]")+
  geom_jitter() +
  geom_smooth(method = loess)
