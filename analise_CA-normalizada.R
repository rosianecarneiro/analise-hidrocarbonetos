#------------Analise de Correspondencia-----------------------#
# nao precisa instalar os pacotes se o R ja foi instalado, so na primeira vez que for rodar o script

library("corrplot")
library("FactoMineR")
library("factoextra")
library("ggplot2")
library("RColorBrewer") #biblioteca de cores

dados<-read.csv("amostra_starburst_normalizada.csv", header=T, dec=",", sep=";") #ler dados

#------------------------Significado estatistico------------------------#
#teste Qui-quadrado avalia se ha dependencia significativa entre as categorias de linha e coluna
chisq <- chisq.test(dados)
chisq
#  Qui-quadrado de Pearson: X-squared = 73.706
# Graus de liberdade:       df = 1323
# P-value:                  pval = 1

#---------------------Analise de Correspondencia------------------------#

CA(dados, ncp = 5, graph = F) #funcao CA forma simplificada
res.ca <- CA(dados, graph = T) #calcular a analise de correspondencia
print(res.ca) #saida da função


#  Valores proprios/variancias: autovalores e a proporcao das variancias retidas pelos diferentes eixos
eig.val <- get_eigenvalue(res.ca)
eig.val   # dimensão 1 explica a maior variacao na solucao, seguida pela dimensão 2 e assim por diante

fviz_eig(res.ca,
         main = "Variâncias explicadas por dimensões",
         ylab = "Porcentagem (%)",
         xlab = "Dimensões",
         addlabels = TRUE,
         barfill="azure4",
         barcolor="azure4",
         xlim = c(1, 10), ylim = c(1, 32)) +
  theme(# titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)+ geom_hline(yintercept=11.11, linetype=2, color="darkblue")

#---------------------GRAFICOS DAS VARIAVEIS DE COLUNA----------------------------------------------#
col <- get_ca_col(res.ca) #extrair os resultados das variaveis da coluna
col

# Coordenadas (onde esta no grafico) de pontos de coluna(propriedades)
head(col$coord)
# Qualidade de representacao (numerico, 0 a 1)
head(col$cos2)
# Contribuicao (porcentagem, 0 a 100%)
head(col$contrib)

# coordenadas das propriedades nas dimensoes 1 e 2
fviz_ca_col(res.ca, title = "Coordenadas das propriedades - CA",  axes = c(1, 2), geom = c("arrow", "text"),
            label = "all", labelsize = 5, 
            col.col = "cos2", alpha.col = 5,
            col.circle = "gray70", circle = 1.2, arrow = (lenght = 1.2),
            xlab = "Dimensão 1 (27.8%)", ylab = "Dimensão 2 (21.6%)",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            legend.title = "Contribuição", repel = TRUE)+
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)

# coordenadas das propriedades nas dimensoes 2 e 3
fviz_ca_col(res.ca, title = "Coordenadas das propriedades - CA",  axes = c(2,3), geom = c("arrow", "text"),
            label = "all", labelsize = 5, 
            col.col = "cos2", alpha.col = 5,
            col.circle = "gray70", circle = 1.2, arrow = (lenght = 1.2),
            xlab = "Dimensão 2 (21.6%)", ylab = "Dimensão 3 (16.7%)", 
            #ylim = c(-1,1), xlim = c(-1,1), 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            legend.title = "Contribuição", repel = TRUE)+
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)

# coordenadas das propriedades nas dimensoes 3 e 4
fviz_ca_col(res.ca, title = "Coordenadas das propriedades - CA",  axes = c(3, 4), geom = c("arrow", "text"),
            label = "all", labelsize = 5, 
            col.col = "cos2", alpha.col = 5,
            col.circle = "gray70", circle = 1.2, arrow = (lenght = 1.2),
            #xlab = "Dimensão 3 (16.5%)", ylab = "Dimensão 4 (10.1%)",
            #ylim = c(-1,1), xlim = c(-1,1), 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            legend.title = "Contribuição", repel = TRUE)+
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)

# contribuicao das propriedades DM1
fviz_contrib(res.ca, choice = "col", axes = 1,
             fill = "slategray4", color = "slategray4", xlim = c(1, 10)) +
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)+labs(title = "Contribuição das propriedades para a Dimensão 1", x = "Propriedades", y = "Porcentagem (%)",  color = "black")

# contribuicao das propriedades DM2
fviz_contrib(res.ca, choice = "col", axes = 2, 
             fill = "slategray4", color = "slategray4", xlim = c(1, 10)) +
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)+ labs(title = "Contribuição das propriedades para a Dimensão 2", x = "Propriedades", y = "Porcentagem (%)",  color = "black")


# contribuicao das propriedades DM3
fviz_contrib(res.ca, choice = "col", axes = 3,
             fill = "slategray4", color = "slategray4",
             xlim = c(1, 10)) +
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)+labs(title = "Contribuição das propriedades para a Dimensão 3", x = "Propriedades", y = "Porcentagem (%)",  color = "black")

# contribuicao das propriedades DM4
fviz_contrib(res.ca, choice = "col", axes = 4,
             fill = "slategray4", color = "slategray4",
             xlim=c(1,10)) +
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)+labs(title = "Contribuição das propriedades para a Dimensão 4", x = "Propriedades", y = "Porcentagem (%)",  color = "black")


# contribuicao das propriedades DM5
fviz_contrib(res.ca, choice = "col", axes = 5,
             fill = "slategray4", color = "slategray4",
             xlim=c(1,10)) +
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)+labs(title = "Contribuição das propriedades para a Dimensão 5", x = "Propriedades", y = "Porcentagem (%)",  color = "black")

# contribuicao das colunas para dimensão
corrplot(col$contrib,
         method = "circle", type = "full", col.lim = c(0, 60),
         col=brewer.pal(n=4, name="Purples"), title = "Contribuição das propriedades para cada dimensão", family="serif",
         addCoef.col = "black", tl.cex = 1, tl.col="black", tl.srt=45,
         cl.cex = 0.8, addshade = "positive",
         number.cex=1,
         mar = c(0,0,1,0),
         is.corr = FALSE)
#https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/

#---------------------GRAFICOS DAS VARIAVEIS DE LINHA----------------------------------------------#
row <- get_ca_row(res.ca) #resultados das variaveis de linha
row
# Coordenadas (onde esta no grafico) de pontos de coluna(propriedades)
head(row$coord)
# Qualidade de representacao (numerico, 0 a 1)
head(row$cos2)
# Contribuicao (porcentagem, 0 a 100%)
head(row$contrib)

#  galaxias mais importante dim1 e dim2
fviz_ca_row(res.ca, title = "Coordenadas das galáxias - CA",  axes = c(1, 2), col.row = "cos2",
            label = "all", labelsize = 5, legend.title = "Contribuição",
            xlab = "Dimensão 1 (27.8%)", ylab = "Dimensão 2 (21.6%)",
            #ylim = c(-1,1), xlim = c(-1,1), 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)+
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)


#   galaxias mais importante dim2 e dim3
fviz_ca_row(res.ca, title = "Coordenadas das galáxias - CA",  axes = c(2, 3), col.row = "cos2",
            label = "all", labelsize = 5, legend.title = "Contribuição",
            xlab = "Dimensão 2 (21.6%)", ylab = "Dimensão 3 (16.7%)",
            #ylim = c(-1,1), xlim = c(-1,1), 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)+
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)

#  galaxias mais importante dim3 e dim4
fviz_ca_row(res.ca, title = "Coordenadas das galáxias - CA",  axes = c(3,4), col.row = "cos2",
            label = "all", labelsize = 5, legend.title = "Contribuição",
            xlab = "Dimensão 3 (16.5%)", ylab = "Dimensão 4 (10.1%)",
            #ylim = c(-1,1), xlim = c(-1,1), 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)+
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)

#   galaxias para Dim.1
fviz_contrib(res.ca, choice = "row", axes = 1, repel = T, addlabels = TRUE,
             labelsize = 2, fill = "slategray4", color = "slategray4", top = 30)+
  theme(# titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor?ao altura/largura do plot
    aspect.ratio = 2/4) + labs(title = "Contribuição das galáxias para a Dimensão 1", x = "Galáxias", y = "Porcentagem (%)",  color = "black")

#    galaxias para Dim.2
fviz_contrib(res.ca, choice = "row", axes = 2, repel = T,
             labelsize = 2, fill = "slategray4", color = "slategray4", top = 41)+
  theme(# titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor?ao altura/largura do plot
    aspect.ratio = 2/4) + labs(title = "Contribuição das galáxias para a Dimensão 2", x = "Galáxias", y = "Porcentagem (%)",  color = "black")

#    galaxias para Dim.3
fviz_contrib(res.ca, choice = "row", axes = 3, repel = T,
             labelsize = 2, fill = "slategray4", color = "slategray4", top = 37)+
  theme(# titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor?ao altura/largura do plot
    aspect.ratio = 2/4) + labs(title = "Contribuição das galáxias para a Dimensão 3", x = "Galáxias", y = "Porcentagem (%)",  color = "black")

#   galaxias para Dim.4
fviz_contrib(res.ca, choice = "row", axes = 4, repel = T,
             labelsize = 2, fill = "slategray4", color = "slategray4", top = 10)+
  theme(# titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor?ao altura/largura do plot
    aspect.ratio = 2/4) + labs(title = "Contribuição das galáxias para a Dimensão 4", x = "Galáxias", y = "Porcentagem (%)",  color = "black")

#   galaxias para Dim.5
fviz_contrib(res.ca, choice = "row", axes = 5, repel = T,
             labelsize = 2, fill = "slategray4", color = "slategray4", top = 10)+
  theme(# titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor?ao altura/largura do plot
    aspect.ratio = 2/4) + labs(title = "Contribuição das galáxias para a Dimensão 5", x = "Galáxias", y = "Porcentagem (%)",  color = "black")

#a linha tracejada indica o valor medio esperado se as contribuicoes fossem uniformes


#maneira diferente para ver resultados anteriores com todas as linhas, fica fora de escala
corrplot(row$cos2, is.corr=FALSE) 

#-------------------------BIPLOT--------------------------------------#
# distancia entre pontos de linha ou pontos de coluna fornece uma medida de sua similaridade (ou dissimilaridade)
# pontos de linha/pontos de coluna semelhantes sao fechados no mapa de fatores


#   biplot dimensao 1 e 2
fviz_ca_biplot(res.ca, 
               geom = c("point", "text"), label = "all", 
               labelsize = 5, pointsize = 2, legend.title= "Contribuição",
               col.row = "cos2", col.col = "black",
               alpha.col = 1, alpha.row = 0.8,
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = T)+
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)+labs(title = "Biplot das propriedades das galáxias", x = "Dimensão 1 (27.8%)", y = "Dimensão 2 (21.6%)")


#   biplot dimensao 1 e 3
#fviz_ca_biplot(res.ca, axes = c(1,3),
              # geom = c("point", "text"), label = "all",
 #              labelsize = 5, pointsize = 2, legend.title= "Contribuição",
  #             col.row = "cos2", col.col = "black",
   #            alpha.col = 1, alpha.row = 0.8,
    #           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              # repel = T)+
  #theme(# título do plot
   # plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    #axis.title.x = element_text(size = 14, family="serif", color = "black"),
    #axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
  #  axis.text.x = element_text(size = 14, family="serif", color = "black"),
   # axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    #aspect.ratio = 3/4)+labs(title = "Biplot das propriedades das galáxias", x = "Dimensão 1 (27.8%)", y = "Dimensão 3 (13.3%)")


 #   biplot dimensao 2 e 3
fviz_ca_biplot(res.ca, axes = c(2,3),
               geom = c("point", "text"), label = "all", 
               labelsize = 5, pointsize = 2, legend.title= "Contribuição", 
               col.row = "cos2", col.col = "black",
               alpha.col = 1, alpha.row = 0.8,
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = T)+
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)+labs(title = "Biplot das propriedades das galáxias", x = "Dimensão 2 (21.6%)", y = "Dimensão 3 (16.7%)")


#   biplot dimensao 3 e 4
fviz_ca_biplot(res.ca, axes = c(3,4),
               geom = c("point", "text"), label = "all", 
               labelsize = 5, pointsize = 2, legend.title= "Contribuição", 
               col.row = "cos2", col.col = "black",
               alpha.col = 1, alpha.row = 0.8,
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = T)+
  theme(# título do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # proporçao altura/largura do plot
    aspect.ratio = 3/4)+labs(title = "Biplot das propriedades das galáxias", x = "Dimensão 3 (16.5%)", y = "Dimensão 4 (10.1%)")
