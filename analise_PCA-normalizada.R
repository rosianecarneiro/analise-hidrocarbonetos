#------------An�lise de Componentes Principais-----------------------#
library("ggplot2")
library("FactoMineR")
library("factoextra")
library("corrplot")
library("RColorBrewer") #biblioteca de cores


amostra<-read.csv("amostra_starburst_normalizada.csv", header=T, dec=",", sep=";")


PCA(amostra, scale.unit = TRUE, ncp = 5, graph = T) # grafico pca das variaveis(coluna) e dos individuos(linhas)
res.pca <- PCA(amostra, graph = F) 
print(res.pca)

eig.val <- get_eigenvalue(res.pca) #extrair autovalores / variacoes dos componentes principais
eig.val 


# visualizar a variacao de cada componentes principais
fviz_eig(res.pca,
         main = "Contribui��o das Componentes Principais",
         ylab = "% de Vari�ncias",
         xlab = "Dimens�es",
         addlabels = TRUE,
         barfill="azure4",
         barcolor="azure4",
         xlim = c(1, 10)) +
  theme(
    # titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor�ao altura/largura do plot
    aspect.ratio = 3/4
  )

#------------------------VISUALIZAR VARIAVEIS--------------------------#
var <- get_pca_var(res.pca) 
var

#  os componentes das variaveis:
# Coordenadas - circulo de correlacao
head(var$coord, 10)

# Cos2: qualidade de representacao no mapa fatorial
head(var$cos2, 10)

# Contribui��es para os componentes principais
head(var$contrib, 10)

#-----------------------circulos de correlacao--------------------------#

# variaveis - dimensao 1, 2
fviz_pca_var(res.pca,  title = "Vari�veis - PCA", axes = c(1, 2), geom = c("arrow", "text"),
             label = "all", labelsize = 5,
             col.var = "cos2", alpha.var = 5,
             col.circle = "gray70", circle = 1.2,
             arrow = (lenght = 1.2),
             repel = TRUE,
             xlab = "Dimens�o 1 (22.2%)", ylab = "Dimens�o 2 (18.2%)",
             legend.title = "Contribui��o",
             ylim = c(-1, 1)) +
  scale_color_gradient2(low="#00AFBB", mid="#E7B800",
                        high="#FC4E07", midpoint=0.5) +
  theme(
    # titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor�ao altura/largura do plot
    aspect.ratio = 2/2
  )


#   variaveis - dimensao 3,4
fviz_pca_var(res.pca,  title = "Vari�veis - PCA", axes = c(3, 4), geom = c("arrow", "text"),
             label = "all", labelsize = 5,
             col.var = "cos2", alpha.var = 5,
             col.circle = "gray70", circle = 1.2, 
             arrow = (lenght = 1.2),
             xlab = "Dimens�o 3 (17.5%)", ylab = "Dimens�o 4 (16%)",
             repel = TRUE,
             legend.title = "Contribui��o",
             ylim = c(-1, 1)) +
  scale_color_gradient2(low="#00AFBB", mid="#E7B800",
                        high="#FC4E07", midpoint=0.5) +
  theme(
    # titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor�ao altura/largura do plot
    aspect.ratio = 2/2
  )


#  variaveis - dimensao 4,5
fviz_pca_var(res.pca,  title = "Vari�veis - PCA", axes = c(4,5), geom = c("arrow", "text"),
             label = "all", labelsize = 5,
             col.var = "cos2", alpha.var = 5,
             col.circle = "gray70", circle = 1.2,
             arrow = (lenght = 1),
             xlab = "Dimens�o 4 (16%)", ylab = "Dimens�o 5 (9.6%)",
             repel = TRUE,
             legend.title = "Contribui��o",
             ylim = c(-1, 1)) +
  scale_color_gradient2(low="#00AFBB", mid="#E7B800",
                        high="#FC4E07", midpoint=0.5) +
  theme(
    # titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor�ao altura/largura do plot
    aspect.ratio = 2/2)

#-------------------QUALIDADE DE REPRESENTACAO-------------------------------#

#   cosseno quadrado=coordenadas quadradas, representa a qualidade de representacao das variaveis
#   valor entre 0 e 1

# visualiza o cos2 das variaveis nas dimensoes mais importantes
corrplot(var$cos2, is.corr=F,
         title="Qualidade de representa��o das vari�veis nas dimens�es", family="serif",
         type = "full", tl.srt=45,
         tl.col="black", addCoef.col = "black", col=brewer.pal(n=6, name="Purples"),
         method = 'circle', number.cex=1, tl.cex = 1,
         mar = c(0,0,1,0)) 


# o mesmo resultado acima em histograma
fviz_cos2(res.pca, choice = "var", axes = 1) 



#-----------CONTRIBUICAO DAS VARIAVEIS PARA AS DIMENSOES(CPs)--------------------#
#  valor em porcentagem

#   contribui��o das variaveis para Dim.1
fviz_contrib(res.pca, choice = "var",
             title= "Contribui��o das Vari�veis Para a Dimens�o 1",
             axes = 1, fill = "slategray4", color = "slategray4",
             addlabels = TRUE,
             xlim = c(1, 10))+theme(# titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor�ao altura/largura do plot
    aspect.ratio = 3/4)+labs(title = "Contribui��o das Vari�veis para a Dimens�o 1", x = "Vari�veis", y = "Contribui��o (%)",  color = "black")


#  contribui��o das variaveis para Dim.2
fviz_contrib(res.pca,
             title= "Contribui��o das Vari�veis Para a Dimens�o 2",
             choice = "var",
             axes = 2, fill = "slategray4", color = "slategray4",
             addlabels = TRUE,
             xlim = c(1, 10))+theme(# titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor�ao altura/largura do plot
    aspect.ratio = 3/4)+labs(title = "Contribui��o das Vari�veis para a Dimens�o 2", x = "Vari�veis", y = "Contribui��o (%)",  color = "black")


#  contribui��o das variaveis para Dim.3
fviz_contrib(res.pca,
             title= "Contribui��o das Vari�veis Para a Dimens�o 3",
             choice = "var",
             axes = 3, fill = "slategray4", color = "slategray4",
             addlabels = TRUE,
             xlim = c(1, 10))+theme(# titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor�ao altura/largura do plot
    aspect.ratio = 3/4)+labs(title = "Contribui��o das Vari�veis para a Dimens�o 3", x = "Vari�veis", y = "Contribui��o (%)",  color = "black")


#  contribui��o das variaveis para Dim.4
fviz_contrib(res.pca,
             title= "Contribui��o das Vari�veis Para a Dimens�o 4",
             choice = "var",
             axes = 4, fill = "slategray4", color = "slategray4",
             addlabels = TRUE,
             xlim = c(1, 10))+theme(# titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor�ao altura/largura do plot
    aspect.ratio = 3/4)+labs(title = "Contribui��o das Vari�veis para a Dimens�o 4", x = "Vari�veis", y = "Contribui��o (%)",  color = "black")


#  contribui��o das variaveis para Dim.5
fviz_contrib(res.pca,
             title= "Contribui��o das Vari�veis Para a Dimens�o 4",
             choice = "var",
             axes = 5, fill = "slategray4", color = "slategray4",
             addlabels = TRUE,
             xlim = c(1, 10))+theme(# titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor�ao altura/largura do plot
    aspect.ratio = 3/4)+labs(title = "Contribui��o das Vari�veis para a Dimens�o 5", x = "Vari�veis", y = "Contribui��o (%)",  color = "black")


# contribuicao da propriedades para dimens�o
colmat <- colorRampPalette(c("white", "red"))
corrplot(var$contrib,
         title = "Contribui��o das propriedades para as dimens�es", family="serif",
         method = "circle", type = "full", col.lim = c(0, 40), tl.srt=45,
         addshade = "positive",
         tl.col="black", addCoef.col = "black", col=brewer.pal(n=5, name="Purples"),
         number.cex=1, tl.cex = 1, cl.cex = 0.8,
         mar = c(0,1,1,0),
         is.corr = FALSE)





#-------------------------GR�FICO BIPLOT-----------------------------------#

# distribui��o de individuos
#fviz_pca_ind(res.pca, col.ind = "cos2", geom = "point",
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
 #            ylim = c(-1, 1), xlim =  c(-1, 1),
  #           repel = TRUE)


# biplot entre individuos e propriedades por dimens�o

fit <- princomp(amostra, cor=TRUE)
fviz_pca_biplot(fit,
                title = "Biplot",
                geom = c("point", "text"),
                col.var = "cos2",
                repel = T)


#----------------------CORRELACAO DE PEARSONS-------------------------#

# estrutura
str(amostra)

# Descri��o das fun��es estat�sticas dos dados da tabela
summary(amostra)

# Criar matriz de correla��o dos dados
res <- cor(amostra)
round(res, 2)

# gr�fico verde e roxo
corrplot(cor(amostra),
         diag = T,
         title="Correla��o entre Vari�veis", size = 18, hjust = "0.5", family="serif", face = "bold",
         type = "full", order = "hclust", tl.srt=45,
         tl.col="black", addCoef.col = "black", col=brewer.pal(n=4, name="PRGn"),
         method = 'circle', number.cex=1, tl.cex = 1,
        is.corr = T,
        mar = c(0,0,1,0))


# gr�fico roxo e marrom
corrplot(cor(amostra),
         diag = T,
         title="Correla��o entre Vari�veis",size = 18,  hjust = "0.5", family="serif", face = "bold",
         type = "full", order = "hclust", tl.srt=45,
         tl.col="black", addCoef.col = "black", col=brewer.pal(n=5, name="PuOr"),
         method = 'circle', number.cex=1, tl.cex = 1,
         is.corr = T,
         mar = c(0,0,1,0))








