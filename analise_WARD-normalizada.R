library("factoextra")
library("cluster")
library("clValid")
library("NbClust")

agrupamento<-read.csv("amostra_starburst_normalizada.csv", header=T, dec=",", sep=";")

#-----------------------COMPARANDO ALGPORITMOS------------------------------------#

# medidas da valida??o interna: define melhor algoritmo e numero de clusters pra todos os metodos
#medida de validacao com 30 indices
res<-NbClust(agrupamento, distance = "euclidean", min.nc = 2, max.nc = 6, 
             method = "ward.D2", index = "alllong")

fviz_nbclust(res, cex = 0.5,
             addlabels = F, ylim=11,
             barfill="#5c0180",
             barcolor="#5c0180")+
  theme(# titulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor?ao altura/largura do plot
    aspect.ratio = 3/4)+ggtitle("Número ideal de clusters")+xlab("Número de clusters (k)")+ylab("Frequêcias entre todos os Índices")


#----------------------Analise de Agrupamento--------------------------------#

# Distancia Euclidiana: define a semelhan?a entre dois objetos
distancia <- dist(agrupamento, method = "euclidean")
plot(distancia, ylab = "Distancia", xlab = "Index", main = "Distância Euclidiana" )

# metodo ward.D
#fit <- hclust(distancia^2, method = "ward.D")
#devido diferen?a no cod, precisa inserir o quadrado na distancia euclidiana
#fviz_dend(fit, k=3, cex= 0.5, repel = T, lwd = 1.5, 
#          k_colors = c("#ed09b0", "#168500", "#0270c9"),
#          color_labels_by_k = TRUE, rect = TRUE,
#          ylab = "Distância", xlab = "Galáxias", main = "Agrupamento Hierárquico")+
#  theme(# t?tulo do plot
#    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
#    axis.title.x = element_text(size = 14, family="serif", color = "black"),
#    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
#    axis.text.x = element_text(size = 14, family="serif", color = "black"),
#    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor?ao altura/largura do plot
#    aspect.ratio = 3/4)



# metodo ward.D2
fit2 <- hclust(distancia, method = "ward.D2")
fviz_dend(fit2, k=3, cex= 0.5, repel = T, lwd = 1.5,
          k_colors = c("#ed09b0", "#168500", "#0270c9"),
          color_labels_by_k = TRUE, rect = TRUE, rect_fill = T,
          ylab = "Distância", xlab = "Galáxias", main = "Agrupamento Hierárquico")+
  theme(# t?tulo do plot
    plot.title = element_text(size = 18, hjust = "0.5", family="serif", face = "bold", color = "black"),
    # nomes dos eixos
    axis.title.x = element_text(size = 14, family="serif", color = "black"),
    axis.title.y = element_text(size = 14, family="serif", color = "black"),
    # texto dos elementos nos eixos
    axis.text.x = element_text(size = 14, family="serif", color = "black"),
    axis.text.y = element_text(size = 14, family="serif", color = "black"),
    # propor?ao altura/largura do plot
    aspect.ratio = 2/4)

#------------------------CLUSTER EM DUAS DIMENSOES-------------------------------#
grp <- cutree(fit2, k = 3)

fviz_cluster(list(data = agrupamento, cluster = grp),
             palette = c("#ed09b0", "#168500", "#0270c9"),
             ylab = "Dimensão 1 (18.4%)", xlab = "Dimensão 1 (22.1%)", main = "Cluster 2D",
             ellipse.type = "convex",
             legend.title = "Cluster",
             repel = TRUE, 
             show.clust.cent = T)+
  theme(plot.title = element_text(size = 18, hjust = "0.5", face = "bold", color = "black"),
    axis.title.x = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14,  color = "black"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    aspect.ratio = 2/4)
#--------------------------------------------------------------#
#SALVAR LISTA DE GRUPOS
grp <- cutree(fit2, k = 3)
save(grp, file = "lista_grupos.txt")

#EXPORTAR E LE NO R
write(dput(grp), file = "lista_grupos.txt")

#SUBSTITUI NOME DA GALAXIA PELO NUMERO DO GRUPO

plot(fit2, labels = as.character(grp))


