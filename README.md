# TestedeHipotese
 Existe diferença significante de atrasos de voo entre duas companhia aéreas?

<h1><b>Sumário</b></h1>
<p> <font color = white>...</font></p>


<p class="shytext"> 1.<a href="#1"><strong>Coletando os Dados</strong></a></p>

<p class="shytext"> 2.<a href="#2"><strong>Exploração de Dados</strong></a></p>

<p class="shytext"> 3.<a href="#3"><strong>Construindo o Dataset</strong></a></p>

<p class="shytext"> 4.<a href="#4"><strong>Criação de Amostragem</strong></a></p>

<p class="shytext"> 5.<a href="#5"><strong>Intervalo de Confiança</strong></a></p>

<p class="shytext"> 6.<a href="#6"><strong>Gráfico dos Intervalos de Confiança </strong></a></p>

<p class="shytext"> 7.<a href="#7"><strong>Criação do Teste de Hipótese</strong></a></p>

<p class="shytext"> 7.1.<a href="#7.1"><strong>Teste t</strong></a></p>

<p class="shytext"> 7.2.<a href="#7.2"><strong>Valor p</strong></a></p>

<p class="shytext"> 8.<a href="#8"><strong>Conclusão</strong></a></p>

<h2><b>Definição do Problema de Negócio</h2></b>

<h5>Para analisar se existe diferença de atraso de vôo entre a companhia utilizaremos teste de hipósete de um conjunto de dados que possui 336,776 observações e 19 variáveis, chamado flights", que demonstram dados pontuais de todos os voos que partiram de Nova York em 2013.
Dentre estas companhia escolheremos duas companhias: Delta Airlines (DL) e a United Airlines (UA). </h5>


<a name="1"> </a>
<h3><b>Etapa 1 - Coletando os Dados</h3></b>

<h5>Aqui está a coleta de dados.</h5>


```{r coleta}
# Coletando dados
library('ggplot2')
library('dplyr')
library('nycflights13')
head(flights)
```

<a name="2"> </a>
<h3><b>Etapa 2 - Exploração dos Dados</h3></b>


```{r explorando}
head(flights)
# Visualizando as variáveis
str(flights)
```

<h3><b>Medidas de Tendência Central da Variável Numéricas</h3></b>

```{r summary}
summary(flights$arr_delay)
# Tabela de contigêcia das linhas aéreas
table(flights$carrier)
#Histograma
hist(flights$arr_delay, main = 'Histograma 1', xlab = 'Atraso de Voo')
```

<a name="3"> </a>
<h3><b>Etapa 3 - Construindo o Dataset</h3></b>

<h5> Construção do dataset pop_data com os dados de voos das companhias aéreas UA (United Airlines) e DL (Delta Airlines). 
 Contruiremos um dataset irá conter apenas duas colunas, nome da companhia e atraso nos voos de chegada.
 Será conssiderado este dataset como sendo nossa população de voos:</h5>


 <b>Metodologia da formula:</b>
 
<h5> * 1º eliminação de todos os dados na (Valores missing, não disponivel);
 
 * 2º Filtro pela companhia aérea UA (United Airlines) e DL (Delta Airlines);
 
 * 3º Filtro para retornar apenas os valores que forem igual ou maior a 'Zero'. Os valores negativos serão descontiderádos;</h5>


```{r modelagem}
pop_data = na.omit(flights) %>%
  filter(carrier == 'UA' | carrier == 'DL', arr_delay >=0) %>%
  select(carrier, arr_delay) %>%
  group_by(carrier) %>%
  sample_n(17000) %>%
  ungroup()
head(pop_data)
tail(pop_data)
hist(pop_data$arr_delay, main = 'Histograma 2', xlab = 'Atraso de Voo', ylab="Frequencia")
```

<h5>É importante observar que neste histograma removemos os valores negativos, pois temos como principal objetivo observar somente os atrasos de voos. Os valores no conjunto de dados que representam menores que zero seguinifica que o voo chegou a sei destido antes da data prevista, então não houve atraso.<h5>

<a name="4"> </a>
<h3><b>Etapa 4 - Criação de Amostragem</h3></b>

<h5>Criação de duas amostras de 1000 observações cada uma a partir do dataset pop_data apenas com dados da companhia DL para amostra 1 e apenas dados da companhia UA na amostra 2.</h5>


```{r amostra}
amostra1 <- na.omit(pop_data) %>% 
  select(carrier, arr_delay) %>%
  filter(carrier == 'DL') %>%
  mutate(sample_id = '1') %>%
  sample_n(1000)
                      
head(amostra1)
amostra2 <- na.omit(pop_data) %>%
  select(carrier, arr_delay) %>%
  filter(carrier == "UA") %>%
  mutate(sample_id = "2") %>%
  sample_n(1000)
head(amostra2)
# Criação de um dataset contendo os dados das 2 amostras criadas no item anterior. 
samples = rbind(amostra1, amostra2)
head(samples)
tail(samples)
```

<a name="5"> </a>
<h3><b>Etapa 5 - Intervalo de Confiança</h3></b>

<h5>Calculo do intervalo de confiança (95%) da amostra1.<h5>

<b>Fórmula de erro padrão: erro_padrao = sd(amostra$arr_delay) / sqrt(nrow(amostra))</b>

<h5>Esta fórmula é usada para calcular o desvio padrão de uma distribuição da média amostral (de um grande número de amostras de uma população). Em outras palavras, só é aplicável quando você está procurando o desvio padrão de médias calculadas a partir de uma amostra de tamanho n𝑛, tirada de uma população.

Digamos que você obtenha 10000 amostras de uma população qualquer com um tamanho de amostra de n = 2.
Então calculamos as médias de cada uma dessas amostras (teremos 10000 médias calculadas).
A equação acima informa que, com um número de amostras grande o suficiente, o desvio padrão das médias da amostra pode ser aproximado usando esta fórmula: sd(amostra) / sqrt(nrow(amostra))
  
Deve ser intuitivo que o seu desvio padrão das médias da amostra será muito pequeno, ou em outras palavras, as médias de cada amostra terão muito pouca variação.

Com determinadas condições de inferência (nossa amostra é aleatória, normal, independente), podemos realmente usar esse cálculo de desvio padrão para estimar o desvio padrão de nossa população. 
Como isso é apenas uma estimativa, é chamado de erro padrão. A condição para usar isso como uma estimativa é que o tamanho da amostra n é maior que 30 (dado pelo teorema do limite central) e atende a condição de independência n <= 10% do tamanho da população.</h5>

<h2>amostra 1</h2>

<b>Erro Padrão</b>

```{r ep1}
erro_padrao_amostra1 = sd(amostra1$arr_delay) / sqrt(nrow(amostra1))
```

<b>Limites Inferior e Superior</b>

<h5>1.96 é o valor de z score para 95% de confiança</h5>

```{r lower_upper1}
lower1 = mean(amostra1$arr_delay) - 1.96 * erro_padrao_amostra1
upper1 = mean(amostra1$arr_delay) + 1.96 * erro_padrao_amostra1
```


<b>Intervalo de Confiança</b>

```{r ic1}
ic_1 = c(lower1, upper1)
mean(amostra1$arr_delay)
ic_1
```


<h2>amostra 2</h2>

<b>Erro Padrão</b>
```{r ep2}
erro_padrao_amostra2 = sd(amostra1$arr_delay) / sqrt(nrow(amostra2))
```
<b>Limites Inferior e Superior</b>

<h5>1.96 é o valor de z score para 95% de confiança</h5>

```{r lower_upper2}
lower2 = mean(amostra2$arr_delay) - 1.96 * erro_padrao_amostra2
upper2 = mean(amostra2$arr_delay) + 1.96 * erro_padrao_amostra2
```


<b>Intervalo de Confiança</b>

```{r ic2}
ic_2 = c(lower2, upper2)
mean(amostra1$arr_delay)
ic_2
```

<a name="6"> </a>
<h3><b>Etapa 6 - Gráfico dos Intervalos de Confianças</h3></b>

<h5>Criação de um plot Visualizando os intervalos de confiança criados nos itens anteriores.</h5>

```{r grafico}
toPlot = summarise(group_by(samples, sample_id), mean = mean(arr_delay))
toPlot = mutate(toPlot, lower = ifelse(toPlot$sample_id == 1,ic_1[1],ic_2[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$sample_id == 1,ic_1[2],ic_2[2]))
ggplot(toPlot, aes(x = sample_id, y=mean, colour = sample_id)) + 
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1)
```


<h5>A maior parte dos dados reside no mesmo intervalo de confiança nas duas amostras.
Motimo pelo qual podemos dizer que muito provavelmente, as amostras vieram da mesma população.</h5>

<a name="7"> </a>
<h3><b>Etapa 7 - Criação do Teste de Hipótese</h3></b>

<h5>Crição de um teste de hipótese para verificar se os voos da Delta Airlines (DL) atrasam mais do que os voos da UA (United Airlines).</h5>

<b>H0 e H1 devem ser mutuamente exclusivas.</b>

<h5>* H0 = Não há diferença significativa entre os atrasos da DL e UA (diff da média de atrasos = 0).
* H1 = Delta atrasa mais (diff das médias > 0).</h5>


<b> Criação das Amostras</b>

```{r amostra3}
dl <- sample_n(filter(pop_data, carrier == "DL", arr_delay > 0), 1000)
ua <- sample_n(filter(pop_data, carrier == "UA", arr_delay > 0), 1000)
```


<b>Calculo do Erro Padrão Médio</b>

<b>Amostra 1</b>

```{r epm1}
se1 = sd(dl$arr_delay) / sqrt(nrow(dl))
mean(dl$arr_delay)
```

<b>Limites Inferior e Superior</b>

```{r lis1}
lower11 = mean(dl$arr_delay) - 1.96 * se1
upper11 = mean(dl$arr_delay) + 1.96 * se1
ic_dl = c(lower11,upper11)
ic_dl
```

<b>Calcula erro padrão e média</b>
<b>Amostra 2</b>

```{r epm2}
se2 = sd(ua$arr_delay) / sqrt(nrow(ua))
mean(ua$arr_delay)
```

<b>Limites inferior e superior</b>

```{r lis2}
lower22 = mean(ua$arr_delay) - 1.96 * se2
upper22 = mean(ua$arr_delay) + 1.96 * se2
ic_ua = c(lower22,upper22)
ic_ua
```

<a name="7.1"> </a>

<h3><b>Etapa 7.1 - Teste T</h3></b>

<h5>O teste t (de Student) foi desenvolvido por Willian Sealy Gosset em 1908 que usou o pseudônimo “Student” em função da confidencialidade requerida por seu empregador (cervejaria Guiness) que considerava o uso de estatística na manutenção da qualidade como uma vantagem competitiva.
O teste t de Student tem diversas variações de aplicação, e pode ser usado na comparação de duas (e somente duas) médias e as variações dizem respeito às hipóteses que são testadas.</h5>

```{r testet}
t.test(dl$arr_delay, ua$arr_delay, alternative="greater")
```
<a name="7.2"> </a>
<h3><b>Etapa 7.2 - Valor p</h3></b>

<h5>O valor-p é uma quantificação da probabilidade de se errar ao rejeitar H0 e a mesma decorre da distribuição estatística adotada.
Se o valor-p é menor que o nível de significância, conclui-se que o correto é rejeitar a hipótese de nulidade.

Valor p é a probabiblidade de que a estatística do teste assuma um valor extremo em relação ao valor observado quando H0 é verdadeira.</h5>

<b>Estamos trabalhando com alfa igual a 0.05 (95% de confiança)</b>

 <b>Regra</b>

<h5>* Baixo valor p: forte evidência empírica contra h0
* Alto valor p: pouca ou nenhuma evidência empírica contra h0</h5>
<a name="8"> </a>

<h3><b>Etapa 8 - Conclusão</h3></b>

<h5>* Falhamos em rejeitar a hipótese nula, pois p-valor é maior que o nível de significância.
* Isso que dizer que há uma probabilidade alta de não haver diferença significativa entre os atrasos.
* Para os nossos dados, não há evidência estatística de que a DL atrase mais que a UA.</h5>

<p> <font color = white>...</font></p>

<h3><b>Dados Pessoais</h3></b>
<p class="shytext">Site <a href=https://www.rodolfoterra.com><strong> www.rodolfoterra.com</strong></a></p>

<p class="shytext">Linkedin <a href=https://www.linkedin.com/in/rodolffoterra/><strong>rodolffoterra</strong></a></p>

<p class="shytext">Repertório no GitHub: <a href=https://github.com/rodolffoterra/TestedeHipotese><strong>  Teste de Hipotese</strong></a></p>

<p class="shytext">E-mail <a href=consultoriaterra@hotmail.com><strong> consultoriaterra@hotmail.com</strong></a></p>
