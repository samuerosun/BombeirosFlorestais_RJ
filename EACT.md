\#EACT (Escala de Avaliação do Contexto do Trabalho) - EACT \#1
Estrutura: possui 27 itens, organizados em três fatores - Organização do
Trabalho (OT)

library(tidyverse) \#dplyr tidyr library(ggplot2) library(psych) dados
&lt;- read.csv(“/Users/samuel/Documents/Arquivos R/Aline
Bombeiros/tratados\_4\_R.csv”, sep = “,”, stringsAsFactors = FALSE) \#-
Itens: itens\_organizacao &lt;- c( “eact1”, “eact2”, “eact3”, “eact4”,
“eact5”, “eact6”, “eact7”, “eact8”, “eact9”, “eact10”, “eact11” )
\#Esses itens avaliam: ritmo de trabalho; \#pressão por resultados;
rigidez das normas; \#fiscalização; divisão do trabalho;
\#repetitividade; pausas insuficientes; \#descontinuidade das tarefas.
Condições de Trabalho (CT)

itens\_condicoes &lt;- c( “eact12”, “eact13”, “eact14”, “eact15”,
“eact16”, “eact17”, “eact26”, “eact27” ) \#Avaliam: ambiente físico;
mobiliário; instrumentos; materiais; riscos; espaço físico. Relações
Socioprofissionais (RS)

itens\_relacoes &lt;- c( “eact18”, “eact19”, “eact20”, “eact21”,
“eact22”, “eact23”, “eact24”, “eact25” ) \#Avaliam: comunicação;
integração; apoio da chefia; acesso às informações; conflitos.

1.  Construção dos indicadores \#library(dplyr) dados &lt;- dados %&gt;%
    mutate( organizacao\_trabalho = rowMeans(select(.,
    all\_of(itens\_organizacao)), na.rm = TRUE),

    condicoes\_trabalho = rowMeans(select(., all\_of(itens\_condicoes)),
    na.rm = TRUE),

    relacoes\_socioprofissionais = rowMeans(select(.,
    all\_of(itens\_relacoes)), na.rm = TRUE),

    contexto\_trabalho = rowMeans(select(.,all\_of(c(
    itens\_organizacao, itens\_condicoes, itens\_relacoes))), na.rm =
    TRUE))

\#3. Consistência interna \#library(psych) alpha(dados\[,
itens\_organizacao\]) alpha(dados\[, itens\_condicoes\]) alpha(dados\[,
itens\_relacoes\]) alpha(dados\[, c(itens\_organizacao,
itens\_condicoes, itens\_relacoes)\])

\#4. Classificação do contexto organizacional. EACT utiliza uma escala
de 1 a 5 dados &lt;- dados %&gt;% mutate( classificacao\_contexto =
case\_when(contexto\_trabalho &lt; 2.30 ~ “Baixo”, contexto\_trabalho
&gt;= 2.30 & contexto\_trabalho &lt;= 3.69 ~ “Moderado”,
contexto\_trabalho &gt; 3.70 ~ “Elevado”))

\#5. Frequencias dados %&gt;% count(classificacao\_contexto) %&gt;%
mutate( percentual = round(n/sum(n)\*100,1) )

\#6. Estatisticas Descritivas dados %&gt;% summarise( media\_OT =
mean(organizacao\_trabalho, na.rm=TRUE), dp\_OT =
sd(organizacao\_trabalho, na.rm=TRUE),

    media_CT = mean(condicoes_trabalho, na.rm=TRUE),
    dp_CT = sd(condicoes_trabalho, na.rm=TRUE),

    media_RS = mean(relacoes_socioprofissionais, na.rm=TRUE),
    dp_RS = sd(relacoes_socioprofissionais, na.rm=TRUE),

    media_global = mean(contexto_trabalho, na.rm=TRUE),
    dp_global = sd(contexto_trabalho, na.rm=TRUE)

)

\#EACT (Escala de Avaliação do Contexto do Trabalho) - EACT \#7.
Comparação entre as dimensões

\#1 Nunca \#2 Raramente \#3 Ás vezes \#4 Frequentemente \#5 Sempre
dados\_long &lt;- dados %&gt;% select( organizacao\_trabalho,
condicoes\_trabalho, relacoes\_socioprofissionais ) %&gt;%
pivot\_longer( everything(), names\_to=“Dimensao”, values\_to=“Escore”)

ggplot(dados\_long, aes(Dimensao,Escore))+ geom\_boxplot()+
theme\_minimal()
