
# 1 Verificação dos dados e limpeza

Quero filtrar aprenas a SC na coluna Sigla_SC

Verificar os nomes dos municípios para verificar erro de digitação 

Após esses 35 municípios em Nome_Municipio:
Araquari
Araranguá
Balneário Arroio da Silva
Balneário Barra do Sul
Balneário Camboriú
Balneário Gaivota
Balneário Piçarras
Balneário Rincão
Barra Velha
Biguaçu
Bombinhas
Florianópolis
Garopaba
Garuva
Governador Celso Ramos
Imbituba
Imaruí
Itajaí
Itapema
Itapoá
Jaraguna
Joinville
Laguna
Navegantes
Palhoça
Passo de Torres
Penha
Pescaria Brava
Porto Belo
Santa Rosa do Sul
São Francisco do Sul
São João do Sul
São José
Sombrio
Tijucas

Separar D-M-AAAA das colunas Data_Registro e Data_Evento, apesar de ambos aparentarem ser iguais, vamos sempre ficar de olho em ambas datas

# 2 Ánalise temporal

Agora podemos separar o mesmo registro por cada evento, porém ajustando grupo_de_desastre somente para:
Hidrológicos que incluem as descricao_tipologia como: Alagamentos, Chuvas Intensas, Enxurradas, Inundações, Movimento de Massa(Erosão?)
Excluindo:
Climatológico inclui: Estiagem e Seca, Incêndio Florestal
Metereológico inclui: Granizo, Tornado, Vendavais e Ciclones
Outros inclui: Doenças infecciosas, Erosão*, Onda de Calor e Baixa Umidade, Outros, Rompimento/Colapso das Barragens
*Interessante

série por ano/mês

média móvel e sazonalidade

comparação entre anos com mais eventos e mais prejuízos

Com isso - consigo ver quantos eventos tiveram registros por ano para cada cidade selecionada e classificar no gráfico de barras quais eventos foram com cores diferentes e acrescentar legenda nesse gráfico

Aqui eu já consigo associar ano com DH_MORTOS, DH_FERIDOS, DH_ENFERMOS, DH_DESABRIGADOS, DH_DESALOJADOS, DH_DESAPARECIDOS, DH_AFETADOS_SECA_ESTIAGEM,	DH_total_danos_humanos_diretos,
DH_OUTROS AFETADOS,	DM_Descricao,	DM_Uni Habita Danificadas,	DM_Uni Habita Destruidas,
DM_Uni Habita Valor,	DM_Inst Saúde Danificadas,	DM_Inst Saúde Destruidas,	DM_Inst Saúde Valor,	DM_Inst Ensino Danificadas,	DM_Inst Ensino Destruidas,
DM_Inst Ensino Valor,	DM_Inst Serviços Danificadas,	DM_Inst Serviços Destruidas,	DM_Inst Serviços Valor,	DM_Inst Comuni Danificadas,
DM_Inst Comuni Destruidas,	DM_Inst Comuni Valor,	DM_Obras de Infra Danificadas,	DM_Obras de Infra Destruidas,	DM_Obras de Infra Valor, DM_total_danos_materiais,	DA_Descricao,	DA_Polui/cont da água,
DA_Polui/cont do ar,	DA_Polui/cont do solo,	DA_Dimi/exauri hídrico,	DA_Incêndi parques/APA's/APP's,	PEPL_Descricao,	PEPL_Assis_méd e emergên(R$),
PEPL_Abast de água pot(R$),	PEPL_sist de esgotos sanit(R$),	PEPL_Sis limp e rec lixo (R$),	PEPL_Sis cont pragas (R$),	PEPL_distrib energia (R$),
PEPL_Telecomunicações (R$),	PEPL_Tran loc/reg/l_curso (R$),	PEPL_Distrib combustíveis(R$),	PEPL_Segurança pública (R$),	PEPL_Ensino (R$),	
PEPL_total_publico,	PEPR_Descricao,	PEPR_Agricultura (R$),	PEPR_Pecuária (R$),	PEPR_Indústria (R$),	PEPR_Comércio (R$),	PEPR_Serviços (R$),
PEPR_total_privado,	PE_PLePR

# 3 Análise espacial?

Tabelas e gráficos por município.
Com o mapa municipal, usar sf + ggplot2 para mapas coropléticos.
Cruzar com distância da costa, baixa altitude ou áreas urbanas, se isso fizer sentido para a pesquisa.

# 4 Análise de impactos

Somatórios e médias por tipologia.
Ranking dos municípios com maior dano humano/material/econômico.
Proporção de eventos com prejuízo zero versus eventos de alto impacto.

# 5 Análise multivariada

PCA ou clustering para agrupar eventos/municípios por perfil de impacto.
Correlação entre tipos de dano e tipologia do desastre.
Modelos exploratórios, como regressão para identificar fatores associados a maiores prejuízos.

<!-- PALPITES -->
Exploração em três níveis:

Nível 1: descrição geral da base.

Nível 2: eventos hidrológicos e de movimento de massa em SC.

Nível 3: recorte costeiro, focando municípios litorâneos e eventos extremos.

Permite passar de uma visão ampla para um recorte temático alinhado na dissertação.

