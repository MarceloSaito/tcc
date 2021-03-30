###########---------------------bibs--------------#######

library(foreign)
library(sf)
library(tidyr)
library(geobr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(viridisLite)
library(viridis)
library(Hmisc)

setwd("C:/Users/mn_sa/Documents/R/Version control/tcc")

###########---------------------grau de participacao sp-------------------####

# 
# muni_sp <- read_municipality(code_muni = "SP")
# 
# 
# dados_grau_part <- read.csv("C:/Users/mn_sa/Documents/R/Version control/tcc/grau_part.csv")
# 
# dados_grau_part_sp <- dados_grau_part %>%
#                 filter(estado=="SP") %>%
#                 select(codigo, grau_part) %>%
#                 rename(code_muni=codigo)
# 
# 
# 
# grau_part_geo_sp <- left_join(muni_sp, dados_grau_part_sp, by = "code_muni")
# 
# 
# ggplot(grau_part_geo_sp) +
#         geom_sf(aes(fill=grau_part, col=grau_part)) +
#         scale_fill_viridis()+
#         scale_color_viridis()




###########---------------------grau part brasil----------#####


muni_br <- read_municipality(code_muni = "all")

dados_grau_part <- read.csv("C:/Users/mn_sa/Documents/R/Version control/tcc/grau_part.csv")

dados_grau_part_br <- dados_grau_part %>% 
        select(codigo, grau_part) %>% 
        rename(code_muni=codigo) 



grau_part_geo_br <- left_join(muni_br, dados_grau_part_br, by = "code_muni")

ggplot(grau_part_geo_br) +
        geom_sf(aes(fill=grau_part, col=grau_part)) +
        scale_color_gradientn(colours = brewer.pal(9, 'RdYlGn'))+
        scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn' ))
       
        #scale_fill_viridis()+
        #scale_color_viridis()



###########---------------------pagamento da divida br metodo original -----------------####

# 
cap_pag_div <- read.csv("C:/Users/mn_sa/Documents/R/Version control/tcc/cap_pag_div.csv")

cap_pag_div1 <-cap_pag_div %>%
                mutate(nova_escala = (2 * cap_pag_div + 115.54) / 152.6) %>%
                rename(code_muni=codigo) %>%
                select(code_muni, nova_escala)




cap_pag_div_br_geo <- left_join( muni_br,cap_pag_div1, by = "code_muni")

ggplot(cap_pag_div_br_geo) +
        geom_sf(aes(fill=nova_escala, col=nova_escala, geometry = geom)) +
        scale_fill_viridis()+
        scale_color_viridis()



###########---------------------pagamento da divida lrf com dados sem e quad 1 a 2-------------------####


divida_lrf_dados <- read.csv("C:/Users/mn_sa/Documents/R/Version control/tcc/divida_lrf.csv")

divida_lrf_dados <- divida_lrf_dados %>% 
        mutate(indice_div = ((2 * (dcl / rcl) + 6.14) / 7.57)) %>% 
        select(indice_div, code_muni)

divida_lrf_dados_geo <- left_join(muni_br, divida_lrf_dados,  by = "code_muni")

ggplot(divida_lrf_dados_geo) +
        geom_sf(aes(fill = indice_div, col = indice_div, geometry = geom)) +
        scale_color_gradientn(colours = brewer.pal(9, 'RdYlGn'))+
        scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn' ))
    #    scale_fill_viridis()+
      #  scale_color_viridis()


###########---------------------versao pagamento da divida lrf com dados sem e quad de 1 a 6 eq1---------------------

# 
# muni_br <- read_municipality(code_muni = "all")
# 
# divida_lrf_dados <- read.csv("C:/Users/mn_sa/Documents/R/Version control/tcc/divida_lrf.csv")
# 
# # divida_lrf_dados <- divida_lrf_dados %>% 
# #         mutate(indice_div = ((6 * (dcl / rcl) + 18.42) / 7.57)) %>% 
# #         select(indice_div, code_muni)
# 
# divida_lrf_dados <- divida_lrf_dados %>% 
#   drop_na() %>% 
#   mutate(dcl_rcl = dcl / rcl,
#          eq1_dcl_rcl = (((dcl_rcl - min(dcl_rcl)) / (max(dcl_rcl) - min(dcl_rcl))) * 5) + 1)
# 
# divida_lrf_dados_geo <- left_join( muni_br, divida_lrf_dados, by = "code_muni")
# 
# ggplot(divida_lrf_dados_geo) +
#         geom_sf(aes(fill = eq1_dcl_rcl, col = eq1_dcl_rcl, geometry = geom)) +
#         scale_color_gradientn(colours = brewer.pal(9, 'RdYlGn'))+
#         scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn' ))

      #  scale_fill_viridis()+
       # scale_color_viridis()

###########---------------------versao pagamento da divida lrf com dados sem e quad de 1 a 2 todo no R---------------------

muni_br <- read_municipality(code_muni = "all")

quad <- read.csv("quad.csv", sep =";", skip = 5, header = T, dec = ",")

rcl_quad <- quad %>% 
  filter(Conta == "RECEITA CORRENTE LÍQUIDA - RCL",
         Coluna == "Até o 3º Quadrimestre",
         PODER == "Executivo") %>% 
  rename(code_muni = Cod.IBGE) %>% 
  select(code_muni, Valor)

dcl_quad <- quad %>% 
  filter(Conta == "DÍVIDA CONSOLIDADA LÍQUIDA (DCL) (III) = (I - II)",
         Coluna == "Até o 3º Quadrimestre",
         PODER == "Executivo") %>% 
  rename(code_muni = Cod.IBGE) %>% 
  select(code_muni, Valor)
  

sem <- read.csv("sem.csv", sep =";", skip = 5, header = T, dec = ",")

rcl_sem<- sem %>% 
  filter(Conta == "RECEITA CORRENTE LÍQUIDA - RCL",
         Coluna == "Até o 2º Semestre",
         PODER == "Executivo") %>% 
  rename(code_muni = Cod.IBGE) %>% 
  select(code_muni, Valor)

dcl_sem <- sem %>% 
  filter(Conta == "DÍVIDA CONSOLIDADA LÍQUIDA (DCL) (III) = (I - II)",
         Coluna == "Até o 2º Semestre",
         PODER == "Executivo") %>% 
  rename(code_muni = Cod.IBGE) %>% 
  select(code_muni, Valor)

# unindo rcl e dcl 

rcl_full <- bind_rows(rcl_quad, rcl_sem) %>% 
  rename(rcl = Valor)

dcl_full <- bind_rows(dcl_quad, dcl_sem) %>% 
  rename(dcl = Valor)

muni_rcl <- left_join(muni_br, rcl_full, by = "code_muni")

#essa versao considera todos os valores na escala de 1 a 2
# 
# muni_rcl_dcl_geo <- left_join(muni_rcl, dcl_full, by = "code_muni") %>% 
#   drop_na() %>% ####essa é a linha de código que retira informações de alguns municipios
#   mutate(dcl_rcl = dcl / rcl, 
#   rcl_dcl_12 = (((2 * dcl_rcl) - (2 * min(dcl_rcl))) / (max(dcl_rcl) - min(dcl_rcl)) - 2) * (-1)
#   ) %>%
#   select(name_muni,code_muni, abbrev_state ,code_state, dcl_rcl, rcl_dcl_12, geom)
# 
# ggplot(muni_rcl_dcl_geo) +
#   geom_sf(aes(fill = rcl_dcl_12, col = rcl_dcl_12, geometry = geom)) +
#   scale_color_gradientn(colours = brewer.pal(9, 'RdYlGn'))+
#   scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn' ))
  
#essa versao considera apenas numeros entre 0 e 1.2

muni_rcl_dcl_geo <- left_join(muni_rcl, dcl_full, by = "code_muni") %>% 
  drop_na() %>% ####essa é a linha de código que retira informações de alguns municipios
  mutate(dcl_rcl = dcl / rcl, 
         dcl_rcl_02 = case_when(
          dcl_rcl > 1.2 ~ 2,
          dcl_rcl < 0 ~ 0,
          TRUE ~ as.numeric(as.character(dcl_rcl))),
         indice_02 = (((2 * dcl_rcl_02) - (2 * min(dcl_rcl_02))) / (max(dcl_rcl_02) - min(dcl_rcl_02)) - 2) * (-1)
         )%>%
  select(name_muni,code_muni, abbrev_state ,code_state, dcl_rcl, dcl_rcl_02, indice_02, geom)
  


ggplot(muni_rcl_dcl_geo) +
  geom_sf(aes(fill = indice_02, col = indice_02, geometry = geom)) +
  scale_color_gradientn(colours = brewer.pal(9, 'RdYlGn'))+
  scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn' ))



###########---------------------lrf sp ----------------#################


# muni_sp <- read_municipality(code_muni = "SP")
# 
# indice_lrf_dados <- read.csv("C:/Users/mn_sa/Documents/R/Version control/tcc/indice_lrf.csv")
# 
# indice_lrf_dados <- indice_lrf_dados %>% 
#         select(code_muni, indice_lrf)
# 
# indice_lrf_sp <- left_join(muni_sp,indice_lrf_dados, by = "code_muni") %>% 
#         filter(code_state==35)
# 
# ggplot(indice_lrf_sp)+
#         geom_sf(aes(fill = indice_lrf, col = indice_lrf, geometry = geom)) +
#         scale_color_gradientn(colours = brewer.pal(9, 'RdYlGn'))+
#         scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn' ))
        #scale_fill_viridis()+
       # scale_color_viridis()


###########---------------------consorcios intermunicipais max=12-------------############


muni_br<- read_municipality(code_muni = "all")

cons_inter <- read.csv("C:/Users/mn_sa/Documents/R/Version control/tcc/cons_inter.csv")

cons_inter <- cons_inter %>% 
        select(code_muni, eq1) %>% 
        rename(eq1_cons_inter = eq1)

cons_inter_geo <- left_join(muni_br,cons_inter, by = "code_muni")


ggplot(cons_inter_geo)+
        geom_sf(aes(fill = eq1, col = eq1, geometry = geom)) +
        scale_color_gradientn(colours = brewer.pal(9, 'RdYlGn'))+
        scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn' ))


###########---------------------prpcapita sem limite---------------------#################

prpcapita <- read.csv("C:/Users/mn_sa/Documents/R/Version control/tcc/prcapita.csv")

muni_br<- read_municipality(code_muni = "all")

prpcapita <- prpcapita %>% 
  drop_na() %>% 
  mutate(eq1_prpcapita2 = (((prpc - min(prpc)) / (max(prpc) - min(prpc))) * 5) + 1) %>% 
  rename(code_muni = codigo)

# prpcapita <- prpcapita %>% 
#   rename(code_muni = codigo) %>% 
#   select(code_muni, eq1_prpcapita)
# 

prpcapita_geo <- left_join(muni_br, prpcapita, by = "code_muni")

ggplot(prpcapita_geo)+
  geom_sf(aes(fill = eq1_prpcapita2, col = eq1_prpcapita2, geometry = geom)) +
  scale_color_gradientn(colours = brewer.pal(9, 'RdYlGn'))+
  scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn' ))


###########---------------------prpcapita limite 3k---------------------#################

muni_br<- read_municipality(code_muni = "all")

prpcapita <- read.csv("C:/Users/mn_sa/Documents/R/Version control/tcc/prcapita.csv")

prpcapita <- prpcapita %>% 
  drop_na() %>% 
  mutate(eq1_prpcapita3k = (((prpcapita_max3k - min(prpcapita_max3k )) / (max(prpcapita_max3k ) - min(prpcapita_max3k ))) * 5) + 1) %>% 
  rename(code_muni = codigo) %>% 
  select(code_muni, eq1_prpcapita3k)

# prpcapita <- prpcapita %>% 
#   rename(code_muni = codigo) %>% 
#   select(code_muni, eq1_prpcapita)
# 

prpcapita_geo <- left_join(muni_br, prpcapita, by = "code_muni")

ggplot(prpcapita_geo)+
  geom_sf(aes(fill = eq1_prpcapita3k, col = eq1_prpcapita3k, geometry = geom)) +
  scale_color_gradientn(colours = brewer.pal(9, 'RdYlGn'))+
  scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn' ))



###########---------------------capacidade financeira---------------###############


cap_fin <- full_join(muni_rcl_dcl_geo, prpcapita, by = "code_muni") 

cap_fin_full <- full_join(cap_fin, cons_inter, by = "code_muni")
  
capacidade_financeira_dados <- cap_fin_full %>% 
  mutate(capacidade_financeira = (eq1_prpcapita3k * 1/3) + (indice_02) + (eq1_cons_inter * 1/3)) %>% 
  select(name_muni, code_muni, code_state, abbrev_state,
         capacidade_financeira, geom)

ggplot(capacidade_financeira_dados)+
  geom_sf(aes(fill = capacidade_financeira, col = capacidade_financeira, geometry = geom)) +
  scale_color_gradientn(colours = brewer.pal(9, 'RdYlGn'))+
  scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn' ))


###########---------------------capacidade gerencial---------------###############
muni_br<- read_municipality(code_muni = "all")

cap_ger <- read.csv("C:/Users/mn_sa/Documents/R/Version control/tcc/cap_ger.csv")

cap_ger <- cap_ger %>% 
  mutate(iptu = if_else(iptu==1, 6, 1),
         ano_da_lei = case_when(ano_da_lei < 1970 ~ 6,
                                ano_da_lei %in% 1970:1990 ~ 5,
                                ano_da_lei > 1991 ~4),
         iptu_ano_lei = ((iptu * 533 / 8.33) + (ano_da_lei * 300 / 8.33)) / 100,
         soma = cadastro_imo + cad_im_info + planta._gen + planta_gen_info,
         eq1_iptu_cad_imo = (((soma - min(soma)) / (max(soma) -min(soma))) * 5) + 1,
         soma2 = lei_zoneamento + lei_parcelamento + codigo_obras + lei_contribuicao,
         eq1_instru_gestao = (((soma2 - min(soma2)) / (max(soma2) -min(soma2))) * 5) + 1,
         soma3 = plano_diretor + reg_fund + leg_zn_esp + leg_impac_amb,
         eq1_instru_plan = (((soma3 - min(soma3)) / (max(soma3) -min(soma3))) * 5) + 1,
         cg = (iptu_ano_lei * 1/4) + (eq1_instru_gestao * 1/4) +(eq1_iptu_cad_imo * 1/4) + (eq1_instru_plan * 1/4)
         ) 

ind_final <- cap_ger %>% 
  select(code_muni, cg)

ind_final_geo <- left_join(muni_br, ind_final, by = "code_muni")  

ggplot(ind_final_geo)+
  geom_sf(aes(fill = cg, col = cg, geometry = geom)) +
  scale_color_gradientn(colours = brewer.pal(9, 'RdYlGn'))+
  scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn' ))
  
###########---------------------IQIM-----------------------#####

iqim1 <- left_join(capacidade_financeira_dados, ind_final, by = "code_muni")

iqim <- left_join(iqim1, dados_grau_part_br, by = "code_muni") %>% 
  mutate(iqim_final = (capacidade_financeira * 1/3) + (cg * 1/3) + (grau_part * 1/3))

ggplot(iqim) +
  geom_sf(aes(fill = iqim_final, col = iqim_final)) +
  scale_color_gradientn(colours = brewer.pal(9, 'RdYlGn'))+
  scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn' ))


write.csv2(iqim, "iqim.csv", row.names = F)

write.dta(iqim, file = "iqim.dta")


