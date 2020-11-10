# Code 0: Process ------------------------------------------------------------
# Valentina Andrade

#0. Cargar librerias ---------------------------------------------------------
pacman::p_load(tidyverse, ggplot2, readr)

# 1. Cargar base de dato -----------------------------------------------------
files <- file.info(Sys.glob("input/data/*19.csv"))
movid <- data.table::fread(row.names(files)[which.max(files[["mtime"]])], sep = ",", encoding = "UTF-8")

# 1.1 Explorar base -----------------------------------------------------------
names(movid)

# 2. Renombrando -------------------------------------------------------------
mv <- movid %>% 
  rename(
    fecha = fecha_obs,
    sexo = r2_sexo,
    region = u1_region,
    comuna = u2_comuna,
    educ = r5_educ,
    tra_salud = pr1_wrk_salud,
    prev = pr2_prevision)

# 3 Recodificacion ----------------------------------------------------------

# 3.1 Fecha -------------------
mv <- mv %>% 
  mutate(fecha_ymd = as.Date(fecha))

mv$semana <- ifelse(mv$semana==15, 16, mv$semana)
# 3.2 Trabajador salud  -------------------
mv$tra_salud_dic <- ifelse(mv$tra_salud== 1, 1, # Si
                           ifelse(mv$tra_salud== 0, 0, NA))

mv$sexo_trasalud <- ifelse(mv$sexo=="Femenino" & mv$tra_salud==1, "Mujer trabajadora de salud",
                           ifelse(mv$sexo=="Femenino" & mv$tra_salud==0, "Mujer no trabajadora de salud",  
                                  ifelse(mv$sexo=="Masculino" & mv$tra_salud==1, "Hombre trabajador de salud",
                                         ifelse(mv$sexo=="Masculino" & mv$tra_salud==0, "Hombre no trabajador de salud", NA))))

# 3.3 Educacion -------------------
mv$educ_4cat <- ifelse(mv$educ=="Sin estudios" | mv$educ=="Educación Básica (primaria o preparatoria)", "Basica o sin estudios",
                       ifelse(mv$educ == "Educación Media (Humanidades)", "Media",
                              ifelse(mv$educ == "Educación Profesional (Carreras de 4 o más años)", "Profesional",
                                     ifelse(mv$educ == "Educación Técnica Nivel Superior (Carreras de 1 a 3 años)", "Tecnica", NA))))
mv$educ_3cat <- ifelse(mv$educ=="Sin estudios" | mv$educ=="Educación Básica (primaria o preparatoria)" | mv$educ == "Educación Media (Humanidades)", "Media o menos",
                       ifelse(mv$educ == "Educación Profesional (Carreras de 4 o más años)", "Profesional",
                              ifelse(mv$educ == "Educación Técnica Nivel Superior (Carreras de 1 a 3 años)", "Técnica", NA)))

mv$educ_2cat <- ifelse(mv$educ=="Sin estudios" | mv$educ=="Educación Básica (primaria o preparatoria)" | mv$educ == "Educación Media (Humanidades)", "Media o menos",
                       ifelse(mv$educ == "Educación Profesional (Carreras de 4 o más años)" | mv$educ == "Educación Técnica Nivel Superior (Carreras de 1 a 3 años)", "Más que media", NA))

# 3.3 Edad sexo  -------------------
mv$edad_3cat <- ifelse(mv$edad<40, "18 a 39",
                       ifelse(mv$edad<65 & mv$edad>39, "40 a 64",
                              ifelse(mv$edad>64, "65 y más", NA)))

mv$sexo_edad <- ifelse(mv$sexo=="Masculino" & mv$edad<65, "Hombre menor a 65",
                       ifelse(mv$sexo=="Masculino" & mv$edad>64, "Hombre mayor a 65",
                              ifelse(mv$sexo=="Femenino" & mv$edad<65, "Mujer menor a 65",
                                     ifelse(mv$sexo=="Femenino" & mv$edad>64, "Mujer mayor a 65", NA))))
# 3.4 Practicas -------------------
mv$dic_trabajo <- ifelse(mv$p1_pra_trabajo==0, 0,
                         ifelse(mv$p1_pra_trabajo>0, 1, NA))

mv$dic_tramite <- ifelse(mv$p1_pra_tramite==0, 0,
                         ifelse(mv$p1_pra_tramite>0, 1, NA))

mv$dic_visita <- ifelse(mv$p1_pra_visita==0, 0,
                        ifelse(mv$p1_pra_visita>0, 1, NA))

mv$dic_recrea <- ifelse(mv$p1_pra_recrea==0, 0,
                        ifelse(mv$p1_pra_recrea>0, 1, NA))

mv$dic_transporte <- ifelse(mv$p1_pra_transporte==0, 0,
                            ifelse(mv$p1_pra_transporte>0, 1, NA))

mv$dic_invitado <- ifelse(mv$p1_pra_invitado==0, 0,
                          ifelse(mv$p1_pra_invitado>0, 1, NA))

mv$dic_otro <- ifelse(mv$p1_pra_otro==0, 0,
                      ifelse(mv$p1_pra_otro>0, 1, NA))

mv$dic_practicas <- ifelse((mv$dic_trabajo==0 & mv$dic_tramite==0 & mv$dic_invitado==0 &
                              mv$dic_recrea==0 & mv$dic_transporte==0 & mv$dic_visita==0), 0,
                           ifelse((mv$dic_trabajo>0 | mv$dic_tramite>0 | mv$dic_invitado>0 |
                                     mv$dic_recrea>0 | mv$dic_transporte>0 | mv$dic_visita>0), 1, NA))

mv$n_salidas <- (mv$p1_pra_trabajo+mv$p1_pra_recrea+mv$p1_pra_tramite+mv$p1_pra_transporte)

# 3.5 Sintomas -------------------
mv$sintoma <- ifelse((mv$s1_snt_fiebre==1 | mv$s1_snt_anosmia==1 | mv$s1_snt_disnea==1 | mv$s1_snt_tos==1 |
                        mv$s1_snt_mialgias==1 | mv$s1_snt_odinofagia==1 | mv$s1_snt_dol_torax==1 |
                        mv$s1_snt_cefalea==1 | mv$s1_snt_diarrea==1 | mv$s1_snt_disgeusia==1), 1,
                     ifelse((mv$s1_snt_fiebre==0 & mv$s1_snt_anosmia==0 & mv$s1_snt_disnea==0 & mv$s1_snt_tos==0 &
                               mv$s1_snt_mialgias==0 & mv$s1_snt_odinofagia==0 & mv$s1_snt_dol_torax==0 &
                               mv$s1_snt_cefalea==0 & mv$s1_snt_diarrea==0 & mv$s1_snt_disgeusia==0), 0, NA))
mv$sintoma <- ifelse(mv$s1_snt_null==1, 0, mv$sintoma)



# 3.6 Actividad -------------------------
# Actividad mezclando actividad de semana referencia y normalmente en el pasado
mv <- mv %>% 
  mutate(actividad = case_when(
    str_detect(pr4_wrk_lastw, "No realicé ") & educ_2cat == "Media o menos" & 
      str_detect(pr6_ocup_normal, "cuenta propia") ~ "Cuenta propia baja",
    str_detect(pr4_wrk_lastw, "No realicé ") & educ_2cat == "Más que media" & 
      str_detect(pr6_ocup_normal, "cuenta propia") ~ "Cuenta propia alta",
    educ_2cat == "Media o menos" & 
      str_detect(pr4_wrk_lastw, "cuenta propia") ~ "Cuenta propia baja",
    educ_2cat == "Más que media" & 
      str_detect(pr4_wrk_lastw, "cuenta propia") ~ "Cuenta propia alta",
    is.na(educ_2cat) & str_detect(pr4_wrk_lastw, "cuenta propia") ~ NA_character_,
    str_detect(pr4_wrk_lastw, "doméstico") | 
      str_detect(pr6_ocup_normal, "doméstico") ~ "Casa particular",
    str_detect(pr4_wrk_lastw, "público") | 
      str_detect(pr6_ocup_normal, "público") ~ "Asalariado/a público",
    str_detect(pr4_wrk_lastw, "privada") | 
      str_detect(pr6_ocup_normal, "privada") ~ "Asalariado/a privado",
    str_detect(pr4_wrk_lastw, "propia empresa") | 
      str_detect(pr6_ocup_normal, "propia empresa") ~ "Empleador",
    !is.na(pr4_wrk_lastw) ~ pr4_wrk_lastw,
    !is.na(pr6_ocup_normal) ~ pr6_ocup_normal,
    TRUE ~ pr6_ocup_normal),
    actividad2 = case_when(
      actividad == "Asalariado/a privado" & 
        educ_2cat == "Media o menos" ~ "Privado baja",
      actividad == "Asalariado/a privado" & 
        educ_2cat == "Más que media" ~ "Privado alta",
      actividad == "Asalariado/a privado" ~ NA_character_,
      TRUE ~ actividad),
    actividad3 = case_when(
      pr3_ocupacion == "Desempleado o desempleada" ~ "Desempleado",
      TRUE ~ actividad))

# 3.7 Semanas - Tiempo  -------------------
# miercoles de la semana anterior
# mv$fecha_ultima_obs
# table( mv$fecha_ultima_obs %>% as.Date(), mv$semana )
# 
# lubridate::wday(Sys.Date(), week_start = 1)
# mv <- mv %>% 
#   mutate(semana_fecha_miercoles = as.Date(paste(2020, semana - 1, 3, sep="-"), "%Y-%U-%u"),
#     semana_fecha_miercoles = if_else(
#       lubridate::wday(as.Date(fecha), week_start = 1) > 3,
#       semana_fecha_miercoles + lubridate::days(7),
#       semana_fecha_miercoles)) %>% 
#   rename(semana_fecha = semana_fecha_miercoles)

mv <- mv %>%
  mutate(semana_fecha = as.Date(paste(2020, semana - 1, 3, sep="-"), "%Y-%U-%u"),
         fecha_date = as.Date(fecha))

table(mv$semana)

# considerar semana completas
semanas_incompletas <- mv %>% 
  count(semana, fecha_date) %>%
  group_by(semana) %>% 
  mutate(dias = n()) %>%
  filter(dias < 7) %>% 
  distinct(semana)

mv <- mv %>% 
  anti_join(semanas_incompletas, by = "semana")

# mv %>% 
#   count(as.Date(fecha)) %>% 
#   tail(10)
# 3.8 Caso probable --------------------
mv <- mv %>% 
  mutate(caso_probable2 =  contacto == 1 & sosp_minsal0530 == 1)


# 3.9 Prevision  ----------------------------------------------------------------
mv <- mv %>% 
  mutate(prev = ifelse(stringr::str_detect(prev, "Otra"), "Otra", prev))


# 4. Selector ----------------------------------------------------------------
# variable auxiliar para el selector dedesagregación
mv <- mv %>% mutate(
  todo = "Total")

names(mv)

# 5. Acceso NC ------
# nc1_problema
table(mv$nc1_problema_seg, useNA = "ifany")
table(mv$nc1_problema_reg, useNA = "ifany")

# nc2_consulta
table(mv$nc2_consulta_reg, useNA = "ifany") # No está

# Posponen consulta nc3_posp_consulta
table(mv$nc3_posp_consulta_reg, useNA = "ifany")
table(mv$nc3_posp_consulta_seg, useNA = "ifany")

table(mv$nc3_posp_vacuna_seg, useNA = "ifany")
table(mv$nc3_posp_vacuna_reg, useNA = "ifany")

table(mv$nc3_posp_examen_seg, useNA = "ifany")
table(mv$nc3_posp_examen_reg, useNA = "ifany")

table(mv$nc3_posp_insumos_seg, useNA = "ifany")
table(mv$nc3_posp_insumos_reg, useNA = "ifany")

table(mv$nc3_posp_cirugia_seg, useNA = "ifany")
table(mv$nc3_posp_cirugia_reg, useNA = "ifany")

table(mv$nc3_posp_otra_seg, useNA = "ifany")
table(mv$nc3_posp_otra_reg, useNA = "ifany")

table(mv$nc3_posp_null_seg, useNA = "ifany")
table(mv$nc3_posp_null_reg, useNA = "ifany")

table(mv$nc3_posp_otra_seg_TEXT, useNA = "ifany")
table(mv$nc3_posp_otra_reg_TEXT, useNA = "ifany")


# Razones para posponer consultade enfermedades crónicas

table(mv$nc4_pq_nimporta_reg, useNA = "ifany")
table(mv$nc4_pq_nimporta_seg, useNA = "ifany")

table(mv$nc4_pq_costo_reg, useNA = "ifany")
table(mv$nc4_pq_costo_seg, useNA = "ifany")

table(mv$nc4_pq_nosabia_reg, useNA = "ifany")
table(mv$nc4_pq_nosabia_seg, useNA = "ifany")

table(mv$nc4_pq_tiempo_reg, useNA = "ifany")
table(mv$nc4_pq_tiempo_seg, useNA = "ifany")

table(mv$nc4_pq_empeorar_reg, useNA = "ifany")
table(mv$nc4_pq_empeorar_seg, useNA = "ifany")

table(mv$nc4_pq_temor_reg, useNA = "ifany")

table(mv$nc4_pq_temor_seg, useNA = "ifany")

table(mv$nc4_pq_leves_reg, useNA = "ifany")
table(mv$nc4_pq_leves_seg, useNA = "ifany")

table(mv$nc4_pq_sistlleno_reg, useNA = "ifany")
table(mv$nc4_pq_sistlleno_seg, useNA = "ifany")

table(mv$nc4_pq_transporte_reg, useNA = "ifany")
table(mv$nc4_pq_transporte_seg, useNA = "ifany")

table(mv$nc4_pq_cuidados_reg, useNA = "ifany")
table(mv$nc4_pq_cuidados_seg, useNA = "ifany")

table(mv$nc4_pq_cancela_reg, useNA = "ifany")
table(mv$nc4_pq_cancela_seg, useNA = "ifany")

table(mv$nc4_pq_otra_reg, useNA = "ifany")
table(mv$nc4_pq_otra_seg, useNA = "ifany")

table(mv$nc4_pq_otra_seg_TEXT, useNA = "ifany")
table(mv$nc4_pq_otra_reg_TEXT, useNA = "ifany")


# cuidados ----------------------------------------------------------------
mv$dic_cui <- ifelse(mv$cui1_dep_null == 1, 0,1)
table(mv$cui1_dep_null)
table(mv$dic_cui)

mv$cui2_d <- car::recode(mv$cui2_delta, c("'Han aumentado, más de 8 horas diarias adicionales'='Aumentó más 8 hrs.';
                             'Han aumentado, entre 4 y 8 horas diarias adicionales'='Aumentó 4 a 8 hrs.';
                             'Han aumentado, entre 1 y 3 horas diarias adicionales'='Aumentó 1 a 3 hrs.';
                             'No han aumentado ni disminuido'='Sigue igual';
                             'Han disminuido'='Ha disminuido'"), as.factor= T,
                             levels = c('Aumentó más 8 hrs.', 'Aumentó 4 a 8 hrs.', 'Aumentó 1 a 3 hrs.',
                                        'Sigue igual','Ha disminuido'))

table(mv$cui2_delta)

mv$dic_cui_d <- ifelse(mv$cui2_d == 'Ha disminuido', "Disminuyó",
                       ifelse(mv$cui2_d == 'Sigue igual', "Sigue igual","Aumentó"))

table(mv$dic_cui_d)
# exportar ----------------------------------------------------------------
saveRDS(mv, "output/movid.rds")

