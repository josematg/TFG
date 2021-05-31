use "C:\Users\josem\Documents\Archivos Medicina\TFG\Raw Data\inocentes.dta"

gen horacat=.
replace horacat=2 if hora>11 & hora<18
replace horacat=0 if hora<6
replace horacat=1 if hora>5 & hora<12
replace horacat=3 if hora>17 & hora<24

gen viacat=.
replace viacat=0 if tipovia<4
replace viacat=1 if tipovia>3 & tipovia<7
replace viacat=2 if tipovia==9
replace viacat=3 if tipovia==7 | tipovia==8 | tipovia==10 | tipovia==11 | tipovia==12 | tipovia==13 | tipovia==14

gen densicat=.
replace densicat=0 if CONDICION_NIVEL_CIRCULA==1
replace densicat=1 if CONDICION_NIVEL_CIRCULA==2
replace densicat=2 if CONDICION_NIVEL_CIRCULA==3
replace densicat=3 if CONDICION_NIVEL_CIRCULA==4 | CONDICION_NIVEL_CIRCULA==5

gen luzcat=.
replace luzcat = 0 if luz == 1
replace luzcat = 1 if luz == 2
replace luzcat = 2 if luz == 3
replace luzcat = 3 if luz == 4
replace luzcat = 4 if luz == 5 | luz == 6

# VIACAT
xi: mlogit viacat i.sexo, base(0) rrr
estimates store m1
xi: mlogit viacat i.cat_edad*i.sexo, base(0) rrr
estimates store m2
lrtest m1 m2

# HORACAT
xi: mlogit horacat i.sexo, base(0) rrr
estimates store m1
xi: mlogit horacat i.cat_edad*i.sexo, base(0) rrr
estimates store m2
lrtest m1 m2

# DENSICAT
xi: mlogit densicat i.sexo, base(0) rrr
estimates store m1
xi: mlogit densicat i.cat_edad*i.sexo, base(0) rrr
estimates store m2
lrtest m1 m2

#LUZCAT
xi: mlogit luzcat i.sexo, base(0) rrr
estimates store m1
xi: mlogit luzcat i.cat_edad*i.sexo, base(0) rrr
estimates store m2
lrtest m1 m2

lincom  [1]_Isexo_2, rrr
lincom  [1]_Isexo_2 + [1]_IcatXsex_2_2, rrr
lincom  [1]_Isexo_2 + [1]_IcatXsex_3_2, rrr
lincom  [1]_Isexo_2 + [1]_IcatXsex_4_2, rrr
lincom  [1]_Isexo_2 + [1]_IcatXsex_5_2, rrr
lincom  [1]_Isexo_2 + [1]_IcatXsex_6_2, rrr
lincom  [1]_Isexo_2 + [1]_IcatXsex_7_2, rrr
lincom  [1]_Isexo_2 + [1]_IcatXsex_8_2, rrr
lincom  [1]_Isexo_2 + [1]_IcatXsex_9_2, rrr
lincom  [1]_Isexo_2 + [1]_IcatXsex_10_2, rrr
lincom  [1]_Isexo_2 + [1]_IcatXsex_11_2, rrr
lincom  [1]_Isexo_2 + [1]_IcatXsex_12_2, rrr
lincom  [1]_Isexo_2 + [1]_IcatXsex_13_2, rrr

lincom  [2]_Isexo_2, rrr
lincom  [2]_Isexo_2 + [2]_IcatXsex_2_2, rrr
lincom  [2]_Isexo_2 + [2]_IcatXsex_3_2, rrr
lincom  [2]_Isexo_2 + [2]_IcatXsex_4_2, rrr
lincom  [2]_Isexo_2 + [2]_IcatXsex_5_2, rrr
lincom  [2]_Isexo_2 + [2]_IcatXsex_6_2, rrr
lincom  [2]_Isexo_2 + [2]_IcatXsex_7_2, rrr
lincom  [2]_Isexo_2 + [2]_IcatXsex_8_2, rrr
lincom  [2]_Isexo_2 + [2]_IcatXsex_9_2, rrr
lincom  [2]_Isexo_2 + [2]_IcatXsex_10_2, rrr
lincom  [2]_Isexo_2 + [2]_IcatXsex_11_2, rrr
lincom  [2]_Isexo_2 + [2]_IcatXsex_12_2, rrr
lincom  [2]_Isexo_2 + [2]_IcatXsex_13_2, rrr

lincom  [3]_Isexo_2, rrr
lincom  [3]_Isexo_2 + [3]_IcatXsex_2_2, rrr
lincom  [3]_Isexo_2 + [3]_IcatXsex_3_2, rrr
lincom  [3]_Isexo_2 + [3]_IcatXsex_4_2, rrr
lincom  [3]_Isexo_2 + [3]_IcatXsex_5_2, rrr
lincom  [3]_Isexo_2 + [3]_IcatXsex_6_2, rrr
lincom  [3]_Isexo_2 + [3]_IcatXsex_7_2, rrr
lincom  [3]_Isexo_2 + [3]_IcatXsex_8_2, rrr
lincom  [3]_Isexo_2 + [3]_IcatXsex_9_2, rrr
lincom  [3]_Isexo_2 + [3]_IcatXsex_10_2, rrr
lincom  [3]_Isexo_2 + [3]_IcatXsex_11_2, rrr
lincom  [3]_Isexo_2 + [3]_IcatXsex_12_2, rrr
lincom  [3]_Isexo_2 + [3]_IcatXsex_13_2, rrr
