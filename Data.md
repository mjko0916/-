## Data

프로젝트에 사용된 데이터는 질병관리본부 국민건강영양조사 홈페이지(https://knhanes.cdc.go.kr/knhanes/main.do) 를 통해 얻을 수 있다.
7기(2016-2017년)에 행해진 데이터를 사용하였는데,
2016년 데이터는 총 8150개의 관측치와 775개의 변수, 2017년 데이터는 8127개의 관측치와 826개의 변수를 가지고 있다. 
국민건강영양조사는 설문조사를 통해 이루어졌으므로 여러 주관이 섞여있기 때문에 최대한 객관적인 수치인 변수를 선택하여 분석을 진행하였다.
분석에 사용된 데이터는 다음과 같다.


R에서는 대용량 데이터를 다루기 어려우므로 이 변수들을 추출하는 과정은 SAS를 이용하였다.
```
DATA data2016;
set 'C:\Users\User\Desktop\graduate\보건의료빅데이터\프로젝트\hn16_all.sas7bdat';
keep DI1_dg DI2_dg DI3_dg DI4_dg LW_ms edu sex age BO1_2 BO1_3 BD1 BD1_11 Total_slp_wk Total_slp_wd mh_stress BS1_1 BS3_2 BE3_33 pa_aerobic HE_fh--HE_STRfh3 HE_rPLS HE_mPLS HE_sbp HE_dbp HE_ht--HE_obe HE_DM HE_chol HE_HCHOL HE_HBsAg--HE_hepaC HE_BUN HE_crea HE_Uacid HE_Pb--HE_Cd;
run;

DATA data2017;
set 'C:\Users\User\Desktop\graduate\보건의료빅데이터\프로젝트\hn17_all.sas7bdat';
keep DI1_dg DI2_dg DI3_dg DI4_dg LW_ms edu sex age BO1_2 BO1_3 BD1 BD1_11 Total_slp_wk Total_slp_wd mh_stress BS1_1 BS3_2 BE3_33 pa_aerobic HE_fh--HE_STRfh3 HE_rPLS HE_mPLS HE_sbp HE_dbp HE_ht--HE_obe HE_DM HE_chol HE_HCHOL HE_HBsAg--HE_hepaC HE_BUN HE_crea HE_Uacid HE_Pb--HE_Cd;
run;

proc export data=data2016 
outfile = 'C:\Users\User\Desktop\graduate\보건의료빅데이터\프로젝트\data2016.csv'
dbms = dlm replace;
delimiter=",";
run;

proc export data=data2017 
outfile = 'C:\Users\User\Desktop\graduate\보건의료빅데이터\프로젝트\data2017.csv' 
dbms = dlm replace;
delimiter=",";
run;
```
