# попередній аналіз декларацій за 2018 рік, дані включають дохід декларанта і членів їх сімей
# аналізуємо лише зарплати

library("dplyr")
library("readr")
library("stringr")
library("tidyr")
library("ggplot2")
library("scales")
library("tm")

setwd("/home/yevheniia/python/declarations/")
doc = read.csv("declarants_salaries.csv", stringsAsFactors = F, header=F)
spheres <- read.csv("/home/yevheniia/python/declarations/salary_classificator.csv", header=F, stringsAsFactors = F) %>% select(1,2)

filteredData = doc %>% 
  group_by(V3, V4, V5, V6, V8, V9) %>% 
  transmute(total=sum(V7)) %>% 
  unique() %>% 
  ungroup() %>% 
  filter(total < 2000000 & total > 0)

# зарплати за професіями
salaries = filteredData %>% 
  filter(V6 == "Заробітна плата отримана за основним місцем роботи" ) %>% 
  filter(total > 44676 ) %>% 
  mutate(V5 = sub("-", " ", V5)) %>% 
  mutate(V5 = tolower(V5)) %>% 
  mutate(V4 = tolower(V4)) %>% 
  mutate(V5 = trimws(V5)) %>% 
  mutate(V5 = removeNumbers(V5)) %>% 
  mutate(V5 = stripWhitespace(V5)) %>% 
  mutate(V5 = removePunctuation(V5)) %>% 
  filter(str_detect(V4, "[А-Яа-я]") & str_detect(V5, "[А-Яа-я]")) %>% 
  filter(!str_detect(V5, "пенсіонер|не прац|не ма(є|ю)")) %>% 
  filter(!str_detect(V5, "абзац"))%>% 
  select(1,2,3,7,5,6)

# to print out column result separeted with comma and with qouts
dput(as.character(proflist$Var1))

professions_list = c("адміністратор", 
                     "бригадир", 
                     "бригадир колії",
                     "бухгалтер", 
                     "бухгалтер головний", 
                     "військовослужбовець", 
                     "водій", 
                     "вчитель", 
                     "виконавець головний державний",
                     "виконавець державний",
                     "інспектор державний", 
                     "інспектор головний", 
                     "інспектор провідний", 
                     "інспектор", 
                     "інспектор молодший",
                     "економіст", 
                     "інженер головний", 
                     "інженер", 
                     "лікар головний",
                     "лікар",
                     "ветлікар",
                     "фельдшер",
                     "механік головний",
                     "механік", 
                     "електромеханік",
                     "спеціаліст головний", 
                     "фахівець головний", 
                     "депутат",
                     "кадастровий реєстратор", 
                     "державний реєстратор", 
                     "директор", 
                     "диспетчер", 
                     "завідувач", 
                     "завідувач сектору",
                     "завідувач аптекою", 
                     "завідувач лабораторією",  
                     "заступник головного бухгалтера", 
                     "заступник директора",
                     "землевпорядник",
                     "командир",
                     "комірник", 
                     "лісничий", 
                     "майстер лісу",
                     "старший майстер лісу",
                     "майстер шляховий", 
                     "майстер", 
                     "старший майстер", 
                     "оперуповноважений",
                     "оперуповноважений старший", 
                     "офіцер",
                     "офіцер старший", 
                     "офіцер поліції", 
                     "старший офіцер поліції",
                     "рятувальник", 
                     "старший рятувальник", 
                     "поліцейський", 
                     "помічник лісничого", 
                     "помічник чергового", 
                     "провідний консультант адміністратор",  
                     "прокурор", 
                     "секретар ради", 
                     "секретар суду", 
                     "секретар", 
                     "голова міський",
                     "голова сільський/селищний",
                     "голова суду",
                     "голова РДА",
                     "голова правління",
                     "голова фермерського г-ва",
                     "голова комісії",
                     "голова райради",
                     "голова комітету/підкомітету ВР",
                     "голова інше",
                     "слідчий", 
                     "спеціаліст", 
                     "суддя", 
                     "судовий розпорядник", 
                     "черговий по станції",
                     "черговий",
                     "юрисконсульт", 
                     "радник посольства",
                     "ректор", 
                     "тракторист", 
                     "агент комерційний", 
                     "агроном", 
                     "архіваріус", 
                     "бібліотекар", 
                     "вихователь", 
                     "відповідальний виконавець", 
                     "завідувач ветлікарнею",
                     "медсестра", 
                     "детектив", 
                     "викладач", 
                     "викладач старший",
                     "казначей", 
                     "діловод", 
                     "документознавець",
                     "доцент", 
                     "касир", 
                     "менеджер", 
                     "військовий комісар",
                     "заступник головного інженера", 
                     "інспектор роти", 
                     "присяжний", 
                     "сапер", 
                     "радник мзс", 
                     "радник голови", 
                     "заступник голови інше", 
                     "фахівець", 
                     "юрист",
                     "заступник міського голови",
                     "заступник сільського/селищного голови",
                     "заступник голови РДА",
                     "заступник голови райради",
                     "заступник голови суду",
                     "заступник командира батальйону",
                     "заступник командира військової частини",
                     "заступник командира дивізіону",
                     "заступник командира роти",
                     "заступник командира взводу",
                     "заступник командира інше",
                     "заступник начальника управління",
                     "заступник начальника відділу",
                     "заступник начальника служби",
                     "заступник начальника станції",
                     "заступник начальника установи",
                     "заступник начальника філії",
                     "заступник начальника цеху",
                     "заступник начальника штабу",
                     "заступник начальника інше",
                     "психолог",
                     "проректор",
                     "професор",
                     "слюсар",
                     "аудитор",
                     "єгер",
                     "експерт головний",
                     "експерт державний",
                     "експерт",
                     "староста",
                     "енергетик",
                     "керуючий справами",
                     "радіотелеграфіст",
                     "тальман",
                     "старшина",
                     "науковий співробітник",
                     "штурман",
                     "хімік",
                     "технік",
                     "ординатор",
                     "соціальний працівник",
                     "сержант", 
                     "пожежний-рятувальник",
                     "начальник конвою (гупн)",
                     "начальник сектору (гупн)",
                     "начальник караулу (дснс)",
                     "начальник рятувального відділення (дснс)",
                     "начальник пожежно рятувальної частини",
                     "начальник чергової зміни (дснс)",
                     "начальник відділу/управління (місцеве самоврядування)",
                     "начальник виправної колоноії",
                     "начальник відділу/управління (пфу)",
                     "начальник відділу/управління (центри зайнятості)",
                     "начальник відділу/управління (військові частини, зсу)",
                     "начальник відділу/управління (нбу, пфу, аму)",
                     "начальник відділу/управління (міністерства)",
                     "начальник відділу/управління (КП)",
                     "начальник відділу/управління (укрзалізниця)",
                     "начальник станції (укрзалізниця)",
                     "начальник поїзда"


)

# Класифікатор
detect_profesion <- function(NAME, SPHERE) {
  
  #адміністратор
  admіnіstrator = str_detect(NAME, "адміністратор\\b") & 
    !str_detect(NAME, "провідний консультант")
  
  #бригадир колії
  brigadir_kolii = str_detect(NAME, "бр(и|е|і|ии)г(а|о)д(и|і)р\\b колії")
  
  #бригадир
  brigadir = !brigadir_kolii & str_detect(NAME, "бр(и|е|і|ии)г(а|о)д(и|і)р\\b")
  
  #бухгалтер
  buhgalter = str_detect(NAME, "бу(хг|г|х)алтер\\b") & 
    !str_detect(NAME, "головний") & 
    !str_detect(NAME, "економіст\\b")
  
  #головний бухгалтер
  golovnij_buhgalter = str_detect(NAME, "^головний бу(хг|г)алтер\\b") & 
    !str_detect(NAME, "економіст\\b")
  
  #військовий
  serviceman = str_detect(NAME, "в(ій|і)(сь|с)ковослужбовець") | 
    str_detect(NAME, "^військовий$")
  
  #водій
  vodіj = str_detect(NAME, "вод(і|и)(й|я|ї)")
  
  #вчитель
  vchitel = str_detect(NAME, "(у|в)ч(и|і)те(ль|лька)")
  
  #головний державний виконавець
  golovnij_derzhavnij_vikonavec = str_detect(NAME, "головний дер(ж|д)авний виконавець") | str_detect(NAME, "\\bгдв\\b")
  
  #головний інспектор
  golovnij_іnspektor = str_detect(NAME, "головний інспектор")
  
  #економіст
  ekonomіst = str_detect(NAME, "економіст\\b") & 
    !str_detect(NAME, "бухгалтер\\b")
  
  #інженер головний 
  golovnij_іnzhener = str_detect(NAME, "головний (і|и)нж(е|и)нер\\b")  
  
  #інженер провідний 
  provіdnij_іnzhener = str_detect(NAME, "провідний (і|и)нж(е|и)нер\\b") 
  
  #інженер
  іnzhener = str_detect(NAME, "(і|и)нж(е|и)нер\\b") & 
    !golovnij_іnzhener & !provіdnij_іnzhener
  
  #головний лікар
  golovnij_likar = str_detect(NAME, "головний лікар\\b") & 
    !str_detect(SPHERE, "\\bвет") & 
    !str_detect(NAME, "\\bвет")
  
  #лікар
  likar = !golovnij_likar & str_detect(NAME, "лікар\\b") & 
    !str_detect(SPHERE, "\\bвет") & 
    !str_detect(NAME, "\\bвет")
  
  #ветлікар
  vetlikar = str_detect(NAME, "лікар\\b")  & 
    (str_detect(SPHERE, "\\bвет") | 
       str_detect(NAME, "\\bвет"))
  
  #фельдшер
  feldsher = str_detect(NAME, "фе(льд|ль)шер\\b") & 
    !str_detect(NAME, "завіду")
  
  #головний механік
  golovnij_mehanіk = str_detect(NAME, "головний механік\\b")
  
  #механік
  mehanіk = str_detect(NAME, "\\bмеханік\\b") & 
    !str_detect(NAME, "головний") & 
    !str_detect(NAME, "^капітан") 
  
  #електромеханік
  elektromehanіk = str_detect(NAME, "електромеханік")
  
  #головний спеціаліст
  golovnij_specіalіst = str_detect(NAME, "головний сп(е|а|і)ц(еа|иа|іа|а)л(і|и)ст\\b") & 
    !str_detect(NAME, "землевпорядник") & 
    !str_detect(NAME, "економіст\\b")
  
  #депутат
  deputat =  str_detect(NAME, "депутат") & 
    (str_detect(SPHERE, "сіль(сь|с)к") | 
       str_detect(SPHERE, "сел(и|е)щн") | 
       str_detect(SPHERE, "міськ") |
       str_detect(SPHERE, "район") | 
       str_detect(SPHERE, "обласн") |
       str_detect(SPHERE, "\\bотг\\b") | 
       str_detect(NAME, "сільськ") | 
       str_detect(NAME, "сел(и|е)щн") | 
       str_detect(NAME, "міськ") | 
       str_detect(NAME, "район") | 
       str_detect(NAME, "обласн") | 
       str_detect(NAME, "\\bотг\\b")
     )
  
  #державний виконавець
  derzhavnij_vikonavec = str_detect(NAME, "державний виконавець") & 
    !str_detect(NAME, "головний")
  
  #державний інспектор
  derzhavnij_іnspektor =  str_detect(NAME, "державн.*інспектор") | str_detect(NAME, "\\bгді\\b") | str_detect(NAME, "\\bгдрі\\b")
  
  #державний кадастровий інспектор
  derzhavnij_kadastrovij = str_detect(NAME, "кадастро") 
  
  #державний реєстратор
  derzhavnij_reеstrator =  str_detect(NAME, "державний реєстратор") & !str_detect(NAME, "кадастро")
  
  #директор
  direktor = str_detect(NAME, "директор\\b")
  
  #диспетчир
  dispetcher = str_detect(NAME, "д(и|і)спе(тч|ч)(е|и)р\\b")
  
  #дільничний офіцер поліції
  dіlnichnij_ofіcer_polіcії = str_detect(NAME, "дільничний офіцер поліції")
  
  #завідувач сектору
  zavіduvach_sectory = str_detect(NAME, "завідувач сектору") | str_detect(NAME, "завідуюч(ий|а) сектору")
  
  #завідувач аптекою
  zavіduvach_aptekoju = str_detect(NAME, "завіду.*апте")  & !str_detect(NAME, "сектору")
  
  #завідувач лабораторією
  zavіduvach_laboratorіju = str_detect(NAME, "завіду.*лабор")
  
  #завідувач ветлікарнею
  zavіdujuchij_vetlіkarneju = str_detect(NAME, "завіду.*вет.*лік.*") | 
    (str_detect(NAME, "завід.*дільн") & 
       str_detect(NAME, "\\bвет"))
  
  #завідувач
  zavіduvach = (str_detect(NAME, "завідувач\\b") | 
    str_detect(NAME, "завідуюч")) & 
    !zavіdujuchij_vetlіkarneju & 
    !zavіduvach_sectory & 
    !zavіduvach_aptekoju & 
    !zavіduvach_laboratorіju & 
    !str_detect(NAME, "заступник директора")
  
  #заступник головного бухгалтера
  zastupnik_golovnogo_buhgaltera = str_detect(NAME, "заступник головного бухгалтера")
  
  #заступник директора
  zastupnik_direktora = str_detect(NAME, "заступник директора") 
  
  #землевпорядник
  zemlevporjadnik =  str_detect(NAME, "земле(в|у)порядник")
  
  #інспектор
  іnspektor = str_detect(NAME, "(і|i)нспектор") &  
    !str_detect(NAME, "головний") & 
    !str_detect(NAME, "провідний") & 
    !str_detect(NAME, "молодший") & 
    !str_detect(NAME, "поліц") & 
    !str_detect(NAME, "черговий")
  
  #командир
  komandir = str_detect(NAME, "командир\\b")
  
  #комірник
  komіrnik = str_detect(NAME, "комірник\\b")
  
  #лісничий
  lіsnichij = str_detect(NAME, "лісничий\\b")
  
  #майстер лісу
  majster_lіsu = str_detect(NAME, "м(ай|а)с(те|т)р лісу")  & 
    !str_detect(NAME, "старший")
  
  #старший майстер лісу
  starshuy_majster_lisu = str_detect(NAME, "старший м(ай|а)стер лісу")
  
  #майстер шляховий
  majster_shljahovij = str_detect(NAME, "м(ай|а)стер") & 
    str_detect(NAME, "(щ|ш)ляховий") & 
    !str_detect(NAME, "лісу")
  
  #майстер
  majster = !majster_lіsu & 
    !starshuy_majster_lisu & 
    !majster_shljahovij & 
    !str_detect(NAME, "старший") & 
    str_detect(NAME, "м(ай|а)стер\\b")  
  
  #старший майстер
  starshij_majster = !majster_lіsu & 
    !majster_shljahovij & 
    str_detect(NAME, "старший м(ай|а)стер\\b")
  
  #молодший іеспектор
  molodshij_іnspektor = str_detect(NAME, "молодший інспектор") 
  
  #оперуповноважений
  operupovnovazhenij = str_detect(NAME, "оперу") & 
    !str_detect(NAME, "старший")
  
  #старший оперуповноважений
  starshij_operupovnovazhenij = str_detect(NAME, "оперу") & 
    str_detect(NAME, "старший")
  
  #офіцер
  ofіcer = str_detect(NAME, "оф(і|и)цер") & 
    !str_detect(NAME, "поліці(і|ї)") & 
    !str_detect(NAME, "старший")
  
  #старший офіцер
  starshuy_oficer = str_detect(NAME, "старший оф(і|и)цер") & 
    !str_detect(NAME, "поліці(і|ї)") & 
    !str_detect(NAME, "дільничн")
  
  #рятівник
  rjatuvalnik = (str_detect(NAME, "рятувальник") | 
                   str_detect(NAME, "рятівник")) & 
    !str_detect(NAME, "старший") & 
    !str_detect(NAME, "пожежн")
  
  #старший рятівник
  starshiy_rjatuvalnik = (str_detect(NAME, "рятувальник") | 
                            str_detect(NAME, "рятівник")) & 
    str_detect(NAME, "старший") & 
    !str_detect(NAME, "пожежний\\b")
  
  
  #пожежний рятівник
  pozhezhnyj_ryatuvalnyk <- (str_detect(NAME, "рятувальник") | 
                               str_detect(NAME, "рятівник")) & 
    str_detect(NAME, "пожежний\\b") & 
    !str_detect(NAME, "старший")
  
  #пожежний рятівник
  starshiy_pozhezhnyj_ryatuvalnyk <- (str_detect(NAME, "рятувальник") | str_detect(NAME, "рятівник")) & str_detect(NAME, "пожежний\\b") & str_detect(NAME, "старший")
  
  #поліцейський
  polіceiskiy = str_detect(NAME, "інспектор патрульної поліці(і|ї)") | 
    str_detect(NAME, "поліцейський") | 
    str_detect(NAME, "інспектор сектору реагування патрульної поліції") |
    str_detect(NAME, "інспектор српп") | 
    str_detect(NAME, "інспектор поліці(і|ї)")
  
  
  #помічник лісничого
  pomіchnik_lіsnichogo =  str_detect(NAME, "помічник лісничого")
  
  #помічник чергового
  pomіchnik_chergovogo = str_detect(NAME, "помічник чергового")
  
  #провідний інспектор
  providnuy_inspektor = str_detect(NAME, "провідний інспектор")
  
  #провідний консультант адміністратор
  provіdnij_konsultant_admin = str_detect(NAME, "провідний консультант адміністратор")
  
  #головний фахівець
  golovnij_fahіvec  = str_detect(NAME, "головний фахівець")
  
  #прокурор
  prokuror = str_detect(NAME, "прокурор")
  
  #секретар ради
  sekretar_rady = str_detect(NAME, "секретар") & 
    str_detect(NAME, "ради") & 
    !str_detect(NAME, "\\bсуд(у|ов)")
  
  #секретар суду
  sekretar_sudu = str_detect(NAME, "секретар") & 
    str_detect(NAME, "\\bсуд(у|ов)")
  
  #секретар
  sekretar = !sekretar_rady & 
    !sekretar_sudu & 
    str_detect(NAME, "секретар")
  
  #міський голова
  golova_city = str_detect(NAME, "голова\\b") &
    !str_detect(NAME, "суд") & 
    !str_detect(NAME, "ферм") &
    !str_detect(NAME, "правління") &
    !str_detect(NAME, "коміс") &
    !str_detect(NAME, "комітет") &
    !str_detect(NAME, "сільськ") &
    !str_detect(NAME, "профспілки") &
    !str_detect(SPHERE, "профспілки") &
    !str_detect(NAME, "громадськ") &
    !str_detect(SPHERE, "громадськ") &
    !str_detect(NAME, "комунальн") &
    !str_detect(SPHERE, "комунальн") &
    !str_detect(SPHERE, "заступник") &
    !str_detect(SPHERE, "організація") &
    !str_detect(NAME, "адміністрац") &
    (str_detect(NAME, "міськ") | 
      str_detect(SPHERE, "міськ"))
  
  #заступник міського голови
  zastupnik_golova_city = str_detect(NAME, "заступник голови") & 
    !str_detect(NAME, "суд") & 
    !str_detect(NAME, "ферм") &
    !str_detect(NAME, "правління") &
    !str_detect(NAME, "коміс") &
    !str_detect(NAME, "комітет") &
    (
      str_detect(NAME, "міськ") | 
        str_detect(SPHERE, "міськ")
    )
  
  #сільський/селищний голова
  golova_village = str_detect(NAME, "голова\\b") & 
    !str_detect(NAME, "суд") & 
    !str_detect(NAME, "ферм") &
    !str_detect(NAME, "правління") &
    !str_detect(NAME, "коміс") &
    !str_detect(NAME, "комітет") &
    (
      str_detect(NAME, "с(і|и)ль(сь|с)к") | 
      str_detect(SPHERE, "с(і|и)ль(сь|с)к") | 
      str_detect(SPHERE, "сел(е|и)щ") | 
      str_detect(NAME, "сел(е|и)щ") 
    )
  
  #заступник сільського голови
  zastupnik_golova_village = str_detect(NAME, "заступник голови") & 
    !str_detect(NAME, "суд") & 
    !str_detect(NAME, "ферм") &
    !str_detect(NAME, "правління") &
    !str_detect(NAME, "коміс") &
    !str_detect(NAME, "комітет") &
    ( str_detect(NAME, "сільськ") | 
        str_detect(SPHERE, "сільськ") | 
        str_detect(SPHERE, "сел(е|и)щ") | 
        str_detect(NAME, "сел(е|и)щ") 
    )
  
  #голова суду
  golova_court = str_detect(NAME, "голова\\b") & 
    !str_detect(NAME, "присяжн") &
    (str_detect(NAME, "суду") | 
        str_detect(SPHERE, "суд")
    )
  
  #заступник голови суду
  zastupnik_golova_court = str_detect(NAME, "заступник голови") & 
    !str_detect(NAME, "присяжн") &
    ( str_detect(NAME, "суду") | 
        str_detect(SPHERE, "суд")
    )
  
  #голова РДА
  golova_rga = str_detect(NAME, "голова\\b") &
    !str_detect(NAME, "суд") & 
    !str_detect(NAME, "ферм") &
    !str_detect(NAME, "правління") &
    !str_detect(NAME, "коміс") &
    !str_detect(NAME, "комітет") &
    (str_detect(NAME, "рай.*держ.*адм") | 
       str_detect(SPHERE, "рай.*держ.*адм") | 
       str_detect(SPHERE, "\\bрда\\b") | 
       str_detect(NAME, "\\bрда\\b")
     )
  
  #заступник голови РДА
  zastupnik_golova_rga = str_detect(NAME, "заступник голови") &
    !str_detect(NAME, "суд") & 
    !str_detect(NAME, "ферм") &
    !str_detect(NAME, "правління") &
    !str_detect(NAME, "коміс") &
    !str_detect(NAME, "комітет") &
    (str_detect(NAME, "рай.*держ.*адм") | 
       str_detect(SPHERE, "рай.*держ.*адм") | 
       str_detect(SPHERE, "\\bрда\\b") | 
       str_detect(NAME, "\\bрда\\b")
     )
  
  #голова правління
  golova_pravlinnia = str_detect(NAME, "голова правління") &
    !str_detect(NAME, "суд") & 
    !str_detect(NAME, "ферм") &
    !str_detect(NAME, "коміс") &
    !str_detect(NAME, "комітет") &
    !str_detect(SPHERE, "суд") & 
    !str_detect(SPHERE, "ферм") &
    !str_detect(SPHERE, "коміс") &
    !str_detect(SPHERE, "комітет") 
  
  #голова фермерського госп-ва
  golova_farm = str_detect(NAME, "голова\\b") &  
    (str_detect(NAME, "фермер") | 
      str_detect(SPHERE, "ферм") |
       str_detect(SPHERE, "\\bфг\\b")|
       str_detect(SPHERE, "\\bcфг\\b")
      )
  
  #голова комісії
  golova_komisii = str_detect(NAME, "голова\\b") & 
    (str_detect(NAME, "комісі") | 
      str_detect(SPHERE, "комісі")
      )
  
  #голова райради
  golova_rairady = str_detect(NAME, "голова\\b") &
    !str_detect(NAME, "суд") & 
    !str_detect(NAME, "ферм") &
    !str_detect(NAME, "правління") &
    !str_detect(NAME, "коміс") &
    !str_detect(NAME, "комітет") &
    (str_detect(NAME, "рай.*рад") | str_detect(SPHERE, "рай.*рад") )
  
  #заступник голови райради
  zastupnik_golova_rairady = str_detect(NAME, "заступник голови") &
    !str_detect(NAME, "суд") & 
    !str_detect(NAME, "ферм") &
    !str_detect(NAME, "правління") &
    !str_detect(NAME, "коміс") &
    !str_detect(NAME, "комітет") &
    (str_detect(NAME, "рай.*рад") | str_detect(SPHERE, "рай.*рад") )
  
  #голова комітетів верховної ради
  golova_vr = str_detect(NAME, "голова\\b") & str_detect(NAME, "комітет") &
    !str_detect(NAME, "суд") & 
    !str_detect(NAME, "ферм") &
    !str_detect(NAME, "правління") &
    !str_detect(NAME, "коміс") &
    (str_detect(NAME, "верхов") | str_detect(SPHERE, "верхов") 
     )
  
  #голова інше
  golova_other = !golova_city & 
    !golova_village & 
    !golova_court & 
    !golova_rga & 
    !golova_pravlinnia & 
    !golova_farm & 
    !golova_komisii &
    !golova_rairady &
    !golova_vr &
    str_detect(NAME, "голова\\b")
  
  #слідчий
  slіdchij = str_detect(NAME, "слідчий")
  
  #спеціалист
  specіalіst = str_detect(NAME, "сп(е|а|і)ц(еа|иа|іа|а)л(і|и)ст") & 
    !str_detect(NAME, "головний") & 
    !str_detect(NAME, "старший") & 
    !str_detect(NAME, "молодший")
  
  #офіцер поліції
  ofіcer_polіcі = !str_detect(NAME, "старший") & 
    (str_detect(NAME, "офіцер.*поліці(і|ї)") | 
       str_detect(NAME, "дільнич.*оф(і|и)цер") | 
       str_detect(NAME, "\\bдоп\\b")
     )  
  
  #старший офіцер поліції
  starshij_ofіcer_polіcі = (str_detect(NAME, "старший") |
    str_detect(NAME, "\\\bст ")) & 
    (str_detect(NAME, "офіцер.*поліці(і|ї)") | 
       str_detect(NAME, "дільнич.*офіцер") | 
       str_detect(NAME, "\\bcдоп\\b")
     )  
  
  #суддя
  suddja = str_detect(NAME, "суддя")
  
  #судовий розпорядник
  sudovij_rozporjadnik =  str_detect(NAME, "судовий розпорядник")
  
  #черговий по станції
  chergovij_po_stancii =  str_detect(NAME, "чергов(а|ий) по\\b") & str_detect(NAME, "станц") 
  
  #черговий
  chergovij = !chergovij_po_stancii & !str_detect(NAME, "інспектор") & 
    !str_detect(NAME, "станц") &
    !str_detect(NAME, "начальник\\b") &
    !str_detect(NAME, "заступник начальника") &
    !str_detect(NAME, "(и|і)(нсп|сп|нс|)е(к|р)тор") &
    str_detect(NAME, "чергов(а|ий)") 
  
  #юрисконсульт
  juriskonsult = str_detect(NAME, "юри(с|ст)консульт\\b") 
  
  #радник посольства
  radnik_posolstva = str_detect(NAME, "радник посольства")
  
  #ректор
  rektor = str_detect(NAME, "\\bректор")
  
  #тракторист
  traktorist = str_detect(NAME, "тракторист\\b")
  
  #агент комерційний
  agent_komercіjnij = str_detect(NAME, "агент комерційний")
  
  #агроном
  agronom = str_detect(NAME, "агроном\\b")
  
  #архіваріус
  arhіvarіus = str_detect(NAME, "архіваріус\\b")
  
  #бібліотекар
  bіblіotekar = str_detect(NAME, "бібліотекар\\b")
  
  #вихователь
  vihovatel = str_detect(NAME, "вихователь\\b") & 
    !str_detect(SPHERE, "\\bколон")
  
  #відповідальний вихонавець
  vіdpovіdalnij_vikonavec = str_detect(NAME, "відповідальний виконавец")
  
  #медсестра
  medsestra = str_detect(NAME, "сестра") &  str_detect(NAME, "мед")
  
  #детектив
  detectiv = str_detect(NAME, "детектив") & str_detect(SPHERE, "бюро")
  
  #викладач
  vuckladach <- str_detect(NAME, "викладач") &  !str_detect(NAME, "старший")
  
  #викладач старший
  starshuy_vuckladach <- str_detect(NAME, "старший викладач")
  
  #казначей
  kaznachey <- str_detect(NAME, "казначей")
  
  #діловод
  dilovod <- str_detect(NAME, "діловод\\b")
  
  #документознавець
  documentoznavec <- str_detect(NAME, "документознавець")
  
  #доцент
  docent <- str_detect(NAME, "доцент")
  
  #касир
  kasur <- str_detect(NAME, "касир")
  
  #менеджер
  menedzher <- str_detect(NAME, "менеджер")
  
  #військовий комісар
  vіjskovij_komіsar <- str_detect(NAME, "військ.*коміс")
  
  #заступник головного інженера
  zastupnik_golovnogo_іnzhenera <- str_detect(NAME, "заступник головного інженера")
  
  #інспектор роти
  іnspektor_roty <- str_detect(NAME, "інспектор роти")
  
  #присяжний
  prisjazhnij <- str_detect(NAME, "присяжн(ий|а)")
  
  #сапер
  saper <- str_detect(NAME, "сапер")
  
  #радник МЗС
  radnik_mzs <- str_detect(NAME, "радник") & 
    (str_detect(SPHERE, "\\bмзс\\b") |
         str_detect(SPHERE, "закордон") | 
         str_detect(SPHERE, "україн") | 
         str_detect(SPHERE, "міністерство")
       )
  
  #радник голови
  radnik_golovy <- str_detect(NAME, "радник.*голови")
  
  #заступник голови інше
  zastupnik_golovi <- str_detect(NAME, "заступник голови") & 
    !zastupnik_golova_city &
    !zastupnik_golova_village &
    !zastupnik_golova_rga &
    !zastupnik_golova_rairady &
    !zastupnik_golova_court 
    
  #фахівець
  fahіvec <- str_detect(NAME, "фахівець") & !str_detect(NAME, "головний")
  
  #юрист
  jurist <- str_detect(NAME, "^юрист\\b") 
  
  #заступник командира батальону
  zastupnyk_komandyra_batalionu <- str_detect(NAME, "^заступник командира батальйон") | 
    (str_detect(NAME, "заступник командира батальйон") & !str_detect(NAME, "начальник"))
  
  #заступник командира військовой частини
  zastupnyk_komandyra_viiskovoi_chastyny <- str_detect(NAME, "заступник командира.*частини") & !str_detect(NAME, "^начальник штабу")
  
  #заступник командира дивізіону
  zastupnyk_komandyra_dyvizionu <- str_detect(NAME, "заступник командира.*дивізіон") & !str_detect(NAME, "^начальник штабу")
  
  #заступник командира роти
  zastupnyk_komandyra_roty <- str_detect(NAME, "заступник командира роти")
  
  #заступник командира взводу
  zastupnyk_komandyra_vzvodu <- str_detect(NAME, "заступник командира взвод")
  
  #заступник командира інше
  zastupnyk_komandyra_inshe <- str_detect(NAME, "заступник командира") & 
    !zastupnyk_komandyra_batalionu &
    !zastupnyk_komandyra_viiskovoi_chastyny &
    !zastupnyk_komandyra_dyvizionu &
    !zastupnyk_komandyra_roty &
    !zastupnyk_komandyra_vzvodu
  
  #заступники начальників
  zastupnyk_nachalnyka_upravlinnia <- str_detect(NAME, "заступник начальника управління") & !str_detect(NAME, "^начальник")
  zastupnyk_nachalnyka_viddilu <- str_detect(NAME, "заступник начальника відділу") & !str_detect(NAME, "^начальник")
  zastupnyk_nachalnyka_sluzhby <- str_detect(NAME, "заступник начальника служби") & !str_detect(NAME, "^начальник")
  zastupnyk_nachalnyka_stantsii <- str_detect(NAME, "заступник начальника станції") 
  zastupnyk_nachalnyka_ustanovy <- str_detect(NAME, "заступник начальника установи") & !str_detect(NAME, "^начальник")
  zastupnyk_nachalnyka_filii <- str_detect(NAME, "заступник начальника філії") & !str_detect(NAME, "^начальник")
  zastupnyk_nachalnyka_tsekhu <- str_detect(NAME, "заступник начальника цех") & !str_detect(NAME, "^начальник")
  zastupnyk_nachalnyka_shtabu  <- str_detect(NAME, "заступник начальника штаб") & !str_detect(NAME, "^начальник")
  zastupnyk_nachalnyka_inshe  <- str_detect(NAME, "заступник начальника") & 
    !str_detect(NAME, "^начальник") &
    !zastupnyk_nachalnyka_upravlinnia &
    !zastupnyk_nachalnyka_viddilu &
    !zastupnyk_nachalnyka_sluzhby &
    !zastupnyk_nachalnyka_stantsii &
    !zastupnyk_nachalnyka_ustanovy &
    !zastupnyk_nachalnyka_filii &
    !zastupnyk_nachalnyka_tsekhu &
    !zastupnyk_nachalnyka_shtabu
  
  #психолог
  psykholoh <- str_detect(NAME, "психолог")
  
  #проректор
  prorektor <- str_detect(NAME, "проректор")
  
  #професор
  profesor <- str_detect(NAME, "професор")
  
  #слюсар
  sliusar <- str_detect(NAME, "слюсар")
  
  #аудитор
  audytor <- str_detect(NAME, "аудитор")
  
  #єгер
  yeher <- str_detect(NAME, "\\bєге(рь|р)\\b")
  
  #головний експерт
  ekspert_holovnyi <- str_detect(NAME, "головний експерт")
  
  #державний експерт
  ekspert_derzhavnyi <- str_detect(NAME, "державний експерт")
  
  #експерт
  ekspert <- str_detect(NAME, "експерт") & !ekspert_holovnyi & !ekspert_derzhavnyi
  
  #староста
  starosta <- str_detect(NAME, "староста")
  
  #енергетик
  enerhetyk <- str_detect(NAME, "енергет(и|і)к")
  
  #керуючий справами
  keruiuchyi_spravamy <- str_detect(NAME, "керуючий справами")
  
  #радіотелеграфіст
  radiotelehrafist <- str_detect(NAME, "радіотеле")
  
  #тальман
  talman <- str_detect(NAME, "тальман")
  
  #старшина
  starshyna <-  str_detect(NAME, "старшина")
  
  #науковий співробітник
  naukovyi_spivrobitnyk <-  str_detect(NAME, "науковий співробітник")
  
  #штурман
  shturman <- str_detect(NAME, "штурман")
  
  #хімік
  khimik <- str_detect(NAME, "хімік\\b")
  
  #технік
  tehnik <- str_detect(NAME, " технік\\b")
  
  #ординатор
  ordynator <- str_detect(NAME, "ординатор\\b")
  
  #соціальний працівник
  sotsialnyi_pratsivnyk <- str_detect(NAME, "соціальний працівник") | 
    str_detect(NAME, "соціальний робітник")
  
  #сержант
  serzhant <- str_detect(NAME, "сержант\\b")
  
  #начальник конвою (поліція)
  nachalnik_konvoju_gupn <- str_detect(NAME, "начальник конвою") & 
    str_detect(SPHERE, "\\bгунп\\b")
  
  #начальник сектору (поліція)
  nachalnik_sektoru_gupn <- str_detect(NAME, "начальник сектор(у|а)") & 
    str_detect(SPHERE, "\\bгунп\\b")
  
  #начальник караулу (дснс)
  nachalnik_karaulu_dsns <- str_detect(NAME, "начальник караул(у|а)") & 
    str_detect(SPHERE, "\\bдснс\\b")
  
  #начальник рятівного відділення (дснс)
  nachalnik_rjatuvalnogo_vіddіlennja_dsns <- str_detect(NAME, "начальник рятувального відділення") & 
    str_detect(SPHERE, "\\bдснс\\b")
  
  #начальник пожежно-рятівної частини
  nachalnik_pozhezhno_rjatuvalnoi_chastini <- (str_detect(NAME, "начальник державної пожежно рятувальної частини") | 
      str_detect(NAME, "начальник дпрч")) &
      str_detect(SPHERE, "\\bдснс\\b")
  
  #начальник чергової зміни (дснс)
  nachalnik_chergovoi_zmіni_dsns <- str_detect(NAME, "начальник чергової зміни") & str_detect(SPHERE, "\\bдснс\\b")
  
  #начальник відділу (місцеве самоврядування)
  nachalnik_vіddіlu_mіsceve <- (str_detect(NAME, "^начальник.*відділу") | 
                                  str_detect(NAME, "^начальник.*управління")) & 
    str_detect(SPHERE, "\\bрад(а|и)\\b")
  
  
  #начальник виправної колонії
  nachalnik_vipravnoї_kolonoії <- (str_detect(NAME, "^начальник установи") | str_detect(NAME, "^начальник$")) & str_detect(SPHERE, "\\bвиправна\\b")
  
  #начальник відділу ПФУ
  nachalnik_vіddіlu_pfu <- (str_detect(NAME, "^начальник.*відділу") | str_detect(NAME, "^начальник.*управління")) & 
    str_detect(SPHERE, "\\bпфу\\b") & !str_detect(SPHERE, "головне")
  
  #начальник відділу ДСЗ
  nachalnik_vіddіlu_cz <- (str_detect(NAME, "^начальник.*відділу") | str_detect(NAME, "^начальник.*управління")) & 
    str_detect(SPHERE, "\\bзайнятості\\b")
  
  #начальник відділу ЗСУ
  nachalnik_vіddіlu_zsy <- (str_detect(NAME, "^начальник.*відділу") | str_detect(NAME, "^начальник.*управління")) & 
    (str_detect(SPHERE, "\\bзсу\\b") | str_detect(SPHERE, "\\bзброй") | str_detect(SPHERE, "\\bвійськ"))
  
  #начальник відділу НБУ
  nachalnik_vіddіlu_nbu <- (str_detect(NAME, "^начальник.*відділу") | 
                              str_detect(NAME, "^начальник.*управління")) & 
    (str_detect(SPHERE, "національний банк") | 
       str_detect(SPHERE, "пенсійний фонд україни") | 
       str_detect(SPHERE, "антимонопольний комітет")
     )
  
  #начальник відділу міністерства
  nachalnik_vіddіlu_ministerstva <- (str_detect(NAME, "^начальник.*відділу") | 
                                       str_detect(NAME, "^начальник.*управління")) & 
    str_detect(SPHERE, "міністерство") 
  
  #начальник відділу комунальні підприємства
  nachalnik_vіddіlu_kp <- (str_detect(NAME, "^начальник.*відділу") | 
                             str_detect(NAME, "^начальник.*управління")) & 
    str_detect(SPHERE, "\\bкп\\b") 
  
  #начальник відділу укрзалізниця
  nachalnik_vіddіlu_zaliznytsa <- (str_detect(NAME, "^начальник.*відділу") | 
                                     str_detect(NAME, "^начальник.*управління")) & 
    str_detect(SPHERE, "зал(і|и)зниця") 
  
  #начальник станції укрзалізниця
  nachalnik_stancii_zaliznytsa <- str_detect(NAME, "^начальник станції")  & str_detect(SPHERE, "зал(і|и)зниця") 
  
  #начальник поїзда укрзалізниця
  nachalnik_poizda_zaliznytsa <- str_detect(NAME, "^начальник.*по(ї|е|є|и)зда")  & str_detect(SPHERE, "зал(і|и)зниця") 
  
  
  # тут усе повинно бути в тому самому порядку що і у religions_list
  vector = str_c(sep=",", admіnіstrator, 
                 brigadir, 
                 brigadir_kolii, 
                 buhgalter, 
                 golovnij_buhgalter, 
                 serviceman, 
                 vodіj, 
                 vchitel,
                 golovnij_derzhavnij_vikonavec, 
                 derzhavnij_vikonavec,
                 derzhavnij_іnspektor, 
                 golovnij_іnspektor, 
                 providnuy_inspektor, 
                 іnspektor, 
                 molodshij_іnspektor, 
                 ekonomіst, 
                 golovnij_іnzhener,
                 іnzhener, 
                 golovnij_likar,
                 likar,
                 vetlikar,
                 feldsher, 
                 golovnij_mehanіk, 
                 mehanіk, 
                 elektromehanіk, 
                 golovnij_specіalіst, 
                 golovnij_fahіvec, 
                 deputat,
                 derzhavnij_kadastrovij,
                 derzhavnij_reеstrator,
                 direktor, 
                 dispetcher, 
                 zavіduvach, 
                 zavіduvach_sectory, 
                 zavіduvach_aptekoju, 
                 zavіduvach_laboratorіju, 
                 zastupnik_golovnogo_buhgaltera, 
                 zastupnik_direktora,
                 zemlevporjadnik,
                 komandir,
                 komіrnik,
                 lіsnichij,
                 majster_lіsu,
                 starshuy_majster_lisu,
                 majster_shljahovij,
                 majster, 
                 starshij_majster, 
                 operupovnovazhenij,
                 starshij_operupovnovazhenij,
                 ofіcer,
                 starshuy_oficer,
                 ofіcer_polіcі,
                 starshij_ofіcer_polіcі, 
                 rjatuvalnik, 
                 starshiy_rjatuvalnik,
                 polіceiskiy,
                 pomіchnik_lіsnichogo,
                 pomіchnik_chergovogo,
                 provіdnij_konsultant_admin,
                 prokuror,
                 sekretar_rady,
                 sekretar_sudu,
                 sekretar, 
                 golova_city,
                 golova_village,
                 golova_court,
                 golova_rga,
                 golova_pravlinnia,
                 golova_farm,
                 golova_komisii,
                 golova_rairady,
                 golova_vr,
                 golova_other,
                 slіdchij,
                 specіalіst,
                 suddja,
                 sudovij_rozporjadnik,
                 chergovij_po_stancii,
                 chergovij,
                 juriskonsult,
                 radnik_posolstva, 
                 rektor,
                 traktorist,
                 agent_komercіjnij,
                 agronom,
                 arhіvarіus,
                 bіblіotekar,
                 vihovatel,
                 vіdpovіdalnij_vikonavec,
                 zavіdujuchij_vetlіkarneju,
                 medsestra,
                 detectiv,
                 vuckladach,
                 starshuy_vuckladach,
                 kaznachey,
                 dilovod,
                 documentoznavec,
                 docent,
                 kasur,
                 menedzher,
                 vіjskovij_komіsar,
                 zastupnik_golovnogo_іnzhenera,
                 іnspektor_roty,
                 prisjazhnij,
                 saper, 
                 radnik_mzs, 
                 radnik_golovy,
                 zastupnik_golovi,
                 fahіvec,
                 jurist,
                 zastupnik_golova_city,
                 zastupnik_golova_village,
                 zastupnik_golova_rga,
                 zastupnik_golova_rairady,
                 zastupnik_golova_court,
                 zastupnyk_komandyra_batalionu,
                 zastupnyk_komandyra_viiskovoi_chastyny,
                 zastupnyk_komandyra_dyvizionu,
                 zastupnyk_komandyra_roty,
                 zastupnyk_komandyra_vzvodu,
                 zastupnyk_komandyra_inshe,
                 zastupnyk_nachalnyka_upravlinnia,
                 zastupnyk_nachalnyka_viddilu,
                 zastupnyk_nachalnyka_sluzhby,
                 zastupnyk_nachalnyka_stantsii,
                 zastupnyk_nachalnyka_ustanovy,
                 zastupnyk_nachalnyka_filii,
                 zastupnyk_nachalnyka_tsekhu,
                 zastupnyk_nachalnyka_shtabu,
                 zastupnyk_nachalnyka_inshe,
                 psykholoh,
                 prorektor,
                 profesor,
                 sliusar,
                 audytor,
                 yeher,
                 ekspert_holovnyi,
                 ekspert_derzhavnyi,
                 ekspert,
                 starosta,
                 enerhetyk,
                 keruiuchyi_spravamy,
                 radiotelehrafist,
                 talman,
                 starshyna,
                 naukovyi_spivrobitnyk,
                 shturman,
                 khimik,
                 tehnik,
                 ordynator,
                 sotsialnyi_pratsivnyk,
                 serzhant,
                 pozhezhnyj_ryatuvalnyk,
                 nachalnik_konvoju_gupn,
                 nachalnik_sektoru_gupn,
                 nachalnik_karaulu_dsns,
                 nachalnik_rjatuvalnogo_vіddіlennja_dsns,
                 nachalnik_pozhezhno_rjatuvalnoi_chastini,
                 nachalnik_chergovoi_zmіni_dsns,
                 nachalnik_vіddіlu_mіsceve,
                 nachalnik_vipravnoї_kolonoії,
                 nachalnik_vіddіlu_pfu,
                 nachalnik_vіddіlu_cz,
                 nachalnik_vіddіlu_zsy,
                 nachalnik_vіddіlu_nbu,
                 nachalnik_vіddіlu_ministerstva,
                 nachalnik_vіddіlu_kp,
                 nachalnik_vіddіlu_zaliznytsa,
                 nachalnik_stancii_zaliznytsa,
                 nachalnik_poizda_zaliznytsa
                 
)
  
  profesion = vector %>% purrr::map(function(str){
    indices = (str %>% str_split(",") %>% unlist == "TRUE") %>% which
    professions_list[indices[1]]
  }) %>% unlist
}
# кінець класифікатора професій
salaries <- salaries %>%  mutate(class = detect_profesion(V5, V4))

# додаємо сфери
salaries <- left_join(salaries, spheres, c("class" = "V1"))

# зберігаємо дані для d3
visdata = salaries %>% 
  group_by(class) %>% 
  mutate(mean = mean(total)) %>% 
  mutate(freq = n()) %>% 
  select(4,7,8,9) %>% 
  filter(!is.na(class)) %>% 
  rename(type = class) %>% 
  rename(sphere = V2) %>% 
  rename(value = total)  

visdata$mean <- round(visdata$mean, 0)

setwd("/home/yevheniia/git/salaries/data/")
write.csv(visdata, "salaries_туц.csv", row.names = F )


# uniqueShperes <- data.frame(vis$sphere) %>% unique()
# dput(as.character(uniqueShperes))


######################################### Блок по помилкам ####################################################################
library("dplyr")
library("readr")
library("stringr")
library("tidyr")
library('stringi')
library("tm")
library('hunspell')

#  функція, що ділить рядок на слова
split_every <- function(x, n, pattern, collapse = pattern, ...) {
  x_split <- strsplit(x, pattern, perl = TRUE, ...)[[1]]
  out <- character(ceiling(length(x_split) / n))
  for (i in seq_along(out)) {
    entry <- x_split[seq((i - 1) * n + 1, i * n, by = 1)]
    out[i] <- paste0(entry[!is.na(entry)], collapse = collapse)
  }
  out
}


# робимо окремий датасет, де не вичищаємо пунктуацію
search_dataset <- filteredData %>%
  filter(V6 == "Заробітна плата отримана за основним місцем роботи" ) %>%
  filter(total > 44676 ) %>%
  mutate(V5 = tolower(V5)) %>%
  mutate(V4 = tolower(V4)) %>%
  mutate(V5 = trimws(V5)) %>%
  filter(str_detect(V4, "[А-Яа-я]") & str_detect(V5, "[А-Яа-я]")) %>%
  filter(!str_detect(V5, "пенсіонер|не прац|не ма(є|ю)")) %>%
  filter(!str_detect(V5, "абзац"))%>%
  select(1,2,3,7,5,6,8)

# щоб упевнитись, що слова не склеєні між собою (народ забуває ставити пробіли перед/після ком і дефісів)
search_dataset$V5 <- gsub(",", ', ', search_dataset$V5)
search_dataset$V5 <- gsub("-", ' - ', search_dataset$V5)
# прибираємо зайву пунктуацію і додаємо трохи пробілів
search_dataset$V5 <- sub(")", "", search_dataset$V5)
search_dataset$V5 <- sub("^\\(", "", search_dataset$V5)
search_dataset$V5 <- sub("\\(", " ", search_dataset$V5)
search_dataset$V5 <- sub("\\/", "", search_dataset$V5)
search_dataset$V5 <- gsub('^\"', '', search_dataset$V5,  perl = T)
search_dataset$V5 <- gsub('\"$', '', search_dataset$V5,  perl = T)
search_dataset$V5 <- gsub(',$', '', search_dataset$V5,  perl = T)
search_dataset$V5 <- gsub('\\.$', '', search_dataset$V5,  perl = T)
search_dataset$V5 <- gsub('\\.', '\\. ', search_dataset$V5,  perl = T)
search_dataset$V5 <- gsub('№', ' № ', search_dataset$V5,  perl = T)
search_dataset$V5 <- gsub('[0-9]', '', search_dataset$V5,  perl = T)
search_dataset$V5 <- gsub('відділу', ' відділу ', search_dataset$V5,  perl = T)
search_dataset$V5 <- gsub('відділення', ' відділення ', search_dataset$V5,  perl = T)
search_dataset$V5 <- gsub('депутат', ' депутат ', search_dataset$V5,  perl = T)
search_dataset$V5 <- gsub('прокурор', ' прокурор ', search_dataset$V5,  perl = T)
search_dataset$V5 <- gsub('спеціаліст', ' спеціаліст ', search_dataset$V5,  perl = T)

search_dataset$V5 <- gsub('  ', ' ', search_dataset$V5,  perl = T)


search_dataset$V5 <- trimws(search_dataset$V5)







# вектор зі слів, по яким робимо пошук TODO його треба розширити
# apostrof_words = c("зв'язку", "об'єктів", "суб'єктів", "ад'юнкт", "об'єднання", "здоров'я", "комп'ютер", "сімʼї", "зобов'язань")
apostrof_words = c("адміністратор", "бригадир",  "зв'язку", "об'єктів", "суб'єктів", "сімʼї", "зобов'язань", "ад'юнкт", "об'єднання", "здоров'я", "комп'ютер",
                   "бухгалтер", "інспектор","економіст", "інженер", "ветеринарний", "лікар", "ветлікар", "фельдшер", "механік", "електромеханік", "спеціаліст",  "фахівець", 
                   "депутат", "реєстратор",  "директор", "диспетчер",  "командир", "майстер", "оперуповноважений", "офіцер", "прокурор", "секретар", "юрисконсульт", "громадян", 
                   "фахівець", "дивізіону", "взводу", "відділення", "відділу", "управління", "юрисконсульт")

# готуємо контейнер для результат ів пошуку
apostrof_vector = c()

# для кожного слова
for(i in 1:length(apostrof_words)){
  apostrof <- unlist(lapply(apostrof_words[i], agrep, x=search_dataset$V5, max.distance=c(cost=1, all=1), value=TRUE)) %>%  unique()
  print(apostrof_words[i])
  onlyFuzzWords = c()
  # для кожного знайденого випадку
  for(j in 1:length(apostrof)){
    # print(apostrof[j])
    string = split_every(apostrof[j], 1, " ")
    # залишаємо лише ті слова, які нам потрібні і зберігаємо їх у вектор
    onlyFuzzWords_current = unlist(lapply(apostrof_words[i], agrep, x=string, max.distance=c(cost=1, all=1), value=TRUE))
    # print(onlyFuzzWords_current)
    onlyFuzzWords = c(onlyFuzzWords, onlyFuzzWords_current)
  }
  # мерджимо в єдиний вектор всі результати
  apostrof_vector <- c(apostrof_vector, onlyFuzzWords)
}


# робимо датафрейм та рахуємо кількість випадків по кожному слову
apostrof_df = data.frame(apostrof_vector)

# залишаємо лише унікальні з результатом підрахунку
apostrof_df <- apostrof_df %>% 
  group_by(apostrof_vector) %>% 
  mutate(freq = n()) %>% 
  ungroup() %>% 
  unique() 

# додаємо до кожного випадку категорію
apostrof_df$category = NA
for(k in 1:length(apostrof_words)){
  print(apostrof_words[k])
  for(i in 1:length(apostrof_df$apostrof_vector)){
    isPresent = lapply(apostrof_words[k], agrepl, x=apostrof_df$apostrof_vector[i], max.distance=c(cost=1, all=1))
    if(isPresent[[1]] == TRUE){
      print("true")
      apostrof_df$category[i] = apostrof_words[k]
    } 
  }
}

# за допомогою hunspell виокремлюємо правильне і неправильне написання
apostrof_df$mistake = NA
apostrof_df$apostrof_vector <- as.character(apostrof_df$apostrof_vector)
for(i in 1:length(apostrof_df$apostrof_vector)){
  if(length(unlist(hunspell(apostrof_df$apostrof_vector[i], dict = 'uk_UA'))) > 0){
    apostrof_df$mistake[i] = TRUE
  } else {
    apostrof_df$mistake[i] = FALSE
  }
}

# # у цього метода є похибка через те, що hunspell приймає за помилки штучно створені слова накшталт Держгеокадастр або телерадіовідеозвʼязок
# check_test <- apostrof_df %>% 
#   group_by(category, mistake) %>% 
#   transmute(freq=sum(freq)) %>% 
#   ungroup() %>% 
#   unique() %>% 
#   arrange(category, mistake)

apostrof_df$apostrof_vector <- gsub("i", "і", apostrof_df$apostrof_vector)
apostrof_df$apostrof_vector <- gsub(",", "", apostrof_df$apostrof_vector) 

apostrof_df$stem = hunspell_stem(apostrof_df$apostrof_vector, dict = 'uk_UA')

## підставляє словоформи, яких немає в цих даних
# for(i in 1:length(apostrof_df$apostrof_vector)){
#   if(apostrof_df$stem[i] != "character(0)" & apostrof_df$mistake[i] == FALSE) {
#     apostrof_df$apostrof_vector[i] = apostrof_df$stem[[i]]
#   } 
# }  

apostrof_df$apostrof_vector <- unlist(apostrof_df$apostrof_vector)

apostrof_df2 <- apostrof_df %>% 
  rename(case = apostrof_vector) %>% 
  group_by(case) %>% 
  mutate(freq2 = sum(freq)) %>% 
  ungroup() %>% 
  select(1,3:6) %>% 
  unique() %>% 
  select(-4)  

apostrof_df2 <- apostrof_df2 %>% 
  arrange(category, freq2)
  


# apostrof_df$stem = NULL
setwd("/home/yevheniia/Desktop/")
write.csv(apostrof_df2, "agrep_mistakes_result.csv", row.names = F)


################################## Абревіатури #################################
abbreviation <- search_dataset %>% 
  filter(str_detect(V5, "^[^аєеиіїоуюяАЄЕІЇОУЮЯ]{2,10}$")) %>% 
  mutate(category = "onlyAbbr")

abbreviation4 <- search_dataset %>% 
  filter(str_detect(V5, "^bсді$") | str_detect(V5, "^сдрі$")) %>% 
  mutate(category = "onlyAbbr")

abbreviation2 <- search_dataset %>% 
  filter(str_detect(V5, "\\bсді\\b") | str_detect(V5, "\\bсдрі\\b")) %>% 
  mutate(category = "partAbbr")

abbreviation3 <- search_dataset %>% 
  filter(str_detect(spellcheckV5, "[А-Я][А-Я]+")) %>% 
  mutate(category = "partAbbr")


abbreviation <- rbind(abbreviation, abbreviation4, abbreviation2, abbreviation3) %>% unique() %>% select(-V8, -V9)
setwd("/home/yevheniia/Desktop/")
write.csv(abbreviation, "abbreviation.csv", row.names = F)

############################## Шукаємо помилки через hunspell ################################################

# він ігнорує слова, написані через зірочку, одинарні лапки тощо
check_spell = function(x) {
  result = ifelse(length(unlist(hunspell_find(x, dict = 'uk_UA'))) > 0, hunspell_find(x, dict = 'uk_UA'), NA)
  return(unlist(result))
}


search_for_mistakes <- filteredData %>%
  filter(V6 == "Заробітна плата отримана за основним місцем роботи") %>%
  filter(total > 44676 ) %>%
  mutate(V5 = trimws(V5)) %>%
  mutate(V5 = removeNumbers(V5)) %>%
  filter(str_detect(V4, "[А-Яа-я]") & str_detect(V5, "[А-Яа-я]")) %>%
  filter(!str_detect(V5, "пенсіонер|не прац|не ма(є|ю)")) %>%
  filter(!str_detect(V5, "абзац"))%>%
  select(1,2,3,7,5,6,8) %>% 
  head(30000)

mistakes_vector = c()

for(i in 1:length(search_for_mistakes$V5)){
  row = check_spell(search_for_mistakes$V5[i])
  if(!is.na(row)){
    mistakes_vector = c(mistakes_vector, check_spell(search_for_mistakes$V5[i]))
  }
}

mistakes_df <- data.frame(table(mistakes_vector))
mistakes_df$category = NA

for(k in 1:length(apostrof_words)){
  print(apostrof_words[k])
  for(i in 1:length(mistakes_df$mistakes_vector)){
    isPresent = lapply(apostrof_words[k], agrepl, x=mistakes_df$mistakes_vector[i], ignore.case = FALSE,  max.distance=c(cost=1, all=1))
    if(isPresent[[1]] == TRUE){
      print("true")
      mistakes_df$category[i] = apostrof_words[k]
    } 
  }
}



######################################### Зайве, непотрібне ####################################################################

## Була ідея додати датасет вичещений від похибки hunspell вручну, але щось пішло не так))) 
# setwd("/home/yevheniia/python/declarations")
# mistakes <-left_join(mistakes, mistakes_cleaned, by=c('spellcheckV5' = 'Var1'))
# mistakes_cleaned = read.csv("probably_mistakes_cleaned.csv")
# mistakes_cleaned <- mistakes_cleaned %>%
# mutate(Var1 = tolower(Var1)) %>%
# group_by(Var1, Mistake) %>%
# transmute(Freq=sum(Freq)) %>%
# unique() %>%
# ungroup()
# mistakes$Freq <- NULL
# mistakes <- mistakes %>%
# mutate(spellcheckV5 = tolower(spellcheckV5))
# mistakes <-left_join(mistakes, mistakes_cleaned, by=c('spellcheckV5' = 'Var1'))


## approximate matching для різних патернів
# search_dataset$spellcheck_new <- sub("[А-ЯҐЄІЇ].*ькому\\b", "0", search_dataset$spellcheckV5)
# search_dataset$spellcheck_new <- sub("[А-ЯҐЄІЇ].*ькому\\b", '', search_dataset$spellcheckV5, perl=T)
# administracii <- unlist(lapply("адміністрації", agrep, x=mistakes_cleaned$Var1, max.distance=c(cost=1, all=1), value=TRUE))
# administracii <- unlist(lapply("адміністрації", agrep, x=mistakes_cleaned$Var1, max.distance=c(cost=1, all=1), value=TRUE))
# police <- unlist(lapply("поліцейський", agrep, x=mistakes_cleaned$Var1, max.distance=c(cost=1, all=1), value=TRUE))
# vetmed <- unlist(lapply("ветеринар", agrep, x=mistakes_cleaned$Var1, max.distance=c(cost=1, all=1), value=TRUE))
# viddil <- unlist(lapply("відділу", agrep, x=mistakes_cleaned$Var1, max.distance=c(cost=1, all=1), value=TRUE))
# golovnuy <- unlist(lapply("головний", agrep, x=mistakes_cleaned$Var1, max.distance=c(cost=1, all=1), value=TRUE))
# nachalnyk <- unlist(lapply("начальник", agrep, x=mistakes_cleaned$Var1, max.distance=c(cost=1, all=1), value=TRUE))
# derzhavnuy <- unlist(lapply("державний", agrep, x=mistakes_cleaned$Var1, max.distance=c(cost=1, all=1), value=TRUE))
# inspector <- unlist(lapply("інспектор", agrep, x=mistakes_cleaned$Var1, max.distance=c(cost=1, all=1), value=TRUE))
# imzhener <- unlist(lapply("інженер", agrep, x=mistakes_cleaned$Var1, max.distance=c(cost=1, all=1), value=TRUE))
# informaciynyh <- unlist(lapply("інформаційн", agrep, x=mistakes_cleaned$Var1, max.distance=c(cost=0.2, all=1), value=TRUE))
# deputat <- unlist(lapply("депутат", agrep, x=mistakes_cleaned$Var1, max.distance=c(cost=1, all=1), value=TRUE))
# sekretar <- unlist(lapply("секретар", agrep, x=mistakes_cleaned$Var1, max.distance=c(cost=1, all=1), value=TRUE))


## професії, що не увійшли до поточного варіанту класифікатору
# заступник начальника начальник сектору  (гупн)
# заступник начальника відділу/управління (гупн)
# заступник начальника державної пожежно рятувальної частини / дпрп
# заступник начальника відділу/управління (місцеве самоврядування)
# заступник начальника виправної колоноії
# заступник начальника відділу/управління (пфу регіональні)
# заступник начальника відділу/управління (центри зайнятості)
# заступник начальника відділу/управління (військові частини, зсу)
# заступник начальника відділу/управління (нбу, пфу(центр))
# заступник начальника відділу/управління (міністерства)
# заступник начальника КП
# заступник начальника відділу/управління (укрзалізниця)



## try clustering
# set.seed(20)
# traindf <- specialists %>% select(2,3)
# traindf$V4 <- tolower(traindf$V4)
# traindf$V4 <- removeNumbers(traindf$V4)
# traindf$V4 <- stripWhitespace(traindf$V4)
# traindf$V4 <- removePunctuation(traindf$V4)
# corpus = Corpus(tm::VectorSource(traindf$V4)) 
# tdm <- DocumentTermMatrix(corpus) 
# tdm.tfidf <- weightTfIdf(tdm)
# tfidf.matrix <- as.matrix(tdm.tfidf) 
# kmeans <- kmeans(tfidf.matrix, 5)
# kmeansRes<-factor(kmeans$cluster)
# 
# traindf$cluster <- kmeans$cluster
# # traindf = traindf %>% select(1,2,7,3)



check_test %>% group_by(mistake) %>% transmute(freq=sum(freq)) %>%  unique()


