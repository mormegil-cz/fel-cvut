CONNECT 'c:\Dokumenty\Petr\SKOLA\DBS\Semestrálka\DBS.GDB' USER 'OPS$KADLECP2' PASSWORD 'password';

Drop table PlanPresBod;
Drop table LetovyPlan;
Drop table RegLetadlo;
Drop table TypLetadla;
Drop table Provozovatel;
Drop table Pilot;
Drop table Letiste;
Drop table NavigacniBod;
Drop table NavObjekt;

Create table TypLetadla (
      CisloSchvaleni Integer Not Null,
      Vyrobce VarChar(100) Not Null,
      Nazev VarChar(100) Not Null,
      Trida Char(1) Not Null,
      Pasazeru Integer Not Null,
      Constraint PK_TypLetadla Primary Key (CisloSchvaleni)
);
Create table Provozovatel (
      ID Integer Not Null,
      Nazev VarChar(100) Not Null,
      Constraint PK_Provozovatel Primary Key (ID)
);
Create table Pilot (
      RodCislo Char(11) Not Null,
      CisloLetPrukazu Integer Not Null,
      Jmeno VarChar(100) Not Null,
      Constraint PK_Pilot Primary Key (CisloLetPrukazu)
);
Create table Letiste (
      NavObjek_ID Char(5) Not Null,
      Constraint PK_Letiste Primary Key (NavObjek_ID)
);
Create table NavigacniBod (
      NavObjek_ID Char(5) Not Null,
      Constraint PK_NavigacniBod Primary Key (NavObjek_ID)
);
Create table PlanPresBod (
      PoradiVPlanu Integer Not Null,
      LetovyPl_SchvalCislo Integer Not Null,
      Navigacn_NavObjek_ID Char(5) Not Null,
      Constraint PK_PlanPresBod Primary Key (PoradiVPlanu, LetovyPl_SchvalCislo, Navigacn_NavObjek_ID)
);
Create table NavObjekt (
      ID Char(5) Not Null,
      Nazev VarChar(100) Not Null,
      ZemSirka Float Not Null,
      ZemDelka Float Not Null,
      VyskaMSL Float Not Null,
      Constraint PK_NavObjekt Primary Key (ID)
);
Create table RegLetadlo (
      Provozov_ID Integer Not Null,
      Typ_CisloSchvaleni Integer Not Null,
      ID Char(10) Not Null,
      Constraint PK_RegLetadlo Primary Key (ID)
);
Create table LetovyPlan (
      KapitanL_CisloLetPrukazu Integer Not Null,
      CasOdletu Date Not Null,
      Predpis Char(1) Not Null,
      SchvalCislo Integer Not Null,
      LetadloP_ID Char(10) Not Null,
      OdletZ_NavObjek_ID Char(5) Not Null,
      Pristani_NavObjek_ID Char(5) Not Null,
      Constraint PK_LetovyPlan Primary Key (SchvalCislo)
);
Alter table Letiste add Constraint FK_Letiste_1 Foreign Key (NavObjek_ID) References NavObjekt(ID);
Alter table NavigacniBod add Constraint FK_NavigacniBod_1 Foreign Key (NavObjek_ID) References NavObjekt(ID);
Alter table PlanPresBod add Constraint FK_PlanPresBod_1 Foreign Key (LetovyPl_SchvalCislo) References LetovyPlan(SchvalCislo);
Alter table PlanPresBod add Constraint FK_PlanPresBod_2 Foreign Key (Navigacn_NavObjek_ID) References NavigacniBod(NavObjek_ID);
Alter table RegLetadlo add Constraint FK_RegLetadlo_1 Foreign Key (Provozov_ID) References Provozovatel(ID);
Alter table RegLetadlo add Constraint FK_RegLetadlo_2 Foreign Key (Typ_CisloSchvaleni) References TypLetadla(CisloSchvaleni);
Alter table LetovyPlan add Constraint FK_LetovyPlan_1 Foreign Key (Pristani_NavObjek_ID) References Letiste(NavObjek_ID);
Alter table LetovyPlan add Constraint FK_LetovyPlan_2 Foreign Key (OdletZ_NavObjek_ID) References Letiste(NavObjek_ID);
Alter table LetovyPlan add Constraint FK_LetovyPlan_3 Foreign Key (LetadloP_ID) References RegLetadlo(ID);
Alter table LetovyPlan add Constraint FK_LetovyPlan_4 Foreign Key (KapitanL_CisloLetPrukazu) References Pilot(CisloLetPrukazu);

INSERT INTO NAVOBJEKT ( ID, NAZEV, ZEMSIRKA, ZEMDELKA, VYSKAMSL ) VALUES (
'RUZYN', 'Ruzyne intl. airport', 14.125, 52.75, 372);
INSERT INTO NAVOBJEKT ( ID, NAZEV, ZEMSIRKA, ZEMDELKA, VYSKAMSL ) VALUES (
'KBELY', 'Letiste Kbely', 14.23, 52.5, 420);
INSERT INTO NAVOBJEKT ( ID, NAZEV, ZEMSIRKA, ZEMDELKA, VYSKAMSL ) VALUES (
'BRNO ', 'Brno intl. airport', 17.822, 50.014, 264);
INSERT INTO NAVOBJEKT ( ID, NAZEV, ZEMSIRKA, ZEMDELKA, VYSKAMSL ) VALUES (
'MEL  ', 'Melnik VORDME', 14.17, 53, 300);
INSERT INTO NAVOBJEKT ( ID, NAZEV, ZEMSIRKA, ZEMDELKA, VYSKAMSL ) VALUES (
'BER  ', 'Beroun VOR', 13.85, 52.05, 479);
INSERT INTO NAVOBJEKT ( ID, NAZEV, ZEMSIRKA, ZEMDELKA, VYSKAMSL ) VALUES (
'LIB  ', 'Liberec VORDME', 14.51, 56.4, 745);
commit;

INSERT INTO NAVIGACNIBOD ( NAVOBJEK_ID ) VALUES (
'MEL  ');
INSERT INTO NAVIGACNIBOD ( NAVOBJEK_ID ) VALUES (
'BER  ');
INSERT INTO NAVIGACNIBOD ( NAVOBJEK_ID ) VALUES (
'LIB  ');
commit;

INSERT INTO LETISTE ( NAVOBJEK_ID ) VALUES (
'RUZYN');
INSERT INTO LETISTE ( NAVOBJEK_ID ) VALUES (
'KBELY');
INSERT INTO LETISTE ( NAVOBJEK_ID ) VALUES (
'BRNO ');
commit;

INSERT INTO PILOT ( RODCISLO, CISLOLETPRUKAZU, JMENO ) VALUES (
'791005/6011', 795170, 'Petr Kadlec');
INSERT INTO PILOT ( RODCISLO, CISLOLETPRUKAZU, JMENO ) VALUES (
'800328/0813', 654321, 'Michal Èihaø');
INSERT INTO PILOT ( RODCISLO, CISLOLETPRUKAZU, JMENO ) VALUES (
'550101/1234', 445566, 'František Dobrota');
commit;

INSERT INTO PROVOZOVATEL ( ID, NAZEV ) VALUES (
1, 'Èeské Aerolinie');
INSERT INTO PROVOZOVATEL ( ID, NAZEV ) VALUES (
2, 'Moravia Airlines');
INSERT INTO PROVOZOVATEL ( ID, NAZEV ) VALUES (
3, 'Martin Novák');
INSERT INTO PROVOZOVATEL ( ID, NAZEV ) VALUES (
4, 'Letectvo Armády Èeské Republiky');
INSERT INTO PROVOZOVATEL ( ID, NAZEV ) VALUES (
5, 'František Dobrota');
commit;

INSERT INTO TYPLETADLA ( CISLOSCHVALENI, VYROBCE, NAZEV, TRIDA,
PASAZERU ) VALUES (
445784, 'Boeing', 'Boeing 757-300', 'B', 150);
INSERT INTO TYPLETADLA ( CISLOSCHVALENI, VYROBCE, NAZEV, TRIDA,
PASAZERU ) VALUES (
449554, 'Cessna', 'Cessna Skyplane IV', 'N', 4);
INSERT INTO TYPLETADLA ( CISLOSCHVALENI, VYROBCE, NAZEV, TRIDA,
PASAZERU ) VALUES (
150454, 'McDonnell Douglas', 'F-2 Phantom', 'M', 2);
INSERT INTO TYPLETADLA ( CISLOSCHVALENI, VYROBCE, NAZEV, TRIDA,
PASAZERU ) VALUES (
460487, 'Let Kunovice', 'L-47 Zlin', 'A', 1);
commit;

INSERT INTO REGLETADLO ( PROVOZOV_ID, TYP_CISLOSCHVALENI, ID ) VALUES (
1, 445784, 'OK-OSTRAVA');
INSERT INTO REGLETADLO ( PROVOZOV_ID, TYP_CISLOSCHVALENI, ID ) VALUES (
2, 445784, 'OK-BLZC   ');
INSERT INTO REGLETADLO ( PROVOZOV_ID, TYP_CISLOSCHVALENI, ID ) VALUES (
3, 449554, 'OK-NOV    ');
INSERT INTO REGLETADLO ( PROVOZOV_ID, TYP_CISLOSCHVALENI, ID ) VALUES (
1, 449554, 'OK-MALACSA');
INSERT INTO REGLETADLO ( PROVOZOV_ID, TYP_CISLOSCHVALENI, ID ) VALUES (
4, 150454, 'BRAVO4    ');
INSERT INTO REGLETADLO ( PROVOZOV_ID, TYP_CISLOSCHVALENI, ID ) VALUES (
5, 460487, 'OK-AKRO1  ');
commit;

INSERT INTO LETOVYPLAN ( KAPITANL_CISLOLETPRUKAZU, CASODLETU, PREDPIS, SCHVALCISLO, LETADLOP_ID,
ODLETZ_NAVOBJEK_ID, PRISTANI_NAVOBJEK_ID ) VALUES (
795170, '15.05.2000 14:50', 'V', 100253, 'OK-OSTRAVA', 'KBELY', 'BRNO ');
INSERT INTO LETOVYPLAN ( KAPITANL_CISLOLETPRUKAZU, CASODLETU, PREDPIS, SCHVALCISLO, LETADLOP_ID,
ODLETZ_NAVOBJEK_ID, PRISTANI_NAVOBJEK_ID ) VALUES (
445566, '25.05.2000 16:00', 'V', 100254, 'OK-AKRO1  ', 'KBELY', 'KBELY');
INSERT INTO LETOVYPLAN ( KAPITANL_CISLOLETPRUKAZU, CASODLETU, PREDPIS, SCHVALCISLO, LETADLOP_ID,
ODLETZ_NAVOBJEK_ID, PRISTANI_NAVOBJEK_ID ) VALUES (
654321, '06.06.2000 22:00', 'I', 100255, 'OK-MALACSA', 'RUZYN', 'BRNO ');
INSERT INTO LETOVYPLAN ( KAPITANL_CISLOLETPRUKAZU, CASODLETU, PREDPIS, SCHVALCISLO, LETADLOP_ID,
ODLETZ_NAVOBJEK_ID, PRISTANI_NAVOBJEK_ID ) VALUES (
795170, '10.07.2000 10:15', 'V', 100256, 'BRAVO4    ', 'BRNO ', 'KBELY');
INSERT INTO LETOVYPLAN ( KAPITANL_CISLOLETPRUKAZU, CASODLETU, PREDPIS, SCHVALCISLO, LETADLOP_ID,
ODLETZ_NAVOBJEK_ID, PRISTANI_NAVOBJEK_ID ) VALUES (
445566, '10.07.2000 10:45', 'V', 100257, 'OK-AKRO1  ', 'KBELY', 'KBELY');
commit;

INSERT INTO PLANPRESBOD ( PORADIVPLANU, LETOVYPL_SCHVALCISLO,
NAVIGACN_NAVOBJEK_ID ) VALUES ( 
0, 100253, 'MEL  '); 
INSERT INTO PLANPRESBOD ( PORADIVPLANU, LETOVYPL_SCHVALCISLO,
NAVIGACN_NAVOBJEK_ID ) VALUES ( 
1, 100253, 'LIB  '); 
INSERT INTO PLANPRESBOD ( PORADIVPLANU, LETOVYPL_SCHVALCISLO,
NAVIGACN_NAVOBJEK_ID ) VALUES ( 
0, 100255, 'BER  '); 
INSERT INTO PLANPRESBOD ( PORADIVPLANU, LETOVYPL_SCHVALCISLO,
NAVIGACN_NAVOBJEK_ID ) VALUES ( 
0, 100256, 'LIB  '); 
INSERT INTO PLANPRESBOD ( PORADIVPLANU, LETOVYPL_SCHVALCISLO,
NAVIGACN_NAVOBJEK_ID ) VALUES ( 
1, 100256, 'MEL  '); 
INSERT INTO PLANPRESBOD ( PORADIVPLANU, LETOVYPL_SCHVALCISLO,
NAVIGACN_NAVOBJEK_ID ) VALUES ( 
0, 100257, 'BER  '); 
commit;
