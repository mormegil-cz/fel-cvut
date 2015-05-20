Drop table TypLetadla cascade constraints;
Drop table Provozovatel cascade constraints;
Drop table Pilot cascade constraints;
Drop table Letiste cascade constraints;
Drop table NavigacniBod cascade constraints;
Drop table PlanPresBod cascade constraints;
Drop table NavObjekt cascade constraints;
Drop table RegLetadlo cascade constraints;
Drop table LetovyPlan cascade constraints;
Create table TypLetadla (
      CisloSchvaleni Integer Not Null,
      Vyrobce VarChar2(100) Not Null,
      Nazev VarChar2(100) Not Null,
      Trida Char(1) Not Null,
      Pasazeru Integer Not Null,
      Constraint PK_TypLetadla Primary Key (CisloSchvaleni)
);
Create table Provozovatel (
      ID Integer Not Null,
      Nazev VarChar2(100) Not Null,
      Constraint PK_Provozovatel Primary Key (ID)
);
Create table Pilot (
      CisloLetPrukazu Integer Not Null,
      Jmeno VarChar2(100) Not Null,
      RodneCislo Char(11) Not Null,
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
      Nazev VarChar2(100) Not Null,
      ZemSirka Float Not Null,
      ZemDelka Float Not Null,
      VyskaMSL Float Not Null,
      Constraint PK_NavObjekt Primary Key (ID)
);
Create table RegLetadlo (
      LetadlaP_ID Integer Not Null,
      LetadlaT_CisloSchvaleni Integer Not Null,
      ID Char(10) Not Null,
      Constraint PK_RegLetadlo Primary Key (ID)
);
Create table LetovyPlan (
      LetyPilo_CisloLetPrukazu Integer Not Null,
      CasOdletu Float Not Null,
      Predpis Char(1) Not Null,
      SchvalCislo Integer Not Null,
      LetyLeta_ID Char(10) Not Null,
      Starty_NavObjek_ID Char(5) Not Null,
      Pristani_NavObjek_ID Char(5) Not Null,
      Constraint PK_LetovyPlan Primary Key (SchvalCislo)
);
Alter table Letiste add (
      Constraint FK_Letiste_1 Foreign Key (NavObjek_ID) References NavObjekt(ID)
);
Alter table NavigacniBod add (
      Constraint FK_NavigacniBod_1 Foreign Key (NavObjek_ID) References NavObjekt(ID)
);
Alter table PlanPresBod add (
      Constraint FK_PlanPresBod_1 Foreign Key (LetovyPl_SchvalCislo) References LetovyPlan(SchvalCislo),
      Constraint FK_PlanPresBod_2 Foreign Key (Navigacn_NavObjek_ID) References NavigacniBod(NavObjek_ID)
);
Alter table RegLetadlo add (
      Constraint FK_RegLetadlo_1 Foreign Key (LetadlaP_ID) References Provozovatel(ID),
      Constraint FK_RegLetadlo_2 Foreign Key (LetadlaT_CisloSchvaleni) References TypLetadla(CisloSchvaleni)
);
Alter table LetovyPlan add (
      Constraint FK_LetovyPlan_1 Foreign Key (Pristani_NavObjek_ID) References Letiste(NavObjek_ID),
      Constraint FK_LetovyPlan_2 Foreign Key (Starty_NavObjek_ID) References Letiste(NavObjek_ID),
      Constraint FK_LetovyPlan_3 Foreign Key (LetyLeta_ID) References RegLetadlo(ID),
      Constraint FK_LetovyPlan_4 Foreign Key (LetyPilo_CisloLetPrukazu) References Pilot(CisloLetPrukazu)
);
