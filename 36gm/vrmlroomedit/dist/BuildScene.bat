@ECHO OFF

REM ***** Zde nastavte cestu ke knihovne Xerces: *****
SET XERCES_CP=C:\Progra~1\JavaSoft\JRE\1.3.1\lib\ext\xerces-1_4_4\xerces.jar

REM ***** Zde nastavte cestu ke knihovne CyberVRML97: *****
SET CV97_CP=C:\Progra~1\JavaSoft\JRE\1.3.1\lib\ext\cv97

JAVA %3 %4 -classpath "sceneautobuilder.jar;%XERCES_CP%;%CV97_CP%" cz.cvut.felk.cs.gm.sceneautobuilder.BuildScene %1 %2
