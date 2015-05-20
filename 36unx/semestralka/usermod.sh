#!/bin/ksh

# Nepracujeme s /etc/passwd, /etc/shadow a /etc/group, ale s kopiemi:
passwd="passwd"
group="group"
shadow="shadow"
temp="ptmp"
temp2="stmp"
# Uschovame jmeno programu (pro chybove hlasky)
progname=$0
# Inicializace promennych
flaglogin=0
flagcomment=0
flaghomedir=0
flagexpire=0
flaginactive=0
flaggroup=0
#flagsecgroups=0
flagloginname=0
flagpasswd=0
flagshell=0
flaguid=0
lockpass=0
unlockpass=0
checkidunique=0
movehome=0
if [ -f "$shadow" ]; then shadowenable=1; else shadowenable=0; fi

# ------------- Kontrola a zpracovani prikazove radky --------------
usage() {
    if [ $shadowenable -eq 1 ]; then
       echo "Usage: $progname [-c comment] [-d home_dir [-m]] [-e expire_date]">&2
       echo "\t\t[-f inactive_time] [-g initial_group] [-G group[,...]]">&2
       echo "\t\t[-l login_name] [-p passwd] [-s shell] [-u uid [-o]]">&2
       echo "\t\t[-L|-U] login">&2
    else
       echo "Usage: $progname [-c comment] [-d home_dir [-m]] [-g initial_group]">&2
       echo "\t\t[-G group[,...]] [-l login_name] [-p passwd] [-s shell]">&2
       echo "\t\t[-u uid [ -o]] [-L|-U] login">&2
    fi
    exit 2
}

checkparam() {
    # Parametr musi mit hodnotu
    if [ $# -lt 2 ]; then usage; fi
    # V parametru nesmi byt dvojtecka!
    if echo "$2" | grep ":" > /dev/null; then usage; fi
}

checkshadow() {
    if [ $shadowenable -eq 0 ]; then
       echo $progname: $shadow not found>&2
       exit 14
    fi
}

# Jednotlive parametry
if [ $# -eq 0 ]; then usage; fi
#echo "At beginning: $#"

while [ $# -gt 1 ]; do
  if [ "$1" = "-c" ]; then
    checkparam $*; shift
    newcomment=$1
    flagcomment=1
    checkparam $*; shift
  elif [ "$1" = "-d" ]; then
    checkparam $*; shift
    newhomedir=$1
    flaghomedir=1
    checkparam $*; shift
    if [ $1 = "-m" ]; then
        movehome=1
	checkparam $*; shift
    fi
  elif [ "$1" = "-e" ]; then
    checkparam $*; shift
    # prepocitej expiration date z YYYY-MM-DD na pocet dni od 1970-01-01
    newexpire=$1
    exp_year=`echo $1 | cut -d- -f 1`
    exp_month=`echo $1 | cut -d- -f 2`
    exp_day=`echo $1 | cut -d- -f 3`
    if [ $(( $exp_year % 4 )) -eq 0 ]; then leapyear=1; else leapyear=0; fi
    case $exp_month in
	"01") exp_monthday=0 ;;
	"02") exp_monthday=31 ;;
	"03") exp_monthday=$(( 59 + $leapyear )) ;;
	"04") exp_monthday=$(( 90 + $leapyear )) ;;
	"05") exp_monthday=$(( 120 + $leapyear )) ;;
	"06") exp_monthday=$(( 151 + $leapyear )) ;;
	"07") exp_monthday=$(( 181 + $leapyear )) ;;
	"08") exp_monthday=$(( 212 + $leapyear )) ;;
	"09") exp_monthday=$(( 243 + $leapyear )) ;;
	"10") exp_monthday=$(( 273 + $leapyear )) ;;
	"11") exp_monthday=$(( 304 + $leapyear )) ;;
	"12") exp_monthday=$(( 334 + $leapyear )) ;;
	*) echo "$progname: invalid date" >&2; exit 3;;	
    esac
    newexpire=$(( ( $exp_year - 1970 ) * 1461 / 4 + $exp_monthday + $exp_day ))
    # WOW! Teda, jestli tohle funguje, tak je to neco... :)
    
    flagexpire=1
    checkparam $*; shift
    checkshadow
  elif [ "$1" = "-f" ]; then
    checkparam $*; shift
    newinactive=$1
    flaginactive=1
    checkparam $*; shift
    checkshadow
  elif [ "$1" = "-g" ]; then
    checkparam $*; shift
    newgroup=$1
    flaggroup=1
    checkparam $*; shift
  elif [ "$1" = "-G" ]; then
    checkparam $*; shift
    #flagsecgroups=1
    #secgroupcount=0
    #grouplist=$1
    #while [ "$grouplist" ]; do
#	group[$secgroupcount]=`echo $grouplist | cut -d, -f 1`
#	grouplist=`echo $grouplist | cut -d, -f 2-`
#	secgroupcount=$(( $secgroupcount + 1 ))
#    done
    echo $progname: -G option not implemented
    exit 2
    checkparam $*; shift
  elif [ "$1" = "-l" ]; then
    checkparam $*; shift
    newlogin=$1
    flaglogin=1
    checkparam $*; shift
  elif [ "$1" = "-p" ]; then
    checkparam $*; shift
    newpass=$1
    flagpasswd=1
    if [ $lockpass -eq 1 ]; then
        echo $progname: You can\'t use -L and -p together>&2
        exit 2
    fi
    if [ $unlockpass -eq 1 ]; then
        echo $progname: You can\'t use -U and -p together>&2
        exit 2
    fi
    checkparam $*; shift
  elif [ "$1" = "-s" ]; then
    checkparam $*; shift
    newshell=$1
    flagshell=1
    checkparam $*; shift
  elif [ "$1" = "-u" ]; then
    checkparam $*; shift
    newuid=$1
    flaguid=1    
    checkparam $*; shift
    checkuidunique=1
    if [ "$1" = "-o" ]; then
        checkuidunique=0
        checkparam $*; shift
    fi
  elif [ "$1" = "-L" ]; then
    checkparam $*; shift
    lockpass=1
    if [ $flagpasswd -eq 1 ]; then
        echo $progname: You can\'t use -L and -p together>&2
        exit 2
    fi
    if [ $unlockpass -eq 1 ]; then usage; fi
  elif [ "$1" = "-U" ]; then
    checkparam $*; shift
    unlockpass=1
    if [ $flagpasswd -eq 1 ]; then
        echo $progname: You can\'t use -U and -p together>&2
        exit 2
    fi
    if [ $lockpass -eq 1 ]; then usage; fi
  else
    usage
  fi
done

if [ $# -ne 1 ]; then usage; fi
login=$1

# ----------------------- Zjisteni puvodnich hodnot pro daneho uzivatele -------------------------
foundline=`grep -n "^${login}:" $passwd | head -1`

if [ ! "$foundline" ] ; then
    echo $progname: $login: No such user>&2
    exit 6;
fi

if who | cut "-d " -f 1 | grep "^${login}\$" > /dev/null; then
    echo $progname: The login to be modified is in use.>&2
    exit 8
fi

olduid=`echo "$foundline" | cut -d: -f 4`

linepos=`echo "$foundline" | cut -d: -f 1`

if [ $shadowenable -eq 1 ]; then
   foundline2=`grep -n "^${login}:" $shadow | head -1`
   if [ ! "$foundline2" ]; then
       # Mozna je tam UID misto loginjmena
       foundline2=`grep -n "^${olduid}:" $shadow | head -1`
       if [ ! "$foundline2" ]; then
           # Uzivatel v shadow neexistuje => ignoruj shadow
           shadowenable=0
           echo $progname: warning: No such user in $shadow>&2
           #exit 6
       fi
   fi
fi

# ----------------------- Nastaveni cilovych hodnot pro daneho uzivatele -------------------------

if [ $shadowenable -eq 1 ]; then
    linepos2=`echo "$foundline2" | cut -d: -f 1`
    lastchg=`echo "$foundline2" | cut -d: -f 4`
    minchg=`echo "$foundline2" | cut -d: -f 5`
    maxchg=`echo "$foundline2" | cut -d: -f 6`
    warnchg=`echo "$foundline2" | cut -d: -f 7`
    if [ $flaginactive -eq 0 ]; then
	newinactive=`echo "$foundline2" | cut -d: -f  8`
    else
	logger -p user.info Change user \`$login\' inactive from `echo "$foundline2" | cut -d: -f  8` to $newinactive
    fi
    if [ $flagexpire -eq 0 ]; then 
	newexpire=`echo "$foundline2" | cut -d: -f 9`
    else
	logger -p user.info Change user \`$login\' expiration from `echo "$foundline2" | cut -d: -f 9` to $newexpire
    fi
    shadowflag=`echo "$foundline2" | cut -d: -f 10`
fi

if [ $flaglogin -eq 0 ]; then
    newlogin=$login;
else
    if [ $newlogin = $login ]; then
	flaglogin=0
    elif grep "^${newlogin}:" $passwd > /dev/null; then
	echo $progname: Requested new login name is not unique>&2
	exit 9
    else
	logger -p user.info change user name \`$login\' to \`$newlogin\'
    fi
fi

if [ $flagpasswd -eq 1 ]; then
    lastchg=$(( `date +%s` / 86400 ))	# Spocitej dnesni "datum" pro polozku posledni zmena hesla
    logger -p user.info change user \`$login\' password
else
    if [ $shadowenable -eq 1 ]; then
	newpass=`echo "$foundline2" | cut -d: -f 3`
    else
        newpass=`echo "$foundline" | cut -d: -f 2`
    fi
fi

if [ $flaguid -eq 0 ]; then
    newuid=$olduid
else
  if [ $checkidunique -ne 0 ]; then
    if [ "$olduid" != "$newuid" ]; then
	if cut -d: -f 2 "$passwd" | grep "$newuid" > /dev/null; then
	    echo "$progname: Requested new uid is not unique (use -o to suppress this error)">&2
	    exit 4
        fi
    fi
  elif [ "$olduid" != "$newuid" ]; then
    logger -p user.info change user \`$login\' UID from \`$olduid\' to \`$newuid\'
  fi  
fi

if [ $flaggroup -eq 0 ]; then
    newgid=`echo "$foundline" | cut -d: -f 5`
else
    # Preklad jmena skupiny na guid
    newgid=`grep "^${newgroup}:" $group | cut -d: -f 3 | head -1`
    if [ ! "$newgid" ]; then 
      newgid=`cut -d: -f 3 $group | grep "^$newgroup\$" | head -1`
      if [ ! "$newgid" ]; then
	  echo $progname: The specified group does not exist>&2
	  exit 6
      fi
    fi
    oldgid=`echo "$foundline" | cut -d: -f 4`
    if [ "$oldgid" != "$newgid" ]; then
	logger -p user.info change user \`$login\' GID from \`$oldgid\' to \`$newgid\'
    fi
fi

if [ $flagcomment -eq 0 ]; then
   newcomment=`echo "$foundline" | cut -d: -f 6`
fi

if [ $flaghomedir -eq 0 ]; then
    newhomedir=`echo "$foundline" | cut -d: -f 7`
else
    oldhomedir=`echo "$foundline" | cut -d: -f 7`
    if [ $movehome -ne 0 ]; then
	if [ ! -d "$oldhomedir" ]; then
	    echo "$progname: $passwd corrupted ($oldhomedir is not a valid directory!)">&2
	    exit 12
        fi
        if [ -d "$newhomedir" ]; then
	    echo "$progname: Cannot move the home directory: the new directory already exists">&2
	    exit 12
        fi
	echo $progname: debug: Moving home directory...
	mv "$oldhomedir" "$newhomedir"
    fi
    logger -p user.info change user \`$login\' home from \`$oldhomedir\' to \`$newhomedir\'
fi

if [ $flagshell -eq 0 ]; then
   newshell=`echo "$foundline" | cut -d: -f 8`
else
   logger -p user.info change user \`$login\' shell from \``echo "$foundline" | cut -d: -f 8`\' to \`$newshell\'
fi

if [ $lockpass -ne 0 ]; then
    if ! echo "$newpass" | grep "^!" > /dev/null; then
	newpass="!$newpass"
	logger -p user.info lock user \`$login\' password
    else
	echo $progname: Password already locked
	lockpass=0
    fi
fi

if [ $unlockpass -ne 0 ]; then
    if echo "$newpass" | grep "^!" > /dev/null; then
	newpass=`echo "$newpass" | cut -c 2-`
	logger -p user.info unlock user \`$login\' password
    else
	echo $progname: Password not locked
	unlockpass=0	
    fi
fi

# ------------------------ Vytvoreni novych souboru s upravenymi hodnotami ---------------------------
#echo "$progname: debug: login=$newlogin, pass=$newpass, uid=$newuid, guid=$newguid"
#echo "\t comment=$newcomment, home=$newhomedir, shell=$newshell"
#echo "\t lastchg=$lastchg, minchg=$minchg, maxchg=$maxchg, warnchg=$warnchg"
#echo "\t inactive=$newinactive, expire=$newexpire, shadowflag=$shadowflag"
#echo "---"

if [ $linepos -gt 1 ]; then head -n $(($linepos-1)) "$passwd" > "$temp"; fi
if [ $shadowenable -eq 1 ]; then
    echo "$newlogin:x:$newuid:$newgid:$newcomment:$newhomedir:$newshell" >> "$temp"
else
    echo "$newlogin:$newpass:$newuid:$newgid:$newcomment:$newhomedir:$newshell" >> "$temp"
fi
tail -n +$(($linepos+1)) "$passwd" >> "$temp"

if [ $shadowenable -eq 1 ]; then
    if [ $linepos2 -gt 1 ]; then head -n $(($linepos2-1)) "$shadow" > "$temp2"; fi
    echo "$newlogin:$newpass:$lastchg:$minchg:$maxchg:$warnchg:$newinactive:$newexpire:$shadowflag" >> "$temp2"
    tail -n +$(($linepos2+1)) "$shadow" >> "$temp2"
fi

# -------------------------- Premazani starych souboru novymi ----------------------------------
mv "$temp" "$passwd"
if [ $shadowenable -eq 1 ]; then mv "$temp2" "$shadow"; fi

#echo $progname: "debug: All done OK"
