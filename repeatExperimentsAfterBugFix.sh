#!/usr/bin/env bash

trap "echo; exit" INT

declare -a ontologies
ontologies+=(ore_ont_22)
ontologies+=(ore_ont_257)
ontologies+=(ore_ont_699)
ontologies+=(ore_ont_802)
ontologies+=(ore_ont_1182)
ontologies+=(ore_ont_1481)
ontologies+=(ore_ont_2111)
ontologies+=(ore_ont_2578)
ontologies+=(ore_ont_2622)
ontologies+=(ore_ont_3087)
ontologies+=(ore_ont_3133)
ontologies+=(ore_ont_3164)
ontologies+=(ore_ont_3241)
ontologies+=(ore_ont_3529)
ontologies+=(ore_ont_3560)
ontologies+=(ore_ont_3561)
ontologies+=(ore_ont_3640)
ontologies+=(ore_ont_3873)
ontologies+=(ore_ont_3914)
ontologies+=(ore_ont_4182)
ontologies+=(ore_ont_4412)
ontologies+=(ore_ont_4598)
ontologies+=(ore_ont_4737)
ontologies+=(ore_ont_5059)
ontologies+=(ore_ont_5253)
ontologies+=(ore_ont_5596)
ontologies+=(ore_ont_5721)
ontologies+=(ore_ont_5781)
ontologies+=(ore_ont_5857)
ontologies+=(ore_ont_6108)
ontologies+=(ore_ont_6132)
ontologies+=(ore_ont_6146)
ontologies+=(ore_ont_6444)
ontologies+=(ore_ont_6627)
ontologies+=(ore_ont_6682)
ontologies+=(ore_ont_6816)
ontologies+=(ore_ont_6948)
ontologies+=(ore_ont_7188)
ontologies+=(ore_ont_7416)
ontologies+=(ore_ont_7578)
ontologies+=(ore_ont_7706)
ontologies+=(ore_ont_8068)
ontologies+=(ore_ont_8148)
ontologies+=(ore_ont_8194)
ontologies+=(ore_ont_8322)
ontologies+=(ore_ont_8627)
ontologies+=(ore_ont_8688)
ontologies+=(ore_ont_9303)
ontologies+=(ore_ont_9400)
ontologies+=(ore_ont_9654)
ontologies+=(ore_ont_10127)
ontologies+=(ore_ont_10212)
ontologies+=(ore_ont_10929)
ontologies+=(ore_ont_10991)
ontologies+=(ore_ont_11207)
ontologies+=(ore_ont_11311)
ontologies+=(ore_ont_11459)
ontologies+=(ore_ont_11502)
ontologies+=(ore_ont_11635)
ontologies+=(ore_ont_11745)
ontologies+=(ore_ont_12128)
ontologies+=(ore_ont_12174)
ontologies+=(ore_ont_12351)
ontologies+=(ore_ont_13052)
ontologies+=(ore_ont_13233)
ontologies+=(ore_ont_13647)
ontologies+=(ore_ont_13700)
ontologies+=(ore_ont_13837)
ontologies+=(ore_ont_13846)
ontologies+=(ore_ont_13991)
ontologies+=(ore_ont_14221)
ontologies+=(ore_ont_14375)
ontologies+=(ore_ont_14380)
ontologies+=(ore_ont_14863)
ontologies+=(ore_ont_14881)
ontologies+=(ore_ont_14897)
ontologies+=(ore_ont_14919)
ontologies+=(ore_ont_15063)
ontologies+=(ore_ont_15172)
ontologies+=(ore_ont_15655)
ontologies+=(ore_ont_16235)
ontologies+=(ore_ont_16315)
ontologies+=(ore_ont_16420)
ontologies+=(ore_ont_16457)
ontologies+=(ore_ont_16666)
ontologies+=(ore_ont_16701)

Reset='\033[0m'
Yellow='\033[0;33m'
Orange='\033[0;31m'
Red='\033[0;35m'
Cyan='\033[0;36m'
Blue='\033[0;34m'
Green='\033[0;32m'
Gray='\033[0;37m'
Black='\033[0;30m'

time="8h"

echo ""
echo "Script running with PID $$"

function runPrototype {
  runPrototypeAtMostTwice $1 $2 $3 "1"
}

function runPrototypeAtMostTwice {
  if [[ $1 == "Reduction" ]]; then
    echo -n "[$(date +"%T")] Computing reduction (timeout: ${time})... "
    arg1="None"
    arg2="onlyComputeReduction"
  else
    echo -n "[$(date +"%T")] Computing experiment (disjointness axioms: $1, maximal role depth: $2, maximal conjunction size: $3, timeout: ${time})... "
    arg1=$1
    arg2="computeEverything"
  fi
  output=$(timeout $time ../graalvm-ee-java19-22.3.0/bin/java -Xms80g -Xmx80g -XX:+ExitOnOutOfMemoryError -jar efficient-axiomatization-of-owl2el-ontologies-from-data-assembly-0.2.1-SNAPSHOT.jar $id $arg1 $arg2 quiet $2 $3 2>&1)
  exitStatus=$?
  if [[ "${output}" == *"OutOfMemory"* ]]; then exitStatus=3; fi
  case $exitStatus in
    0) # successful computation
      echo -e "${Green}Success${Reset}"
      ;;
    124|137) # Timeout
      echo -e "${Yellow}Timeout${Reset}"
      echo "${key};$1-$2-$3;Timeout(${time});;;;;;;;;;;;;;;;;;;;;;;;;" >> "ore2015_pool_sample_experiments/results/${key}.csv"
      ;;
    3) # Out of memory
      echo -e "${Orange}Out of memory${Reset}"
      echo "${key};$1-$2-$3;OutOfMemory(80g);;;;;;;;;;;;;;;;;;;;;;;;;" >> "ore2015_pool_sample_experiments/results/${key}.csv"
      ;;
    4) # Inconsistent
      echo -e "${Cyan}Inconsistent${Reset}"
      ;;
    5) # Powering too large
      echo -e "${Cyan}Powering too large${Reset}"
      ;;
    *) # other error
      echo -e "${Red}Error ${exitStatus}${Reset}"
      echo "${key};$1-$2-$3;Error(${exitStatus});;;;;;;;;;;;;;;;;;;;;;;;;" >> "ore2015_pool_sample_experiments/results/${key}.csv"
      ;;
  esac
  if [[ ${exitStatus} -ne 0 ]]; then echo "$output"; fi
  if [[ $4 -ne 0 ]] && [[ ${exitStatus} -ne 0 ]] && [[ ${exitStatus} -ne 124 ]] && [[ ${exitStatus} -ne 137 ]] && [[ ${exitStatus} -ne 3 ]] && [[ ${exitStatus} -ne 4 ]] && [[ ${exitStatus} -ne 5 ]]; then
    runPrototypeAtMostTwice $1 $2 $3 "0"
  fi
}

function skipRunPrototype {
  echo -n "[$(date +"%T")] Computing experiment (disjointness axioms: $1, maximal role depth: $2, maximal conjunction size: $3, timeout: ${time})... "
  echo -e "${Blue}Skipped${Reset}"
}

for key in "${ontologies[@]}"; do

  if [[ -f "ore2015_pool_sample_experiments/files/${key}_reduced.owl" ]]; then

    echo ""
    echo "Processing $key..."
    id=${key//ore_ont_/}

    reducedDomainSize=$(cut -d';' -f2 < "ore2015_pool_sample_experiments/statistics/${key}.csv")
    echo "           ${reducedDomainSize} objects in the reduction"

    if [[ ${reducedDomainSize} -ge 10 ]]; then

      gawk -i inplace '/Reduction/' "ore2015_pool_sample_experiments/results/${key}.csv"

      exitStatus_None_0_32=999
      exitStatus_Fast_0_32=999
      exitStatus_Canonical_0_32=999

      runPrototype "None" "0" "32"
      exitStatus_None_0_32=${exitStatus}

      if [[ ${exitStatus_None_0_32} -eq 0 ]]; then
        runPrototype "Fast" "0" "32"
        exitStatus_Fast_0_32=${exitStatus}
      else
        skipRunPrototype "Fast" "0" "32"
      fi

      if [[ ${exitStatus_None_0_32} -eq 0 ]]; then
        runPrototype "Canonical" "0" "32"
        exitStatus_Canonical_0_32=${exitStatus}
      else
        skipRunPrototype "Canonical" "0" "32"
      fi

      exitStatus_None_1_8=999
      exitStatus_Fast_1_8=999
      exitStatus_Canonical_1_8=999

      runPrototype "None" "1" "8"
      exitStatus_None_1_8=${exitStatus}

      if [[ ${exitStatus_None_1_8} -eq 0 ]]; then
        runPrototype "Fast" "1" "8"
        exitStatus_Fast_1_8=${exitStatus}
      else
        skipRunPrototype "Fast" "1" "8"
      fi

      if [[ ${exitStatus_Fast_1_8} -eq 0 ]]; then
        runPrototype "Canonical" "1" "8"
        exitStatus_Canonical_1_8=${exitStatus}
      else
        skipRunPrototype "Canonical" "1" "8"
      fi

      exitStatus_None_1_32=999
      exitStatus_Fast_1_32=999
      exitStatus_Canonical_1_32=999

      if [[ ${exitStatus_None_1_8} -eq 0 ]] && [[ ${exitStatus_None_0_32} -eq 0 ]]; then
        runPrototype "None" "1" "32"
        exitStatus_None_1_32=${exitStatus}
      else
        skipRunPrototype "None" "1" "32"
      fi

      if [[ ${exitStatus_None_1_32} -eq 0 ]] && [[ ${exitStatus_Fast_0_32} -eq 0 ]] &&[[ ${exitStatus_Fast_1_8} -eq 0 ]]; then
        runPrototype "Fast" "1" "32"
        exitStatus_Fast_1_32=${exitStatus}
      else
        skipRunPrototype "Fast" "1" "32"
      fi

      if [[ ${exitStatus_Fast_1_32} -eq 0 ]] && [[ ${exitStatus_Canonical_0_32} -eq 0 ]] && [[ ${exitStatus_Canonical_1_8} -eq 0 ]]; then
        runPrototype "Canonical" "1" "32"
        exitStatus_Canonical_1_32=${exitStatus}
      else
        skipRunPrototype "Canonical" "1" "32"
      fi

      exitStatus_None_2_32=999
      exitStatus_Fast_2_32=999
      exitStatus_Canonical_2_32=999

      if [[ ${exitStatus_None_1_32} -eq 0 ]]; then
        runPrototype "None" "2" "32"
        exitStatus_None_2_32=${exitStatus}
      else
        skipRunPrototype "None" "2" "32"
      fi

      if [[ ${exitStatus_None_2_32} -eq 0 ]] && [[ ${exitStatus_Fast_1_32} -eq 0 ]]; then
        runPrototype "Fast" "2" "32"
        exitStatus_Fast_2_32=${exitStatus}
      else
        skipRunPrototype "Fast" "2" "32"
      fi

      if [[ ${exitStatus_Fast_2_32} -eq 0 ]] && [[ ${exitStatus_Canonical_1_32} -eq 0 ]]; then
        runPrototype "Canonical" "2" "32"
        exitStatus_Canonical_2_32=${exitStatus}
      else
        skipRunPrototype "Canonical" "2" "32"
      fi

      exitStatus_None_INF_32=999
      exitStatus_Fast_INF_32=999
      exitStatus_Canonical_INF_32=999

      if [[ ${exitStatus_None_2_32} -eq 0 ]]; then
        runPrototype "None" "INF" "32"
        exitStatus_None_INF_32=${exitStatus}
      else
        skipRunPrototype "None" "INF" "32"
      fi

      if [[ ${exitStatus_None_INF_32} -eq 0 ]] && [[ ${exitStatus_Fast_2_32} -eq 0 ]]; then
        runPrototype "Fast" "INF" "32"
        exitStatus_Fast_INF_32=${exitStatus}
      else
        skipRunPrototype "Fast" "INF" "32"
      fi

      if [[ ${exitStatus_Fast_INF_32} -eq 0 ]] && [[ ${exitStatus_Canonical_2_32} -eq 0 ]]; then
        runPrototype "Canonical" "INF" "32"
        exitStatus_Canonical_INF_32=${exitStatus}
      else
        skipRunPrototype "Canonical" "INF" "32"
      fi

      exitStatus_None_INF_INF=999
      exitStatus_Fast_INF_INF=999

      if [[ ${exitStatus_None_INF_32} -eq 0 ]]; then
        runPrototype "None" "INF" "INF"
        exitStatus_None_INF_INF=${exitStatus}
      else
        skipRunPrototype "None" "INF" "INF"
      fi

      if [[ ${exitStatus_None_INF_INF} -eq 0 ]] && [[ ${exitStatus_Fast_INF_32} -eq 0 ]]; then
        runPrototype "Fast" "INF" "INF"
        exitStatus_Fast_INF_INF=${exitStatus}
      else
        skipRunPrototype "Fast" "INF" "INF"
      fi

      if [[ ${exitStatus_Fast_INF_INF} -eq 0 ]] && [[ ${exitStatus_Canonical_INF_32} -eq 0 ]]; then
        runPrototype "Canonical" "INF" "INF"
      else
        skipRunPrototype "Canonical" "INF" "INF"
      fi

    fi

  fi

done
