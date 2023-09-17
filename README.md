# local_adaptation

This is the repository for data and code derived from the work, "Ecological conditions and cryptic host specialization within Poecilochirus carabi mites together cause variation in the extent of co-adaptation with their Nicrophorus vespilloides burying beetle hosts".

Authors: Syuan-Jyun Sun1,2,* and Rebecca M. Kilner1
Affiliations:
1Department of Zoology, University of Cambridge, Downing Street, Cambridge, CB2 3EJ, UK
2International Degree Program in Climate Change and Sustainable Development, National Taiwan University, Taipei 10617, Taiwan

Corresponding author: Syuan-Jyun Sun; sjs243@ntu.edu.tw 

This work investigates how local communities of host burying beetles Nicrophorus spp. have harboured different cryptic species of phoretic mites, and tests for evidence of local adaptation between populations. It consists of four dataset, based on three experiments.

Metadata:
1. first_choice:
   miteorigin: population of mite
   bl: trial id
   year: year of experiment was done
   sex: sex of beetles
   sp: species of beetles
   origin: population of individual beetles
   bodysize: beetle body size, measured as pronotum width (in mm)
   num: number of mites attached to the beetle (according to sp)
   vesnum: number of mites attached to N. vespilloides
   hunum: number of mites attached to N. humator
   invnum: number of mites attached to N. investigator
   intnum: number of mites attached to N. interruptus
   sum: total number of mites that made a decision
   vesp: proportion of mites attached to N. vespilloides
   hup: proportion of mites attached to N. humator
   invp: proportion of mites attached to N. investigator
   intp: proportion of mites attached to N. interruptus
   ptotal: proportion of mites that made a decision
2. second_choice:
   miteorigin: population of mite
   sp: species on which mites made a decision
   year: year of experiment was done
   bl: trial id
   sex: sex of beetles
   origin: population of individual beetles
   bodysize: beetle body size, measured as pronotum width (in mm)
   Species: species of beetles
   num: number of mites attached to the beetle (according to Species)
   hunum: number of mites attached to N. humator
   invnum: number of mites attached to N. investigator
   intnum: number of mites attached to N. interruptus
   sum: total number of mites that made a decision
   p: proportion of mites attached to focal beetles (according to Species)
   vesp: proportion of mites attached to N. vespilloides
   hup: proportion of mites attached to N. humator
   invp: proportion of mites attached to N. investigator
   intp: proportion of mites attached to N. interruptus
   newcomp: whether or not mites derived from the same beetle specie (0: different; 1: same)
   totalp: proportion of mites that made a decision
3. g_mite:
   tr: mite treatment (control: no mites; mix: a mixture of mites; ves: purified ves mites)
   wt: carcass mass
   mfamily: family of male beetles
   ffamily: family of female beetles
   mmite: number of mites on male beetles
   fmite: number of mites on female beetles
   mitenum: mmite+fmite
   broodsize: number of beetle larvae
   broodmass: total brood mass
   larvaldensity: broodsize divided by wt
   yield: broodmass divided by broodsize
4. local_adaptation_data:
   wt: carcass mass
   bl: block of experiments
   beetle: beetle origin (G: Gamlingay; W: Waresley; C: no beetle)
   mite: mite origin (G: Gamlingay; W: Waresley; C: no beetle)
   msize: size of male beetles
   fsize: size of female beetles
   mmite: number of mites on male beetles
   fmite: number of mites on female beetles
   mitenum: mmite+fmite
   logmitenum: log (mitenum)
   broodsize: number of beetle larvae
   broodmass: total brood mass
   yield: broodmass divided by broodsize
   density: broodsize divided by wt
   clutchsize: clutch size


 
