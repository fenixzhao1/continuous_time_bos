***** Full dataset  *****
* open dataset
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos.dta", clear

* generate interaction terms
gen continuous_BoS1_4 = continuous * BoS1_4
gen continuous_BoS10 = continuous * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen block2_continuous = block2 * continuous

gen sequence2 = 0
replace sequence2 = 1 if sequence == 2
gen sequence2_continuous = sequence2 * continuous

* encode session_round_pair_id
encode session_round_pair_id, gen (individual_id)

* logit regression for treatment effect
logit coordinate continuous BoS1_4 BoS10 continuous_BoS1_4 continuous_BoS10 ///
      sequence2 sequence2_continuous block2 block2_continuous, cluster(individual_id) 
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)

* t-test with clustered se
reg coordinate continuous if BoS1_4 == 1, cluster(individual_id)
reg coordinate continuous if BoS2_5 == 1, cluster(individual_id)
reg coordinate continuous if BoS10 == 1, cluster(individual_id)
reg coordinate BoS1_4 BoS10 if continuous == 1, cluster(individual_id)
reg coordinate BoS1_4 BoS10 if continuous == 0, cluster(individual_id)

reg coordinate sequence2 if treatment == "Continuous_BoS10", cluster(individual_id)
reg coordinate sequence2 if treatment == "Continuous_BoS2.5", cluster(individual_id)
reg coordinate sequence2 if treatment == "Continuous_BoS1.4", cluster(individual_id)
reg coordinate sequence2 if treatment == "Discrete_BoS10", cluster(individual_id)
reg coordinate sequence2 if treatment == "Discrete_BoS2.5", cluster(individual_id)
reg coordinate sequence2 if treatment == "Discrete_BoS1.4", cluster(individual_id)

reg coordinate block2 if treatment == "Continuous_BoS10", cluster(individual_id)
reg coordinate block2 if treatment == "Continuous_BoS2.5", cluster(individual_id)
reg coordinate block2 if treatment == "Continuous_BoS1.4", cluster(individual_id)
reg coordinate block2 if treatment == "Discrete_BoS10", cluster(individual_id)
reg coordinate block2 if treatment == "Discrete_BoS2.5", cluster(individual_id)
reg coordinate block2 if treatment == "Discrete_BoS1.4", cluster(individual_id)

* random effect logit regression
xtset individual_id tick
xtlogit coordinate continuous BoS1_4 BoS10 continuous_BoS1_4 continuous_BoS10 ///
        sequence2 sequence2_continuous block2 block2_continuous, re vce(robust) 
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)


***** Pair-level summary*****
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair3.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if time == "Continuous"
gen BoS1_4 = 0
replace BoS1_4 = 1 if game == "BoS1.4"
gen BoS10 = 0
replace BoS10 = 1 if game == "BoS10"

gen continuous_BoS1_4 = continuous * BoS1_4
gen continuous_BoS10 = continuous * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen block2_continuous = block2 * continuous

gen sequence2 = 0
replace sequence2 = 1 if sequence == 2
gen sequence2_continuous = sequence2 * continuous

* logit regression for treatment effect
reg coordinate continuous BoS1_4 BoS10 continuous_BoS1_4 continuous_BoS10 ///
    sequence2 sequence2_continuous block2 block2_continuous
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)



***** Pair-level classification *****
* second half data
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if time == "Continuous"
gen BoS1_4 = 0
replace BoS1_4 = 1 if game == "BoS1.4"
gen BoS10 = 0
replace BoS10 = 1 if game == "BoS10"

gen continuous_BoS1_4 = continuous * BoS1_4
gen continuous_BoS10 = continuous * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen block2_continuous = block2 * continuous

gen sequence2 = 0
replace sequence2 = 1 if sequence == 2
gen sequence2_continuous = sequence2 * continuous

* logit regressions
logit alternating continuous BoS1_4 BoS10 continuous_BoS1_4 continuous_BoS10 ///
      sequence2 block2, vce(robust)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
logit one_ne continuous BoS1_4 BoS10 continuous_BoS1_4 continuous_BoS10 ///
      sequence2 block2, vce(robust)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

* t-test with clustered se
reg alternating continuous if game == "BoS10"
reg alternating continuous if game == "BoS2.5"
reg alternating continuous if game == "BoS1.4"
reg one_ne continuous if game == "BoS10"
reg one_ne continuous if game == "BoS2.5"
reg one_ne continuous if game == "BoS1.4"

* full data
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair2.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if time == "Continuous"
gen BoS1_4 = 0
replace BoS1_4 = 1 if game == "BoS1.4"
gen BoS10 = 0
replace BoS10 = 1 if game == "BoS10"

gen continuous_BoS1_4 = continuous * BoS1_4
gen continuous_BoS10 = continuous * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen block2_continuous = block2 * continuous

gen sequence2 = 0
replace sequence2 = 1 if sequence == 2
gen sequence2_continuous = sequence2 * continuous

* logit regressions
logit alternating continuous BoS1_4 BoS10 continuous_BoS1_4 continuous_BoS10 ///
      sequence2 block2, vce(robust)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
logit one_ne continuous BoS1_4 BoS10 continuous_BoS1_4 continuous_BoS10 ///
      sequence2 block2, vce(robust)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)

* t-test with clustered se
reg alternating continuous if game == "BoS10"
reg alternating continuous if game == "BoS2.5"
reg alternating continuous if game == "BoS1.4"
reg one_ne continuous if game == "BoS10"
reg one_ne continuous if game == "BoS2.5"
reg one_ne continuous if game == "BoS1.4"



***** Full dataset - only C and D *****
* open dataset
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos.dta", clear

* generate interaction terms
gen continuous_BoS1_4 = continuous * BoS1_4
gen continuous_BoS10 = continuous * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen sequence2 = 0
replace sequence2 = 1 if sequence == 2

* encode session_round_pair_id
encode session_round_pair_id, gen (individual_id)

* t-test with clustered se
reg coordinate continuous, cluster(individual_id)
reg coordinate sequence2 if time == "Continuous", cluster(individual_id)
reg coordinate sequence2 if time == "Discrete", cluster(individual_id)
reg coordinate block2 if time == "Continuous", cluster(individual_id)
reg coordinate block2 if time == "Discrete", cluster(individual_id)



***** Pair-level dataset - only C and D *****
* second half data
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if time == "Continuous"
gen BoS1_4 = 0
replace BoS1_4 = 1 if game == "BoS1.4"
gen BoS10 = 0
replace BoS10 = 1 if game == "BoS10"

gen continuous_BoS1_4 = continuous * BoS1_4
gen continuous_BoS10 = continuous * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen sequence2 = 0
replace sequence2 = 1 if sequence == 2

* t-test with clustered se
reg alternating continuous
reg one_ne continuous

* full data
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair2.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if time == "Continuous"
gen BoS1_4 = 0
replace BoS1_4 = 1 if game == "BoS1.4"
gen BoS10 = 0
replace BoS10 = 1 if game == "BoS10"

gen continuous_BoS1_4 = continuous * BoS1_4
gen continuous_BoS10 = continuous * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen sequence2 = 0
replace sequence2 = 1 if sequence == 2

* t-test with clustered se
reg alternating continuous
reg one_ne continuous
