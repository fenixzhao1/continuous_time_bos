***** Full dataset *****
* open dataset
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos.dta", clear

* generate interaction terms
gen continuous_BoS1_4 = continuous * BoS1_4
gen continuous_BoS10 = continuous * BoS10
gen hybrid_BoS1_4 = hybrid * BoS1_4
gen hybrid_BoS10 = hybrid * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen block2_continuous = block2 * continuous
gen block2_hybrid = block2 * hybrid

gen sequence2 = 0
replace sequence2 = 1 if sequence == 2
gen sequence2_continuous = sequence2 * continuous
gen sequence2_hybrid = sequence2 * hybrid

* encode session_round_pair_id
encode session_round_pair_id, gen (individual_id)

* logit regression for treatment effect
logit coordinate continuous BoS1_4 BoS10 continuous_BoS1_4 continuous_BoS10 ///
      sequence2 sequence2_continuous block2 block2_continuous, cluster(individual_id) 
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)


* random effect logit regression
xtset individual_id tick
xtlogit coordinate continuous BoS1_4 BoS10 continuous_BoS1_4 continuous_BoS10 ///
        sequence2 sequence2_continuous block2 block2_continuous, re vce(robust) 
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)



***** Pair-level Analysis *****
** coordinate rate summary
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair_full.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if time == "Continuous"
gen hybrid = 0
replace hybrid = 1 if time == "Hybrid"
gen BoS1_4 = 0
replace BoS1_4 = 1 if game == "BoS1.4"
gen BoS10 = 0
replace BoS10 = 1 if game == "BoS10"

gen continuous_BoS1_4 = continuous * BoS1_4
gen continuous_BoS10 = continuous * BoS10
gen hybrid_BoS1_4 = hybrid * BoS1_4
gen hybrid_BoS10 = hybrid * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen block2_continuous = block2 * continuous
gen block2_hybrid = block2 * hybrid

gen sequence2 = 0
replace sequence2 = 1 if sequence == 2
gen sequence2_continuous = sequence2 * continuous
gen sequence2_hybrid = sequence2 * hybrid

* logit regression for treatment effect
reg coordinate continuous hybrid BoS1_4 BoS10 ///
    continuous_BoS1_4 continuous_BoS10 hybrid_BoS1_4 hybrid_BoS10 ///
    sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)


** classification second half data
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair_half.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if time == "Continuous"
gen hybrid = 0
replace hybrid = 1 if time == "Hybrid"
gen BoS1_4 = 0
replace BoS1_4 = 1 if game == "BoS1.4"
gen BoS10 = 0
replace BoS10 = 1 if game == "BoS10"

gen continuous_BoS1_4 = continuous * BoS1_4
gen continuous_BoS10 = continuous * BoS10
gen hybrid_BoS1_4 = hybrid * BoS1_4
gen hybrid_BoS10 = hybrid * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen block2_continuous = block2 * continuous
gen block2_hybrid = block2 * hybrid

gen sequence2 = 0
replace sequence2 = 1 if sequence == 2
gen sequence2_continuous = sequence2 * continuous
gen sequence2_hybrid = sequence2 * hybrid

* logit regressions
logit alternating continuous hybrid BoS1_4 BoS10 ///
    continuous_BoS1_4 continuous_BoS10 hybrid_BoS1_4 hybrid_BoS10 ///
    sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
logit one_ne continuous hybrid BoS1_4 BoS10 ///
    continuous_BoS1_4 continuous_BoS10 hybrid_BoS1_4 hybrid_BoS10 ///
    sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)


** classfication full data
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair_full.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if time == "Continuous"
gen hybrid = 0
replace hybrid = 1 if time == "Hybrid"
gen BoS1_4 = 0
replace BoS1_4 = 1 if game == "BoS1.4"
gen BoS10 = 0
replace BoS10 = 1 if game == "BoS10"

gen continuous_BoS1_4 = continuous * BoS1_4
gen continuous_BoS10 = continuous * BoS10
gen hybrid_BoS1_4 = hybrid * BoS1_4
gen hybrid_BoS10 = hybrid * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen block2_continuous = block2 * continuous
gen block2_hybrid = block2 * hybrid

gen sequence2 = 0
replace sequence2 = 1 if sequence == 2
gen sequence2_continuous = sequence2 * continuous
gen sequence2_hybrid = sequence2 * hybrid

* logit regressions
logit alternating continuous hybrid BoS1_4 BoS10 ///
    continuous_BoS1_4 continuous_BoS10 hybrid_BoS1_4 hybrid_BoS10 ///
    sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
logit one_ne continuous hybrid BoS1_4 BoS10 ///
    continuous_BoS1_4 continuous_BoS10 hybrid_BoS1_4 hybrid_BoS10 ///
    sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)



***** Pair-level Analysis with only time treatments *****
** coordinate rate summary
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair_full.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if time == "Continuous"
gen hybrid = 0
replace hybrid = 1 if time == "Hybrid"

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen sequence2 = 0
replace sequence2 = 1 if sequence == 2

* logit regression for treatment effect
reg coordinate continuous hybrid sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)


** classification second half data
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair_half.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if time == "Continuous"
gen hybrid = 0
replace hybrid = 1 if time == "Hybrid"

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen sequence2 = 0
replace sequence2 = 1 if sequence == 2

* logit regressions
logit alternating continuous hybrid sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
logit one_ne continuous hybrid sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)


** classfication full data
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair_full.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if time == "Continuous"
gen hybrid = 0
replace hybrid = 1 if time == "Hybrid"

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen sequence2 = 0
replace sequence2 = 1 if sequence == 2

* logit regressions
logit alternating continuous hybrid sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
logit one_ne continuous hybrid sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)



***** Pair-level Analysis (continuous as baseline) *****
** coordinate rate summary
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair_full.dta", clear

* generate treatment dummies
gen discrete = 0
replace discrete = 1 if time == "Discrete"
gen hybrid = 0
replace hybrid = 1 if time == "Hybrid"
gen BoS1_4 = 0
replace BoS1_4 = 1 if game == "BoS1.4"
gen BoS10 = 0
replace BoS10 = 1 if game == "BoS10"

gen discrete_BoS1_4 = discrete * BoS1_4
gen discrete_BoS10 = discrete * BoS10
gen hybrid_BoS1_4 = hybrid * BoS1_4
gen hybrid_BoS10 = hybrid * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen block2_discrete = block2 * discrete
gen block2_hybrid = block2 * hybrid

gen sequence2 = 0
replace sequence2 = 1 if sequence == 2
gen sequence2_discrete = sequence2 * discrete
gen sequence2_hybrid = sequence2 * hybrid

* logit regression for treatment effect
reg coordinate discrete hybrid BoS1_4 BoS10 ///
    discrete_BoS1_4 discrete_BoS10 hybrid_BoS1_4 hybrid_BoS10 ///
    sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)


** classification second half data
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair_half.dta", clear

* generate treatment dummies
gen discrete = 0
replace discrete = 1 if time == "Discrete"
gen hybrid = 0
replace hybrid = 1 if time == "Hybrid"
gen BoS1_4 = 0
replace BoS1_4 = 1 if game == "BoS1.4"
gen BoS10 = 0
replace BoS10 = 1 if game == "BoS10"

gen discrete_BoS1_4 = discrete * BoS1_4
gen discrete_BoS10 = discrete * BoS10
gen hybrid_BoS1_4 = hybrid * BoS1_4
gen hybrid_BoS10 = hybrid * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen block2_discrete = block2 * discrete
gen block2_hybrid = block2 * hybrid

gen sequence2 = 0
replace sequence2 = 1 if sequence == 2
gen sequence2_discrete = sequence2 * discrete
gen sequence2_hybrid = sequence2 * hybrid

* logit regressions
logit alternating discrete hybrid BoS1_4 BoS10 ///
    discrete_BoS1_4 discrete_BoS10 hybrid_BoS1_4 hybrid_BoS10 ///
    sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
logit one_ne discrete hybrid BoS1_4 BoS10 ///
    discrete_BoS1_4 discrete_BoS10 hybrid_BoS1_4 hybrid_BoS10 ///
    sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)


** classfication full data
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair_full.dta", clear

* generate treatment dummies
gen discrete = 0
replace discrete = 1 if time == "Discrete"
gen hybrid = 0
replace hybrid = 1 if time == "Hybrid"
gen BoS1_4 = 0
replace BoS1_4 = 1 if game == "BoS1.4"
gen BoS10 = 0
replace BoS10 = 1 if game == "BoS10"

gen discrete_BoS1_4 = discrete * BoS1_4
gen discrete_BoS10 = discrete * BoS10
gen hybrid_BoS1_4 = hybrid * BoS1_4
gen hybrid_BoS10 = hybrid * BoS10

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen block2_discrete = block2 * discrete
gen block2_hybrid = block2 * hybrid

gen sequence2 = 0
replace sequence2 = 1 if sequence == 2
gen sequence2_discrete = sequence2 * discrete
gen sequence2_hybrid = sequence2 * hybrid

* logit regressions
logit alternating discrete hybrid BoS1_4 BoS10 ///
    discrete_BoS1_4 discrete_BoS10 hybrid_BoS1_4 hybrid_BoS10 ///
    sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
logit one_ne discrete hybrid BoS1_4 BoS10 ///
    discrete_BoS1_4 discrete_BoS10 hybrid_BoS1_4 hybrid_BoS10 ///
    sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)



***** Pair-level Analysis with only time treatments (continuous as baseline) *****
** coordinate rate summary
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair_full.dta", clear

* generate treatment dummies
gen discrete = 0
replace discrete = 1 if time == "Discrete"
gen hybrid = 0
replace hybrid = 1 if time == "Hybrid"

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen sequence2 = 0
replace sequence2 = 1 if sequence == 2

* logit regression for treatment effect
reg coordinate discrete hybrid sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se replace nolabel bdec(3)


** classification second half data
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair_half.dta", clear

* generate treatment dummies
gen discrete = 0
replace discrete = 1 if time == "Discrete"
gen hybrid = 0
replace hybrid = 1 if time == "Hybrid"

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen sequence2 = 0
replace sequence2 = 1 if sequence == 2

* logit regressions
logit alternating discrete hybrid sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
logit one_ne discrete hybrid sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)


** classfication full data
use "D:\Dropbox\Working Papers\Continuous Time BOS\data\stata_bos_pair_full.dta", clear

* generate treatment dummies
gen discrete = 0
replace discrete = 1 if time == "Discrete"
gen hybrid = 0
replace hybrid = 1 if time == "Hybrid"

* generate block2 and sequence 2 dummies
gen block2 = 0
replace block2 = 1 if block == 2
gen sequence2 = 0
replace sequence2 = 1 if sequence == 2

* logit regressions
logit alternating discrete hybrid sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
logit one_ne discrete hybrid sequence2 block2, cluster(session)
outreg2 using D:\Dropbox\stata_table, tex nonote se append nolabel bdec(3)
