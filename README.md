This will become a group of calculators designed to calculate changes in net and gross pay for Resident doctors and Consultants across the UK. It is intended for these calculators to take pensions, student loans and pay premia into account.

As of yet, the only calculator that has been mostly finished is the English resident doctors calculator (2016 contract).

[Live instance of the public version (1.1.8) Resident Doctors calculator - ](https://virenb.shinyapps.io/Resident_Doctors_pay_uplift_calculator_DDRB_25-26/)

[Live instance of most current testing version (1.1.8) - ](https://virenb.shinyapps.io/testPayCalc/)

To do:

1) account for locum pay without accidentally uplifting it - this is harder than it sounds because how do you account for a user who includes it in their gross vs separately
2) Add scottish and northern irish pay (may exclude scotland an NI from live public versions due to confusing pay circular regarding GPST pay - once clarity is attained, this will be resolved)
3) Update calcs so NROC allowance for LTFT users is considered separtely from LTFT weekend allowance calc (see below for details about the issue)
4) check Scot/NI pension schemes
5) verify scot tax calc
6) Check that GP trainees have access to the same F5-9 Banding as non GPSTs in Wales/Scot/NI (assumption of this was made in v1.1.6)
7) Make consultant calculators.

Issues: Scottish taxes seem to be universally hard to calculate - I have checked 3 other websites including gov.uk, a building society, and one other doctor pay calculator, none of us agree on scottish taxes, and gov.uk is the one that agrees the least.

Other: as far as i can tell NROC allowance is calculated as 8% of basic salary, but what's unclear is how this plays in with LTFT. NHS employers has a "ready reckoner" for LTFT weekend and NROC (for 23/24 - although pay nodes have increased in England as of May 2025 , the terms for calculating NROC and weekend allowances have not changed) - it assumes that NROC allowance is pro-rata by the same percentage as weekend allowance. I think it's possible to have a different on-call comittment to weekend comittment, but I have used the assumption in the "ready reckoner" regardless.
    https://leademployer.merseywestlancs.nhs.uk/media/Documents/Employee%20Hub/How%20to%20read%20your%20Work%20Schedule%202.pdf this link implies NROC and weekend adjustments for LTFT acan be different - I will try to implement this for v1.1.8

The explanations for how to calculate allowances and such can be found here - https://www.nhsemployers.org/system/files/2023-02/NHS-Doctors-and-Dentists-in-Training-England-TCS-2016-VERSION-11.pdf

Further ideas:
Make a pension projection tool for doctors. If done, I can't imagine designing anything that accounts for switching schemes.
Make a student loan repayment calculator that is more sepcific for doctors (likley easier than other public facing calculators due to the semi-predicatble nature of our pay) - examples of other public calculators - https://www.studentloanrepaymentcalc.co.uk/, https://www.student-loan-calculator.co.uk/

Unlikely ideas:
extending any of the above to AFC contracts is unlikely, but not infeasible
