savings(Account) ~ gaussian(3030.75,651688.916667) := true.
freq(Account) ~ finite([0.6:high,0.4:low]) := true.
age(Client) ~ gaussian(42.6666666667,126.333333333) := true.
creditScore(Client) ~ gaussian(550.0,17500.0) := true.
loanAmt(Loan) ~ gaussian(18350.0,58417500.0) := true.
status(Loan) ~ finite([0.3333333333333333:decl,0.3333333333333333:pend,0.3333333333333333:appr]) := true.
