savings(Account) ~ gaussian(4254.6,2297792.71111) := true.
freq(Account) ~ finite([Probability:high,Probability2:low]) := savings(Account)~=X1_M, logistic([0.029202652089567977, -138.90846623285694],[X1_M],Probability2), Probability is 1.0-Probability2.
freq(Account) ~ finite([0.6:high,0.4:low]) := true.
age(Client) ~ gaussian(24.6,13.3) := findall_forward(X2_M,(hasAcc(Client,Account_M)~=true,freq(Account_M)~=X2_M),X_T_3),minMod(X_T_3)~=low.
age(Client) ~ gaussian(43.6666666667,145.466666667) := findall_forward(X2_M,(hasAcc(Client,Account_M)~=true,freq(Account_M)~=X2_M),X_T_4),minMod(X_T_4)~=high.
age(Client) ~ gaussian(35.0,177.2) := true.
creditScore(Client) ~ gaussian(Mean,6361.365719523376) := age(Client)~=X4_M,findall_forward(X2_M,(hasAcc(Client,Account_M)~=true,freq(Account_M)~=X2_M),X_T_12),minMod(X_T_12)~=high,getMean([X4_M],[10.417048579285058,138.45554537121916],Mean).
creditScore(Client) ~ gaussian(497.0,33740.3333333) := true.
loanAmt(Loan) ~ gaussian(Mean,168.58985572517446) := findall_forward(X1_M,(hasLoan(Account_M,Loan)~=true,savings(Account_M)~=X1_M),X_T_1_Temp),avg(X_T_1_Temp)~=X_T_1,findall_forward(X2_M,(hasLoan(Account_M_1,Loan)~=true,freq(Account_M_1)~=X2_M),X_T_10),minMod(X_T_10)~=high,getMean([X_T_1],[9.960409336974106,86.87358176716589],Mean).
loanAmt(Loan) ~ gaussian(52000.0,558666666.667) := findall_forward(X1_M,(hasLoan(Account_M,Loan)~=true,savings(Account_M)~=X1_M),X_T_2),\+avg(X_T_2)~=_.
loanAmt(Loan) ~ gaussian(46812.5,382829166.667) := true.
status(Loan) ~ finite([Probability1:appr,Probability2:decl,Probability3:pend]) := findall_forward(X1_M,(hasLoan(Account_M,Loan)~=true,savings(Account_M)~=X1_M),X_T_1_Temp),avg(X_T_1_Temp)~=X_T_1, softmax([[-0.059928311835396424,212.7584889008575],[0.05575380542759868,-216.21477724047696],[0.004174506330970806,3.4562883396198543]],[X_T_1],[Probability1,Probability2,Probability3]).
status(Loan) ~ finite([0.42857142857142855:decl,0.42857142857142855:pend,0.14285714285714285:appr]) := findall_forward(X1_M,(hasLoan(Account_M,Loan)~=true,savings(Account_M)~=X1_M),X_T_2),\+avg(X_T_2)~=_.
status(Loan) ~ finite([0.4375:decl,0.3125:pend,0.25:appr]) := true.
