%%% -*- Mode: Prolog; -*-

:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').
:- use_module('../distributionalclause.pl').
:- use_module(library(lists)).

:- set_options(default).
:- set_inference(backward(lazy)).
:- initialization(init).

builtin(pPoisson(_)).
builtin(nAnchors(_)).
builtin(visionfield(_,_)).
builtin(minDist(_)).
builtin(dimbbox(_,_,_)).
builtin(colorList(_)).
builtin(rgbcolor(_,_)).
builtin(actionList(_)).
builtin(neuralPredList(_)).
builtin(deviation(_)).

%number of samples
nSamp(30).

%deviation
deviation(0.1).

%dimensions
visionfield(x,(0.0,120)).
visionfield(y,(0.0,60)).
visionfield(z,(0.0,50)).

%bounding box
dimbbox(x,2.0,5.0).
dimbbox(y,2.0,5.0).
dimbbox(z,2.0,5.0).

%minimal distance
minDist(5).

%color
colorList([white,gray,black,magenta,red,brown,orange,yellow,green,cyan,blue,purple,pink]).
rgbcolor(white,(1.0,1.0,1.0)).
rgbcolor(gray,(0.3,0.3,0.3)).
rgbcolor(black,(0.0,0.0,0.0)).
rgbcolor(magenta,(1.0,0.0,1.0)).
rgbcolor(red,(1.0,0.0,0.0)).
rgbcolor(brown,(0.55,0.27,0.7)).
rgbcolor(orange,(1.0,0.5,0.0)).
rgbcolor(yellow,(1.0,1.0,0.0)).
rgbcolor(green,(0.0,1.0,0.0)).
rgbcolor(cyan,(0.0,1.0,1.0)).
rgbcolor(blue,(0.0,0.0,1.0)).
rgbcolor(purple,(0.5,0.0,0.5)).
rgbcolor(pink,(1.0,0.0,0.8)).

%number of anchors
nanchors ~ val(4) := true.

%actions
actionList([move_left_of, move_right_of, move_behind_of, move_infront_of]).
neuralPredList([left_of, right_of, behind_of, infront_of]).

%displacement
disp:t+1 ~ gaussian(10,0.1) := true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%generate objects and actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
col(A_ID):t+1 ~ uniform(CL) := \+col(A_ID):t~=_, nanchors~=N,  between(1,N,A_ID), colorList(CL).
col(A_ID):t+1 ~ val(Col) := col(A_ID):t~=Col.


bbox(A_ID,C):t+1 ~ contUniform(L,H) := \+bbox(A_ID,C):t ~=_,  nanchors~=N, between(1,N,A_ID), dimbbox(C,L,H).
bbox(A_ID,C):t+1 ~ val(BB) := bbox(A_ID,C):t ~= BB.


anchorS_rel:t+1 ~ uniform(AList) :=  nanchors~=Num, Num1 is Num - 1, findall_forward(N, between(1,Num1,N), AList).
neuralPredAnchorS_rel:t+1 ~ val(Val) :=  anchorS_rel:t+1~=A_ID_rel, Val is A_ID_rel+1.
anchorS:t+1 ~ uniform(AList) :=  nanchors~=Num, neuralPredAnchorS_rel:t+1~=N_A_ID_rel, anchorS_rel:t+1~=A_ID_rel, findall_forward(A_ID, (between(1,Num,A_ID), \+A_ID == N_A_ID_rel, \+A_ID == A_ID_rel), AList).

actionS:t+1 ~ uniform(ActionList) := actionList(ActionList).
neuralPredS:t+1 ~ uniform(NeuralPredList) := neuralPredList(NeuralPredList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%position transition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%initial
pos(A_ID,z):t+1 ~ val(Z) :=
	\+pos(A_ID,z):t ~=_,
	nanchors~=N,
	between(1,N,A_ID),
	bbox(A_ID,z)~=BBZ,
	Z is BBZ/2.

pos(N_A_ID_rel,C):t+1 ~ contUniform(L,H) :=
	\+pos(N_A_ID_rel,C):t~=_,
	nanchors~=N,
	between(1,N,N_A_ID_rel),
	visionfield(C,(L,H)),
	anchorS_rel:t+1 ~= ID,
	ID == N_A_ID_rel.

%infront
pos(N_A_ID_rel,x):t+1 ~ gaussian(NewX,Dev) :=
	\+pos(N_A_ID_rel,x):t~=_,
	deviation(Dev),
	neuralPredAnchorS_rel:t+1 ~= N_A_ID_rel,
	anchorS_rel:t+1 ~= A_ID_rel,
	pos(A_ID_rel,x):t+1 ~=_,
	neuralPredS:t+1 ~= infront_of,
	pos(A_ID_rel,x):t+1 ~= X_rel,
	disp:t+1 ~= Displacement,
	NewX is X_rel+Displacement.
pos(N_A_ID_rel,y):t+1 ~ gaussian(Y_rel,Dev) :=
	\+pos(N_A_ID_rel,y):t~=_,
	deviation(Dev),
	neuralPredAnchorS_rel:t+1 ~= N_A_ID_rel,
	anchorS_rel:t+1 ~= A_ID_rel,
	pos(A_ID_rel,y):t+1 ~=_,
	neuralPredS:t+1 ~= infront_of,
	pos(A_ID_rel,y):t+1 ~= Y_rel.

%behind
pos(N_A_ID_rel,x):t+1 ~ gaussian(NewX,Dev) :=
	\+pos(N_A_ID_rel,x):t~=_,
	deviation(Dev),
	neuralPredAnchorS_rel:t+1 ~= N_A_ID_rel,
	anchorS_rel:t+1 ~= A_ID_rel,
	pos(A_ID_rel,x):t+1 ~=_,
	neuralPredS:t+1 ~= behind_of,
	pos(A_ID_rel,x):t+1 ~= X_rel,
	disp:t+1 ~= Displacement,
	NewX is X_rel-Displacement.
pos(N_A_ID_rel,y):t+1 ~ gaussian(Y_rel,Dev) :=
	\+pos(N_A_ID_rel,y):t~=_,
	deviation(Dev),
	neuralPredAnchorS_rel:t+1 ~= N_A_ID_rel,
	anchorS_rel:t+1 ~= A_ID_rel,
	pos(A_ID_rel,y):t+1 ~=_,	
	neuralPredS:t+1 ~= behind_of,
	pos(A_ID_rel,y):t+1 ~= Y_rel.

%left
pos(N_A_ID_rel,y):t+1 ~ gaussian(NewY,Dev) :=
	\+pos(N_A_ID_rel,y):t~=_,
	deviation(Dev),
	neuralPredAnchorS_rel:t+1 ~= N_A_ID_rel,
	anchorS_rel:t+1 ~= A_ID_rel,	
	pos(A_ID_rel,x):t+1 ~=_,
	neuralPredS:t+1 ~= left_of,
	pos(A_ID_rel,y):t+1 ~= Y_rel,
	disp:t+1 ~= Displacement,
	NewY is Y_rel-Displacement.
pos(N_A_ID_rel,x):t+1 ~ gaussian(X_rel,Dev) := 
	\+pos(N_A_ID_rel,x):t~=_,
	deviation(Dev),
	neuralPredAnchorS_rel:t+1 ~= N_A_ID_rel,
	anchorS_rel:t+1 ~= A_ID_rel,
	pos(A_ID_rel,y):t+1 ~=_,	
	neuralPredS:t+1 ~= left_of,
	pos(A_ID_rel,x):t+1 ~= X_rel.

%right
pos(N_A_ID_rel,y):t+1 ~ gaussian(NewY,Dev) :=
	\+pos(N_A_ID_rel,y):t~=_,
	deviation(Dev),
	neuralPredAnchorS_rel:t+1 ~= N_A_ID_rel,
	anchorS_rel:t+1 ~= A_ID_rel,	
	pos(A_ID_rel,x):t+1 ~=_,
	neuralPredS:t+1 ~= right_of,
	pos(A_ID_rel,y):t+1 ~= Y_rel,
	disp:t+1 ~= Displacement,
	NewY is Y_rel+Displacement.
pos(N_A_ID_rel,x):t+1 ~ gaussian(X_rel,Dev) := 
	\+pos(N_A_ID_rel,x):t~=_,
	deviation(Dev),
	neuralPredAnchorS_rel:t+1 ~= N_A_ID_rel,
	anchorS_rel:t+1 ~= A_ID_rel,
	pos(A_ID_rel,y):t+1 ~=_,
	neuralPredS:t+1 ~= right_of,
	pos(A_ID_rel,x):t+1 ~= X_rel.

pos(N_A_ID_rel,C):t+1 ~ contUniform(L,H) :=
	\+pos(N_A_ID_rel,C):t~=_,
	nanchors~=N,
	between(1,N,N_A_ID_rel),
	visionfield(C,(L,H)),
	anchorS_rel:t+1 ~= A_ID_rel,
	neuralPredAnchorS_rel:t+1 ~= ID,
	anchorS:t+1 ~= ID1,
	actionS:t+1 ~= Act1,
	neuralPredS:t+1 ~= Pred.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%infront
pos(A_ID,x):t+1 ~ gaussian(NewX,Dev) :=
	neuralPredS:t ~= infront_of,
	deviation(Dev),
	anchorS:t ~= A_ID,
	neuralPredAnchorS_rel:t ~= N_A_ID_rel,
	actionS:t ~= move_infront_of,
	pos(N_A_ID_rel,x):t ~= X_rel,
	disp:t+1 ~= Displacement,
	NewX is X_rel+Displacement.
pos(A_ID,y):t+1 ~ gaussian(Y_rel,Dev) := 
	neuralPredS:t ~= infront_of,
	deviation(Dev),
	anchorS:t ~= A_ID,
	neuralPredAnchorS_rel:t ~= N_A_ID_rel,
	actionS:t ~= move_infront_of,
	pos(N_A_ID_rel,y):t ~= Y_rel.
pos(A_ID,x):t+1 ~ gaussian(NewX,Dev) :=
	anchorS:t ~= A_ID,
	deviation(Dev),
	anchorS_rel:t ~= A_ID_rel,
	actionS:t ~= move_infront_of,
	pos(A_ID_rel,x):t ~= X_rel,
	disp:t+1 ~= Displacement,
	NewX is X_rel+Displacement.
pos(A_ID,y):t+1 ~ gaussian(Y_rel,Dev) := 
	anchorS:t ~= A_ID,
	deviation(Dev),
	anchorS_rel:t ~= A_ID_rel,
	actionS:t ~= move_infront_of,
	pos(A_ID_rel,y):t ~= Y_rel.

%behind
pos(A_ID,x):t+1 ~ gaussian(NewX,Dev) :=
	neuralPredS:t ~= behind_of,
	deviation(Dev),
	anchorS:t ~= A_ID,
	neuralPredAnchorS_rel:t ~= N_A_ID_rel,
	actionS:t ~= move_behind_of,
	pos(N_A_ID_rel,x):t ~= X_rel,
	disp:t+1 ~= Displacement,
	NewX is X_rel-Displacement.
pos(A_ID,y):t+1 ~ gaussian(Y_rel,Dev) :=
	neuralPredS:t ~= behind_of, 
	deviation(Dev),
	anchorS:t ~= A_ID,
	neuralPredAnchorS_rel:t ~= N_A_ID_rel,
	actionS:t ~= move_behind_of,
	pos(N_A_ID_rel,y):t ~= Y_rel.
pos(A_ID,x):t+1 ~ gaussian(NewX,Dev) :=
	anchorS:t ~= A_ID,
	deviation(Dev),
	anchorS_rel:t ~= A_ID_rel,
	actionS:t ~= move_behind_of,
	pos(A_ID_rel,x):t ~= X_rel,
	disp:t+1 ~= Displacement,
	NewX is X_rel-Displacement.
pos(A_ID,y):t+1 ~ gaussian(Y_rel,Dev) := 
	anchorS:t ~= A_ID,
	deviation(Dev),
	anchorS_rel:t ~= A_ID_rel,
	actionS:t ~= move_behind_of,
	pos(A_ID_rel,y):t ~= Y_rel.

%left
pos(A_ID,y):t+1 ~ gaussian(NewY,Dev) :=
	neuralPredS:t ~= left_of, 
	deviation(Dev),
	anchorS:t ~= A_ID,
	neuralPredAnchorS_rel:t ~= N_A_ID_rel,
	actionS:t ~= move_left_of,
	pos(N_A_ID_rel,y):t ~= Y_rel,
	disp:t+1 ~= Displacement,
	NewY is Y_rel-Displacement.
pos(A_ID,x):t+1 ~ gaussian(X_rel,Dev) := 
	neuralPredS:t ~= left_of,
	deviation(Dev),
	anchorS:t ~= A_ID,
	neuralPredAnchorS_rel:t ~= N_A_ID_rel,
	actionS:t ~= move_left_of,
	pos(N_A_ID_rel,x):t ~= X_rel.
pos(A_ID,y):t+1 ~ gaussian(NewY,Dev) :=
	anchorS:t ~= A_ID,
	deviation(Dev),
	anchorS_rel:t ~= A_ID_rel,
	actionS:t ~= move_left_of,
	pos(A_ID_rel,y):t ~= Y_rel,
	disp:t+1 ~= Displacement,
	NewY is Y_rel-Displacement.
pos(A_ID,x):t+1 ~ gaussian(X_rel,Dev) := 
	anchorS:t ~= A_ID,
	deviation(Dev),
	anchorS_rel:t ~= A_ID_rel,
	actionS:t ~= move_left_of,
	pos(A_ID_rel,x):t ~= X_rel.

%right
pos(A_ID,y):t+1 ~ gaussian(NewY,Dev) :=
	neuralPredS:t ~= right_of,
	deviation(Dev),
	anchorS:t ~= A_ID,
	neuralPredAnchorS_rel:t ~= N_A_ID_rel,
	actionS:t ~= move_right_of,
	pos(N_A_ID_rel,y):t ~= Y_rel,
	disp:t+1 ~= Displacement,
	NewY is Y_rel+Displacement.
pos(A_ID,x):t+1 ~ gaussian(X_rel,Dev) := 
	neuralPredS:t ~= right_of,
	deviation(Dev),
	anchorS:t ~= A_ID,
	neuralPredAnchorS_rel:t ~= N_A_ID_rel,
	actionS:t ~= move_right_of,
	pos(N_A_ID_rel,x):t ~= X_rel.
pos(A_ID,y):t+1 ~ gaussian(NewY,Dev) :=
	anchorS:t ~= A_ID,
	deviation(Dev),
	anchorS_rel:t ~= A_ID_rel,
	actionS:t ~= move_right_of,
	pos(A_ID_rel,y):t ~= Y_rel,
	disp:t+1 ~= Displacement,
	NewY is Y_rel+Displacement.
pos(A_ID,x):t+1 ~ gaussian(X_rel,Dev) := 
	anchorS:t ~= A_ID,
	deviation(Dev),
	anchorS_rel:t ~= A_ID_rel,
	actionS:t ~= move_right_of,
	pos(A_ID_rel,x):t ~= X_rel.

%no action
pos(A_ID,C):t+1 ~ val(X) :=
	pos(A_ID,C):t ~= X.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dist(A_ID1,A_ID2):t+1 ~ val(Dist) :=
	pos(A_ID1,x):t+1 ~= X1,
	pos(A_ID1,y):t+1 ~= Y1,
	pos(A_ID1,z):t+1 ~= Z1,
	pos(A_ID2,x):t+1 ~= X2,
	pos(A_ID2,y):t+1 ~= Y2,
	pos(A_ID2,z):t+1 ~= Z2,
	Dist is sqrt((X1-X2)**2+(Y1-Y2)**2+(Z1-Z2)**2).

distances((A_ID1,A_ID2)):t+1 ~ val(Dist) :=
	nanchors ~= N,
	between(1,N,A_ID1),
	A_IDh is A_ID1+1,
	between(A_IDh,N,A_ID2),
	dist(A_ID1,A_ID2):t+1 ~= Dist.	

distPass:t+1 :=
	minDist(MD),
	findall_forward(A_ID,(distances(A_ID):t+1 ~= Dist, Dist<MD),AList),
	AList==[].

limits(A_ID):t+1 := 
	nanchors ~= N,
	between(1,N,A_ID),
        pos(A_ID,x):t+1~=X,
        pos(A_ID,y):t+1~=Y,
        pos(A_ID,z):t+1~=Z,
        visionfield(x, (Lx,Hx)),
        visionfield(y, (Ly,Hy)),
        visionfield(z, (Lz,Hz)),
	Lx<X,
	Hx>X,
	Ly<Y,
	Hy>Y,	
	Lz<Z,
	Hz>Z.

limitsPass:t+1 := 
        findall_forward(A_ID, (limits(A_ID):t+1), AList),
	nanchors ~= N,
        length(AList, L),
	N==L.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%output predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
position(A_ID):t+1 ~ val((X,Y,Z)) :=
	distPass:t+1,
	limitsPass:t+1,
	pos(A_ID,x):t+1~=X,
	pos(A_ID,y):t+1~=Y,
	pos(A_ID,z):t+1~=Z.

color(A_ID):t+1 ~ val(C) :=
	col(A_ID):t+1~=C.


actionS_rel:t+1 ~ val((Action,A_ID,A_ID_rel,NeuralPred,N_A_ID_rel)) :=
	actionS:t ~= Action,
	anchorS:t ~= A_ID,
	anchorS_rel:t ~= A_ID_rel,
	neuralPredS:t ~= NeuralPred,
	neuralPredAnchorS_rel:t ~= N_A_ID_rel. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%data fetching
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
q	:-
	nSamp(NSamp),
	init_particle(NSamp),
	step_particle([],[], NSamp, 1.0),
	getData(NSamp).

search_query(I,Q) :-
	eraseall(tempparticle),
	abolish_all_tables,
	distributionalclause:proof_query_backward_lazy(I,tempparticle,Q).

getData(NSamp) :-
	dcpf:bb_get(offset,Offset),
	open('data.txt','write', S),
	(
	between(1,NSamp,SampID),
	I is Offset+SampID,
	search_query(I,(
		findall_forward(
			[A_ID, XYZ_c, Col_c],
			(current(position(A_ID)) ~= XYZ_c, current(color(A_ID)) ~= Col_c),
			State1)
		)
	),
	
	\+State1==[],
	append([SampID], State1, SampS1),
	search_query(I,(next(actionS_rel)~=(Action_pred, A_ID_action, A_ID_action_rel, NeuralPred, N_A_ID_rel))),
	Action = [Action_pred, A_ID_action, A_ID_action_rel, NeuralPred, N_A_ID_rel],

	search_query(I,(
		findall_forward(
			[A_ID, XYZ_n, Col_n],
			(next(position(A_ID)) ~= XYZ_n, next(color(A_ID)) ~= Col_n),
			State2)
		)
	),
	
	\+State2==[],
	append([SampID], State2, SampS2),
	write(S, SampS1), nl(S),
	write(S, Action), nl(S),
	write(S, SampS2), nl(S),
	fail;
	true
	),
	nl(S), close(S).


