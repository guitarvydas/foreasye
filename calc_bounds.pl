:- initialization(main).

:- dynamic(arrow/1).
:- dynamic(arrow_x/2).
:- dynamic(arrow_y/2).
:- dynamic(bounding_box_bottom/2).
:- dynamic(bounding_box_left/2).
:- dynamic(bounding_box_right/2).
:- dynamic(bounding_box_top/2).
:- dynamic(eltype/2).
:- dynamic(geometry_h/2).
:- dynamic(geometry_w/2).
:- dynamic(geometry_left_x/2).
:- dynamic(geometry_center_x/2).
:- dynamic(geometry_center_y/2).
:- dynamic(geometry_top_y/2).
:- dynamic(line_begin_x/2).
:- dynamic(line_begin_y/2).
:- dynamic(line_end_x/2).
:- dynamic(line_end_y/2).
:- dynamic(source/2).
:- dynamic(source_x/2).
:- dynamic(source_y/2).
:- dynamic(sink/2).
:- dynamic(sink_x/2).
:- dynamic(sink_y/2).
:- dynamic(text/2).
:- dynamic(text_x/2).
:- dynamic(text_y/2).
:- dynamic(text_w/2).
:- dynamic(text_h/2).

:- dynamic(line/1).
:- dynamic(source/1).
:- dynamic(sink/1).
:- dynamic(speechbubble/1).
:- dynamic(component/1).
:- dynamic(edge/1).
:- dynamic(ellipse/1).

:- dynamic(log/1).
:- dynamic(log/2).
:- dynamic(log/3).
:- dynamic(log/4).
:- dynamic(log/5).
:- dynamic(log/6).
:- dynamic(log/7).
:- dynamic(log/8).

main :-
    readFactBase(user_input), 
    createBoundingBoxes,
    writeFactBase,
    halt.

createBoundingBoxes :-
    conditionalCreateEllipseBB,
    condRect,
    condSpeech,
    condText.

condRect :-
    forall(rect(ID), createRectBoundingBox(ID)).
condRect :-
    true.

condSpeech :-
    forall(speechbubble(ID), createRectBoundingBox(ID)).
condSpeech :-
    true.

condText :-
    forall(text(ID,_), createTextBoundingBox(ID)).
condText :-
    true.

conditionalCreateEllipseBB:-
    ellipse(_),
    forall(ellipse(ID), createEllipseBoundingBox(ID)).

conditionalCreateEllipseBB :- % for pre-ellipse code  
    true.

createRectBoundingBox(ID) :-
    geometry_left_x(ID,X),
    geometry_top_y(ID, Y),
    geometry_w(ID, Width),
    geometry_h(ID, Height),
    asserta(bounding_box_left(ID,X)),
    asserta(bounding_box_top(ID,Y)),
    Right is X + Width,
    Bottom is Y + Height,
    asserta(bounding_box_right(ID,Right)),
    asserta(bounding_box_bottom(ID,Bottom)).

createTextBoundingBox(ID) :-
    geometry_center_x(ID,CX),
    geometry_top_y(ID, Y),
    geometry_w(ID, HalfWidth),
    geometry_h(ID, Height),
    X is (CX - HalfWidth),
    asserta(bounding_box_left(ID,X)),
    asserta(bounding_box_top(ID,Y)),
    Right is CX + HalfWidth,
    Bottom is Y + Height,
    asserta(bounding_box_right(ID,Right)),
    asserta(bounding_box_bottom(ID,Bottom)).

createEllipseBoundingBox(ID) :-
    geometry_center_x(ID,CX),
    geometry_center_y(ID,CY),
    geometry_w(ID,HalfWidth),
    geometry_h(ID,HalfHeight),
    Left is CX - HalfWidth,
    Top is CY - HalfHeight,
    asserta(bounding_box_left(ID,Left)),
    asserta(bounding_box_top(ID,Top)),
    Right is CX + HalfWidth,
    Bottom is CY + HalfHeight,
    asserta(bounding_box_right(ID,Right)),
    asserta(bounding_box_bottom(ID,Bottom)).




%%% boilerplate

writeterm(Term) :- current_output(Out), write_term(Out, Term, []), write(Out, '.'), nl.


writeFactBase :-
    forall(line(X),writeTerm(line(X))),
    forall(sink(X),writeTerm(sink(X))),
    forall(source(X),writeTerm(source(X))),
    forall(speechbubble(X),writeTerm(speechbubble(X))),
    forall(component(X),writeTerm(component(X))),
    forall(edge(X),writeTerm(edge(X))),
    forall(ellipse(X),writeTerm(ellipse(X))),

    forall(bounding_box_bottom(X,Y),writeTerm(bounding_box_bottom(X,Y))),
    forall(bounding_box_left(X,Y),writeTerm(bounding_box_left(X,Y))),
    forall(bounding_box_right(X,Y),writeTerm(bounding_box_right(X,Y))),
    forall(bounding_box_top(X,Y),writeTerm(bounding_box_top(X,Y))),
    forall(eltype(X,Y),writeTerm(eltype(X,Y))),
    forall(geometry_h(X,Y),writeTerm(geometry_h(X,Y))),
    forall(geometry_w(X,Y),writeTerm(geometry_w(X,Y))),
    forall(geometry_left_x(X,Y),writeTerm(geometry_left_x(X,Y))),
    forall(geometry_center_x(X,Y),writeTerm(geometry_center_x(X,Y))),
    forall(geometry_center_y(X,Y),writeTerm(geometry_center_y(X,Y))),
    forall(geometry_top_y(X,Y),writeTerm(geometry_top_y(X,Y))),
    forall(line_begin_x(X,Y),writeTerm(line_begin_x(X,Y))),
    forall(line_begin_y(X,Y),writeTerm(line_begin_y(X,Y))),
    forall(line_end_x(X,Y),writeTerm(line_end_x(X,Y))),
    forall(line_end_y(X,Y),writeTerm(line_end_y(X,Y))),
    forall(source(X,Y),writeTerm(source(X,Y))),
    forall(source_x(X,Y),writeTerm(source_x(X,Y))),
    forall(source_y(X,Y),writeTerm(source_y(X,Y))),
    forall(sink(X,Y),writeTerm(sink(X,Y))),
    forall(sink_x(X,Y),writeTerm(sink_x(X,Y))),
    forall(sink_y(X,Y),writeTerm(sink_y(X,Y))),
    forall(text(X,Y),writeTerm(text(X,Y))),
    forall(text_x(X,Y),writeTerm(text_x(X,Y))),
    forall(text_y(X,Y),writeTerm(text_y(X,Y))),
    forall(text_w(X,Y),writeTerm(text_w(X,Y))),
    forall(text_h(X,Y),writeTerm(text_h(X,Y))),

    writelog.

writelog :-
    forall(log(X),writelog(X)),
    forall(log(Z,Y),writelog(Z,Y)),
    forall(log(A,B,C),writelog(A,B,C)),
    forall(log(D,E,F,G),writelog(D,E,F,G)),
    forall(log(H,I,J,K,L),writelog(H,I,J,K,L)),
    forall(log(M,N,O,P,Q,R),writelog(M,N,O,P,Q,R)),
    forall(log(S,T,U,V,W,X,Y),writelog(S,T,U,V,W,X,Y)),
    forall(log(R,S,T,U,V,W,X,Y),writelog(R,S,T,U,V,W,X,Y)).

writelog(X) :- writeterm(log(X)).
writelog(Y,Z) :-writeterm(log(Y,Z)).
writelog(X,Y,Z) :-writeterm(log(X,Y,Z)).
writelog(W,X,Y,Z) :-writeterm(log(W,X,Y,Z)).
writelog(A,B,C,D,E) :-writeterm(log(A,B,C,D,E)).
writelog(A,B,C,D,E,F) :-writeterm(log(A,B,C,D,E,F)).
writelog(A,B,C,D,E,F,G) :-writeterm(log(A,B,C,D,E,F,G)).
writelog(A,B,C,D,E,F,G,H) :-writeterm(log(A,B,C,D,E,F,G,H)).


wspc :-
    write(user_error,' ').

nle :- nl(user_error).

we(X) :- write(user_error,X).

wen(X):- we(X),nle.

readFactBase(Stream) :-
    read_term(Stream,T0,[]),
    element(T0,Stream).

element(end_of_file, _) :- !.
element(bounding_box_bottom(X,Y), Stream) :- !,asserta(bounding_box_bottom(X,Y)),readFactBase(Stream).
element(bounding_box_left(X,Y), Stream) :- !,asserta(bounding_box_left(X,Y)),readFactBase(Stream).
element(bounding_box_right(X,Y), Stream) :- !,asserta(bounding_box_right(X,Y)),readFactBase(Stream).
element(bounding_box_top(X,Y), Stream) :- !,asserta(bounding_box_top(X,Y)),readFactBase(Stream).
element(eltype(X,Y), Stream) :- !,asserta(eltype(X,Y)),readFactBase(Stream).
element(geometry_h(X,Y), Stream) :- !,asserta(geometry_h(X,Y)),readFactBase(Stream).
element(geometry_w(X,Y), Stream) :- !,asserta(geometry_w(X,Y)),readFactBase(Stream).
element(geometry_left_x(X,Y), Stream) :- !,asserta(geometry_left_x(X,Y)),readFactBase(Stream).
element(geometry_center_x(X,Y), Stream) :- !,asserta(geometry_center_x(X,Y)),readFactBase(Stream).
element(geometry_center_y(X,Y), Stream) :- !,asserta(geometry_center_y(X,Y)),readFactBase(Stream).
element(geometry_top_y(X,Y), Stream) :- !,asserta(geometry_top_y(X,Y)),readFactBase(Stream).
element(line_begin_x(X,Y), Stream) :- !,asserta(line_begin_x(X,Y)),readFactBase(Stream).
element(line_begin_y(X,Y), Stream) :- !,asserta(line_begin_y(X,Y)),readFactBase(Stream).
element(line_end_x(X,Y), Stream) :- !,asserta(line_end_x(X,Y)),readFactBase(Stream).
element(line_end_y(X,Y), Stream) :- !,asserta(line_end_y(X,Y)),readFactBase(Stream).
element(source(X,Y), Stream) :- !,asserta(source(X,Y)),readFactBase(Stream).
element(source_x(X,Y), Stream) :- !,asserta(source_x(X,Y)),readFactBase(Stream).
element(source_y(X,Y), Stream) :- !,asserta(source_y(X,Y)),readFactBase(Stream).
element(sink(X,Y), Stream) :- !,asserta(sink(X,Y)),readFactBase(Stream).
element(sink_x(X,Y), Stream) :- !,asserta(sink_x(X,Y)),readFactBase(Stream).
element(sink_y(X,Y), Stream) :- !,asserta(sink_y(X,Y)),readFactBase(Stream).
element(text(X,Y), Stream) :- !,asserta(text(X,Y)),readFactBase(Stream).
element(text_x(X,Y), Stream) :- !,asserta(text_x(X,Y)),readFactBase(Stream).
element(text_y(X,Y), Stream) :- !,asserta(text_y(X,Y)),readFactBase(Stream).
element(text_w(X,Y), Stream) :- !,asserta(text_w(X,Y)),readFactBase(Stream).
element(text_h(X,Y), Stream) :- !,asserta(text_h(X,Y)),readFactBase(Stream).

element(line(X), Stream) :- !,asserta(line(X)),readFactBase(Stream).
element(source(X), Stream) :- !,asserta(source(X)),readFactBase(Stream).
element(speechbubble(X), Stream) :- !,asserta(speechbubble(X)),readFactBase(Stream).
element(component(X), Stream) :- !,asserta(component(X)),readFactBase(Stream).
element(edge(X), Stream) :- !,asserta(edge(X)),readFactBase(Stream).
element(ellipse(X), Stream) :- !,asserta(ellipse(X)),readFactBase(Stream).

element(log(W), Stream) :- !,
			     asserta(log(W)),
			     readFactBase(Stream).

element(log(W,X), Stream) :- !,
			     asserta(log(W,X)),
			     readFactBase(Stream).

element(log(W,X,Y), Stream) :- !,
			     asserta(log(W,X,Y)),
			     readFactBase(Stream).

element(log(W,X,Y,Z), Stream) :- !,
			     asserta(log(W,X,Y,Z)),
			     readFactBase(Stream).

element(log(A,W,X,Y,Z), Stream) :- !,
			     asserta(log(A,W,X,Y,Z)),
			     readFactBase(Stream).

element(log(A,B,W,X,Y,Z), Stream) :- !,
			     asserta(log(A,B,W,X,Y,Z)),
			     readFactBase(Stream).

element(log(A,B,C,W,X,Y,Z), Stream) :- !,
			     asserta(log(A,B,C,W,X,Y,Z)),
			     readFactBase(Stream).

element(log(A,B,C,D,W,X,Y,Z), Stream) :- !,
			     asserta(log(A,B,C,D,W,X,Y,Z)),
			     readFactBase(Stream).


    
element(Term, _) :-
    write(user_error,'failed read '),
    write(user_error,Term),
    nl(user_error).
    % type_error(element, Term).

inc(Var, Value) :-
    g_read(Var, Value),
    X is Value+1,
    g_assign(Var, X).

boundingboxCompletelyInside(ID1,ID2) :-
    bounding_box_left(ID1,L1),
    bounding_box_top(ID1,T1),
    bounding_box_right(ID1,R1),
    bounding_box_bottom(ID1,B1),

    bounding_box_left(ID2,L2),
    bounding_box_top(ID2,T2),
    bounding_box_right(ID2,R2),
    bounding_box_bottom(ID2,B2),

    L1 >= L2,
    T1 >= T2,
    R2 >= R1,
    B2 >= B1.

pointCompletelyInsideBoundingBox(ID1,ID2) :-
    bounding_box_left(ID1,L1),
    bounding_box_top(ID1,T1),

    bounding_box_left(ID2,L2),
    bounding_box_top(ID2,T2),
    bounding_box_right(ID2,R2),
    bounding_box_bottom(ID2,B2),

    % we('point inside: L1/T1/L2/T2/R2/B2: '),we(L1),wspc,we(T1),wspc,we(L2),wspc,we(T2),wspc,we(R2),wspc,wen(B2),

    L1 >= L2,
    T1 >= T2,
    R2 >= L1,
    B2 >= T1.

centerCompletelyInsideBoundingBox(ID1,ID2) :-
    bounding_box_left(ID1,L1),
    bounding_box_top(ID1,T1),
    bounding_box_right(ID1,R1),
    bounding_box_bottom(ID1,B1),
    
    Cx is L1 + (R1 - L1),
    Cy is T1 + (B1 - T1),

    bounding_box_left(ID2,L2),
    bounding_box_top(ID2,T2),
    bounding_box_right(ID2,R2),
    bounding_box_bottom(ID2,B2),

    %% we('ccibb id1/center/id2 '),
    %% we(ID1), wspc,
    %% we(L1), wspc,
    %% we(T1), wspc,
    %% we(R1), wspc,
    %% we(B1), wspc,
    %% we(Cx), wspc,
    %% we(Cy), wspc,
    %% we(ID2), wspc,
    %% we(L2), wspc,
    %% we(T2), wspc,
    %% we(R2), wspc,
    %% wen(B2),

    Cx >= L2,
    Cx =< R2,
    Cy >= T2,
    Cy =< B2.

dumplog :-
    forall(log(X),dumplog(X)),
    forall(log(Z,Y),dumplog(Z,Y)),
    forall(log(A,B,C),dumplog(A,B,C)),
    forall(log(D,E,F,G),dumplog(D,E,F,G)),
    forall(log(H,I,J,K,L),dumplog(H,I,J,K,L)),
    forall(log(M,N,O,P,Q,R),dumplog(M,N,O,P,Q,R)),
    forall(log(M1,N1,O1,P1,Q1,R1,S1),dumplog(M1,N1,O1,P1,Q1,R1,S1)),
    forall(log(M2,N2,O2,P2,Q2,R2,S2,T2),dumplog(M2,N2,O2,P2,Q2,R2,S2,T2)).

dumplog(W) :- wen(W).
dumplog(W,X) :- we(W),wspc,wen(X).
dumplog(W,X,Y) :- we(W),wspc,we(X),wspc,wen(Y).
dumplog(W,X,Y,Z) :- we(W),wspc,we(X),wspc,we(Y),wspc,wen(Z).
dumplog(V,W,X,Y,Z) :- we(V),wspc,we(W),wspc,we(X),wspc,we(Y),wspc,wen(Z).
dumplog(U,V,W,X,Y,Z) :- we(U),wspc,we(V),wspc,we(W),wspc,we(X),wspc,we(Y),wspc,wen(Z).
dumplog(T,U,V,W,X,Y,Z) :- we(T),wspc,we(U),wspc,we(V),wspc,we(W),wspc,we(X),wspc,we(Y),wspc,wen(Z).
dumplog(S,T,U,V,W,X,Y,Z) :- we(S),wspc,we(T),wspc,we(U),wspc,we(V),wspc,we(W),wspc,we(X),wspc,we(Y),wspc,wen(Z).
