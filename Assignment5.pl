% Areege Chaudhary
% 10197607

sublistSum(List1, Sum, List2):-
	sublist(List1, List2),
	listSum(List2, Sum).

sublist(_, []).
sublist([Head|Tail1], [Head|Tail2]):-
	sublist(Tail1, Tail2).
sublist([_|Tail1], [Head2|Tail2]):-
	sublist(Tail1, [Head2|Tail2]).

listSum([], 0).
listSum([Head|Tail], LSum):-
	listSum(Tail, TailSum),
	LSum is Head+TailSum.

matching(List1, List2, Result):-
	matchHelper(List1, List2, 0, Result).

matchHelper([],_,_,[]).
matchHelper(_,[],_,[]).

matchHelper([X|Xs],[Y|Ys],Count,[Count|Rest]):-
	X == Y,
	NewCount is Count+1,
	matchHelper(Xs, Ys, NewCount, Rest).

matchHelper([X|Xs],[Y|Ys],Count,Rest):-
	X \= Y,
	NewCount is Count+1,
	matchHelper(Xs,Ys,NewCount,Rest).

legalCourse([]).
legalCourse([Name1+Name2+_|Xs]) :- 
	not(member(Name1+Name1+_,[Name1+Name2+_|Xs])), 
	not(member(Name1+Name2+_,Xs)),
	not(member(Name2+Name1+_,Xs)),
	legalCourse(Xs).

studentCount(_,[],0).
studentCount(Name,[Name1+Name2+_|Xs],Count) :-
	(Name = Name1 ; Name = Name2),
	studentCount(Name,Xs,TailCount),
	Count is TailCount+1.
studentCount(Name,[Name1+Name2+_|Xs],TailCount) :-
	Name \= Name1,
	Name \= Name2,
	studentCount(Name,Xs,TailCount).

partners(_,[],[]).
partners(Name, [Name1+Name2+_|Xs], [Name2|Result]):-
	Name = Name1,
	partners(Name, Xs, Result).
partners(Name, [Name1+Name2+_|Xs], [Name1|Result]):-
	Name = Name2,
	partners(Name, Xs, Result).
partners(Name, [Name1+Name2+_|Xs], Result):-
	Name \= Name1,
	Name \= Name2,
	partners(Name, Xs, Result).

courseAvg([],0).
courseAvg([_+_+Mark|Xs],Number) :-
	sumMarks([_+_+Mark|Xs],Sum),
	length([_+_+Mark|Xs],L),
	Number is Sum/L.

sumMarks([],0).
sumMarks([_+_+Mark|Xs], Sum) :-
	sumMarks(Xs,SumTail),
	Sum is Mark+SumTail.

studentAvg(_,[],0).
studentAvg(Name, CourseList, Avg) :-
	studentSum(Name, CourseList, Sum),
	sLen(Name, CourseList, L),
	Avg is Sum/L.

studentSum(_,[],0).
studentSum(Name, [Name1+Name2+Mark|Xs], Sum) :-
	(Name = Name1; Name = Name2),
	studentSum(Name, Xs, TailSum),
	Sum is TailSum + Mark.

studentSum(Name, [Name1+Name2+_|Xs], Sum) :-
	Name \= Name1,
	Name \= Name2,
	studentSum(Name, Xs, Sum).

sLen(_,[],0).
sLen(Name, [Name1+Name2+_|Xs], L) :-
     (Name = Name1 ; Name = Name2),
     sLen(Name, Xs, TailL),
     L is TailL+1.
     
sLen(Name, [Name1+Name2+_|Xs], L) :-
     Name \= Name1 ,
     Name \= Name2 ,
     sLen(Name, Xs, L).

students([Name1+Name2+_|[]], [Name1, Name2]).
students([Name1+Name2+_|Xs], [Name1,Name2|Result]):-
	students(Xs, Result),
	not(member(Name1, Result)),
	not(member(Name2, Result)).

students([Name1+_+_|Xs], [Name1|Result]):-
	students(Xs, Result),
	not(member(Name1, Result)).
	
students([_+Name1+_|Xs], [Name1|Result]):-
	students(Xs, Result),
	not(member(Name1, Result)).