% Areege Chaudhary
% 10197607

%true if there is a fact that contains the month, day and word
match(Month,Day,Word) :- 
	weather(Month,Day,Word,_,_) ;
	weather(Month,Day,_,Word,_) ;
	weather(Month,Day,_,_,Word).

%day had either rain or snow
wet(Month,Day) :- 
	match(Month,Day,rain) ; 
	match(Month,Day,snow).

%day was hot and dry
desert(Month,Day) :- 
	match(Month,Day,hot) , match(Month,Day,dry).

%day was hot and had rain
tropical(Month,Day) :- 
	match(Month,Day,hot) , match(Month,Day,rain).

%day was hot or warm but had snow
impossible(Month,Day) :- 
	match(Month,Day,hot) , match(Month,Day,snow) ;
	match(Month,Day,warm) , match(Month,Day,snow).

%month is jun, jul or aug
summer(Month) :-
	Month = jun ;
	Month = jul ; 
	Month = aug.

%month is jan, feb or mar
winter(Month) :-
	Month = jan ;
	Month = feb ; 
	Month = mar.

%warm or hot day in the winter or cool or cold day in the summer	
unseasonable(Month, Day) :-
	summer(Month), match(Month,Day,cool) ;
	summer(Month), match(Month,Day,cold) ;
	winter(Month), match(Month,Day,warm) ;
	winter(Month), match(Month,Day,hot).

%there's at least one hot day and one cold day in the month
mixed(Month) :-
	match(Month,_,hot) ,
	match(Month,_,cold).



