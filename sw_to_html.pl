%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A simple script to create a visual report of PRISM parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 
% usage: sw_to_html(PRISM_SWITCH_FILE, HTML_OUTPUT_FILE).

:- table int_to_hex/2.
:- table stats/2.

sw_to_html(ParamsFile,HTMLFile) :-
	create_grouped_report(ParamsFile,HTMLFile).

create_report(ParamsFile, HTMLFile) :-
	terms_from_file(ParamsFile,Params),
	findall(Table,(member(Param,Params),parameter_table(Param,Table)),Tables),
	% Write the HTML to file
	Header = head(title([text('PRISM parameter view:'),text(ParamsFile)])),
	html_write_file(HTMLFile, html([Header,body([h2(ParamsFile),Tables])])).

switch_count_report(Params,Report) :-
	length(Params,ParamsLen),
	findall(L,(member(Sw,Params),switch_outcomes(Sw,O),length(O,L)),OLs),
	sumlist(OLs,NumOutcomes),
	Report = [p([text('number of switches: '),text(ParamsLen)]),
		  p([text('number of outcomes: '),text(NumOutcomes)])].

create_grouped_report(ParamsFile, HTMLFile) :-
	terms_from_file(ParamsFile,Params),
	group_by_functor(Params,ParamGroups),!,
	write('grouped parameters with same functor'),nl,
	findall(Table,(member(PG,ParamGroups),parameter_table(PG,Table)),Tables),!,
	switch_count_report(Params,SwCountReport),
	% Write the HTML to file
	Header = head(title(['PRISM parameter view:',ParamsFile])),
	Overview = [h1(text('PRISM parameter vizualizer')),
			h3([text('file: '), text(ParamsFile)])],
	html_write_file(HTMLFile, html([Header,Overview,SwCountReport,body(Tables)])).	
	
report_groups([]).
report_groups([G|GRest]) :-
	write('GROUP:'),
	forall(member(S,G), (switch_name(S,N),write(N),write(','))),nl,
	report_groups(GRest).
	
group_by_functor(SwitchTerms,Groups) :-
	findall(N,(member(T,SwitchTerms),switch_name(T,N)),Names),
	findall(F,(member(N,Names),functor(N,F,_)),Functors),
	eliminate_duplicate(Functors,UniqFunctors),
	findall(G,(member(F,UniqFunctors),group_members(F,SwitchTerms,G)),Groups).

group_members(F,Terms,Group) :-
	findall(T,(member(T,Terms),switch_name(T,N),functor(N,F,_)), Group).
	
parameter_table(SwitchTerm,Table) :-
	SwitchTerm = switch(SwName,_Fixed,OutComes,Probabilities),
	write('Creating single parameter table for switch: '), write(SwName),nl,
	Table = table('border=1',[th([text('Switch: '),text(SwName)]),tr(OutcomeRow),tr(ProbRow)]),
	stats(Probabilities,Stats),
	findall(TD,(member(O,OutComes),TD=td(b(text(O)))),OutcomeRow),
	findall(TD,(member(P,Probabilities),probability_table_entry(Stats,P,TD)),ProbRow).

parameter_table([SwitchTerm],Table) :-
	!,
	parameter_table(SwitchTerm,Table).

parameter_table(SwitchTerms,Table) :-
	SwitchTerms = [First|_],
	switch_functor(First,SwitchFunctor),	
	write('Creating multi parameter table for switches with functor: '), write(SwitchFunctor),nl,
	determine_outcomes(SwitchTerms,AllOutcomes),!,
	findall(O,(member(T,SwitchTerms),switch_probabilities(T,O)),AllProbsNest),
	flatten(AllProbsNest,AllProbs),
	stats(AllProbs,Stats),
	findall(TD,(member(O,AllOutcomes),TD=td(b(text(O)))),OutcomeRow),!,
	write('created outcome row.'),nl,
	findall(Row,(member(ST,SwitchTerms),switch_name(ST,Name),switch_probability_row(Stats,AllOutcomes,ST,Row1),Row=tr([td(text(Name))|Row1])),ProbRows),!,
	write('created probability row'),nl,
	Table = table('border=1',[th([text('Switches: '),text(SwitchFunctor),text('(_)')]),OutcomeRow,ProbRows]).

determine_outcomes(SwitchTerms,UniqueOutcomes) :-
	findall(O,(member(T,SwitchTerms),switch_outcomes(T,O)),OAll),
	flatten_once(OAll,FlatOutcomes),
	eliminate_duplicate(FlatOutcomes,UniqueOutcomes).
	
switch_outcomes(switch(_,_,Outcomes,_), Outcomes).
switch_probabilities(switch(_,_,_,Probs),Probs).
switch_name(switch(SwName,_,_,_), SwName).
switch_functor(switch(SwName,_,_,_), F) :- functor(SwName,F,_).
	
switch_probability_row(_,[],_SwitchTerm,[]).

switch_probability_row([Mean,Low,High],[O|Os],Switch,[RowEntry|RowRest]) :-
	Switch = switch(_SwName,_Fixed,Outcomes,Probabilities),
	nth(N,Outcomes,O),!,
	nth(N,Probabilities,P),!,
	probability_table_entry([Mean,Low,High],P,RowEntry), !,
	switch_probability_row([Mean,Low,High],Os,Switch,RowRest).
	
switch_probability_row([Mean,Low,High],[_|Os],Switch,[td(text(' '))|RowRest]) :-
	switch_probability_row([Mean,Low,High],Os,Switch,RowRest).

probability_table_entry([Mean,Low,High],P,Entry) :-
	probability_html_color([Mean,Low,High],P,ColorCode),!,
	atom_concat_list(['bgcolor="',ColorCode,'" '],ColorAttr),
	Entry = td(ColorAttr, text(P)).

probability_html_color([Mean,_Low,High],Probability,ColorCode) :-
	Probability > Mean,
	Diff is Probability - Mean, % Diff is the interval between the 
	HighInterval is High - Mean,
	DiffNormal is Diff / HighInterval,
	ByteEnc is round(DiffNormal * 255),
	HalfByteEnc is ByteEnc // 2,
	HalfGreenByte is 127 + HalfByteEnc,
	RedByte is 127 - HalfByteEnc,
	int_to_hex(HalfGreenByte,HexEnc),
	int_to_hex(RedByte,RedByteHex),
	pad_hex(2,HexEnc,HexEncPad),
	pad_hex(2,RedByteHex,RedByteHexPad),
	atom_concat_list(['#', RedByteHexPad, HexEncPad, '00' ], ColorCode).

probability_html_color([Mean,Low,_High],Probability,ColorCode) :-
	Probability =< Mean,
	Diff is abs(Mean - Probability),
	LowInterval is abs(Low - Mean),
	((LowInterval > 0) ->
		DiffNormal is Diff / LowInterval,
		ByteEnc is round(DiffNormal * 255),
		HalfByteEnc is ByteEnc // 2,
		HalfRedByte is 127 + HalfByteEnc,
		GreenByte is 127 - HalfByteEnc,
		int_to_hex(GreenByte,GreenHex),
		int_to_hex(HalfRedByte,HexEnc),
		pad_hex(2,HexEnc,HexEncPad),
		pad_hex(2,GreenHex,GreenHexPad),
		atom_concat_list(['#', HexEncPad, GreenHexPad, '00' ], ColorCode)
		;
		atom_concat_list(['#', '00', 'FF', '00' ], ColorCode)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hexadecimal number conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pad_hex(N,Hex,Hex) :-
	atom_codes(Hex,HexCodes),
	length(HexCodes,N),
	!.
	
pad_hex(N,Hex,PaddedHex) :-
	N1 is N - 1,
	pad_hex(N1,Hex,PaddedHexRest),
	atom_concat('0',PaddedHexRest,PaddedHex).
	
int_to_hex(Int,Hex) :-
	find_digits(16,Int,NumDigits),!,
	int_to_hex_rec(NumDigits,Int,HexDigits),!,
	atom_concat_list(HexDigits,Hex).

int_to_hex_rec(1,Int,[HexDigit]) :-
	Digit is Int mod 16,
	hex_digit(Digit,HexDigit).
	
int_to_hex_rec(DigitIndex,Int,[HexDigit|HexDigitRest]) :-
	DigitIndex > 1,
	NextDigitIndex is DigitIndex - 1,
	power(16,NextDigitIndex,Divisor),
	Digit is Int // Divisor,
	hex_digit(Digit,HexDigit),
	NextInt is Int - (Digit*Divisor),
	int_to_hex_rec(NextDigitIndex,NextInt,HexDigitRest).
	
	
find_digits(Base,Number,1) :-
	0 is Number // Base.
find_digits(Base,Number,Digits) :-
	NumTimes is Number // Base,
	find_digits(Base,NumTimes,DigitsRest),
	Digits is 1 + DigitsRest.

power(_,0,1).
power(Base,1,Base).
power(Base,Exponent,Result) :-
	Exponent1 is Exponent - 1,
	power(Base,Exponent1,Res1),
	Result is Res1 * Base.


hex_digit(0,'0').
hex_digit(1,'1').
hex_digit(2,'2').
hex_digit(3,'3').
hex_digit(4,'4').
hex_digit(5,'5').
hex_digit(6,'6').
hex_digit(7,'7').
hex_digit(8,'8').
hex_digit(9,'9').
hex_digit(10,'A').
hex_digit(11,'B').
hex_digit(12,'C').
hex_digit(13,'D').
hex_digit(14,'E').
hex_digit(15,'F').

% atom_concat_list(++List, --Atom)
% Concatenates all atoms in List in the order they appear
% to form a concatenation, Atom
atom_concat_list([Atom],Atom).
atom_concat_list([Elem1,Elem2|Rest], CompositeAtom) :-
        atom_concat(Elem1,Elem2,Elem3),
        atom_concat_list([Elem3|Rest], CompositeAtom).

% Convert between atoms and integers
atom_integer(Atom,Integer) :-
	ground(Atom),
	atom_chars(Atom, Chars),
	number_chars(Integer, Chars).

atom_integer(Atom,Integer) :-
	ground(Integer),
	number_chars(Integer,Chars),
	atom_chars(Atom,Chars).

% terms_from_file(++File,--Terms)
% Reads all Terms from named file File
terms_from_file(File, Terms) :-
	open(File, read, Stream),
	ground(Stream),
	collect_stream_terms(Stream,Terms),
	close(Stream).
	
% Create list of Rules found in Stream
collect_stream_terms(Stream, Rules) :-
	read(Stream, T),
	((T == end_of_file) ->
		Rules = []
		;
		collect_stream_terms(Stream,Rest),
		append([T],Rest,Rules)).
		
% Merge list of lists into one long list, e.g.
% flatten_once([[a,b],[c,d],[e,f]],E) => E = [a, b, c, d, e, f].
flatten_once([],[]).
flatten_once([[]|Rest],OutRest) :-
       !,
       flatten_once(Rest,OutRest).
flatten_once([A|Rest],[A|OutRest]) :-
        atom(A),
        !,
        flatten_once(Rest,OutRest).
flatten_once([E1|Rest],Out) :-
        is_list(E1),
        append(E1,FlatRest,Out),
        !,
        flatten_once(Rest,FlatRest).

stats(Probs,[Mean,Low,High]) :-
	length(Probs,ProbLen),
	Mean is 1 / ProbLen,
	list_max(Probs,High),
	list_min(Probs,Low).

% Find minimum element
min(A,A,A).
min(A,B,A) :- A < B.
min(A,B,B) :- B < A.

% Find maximum of two elements
max(A,A,A).
max(A,B,A) :- B < A.
max(A,B,B) :- A < B.

% Find maximum of list
list_max([E],E).
list_max([E|R],Max) :-
	list_max(R,MR),
	((E > MR) -> Max = E ; Max = MR).

% Find minimum of list
list_min([E],E).
list_min([E|R],Min) :-
	list_min(R,MR),
	((E < MR) -> Min = E ; Min = MR).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HTML output 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

html_write_file(File,SExpr) :-
	tell(File),
	html_write(SExpr),
	told.

html_write(text(Contents)) :-
	!,
	write(Contents),
	nl.
	
html_write(Contents) :-
	is_list(Contents),
	!,
	forall(member(C,Contents),html_write(C)).
	
html_write(SExpr) :-
	SExpr =.. [ Tag, Attributes, Contents ],
	write('<'), write(Tag), write(' '), write(Attributes), write('>'),nl,
	html_write(Contents),
	write('</'), write(Tag), write('>'), nl.
	
html_write(SExpr) :-
	SExpr =.. [ Tag, Contents ],
	write('<'), write(Tag), write('>'),nl,
	html_write(Contents),
	write('</'), write(Tag), write('>'), nl.
	
html_write(SExpr) :-
	SExpr =.. [ Tag ],
	write('<'), write(Tag), write(' />'),nl.
