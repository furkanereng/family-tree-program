% CSE 2260 - Project #2
% Eren Emre Aycibin
% Furkan Eren Gülçay
% Yasin Emre Çetin
% Usage: ?- main.

main :- menu.

% Person -> Name & Surname, Gender, Birth Year, Death Year, Mother Name & Surname, Father Name & Surname, List of Children Names & Surnames.
:- dynamic person/7.
person('Murat Aslan', male, 1940, none, none, none, []).
person('Sedanur Aslan', female, 1942, none, none, none, []).

% Marriage -> Wife Name & Surname, Husband Name & Surname, Level in the Family Tree.
:- dynamic marriage/3.
marriage('Sedanur Aslan', 'Murat Aslan', 0).

% Used for getting the current year.
current_year(YEAR) :- YEAR is 2025.

% Used for determining that is person alive or dead.
alive(Name) :-
    person(Name, _, _, Death, _, _, _),
    Death == none.

% Used for getting the age of the person.
age(Name, Age) :-
    person(Name, _, Birth, Death, _, _, _),
    current_year(YEAR),
    (alive(Name) -> Age is YEAR - Birth; Age is Death - Birth).

% Prints the menu.
print_menu :-
    write('1-) Ask relation'), nl,
    write('2-) Add/Update person'), nl,
    write('3-) Get information of any person'), nl,
    write('4-) Print the family tree'), nl,
    write('5-) Add marriage'), nl,
    write('6-) Terminate the program'), nl. 

% Recursively calls itself until user decides to terminate the program. 
menu :-
    print_menu,
    write('Please choose an operation! '),
    read(Choice),
    (handle_choice(Choice) -> true; true),
    (Choice == 6 -> !, fail; true),
    nl, menu.

% Choice 1: Ask relation.
handle_choice(1) :-
    write('Please type the name and surname of first person  '),
    read(FirstPerson), 
    write('Please type the name and surname of second person '),
    read(SecondPerson), 
    ((person(FirstPerson, _, _, _, _, _, _), person(SecondPerson, _, _, _, _, _, _)) ->
        true; write('ERROR: INVALID PERSON DETECTED!'), nl, !, fail),
    determine_relation(FirstPerson, SecondPerson).

% Choice 2: Add/Update person.
handle_choice(2) :-
    write('1-) Add person'), nl,
    write('2-) Update person'), nl,
    write('Please choose an operation! '),
    read(Choice),
    handle_choice2(Choice).

% Choice 3: Get information of any person.
handle_choice(3) :-
    write('Please type the person name and surname '),
    read(Person),
    (person(Person, G, B, D, _, _, C) ->
        format('Gender: ~w~n', [G]),
        age(Person, Age),
        format('Birth Year: ~w~n', [B]),
        (D \== none -> format('Death Year: ~w~n', [D]); true),
        (alive(Person) -> format('Age: ~w~n', [Age]), write('Alive'), nl; format('Death Age: ~w~n', [Age]), write('Dead'), nl),
        length(C, NumOfChildren),
        format('Number of Children: ~w~n', [NumOfChildren]),
        level(Person, Level),
        format('Level: ~w~n', [Level]),
        write('Marital Status: '),
        marital_status(Person), nl
    ; write('ERROR: NO SUCH A PERSON EXISTING.'), nl, !, fail).

% Choice 4: Print the family tree.
handle_choice(4) :-
    findall(Person, person(Person, _, _, _, _, _, _), People),
    print_tree(People, 0).

% Choice 5: Add marriage.
handle_choice(5) :- 
    write('Please type the name of the first person  '), 
    read(FirstPerson), 
    write('Please type the name of the second person '),
    read(SecondPerson), 
    (
        (
            ((person(FirstPerson, male, _, _, _, _, _), person(SecondPerson, female, _, _, _, _, _), Wife = SecondPerson, Husband = FirstPerson);
             (person(FirstPerson, female, _, _, _, _, _), person(SecondPerson, male, _, _, _, _, _), Wife = FirstPerson, Husband = SecondPerson)),
            level(Wife, WifeLevel), level(Husband, HusbandLevel),
            (WifeLevel >= HusbandLevel -> Level is WifeLevel; Level is HusbandLevel)
        );
        (
            (person(FirstPerson, G1, _, _, _, _, _) ->
                write('Please type the birth date of the new person   '),
                read(Birth), 
                write('Please type the death date of the new person   '),
                read(Death),
                write('Please type the gender of the new person (m/f) '),
                read(Gender),
                (G1 == male, Gender == m -> write('ERROR: ONLY HETEROSEXUAL MARRIAGES ALLOWED!'), nl, !, fail; true),
                (G1 == female, Gender == f -> write('ERROR: ONLY HETEROSEXUAL MARRIAGES ALLOWED!'), nl, !, fail; true),
                (Gender \== m, Gender \== f -> write('ERROR: INVALID GENDER!'), nl, !, fail; true),
                (current_year(YEAR), integer(Birth), (Death == none; integer(Death), 0 < Death, Death < YEAR, Birth =< Death), 0 < Birth ->
                    true; write('ERROR: INVALID BIRTH OR DEATH DATES DETECTED! MARRIAGE CAN NOT BE CREATED!'), nl, !, fail),
                (G1 == male -> assertz(person(SecondPerson, female, Birth, Death, none, none, [])), Wife = SecondPerson, Husband = FirstPerson
                ; assertz(person(SecondPerson, male, Birth, Death, none, none, [])), Wife = FirstPerson, Husband = SecondPerson
                ),
                level(FirstPerson, Level)
            )
        ;
            (person(SecondPerson, G2, _, _, _, _, _) ->
                write('Please type the birth date of the new person   '),
                read(Birth), 
                write('Please type the death date of the new person   '),
                read(Death),
                write('Please type the gender of the new person (m/f) '),
                read(Gender),
                (G2 == male, Gender == m -> write('ERROR: ONLY HETEROSEXUAL MARRIAGES ALLOWED!'), nl, !, fail; true),
                (G2 == female, Gender == f -> write('ERROR: ONLY HETEROSEXUAL MARRIAGES ALLOWED!'), nl, !, fail; true),
                (Gender \== m, Gender \== f -> write('ERROR: INVALID GENDER!'), nl, !, fail; true),
                (current_year(YEAR), integer(Birth), (Death == none; integer(Death), 0 < Death, Death < YEAR, Birth =< Death), 0 < Birth ->
                    true; write('ERROR: INVALID BIRTH OR DEATH DATES DETECTED! MARRIAGE CAN NOT BE CREATED!'), nl, !, fail),
                (G2 == male -> assertz(person(FirstPerson, female, Birth, Death, none, none, [])), Wife = FirstPerson, Husband = SecondPerson
                ; assertz(person(FirstPerson, male, Birth, Death, none, none, [])), Wife = SecondPerson, Husband = FirstPerson
                ),
                level(SecondPerson, Level)
            )
        ; write('ERROR: BOTH OF PEOPLE ARE NOT EXISTING, CAN NOT ADD TO FAMILY TREE!'), nl, !, fail
        )
    ),
    (valid_marriage(Wife, Husband) ->
        assertz(marriage(Wife, Husband, Level)),
        write('Marriage added successfully!'), nl
    ; nl, !, fail
    ).

% Choice 6: Terminate the program.
handle_choice(6) :- write('Good Bye!'), nl.
handle_choice(C) :- (integer(C), 0 > C, 6 < C -> write('Invalid Choice!'), nl, !, fail).

% Determines and prints the relation between Person1 and Person2. (Who is Person1 in relation to Person2?)
determine_relation(Person1, Person2) :-
    (
        (anne(Person1, Person2) -> write('Anne'));
        (baba(Person1, Person2) -> write('Baba'));
        (ogul(Person1, Person2) -> write('Ogul'));
        (kiz(Person1, Person2) -> write('Kiz'));
        (erkek_kardes(Person1, Person2) -> write('Erkek Kardes'));
        (kiz_kardes(Person1, Person2) -> write('Kiz Kardes'));
        (abi(Person1, Person2) -> write('Abi'));
        (abla(Person1, Person2) -> write('Abla'));
        (amca(Person1, Person2) -> write('Amca'));
        (hala(Person1, Person2) -> write('Hala'));
        (dayi(Person1, Person2) -> write('Dayi'));
        (teyze(Person1, Person2) -> write('Teyze'));
        (kuzen(Person1, Person2) -> write('Kuzen'));
        (yegen(Person1, Person2) -> write('Yegen'));
        ((es(Person1, Person2); es(Person2, Person1)) -> write('Es'));
        (eniste(Person1, Person2) -> write('Eniste'));
        (yenge(Person1, Person2) -> write('Yenge'));
        (anneanne(Person1, Person2) -> write('Anneanne'));
        (babaanne(Person1, Person2) -> write('Babaanne'));
        (dede(Person1, Person2) -> write('Dede'));
        (torun(Person1, Person2) -> write('Torun'));
        (kayinvalide(Person1, Person2) -> write('Kayinvalide'));
        (kayinpeder(Person1, Person2) -> write('Kayinpeder'));
        (gelin(Person1, Person2) -> write('Gelin'));
        (damat(Person1, Person2) -> write('Damat'));
        (baldiz(Person1, Person2) -> write('Baldiz'));
        (bacanak(Person1, Person2) -> write('Bacanak'));
        (kayinbirader(Person1, Person2) -> write('Kayinbirader'));
        (elti(Person1, Person2) -> write('Elti'));
        (gorumce(Person1, Person2) -> write('Gorumce'));
        (dunur(Person1, Person2) -> write('Dunur'));
        write('Not defined relation')
    ), nl.

% Choice 2.1: Add person.
handle_choice2(1) :-
    write('Please type the father name and surname   '),
    read(FatherName),
    write('Please type the mother name and surname   '),
    read(MotherName),
    write('Please type the child name and surname    '),
    read(ChildName),
    write('Please type the birth year of the child   '),
    read(Birth),
    write('Please type the death year of the child   '),
    read(Death),
    write('Please type the gender of the child (m/f) '),
    read(Gender),
    (es(MotherName, FatherName) -> true; write('ERROR: INVALID PARENTS!'), nl, !, fail),
    person(MotherName, female, BM, _, _, _, _), person(FatherName, male, BF, _, _, _, _),
    (integer(Birth), Birth >= BM + 18, Birth >= BF + 18 -> true; write('ERROR: NON-REALISTIC BIRTH DATE! CHILD CAN NOT BORN BEFORE PARENTS NOT MARRIED!'), nl, !, fail),
    add_person(ChildName, Gender, Birth, Death, MotherName, FatherName),
    write('Person added successfully!'), nl.

% Choice 2.2: Update person.
handle_choice2(2) :-
    write('1. Update birth year of someone'), nl,
    write('2. Update death year of someone'), nl,
    write('0. Cancel'), nl,
    write('Please choose an operation! '),
    read(Choice),
    handle_choice2_(Choice).

% Choice 2.X: Invalid.
handle_choice2(C) :- (integer(C), 0 > C, C < 2 -> write('Invalid choice!'), nl).

% Used for handling the subchoices of Choice 2.
handle_choice2_(0) :- write('Update operation is cancelled!'), nl.
handle_choice2_(1) :- (update_birthyear -> true; true).
handle_choice2_(2) :- (update_deathyear -> true; true).
handle_choice2_(C) :- (0 > C, C > 2 -> write('Invalid choice!'), nl).

% Adds the person if the inputs are valid.
add_person(Name, Gender, Birth, Death, Mother, Father) :-
    % Checks for the existence of parents.
    (person(Mother, female, _, _, _, _, _) -> true; write('ERROR: MOTHER CAN NOT BE FOUND!'), nl, !, fail),
    (person(Father, male, _, _, _, _, _) -> true; write('ERROR: FATHER CAN NOT BE FOUND!'), nl, !, fail),
    % Checks the dates are valid or not.
    (valid_dates(Birth, Death, Mother, Father) -> true
    ; write('ERROR: INVALID/NON-REALISTIC BIRTH OR DEATH YEARS DETECTED!'), nl, !, fail),
    % Checks the gender is valid or not.
    ((Gender == m -> G = male); (Gender == f -> G = female)
    ; write('ERROR: INVALID GENDER DETECTED!'), nl, !, fail),
    (person(Name, _, _, _, _, _, _) -> write('ERROR: PERSON ALREADY EXIST!'), nl, !, fail; true),
    assertz(person(Name, G, Birth, Death, Mother, Father, [])),
    insert_child(Name, Mother, Father).

% Updates the Children lists of Mother and Father.
insert_child(ChildName, Mother, Father) :-
    person(Mother, MG, MB, MD, MM, MF, ChildrenM), person(Father, FG, FB, FD, FM, FF, ChildrenF),
    append(ChildrenM, [ChildName], NewChildrenM),
    append(ChildrenF, [ChildName], NewChildrenF),
    retract(person(Mother, MG, MB, MD, MM, MF, ChildrenM)),
    assertz(person(Mother, MG, MB, MD, MM, MF, NewChildrenM)),
    retract(person(Father, FG, FB, FD, FM, FF, ChildrenF)),
    assertz(person(Father, FG, FB, FD, FM, FF, NewChildrenF)).

% Checks that are birth and death years are realistic and valid or not.
valid_dates(Birth, Death, Mother, Father) :-
    person(Mother, _, MotherBirth, MotherDeath, _, _, _),
    person(Father, _, FatherBirth, FatherDeath, _, _, _),
    % Birth/Death years can not be after the current year & Birth year must be before Death year.
    current_year(YEAR), integer(Birth), (Death == none; integer(Death), 0 < Death, Death < YEAR, Birth =< Death), 0 < Birth, Birth =< YEAR,
    % Child can not born before birth year of any of his/her parents.
    Birth > MotherBirth, Birth > FatherBirth,
    % Child can not born after death year of any of his/her parents.
    (FatherDeath \== none -> Birth < FatherDeath; true),
    (MotherDeath \== none -> Birth < MotherDeath; true).

% Choice 2.2.1: Update birth year of someone.
update_birthyear :- 
    write('Please enter the name of the person that you want to update '),
    read(Name),
    write('Please enter the new birth year '),
    read(NewBirth),
    ( person(Name, G, B, D, M, F, C) -> true
    ; write('ERROR: NO SUCH A PERSON EXISTING!'), nl, !, fail
    ),
    ( anne(Anne, Name), baba(Baba, Name) ->
        person(Anne, _, AnneBirth, AnneDeath, _, _, _),
        person(Baba, _, BabaBirth, BabaDeath, _, _, _),
        (AnneBirth > BabaBirth -> ParentMaxBirth = AnneBirth; ParentMaxBirth = BabaBirth),
        current_year(YEAR),
        (AnneDeath \== none -> AnneD = AnneDeath; AnneD = YEAR),
        (BabaDeath \== none -> BabaD = BabaDeath; BabaD = YEAR),
        (AnneD < BabaD -> ParentMinDeath = AnneD; ParentMinDeath = BabaD),
        min_birthdate(C, ChildrenMinBirth),
        Min = ParentMaxBirth,
        % New birth year must be in the interval [max(Births of Parents), min(Deaths of Parents & Births of Children)].
        (ChildrenMinBirth < ParentMinDeath -> Max = ChildrenMinBirth; Max = ParentMinDeath),
        ( Min =< NewBirth, NewBirth =< Max -> 
            retract(person(Name, G, B, D, M, F, C)),
            assertz(person(Name, G, NewBirth, D, M, F, C)),
            write('Person updated successfully!'), nl
        ; write('ERROR: INVALID BIRTH YEAR!'), nl, !, fail)
    ; % If mother and father are 'none'...
        min_birthdate(C, ChildrenMinBirth),
        (NewBirth < ChildrenMinBirth -> 
            retract(person(Name, G, B, D, M, F, C)),
            assertz(person(Name, G, NewBirth, D, M, F, C)),
            write('Person updated successfully!'), nl
        ; write('ERROR: INVALID BIRTH YEAR!'), nl, !, fail)
    ).

% Used for finding the minimum birthdate among Children of a parent.
min_birthdate([], MinBirthYear) :- current_year(YEAR), MinBirthYear is YEAR.
min_birthdate([ID], MinBirthYear) :- 
    person(ID, _, Birth, _, _, _, _),
    MinBirthYear is Birth.
min_birthdate([ID1, ID2 | IDs], MinBirthYear) :-
    person(ID1, _, Birth1, _, _, _, _),
    min_birthdate([ID2 | IDs], Min),
    (Min > Birth1 -> MinBirthYear is Birth1; MinBirthYear is Min).

% Choice 2.2.2: Update death year of someone.
update_deathyear :- 
    write('Please enter the name of the person that you want to update '),
    read(Name),
    write('Please enter the new death year '),
    read(NewDeath),
    (person(Name, G, B, D, M, F, C) -> true; write('ERROR: NO SUCH A PERSON EXISTING!'), nl, !, fail),
    % If the person has children, he/she must live at least until to the year which the youngest child born (max(Birth years of Children)). 
    (C == [] -> MaxBirthYear = -1; max_birthdate(C, MaxBirthYear)),
    (NewDeath \== none -> 
        (B =< NewDeath, NewDeath >= MaxBirthYear ->
        retract(person(Name, G, B, D, M, F, C)), assertz(person(Name, G, B, NewDeath, M, F, C)),
        write('Person updated successfully!'), nl;
        write('ERROR: INVALID DEATH YEAR!'), nl, !, fail)
    ;   retract(person(Name, G, B, D, M, F, C)), assertz(person(Name, G, B, none, M, F, C))
    ).

% Used for finding the maximum birthdate among Children of a parent.
max_birthdate([], MaxBirthYear) :- current_year(YEAR), MaxBirthYear is YEAR.
max_birthdate([ID], MaxBirthYear) :- 
    person(ID, _, Birth, _, _, _, _),
    MaxBirthYear is Birth.
max_birthdate([ID1, ID2 | IDs], MaxBirthYear) :-
    person(ID1, _, Birth1, _, _, _, _),
    max_birthdate([ID2 | IDs], Max),
    (Max < Birth1 -> MaxBirthYear is Birth1; MaxBirthYear is Max).

% Get Level information from Anne/Baba.
level(Person, Level) :-
    person(Parent, _, _, _, _, _, C),
    member(Person, C),
    level(Parent, ParentLevel),
    Level is ParentLevel + 1, !.

% Get Level information from marriage rule.
level(Person, Level) :-
    (marriage(Person, _, Level); marriage(_, Person, Level)), !.

% Get Level information from Eş.
level(Person, Level) :-
    (es(Person, Es); es(Es, Person)), level(Es, Level), !.

% Prints the marital status of Person.
marital_status(Person) :-
    person(Person, G, _, DP, _, _, _),
    (
        (marriage(Person, Es, _); marriage(Es, Person, _)) ->
            (
                alive(Es), alive(Person) ->
                    write('married');
                alive(Es) ->
                    write('married when died');
                alive(Person) ->
                    (G == male -> write('married, but became widower'); write('married, but became widow'));
                person(Es, _, _, DE, _, _, _),
                DP > DE ->
                    (G == male -> write('married, but widower when died'); write('married, but widow when died'));
                write('married when died')
            )
        ;
        (G == male -> write('widower'); write('widow'))
    ).

% Prints the whole tree.
print_tree([], _) :- !.
print_tree(People, Level) :-
    format('~n----- LEVEL ~w -----~n', [Level]),
    print_level(People, Level, Remaining),
    NextLevel is Level + 1,
    print_tree(Remaining, NextLevel).

% Prints the people in this Level.
print_level([], _, []).
print_level([P | Rest], Level, Remaining) :-
    level(P, L),
    (L == Level ->
        (   % Print with spouse.
            (marriage(P, Es, _); marriage(Es, P, _)), member(Es, Rest) ->  
                format('~w - ~w~n', [P, Es]),
                remove_if_present(Es, Rest, TempRest),
                remove_if_present(P, TempRest, NewRest),
                print_level(NewRest, Level, Remaining)
        ;
            % Print alone if not married.
            \+ (marriage(P, _, _); marriage(_, P, _)) -> 
                format('~w~n', [P]),
                remove_if_present(P, Rest, NewRest),
                print_level(NewRest, Level, Remaining)
        ;
            % Skip if spouse is already printed.
            print_level(Rest, Level, TempRemaining),
            Remaining = [P | TempRemaining]
        )
    ;
        % Skip if not at this Level.
        print_level(Rest, Level, TempRemaining),
        Remaining = [P | TempRemaining]
    ).

% Remove element if it's in the list
remove_if_present(X, List, Result) :- select(X, List, Result), !.
remove_if_present(_, List, List).

% Checks that is marriage valid or not.
valid_marriage(Wife, Husband) :-
    age(Wife, WifeAge), age(Husband, HusbandAge),
    (
        (HusbandAge < 18; WifeAge < 18) -> 
        write('INVALID MARRIAGE: ONE OF THE SPOUSE IS BELOW 18!'), nl, !, fail
        ; true
    ),
    (
        marriage(Wife, _, _); marriage(_, Husband, _) -> 
        write('INVALID MARRIAGE: ONE OF THE SELECTED SPOUSE IS ALREADY MARRIED!'), nl, !, fail
        ; true
    ),
    (
        (
            (anne(Wife, Husband) -> write('INVALID MARRIAGE: ANNE - OGUL')); 
            (baba(Husband, Wife) -> write('INVALID MARRIAGE: BABA - KIZ')); 
            (abi(Husband, Wife) -> write('INVALID MARRIAGE: ABI - KIZ KARDES')); 
            (erkek_kardes(Husband, Wife) -> write('INVALID MARRIAGE: ABLA - ERKEK KARDES'));
            (amca(Husband, Wife) -> write('INVALID MARRIAGE: AMCA - YEGEN')); 
            (dayi(Husband, Wife) -> write('INVALID MARRIAGE: DAYI - YEGEN'));
            (hala(Wife, Husband) -> write('INVALID MARRIAGE: HALA - YEGEN')); 
            (teyze(Wife, Husband) -> write('INVALID MARRIAGE: TEYZE - YEGEN'));
            (anneanne(Wife, Husband) -> write('INVALID MARRIAGE: ANNEANNE - TORUN'));
            (babaanne(Wife, Husband) -> write('INVALID MARRIAGE: BABAANNE - TORUN'));
            (dede(Husband, Wife) -> write('INVALID MARRIAGE: DEDE - TORUN'))
        ) -> nl, !, fail; true
    ).

% All the rest are definitions of the relations.
anne(Anne, Person) :-
    person(Anne, female, _, _, _, _, Children), person(Person, _, _, _, Anne, _, _),
    member(Person, Children).

baba(Baba, Person) :-
    person(Baba, male, _, _, _, _, Children), person(Person, _, _, _, _, Baba, _),
    member(Person, Children).

ogul(Ogul, Person) :-
    person(Ogul, male, _, _, _, _, _),
    person(Person, _, _, _, _, _, C), member(Ogul, C).

kiz(Kiz, Person) :-
    person(Kiz, female, _, _, _, _, _),
    person(Person, _, _, _, _, _, C), member(Kiz, C).

erkek_kardes(Kardes, Person) :-
    person(Kardes, male, _, _, Anne, Baba, _),
    person(Person, _, _, _, Anne, Baba, _),
    Anne \== none, Baba \== none,
    Kardes \== Person, age(Kardes, KardesAge), age(Person, Age),
    KardesAge =< Age.

kiz_kardes(Kardes, Person) :-
    person(Kardes, female, _, _, Anne, Baba, _),
    person(Person, _, _, _, Anne, Baba, _),
    Anne \== none, Baba \== none,
    Kardes \== Person, age(Kardes, KardesAge), age(Person, Age),
    KardesAge =< Age.

abi(Kardes, Person) :-
    person(Kardes, male, _, _, Anne, Baba, _),
    person(Person, _, _, _, Anne, Baba, _),
    Anne \== none, Baba \== none,
    Kardes \== Person, age(Kardes, KardesAge), age(Person, Age),
    KardesAge > Age.

abla(Kardes, Person) :-
    person(Kardes, female, _, _, Anne, Baba, _),
    person(Person, _, _, _, Anne, Baba, _),
    Anne \== none, Baba \== none,
    Kardes \== Person, age(Kardes, KardesAge), age(Person, Age),
    KardesAge > Age.

amca(Amca, Person) :-
    person(Person, _, _, _, _, Baba, _),
    person(Baba, male, _, _, _, _, _),
    person(Amca, male, _, _, _, _, _),
    (abi(Amca, Baba); erkek_kardes(Amca, Baba)).

hala(Hala, Person) :-
    person(Person, _, _, _, _, Baba, _),
    person(Baba, male, _, _, _, _, _),
    person(Hala, female, _, _, _, _, _),
    (abla(Hala, Baba); kiz_kardes(Hala, Baba)).

dayi(Dayi, Person) :-
    person(Person, _, _, _, Anne, _, _),
    person(Anne, female, _, _, _, _, _),
    person(Dayi, male, _, _, _, _, _),
    (abi(Dayi, Anne); erkek_kardes(Dayi, Anne)).

teyze(Teyze, Person) :-
    person(Person, _, _, _, Anne, _, _),
    person(Anne, female, _, _, _, _, _),
    person(Teyze, female, _, _, _, _, _),
    (abla(Teyze, Anne); kiz_kardes(Teyze, Anne)).

yegen(Yegen, Person) :-
    person(Yegen, _, _, _, _, _, _),
    amca(Person, Yegen); hala(Person, Yegen); dayi(Person, Yegen); teyze(Person, Yegen).

kuzen(Kuzen, Person) :-
    person(Kuzen, _, _, _, _, _, _),
    (amca(Parent, Person); hala(Parent, Person); dayi(Parent, Person); teyze(Parent, Person)),
    person(Parent, _, _, _, _, _, Children), member(Kuzen, Children).

es(Kari, Koca) :-
    person(Kari, female, _, _, _, _, _), person(Koca, male, _, _, _, _, _),
    marriage(Kari, Koca, _).

eniste(Eniste, Person) :-
    person(Eniste, male, _, _, _, _, _),
    (hala(Parent, Person); teyze(Parent, Person)), es(Parent, Eniste).

yenge(Yenge, Person) :-
    person(Yenge, female, _, _, _, _, _),
    (amca(Parent, Person); dayi(Parent, Person)), es(Yenge, Parent).

anneanne(Anneanne, Person) :-
    person(Anneanne, female, _, _, _, _, _),
    anne(Anne, Person), anne(Anneanne, Anne).

babaanne(Babaanne, Person) :-
    person(Babaanne, female, _, _, _, _, _),
    baba(Baba, Person), anne(Babaanne, Baba).

dede(Dede, Person) :-
    person(Dede, male, _, _, _, _, _),
    (anne(Parent, Person); baba(Parent, Person)), baba(Dede, Parent).

torun(Torun, Person) :-
    person(Torun, _, _, _, _, _, _),
    (dede(Person, Torun); anneanne(Person, Torun); babaanne(Person, Torun)).

kayinvalide(Kayinvalide, Person) :-
    person(Kayinvalide, female, _, _, _, _, _),
    (es(Es, Person); es(Person, Es)), anne(Kayinvalide, Es).

kayinpeder(Kayinpeder, Person) :-
    person(Kayinpeder, male, _, _, _, _, _),
    (es(Es, Person); es(Person, Es)), baba(Kayinpeder, Es).

gelin(Gelin, Person) :-
    person(Gelin, female, _, _, _, _, _),
    (kayinpeder(Person, Gelin); kayinvalide(Person, Gelin)).

damat(Damat, Person) :-
    person(Damat, male, _, _, _, _, _),
    (kayinpeder(Person, Damat); kayinvalide(Person, Damat)).

baldiz(Baldiz, Person) :-
    person(Baldiz, female, _, _, _, _, _),
    es(Kari, Person), (abla(Kari, Baldiz); kiz_kardes(Kari, Baldiz)).

bacanak(Bacanak, Person) :-
    person(Bacanak, male, _, _, _, _, _),
    baldiz(Baldiz, Person), es(Baldiz, Bacanak).

elti(Elti, Person) :-
    person(Elti, female, _, _, _, _, _), person(Person, female, _, _, _, _, _),
    es(Person, Koca), (abi(Koca, Kardes); erkek_kardes(Koca, Kardes)), es(Elti, Kardes).

gorumce(Gorumce, Person) :-
    es(Person, Koca), (abla(Gorumce, Koca); kiz_kardes(Gorumce, Koca)),
    person(Gorumce, female, _, _, _, _, _), person(Person, female, _, _, _, _, _).

kayinbirader(Kayinbirader, Person) :-
    (es(Person, Es); es(Es, Person)), (erkek_kardes(Kayinbirader, Es); abi(Kayinbirader, Es)),
    person(Kayinbirader, male, _, _, _, _, _).

dunur(Dunur, Person) :-
    Person \== Dunur,
    person(Person, _, _, _, _, _, _), person(Dunur, _, _, _, _, _, C),
    member(Cocuk, C), (gelin(Cocuk, Person); damat(Cocuk, Person)).
