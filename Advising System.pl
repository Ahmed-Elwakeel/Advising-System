:- use_module(library(csv)).
:- use_module(library(clpfd)).
:- ['cur.pl'].
:- ['obl.pl'].

readFile(File , Rows , RowName):-
                csv_read_file(File, Rows , [functor(RowName)]),
                maplist(assert, Rows).
                
                
                
testt(X):-

          X = [A,B,C],
          X ins 1..4,
           print(X),
          labeling([],X).


test( Tobechecked  , [] ,[],0 , L ).

test(schRow(Course, _, _, _, TutLecLab, _, _) , [schRow(CourseS, _, _, _, TutLecLabS, _, _)|T], S, N ,L):-
                 schRow(CourseS, _, _, _, TutLecLabS, _, _) \= schRow(Course, _, _, _, TutLecLab, _, _),
                 test(schRow(Course, _, _, _, TutLecLab, _, _) , T, S, N1,L),
                 N #= N1+1.
test(schRow(Course, _, _, _, TutLecLab, _, _) , [schRow(CourseS, _, _, _, TutLecLabS, _, _)|T], [HC|TC], N ,L ):-
                 schRow(CourseS, _, _, _, TutLecLabS, _, _) = schRow(Course, _, _, _, TutLecLab, _, _),
                 

                 test(schRow(Course, _, _, _, TutLecLab, _, _) , T, TC,N1 ,L ),
                   N #= N1+1,
                   Out #=  L-N ,
                   Out1 #= Out+1,
                 HC=Out1.
                 

testHelper([H|[]],H ).
testHelper([H|T],A ):-
               testHelper(T , A1),
               A = H \/ A1.


              


advising_system(Chs ,Ncourses,TLLCourses,Schedule, Gaps,DaysNoDuplicates, G , Elem):-
        %readFile('data_formated_22556.csv' , Schedule , schRow),
        readFile('s2.csv' , Schedule , schRow),
        %readFile('history.csv' , History , histrow),
        %getList(Cur),
        getObl(OblCourses),
%Cur = [curRow('CSEN 503',4,['CSEN 501']),curRow('AE 101',2,[]),curRow('DE 101',2,[]),curRow('CHEMt 102',4,[]),curRow('AS 102',4,['AE 101'])],
%History = [histrow('AE 101','B'),histrow('CHEMt 102','B'),histrow('CSEN 501','B')],
Cur = [curRow('CSEN 102',2,[]),curRow('CSEN 301',2,[]) , curRow('PHYSp 301',2,[]), curRow('CSEN 909',2,[]),curRow('CSEN 702',2,[]) ,curRow('CSEN 704',2,[])],
History = [],
        hc(Cur, History, Ncourses1),       % remove courses with a grade not equal F from curriculum
        checkPre(Ncourses1, History, Ncourses),
        
        length(Schedule , ScheduleSize),
        length(Ncourses , CoursesSize),
        NewCoursesSize #= CoursesSize * 3,
        length(TLLCourses, NewCoursesSize),  %TLLCourses represent the number of courses that could be taken multiplied by 3 to account for the lecture, lab, and tutorial.
        TLLCourses ins 0..ScheduleSize,  %0 means there is not lecture , tutorial , or lab for this course. else, it represent a row in the schedule.

        checkCorrectness(0 ,TLLCourses , Ncourses, Schedule ,OblCourses, Chs ), %After checkCorrectness ,   TLLCourses is correct, such that each 3 consecutive elements are matching correctly with the element in the Ncourses.
        checkDifferentTime(TLLCourses , Schedule , Gaps , Days),

       % sort( Days, DaysSorted),
        %sort(Gaps, GapsSorted),
        %minGap(Gaps , GapVar,  DaysNoDuplicates),
        %G #= GapVar,
        %print(G),print('  '),print( TLLCourses),
       print('TTL'),
       %delete(TLLCourses, 0, NewList),
       %all_different(NewList),
        print( TLLCourses),
        labeling([],TLLCourses),

        list_to_set(Days , DaysNoDuplicates),

        length( DaysNoDuplicates , DaysSize),
        DaysSize #< 6.



% remove courses with a grade not equal F from cur

hc(Cur , [], Cur).
hc(Cur , [histrow(Course , Grade)|T] , Out):-
                           Grade ='F',
                           hc(Cur , T ,Out).
hc(Cur , [histrow(Course , Grade)|T], Out1):-
                           Grade \= 'F',
                           hc(Cur , T ,Out),
                           delete(Out,curRow(Course,_,_),Out1).



% create a list of all the courses that could be taken == > the result of hc/3
cList([],[]).
cList([curRow(Course,_,_)|T],[A|B]):- cList(T,B),
                                      A = Course.



% After checkCorrectness ,   TLLCourses is correct, such that each 3 consecutive elements are matching correctly with the element in the Ncourses.
% check that the summation of all the credit hours equals the given credit hours
checkCorrectness(ChsSoFar , []  ,  [],  Schedule ,OblCourses, Chs):- ChsSoFar #=Chs.

checkCorrectness(ChsSoFar , [T,Lec,Lab|Tail]  ,  [curRow(Course,Ch,Pre) | TC],  Schedule ,OblCourses, Chs) :-
                                             T #= 0,
                                             Lec #= 0,
                                             Lab #=0 ,
                                             %\+(member(Course , OblCourses)), %obligatory courses that the student won’t be able to skip
                                             checkCorrectness(ChsSoFar, Tail , TC , Schedule ,OblCourses,Chs).
                                             
checkCorrectness(ChsSoFar , [T,Lec,Lab|Tail]  ,  [curRow(Course,Ch,Pre) | TC],  Schedule ,OblCourses, Chs):-

                                    checkT( [T,Lec,Lab |Tail]  ,  [curRow(Course,Ch,Pre) | TC],  Schedule),
                                    checkLec([T,Lec,Lab |Tail]  ,  [curRow(Course,Ch,Pre) | TC],  Schedule),
                                    checkLab([T,Lec,Lab |Tail]  ,  [curRow(Course,Ch,Pre) | TC],  Schedule),
                                  checkSameGroup(T,Lab, Schedule),
                                   X1 #= ChsSoFar + Ch ,
                                    checkCorrectness(X1 , Tail , TC , Schedule,OblCourses, Chs).

% check if it is 0 in one of the elements , then check that it doesnot exist in the schedule so it is true.
% if it is not 0 then the number (T , Lec , Lab) should represent a row in the schedule with the same course in Ncourses and the same thing( lab , tut ,lecture).
checkT([T,_,_|Tail]   , [curRow(Course,Ch,Pre) | TC],  Schedule):-  T #= 0 ,\+(member( schRow(Course, _, _, _, 'Tut', _, _) , Schedule )).

checkT([T,_,_|Tail]   , [curRow(Course,Ch,Pre) | TC],  Schedule):-  T #\= 0,
                                                                    length(Schedule, Size),
                                                                    test(schRow(Course, _, _, _, 'Tut', _, _), Schedule, List , Size, Size),

                                                                    testHelper(List,Domain),

                                                                    in(T,Domain).
                                                                   %  nth1(T , Schedule, schRow(Course, _, _, _, 'Tut', _, _)).


checkLec([_,Lec,_|Tail]   , [curRow(Course,Ch,Pre) | TC],  Schedule):- Lec #= 0, \+(member( schRow(Course, _, _, _, 'Lecture', _, _) , Schedule )).
checkLec([_,Lec,_|Tail]   , [curRow(Course,Ch,Pre) | TC],  Schedule):- Lec #\= 0,
                                                                           length(Schedule, Size),
                                                                    test(schRow(Course, _, _, _, 'Lecture', _, _), Schedule, List , Size, Size),
                                                                    testHelper(List,Domain),

                                                                    in(Lec,Domain).

                                                                    %nth1(Lec , Schedule, schRow(Course, _, _, _, 'Lecture', _, _)).
                                   
checkLab([_,_,Lab|Tail]  , [curRow(Course,Ch,Pre) | TC],  Schedule):- Lab #= 0, \+(member( schRow(Course, _, _, _, 'Lab', _, _) , Schedule )).
checkLab([_,_,Lab|Tail]  , [curRow(Course,Ch,Pre) | TC],  Schedule):- Lab #\= 0,
                                                                          length(Schedule, Size),
                                                                    test(schRow(Course, _, _, _, 'Lab', _, _), Schedule, List , Size, Size),
                                                                    testHelper(List,Domain),

                                                                    in(Lab,Domain).
                                                                    %nth1(Lab , Schedule, schRow(Course, _, _, _, 'Lab', _, _)).

%course has a lab and a tutorial, it is preferable that the student attends both with the same group.
%the student should attend the tutorial before the lab.
checkSameGroup(0,_,S).
checkSameGroup(_,0,S).
checkSameGroup(T,Lab ,Schedule ):-
                         convert(Schedule , CourseCode , DayCode ,SlotCode ,TypeCode) ,
                         element(T1 , DayCode, TD),
                         element(T2, SlotCode ,TS),
                         element(T3 , TypeCode ,GT),
                         element(Lab1 , DayCode, LD),
                         element(Lab2 , SlotCode ,LS),
                         element(Lab3 , TypeCode ,GL),
                         T1 #= T2,T2 #= T3,
                         Lab1 #= Lab2, Lab2 #= Lab3,
                         
                         %nth1(T , Schedule, schRow(_, _, DayT, SlotT, _, _, GT)),
                         %nth1(Lab , Schedule, schRow(_, _, DayL, SlotL, _, _, GL)),

                         %atom_length( GT,SGT),
                         %atom_length( GL,SGL),
                         %IGT1 #= SGT - 2,
                         %IGL1 #=  SGL - 2 ,
                         %sub_atom(GT, IGT1, 2, _, A) ,
                         %sub_atom(GL, IGL1, 2, _, B) ,
                         %atom_number(A, A1),
                         %atom_number(B,B1),
                         A1 #= GT mod 1000,
                         B1 #= GL mod 1000,
                         A1 #= B1,
                         
                         %atom_concat(DayT, SlotT, Out),
                         %atom_number(Out , DST),
                         %atom_concat(DayL, SlotL, Out1),
                         %atom_number(Out1 , DSL),
                         TDay10 #= TD * 10,
                         DST #= TDay10 + TS,
                         LDay10 #= LD * 10,
                         DSL #= LDay10 + 10,
                         DST #< DSL.

% check that the summation of all the credit hours equals the given credit hours
%checkCreditHours(Chs , [] , [] ,  Chs , History).
%checkCreditHours(ChsSoFar , [0 , 0 , 0 |Tail] ,[curRow(Course,Ch,Pre) | TC] ,  Chs,History):- checkCreditHours(ChsSoFar , Tail, TC ,Chs, History).
%checkCreditHours(ChsSoFar , [T , Lec , Lab |Tail] ,[curRow(Course,Ch,Pre) | TC] ,  Chs , History):-
%                            (T #\= 0;
%                            Lec #\= 0;
%                            Lab #\= 0),
%                            X1 #= ChsSoFar + Ch,
%                            %checkPreHelper(Pre, History),
%                            checkCreditHours(X1 , Tail , TC ,Chs, History).


%A student cannot take a course unless they have attended all its prerequisites
checkPre([],History,[]).
checkPre( [curRow(Course,Ch,Pre) | TC ] ,History, X ):-
                            \+(checkPreHelper(Pre, History)),
                            checkPre(TC,History,X).
checkPre( [curRow(Course,Ch,Pre) | TC ] , History , [curRow(Course,Ch,Pre) |T] ):-
                            checkPreHelper(Pre, History),
                            checkPre(TC,History,T).

checkPreHelper([], History).
checkPreHelper([H|T], History):-

                       member(histrow(H,Grade) , History),
                       Grade \= 'F',
                       checkPreHelper(T , History).



%The student cannot be assigned to attend two meetings at the same time.
% Each student should have at least one day off.

checkDifferentTime(A , B , Gaps , Days) :-   checkDifferentTimeHelper(A,B,X,Days , Gaps),

                               all_different(X).
                               %list_to_set(Days , DaysNoDuplicates)    ,
                               %print(Days),print(DaysNoDuplicates),
                               %remove_duplicates(Days , DaysNoDuplicates),
                               %length( DaysNoDuplicates , DaysSize),
                               %DaysSize #< 6.

checkDifferentTimeHelper([0|T],Schedule , X , Days , Gaps):- checkDifferentTimeHelper(T , Schedule , X , Days , Gaps).
checkDifferentTimeHelper([] , Schedule , [],[],[]).
checkDifferentTimeHelper([H|T] , Schedule , [HC|TC] , [HD|TD], [HG|TG]):-

                         checkDifferentTimeHelper(T , Schedule , TC , TD , TG),

                         convert(Schedule , CourseCode , DayCode ,SlotCode ,TypeCode)  ,

                         element(H ,   DayCode , A),

                         element(H, SlotCode , B),

                         AA #= BB ,

                         AA #= H,
                         BB #= H,
                         A in 1..6,
                         B in 1..5,
                         FirstDigit #= A * 10,
                         HC #= FirstDigit + B,
                         HG #= FirstDigit + B,
                         HD #= A.
                         %nth1(H , Schedule, schRow(_, _, A, B, _, _, _)),
                         %length(Schedule , Size),
                         %test1(  schRow(_, _, A, B, _, _, _) , Schedule , List , Size , Size),

                          %testHelper(List,Domain),

                            %in(H,Domain),

                           % print(Domain),

                         %atom_concat(A, B, Out),

                         %atom_number(Out , HC),
                         %atom_number(Out ,HG),
                         %atom_concat(A, '', A1),
                         %atom_number(A1 , HD).
                         


minGap([B|[]],0,[B]).
minGap([A,B|T] , GapVar , [HD|TD]):-
               #\(checkFirstDigits(A,B)),
                HD #= A,
               minGap([B|T] , GapVar , TD ).

minGap([A,B|T] , GapVar , DaysNoDuplicates):-
              checkFirstDigits(A,B),
               minGap([B|T] , GapVar1 , DaysNoDuplicates),
               Var #= B - A ,
               Var2 #= Var -1 ,
               GapVar #= Var2 + GapVar1.

convert([] , [] , [] , [],[]).
convert([schRow(CourseS, _, DayS, SlotS, TypeS, _, _)|T] , [CourseO|TC] , [DayS|TD] , [SlotS|TS] ,[TypeO|TT]):-

                          atom_codes(CourseS, CourseCodes),
                          atomic_list_concat(CourseCodes, CourseCode),
                          atom_number(CourseCode, CourseO),
                         
                          atom_codes(TypeS, TypeSCodes),
                          atomic_list_concat(TypeSCodes, TypeCode),
                          atom_number(TypeCode, TypeO),
                          convert(T , TC , TD ,TS ,TT).




                         
checkFirstDigits(A ,B):-
                ModA #= A mod 10,
               ModB #= B mod 10,
               FirstDigitA #= A - ModA,
               FirstDigitB #= B - ModB,

               FirstDigitA1 #= FirstDigitA // 10,

               FirstDigitB1 #=  FirstDigitB // 10,
               FirstDigitA1 #=  FirstDigitB1.
               
notcheckFirstDigits(A,B):-
                             ModA #= A mod 10,
               ModB #= B mod 10,
               FirstDigitA #= A - ModA,
               FirstDigitB #= B - ModB,

               FirstDigitA1 #= FirstDigitA // 10,

               FirstDigitB1 #=  FirstDigitB // 10,
               FirstDigitA1 #\=  FirstDigitB1.
               
               
remove_duplicates([] , []).
remove_duplicates([H|T] , [A|B]):-
                        all_different([H|T]),
                         remove_duplicates(T, B),
                        A #= H.

remove_duplicates([H|T] , S):-
                        #\(all_different([H|T])),
                        remove_duplicates(T, S).
                        





