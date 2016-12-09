:- use_module(library(csv)).
:- use_module(library(clpfd)).
:- ['cur.pl'].
:- ['obl.pl'].

readFile(File , Rows , RowName):-
                csv_read_file(File, Rows , [functor(RowName)]),
                maplist(assert, Rows).



test(X):-
         X = [A,B,C],
         X ins 0..2,
         testHelper(X),
         labeling([],X).
         
testHelpe([H|T]):-
              H #\= 0 ,
              testHelper(T).

advising_system(Chs ,Ncourses,TLLCourses,Schedule):-
        readFile('data_formated_22556.csv' , Schedule , schRow),
        %readFile('history.csv' , History , histrow),
        %getList(Cur),
        getObl(OblCourses),
%Cur = [curRow('CSEN 503',4,['CSEN 501']),curRow('AE 101',2,[]),curRow('DE 101',2,[]),curRow('CHEMt 102',4,[]),curRow('As 102',4,['AE 101'])],
%History = [histrow('AE 101','F'),histrow('CHEMt 102','B'),histrow('CSEN 501','B')],
Cur = [curRow('PHYS 101',4,[]),curRow('AE 101',2,[]) , curRow('CSEN 102',1,[])],
History = [histrow('AE 101','B'),histrow('As 102','B'),histrow('CHEMt 102','B')],
        hc(Cur, History, Ncourses1),       % remove courses with a grade not equal F from cur
        checkPre(Ncourses1, History, Ncourses),
        
        length(Schedule , ScheduleSize),
        length(Ncourses , CoursesSize),
        NewCoursesSize #= CoursesSize * 3,
        length(TLLCourses, NewCoursesSize),  %TLLCourses represent the number of courses that could be taken multiplied by 3 to account for the lecture, lab, and tutorial.
        TLLCourses ins 0..ScheduleSize,  %0 means there is not lecture , tutorial , or lab for this course. else, it represent a row in the schedule.

        checkCorrectness(0 ,TLLCourses , Ncourses, Schedule ,OblCourses, Chs ), %After checkCorrectness ,   TLLCourses is correct, such that each 3 consecutive elements are matching correctly with the element in the Ncourses.
        checkDifferentTime(TLLCourses , Schedule ),

        labeling([],TLLCourses).




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
                                             \+(member(Course , OblCourses)), %obligatory courses that the student won�t be able to skip
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
checkT([T,_,_|Tail]   , [curRow(Course,Ch,Pre) | TC],  Schedule):-  T #= 0 , \+(member( schRow(Course, _, _, _, 'Tut', _, _) , Schedule )).
checkT([T,_,_|Tail]   , [curRow(Course,Ch,Pre) | TC],  Schedule):-  T #\= 0, nth1(T , Schedule, schRow(Course, _, _, _, 'Tut', _, _)).

checkLec([_,Lec,_|Tail]   , [curRow(Course,Ch,Pre) | TC],  Schedule):- Lec #= 0, \+(member( schRow(Course, _, _, _, 'Lecture', _, _) , Schedule )).
checkLec([_,Lec,_|Tail]   , [curRow(Course,Ch,Pre) | TC],  Schedule):- Lec #\= 0, nth1(Lec , Schedule, schRow(Course, _, _, _, 'Lecture', _, _)).
                                   
checkLab([_,_,Lab|Tail]  , [curRow(Course,Ch,Pre) | TC],  Schedule):- Lab #= 0, \+(member( schRow(Course, _, _, _, 'Lab', _, _) , Schedule )).
checkLab([_,_,Lab|Tail]  , [curRow(Course,Ch,Pre) | TC],  Schedule):- Lab #\= 0, nth1(Lab , Schedule, schRow(Course, _, _, _, 'Lab', _, _)).


%course has a lab and a tutorial, it is preferable that the student attends both with the same group.
%the student should attend the tutorial before the lab.
checkSameGroup(0,_,S).
checkSameGroup(_,0,S).
checkSameGroup(T,Lab ,Schedule ):-
                         nth1(T , Schedule, schRow(_, _, DayT, SlotT, _, _, GT)),
                         nth1(Lab , Schedule, schRow(_, _, DayL, SlotL, _, _, GL)),
                         atom_length( GT,SGT),
                         atom_length( GL,SGL),
                         IGT1 #= SGT - 2,
                         IGL1 #=  SGL - 2 ,
                         sub_atom(GT, IGT1, 2, _, A) ,
                         sub_atom(GL, IGL1, 2, _, B) ,
                         atom_number(A, A1),
                         atom_number(B,B1),
                         A1 #= B1,
                         atom_concat(DayT, SlotT, Out),
                         atom_number(Out , DST),
                         atom_concat(DayL, SlotL, Out1),
                         atom_number(Out1 , DSL),
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

checkDifferentTime(A , B) :-   checkDifferentTimeHelper(A,B,X,Days),
                               all_different(X),
                               list_to_set(Days , DaysNoDuplicates),
                               length( DaysNoDuplicates , DaysSize),
                               DaysSize #< 6.

checkDifferentTimeHelper([0|T],Schedule , X , Days):- checkDifferentTimeHelper(T , Schedule , X , Days).
checkDifferentTimeHelper([] , Schedule , [],[]).
checkDifferentTimeHelper([H|T] , Schedule , [HC|TC] , [HD|TD]):-
                         checkDifferentTimeHelper(T , Schedule , TC , TD),
                         nth1(H , Schedule, schRow(_, _, A, B, _, _, _)),
                         atom_concat(A, B, Out),
                         atom_number(Out , HC),
                         atom_concat(A, '', A1),
                         atom_number(A1 , HD).



                         








