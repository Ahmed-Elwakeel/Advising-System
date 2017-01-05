:- use_module(library(csv)).
:- use_module(library(clpfd)).
:- ['cur.pl'].
:- ['obl.pl'].

readFile(File , Rows , RowName):-
                csv_read_file(File, Rows , [functor(RowName)]),
                maplist(assert, Rows).
                
forTestingB(X,Y):- X = [A,B,C],
                  X ins 0..4,
                  forTesting(X,Y),
                  print(X),
                  labeling([],X).

forTestingC([A,B,C],Y):-  A #\= 0  , \+(element(3,Y,3)),B #= 3.
forTestingC([A,B,C],Y):-  A #=0 ,(element(3,Y,3)).
forTesting(X,Y):-
               X ins 0..4,
               forTestingC(X,Y).

testtHelper([A,B,C] , Y ):-  A#\= 0, Y #= 0.
testtHelper([0,B,C] , Y):-  A#= 0, Y #= 1.



testt(X , Y):-
          X = [A,B,C],
          X ins 0..100,
          Temp #= A ^ B,
          Temp #\= 0,
          labeling([],X).
          %testtHelper(X,Y).


%test( Tobechecked  , [] ,[],0 , L ).

%test(schRow(Course, _, _, _, TutLecLab, _, _) , [schRow(CourseS, _, _, _, TutLecLabS, _, _)|T], S, N ,L):-

%                 schRow(CourseS, _, _, _, TutLecLabS, _, _) \= schRow(Course, _, _, _, TutLecLab, _, _),
%                 test(schRow(Course, _, _, _, TutLecLab, _, _) , T, S, N1,L),
%                 N #= N1+1.
%test(schRow(Course, _, _, _, TutLecLab, _, _) , [schRow(CourseS, _, _, _, TutLecLabS, _, _)|T], [HC|TC], N ,L ):-
%                 schRow(CourseS, _, _, _, TutLecLabS, _, _) = schRow(Course, _, _, _, TutLecLab, _, _),
                 

%                 test(schRow(Course, _, _, _, TutLecLab, _, _) , T, TC,N1 ,L ),
%                   N #= N1+1,
%                   Out #=  L-N ,
%                   Out1 #= Out+1,
%                 HC=Out1.
                 

test( Tobechecked  ,[], [] ,[0],0 , L ).

test(CourseTypeN, [CourseCode|CCT], [TypeCode|TCT],  S, N ,L):-
                 atom_concat(CourseCode,TypeCode, CourseTypeSch),
                 atom_number(CourseTypeSch , CourseTypeSchN),
                 CourseTypeN #\= CourseTypeSchN,
                 test(CourseTypeN ,CCT, TCT, S, N1,L),
                 N #= N1+1.
test(CourseTypeN , [CourseCode|CCT], [TypeCode|TCT], [HC|TC], N ,L ):-
                 atom_concat(CourseCode,TypeCode, CourseTypeSch),
                 atom_number(CourseTypeSch , CourseTypeSchN),
                 CourseTypeN #= CourseTypeSchN,



                 test(CourseTypeN , CCT, TCT , TC,N1 ,L ),
                   N #= N1+1,
                   Out #=  L-N ,
                   Out1 #= Out+1,
                 HC#=Out1.

                 

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
%Cur = [curRow('CSEN 102',2,[]),curRow('CSEN 301',2,[]) , curRow('PHYSp 301',2,[]), curRow('CSEN 909',2,[]),curRow('CSEN 702',2,[]) ,curRow('CSEN 704',2,[])],
%History = [],
Cur = [curRow('CSEN 301',2,[]),curRow('CSEN 102',2,[])],
History = [],
        hc(Cur, History, Ncourses1),       % remove courses with a grade not equal F from curriculum
        checkPre(Ncourses1, History, Ncourses),
        
        length(Schedule , ScheduleSize),
        length(Ncourses , CoursesSize),
        NewCoursesSize #= CoursesSize * 3,
        length(TLLCourses, NewCoursesSize),  %TLLCourses represent the number of courses that could be taken multiplied by 3 to account for the lecture, lab, and tutorial.
        TLLCourses ins 0..ScheduleSize,  %0 means there is not lecture , tutorial , or lab for this course. else, it represent a row in the schedule.
        convertArrayOfAtoms(OblCourses , OblCoursesCode),
        convert(Schedule , CourseCode , DayCode ,SlotCode ,TypeCode , GroupCode),

        checkCorrectness(0,TLLCourses , Ncourses, Schedule ,OblCoursesCode, Chs ,  CourseCode , DayCode ,SlotCode ,TypeCode , GroupCode  ), %After checkCorrectness ,   TLLCourses is correct, such that each 3 consecutive elements are matching correctly with the element in the Ncourses.
        %checkGroup(TLLCourses , DayCode ,SlotCode ,TypeCode , GroupCode ),

        checkDifferentTime(TLLCourses , Schedule , Gaps , Days ,  DayCode ,SlotCode),

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

        print(TLLCourses).
        %list_to_set(Days , DaysNoDuplicates),

        %length( DaysNoDuplicates , DaysSize),
        %DaysSize #< 6.



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



checkCHWithObl(ChsSoFar ,[T,Lec,Lab|Tail] ,Course,Ch,OblCoursesCode,Chout ,Res):-
                                                               atom_codes(Course, CourseCodes),
                                                               atomic_list_concat(CourseCodes, Course1),
                                                               atom_number(Course1, CourseC ),
                                                               element(A , OblCoursesCode , CourseC),
                                                               T + Lec + Lab #\= 0,
                                                               Chout #= Ch,
                                                               print("HEHEHEHEHEHE").
                                                               


checkCHWithObl(ChsSoFar ,[T,Lec,Lab|Tail] ,Course,Ch,OblCoursesCode,Chout,Res):-

                                                               atom_codes(Course, CourseCodes),
                                                               atomic_list_concat(CourseCodes, Course1),
                                                               atom_number(Course1, CourseC ),
                                                               \+(element(A , OblCoursesCode , CourseC)),
                                                               B #= T + Lec + Lab,
                                                               %Res #= A // A ,
                                                               B2 #= 2 * B ,
                                                               BBot #= B +1,
                                                               Res #= B2 // BBot,
                                                               %CH1 #=  ChsSoFar + Ch,
                                                               Chout #= Ch * Res,

                                                               print(Chout).

                                                               



% After checkCorrectness ,   TLLCourses is correct, such that each 3 consecutive elements are matching correctly with the element in the Ncourses.
% check that the summation of all the credit hours equals the given credit hours
checkCorrectness(ChsSoFar , []  ,  [],  Schedule ,OblCourses, Chs , CourseCode , DayCode ,SlotCode ,TypeCode , GroupCode):-ChsSoFar #= Chs ,print("EEEEEEE"), print(Chs) .




checkCorrectness(ChsSoFar , [T,Lec,Lab|Tail]  ,  [curRow(Course,Ch,Pre) | TC],  Schedule ,OblCourses, Chs , CourseCode , DayCode ,SlotCode ,TypeCode , GroupCode ):-

                                    checkT( [T,_,_ |Tail]  ,  [curRow(Course,Ch,Pre) | TC],  Schedule , OblCourses ,DomainListT , CourseCode , TypeCode ),
                                    checkLec([T,Lec,_ |Tail]  ,  [curRow(Course,Ch,Pre) | TC],  Schedule,OblCourses,DomainListLec , DomainListT , CourseCode , TypeCode ),
                                    checkLab([_,Lec,Lab |Tail]  ,  [curRow(Course,Ch,Pre) | TC],  Schedule,OblCourses , DomainListLab , DomainListLec , CourseCode , TypeCode ),

                                    removeWrongCombinations(T ,  Lec,  Lab , DomainListT ),
                                    removeWrongCombinations(T ,  Lab,  Lab , DomainListT ),
                                    removeWrongCombinations(Lec ,  Lab, T , DomainListLec ),
                                    removeWrongCombinations(Lec ,  T, T , DomainListLec ),
                                    removeWrongCombinations(Lab ,  T,Lec,DomainListLab ),
                                    removeWrongCombinations(Lab ,  Lec ,Lec,DomainListLab ),
                                    checkCHWithObl(ChsSoFar,[T,Lec,Lab|Tail] , Course ,Ch, OblCourses  ,X1 , Res),

                                    X2 #= ChsSoFar + X1,
                                    checkCorrectness(X2 , Tail , TC , Schedule,OblCourses, Chs ,  CourseCode , DayCode ,SlotCode ,TypeCode , GroupCode),
                                    checkSameGroup(T,Lab , DayCode ,SlotCode ,TypeCode , GroupCode , Res).

% check if it is 0 in one of the elements , then check that it doesnot exist in the schedule so it is true.
% if it is not 0 then the number (T , Lec , Lab) should represent a row in the schedule with the same course in Ncourses and the same thing( lab , tut ,lecture).

myMember(Source , Course , Schedule , Type , CourseC , List, CourseCode , TypeCode):-
                            %convert(Schedule , CourseCode , DayCode ,SlotCode ,TypeCode),

                            atom_codes(Course, CourseCodes),
                            atomic_list_concat(CourseCodes, Course1),
                            atom_number(Course1, CourseC ),
                            atom_codes(Type, L),
                            atomic_list_concat(L, L1),
                            atom_number(L1, TypeO),
                            atom_concat(CourseC , TypeO , CourseType1),
                            atom_number(CourseType1, CourseType ),
                            length(Schedule, Size),

                            test(CourseType, CourseCode , TypeCode , List , Size, Size),



                            testHelper(List,Domain),


                            in(Source,Domain).



% If it is 0 X , or X 0 then it is false , otherwise it will be taken. for example :  (0 0) and (X, X) will be taken
removeWrongCombinations(First , Second ,Third, DomainListFirst):-


                                       length(DomainListFirst , Size),
                                       Size #= 1.


removeWrongCombinations(First , Second ,Third, DomainListFirst):-

                                       length(DomainListFirst , Size),
                                      Size #\= 1,
                                       
                                       %B = Second,

                                       %B2 #= 2 * Second ,
                                       %BBot #= Second +1,
                                       %Res #= B2 // BBot,



                                       Second #\=0 #<==> Res,

                                       Temp #= First ^ Res,


                                       Temp #\= 0.

checkT([T,_,_|Tail]   , [curRow(Course,Ch,Pre) | TC],  Schedule , OblCourses ,DomainOut  , CourseCode , TypeCode ):-   \+(myMember(T , Course , Schedule , 'Tut' , CourseC ,DomainOut , CourseCode , TypeCode)) , T #= 0 .
checkT([T,_,_|Tail]   , [curRow(Course,Ch,Pre) | TC],  Schedule , OblCourses,DomainOut , CourseCode , TypeCode ):-     myMember(T , Course , Schedule , 'Tut' , CourseC ,DomainOut, CourseCode , TypeCode).
                                                                  %, element(C , OblCourses , CourseC ).%,T #\= 0.
                                                                    % member( schRow(Course, _, _, _, 'Tut', _, _) , Schedule ), member(Course , OblCourses),
                                                                    %length(Schedule, Size),
                                                                    %test(schRow(Course, _, _, _, 'Tut', _, _), Schedule, List , Size, Size),
                                                                    %testHelper(List,Domain),
                                                                    %in(T,Domain).
%checkT([T,_,_|Tail]   , [curRow(Course,Ch,Pre) | TC],  Schedule, OblCourses):- myMember(A , Course , Schedule , 'Tut' , CourseC), \+(element(C , OblCourses , CourseC )), T #= 0.
                                                                     %member( schRow(Course, _, _, _, 'Tut', _, _) , Schedule ), \+(member(Course , OblCourses)),
                                                                     %length(Schedule, Size),
                                                                     %test(schRow(Course, _, _, _, 'Tut', _, _), Schedule, List , Size, Size),
                                                                     %testHelper(List,Domain),
                                                                    %in(T,Domain).


checkLec([T,Lec,_|Tail]   , [curRow(Course,Ch,Pre) | TC],  Schedule,OblCourses ,DomainOut ,DomainList , CourseCode , TypeCode ):-  \+(myMember(Lec , Course , Schedule , 'Lecture' , CourseC,DomainOut, CourseCode , TypeCode)) , Lec #= 0.
checkLec([T,Lec,_|Tail]   , [curRow(Course,Ch,Pre) | TC],  Schedule,OblCourses,DomainOut, DomainList , CourseCode , TypeCode ):-   myMember(Lec , Course , Schedule , 'Lecture' , CourseC,DomainOut, CourseCode , TypeCode).



checkLab([_,Lec,Lab|Tail]  , [curRow(Course,Ch,Pre) | TC],  Schedule,OblCourses,DomainOut, DomainList , CourseCode , TypeCode ):- \+(myMember(Lab , Course , Schedule , 'Lab' , CourseC,DomainOut, CourseCode , TypeCode)) ,Lab #= 0.
checkLab([_,Lec,Lab|Tail]  , [curRow(Course,Ch,Pre) | TC],  Schedule,OblCourses,DomainOut , DomainList , CourseCode , TypeCode ):- myMember(Lab , Course , Schedule , 'Lab' , CourseC,DomainOut, CourseCode , TypeCode).







%course has a lab and a tutorial, it is preferable that the student attends both with the same group.
%the student should attend the tutorial before the lab.
checkSameGroupHelper(T, Lab , Temp ):-
                          %T1 #= T2 , T2 #= T3 , T3 #= T,
                         %Lab1 #= Lab2 , Lab2 #= Lab3, Lab3 #= Lab,

                         /*element(T1 , DayCode, TD),
                         element(T1, SlotCode ,TS),
                         element(T1 , GroupCode ,GT),
                         element(Lab1 , DayCode, LD),
                         element(Lab1 , SlotCode ,LS),
                         element(Lab1 , GroupCode ,GL),
                         T1 #= T #<==> Res1,
                         Lab1 #= Lab #<==> Res2,
                         Res1 + Res2 #= 2.*/
                         T #\=0 #<==> Res1,
                         Lab #\=0 #<==> Res2,
                         Temp #= Res1 +Res2 .
                         



                         %T1 #= T2 , T2 #= T3 , T3 #= T,
                         %Lab1 #= Lab2 , Lab2 #= Lab3, Lab3 #= Lab,
                         /*
                         T1Res #= T1 * Res ,T2Res #= T2 * Res, T3Res #= T3 * Res,
                         Lab1Res #= Lab1 * Res,Lab2Res #= Lab2 * Res,Lab3Res #= Lab3 * Res,
                         T1Res #= T2Res ,T2Res #= T3Res , T3Res #= T,
                         Lab1Res #= Lab2Res, Lab2Res #= Lab3Res, Lab3Res #= Lab,
                         
                         T11 = T1 , T22 = T2 , T33 = T3,  T44 = T ,
                         Lab11 = Lab1 , Lab22 = Lab2, Lab33 = Lab3, Lab44 = Lab,
                         T11 #= T22 , T22 #= T33 , T33 #= T44,
                         Lab11 #= Lab22 , Lab22 #= Lab33, Lab33 #= Lab44.
                         */

/*checkSameGroup(T,Lab,DayCode ,SlotCode ,TypeCode , GroupCode , Res):-
                         checkSameGroupHelper(T, Lab , Temp ),
                         Temp #\= 2.
*/
checkSameGroup(T,Lab  ,DayCode ,SlotCode ,TypeCode , GroupCode , Res ):-

                         %convert(Schedule , CourseCode , DayCode ,SlotCode ,TypeCode) ,
                         %checkSameGroupHelper(T,Lab , DayCode ,SlotCode ,TypeCode , GroupCode , Res ),
                         %checkSameGroupHelper(T, Lab , Temp )     ,
                         %Temp #= 2,
                         element(T1 , DayCode, TD),
                         element(T1, SlotCode ,TS),
                         element(T1 , GroupCode ,GT),
                         element(Lab1 , DayCode, LD),
                         element(Lab1 , SlotCode ,LS),
                         element(Lab1 , GroupCode ,GL),
                        % T #= T1 #<==> Lab #= Lab1T ,
                          T #= T1 #\/ T #= 0 ,Lab #= Lab1 #\/ Lab #= 0 ,

                         A1 #= GT mod 1000,
                         B1 #= GL mod 1000,
                         A1 #= B1,


                         TDay10 #= TD * 10,
                         DST #= TDay10 + TS,
                         LDay10 #= LD * 10,
                         DSL #= LDay10 + LS,

                         DST #=< DSL.

checkGroup([] , DayCode ,SlotCode ,TypeCode , GroupCode ):- print("I AM ").
checkGroup([T,_,Lab|Tail] , DayCode ,SlotCode ,TypeCode , GroupCode ):-   checkSameGroup(T,Lab , DayCode ,SlotCode ,TypeCode , GroupCode , 1),checkGroup(Tail , DayCode ,SlotCode ,TypeCode , GroupCode ).


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

checkDifferentTime(A , B , Gaps , Days, DayCode ,SlotCode) :-   checkDifferentTimeHelper(A,B,X,Days , Gaps, DayCode ,SlotCode),

                               all_different(X).
                               %list_to_set(Days , DaysNoDuplicates)    ,
                               %print(Days),print(DaysNoDuplicates),
                               %remove_duplicates(Days , DaysNoDuplicates),
                               %length( DaysNoDuplicates , DaysSize),
                               %DaysSize #< 6.

checkDifferentTimeHelper([0|T],Schedule , X , Days , Gaps ,  DayCode ,SlotCode):-checkDifferentTimeHelper(T , Schedule , X , Days , Gaps ,  DayCode ,SlotCode).
checkDifferentTimeHelper([] , Schedule , [],[],[] ,  DayCode ,SlotCode).
checkDifferentTimeHelper([H|T] , Schedule , [HC|TC] , [HD|TD], [HG|TG] , DayCode ,SlotCode):-

                         checkDifferentTimeHelper(T , Schedule , TC , TD , TG ,DayCode ,SlotCode),

                         %convert(Schedule , CourseCode , DayCode ,SlotCode ,TypeCode,  GroupCode)  ,

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

convert([] , [] , [] , [],[] , []).
convert([schRow(CourseS, _, DayS, SlotS, TypeS, _, Group)|T] , [CourseO|TC] , [DayS|TD] , [SlotS|TS] ,[TypeO|TT] , [GroupO|TG]):-

                          atom_codes(CourseS, CourseCodes),
                          atomic_list_concat(CourseCodes, CourseCode),
                          atom_number(CourseCode, CourseO),
                         
                          atom_codes(TypeS, TypeSCodes),
                          atomic_list_concat(TypeSCodes, TypeCode),
                          atom_number(TypeCode, TypeO),

                          atom_codes(Group, GroupCodes),
                          atomic_list_concat(GroupCodes, GroupCode),
                          atom_number(GroupCode, GroupO1),
                          GroupO #= GroupO1 mod 10000,
                          convert(T , TC , TD ,TS ,TT , TG).




                         
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
                        

convertArrayOfAtoms([] , []).
convertArrayOfAtoms([H|T] , [CourseO|TO]):-

                          atom_codes(H, CourseCodes),
                          atomic_list_concat(CourseCodes, CourseCode),
                          atom_number(CourseCode, CourseO),
                          convertArrayOfAtoms(T , TO).

                        





