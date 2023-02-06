%\\\\\\\\\\\\\\\\\\\\\\\\\ Knowladge Base /////////////////////////%

room(z23).
room(z11).
room(z06).
room(z10).

course(101).
course(102).
course(241).
course(222).
course(331).
course(341).
course(455).
course(452).

instructor(mehmet).
instructor(yakup).
instructor(erdogan).
instructor(yusuf).
instructor(alp).

student(student1).
student(student2).
student(student3).
student(student4).
student(student5).

% course time
when(101, 12).
when(102, 12).
when(241, 12).
when(222, 11).
when(331, 14).
when(341, 14).
when(455, 16).
when(452, 17).

% course room
where(101, z11).
where(102, z23).
where(241, z23).
where(222, z11).
where(331, z23).
where(341, z06).
where(455, z10).
where(452, z10).

% room provides the given special equipment 
equipment(z23, projector).
equipment(z11, computer).
equipment(z06, projector).
equipment(z10, smart_board).
equipment(z10, projector).

% room has access for the handicapped students
access_for_handicapped(z10). 
access_for_handicapped(z06).

% room capacity
capacity(z23, 120).
capacity(z11, 50).
capacity(z06, 150).
capacity(z10, 40).

% course capacity
capacity(101, 120).
capacity(102, 120).
capacity(241, 150).
capacity(222, 90).
capacity(331, 150).
capacity(341, 150).
capacity(455, 40).
capacity(452, 40).

% course requires the given special equipment 
needs(101, projector).
needs(102, projector).
needs(222, projector).
needs(241, smart_board).
needs(341, smart_board).
needs(331, smart_board).
needs(455, projector).
needs(452, smart_board).

% instructor teaches the given course
teaches(mehmet, 101).
teaches(yakup, 102).
teaches(erdogan, 222).
teaches(yusuf, 241).
teaches(yakup, 341).
teaches(alp, 331).
teaches(yakup, 455).
teaches(yakup, 452).

% instructor prefers a room with the special equipment
prefers(yakup, projector).
prefers(yakup, projector).
prefers(yusuf, smart_board).
prefers(mehmet, projector).
prefers(alp, smart_board).

% handicapped student
handicapped(student2).

% student enrollees to the given course
enroll(student1, 102).
enroll(student2, 341).
enroll(student3, 241).
enroll(student4, 101).
enroll(student5, 331).
enroll(student5, 331).

%\\\\\\\\\\\\\\\\\\\\\\\\\ Rules /////////////////////////%

enroll(S, C) :-
    student(S), 
    handicapped(S),
    course(C),
    where(C, R),
    access_for_handicapped(R).

enroll(S, C) :-
    student(S),
    \+ (handicapped(S)),
    course(C).

% checks if there is any scheduling conflict between two course
conflict(X, Y) :-
    course(X), 
    course(Y),
    where(X, P),
    where(Y, P),
    when(X, T),
    when(Y, T),
    X \= Y.

% asssign room for the given course
assign(C, R) :-
    course(C),
    room(R),
    teaches(I, C),
    capacity(R, CR),
    capacity(C, CC),
    CC =< CR,
    forall(needs(C, E), equipment(R, E)),
    forall(prefers(I, E), equipment(R, E)).